(** Stripe Core - Runtime-agnostic interfaces for Stripe API *)

(** {1 Security Notes}
    
    This library handles sensitive payment data. Users should:
    - Never log config objects (contains API keys)
    - Use environment variables for API keys
    - Only transmit client_secret values over HTTPS to frontend clients
    - Never log raw API responses (may contain sensitive data)
*)

(** HTTP Method type *)
type http_method = GET | POST | DELETE | PUT | PATCH

let string_of_http_method = function
  | GET -> "GET"
  | POST -> "POST"
  | DELETE -> "DELETE"
  | PUT -> "PUT"
  | PATCH -> "PATCH"

(** HTTP Response *)
type http_response = {
  status_code : int;
  headers : (string * string) list;
  body : string;
}

(** HTTP Request signature for CPS-style *)
module type HTTP_CLIENT = sig
  type 'a io
  
  val request :
    meth:http_method ->
    url:string ->
    headers:(string * string) list ->
    ?body:string ->
    unit ->
    http_response io
end

(** Stripe API Error types *)
type error_type =
  | Api_error
  | Card_error
  | Idempotency_error
  | Invalid_request_error
  | Authentication_error
  | Rate_limit_error

type stripe_error = {
  error_type : error_type;
  message : string;
  code : string option;
  param : string option;
  decline_code : string option;
  doc_url : string option;
}

let error_type_of_string = function
  | "api_error" -> Api_error
  | "card_error" -> Card_error
  | "idempotency_error" -> Idempotency_error
  | "invalid_request_error" -> Invalid_request_error
  | "authentication_error" -> Authentication_error
  | "rate_limit_error" -> Rate_limit_error
  | _ -> Api_error

(** Parse a Stripe error from JSON response *)
let parse_error (json : Yojson.Safe.t) : stripe_error option =
  let open Yojson.Safe.Util in
  try
    let error = json |> member "error" in
    let error_type = 
      error |> member "type" |> to_string |> error_type_of_string 
    in
    let message = error |> member "message" |> to_string in
    let code = error |> member "code" |> to_string_option in
    let param = error |> member "param" |> to_string_option in
    let decline_code = error |> member "decline_code" |> to_string_option in
    let doc_url = error |> member "doc_url" |> to_string_option in
    Some { error_type; message; code; param; decline_code; doc_url }
  with _ -> None

(** Stripe API configuration *)
type config = {
  api_key : string;
  api_base : string;
  api_version : string option;
  max_network_retries : int;
  timeout : float;  (** in seconds *)
}

let default_config ~api_key = {
  api_key;
  api_base = "https://api.stripe.com";
  api_version = None;
  max_network_retries = 0;
  timeout = 30.0;
}

(** Request options for individual API calls *)
type request_options = {
  idempotency_key : string option;
  stripe_account : string option;  (** For Connect: the connected account ID *)
}

let default_request_options = {
  idempotency_key = None;
  stripe_account = None;
}

(** Generate a cryptographically secure random idempotency key (UUID v4 format).
    Uses /dev/urandom for secure random bytes. *)
let generate_idempotency_key () =
  (* Read 16 random bytes from /dev/urandom for cryptographic security *)
  let ic = open_in_bin "/dev/urandom" in
  let bytes = really_input_string ic 16 in
  close_in ic;
  (* Format as UUID v4 *)
  let hex_of_char c = Printf.sprintf "%02x" (Char.code c) in
  Printf.sprintf "%s%s%s%s-%s%s-%s%s-%s%s-%s%s%s%s%s%s"
    (hex_of_char bytes.[0]) (hex_of_char bytes.[1])
    (hex_of_char bytes.[2]) (hex_of_char bytes.[3])
    (hex_of_char bytes.[4]) (hex_of_char bytes.[5])
    (hex_of_char bytes.[6]) (hex_of_char bytes.[7])
    (hex_of_char bytes.[8]) (hex_of_char bytes.[9])
    (hex_of_char bytes.[10]) (hex_of_char bytes.[11])
    (hex_of_char bytes.[12]) (hex_of_char bytes.[13])
    (hex_of_char bytes.[14]) (hex_of_char bytes.[15])

(** Build authorization header *)
let auth_header config =
  ("Authorization", "Bearer " ^ config.api_key)

(** Build common headers for Stripe API requests *)
let build_headers ?(options = default_request_options) config =
  let headers = [
    auth_header config;
    ("Content-Type", "application/x-www-form-urlencoded");
  ] in
  let headers = match config.api_version with
    | Some version -> ("Stripe-Version", version) :: headers
    | None -> headers
  in
  let headers = match options.idempotency_key with
    | Some key -> ("Idempotency-Key", key) :: headers
    | None -> headers
  in
  let headers = match options.stripe_account with
    | Some account -> ("Stripe-Account", account) :: headers
    | None -> headers
  in
  headers

(** URL-encode a form parameter *)
let url_encode_param (key, value) =
  Uri.pct_encode key ^ "=" ^ Uri.pct_encode value

(** Build form-encoded body from parameters *)
let build_form_body params =
  params
  |> List.map url_encode_param
  |> String.concat "&"

(** {2 Security Validation} *)

(** Validate that a Stripe resource ID matches expected format.
    Stripe IDs are alphanumeric with underscores, prefixed by resource type.
    Returns true if valid, false otherwise.
    
    @param id The ID to validate
    @return true if the ID appears to be a valid Stripe ID *)
let is_valid_stripe_id id =
  let len = String.length id in
  if len < 3 || len > 255 then false
  else
    (* Must contain only alphanumeric chars and underscores *)
    let valid_char c =
      (c >= 'a' && c <= 'z') ||
      (c >= 'A' && c <= 'Z') ||
      (c >= '0' && c <= '9') ||
      c = '_'
    in
    try
      for i = 0 to len - 1 do
        if not (valid_char id.[i]) then raise Exit
      done;
      (* Should have at least one underscore (prefix separator) *)
      String.contains id '_'
    with Exit -> false

(** Validate and sanitize a resource ID for use in URL paths.
    Raises Invalid_argument if the ID is invalid.
    
    @param resource_type Description of the resource type (for error messages)
    @param id The ID to validate
    @return The validated ID (unchanged if valid)
    @raise Invalid_argument if the ID is invalid *)
let validate_id ~resource_type id =
  if is_valid_stripe_id id then id
  else invalid_arg (Printf.sprintf "Invalid %s ID: %s" resource_type id)

(** Validate that an API base URL uses HTTPS.
    Raises Invalid_argument if not HTTPS.
    
    @param url The base URL to validate
    @return The validated URL
    @raise Invalid_argument if the URL doesn't use HTTPS *)
let validate_api_base url =
  if String.length url >= 8 && String.sub url 0 8 = "https://" then url
  else invalid_arg "API base URL must use HTTPS"

(** Maximum allowed error message length to prevent memory issues *)
let max_error_body_length = 1000

(** Truncate a string to maximum length, adding ellipsis if truncated *)
let truncate_string ~max_len s =
  if String.length s <= max_len then s
  else String.sub s 0 (max_len - 3) ^ "..."

(** Stripe List response (paginated) *)
type 'a list_response = {
  data : 'a list;
  has_more : bool;
  url : string;
}

(** Get the ID of the last item in a list response for pagination.
    Requires a function to extract the ID from each item. *)
let last_id ~get_id (response : 'a list_response) : string option =
  match List.rev response.data with
  | [] -> None
  | last :: _ -> Some (get_id last)

(** Check if there are more pages available *)
let has_next_page (response : 'a list_response) : bool =
  response.has_more

(** Pagination helpers for iterating through list responses *)
module Pagination = struct
  (** Get the ID of the last item for use with starting_after *)
  let next_page_cursor ~get_id (response : 'a list_response) : string option =
    if response.has_more then
      match List.rev response.data with
      | [] -> None
      | last :: _ -> Some (get_id last)
    else
      None
  
  (** Accumulate items from multiple pages into a single list.
      [fetch_page] should take an optional starting_after cursor and return
      the next page of results.
      
      Example usage with Lwt:
      {[
        let get_all_customers ~config =
          Pagination.fold_pages
            ~get_id:(fun c -> c.Stripe.Customer.id)
            ~fetch_page:(fun ?starting_after () ->
              Customer.list ~config ?starting_after ())
            ~init:[]
            ~f:(fun acc response -> Lwt.return (acc @ response.data))
            ()
      ]}
  *)
  let rec fold_pages_sync
      ~get_id
      ~fetch_page
      ~init
      ~f
      ?starting_after
      () =
    match fetch_page ?starting_after () with
    | Error e -> Error e
    | Ok response ->
      let acc = f init response in
      match next_page_cursor ~get_id response with
      | None -> Ok acc
      | Some cursor ->
        fold_pages_sync ~get_id ~fetch_page ~init:acc ~f ~starting_after:(Some cursor) ()
  
  (** Collect all items from all pages into a single list (synchronous version).
      Returns Error if any page fetch fails. *)
  let collect_all_sync ~get_id ~fetch_page () =
    fold_pages_sync
      ~get_id
      ~fetch_page
      ~init:[]
      ~f:(fun acc response -> acc @ response.data)
      ()
end

(** Result type for API calls *)
type 'a api_result = ('a, stripe_error) result

(** Webhook signature verification error.
    Note: payload is intentionally NOT included to prevent sensitive data leakage in logs. *)
type signature_verification_error = {
  message : string;
  sig_header : string;
}

exception Signature_verification_error of signature_verification_error

(** Webhook signature verification - adapted from stripe-python *)
module Webhook_signature = struct
  let expected_scheme = "v1"
  let default_tolerance = 300 (* 5 minutes in seconds *)

  (** Compute HMAC-SHA256 signature *)
  let compute_signature ~payload ~secret =
    let key = Digestif.SHA256.hmac_string ~key:secret payload in
    Digestif.SHA256.to_hex key

  (** Constant-time string comparison to prevent timing attacks *)
  let secure_compare a b =
    if String.length a <> String.length b then false
    else
      let result = ref 0 in
      for i = 0 to String.length a - 1 do
        result := !result lor (Char.code a.[i] lxor Char.code b.[i])
      done;
      !result = 0

  (** Parse the Stripe-Signature header to extract timestamp and signatures *)
  let get_timestamp_and_signatures header scheme =
    let items = String.split_on_char ',' header in
    let parse_item item =
      match String.split_on_char '=' item with
      | [key; value] -> Some (String.trim key, String.trim value)
      | _ -> None
    in
    let parsed = List.filter_map parse_item items in
    let timestamp = 
      List.find_map (fun (k, v) -> 
        if k = "t" then int_of_string_opt v else None
      ) parsed
    in
    let signatures = 
      List.filter_map (fun (k, v) -> 
        if k = scheme then Some v else None
      ) parsed
    in
    (timestamp, signatures)

  (** Maximum number of signature items to parse (DoS protection) *)
  let max_signature_items = 20

  (** Verify the webhook signature header *)
  let verify_header ~payload ~header ~secret ?(tolerance = default_tolerance) () =
    let timestamp, signatures = 
      try get_timestamp_and_signatures header expected_scheme
      with _ -> 
        raise (Signature_verification_error {
          message = "Unable to extract timestamp and signatures from header";
          sig_header = header;
        })
    in
    
    let timestamp = match timestamp with
      | Some t -> t
      | None -> 
        raise (Signature_verification_error {
          message = "Unable to extract timestamp and signatures from header";
          sig_header = header;
        })
    in
    
    if signatures = [] then
      raise (Signature_verification_error {
        message = Printf.sprintf "No signatures found with expected scheme %s" expected_scheme;
        sig_header = header;
      });
    
    (* Limit signatures to prevent DoS *)
    let signatures = 
      if List.length signatures > max_signature_items 
      then List.filteri (fun i _ -> i < max_signature_items) signatures
      else signatures
    in
    
    let signed_payload = Printf.sprintf "%d.%s" timestamp payload in
    let expected_sig = compute_signature ~payload:signed_payload ~secret in
    
    let has_valid_signature = 
      List.exists (fun sig_ -> secure_compare expected_sig sig_) signatures
    in
    
    if not has_valid_signature then
      raise (Signature_verification_error {
        message = "No signatures found matching the expected signature for payload";
        sig_header = header;
      });
    
    (* Check timestamp tolerance if specified - both past AND future *)
    if tolerance > 0 then begin
      let now = int_of_float (Unix.gettimeofday ()) in
      if timestamp < now - tolerance then
        raise (Signature_verification_error {
          message = Printf.sprintf "Timestamp outside the tolerance zone (too old: %d)" timestamp;
          sig_header = header;
        });
      if timestamp > now + tolerance then
        raise (Signature_verification_error {
          message = Printf.sprintf "Timestamp outside the tolerance zone (in future: %d)" timestamp;
          sig_header = header;
        })
    end;
    
    true
end

(** Webhook module for constructing events from webhook payloads *)
module Webhook = struct
  let default_tolerance = Webhook_signature.default_tolerance

  (** Verify webhook signature - returns true or raises Signature_verification_error *)
  let verify_signature ~payload ~sig_header ~secret ?tolerance () =
    let tolerance = Option.value ~default:default_tolerance tolerance in
    Webhook_signature.verify_header ~payload ~header:sig_header ~secret ~tolerance ()
end
