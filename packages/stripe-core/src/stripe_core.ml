(** Stripe Core - Runtime-agnostic interfaces for Stripe API *)

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

(** Build authorization header *)
let auth_header config =
  let credentials = Base64.encode_string config.api_key in
  ("Authorization", "Basic " ^ credentials ^ ":")

(** Build common headers for Stripe API requests *)
let build_headers config =
  let headers = [
    auth_header config;
    ("Content-Type", "application/x-www-form-urlencoded");
  ] in
  match config.api_version with
  | Some version -> ("Stripe-Version", version) :: headers
  | None -> headers

(** URL-encode a form parameter *)
let url_encode_param (key, value) =
  Uri.pct_encode key ^ "=" ^ Uri.pct_encode value

(** Build form-encoded body from parameters *)
let build_form_body params =
  params
  |> List.map url_encode_param
  |> String.concat "&"

(** Stripe List response (paginated) *)
type 'a list_response = {
  data : 'a list;
  has_more : bool;
  url : string;
}

(** Result type for API calls *)
type 'a api_result = ('a, stripe_error) result

module Base64 = struct
  let encode_string s =
    (* Simple base64 encoding - in real implementation use base64 library *)
    Base64.encode_string s
end

(** Webhook signature verification error *)
type signature_verification_error = {
  message : string;
  sig_header : string;
  payload : string;
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

  (** Verify the webhook signature header *)
  let verify_header ~payload ~header ~secret ?(tolerance = default_tolerance) () =
    let timestamp, signatures = 
      try get_timestamp_and_signatures header expected_scheme
      with _ -> 
        raise (Signature_verification_error {
          message = "Unable to extract timestamp and signatures from header";
          sig_header = header;
          payload;
        })
    in
    
    let timestamp = match timestamp with
      | Some t -> t
      | None -> 
        raise (Signature_verification_error {
          message = "Unable to extract timestamp and signatures from header";
          sig_header = header;
          payload;
        })
    in
    
    if signatures = [] then
      raise (Signature_verification_error {
        message = Printf.sprintf "No signatures found with expected scheme %s" expected_scheme;
        sig_header = header;
        payload;
      });
    
    let signed_payload = Printf.sprintf "%d.%s" timestamp payload in
    let expected_sig = compute_signature ~payload:signed_payload ~secret in
    
    let has_valid_signature = 
      List.exists (fun sig_ -> secure_compare expected_sig sig_) signatures
    in
    
    if not has_valid_signature then
      raise (Signature_verification_error {
        message = "No signatures found matching the expected signature for payload";
        sig_header = header;
        payload;
      });
    
    (* Check timestamp tolerance if specified *)
    if tolerance > 0 then begin
      let now = int_of_float (Unix.gettimeofday ()) in
      if timestamp < now - tolerance then
        raise (Signature_verification_error {
          message = Printf.sprintf "Timestamp outside the tolerance zone (%d)" timestamp;
          sig_header = header;
          payload;
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
