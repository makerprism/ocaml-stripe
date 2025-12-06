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
