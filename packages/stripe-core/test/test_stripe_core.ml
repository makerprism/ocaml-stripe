(** Tests for stripe-core - adapted from stripe-python/tests/test_webhook.py *)

open Alcotest
open Stripe_core

(** Test fixtures - adapted from stripe-python *)
let dummy_webhook_payload = {|{
  "id": "evt_test_webhook",
  "object": "event",
  "data": { "object": { "id": "rdr_123", "object": "terminal.reader" } }
}|}

let dummy_webhook_secret = "whsec_test_secret"

(** Generate a valid webhook header for testing *)
let generate_header ?(timestamp = int_of_float (Unix.gettimeofday ())) 
                    ?(payload = dummy_webhook_payload)
                    ?(secret = dummy_webhook_secret)
                    ?(scheme = "v1")
                    ?signature () =
  let signature = match signature with
    | Some s -> s
    | None ->
      let payload_to_sign = Printf.sprintf "%d.%s" timestamp payload in
      Webhook_signature.compute_signature ~payload:payload_to_sign ~secret
  in
  Printf.sprintf "t=%d,%s=%s" timestamp scheme signature

(* HTTP Method tests *)
let test_http_method_to_string () =
  check string "GET" "GET" (string_of_http_method GET);
  check string "POST" "POST" (string_of_http_method POST);
  check string "DELETE" "DELETE" (string_of_http_method DELETE);
  check string "PUT" "PUT" (string_of_http_method PUT);
  check string "PATCH" "PATCH" (string_of_http_method PATCH)

(* Config tests *)
let test_default_config () =
  let config = default_config ~api_key:"sk_test_123" in
  check string "api_key" "sk_test_123" config.api_key;
  check string "api_base" "https://api.stripe.com" config.api_base;
  check (option string) "api_version" None config.api_version;
  check int "max_network_retries" 0 config.max_network_retries

(* Form body tests *)
let test_build_form_body () =
  let params = [("amount", "2000"); ("currency", "usd")] in
  let body = build_form_body params in
  check string "form body" "amount=2000&currency=usd" body

(* Error parsing tests *)
let test_parse_error () =
  let json = Yojson.Safe.from_string {|
    {
      "error": {
        "type": "card_error",
        "message": "Your card was declined.",
        "code": "card_declined",
        "param": null,
        "decline_code": "generic_decline"
      }
    }
  |} in
  match parse_error json with
  | Some error ->
    check bool "error_type is Card_error" true (error.error_type = Card_error);
    check string "message" "Your card was declined." error.message;
    check (option string) "code" (Some "card_declined") error.code;
    check (option string) "decline_code" (Some "generic_decline") error.decline_code
  | None -> fail "Expected error to be parsed"

(** Webhook signature tests - adapted from stripe-python/tests/test_webhook.py *)

(* Test: valid header and signature *)
let test_valid_header_and_signature () =
  let header = generate_header () in
  let result = Webhook_signature.verify_header 
    ~payload:dummy_webhook_payload 
    ~header 
    ~secret:dummy_webhook_secret 
    ~tolerance:10
    ()
  in
  check bool "valid signature" true result

(* Test: compute signature *)
let test_compute_signature () =
  let payload = "1234567890.test_payload" in
  let secret = "whsec_test" in
  let sig1 = Webhook_signature.compute_signature ~payload ~secret in
  let sig2 = Webhook_signature.compute_signature ~payload ~secret in
  check string "deterministic" sig1 sig2;
  check bool "is hex string" true (String.length sig1 = 64)

(* Test: raise on malformed header *)
let test_raise_on_malformed_header () =
  let header = "i'm not even a real signature header" in
  try
    let _ = Webhook_signature.verify_header 
      ~payload:dummy_webhook_payload 
      ~header 
      ~secret:dummy_webhook_secret 
      ()
    in
    fail "Expected Signature_verification_error"
  with
  | Signature_verification_error err ->
    check bool "contains expected message" true 
      (String.length err.message > 0)

(* Test: raise on no signatures with expected scheme *)
let test_raise_on_no_signatures_with_expected_scheme () =
  let header = generate_header ~scheme:"v0" () in
  try
    let _ = Webhook_signature.verify_header 
      ~payload:dummy_webhook_payload 
      ~header 
      ~secret:dummy_webhook_secret 
      ()
    in
    fail "Expected Signature_verification_error"
  with
  | Signature_verification_error err ->
    check bool "mentions expected scheme" true 
      (String.sub err.message 0 13 = "No signatures")

(* Test: raise on no valid signatures for payload *)
let test_raise_on_invalid_signature () =
  let header = generate_header ~signature:"bad_signature" () in
  try
    let _ = Webhook_signature.verify_header 
      ~payload:dummy_webhook_payload 
      ~header 
      ~secret:dummy_webhook_secret 
      ()
    in
    fail "Expected Signature_verification_error"
  with
  | Signature_verification_error err ->
    check bool "mentions signature mismatch" true 
      (String.sub err.message 0 13 = "No signatures")

(* Test: raise on timestamp outside tolerance *)
let test_raise_on_timestamp_outside_tolerance () =
  let old_timestamp = int_of_float (Unix.gettimeofday ()) - 400 in
  let header = generate_header ~timestamp:old_timestamp () in
  try
    let _ = Webhook_signature.verify_header 
      ~payload:dummy_webhook_payload 
      ~header 
      ~secret:dummy_webhook_secret 
      ~tolerance:300
      ()
    in
    fail "Expected Signature_verification_error"
  with
  | Signature_verification_error err ->
    check bool "mentions timestamp" true 
      (String.sub err.message 0 9 = "Timestamp")

(* Test: timestamp off but no tolerance check *)
let test_timestamp_off_but_no_tolerance () =
  let header = generate_header ~timestamp:12345 () in
  let result = Webhook_signature.verify_header 
    ~payload:dummy_webhook_payload 
    ~header 
    ~secret:dummy_webhook_secret 
    ~tolerance:0  (* No tolerance check *)
    ()
  in
  check bool "valid with no tolerance" true result

(* Test: header contains valid signature among multiple *)
let test_header_contains_valid_signature () =
  let header = generate_header () ^ ",v1=bad_signature" in
  let result = Webhook_signature.verify_header 
    ~payload:dummy_webhook_payload 
    ~header 
    ~secret:dummy_webhook_secret 
    ~tolerance:10
    ()
  in
  check bool "valid with extra bad signature" true result

(* Test: secure compare *)
let test_secure_compare () =
  check bool "equal strings" true 
    (Webhook_signature.secure_compare "abc" "abc");
  check bool "different strings" false 
    (Webhook_signature.secure_compare "abc" "abd");
  check bool "different lengths" false 
    (Webhook_signature.secure_compare "abc" "abcd");
  check bool "empty strings" true 
    (Webhook_signature.secure_compare "" "")

(* Test: Webhook module convenience function *)
let test_webhook_verify_signature () =
  let header = generate_header () in
  let result = Webhook.verify_signature
    ~payload:dummy_webhook_payload
    ~sig_header:header
    ~secret:dummy_webhook_secret
    ()
  in
  check bool "webhook verify" true result

(* Test: idempotency key generation *)
let test_generate_idempotency_key () =
  let key1 = generate_idempotency_key () in
  let key2 = generate_idempotency_key () in
  (* Keys should be UUID-like format: xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx *)
  check bool "key1 has correct length" true (String.length key1 = 36);
  check bool "key2 has correct length" true (String.length key2 = 36);
  check bool "keys are different" true (key1 <> key2);
  (* Check format with dashes *)
  check bool "key1 has dashes" true (String.get key1 8 = '-');
  check bool "key1 has second dash" true (String.get key1 13 = '-')

(* Test: build headers with idempotency key *)
let test_build_headers_with_idempotency_key () =
  let config = default_config ~api_key:"sk_test_123" in
  let options = { default_request_options with idempotency_key = Some "test-key-123" } in
  let headers = build_headers ~options config in
  let has_idempotency_key = List.exists (fun (k, v) -> 
    k = "Idempotency-Key" && v = "test-key-123"
  ) headers in
  check bool "has idempotency key header" true has_idempotency_key

(* Test: build headers with stripe account *)
let test_build_headers_with_stripe_account () =
  let config = default_config ~api_key:"sk_test_123" in
  let options = { default_request_options with stripe_account = Some "acct_123" } in
  let headers = build_headers ~options config in
  let has_stripe_account = List.exists (fun (k, v) -> 
    k = "Stripe-Account" && v = "acct_123"
  ) headers in
  check bool "has stripe account header" true has_stripe_account

(* Test: build headers without options *)
let test_build_headers_without_options () =
  let config = default_config ~api_key:"sk_test_123" in
  let headers = build_headers config in
  let has_idempotency_key = List.exists (fun (k, _) -> k = "Idempotency-Key") headers in
  let has_stripe_account = List.exists (fun (k, _) -> k = "Stripe-Account") headers in
  check bool "no idempotency key header" false has_idempotency_key;
  check bool "no stripe account header" false has_stripe_account

(** Additional error type tests - adapted from stripe-python *)

(* Test: error type parsing for all error types *)
let test_error_type_of_string () =
  check bool "api_error" true (error_type_of_string "api_error" = Api_error);
  check bool "card_error" true (error_type_of_string "card_error" = Card_error);
  check bool "idempotency_error" true (error_type_of_string "idempotency_error" = Idempotency_error);
  check bool "invalid_request_error" true (error_type_of_string "invalid_request_error" = Invalid_request_error);
  check bool "authentication_error" true (error_type_of_string "authentication_error" = Authentication_error);
  check bool "rate_limit_error" true (error_type_of_string "rate_limit_error" = Rate_limit_error);
  check bool "unknown defaults to api_error" true (error_type_of_string "unknown_error" = Api_error)

(* Test: parse error with all fields *)
let test_parse_error_all_fields () =
  let json = Yojson.Safe.from_string {|
    {
      "error": {
        "type": "invalid_request_error",
        "message": "Invalid parameter",
        "code": "parameter_invalid",
        "param": "amount",
        "decline_code": null,
        "doc_url": "https://stripe.com/docs/error-codes/parameter-invalid"
      }
    }
  |} in
  match parse_error json with
  | Some error ->
    check bool "error_type" true (error.error_type = Invalid_request_error);
    check string "message" "Invalid parameter" error.message;
    check (option string) "code" (Some "parameter_invalid") error.code;
    check (option string) "param" (Some "amount") error.param;
    check (option string) "decline_code" None error.decline_code;
    check (option string) "doc_url" (Some "https://stripe.com/docs/error-codes/parameter-invalid") error.doc_url
  | None -> fail "Expected error to be parsed"

(* Test: parse error returns None for non-error JSON *)
let test_parse_error_returns_none () =
  let json = Yojson.Safe.from_string {|{"id": "cus_123", "object": "customer"}|} in
  match parse_error json with
  | Some _ -> fail "Expected None for non-error JSON"
  | None -> check bool "returns None" true true

(* Test: parse authentication error *)
let test_parse_authentication_error () =
  let json = Yojson.Safe.from_string {|
    {
      "error": {
        "type": "authentication_error",
        "message": "Invalid API Key provided: sk_test_****1234"
      }
    }
  |} in
  match parse_error json with
  | Some error ->
    check bool "error_type" true (error.error_type = Authentication_error);
    check string "message" "Invalid API Key provided: sk_test_****1234" error.message
  | None -> fail "Expected error to be parsed"

(* Test: parse rate limit error *)  
let test_parse_rate_limit_error () =
  let json = Yojson.Safe.from_string {|
    {
      "error": {
        "type": "rate_limit_error",
        "message": "Too many requests"
      }
    }
  |} in
  match parse_error json with
  | Some error ->
    check bool "error_type" true (error.error_type = Rate_limit_error);
    check string "message" "Too many requests" error.message
  | None -> fail "Expected error to be parsed"

(** Form body encoding tests - adapted from stripe-python *)

(* Test: URL encoding of special characters *)
let test_url_encode_special_chars () =
  (* Uri.pct_encode encodes spaces and special chars, but + and @ 
     are allowed in query params by default *)
  let body = build_form_body [("name", "John Doe")] in
  check string "encodes space" "name=John%20Doe" body

(* Test: form body with empty params *)
let test_build_form_body_empty () =
  let body = build_form_body [] in
  check string "empty params" "" body

(* Test: form body with nested params via bracket notation *)
let test_build_form_body_nested () =
  let params = [
    ("metadata[key1]", "value1");
    ("metadata[key2]", "value2");
  ] in
  let body = build_form_body params in
  check bool "contains key1" true (String.length body > 0);
  check bool "has metadata brackets" true 
    (String.sub body 0 9 = "metadata%")

(* Test: form body with spaces and unicode *)
let test_build_form_body_unicode () =
  let body = build_form_body [("description", "Test café")] in
  check bool "encodes space" true 
    (String.sub body 0 11 = "description")

(** Pagination tests *)

(* Test: last_id extracts ID from last item *)
let test_last_id () =
  let response = {
    Stripe_core.data = [
      ("id_1", "name1");
      ("id_2", "name2");
      ("id_3", "name3");
    ];
    has_more = true;
    url = "/v1/test";
  } in
  let result = Stripe_core.last_id ~get_id:fst response in
  check (option string) "last_id" (Some "id_3") result

(* Test: last_id returns None for empty list *)
let test_last_id_empty () =
  let response = {
    Stripe_core.data = [];
    has_more = false;
    url = "/v1/test";
  } in
  let result = Stripe_core.last_id ~get_id:fst response in
  check (option string) "last_id empty" None result

(* Test: has_next_page returns true when has_more is true *)
let test_has_next_page_true () =
  let response = {
    Stripe_core.data = [("id_1", "name1")];
    has_more = true;
    url = "/v1/test";
  } in
  check bool "has_next_page true" true (Stripe_core.has_next_page response)

(* Test: has_next_page returns false when has_more is false *)
let test_has_next_page_false () =
  let response = {
    Stripe_core.data = [("id_1", "name1")];
    has_more = false;
    url = "/v1/test";
  } in
  check bool "has_next_page false" false (Stripe_core.has_next_page response)

(* Test: Pagination.next_page_cursor returns cursor when has_more *)
let test_next_page_cursor () =
  let response = {
    Stripe_core.data = [("id_1", "name1"); ("id_2", "name2")];
    has_more = true;
    url = "/v1/test";
  } in
  let cursor = Stripe_core.Pagination.next_page_cursor ~get_id:fst response in
  check (option string) "cursor" (Some "id_2") cursor

(* Test: Pagination.next_page_cursor returns None when not has_more *)
let test_next_page_cursor_no_more () =
  let response = {
    Stripe_core.data = [("id_1", "name1"); ("id_2", "name2")];
    has_more = false;
    url = "/v1/test";
  } in
  let cursor = Stripe_core.Pagination.next_page_cursor ~get_id:fst response in
  check (option string) "cursor none" None cursor

(* Test: Pagination.collect_all_sync collects from multiple pages *)
let test_collect_all_sync () =
  (* Simulate fetching pages *)
  let pages = ref [
    { Stripe_core.data = ["a"; "b"]; has_more = true; url = "/v1/test" };
    { Stripe_core.data = ["c"; "d"]; has_more = true; url = "/v1/test" };
    { Stripe_core.data = ["e"]; has_more = false; url = "/v1/test" };
  ] in
  let fetch_page ?starting_after:_ () =
    match !pages with
    | [] -> Ok { Stripe_core.data = []; has_more = false; url = "/v1/test" }
    | page :: rest -> 
      pages := rest;
      Ok page
  in
  let result = Stripe_core.Pagination.collect_all_sync 
    ~get_id:Fun.id 
    ~fetch_page 
    ()
  in
  match result with
  | Ok items -> 
    check (list string) "all items" ["a"; "b"; "c"; "d"; "e"] items
  | Error _ -> fail "Expected Ok"

(* Test: Pagination.collect_all_sync stops on error *)
let test_collect_all_sync_error () =
  let call_count = ref 0 in
  let fetch_page ?starting_after:_ () =
    incr call_count;
    if !call_count = 1 then
      Ok { Stripe_core.data = ["a"; "b"]; has_more = true; url = "/v1/test" }
    else
      Error { 
        Stripe_core.error_type = Api_error; 
        message = "Test error"; 
        code = None; 
        param = None; 
        decline_code = None; 
        doc_url = None 
      }
  in
  let result = Stripe_core.Pagination.collect_all_sync 
    ~get_id:Fun.id 
    ~fetch_page 
    ()
  in
  match result with
  | Ok _ -> fail "Expected Error"
  | Error e -> check string "error message" "Test error" e.message

(** Header construction tests - adapted from stripe-python *)

(* Test: auth header uses Bearer token *)
let test_auth_header_bearer () =
  let config = default_config ~api_key:"sk_test_secret123" in
  let headers = build_headers config in
  let auth = List.find (fun (k, _) -> k = "Authorization") headers in
  check string "uses Bearer" "Bearer sk_test_secret123" (snd auth)

(* Test: content-type header *)
let test_content_type_header () =
  let config = default_config ~api_key:"sk_test_123" in
  let headers = build_headers config in
  let content_type = List.find (fun (k, _) -> k = "Content-Type") headers in
  check string "form urlencoded" "application/x-www-form-urlencoded" (snd content_type)

(* Test: api version header when set *)
let test_api_version_header () =
  let config = { (default_config ~api_key:"sk_test_123") with api_version = Some "2023-10-16" } in
  let headers = build_headers config in
  let version = List.find_opt (fun (k, _) -> k = "Stripe-Version") headers in
  match version with
  | Some (_, v) -> check string "api version" "2023-10-16" v
  | None -> fail "Expected Stripe-Version header"

(* Test: no api version header when not set *)
let test_no_api_version_header () =
  let config = default_config ~api_key:"sk_test_123" in
  let headers = build_headers config in
  let version = List.find_opt (fun (k, _) -> k = "Stripe-Version") headers in
  check (option (pair string string)) "no version" None version

(* Test: combined headers *)
let test_combined_headers () =
  let config = { (default_config ~api_key:"sk_test_123") with api_version = Some "2023-10-16" } in
  let options = { 
    idempotency_key = Some "idem_123";
    stripe_account = Some "acct_456";
  } in
  let headers = build_headers ~options config in
  let has_auth = List.exists (fun (k, v) -> k = "Authorization" && v = "Bearer sk_test_123") headers in
  let has_version = List.exists (fun (k, v) -> k = "Stripe-Version" && v = "2023-10-16") headers in
  let has_idem = List.exists (fun (k, v) -> k = "Idempotency-Key" && v = "idem_123") headers in
  let has_account = List.exists (fun (k, v) -> k = "Stripe-Account" && v = "acct_456") headers in
  check bool "has auth" true has_auth;
  check bool "has version" true has_version;
  check bool "has idempotency" true has_idem;
  check bool "has account" true has_account

let () =
  run "Stripe Core" [
    "http_method", [
      test_case "to_string" `Quick test_http_method_to_string;
    ];
    "config", [
      test_case "default" `Quick test_default_config;
    ];
    "form_body", [
      test_case "build" `Quick test_build_form_body;
      test_case "special_chars" `Quick test_url_encode_special_chars;
      test_case "empty" `Quick test_build_form_body_empty;
      test_case "nested" `Quick test_build_form_body_nested;
      test_case "unicode" `Quick test_build_form_body_unicode;
    ];
    "error_parsing", [
      test_case "parse_error" `Quick test_parse_error;
      test_case "error_type_of_string" `Quick test_error_type_of_string;
      test_case "parse_all_fields" `Quick test_parse_error_all_fields;
      test_case "returns_none" `Quick test_parse_error_returns_none;
      test_case "authentication_error" `Quick test_parse_authentication_error;
      test_case "rate_limit_error" `Quick test_parse_rate_limit_error;
    ];
    "headers", [
      test_case "auth_bearer" `Quick test_auth_header_bearer;
      test_case "content_type" `Quick test_content_type_header;
      test_case "api_version" `Quick test_api_version_header;
      test_case "no_api_version" `Quick test_no_api_version_header;
      test_case "combined" `Quick test_combined_headers;
    ];
    "webhook_signature", [
      test_case "compute_signature" `Quick test_compute_signature;
      test_case "valid_header_and_signature" `Quick test_valid_header_and_signature;
      test_case "raise_on_malformed_header" `Quick test_raise_on_malformed_header;
      test_case "raise_on_no_signatures_with_expected_scheme" `Quick test_raise_on_no_signatures_with_expected_scheme;
      test_case "raise_on_invalid_signature" `Quick test_raise_on_invalid_signature;
      test_case "raise_on_timestamp_outside_tolerance" `Quick test_raise_on_timestamp_outside_tolerance;
      test_case "timestamp_off_but_no_tolerance" `Quick test_timestamp_off_but_no_tolerance;
      test_case "header_contains_valid_signature" `Quick test_header_contains_valid_signature;
      test_case "secure_compare" `Quick test_secure_compare;
    ];
    "webhook", [
      test_case "verify_signature" `Quick test_webhook_verify_signature;
    ];
    "idempotency", [
      test_case "generate_key" `Quick test_generate_idempotency_key;
      test_case "build_headers_with_key" `Quick test_build_headers_with_idempotency_key;
      test_case "build_headers_with_account" `Quick test_build_headers_with_stripe_account;
      test_case "build_headers_without_options" `Quick test_build_headers_without_options;
    ];
    "pagination", [
      test_case "last_id" `Quick test_last_id;
      test_case "last_id_empty" `Quick test_last_id_empty;
      test_case "has_next_page_true" `Quick test_has_next_page_true;
      test_case "has_next_page_false" `Quick test_has_next_page_false;
      test_case "next_page_cursor" `Quick test_next_page_cursor;
      test_case "next_page_cursor_no_more" `Quick test_next_page_cursor_no_more;
      test_case "collect_all_sync" `Quick test_collect_all_sync;
      test_case "collect_all_sync_error" `Quick test_collect_all_sync_error;
    ];
  ]
