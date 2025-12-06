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
    ];
    "error_parsing", [
      test_case "parse_error" `Quick test_parse_error;
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
  ]
