(** Tests for stripe-core *)

open Alcotest
open Stripe_core

let test_http_method_to_string () =
  check string "GET" "GET" (string_of_http_method GET);
  check string "POST" "POST" (string_of_http_method POST);
  check string "DELETE" "DELETE" (string_of_http_method DELETE);
  check string "PUT" "PUT" (string_of_http_method PUT);
  check string "PATCH" "PATCH" (string_of_http_method PATCH)

let test_default_config () =
  let config = default_config ~api_key:"sk_test_123" in
  check string "api_key" "sk_test_123" config.api_key;
  check string "api_base" "https://api.stripe.com" config.api_base;
  check (option string) "api_version" None config.api_version;
  check int "max_network_retries" 0 config.max_network_retries

let test_build_form_body () =
  let params = [("amount", "2000"); ("currency", "usd")] in
  let body = build_form_body params in
  check string "form body" "amount=2000&currency=usd" body

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
  ]
