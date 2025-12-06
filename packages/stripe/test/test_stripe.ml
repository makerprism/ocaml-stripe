(** Tests for stripe package - adapted from stripe-python test suite *)

open Alcotest
open Stripe

(** Test fixtures - adapted from stripe-python/tests/test_stripe_object.py *)

let sample_customer = {|
{
  "id": "cus_123",
  "object": "customer",
  "address": null,
  "balance": 0,
  "created": 1694725031,
  "currency": "usd",
  "default_source": null,
  "delinquent": false,
  "description": "Test customer",
  "email": "test@example.com",
  "invoice_prefix": "ABC123",
  "livemode": false,
  "name": "Test User",
  "phone": "+1234567890"
}
|}

let sample_charge = {|
{
  "id": "ch_123",
  "object": "charge",
  "amount": 2000,
  "amount_captured": 2000,
  "amount_refunded": 0,
  "captured": true,
  "created": 1694725031,
  "currency": "usd",
  "customer": "cus_123",
  "description": "Test charge",
  "failure_code": null,
  "failure_message": null,
  "livemode": false,
  "paid": true,
  "payment_intent": "pi_123",
  "refunded": false,
  "status": "succeeded"
}
|}

let sample_payment_intent = {|
{
  "id": "pi_123",
  "object": "payment_intent",
  "amount": 2000,
  "amount_received": 2000,
  "capture_method": "automatic",
  "client_secret": "pi_123_secret_456",
  "confirmation_method": "automatic",
  "created": 1694725031,
  "currency": "usd",
  "customer": "cus_123",
  "description": "Test payment",
  "livemode": false,
  "payment_method": "pm_123",
  "status": "succeeded"
}
|}

let sample_invoice = {|
{
  "id": "in_123",
  "object": "invoice",
  "amount_due": 2000,
  "amount_paid": 2000,
  "amount_remaining": 0,
  "created": 1694725031,
  "currency": "usd",
  "customer": "cus_123",
  "livemode": false,
  "status": "paid",
  "subscription": "sub_123",
  "total": 2000
}
|}

let sample_list_response = {|
{
  "object": "list",
  "data": [
    {
      "id": "cus_123",
      "object": "customer",
      "address": null,
      "balance": 0,
      "created": 1694725031,
      "currency": "usd",
      "default_source": null,
      "delinquent": false,
      "description": "Test customer",
      "email": "test@example.com",
      "invoice_prefix": "ABC123",
      "livemode": false,
      "name": "Test User",
      "phone": null
    }
  ],
  "has_more": false,
  "url": "/v1/customers"
}
|}

let sample_error_response = {|
{
  "error": {
    "type": "card_error",
    "message": "Your card was declined.",
    "code": "card_declined",
    "param": null,
    "decline_code": "generic_decline",
    "doc_url": "https://stripe.com/docs/error-codes/card-declined"
  }
}
|}

let sample_deleted_response = {|
{
  "id": "cus_123",
  "object": "customer",
  "deleted": true
}
|}

(** Test Customer parsing - adapted from test_stripe_object.py *)
let test_customer_parsing () =
  let json = Yojson.Safe.from_string sample_customer in
  let customer = Customer.of_json json in
  check string "id" "cus_123" customer.id;
  check string "object" "customer" customer.object_;
  check (option string) "email" (Some "test@example.com") customer.email;
  check (option string) "name" (Some "Test User") customer.name;
  check (option string) "description" (Some "Test customer") customer.description;
  check int "balance" 0 customer.balance;
  check bool "livemode" false customer.livemode

(** Test Charge parsing *)
let test_charge_parsing () =
  let json = Yojson.Safe.from_string sample_charge in
  let charge = Charge.of_json json in
  check string "id" "ch_123" charge.id;
  check string "object" "charge" charge.object_;
  check int "amount" 2000 charge.amount;
  check string "currency" "usd" charge.currency;
  check bool "captured" true charge.captured;
  check bool "paid" true charge.paid;
  check bool "refunded" false charge.refunded;
  check (option string) "customer" (Some "cus_123") charge.customer;
  check bool "status is Succeeded" true (charge.status = Charge.Succeeded)

(** Test PaymentIntent parsing *)
let test_payment_intent_parsing () =
  let json = Yojson.Safe.from_string sample_payment_intent in
  let pi = Payment_intent.of_json json in
  check string "id" "pi_123" pi.id;
  check string "object" "payment_intent" pi.object_;
  check int "amount" 2000 pi.amount;
  check int "amount_received" 2000 pi.amount_received;
  check string "currency" "usd" pi.currency;
  check (option string) "client_secret" (Some "pi_123_secret_456") pi.client_secret;
  check bool "status is Succeeded" true (pi.status = Payment_intent.Succeeded)

(** Test Invoice parsing *)
let test_invoice_parsing () =
  let json = Yojson.Safe.from_string sample_invoice in
  let invoice = Invoice.of_json json in
  check string "id" "in_123" invoice.id;
  check string "object" "invoice" invoice.object_;
  check int "amount_due" 2000 invoice.amount_due;
  check int "amount_paid" 2000 invoice.amount_paid;
  check int "amount_remaining" 0 invoice.amount_remaining;
  check int "total" 2000 invoice.total;
  check (option string) "subscription" (Some "sub_123") invoice.subscription;
  check bool "status is Paid" true (invoice.status = Some Invoice.Paid)

(** Test List response parsing *)
let test_list_response_parsing () =
  let json = Yojson.Safe.from_string sample_list_response in
  let list_resp = List_response.of_json Customer.of_json json in
  check int "data length" 1 (List.length list_resp.data);
  check bool "has_more" false list_resp.has_more;
  check string "url" "/v1/customers" list_resp.url;
  let first_customer = List.hd list_resp.data in
  check string "first customer id" "cus_123" first_customer.id

(** Test error parsing - adapted from test_api_requestor.py *)
let test_error_parsing () =
  let json = Yojson.Safe.from_string sample_error_response in
  match Core.parse_error json with
  | Some error ->
    check bool "error_type is Card_error" true (error.error_type = Core.Card_error);
    check string "message" "Your card was declined." error.message;
    check (option string) "code" (Some "card_declined") error.code;
    check (option string) "decline_code" (Some "generic_decline") error.decline_code;
    check (option string) "doc_url" (Some "https://stripe.com/docs/error-codes/card-declined") error.doc_url
  | None -> fail "Expected error to be parsed"

(** Test Deleted response parsing *)
let test_deleted_parsing () =
  let json = Yojson.Safe.from_string sample_deleted_response in
  let deleted = Deleted.of_json json in
  check string "id" "cus_123" deleted.id;
  check string "object" "customer" deleted.object_;
  check bool "deleted" true deleted.deleted

(** Test config defaults *)
let test_config_defaults () =
  let config = default_config ~api_key:"sk_test_123" in
  check string "api_key" "sk_test_123" config.api_key;
  check string "api_base" "https://api.stripe.com" config.api_base;
  check (option string) "api_version" None config.api_version;
  check int "max_network_retries" 0 config.max_network_retries

(** Test to_json roundtrip *)
let test_customer_to_json_roundtrip () =
  let json = Yojson.Safe.from_string sample_customer in
  let customer = Customer.of_json json in
  let json2 = Customer.to_json customer in
  check string "roundtrip" (Yojson.Safe.to_string json) (Yojson.Safe.to_string json2)

(** Test Address parsing *)
let test_address_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "city": "San Francisco",
      "country": "US",
      "line1": "123 Market St",
      "line2": "Suite 100",
      "postal_code": "94102",
      "state": "CA"
    }
  |} in
  let address = Address.of_json json in
  check (option string) "city" (Some "San Francisco") address.city;
  check (option string) "country" (Some "US") address.country;
  check (option string) "line1" (Some "123 Market St") address.line1;
  check (option string) "line2" (Some "Suite 100") address.line2;
  check (option string) "postal_code" (Some "94102") address.postal_code;
  check (option string) "state" (Some "CA") address.state

(** Test PaymentIntent status variants *)
let test_payment_intent_status_variants () =
  let test_status json_status expected =
    let json = Yojson.Safe.from_string (Printf.sprintf {|
      {
        "id": "pi_123",
        "object": "payment_intent",
        "amount": 2000,
        "created": 1694725031,
        "currency": "usd",
        "livemode": false,
        "status": "%s"
      }
    |} json_status) in
    let pi = Payment_intent.of_json json in
    check bool (Printf.sprintf "status %s" json_status) true (pi.status = expected)
  in
  test_status "requires_payment_method" Payment_intent.Requires_payment_method;
  test_status "requires_confirmation" Payment_intent.Requires_confirmation;
  test_status "requires_action" Payment_intent.Requires_action;
  test_status "processing" Payment_intent.Processing;
  test_status "requires_capture" Payment_intent.Requires_capture;
  test_status "canceled" Payment_intent.Canceled;
  test_status "succeeded" Payment_intent.Succeeded

(** Test Subscription status variants *)
let test_subscription_status_variants () =
  let test_status json_status expected =
    let json = Yojson.Safe.from_string (Printf.sprintf {|
      {
        "id": "sub_123",
        "object": "subscription",
        "cancel_at_period_end": false,
        "current_period_end": 1694725031,
        "current_period_start": 1694725031,
        "customer": "cus_123",
        "livemode": false,
        "status": "%s"
      }
    |} json_status) in
    let sub = Subscription.of_json json in
    check bool (Printf.sprintf "status %s" json_status) true (sub.status = expected)
  in
  test_status "active" Subscription.Active;
  test_status "past_due" Subscription.Past_due;
  test_status "unpaid" Subscription.Unpaid;
  test_status "canceled" Subscription.Canceled;
  test_status "incomplete" Subscription.Incomplete;
  test_status "incomplete_expired" Subscription.Incomplete_expired;
  test_status "trialing" Subscription.Trialing;
  test_status "paused" Subscription.Paused

(** Test error type variants *)
let test_error_type_variants () =
  check bool "api_error" true (Core.error_type_of_string "api_error" = Core.Api_error);
  check bool "card_error" true (Core.error_type_of_string "card_error" = Core.Card_error);
  check bool "idempotency_error" true (Core.error_type_of_string "idempotency_error" = Core.Idempotency_error);
  check bool "invalid_request_error" true (Core.error_type_of_string "invalid_request_error" = Core.Invalid_request_error);
  check bool "authentication_error" true (Core.error_type_of_string "authentication_error" = Core.Authentication_error);
  check bool "rate_limit_error" true (Core.error_type_of_string "rate_limit_error" = Core.Rate_limit_error);
  check bool "unknown defaults to Api_error" true (Core.error_type_of_string "unknown" = Core.Api_error)

let () =
  run "Stripe" [
    "Customer", [
      test_case "parsing" `Quick test_customer_parsing;
      test_case "to_json roundtrip" `Quick test_customer_to_json_roundtrip;
    ];
    "Charge", [
      test_case "parsing" `Quick test_charge_parsing;
    ];
    "PaymentIntent", [
      test_case "parsing" `Quick test_payment_intent_parsing;
      test_case "status variants" `Quick test_payment_intent_status_variants;
    ];
    "Invoice", [
      test_case "parsing" `Quick test_invoice_parsing;
    ];
    "List", [
      test_case "response parsing" `Quick test_list_response_parsing;
    ];
    "Error", [
      test_case "parsing" `Quick test_error_parsing;
      test_case "type variants" `Quick test_error_type_variants;
    ];
    "Deleted", [
      test_case "parsing" `Quick test_deleted_parsing;
    ];
    "Config", [
      test_case "defaults" `Quick test_config_defaults;
    ];
    "Address", [
      test_case "parsing" `Quick test_address_parsing;
    ];
    "Subscription", [
      test_case "status variants" `Quick test_subscription_status_variants;
    ];
  ]
