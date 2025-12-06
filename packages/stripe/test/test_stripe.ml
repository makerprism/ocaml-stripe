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

(** Test PaymentMethod parsing *)
let test_payment_method_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "pm_123",
      "object": "payment_method",
      "billing_details": {"address": null, "email": null, "name": null, "phone": null},
      "card": {
        "brand": "visa",
        "exp_month": 12,
        "exp_year": 2025,
        "last4": "4242"
      },
      "created": 1694725031,
      "customer": "cus_123",
      "livemode": false,
      "type": "card"
    }
  |} in
  let pm = Payment_method.of_json json in
  check string "id" "pm_123" pm.id;
  check string "object" "payment_method" pm.object_;
  check string "type" "card" pm.type_;
  check (option string) "customer" (Some "cus_123") pm.customer;
  match pm.card with
  | Some card ->
    check string "card brand" "visa" card.brand;
    check string "card last4" "4242" card.last4;
    check int "card exp_month" 12 card.exp_month;
    check int "card exp_year" 2025 card.exp_year
  | None -> fail "Expected card to be present"

(** Test SetupIntent parsing *)
let test_setup_intent_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "seti_123",
      "object": "setup_intent",
      "client_secret": "seti_123_secret_456",
      "created": 1694725031,
      "customer": "cus_123",
      "description": "Setup for subscription",
      "livemode": false,
      "payment_method": "pm_123",
      "status": "succeeded",
      "usage": "off_session"
    }
  |} in
  let si = Setup_intent.of_json json in
  check string "id" "seti_123" si.id;
  check string "object" "setup_intent" si.object_;
  check (option string) "client_secret" (Some "seti_123_secret_456") si.client_secret;
  check (option string) "customer" (Some "cus_123") si.customer;
  check bool "status is Succeeded" true (si.status = Setup_intent.Succeeded)

(** Test Coupon parsing *)
let test_coupon_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "coupon_123",
      "object": "coupon",
      "amount_off": null,
      "created": 1694725031,
      "currency": null,
      "duration": "repeating",
      "duration_in_months": 3,
      "livemode": false,
      "max_redemptions": 100,
      "name": "25% off",
      "percent_off": 25.0,
      "times_redeemed": 10,
      "valid": true
    }
  |} in
  let coupon = Coupon.of_json json in
  check string "id" "coupon_123" coupon.id;
  check string "object" "coupon" coupon.object_;
  check bool "duration is Repeating" true (coupon.duration = Coupon.Repeating);
  check (option int) "duration_in_months" (Some 3) coupon.duration_in_months;
  check (option string) "name" (Some "25% off") coupon.name;
  check bool "valid" true coupon.valid;
  check int "times_redeemed" 10 coupon.times_redeemed

(** Test BalanceTransaction parsing *)
let test_balance_transaction_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "txn_123",
      "object": "balance_transaction",
      "amount": 1000,
      "available_on": 1694725031,
      "created": 1694725031,
      "currency": "usd",
      "description": "Charge for order",
      "fee": 59,
      "net": 941,
      "source": "ch_123",
      "status": "available",
      "type": "charge"
    }
  |} in
  let txn = Balance_transaction.of_json json in
  check string "id" "txn_123" txn.id;
  check string "object" "balance_transaction" txn.object_;
  check int "amount" 1000 txn.amount;
  check int "fee" 59 txn.fee;
  check int "net" 941 txn.net;
  check string "status" "available" txn.status;
  check string "type" "charge" txn.type_

(** Test Payout parsing *)
let test_payout_parsing () =
  let json = Yojson.Safe.from_string {|
    {
      "id": "po_123",
      "object": "payout",
      "amount": 10000,
      "arrival_date": 1694725031,
      "created": 1694725031,
      "currency": "usd",
      "description": "STRIPE PAYOUT",
      "destination": "ba_123",
      "livemode": false,
      "method": "standard",
      "status": "paid",
      "type": "bank_account"
    }
  |} in
  let payout = Payout.of_json json in
  check string "id" "po_123" payout.id;
  check string "object" "payout" payout.object_;
  check int "amount" 10000 payout.amount;
  check string "status" "paid" payout.status;
  check string "type" "bank_account" payout.type_

let test_checkout_session_parsing () =
  let open Stripe.Checkout_session in
  let json = Yojson.Safe.from_string {|
    {
      "id": "cs_test_123",
      "object": "checkout.session",
      "cancel_url": "https://example.com/cancel",
      "client_reference_id": null,
      "currency": "usd",
      "customer": "cus_123",
      "customer_email": "test@example.com",
      "livemode": false,
      "mode": "payment",
      "payment_intent": "pi_123",
      "payment_status": "unpaid",
      "status": "open",
      "success_url": "https://example.com/success",
      "url": "https://checkout.stripe.com/pay/cs_test_123"
    }
  |} in
  let session = of_json json in
  check string "id" "cs_test_123" session.id;
  check string "object" "checkout.session" session.object_;
  check string "mode" "payment" session.mode;
  check string "payment_status" "unpaid" session.payment_status;
  check (option string) "customer" (Some "cus_123") session.customer

let test_tax_rate_parsing () =
  let open Stripe.Tax_rate in
  let json = Yojson.Safe.from_string {|
    {
      "id": "txr_123",
      "object": "tax_rate",
      "active": true,
      "country": "US",
      "description": "Sales Tax",
      "display_name": "Sales Tax",
      "inclusive": false,
      "jurisdiction": "California",
      "percentage": 8.25,
      "state": "CA",
      "tax_type": "sales_tax"
    }
  |} in
  let tax_rate = of_json json in
  check string "id" "txr_123" tax_rate.id;
  check string "object" "tax_rate" tax_rate.object_;
  check bool "active" true tax_rate.active;
  check string "display_name" "Sales Tax" tax_rate.display_name;
  check bool "inclusive" false tax_rate.inclusive;
  check (float 0.01) "percentage" 8.25 tax_rate.percentage

let test_payment_link_parsing () =
  let open Stripe.Payment_link in
  let json = Yojson.Safe.from_string {|
    {
      "id": "plink_123",
      "object": "payment_link",
      "active": true,
      "currency": "usd",
      "livemode": false,
      "url": "https://buy.stripe.com/test_123"
    }
  |} in
  let plink = of_json json in
  check string "id" "plink_123" plink.id;
  check string "object" "payment_link" plink.object_;
  check bool "active" true plink.active;
  check string "url" "https://buy.stripe.com/test_123" plink.url

let test_dispute_parsing () =
  let open Stripe.Dispute in
  let json = Yojson.Safe.from_string {|
    {
      "id": "dp_123",
      "object": "dispute",
      "amount": 1000,
      "charge": "ch_123",
      "currency": "usd",
      "created": 1234567890,
      "is_charge_refundable": false,
      "livemode": false,
      "payment_intent": "pi_123",
      "reason": "fraudulent",
      "status": "needs_response"
    }
  |} in
  let dispute = of_json json in
  check string "id" "dp_123" dispute.id;
  check string "object" "dispute" dispute.object_;
  check int "amount" 1000 dispute.amount;
  check string "charge" "ch_123" dispute.charge;
  check string "reason" "fraudulent" dispute.reason;
  check string "status" "needs_response" dispute.status

let test_account_parsing () =
  let open Stripe.Account in
  let json = Yojson.Safe.from_string {|
    {
      "id": "acct_123",
      "object": "account",
      "business_type": "company",
      "charges_enabled": true,
      "country": "US",
      "created": 1234567890,
      "default_currency": "usd",
      "details_submitted": true,
      "email": "test@example.com",
      "payouts_enabled": true,
      "type": "standard"
    }
  |} in
  let account = of_json json in
  check string "id" "acct_123" account.id;
  check string "object" "account" account.object_;
  check bool "charges_enabled" true account.charges_enabled;
  check string "country" "US" account.country;
  check bool "payouts_enabled" true account.payouts_enabled

let test_transfer_parsing () =
  let open Stripe.Transfer in
  let json = Yojson.Safe.from_string {|
    {
      "id": "tr_123",
      "object": "transfer",
      "amount": 1000,
      "amount_reversed": 0,
      "created": 1234567890,
      "currency": "usd",
      "description": "Test transfer",
      "destination": "acct_456",
      "livemode": false,
      "reversed": false,
      "source_transaction": "ch_123",
      "source_type": "card",
      "transfer_group": "ORDER_123"
    }
  |} in
  let transfer = of_json json in
  check string "id" "tr_123" transfer.id;
  check string "object" "transfer" transfer.object_;
  check int "amount" 1000 transfer.amount;
  check string "destination" "acct_456" transfer.destination;
  check string "source_type" "card" transfer.source_type

let test_file_parsing () =
  let open Stripe.File in
  let json = Yojson.Safe.from_string {|
    {
      "id": "file_123",
      "object": "file",
      "created": 1234567890,
      "expires_at": 1234657890,
      "filename": "document.pdf",
      "purpose": "dispute_evidence",
      "size": 12345,
      "type": "pdf",
      "url": "https://files.stripe.com/v1/files/file_123/contents"
    }
  |} in
  let file = of_json json in
  check string "id" "file_123" file.id;
  check string "object" "file" file.object_;
  check string "purpose" "dispute_evidence" file.purpose;
  check int "size" 12345 file.size

let test_file_link_parsing () =
  let open Stripe.File_link in
  let json = Yojson.Safe.from_string {|
    {
      "id": "link_123",
      "object": "file_link",
      "created": 1234567890,
      "expired": false,
      "expires_at": 1234657890,
      "file": "file_123",
      "livemode": false,
      "url": "https://files.stripe.com/links/link_123"
    }
  |} in
  let link = of_json json in
  check string "id" "link_123" link.id;
  check string "object" "file_link" link.object_;
  check bool "expired" false link.expired;
  check string "file" "file_123" link.file

let test_mandate_parsing () =
  let open Stripe.Mandate in
  let json = Yojson.Safe.from_string {|
    {
      "id": "mandate_123",
      "object": "mandate",
      "livemode": false,
      "payment_method": "pm_123",
      "status": "active",
      "type": "multi_use"
    }
  |} in
  let mandate = of_json json in
  check string "id" "mandate_123" mandate.id;
  check string "object" "mandate" mandate.object_;
  check string "payment_method" "pm_123" mandate.payment_method;
  check string "status" "active" mandate.status;
  check string "type" "multi_use" mandate.type_

let test_review_parsing () =
  let open Stripe.Review in
  let json = Yojson.Safe.from_string {|
    {
      "id": "prv_123",
      "object": "review",
      "charge": "ch_123",
      "created": 1234567890,
      "livemode": false,
      "open": true,
      "payment_intent": "pi_123",
      "reason": "rule"
    }
  |} in
  let review = of_json json in
  check string "id" "prv_123" review.id;
  check string "object" "review" review.object_;
  check bool "open" true review.open_;
  check string "reason" "rule" review.reason

let test_promotion_code_parsing () =
  let open Stripe.Promotion_code in
  let json = Yojson.Safe.from_string {|
    {
      "id": "promo_123",
      "object": "promotion_code",
      "active": true,
      "code": "SUMMER20",
      "coupon": {
        "id": "coupon_123",
        "object": "coupon",
        "amount_off": null,
        "created": 1234567890,
        "currency": null,
        "duration": "once",
        "duration_in_months": null,
        "livemode": false,
        "max_redemptions": null,
        "name": "Summer Sale",
        "percent_off": 20.0,
        "times_redeemed": 5,
        "valid": true
      },
      "created": 1234567890,
      "customer": null,
      "expires_at": null,
      "livemode": false,
      "max_redemptions": 100,
      "times_redeemed": 10
    }
  |} in
  let promo = of_json json in
  check string "id" "promo_123" promo.id;
  check string "object" "promotion_code" promo.object_;
  check bool "active" true promo.active;
  check string "code" "SUMMER20" promo.code;
  check int "times_redeemed" 10 promo.times_redeemed

let test_invoice_item_parsing () =
  let open Stripe.Invoice_item in
  let json = Yojson.Safe.from_string {|
    {
      "id": "ii_123",
      "object": "invoiceitem",
      "amount": 1000,
      "currency": "usd",
      "customer": "cus_123",
      "date": 1234567890,
      "description": "Test item",
      "discountable": true,
      "invoice": "in_123",
      "livemode": false,
      "period": {
        "start": 1234567890,
        "end": 1237246290
      },
      "price": null,
      "proration": false,
      "quantity": 1,
      "subscription": null,
      "unit_amount": 1000
    }
  |} in
  let item = of_json json in
  check string "id" "ii_123" item.id;
  check string "object" "invoiceitem" item.object_;
  check int "amount" 1000 item.amount;
  check string "customer" "cus_123" item.customer;
  check bool "discountable" true item.discountable

let test_quote_parsing () =
  let open Stripe.Quote in
  let json = Yojson.Safe.from_string {|
    {
      "id": "qt_123",
      "object": "quote",
      "amount_subtotal": 1000,
      "amount_total": 1100,
      "created": 1234567890,
      "currency": "usd",
      "customer": "cus_123",
      "description": "Project quote",
      "expires_at": 1237246290,
      "livemode": false,
      "status": "draft",
      "subscription": null
    }
  |} in
  let quote = of_json json in
  check string "id" "qt_123" quote.id;
  check string "object" "quote" quote.object_;
  check int "amount_total" 1100 quote.amount_total;
  check string "status" "draft" quote.status

let test_credit_note_parsing () =
  let open Stripe.Credit_note in
  let json = Yojson.Safe.from_string {|
    {
      "id": "cn_123",
      "object": "credit_note",
      "amount": 500,
      "created": 1234567890,
      "currency": "usd",
      "customer": "cus_123",
      "invoice": "in_123",
      "livemode": false,
      "memo": "Refund for defective item",
      "number": "CN-0001",
      "out_of_band_amount": null,
      "reason": "product_unsatisfactory",
      "refund": "re_123",
      "status": "issued",
      "subtotal": 500,
      "total": 500,
      "type": "pre_payment"
    }
  |} in
  let cn = of_json json in
  check string "id" "cn_123" cn.id;
  check string "object" "credit_note" cn.object_;
  check int "amount" 500 cn.amount;
  check string "status" "issued" cn.status;
  check string "number" "CN-0001" cn.number

let test_application_fee_parsing () =
  let open Stripe.Application_fee in
  let json = Yojson.Safe.from_string {|
    {
      "id": "fee_123",
      "object": "application_fee",
      "account": "acct_123",
      "amount": 100,
      "amount_refunded": 0,
      "application": "ca_123",
      "charge": "ch_123",
      "created": 1234567890,
      "currency": "usd",
      "livemode": false,
      "refunded": false
    }
  |} in
  let fee = of_json json in
  check string "id" "fee_123" fee.id;
  check string "object" "application_fee" fee.object_;
  check int "amount" 100 fee.amount;
  check string "account" "acct_123" fee.account

let test_topup_parsing () =
  let open Stripe.Topup in
  let json = Yojson.Safe.from_string {|
    {
      "id": "tu_123",
      "object": "topup",
      "amount": 10000,
      "created": 1234567890,
      "currency": "usd",
      "description": "Top up balance",
      "livemode": false,
      "source": "src_123",
      "status": "succeeded"
    }
  |} in
  let topup = of_json json in
  check string "id" "tu_123" topup.id;
  check string "object" "topup" topup.object_;
  check int "amount" 10000 topup.amount;
  check string "status" "succeeded" topup.status

let test_subscription_item_parsing () =
  let open Stripe.Subscription_item in
  let json = Yojson.Safe.from_string {|
    {
      "id": "si_123",
      "object": "subscription_item",
      "created": 1234567890,
      "price": {
        "id": "price_123",
        "object": "price",
        "active": true,
        "billing_scheme": "per_unit",
        "created": 1234567890,
        "currency": "usd",
        "livemode": false,
        "product": "prod_123",
        "recurring": null,
        "type": "recurring",
        "unit_amount": 2000
      },
      "quantity": 2,
      "subscription": "sub_123"
    }
  |} in
  let si = of_json json in
  check string "id" "si_123" si.id;
  check string "object" "subscription_item" si.object_;
  check string "subscription" "sub_123" si.subscription;
  check (option int) "quantity" (Some 2) si.quantity

let test_subscription_schedule_parsing () =
  let open Stripe.Subscription_schedule in
  let json = Yojson.Safe.from_string {|
    {
      "id": "sub_sched_123",
      "object": "subscription_schedule",
      "canceled_at": null,
      "completed_at": null,
      "created": 1234567890,
      "customer": "cus_123",
      "end_behavior": "release",
      "livemode": false,
      "status": "active",
      "subscription": "sub_123"
    }
  |} in
  let ss = of_json json in
  check string "id" "sub_sched_123" ss.id;
  check string "object" "subscription_schedule" ss.object_;
  check string "customer" "cus_123" ss.customer;
  check string "status" "active" ss.status;
  check string "end_behavior" "release" ss.end_behavior

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
    "PaymentMethod", [
      test_case "parsing" `Quick test_payment_method_parsing;
    ];
    "SetupIntent", [
      test_case "parsing" `Quick test_setup_intent_parsing;
    ];
    "Coupon", [
      test_case "parsing" `Quick test_coupon_parsing;
    ];
    "BalanceTransaction", [
      test_case "parsing" `Quick test_balance_transaction_parsing;
    ];
    "Payout", [
      test_case "parsing" `Quick test_payout_parsing;
    ];
    "CheckoutSession", [
      test_case "parsing" `Quick test_checkout_session_parsing;
    ];
    "TaxRate", [
      test_case "parsing" `Quick test_tax_rate_parsing;
    ];
    "PaymentLink", [
      test_case "parsing" `Quick test_payment_link_parsing;
    ];
    "Dispute", [
      test_case "parsing" `Quick test_dispute_parsing;
    ];
    "Account", [
      test_case "parsing" `Quick test_account_parsing;
    ];
    "Transfer", [
      test_case "parsing" `Quick test_transfer_parsing;
    ];
    "File", [
      test_case "parsing" `Quick test_file_parsing;
    ];
    "FileLink", [
      test_case "parsing" `Quick test_file_link_parsing;
    ];
    "Mandate", [
      test_case "parsing" `Quick test_mandate_parsing;
    ];
    "Review", [
      test_case "parsing" `Quick test_review_parsing;
    ];
    "PromotionCode", [
      test_case "parsing" `Quick test_promotion_code_parsing;
    ];
    "InvoiceItem", [
      test_case "parsing" `Quick test_invoice_item_parsing;
    ];
    "Quote", [
      test_case "parsing" `Quick test_quote_parsing;
    ];
    "CreditNote", [
      test_case "parsing" `Quick test_credit_note_parsing;
    ];
    "ApplicationFee", [
      test_case "parsing" `Quick test_application_fee_parsing;
    ];
    "Topup", [
      test_case "parsing" `Quick test_topup_parsing;
    ];
    "SubscriptionItem", [
      test_case "parsing" `Quick test_subscription_item_parsing;
    ];
    "SubscriptionSchedule", [
      test_case "parsing" `Quick test_subscription_schedule_parsing;
    ];
  ]
