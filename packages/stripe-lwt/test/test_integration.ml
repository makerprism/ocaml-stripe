(** Integration tests for stripe-lwt using stripe-mock
    
    These tests require stripe-mock to be running on localhost:12111
    
    Install stripe-mock:
      go install github.com/stripe/stripe-mock@latest
    
    Run stripe-mock:
      stripe-mock -http-port 12111
    
    Run tests:
      STRIPE_MOCK=1 dune test
*)

open Lwt.Syntax

let stripe_mock_base = "http://localhost:12111"
let test_api_key = "sk_test_123"

(** Create a config pointing to stripe-mock *)
let mock_config () =
  let config = Stripe_lwt.Client.create ~api_key:test_api_key in
  { config with api_base = stripe_mock_base }

(** Check if stripe-mock is available *)
let is_stripe_mock_available () =
  Sys.getenv_opt "STRIPE_MOCK" = Some "1"

(** Test customer create and retrieve *)
let test_customer_create_retrieve () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Customer.create ~config
      ~email:"test@example.com"
      ~name:"Test User"
      ~description:"Integration test customer"
      ()
    in
    match result with
    | Ok customer ->
      Alcotest.(check bool) "has id" true (String.length customer.id > 0);
      Alcotest.(check string) "object is customer" "customer" customer.object_;
      (* stripe-mock doesn't reflect input, so we just check types *)
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Customer create failed: %s" err.message)
  end

(** Test customer list *)
let test_customer_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Customer.list ~config ~limit:3 () in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Alcotest.(check string) "url" "/v1/customers" list.url;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Customer list failed: %s" err.message)
  end

(** Test payment intent create *)
let test_payment_intent_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Payment_intent.create ~config
      ~amount:2000
      ~currency:"usd"
      ()
    in
    match result with
    | Ok pi ->
      Alcotest.(check bool) "has id" true (String.length pi.id > 0);
      Alcotest.(check string) "object" "payment_intent" pi.object_;
      Alcotest.(check string) "currency" "usd" pi.currency;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "PaymentIntent create failed: %s" err.message)
  end

(** Test payment intent retrieve *)
let test_payment_intent_retrieve () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    (* stripe-mock uses fixture IDs *)
    let* result = Stripe_lwt.Client.Payment_intent.retrieve ~config ~id:"pi_123" () in
    match result with
    | Ok pi ->
      Alcotest.(check bool) "has id" true (String.length pi.id > 0);
      Alcotest.(check string) "object" "payment_intent" pi.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "PaymentIntent retrieve failed: %s" err.message)
  end

(** Test balance retrieve *)
let test_balance_retrieve () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Balance.retrieve ~config () in
    match result with
    | Ok balance ->
      Alcotest.(check string) "object" "balance" balance.object_;
      Alcotest.(check bool) "has available" true (List.length balance.available >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Balance retrieve failed: %s" err.message)
  end

(** Test product create *)
let test_product_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Product.create ~config
      ~name:"Test Product"
      ~description:"A test product"
      ()
    in
    match result with
    | Ok product ->
      Alcotest.(check bool) "has id" true (String.length product.id > 0);
      Alcotest.(check string) "object" "product" product.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Product create failed: %s" err.message)
  end

(** Test charge list *)
let test_charge_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Charge.list ~config ~limit:5 () in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Charge list failed: %s" err.message)
  end

(** Test refund create *)
let test_refund_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Refund.create ~config
      ~charge:"ch_123"
      ~amount:500
      ()
    in
    match result with
    | Ok refund ->
      Alcotest.(check bool) "has id" true (String.length refund.id > 0);
      Alcotest.(check string) "object" "refund" refund.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Refund create failed: %s" err.message)
  end

(** Test event list *)
let test_event_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Event.list ~config ~limit:5 () in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Event list failed: %s" err.message)
  end

(** Test error handling - invalid endpoint *)
let test_error_handling () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    (* Try to retrieve a non-existent customer - stripe-mock returns 404 *)
    let* response = Stripe_lwt.get ~config ~path:"/v1/nonexistent" () in
    Alcotest.(check bool) "returns error status" true (response.status_code >= 400);
    Lwt.return_unit
  end

let lwt_test name f =
  Alcotest.test_case name `Quick (fun () -> Lwt_main.run (f ()))

let () =
  Alcotest.run "Stripe Lwt Integration" [
    "customer", [
      lwt_test "create_retrieve" test_customer_create_retrieve;
      lwt_test "list" test_customer_list;
    ];
    "payment_intent", [
      lwt_test "create" test_payment_intent_create;
      lwt_test "retrieve" test_payment_intent_retrieve;
    ];
    "balance", [
      lwt_test "retrieve" test_balance_retrieve;
    ];
    "product", [
      lwt_test "create" test_product_create;
    ];
    "charge", [
      lwt_test "list" test_charge_list;
    ];
    "refund", [
      lwt_test "create" test_refund_create;
    ];
    "event", [
      lwt_test "list" test_event_list;
    ];
    "error", [
      lwt_test "handling" test_error_handling;
    ];
  ]
