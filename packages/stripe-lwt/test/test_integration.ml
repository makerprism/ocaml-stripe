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

(** Test payment method retrieve *)
let test_payment_method_retrieve () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Payment_method.retrieve ~config ~id:"pm_123" () in
    match result with
    | Ok pm ->
      Alcotest.(check bool) "has id" true (String.length pm.id > 0);
      Alcotest.(check string) "object" "payment_method" pm.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "PaymentMethod retrieve failed: %s" err.message)
  end

(** Test setup intent create *)
let test_setup_intent_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Setup_intent.create ~config () in
    match result with
    | Ok si ->
      Alcotest.(check bool) "has id" true (String.length si.id > 0);
      Alcotest.(check string) "object" "setup_intent" si.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "SetupIntent create failed: %s" err.message)
  end

(** Test coupon create *)
let test_coupon_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Coupon.create ~config
      ~percent_off:25.0
      ~duration:"once"
      ()
    in
    match result with
    | Ok coupon ->
      Alcotest.(check bool) "has id" true (String.length coupon.id > 0);
      Alcotest.(check string) "object" "coupon" coupon.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Coupon create failed: %s" err.message)
  end

(** Test balance transaction list *)
let test_balance_transaction_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Balance_transaction.list ~config ~limit:5 () in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "BalanceTransaction list failed: %s" err.message)
  end

(** Test payout list *)
let test_payout_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Payout.list ~config ~limit:5 () in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Payout list failed: %s" err.message)
  end

(** Test invoice create *)
let test_invoice_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Invoice.create ~config
      ~customer:"cus_123"
      ~description:"Test invoice"
      ~auto_advance:false
      ~collection_method:Stripe_lwt.Client.Send_invoice
      ~days_until_due:30
      ()
    in
    match result with
    | Ok invoice ->
      Alcotest.(check bool) "has id" true (String.length invoice.id > 0);
      Alcotest.(check string) "object" "invoice" invoice.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Invoice create failed: %s" err.message)
  end

(** Test invoice retrieve *)
let test_invoice_retrieve () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Invoice.retrieve ~config ~id:"in_123" () in
    match result with
    | Ok invoice ->
      Alcotest.(check bool) "has id" true (String.length invoice.id > 0);
      Alcotest.(check string) "object" "invoice" invoice.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Invoice retrieve failed: %s" err.message)
  end

(** Test invoice list *)
let test_invoice_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Invoice.list ~config ~limit:5 () in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Invoice list failed: %s" err.message)
  end

(** Test invoice finalize *)
let test_invoice_finalize () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Invoice.finalize_invoice ~config ~id:"in_123" () in
    match result with
    | Ok invoice ->
      Alcotest.(check bool) "has id" true (String.length invoice.id > 0);
      Alcotest.(check string) "object" "invoice" invoice.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Invoice finalize failed: %s" err.message)
  end

(** Test invoice pay *)
let test_invoice_pay () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Invoice.pay ~config ~id:"in_123" () in
    match result with
    | Ok invoice ->
      Alcotest.(check bool) "has id" true (String.length invoice.id > 0);
      Alcotest.(check string) "object" "invoice" invoice.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Invoice pay failed: %s" err.message)
  end

(** Test invoice upcoming (preview) *)
let test_invoice_upcoming () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Invoice.upcoming ~config ~customer:"cus_123" () in
    match result with
    | Ok invoice ->
      (* upcoming invoices don't have an id yet *)
      Alcotest.(check string) "object" "invoice" invoice.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Invoice upcoming failed: %s" err.message)
  end

(** Test tax_id create *)
let test_tax_id_create () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Customer.Tax_id.create ~config
      ~customer:"cus_123"
      ~type_:"eu_vat"
      ~value:"DE123456789"
      ()
    in
    match result with
    | Ok tax_id ->
      Alcotest.(check bool) "has id" true (String.length tax_id.id > 0);
      Alcotest.(check string) "object" "tax_id" tax_id.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Tax_id create failed: %s" err.message)
  end

(** Test tax_id retrieve *)
let test_tax_id_retrieve () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Customer.Tax_id.retrieve ~config
      ~customer:"cus_123"
      ~id:"txi_123"
      ()
    in
    match result with
    | Ok tax_id ->
      Alcotest.(check bool) "has id" true (String.length tax_id.id > 0);
      Alcotest.(check string) "object" "tax_id" tax_id.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Tax_id retrieve failed: %s" err.message)
  end

(** Test tax_id list *)
let test_tax_id_list () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Customer.Tax_id.list ~config
      ~customer:"cus_123"
      ~limit:5
      ()
    in
    match result with
    | Ok list ->
      Alcotest.(check bool) "has data" true (List.length list.data >= 0);
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Tax_id list failed: %s" err.message)
  end

(** Test tax_id delete *)
let test_tax_id_delete () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Customer.Tax_id.delete ~config
      ~customer:"cus_123"
      ~id:"txi_123"
      ()
    in
    match result with
    | Ok deleted ->
      Alcotest.(check bool) "deleted" true deleted.deleted;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Tax_id delete failed: %s" err.message)
  end

(** Test subscription create with trial *)
let test_subscription_create_with_trial () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Subscription.create ~config
      ~customer:"cus_123"
      ~price:"price_123"
      ~trial_period_days:14
      ()
    in
    match result with
    | Ok sub ->
      Alcotest.(check bool) "has id" true (String.length sub.id > 0);
      Alcotest.(check string) "object" "subscription" sub.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Subscription create with trial failed: %s" err.message)
  end

(** Test subscription create with automatic_tax *)
let test_subscription_create_with_automatic_tax () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Subscription.create ~config
      ~customer:"cus_123"
      ~price:"price_123"
      ~automatic_tax:true
      ()
    in
    match result with
    | Ok sub ->
      Alcotest.(check bool) "has id" true (String.length sub.id > 0);
      Alcotest.(check string) "object" "subscription" sub.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Subscription create with automatic_tax failed: %s" err.message)
  end

(** Test subscription update with trial_end *)
let test_subscription_update_trial_end () =
  if not (is_stripe_mock_available ()) then
    Lwt.return_unit
  else begin
    let config = mock_config () in
    let* result = Stripe_lwt.Client.Subscription.update ~config
      ~id:"sub_123"
      ~trial_end:Stripe_lwt.Client.Subscription.Trial_end_now
      ()
    in
    match result with
    | Ok sub ->
      Alcotest.(check bool) "has id" true (String.length sub.id > 0);
      Alcotest.(check string) "object" "subscription" sub.object_;
      Lwt.return_unit
    | Error err ->
      Alcotest.fail (Printf.sprintf "Subscription update trial_end failed: %s" err.message)
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
    "payment_method", [
      lwt_test "retrieve" test_payment_method_retrieve;
    ];
    "setup_intent", [
      lwt_test "create" test_setup_intent_create;
    ];
    "coupon", [
      lwt_test "create" test_coupon_create;
    ];
    "balance_transaction", [
      lwt_test "list" test_balance_transaction_list;
    ];
    "payout", [
      lwt_test "list" test_payout_list;
    ];
    "invoice", [
      lwt_test "create" test_invoice_create;
      lwt_test "retrieve" test_invoice_retrieve;
      lwt_test "list" test_invoice_list;
      lwt_test "finalize" test_invoice_finalize;
      lwt_test "pay" test_invoice_pay;
      lwt_test "upcoming" test_invoice_upcoming;
    ];
    "tax_id", [
      lwt_test "create" test_tax_id_create;
      lwt_test "retrieve" test_tax_id_retrieve;
      lwt_test "list" test_tax_id_list;
      lwt_test "delete" test_tax_id_delete;
    ];
    "subscription_trial", [
      lwt_test "create_with_trial" test_subscription_create_with_trial;
      lwt_test "create_with_automatic_tax" test_subscription_create_with_automatic_tax;
      lwt_test "update_trial_end" test_subscription_update_trial_end;
    ];
  ]
