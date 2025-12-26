(** Tests for Stripe_sane *)

(** {1 Test normalize_email} *)

let test_normalize_email_lowercase () =
  Alcotest.(check string) "lowercase" 
    "user@example.com" 
    (Stripe_sane.normalize_email "USER@EXAMPLE.COM")

let test_normalize_email_trim () =
  Alcotest.(check string) "trim whitespace" 
    "user@example.com" 
    (Stripe_sane.normalize_email "  user@example.com  ")

let test_normalize_email_both () =
  Alcotest.(check string) "lowercase and trim" 
    "user@example.com" 
    (Stripe_sane.normalize_email "  USER@Example.COM  ")

let test_normalize_email_already_normal () =
  Alcotest.(check string) "already normalized" 
    "user@example.com" 
    (Stripe_sane.normalize_email "user@example.com")

(** {1 Test lookup_result} *)

let test_lookup_value_created () =
  let result = Stripe_sane.Created 42 in
  Alcotest.(check int) "extract from Created" 42 (Stripe_sane.lookup_value result)

let test_lookup_value_found () =
  let result = Stripe_sane.Found 42 in
  Alcotest.(check int) "extract from Found" 42 (Stripe_sane.lookup_value result)

(** {1 Mock Client for testing functor} *)

(** Simple identity monad for synchronous testing *)
module Id = struct
  type 'a t = 'a
  let bind x f = f x
  let return x = x
end

(** Create a mock customer for testing *)
let make_mock_customer ~id ~email : Stripe.Customer.t =
  let raw = `Assoc [
    ("id", `String id);
    ("object", `String "customer");
    ("email", `String email);
    ("name", `Null);
    ("description", `Null);
    ("phone", `Null);
    ("created", `Int 1234567890);
    ("livemode", `Bool false);
    ("balance", `Int 0);
  ] in
  {
    Stripe.Customer.
    id;
    object_ = "customer";
    address = None;
    balance = 0;
    created = 1234567890;
    currency = None;
    default_source = None;
    delinquent = None;
    description = None;
    email = Some email;
    invoice_prefix = None;
    livemode = false;
    name = None;
    phone = None;
    raw;
  }

(** Create a mock subscription for testing *)
let make_mock_subscription ~id ~customer ~price ~status : Stripe.Subscription.t =
  let raw = `Assoc [
    ("id", `String id);
    ("object", `String "subscription");
    ("customer", `String customer);
    ("status", `String status);
    ("items", `Assoc [
      ("data", `List [
        `Assoc [
          ("id", `String "si_test");
          ("price", `Assoc [("id", `String price)]);
        ]
      ])
    ]);
    ("created", `Int 1234567890);
    ("current_period_start", `Int 1234567890);
    ("current_period_end", `Int 1234567890);
    ("livemode", `Bool false);
    ("metadata", `Assoc []);
    ("cancel_at_period_end", `Bool false);
  ] in
  {
    Stripe.Subscription.
    id;
    object_ = "subscription";
    billing_cycle_anchor = None;
    cancel_at = None;
    cancel_at_period_end = false;
    canceled_at = None;
    collection_method = None;
    created = 1234567890;
    currency = None;
    current_period_end = 1234567890;
    current_period_start = 1234567890;
    customer;
    days_until_due = None;
    default_payment_method = None;
    default_source = None;
    description = None;
    ended_at = None;
    latest_invoice = None;
    livemode = false;
    metadata = [];
    status = (match status with
      | "active" -> Stripe.Subscription.Active
      | "trialing" -> Stripe.Subscription.Trialing
      | "canceled" -> Stripe.Subscription.Canceled
      | "incomplete" -> Stripe.Subscription.Incomplete
      | "incomplete_expired" -> Stripe.Subscription.Incomplete_expired
      | "past_due" -> Stripe.Subscription.Past_due
      | "unpaid" -> Stripe.Subscription.Unpaid
      | "paused" -> Stripe.Subscription.Paused
      | _ -> Stripe.Subscription.Active);
    start_date = None;
    trial_end = None;
    trial_start = None;
    raw;
  }

(** Mock client that returns a customer when listing *)
module Mock_client_with_existing_customer : Stripe_sane.STRIPE_CLIENT 
  with type 'a io = 'a Id.t = struct
  type 'a io = 'a Id.t
  let bind = Id.bind
  let return = Id.return

  module Customer = struct
    let list ~config:_ ?limit:_ ?starting_after:_ ?ending_before:_ ?email:_ () =
      let customer = make_mock_customer ~id:"cus_existing" ~email:"user@example.com" in
      Ok { Stripe.List_response.data = [customer]; has_more = false; url = "/v1/customers" }

    let create ~config:_ ?idempotency_key:_ ?email:_ ?name:_ ?description:_ ?phone:_ ?metadata:_ () =
      failwith "Should not be called when customer exists"
  end

  module Subscription = struct
    let list ~config:_ ?limit:_ ?starting_after:_ ?customer:_ ?status:_ () =
      Ok { Stripe.List_response.data = []; has_more = false; url = "/v1/subscriptions" }

    let create ~config:_ ~customer:_ ~price:_ ?idempotency_key:_ 
        ?default_payment_method:_ ?trial_period_days:_ ?coupon:_ 
        ?promotion_code:_ ?automatic_tax:_ ?description:_ ?metadata:_ () =
      failwith "Not implemented for this test"
  end
end

(** Mock client that returns no customers (triggers creation) *)
module Mock_client_no_customer : Stripe_sane.STRIPE_CLIENT 
  with type 'a io = 'a Id.t = struct
  type 'a io = 'a Id.t
  let bind = Id.bind
  let return = Id.return

  module Customer = struct
    let list ~config:_ ?limit:_ ?starting_after:_ ?ending_before:_ ?email:_ () =
      Ok { Stripe.List_response.data = []; has_more = false; url = "/v1/customers" }

    let create ~config:_ ?idempotency_key:_ ?email ?name:_ ?description:_ ?phone:_ ?metadata:_ () =
      let email = Option.value ~default:"unknown@example.com" email in
      let customer = make_mock_customer ~id:"cus_new" ~email in
      Ok customer
  end

  module Subscription = struct
    let list ~config:_ ?limit:_ ?starting_after:_ ?customer:_ ?status:_ () =
      Ok { Stripe.List_response.data = []; has_more = false; url = "/v1/subscriptions" }

    let create ~config:_ ~customer ~price ?idempotency_key:_ 
        ?default_payment_method:_ ?trial_period_days:_ ?coupon:_ 
        ?promotion_code:_ ?automatic_tax:_ ?description:_ ?metadata:_ () =
      let sub = make_mock_subscription ~id:"sub_new" ~customer ~price ~status:"active" in
      Ok sub
  end
end

(** Mock client with existing subscription *)
module Mock_client_with_existing_subscription : Stripe_sane.STRIPE_CLIENT 
  with type 'a io = 'a Id.t = struct
  type 'a io = 'a Id.t
  let bind = Id.bind
  let return = Id.return

  module Customer = struct
    let list ~config:_ ?limit:_ ?starting_after:_ ?ending_before:_ ?email:_ () =
      Ok { Stripe.List_response.data = []; has_more = false; url = "/v1/customers" }

    let create ~config:_ ?idempotency_key:_ ?email:_ ?name:_ ?description:_ ?phone:_ ?metadata:_ () =
      failwith "Not implemented for this test"
  end

  module Subscription = struct
    let list ~config:_ ?limit:_ ?starting_after:_ ?customer ?status () =
      let customer_id = Option.value ~default:"cus_test" customer in
      let status_str = Option.value ~default:"active" status in
      if status_str = "active" then
        let sub = make_mock_subscription 
          ~id:"sub_existing" ~customer:customer_id ~price:"price_123" ~status:"active" in
        Ok { Stripe.List_response.data = [sub]; has_more = false; url = "/v1/subscriptions" }
      else
        Ok { Stripe.List_response.data = []; has_more = false; url = "/v1/subscriptions" }

    let create ~config:_ ~customer:_ ~price:_ ?idempotency_key:_ 
        ?default_payment_method:_ ?trial_period_days:_ ?coupon:_ 
        ?promotion_code:_ ?automatic_tax:_ ?description:_ ?metadata:_ () =
      failwith "Should not be called when subscription exists"
  end
end

(** {1 Test Customer.get_or_create} *)

module Sane_with_existing = Stripe_sane.Make(Mock_client_with_existing_customer)
module Sane_no_customer = Stripe_sane.Make(Mock_client_no_customer)
module Sane_with_subscription = Stripe_sane.Make(Mock_client_with_existing_subscription)

let mock_config = Stripe_core.default_config ~api_key:"sk_test_xxx"

let test_get_or_create_finds_existing () =
  match Sane_with_existing.Customer.get_or_create ~config:mock_config ~email:"USER@example.com" () with
  | Ok (Stripe_sane.Found customer) ->
    Alcotest.(check string) "returns existing customer id" "cus_existing" customer.id
  | Ok (Stripe_sane.Created _) ->
    Alcotest.fail "Expected Found, got Created"
  | Error _ ->
    Alcotest.fail "Expected Ok, got Error"

let test_get_or_create_creates_new () =
  match Sane_no_customer.Customer.get_or_create ~config:mock_config ~email:"new@example.com" () with
  | Ok (Stripe_sane.Created customer) ->
    Alcotest.(check string) "returns new customer id" "cus_new" customer.id
  | Ok (Stripe_sane.Found _) ->
    Alcotest.fail "Expected Created, got Found"
  | Error _ ->
    Alcotest.fail "Expected Ok, got Error"

(** {1 Test Subscription.create_if_not_subscribed} *)

let test_create_if_not_subscribed_creates () =
  match Sane_no_customer.Subscription.create_if_not_subscribed 
    ~config:mock_config ~customer:"cus_test" ~price:"price_456" () with
  | Ok sub ->
    Alcotest.(check string) "returns new subscription id" "sub_new" sub.id
  | Error (Stripe_sane.Already_subscribed _) ->
    Alcotest.fail "Expected Ok, got Already_subscribed"
  | Error (Stripe_sane.Stripe_error _) ->
    Alcotest.fail "Expected Ok, got Stripe_error"

let test_create_if_not_subscribed_already_exists () =
  match Sane_with_subscription.Subscription.create_if_not_subscribed 
    ~config:mock_config ~customer:"cus_test" ~price:"price_123" () with
  | Ok _ ->
    Alcotest.fail "Expected Already_subscribed, got Ok"
  | Error (Stripe_sane.Already_subscribed sub) ->
    Alcotest.(check string) "returns existing subscription id" "sub_existing" sub.id
  | Error (Stripe_sane.Stripe_error _) ->
    Alcotest.fail "Expected Already_subscribed, got Stripe_error"

(** {1 Test suite} *)

let () =
  Alcotest.run "Stripe_sane" [
    "normalize_email", [
      Alcotest.test_case "lowercase" `Quick test_normalize_email_lowercase;
      Alcotest.test_case "trim" `Quick test_normalize_email_trim;
      Alcotest.test_case "lowercase and trim" `Quick test_normalize_email_both;
      Alcotest.test_case "already normalized" `Quick test_normalize_email_already_normal;
    ];
    "lookup_result", [
      Alcotest.test_case "lookup_value Created" `Quick test_lookup_value_created;
      Alcotest.test_case "lookup_value Found" `Quick test_lookup_value_found;
    ];
    "Customer.get_or_create", [
      Alcotest.test_case "finds existing customer" `Quick test_get_or_create_finds_existing;
      Alcotest.test_case "creates new customer" `Quick test_get_or_create_creates_new;
    ];
    "Subscription.create_if_not_subscribed", [
      Alcotest.test_case "creates new subscription" `Quick test_create_if_not_subscribed_creates;
      Alcotest.test_case "returns error when already subscribed" `Quick test_create_if_not_subscribed_already_exists;
    ];
  ]
