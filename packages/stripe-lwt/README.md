# stripe-lwt

Lwt runtime adapter for the OCaml Stripe SDK.

> **Warning**: This package is experimental and not production-ready.

## Overview

`stripe-lwt` provides a full-featured Stripe API client using Lwt for async I/O and Cohttp for HTTP requests. This is the main package you'll use to interact with the Stripe API.

## Installation

```
opam install stripe-lwt
```

Or with dune package management, add to your `dune-project`:

```scheme
(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe-lwt)))
```

## Usage

### Configuration

```ocaml
open Stripe_lwt

let config = Client.create ~api_key:"sk_test_..."

(* With retries *)
let config = { 
  (Client.create ~api_key:"sk_test_...") with 
  max_network_retries = 2 
}
```

### Customer Operations

```ocaml
open Lwt.Syntax
open Stripe_lwt

let () = Lwt_main.run begin
  let config = Client.create ~api_key:"sk_test_..." in

  (* Create *)
  let* result = Client.Customer.create ~config
    ~email:"customer@example.com"
    ~name:"Jane Doe"
    () in
  
  (* List *)
  let* result = Client.Customer.list ~config ~limit:10 () in
  
  (* Retrieve *)
  let* result = Client.Customer.retrieve ~config ~id:"cus_xxx" () in
  
  (* Update *)
  let* result = Client.Customer.update ~config ~id:"cus_xxx" 
    ~name:"New Name" () in
  
  (* Delete *)
  let* result = Client.Customer.delete ~config ~id:"cus_xxx" () in
  
  Lwt.return_unit
end
```

### Subscriptions

```ocaml
(* Create subscription *)
let* result = Client.Subscription.create ~config
  ~customer:"cus_xxx"
  ~price:"price_xxx"
  ~trial_period_days:14
  ()

(* Cancel subscription *)
let* result = Client.Subscription.cancel ~config ~id:"sub_xxx" ()
```

### Payment Intents

```ocaml
(* Create payment intent *)
let* result = Client.Payment_intent.create ~config
  ~amount:2000  (* $20.00 *)
  ~currency:"usd"
  ~customer:"cus_xxx"
  ()

(* Confirm *)
let* result = Client.Payment_intent.confirm ~config ~id:"pi_xxx" ()

(* Capture *)
let* result = Client.Payment_intent.capture ~config ~id:"pi_xxx" ()
```

### Pagination

```ocaml
(* Collect all customers *)
let* all_customers = Pagination.collect_all
  ~get_id:(fun c -> c.Stripe.Customer.id)
  ~fetch_page:(fun ?starting_after () ->
    Client.Customer.list ~config ?starting_after ())
  ()

(* Iterate over all *)
let* () = Pagination.iter_all
  ~get_id:(fun c -> c.Stripe.Customer.id)
  ~fetch_page:(fun ?starting_after () ->
    Client.Customer.list ~config ?starting_after ())
  ~f:(fun customer -> Lwt_io.printlf "Customer: %s" customer.id)
  ()

(* Stream lazily *)
let stream = Pagination.to_stream
  ~get_id:(fun c -> c.Stripe.Customer.id)
  ~fetch_page:(fun ?starting_after () ->
    Client.Customer.list ~config ?starting_after ())
  ()
```

## Available Client Modules

- `Customer`, `Payment_intent`, `Payment_method`, `Setup_intent`
- `Charge`, `Refund`, `Dispute`
- `Subscription`, `Invoice`, `Invoice_item`, `Quote`, `Credit_note`
- `Product`, `Price`, `Coupon`, `Promotion_code`, `Tax_rate`
- `Checkout_session`, `Payment_link`, `Billing_portal_session`
- `Account`, `Transfer`, `Payout`, `Balance`, `Balance_transaction`
- `File`, `File_link`, `Event`
- And more...

## Features

- **Automatic retries** with exponential backoff for network errors and 5xx responses
- **Idempotency key support** for safe retries
- **Stripe-Account header** for Connect platforms
- **Pagination helpers** for listing resources

## Dependencies

- `stripe-core` - Core types
- `stripe` - Resource types
- `lwt` - Async runtime
- `cohttp-lwt-unix` - HTTP client
- `tls-lwt` - TLS support

## License

MIT
