# ocaml-stripe

[![CI](https://github.com/makerprism/ocaml-stripe/actions/workflows/ci.yml/badge.svg)](https://github.com/makerprism/ocaml-stripe/actions/workflows/ci.yml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![OCaml](https://img.shields.io/badge/OCaml-%3E%3D4.14-orange)](https://ocaml.org/)

OCaml SDK for the Stripe API. Process payments, manage customers, handle subscriptions, and verify webhooks. Runtime-agnostic design works with Lwt, Eio, or sync code.

> **Warning: Experimental Software**
>
> This SDK is **not production-ready**. It was primarily built using LLMs and is under active development. We are working towards making these libraries stable and usable.
>
> **What has been implemented:**
> - Core types for Stripe resources (Customer, Charge, PaymentIntent, etc.)
> - Webhook signature verification
> - Error parsing and handling
> - Full CRUD operations via Lwt client
>
> All functionality should be considered untested against the live Stripe API. Use at your own risk and expect breaking changes.

## Packages

| Package | Description |
|---------|-------------|
| `stripe-core` | Core interfaces and types (runtime-agnostic) |
| `stripe-lwt` | Lwt runtime adapter with full API client |
| `stripe` | Stripe resource types and parsing |
| `stripe-sane` | **Experimental** - Opinionated convenience helpers (customer deduplication, subscription guards) |

## Installation

### Using Dune Package Management (recommended)

Add to your `dune-project`:

```scheme
(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe-core)))

(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe-lwt)))

(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe)))
```

Then run:
```bash
dune pkg lock
dune build
```

## Usage

### Configuration

```ocaml
open Stripe_lwt

let config = Client.create ~api_key:"sk_test_..."
```

### Customer Operations

```ocaml
open Lwt.Syntax
open Stripe_lwt

let () = Lwt_main.run begin
  let config = Client.create ~api_key:"sk_test_..." in

  (* Create a customer *)
  let* result = Client.Customer.create ~config
    ~email:"customer@example.com"
    ~name:"Jane Doe"
    ~description:"New customer"
    ()
  in
  match result with
  | Ok customer ->
    Printf.printf "Created customer: %s\n" customer.id;
    
    (* Retrieve the customer *)
    let* result = Client.Customer.retrieve ~config ~id:customer.id () in
    (match result with
    | Ok c -> Printf.printf "Retrieved: %s\n" (Option.value ~default:"" c.email)
    | Error e -> Printf.printf "Error: %s\n" e.message);
    
    (* List customers *)
    let* result = Client.Customer.list ~config ~limit:10 () in
    (match result with
    | Ok list -> Printf.printf "Found %d customers\n" (List.length list.data)
    | Error e -> Printf.printf "Error: %s\n" e.message);
    
    Lwt.return_unit
  | Error e ->
    Printf.printf "Error: %s\n" e.message;
    Lwt.return_unit
end
```

### Payment Intents

```ocaml
open Lwt.Syntax
open Stripe_lwt

let create_payment config ~amount ~currency ~customer =
  let* result = Client.Payment_intent.create ~config
    ~amount
    ~currency
    ~customer
    ~capture_method:"automatic"
    ()
  in
  match result with
  | Ok pi ->
    Printf.printf "PaymentIntent %s: %s\n" pi.id
      (match pi.status with
       | Stripe.Payment_intent.Succeeded -> "succeeded"
       | Stripe.Payment_intent.Requires_payment_method -> "requires_payment_method"
       | Stripe.Payment_intent.Requires_confirmation -> "requires_confirmation"
       | Stripe.Payment_intent.Requires_action -> "requires_action"
       | Stripe.Payment_intent.Processing -> "processing"
       | Stripe.Payment_intent.Requires_capture -> "requires_capture"
       | Stripe.Payment_intent.Canceled -> "canceled");
    Lwt.return_ok pi
  | Error e ->
    Lwt.return_error e
```

### Subscriptions

```ocaml
open Lwt.Syntax
open Stripe_lwt

let create_subscription config ~customer ~price =
  Client.Subscription.create ~config ~customer ~price ()

let cancel_subscription config ~id =
  Client.Subscription.cancel ~config ~id ()
```

### Pagination

The SDK provides helpers for iterating through paginated list results:

```ocaml
open Lwt.Syntax
open Stripe_lwt

(* Collect all customers into a single list *)
let get_all_customers config =
  Pagination.collect_all
    ~get_id:(fun c -> c.Stripe.Customer.id)
    ~fetch_page:(fun ?starting_after () ->
      Client.Customer.list ~config ?starting_after ())
    ()

(* Iterate over all customers *)
let process_all_customers config =
  Pagination.iter_all
    ~get_id:(fun c -> c.Stripe.Customer.id)
    ~fetch_page:(fun ?starting_after () ->
      Client.Customer.list ~config ?starting_after ())
    ~f:(fun customer ->
      Lwt_io.printlf "Customer: %s" customer.Stripe.Customer.id)
    ()

(* Stream customers lazily *)
let stream_customers config =
  let stream = Pagination.to_stream
    ~get_id:(fun c -> c.Stripe.Customer.id)
    ~fetch_page:(fun ?starting_after () ->
      Client.Customer.list ~config ?starting_after ())
    ()
  in
  Lwt_stream.iter_s (fun customer ->
    Lwt_io.printlf "Customer: %s" customer.Stripe.Customer.id
  ) stream
```

### Automatic Retries

The SDK automatically retries failed requests with exponential backoff:

```ocaml
(* Enable retries (default is 0 = no retries) *)
let config = { 
  (Stripe_lwt.Client.create ~api_key:"sk_test_...") with 
  max_network_retries = 2 
}

(* Requests will now retry up to 2 times on:
   - Network errors (connection failures, timeouts)
   - 5xx server errors
   - 409 Conflict errors (rate limiting)
   
   Retries use exponential backoff with jitter. *)
```

### Webhook Signature Verification

```ocaml
open Stripe_core

let handle_webhook ~payload ~sig_header ~webhook_secret =
  try
    let _ = Webhook.verify_signature
      ~payload
      ~sig_header
      ~secret:webhook_secret
      ()
    in
    (* Signature valid, parse the event *)
    let json = Yojson.Safe.from_string payload in
    let event = Stripe.Event.of_json json in
    Printf.printf "Received event: %s\n" event.type_;
    Ok event
  with
  | Signature_verification_error err ->
    Error (Printf.sprintf "Invalid signature: %s" err.message)
  | exn ->
    Error (Printf.sprintf "Error: %s" (Printexc.to_string exn))
```

### Convenience Helpers (stripe-sane)

> **Experimental**: `stripe-sane` provides opinionated helpers for common patterns.
> These are NOT part of the official Stripe API - official SDKs intentionally leave
> these patterns to application developers.

The `stripe-sane` package helps prevent common issues like duplicate customers and subscriptions:

```ocaml
open Lwt.Syntax

(* Define a client adapter (do this once in your project) *)
module Lwt_client : Stripe_sane.STRIPE_CLIENT with type 'a io = 'a Lwt.t = struct
  type 'a io = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return

  module Customer = struct
    let list = Stripe_lwt.Client.Customer.list
    let create = Stripe_lwt.Client.Customer.create
  end

  module Subscription = struct
    let list = Stripe_lwt.Client.Subscription.list
    let create ~config ~customer ~price ?idempotency_key
        ?default_payment_method ?trial_period_days ?coupon ?promotion_code
        ?automatic_tax ?description ?metadata () =
      Stripe_lwt.Client.Subscription.create
        ~config ~customer ~price ?idempotency_key
        ?default_payment_method ?trial_period_days ?coupon ?promotion_code
        ?automatic_tax ?description ?metadata ()
  end
end

module Sane = Stripe_sane.Make (Lwt_client)

(* Get or create a customer by email - no duplicates *)
let get_customer config email =
  let* result = Sane.Customer.get_or_create ~config ~email () in
  match result with
  | Ok (Stripe_sane.Found customer) -> 
    Printf.printf "Found existing customer: %s\n" customer.id;
    Lwt.return_ok customer
  | Ok (Stripe_sane.Created customer) -> 
    Printf.printf "Created new customer: %s\n" customer.id;
    Lwt.return_ok customer
  | Error e -> 
    Lwt.return_error e

(* Create subscription only if not already subscribed to this price *)
let subscribe_customer config ~customer_id ~price_id =
  let* result = Sane.Subscription.create_if_not_subscribed 
    ~config ~customer:customer_id ~price:price_id () in
  match result with
  | Ok sub -> 
    Printf.printf "Created subscription: %s\n" sub.id;
    Lwt.return_ok sub
  | Error (Stripe_sane.Already_subscribed existing) ->
    Printf.printf "Already subscribed: %s\n" existing.id;
    Lwt.return_ok existing
  | Error (Stripe_sane.Stripe_error e) ->
    Lwt.return_error e
```

Key features:
- **Email normalization**: Emails are lowercased and trimmed for consistent lookups
- **Idempotency keys**: Automatic idempotency keys prevent race conditions
- **Pagination**: Subscription checks paginate through all results

## Architecture

The SDK follows a runtime-agnostic design:

1. **Core** (`stripe-core`): Pure OCaml types, interfaces, error handling, and webhook verification
2. **Runtime Adapters** (`stripe-lwt`): HTTP client and full API operations
3. **Resource Types** (`stripe`): Stripe resource types and JSON parsing

### Features

- **Webhook Verification**: Secure HMAC-SHA256 signature verification with timing attack protection
- **Error Handling**: Comprehensive Stripe error type parsing
- **Type Safety**: Strongly typed resource models with variant types for statuses
- **Runtime Agnostic**: Core logic works with any async runtime
- **Idiomatic OCaml**: Uses Result types, optional parameters, and modules
- **Pagination Helpers**: Easy iteration through paginated results with `collect_all`, `iter_all`, `fold_pages`, and `to_stream`
- **Automatic Retries**: Exponential backoff with jitter for network errors and 5xx responses

## API Reference

### Client Module

```ocaml
module Client : sig
  type t = Stripe_core.config
  val create : api_key:string -> t

  module Customer : sig
    val create : config:t -> ?email:string -> ?name:string -> 
      ?description:string -> ?phone:string -> 
      ?metadata:(string * string) list -> unit -> 
      (Stripe.Customer.t, Stripe_core.stripe_error) result Lwt.t
    val retrieve : config:t -> id:string -> unit -> 
      (Stripe.Customer.t, Stripe_core.stripe_error) result Lwt.t
    val update : config:t -> id:string -> ?email:string -> ?name:string -> 
      ?description:string -> ?phone:string -> 
      ?metadata:(string * string) list -> unit -> 
      (Stripe.Customer.t, Stripe_core.stripe_error) result Lwt.t
    val delete : config:t -> id:string -> unit -> 
      (Stripe.Deleted.t, Stripe_core.stripe_error) result Lwt.t
    val list : config:t -> ?limit:int -> ?starting_after:string -> 
      ?ending_before:string -> ?email:string -> unit -> 
      (Stripe.Customer.t Stripe.List_response.t, Stripe_core.stripe_error) result Lwt.t
  end

  module Payment_intent : sig
    val create : config:t -> amount:int -> currency:string -> 
      ?customer:string -> ?description:string -> ?payment_method:string -> 
      ?confirm:bool -> ?capture_method:string -> 
      ?metadata:(string * string) list -> unit -> 
      (Stripe.Payment_intent.t, Stripe_core.stripe_error) result Lwt.t
    val retrieve : config:t -> id:string -> unit -> 
      (Stripe.Payment_intent.t, Stripe_core.stripe_error) result Lwt.t
    val confirm : config:t -> id:string -> ?payment_method:string -> unit -> 
      (Stripe.Payment_intent.t, Stripe_core.stripe_error) result Lwt.t
    val capture : config:t -> id:string -> ?amount_to_capture:int -> unit -> 
      (Stripe.Payment_intent.t, Stripe_core.stripe_error) result Lwt.t
    val cancel : config:t -> id:string -> ?cancellation_reason:string -> unit -> 
      (Stripe.Payment_intent.t, Stripe_core.stripe_error) result Lwt.t
    (* ... and more *)
  end
  
  (* Additional modules with similar signatures: *)
  module Charge : sig ... end
  module Refund : sig ... end
  module Subscription : sig ... end
  module Invoice : sig ... end
  module Product : sig ... end
  module Price : sig ... end
  module Balance : sig ... end
  module Event : sig ... end
  module Payment_method : sig ... end
  module Setup_intent : sig ... end
  module Coupon : sig ... end
  module Balance_transaction : sig ... end
  module Payout : sig ... end
  module Checkout_session : sig ... end
  module Tax_rate : sig ... end
  module Payment_link : sig ... end
  module Dispute : sig ... end
end
```

## Testing

The test suite is adapted from the official [stripe-python](https://github.com/stripe/stripe-python) SDK:

```bash
make test
# or
dune test
```

## License

MIT - Copyright (c) 2025 Sabine Schmaltz
