# ocaml-stripe

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

## Architecture

The SDK follows a runtime-agnostic design:

1. **Core** (`stripe-core`): Pure OCaml types, interfaces, error handling, and webhook verification
2. **Runtime Adapters** (`stripe-lwt`): HTTP client and full API operations
3. **Resource Types** (`stripe`): Stripe resource types and JSON parsing

### Supported Resources

#### Core Payments

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| PaymentIntent | ✅ | ✅ | ✅ | - | ✅ | confirm, capture, cancel |
| PaymentMethod | - | ✅ | - | - | ✅ | attach, detach |
| SetupIntent | ✅ | ✅ | - | - | ✅ | confirm, cancel |
| Charge | - | ✅ | - | - | ✅ | capture |
| Refund | ✅ | ✅ | - | - | ✅ | |

#### Checkout & Payment Links

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| Checkout Session | ✅ | ✅ | - | - | ✅ | expire |
| Payment Link | ✅ | ✅ | ✅ | - | ✅ | |

#### Billing

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| Customer | ✅ | ✅ | ✅ | ✅ | ✅ | |
| Subscription | ✅ | ✅ | ✅ | ✅ | ✅ | |
| Invoice | - | ✅ | - | - | ✅ | pay, void |
| Price | ✅ | ✅ | - | - | ✅ | |
| Product | ✅ | ✅ | ✅ | ✅ | ✅ | |
| Coupon | ✅ | ✅ | ✅ | ✅ | ✅ | |
| TaxRate | ✅ | ✅ | ✅ | - | ✅ | |

#### Connect & Payouts

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| Account | ✅ | ✅ | ✅ | ✅ | ✅ | retrieve_current |
| Transfer | ✅ | ✅ | ✅ | - | ✅ | |
| Payout | ✅ | ✅ | - | - | ✅ | cancel |
| BalanceTransaction | - | ✅ | - | - | ✅ | |
| Balance | - | ✅ | - | - | - | |

#### Fraud & Disputes

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| Dispute | - | ✅ | ✅ | - | ✅ | close |
| Review | - | ✅ | - | - | ✅ | approve |

#### Files

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| File | - | ✅ | - | - | ✅ | |
| FileLink | ✅ | ✅ | ✅ | - | ✅ | |

#### Other

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| Mandate | - | ✅ | - | - | - | |

#### Events & Webhooks

| Resource | Create | Retrieve | Update | Delete | List | Other |
|----------|:------:|:--------:|:------:|:------:|:----:|-------|
| Event | - | ✅ | - | - | ✅ | |
| Webhook | - | - | - | - | - | verify_signature |

✅ = Implemented, - = Not applicable or not yet implemented

### Features

- **Webhook Verification**: Secure HMAC-SHA256 signature verification with timing attack protection
- **Error Handling**: Comprehensive Stripe error type parsing
- **Type Safety**: Strongly typed resource models with variant types for statuses
- **Runtime Agnostic**: Core logic works with any async runtime
- **Idiomatic OCaml**: Uses Result types, optional parameters, and modules

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
