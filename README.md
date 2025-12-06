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
>
> All functionality should be considered untested against the live Stripe API. Use at your own risk and expect breaking changes.

## Packages

| Package | Description |
|---------|-------------|
| `stripe-core` | Core interfaces and types (runtime-agnostic) |
| `stripe-lwt` | Lwt runtime adapter with Cohttp |
| `stripe` | Full Stripe API client with resource types |

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
open Stripe

let config = default_config ~api_key:"sk_test_..."
```

### Parsing API Responses

```ocaml
open Stripe

(* Parse a customer from JSON response *)
let customer = Customer.of_json json in
Printf.printf "Customer: %s (%s)\n" customer.id (Option.value ~default:"" customer.email)

(* Parse a payment intent *)
let pi = Payment_intent.of_json json in
match pi.status with
| Payment_intent.Succeeded -> print_endline "Payment successful!"
| Payment_intent.Requires_action -> print_endline "Additional action required"
| _ -> print_endline "Payment pending"
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

### With Lwt Runtime

```ocaml
open Lwt.Infix
open Stripe_lwt

let list_customers config =
  get ~config ~path:"/v1/customers" () >>= fun response ->
  if response.status_code = 200 then
    let json = Yojson.Safe.from_string response.body in
    let customers = Stripe.List_response.of_json Stripe.Customer.of_json json in
    Lwt.return_ok customers
  else
    let json = Yojson.Safe.from_string response.body in
    match Stripe_core.parse_error json with
    | Some err -> Lwt.return_error err
    | None -> Lwt.return_error { error_type = Api_error; message = "Unknown error"; code = None; param = None; decline_code = None; doc_url = None }
```

## Architecture

The SDK follows a runtime-agnostic design:

1. **Core** (`stripe-core`): Pure OCaml types, interfaces, error handling, and webhook verification
2. **Runtime Adapters** (`stripe-lwt`): HTTP client implementations
3. **API Client** (`stripe`): Stripe resource types and parsing

### Supported Resources

| Resource | Parse | Create | Retrieve | Update | Delete | List |
|----------|-------|--------|----------|--------|--------|------|
| Customer | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Charge | ✅ | ⚠️ | ⚠️ | ⚠️ | - | ⚠️ |
| PaymentIntent | ✅ | ⚠️ | ⚠️ | ⚠️ | - | ⚠️ |
| Refund | ✅ | ⚠️ | ⚠️ | ⚠️ | - | ⚠️ |
| Subscription | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Invoice | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Product | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Price | ✅ | ⚠️ | ⚠️ | ⚠️ | ⚠️ | ⚠️ |
| Balance | ✅ | - | ⚠️ | - | - | - |
| Event | ✅ | - | ⚠️ | - | - | ⚠️ |

✅ = Implemented and tested, ⚠️ = Planned/In progress, - = Not applicable

### Features

- **Webhook Verification**: Secure HMAC-SHA256 signature verification with timing attack protection
- **Error Handling**: Comprehensive Stripe error type parsing
- **Type Safety**: Strongly typed resource models with variant types for statuses
- **Runtime Agnostic**: Core logic works with any async runtime

## Testing

The test suite is adapted from the official [stripe-python](https://github.com/stripe/stripe-python) SDK:

```bash
make test
# or
dune test
```

## License

MIT - Copyright (c) 2025 Sabine Schmaltz
