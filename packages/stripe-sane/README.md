# stripe-sane

Opinionated convenience helpers for Stripe.

> **Warning**: This package is **experimental** and not production-ready.

## Overview

`stripe-sane` provides higher-level patterns for common Stripe operations that official SDKs intentionally leave to application developers. This package helps prevent issues like:

- Duplicate customers with the same email
- Duplicate subscriptions to the same price

**Note**: These patterns are NOT part of the official Stripe API. They represent opinionated choices that may not fit all use cases.

## Installation

```
opam install stripe-sane
```

Or with dune package management, add to your `dune-project`:

```scheme
(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe-sane)))
```

## Usage

### Setup

First, create a client adapter for your async runtime:

```ocaml
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
```

### Customer Deduplication

Get or create a customer by email. Emails are normalized (lowercased, trimmed) for consistent lookups:

```ocaml
open Lwt.Syntax

let get_customer config email =
  let* result = Sane.Customer.get_or_create ~config ~email () in
  match result with
  | Ok (Stripe_sane.Found customer) -> 
    Printf.printf "Found existing: %s\n" customer.id;
    Lwt.return_ok customer
  | Ok (Stripe_sane.Created customer) -> 
    Printf.printf "Created new: %s\n" customer.id;
    Lwt.return_ok customer
  | Error e -> 
    Lwt.return_error e
```

### Subscription Guards

Create a subscription only if the customer doesn't already have one to the same price:

```ocaml
let subscribe config ~customer_id ~price_id =
  let* result = Sane.Subscription.create_if_not_subscribed 
    ~config 
    ~customer:customer_id 
    ~price:price_id 
    ~trial_period_days:14
    () 
  in
  match result with
  | Ok sub -> 
    Printf.printf "Created: %s\n" sub.id;
    Lwt.return_ok sub
  | Error (Stripe_sane.Already_subscribed existing) ->
    Printf.printf "Already subscribed: %s\n" existing.id;
    Lwt.return_ok existing
  | Error (Stripe_sane.Stripe_error e) ->
    Lwt.return_error e
```

## Features

### Email Normalization

`Customer.get_or_create` normalizes emails before lookup:
- Trims whitespace
- Converts to lowercase

This ensures `User@Example.com` and `user@example.com` are treated as the same customer.

### Idempotency Keys

Automatic idempotency keys prevent race conditions:
- Customer creation: `stripe_sane_customer_<normalized_email>`
- Subscription creation: `stripe_sane_sub_<customer_id>_<price_id>`

### Pagination

Subscription checks paginate through all results, so customers with many subscriptions are handled correctly.

## API

### Types

```ocaml
type 'a lookup_result = Created of 'a | Found of 'a

type error =
  | Stripe_error of Stripe_core.stripe_error
  | Already_subscribed of Stripe.Subscription.t

val lookup_value : 'a lookup_result -> 'a
val normalize_email : string -> string
```

### Customer

```ocaml
val get_or_create :
  config:Stripe_core.config ->
  email:string ->
  ?name:string ->
  ?description:string ->
  ?phone:string ->
  ?metadata:(string * string) list ->
  unit ->
  (Stripe.Customer.t lookup_result, Stripe_core.stripe_error) result io
```

### Subscription

```ocaml
val create_if_not_subscribed :
  config:Stripe_core.config ->
  customer:string ->
  price:string ->
  ?default_payment_method:string ->
  ?trial_period_days:int ->
  ?coupon:string ->
  ?promotion_code:string ->
  ?automatic_tax:bool ->
  ?description:string ->
  ?metadata:(string * string) list ->
  unit ->
  (Stripe.Subscription.t, error) result io
```

## Dependencies

- `stripe-core` - Core types
- `stripe` - Resource types
- `yojson` - JSON parsing

## License

MIT
