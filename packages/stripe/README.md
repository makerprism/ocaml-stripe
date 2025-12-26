# stripe

Stripe resource types and JSON parsing for OCaml.

> **Warning**: This package is experimental and not production-ready.

## Overview

`stripe` provides strongly-typed OCaml representations of Stripe API resources. This package contains type definitions and JSON parsing for 33+ Stripe resources.

## Installation

```
opam install stripe
```

Or with dune package management, add to your `dune-project`:

```scheme
(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe)))
```

## Supported Resources

### Core Payments
- `Payment_intent` - Payment intents with status tracking
- `Payment_method` - Payment methods (cards, bank accounts, etc.)
- `Setup_intent` - Setup intents for future payments
- `Charge` - Charges
- `Refund` - Refunds

### Checkout
- `Checkout_session` - Checkout sessions
- `Payment_link` - Payment links

### Billing
- `Customer` - Customers
- `Subscription` - Subscriptions with status enums
- `Subscription_item` - Subscription line items
- `Subscription_schedule` - Subscription schedules
- `Invoice` - Invoices
- `Invoice_item` - Invoice line items
- `Quote` - Quotes
- `Credit_note` - Credit notes
- `Price` - Prices
- `Product` - Products
- `Coupon` - Coupons
- `Promotion_code` - Promotion codes
- `Discount` - Discounts
- `Tax_rate` - Tax rates
- `Usage_record` - Usage records

### Connect
- `Account` - Connected accounts
- `Transfer` - Transfers
- `Payout` - Payouts
- `Balance` - Balance
- `Balance_transaction` - Balance transactions

### Other
- `Event` - Webhook events
- `File` - Files
- `File_link` - File links
- `Dispute` - Disputes
- `Mandate` - Mandates

## Usage

```ocaml
open Stripe

(* Parse a customer from JSON *)
let json = Yojson.Safe.from_string raw_json in
let customer = Customer.of_json json

(* Access typed fields *)
let email = customer.email  (* string option *)
let id = customer.id        (* string *)

(* Check subscription status *)
let sub = Subscription.of_json sub_json in
match sub.status with
| Subscription.Active -> "active"
| Subscription.Trialing -> "trialing"
| Subscription.Canceled -> "canceled"
| _ -> "other"

(* Use helper functions *)
let is_active = Subscription.is_active sub
let will_cancel = Subscription.will_cancel sub
```

## Type Safety

This package provides:

- **Variant types** for statuses (e.g., `Subscription.status`, `Payment_intent.status`)
- **Option types** for nullable fields
- **Helper functions** like `is_active`, `is_trialing`, `will_cancel`
- **Raw JSON access** via `.raw` field for additional data

## Dependencies

- `stripe-core` - Core types and interfaces
- `yojson` - JSON parsing
- `uri` - URI handling

## License

MIT
