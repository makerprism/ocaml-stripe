# stripe-core

Core interfaces and types for the OCaml Stripe SDK.

> **Warning**: This package is experimental and not production-ready.

## Overview

`stripe-core` provides runtime-agnostic, HTTP-client-agnostic core interfaces for building Stripe API clients. This package contains:

- Configuration and request options types
- Stripe error types and parsing
- Webhook signature verification (HMAC-SHA256)
- Idempotency key generation
- Pagination helpers

## Installation

```
opam install stripe-core
```

Or with dune package management, add to your `dune-project`:

```scheme
(pin
 (url "git+https://github.com/makerprism/ocaml-stripe")
 (package (name stripe-core)))
```

## Usage

### Configuration

```ocaml
open Stripe_core

let config = default_config ~api_key:"sk_test_..."

(* With custom options *)
let config = { 
  (default_config ~api_key:"sk_test_...") with 
  max_network_retries = 2;
  timeout = 30.0;
}
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
    Ok "Valid signature"
  with
  | Signature_verification_error err ->
    Error (Printf.sprintf "Invalid signature: %s" err.message)
```

### Idempotency Keys

```ocaml
open Stripe_core

(* Generate a cryptographically secure idempotency key *)
let key = generate_idempotency_key ()
(* e.g., "a1b2c3d4e5f6..." *)
```

## Dependencies

- `yojson` - JSON parsing
- `uri` - URI handling  
- `ptime` - Time handling
- `base64` - Base64 encoding
- `digestif` - Cryptographic hashing

## License

MIT
