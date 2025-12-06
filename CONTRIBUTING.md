# Contributing to ocaml-stripe

Thank you for your interest in contributing!

## Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/makerprism/ocaml-stripe.git
   cd ocaml-stripe
   ```

2. Lock and build dependencies:
   ```bash
   dune pkg lock
   dune build
   ```

3. Run tests:
   ```bash
   make test
   # or
   dune test
   ```

## Project Structure

```
ocaml-stripe/
├── packages/
│   ├── stripe-core/          # Core abstractions, types, webhook verification
│   │   ├── src/
│   │   │   └── stripe_core.ml
│   │   └── test/
│   │       └── test_stripe_core.ml
│   ├── stripe-lwt/           # Lwt runtime adapter
│   │   ├── src/
│   │   │   └── stripe_lwt.ml
│   │   └── test/
│   │       └── test_stripe_lwt.ml
│   └── stripe/               # Full API client
│       ├── src/
│       │   └── stripe.ml
│       └── test/
│           └── test_stripe.ml
├── dune-project
├── dune-workspace
├── Makefile
├── LICENSE
├── README.md
└── CONTRIBUTING.md
```

## Adding a New Resource

1. Add the resource type to `packages/stripe/src/stripe.ml`:
   ```ocaml
   module MyResource = struct
     type t = {
       id : string;
       object_ : string;
       (* ... fields ... *)
       raw : Yojson.Safe.t;
     }

     let of_json json =
       let open Yojson.Safe.Util in
       {
         id = json |> member "id" |> to_string;
         object_ = json |> member "object" |> to_string;
         (* ... parse fields ... *)
         raw = json;
       }

     let to_json t = t.raw
   end
   ```

2. Add tests in `packages/stripe/test/test_stripe.ml`:
   ```ocaml
   let sample_my_resource = {| ... JSON fixture ... |}

   let test_my_resource_parsing () =
     let json = Yojson.Safe.from_string sample_my_resource in
     let resource = MyResource.of_json json in
     check string "id" "expected_id" resource.id
   ```

3. Run tests to verify:
   ```bash
   dune test
   ```

## Testing with stripe-mock

For integration testing, you can use [stripe-mock](https://github.com/stripe/stripe-mock):

```bash
# Install stripe-mock
go install github.com/stripe/stripe-mock@latest

# Run stripe-mock
stripe-mock

# Run tests against stripe-mock (on port 12111 by default)
STRIPE_MOCK_PORT=12111 dune test
```

## Code Style

- Follow OCaml conventions
- Use meaningful names
- Add documentation comments for public APIs
- Keep functions small and focused
- Handle errors explicitly with Result types
- Use variant types for enums/statuses

## Pull Request Process

1. Create a feature branch
2. Make your changes
3. Ensure tests pass: `dune test`
4. Format code: `dune fmt` (if ocamlformat is configured)
5. Update documentation if needed
6. Submit a PR with a clear description

## Test Suite

Tests are adapted from the official Stripe SDKs:
- [stripe-python](https://github.com/stripe/stripe-python) - Primary reference
- [stripe-node](https://github.com/stripe/stripe-node) - Additional patterns

Key test files from stripe-python that inform our tests:
- `tests/test_stripe_object.py` - Object parsing and serialization
- `tests/test_webhook.py` - Webhook signature verification
- `tests/test_api_requestor.py` - Request/response handling

## Releasing

Releases are created by pushing tags in the format `<package>@<version>`:

```bash
git tag stripe@0.1.0
git push origin stripe@0.1.0
```
