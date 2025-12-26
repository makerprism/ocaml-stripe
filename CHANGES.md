# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial release of ocaml-stripe SDK
- **stripe-sane** (Experimental): Opinionated convenience helpers for common Stripe patterns
  - `Customer.get_or_create`: Find or create customer by email with automatic deduplication
    - Email normalization (lowercase, trim) for consistent lookups
    - Idempotency keys to prevent race conditions
  - `Subscription.create_if_not_subscribed`: Create subscription only if customer doesn't already have one
    - Checks both `active` and `trialing` subscriptions
    - Pagination support for customers with many subscriptions
    - Common parameters: `trial_period_days`, `coupon`, `promotion_code`, `automatic_tax`, `metadata`
  - Functor-based design works with any async runtime (Lwt, Eio, etc.)
- **stripe-core**: Core types, interfaces, and webhook signature verification
  - HTTP method and response types
  - Stripe error types and parsing
  - Configuration and request options (including `max_network_retries`)
  - Idempotency key generation
  - Pagination helpers (`last_id`, `has_next_page`, `Pagination.next_page_cursor`, `collect_all_sync`)
  - Webhook signature verification with HMAC-SHA256
  - Timing-attack-safe signature comparison
- **stripe**: Stripe resource types with JSON parsing (33 resources)
  - Core Payments: PaymentIntent, PaymentMethod, SetupIntent, Charge, Refund
  - Checkout: CheckoutSession, PaymentLink
  - Billing: Customer, CustomerBalanceTransaction, Subscription, SubscriptionItem, SubscriptionSchedule, Invoice, InvoiceItem, Quote, CreditNote, Price, Product, Coupon, PromotionCode, Discount, TaxRate, UsageRecord, BillingPortalSession
  - Connect: Account, Transfer, Payout, Topup, ApplicationFee, BalanceTransaction, Balance
  - Fraud: Dispute, Review
  - Files: File, FileLink
  - Other: Mandate, Event
- **stripe-lwt**: Lwt-based HTTP client with full API operations (35 client modules)
  - CRUD operations for all resources with API endpoints
  - Pagination support via List responses with helpers (`Pagination.fold_pages`, `collect_all`, `iter_all`, `to_stream`)
  - Automatic retry with exponential backoff for network errors and 5xx responses
  - Idempotency key support
  - Stripe-Account header for Connect
  - Bearer token authentication
- GitHub Actions CI workflow for Ubuntu and macOS
- GitHub Actions release workflow for tagged releases

### Security

- **Cryptographically secure idempotency keys**: Replaced `Random.int` (predictable PRNG) with `/dev/urandom` for secure random generation
- **ID validation**: Added `is_valid_stripe_id` and `validate_id` functions to prevent path injection attacks; validates alphanumeric + underscore format. Applied to all 35 client modules with ID-based endpoints.
- **Request timeout implementation**: Added `Request_timeout` exception with proper enforcement using `Lwt.pick` (timeout was previously configured but not enforced)
- **HTTPS enforcement**: Added `validate_api_base` to reject non-HTTPS API base URLs
- **Future timestamp rejection**: Webhook signature verification now rejects timestamps in the future (not just expired ones)
- **DoS protection**: Limited webhook signature parsing to 20 items maximum
- **Error message sanitization**: Removed raw payload from `Signature_verification_error` to prevent sensitive data leakage in logs
- **Response truncation**: Limited error response bodies to 1000 characters to prevent memory issues with large error responses

### Notes

- This SDK is experimental and not yet production-ready
- All functionality should be tested against the Stripe API before production use
