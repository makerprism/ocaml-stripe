# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial release of ocaml-stripe SDK
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

### Notes

- This SDK is experimental and not yet production-ready
- All functionality should be tested against the Stripe API before production use
