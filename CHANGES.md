# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial release of ocaml-stripe SDK
- **stripe-core**: Core types, interfaces, and webhook signature verification
  - HTTP method and response types
  - Stripe error types and parsing
  - Configuration and request options
  - Idempotency key generation
  - Webhook signature verification with HMAC-SHA256
  - Timing-attack-safe signature comparison
- **stripe**: Stripe resource types with JSON parsing
  - Core Payments: PaymentIntent, PaymentMethod, SetupIntent, Charge, Refund
  - Checkout: CheckoutSession, PaymentLink
  - Billing: Customer, Subscription, SubscriptionItem, SubscriptionSchedule, Invoice, InvoiceItem, Quote, CreditNote, Price, Product, Coupon, PromotionCode, TaxRate, UsageRecord
  - Connect: Account, Transfer, Payout, Topup, ApplicationFee, BalanceTransaction, Balance
  - Fraud: Dispute, Review
  - Files: File, FileLink
  - Other: Mandate, Discount, Event
- **stripe-lwt**: Lwt-based HTTP client with full API operations
  - CRUD operations for most resources
  - Pagination support via List responses
  - Idempotency key support
  - Stripe-Account header for Connect
  - Bearer token authentication

### Changed

- Nothing yet

### Fixed

- Nothing yet
