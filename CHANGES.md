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
- **stripe**: Stripe resource types with JSON parsing (31 resources)
  - Core Payments: PaymentIntent, PaymentMethod, SetupIntent, Charge, Refund
  - Checkout: CheckoutSession, PaymentLink
  - Billing: Customer, Subscription, SubscriptionItem, SubscriptionSchedule, Invoice, InvoiceItem, Quote, CreditNote, Price, Product, Coupon, PromotionCode, Discount, TaxRate, UsageRecord
  - Connect: Account, Transfer, Payout, Topup, ApplicationFee, BalanceTransaction, Balance
  - Fraud: Dispute, Review
  - Files: File, FileLink
  - Other: Mandate, Event
- **stripe-lwt**: Lwt-based HTTP client with full API operations (33 client modules)
  - CRUD operations for all resources with API endpoints
  - Pagination support via List responses
  - Idempotency key support
  - Stripe-Account header for Connect
  - Bearer token authentication
- GitHub Actions CI workflow for Ubuntu and macOS
- GitHub Actions release workflow for tagged releases

### Notes

- This SDK is experimental and not yet production-ready
- All functionality should be tested against the Stripe API before production use
