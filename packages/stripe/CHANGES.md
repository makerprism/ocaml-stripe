# Changelog

All notable changes to `stripe` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- `Checkout_session.checkout_promotion` ADT enforcing mutual exclusion between
  `allow_promotion_codes` and `discounts` at the type level
- Initial release with 33 Stripe resource types
- **Core Payments**: PaymentIntent, PaymentMethod, SetupIntent, Charge, Refund
- **Checkout**: CheckoutSession, PaymentLink
- **Billing**: Customer, Subscription, SubscriptionItem, SubscriptionSchedule, Invoice, InvoiceItem, Quote, CreditNote, Price, Product, Coupon, PromotionCode, Discount, TaxRate, UsageRecord, BillingPortalSession, CustomerBalanceTransaction
- **Connect**: Account, Transfer, Payout, Topup, ApplicationFee, BalanceTransaction, Balance
- **Fraud**: Dispute, Review
- **Files**: File, FileLink
- **Other**: Mandate, Event, TaxId
- Type-safe status enums for Subscription, PaymentIntent, Invoice, etc.
- Helper functions: `is_active`, `is_trialing`, `will_cancel`, `is_verified`
- `List_response` type for paginated responses
- `Deleted` type for deletion confirmations
