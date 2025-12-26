# Changelog

All notable changes to `stripe-sane` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial release (experimental)
- `Customer.get_or_create`: Find or create customer by email
  - Email normalization (lowercase, trim) for consistent lookups
  - Idempotency keys to prevent race conditions during creation
- `Subscription.create_if_not_subscribed`: Create subscription only if not already subscribed
  - Checks both `active` and `trialing` subscription statuses
  - Pagination support for customers with many subscriptions
  - Common parameters: `trial_period_days`, `coupon`, `promotion_code`, `automatic_tax`, `metadata`
- Functor-based design for runtime-agnostic usage (Lwt, Eio, etc.)
- `lookup_result` type to distinguish between created and found resources
- `normalize_email` helper function exposed for custom usage
