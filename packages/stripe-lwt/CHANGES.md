# Changelog

All notable changes to `stripe-lwt` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial release
- Full Stripe API client with 35 resource modules
- CRUD operations for all resources
- Automatic retry with exponential backoff for network errors and 5xx responses
- Idempotency key support for safe retries
- Stripe-Account header support for Connect platforms
- Pagination helpers: `fold_pages`, `collect_all`, `iter_all`, `to_stream`
- Type-safe enums for subscription parameters (trial_end, payment_behavior, proration_behavior)

### Security

- ID validation on all endpoints to prevent path injection
- Request timeout enforcement using `Lwt.pick`
- HTTPS enforcement for API base URL
- Response body truncation (1000 chars) to prevent memory issues
