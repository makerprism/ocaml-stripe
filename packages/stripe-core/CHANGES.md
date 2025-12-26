# Changelog

All notable changes to `stripe-core` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added

- Initial release
- Configuration types (`config`, `request_options`)
- Stripe error types and parsing (`stripe_error`, `error_type`)
- Webhook signature verification with HMAC-SHA256
- Timing-attack-safe signature comparison
- Idempotency key generation with cryptographically secure randomness
- Pagination helpers (`last_id`, `has_next_page`, `collect_all_sync`)
- HTTP method and response types

### Security

- Cryptographically secure idempotency keys using `/dev/urandom`
- Future timestamp rejection in webhook verification
- DoS protection: limited webhook signature parsing to 20 items
- Error message sanitization to prevent sensitive data leakage
