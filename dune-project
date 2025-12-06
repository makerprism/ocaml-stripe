(lang dune 3.17)
(name ocaml-stripe)

(generate_opam_files true)

(source (github makerprism/ocaml-stripe))
(license MIT)
(authors "Sabine Schmaltz")
(maintainers "sabine@makerprism.com")
(documentation "https://github.com/makerprism/ocaml-stripe")
(bug_reports "https://github.com/makerprism/ocaml-stripe/issues")

(package
 (name stripe-core)
 (synopsis "Core interfaces for Stripe API client")
 (description "Runtime-agnostic, HTTP-client-agnostic core interfaces for building Stripe API clients. This package provides CPS-style interfaces that can be implemented with any async runtime (Lwt, Eio, synchronous) and any HTTP client library.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  yojson
  uri
  digestif
  (alcotest :with-test)))

(package
 (name stripe-lwt)
 (synopsis "Lwt runtime adapter for stripe-core")
 (description "Lwt runtime adapter for stripe-core with Cohttp_lwt_unix HTTP client implementation.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (stripe-core (= :version))
  (stripe (= :version))
  lwt
  (lwt_ppx (>= 2.0.0))
  cohttp-lwt-unix
  tls-lwt
  (alcotest :with-test)))

(package
 (name stripe)
 (synopsis "Stripe API client for OCaml")
 (description "Full-featured Stripe API client. Supports payments, customers, subscriptions, and more.")
 (depends
  (ocaml (>= 4.14))
  (dune (>= 3.0))
  (stripe-core (= :version))
  yojson
  uri
  (alcotest :with-test)))
