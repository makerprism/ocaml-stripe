(** Tests for stripe-lwt *)

(* Basic tests - more comprehensive tests require stripe-mock *)

let test_module_exists () =
  (* Just verify the module compiles and links *)
  let _ = Stripe_lwt.get in
  let _ = Stripe_lwt.post in
  let _ = Stripe_lwt.delete in
  Alcotest.(check bool) "module exists" true true

let test_client_create () =
  let config = Stripe_lwt.Client.create ~api_key:"sk_test_123" in
  Alcotest.(check string) "api_key" "sk_test_123" config.api_key;
  Alcotest.(check string) "api_base" "https://api.stripe.com" config.api_base

let () =
  Alcotest.run "Stripe Lwt" [
    "module", [
      Alcotest.test_case "exists" `Quick test_module_exists;
      Alcotest.test_case "client_create" `Quick test_client_create;
    ];
  ]
