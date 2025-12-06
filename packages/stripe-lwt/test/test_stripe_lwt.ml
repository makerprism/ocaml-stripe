(** Tests for stripe-lwt *)

(* Basic tests - more comprehensive tests require stripe-mock *)

let test_module_exists () =
  (* Just verify the module compiles and links *)
  let _ = Stripe_lwt.get in
  let _ = Stripe_lwt.post in
  let _ = Stripe_lwt.delete in
  Alcotest.(check bool) "module exists" true true

let () =
  Alcotest.run "Stripe Lwt" [
    "module", [
      Alcotest.test_case "exists" `Quick test_module_exists;
    ];
  ]
