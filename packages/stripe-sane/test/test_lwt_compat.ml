(** Test that stripe-sane can be used with stripe-lwt via an adapter.
    
    Because STRIPE_CLIENT has a specific signature, and Stripe_lwt.Client
    has additional optional parameters, users need to create a thin adapter. *)

module Lwt_client : Stripe_sane.STRIPE_CLIENT with type 'a io = 'a Lwt.t = struct
  type 'a io = 'a Lwt.t
  let bind = Lwt.bind
  let return = Lwt.return

  module Customer = struct
    let list = Stripe_lwt.Client.Customer.list
    let create = Stripe_lwt.Client.Customer.create
  end

  module Subscription = struct
    let list = Stripe_lwt.Client.Subscription.list

    (* Adapter: forward only the parameters that STRIPE_CLIENT supports *)
    let create ~config ~customer ~price ?idempotency_key
        ?default_payment_method ?trial_period_days ?coupon ?promotion_code
        ?automatic_tax ?description ?metadata () =
      Stripe_lwt.Client.Subscription.create
        ~config ~customer ~price ?idempotency_key
        ?default_payment_method ?trial_period_days ?coupon ?promotion_code
        ?automatic_tax ?description ?metadata ()
  end
end

module Sane = Stripe_sane.Make (Lwt_client)

(* Verify the module compiles and has the expected functions *)
let _ = Sane.Customer.get_or_create
let _ = Sane.Subscription.create_if_not_subscribed
