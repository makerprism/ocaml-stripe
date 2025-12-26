(** Stripe Sane - Opinionated convenience helpers for Stripe *)

type 'a lookup_result = 
  | Created of 'a
  | Found of 'a

let lookup_value = function
  | Created x -> x
  | Found x -> x

type error =
  | Stripe_error of Stripe_core.stripe_error
  | Already_subscribed of Stripe.Subscription.t

let normalize_email email =
  email |> String.trim |> String.lowercase_ascii

module type STRIPE_CLIENT = sig
  type 'a io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io

  module Customer : sig
    val list :
      config:Stripe_core.config ->
      ?limit:int ->
      ?starting_after:string ->
      ?ending_before:string ->
      ?email:string ->
      unit ->
      (Stripe.Customer.t Stripe.List_response.t, Stripe_core.stripe_error) result io

    val create :
      config:Stripe_core.config ->
      ?idempotency_key:string ->
      ?email:string ->
      ?name:string ->
      ?description:string ->
      ?phone:string ->
      ?metadata:(string * string) list ->
      unit ->
      (Stripe.Customer.t, Stripe_core.stripe_error) result io
  end

  module Subscription : sig
    val list :
      config:Stripe_core.config ->
      ?limit:int ->
      ?starting_after:string ->
      ?customer:string ->
      ?status:string ->
      unit ->
      (Stripe.Subscription.t Stripe.List_response.t, Stripe_core.stripe_error) result io

    val create :
      config:Stripe_core.config ->
      customer:string ->
      price:string ->
      ?idempotency_key:string ->
      ?default_payment_method:string ->
      ?trial_period_days:int ->
      ?coupon:string ->
      ?promotion_code:string ->
      ?automatic_tax:bool ->
      ?description:string ->
      ?metadata:(string * string) list ->
      unit ->
      (Stripe.Subscription.t, Stripe_core.stripe_error) result io
  end
end

module Make (Client : STRIPE_CLIENT) = struct
  let ( >>= ) = Client.bind
  
  module Customer = struct
    let get_or_create ~config ~email ?name ?description ?phone ?metadata () =
      let normalized_email = normalize_email email in
      Client.Customer.list ~config ~email:normalized_email ~limit:1 () >>= function
      | Error e -> Client.return (Error e)
      | Ok response ->
        match response.data with
        | customer :: _ -> 
          Client.return (Ok (Found customer))
        | [] ->
          let idempotency_key = "stripe_sane_customer_" ^ normalized_email in
          Client.Customer.create 
            ~config 
            ~idempotency_key 
            ~email:normalized_email 
            ?name 
            ?description 
            ?phone
            ?metadata 
            () >>= function
          | Error e -> Client.return (Error e)
          | Ok customer -> Client.return (Ok (Created customer))
  end

  module Subscription = struct
    (** Check if a subscription contains the given price ID.
        Inspects the subscription's raw JSON for items containing the price. *)
    let subscription_has_price ~price (sub : Stripe.Subscription.t) =
      let open Yojson.Safe.Util in
      try
        let items = sub.raw |> member "items" |> member "data" |> to_list in
        List.exists (fun item ->
          let item_price = item |> member "price" |> member "id" |> to_string in
          String.equal item_price price
        ) items
      with _ -> false

    (** Find a subscription with the given price, paginating through all results.
        Returns the first matching subscription or None. *)
    let rec find_subscription_with_price ~config ~customer ~status ~price ?starting_after () =
      Client.Subscription.list ~config ~customer ~status ?starting_after ~limit:100 () >>= function
      | Error e -> Client.return (Error e)
      | Ok response ->
        match List.find_opt (subscription_has_price ~price) response.data with
        | Some sub -> Client.return (Ok (Some sub))
        | None ->
          if response.has_more then
            match List.rev response.data with
            | [] -> Client.return (Ok None)
            | last :: _ -> 
              find_subscription_with_price ~config ~customer ~status ~price 
                ~starting_after:last.id ()
          else
            Client.return (Ok None)

    let create_if_not_subscribed ~config ~customer ~price 
        ?default_payment_method ?trial_period_days ?coupon ?promotion_code 
        ?automatic_tax ?description ?metadata () =
      (* Check active subscriptions (with pagination) *)
      find_subscription_with_price ~config ~customer ~status:"active" ~price () >>= function
      | Error e -> Client.return (Error (Stripe_error e))
      | Ok (Some existing) -> Client.return (Error (Already_subscribed existing))
      | Ok None ->
        (* Also check trialing subscriptions (with pagination) *)
        find_subscription_with_price ~config ~customer ~status:"trialing" ~price () >>= function
        | Error e -> Client.return (Error (Stripe_error e))
        | Ok (Some existing) -> Client.return (Error (Already_subscribed existing))
        | Ok None ->
          (* No existing subscription, create new one *)
          let idempotency_key = 
            Printf.sprintf "stripe_sane_sub_%s_%s" customer price 
          in
          Client.Subscription.create 
            ~config 
            ~customer 
            ~price 
            ~idempotency_key
            ?default_payment_method
            ?trial_period_days
            ?coupon
            ?promotion_code
            ?automatic_tax
            ?description
            ?metadata
            () >>= function
          | Error e -> Client.return (Error (Stripe_error e))
          | Ok sub -> Client.return (Ok sub)
  end
end
