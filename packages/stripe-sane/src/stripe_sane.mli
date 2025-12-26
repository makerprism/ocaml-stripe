(** Stripe Sane - Opinionated convenience helpers for Stripe.
    
    This package provides higher-level patterns that are NOT part of
    the official Stripe API. The official Stripe SDKs intentionally
    leave these patterns to application developers.
    
    Key assumptions:
    - Email addresses uniquely identify customers (normalized to lowercase)
    - A customer should not have duplicate active subscriptions to the same price
    
    {1 Idempotency Keys}
    
    This library uses idempotency keys to prevent duplicate operations during
    retries or race conditions. An idempotency key is a unique string sent with
    a Stripe API request that ensures the same operation isn't performed twice.
    
    For example, if you call [Customer.get_or_create] twice with the same email
    within 24 hours (Stripe's idempotency window), and the first call created
    a customer, the second call will return the same customer rather than
    creating a duplicate.
    
    The idempotency keys used are:
    - Customer creation: ["stripe_sane_customer_<normalized_email>"]
    - Subscription creation: ["stripe_sane_sub_<customer_id>_<price_id>"]
    
    @see <https://github.com/makerprism/ocaml-stripe> for the core SDK *)

(** {1 Result Types} *)

(** Result of a get_or_create operation *)
type 'a lookup_result = 
  | Created of 'a  (** A new resource was created *)
  | Found of 'a    (** An existing resource was found *)

(** Extract the value regardless of how it was obtained *)
val lookup_value : 'a lookup_result -> 'a

(** {1 Errors} *)

(** Errors specific to stripe-sane convenience functions *)
type error =
  | Stripe_error of Stripe_core.stripe_error
      (** An error from the Stripe API *)
  | Already_subscribed of Stripe.Subscription.t
      (** Customer already has an active/trialing subscription to this price *)

(** {1 Email Normalization} *)

(** Normalize an email address for consistent lookups.
    - Trims leading/trailing whitespace
    - Converts to lowercase
    
    This is applied automatically by [Customer.get_or_create]. *)
val normalize_email : string -> string

(** {1 Client Module Type} *)

(** Module type for the Stripe client operations required by stripe-sane.
    
    This allows stripe-sane to work with any Stripe client implementation
    (Lwt, Eio, etc.) without depending on a specific runtime. *)
module type STRIPE_CLIENT = sig
  (** The IO monad type (e.g., Lwt.t, Eio.Promise.t) *)
  type 'a io
  
  (** Monadic bind *)
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  
  (** Monadic return *)
  val return : 'a -> 'a io

  (** Customer operations *)
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

  (** Subscription operations *)
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

(** {1 Functor} *)

(** Create convenience functions from a Stripe client implementation.
    
    Example with Lwt:
    {[
      (* Define the client adapter once in your project *)
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

          (* Forward only the parameters that STRIPE_CLIENT supports *)
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
      
      let subscribe_user ~config ~email ~price =
        let open Lwt.Syntax in
        let* result = Sane.Customer.get_or_create ~config ~email () in
        match result with
        | Error e -> Lwt.return_error (Stripe_sane.Stripe_error e)
        | Ok lookup ->
          let customer = Stripe_sane.lookup_value lookup in
          Sane.Subscription.create_if_not_subscribed 
            ~config ~customer:customer.id ~price ()
    ]} *)
module Make (Client : STRIPE_CLIENT) : sig
  
  (** {2 Customer Helpers} *)
  
  module Customer : sig
    (** Find existing customer by email or create a new one.
        
        The email is normalized (lowercase, trimmed) before lookup.
        Uses an idempotency key based on the normalized email to prevent
        race conditions during creation.
        
        {b Note:} Stripe allows multiple customers with the same email.
        This function returns the first customer found. In a well-designed
        application, each email should correspond to exactly one customer.
        
        @param config Stripe API configuration
        @param email Customer email address (will be normalized)
        @param name Optional customer name
        @param description Optional customer description
        @param phone Optional customer phone
        @param metadata Optional key-value metadata
        @return [Ok (Found customer)] if a customer with this email exists,
                [Ok (Created customer)] if a new customer was created,
                [Error e] if a Stripe API error occurred *)
    val get_or_create :
      config:Stripe_core.config ->
      email:string ->
      ?name:string ->
      ?description:string ->
      ?phone:string ->
      ?metadata:(string * string) list ->
      unit ->
      (Stripe.Customer.t lookup_result, Stripe_core.stripe_error) result Client.io
  end

  (** {2 Subscription Helpers} *)
  
  module Subscription : sig
    (** Create a subscription only if the customer doesn't already have one
        to the specified price.
        
        Checks for existing subscriptions with status [active] or [trialing]
        that include the given price ID.
        
        @param config Stripe API configuration
        @param customer Customer ID (cus_xxx)
        @param price Price ID (price_xxx)
        @param default_payment_method Optional default payment method ID
        @param trial_period_days Optional number of trial days
        @param coupon Optional coupon ID to apply
        @param promotion_code Optional promotion code ID to apply
        @param automatic_tax Enable automatic tax calculation
        @param description Optional subscription description
        @param metadata Optional key-value metadata
        @return [Ok subscription] if a new subscription was created,
                [Error (Already_subscribed sub)] if customer already has
                an active/trialing subscription to this price,
                [Error (Stripe_error e)] if a Stripe API error occurred *)
    val create_if_not_subscribed :
      config:Stripe_core.config ->
      customer:string ->
      price:string ->
      ?default_payment_method:string ->
      ?trial_period_days:int ->
      ?coupon:string ->
      ?promotion_code:string ->
      ?automatic_tax:bool ->
      ?description:string ->
      ?metadata:(string * string) list ->
      unit ->
      (Stripe.Subscription.t, error) result Client.io
  end
end
