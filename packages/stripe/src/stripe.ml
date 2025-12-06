(** Stripe API Client for OCaml *)

module Core = Stripe_core

(** Re-export core types *)
type config = Core.config
type stripe_error = Core.stripe_error
type error_type = Core.error_type
type 'a api_result = 'a Core.api_result

let default_config = Core.default_config

(** Stripe Object - base type for all Stripe resources *)
module Stripe_object = struct
  type t = {
    id : string;
    object_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      raw = json;
    }

  let to_json t = t.raw

  let get_string t key =
    let open Yojson.Safe.Util in
    t.raw |> member key |> to_string_option

  let get_int t key =
    let open Yojson.Safe.Util in
    t.raw |> member key |> to_int_option

  let get_bool t key =
    let open Yojson.Safe.Util in
    t.raw |> member key |> to_bool_option
end

(** Address type used in many Stripe resources *)
module Address = struct
  type t = {
    city : string option;
    country : string option;
    line1 : string option;
    line2 : string option;
    postal_code : string option;
    state : string option;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      city = json |> member "city" |> to_string_option;
      country = json |> member "country" |> to_string_option;
      line1 = json |> member "line1" |> to_string_option;
      line2 = json |> member "line2" |> to_string_option;
      postal_code = json |> member "postal_code" |> to_string_option;
      state = json |> member "state" |> to_string_option;
    }

  let to_params ?prefix t =
    let p = match prefix with Some p -> p ^ "[" | None -> "" in
    let s = match prefix with Some _ -> "]" | None -> "" in
    List.filter_map Fun.id [
      Option.map (fun v -> (p ^ "city" ^ s, v)) t.city;
      Option.map (fun v -> (p ^ "country" ^ s, v)) t.country;
      Option.map (fun v -> (p ^ "line1" ^ s, v)) t.line1;
      Option.map (fun v -> (p ^ "line2" ^ s, v)) t.line2;
      Option.map (fun v -> (p ^ "postal_code" ^ s, v)) t.postal_code;
      Option.map (fun v -> (p ^ "state" ^ s, v)) t.state;
    ]
end

(** Customer resource *)
module Customer = struct
  type t = {
    id : string;
    object_ : string;
    address : Address.t option;
    balance : int;
    created : int;
    currency : string option;
    default_source : string option;
    delinquent : bool option;
    description : string option;
    email : string option;
    invoice_prefix : string option;
    livemode : bool;
    name : string option;
    phone : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      address = (
        let addr = json |> member "address" in
        if addr = `Null then None else Some (Address.of_json addr)
      );
      balance = json |> member "balance" |> to_int_option |> Option.value ~default:0;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string_option;
      default_source = json |> member "default_source" |> to_string_option;
      delinquent = json |> member "delinquent" |> to_bool_option;
      description = json |> member "description" |> to_string_option;
      email = json |> member "email" |> to_string_option;
      invoice_prefix = json |> member "invoice_prefix" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      name = json |> member "name" |> to_string_option;
      phone = json |> member "phone" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Charge resource *)
module Charge = struct
  type status = Succeeded | Pending | Failed

  let status_of_string = function
    | "succeeded" -> Succeeded
    | "pending" -> Pending
    | "failed" -> Failed
    | _ -> Pending

  type t = {
    id : string;
    object_ : string;
    amount : int;
    amount_captured : int;
    amount_refunded : int;
    captured : bool;
    created : int;
    currency : string;
    customer : string option;
    description : string option;
    failure_code : string option;
    failure_message : string option;
    livemode : bool;
    paid : bool;
    payment_intent : string option;
    refunded : bool;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_captured = json |> member "amount_captured" |> to_int_option |> Option.value ~default:0;
      amount_refunded = json |> member "amount_refunded" |> to_int_option |> Option.value ~default:0;
      captured = json |> member "captured" |> to_bool;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      failure_code = json |> member "failure_code" |> to_string_option;
      failure_message = json |> member "failure_message" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      paid = json |> member "paid" |> to_bool;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      refunded = json |> member "refunded" |> to_bool;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** PaymentIntent resource *)
module Payment_intent = struct
  type status =
    | Requires_payment_method
    | Requires_confirmation
    | Requires_action
    | Processing
    | Requires_capture
    | Canceled
    | Succeeded

  let status_of_string = function
    | "requires_payment_method" -> Requires_payment_method
    | "requires_confirmation" -> Requires_confirmation
    | "requires_action" -> Requires_action
    | "processing" -> Processing
    | "requires_capture" -> Requires_capture
    | "canceled" -> Canceled
    | "succeeded" -> Succeeded
    | _ -> Requires_payment_method

  type t = {
    id : string;
    object_ : string;
    amount : int;
    amount_received : int;
    capture_method : string;
    client_secret : string option;
    confirmation_method : string;
    created : int;
    currency : string;
    customer : string option;
    description : string option;
    livemode : bool;
    payment_method : string option;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_received = json |> member "amount_received" |> to_int_option |> Option.value ~default:0;
      capture_method = json |> member "capture_method" |> to_string_option |> Option.value ~default:"automatic";
      client_secret = json |> member "client_secret" |> to_string_option;
      confirmation_method = json |> member "confirmation_method" |> to_string_option |> Option.value ~default:"automatic";
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      payment_method = json |> member "payment_method" |> to_string_option;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Refund resource *)
module Refund = struct
  type status = Pending | Succeeded | Failed | Canceled

  let status_of_string = function
    | "pending" -> Pending
    | "succeeded" -> Succeeded
    | "failed" -> Failed
    | "canceled" -> Canceled
    | _ -> Pending

  type t = {
    id : string;
    object_ : string;
    amount : int;
    charge : string option;
    created : int;
    currency : string;
    payment_intent : string option;
    reason : string option;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      charge = json |> member "charge" |> to_string_option;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      reason = json |> member "reason" |> to_string_option;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Balance resource *)
module Balance = struct
  type balance_amount = {
    amount : int;
    currency : string;
  }

  let balance_amount_of_json json =
    let open Yojson.Safe.Util in
    {
      amount = json |> member "amount" |> to_int;
      currency = json |> member "currency" |> to_string;
    }

  type t = {
    object_ : string;
    available : balance_amount list;
    pending : balance_amount list;
    livemode : bool;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      object_ = json |> member "object" |> to_string;
      available = json |> member "available" |> to_list |> List.map balance_amount_of_json;
      pending = json |> member "pending" |> to_list |> List.map balance_amount_of_json;
      livemode = json |> member "livemode" |> to_bool;
      raw = json;
    }

  let to_json t = t.raw
end

(** Event resource for webhooks *)
module Event = struct
  type t = {
    id : string;
    object_ : string;
    api_version : string option;
    created : int;
    data : Yojson.Safe.t;
    livemode : bool;
    pending_webhooks : int;
    request : Yojson.Safe.t option;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      api_version = json |> member "api_version" |> to_string_option;
      created = json |> member "created" |> to_int;
      data = json |> member "data";
      livemode = json |> member "livemode" |> to_bool;
      pending_webhooks = json |> member "pending_webhooks" |> to_int;
      request = (
        let req = json |> member "request" in
        if req = `Null then None else Some req
      );
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Subscription resource *)
module Subscription = struct
  type status =
    | Active
    | Past_due
    | Unpaid
    | Canceled
    | Incomplete
    | Incomplete_expired
    | Trialing
    | Paused

  let status_of_string = function
    | "active" -> Active
    | "past_due" -> Past_due
    | "unpaid" -> Unpaid
    | "canceled" -> Canceled
    | "incomplete" -> Incomplete
    | "incomplete_expired" -> Incomplete_expired
    | "trialing" -> Trialing
    | "paused" -> Paused
    | _ -> Active

  type t = {
    id : string;
    object_ : string;
    cancel_at_period_end : bool;
    current_period_end : int;
    current_period_start : int;
    customer : string;
    default_payment_method : string option;
    livemode : bool;
    status : status;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      cancel_at_period_end = json |> member "cancel_at_period_end" |> to_bool;
      current_period_end = json |> member "current_period_end" |> to_int;
      current_period_start = json |> member "current_period_start" |> to_int;
      customer = json |> member "customer" |> to_string;
      default_payment_method = json |> member "default_payment_method" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string |> status_of_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Product resource *)
module Product = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    created : int;
    description : string option;
    livemode : bool;
    name : string;
    updated : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      created = json |> member "created" |> to_int;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      name = json |> member "name" |> to_string;
      updated = json |> member "updated" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** Price resource *)
module Price = struct
  type recurring = {
    interval : string;
    interval_count : int;
  }

  let recurring_of_json json =
    let open Yojson.Safe.Util in
    {
      interval = json |> member "interval" |> to_string;
      interval_count = json |> member "interval_count" |> to_int;
    }

  type t = {
    id : string;
    object_ : string;
    active : bool;
    billing_scheme : string;
    created : int;
    currency : string;
    livemode : bool;
    product : string;
    recurring : recurring option;
    unit_amount : int option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      billing_scheme = json |> member "billing_scheme" |> to_string;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      product = json |> member "product" |> to_string;
      recurring = (
        let rec_json = json |> member "recurring" in
        if rec_json = `Null then None else Some (recurring_of_json rec_json)
      );
      unit_amount = json |> member "unit_amount" |> to_int_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Invoice resource *)
module Invoice = struct
  type status =
    | Draft
    | Open
    | Paid
    | Uncollectible
    | Void

  let status_of_string = function
    | "draft" -> Draft
    | "open" -> Open
    | "paid" -> Paid
    | "uncollectible" -> Uncollectible
    | "void" -> Void
    | _ -> Draft

  type t = {
    id : string;
    object_ : string;
    amount_due : int;
    amount_paid : int;
    amount_remaining : int;
    created : int;
    currency : string;
    customer : string;
    livemode : bool;
    status : status option;
    subscription : string option;
    total : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount_due = json |> member "amount_due" |> to_int;
      amount_paid = json |> member "amount_paid" |> to_int;
      amount_remaining = json |> member "amount_remaining" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string_option |> Option.map status_of_string;
      subscription = json |> member "subscription" |> to_string_option;
      total = json |> member "total" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** PaymentMethod resource *)
module Payment_method = struct
  type card = {
    brand : string;
    exp_month : int;
    exp_year : int;
    last4 : string;
  }

  let card_of_json json =
    let open Yojson.Safe.Util in
    {
      brand = json |> member "brand" |> to_string;
      exp_month = json |> member "exp_month" |> to_int;
      exp_year = json |> member "exp_year" |> to_int;
      last4 = json |> member "last4" |> to_string;
    }

  type t = {
    id : string;
    object_ : string;
    billing_details : Yojson.Safe.t;
    card : card option;
    created : int;
    customer : string option;
    livemode : bool;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      billing_details = json |> member "billing_details";
      card = (
        let c = json |> member "card" in
        if c = `Null then None else Some (card_of_json c)
      );
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** SetupIntent resource *)
module Setup_intent = struct
  type status =
    | Requires_payment_method
    | Requires_confirmation
    | Requires_action
    | Processing
    | Canceled
    | Succeeded

  let status_of_string = function
    | "requires_payment_method" -> Requires_payment_method
    | "requires_confirmation" -> Requires_confirmation
    | "requires_action" -> Requires_action
    | "processing" -> Processing
    | "canceled" -> Canceled
    | "succeeded" -> Succeeded
    | _ -> Requires_payment_method

  type t = {
    id : string;
    object_ : string;
    client_secret : string option;
    created : int;
    customer : string option;
    description : string option;
    livemode : bool;
    payment_method : string option;
    status : status;
    usage : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      client_secret = json |> member "client_secret" |> to_string_option;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      payment_method = json |> member "payment_method" |> to_string_option;
      status = json |> member "status" |> to_string |> status_of_string;
      usage = json |> member "usage" |> to_string_option |> Option.value ~default:"off_session";
      raw = json;
    }

  let to_json t = t.raw
end

(** Coupon resource *)
module Coupon = struct
  type duration = Once | Repeating | Forever

  let duration_of_string = function
    | "once" -> Once
    | "repeating" -> Repeating
    | "forever" -> Forever
    | _ -> Once

  type t = {
    id : string;
    object_ : string;
    amount_off : int option;
    created : int;
    currency : string option;
    duration : duration;
    duration_in_months : int option;
    livemode : bool;
    max_redemptions : int option;
    name : string option;
    percent_off : float option;
    times_redeemed : int;
    valid : bool;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount_off = json |> member "amount_off" |> to_int_option;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string_option;
      duration = json |> member "duration" |> to_string |> duration_of_string;
      duration_in_months = json |> member "duration_in_months" |> to_int_option;
      livemode = json |> member "livemode" |> to_bool;
      max_redemptions = json |> member "max_redemptions" |> to_int_option;
      name = json |> member "name" |> to_string_option;
      percent_off = json |> member "percent_off" |> to_float_option;
      times_redeemed = json |> member "times_redeemed" |> to_int;
      valid = json |> member "valid" |> to_bool;
      raw = json;
    }

  let to_json t = t.raw
end

(** Discount resource *)
module Discount = struct
  type t = {
    id : string;
    object_ : string;
    coupon : Coupon.t;
    customer : string option;
    start : int;
    end_ : int option;
    subscription : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      coupon = json |> member "coupon" |> Coupon.of_json;
      customer = json |> member "customer" |> to_string_option;
      start = json |> member "start" |> to_int;
      end_ = json |> member "end" |> to_int_option;
      subscription = json |> member "subscription" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** BalanceTransaction resource *)
module Balance_transaction = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    available_on : int;
    created : int;
    currency : string;
    description : string option;
    fee : int;
    net : int;
    source : string option;
    status : string;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      available_on = json |> member "available_on" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      fee = json |> member "fee" |> to_int;
      net = json |> member "net" |> to_int;
      source = json |> member "source" |> to_string_option;
      status = json |> member "status" |> to_string;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Payout resource *)
module Payout = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    arrival_date : int;
    created : int;
    currency : string;
    description : string option;
    destination : string option;
    livemode : bool;
    method_ : string;
    status : string;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      arrival_date = json |> member "arrival_date" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      destination = json |> member "destination" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      method_ = json |> member "method" |> to_string;
      status = json |> member "status" |> to_string;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Checkout Session resource *)
module Checkout_session = struct
  type line_item = {
    id : string;
    object_ : string;
    amount_total : int;
    currency : string;
    description : string option;
    quantity : int option;
  }

  type t = {
    id : string;
    object_ : string;
    cancel_url : string option;
    client_reference_id : string option;
    currency : string option;
    customer : string option;
    customer_email : string option;
    livemode : bool;
    mode : string;
    payment_intent : string option;
    payment_status : string;
    status : string option;
    success_url : string option;
    url : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      cancel_url = json |> member "cancel_url" |> to_string_option;
      client_reference_id = json |> member "client_reference_id" |> to_string_option;
      currency = json |> member "currency" |> to_string_option;
      customer = json |> member "customer" |> to_string_option;
      customer_email = json |> member "customer_email" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      mode = json |> member "mode" |> to_string;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      payment_status = json |> member "payment_status" |> to_string;
      status = json |> member "status" |> to_string_option;
      success_url = json |> member "success_url" |> to_string_option;
      url = json |> member "url" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** TaxRate resource *)
module Tax_rate = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    country : string option;
    description : string option;
    display_name : string;
    inclusive : bool;
    jurisdiction : string option;
    percentage : float;
    state : string option;
    tax_type : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      country = json |> member "country" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      display_name = json |> member "display_name" |> to_string;
      inclusive = json |> member "inclusive" |> to_bool;
      jurisdiction = json |> member "jurisdiction" |> to_string_option;
      percentage = json |> member "percentage" |> to_float;
      state = json |> member "state" |> to_string_option;
      tax_type = json |> member "tax_type" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** PaymentLink resource *)
module Payment_link = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    currency : string option;
    livemode : bool;
    url : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      currency = json |> member "currency" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      url = json |> member "url" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Dispute resource *)
module Dispute = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    charge : string;
    currency : string;
    created : int;
    is_charge_refundable : bool;
    livemode : bool;
    payment_intent : string option;
    reason : string;
    status : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      charge = json |> member "charge" |> to_string;
      currency = json |> member "currency" |> to_string;
      created = json |> member "created" |> to_int;
      is_charge_refundable = json |> member "is_charge_refundable" |> to_bool;
      livemode = json |> member "livemode" |> to_bool;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      reason = json |> member "reason" |> to_string;
      status = json |> member "status" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Account resource (Connect) *)
module Account = struct
  type business_profile = {
    mcc : string option;
    name : string option;
    url : string option;
  }

  type t = {
    id : string;
    object_ : string;
    business_type : string option;
    charges_enabled : bool;
    country : string;
    created : int;
    default_currency : string option;
    details_submitted : bool;
    email : string option;
    payouts_enabled : bool;
    type_ : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      business_type = json |> member "business_type" |> to_string_option;
      charges_enabled = json |> member "charges_enabled" |> to_bool;
      country = json |> member "country" |> to_string;
      created = json |> member "created" |> to_int;
      default_currency = json |> member "default_currency" |> to_string_option;
      details_submitted = json |> member "details_submitted" |> to_bool;
      email = json |> member "email" |> to_string_option;
      payouts_enabled = json |> member "payouts_enabled" |> to_bool;
      type_ = json |> member "type" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Transfer resource (Connect) *)
module Transfer = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    amount_reversed : int;
    created : int;
    currency : string;
    description : string option;
    destination : string;
    livemode : bool;
    reversed : bool;
    source_transaction : string option;
    source_type : string;
    transfer_group : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_reversed = json |> member "amount_reversed" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      destination = json |> member "destination" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      reversed = json |> member "reversed" |> to_bool;
      source_transaction = json |> member "source_transaction" |> to_string_option;
      source_type = json |> member "source_type" |> to_string;
      transfer_group = json |> member "transfer_group" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** File resource *)
module File = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    expires_at : int option;
    filename : string option;
    purpose : string;
    size : int;
    type_ : string option;
    url : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      expires_at = json |> member "expires_at" |> to_int_option;
      filename = json |> member "filename" |> to_string_option;
      purpose = json |> member "purpose" |> to_string;
      size = json |> member "size" |> to_int;
      type_ = json |> member "type" |> to_string_option;
      url = json |> member "url" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** FileLink resource *)
module File_link = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    expired : bool;
    expires_at : int option;
    file : string;
    livemode : bool;
    url : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      expired = json |> member "expired" |> to_bool;
      expires_at = json |> member "expires_at" |> to_int_option;
      file = json |> member "file" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      url = json |> member "url" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Mandate resource *)
module Mandate = struct
  type customer_acceptance = {
    accepted_at : int option;
    type_ : string;
  }

  type t = {
    id : string;
    object_ : string;
    livemode : bool;
    payment_method : string;
    status : string;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      payment_method = json |> member "payment_method" |> to_string;
      status = json |> member "status" |> to_string;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Review resource (Radar) *)
module Review = struct
  type t = {
    id : string;
    object_ : string;
    charge : string option;
    created : int;
    livemode : bool;
    open_ : bool;
    payment_intent : string option;
    reason : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      charge = json |> member "charge" |> to_string_option;
      created = json |> member "created" |> to_int;
      livemode = json |> member "livemode" |> to_bool;
      open_ = json |> member "open" |> to_bool;
      payment_intent = json |> member "payment_intent" |> to_string_option;
      reason = json |> member "reason" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** PromotionCode resource *)
module Promotion_code = struct
  type t = {
    id : string;
    object_ : string;
    active : bool;
    code : string;
    coupon : Coupon.t;
    created : int;
    customer : string option;
    expires_at : int option;
    livemode : bool;
    max_redemptions : int option;
    times_redeemed : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      active = json |> member "active" |> to_bool;
      code = json |> member "code" |> to_string;
      coupon = json |> member "coupon" |> Coupon.of_json;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string_option;
      expires_at = json |> member "expires_at" |> to_int_option;
      livemode = json |> member "livemode" |> to_bool;
      max_redemptions = json |> member "max_redemptions" |> to_int_option;
      times_redeemed = json |> member "times_redeemed" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** InvoiceItem resource *)
module Invoice_item = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    currency : string;
    customer : string;
    date : int;
    description : string option;
    discountable : bool;
    invoice : string option;
    livemode : bool;
    period_start : int;
    period_end : int;
    price : Price.t option;
    proration : bool;
    quantity : int;
    subscription : string option;
    unit_amount : int option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    let period = json |> member "period" in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      date = json |> member "date" |> to_int;
      description = json |> member "description" |> to_string_option;
      discountable = json |> member "discountable" |> to_bool;
      invoice = json |> member "invoice" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      period_start = period |> member "start" |> to_int;
      period_end = period |> member "end" |> to_int;
      price = (match json |> member "price" with
        | `Null -> None
        | p -> Some (Price.of_json p));
      proration = json |> member "proration" |> to_bool;
      quantity = json |> member "quantity" |> to_int;
      subscription = json |> member "subscription" |> to_string_option;
      unit_amount = json |> member "unit_amount" |> to_int_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** Quote resource *)
module Quote = struct
  type t = {
    id : string;
    object_ : string;
    amount_subtotal : int;
    amount_total : int;
    created : int;
    currency : string option;
    customer : string option;
    description : string option;
    expires_at : int;
    livemode : bool;
    status : string;
    subscription : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount_subtotal = json |> member "amount_subtotal" |> to_int;
      amount_total = json |> member "amount_total" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string_option;
      customer = json |> member "customer" |> to_string_option;
      description = json |> member "description" |> to_string_option;
      expires_at = json |> member "expires_at" |> to_int;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string;
      subscription = json |> member "subscription" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** CreditNote resource *)
module Credit_note = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    created : int;
    currency : string;
    customer : string;
    invoice : string;
    livemode : bool;
    memo : string option;
    number : string;
    out_of_band_amount : int option;
    reason : string option;
    refund : string option;
    status : string;
    subtotal : int;
    total : int;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      invoice = json |> member "invoice" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      memo = json |> member "memo" |> to_string_option;
      number = json |> member "number" |> to_string;
      out_of_band_amount = json |> member "out_of_band_amount" |> to_int_option;
      reason = json |> member "reason" |> to_string_option;
      refund = json |> member "refund" |> to_string_option;
      status = json |> member "status" |> to_string;
      subtotal = json |> member "subtotal" |> to_int;
      total = json |> member "total" |> to_int;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** ApplicationFee resource *)
module Application_fee = struct
  type t = {
    id : string;
    object_ : string;
    account : string;
    amount : int;
    amount_refunded : int;
    application : string;
    charge : string;
    created : int;
    currency : string;
    livemode : bool;
    refunded : bool;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      account = json |> member "account" |> to_string;
      amount = json |> member "amount" |> to_int;
      amount_refunded = json |> member "amount_refunded" |> to_int;
      application = json |> member "application" |> to_string;
      charge = json |> member "charge" |> to_string;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      refunded = json |> member "refunded" |> to_bool;
      raw = json;
    }

  let to_json t = t.raw
end

(** Topup resource *)
module Topup = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    created : int;
    currency : string;
    description : string option;
    livemode : bool;
    source : string option;
    status : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      description = json |> member "description" |> to_string_option;
      livemode = json |> member "livemode" |> to_bool;
      source = json |> member "source" |> to_string_option;
      status = json |> member "status" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** UsageRecord resource (for metered billing) *)
module Usage_record = struct
  type t = {
    id : string;
    object_ : string;
    livemode : bool;
    quantity : int;
    subscription_item : string;
    timestamp : int;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      quantity = json |> member "quantity" |> to_int;
      subscription_item = json |> member "subscription_item" |> to_string;
      timestamp = json |> member "timestamp" |> to_int;
      raw = json;
    }

  let to_json t = t.raw
end

(** SubscriptionItem resource *)
module Subscription_item = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    price : Price.t;
    quantity : int option;
    subscription : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      price = json |> member "price" |> Price.of_json;
      quantity = json |> member "quantity" |> to_int_option;
      subscription = json |> member "subscription" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** SubscriptionSchedule resource *)
module Subscription_schedule = struct
  type t = {
    id : string;
    object_ : string;
    canceled_at : int option;
    completed_at : int option;
    created : int;
    customer : string;
    end_behavior : string;
    livemode : bool;
    status : string;
    subscription : string option;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      canceled_at = json |> member "canceled_at" |> to_int_option;
      completed_at = json |> member "completed_at" |> to_int_option;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string;
      end_behavior = json |> member "end_behavior" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      status = json |> member "status" |> to_string;
      subscription = json |> member "subscription" |> to_string_option;
      raw = json;
    }

  let to_json t = t.raw
end

(** List response wrapper *)
module List_response = struct
  type 'a t = {
    data : 'a list;
    has_more : bool;
    url : string;
  }

  let of_json parse_item json =
    let open Yojson.Safe.Util in
    {
      data = json |> member "data" |> to_list |> List.map parse_item;
      has_more = json |> member "has_more" |> to_bool;
      url = json |> member "url" |> to_string;
    }
end

(** Deleted response wrapper *)
module Deleted = struct
  type t = {
    id : string;
    object_ : string;
    deleted : bool;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      deleted = json |> member "deleted" |> to_bool;
    }
end

(** Billing Portal Session resource *)
module Billing_portal_session = struct
  type t = {
    id : string;
    object_ : string;
    created : int;
    customer : string;
    livemode : bool;
    return_url : string;
    url : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      created = json |> member "created" |> to_int;
      customer = json |> member "customer" |> to_string;
      livemode = json |> member "livemode" |> to_bool;
      return_url = json |> member "return_url" |> to_string;
      url = json |> member "url" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end

(** Customer Balance Transaction resource *)
module Customer_balance_transaction = struct
  type t = {
    id : string;
    object_ : string;
    amount : int;
    created : int;
    currency : string;
    customer : string;
    description : string option;
    ending_balance : int;
    livemode : bool;
    type_ : string;
    raw : Yojson.Safe.t;
  }

  let of_json json =
    let open Yojson.Safe.Util in
    {
      id = json |> member "id" |> to_string;
      object_ = json |> member "object" |> to_string;
      amount = json |> member "amount" |> to_int;
      created = json |> member "created" |> to_int;
      currency = json |> member "currency" |> to_string;
      customer = json |> member "customer" |> to_string;
      description = json |> member "description" |> to_string_option;
      ending_balance = json |> member "ending_balance" |> to_int;
      livemode = json |> member "livemode" |> to_bool;
      type_ = json |> member "type" |> to_string;
      raw = json;
    }

  let to_json t = t.raw
end
