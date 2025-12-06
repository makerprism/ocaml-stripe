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
