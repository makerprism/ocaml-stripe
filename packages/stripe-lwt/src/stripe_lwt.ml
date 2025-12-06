(** Stripe Lwt - Lwt runtime adapter for Stripe API *)

open Lwt.Infix

module Lwt_http_client : Stripe_core.HTTP_CLIENT with type 'a io = 'a Lwt.t = struct
  type 'a io = 'a Lwt.t
  
  let cohttp_method_of_http_method : Stripe_core.http_method -> Cohttp.Code.meth = function
    | GET -> `GET
    | POST -> `POST
    | DELETE -> `DELETE
    | PUT -> `PUT
    | PATCH -> `PATCH
  
  let request ~meth ~url ~headers ?body () : Stripe_core.http_response Lwt.t =
    let uri = Uri.of_string url in
    let headers = Cohttp.Header.of_list headers in
    let body = match body with
      | Some b -> Cohttp_lwt.Body.of_string b
      | None -> Cohttp_lwt.Body.empty
    in
    let meth = cohttp_method_of_http_method meth in
    Cohttp_lwt_unix.Client.call ~headers ~body meth uri >>= fun (resp, body) ->
    let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
    let headers = Cohttp.Response.headers resp |> Cohttp.Header.to_list in
    Cohttp_lwt.Body.to_string body >|= fun body_str ->
    { Stripe_core.status_code; headers; body = body_str }
end

(** Re-export request options *)
type request_options = Stripe_core.request_options = {
  idempotency_key : string option;
  stripe_account : string option;
}

let default_request_options = Stripe_core.default_request_options
let generate_idempotency_key = Stripe_core.generate_idempotency_key

(** Make a Stripe API request using Lwt *)
let request
    ~(config : Stripe_core.config)
    ~(meth : Stripe_core.http_method)
    ~(path : string)
    ?(options : request_options = default_request_options)
    ?(params : (string * string) list = [])
    ()
  : Stripe_core.http_response Lwt.t =
  let url = config.api_base ^ path in
  let headers = Stripe_core.build_headers ~options config in
  let body = if List.length params > 0 then
    Some (Stripe_core.build_form_body params)
  else
    None
  in
  Lwt_http_client.request ~meth ~url ~headers ?body ()

(** Make a GET request *)
let get ~config ~path ?options ?params () =
  request ~config ~meth:GET ~path ?options ?params ()

(** Make a POST request *)
let post ~config ~path ?options ?params () =
  request ~config ~meth:POST ~path ?options ?params ()

(** Make a DELETE request *)
let delete ~config ~path ?options ?params () =
  request ~config ~meth:DELETE ~path ?options ?params ()

(** Parse response and handle errors *)
let handle_response ~parse_ok response =
  let open Stripe_core in
  if response.status_code >= 200 && response.status_code < 300 then
    let json = Yojson.Safe.from_string response.body in
    Lwt.return_ok (parse_ok json)
  else
    let json = Yojson.Safe.from_string response.body in
    match parse_error json with
    | Some err -> Lwt.return_error err
    | None -> 
      Lwt.return_error {
        error_type = Api_error;
        message = Printf.sprintf "HTTP %d: %s" response.status_code response.body;
        code = None;
        param = None;
        decline_code = None;
        doc_url = None;
      }

(** Stripe API Client using Lwt *)
module Client = struct
  type t = Stripe_core.config

  let create = Stripe_core.default_config

  (** Customer API *)
  module Customer = struct
    open Stripe.Customer

    let create ~config ?idempotency_key ?email ?name ?description ?phone ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("email", v)) email;
        Option.map (fun v -> ("name", v)) name;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("phone", v)) phone;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/customers" ~params () >>= 
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/customers/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?idempotency_key ?email ?name ?description ?phone ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("email", v)) email;
        Option.map (fun v -> ("name", v)) name;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("phone", v)) phone;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:("/v1/customers/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      delete ~config ~path:("/v1/customers/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

    let list ~config ?limit ?starting_after ?ending_before ?email () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("ending_before", v)) ending_before;
        Option.map (fun v -> ("email", v)) email;
      ] in
      get ~config ~path:"/v1/customers" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** PaymentIntent API *)
  module Payment_intent = struct
    open Stripe.Payment_intent

    let create ~config ~amount ~currency 
        ?idempotency_key ?customer ?description ?payment_method ?confirm 
        ?capture_method ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("amount", string_of_int amount);
        ("currency", currency);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("payment_method", v)) payment_method;
        Option.map (fun v -> ("confirm", string_of_bool v)) confirm;
        Option.map (fun v -> ("capture_method", v)) capture_method;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/payment_intents" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/payment_intents/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/payment_intents/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let confirm ~config ~id ?payment_method () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("payment_method", v)) payment_method;
      ] in
      post ~config ~path:("/v1/payment_intents/" ^ id ^ "/confirm") ~params () >>=
      handle_response ~parse_ok:of_json

    let capture ~config ~id ?amount_to_capture () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("amount_to_capture", string_of_int v)) amount_to_capture;
      ] in
      post ~config ~path:("/v1/payment_intents/" ^ id ^ "/capture") ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id ?cancellation_reason () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("cancellation_reason", v)) cancellation_reason;
      ] in
      post ~config ~path:("/v1/payment_intents/" ^ id ^ "/cancel") ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
      ] in
      get ~config ~path:"/v1/payment_intents" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Charge API *)
  module Charge = struct
    open Stripe.Charge

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/charges/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?payment_intent () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("payment_intent", v)) payment_intent;
      ] in
      get ~config ~path:"/v1/charges" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)

    let capture ~config ~id ?amount () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("amount", string_of_int v)) amount;
      ] in
      post ~config ~path:("/v1/charges/" ^ id ^ "/capture") ~params () >>=
      handle_response ~parse_ok:of_json
  end

  (** Refund API *)
  module Refund = struct
    open Stripe.Refund

    let create ~config ?idempotency_key ?charge ?payment_intent ?amount ?reason ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("charge", v)) charge;
        Option.map (fun v -> ("payment_intent", v)) payment_intent;
        Option.map (fun v -> ("amount", string_of_int v)) amount;
        Option.map (fun v -> ("reason", v)) reason;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/refunds" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/refunds/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?charge ?payment_intent () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("charge", v)) charge;
        Option.map (fun v -> ("payment_intent", v)) payment_intent;
      ] in
      get ~config ~path:"/v1/refunds" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Balance API *)
  module Balance = struct
    let retrieve ~config () =
      get ~config ~path:"/v1/balance" () >>=
      handle_response ~parse_ok:Stripe.Balance.of_json
  end

  (** Product API *)
  module Product = struct
    open Stripe.Product

    let create ~config ?idempotency_key ~name ?description ?active ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("name", name)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/products" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/products/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?name ?description ?active ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("name", v)) name;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/products/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?active () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      get ~config ~path:"/v1/products" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)

    let delete ~config ~id () =
      delete ~config ~path:("/v1/products/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json
  end

  (** Price API *)
  module Price = struct
    open Stripe.Price

    let create ~config ~currency ~product 
        ?unit_amount ?recurring_interval ?recurring_interval_count ?metadata () =
      let params = [
        ("currency", currency);
        ("product", product);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("unit_amount", string_of_int v)) unit_amount;
        Option.map (fun v -> ("recurring[interval]", v)) recurring_interval;
        Option.map (fun v -> ("recurring[interval_count]", string_of_int v)) recurring_interval_count;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:"/v1/prices" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/prices/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?product ?active () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("product", v)) product;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      get ~config ~path:"/v1/prices" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Subscription API *)
  module Subscription = struct
    open Stripe.Subscription

    let create ~config ~customer ~price 
        ?default_payment_method ?metadata () =
      let params = [
        ("customer", customer);
        ("items[0][price]", price);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:"/v1/subscriptions" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/subscriptions/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?cancel_at_period_end ?default_payment_method ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("cancel_at_period_end", string_of_bool v)) cancel_at_period_end;
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/subscriptions/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      delete ~config ~path:("/v1/subscriptions/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?status () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("status", v)) status;
      ] in
      get ~config ~path:"/v1/subscriptions" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Invoice API *)
  module Invoice = struct
    open Stripe.Invoice

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/invoices/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?subscription ?status () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("subscription", v)) subscription;
        Option.map (fun v -> ("status", v)) status;
      ] in
      get ~config ~path:"/v1/invoices" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)

    let pay ~config ~id ?payment_method () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("payment_method", v)) payment_method;
      ] in
      post ~config ~path:("/v1/invoices/" ^ id ^ "/pay") ~params () >>=
      handle_response ~parse_ok:of_json

    let void ~config ~id () =
      post ~config ~path:("/v1/invoices/" ^ id ^ "/void") () >>=
      handle_response ~parse_ok:of_json
  end

  (** Event API *)
  module Event = struct
    let retrieve ~config ~id () =
      get ~config ~path:("/v1/events/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Event.of_json

    let list ~config ?limit ?starting_after ?type_ ?created_gte ?created_lte () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("type", v)) type_;
        Option.map (fun v -> ("created[gte]", string_of_int v)) created_gte;
        Option.map (fun v -> ("created[lte]", string_of_int v)) created_lte;
      ] in
      get ~config ~path:"/v1/events" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json Stripe.Event.of_json)
  end

  (** PaymentMethod API *)
  module Payment_method = struct
    open Stripe.Payment_method

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/payment_methods/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let attach ~config ~id ~customer () =
      let params = [("customer", customer)] in
      post ~config ~path:("/v1/payment_methods/" ^ id ^ "/attach") ~params () >>=
      handle_response ~parse_ok:of_json

    let detach ~config ~id () =
      post ~config ~path:("/v1/payment_methods/" ^ id ^ "/detach") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ~customer ?type_ ?limit ?starting_after () =
      let params = [("customer", customer)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("type", v)) type_;
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
      ] in
      get ~config ~path:"/v1/payment_methods" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** SetupIntent API *)
  module Setup_intent = struct
    open Stripe.Setup_intent

    let create ~config ?idempotency_key ?customer ?payment_method 
        ?payment_method_types ?usage ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("payment_method", v)) payment_method;
        Option.map (fun v -> ("usage", v)) usage;
      ] in
      let params = match payment_method_types with
        | Some types -> params @ List.mapi (fun i t -> 
            (Printf.sprintf "payment_method_types[%d]" i, t)) types
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/setup_intents" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/setup_intents/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let confirm ~config ~id ?payment_method () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("payment_method", v)) payment_method;
      ] in
      post ~config ~path:("/v1/setup_intents/" ^ id ^ "/confirm") ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      post ~config ~path:("/v1/setup_intents/" ^ id ^ "/cancel") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?payment_method () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("payment_method", v)) payment_method;
      ] in
      get ~config ~path:"/v1/setup_intents" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Coupon API *)
  module Coupon = struct
    open Stripe.Coupon

    let create ~config ?idempotency_key ?id ?percent_off ?amount_off 
        ?currency ?duration ?duration_in_months ?max_redemptions ?name ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("id", v)) id;
        Option.map (fun v -> ("percent_off", Printf.sprintf "%.2f" v)) percent_off;
        Option.map (fun v -> ("amount_off", string_of_int v)) amount_off;
        Option.map (fun v -> ("currency", v)) currency;
        Option.map (fun v -> ("duration", v)) duration;
        Option.map (fun v -> ("duration_in_months", string_of_int v)) duration_in_months;
        Option.map (fun v -> ("max_redemptions", string_of_int v)) max_redemptions;
        Option.map (fun v -> ("name", v)) name;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/coupons" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/coupons/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?name ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("name", v)) name;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/coupons/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      delete ~config ~path:("/v1/coupons/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

    let list ~config ?limit ?starting_after () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
      ] in
      get ~config ~path:"/v1/coupons" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** BalanceTransaction API *)
  module Balance_transaction = struct
    open Stripe.Balance_transaction

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/balance_transactions/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?type_ ?source ?created_gte ?created_lte () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("type", v)) type_;
        Option.map (fun v -> ("source", v)) source;
        Option.map (fun v -> ("created[gte]", string_of_int v)) created_gte;
        Option.map (fun v -> ("created[lte]", string_of_int v)) created_lte;
      ] in
      get ~config ~path:"/v1/balance_transactions" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Payout API *)
  module Payout = struct
    open Stripe.Payout

    let create ~config ~amount ~currency ?idempotency_key 
        ?description ?destination ?method_ ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("amount", string_of_int amount);
        ("currency", currency);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("destination", v)) destination;
        Option.map (fun v -> ("method", v)) method_;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/payouts" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/payouts/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      post ~config ~path:("/v1/payouts/" ^ id ^ "/cancel") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?status ?created_gte ?created_lte () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("status", v)) status;
        Option.map (fun v -> ("created[gte]", string_of_int v)) created_gte;
        Option.map (fun v -> ("created[lte]", string_of_int v)) created_lte;
      ] in
      get ~config ~path:"/v1/payouts" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Checkout Session API *)
  module Checkout_session = struct
    open Stripe.Checkout_session

    let create ~config ~mode ~success_url ~cancel_url 
        ?idempotency_key ?customer ?customer_email ?client_reference_id
        ?line_items ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("mode", mode);
        ("success_url", success_url);
        ("cancel_url", cancel_url);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("customer_email", v)) customer_email;
        Option.map (fun v -> ("client_reference_id", v)) client_reference_id;
      ] in
      let params = match line_items with
        | Some items -> params @ List.concat (List.mapi (fun i (price, qty) -> [
            (Printf.sprintf "line_items[%d][price]" i, price);
            (Printf.sprintf "line_items[%d][quantity]" i, string_of_int qty);
          ]) items)
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/checkout/sessions" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/checkout/sessions/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let expire ~config ~id () =
      post ~config ~path:("/v1/checkout/sessions/" ^ id ^ "/expire") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?payment_intent ?status () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("payment_intent", v)) payment_intent;
        Option.map (fun v -> ("status", v)) status;
      ] in
      get ~config ~path:"/v1/checkout/sessions" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** TaxRate API *)
  module Tax_rate = struct
    open Stripe.Tax_rate

    let create ~config ~display_name ~inclusive ~percentage
        ?idempotency_key ?active ?country ?description ?jurisdiction 
        ?state ?tax_type ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("display_name", display_name);
        ("inclusive", string_of_bool inclusive);
        ("percentage", Printf.sprintf "%.2f" percentage);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("active", string_of_bool v)) active;
        Option.map (fun v -> ("country", v)) country;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("jurisdiction", v)) jurisdiction;
        Option.map (fun v -> ("state", v)) state;
        Option.map (fun v -> ("tax_type", v)) tax_type;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/tax_rates" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/tax_rates/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?active ?description ?display_name ?jurisdiction
        ?state ?tax_type ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("active", string_of_bool v)) active;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("display_name", v)) display_name;
        Option.map (fun v -> ("jurisdiction", v)) jurisdiction;
        Option.map (fun v -> ("state", v)) state;
        Option.map (fun v -> ("tax_type", v)) tax_type;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/tax_rates/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?active ?inclusive () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("active", string_of_bool v)) active;
        Option.map (fun v -> ("inclusive", string_of_bool v)) inclusive;
      ] in
      get ~config ~path:"/v1/tax_rates" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** PaymentLink API *)
  module Payment_link = struct
    open Stripe.Payment_link

    let create ~config ~line_items ?idempotency_key ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.concat (List.mapi (fun i (price, qty) -> [
        (Printf.sprintf "line_items[%d][price]" i, price);
        (Printf.sprintf "line_items[%d][quantity]" i, string_of_int qty);
      ]) line_items) in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/payment_links" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/payment_links/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?active ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/payment_links/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?active () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      get ~config ~path:"/v1/payment_links" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Dispute API *)
  module Dispute = struct
    open Stripe.Dispute

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/disputes/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?metadata () =
      let params = match metadata with
        | Some m -> List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> []
      in
      post ~config ~path:("/v1/disputes/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let close ~config ~id () =
      post ~config ~path:("/v1/disputes/" ^ id ^ "/close") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?charge ?payment_intent () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("charge", v)) charge;
        Option.map (fun v -> ("payment_intent", v)) payment_intent;
      ] in
      get ~config ~path:"/v1/disputes" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end
end
