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

(** Make a Stripe API request using Lwt *)
let request
    ~(config : Stripe_core.config)
    ~(meth : Stripe_core.http_method)
    ~(path : string)
    ?(params : (string * string) list = [])
    ()
  : Stripe_core.http_response Lwt.t =
  let url = config.api_base ^ path in
  let headers = Stripe_core.build_headers config in
  let body = if List.length params > 0 then
    Some (Stripe_core.build_form_body params)
  else
    None
  in
  Lwt_http_client.request ~meth ~url ~headers ?body ()

(** Make a GET request *)
let get ~config ~path ?params () =
  request ~config ~meth:GET ~path ?params ()

(** Make a POST request *)
let post ~config ~path ?params () =
  request ~config ~meth:POST ~path ?params ()

(** Make a DELETE request *)
let delete ~config ~path ?params () =
  request ~config ~meth:DELETE ~path ?params ()

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

    let create ~config ?email ?name ?description ?phone ?metadata () =
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
      post ~config ~path:"/v1/customers" ~params () >>= 
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/customers/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?email ?name ?description ?phone ?metadata () =
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
      post ~config ~path:("/v1/customers/" ^ id) ~params () >>=
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
        ?customer ?description ?payment_method ?confirm 
        ?capture_method ?metadata () =
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
      post ~config ~path:"/v1/payment_intents" ~params () >>=
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

    let create ~config ?charge ?payment_intent ?amount ?reason ?metadata () =
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
      post ~config ~path:"/v1/refunds" ~params () >>=
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

    let create ~config ~name ?description ?active ?metadata () =
      let params = [("name", name)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:"/v1/products" ~params () >>=
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
end
