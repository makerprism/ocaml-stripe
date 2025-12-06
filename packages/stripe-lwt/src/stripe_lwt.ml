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

(** Check if a response status code is retryable.
    Stripe recommends retrying on connection errors and 5xx server errors.
    Additionally, retry on 409 Conflict (rate limiting, lock contention). *)
let is_retryable_status_code status_code =
  status_code >= 500 || status_code = 409

(** Calculate sleep duration with exponential backoff and jitter.
    Based on stripe-python's retry logic. *)
let sleep_time_for_retry ~retry_count ~initial_delay ~max_delay =
  (* Exponential backoff: initial_delay * 2^retry_count *)
  let base_delay = initial_delay *. (2.0 ** float_of_int retry_count) in
  (* Add jitter: random value between 0 and base_delay *)
  let jitter = Random.float base_delay in
  let delay = base_delay +. jitter in
  (* Cap at max_delay *)
  Float.min delay max_delay

(** Make a Stripe API request using Lwt with automatic retries *)
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
  let initial_delay = 0.5 in  (* Start with 500ms *)
  let max_delay = 8.0 in      (* Cap at 8 seconds *)
  
  let rec attempt retry_count =
    Lwt.catch
      (fun () ->
        Lwt_http_client.request ~meth ~url ~headers ?body () >>= fun response ->
        (* Check if we should retry based on status code *)
        if is_retryable_status_code response.status_code 
           && retry_count < config.max_network_retries then
          let sleep_time = sleep_time_for_retry ~retry_count ~initial_delay ~max_delay in
          Lwt_unix.sleep sleep_time >>= fun () ->
          attempt (retry_count + 1)
        else
          Lwt.return response)
      (fun exn ->
        (* Retry on network errors *)
        if retry_count < config.max_network_retries then
          let sleep_time = sleep_time_for_retry ~retry_count ~initial_delay ~max_delay in
          Lwt_unix.sleep sleep_time >>= fun () ->
          attempt (retry_count + 1)
        else
          Lwt.fail exn)
  in
  attempt 0

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

(** Pagination helpers for Lwt *)
module Pagination = struct
  (** Fold over all pages of a list operation, accumulating results.
      
      @param get_id Function to extract the ID from each item (for cursor pagination)
      @param fetch_page Function that fetches a page, taking an optional starting_after cursor
      @param init Initial accumulator value
      @param f Function to combine accumulator with each page's response
      
      Example:
      {[
        Pagination.fold_pages
          ~get_id:(fun c -> c.Stripe.Customer.id)
          ~fetch_page:(fun ?starting_after () ->
            Client.Customer.list ~config ?starting_after ())
          ~init:[]
          ~f:(fun acc response -> Lwt.return (acc @ response.Stripe_core.data))
          ()
      ]}
  *)
  let rec fold_pages
      ~get_id
      ~fetch_page
      ~init
      ~f
      ?starting_after
      () =
    fetch_page ?starting_after () >>= function
    | Error e -> Lwt.return_error e
    | Ok response ->
      f init response >>= fun acc ->
      if response.Stripe_core.has_more then
        match List.rev response.Stripe_core.data with
        | [] -> Lwt.return_ok acc
        | last :: _ ->
          let cursor = get_id last in
          fold_pages ~get_id ~fetch_page ~init:acc ~f ~starting_after:(Some cursor) ()
      else
        Lwt.return_ok acc
  
  (** Collect all items from all pages into a single list.
      
      @param get_id Function to extract the ID from each item
      @param fetch_page Function that fetches a page
      
      Example:
      {[
        let%lwt all_customers = Pagination.collect_all
          ~get_id:(fun c -> c.Stripe.Customer.id)
          ~fetch_page:(fun ?starting_after () ->
            Client.Customer.list ~config ?starting_after ())
          ()
        in
        ...
      ]}
  *)
  let collect_all ~get_id ~fetch_page () =
    fold_pages
      ~get_id
      ~fetch_page
      ~init:[]
      ~f:(fun acc response -> Lwt.return (acc @ response.Stripe_core.data))
      ()
  
  (** Iterate over all items across all pages, calling a function for each item.
      Stops and returns Error if any page fetch fails.
      
      @param get_id Function to extract the ID from each item
      @param fetch_page Function that fetches a page
      @param f Function to call for each item
      
      Example:
      {[
        Pagination.iter_all
          ~get_id:(fun c -> c.Stripe.Customer.id)
          ~fetch_page:(fun ?starting_after () ->
            Client.Customer.list ~config ?starting_after ())
          ~f:(fun customer -> 
            Lwt_io.printlf "Customer: %s" customer.email)
          ()
      ]}
  *)
  let iter_all ~get_id ~fetch_page ~f () =
    fold_pages
      ~get_id
      ~fetch_page
      ~init:()
      ~f:(fun () response ->
        Lwt_list.iter_s f response.Stripe_core.data)
      ()
  
  (** Create an Lwt_stream that yields items from all pages lazily.
      The stream fetches pages on demand as items are consumed.
      
      @param get_id Function to extract the ID from each item
      @param fetch_page Function that fetches a page
      
      Example:
      {[
        let stream = Pagination.to_stream
          ~get_id:(fun c -> c.Stripe.Customer.id)
          ~fetch_page:(fun ?starting_after () ->
            Client.Customer.list ~config ?starting_after ())
          ()
        in
        Lwt_stream.iter_s (fun customer -> ...) stream
      ]}
  *)
  let to_stream ~get_id ~fetch_page () =
    let current_page = ref [] in
    let cursor = ref None in
    let has_more = ref true in
    let fetch_next () =
      if not !has_more then
        Lwt.return_none
      else
        fetch_page ?starting_after:!cursor () >>= function
        | Error _ -> 
          has_more := false;
          Lwt.return_none
        | Ok response ->
          has_more := response.Stripe_core.has_more;
          (match List.rev response.Stripe_core.data with
           | [] -> cursor := None
           | last :: _ -> cursor := Some (get_id last));
          current_page := response.Stripe_core.data;
          Lwt.return_some response.Stripe_core.data
    in
    Lwt_stream.from (fun () ->
      match !current_page with
      | item :: rest ->
        current_page := rest;
        Lwt.return_some item
      | [] ->
        fetch_next () >>= function
        | None -> Lwt.return_none
        | Some [] -> Lwt.return_none
        | Some (item :: rest) ->
          current_page := rest;
          Lwt.return_some item
    )
end

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

    (** Create a Checkout Session.
        @param mode "payment", "subscription", or "setup"
        @param success_url URL to redirect after successful payment
        @param cancel_url URL to redirect if customer cancels
        @param allow_promotion_codes Enable promotion code input field
        @param discounts List of coupon IDs to apply (e.g., [("coupon", "SAVE20")])
        @param line_items List of (price_id, quantity) tuples *)
    let create ~config ~mode ~success_url ~cancel_url 
        ?idempotency_key ?customer ?customer_email ?client_reference_id
        ?allow_promotion_codes ?discounts
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
        Option.map (fun v -> ("allow_promotion_codes", string_of_bool v)) allow_promotion_codes;
      ] in
      (* Add discounts - each discount can have a coupon or promotion_code *)
      let params = match discounts with
        | Some items -> params @ List.concat (List.mapi (fun i discount ->
            List.filter_map Fun.id [
              Option.map (fun c -> (Printf.sprintf "discounts[%d][coupon]" i, c)) 
                (List.assoc_opt "coupon" discount);
              Option.map (fun p -> (Printf.sprintf "discounts[%d][promotion_code]" i, p)) 
                (List.assoc_opt "promotion_code" discount);
            ]
          ) items)
        | None -> params
      in
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

  (** Account API (Connect) *)
  module Account = struct
    open Stripe.Account

    let create ~config ?idempotency_key ?type_ ?country ?email 
        ?business_type ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("type", v)) type_;
        Option.map (fun v -> ("country", v)) country;
        Option.map (fun v -> ("email", v)) email;
        Option.map (fun v -> ("business_type", v)) business_type;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/accounts" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/accounts/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let retrieve_current ~config () =
      get ~config ~path:"/v1/account" () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?email ?business_type ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("email", v)) email;
        Option.map (fun v -> ("business_type", v)) business_type;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/accounts/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      delete ~config ~path:("/v1/accounts/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

    let list ~config ?limit ?starting_after () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
      ] in
      get ~config ~path:"/v1/accounts" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Transfer API (Connect) *)
  module Transfer = struct
    open Stripe.Transfer

    let create ~config ~amount ~currency ~destination
        ?idempotency_key ?description ?source_transaction 
        ?transfer_group ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("amount", string_of_int amount);
        ("currency", currency);
        ("destination", destination);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("source_transaction", v)) source_transaction;
        Option.map (fun v -> ("transfer_group", v)) transfer_group;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/transfers" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/transfers/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/transfers/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?destination ?transfer_group () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("destination", v)) destination;
        Option.map (fun v -> ("transfer_group", v)) transfer_group;
      ] in
      get ~config ~path:"/v1/transfers" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** File API *)
  module File = struct
    open Stripe.File

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/files/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?purpose () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("purpose", v)) purpose;
      ] in
      get ~config ~path:"/v1/files" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** FileLink API *)
  module File_link = struct
    open Stripe.File_link

    let create ~config ~file ?idempotency_key ?expires_at ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("file", file)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("expires_at", string_of_int v)) expires_at;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/file_links" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/file_links/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?expires_at ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("expires_at", string_of_int v)) expires_at;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/file_links/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?file ?expired () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("file", v)) file;
        Option.map (fun v -> ("expired", string_of_bool v)) expired;
      ] in
      get ~config ~path:"/v1/file_links" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Mandate API *)
  module Mandate = struct
    let retrieve ~config ~id () =
      get ~config ~path:("/v1/mandates/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Mandate.of_json
  end

  (** Review API (Radar) *)
  module Review = struct
    open Stripe.Review

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/reviews/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let approve ~config ~id () =
      post ~config ~path:("/v1/reviews/" ^ id ^ "/approve") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
      ] in
      get ~config ~path:"/v1/reviews" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** PromotionCode API *)
  module Promotion_code = struct
    open Stripe.Promotion_code

    let create ~config ~coupon ?idempotency_key ?code ?customer 
        ?expires_at ?max_redemptions ?active ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("coupon", coupon)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("code", v)) code;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("expires_at", string_of_int v)) expires_at;
        Option.map (fun v -> ("max_redemptions", string_of_int v)) max_redemptions;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/promotion_codes" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/promotion_codes/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?active ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/promotion_codes/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?code ?coupon ?customer ?active () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("code", v)) code;
        Option.map (fun v -> ("coupon", v)) coupon;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      get ~config ~path:"/v1/promotion_codes" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** InvoiceItem API *)
  module Invoice_item = struct
    open Stripe.Invoice_item

    let create ~config ~customer ?idempotency_key ?amount ?currency 
        ?description ?invoice ?price ?quantity ?unit_amount ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("customer", customer)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("amount", string_of_int v)) amount;
        Option.map (fun v -> ("currency", v)) currency;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("invoice", v)) invoice;
        Option.map (fun v -> ("price", v)) price;
        Option.map (fun v -> ("quantity", string_of_int v)) quantity;
        Option.map (fun v -> ("unit_amount", string_of_int v)) unit_amount;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/invoiceitems" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/invoiceitems/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?amount ?description ?quantity ?unit_amount ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("amount", string_of_int v)) amount;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("quantity", string_of_int v)) quantity;
        Option.map (fun v -> ("unit_amount", string_of_int v)) unit_amount;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/invoiceitems/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      delete ~config ~path:("/v1/invoiceitems/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

    let list ~config ?limit ?starting_after ?customer ?invoice () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("invoice", v)) invoice;
      ] in
      get ~config ~path:"/v1/invoiceitems" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Quote API *)
  module Quote = struct
    open Stripe.Quote

    let create ~config ?idempotency_key ?customer ?description 
        ?expires_at ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("expires_at", string_of_int v)) expires_at;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/quotes" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/quotes/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?expires_at ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("expires_at", string_of_int v)) expires_at;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/quotes/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let finalize_quote ~config ~id () =
      post ~config ~path:("/v1/quotes/" ^ id ^ "/finalize") () >>=
      handle_response ~parse_ok:of_json

    let accept ~config ~id () =
      post ~config ~path:("/v1/quotes/" ^ id ^ "/accept") () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      post ~config ~path:("/v1/quotes/" ^ id ^ "/cancel") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?status () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("status", v)) status;
      ] in
      get ~config ~path:"/v1/quotes" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** CreditNote API *)
  module Credit_note = struct
    open Stripe.Credit_note

    let create ~config ~invoice ?idempotency_key ?amount ?memo 
        ?reason ?refund ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("invoice", invoice)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("amount", string_of_int v)) amount;
        Option.map (fun v -> ("memo", v)) memo;
        Option.map (fun v -> ("reason", v)) reason;
        Option.map (fun v -> ("refund", v)) refund;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/credit_notes" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/credit_notes/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?memo ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("memo", v)) memo;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/credit_notes/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let void_credit_note ~config ~id () =
      post ~config ~path:("/v1/credit_notes/" ^ id ^ "/void") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?invoice ?customer () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("invoice", v)) invoice;
        Option.map (fun v -> ("customer", v)) customer;
      ] in
      get ~config ~path:"/v1/credit_notes" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** ApplicationFee API *)
  module Application_fee = struct
    open Stripe.Application_fee

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/application_fees/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?charge () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("charge", v)) charge;
      ] in
      get ~config ~path:"/v1/application_fees" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Topup API *)
  module Topup = struct
    open Stripe.Topup

    let create ~config ~amount ~currency ?idempotency_key 
        ?description ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("amount", string_of_int amount);
        ("currency", currency);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/topups" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/topups/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/topups/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      post ~config ~path:("/v1/topups/" ^ id ^ "/cancel") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?status () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("status", v)) status;
      ] in
      get ~config ~path:"/v1/topups" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** SubscriptionItem API *)
  module Subscription_item = struct
    open Stripe.Subscription_item

    let create ~config ~subscription ~price ?idempotency_key 
        ?quantity ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("subscription", subscription);
        ("price", price);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("quantity", string_of_int v)) quantity;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/subscription_items" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/subscription_items/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?price ?quantity ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("price", v)) price;
        Option.map (fun v -> ("quantity", string_of_int v)) quantity;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/subscription_items/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      delete ~config ~path:("/v1/subscription_items/" ^ id) () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

    let list ~config ~subscription ?limit ?starting_after () =
      let params = [("subscription", subscription)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
      ] in
      get ~config ~path:"/v1/subscription_items" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)

    (** Create a usage record for metered billing *)
    let create_usage_record ~config ~id ~quantity ~timestamp 
        ?idempotency_key ?action () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("quantity", string_of_int quantity);
        ("timestamp", string_of_int timestamp);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("action", v)) action;
      ] in
      post ~config ~options 
        ~path:("/v1/subscription_items/" ^ id ^ "/usage_records") 
        ~params () >>=
      handle_response ~parse_ok:Stripe.Usage_record.of_json
  end

  (** SubscriptionSchedule API *)
  module Subscription_schedule = struct
    open Stripe.Subscription_schedule

    let create ~config ?idempotency_key ?customer ?from_subscription 
        ?end_behavior ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("from_subscription", v)) from_subscription;
        Option.map (fun v -> ("end_behavior", v)) end_behavior;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/subscription_schedules" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      get ~config ~path:("/v1/subscription_schedules/" ^ id) () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?end_behavior ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("end_behavior", v)) end_behavior;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~path:("/v1/subscription_schedules/" ^ id) ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      post ~config ~path:("/v1/subscription_schedules/" ^ id ^ "/cancel") () >>=
      handle_response ~parse_ok:of_json

    let release ~config ~id () =
      post ~config ~path:("/v1/subscription_schedules/" ^ id ^ "/release") () >>=
      handle_response ~parse_ok:of_json

    let list ~config ?limit ?starting_after ?customer ?scheduled () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("customer", v)) customer;
        Option.map (fun v -> ("scheduled", string_of_bool v)) scheduled;
      ] in
      get ~config ~path:"/v1/subscription_schedules" ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end

  (** Billing Portal Session API *)
  module Billing_portal_session = struct
    open Stripe.Billing_portal_session

    (** Create a billing portal session.
        @param customer The ID of the customer to create a session for
        @param return_url URL to redirect to after the portal session ends
        @param configuration Optional portal configuration ID
        @param flow_data Optional flow data for specific portal flows
        @param locale Optional locale for the portal (e.g., "en", "fr")
        @param on_behalf_of Optional connected account ID (for Connect) *)
    let create ~config ~customer ~return_url 
        ?idempotency_key ?configuration ?locale ?on_behalf_of () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("customer", customer);
        ("return_url", return_url);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("configuration", v)) configuration;
        Option.map (fun v -> ("locale", v)) locale;
        Option.map (fun v -> ("on_behalf_of", v)) on_behalf_of;
      ] in
      post ~config ~options ~path:"/v1/billing_portal/sessions" ~params () >>=
      handle_response ~parse_ok:of_json
  end

  (** Customer Balance Transaction API *)
  module Customer_balance_transaction = struct
    open Stripe.Customer_balance_transaction

    (** Create a customer balance transaction (adjust customer credit balance).
        @param customer_id The ID of the customer
        @param amount Amount in cents (positive = credit, negative = debit)
        @param currency Three-letter ISO currency code
        @param description Optional description for the transaction *)
    let create ~config ~customer_id ~amount ~currency 
        ?idempotency_key ?description ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("amount", string_of_int amount);
        ("currency", currency);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions" customer_id) 
        ~params () >>=
      handle_response ~parse_ok:of_json

    (** Retrieve a customer balance transaction *)
    let retrieve ~config ~customer_id ~transaction_id () =
      get ~config 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions/%s" 
                 customer_id transaction_id) () >>=
      handle_response ~parse_ok:of_json

    (** Update a customer balance transaction *)
    let update ~config ~customer_id ~transaction_id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions/%s" 
                 customer_id transaction_id) 
        ~params () >>=
      handle_response ~parse_ok:of_json

    (** List customer balance transactions *)
    let list ~config ~customer_id ?limit ?starting_after ?ending_before () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("ending_before", v)) ending_before;
      ] in
      get ~config 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions" customer_id) 
        ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end
end
