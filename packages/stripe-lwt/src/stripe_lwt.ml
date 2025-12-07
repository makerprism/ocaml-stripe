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

(** Timeout exception *)
exception Request_timeout

(** Make a Stripe API request using Lwt with automatic retries and timeout *)
let request
    ~(config : Stripe_core.config)
    ~(meth : Stripe_core.http_method)
    ~(path : string)
    ?(options : request_options = default_request_options)
    ?(params : (string * string) list = [])
    ()
  : Stripe_core.http_response Lwt.t =
  (* Validate API base URL uses HTTPS *)
  let _ = Stripe_core.validate_api_base config.api_base in
  let url = config.api_base ^ path in
  let headers = Stripe_core.build_headers ~options config in
  let body = if List.length params > 0 then
    Some (Stripe_core.build_form_body params)
  else
    None
  in
  let initial_delay = 0.5 in  (* Start with 500ms *)
  let max_delay = 8.0 in      (* Cap at 8 seconds *)
  
  (* Create a request with timeout *)
  let make_request () =
    if config.timeout > 0.0 then
      Lwt.pick [
        Lwt_http_client.request ~meth ~url ~headers ?body ();
        (Lwt_unix.sleep config.timeout >>= fun () -> Lwt.fail Request_timeout);
      ]
    else
      Lwt_http_client.request ~meth ~url ~headers ?body ()
  in
  
  let rec attempt retry_count =
    Lwt.catch
      (fun () ->
        make_request () >>= fun response ->
        (* Check if we should retry based on status code *)
        if is_retryable_status_code response.status_code 
           && retry_count < config.max_network_retries then
          let sleep_time = sleep_time_for_retry ~retry_count ~initial_delay ~max_delay in
          Lwt_unix.sleep sleep_time >>= fun () ->
          attempt (retry_count + 1)
        else
          Lwt.return response)
      (fun exn ->
        (* Retry on network errors and timeouts *)
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

(** Build a path with a validated resource ID.
    @param base The base path (e.g., "/v1/customers/")
    @param resource_type Description for error messages (e.g., "customer")
    @param id The resource ID to validate and append *)
let path_with_id ~base ~resource_type id =
  let validated_id = Stripe_core.validate_id ~resource_type id in
  base ^ validated_id

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
      (* Truncate response body to prevent sensitive data leakage in logs *)
      let truncated_body = truncate_string ~max_len:max_error_body_length response.body in
      Lwt.return_error {
        error_type = Api_error;
        message = Printf.sprintf "HTTP %d: %s" response.status_code truncated_body;
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

  (** Collection method for invoices - shared across Subscription and Invoice modules *)
  type collection_method =
    | Charge_automatically  (** Automatically charge the customer *)
    | Send_invoice          (** Email an invoice to the customer *)

  let collection_method_to_string = function
    | Charge_automatically -> "charge_automatically"
    | Send_invoice -> "send_invoice"

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
      let path = path_with_id ~base:"/v1/customers/" ~resource_type:"customer" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/customers/" ~resource_type:"customer" id in
      post ~config ~options ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      let path = path_with_id ~base:"/v1/customers/" ~resource_type:"customer" id in
      delete ~config ~path () >>=
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

    (** Tax ID API - nested under customers *)
    module Tax_id = struct
      open Stripe.Tax_id

      (** Create a tax ID for a customer.
          @param customer The customer ID
          @param type_ The type of tax ID (e.g., "eu_vat", "gb_vat", "us_ein")
          @param value The value of the tax ID *)
      let create ~config ~customer ~type_ ~value () =
        let customer_id = Stripe_core.validate_id ~resource_type:"customer" customer in
        let params = [
          ("type", type_);
          ("value", value);
        ] in
        post ~config ~path:("/v1/customers/" ^ customer_id ^ "/tax_ids") ~params () >>=
        handle_response ~parse_ok:of_json

      (** Retrieve a tax ID.
          @param customer The customer ID
          @param id The tax ID (txi_xxx) *)
      let retrieve ~config ~customer ~id () =
        let customer_id = Stripe_core.validate_id ~resource_type:"customer" customer in
        let tax_id = Stripe_core.validate_id ~resource_type:"tax_id" id in
        get ~config ~path:("/v1/customers/" ^ customer_id ^ "/tax_ids/" ^ tax_id) () >>=
        handle_response ~parse_ok:of_json

      (** Delete a tax ID.
          @param customer The customer ID
          @param id The tax ID (txi_xxx) *)
      let delete ~config ~customer ~id () =
        let customer_id = Stripe_core.validate_id ~resource_type:"customer" customer in
        let tax_id = Stripe_core.validate_id ~resource_type:"tax_id" id in
        let path = "/v1/customers/" ^ customer_id ^ "/tax_ids/" ^ tax_id in
        request ~config ~meth:DELETE ~path () >>=
        handle_response ~parse_ok:Stripe.Deleted.of_json

      (** List all tax IDs for a customer.
          @param customer The customer ID *)
      let list ~config ~customer ?limit ?starting_after () =
        let customer_id = Stripe_core.validate_id ~resource_type:"customer" customer in
        let params = List.filter_map Fun.id [
          Option.map (fun v -> ("limit", string_of_int v)) limit;
          Option.map (fun v -> ("starting_after", v)) starting_after;
        ] in
        get ~config ~path:("/v1/customers/" ^ customer_id ^ "/tax_ids") ~params () >>=
        handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
    end
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
      let path = path_with_id ~base:"/v1/payment_intents/" ~resource_type:"payment_intent" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/payment_intents/" ~resource_type:"payment_intent" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let confirm ~config ~id ?payment_method () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("payment_method", v)) payment_method;
      ] in
      let path = path_with_id ~base:"/v1/payment_intents/" ~resource_type:"payment_intent" id in
      post ~config ~path:(path ^ "/confirm") ~params () >>=
      handle_response ~parse_ok:of_json

    let capture ~config ~id ?amount_to_capture () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("amount_to_capture", string_of_int v)) amount_to_capture;
      ] in
      let path = path_with_id ~base:"/v1/payment_intents/" ~resource_type:"payment_intent" id in
      post ~config ~path:(path ^ "/capture") ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id ?cancellation_reason () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("cancellation_reason", v)) cancellation_reason;
      ] in
      let path = path_with_id ~base:"/v1/payment_intents/" ~resource_type:"payment_intent" id in
      post ~config ~path:(path ^ "/cancel") ~params () >>=
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
      let path = path_with_id ~base:"/v1/charges/" ~resource_type:"charge" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/charges/" ~resource_type:"charge" id in
      post ~config ~path:(path ^ "/capture") ~params () >>=
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
      let path = path_with_id ~base:"/v1/refunds/" ~resource_type:"refund" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/products/" ~resource_type:"product" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/products/" ~resource_type:"product" id in
      post ~config ~path ~params () >>=
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
      let path = path_with_id ~base:"/v1/products/" ~resource_type:"product" id in
      delete ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/prices/" ~resource_type:"price" id in
      get ~config ~path () >>=
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

    (** Subscription item update for modifying subscription items.
        Used with the update function to change prices, quantities, etc. *)
    type item_update = {
      id : string;           (** The subscription item ID (si_xxx) *)
      price : string option; (** New price ID to switch to *)
      quantity : int option; (** New quantity *)
    }

    (** Create an item_update for modifying an existing subscription item *)
    let item_update ~id ?price ?quantity () = { id; price; quantity }

    (** Subscription item for creating subscriptions with multiple items *)
    type item_create = {
      price : string;                            (** Price ID for the item *)
      quantity : int option;                     (** Quantity for the item *)
      tax_rates : string list option;            (** Tax rates for this item *)
      metadata : (string * string) list option;  (** Metadata for this item *)
    }

    (** Create a subscription item with just a price *)
    let item ~price ?quantity ?tax_rates ?metadata () =
      { price; quantity; tax_rates; metadata }

    (** Trial end specification for subscriptions.
        Use this type-safe variant instead of raw strings. *)
    type trial_end =
      | Trial_end_at of int   (** End trial at specific Unix timestamp *)
      | Trial_end_now         (** End trial immediately *)

    (** Convert trial_end to API string parameter *)
    let trial_end_to_string = function
      | Trial_end_at ts -> string_of_int ts
      | Trial_end_now -> "now"

    (** Billing cycle anchor specification *)
    type billing_cycle_anchor =
      | Billing_cycle_at of int   (** Set anchor to specific Unix timestamp *)
      | Billing_cycle_now         (** Set anchor to now *)
      | Billing_cycle_unchanged   (** Keep current anchor (for updates) *)

    let billing_cycle_anchor_to_string = function
      | Billing_cycle_at ts -> string_of_int ts
      | Billing_cycle_now -> "now"
      | Billing_cycle_unchanged -> "unchanged"

    (** Cancel at specification *)
    type cancel_at =
      | Cancel_at_timestamp of int  (** Cancel at specific Unix timestamp *)
      | Cancel_at_period_end        (** Cancel at end of current period *)

    let cancel_at_to_string = function
      | Cancel_at_timestamp ts -> string_of_int ts
      | Cancel_at_period_end -> "period_end"

    (** Payment behavior when creating/updating subscriptions *)
    type payment_behavior =
      | Allow_incomplete       (** Allow incomplete payments *)
      | Default_incomplete     (** Use Stripe's default behavior for incomplete *)
      | Error_if_incomplete    (** Return an error if payment fails *)
      | Pending_if_incomplete  (** Mark as pending if payment fails *)

    let payment_behavior_to_string = function
      | Allow_incomplete -> "allow_incomplete"
      | Default_incomplete -> "default_incomplete"
      | Error_if_incomplete -> "error_if_incomplete"
      | Pending_if_incomplete -> "pending_if_incomplete"

    (** Proration behavior when changing subscriptions *)
    type proration_behavior =
      | Create_prorations  (** Create prorations for changes *)
      | No_proration       (** Don't prorate *)
      | Always_invoice     (** Always create an invoice for prorations *)

    let proration_behavior_to_string = function
      | Create_prorations -> "create_prorations"
      | No_proration -> "none"
      | Always_invoice -> "always_invoice"

    (** Trial end behavior when payment method is missing *)
    type trial_end_behavior =
      | Trial_cancel          (** Cancel the subscription *)
      | Trial_create_invoice  (** Create an invoice *)
      | Trial_pause           (** Pause the subscription *)

    let trial_end_behavior_to_string = function
      | Trial_cancel -> "cancel"
      | Trial_create_invoice -> "create_invoice"
      | Trial_pause -> "pause"

    (** Trial settings configuration *)
    type trial_settings = {
      end_behavior_missing_payment_method : trial_end_behavior;
    }

    (** Create trial settings *)
    let make_trial_settings ~end_behavior () =
      { end_behavior_missing_payment_method = end_behavior }

    (** Pause collection behavior *)
    type pause_behavior =
      | Pause_keep_as_draft       (** Keep invoices as drafts *)
      | Pause_mark_uncollectible  (** Mark invoices as uncollectible *)
      | Pause_void                (** Void invoices *)

    let pause_behavior_to_string = function
      | Pause_keep_as_draft -> "keep_as_draft"
      | Pause_mark_uncollectible -> "mark_uncollectible"
      | Pause_void -> "void"

    (** Single discount to apply to a subscription. *)
    type discount =
      | Coupon of string          (** Apply a coupon by ID *)
      | Promotion_code of string  (** Apply a promotion code by ID *)

    (** Discounts parameter for create/update operations.
        Use [Set] to apply discounts, [Clear] to remove all. *)
    type discounts_param =
      | Set of discount list  (** Apply these discounts *)
      | Clear                 (** Remove all discounts *)

    (** Create a subscription.
        @param customer The customer ID
        @param price The price ID for the subscription
        @param idempotency_key Idempotency key for safe retries
        @param default_payment_method Default payment method ID
        @param trial_end When the trial ends (use Trial_end_at or Trial_end_now)
        @param trial_period_days Number of days for the trial period
        @param trial_from_plan Use the plan's trial period settings
        @param trial_settings Settings for trial behavior
        @param cancel_at_period_end Whether to cancel at period end
        @param cancel_at When to cancel the subscription
        @param billing_cycle_anchor When to anchor billing cycles
        @param coupon Coupon ID to apply (use discounts for multiple)
        @param promotion_code Promotion code ID to apply (use discounts for multiple)
        @param discounts Discounts to apply: [Set [...]] to set, [Clear] to remove all
        @param automatic_tax Enable automatic tax calculation
        @param collection_method How to collect payment
        @param days_until_due Days until due (for Send_invoice collection)
        @param description Subscription description
        @param payment_behavior How to handle payment failures
        @param proration_behavior How to handle proration
        @param metadata Key-value metadata *)
    let create ~config ~customer ~price ?idempotency_key
        ?default_payment_method ?trial_end ?trial_period_days ?trial_from_plan
        ?trial_settings ?cancel_at_period_end ?cancel_at ?billing_cycle_anchor
        ?coupon ?promotion_code ?discounts ?automatic_tax ?collection_method 
        ?days_until_due ?description ?payment_behavior ?proration_behavior 
        ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [
        ("customer", customer);
        ("items[0][price]", price);
      ] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
        Option.map (fun v -> ("trial_end", trial_end_to_string v)) trial_end;
        Option.map (fun v -> ("trial_period_days", string_of_int v)) trial_period_days;
        Option.map (fun v -> ("trial_from_plan", string_of_bool v)) trial_from_plan;
        Option.map (fun v -> ("cancel_at_period_end", string_of_bool v)) cancel_at_period_end;
        Option.map (fun v -> ("cancel_at", cancel_at_to_string v)) cancel_at;
        Option.map (fun v -> ("billing_cycle_anchor", billing_cycle_anchor_to_string v)) billing_cycle_anchor;
        Option.map (fun v -> ("coupon", v)) coupon;
        Option.map (fun v -> ("promotion_code", v)) promotion_code;
        Option.map (fun v -> ("automatic_tax[enabled]", string_of_bool v)) automatic_tax;
        Option.map (fun v -> ("collection_method", collection_method_to_string v)) collection_method;
        Option.map (fun v -> ("days_until_due", string_of_int v)) days_until_due;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("payment_behavior", payment_behavior_to_string v)) payment_behavior;
        Option.map (fun v -> ("proration_behavior", proration_behavior_to_string v)) proration_behavior;
      ] in
      (* Add discounts as array or clear *)
      let params = match discounts with
        | Some (Set ds) ->
          params @ List.mapi (fun i d ->
            match d with
            | Coupon c -> (Printf.sprintf "discounts[%d][coupon]" i, c)
            | Promotion_code p -> (Printf.sprintf "discounts[%d][promotion_code]" i, p)
          ) ds
        | Some Clear -> params @ [("discounts", "")]
        | None -> params
      in
      (* Add trial_settings *)
      let params = match trial_settings with
        | Some ts ->
          params @ [("trial_settings[end_behavior][missing_payment_method]", 
                     trial_end_behavior_to_string ts.end_behavior_missing_payment_method)]
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/subscriptions" ~params () >>=
      handle_response ~parse_ok:of_json

    (** Create a subscription with multiple items.
        @param customer The customer ID
        @param items List of subscription items with prices and quantities
        @param idempotency_key Idempotency key for safe retries
        @param default_payment_method Default payment method ID
        @param trial_end When the trial ends
        @param trial_period_days Number of days for the trial period
        @param trial_from_plan Use the plan's trial period settings
        @param trial_settings Settings for trial behavior
        @param cancel_at_period_end Whether to cancel at period end
        @param cancel_at When to cancel the subscription
        @param billing_cycle_anchor When to anchor billing cycles
        @param coupon Coupon ID to apply (use discounts for multiple)
        @param promotion_code Promotion code ID to apply (use discounts for multiple)
        @param discounts Discounts to apply: [Set [...]] to set, [Clear] to remove all
        @param automatic_tax Enable automatic tax calculation
        @param collection_method How to collect payment
        @param days_until_due Days until due (for Send_invoice collection)
        @param description Subscription description
        @param default_tax_rates Default tax rate IDs
        @param payment_behavior How to handle payment failures
        @param proration_behavior How to handle proration
        @param metadata Key-value metadata *)
    let create_multi ~config ~customer ~items ?idempotency_key
        ?default_payment_method ?trial_end ?trial_period_days ?trial_from_plan
        ?trial_settings ?cancel_at_period_end ?cancel_at ?billing_cycle_anchor
        ?coupon ?promotion_code ?discounts ?automatic_tax ?collection_method 
        ?days_until_due ?description ?default_tax_rates ?payment_behavior 
        ?proration_behavior ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("customer", customer)] in
      (* Add items as array *)
      let params = params @ List.concat (List.mapi (fun i (item : item_create) ->
        let base = Printf.sprintf "items[%d]" i in
        [(base ^ "[price]", item.price)] @
        (match item.quantity with
         | Some q -> [(base ^ "[quantity]", string_of_int q)]
         | None -> []) @
        (match item.tax_rates with
         | Some rates -> List.mapi (fun j r -> 
             (Printf.sprintf "%s[tax_rates][%d]" base j, r)) rates
         | None -> []) @
        (match item.metadata with
         | Some m -> List.map (fun (k, v) -> 
             (Printf.sprintf "%s[metadata][%s]" base k, v)) m
         | None -> [])
      ) items) in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
        Option.map (fun v -> ("trial_end", trial_end_to_string v)) trial_end;
        Option.map (fun v -> ("trial_period_days", string_of_int v)) trial_period_days;
        Option.map (fun v -> ("trial_from_plan", string_of_bool v)) trial_from_plan;
        Option.map (fun v -> ("cancel_at_period_end", string_of_bool v)) cancel_at_period_end;
        Option.map (fun v -> ("cancel_at", cancel_at_to_string v)) cancel_at;
        Option.map (fun v -> ("billing_cycle_anchor", billing_cycle_anchor_to_string v)) billing_cycle_anchor;
        Option.map (fun v -> ("coupon", v)) coupon;
        Option.map (fun v -> ("promotion_code", v)) promotion_code;
        Option.map (fun v -> ("automatic_tax[enabled]", string_of_bool v)) automatic_tax;
        Option.map (fun v -> ("collection_method", collection_method_to_string v)) collection_method;
        Option.map (fun v -> ("days_until_due", string_of_int v)) days_until_due;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("payment_behavior", payment_behavior_to_string v)) payment_behavior;
        Option.map (fun v -> ("proration_behavior", proration_behavior_to_string v)) proration_behavior;
      ] in
      (* Add discounts as array or clear *)
      let params = match discounts with
        | Some (Set ds) ->
          params @ List.mapi (fun i d ->
            match d with
            | Coupon c -> (Printf.sprintf "discounts[%d][coupon]" i, c)
            | Promotion_code p -> (Printf.sprintf "discounts[%d][promotion_code]" i, p)
          ) ds
        | Some Clear -> params @ [("discounts", "")]
        | None -> params
      in
      (* Add default_tax_rates as array *)
      let params = match default_tax_rates with
        | Some rates ->
          params @ List.mapi (fun i r ->
            (Printf.sprintf "default_tax_rates[%d]" i, r)
          ) rates
        | None -> params
      in
      (* Add trial_settings *)
      let params = match trial_settings with
        | Some ts ->
          params @ [("trial_settings[end_behavior][missing_payment_method]", 
                     trial_end_behavior_to_string ts.end_behavior_missing_payment_method)]
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/subscriptions" ~params () >>=
      handle_response ~parse_ok:of_json

    let retrieve ~config ~id () =
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    (** Pause collection configuration *)
    type pause_collection = {
      behavior : pause_behavior;
      resumes_at : int option;  (** Unix timestamp when to resume collection *)
    }

    (** Create pause_collection config *)
    let pause_collection_config ~behavior ?resumes_at () =
      { behavior; resumes_at }

    (** Update a subscription.
        @param id The subscription ID
        @param items List of item updates for changing prices/quantities (for proration)
        @param proration_behavior How to handle proration
        @param coupon Coupon ID to apply to the subscription (use empty string "" to remove)
        @param promotion_code Promotion code ID to apply
        @param discounts Discounts to apply: [Set [...]] to set, [Clear] to remove all
        @param cancel_at_period_end Whether to cancel at period end
        @param cancel_at When to cancel the subscription
        @param default_payment_method Default payment method ID
        @param trial_end When the trial ends (use Trial_end_at or Trial_end_now)
        @param trial_settings Settings for trial behavior
        @param billing_cycle_anchor When to anchor billing cycles
        @param pause_collection Pause collection settings
        @param automatic_tax Enable automatic tax calculation
        @param collection_method How to collect payment
        @param days_until_due Days until due (for Send_invoice collection)
        @param description Subscription description
        @param payment_behavior How to handle payment failures
        @param off_session Whether customer is off-session
        @param proration_date Unix timestamp for proration calculation
        @param metadata Key-value metadata *)
    let update ~config ~id ?items ?proration_behavior ?coupon ?promotion_code
        ?discounts ?cancel_at_period_end ?cancel_at ?default_payment_method ?trial_end
        ?trial_settings ?billing_cycle_anchor ?pause_collection ?automatic_tax
        ?collection_method ?days_until_due ?description ?payment_behavior
        ?off_session ?proration_date ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("cancel_at_period_end", string_of_bool v)) cancel_at_period_end;
        Option.map (fun v -> ("cancel_at", cancel_at_to_string v)) cancel_at;
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
        Option.map (fun v -> ("proration_behavior", proration_behavior_to_string v)) proration_behavior;
        Option.map (fun v -> ("coupon", v)) coupon;
        Option.map (fun v -> ("promotion_code", v)) promotion_code;
        Option.map (fun v -> ("trial_end", trial_end_to_string v)) trial_end;
        Option.map (fun v -> ("billing_cycle_anchor", billing_cycle_anchor_to_string v)) billing_cycle_anchor;
        Option.map (fun v -> ("automatic_tax[enabled]", string_of_bool v)) automatic_tax;
        Option.map (fun v -> ("collection_method", collection_method_to_string v)) collection_method;
        Option.map (fun v -> ("days_until_due", string_of_int v)) days_until_due;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("payment_behavior", payment_behavior_to_string v)) payment_behavior;
        Option.map (fun v -> ("off_session", string_of_bool v)) off_session;
        Option.map (fun v -> ("proration_date", string_of_int v)) proration_date;
      ] in
      (* Add trial_settings *)
      let params = match trial_settings with
        | Some ts ->
          params @ [("trial_settings[end_behavior][missing_payment_method]", 
                     trial_end_behavior_to_string ts.end_behavior_missing_payment_method)]
        | None -> params
      in
      (* Add pause_collection *)
      let params = match pause_collection with
        | Some pc ->
          params @ [("pause_collection[behavior]", pause_behavior_to_string pc.behavior)] @
          (match pc.resumes_at with
           | Some ts -> [("pause_collection[resumes_at]", string_of_int ts)]
           | None -> [])
        | None -> params
      in
      (* Add discounts as array or clear *)
      let params = match discounts with
        | Some (Set ds) ->
          params @ List.mapi (fun i d ->
            match d with
            | Coupon c -> (Printf.sprintf "discounts[%d][coupon]" i, c)
            | Promotion_code p -> (Printf.sprintf "discounts[%d][promotion_code]" i, p)
          ) ds
        | Some Clear -> params @ [("discounts", "")]
        | None -> params
      in
      (* Add subscription items for proration updates *)
      let params = match items with
        | Some item_list ->
          params @ List.concat (List.mapi (fun i item ->
            let base = Printf.sprintf "items[%d]" i in
            List.filter_map Fun.id [
              Some (base ^ "[id]", item.id);
              Option.map (fun p -> (base ^ "[price]", p)) item.price;
              Option.map (fun q -> (base ^ "[quantity]", string_of_int q)) item.quantity;
            ]
          ) item_list)
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    (** Apply a coupon to an existing subscription.
        @param id The subscription ID
        @param coupon_id The coupon ID to apply *)
    let apply_coupon ~config ~id ~coupon_id () =
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      let params = [("coupon", coupon_id)] in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    (** Remove a coupon from an existing subscription.
        @param id The subscription ID *)
    let remove_coupon ~config ~id () =
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      let params = [("coupon", "")] in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    (** Cancellation feedback options *)
    type cancellation_feedback =
      | Feedback_customer_service
      | Feedback_low_quality
      | Feedback_missing_features
      | Feedback_other
      | Feedback_switched_service
      | Feedback_too_complex
      | Feedback_too_expensive
      | Feedback_unused

    let cancellation_feedback_to_string = function
      | Feedback_customer_service -> "customer_service"
      | Feedback_low_quality -> "low_quality"
      | Feedback_missing_features -> "missing_features"
      | Feedback_other -> "other"
      | Feedback_switched_service -> "switched_service"
      | Feedback_too_complex -> "too_complex"
      | Feedback_too_expensive -> "too_expensive"
      | Feedback_unused -> "unused"

    (** Cancellation details *)
    type cancellation_details = {
      comment : string option;
      feedback : cancellation_feedback option;
    }

    (** Cancel a subscription.
        @param id The subscription ID
        @param invoice_now Generate a final invoice immediately
        @param prorate Create a prorated credit for unused time
        @param cancellation_details Details about why the subscription was cancelled *)
    let cancel ~config ~id ?invoice_now ?prorate ?cancellation_details () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("invoice_now", string_of_bool v)) invoice_now;
        Option.map (fun v -> ("prorate", string_of_bool v)) prorate;
      ] in
      let params = match cancellation_details with
        | Some cd ->
          params @
          (match cd.comment with
           | Some c -> [("cancellation_details[comment]", c)]
           | None -> []) @
          (match cd.feedback with
           | Some f -> [("cancellation_details[feedback]", cancellation_feedback_to_string f)]
           | None -> [])
        | None -> params
      in
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      delete ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    (** Resume a paused subscription.
        @param id The subscription ID
        @param billing_cycle_anchor When to anchor the next billing cycle ("now" or "unchanged")
        @param proration_behavior How to handle proration *)
    let resume ~config ~id ?billing_cycle_anchor ?proration_behavior () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("billing_cycle_anchor", billing_cycle_anchor_to_string v)) billing_cycle_anchor;
        Option.map (fun v -> ("proration_behavior", v)) proration_behavior;
      ] in
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      post ~config ~path:(path ^ "/resume") ~params () >>=
      handle_response ~parse_ok:of_json

    (** Delete the discount on a subscription.
        @param id The subscription ID *)
    let delete_discount ~config ~id () =
      let path = path_with_id ~base:"/v1/subscriptions/" ~resource_type:"subscription" id in
      delete ~config ~path:(path ^ "/discount") () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

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
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      get ~config ~path () >>=
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

    (** Pay an invoice.
        @param id The invoice ID
        @param payment_method PaymentMethod ID to use for payment
        @param forgive Forgive the difference if source has insufficient funds
        @param off_session Whether customer is off-session (default true)
        @param paid_out_of_band Mark as paid outside of Stripe
        @param mandate Mandate ID to use for payment
        @param source Payment source ID to use *)
    let pay ~config ~id ?payment_method ?forgive ?off_session ?paid_out_of_band
        ?mandate ?source () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("payment_method", v)) payment_method;
        Option.map (fun v -> ("forgive", string_of_bool v)) forgive;
        Option.map (fun v -> ("off_session", string_of_bool v)) off_session;
        Option.map (fun v -> ("paid_out_of_band", string_of_bool v)) paid_out_of_band;
        Option.map (fun v -> ("mandate", v)) mandate;
        Option.map (fun v -> ("source", v)) source;
      ] in
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      post ~config ~path:(path ^ "/pay") ~params () >>=
      handle_response ~parse_ok:of_json

    let void ~config ~id () =
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      post ~config ~path:(path ^ "/void") () >>=
      handle_response ~parse_ok:of_json

    (** Delete a draft invoice. Only works for invoices in draft state.
        @param id The invoice ID *)
    let delete ~config ~id () =
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      request ~config ~meth:DELETE ~path () >>=
      handle_response ~parse_ok:Stripe.Deleted.of_json

    (** Custom field for invoice display *)
    type custom_field = {
      name : string;
      value : string;
    }

    (** Automatic tax configuration *)
    type automatic_tax_config = {
      enabled : bool;
      liability_type : string option;  (** "account" or "self" *)
      liability_account : string option;  (** Connected account ID when liability_type is "account" *)
    }

    (** Create automatic_tax config with just enabled flag *)
    let automatic_tax_enabled enabled =
      { enabled; liability_type = None; liability_account = None }

    (** Create automatic_tax config for Connect with liability *)
    let automatic_tax_with_liability ~enabled ~liability_type ?liability_account () =
      { enabled; liability_type = Some liability_type; liability_account }

    (** Rendering options for invoice PDF *)
    type rendering_options = {
      amount_tax_display : string option;  (** "exclude_tax" or "include_inclusive_tax" *)
      pdf_page_size : string option;  (** "a4", "letter", or "auto" *)
    }

    (** Shipping cost for invoice *)
    type shipping_cost = {
      shipping_rate : string option;  (** ID of an existing ShippingRate *)
      shipping_rate_data_display_name : string option;  (** Name for inline rate *)
      shipping_rate_data_type : string option;  (** "fixed_amount" *)
      shipping_rate_data_fixed_amount : int option;  (** Amount in cents *)
      shipping_rate_data_fixed_currency : string option;  (** Currency code *)
    }

    (** Create shipping_cost with an existing ShippingRate ID *)
    let shipping_cost_rate ~shipping_rate () =
      { shipping_rate = Some shipping_rate;
        shipping_rate_data_display_name = None;
        shipping_rate_data_type = None;
        shipping_rate_data_fixed_amount = None;
        shipping_rate_data_fixed_currency = None }

    (** Create shipping_cost with inline fixed amount *)
    let shipping_cost_fixed ~display_name ~amount ~currency () =
      { shipping_rate = None;
        shipping_rate_data_display_name = Some display_name;
        shipping_rate_data_type = Some "fixed_amount";
        shipping_rate_data_fixed_amount = Some amount;
        shipping_rate_data_fixed_currency = Some currency }

    (** Shipping details (address) for invoice *)
    type shipping_details = {
      name : string;
      address_line1 : string option;
      address_line2 : string option;
      address_city : string option;
      address_state : string option;
      address_postal_code : string option;
      address_country : string option;
      phone : string option;
    }

    (** Create shipping_details with name only *)
    let shipping_details_name ~name () =
      { name; address_line1 = None; address_line2 = None;
        address_city = None; address_state = None;
        address_postal_code = None; address_country = None;
        phone = None }

    (** Create shipping_details with full address *)
    let shipping_details_full ~name ?line1 ?line2 ?city ?state ?postal_code ?country ?phone () =
      { name; address_line1 = line1; address_line2 = line2;
        address_city = city; address_state = state;
        address_postal_code = postal_code; address_country = country;
        phone }

    (** Discount to apply to an invoice *)
    type discount = {
      coupon : string option;          (** Coupon ID *)
      discount : string option;        (** Existing discount ID to clone *)
      promotion_code : string option;  (** Promotion code ID *)
    }

    (** Create discount from a coupon ID *)
    let discount_coupon ~coupon () =
      { coupon = Some coupon; discount = None; promotion_code = None }

    (** Create discount from a promotion code ID *)
    let discount_promotion_code ~promotion_code () =
      { coupon = None; discount = None; promotion_code = Some promotion_code }

    (** Create discount from an existing discount ID *)
    let discount_existing ~discount () =
      { coupon = None; discount = Some discount; promotion_code = None }

    (** Transfer data for Connect platforms *)
    type transfer_data = {
      destination : string;   (** Connected account ID *)
      amount : int option;    (** Amount to transfer (defaults to full amount) *)
    }

    (** Create transfer_data for Connect *)
    let transfer_data ~destination ?amount () =
      { destination; amount }

    (** Issuer configuration for Connect *)
    type issuer = {
      issuer_type : string;    (** "account" or "self" *)
      account : string option; (** Connected account ID when type is "account" *)
    }

    (** Create issuer for self *)
    let issuer_self () = { issuer_type = "self"; account = None }

    (** Create issuer for a connected account *)
    let issuer_account ~account () = { issuer_type = "account"; account = Some account }

    (** Create an invoice.
        @param customer The customer ID
        @param subscription Optional subscription ID to invoice
        @param description Optional description
        @param auto_advance Whether to auto-advance the invoice
        @param collection_method How to collect payment
        @param days_until_due Days until due (for Send_invoice collection)
        @param due_date Unix timestamp for due date
        @param automatic_tax Automatic tax configuration
        @param payment_method_types List of allowed payment method types (e.g., ["card"; "bank_transfer"])
        @param pending_invoice_items_behavior "include" or "exclude" pending items
        @param custom_fields Up to 4 custom fields for the invoice
        @param footer Footer text for the invoice
        @param rendering Rendering options for invoice display
        @param default_payment_method Default payment method ID
        @param default_tax_rates Default tax rate IDs
        @param currency Invoice currency
        @param shipping_cost Shipping cost (rate ID or inline data)
        @param shipping_details Shipping address details
        @param discounts List of discounts to apply
        @param application_fee_amount Application fee for Connect (in cents)
        @param on_behalf_of Connected account ID for Connect
        @param transfer_data Transfer data for Connect
        @param issuer Issuer configuration for Connect
        @param account_tax_ids Tax IDs for the account
        @param statement_descriptor Statement descriptor for credit card
        @param effective_at Effective date timestamp
        @param metadata Key-value metadata *)
    let create ~config ?idempotency_key ~customer ?subscription ?description
        ?auto_advance ?collection_method ?days_until_due ?due_date
        ?automatic_tax ?payment_method_types ?pending_invoice_items_behavior 
        ?custom_fields ?footer ?rendering ?default_payment_method 
        ?default_tax_rates ?currency ?shipping_cost ?shipping_details 
        ?discounts ?application_fee_amount ?on_behalf_of ?transfer_data
        ?issuer ?account_tax_ids ?statement_descriptor ?effective_at
        ?metadata () =
      let options = { default_request_options with idempotency_key } in
      let params = [("customer", customer)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("subscription", v)) subscription;
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("auto_advance", string_of_bool v)) auto_advance;
        Option.map (fun v -> ("collection_method", collection_method_to_string v)) collection_method;
        Option.map (fun v -> ("days_until_due", string_of_int v)) days_until_due;
        Option.map (fun v -> ("due_date", string_of_int v)) due_date;
        Option.map (fun v -> ("pending_invoice_items_behavior", v)) pending_invoice_items_behavior;
        Option.map (fun v -> ("footer", v)) footer;
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
        Option.map (fun v -> ("currency", v)) currency;
        Option.map (fun v -> ("application_fee_amount", string_of_int v)) application_fee_amount;
        Option.map (fun v -> ("on_behalf_of", v)) on_behalf_of;
        Option.map (fun v -> ("statement_descriptor", v)) statement_descriptor;
        Option.map (fun v -> ("effective_at", string_of_int v)) effective_at;
      ] in
      (* Add automatic_tax configuration *)
      let params = match automatic_tax with
        | Some at ->
          params @ [("automatic_tax[enabled]", string_of_bool at.enabled)] @
          (match at.liability_type with
           | Some lt -> [("automatic_tax[liability][type]", lt)]
           | None -> []) @
          (match at.liability_account with
           | Some acc -> [("automatic_tax[liability][account]", acc)]
           | None -> [])
        | None -> params
      in
      (* Add payment_method_types as array *)
      let params = match payment_method_types with
        | Some types ->
          params @ List.mapi (fun i t ->
            (Printf.sprintf "payment_settings[payment_method_types][%d]" i, t)
          ) types
        | None -> params
      in
      (* Add custom_fields as array *)
      let params = match custom_fields with
        | Some fields ->
          params @ List.concat (List.mapi (fun i (f : custom_field) ->
            [(Printf.sprintf "custom_fields[%d][name]" i, f.name);
             (Printf.sprintf "custom_fields[%d][value]" i, f.value)]
          ) fields)
        | None -> params
      in
      (* Add rendering options *)
      let params = match rendering with
        | Some r ->
          params @
          (match r.amount_tax_display with
           | Some v -> [("rendering[amount_tax_display]", v)]
           | None -> []) @
          (match r.pdf_page_size with
           | Some v -> [("rendering[pdf][page_size]", v)]
           | None -> [])
        | None -> params
      in
      (* Add default_tax_rates as array *)
      let params = match default_tax_rates with
        | Some rates ->
          params @ List.mapi (fun i r ->
            (Printf.sprintf "default_tax_rates[%d]" i, r)
          ) rates
        | None -> params
      in
      (* Add shipping_cost *)
      let params = match shipping_cost with
        | Some sc ->
          params @
          (match sc.shipping_rate with
           | Some r -> [("shipping_cost[shipping_rate]", r)]
           | None -> []) @
          (match sc.shipping_rate_data_display_name with
           | Some n -> [("shipping_cost[shipping_rate_data][display_name]", n)]
           | None -> []) @
          (match sc.shipping_rate_data_type with
           | Some t -> [("shipping_cost[shipping_rate_data][type]", t)]
           | None -> []) @
          (match sc.shipping_rate_data_fixed_amount, sc.shipping_rate_data_fixed_currency with
           | Some amt, Some curr ->
             [("shipping_cost[shipping_rate_data][fixed_amount][amount]", string_of_int amt);
              ("shipping_cost[shipping_rate_data][fixed_amount][currency]", curr)]
           | _ -> [])
        | None -> params
      in
      (* Add shipping_details *)
      let params = match shipping_details with
        | Some sd ->
          params @ [("shipping_details[name]", sd.name)] @
          List.filter_map Fun.id [
            Option.map (fun v -> ("shipping_details[address][line1]", v)) sd.address_line1;
            Option.map (fun v -> ("shipping_details[address][line2]", v)) sd.address_line2;
            Option.map (fun v -> ("shipping_details[address][city]", v)) sd.address_city;
            Option.map (fun v -> ("shipping_details[address][state]", v)) sd.address_state;
            Option.map (fun v -> ("shipping_details[address][postal_code]", v)) sd.address_postal_code;
            Option.map (fun v -> ("shipping_details[address][country]", v)) sd.address_country;
            Option.map (fun v -> ("shipping_details[phone]", v)) sd.phone;
          ]
        | None -> params
      in
      (* Add discounts as array *)
      let params = match discounts with
        | Some ds ->
          params @ List.concat (List.mapi (fun i (d : discount) ->
            List.filter_map Fun.id [
              Option.map (fun v -> (Printf.sprintf "discounts[%d][coupon]" i, v)) d.coupon;
              Option.map (fun v -> (Printf.sprintf "discounts[%d][discount]" i, v)) d.discount;
              Option.map (fun v -> (Printf.sprintf "discounts[%d][promotion_code]" i, v)) d.promotion_code;
            ]
          ) ds)
        | None -> params
      in
      (* Add transfer_data for Connect *)
      let params = match transfer_data with
        | Some td ->
          params @ [("transfer_data[destination]", td.destination)] @
          (match td.amount with
           | Some amt -> [("transfer_data[amount]", string_of_int amt)]
           | None -> [])
        | None -> params
      in
      (* Add issuer for Connect *)
      let params = match issuer with
        | Some iss ->
          params @ [("issuer[type]", iss.issuer_type)] @
          (match iss.account with
           | Some acc -> [("issuer[account]", acc)]
           | None -> [])
        | None -> params
      in
      (* Add account_tax_ids as array *)
      let params = match account_tax_ids with
        | Some ids ->
          params @ List.mapi (fun i id ->
            (Printf.sprintf "account_tax_ids[%d]" i, id)
          ) ids
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config ~options ~path:"/v1/invoices" ~params () >>=
      handle_response ~parse_ok:of_json

    (** Update a draft invoice.
        @param id The invoice ID
        @param description Optional description
        @param auto_advance Whether to auto-advance the invoice
        @param collection_method How to collect payment
        @param days_until_due Days until due (for Send_invoice collection)
        @param due_date Unix timestamp for due date
        @param automatic_tax Automatic tax configuration
        @param payment_method_types List of allowed payment method types
        @param custom_fields Up to 4 custom fields for the invoice
        @param footer Footer text for the invoice
        @param rendering Rendering options for invoice display
        @param default_payment_method Default payment method ID
        @param default_tax_rates Default tax rate IDs
        @param shipping_cost Shipping cost (rate ID or inline data)
        @param shipping_details Shipping address details
        @param discounts List of discounts to apply
        @param application_fee_amount Application fee for Connect (in cents)
        @param on_behalf_of Connected account ID for Connect
        @param transfer_data Transfer data for Connect
        @param issuer Issuer configuration for Connect
        @param account_tax_ids Tax IDs for the account
        @param statement_descriptor Statement descriptor for credit card
        @param effective_at Effective date timestamp
        @param metadata Key-value metadata *)
    let update ~config ~id ?description ?auto_advance ?collection_method 
        ?days_until_due ?due_date ?automatic_tax ?payment_method_types
        ?custom_fields ?footer ?rendering ?default_payment_method 
        ?default_tax_rates ?shipping_cost ?shipping_details 
        ?discounts ?application_fee_amount ?on_behalf_of ?transfer_data
        ?issuer ?account_tax_ids ?statement_descriptor ?effective_at ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
        Option.map (fun v -> ("auto_advance", string_of_bool v)) auto_advance;
        Option.map (fun v -> ("collection_method", collection_method_to_string v)) collection_method;
        Option.map (fun v -> ("days_until_due", string_of_int v)) days_until_due;
        Option.map (fun v -> ("due_date", string_of_int v)) due_date;
        Option.map (fun v -> ("footer", v)) footer;
        Option.map (fun v -> ("default_payment_method", v)) default_payment_method;
        Option.map (fun v -> ("application_fee_amount", string_of_int v)) application_fee_amount;
        Option.map (fun v -> ("on_behalf_of", v)) on_behalf_of;
        Option.map (fun v -> ("statement_descriptor", v)) statement_descriptor;
        Option.map (fun v -> ("effective_at", string_of_int v)) effective_at;
      ] in
      (* Add automatic_tax configuration *)
      let params = match automatic_tax with
        | Some at ->
          params @ [("automatic_tax[enabled]", string_of_bool at.enabled)] @
          (match at.liability_type with
           | Some lt -> [("automatic_tax[liability][type]", lt)]
           | None -> []) @
          (match at.liability_account with
           | Some acc -> [("automatic_tax[liability][account]", acc)]
           | None -> [])
        | None -> params
      in
      (* Add payment_method_types as array *)
      let params = match payment_method_types with
        | Some types ->
          params @ List.mapi (fun i t ->
            (Printf.sprintf "payment_settings[payment_method_types][%d]" i, t)
          ) types
        | None -> params
      in
      (* Add custom_fields as array *)
      let params = match custom_fields with
        | Some fields ->
          params @ List.concat (List.mapi (fun i (f : custom_field) ->
            [(Printf.sprintf "custom_fields[%d][name]" i, f.name);
             (Printf.sprintf "custom_fields[%d][value]" i, f.value)]
          ) fields)
        | None -> params
      in
      (* Add rendering options *)
      let params = match rendering with
        | Some r ->
          params @
          (match r.amount_tax_display with
           | Some v -> [("rendering[amount_tax_display]", v)]
           | None -> []) @
          (match r.pdf_page_size with
           | Some v -> [("rendering[pdf][page_size]", v)]
           | None -> [])
        | None -> params
      in
      (* Add default_tax_rates as array *)
      let params = match default_tax_rates with
        | Some rates ->
          params @ List.mapi (fun i r ->
            (Printf.sprintf "default_tax_rates[%d]" i, r)
          ) rates
        | None -> params
      in
      (* Add shipping_cost *)
      let params = match shipping_cost with
        | Some sc ->
          params @
          (match sc.shipping_rate with
           | Some r -> [("shipping_cost[shipping_rate]", r)]
           | None -> []) @
          (match sc.shipping_rate_data_display_name with
           | Some n -> [("shipping_cost[shipping_rate_data][display_name]", n)]
           | None -> []) @
          (match sc.shipping_rate_data_type with
           | Some t -> [("shipping_cost[shipping_rate_data][type]", t)]
           | None -> []) @
          (match sc.shipping_rate_data_fixed_amount, sc.shipping_rate_data_fixed_currency with
           | Some amt, Some curr ->
             [("shipping_cost[shipping_rate_data][fixed_amount][amount]", string_of_int amt);
              ("shipping_cost[shipping_rate_data][fixed_amount][currency]", curr)]
           | _ -> [])
        | None -> params
      in
      (* Add shipping_details *)
      let params = match shipping_details with
        | Some sd ->
          params @ [("shipping_details[name]", sd.name)] @
          List.filter_map Fun.id [
            Option.map (fun v -> ("shipping_details[address][line1]", v)) sd.address_line1;
            Option.map (fun v -> ("shipping_details[address][line2]", v)) sd.address_line2;
            Option.map (fun v -> ("shipping_details[address][city]", v)) sd.address_city;
            Option.map (fun v -> ("shipping_details[address][state]", v)) sd.address_state;
            Option.map (fun v -> ("shipping_details[address][postal_code]", v)) sd.address_postal_code;
            Option.map (fun v -> ("shipping_details[address][country]", v)) sd.address_country;
            Option.map (fun v -> ("shipping_details[phone]", v)) sd.phone;
          ]
        | None -> params
      in
      (* Add discounts as array *)
      let params = match discounts with
        | Some ds ->
          params @ List.concat (List.mapi (fun i (d : discount) ->
            List.filter_map Fun.id [
              Option.map (fun v -> (Printf.sprintf "discounts[%d][coupon]" i, v)) d.coupon;
              Option.map (fun v -> (Printf.sprintf "discounts[%d][discount]" i, v)) d.discount;
              Option.map (fun v -> (Printf.sprintf "discounts[%d][promotion_code]" i, v)) d.promotion_code;
            ]
          ) ds)
        | None -> params
      in
      (* Add transfer_data for Connect *)
      let params = match transfer_data with
        | Some td ->
          params @ [("transfer_data[destination]", td.destination)] @
          (match td.amount with
           | Some amt -> [("transfer_data[amount]", string_of_int amt)]
           | None -> [])
        | None -> params
      in
      (* Add issuer for Connect *)
      let params = match issuer with
        | Some iss ->
          params @ [("issuer[type]", iss.issuer_type)] @
          (match iss.account with
           | Some acc -> [("issuer[account]", acc)]
           | None -> [])
        | None -> params
      in
      (* Add account_tax_ids as array *)
      let params = match account_tax_ids with
        | Some ids ->
          params @ List.mapi (fun i id ->
            (Printf.sprintf "account_tax_ids[%d]" i, id)
          ) ids
        | None -> params
      in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let finalize_invoice ~config ~id ?auto_advance () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("auto_advance", string_of_bool v)) auto_advance;
      ] in
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      post ~config ~path:(path ^ "/finalize") ~params () >>=
      handle_response ~parse_ok:of_json

    let send ~config ~id () =
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      post ~config ~path:(path ^ "/send") () >>=
      handle_response ~parse_ok:of_json

    let mark_uncollectible ~config ~id () =
      let path = path_with_id ~base:"/v1/invoices/" ~resource_type:"invoice" id in
      post ~config ~path:(path ^ "/mark_uncollectible") () >>=
      handle_response ~parse_ok:of_json

    (** Subscription item for upcoming invoice preview.
        Used to simulate changes to a subscription. *)
    type upcoming_subscription_item = {
      id : string option;       (** Existing subscription item ID (si_xxx) for updates *)
      price : string option;    (** Price ID for new or updated item *)
      quantity : int option;    (** Quantity for the item *)
      deleted : bool option;    (** Set to true to remove this item *)
    }

    (** Create an upcoming subscription item for adding a new price *)
    let upcoming_item_add ~price ?quantity () =
      { id = None; price = Some price; quantity; deleted = None }

    (** Create an upcoming subscription item for updating an existing item *)
    let upcoming_item_update ~id ?price ?quantity () =
      { id = Some id; price; quantity; deleted = None }

    (** Create an upcoming subscription item for deleting an existing item *)
    let upcoming_item_delete ~id () =
      { id = Some id; price = None; quantity = None; deleted = Some true }

    (** Preview an upcoming invoice.
        @param customer The customer ID
        @param subscription Optional existing subscription ID to preview changes to
        @param subscription_items List of item changes to simulate
        @param subscription_proration_behavior How to handle proration
        @param subscription_proration_date Timestamp for proration calculation *)
    let upcoming ~config ~customer ?subscription ?subscription_items
        ?subscription_proration_behavior ?subscription_proration_date () =
      let params = [("customer", customer)] in
      let params = params @ List.filter_map Fun.id [
        Option.map (fun v -> ("subscription", v)) subscription;
        Option.map (fun v -> ("subscription_proration_behavior", v)) subscription_proration_behavior;
        Option.map (fun v -> ("subscription_proration_date", string_of_int v)) subscription_proration_date;
      ] in
      (* Build subscription_items array *)
      let params = match subscription_items with
        | Some items ->
          params @ List.concat (List.mapi (fun i item ->
            let base = Printf.sprintf "subscription_items[%d]" i in
            List.filter_map Fun.id [
              Option.map (fun v -> (base ^ "[id]", v)) item.id;
              Option.map (fun v -> (base ^ "[price]", v)) item.price;
              Option.map (fun v -> (base ^ "[quantity]", string_of_int v)) item.quantity;
              Option.map (fun v -> (base ^ "[deleted]", string_of_bool v)) item.deleted;
            ]
          ) items)
        | None -> params
      in
      get ~config ~path:"/v1/invoices/upcoming" ~params () >>=
      handle_response ~parse_ok:of_json
  end

  (** Event API *)
  module Event = struct
    let retrieve ~config ~id () =
      let path = path_with_id ~base:"/v1/events/" ~resource_type:"event" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/payment_methods/" ~resource_type:"payment_method" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let attach ~config ~id ~customer () =
      let params = [("customer", customer)] in
      let path = path_with_id ~base:"/v1/payment_methods/" ~resource_type:"payment_method" id in
      post ~config ~path:(path ^ "/attach") ~params () >>=
      handle_response ~parse_ok:of_json

    let detach ~config ~id () =
      let path = path_with_id ~base:"/v1/payment_methods/" ~resource_type:"payment_method" id in
      post ~config ~path:(path ^ "/detach") () >>=
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
      let path = path_with_id ~base:"/v1/setup_intents/" ~resource_type:"setup_intent" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let confirm ~config ~id ?payment_method () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("payment_method", v)) payment_method;
      ] in
      let path = path_with_id ~base:"/v1/setup_intents/" ~resource_type:"setup_intent" id in
      post ~config ~path:(path ^ "/confirm") ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      let path = path_with_id ~base:"/v1/setup_intents/" ~resource_type:"setup_intent" id in
      post ~config ~path:(path ^ "/cancel") () >>=
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
      let path = path_with_id ~base:"/v1/coupons/" ~resource_type:"coupon" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?name ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("name", v)) name;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/coupons/" ~resource_type:"coupon" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      let path = path_with_id ~base:"/v1/coupons/" ~resource_type:"coupon" id in
      delete ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/balance_transactions/" ~resource_type:"balance_transaction" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/payouts/" ~resource_type:"payout" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      let path = path_with_id ~base:"/v1/payouts/" ~resource_type:"payout" id in
      post ~config ~path:(path ^ "/cancel") () >>=
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
      let path = path_with_id ~base:"/v1/checkout/sessions/" ~resource_type:"checkout_session" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let expire ~config ~id () =
      let path = path_with_id ~base:"/v1/checkout/sessions/" ~resource_type:"checkout_session" id in
      post ~config ~path:(path ^ "/expire") () >>=
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
      let path = path_with_id ~base:"/v1/tax_rates/" ~resource_type:"tax_rate" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/tax_rates/" ~resource_type:"tax_rate" id in
      post ~config ~path ~params () >>=
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
      let path = path_with_id ~base:"/v1/payment_links/" ~resource_type:"payment_link" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?active ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/payment_links/" ~resource_type:"payment_link" id in
      post ~config ~path ~params () >>=
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
      let path = path_with_id ~base:"/v1/disputes/" ~resource_type:"dispute" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?metadata () =
      let params = match metadata with
        | Some m -> List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> []
      in
      let path = path_with_id ~base:"/v1/disputes/" ~resource_type:"dispute" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let close ~config ~id () =
      let path = path_with_id ~base:"/v1/disputes/" ~resource_type:"dispute" id in
      post ~config ~path:(path ^ "/close") () >>=
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
      let path = path_with_id ~base:"/v1/accounts/" ~resource_type:"account" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/accounts/" ~resource_type:"account" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      let path = path_with_id ~base:"/v1/accounts/" ~resource_type:"account" id in
      delete ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/transfers/" ~resource_type:"transfer" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/transfers/" ~resource_type:"transfer" id in
      post ~config ~path ~params () >>=
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
      let path = path_with_id ~base:"/v1/files/" ~resource_type:"file" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/file_links/" ~resource_type:"file_link" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?expires_at ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("expires_at", string_of_int v)) expires_at;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/file_links/" ~resource_type:"file_link" id in
      post ~config ~path ~params () >>=
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
      let path = path_with_id ~base:"/v1/mandates/" ~resource_type:"mandate" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:Stripe.Mandate.of_json
  end

  (** Review API (Radar) *)
  module Review = struct
    open Stripe.Review

    let retrieve ~config ~id () =
      let path = path_with_id ~base:"/v1/reviews/" ~resource_type:"review" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let approve ~config ~id () =
      let path = path_with_id ~base:"/v1/reviews/" ~resource_type:"review" id in
      post ~config ~path:(path ^ "/approve") () >>=
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
      let path = path_with_id ~base:"/v1/promotion_codes/" ~resource_type:"promotion_code" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?active ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("active", string_of_bool v)) active;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/promotion_codes/" ~resource_type:"promotion_code" id in
      post ~config ~path ~params () >>=
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
      let path = path_with_id ~base:"/v1/invoiceitems/" ~resource_type:"invoice_item" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/invoiceitems/" ~resource_type:"invoice_item" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      let path = path_with_id ~base:"/v1/invoiceitems/" ~resource_type:"invoice_item" id in
      delete ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/quotes/" ~resource_type:"quote" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/quotes/" ~resource_type:"quote" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let finalize_quote ~config ~id () =
      let path = path_with_id ~base:"/v1/quotes/" ~resource_type:"quote" id in
      post ~config ~path:(path ^ "/finalize") () >>=
      handle_response ~parse_ok:of_json

    let accept ~config ~id () =
      let path = path_with_id ~base:"/v1/quotes/" ~resource_type:"quote" id in
      post ~config ~path:(path ^ "/accept") () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      let path = path_with_id ~base:"/v1/quotes/" ~resource_type:"quote" id in
      post ~config ~path:(path ^ "/cancel") () >>=
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
      let path = path_with_id ~base:"/v1/credit_notes/" ~resource_type:"credit_note" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?memo ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("memo", v)) memo;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/credit_notes/" ~resource_type:"credit_note" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let void_credit_note ~config ~id () =
      let path = path_with_id ~base:"/v1/credit_notes/" ~resource_type:"credit_note" id in
      post ~config ~path:(path ^ "/void") () >>=
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
      let path = path_with_id ~base:"/v1/application_fees/" ~resource_type:"application_fee" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/topups/" ~resource_type:"topup" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?description ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/topups/" ~resource_type:"topup" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      let path = path_with_id ~base:"/v1/topups/" ~resource_type:"topup" id in
      post ~config ~path:(path ^ "/cancel") () >>=
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
      let path = path_with_id ~base:"/v1/subscription_items/" ~resource_type:"subscription_item" id in
      get ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/subscription_items/" ~resource_type:"subscription_item" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let delete ~config ~id () =
      let path = path_with_id ~base:"/v1/subscription_items/" ~resource_type:"subscription_item" id in
      delete ~config ~path () >>=
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
      let path = path_with_id ~base:"/v1/subscription_items/" ~resource_type:"subscription_item" id in
      post ~config ~options 
        ~path:(path ^ "/usage_records") 
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
      let path = path_with_id ~base:"/v1/subscription_schedules/" ~resource_type:"subscription_schedule" id in
      get ~config ~path () >>=
      handle_response ~parse_ok:of_json

    let update ~config ~id ?end_behavior ?metadata () =
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("end_behavior", v)) end_behavior;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      let path = path_with_id ~base:"/v1/subscription_schedules/" ~resource_type:"subscription_schedule" id in
      post ~config ~path ~params () >>=
      handle_response ~parse_ok:of_json

    let cancel ~config ~id () =
      let path = path_with_id ~base:"/v1/subscription_schedules/" ~resource_type:"subscription_schedule" id in
      post ~config ~path:(path ^ "/cancel") () >>=
      handle_response ~parse_ok:of_json

    let release ~config ~id () =
      let path = path_with_id ~base:"/v1/subscription_schedules/" ~resource_type:"subscription_schedule" id in
      post ~config ~path:(path ^ "/release") () >>=
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
      let validated_customer_id = Stripe_core.validate_id ~resource_type:"customer" customer_id in
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
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions" validated_customer_id) 
        ~params () >>=
      handle_response ~parse_ok:of_json

    (** Retrieve a customer balance transaction *)
    let retrieve ~config ~customer_id ~transaction_id () =
      let validated_customer_id = Stripe_core.validate_id ~resource_type:"customer" customer_id in
      let validated_transaction_id = Stripe_core.validate_id ~resource_type:"customer_balance_transaction" transaction_id in
      get ~config 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions/%s" 
                 validated_customer_id validated_transaction_id) () >>=
      handle_response ~parse_ok:of_json

    (** Update a customer balance transaction *)
    let update ~config ~customer_id ~transaction_id ?description ?metadata () =
      let validated_customer_id = Stripe_core.validate_id ~resource_type:"customer" customer_id in
      let validated_transaction_id = Stripe_core.validate_id ~resource_type:"customer_balance_transaction" transaction_id in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("description", v)) description;
      ] in
      let params = match metadata with
        | Some m -> params @ List.map (fun (k, v) -> ("metadata[" ^ k ^ "]", v)) m
        | None -> params
      in
      post ~config 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions/%s" 
                 validated_customer_id validated_transaction_id) 
        ~params () >>=
      handle_response ~parse_ok:of_json

    (** List customer balance transactions *)
    let list ~config ~customer_id ?limit ?starting_after ?ending_before () =
      let validated_customer_id = Stripe_core.validate_id ~resource_type:"customer" customer_id in
      let params = List.filter_map Fun.id [
        Option.map (fun v -> ("limit", string_of_int v)) limit;
        Option.map (fun v -> ("starting_after", v)) starting_after;
        Option.map (fun v -> ("ending_before", v)) ending_before;
      ] in
      get ~config 
        ~path:(Printf.sprintf "/v1/customers/%s/balance_transactions" validated_customer_id) 
        ~params () >>=
      handle_response ~parse_ok:(Stripe.List_response.of_json of_json)
  end
end
