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
