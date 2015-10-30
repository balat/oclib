(* Copyright Vincent Balat *)

class type hammer = object
  method on : Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth
end

val _Hammer :
  unit -> (Dom_html.element Js.t -> 'a Js.t -> hammer Js.t) Js.constr

val panmove : Dom_html.element Js.t -> Dom_html.event Js.t Lwt.t
val panstart : Dom_html.element Js.t -> Dom_html.event Js.t Lwt.t

val swipe : Dom_html.element Js.t -> Dom_html.event Js.t Lwt.t
val swipeleft : Dom_html.element Js.t -> Dom_html.event Js.t Lwt.t
val swiperight : Dom_html.element Js.t -> Dom_html.event Js.t Lwt.t


val swipes :
  ?cancel_handler:bool ->
  Dom_html.element Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val swipelefts :
  ?cancel_handler:bool ->
  Dom_html.element Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val swiperights :
  ?cancel_handler:bool ->
  Dom_html.element Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val panends :
  ?cancel_handler:bool ->
  Dom_html.element Js.t ->
  (Dom_html.event Js.t -> unit Lwt.t -> unit Lwt.t) -> unit Lwt.t

val panmoves :
  Dom_html.element Js.t ->
  (Dom_html.event Js.t -> Dom_html.element Js.t -> unit Lwt.t) -> unit

val panstarts :
  Dom_html.element Js.t ->
  (Dom_html.event Js.t -> Dom_html.element Js.t -> unit Lwt.t) -> unit
