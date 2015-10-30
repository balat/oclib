
(* The query string and whether we have an empty set of suggestions *)
type query = { query : string; dataset : string }

type _ templates

val templates :
  ?not_found:(query -> Dom_html.element Js.t array) ->
  ?footer:(query -> Dom_html.element Js.t array) ->
  ?header:(query -> Dom_html.element Js.t array) ->
  ?suggestion:('a -> Dom_html.element Js.t) ->
  unit -> 'a templates

type _ dataset

val dataset :
  source:(string -> ('a array -> unit) -> ('a array -> unit) -> unit) ->
  async:bool ->
  ?name:string ->
  ?limit:int ->
  ?display:('a -> string) ->
  ?templates:'a templates ->
  unit -> 'a dataset

(* By default, an array of suggestions of this type is expected.
   You can use any other type if you set the suggestion template
   accordingly. *)
class type value = object
  method value : Js.js_string Js.t Js.readonly_prop
end

val value : string -> value Js.t

(* Collection of DOM elements *)
type _ collection

(* From a selector string *)
val select : string -> _ collection
(* From a single object *)
val element : #Dom_html.element Js.t -> _ collection

(* Turns any input[type="text"] element into a typeahead. *)
val create :
  'a collection -> ?highlight:bool -> ?hint:bool -> ?min_length:int ->
  'a dataset array -> unit

val destroy : _ collection -> unit
val open_menu : _ collection -> unit
val close_menu : _ collection -> unit
val current_value : _ collection -> string
val set_value : _ collection -> string -> unit

val on_open : 'a collection -> (unit -> unit) -> unit
val on_close : 'a collection -> (unit -> unit) -> unit
val on_cursor_change : 'a collection -> ('a -> unit) -> unit
val on_select : 'a collection -> ('a -> unit) -> unit
val on_autocomplete : 'a collection -> ('a -> unit) -> unit
