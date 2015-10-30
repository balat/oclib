{shared{
open Eliom_content.Html5
open Html5_types

val hcf :
  ?a:[< div_attrib ] attrib list ->
  ?header:[< flow5_without_header_footer ] elt list ->
  ?footer:[< flow5_without_header_footer ] elt list ->
  [< div_content_fun ] elt list ->
  [> section ] elt
}}

{client{
val ask_question :
  [< div_content_fun ] elt list ->
  ([< button_content_fun ] elt * (unit -> 'a Lwt.t)) list ->
  'a Lwt.t

val popup :
  ?a:[< div_attrib > `Class ] attrib list ->
  ?closeable:bool ->
  ?onclose:(unit -> unit) ->
  ((unit -> unit) -> [< div_content_fun ] elt Lwt.t) ->
  unit Lwt.t

val yesno_popup :
  [< div_content_fun ] elt list ->
  ([< button_content_fun ] elt * (unit -> unit Lwt.t)) list ->
  (unit -> unit) ->
  [> div ] elt Lwt.t


}}
