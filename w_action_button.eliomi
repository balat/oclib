

{shared{
type t =
  | State of (string (* text of the button *) *
              string list (* classes *) *
              bool * (* button has class "imp" *)
              (unit -> (t option (* action.
                                    None means "turn back in same state",
                                    for example in case of failure. *) Lwt.t *
                        string (* text of the button during action *)))
                client_value)
        list
  | Final_state of string

open Html5_types
open Eliom_content.Html5

val button :
  state:'a Eliom_shared.React.S.t *
        (?step:React.step -> 'a -> unit) Eliom_lib.shared_value->
  button:[< div_content_fun ] elt ->
  transition:('a -> 'a) Eliom_pervasives.client_value ->
  action:('a -> unit Lwt.t) Eliom_pervasives.client_value ->
  unit -> [> div ] elt

val display_automata :
  ?a:[< div_attrib ] attrib list -> t -> [> div ] elt

val openable :
  ?a:[< div_attrib ] attrib list -> [< label_content_fun ] elt ->
  [< div_content_fun ] elt list ->
  [> div ] elt *
  (unit -> unit) Eliom_pervasives.client_value *
  (unit -> unit) Eliom_pervasives.client_value

}}
