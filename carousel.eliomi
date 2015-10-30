(* Copyright Vincent Balat *)

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F
}}

{shared{
(** Creates a carousel with the elements of the list.
    [?position] is the initial position (default 0).
    [?update] is a react event you can use to command the carousel from outside.
    [?disabled] is always [false] by default. When [true], it is not possible
    to change page.
    It returns the element, the current position (as a react signal),
    and the number of elements visible (more than 1 if carousel window is
    set, in CSS, to a value larger than elements width.
    The width of all elements is supposed to be equal,
    and the size of carousel must be a multiple of the size of elements.).
*)
val make :
  ?a: [< Html5_types.div_attrib > `Class ] Eliom_content.Html5.D.attrib list ->
  ?position:int ->
  ?update: [> `Goto of int | `Next | `Prev ] React.event client_value ->
  ?disabled: bool Eliom_shared.React.S.t ->
  [< Html5_types.div_content_fun ] Eliom_content.Html5.D.elt list ->
  [> Html5_types.div ] Eliom_content.Html5.D.elt *
  int Eliom_shared.React.S.t *
  int Eliom_shared.React.S.t

(** List of bullets for carousel. Current page has class ["active"].
    [pos] is a signal corresponding to current position.
    [change] is a function to change position of carousel.
    This is usually the function to trigger the event given as
    parameter to [make].
    [length] must be exactly the number of elements in the carousel.
    Optional parameter [attributes] makes possible to give an HTML attribute
    to each bullet (for example a reactive class).
    Optional parameter [size] is the number of elements visible at the same time
    in the carousel (return by function [make]).
 *)
val bullets :
  ?a:[< Html5_types.ul_attrib > `Class ] Eliom_content.Html5.F.attrib list ->
  ?attributes:[< Html5_types.li_attrib > `Class `OnClick ]
    Eliom_content.Html5.F.attrib list list ->
  change: ([> `Goto of int | `Next | `Prev ] -> unit) client_value ->
  pos:int Eliom_shared.React.S.t ->
  length:int ->
  ?size:int Eliom_shared.React.S.t ->
  unit -> [> Html5_types.ul ] Eliom_content.Html5.F.elt
}}
{client{
(** Make arrow keys cause event change.
    Returns a thread that never stops until you call Lwt.cancel on it. *)
val bind_arrow_keys :
  change: ([> `Goto of int | `Next | `Prev ] -> unit) ->
  unit Lwt.t
}}
