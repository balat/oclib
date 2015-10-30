(* Copyright Vincent Balat - not part of BeSport *)



(** This module is very experimental.
    I'm experimenting several interfaces for button,
    but haven't found the perfect one ... *)

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F

}}

{shared{

   (** Display a button which can have several states.
       Parameter [~state] is a react signal/set pair
       (current state of the button, for example a boolean pressed/unpressed).
       Parameter [~button] is a (possibly reactive) element that will
       be used as button.
       Parameter [~transition] is the transition function between states.
       To each state it associates the new state after pressing the button.
       If the state is a boolean, use client function [{{ not }}].
       Parameter [~action] is the client side function the will be called
       when the state changes. It takes the old state as parameter.
       During the action, the button temporarily receives class "blurred".
       If the action fails (by throwing an exception), the state will turn back
       to the previous value.
   *)
   let button ~state ~button ~transition ~action () =
     (* We need this D.div because we cannot use %button
        if button is a R node for now: *)
     let button = D.div ~a:[a_class ["wab"]] [button] in
     ignore {unit{
       let s, set = %state in
       Lwt.async (fun () ->
         Lwt_js_events.clicks (To_dom.of_element %button) (fun _ _ ->
           Manip.Class.add %button "blurred";
           let oldv = Eliom_shared.React.S.value s in
           let v = %transition oldv in
           set ?step:None v;
           lwt () = try_lwt
               %action oldv
             with _ -> set ?step:None oldv; Lwt.return ()
           in
           Manip.Class.remove %button "blurred";
           Lwt.return ()
         ))
     }};
     button

}}
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

}}
{client{
   let button_automata_ =
     ref (fun _ _ ->
       (assert false : Html5_types.button Eliom_content.Html5.D.elt))
}}

{shared{

   let rec button_automata old (text, cl, imp, action) =
     let cla = if imp then "imp"::cl else cl in
     let a = [a_class cla] in
     let b = D.button ~a ~button_type:`Button [pcdata text] in
     let _ = {unit{
       Lwt.async (fun () ->
         lwt ev = Lwt_js_events.click (To_dom.of_element %b) in
         match Manip.parentNode %b with
         | None -> Lwt.return ()
         | Some container ->
           let th, text2 = %action () in
           Manip.removeChildren container;
           let tmp =
             D.button ~a:[a_class ("blurred"::%cl)] ~button_type:`Button
               [pcdata text2]
           in
           Manip.appendChild container tmp;
           lwt result = try_lwt th
             with e -> Eliom_lib.debug_exn "exception: " e; Lwt.return None
           in
           Manip.removeChildren container;
           let new_state = match result with
             | None -> (* Failure. We turn back to the old state *) %old
             | Some s -> (* new state *) s
           in
           (match new_state with
            | State l ->
              List.iter
                (fun v ->
                   Manip.appendChild container (!button_automata_ new_state v))
                l
            | Final_state text -> Manip.appendChild container (pcdata text));
           Lwt.return ()
       )
     }}
     in
     b

 }}

{client{
   let _ = button_automata_ := button_automata
 }}

{shared{

   let display_automata ?a = function
     | State l as state ->
       let c = List.map (button_automata state) l in
       D.div ?a c
     | Final_state text -> D.div ?a [pcdata text]

}}

{shared{

   (** openable displays a button which opens a menu when clicked. *)

   (*VVV Problem:
     * close it when clicking outside

     We should probably remove the checkbox and just use a onclick.
     But i found this funny ;)
   *)
   let count = ref (Random.int 10000000)

   let openable ?(a = []) title content =
       let id = "w_op_"^string_of_int !count in
       count := !count + 1;
       let inp = D.Raw.input
           ~a:[a_id id; a_class ["w_op_i"]; a_input_type `Checkbox] ()
       in
       let inp' = {_ Js.t{ To_dom.of_input %inp }} in
       let op = {unit -> unit{ fun () -> %inp'##checked <- Js._true }} in
       let close = {_ -> _{ fun () -> %inp'##checked <- Js._false }} in
       div ~a:(a_class ["w_op"]::(a :> [Html5_types.div_attrib] attrib list))
         [inp;
          Raw.label ~a:[Raw.a_for id] [title];
          div ~a:[a_class ["w_op_c"]; a_onclick {{fun _ -> %close ()}}]
            [div content]
         (* I use two nested div elements,
            because in case the screen is narrow
            (or: in case of touch interface?)
            the widget is displayed centered, with full screen background.
            The onclick is also for this case: if we click outside, it closes
            the popup.
         *)
         ],
       op,
       close

 }}
