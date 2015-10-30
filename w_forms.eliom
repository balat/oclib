(* Copyright Vincent Balat - not part of BeSport *)


{shared{
  open Eliom_content.Html5
  open Eliom_content.Html5.F


  let opcls = "w_subf_focused"
  let hcfcls = "w_hcf"
}}

{shared{

   let input ?(a = []) ~value () =
     let e = D.Raw.input ~a:(a_value value::a) () in
     let signal, set_signal = Eliom_shared.React.S.create value in
     ignore {unit{
       Lwt.async (fun () ->
         let e = To_dom.of_input %e in
         Lwt_js_events.inputs e
           (fun _ _ -> %set_signal (Js.to_string e##value); Lwt.return ())
       )
     }};
     e, signal

   (*VVV No idea what this should really do. *)
   (** If optional parameter signal is present,
       widget is closed when signal changes. *)
   (*VVV Ahahah! *)
   let subform ?(ok = false) ?signal input title content =
     let header = D.header [h2 title] in
     let button = if ok
       then Some (D.button ~button_type:`Button [Bs_i18n.F.ok ()])
       else None
     in
     let footer = footer (match button with Some b -> [b] | _ -> []) in
     let d0 =
       D.div ~a:[a_class ["w_subf_int"]]
         [header; (content :> [Html5_types.div_content_fun] elt); footer] in
     (* The external div has dark transparent background,
        the internal one (d0) is white. *)
     let d = D.div ~a:[a_class ["fieldset"; "w_subf"]] [d0] in
     (* I am not using <fieldset> because they cannot be flex *)
     let _ = {unit{
       let opened = ref false in
       let op () = if not !opened
         then begin
           opened := true;
           Manip.Class.add %d opcls;
           Manip.Class.add %d0 hcfcls
         end
       in
       let cl () =
         Manip.Class.remove %d opcls;
         Manip.Class.remove %d0 hcfcls;
         opened := false
       in
       let _ = Eliom_lib.Option.map (Eliom_shared.React.S.map (fun _ -> cl ())) %signal in
       let inp' = To_dom.of_input %input in
       Lwt.async (fun () ->
         Lwt_js_events.clicks inp' (fun _ _ -> op (); Lwt.return ()));
       Lwt.async (fun () ->
         Lwt_js_events.keyups inp' (fun _ _ -> op (); Lwt.return ()));
       Lwt.async (fun () ->
         Lwt_js_events.clicks (To_dom.of_element %header)
           (fun _ _ -> cl (); Lwt.return ()));
       Lwt.async (fun () ->
         Eliom_lib.Option.Lwt.iter
           (fun button -> Lwt_js_events.clicks (To_dom.of_element button)
             (fun _ _ -> cl (); Lwt.return ()))
           %button);
     }} in
     d

(** Form element. *)
let form_element ?(a=[]) header content =
  (* I am not using <fieldset> because they cannot be flex *)
  div ~a:(a_class ["fieldset"] :: (a :> [Html5_types.div_attrib ] attrib list))
    [F.header header;
     div ~a:[a_class ["w_content"]] content]

 }}
