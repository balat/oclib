(* Copyright Vincent Balat - not part of BeSport *)

{shared{
  open Eliom_content.Html5
  open Eliom_content.Html5.F
}}


{client{
let popup ?(a = []) ?(closeable = true) ?(onclose = fun () -> ()) gen_content =
  let popup = ref None in
  let close () =
    Dom_html.document##body##style##overflow <- Js.string "auto";
    Eliom_lib.Option.iter Manip.removeSelf !popup;
    onclose ()
  in
  lwt content = Ow_spinner.with_spinner ~a:[a_class ["ow_popup_content"]]
      (gen_content close)
  in
  let content = [content] in
  let content = if closeable
    then begin
      let close_button = Ow_icons.D.close () in
      Lwt_js_events.async (fun () ->
        Lwt_js_events.clicks (To_dom.of_element close_button)
          (fun _ _ ->
             close ();
             Lwt.return ()
          ));
      close_button :: content
    end
    else content
  in
  let box = D.div ~a:(a_class ["ow_background"]::a)
      [div ~a:[a_class ["ow_popup"]] content]
  in
  popup := Some box;
  Dom_html.document##body##style##overflow <- Js.string "hidden";
  Manip.appendToBody box;
  Lwt.return ()
}}


{shared{

(** Box with header, content and footer.
    Header and footer can be empty (default) and
    have fix size. Content has scrollbar if too high. *)
let hcf ?(a=[]) ?(header=[]) ?(footer=[]) content =
  D.section
    ~a:(a_class ["w_hcf"] :: (a :> [ Html5_types.div_attrib ] attrib list))
    [F.header header;
     div ~a:[a_class ["w_content"]] content;
     F.footer footer]
(*VVV We need D for some uses. But would be great to have D.hcf and F.hcf. *)

}}


{client{

   let yesno_popup text l close =
     let buttons =
       List.map
         (fun (s, f) ->
            let b = D.Raw.button [s] in
            Lwt.async (fun () ->
              Lwt_js_events.clicks (To_dom.of_element b)
                (fun _ _ -> lwt () = f () in close (); Lwt.return ()));
            b)
         l
     in
     let l = div text::buttons in
     Lwt.return (D.div ~a:[a_class ["yesno"]] l)

let ask_question text l =
  let t, w = Lwt.wait () in
  let wrap f () =
    lwt r = f () in
    Lwt.wakeup w r;
    Lwt.return ()
  in
  lwt () = popup ~closeable:false (fun close ->
    yesno_popup text (List.map (fun (v, f) -> (v, wrap f)) l) close)
  in
  t


 }}
