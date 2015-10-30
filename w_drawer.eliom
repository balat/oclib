(* Copyright Vincent Balat *)

{shared{
  open Eliom_content.Html5
  open Eliom_content.Html5.F
  
}}
{shared{

let drawer ?(a = []) drawer =
  let close_button = Ow_icons.D.close () in
  let open_button = D.Raw.span ~a:[a_class ["dr-toggle-button"]] [] in
  let dr = D.div ~a:[a_class ["dr-drawer"]] (close_button::drawer) in
  let d = D.div ~a:(a_class ["dr-container"] :: a)
      [open_button; dr]
  in

  let close = {unit -> unit{
    fun () -> Manip.Class.remove %d "open"
  }} in

  let open_ = {unit -> unit{
    fun () -> Manip.Class.add %d "open"
  }} in

  let _ = {unit{
    Lwt_js_events.async (fun () ->
      Lwt_js_events.clicks (To_dom.of_element %close_button)
        (fun ev _ ->
           Dom.preventDefault ev;
           Dom_html.stopPropagation ev;
           %close (); Lwt.return ()));
    Lwt_js_events.async (fun () ->
      Lwt_js_events.clicks (To_dom.of_element %open_button)
        (fun ev _ ->
           Dom.preventDefault ev;
           Dom_html.stopPropagation ev;
           %open_ (); Lwt.return ()));
  }}
  in


  let _ = {unit{
    (* Swipe to close: *)
    let dr = To_dom.of_element %dr in
    let cl = %close in
    let animation_frame_requested = ref false in
    let action = ref (`Move 0) in
    let perform_animation a =
      if !action = `Close && a = `Open
      then (* We received a panend after a swipeleft. We ignore it. *)
        Lwt.return ()
      else begin
        action := a;
        if not !animation_frame_requested
        then begin
          animation_frame_requested := true;
          lwt () = Lwt_js_events.request_animation_frame () in
          animation_frame_requested := false;
          (match !action with
           | `Move delta ->
             (* translate3d seems faster than left
                because forces acceleration *)
             let s = Js.string ("translate3d("^string_of_int delta^"px, 0, 0)") in
             (Js.Unsafe.coerce (dr##style))##transform <- s;
             (Js.Unsafe.coerce (dr##style))##webkitTransform <- s
           | `Close ->
             (Js.Unsafe.coerce (dr##style))##transform <- Js.string "";
             (Js.Unsafe.coerce (dr##style))##webkitTransform <- Js.string "";
             cl ()
           | `Open ->
             let s = Js.string "translate3d(0, 0, 0)" in
             (Js.Unsafe.coerce (dr##style))##transform <- s;
             (Js.Unsafe.coerce (dr##style))##webkitTransform <- s);
          Lwt.return ()
        end
        else Lwt.return ()
      end
    in
    let onpan ev _ =
      let d = (Js.Unsafe.coerce ev)##deltaX in
      if d <= 0
      then perform_animation (`Move d)
      else Lwt.return ()
    in
    Hammer.panstarts dr (fun ev a ->
      (Js.Unsafe.coerce (dr##style))##transition <-
        Js.string "-webkit-transform 0s, transform 0s";
      onpan ev a);
    Hammer.panmoves dr onpan;
    Lwt.async (fun () ->
      Hammer.panends dr (fun ev _ ->
        (Js.Unsafe.coerce (dr##style))##transition <-
          Js.string "-wekbit-transform .2s, transform .2s";
        let width = dr##offsetWidth in
        let delta = (Js.Unsafe.coerce ev)##deltaX in
        if delta < -0.5 *. float width
        then perform_animation `Close
        else perform_animation `Open));
    Lwt.async (fun () ->
      Hammer.swipelefts dr (fun ev _ -> perform_animation `Close))
  }}
  in

  d, open_, close

 }}
