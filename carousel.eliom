(* Copyright Vincent Balat *)


{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F
}}

{shared{
let make
    ?(a = [])
    ?(position = 0)
    ?update
    ?(disabled = Eliom_shared.React.S.const false)
    l =
  let pos_signal, pos_set = Eliom_shared.React.S.create position in
  let d2 = D.div ~a:[a_class ["car2"]] l in
  let d = D.div ~a:(a_class ["carousel"]::a) [d2] in
  let max = List.length l - 1 in
  let nb_visible_elements, set_nb_visible_elements =
    Eliom_shared.React.S.create 1
  in
  let _ = {unit{
    let firstchild = (To_dom.of_element %d2)##firstChild in
    let comp_nb_visible_elements () =
      (* We suppose that all elements have the same width *)
      Js.Opt.case firstchild
        (fun () -> 1)
        (fun e ->
           Js.Opt.case (Dom_html.CoerceTo.element e)
             (fun () -> 1)
             (fun firstchild ->
                let width_element = firstchild##offsetWidth in
                let width_carousel = (To_dom.of_element %d2)##offsetWidth in
                truncate
                  ((float width_carousel) /. (float width_element) +. 0.5)))
    in
    %set_nb_visible_elements (comp_nb_visible_elements ());
    let max () = %max - (React.S.value %nb_visible_elements) + 1 in
    let d2 = To_dom.of_element %d2 in
    let pos_signal = %pos_signal in
    let pos_set = %pos_set in
    let action = ref (`Move 0) in
    let animation_frame_requested = ref false in
    let set_position pos =
      let s = Js.string
          ("translate3d("^
           string_of_int (pos * (-100 / (React.S.value %nb_visible_elements)))^
           "%, 0, 0)")
      in
      (Js.Unsafe.coerce (d2##style))##transform <- s;
      (Js.Unsafe.coerce (d2##style))##webkitTransform <- s;
      pos_set pos
    in
    set_position %position;
    (*VVV I recompute the size everytime we touch the carousel
        and when the window is resized (?).
        Should be: every time the carousel size or content size changes
        and/or: provide a function to recompute size *)
    ignore (React.S.map
              (fun _ -> %set_nb_visible_elements (comp_nb_visible_elements ()))
              Ow_size.width);
    let perform_animation a =
      %set_nb_visible_elements (comp_nb_visible_elements ());
      if not (React.S.value %disabled)
      then
        match !action, a with
        | `Change _, _ ->
          (* We received both a panend and a swipe.
             The panend can be a `Goback and the swipe a `Change.
             We ignore the `Goback. *)
          Lwt.return ()
        | _ ->
          action := a;
          if not !animation_frame_requested
          then begin
            animation_frame_requested := true;
            lwt () = Lwt_js_events.request_animation_frame () in
            animation_frame_requested := false;
            (match !action with
             | `Move delta ->
               let sign = if delta < 0 then " - " else " + " in
               let pos = Eliom_shared.React.S.value pos_signal in
               let s = Js.string ("translate3d(calc("^
                                  string_of_int
                                    (pos *
                                     (-100 /
                                       (React.S.value %nb_visible_elements)))
                                  ^"%"^
                                  sign^string_of_int (abs delta)^"px), 0, 0)")
               in
               (Js.Unsafe.coerce (d2##style))##transform <- s;
               (Js.Unsafe.coerce (d2##style))##webkitTransform <- s;
             | `Goback position
             | `Change position -> action := `Move 0; set_position position);
            Lwt.return ()
          end
          else Lwt.return ()
      else Lwt.return ()
    in
    let onpan ev _ =
      let delta = (Js.Unsafe.coerce ev)##deltaX in
      perform_animation (`Move delta)
    in
    Hammer.panstarts d2 (fun ev a ->
      (Js.Unsafe.coerce (d2##style))##transition <-
        Js.string "-webkit-transform 0s, transform 0s";
      onpan ev a);
    Hammer.panmoves d2 onpan;
    Lwt.async (fun () ->
      Hammer.panends d2 (fun ev _ ->
        (Js.Unsafe.coerce (d2##style))##transition <-
          Js.string "-wekbit-transform .2s, transform .2s";
        let width = (To_dom.of_element %d)##offsetWidth in
        let delta = (Js.Unsafe.coerce ev)##deltaX in
        let pos = Eliom_shared.React.S.value pos_signal in
        let newpos =
          if float (abs delta) > 0.3 *. float width
          then
            if delta > 0
            then (if pos > 0 then pos - 1 else pos)
            else (if pos < max () then pos + 1 else pos)
          else pos
        in
        if newpos <> pos
        then perform_animation (`Change newpos)
        else perform_animation (`Goback newpos)));
    Lwt.async (fun () ->
      Hammer.swiperights d2 (fun ev _ ->
        let pos = Eliom_shared.React.S.value pos_signal in
        if pos > 0
        then perform_animation (`Change (pos - 1))
        else perform_animation (`Goback pos)
      ));
    Lwt.async (fun () ->
      Hammer.swipelefts d2 (fun ev _ ->
        let pos = Eliom_shared.React.S.value pos_signal in
        if pos < max ()
        then perform_animation (`Change (pos + 1))
        else perform_animation (`Goback pos)
      ));
    ignore
      (Eliom_lib.Option.map
         (fun update ->
            let max = max () in
            React.E.map (function
              | `Goto pos when pos >= 0 && pos <= max ->
                perform_animation (`Change pos)
              | `Next ->
                let curpos = Eliom_shared.React.S.value pos_signal in
                if curpos < max then perform_animation (`Change (curpos + 1))
                else Lwt.return ()
              | `Prev ->
                let curpos = Eliom_shared.React.S.value pos_signal in
                if curpos > 0 then perform_animation (`Change (curpos - 1))
                else Lwt.return ()
              | _ -> Lwt.return ()
            ) update)
       %update);
  }}
  in
  d, pos_signal, nb_visible_elements
 }}

{shared{
let bullets
    ?(a = []) ?attributes ~change ~pos ~length
    ?(size = Eliom_shared.React.S.const 1) () =
  let bullet i =
    let class_ =
      Eliom_shared.React.S.l2
        {shared#{ fun p size ->
           if %i >= p && %i < p + size
           then ["active"] else [] }} pos size
    in
    let a = match attributes with
      | None -> []
      | Some l -> List.nth l i
    in
    li ~a:(R.a_class class_:: a_onclick {{fun _ -> %change (`Goto %i)}}::a) []
  in
  let rec aux acc i = if i = 0 then acc else aux (bullet (i-1)::acc) (i-1) in
  ul ~a:(a_class ["bullet-nav"]::a) (aux [] length)

 }}


{client{

   let bind_arrow_keys ~change =
     Lwt_js_events.keydowns Dom_html.document (fun ev _ ->
       let key = ev##keyCode in
       if key = 39 (* right *)
       then change `Next
       else if key = 37 (* left *)
       then change `Prev;
       Lwt.return ()
     )

 }}

(* To test, uncomment the following lines: *)

(* {client{ *)

(*    let _ = *)
(*      Lwt.async (fun () -> *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        let ev, send_ev = React.E.create () in *)
(*        let d, _ = make *)
(*            ~position:2 *)
(*            ~update:ev *)
(*            [div [h1 [pcdata "Coucou 1"]]; *)
(*             div [h1 [pcdata "Bonjour 2"]]; *)
(*             div [h1 [pcdata "Salut 3"]]; *)
(*             div [h1 [pcdata "Hello 4"]]; *)
(*            ] *)
(*        in *)
(*        Manip.appendToBody d; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Next; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Next; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Prev; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Next; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev `Prev; *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 0); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 3); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 1); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 0); *)
(*        lwt () = Lwt_js.sleep 1. in *)
(*        send_ev (`Goto 3); *)
(*        Lwt.return () *)
(*      ) *)

(*  }} *)
