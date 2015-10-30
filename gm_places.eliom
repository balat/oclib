(* Copyright Vincent Balat *)


(* FIX: we should install a function to be called as a callback
       for when the library is ready *)

(* Minimal binding of Google map places *)

{shared{
  open Eliom_content.Html5
  open Eliom_content.Html5.F
}}

(* Example:
var defaultBounds = new google.maps.LatLngBounds(
	new google.maps.LatLng(40.802089, -124.163751)
	);

let options = {
	bounds: defaultBounds,
	componentRestrictions: {country: 'us'}
};
*)

{client{

let init =
  lwt _ = Lwt_js_events.onload () in
  let script = Dom_html.createScript Dom_html.document in
  script##_type <- Js.string "text/javascript";
  script##src <-
    Js.string "https://maps.googleapis.com/maps/api/js?libraries=places\
               &callback=escape";
  Dom.appendChild Dom_html.document##body script;
  Lwt.return ()

let bind_autocomplete input =
  try
    let google = Js.Unsafe.variable "google" in
    let autocomplete = google##maps##places##_Autocomplete in
    let options = [||] in
    let o = jsnew autocomplete(input, options) in
    let get_place () =
      let pl = o##getPlace() in
      Js.Optdef.case
        pl
        (fun () -> None)
        (fun pl ->
          let loc = pl##geometry##location in
          Some (Js.to_string pl##formatted_address_,
                Js.to_string pl##name, loc##lat(), loc##lng()))
    in
    (* I create a react signal for the place *)
    let s, set = Eliom_shared.React.S.create None in
    (* When we type something, we remove the selected location
       until it corresponds to a valid location according to google: *)
    Lwt.async (fun () ->
      Lwt_js_events.inputs input (fun _ _ ->
        set None;
        Lwt.return ()));
    (* When we loose focus, we remove contents if no place is selected: *)
    Lwt.async (fun () ->
      Lwt_js_events.blurs input (fun _ _ ->
        if React.S.value s = None then input##value <- Js.string "";
        Lwt.return ()));
    let _ = google##maps##event##addListener
      (o, (Dom.Event.make "place_changed"),
       (Dom_html.handler
          (fun (ev : #Dom_html.event Js.t) ->
            set (get_place ());
            Js.bool true)),
       (Js.bool false))
    in
    s, get_place
  with _ -> (Eliom_shared.React.S.const None, fun () -> None)

let location_picker ?default () =
  let a = match default with
    | None -> []
    | Some d -> [a_value d]
  in
  let inp = D.Raw.input ~a:(a_input_type `Text :: a) () in
  let inp' = To_dom.of_input inp in
  let s, get_loc = bind_autocomplete inp' in
  inp, s, get_loc

}}
