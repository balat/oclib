(* Copyright Vincent Balat - not part of BeSport *)



{shared{
  open Eliom_content.Html5
  open Eliom_content.Html5.F


  type slot = float * float * bool (* all day *) * float option deriving (Json)

}}

{shared{
   let print_time (h, m) =
     Bs_date.smart_hours_minutes (CalendarLib.Time.make h m 0)
   let print_date (y, m, d) =
     let timestamp = CalendarLib.Calendar.make y m d 0 0 0 in
     let timestamp = Eba_date.local_from_calendar timestamp in
     Bs_date.smart_date timestamp
 }}

{shared{
   let print_time = Eliom_lib.create_shared_value print_time {{ print_time }}
   let print_date = Eliom_lib.create_shared_value print_date {{ print_date }}
 }}

{shared{
   let make_int_options
       ?default ?(print = string_of_int) start endd step =
     let rec aux start l =
       if (start - endd) * step > 0
       then l
       else
         let v = string_of_int start in
         let c = print start in
         let a = if default = Some start then [a_selected `Selected] else [] in
         aux (start + step) (option ~a:(a_value v::a) (pcdata c)::l)
     in
     aux start []

   let ymd_of_date date =
     let open CalendarLib.Date in
     let date = Eba_date.to_local_date date in
     let date_d = day_of_month date in
     let date_m = CalendarLib.Date.int_of_month (month date) in
     let date_y = year date in
     (date_y, date_m, date_d)

   let basic_date_picker ?a ?(default = Eba_date.now ()) () =
     let (default_y, default_m, default_d) = ymd_of_date default in
     let d = D.Raw.select (make_int_options ~default:default_d 31 1 (-1)) in
     let m = D.Raw.select
         (make_int_options ~default:default_m
            ~print:(fun x -> CalendarLib.Printer.name_of_month
                     @@ CalendarLib.Date.month_of_int @@ x)
            12 1 (-1))
     in
     let lasty = default_y + 12 in
     let y = D.Raw.select (make_int_options ~default:default_y 1905 lasty 1) in
     let signal, set = Eliom_shared.React.S.create (default_y, default_m, default_d) in
     let _ = {unit{
       let m' = To_dom.of_select %m in
       let d' = To_dom.of_select %d in
       let y' = To_dom.of_select %y in
       let get_date () = int_of_string (Js.to_string y'##value),
                         int_of_string (Js.to_string m'##value),
                         int_of_string (Js.to_string d'##value)
       in
       Lwt.async (fun () ->
         Lwt_js_events.changes d'
           (fun _ _ -> %set (get_date ()); Lwt.return ()));
       Lwt.async (fun () ->
         Lwt_js_events.changes m'
           (fun _ _ -> %set (get_date ()); Lwt.return ()));
       Lwt.async (fun () ->
         Lwt_js_events.changes y'
           (fun _ _ -> %set (get_date ()); Lwt.return ()));
     }} in
     (span ?a [d; m; y],
      signal)

let advanced_date_picker ?(a = []) ?title ?(default = Eba_date.now ())
    ?update () =
     let init = ymd_of_date default in
     let w, s = Ot_calendar.make_date_picker ~init ?update () in
     let date = Eliom_shared.React.S.map print_date s in
     let d = D.span ~a:(a_class ["w_dt"; "d"]::a_tabindex 0::a) [R.pcdata date]
     in
     let _ = {unit{
       let close_r = ref (fun () -> ()) in
       ignore (Lwt_react.S.map_s (fun _ ->
         lwt () = Lwt_js.sleep 0.5 in
         !close_r (); Lwt.return ())
               %s);
       Lwt_js_events.(async (fun () ->
         clicks (To_dom.of_element %d)
           (fun _ _ -> W_popup.popup ~a:[a_class ["w_d"]]
             (fun close ->
               close_r := close;
               let footer = [button ~a:[a_onclick (fun _ -> close ())]
                                ~button_type:`Button [Bs_i18n.F.ok ()]]
               in
               let header = match %title with
                 | None -> []
                 | Some t ->
                   [h2 ~a:[a_onclick (fun _ -> close ())] [pcdata t]]
               in
               Lwt.return (W_popup.hcf
                             ~header ~footer
                             [%w])
             ))))
     }} in
     d, s

   let date_picker ?a ?(basic = false) ?title ?default ?update () =
     if basic
     then basic_date_picker ?a ?default ()
     else advanced_date_picker ?a ?title ?default ?update ()

   let basic_time_picker ?a ?(default = Eba_date.now ()) () =
     let default = Eba_date.to_local_time default in
     let defaulth = CalendarLib.Time.hour default in
     let defaultm = CalendarLib.Time.minute default in
     let h = D.Raw.select (make_int_options ~default:defaulth 23 0 (-1)) in
     let m = D.Raw.select (make_int_options
                             ~print:(Printf.sprintf "%02d")
                             ~default:defaultm 45 0 (-15))
     in
     let signal, set = Eliom_shared.React.S.create (defaulth, defaultm) in
     let _ = {unit{
       let h' = To_dom.of_select %h in
       let m' = To_dom.of_select %m in
       let get_time () = int_of_string (Js.to_string h'##value),
                         int_of_string (Js.to_string m'##value)
       in
       Lwt.async (fun () ->
         Lwt_js_events.changes h'
           (fun _ _ -> %set (get_time ()); Lwt.return ()));
       Lwt.async (fun () ->
         Lwt_js_events.changes m'
           (fun _ _ -> %set (get_time ()); Lwt.return ()));
     }} in
     (span ?a [h; pcdata ":"; m],
      signal)

let advanced_time_picker ?title ?(a = []) ?(default = Eba_date.now ())
    ?update () =
     let default = Eba_date.to_local_time default in
     let defaulth = CalendarLib.Time.hour default in
     let defaultm = CalendarLib.Time.minute default in
     let w, s, reset =
       Ot_time_picker.make_hours_minutes_seq
         ~h24:(Bs_i18n.get_24 ()) ~round_5:true ~init:(defaulth, defaultm)
         ?update ()
     in
     let date = Eliom_shared.React.S.map print_time s in
     let d = D.span ~a:(a_class ["w_dt"; "t"]::a_tabindex 0::a) [R.pcdata date]
     in
     let _ = {unit{
       Lwt_js_events.(async (fun () ->
         clicks (To_dom.of_element %d)
           (fun _ _ ->
              %reset ();
              W_popup.popup ~a:[a_class ["w_t"]] (fun close ->
                let footer = [button ~a:[a_onclick (fun _ -> close ())]
                                 ~button_type:`Button [Bs_i18n.F.ok ()]]
                in
                let header = match %title with
                  | None -> []
                  | Some t ->
                    [h2 ~a:[a_onclick (fun _ -> close ())] [pcdata t]]
                in
                Lwt.return (W_popup.hcf
                              ~header ~footer
                              [%w])
              ))))
     }} in
     d, s

   let time_picker ?a ?(basic = false) ?title ?default ?update () =
     if basic
     then basic_time_picker ?a ?default ()
     else advanced_time_picker ?a ?title ?default ?update ()

}}
{client{
   let reactive_checkbox c =
     let s, set = Eliom_shared.React.S.create (Js.to_bool c##checked) in
     Lwt.async (fun () ->
       Lwt_js_events.changes c (fun _ _ ->
         set (Js.to_bool c##checked);
         Lwt.return ()));
     s

   let make_id =
     let r = ref 0 in
     fun s -> incr r; s^string_of_int !r

   (*TODO: turn this into a shared section *)
   let slot_picker ?(a = []) ?basic ?default () =
     let open CalendarLib in
     let startdate, enddate, allday, recurring, recurringdate =
       match default with
       | None ->
           let now = Eba_date.now () in
           let enddate =
             Eba_date.local_from_calendar
               (Calendar.add (Eba_date.local_to_calendar now)
                  (CalendarLib.Calendar.Period.hour 2))
           in
           (now, enddate, [], [], enddate)
       | Some (startdate, enddate, allday, recurringdate) ->
           let startdate = Eba_date.to_local startdate in
           let enddate = Eba_date.to_local enddate in
           let (recurring, recurringdate) =
             match recurringdate with
               None ->
                 ([], enddate)
             | Some date ->
                 ([a_checked `Checked], Eba_date.to_local date)
           in
           (startdate, enddate, (if allday then [a_checked `Checked] else []),
            recurring, recurringdate)
     in
     let visible = function true -> ["visible"]
                          | false -> ["hidden"]
     in
     let displayed = function true -> ["inline"]
                            | false -> ["none"]
     in
     let blur = function true -> ["blurred"]
                       | false -> []
     in
     let alldayid = make_id "ad" in
     let recid = make_id "rec" in
     let all_day_inp = D.Raw.input ~a:(a_input_type `Checkbox::
                                       a_id alldayid::allday) () in
     let all_day_inp' = To_dom.of_input all_day_inp in
     let all_day_signal = reactive_checkbox all_day_inp' in
     let recurring_inp =
       D.Raw.input ~a:(a_input_type `Checkbox::a_id recid::recurring) () in
     let recurring_inp' = To_dom.of_input recurring_inp in
     let recurring_signal = reactive_checkbox recurring_inp' in
     let time2_event, send_time2_event = React.E.create () in
     let date2_event, send_date2_event = React.E.create () in
     let date_picker1, date1_signal =
       date_picker ~title:(Bs_i18n.S.start ()) ?basic ~default:startdate () in
     let date_picker2, date2_signal =
       date_picker ~update:date2_event
         ~title:(Bs_i18n.S.end_ ()) ?basic ~default:enddate ()
     in
     let date_picker3, date3_signal =
       date_picker ~a:[R.a_class (Eliom_shared.React.S.map visible recurring_signal)]
         ~title:(Bs_i18n.S.until ()) ?basic ~default:recurringdate ()
     in
     let time_picker1, time1_signal =
       time_picker ~a:[R.a_class (Eliom_shared.React.S.map blur all_day_signal)]
         ?basic ~title:(Bs_i18n.S.start ()) ~default:startdate ()
     in
     let time_picker2, time2_signal =
       time_picker ~a:[R.a_class (Eliom_shared.React.S.map blur all_day_signal)]
         ~update:time2_event
         ?basic ~title:(Bs_i18n.S.end_ ()) ~default:enddate ()
     in
     let start1 = label
         [pcdata (Bs_i18n.S.start ());
          pcdata (Bs_i18n.S.colon ())]
     in
     let end1 = label [pcdata (Bs_i18n.S.end_ ()); pcdata (Bs_i18n.S.colon ())]
     in
     let end3 = label ~a:[R.a_class (Eliom_shared.React.S.map displayed recurring_signal)]
         [pcdata (Bs_i18n.S.until ()); pcdata (Bs_i18n.S.colon ())]
     in
     let timestamp1_signal =
       Eliom_shared.React.S.l2
         (fun (date1, time1) all_day ->
            let (y1, m1, d1) = date1 in
            let (h1, min1) = if all_day
              then (0, 0)
              else time1
            in
            CalendarLib.Calendar.make y1 m1 d1 h1 min1 0)
         (Eliom_shared.React.S.l2 (fun d t -> (d, t)) date1_signal time1_signal)
         all_day_signal
     in
     let timestamp2_signal =
       Eliom_shared.React.S.l2
         (fun (date2, time2) all_day ->
            let (y2, m2, d2) = date2 in
            let (h2, min2) = if all_day
              then (24, 0) (* That is, 00:00 the day after *)
              else time2
            in
            (CalendarLib.Calendar.make y2 m2 d2 h2 min2 0, h2, min2))
         (Eliom_shared.React.S.l2 (fun d t -> (d, t)) date2_signal time2_signal)
         all_day_signal
     in
     (* The following lines make the end time change automatically
        when start time is set after end.
        COULD BE IMPROVED A LOT. Take example on Google+ app for example. *)
     ignore (Eliom_shared.React.S.map
               (fun ts1 ->
                  let ts2 = Eliom_shared.React.S.value timestamp2_signal in
                  let (ts2, _, _) = ts2 in
                  if CalendarLib.Calendar.compare ts1 ts2 > 0
                  then begin
                    let ts2 = CalendarLib.Calendar.add ts1
                        (CalendarLib.Calendar.Period.hour 2)
                    in
                    let new_date2 =
                      (CalendarLib.Calendar.year ts2,
                       CalendarLib.Date.int_of_month
                         (CalendarLib.Calendar.month ts2),
                       CalendarLib.Calendar.day_of_month ts2)
                    in
                    let new_time2 =
                      (CalendarLib.Calendar.hour ts2,
                       CalendarLib.Calendar.minute ts2)
                    in
                    send_date2_event new_date2;
                    send_time2_event new_time2
                  end
               )
               timestamp1_signal);
     (* The final signal: *)
     let slot_signal =
       Eliom_shared.React.S.l5
         (fun timestamp1 (timestamp2, h2, min2) all_day date3 recurring ->
            let (y3, m3, d3) = date3 in
            let timestamp3 = CalendarLib.Calendar.make y3 m3 d3 h2 min2 0 in
            (* Avoid zero-length intervals. *)
            (* XXX Is this the right place? *)
            let timestamp2 =
              if CalendarLib.Calendar.compare timestamp1 timestamp2 = 0 then
                CalendarLib.Calendar.next timestamp2 `Second
              else
                timestamp2
            in
            let timestamp3 =
              if CalendarLib.Calendar.compare timestamp1 timestamp3 = 0 then
                CalendarLib.Calendar.next timestamp3 `Second
              else
                timestamp3
            in
            let (timestamp1, timestamp2, timestamp3) =
              if all_day then
                (* For all day events, we use 00:00 GMT for start/end time *)
                (timestamp1, timestamp2, timestamp3)
              else
                (Eba_date.to_utc (Eba_date.local_from_calendar timestamp1),
                 Eba_date.to_utc (Eba_date.local_from_calendar timestamp2),
                 Eba_date.to_utc (Eba_date.local_from_calendar timestamp3))
            in
            let status =
              if CalendarLib.Calendar.compare timestamp1 timestamp2 >= 0 then
                `Issue (Bs_i18n.S.wrong_end_date ())
              else if
                recurring &&
                CalendarLib.Calendar.compare timestamp2 timestamp3 > 0 then
                `Issue (Bs_i18n.S.wrong_recurring_end_date ())
              else
                `Ok
            in
            (status,
             (timestamp1, timestamp2, all_day,
              if recurring then Some timestamp3 else None)))
         timestamp1_signal
         timestamp2_signal
         all_day_signal
         date3_signal
         recurring_signal
     in
     (D.div ~a:(a_class ["slotpicker"]::a)
        [h2 [start1];
         date_picker1;
         time_picker1;
         h2 [end1];
         date_picker2;
         time_picker2;
         div [all_day_inp;
              label ~a:[Raw.a_for alldayid] [pcdata (Bs_i18n.S.allday ())]];
         div [recurring_inp;
              label ~a:[Raw.a_for recid]
                [pcdata (Bs_i18n.S.recurring_weekly ())];
              end3;
              date_picker3]
        ],
     slot_signal)

 }}
