(* Copyright Vincent Balat *)

let (>>=) = Lwt.bind

class type hammer = object
  method on : Js.js_string Js.t -> (unit -> unit) Js.callback -> unit Js.meth
end

let _Hammer () = Js.Unsafe.global##_Hammer

(* I want to take only touch actions,
   because otherwise panend also raises a click.
   And I can't prevent it using preventDefault :(
   Make this customizable!  *)
let opts () = Js.Unsafe.obj
    [|("inputClass",
       (Js.Unsafe.coerce (_Hammer ()))##_TouchInput)|]

let catch_cancel f x =
  Lwt.catch
    (fun () -> f x)
    (function
    | Lwt.Canceled -> Lwt.return ()
    | e -> Lwt.fail e)

let with_error_log f x =
  Lwt.catch
    (fun () -> f x)
    (fun e -> (Firebug.console##log(Js.string (Printexc.to_string e));
               Lwt.return ()))

let make_event name target =
  let name = Js.string name in
  let hammertime =
    jsnew (_Hammer ())(target, opts ())
  in
  let el = ref Js.null in
  let t, w = Lwt.task () in
  let cancel () =
    Js.Opt.iter !el (fun handler -> hammertime##off(name, handler))
  in
  Lwt.on_cancel t cancel;
  el := Js.some (Js.wrap_callback (fun ev ->
    cancel ();
    Lwt.wakeup w ev));
  Js.Opt.iter !el (fun handler -> hammertime##on(name, handler));
  t

let seq_loop evh ?(cancel_handler = false) target handler =
  let cancelled = ref false in
  let cur = ref (Lwt.fail (Failure "Hammer")) in
  (* Using Lwt.fail as default, to be polymorphic *)
  let cur_handler = ref (Lwt.return ()) in
  let lt, _lw = Lwt.task () in
  Lwt.on_cancel lt
    (fun () ->
      Lwt.cancel !cur;
      if cancel_handler then Lwt.cancel !cur_handler;
      cancelled := true);
  let rec aux () =
    if not !cancelled (* In the case it has been cancelled
                         during the previous handler,
                         we do not reinstall the event handler *)
    then begin
      let t = evh target in
      cur := t;
      t >>= fun e ->
      cur_handler := with_error_log (handler e) lt;
      !cur_handler >>= aux
    end
    else Lwt.return ()
  in
  Lwt.async (catch_cancel aux);
  lt


let panmove = make_event "panmove"
let panstart = make_event "panstart"

let swipe = make_event "swipe"
let swipeleft = make_event "swipeleft"
let swiperight = make_event "swiperight"
let panend = make_event "panend"


let swipes ?cancel_handler t = seq_loop swipe ?cancel_handler t
let swipelefts ?cancel_handler t = seq_loop swipeleft ?cancel_handler t
let swiperights ?cancel_handler t = seq_loop swiperight ?cancel_handler t

let panends ?cancel_handler t = seq_loop panend ?cancel_handler t

(* I don't use make_event for pans,
   because it does not work if we remove the handler between each event: *)
(*VVV To be improved *)
let make_evs name elt f =
  let name = Js.string name in
  let hammertime = jsnew (_Hammer ())(elt, opts ()) in
  hammertime##on(name,
                 Js.wrap_callback (fun ev -> f ev elt));
  ()

let panmoves = make_evs "panmove"
let panstarts = make_evs "panstart"
