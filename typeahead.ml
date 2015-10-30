(* Copyright Jérôme Vouillon *)

class type js_query = object
  method query : Js.js_string Js.t Js.readonly_prop
  method dataset : Js.js_string Js.t Js.readonly_prop
end

class type ['a] js_query_with_suggestions = object
  inherit js_query
  method suggestions : 'a Js.js_array Js.t
end

type element_array = Dom_html.element Js.t Js.js_array Js.t

class type ['a] js_templates = object
  method notFound :
    (js_query Js.t -> element_array) Js.callback Js.writeonly_prop
  method footer :
    ('a js_query_with_suggestions Js.t -> element_array)
      Js.callback Js.writeonly_prop
  method header :
    ('a js_query_with_suggestions Js.t -> element_array)
      Js.callback Js.writeonly_prop
  method suggestion :
    ('a -> Dom_html.element Js.t) Js.callback Js.writeonly_prop
end

class type ['a] js_dataset = object
  method source :
    (Js.js_string Js.t ->
     ('a Js.js_array Js.t -> unit) Js.callback ->
     ('a Js.js_array Js.t -> unit) Js.callback -> unit)
     Js.callback Js.writeonly_prop
  method async : bool Js.t Js.writeonly_prop
  method name : Js.js_string Js.t Js.writeonly_prop
  method limit : int Js.writeonly_prop
  method display : ('a -> Js.js_string Js.t) Js.callback Js.writeonly_prop
  method templates : 'a js_templates Js.t Js.writeonly_prop
end

class type options = object
  method highlight : bool Js.t Js.writeonly_prop
  method hint : bool Js.t Js.writeonly_prop
  method minLength : int Js.writeonly_prop
end

class type ['a] js_collection = object ('self)
  method typeahead :
    options Js.t Js.opt -> 'a js_dataset Js.t Js.js_array Js.t -> unit Js.meth
  method typeahead_str : Js.js_string Js.t -> _ Js.meth
  method typeahead_str2 : Js.js_string Js.t -> _ -> unit Js.meth
  method on : Js.js_string Js.t -> _ Js.callback -> 'self Js.t Js.meth
end

let select selector : _ js_collection Js.t =
  Js.Unsafe.global ## jQuery (Js.string selector)

let element (elt : #Dom_html.element Js.t) : _ js_collection Js.t =
  Js.Unsafe.global ## jQuery (elt)

(****)

type query = { query : string; dataset : string }

let query (q : #js_query Js.t) =
  { query = Js.to_string q##query; dataset = Js.to_string q##dataset }

let opt_map x f = match x with None -> () | Some v -> f v

(*XXX We wrap data. Otherwise, [on_autocomplete] does not work properly:
  jquery turns an array argument into a many arguments when calling the
  callback... *)
type 'a wrapped = <v : 'a Js.readonly_prop> Js.t

let wrap v = jsobject val v = v end
let wrap_array a = a |> Array.map wrap |> Js.array

type 'a templates = 'a wrapped js_templates Js.t

let templates ?not_found ?footer ?header ?suggestion () =
  let obj : _ wrapped js_templates Js.t = Js.Unsafe.obj [||] in
  opt_map not_found
    (fun f ->
       obj##notFound <- Js.wrap_callback (fun q -> Js.array (f (query q))));
  opt_map footer
    (fun f ->
       obj##footer <- Js.wrap_callback (fun q -> Js.array (f (query q))));
  opt_map header
    (fun f ->
       obj##header <- Js.wrap_callback (fun q -> Js.array (f (query q))));
  opt_map suggestion
    (fun f -> obj##suggestion <- Js.wrap_callback (fun s -> f (s##v)));
  obj

type 'a dataset = 'a wrapped js_dataset Js.t

let dataset ~source ~async ?name ?limit ?display ?templates () =
  let obj : _ js_dataset Js.t = Js.Unsafe.obj [||] in
  obj##source <-
    Js.wrap_callback
      (fun q c1 c2 ->
         source (Js.to_string q)
           (fun a ->
              Js.Unsafe.fun_call c1 [|Js.Unsafe.inject (wrap_array a)|])
           (fun a ->
              Js.Unsafe.fun_call c2 [|Js.Unsafe.inject (wrap_array a)|]));
  obj##async <- Js.bool async;
  opt_map name (fun nm -> obj##name <- Js.string nm);
  opt_map limit (fun n -> obj##limit <- n);
  opt_map display
    (fun f -> obj##display <- Js.wrap_callback (fun v -> Js.string (f v##v)));
  opt_map templates (fun t -> obj##templates <- t);
  obj

let options ?(highlight=false) ?(hint=true) ?(min_length=1) () : options Js.t =
  Js.Unsafe.obj
    [|"highlight", Js.Unsafe.inject (Js.bool highlight);
      "hint", Js.Unsafe.inject (Js.bool hint);
      "minLength", Js.Unsafe.inject min_length |]

class type value = object
  method value : Js.js_string Js.t Js.readonly_prop
end

let value s : value Js.t =
  Js.Unsafe.obj [|"value", Js.Unsafe.inject (Js.string s)|]

type 'a collection = 'a wrapped js_collection Js.t

let create (col : _ collection) ?highlight ?hint ?min_length datasets =
  col##typeahead(Js.some (options ?highlight ?hint ?min_length ()),
                 Js.array datasets)

let destroy (col : _ collection) : unit =
  col##typeahead_str (Js.string "destroy")

let open_menu (col : _ collection) : unit =
  col##typeahead_str (Js.string "open")

let close_menu (col : _ collection) : unit =
  col##typeahead_str (Js.string "close")

let current_value (col : _ collection) : string =
  Js.to_string (col##typeahead_str (Js.string "val"))

let set_value (col : _ collection) (v : string) : unit =
  col##typeahead_str2 (Js.string "val", Js.string v)
  |> ignore

let on_open (col : _ collection) (f : unit -> unit) =
  col##on(Js.string "typeahead:open", Js.wrap_callback (fun _ -> f ()))
  |> ignore

let on_close (col : _ collection) (f : unit -> unit) =
  col##on(Js.string "typeahead:close", Js.wrap_callback (fun _ -> f ()))
  |> ignore

let on_cursor_change (col : 'a collection) (f : 'a -> unit) =
  col##on(Js.string "typeahead:cursorchange",
          Js.wrap_callback (fun _ sug -> f sug##v))
  |> ignore

let on_select (col : 'a collection) (f : 'a -> unit) =
  col##on(Js.string "typeahead:select",
          Js.wrap_callback (fun ev sug ->
            Firebug.console##log_2(ev, sug); f sug##v))
  |> ignore

let on_autocomplete (col : 'a collection) (f : 'a -> unit) =
  col##on(Js.string "typeahead:autocomplete",
          Js.wrap_callback (fun _ sug -> f sug##v))
  |> ignore
