{client{
module Html5 = Eliom_content.Html5

let get_files inp =
  Js.Optdef.case ((Html5.To_dom.of_input inp)##files)
    (fun _ -> [||])
    (function files ->
        Array.init (files##length)
          (fun i -> Js.Opt.get (files##item (i)) (fun _ -> assert false)))

let get_file inp =
  let a = get_files inp in
  if a = [||] then None else Some a.(0)
}}
