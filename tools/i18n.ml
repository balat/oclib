(*
==> Scanf.unescaped

- Warn if we do not have all strings
- Warn when the English text has changed
- Warn when some text is not used


*)

let write f l =
  Format.fprintf f "{shared{@.";
  Format.fprintf f "let h = Hashtbl.create %d@." (2 * List.length l);
  Format.fprintf f
    "let _ = List.iter (fun (a, b, c) -> Hashtbl.add h a (b, c))@.";
  Format.fprintf f "@[<1>[";
  List.iter
    (fun (a, b, c) ->
       Format.fprintf f "\"%s\",\"%s\",\"%s\";@,"
         (String.escaped a) (String.escaped b) (String.escaped c))
    l;
  Format.fprintf f "]@]@.";
  Format.fprintf f
    "let find s = try Hashtbl.find h s with Not_found -> (s, s)@.";
  Format.fprintf f "}}@."

let check_changes l =
  List.iter
    (fun (a, b, _) ->
       if a <> b then
         Format.eprintf "Warning: English text changed for '%s'@."
           (String.escaped a))
    l

let trans_re = Str.regexp ".*tr \"\\(.*\\)\".*$"
let skip_re = Str.regexp "^\\(let .* () =\\|([*].*[*]*)\\)?$"

module StringSet = Set.Make (String)

let check_uses lst =
  if Array.length Sys.argv > 1 then begin
    let ch = open_in Sys.argv.(1) in
    let skip = ref true in
    let keys = ref StringSet.empty in
    begin try
      while true do
        let l = String.trim (input_line ch) in
        if l = "(*START*)" then
          skip := false
        else if l = "(*STOP*)" then
          skip := true
        else if !skip || Str.string_match skip_re l 0 then
          ()
        else if Str.string_match trans_re l 0 then begin
          keys := StringSet.add (Scanf.unescaped (Str.matched_group 1 l)) !keys
        end else
          Format.eprintf "Warning: line not recognized: <%s>@." l
      done
    with End_of_file -> () end;
    let keys = !keys in
    let translations =
      List.fold_left (fun s (a, _, _) -> StringSet.add a s) StringSet.empty lst
    in
    StringSet.iter
      (fun a ->
         if not (StringSet.mem a keys) then
           Format.eprintf "Warning: unused key <%s>@." a)
      translations;
    StringSet.iter
      (fun a ->
         if not (StringSet.mem a translations) then
           Format.eprintf "Warning: no translation for <%s>@." a)
      keys
  end

let (||) f g x = f x; g x

let tab_re = Str.regexp "\t"

let load ch =
  let lst = ref [] in
  begin try
    while true do
      lst := Str.split_delim tab_re (input_line ch) :: !lst
    done
  with End_of_file -> () end;
  !lst

let _ =
  stdin
  |> (*Csv.load_in*) load
  |> List.map
    (fun r -> match r with a::b::c::_ -> (a, b, c) | _ -> assert false)
  |> List.filter (fun (a, _, _) -> a <> "")
  |> (Format.printf "%a" write
        ||
      check_changes
        ||
      check_uses)
