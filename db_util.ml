
let (>>=) = Lwt.bind

let split_20 l =
  match l with
    a::b::c::d::e::f::g::h::i::j::k::l::m::n::o::p::q::r::s::t::rem ->
      [a;b;c;d;e;f;g;h;i;j;k;l;m;n;o;p;q;r;s;t], rem
  | _ ->
      l, []

let batch f l =
  let rec loop f l acc =
    if l = [] then
      Lwt.return (List.flatten (List.rev acc))
    else
      let (beg, rem) = split_20 l in
      f beg >>= fun res -> loop f rem (res :: acc)
  in
  loop f l []

let array_iter_2_lwt f a b =
  let n = Array.length a in
  let rec loop i =
    if i = n then Lwt.return_unit else begin
      f a.(i) b.(i) >>= fun () ->
      loop (i + 1)
    end
  in
  loop 0

let complete l key1 dispatch retrieve key2 merge =
  let h = Hashtbl.create (2 * List.length l) in
  let n = Array.length retrieve in
  let a = Array.make n [] in
  List.iter
    (fun x ->
       let i = dispatch x in
       assert (i >= 0 && i < n);
       a.(i) <- x :: a.(i))
    l;
  array_iter_2_lwt
    (fun f l ->
       f l >>= fun r ->
       List.iter (fun y -> Hashtbl.add h (key2 y) y) r;
       Lwt.return_unit)
    retrieve a
      >>= fun () ->
  Lwt.return (List.map (fun x -> merge x (Hashtbl.find h (key1 x))) l)

let group l key1 r key2 extract merge =
  let h = Hashtbl.create (2 * List.length l) in
  List.iter
    (fun y ->
       let k = key2 y in
       Hashtbl.replace h k
         (extract y :: try Hashtbl.find h k with Not_found -> []))
    r;
 List.map
   (fun x ->
      merge x (try List.rev (Hashtbl.find h (key1 x)) with Not_found -> [])) l

let rec filter_out_nulls l =
  match l with
  | []          -> []
  | None :: r   -> filter_out_nulls r
  | Some v :: r -> v :: filter_out_nulls r

let normalize_array a =
  match a with
    Some l -> filter_out_nulls l
  | None   -> []
