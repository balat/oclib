
val batch : ('a list -> 'b list Lwt.t) -> 'a list -> 'b list Lwt.t
(* [batch f l] applies [f] to chunks of length 20 at most of [l],
   and returns the concatenation of the results. *)

val complete :
  'a list -> ('a -> 'key) ->
  ('a -> int) -> ('a list -> 'b list Lwt.t) array ->
  ('b -> 'key) ->
  ('a -> 'b -> 'c) ->
  'c list Lwt.t
(* Batch apply subqueries to complete the contents of a list. *)

val group :
  'a list -> ('a -> 'key) ->
  'b list -> ('b -> 'key) -> ('b -> 'c) ->
  ('a -> 'c list -> 'd) ->
  'd list

val normalize_array : 'a option list option -> 'a list
