type s =
  | Reused of string
  | Next_line

type 'a t = s -> ('a, string) result * s

val return : 'a -> 'a t
val fail_gobble : string -> 'a t
val not_interested : 'a -> string -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

val read_one : string t
val run : 'a t -> 'a
val read_nonempty : string t
val read_par : string list t
val read_join : string t

val expect : 'a -> 'a t -> ('a -> string) -> unit t
val read_if : string -> string t -> bool t
val id : 'a -> 'a
