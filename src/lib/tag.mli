open Sexplib

type ('a, 'b) tagged 

val sexp_of_tagged : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) tagged -> Sexp.t
val tagged_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) tagged

type 'a located

val make : 'a -> 'b -> ('a, 'b) tagged

val with_pos : Position.t -> 'a -> ('a, Position.t) tagged

val set_tag : ('a, 'b) tagged -> 'c -> ('a, 'c) tagged
val set_value : ('a, 'b) tagged -> 'c -> ('c, 'b) tagged

val clear_tag : ('a, 'b) tagged -> ('a, unit) tagged

val tag : (_, 'a) tagged -> 'a
val value: ('a, _) tagged -> 'a
