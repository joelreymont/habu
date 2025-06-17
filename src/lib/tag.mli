open Sexplib

type ('a, 'b) tagged = private
   { value : 'a
   ; tag : 'b
   }
 [@@deriving sexp]
 
val make : 'a -> 'b -> ('a, 'b) tagged

val sexp_of_tagged : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) tagged -> Sexp.t
val tagged_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) tagged

type 'a located = ('a, Position.t) tagged

val with_loc : 'a Position.located -> 'b -> ('b, Position.t) tagged
val with_pos : Position.t -> 'a -> ('a, Position.t) tagged

val set_tag : 'c -> ('a, 'b) tagged -> ('a, 'c) tagged
val set_value : 'c -> ('a, 'b) tagged -> ('c, 'b) tagged

val clear_tag : ('a, 'b) tagged -> ('a, unit) tagged

val tag : (_, 'a) tagged -> 'a

val value: ('a, _) tagged -> 'a
