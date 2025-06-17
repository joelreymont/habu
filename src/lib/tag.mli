type ('a, 'b) tagged [@@deriving sexp]

type 'a located

val make : 'a -> 'b -> ('a, 'b) tagged

val with_pos : Position.t -> 'a -> ('a, Position.t) tagged

val set_tag : ('a, 'b) tagged -> 'c -> ('a, 'c) tagged
val set_value : ('a, 'b) tagged -> 'c -> ('c, 'b) tagged

val clear_tag : ('a, 'b) tagged -> ('a, unit) tagged

val tag : (_, 'a) tagged -> 'a
val value: ('a, _) tagged -> 'a

val error : 'n -> string -> 'a
