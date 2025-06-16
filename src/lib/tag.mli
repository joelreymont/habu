type ('a, 'b) tagged =
  { value : 'a
  ; tag : 'b
  }
[@@deriving sexp]

type 'a located = ('a, Position.t) tagged

val with_pos : Position.t -> 'a -> ('a, Position.t) tagged
val retag : 'a -> ('b, 'c) tagged -> ('b, 'a) tagged
val tag : (_, 'a) tagged -> 'a

val error : 'n -> string -> 'a
