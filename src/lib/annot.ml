type ('a, 'b) annotated =
  { value : 'a
  ; note : 'b
  }
[@@deriving sexp]

type 'a located = ('a, Position.t) annotated

let with_pos (pos: Position.t) value = { value; note = pos }
  
let error (type n) (a : n) s =
  let exception Exc of (n * string) in
  raise (Exc (a, s))
;;
