type ('a, 'b) tagged =
  { value : 'a
  ; tag : 'b
  }
[@@deriving sexp]

type 'a located = ('a, Position.t) tagged

let with_pos (pos : Position.t) value = { value; tag = pos }
let retag tag { value; _ } = { value; tag }
let tag a = a.tag

let error (type n) (a : n) s =
  let exception Exc of (n * string) in
  raise (Exc (a, s))
;;
