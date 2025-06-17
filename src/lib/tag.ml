type ('a, 'b) tagged =
  { value : 'a
  ; tag : 'b
  }
[@@deriving sexp]

type 'a located = ('a, Position.t) tagged

let make value tag = { value; tag }
let with_pos (pos : Position.t) value = { value; tag = pos }
let set_tag tagged tag = { tagged with tag }
let set_value tagged value = { tagged with value }
let clear_tag tagged = { tagged with tag = () }
let tag a = a.tag
let value a = a.value

let error (type n) (a : n) s =
  let exception Exc of (n * string) in
  raise (Exc (a, s))
;;
