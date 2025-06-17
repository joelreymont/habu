type ('a, 'b) tagged =
  { value : 'a
  ; tag : 'b
  }

let sexp_of_tagged sexp_of_a _ { value; tag = _ } = sexp_of_a value

let tagged_of_sexp _ _ _ =
  let exception Exc of string in
  raise (Exc "tagged_of_sexp unimplemented!")
;;

type 'a located = ('a, Position.t) tagged

let make value tag = { value; tag }

let with_loc (pos : 'a Position.located) value =
  { value; tag = Position.position pos }
;;

let with_pos (pos : Position.t) value = { value; tag = pos }
let set_tag tag tagged = { tagged with tag }
let set_value value tagged = { tagged with value }
let clear_tag tagged = { tagged with tag = () }
let tag a = a.tag
let value a = a.value
