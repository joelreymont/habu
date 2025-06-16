open Sexplib.Std
open Tag

type 'a integer = (int, 'a) tagged [@@deriving sexp]
type 'a id = (string, 'a) tagged [@@deriving sexp]

let strcmp a b =
  let open String in
  equal (lowercase_ascii a) (lowercase_ascii b)
;;

let lower = String.lowercase_ascii

(* define endian=big; *)
module Endian = struct
  type t =
    | Big
    | Little
  [@@deriving sexp]
end

(*
   define space ram type=ram_space size=4 wordsize=1 default;
   define space register type=register_space size=4;
*)
module Space = struct
  type 'a t =
    { id : 'a id
    ; kind : k
    ; size : int
    ; word_size : int
    ; is_default : bool
    }

  and 'a m =
    [ `Kind of k
    | `Size of 'a integer
    | `WordSize of 'a integer
    | `Default of bool
    ]

  and k =
    | Rom
    | Ram
    | Register
  [@@deriving sexp]

  let make ~(id : 'a id) ~mods =
    let kind = ref None
    and size = ref None
    and word_size = ref 1
    and is_default = ref false in
    let f = function
      | `Kind k -> kind := Some k
      | `Size n -> size := Some n
      | `WordSize n -> word_size := n.value
      | `Default _ -> is_default := true
    in
    List.iter f mods;
    if !kind == None then error (tag id) "missing space type";
    match !size with
    | None -> error (tag id) "missing space type"
    | Some n when n.value == 0 -> error (tag id) "size must not be 0"
    | _ ->
      ();
      let kind = Option.get !kind
      and size = (Option.get !size).value
      and word_size = !word_size
      and is_default = !is_default in
      { id; kind; size; word_size; is_default }
  ;;

  let make_kind id1 id2 =
    if not (strcmp id1.value "type") then error (tag id1) "expecting 'type'";
    let kind = lower id2.value in
    let kind =
      match kind with
      | "rom" -> Rom
      | "ram" -> Ram
      | "register" -> Register
      | _ -> error (tag id2) "expecting 'rom', 'ram' or 'register'"
    in
    `Kind kind
  ;;

  let make_size id n =
    if n.value == 0 then error n "size must not be 0";
    match lower id.value with
    | "size" -> `Size n
    | "wordsize" -> `WordSize n
    | _ -> error (tag id) "expecting 'size' or 'wordsize'"
  ;;

  let make_default b = `Default b

  let strip_tags space =
    let id = retag () space.id in
    { space with id }
  ;;
end

(* define register offset=0 size=4 [r0 r1 r2 r3]; *)
module Varnode = struct
  type 'a t =
    { registers : 'a id list
    ; size : int
    ; offset : int
    }
  [@@deriving sexp]

  type m =
    | Offset of int
    | Size of int
  [@@deriving sexp]

  let make ~pos ~mods ~registers =
    let size = ref None
    and offset = ref None in
    let f = function
      | Size n -> size := Some n
      | Offset n -> offset := Some n
    in
    List.iter f mods;
    if !size == None then error pos "missing varnode size";
    if !offset == None then error pos "missing varnode offset";
    if List.is_empty registers then error pos "register list must not be empty";
    let size = Option.get !size
    and offset = Option.get !offset in
    { size; offset; registers }
  ;;

  let make_mod id n =
    match lower id.value with
    | "offset" -> Offset n.value
    | "size" ->
      if n.value == 0 then error n "size must not be 0";
      Size n.value
    | _ -> error (tag id) "expecting 'size' or 'offset'"
  ;;

  let strip_tags vn =
    let registers = List.map (retag ()) vn.registers in
    { vn with registers }
  ;;
end

(* attach variables [Rt Rs] [r0 r1];*)
module Varnode_attach = struct
  type 'a t =
    { fields : 'a id list
    ; registers : 'a id list
    }
  [@@deriving sexp]

  let make ~pos ~fields ~registers =
    if List.is_empty fields then error pos "field list must not be empty";
    if List.is_empty registers then error pos "register list must not be empty";
    { fields; registers }
  ;;

  let strip_tags va =
    let fields = List.map (retag ()) va.fields
    and registers = List.map (retag ()) va.registers in
    { fields; registers }
  ;;
end

module Token_field = struct
  type 'a t =
    { id : 'a id
    ; start_bit : 'a integer
    ; end_bit : 'a integer
    ; is_signed : bool
    ; is_hex : bool
    }

  and m =
    | Signed of bool
    | Hex of bool
  [@@deriving sexp]

  let make ~id ~start_bit ~end_bit ~mods =
    let is_signed = ref false
    and is_hex = ref false in
    let f = function
      | Signed flag -> is_signed := flag
      | Hex flag -> is_hex := flag
    in
    List.iter f mods;
    let is_signed = !is_signed
    and is_hex = !is_hex in
    { id; start_bit; end_bit; is_signed; is_hex }
  ;;

  let strip_tags tf =
    let id = retag () tf.id
    and start_bit = retag () tf.start_bit
    and end_bit = retag () tf.end_bit in
    { tf with id; start_bit; end_bit }
  ;;
end

module Token = struct
  type 'a t =
    { id : 'a id
    ; bit_size : 'a integer
    ; fields : 'a Token_field.t list
    }
  [@@deriving sexp]

  let strip_tags tok =
    let id = retag () tok.id
    and bit_size = retag () tok.bit_size
    and fields = List.map Token_field.strip_tags tok.fields in
    { id; bit_size; fields }
  ;;
end

module Expr = struct
  type 'a t =
    | Binary of binary_op * 'a t * 'a t
    | Unary of unary_op * 'a t
    | Paren of 'a t
    | FunCall of 'a id * 'a t list
    | Id of 'a id
    | Int of 'a integer
    | BitRange of 'a id * 'a integer * 'a integer
    | Pointer of 'a t * 'a id option
    | Sized of 'a t * 'a integer

  and binary_op =
    | JOIN
    | BOR
    | BAND
    | BXOR
    | OR
    | AND
    | XOR
    | EQ
    | NE
    | GT
    | LT
    | GE
    | LE
    | SGT
    | SLT
    | SGE
    | SLE
    | FEQ
    | FNE
    | FGT
    | FLT
    | FGE
    | FLE
    | LSHIFT
    | RSHIFT
    | SLSHIFT
    | SRSHIFT
    | PLUS
    | MINUS
    | FPLUS
    | FMINUS
    | MUL
    | DIV
    | MOD
    | SDIV
    | SMOD
    | FMUL
    | FDIV

  and unary_op =
    | ALEFT
    | ARIGHT
    | NOT
    | INV
    | NEG
    | FNEG
  [@@deriving sexp]

  let rec strip_tags = function
    | Binary (op, lhs, rhs) ->
      let lhs = strip_tags lhs
      and rhs = strip_tags rhs in
      Binary (op, lhs, rhs)
    | Unary (op, rhs) ->
      let rhs = strip_tags rhs in
      Unary (op, rhs)
    | Paren x -> Paren (strip_tags x)
    | FunCall (id, args) ->
      let id = retag () id
      and args = List.map strip_tags args in
      FunCall (id, args)
    | Id x -> Id (retag () x)
    | Int x -> Int (retag () x)
    | BitRange (id, n, m) ->
      let id = retag () id
      and n = retag () n
      and m = retag () m in
      BitRange (id, n, m)
    | Pointer (expr, space) ->
      let expr = strip_tags expr
      and space = Option.map (retag ()) space in
      Pointer (expr, space)
    | Sized (expr, n) ->
      let expr = strip_tags expr
      and n = retag () n in
      Sized (expr, n)
  ;;
end

module Pattern = struct
  type 'a t = 'a pattern list

  and 'a pattern =
    | Pattern of 'a pattern * pattern_op * 'a pattern
    | Constraint of 'a condition

  and 'a condition =
    | Condition of 'a id * condition_op * 'a expr
    | Symbol of 'a id

  and 'a expr = 'a Expr.t

  and condition_op =
    | EQ
    | NE
    | GT
    | LT

  and pattern_op =
    | OR
    | AND
  [@@deriving sexp]
end

module Display = struct
  type 'a t =
    { mnemonic : 'a piece list
    ; output : 'a piece list
    }

  and 'a piece =
    | Id of 'a id
    | Text of 'a id
    | Caret
    | Whitespace
  [@@deriving sexp]

  let strip_tags disp =
    let strip_piece_tags = function
      | Id x -> Id (retag () x)
      | Text x -> Text (retag () x)
      | Caret -> Caret
      | Whitespace -> Whitespace
    in
    let mnemonic = List.map strip_piece_tags disp.mnemonic
    and output = List.map strip_piece_tags disp.output in
    { mnemonic; output }
  ;;
end

module Jump_target = struct
  type 'a t =
    | Fixed of 'a integer * 'a id option
    | Direct of 'a id
    | Indirect of 'a Expr.t
    | Relative of 'a integer * 'a id
    | Label of 'a id
  [@@deriving sexp]

  let strip_tags = function
    | Fixed (addr, space) ->
      let addr = retag () addr
      and space = Option.map (retag ()) space in
      Fixed (addr, space)
    | Direct id -> Direct (retag () id)
    | Indirect expr -> Indirect (Expr.strip_tags expr)
    | Relative (ofs, space) ->
      let ofs = retag () ofs
      and space = retag () space in
      Relative (ofs, space)
    | Label id -> Label (retag () id)
  ;;
end

module Statement = struct
  type 'a t =
    | Assign of 'a Expr.t * 'a Expr.t
    | Declare of 'a id * 'a integer option
    | Fun_call of 'a id * 'a Expr.t list
    | Build of 'a id
    | Goto of 'a Jump_target.t
    | Call of 'a Jump_target.t
    | Return of 'a Jump_target.t
    | Branch of 'a Expr.t * 'a Jump_target.t
    | Label of 'a id
    | Export of 'a Expr.t
  [@@deriving sexp]

  let strip_tags = function
    | Assign (lhs, rhs) ->
      let lhs = Expr.strip_tags lhs
      and rhs = Expr.strip_tags rhs in
      Assign (lhs, rhs)
    | Declare (id, size) ->
      let id = retag () id
      and size = Option.map (retag ()) size in
      Declare (id, size)
    | Fun_call (id, args) ->
      let id = retag () id
      and args = List.map Expr.strip_tags args in
      Fun_call (id, args)
    | Build id -> Build (retag () id)
    | Goto target -> Goto (Jump_target.strip_tags target)
    | Call target -> Call (Jump_target.strip_tags target)
    | Return target -> Return (Jump_target.strip_tags target)
    | Branch (expr, target) ->
      let expr = Expr.strip_tags expr
      and target = Jump_target.strip_tags target in
      Branch (expr, target)
    | Label id -> Label (retag () id)
    | Export expr -> Export (Expr.strip_tags expr)
  ;;
end

module Macro = struct
  type 'a t =
    { id : 'a id
    ; args : 'a id list
    ; body : 'a Statement.t list
    }
  [@@deriving sexp]

  let strip_tags m =
    let id = retag () m.id
    and args = List.map (retag ()) m.args
    and body = List.map Statement.strip_tags m.body in
    { id; args; body }
  ;;
end

module Constructor = struct
  type 'a t =
    { id : 'a id option
    ; display : 'a Display.t
    ; pattern : 'a Expr.t option
    ; context : 'a Statement.t list
    ; body : 'a Statement.t list
    }
  [@@deriving sexp]

  let strip_tags ctr =
    let id = Option.map (retag ()) ctr.id
    and display = Display.strip_tags ctr.display
    and pattern = Option.map Expr.strip_tags ctr.pattern
    and context = List.map Statement.strip_tags ctr.context
    and body = List.map Statement.strip_tags ctr.body in
    { id; display; pattern; context; body }
  ;;
end

module Definition = struct
  type 'a t =
    | Endian of (Endian.t, 'a) tagged
    | Alignment of 'a integer
    | Space of 'a Space.t
    | Varnode of 'a Varnode.t
    | Varnode_attach of 'a Varnode_attach.t
    | Token of 'a Token.t
    | Pcode_op of 'a id
    | Constructor of 'a Constructor.t
    | Macro of 'a Macro.t
  [@@deriving sexp]

  let strip_tags = function
    | Endian x -> Endian (retag () x)
    | Alignment x -> Alignment (retag () x)
    | Space x -> Space (Space.strip_tags x)
    | Varnode x -> Varnode (Varnode.strip_tags x)
    | Varnode_attach x -> Varnode_attach (Varnode_attach.strip_tags x)
    | Token x -> Token (Token.strip_tags x)
    | Pcode_op x -> Pcode_op (retag () x)
    | Constructor x -> Constructor (Constructor.strip_tags x)
    | Macro x -> Macro (Macro.strip_tags x)
  ;;
end

type 'a t = 'a Definition.t list [@@deriving sexp]

let strip_tags tree = List.map Definition.strip_tags tree
