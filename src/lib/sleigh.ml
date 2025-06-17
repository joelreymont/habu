open Sexplib.Std
open Tag
open Error

type loc = Position.t [@@deriving sexp]
type integer = (int, loc) tagged [@@deriving sexp]
type id = (string, loc) tagged [@@deriving sexp]

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
  type t =
    { id : id
    ; kind : k
    ; size : int
    ; word_size : int
    ; is_default : bool
    }

  and m =
    [ `Kind of k
    | `Size of int
    | `Word_size of int
    | `Default of bool
    ]

  and k =
    | Rom
    | Ram
    | Register
  [@@deriving sexp]

  let make ~(id : id) ~mods =
    let space =
      { id; kind = Ram; size = 0; word_size = 1; is_default = false }
    and f space = function
      | `Kind k -> { space with kind = k }
      | `Size n -> { space with size = n.value }
      | `Word_size n -> { space with word_size = n.value }
      | `Default _ -> { space with is_default = true }
    in
    let pred space = space.size > 0 && space.word_size > 0 in
    let* space = Result.fold f space mods in
    Result.of_value pred (with_pos (tag id) space)
  ;;

  let make_kind key value =
    let* _ = Result.of_value (fun _ -> strcmp key.value "type") key in
    let kind = lower value.value in
    let* kind =
      match kind with
      | "rom" -> Ok Rom
      | "ram" -> Ok Ram
      | "register" -> Ok Register
      | _ -> Error (`Invalid (tag value))
    in
    Ok (`Kind kind)
  ;;

  let make_size key value =
    let* _ = Result.ensure_positive value in
    match lower key.value with
    | "size" -> Ok (`Size value)
    | "wordsize" -> Ok (`Word_size value)
    | _ -> Error (`Invalid (tag key))
  ;;

  let make_default b = Ok (`Default b)
end

(* define register offset=0 size=4 [r0 r1 r2 r3]; *)
module Varnode = struct
  type t =
    { registers : id list
    ; size : int
    ; offset : int
    }
  [@@deriving sexp]

  type m =
    | Offset of int
    | Size of int
  [@@deriving sexp]

  let make ~pos ~mods ~registers =
    let node = { registers; size = 0; offset = 0 } in
    let f node = function
      | Size n -> { node with size = n }
      | Offset n -> { node with offset = n }
    in
    let* node = Result.fold f node mods in
    let pred _ =
      node.size > 0
      && node.offset >= 0
      && not (List.is_empty node.registers)
    in
    Result.of_value pred (with_loc pos node)
  ;;

  let make_mod key value =
    match lower key.value with
    | "offset" -> Ok (Offset value.value)
    | "size" -> Ok (Size value.value)
    | _ -> Error (`Invalid (tag key))
  ;;
end

(* attach variables [Rt Rs] [r0 r1];*)
module Varnode_attach = struct
  type t =
    { fields : id list
    ; registers : id list
    }
  [@@deriving sexp]

  let make ~pos ~fields ~registers =
    let* _ = Result.ensure_not_empty (with_loc pos fields) in
    let* _ = Result.ensure_not_empty (with_loc pos registers) in
    Ok { fields; registers }
  ;;
end

module Token_field = struct
  type t =
    { id : id
    ; start_bit : integer
    ; end_bit : integer
    ; is_signed : bool
    ; is_hex : bool
    }

  and m =
    | Signed of bool
    | Hex of bool
  [@@deriving sexp]

  let make ~id ~start_bit ~end_bit ~mods =
    let field =
      { id; start_bit; end_bit; is_signed = false; is_hex = false }
    in
    let f field = function
      | Signed flag -> { field with is_signed = flag }
      | Hex flag -> { field with is_hex = flag }
    in
    List.fold_left f field mods
  ;;
end

module Token = struct
  type t =
    { id : id
    ; bit_size : integer
    ; fields : Token_field.t list
    }
  [@@deriving sexp]
end

module Expr = struct
  type t =
    | Binary of binary_op * t * t
    | Unary of unary_op * t
    | Paren of t
    | Fun_call of id * t list
    | Id of id
    | Int of integer
    | Bit_range of id * integer * integer
    | Pointer of t * id option
    | Sized of t * integer

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
end

module Pattern = struct
  type t = pattern list

  and pattern =
    | Pattern of pattern * pattern_op * pattern
    | Constraint of condition

  and condition =
    | Condition of id * condition_op * expr
    | Symbol of id

  and expr = Expr.t

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
  type t =
    { mnemonic : piece list
    ; output : piece list
    }

  and piece =
    | Id of id
    | Text of id
    | Caret
    | Whitespace
  [@@deriving sexp]
end

module Jump_target = struct
  type t =
    | Fixed of integer * id option
    | Direct of id
    | Indirect of Expr.t
    | Relative of integer * id
    | Label of id
  [@@deriving sexp]
end

module Statement = struct
  type t =
    | Assign of Expr.t * Expr.t
    | Declare of id * integer option
    | Fun_call of id * Expr.t list
    | Build of id
    | Goto of Jump_target.t
    | Call of Jump_target.t
    | Return of Jump_target.t
    | Branch of Expr.t * Jump_target.t
    | Label of id
    | Export of Expr.t
  [@@deriving sexp]
end

module Macro = struct
  type t =
    { id : id
    ; args : id list
    ; body : Statement.t list
    }
  [@@deriving sexp]
end

module Constructor = struct
  type t =
    { id : id option
    ; display : Display.t
    ; pattern : Expr.t option
    ; context : Statement.t list
    ; body : Statement.t list
    }
  [@@deriving sexp]
end

module Definition = struct
  type t =
    | Endian of (Endian.t, loc) tagged
    | Alignment of integer
    | Space of Space.t
    | Varnode of Varnode.t
    | Varnode_attach of Varnode_attach.t
    | Token of Token.t
    | Pcode_op of id
    | Constructor of Constructor.t
    | Macro of Macro.t
  [@@deriving sexp]

  let make_endian r = Result.map (fun x -> Endian x) r
  let make_alignment r = Result.map (fun x -> Alignment x) r
  let make_space r = Result.map (fun x -> Space x) r
  let make_varnode r = Result.map (fun x -> Varnode x) r
  let make_varnode_attach r = Result.map (fun x -> Varnode_attach x) r
  let make_token r = Result.map (fun x -> Token x) r
  let make_pcode_op r = Result.map (fun x -> Pcode_op x) r
  let make_constructor r = Result.map (fun x -> Constructor x) r
  let make_macro r = Result.map (fun x -> Macro x) r
end

type t = Definition.t list [@@deriving sexp]
