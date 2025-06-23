open Sexplib.Std
open Error
open Tag

type integer = (int, Position.t) tagged [@@deriving sexp]
type id = (string, Position.t) tagged [@@deriving sexp]

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
    | `Size of integer
    | `WordSize of integer
    | `Default of bool
    ]

  and k =
    | Rom
    | Ram
    | Register
  [@@deriving sexp]

  let make ~(id : id) ~mods =
    let kind = ref None
    and size = ref None
    and word_size = ref 1
    and is_default = ref false in
    let f = function
      | `Kind k -> kind := Some k
      | `Size n -> size := Some n
      | `WordSize n -> word_size := value n
      | `Default _ -> is_default := true
    in
    List.iter f mods;
    if !kind == None then error (tag id) "missing space type";
    match !size with
    | None -> error (tag id) "missing space type"
    | Some n when value n == 0 -> error (tag id) "size must not be 0"
    | _ ->
      ();
      let kind = Option.get !kind
      and size = value (Option.get !size)
      and word_size = !word_size
      and is_default = !is_default in
      { id; kind; size; word_size; is_default }
  ;;

  let make_kind id1 id2 =
    if not (strcmp (value id1) "type") then error (tag id1) "expecting 'type'";
    let kind = value id2 |> lower in
    let kind =
      match kind with
      | "rom_space" -> Rom
      | "ram_space" -> Ram
      | "register_space" -> Register
      | _ -> error (tag id2) "expecting 'rom', 'ram' or 'register'"
    in
    `Kind kind
  ;;

  let make_size id n =
    if value n == 0 then error (tag n) "size must not be 0";
    match value id |> lower with
    | "size" -> `Size n
    | "wordsize" -> `WordSize n
    | _ -> error (tag id) "expecting 'size' or 'wordsize'"
  ;;

  let make_default b = `Default b
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
    match value id |> lower with
    | "offset" -> Offset (value n)
    | "size" ->
      if value n == 0 then error (tag n) "size must not be 0";
      Size (value n)
    | _ -> error (tag id) "expecting 'size' or 'offset'"
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
    if List.is_empty fields then error pos "field list must not be empty";
    if List.is_empty registers then error pos "register list must not be empty";
    { fields; registers }
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
    | NOT
    | INV
    | NEG
    | FNEG
  [@@deriving sexp]
end

module Pattern = struct
  type t =
    | Binary of t * pattern_op * t
    | Paren of t
    | Align_left of t
    | Align_right of t
    | Constraint of condition

  and condition =
    | Condition of id * condition_op * expr
    | Symbol of id

  and expr = Expr.t

  and alignment =
    | Left
    | Right

  and condition_op =
    | EQ
    | NE
    | GT
    | LT
    | GE
    | LE

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
    { id : id
    ; display : Display.t
    ; pattern : Pattern.t list
    ; context : Statement.t list
    ; body : Statement.t list
    }
  [@@deriving sexp]
end

module Definition = struct
  type t =
    | Endian of (Endian.t, Position.t) tagged
    | Alignment of integer
    | Space of Space.t
    | Varnode of Varnode.t
    | Varnode_attach of Varnode_attach.t
    | Token of Token.t
    | Pcode_op of id
    | Constructor of Constructor.t
    | Macro of Macro.t
  [@@deriving sexp]
end

type t = Definition.t list [@@deriving sexp]
