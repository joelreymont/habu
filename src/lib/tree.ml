open Sexplib.Std
open Error
open Tag
module S = Sleigh_tree
module StringMap = Map.Make (String)

let ( let* ) = Result.bind

module Tag = struct
  type t =
    { size : int option
    ; hint : hint
    ; pos : Position.t
    }

  and hint =
    | Unsigned
    | Signed
    | Float

  let empty = { size = None; hint = Unsigned; pos = Position.dummy }
  let t_of_sexp _ = empty
  let sexp_of_t _ = sexp_of_unit ()
end

type endian = S.Endian.t [@@deriving sexp]
type integer = (int, Tag.t) tagged [@@deriving sexp]

module Symbol = struct
  type t = int [@@deriving sexp]

  let next = ref 0
  let table = Hashtbl.create 100

  let make name =
    try Hashtbl.find table name with
    | Not_found ->
      next := !next + 1;
      Hashtbl.add table name !next;
      !next
  ;;
end

module Env = struct
  type 'a t = (Symbol.t, 'a) Hashtbl.t

  let add (env : 'a t) = Hashtbl.add env

  let find (env : 'a t) name =
    try Some (Hashtbl.find env name) with
    | Not_found -> None
  ;;

  let empty () = Hashtbl.create 100
end

module Id = struct
  type t = (Symbol.t, Tag.t) tagged [@@deriving sexp]

  let lift id =
    let name = value id in
    let sym = Symbol.make name in
    make sym Tag.empty
  ;;
end

module rec Memory_region' : sig
  type kind = S.Space.k [@@deriving sexp]

  type t =
    { kind : kind
    ; word_size : int
    ; is_default : bool
    ; tag : Tag.t
    }
  [@@deriving sexp]
end =
  Memory_region'

and Address' : sig
  type t =
    { offset : int
    ; region : Memory_region'.t option
    ; tag : Tag.t
    }
  [@@deriving sexp]
end =
  Address'

and Register' : sig
  type t = { tag : Tag.t } [@@deriving sexp]
end =
  Register'

and Register_map' : sig
  type t : value = { registers : Register'.t option array } [@@deriving sexp]
end =
  Register_map'

and Bit_field' : sig
  type t =
    { start_bit : int
    ; end_bit : int
    ; tag : Tag.t
    }
  [@@deriving sexp]
end =
  Bit_field'

and Register_index' : sig
  type t =
    { map : Register_map'.t
    ; index : int
    ; field : Bit_field'.t
    }
  [@@deriving sexp]
end =
  Register_index'

and Variable' : sig
  type t = { tag : Tag.t } [@@deriving sexp]
end =
  Variable'

and Intrinsic' : sig
  type t = { tag : Tag.t } [@@deriving sexp]
end =
  Intrinsic'

and Expr' : sig
  type t =
    | Binary of t * binary_op * t
    | Unary of unary_op * t
    | Paren of t
    | Pointer of t * Memory_region'.t option
    | Take_bits of t * int * int (* start, width *)
    (* | Take_bytes of t * int * bytes_from * int (* n, from, bit width *) *)
    | Take_bytes of t * int
    | Fun_call of Intrinsic'.t * t list
    | Register of Register'.t
    | Register_index of Register_index'.t
    | Bit_field of Bit_field'.t
    | Scanner of Scanner'.t
    | Variable of Variable'.t
    | Number of integer
    | Void

  and bytes_from =
    | Top
    | Bottom

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
    | LE
    | GE
    | LSHIFT
    | RSHIFT
    | PLUS
    | MINUS
    | MUL
    | DIV
    | MOD

  and unary_op =
    | NOT
    | INV
    | NEG
  [@@deriving sexp]
end =
  Expr'

and Jump_target' : sig
  type t =
    | Fixed of Address'.t
    | Relative of Address'.t
    | Direct of Expr'.t
    | Indirect of Expr'.t
    | Label of Id.t
  [@@deriving sexp]
end =
  Jump_target'

and Transfer' : sig
  type t =
    | Goto of Jump_target'.t
    | Call of Jump_target'.t
    | Return of Jump_target'.t
  [@@deriving sexp]
end =
  Transfer'

and Statement' : sig
  type t =
    | Assign of Expr'.t * Expr'.t
    | Macro_call of Macro'.t * Expr'.t list
    | Fun_call of Intrinsic'.t * Expr'.t list
    | Transfer of Transfer'.t
    | Branch of Expr'.t * t
    | Export of Expr'.t
    | Build of Id.t
    | Label of Id.t
  [@@deriving sexp]
end =
  Statement'

and Macro' : sig
  type t =
    { args : Id.t list
    ; statements : Statement'.t list
    }
  [@@deriving sexp]
end =
  Macro'

and Pcode_op' : sig
  type t : value = { tag : Tag.t } [@@deriving sexp]
end =
  Pcode_op'

and Scanner' : sig
  type t =
    { rules : Rule'.t list ref
    ; is_instruction : bool
    }
  [@@deriving sexp]
end =
  Scanner'

and Rule' : sig
  type t =
    { mnemonic : Output'.t list
    ; output : Output'.t list
    ; setup : Statement'.t list
    ; effects : Statement'.t list
    ; pattern : Expr'.t list
    }
  [@@deriving sexp]
end =
  Rule'

and Output' : sig
  type t =
    | Text : string -> t
    | Register : Register'.t -> t
    | Register_index : Register_index'.t -> t
    | Bit_field : Bit_field'.t -> t
    | Scanner : Scanner'.t -> t
end =
  Output'

module Value = struct
  type t =
    | Register of Register'.t
    | Register_index of Register_index'.t
    | Bit_field of Bit_field'.t
    | Intrinsic of Intrinsic'.t
    | Macro of Macro'.t
    | Pcode_op of Pcode_op'.t
    | Scanner of Scanner'.t
    | Memory_region of Memory_region'.t
    | Variable of Variable'.t

  type env = t Env.t

  let env : unit -> env = Env.empty
  let add = Env.add
  let find = Env.find
end

module Memory_region = struct
  include Memory_region'

  let lift env (s : S.Space.t) =
    let name = value s.id in
    let tag = { Tag.empty with pos = tag s.id; size = Some s.size } in
    let sym = Symbol.make name in
    let region =
      { kind = s.kind; word_size = s.word_size; is_default = s.is_default; tag }
    in
    let value = Value.Memory_region region in
    Env.add env sym value
  ;;
end

module Address = struct
  include Address'
end

module Register = struct
  include Register'

  let lift_exn (env : Value.env) (v : S.Varnode.t) =
    let f id =
      let name = value id in
      let sym = Symbol.make name in
      (match Value.find env sym with
       | None -> error (tag id) (Printf.sprintf "Duplicate register '%s'" name)
       | _ -> ());
      let tag = { Tag.empty with pos = tag id; size = Some v.size } in
      let value = Value.Register { tag } in
      Value.add env sym value
    in
    List.iter f v.registers
  ;;
end

module Register_map = struct
  include Register_map'

  let lift_exn (env : Value.env) registers =
    let f id =
      let name = value id in
      if name == "_" then
        None
      else (
        let sym = Symbol.make name
        and pos = tag id in
        match Value.find env sym with
        | None -> error pos (Printf.sprintf "Unknown register '%s'" name)
        | Some value ->
          (match value with
           | Register value -> Some value
           | _ -> error pos (Printf.sprintf "Not a register '%s'" name))
      )
    in
    let registers = Array.of_list (List.map f registers) in
    { registers }
  ;;
end

module Bit_field = struct
  include Bit_field'

  let lift_exn env (tok : S.Token.t) =
    let id = tok.id in
    let name = value id
    and pos = tag id in
    let sym = Symbol.make name in
    match Value.find env sym with
    | Some _ -> error pos (Printf.sprintf "Duplicate bit field '%s'" name)
    | None ->
      let size = Some (value tok.bit_size lsr 3) in
      let lift (field : S.Token_field.t) =
        let hint =
          if field.is_signed then
            Tag.Signed
          else
            Tag.Unsigned
        in
        let tag = Tag.{ pos; size; hint } in
        let field =
          { start_bit = value field.start_bit
          ; end_bit = value field.end_bit
          ; tag
          }
        in
        let value = Value.Bit_field field in
        Value.add env sym value
      in
      List.iter lift tok.fields
  ;;
end

module Register_index = struct
  include Register_index'

  let lift_exn (env : Value.env) (vna : S.Varnode_attach.t) =
    let map = Register_map.lift_exn env vna.registers in
    let f index id =
      let name = value id
      and pos = tag id in
      let sym = Symbol.make name in
      let field =
        match Value.find env sym with
        | None -> error pos (Printf.sprintf "Unknown bit field '%s'" name)
        | Some value ->
          (match value with
           | Bit_field field -> field
           | _ -> error pos (Printf.sprintf "Not a bit field '%s'" name))
      in
      let value = Value.Register_index { map; index; field } in
      Value.add env sym value
    in
    List.iteri f vna.fields
  ;;
end

module Variable = struct
  include Variable'

  let lift_exn env (id : S.id) size =
    let pos = tag id
    and name = value id in
    let sym = Symbol.make name in
    match Value.find env sym with
    | Some _ -> error pos (Printf.sprintf "Re-declaration of '%s'" name)
    | None ->
      let tag = { Tag.empty with pos; size } in
      let var = { tag } in
      let value = Value.Variable var in
      Value.add env sym value;
      var
  ;;
end

module Intrinsic = struct
  include Intrinsic'
end

let region_of_id_exn env (id : S.id option) =
  match id with
  | None -> None
  | Some id ->
    let name = value id in
    let sym = Symbol.make name in
    (match Value.find env sym with
     | None -> error (tag id) (Printf.sprintf "Unknown memory space '%s'" name)
     | Some (Value.Memory_region value) -> Some value
     | _ -> error (tag id) (Printf.sprintf "Not a memory space '%s'" name))
;;

module Expr = struct
  include Expr'

  let lift_binary_op (op : S.Expr.binary_op) =
    let module E = S.Expr in
    match op with
    | E.BOR -> BOR, Tag.Unsigned
    | E.BAND -> BAND, Tag.Unsigned
    | E.BXOR -> BXOR, Tag.Unsigned
    | E.OR -> OR, Tag.Unsigned
    | E.AND -> AND, Tag.Unsigned
    | E.XOR -> XOR, Tag.Unsigned
    | E.EQ -> EQ, Tag.Unsigned
    | E.NE -> NE, Tag.Unsigned
    | E.GT -> GT, Tag.Unsigned
    | E.LT -> LT, Tag.Unsigned
    | E.GE -> GE, Tag.Unsigned
    | E.LE -> LE, Tag.Unsigned
    | E.SGT -> GT, Tag.Signed
    | E.SLT -> LT, Tag.Signed
    | E.SGE -> GE, Tag.Signed
    | E.SLE -> LE, Tag.Signed
    | E.FEQ -> EQ, Tag.Float
    | E.FNE -> NE, Tag.Float
    | E.FGT -> GT, Tag.Float
    | E.FLT -> LT, Tag.Float
    | E.FGE -> GE, Tag.Float
    | E.FLE -> LE, Tag.Float
    | E.LSHIFT -> LSHIFT, Tag.Unsigned
    | E.RSHIFT -> RSHIFT, Tag.Unsigned
    | E.SLSHIFT -> LSHIFT, Tag.Signed
    | E.SRSHIFT -> RSHIFT, Tag.Signed
    | E.PLUS -> PLUS, Tag.Unsigned
    | E.MINUS -> MINUS, Tag.Unsigned
    | E.FPLUS -> PLUS, Tag.Float
    | E.FMINUS -> MINUS, Tag.Float
    | E.MUL -> MUL, Tag.Unsigned
    | E.DIV -> DIV, Tag.Unsigned
    | E.MOD -> MOD, Tag.Unsigned
    | E.SDIV -> DIV, Tag.Signed
    | E.SMOD -> MOD, Tag.Signed
    | E.FMUL -> MUL, Tag.Float
    | E.FDIV -> DIV, Tag.Float
  ;;

  let lift_unary_op (op : S.Expr.unary_op) =
    let module E = S.Expr in
    match op with
    | E.NOT -> NOT, Tag.Unsigned
    | E.INV -> INV, Tag.Unsigned
    | E.NEG -> NEG, Tag.Unsigned
    | E.FNEG -> NEG, Tag.Float
  ;;

  let lift_exn env (expr : S.Expr.t) =
    let module E = S.Expr in
    let rec lift env hint = function
      | E.Binary (op, lhs, rhs) ->
        let op, hint = lift_binary_op op in
        let lhs = lift env hint lhs
        and rhs = lift env hint rhs in
        Binary (lhs, op, rhs)
      | E.Unary (op, rhs) ->
        let op, hint = lift_unary_op op in
        let rhs = lift env hint rhs in
        Unary (op, rhs)
      | E.Paren expr ->
        let expr = lift env hint expr in
        Paren expr
      | E.Pointer (expr, id) ->
        let region = region_of_id_exn env id in
        let expr = lift env hint expr in
        Pointer (expr, region)
      | E.Bit_range (id, start, width) ->
        let name = value id in
        let sym = Symbol.make name in
        let var =
          match Value.find env sym with
          | None -> error (tag id) (Printf.sprintf "Unknown variable '%s'" name)
          | Some (Value.Variable var) -> var
          | _ -> error (tag id) (Printf.sprintf "Not a variable'%s'" name)
        in
        Take_bits (Variable var, value start, value width)
      | E.Id id ->
        let name = value id in
        let sym = Symbol.make name in
        (match Value.find env sym with
         | Some (Register value) -> Register value
         | Some (Register_index value) -> Register_index value
         | Some (Bit_field value) -> Bit_field value
         | Some (Scanner value) -> Scanner value
         | Some (Variable value) -> Variable value
         | _ -> error (tag id) (Printf.sprintf "Invalid id '%s'" name))
      | E.Fun_call (id, args) ->
        let name = value id in
        let sym = Symbol.make name in
        (match Value.find env sym with
         | None -> error (tag id) (Printf.sprintf "Unknown function '%s'" name)
         | Some (Intrinsic f) ->
           let args = List.map (lift env hint) args in
           Fun_call (f, args)
         | _ ->
           error (tag id) (Printf.sprintf "Not a built-in function '%s'" name))
      | E.Sized (expr, n) ->
        let n = value n
        and expr = lift env hint expr in
        Take_bytes (expr, n)
      | E.Int n ->
        let pos = tag n in
        let tag = { Tag.empty with pos } in
        let n = make (value n) tag in
        Number n
    in
    lift env Unsigned expr
  ;;
end

module Jump_target = struct
  include Jump_target'

  let lift_exn env (target : S.Jump_target.t) =
    let lift_addr offset id =
      let offset = value offset
      and pos = tag offset in
      let region = region_of_id_exn env id
      and tag = { Tag.empty with pos } in
      Address.{ offset; region; tag }
    in
    match target with
    | Fixed (offset, id) -> Fixed (lift_addr offset id)
    | Indirect expr -> Indirect (Expr.lift_exn env expr)
    | Label id -> Label (Id.lift id)
    | Direct id ->
      let name = value id in
      let sym = Symbol.make name in
      let expr =
        let open Expr in
        match Value.find env sym with
        | Some (Register value) -> Register value
        | Some (Register_index value) -> Register_index value
        | Some (Bit_field value) -> Bit_field value
        | Some (Scanner value) -> Scanner value
        | Some (Variable value) -> Variable value
        | _ -> error (tag id) (Printf.sprintf "Unknown varnode '%s'" name)
      in
      Direct expr
  ;;
end

module Transfer = struct
  include Transfer'
end

module Statement = struct
  include Statement'

  let lift_exn env (stmt : S.Statement.t) =
    let open Transfer in
    match stmt with
    | Assign (lhs, rhs) ->
      let lhs = Expr.lift_exn env lhs
      and rhs = Expr.lift_exn env rhs in
      Assign (lhs, rhs)
    | Declare (id, size) ->
      let size = Option.map value size in
      let var = Variable.lift_exn env id size in
      Assign (Expr.Variable var, Expr.Void)
    | Fun_call (id, args) ->
      let name = value id in
      let sym = Symbol.make name in
      (match Value.find env sym with
       | None -> error (tag id) (Printf.sprintf "Unknown function '%s'" name)
       | Some (Intrinsic f) ->
         let args = List.map (Expr.lift_exn env) args in
         Fun_call (f, args)
       | _ ->
         error (tag id) (Printf.sprintf "Not a built-in function '%s'" name))
    | Build id -> Build (Id.lift id)
    | Label id -> Build (Id.lift id)
    | Goto target ->
      let target = Jump_target.lift_exn env target in
      let xfer = Goto target in
      Transfer xfer
    | Call target ->
      let target = Jump_target.lift_exn env target in
      let xfer = Call target in
      Transfer xfer
    | Return target ->
      let target = Jump_target.lift_exn env target in
      let xfer = Return target in
      Transfer xfer
    | Branch (expr, target) ->
      let expr = Expr.lift_exn env expr
      and target = Jump_target.lift_exn env target in
      let xfer = Transfer (Goto target) in
      Branch (expr, xfer)
    | Export expr -> Export (Expr.lift_exn env expr)
  ;;
end

module Macro = struct
  include Macro'
end

module Pcode_op = struct
  include Pcode_op'

  let lift_exn env (id : S.id) =
    let name = value id
    and pos = tag id in
    let sym = Symbol.make name in
    match Value.find env sym with
    | Some _ -> error pos (Printf.sprintf "Duplicate pcode op '%s'" name)
    | None ->
      let tag = { Tag.empty with pos } in
      let value = Value.Pcode_op { tag } in
      Value.add env sym value
  ;;
end

module Output = struct
  include Output'

  let lift_exn (env : Value.env) (piece : S.Display.piece) =
    match piece with
    | Text id -> Some (Text (value id))
    | Id id ->
      let name = value id
      and tag = tag id in
      let sym = Symbol.make name in
      (match Value.find env sym with
       | Some value ->
         let out =
           match value with
           | Register x -> Register x
           | Register_index x -> Register_index x
           | Bit_field x -> Bit_field x
           | Scanner x -> Scanner x
           | _ -> error tag (Printf.sprintf "Invalid id '%s'" name)
         in
         Some out
       | None -> error tag (Printf.sprintf "Unknown id '%s'" name))
    | Caret | Whitespace -> None
  ;;
end

(*
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

  and pattern_op =
    | OR
    | AND
  [@@deriving sexp]
end
*)

module Rule = struct
  include Rule'

  let lift_exn (env : Value.env) (ctr : S.Constructor.t) =
    let rec lift_pat env (pat : S.Pattern.t) : Expr.t =
      match pat with
      | Binary (lhs, op, rhs) ->
        let lhs = lift_pat env lhs
        and rhs = lift_pat env rhs
        and op =
          match op with
          | OR -> Expr.BOR
          | AND -> Expr.BAND
        in
        Binary (lhs, op, rhs)
      | Paren pat ->
        let expr = lift_pat env pat in
        Paren expr
      (* TODO: Fix aligned patterns *)
      | Align_left pat -> lift_pat env pat
      | Align_right pat -> lift_pat env pat
      | Constraint cond ->
        (match cond with
         | Condition (id, op, expr) ->
           let lhs = Expr.lift_exn env (S.Expr.Id id)
           and op =
             match op with
             | EQ -> Expr.EQ
             | NE -> Expr.NE
             | GE -> Expr.GE
             | LE -> Expr.LE
             | GT -> Expr.GT
             | LT -> Expr.LT
           and rhs = Expr.lift_exn env expr in
           Binary (lhs, op, rhs)
         | Symbol id -> Expr.lift_exn env (S.Expr.Id id))
    in
    let mnemonic = List.filter_map (Output.lift_exn env) ctr.display.mnemonic
    and output = List.filter_map (Output.lift_exn env) ctr.display.output
    and setup = List.map (Statement.lift_exn env) ctr.context
    and effects = List.map (Statement.lift_exn env) ctr.body
    and pattern = List.map (lift_pat env) ctr.pattern in
    { mnemonic; output; setup; effects; pattern }
  ;;
end

module Scanner = struct
  include Scanner'

  let lift_exn env (ctr : S.Constructor.t) =
    let name = value ctr.id
    and is_instruction = List.length ctr.display.mnemonic > 0 in
    let sym = Symbol.make name in
    let result = Value.find env sym in
    let scanner =
      match is_instruction, result with
      | true, Some _ ->
        error (tag ctr.id) (Printf.sprintf "Duplicate constructor '%s'" name)
      | _, None ->
        let scanner = { rules = ref []; is_instruction } in
        let value = Value.Scanner scanner in
        Value.add env sym value;
        scanner
      | false, Some (Value.Scanner s) -> s
      | _ ->
        error (tag ctr.id) (Printf.sprintf "Invalid constructor name '%s'" name)
    in
    let rule = Rule.lift_exn env ctr in
    let rules = scanner.rules in
    scanner.rules := rule :: !rules
  ;;
end

type t =
  { endian : endian
  ; alignment : int
  ; default_region : Memory_region.t option
  ; scanners : Scanner.t array
  ; env : Value.env
  }

let dummy_id () =
  let open Position in
  with_pos dummy ""
;;

let lift_defs_exn defs =
  let env = Value.env ()
  and endian = ref None
  and alignment = ref None
  and default_region = ref None
  and scanners = ref [] in
  let open S.Definition in
  let f = function
    | Endian e -> endian := Some (value e)
    | Alignment a -> alignment := Some (value a)
    | Space s -> Memory_region.lift env s
    | Varnode vn -> Register.lift_exn env vn
    | Varnode_attach vna -> Register_index.lift_exn env vna
    | Token tok -> Bit_field.lift_exn env tok
    | Pcode_op op -> Pcode_op.lift_exn env op
    | Constructor ctr -> Scanner.lift_exn env ctr
    (*
       | D.Macro m -> lift_macro_exn env m
    *)
    | _ -> assert false
  in
  List.iter f defs;
  { endian = Option.get !endian
  ; alignment = Option.get !alignment
  ; default_region = !default_region
  ; scanners = Array.of_list !scanners
  ; env
  }
;;

let gensym prefix =
  let count = ref (-1) in
  fun () ->
    incr count;
    prefix ^ string_of_int !count
;;

let inline_macros tree = Ok tree
let inline_scanners tree = Ok tree

let lift defs text buffer =
  let* tree =
    try Ok (lift_defs_exn defs) with
    | Error.Error (poss, msg) ->
      output_string stderr (Error.print_error poss msg);
      Error (fail text buffer)
  in
  let* tree = inline_macros tree in
  let* tree = inline_scanners tree in
  Ok tree
;;
