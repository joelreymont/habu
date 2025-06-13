open Sexplib.Std
open Position
module S = Sleigh

type endian = S.Endian.t [@@deriving sexp]
type integer = S.integer [@@deriving sexp]
type id = S.id [@@deriving sexp]

module rec Memory_region : sig
  type kind = S.Space.k [@@deriving sexp]

  type t =
    { id : id
    ; kind : kind
    ; size : int
    ; word_size : int
    ; is_default : bool
    }
  [@@deriving sexp]
end =
  Memory_region

and Address : sig
  type t =
    { offset : int
    ; segment : Memory_region.t option
    }
  [@@deriving sexp]
end =
  Address

and Register : sig
  type t =
    { id : id
    ; size : int
    }
  [@@deriving sexp]
end =
  Register

and Register_map : sig
  type t : value = Register.t option array [@@deriving sexp]
end =
  Register_map

and Bit_field : sig
  type t =
    { id : id
    ; size : int
    ; start_bit : int
    ; end_bit : int
    }
  [@@deriving sexp]
end =
  Bit_field

and Register_index : sig
  type t =
    { id : id
    ; map : Register_map.t
    ; index : int
    ; field : Bit_field.t
    }
  [@@deriving sexp]
end =
  Register_index

and Variable : sig
  type t =
    { id : id
    ; size : int
    }
  [@@deriving sexp]
end =
  Variable

and Intrinsic : sig
  type t = id [@@deriving sexp]
end =
  Intrinsic

and Expr : sig
  type t =
    | Binary of t * binary_op * t
    | Unary of unary_op * t
    | Pointer of t * Memory_region.t option
    | TakeBits of int * int (* start, width *)
    | TakeBytes of int * bytes_from * int (* n, from, bit width *)
    | FunCall of id * t list
    | Register of Register.t
    | Register_index of Register_index.t
    | Bit_field of Bit_field.t
    | Number of int

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
  Expr

and Jump_target : sig
  type t =
    | Fixed of Address.t
    | Direct of Expr.t
    | Indirect of Expr.t
    | Label of id
  [@@deriving sexp]
end =
  Jump_target

and Transfer : sig
  type t =
    | Goto of Jump_target.t
    | Call of Jump_target.t
    | Return of Jump_target.t
  [@@deriving sexp]
end =
  Transfer

and Statement : sig
  type t =
    | Assign of Expr.t * Expr.t
    | Macro_call of id * Expr.t list
    | Fun_call of id * Expr.t list
    | Transfer of Transfer.t
    | Branch of Expr.t * t
    | Export of Expr.t
    | Build of id
    | Label of id
  [@@deriving sexp]
end =
  Statement

and Macro : sig
  type t =
    { id : id
    ; args : id list
    ; statements : Statement.t list
    }
  [@@deriving sexp]
end =
  Macro

and Pcode_op : sig
  type t : value = string [@@deriving sexp]
end =
  Pcode_op

and Scanner : sig
  type t =
    { rules : Rule.t list
    ; is_instruction : bool
    }
  [@@deriving sexp]
end =
  Scanner

and Rule : sig
  type t =
    { id : id
    ; mnemonic : Output.t list
    ; output : Output.t list
    ; setup : Statement.t list
    ; effects : Statement.t list
    ; pattern : Expr.t option
    ; byte_width : int
    ; alignment : int
    }
  [@@deriving sexp]
end =
  Rule

and Output : sig
  type t =
    | Text : string -> t
    | Register : Register.t -> t
    | Register_index : Register_index.t -> t
    | Bit_field : Bit_field.t -> t
    | Scanner : Scanner.t -> t
end =
  Output

and Type : sig
  type _ t =
    | Register : Register.t t
    | Register_index : Register_index.t t
    | Bit_field : Bit_field.t t
    | Macro : Macro.t t
    | Memory_region : Memory_region.t t
    | Pcode_op : Pcode_op.t t
    | Scanner : Scanner.t t
  [@@deriving sexp]
end =
  Type

module Type_env = struct
  type t =
    | Nil
    | Cons : string * 'a Type.t * 'a * t -> t

  type ('a, 'b) eq = Refl : ('a, 'a) eq

  let make () = Nil

  let types_equal : type a b. a Type.t -> b Type.t -> (a, b) eq option =
    fun a b ->
    let open Type in
    match a, b with
    | Register, Register -> Some Refl
    | Register_index, Register_index -> Some Refl
    | Bit_field, Bit_field -> Some Refl
    | Macro, Macro -> Some Refl
    | Memory_region, Memory_region -> Some Refl
    | _, _ -> None
  ;;

  let append ~env ~name ~ty ~value = Cons (name, ty, value, env)

  let rec lookup : type a. t -> string -> a Type.t -> a option =
    fun env name ty ->
    match env with
    | Nil -> None
    | Cons (xname, xty, value, rest) ->
      if name = xname then (
        match types_equal ty xty with
        | Some Refl -> Some value
        | None -> assert false
      ) else
        lookup rest name ty
  ;;

  type value_type = Value_type : ('a * 'a Type.t) -> value_type [@@unboxed]

  let rec find : t -> string -> value_type option =
    fun env name ->
    match env with
    | Nil -> None
    | Cons (xname, ty, value, rest) ->
      if name = xname then
        Some (Value_type (value, ty))
      else
        find rest name
  ;;
end

type t =
  { endian : endian
  ; alignment : int
  ; default_segment : Memory_region.t option
  ; scanners : Scanner.t array
  }
[@@deriving sexp]

let dummy_id () =
  let open Position in
  with_pos dummy ""
;;

let lift_space (space : S.Space.t) =
  Memory_region.
    { id = space.id
    ; kind = space.kind
    ; size = space.size
    ; word_size = space.word_size
    ; is_default = space.is_default
    }
;;

let lift_varnode_exn env (v : S.Varnode.t) =
  let f id =
    let name = id.value in
    (match Type_env.lookup !env name Type.Register with
     | None ->
       let pos = position id in
       Error.error pos (Printf.sprintf "Duplicate register '%s'" name)
     | _ -> ());
    let ty = Type.Register
    and value = Register.{ id; size = v.size } in
    env := Type_env.append ~env:!env ~name ~ty ~value
  in
  List.iter f v.registers
;;

let lift_space_exn env (s : S.Space.t) =
  let name = s.id.value
  and ty = Type.Memory_region
  and value = lift_space s in
  env := Type_env.append ~env:!env ~name ~ty ~value
;;

let make_register_map_exn env registers =
  let f id =
    let name = id.value in
    if name == "_" then
      None
    else (
      match Type_env.lookup !env name Type.Register with
      | None ->
        let pos = position id in
        Error.error pos (Printf.sprintf "Unknown register '%s'" name)
      | x -> x
    )
  in
  Array.of_list (List.map f registers)
;;

let lift_varnode_attach_exn env (vna : S.Varnode_attach.t) =
  let map = make_register_map_exn env vna.registers in
  let f index id =
    let name = id.value in
    let field =
      match Type_env.lookup !env name Type.Bit_field with
      | None ->
        let pos = position id in
        Error.error pos (Printf.sprintf "Unknown bit field '%s'" name)
      | Some value -> value
    in
    let ty = Type.Register_index
    and value = Register_index.{ id; map; index; field } in
    env := Type_env.append ~env:!env ~name ~ty ~value
  in
  List.iteri f vna.fields
;;

let lift_pcode_op_exn env id =
  let name = id.value in
  match Type_env.lookup !env name Type.Pcode_op with
  | Some _ ->
    let pos = position id in
    Error.error pos (Printf.sprintf "Duplicate pcode op '%s'" name)
  | None ->
    let ty = Type.Pcode_op
    and value : Pcode_op.t = name in
    env := Type_env.append ~env:!env ~name ~ty ~value
;;

(*
   let lift_macro_exn env (m : S.Macro.t) =
  let name = m.id.value in
  match Type_env.lookup !env name Type.Pcode_op with
  | Some _ ->
    let pos = position m.id in
    Error.error pos (Printf.sprintf "Duplicate macro '%s'" name)
  | None ->
    (* TODO: Check for duplicate arg names? *)
    let statements = List.map lift_statement_exn m.body in
    let ty = Type.Macro
    and value = Macro.make ~id:m.id ~args:m.args ~statements in
    env := Type_env.append ~env:!env ~name ~ty ~value
;;
*)
let lift_expr_exn env (expr : S.Expr.t) =
  (* | Binary of binary_op * t * t *)
  (* | Unary of unary_op * t *)
  (* | Paren of t *)
  (* | FunCall of id * t list *)
  (* | Id of id *)
  (* | Int of integer *)
  (* | BitRange of id * integer * integer *)
  (* | Pointer of t * id option *)
  (* | Sized of t * integer *)
  match expr with
  | Id id ->
    let name = id.value in
    (match Type_env.find !env name with
     | None ->
       let pos = position id in
       Error.error pos (Printf.sprintf "Unknown register '%s'" name)
     | Some _value -> ())
  | _ -> ()
;;

let lift_defs_exn defs =
  let env = ref (Type_env.make ())
  and endian = ref None
  and alignment = ref None
  and default_segment = ref None
  and scanners = ref [] in
  let module D = S.Definition in
  let f = function
    | D.Endian e -> endian := Some e.value
    | D.Alignment a -> alignment := Some a.value
    | D.Space s -> lift_space_exn env s
    | D.Varnode vn -> lift_varnode_exn env vn
    | D.Varnode_attach vna -> lift_varnode_attach_exn env vna
    | D.Pcode_op op -> lift_pcode_op_exn env op
    (* | D.Macro m -> lift_macro_exn env m *)
    | _ -> assert false
  in
  List.iter f defs;
  { endian = Option.get !endian
  ; alignment = Option.get !alignment
  ; default_segment = !default_segment
  ; scanners = Array.of_list !scanners
  }
;;

let gensym prefix =
  let count = ref (-1) in
  fun () ->
    incr count;
    prefix ^ string_of_int !count
;;
