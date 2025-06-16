open Sexplib.Std
open Tag
module S = Sleigh

type endian = S.Endian.t [@@deriving sexp]
type 'n integer = 'n S.integer [@@deriving sexp]
type 'n id = 'n S.id [@@deriving sexp]

module rec Memory_region : sig
  type kind = S.Space.k [@@deriving sexp]

  type 'n t =
    { id : 'n id
    ; kind : kind
    ; size : int
    ; word_size : int
    ; is_default : bool
    }
  [@@deriving sexp]
end =
  Memory_region

and Address : sig
  type 'n t =
    { offset : int
    ; segment : 'n Memory_region.t option
    }
  [@@deriving sexp]
end =
  Address

and Register : sig
  type 'n t =
    { id : 'n id
    ; size : int
    }
  [@@deriving sexp]
end =
  Register

and Register_map : sig
  type 'n t : value = 'n Register.t option array [@@deriving sexp]
end =
  Register_map

and Bit_field : sig
  type 'n t =
    { id : 'n id
    ; size : int
    ; start_bit : int
    ; end_bit : int
    }
  [@@deriving sexp]
end =
  Bit_field

and Register_index : sig
  type 'n t =
    { id : 'n id
    ; map : 'n Register_map.t
    ; index : int
    ; field : 'n Bit_field.t
    }
  [@@deriving sexp]
end =
  Register_index

and Variable : sig
  type 'n t =
    { id : 'n id
    ; size : int
    }
  [@@deriving sexp]
end =
  Variable

and Intrinsic : sig
  type 'n t = 'n id [@@deriving sexp]
end =
  Intrinsic

and Expr : sig
  type 'n t =
    | Binary of 'n t * binary_op * 'n t
    | Unary of unary_op * 'n t
    | Pointer of 'n t * 'n Memory_region.t option
    | TakeBits of int * int * 'n (* start, width *)
    | TakeBytes of int * bytes_from * int * 'n (* n, from, bit width *)
    | FunCall of 'n id * 'n t list
    | Register of 'n Register.t
    | Register_index of 'n Register_index.t
    | Bit_field of 'n Bit_field.t
    | Number of 'n integer

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
  type 'n t =
    | Fixed of 'n Address.t
    | Direct of 'n Expr.t
    | Indirect of 'n Expr.t
    | Label of 'n id
  [@@deriving sexp]
end =
  Jump_target

and Transfer : sig
  type 'n t =
    | Goto of 'n Jump_target.t
    | Call of 'n Jump_target.t
    | Return of 'n Jump_target.t
  [@@deriving sexp]
end =
  Transfer

and Statement : sig
  type 'n t =
    | Assign of 'n Expr.t * 'n Expr.t
    | Macro_call of 'n id * 'n Expr.t list
    | Fun_call of 'n id * 'n Expr.t list
    | Transfer of 'n Transfer.t
    | Branch of 'n Expr.t * 'n t
    | Export of 'n Expr.t
    | Build of 'n id
    | Label of 'n id
  [@@deriving sexp]
end =
  Statement

and Macro : sig
  type 'n t =
    { id : 'n id
    ; args : 'n id list
    ; statements : 'n Statement.t list
    }
  [@@deriving sexp]
end =
  Macro

and Pcode_op : sig
  type 'n t : value = 'n id [@@deriving sexp]
end =
  Pcode_op

and Scanner : sig
  type 'n t =
    { rules : 'n Rule.t list
    ; is_instruction : bool
    }
  [@@deriving sexp]
end =
  Scanner

and Rule : sig
  type 'n t =
    { id : 'n id
    ; mnemonic : 'n Output.t list
    ; output : 'n Output.t list
    ; setup : 'n Statement.t list
    ; effects : 'n Statement.t list
    ; pattern : 'n Expr.t option
    ; byte_width : int
    ; alignment : int
    }
  [@@deriving sexp]
end =
  Rule

and Output : sig
  type 'n t =
    | Text : string -> 'n t
    | Register : 'n Register.t -> 'n t
    | Register_index : 'n Register_index.t -> 'n t
    | Bit_field : 'n Bit_field.t -> 'n t
    | Scanner : 'n Scanner.t -> 'n t
end =
  Output

and Type : sig
  type (_, _) t =
    | Register : ('n Register.t, 'n) t
    | Register_index : ('n Register_index.t, 'n) t
    | Bit_field : ('n Bit_field.t, 'n) t
    | Macro : ('n Macro.t, 'n) t
    | Memory_region : ('n Memory_region.t, 'n) t
    | Pcode_op : ('n Pcode_op.t, 'n) t
    | Scanner : ('n Scanner.t, 'n) t
  [@@deriving sexp]
end =
  Type

module Type_env = struct
  type 'n t =
    | Nil
    | Cons : string * ('a, 'n) Type.t * 'a * 'n t -> 'n t

  type ('a, 'b, 'na, 'nb) eq = Refl : ('a, 'a, 'na, 'na) eq

  let make () = Nil

  let types_equal
    : type a b n. (a, n) Type.t -> (b, n) Type.t -> (a, b, n, n) eq option
    =
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

  let rec lookup : type a n. n t -> string -> (a, n) Type.t -> a option =
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

  type 'n value = Value : ('a * ('a, 'n) Type.t) -> 'n value [@@unboxed]

  let rec find : 'n t -> string -> 'n value option =
    fun env name ->
    match env with
    | Nil -> None
    | Cons (xname, ty, value, rest) ->
      if name = xname then
        Some (Value (value, ty))
      else
        find rest name
  ;;
end

type 'a t =
  { endian : endian
  ; alignment : int
  ; default_segment : 'a Memory_region.t option
  ; scanners : 'a Scanner.t array
  }
[@@deriving sexp]

let dummy_id () =
  let open Position in
  with_pos dummy ""
;;

let lift_space (space : 'a S.Space.t) =
  Memory_region.
    { id = space.id
    ; kind = space.kind
    ; size = space.size
    ; word_size = space.word_size
    ; is_default = space.is_default
    }
;;

let lift_varnode_exn env (v : 'a S.Varnode.t) =
  let f id =
    let name = id.value in
    (match Type_env.lookup !env name Type.Register with
     | None -> error (tag id) (Printf.sprintf "Duplicate register '%s'" name)
     | _ -> ());
    let ty = Type.Register
    and value = Register.{ id; size = v.size } in
    env := Type_env.append ~env:!env ~name ~ty ~value
  in
  List.iter f v.registers
;;

let lift_space_exn env (s : 'a S.Space.t) =
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
      | None -> error (tag id) (Printf.sprintf "Unknown register '%s'" name)
      | x -> x
    )
  in
  Array.of_list (List.map f registers)
;;

let lift_varnode_attach_exn env (vna : 'a S.Varnode_attach.t) =
  let map = make_register_map_exn env vna.registers in
  let f index id =
    let name = id.value in
    let field =
      match Type_env.lookup !env name Type.Bit_field with
      | None -> error (tag id) (Printf.sprintf "Unknown bit field '%s'" name)
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
  | Some _ -> error (tag id) (Printf.sprintf "Duplicate pcode op '%s'" name)
  | None ->
    let ty = Type.Pcode_op
    and value : 'a Pcode_op.t = id in
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
let lift_expr_exn env (expr : 'a S.Expr.t) =
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
     | None -> error (tag id) (Printf.sprintf "Unknown register '%s'" name)
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
