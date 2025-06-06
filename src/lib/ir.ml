open Position
module S = Sleigh

type endian = S.Endian.t
type integer = S.integer
type id = S.id

module Memory_segment = struct
  type kind = S.Space.k

  type t =
    { id : id
    ; kind : kind
    ; size : int
    ; word_size : int
    ; is_default : bool
    }

  let lift (space : S.Space.t) =
    { id = space.id
    ; kind = space.kind
    ; size = space.size
    ; word_size = space.word_size
    ; is_default = space.is_default
    }
  ;;
end

module Address = struct
  type t =
    { offset : int
    ; segment : Memory_segment.t option
    }
end

module Register = struct
  type t =
    { id : id
    ; size : int
    }

  let make ~id ~size = { id; size }
end

module Register_map = struct
  type t = Register.t option array

  let make ~arr : t = arr
end

module Bit_field = struct
  type t =
    { id : id
    ; size : int
    ; start_bit : int
    ; end_bit : int
    }
end

module Register_index = struct
  type t =
    { id : id
    ; map : Register_map.t
    ; map_index : int
    ; bit_field : Bit_field.t
    }

  let make ~id ~map ~index ~field =
    { id; map; map_index = index; bit_field = field }
  ;;
end

module Variable = struct
  type t =
    { id : id
    ; size : int
    }
end

module Intrinsic = struct
  type t = id
end

module Term = struct
  type _ t =
    | Memory_segment : Memory_segment.t -> Memory_segment.t t
    | Register : Register.t -> Register.t t
    | Register_index : Register_index.t -> Register_index.t t
    | Bit_field : Bit_field.t -> Bit_field.t t

  let value : type a. a t -> a = function
    | Memory_segment x -> x
    | Register x -> x
    | Register_index x -> x
    | Bit_field x -> x
  ;;
end

type term' = Term : 'a Term.t -> term' [@@unboxed]

module Expr = struct
  type t =
    | Binary of t * binary_op * t
    | Unary of unary_op * t
    | Pointer of t * Memory_segment.t option
    | TakeBits of int * int (* start, width *)
    | TakeBytes of int * bytes_from * int (* n, from, bit width *)
    | FunCall of id * t list
    | Term of term'
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
end

module Jump_target = struct
  type t =
    | Fixed of Address.t
    | Direct of Expr.t
    | Indirect of Expr.t
    | Label of id
end

module Transfer = struct
  type t =
    | Goto of Jump_target.t
    | Call of Jump_target.t
    | Return of Jump_target.t
end

module Statement = struct
  type t =
    | Assign of Expr.t * Expr.t
    | Macro_call of id * Expr.t list
    | Fun_call of id * Expr.t list
    | Transfer of Transfer.t
    | Branch of Expr.t * t
    | Export of Expr.t
    | Build of id
    | Label of id
end

module Macro = struct
  type t =
    { id : id
    ; args : id list
    ; statements : Statement.t list
    }

  let make ~id ~args ~statements = { id; args; statements }
end

module Pcode_op = struct
  type t = string
end

module Type = struct
  type _ t =
    | Register : Register.t t
    | Register_index : Register_index.t t
    | Bit_field : Bit_field.t t
    | Macro : Macro.t t
    | Memory_segment : Memory_segment.t t
    | Pcode_op : Pcode_op.t t
end

let term_of_value : type a. a -> a Type.t -> a Term.t =
  fun value -> function
  | Register -> Term.Register value
  | Register_index -> Term.Register_index value
  | Bit_field -> Term.Bit_field value
  | Macro -> Term.Macro value
  | Memory_segment -> Memory_segment value
  | Pcode_op -> Pcode_op value
;;

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
    | Memory_segment, Memory_segment -> Some Refl
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

  let rec find : type a. t -> string -> value_type option =
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

module Output = struct
  type t =
    | Text : string -> t
    | Term : 'a Term.t -> t
end

module Rule = struct
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
end

module Scanner = struct
  type t =
    { rules : Rule.t list
    ; is_instruction : bool
    }
end

type t =
  { endian : endian
  ; alignment : int
  ; default_segment : Memory_segment.t option
  ; scanners : Scanner.t array
  }

let dummy_id () =
  let open Position in
  with_pos dummy ""
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
    and value = Register.make ~id ~size:v.size in
    env := Type_env.append ~env:!env ~name ~ty ~value
  in
  List.iter f v.registers
;;

let lift_space_exn env (s : S.Space.t) =
  let name = s.id.value
  and ty = Type.Memory_segment
  and value = Memory_segment.lift s in
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
  let arr = Array.of_list (List.map f registers) in
  Register_map.make ~arr
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
    and value = Register_index.make ~id ~map ~index ~field in
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
     | Some (value, ty) -> ())
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
    | D.Macro m -> lift_macro_exn env m
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
| _ -> ()
