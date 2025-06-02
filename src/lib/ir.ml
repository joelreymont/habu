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
  type t = Register.t array
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
    | Register_index : Register_index.t t
    | Bit_field : Bit_field.t t
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
end

module Type = struct
  type _ t =
    | Register : Register.t t
    | Register_index : Register_index.t t
    | Bit_field : Bit_field.t t
    | Macro : Macro.t t
    | Memory_segment : Memory_segment.t t
end

module Type_env = struct
  type t =
    | Nil
    | Cons : string * 'a Type.t * 'a Term.t * t -> t

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

  let append ~env ~name ~ty ~term = Cons (name, ty, term, env)

  let rec lookup_exn : type a. t -> string -> a Type.t -> a Term.t =
    fun env name ty ->
    match env with
    | Nil -> raise Not_found
    | Cons (xname, xty, term, rest) ->
      if name = xname then (
        match types_equal ty xty with
        | Some Refl -> term
        | None -> assert false
      ) else
        lookup_exn rest name ty
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

let lift_exn defs =
  let env = ref (Type_env.make ())
  and endian = ref None
  and alignment = ref None
  and default_segment = ref None
  and scanners = ref [] in
  let module D = S.Definition in
  let f = function
    | D.Endian e -> endian := Some e.value
    | D.Alignment a -> alignment := Some a.value
    | D.Space s ->
      let name = s.id.value
      and ty = Type.Memory_segment
      and segment = Memory_segment.lift s in
      let term = Term.Memory_segment segment in
      env := Type_env.append ~env:!env ~name ~ty ~term
    | D.Varnode v ->
      let f id =
        let name = id.value
        and ty = Type.Register
        and reg = Register.make ~id ~size:v.size in
        let term = Term.Register reg in
        env := Type_env.append ~env:!env ~name ~ty ~term
      in
      List.iter f v.registers
    | _ -> assert false
  in
  List.iter f defs;
  { endian = Option.get !endian
  ; alignment = Option.get !alignment
  ; default_segment = !default_segment
  ; scanners = Array.of_list !scanners
  }
;;
