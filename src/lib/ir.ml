open Sexplib
open Sexplib.Std
open Tag
open Error
module S = Sleigh

type endian = S.Endian.t [@@deriving sexp]

module Id : sig
  type t = private int

  val intern : string -> t
  val get : t -> string option
end = struct
  type t = int

  let id = ref (-1)
  let map = Hashtbl.create 100
  let xref = Hashtbl.create 100

  let intern s =
    try Hashtbl.find xref s with
    | Not_found ->
      id := !id + 1;
      Hashtbl.add map !id s;
      Hashtbl.add xref s !id;
      !id
  ;;

  let get id =
    try Some (Hashtbl.find map id) with
    | Not_found -> None
  ;;

  let sexp_of_t =
    Conv.sexp_of_error

    function
    | Ok r -> sexp_of_a r
    | Error e -> Error.sexp_of_t e
  ;;

  let t_of_sexp a_of_sexp sexp =
    match sexp with
    | Sexp.List [ Atom "ok"; s ] -> Ok (a_of_sexp s)
    | Sexp.List [ Atom "error"; s ] -> Error (Error.t_of_sexp s)
    | _ -> Conv.of_sexp_error "t_of_sexp: atom needed" sexp
  ;;
end

type id = (Id.t, Position.t) tagged [@@deriving sexp]

module rec Memory_region' : sig
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
  Memory_region'

and Address' : sig
  type t =
    { offset : int
    ; region : Memory_region'.t option
    }
  [@@deriving sexp]
end =
  Address'

and Register' : sig
  type t =
    { id : id
    ; size : int
    }
  [@@deriving sexp]
end =
  Register'

and Register_map' : sig
  type t : value = Register'.t option array [@@deriving sexp]
end =
  Register_map'

and Bit_field' : sig
  type t =
    { id : id
    ; bit_size : int
    ; start_bit : int
    ; end_bit : int
    ; is_signed : bool
    }
  [@@deriving sexp]
end =
  Bit_field'

and Register_index' : sig
  type t =
    { map : Register_map'.t
    ; field : Bit_field'.t
    }
  [@@deriving sexp]
end =
  Register_index'

and Variable' : sig
  type t =
    { id : id
    ; size : int
    }
  [@@deriving sexp]
end =
  Variable'

and Intrinsic' : sig
  type t = { id : id } [@@deriving sexp]
end =
  Intrinsic'

and Expr' : sig
  type t =
    | Binary of
        { lhs : t
        ; op : binary_op
        ; rhs : t
        }
    | Unary of
        { op : unary_op
        ; rhs : t
        }
    | Pointer of
        { expr : t
        ; region : Memory_region'.t option
        }
    (* TODO: is it a variable? *)
    | TakeBits of
        { id : id
        ; start_bit : int
        ; bit_width : int
        }
    (* | TakeBytes of int * bytes_from * int *)
    | FunCall of
        { id : id
        ; args : t list
        }
    | Register of Register'.t
    | Register_index of Register_index'.t
    | Bit_field of Bit_field'.t
    | Scanner of Scanner'.t
    | Int of int
    (* Invalid expressions *)
    | Intrinsic of Intrinsic'.t
    | Macro of Macro'.t
    | Memory_region of Memory_region'.t
    | Pcode_op of Pcode_op'.t

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
    | Direct of Expr'.t
    | Indirect of Expr'.t
    | Label of id
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
    | Macro_call of id * Expr'.t list
    | Fun_call of id * Expr'.t list
    | Transfer of Transfer'.t
    | Branch of Expr'.t * Jump_target'.t
    | Export of Expr'.t
    | Build of id
    | Label of id
  [@@deriving sexp]
end =
  Statement'

and Macro' : sig
  type t =
    { args : id list
    ; statements : Statement'.t list
    }
  [@@deriving sexp]
end =
  Macro'

and Pcode_op' : sig
  type t : value = unit [@@deriving sexp]
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
    ; pattern : Expr'.t option
    }
  [@@deriving sexp]
end =
  Rule'

and Output' : sig
  type t =
    | Text of string
    | Register of Register'.t
    | Register_index of Register_index'.t
    | Bit_field of Bit_field'.t
    | Scanner of Scanner'.t
end =
  Output'

and Env' : sig
  type t = private (Id.t, Expr'.t) Hashtbl.t
end =
  Env'

module Env = struct
  include Env'

  let add env id expr = Hashtbl.add env id expr

  let get env id =
    try Ok (Hashtbl.find env id) with
    | Not_found -> Error `Not_found
  ;;

  (*
     | Register of Register'.t
    | Register_index of Register_index'.t
    | Bit_field of Bit_field'.t
    | Scanner of Scanner'.t
    | Int of int
    | Intrinsic of Intrinsic'.t
    | Macro of Macro'.t
    | Memory_region of Memory_region'.t
    | Pcode_op of Pcode_op'.t
  *)
end

module Memory_region = struct
  include Memory_region'

  let lift env (s : 'a S.Space.t) =
    let id = Id.intern (value s.id) in
    let region =
      { id
      ; kind = s.kind
      ; size = s.size
      ; word_size = s.word_size
      ; is_default = s.is_default
      }
    in
    let expr = Expr'.Memory_region region in
    Env.add !env id expr;
    Ok ()
  ;;
end

module Address = struct
  include Address'
end

module Register = struct
  include Register'

  let lift env (v : 'a S.Varnode.t) =
    let f id =
      let pos = tag id
      and id = Id.intern (value id) in
      match Env.get !env id with
      | Ok _ -> Error (`Duplicate pos)
      | Error _ ->
        let reg = { id; size = v.size } in
        let expr = Expr'.Register reg in
        Env.add !env id expr;
        Ok ()
    in
    Result.iter f v.registers
  ;;

  let of_expr pos expr =
    match expr with
    | Expr'.Register x -> Ok x
    | _ -> Error (`Invalid pos)
  ;;
end

module Register_map = struct
  include Register_map'

  let lift env registers =
    let f id =
      let pos = tag id in
      let name = value id in
      if name == "_" then
        Ok None
      else (
        let id = Id.intern name in
        match Env.get !env id with
        | Error _ -> Error (`Not_found pos)
        | Ok expr ->
          let* reg = Register.of_expr pos expr in
          Ok (Some reg)
      )
    in
    let* l = Result.map_list f registers in
    Ok (Array.of_list l)
  ;;
end

module Bit_field = struct
  include Bit_field'

  let lift env (tok : 'a S.Token.t) =
    let id = tok.id in
    let pos = tag id
    and name = value id in
    let id = Id.intern id in
    match Env.get !env id with
    | Ok _ -> Error (`Duplicate pos)
    | Error _ ->
      let byte_size = tok.bit_size.value lsr 3 in
      let lift (field : 'a S.Token_field.t) =
        let value =
          { id = field.id
          ; byte_size
          ; start_bit = field.start_bit.value
          ; end_bit = field.end_bit.value
          ; is_signed = field.is_signed
          }
        in
        Env.add !env
      in
      List.iter lift tok.fields
  ;;
end

module Register_index = struct
  include Register_index'

  let lift_exn env (vna : 'a S.Varnode_attach.t) =
    let map = Register_map.lift_exn env vna.registers in
    let f index id =
      let name = id.value in
      let field =
        match Type_env.lookup !env name Type.Bit_field with
        | None ->
          error
            (tag id)
            (Printf.sprintf "Unknown bit field '%s'" name)
        | Some value -> value
      in
      let ty = Type.Register_index
      and value = { id; map; index; field } in
      env := Type_env.append ~env:!env ~name ~ty ~value
    in
    List.iteri f vna.fields
  ;;
end

module Variable = struct
  include Variable'
end

module Intrinsic = struct
  include Intrinsic'
end

module Expr = struct
  include Expr'
end

module Jump_target = struct
  include Jump_target'
end

module Transfer = struct
  include Transfer'
end

module Statement = struct
  include Statement'
end

module Macro = struct
  include Macro'
end

module Pcode_op = struct
  include Pcode_op'

  let lift_exn env id =
    let name = id.value in
    match Type_env.lookup !env name Type.Pcode_op with
    | Some _ ->
      error (tag id) (Printf.sprintf "Duplicate pcode op '%s'" name)
    | None ->
      let ty = Type.Pcode_op
      and value : 'a t = id in
      env := Type_env.append ~env:!env ~name ~ty ~value
  ;;
end

module Output = struct
  include Output'

  let lift env (piece : 'a S.Display.piece) =
    match piece with
    | Text id -> Text id.value
    | Id id ->
      let name = id.value in
      (match Type_env.lookup !env name Type.Register with
       | Some value -> Register value
       | None ->
         (match Type_env.lookup !env name Type.Register_index with
          | Some value -> Register_index value
          | None ->
            (match Type_env.lookup !env name Type.Bit_field with
             | Some value -> Bit_field value
             | None ->
               (match Type_env.lookup !env name Type.Scanner with
                | Some value -> Bit_field value
                | None ->
                  error
                    (tag id)
                    (Printf.sprintf "Unknown id '%s'" name)))))
  ;;
end

module Rule = struct
  include Rule'

  let lift_exn env (ctr : 'a S.Constructor.t) =
    let id = ctr.id
    and mnemonic = List.map (Output.lift_exn env) ctr.display.mnemonic
    and output = List.map (Output.lift_exn env) ctr.display.output
    and setup = List.map (Statement.lift_exn env) ctr.context
    and effects = List.map (Statement.lift_exn env) ctr.body
    and pattern = Expr.lift_exn env ctr.pattern in
    { id; mnemonic; output; setup; effects; pattern }
  ;;
end

module Scanner = struct
  include Scanner'

  let lift_exn (ctr : 'a S.Constructor.t) =
    let name = ctr.id.value
    and is_instruction = List.length ctr.display.mnemonic > 0 in
    let result = Type_env.lookup !env name Type.Scanner in
    let scanner =
      match is_instruction, result with
      | true, Some _ ->
        error (tag id) (Printf.sprintf "Duplicate rule '%s'" name)
      | _, None ->
        let value = { rules = ref []; is_instruction }
        and ty = Type.Scanner in
        env := Type_env.append ~env:!env ~name ~ty ~value;
        value
      | false, Some s -> s
    in
    let rule = Rule.lift_exn env ctr in
    scanner.rules := rule :: !scanner.rules
  ;;
end

(*
   fn handleConstructor(
    ir: *IR,
    node: AST.Constructor,
    scanners: *Symbols,
) Error!void {
    const name = node.id.name;
    const loc = node.id.loc;
    const is_instruction = node.have_mnemonic();
    var symbol: *Symbol = undefined;
    if (is_instruction or !ir.type_env.contains(name, .scanner)) {
        var scanner = IR.Scanner.init(ir.alloc);
        scanner.is_instruction = is_instruction;
        symbol = try Symbol.make(ir.alloc, .{
            .name = name,
            .loc = loc,
            .ty = null,
            .kind = .{ .scanner = scanner },
        });
        if (!is_instruction) {
            try ir.type_env.add(name, .scanner, symbol);
        }
        try scanners.append(symbol);
    }
    if (!is_instruction)
        symbol = ir.type_env.get(name, .scanner) orelse
            return fail(ir, node.id.loc, error.MissingSymbol);
    var rule = IR.Rule.init(ir.alloc, node.id.loc);
    rule.symbol = symbol;
    try handleContext(ir, &rule, node.context);
    var actions = Actions.init(ir.alloc);
    try handleBitPattern(ir, &rule, node.bit_pattern, &actions);
    if (is_instruction)
        try handleMnemonic(ir, &rule, node);
    try handleActions(ir, &rule, node.body, &actions);
    rule.actions = try actions.toOwnedSlice();
    try handleRuleOutput(ir, &rule, node);
    try symbol.kind.scanner.rules.append(rule);
}
*)

type 'a t =
  { endian : endian
  ; alignment : int
  ; default_region : 'a Memory_region'.t option
  ; scanners : 'a Scanner.t array
  }
[@@deriving sexp]

let dummy_id () =
  let open Position in
  with_pos dummy ""
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
    (match Type_env.exists !env name with
     | None ->
       error (tag id) (Printf.sprintf "Unknown register '%s'" name)
     | Some _value -> ())
  | _ -> ()
;;

let lift_defs_exn defs =
  let env = ref (Type_env.make ())
  and endian = ref None
  and alignment = ref None
  and default_region = ref None
  and scanners = ref [] in
  let module D = S.Definition in
  let f = function
    | D.Endian e -> endian := Some e.value
    | D.Alignment a -> alignment := Some a.value
    | D.Space s -> Memory_region.lift env s
    | D.Varnode vn -> Register.lift_exn env vn
    | D.Varnode_attach vna -> Register_index.lift_exn env vna
    | D.Token tok -> Bit_field.lift_exn env tok
    | D.Pcode_op op -> Pcode_op.lift_exn env op
    | D.Constructor ctr -> Scanner.lift_exn ctr
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
  }
;;

let gensym prefix =
  let count = ref (-1) in
  fun () ->
    incr count;
    prefix ^ string_of_int !count
;;
