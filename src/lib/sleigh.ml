open Sexplib.Std
open Position

type endian =
  | Send_big
  | Send_little
[@@deriving sexp]

type integer = int located [@@deriving sexp]
type identifier = string located [@@deriving sexp]

type program = definition list

and definition =
  | Sdef_endian of endian located
  | Sdef_alignment of integer
  | Sdef_space of space
  | Sdef_varnode of varnode
  | Sdef_varnode_attach of varnode_attach
  | Sdef_token of tok
  | Sdef_pcode_op of identifier
  | Sdef_constructor of constructor
  | Sdef_macro of macro

and space =
  { ssp_id : identifier
  ; ssp_mods : space_mod list
  }

and space_mod =
  | Sspmod_id of identifier * identifier
  | Sspmod_int of identifier * integer
  | Sspmod_is_default of bool

and varnode =
  { svn_mods : (identifier * integer) list
  ; svn_registers : identifier list
  }

and varnode_attach =
  { svna_nodes : identifier list
  ; svna_values : identifier list
  }

and tok =
  { stok_id : identifier
  ; stok_bit_size : integer
  ; stok_fields : token_field list
  }

and constructor =
  { sctr_id : identifier option
  ; sctr_display : display
  ; sctr_pattern : expr option
  ; sctr_context : statement list
  ; sctr_body : statement list
  }

and macro =
  { smac_id : identifier
  ; smac_args : identifier list
  ; smac_body : statement list
  }

and expr =
  | Sexp_binary of binary_op * expr * expr
  | Sexp_unary of unary_op * expr
  | Sexp_paren of expr
  | Sexp_fun_call of identifier * args
  | Sexp_id of identifier
  | Sexp_int of integer
  | Sexp_bit_range of range
  | Sexp_pointer of pointer
  | Sexp_sized of sized

and args = expr list

and binary_op =
  | Sexp_op_join
  | Sexp_op_bool_or
  | Sexp_op_bool_and
  | Sexp_op_bool_xor
  | Sexp_op_or
  | Sexp_op_and
  | Sexp_op_xor
  | Sexp_op_eq
  | Sexp_op_ne
  | Sexp_op_float_eq
  | Sexp_op_float_ne
  | Sexp_op_gt
  | Sexp_op_lt
  | Sexp_op_ge
  | Sexp_op_le
  | Sexp_op_signed_gt
  | Sexp_op_signed_lt
  | Sexp_op_signed_ge
  | Sexp_op_signed_le
  | Sexp_op_float_gt
  | Sexp_op_float_lt
  | Sexp_op_float_ge
  | Sexp_op_float_le
  | Sexp_op_shift_left
  | Sexp_op_shift_right
  | Sexp_op_signed_shift_left
  | Sexp_op_signed_shift_right
  | Sexp_op_plus
  | Sexp_op_minus
  | Sexp_op_float_plus
  | Sexp_op_float_minus
  | Sexp_op_mul
  | Sexp_op_div
  | Sexp_op_mod
  | Sexp_op_signed_div
  | Sexp_op_signed_mod
  | Sexp_op_float_mul
  | Sexp_op_float_div

and unary_op =
  | Sexp_op_align_left
  | Sexp_op_align_right
  | Sexp_op_not
  | Sexp_op_invert
  | Sexp_op_negate
  | Sexp_op_float_negate

and pointer =
  { sptr_space : identifier option
  ; sptr_expr : expr
  }

and sized =
  { ssz_expr : expr
  ; ssz_size : integer
  }

and range =
  { srng_id : identifier
  ; srng_start_bit : integer
  ; srng_width : integer
  }

and display =
  { sdsp_mnemonic : pieces
  ; sdsp_output : pieces
  }

and pieces = piece list

and piece =
  | Spc_id of identifier
  | Spc_text of identifier
  | Spc_caret
  | Spc_whitespace

and token_field =
  { stf_id : identifier
  ; stf_start_bit : integer
  ; stf_end_bit : integer
  ; stf_mods : token_field_mod list
  }

and token_field_mod =
  | Stfm_signed of bool
  | Stfm_hex of bool

and statement =
  | Sstm_assign of expr * expr
  | Sstm_declare of declare
  | Sstm_fun_call of identifier * args
  | Sstm_build of identifier
  | Sstm_goto of jump_target
  | Sstm_call of jump_target
  | Sstm_return of jump_target
  | Sstm_branch of branch
  | Sstm_label of identifier
  | Sstm_export of expr

and declare =
  { sdecl_id : identifier
  ; sdecl_size : integer option
  }

and branch =
  { sbr_condition : expr
  ; sbr_target : jump_target
  }

and jump_target =
  | Sjmp_fixed of integer * identifier option
  | Sjmp_direct of identifier
  | Sjmp_indirect of expr
  | Sjmp_relative of integer * identifier
  | Sjmp_label of identifier
[@@deriving sexp]

and t = program
