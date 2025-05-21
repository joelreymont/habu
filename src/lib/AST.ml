open Position

type annotation = [ `Location of int * int ]

type endian =
  | Big
  | Little

type space_type =
  | Rom
  | Ram
  | Register

type integer = int located
type identifier = string located

type program = definition list

and definition =
  | Endian of endian located
  | Alignment of integer
  | Space of space
  | VarNode of varnode
  | VarNodeAttach of varnode_attach
  | Token of tok
  | PCodeOp of identifier
  | Constructor of constructor

and space =
  { id : identifier
  ; mods : space_mod list
  }

and space_mod =
  | Size of integer
  | WordSize of integer
  | Default of bool
  | Type of space_type located

and varnode =
  { mods : varnode_mod list
  ; registers : identifier list
  }

and varnode_mod =
  | Size of integer
  | Offset of integer

and varnode_attach =
  { nodes : identifier list
  ; values : identifier list
  }

and tok =
  { id : identifier
  ; bit_size : integer
  ; fields : token_field list
  }

and constructor =
  { id : identifier option
  ; display : display
  ; pattern : expr option
  ; context : statement list
  ; body : statement list
  }

and macro =
  { id : identifier
  ; args : identifier list
  ; body : statement list
  }

and expr =
  | Binary of binary_op * expr * expr
  | Unary of unary_op * expr
  | Paren of expr
  | FunCall of identifier * args
  | Id of identifier
  | Int of integer
  | BitRange of range
  | Pointer of pointer
  | Sized of sized

and args = expr list

and binary_op =
  | OpJoin
  | OpBoolOr
  | OpBoolAnd
  | OpBoolXor
  | OpOr
  | OpAnd
  | OpXor
  | OpEq
  | OpNe
  | OpFloatEq
  | OpFloatNe
  | OpGt
  | OpLt
  | OpGe
  | OpLe
  | OpSignedGt
  | OpSignedLt
  | OpSignedGe
  | OpSignedLe
  | OpFloatGt
  | OpFloatLt
  | OpFloatGe
  | OpFloatLe
  | OpShiftLeft
  | OpShiftRight
  | OpSignedShiftLeft
  | OpSignedShiftRight
  | OpPlus
  | OpMinus
  | OpFloatPlus
  | OpFloatMinus
  | OpMul
  | OpDiv
  | OpMod
  | OpSignedDiv
  | OpSignedMod
  | OpFloatMul
  | OpFloatDiv

and unary_op =
  | OpAlignLeft
  | OpAlignRight
  | OpBang
  | OpTilde
  | OpNeg
  | OpFloatNeg

and pointer =
  { space : identifier option
  ; expr : expr
  }

and sized =
  { expr : expr
  ; size : integer
  }

and range =
  { id : identifier
  ; start_bit : integer
  ; width : integer
  }

and display =
  { mnemonic : piece list
  ; output : piece list
  }

and piece =
  | Id of identifier
  | Text of identifier
  | Caret

and token_field =
  { id : identifier
  ; start_bit : integer
  ; end_bit : integer
  ; mods : token_field_mod list
  }

and token_field_mod =
  | Signed of bool
  | Hex of bool

and statement =
  | Assign of expr * expr
  | Declare of declare
  | FunCall of identifier * args
  | Build of identifier
  | Goto of jump_target
  | Call of jump_target
  | Return of jump_target
  | Branch of branch
  | Label of identifier
  | Export of expr

and declare =
  { id : identifier
  ; size : integer option
  }

and branch =
  { condition : expr
  ; target : jump_target
  }

and jump_target =
  | FixedJump of integer * identifier option
  | DirectJump of identifier
  | IndirectJump of expr
  | RelativeJump of integer * identifier
  | LabelJump of identifier

and t = program
