//! Habu Type System
//!
//! Racket-style gradual typing with:
//! - Type ADT (primitive, union, arrow, list, vector, any)
//! - Contract checking at typed/untyped boundaries
//! - Blame tracking for contract violations
//! - Occurrence typing (type narrows after predicates)

pub const type_adt = @import("type.zig");
pub const contract = @import("contract.zig");
pub const blame = @import("blame.zig");
pub const check = @import("check.zig");

// Re-export commonly used types
pub const Type = type_adt.Type;
pub const Primitive = type_adt.Primitive;
pub const TypeBuilder = type_adt.TypeBuilder;

pub const Contract = contract.Contract;
pub const ContractCompiler = contract.ContractCompiler;
pub const predicates = contract.predicates;

pub const Blame = blame.Blame;
pub const BlameLabel = blame.BlameLabel;
pub const SourceLoc = blame.SourceLoc;
pub const ContractError = blame.ContractError;

pub const TypeEnv = check.TypeEnv;
pub const OccurrenceCtx = check.OccurrenceCtx;
pub const TypeChecker = check.TypeChecker;

// Common type constants
pub const t_fixnum = type_adt.t_fixnum;
pub const t_float = type_adt.t_float;
pub const t_cons = type_adt.t_cons;
pub const t_symbol = type_adt.t_symbol;
pub const t_string = type_adt.t_string;
pub const t_vector = type_adt.t_vector;
pub const t_closure = type_adt.t_closure;
pub const t_keyword = type_adt.t_keyword;
pub const t_nil = type_adt.t_nil;
pub const t_char = type_adt.t_char;
pub const t_any = type_adt.t_any;
pub const t_list_any = type_adt.t_list_any;
pub const t_non_nil = type_adt.t_non_nil;

test {
    _ = type_adt;
    _ = contract;
    _ = blame;
    _ = check;
}
