//! Habu Type System
//!
//! Gradual typing with dependent types (QTT):
//! - Type ADT (primitive, union, arrow, list, vector, any)
//! - Dependent types: Pi (functions), Sigma (pairs), Refinements
//! - Term ADT for type-level computation
//! - Contract checking at typed/untyped boundaries
//! - Blame tracking for contract violations
//! - Occurrence typing (type narrows after predicates)

pub const type_adt = @import("type.zig");
pub const term = @import("term.zig");
pub const normalize = @import("normalize.zig");
pub const conversion = @import("conversion.zig");
pub const bicheck = @import("bicheck.zig");
pub const contract = @import("contract.zig");
pub const blame = @import("blame.zig");
pub const check = @import("check.zig");
pub const infer = @import("infer.zig");
pub const smt = @import("smt.zig");
pub const refinement = @import("refinement.zig");
pub const erasure = @import("erasure.zig");

// Re-export commonly used types
pub const Type = type_adt.Type;
pub const Primitive = type_adt.Primitive;
pub const TypeBuilder = type_adt.TypeBuilder;
pub const StructField = type_adt.StructField;
pub const StructType = type_adt.StructType;
pub const Quantity = type_adt.Quantity;

// Term ADT for type-level computation
pub const Term = term.Term;
pub const TermBuilder = term.TermBuilder;
pub const BinOp = term.BinOp;
pub const UnOp = term.UnOp;
pub const CmpOp = term.CmpOp;
pub const Builtin = term.Builtin;

// Normalizer for type-level computation
pub const Normalizer = normalize.Normalizer;
pub const NormEnv = normalize.NormEnv;
pub const EnvValue = normalize.EnvValue;
pub const NormError = normalize.NormError;

// Type conversion checking
pub const TypeConverter = conversion.TypeConverter;
pub const ConversionError = conversion.ConversionError;

// Bidirectional type checker
pub const BiChecker = bicheck.BiChecker;
pub const TypingCtx = bicheck.TypingCtx;
pub const CheckError = bicheck.CheckError;

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

pub const InferCtx = infer.InferCtx;
pub const TypeVar = infer.TypeVar;
pub const InferType = infer.InferType;
pub const TypeScheme = infer.TypeScheme;
pub const Constraint = infer.Constraint;

// SMT solver for refinement types
pub const SmtContext = smt.SmtContext;
pub const RefineResult = smt.RefineResult;

// Refinement type checker
pub const RefinementChecker = refinement.RefinementChecker;

// Erasure pass for QTT
pub const ErasureCtx = erasure.ErasureCtx;
pub const Eraser = erasure.Eraser;
pub const eraseZeroQuantity = erasure.eraseZeroQuantity;

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
pub const t_hashtable = type_adt.t_hashtable;
pub const t_rational = type_adt.t_rational;
pub const t_complex = type_adt.t_complex;
pub const t_stream = type_adt.t_stream;
pub const t_bignum = type_adt.t_bignum;
pub const t_array = type_adt.t_array;
pub const t_pathname = type_adt.t_pathname;
pub const t_any = type_adt.t_any;
pub const t_list_any = type_adt.t_list_any;
pub const t_non_nil = type_adt.t_non_nil;

// Universe levels
pub const t_type = type_adt.t_type;
pub const t_type1 = type_adt.t_type1;
pub const t_type2 = type_adt.t_type2;

test {
    _ = type_adt;
    _ = term;
    _ = normalize;
    _ = conversion;
    _ = bicheck;
    _ = contract;
    _ = blame;
    _ = check;
    _ = infer;
    _ = smt;
    _ = refinement;
    _ = erasure;
}
