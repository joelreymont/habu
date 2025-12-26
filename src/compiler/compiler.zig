//! Habu compiler module
//!
//! Exports all compiler components.

pub const ir = @import("ir.zig");
pub const Ir = ir.Ir;
pub const IrBuilder = ir.IrBuilder;

pub const compile = @import("compile.zig");
pub const Compiler = compile.Compiler;
pub const Env = compile.Env;
pub const CompileError = compile.CompileError;
pub const TypedIr = compile.TypedIr;
pub const GlobalEnv = compile.GlobalEnv;

test {
    _ = ir;
    _ = compile;
}
