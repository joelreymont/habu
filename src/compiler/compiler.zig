//! Habu compiler module
//!
//! Exports all compiler components.

pub const ir = @import("ir.zig");
pub const Ir = ir.Ir;
pub const IrBuilder = ir.IrBuilder;

pub const compile = @import("compile.zig");
pub const Compiler = compile.Compiler;
pub const Env = compile.Env;
pub const Error = compile.Error;
pub const TypedIr = compile.TypedIr;
pub const GlobalEnv = compile.GlobalEnv;

pub const passes = @import("passes/passes.zig");
pub const Pass = passes.Pass;
pub const Pipeline = passes.Pipeline;
pub const PipelineConfig = passes.PipelineConfig;
pub const TypeCheckPass = passes.TypeCheckPass;
pub const ErasurePass = passes.ErasurePass;

test {
    _ = ir;
    _ = compile;
    _ = passes;
}
