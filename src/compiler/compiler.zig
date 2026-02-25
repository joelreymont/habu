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
pub const IrPass = passes.IrPass;
pub const IrPipeline = passes.IrPipeline;
pub const PipelineResult = passes.PipelineResult;
pub const runPipeline = passes.runPipeline;
pub const createPipeline = passes.createPipeline;

test {
    _ = ir;
    _ = compile;
    _ = passes;
}
