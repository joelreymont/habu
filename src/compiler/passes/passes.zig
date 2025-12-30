//! Compiler Passes
//!
//! Nanopass transformations for the Habu compiler.
//! Each pass does one thing and produces modified IR for subsequent passes.
//!
//! Pipeline:
//!   1. typecheck - Bidirectional type checking with QTT
//!   2. erasure - Remove 0-quantity terms
//!   (future: optimize, inline, etc.)

pub const pass = @import("pass.zig");
pub const pipeline = @import("pipeline.zig");
pub const typecheck = @import("p01_typecheck.zig");
pub const erasure = @import("p02_erasure.zig");

// Re-export commonly used types
pub const Pass = pass.Pass;
pub const PassError = pass.PassError;
pub const PassResult = pass.PassResult;
pub const PassDiagnostic = pass.PassDiagnostic;
pub const makePass = pass.makePass;
pub const IdentityPass = pass.IdentityPass;

pub const Pipeline = pipeline.Pipeline;
pub const PipelineConfig = pipeline.PipelineConfig;
pub const PipelineResult = pipeline.PipelineResult;
pub const PipelineStats = pipeline.PipelineStats;

pub const TypeCheckPass = typecheck.TypeCheckPass;
pub const TypeCheckConfig = typecheck.TypeCheckConfig;

pub const ErasurePass = erasure.ErasurePass;
pub const ErasureConfig = erasure.ErasureConfig;

/// Create a standard compilation pipeline with default passes
pub fn createStandardPipeline(allocator: std.mem.Allocator, config: PipelineConfig) !Pipeline {
    var p = Pipeline.init(allocator, config);

    // Add type checking pass
    var typecheck_pass = TypeCheckPass.init(allocator, .{});
    try p.addPass(makePass(TypeCheckPass, &typecheck_pass));

    // Add erasure pass
    var erasure_pass = ErasurePass.init(allocator, .{});
    try p.addPass(makePass(ErasurePass, &erasure_pass));

    return p;
}

const std = @import("std");

test {
    _ = pass;
    _ = pipeline;
    _ = typecheck;
    _ = erasure;
}
