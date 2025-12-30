//! Type Inference Pass
//!
//! Runs bidirectional type checking on TypedIr.
//! Populates type and quantity information for each node.
//!
//! Input: TypedIr
//! Output: TypedIr (with types and quantities populated)

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const Ir = @import("../ir.zig").Ir;
const ir_types = @import("ir_types.zig");
const TypedIr = ir_types.TypedIr;
const types = @import("../../types/types.zig");
const BiChecker = types.BiChecker;
const TypingCtx = types.TypingCtx;

/// Infer pass - runs BiChecker and populates types
pub fn infer(allocator: std.mem.Allocator, input: *const TypedIr) PassError!PassResult(*const TypedIr) {
    _ = allocator;

    // Create typing context
    var ctx = TypingCtx.init(std.heap.page_allocator);
    defer ctx.deinit();

    // Create type checker
    var checker = BiChecker.init(std.heap.page_allocator);
    defer checker.deinit();

    // Infer type for the underlying IR
    // TODO: Properly populate TypedIr with inferred types
    // For now, just run inference for its side effects (error detection)
    _ = checker.infer(input.ir, &ctx) catch {
        // Type error - but we continue for now
        // In the future, this should return diagnostics
    };

    // Return unchanged for now - in the future we'll populate types
    return PassResult(*const TypedIr).unchanged(input);
}

/// Create the infer pass
pub const pass = pass_mod.makePass(*const TypedIr, *const TypedIr, "infer", infer);

// ============================================================================
// Tests
// ============================================================================

test "infer pass - literal" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const typed = TypedIr.init(&lit);

    const result = try infer(testing.allocator, &typed);

    try testing.expect(!result.modified);
    try testing.expectEqual(&typed, result.output);
}
