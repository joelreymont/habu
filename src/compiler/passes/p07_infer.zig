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
    // Create typing context
    var ctx = TypingCtx.init(std.heap.page_allocator);
    defer ctx.deinit();

    // Create type checker
    var checker = BiChecker.init(std.heap.page_allocator);
    defer checker.deinit();

    // Infer type for the underlying IR
    const inferred_ty = checker.infer(input.ir, &ctx) catch {
        // Type error - return unchanged on error
        return PassResult(*const TypedIr).unchanged(input);
    };

    // If type changed, create new TypedIr with populated type
    if (input.ty == null or input.ty.? != inferred_ty) {
        const new_typed = try allocator.create(TypedIr);
        new_typed.* = input.withType(inferred_ty);
        return PassResult(*const TypedIr).changed(new_typed);
    }

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
    defer if (result.modified) testing.allocator.destroy(@constCast(result.output));

    try testing.expect(result.modified);
    try testing.expect(result.output.ty != null);
}
