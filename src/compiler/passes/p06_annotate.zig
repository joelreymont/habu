//! Annotation Pass
//!
//! Wraps plain IR nodes with type/quantity slots.
//! This is the first step in the type pipeline.
//!
//! Input: Ir
//! Output: TypedIr (with null types and default quantities)

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const Ir = @import("../ir.zig").Ir;
const ir_types = @import("ir_types.zig");
const TypedIr = ir_types.TypedIr;
const TypedIrBuilder = ir_types.TypedIrBuilder;

/// Annotate pass - wraps IR with type/quantity slots
pub fn annotate(allocator: std.mem.Allocator, input: *const Ir) !PassResult(*const TypedIr) {
    var builder = TypedIrBuilder.init(allocator);
    const typed = try builder.buildTree(input);
    return PassResult(*const TypedIr).changed(typed);
}

/// Create the annotate pass
pub const pass = pass_mod.makePass(*const Ir, *const TypedIr, "annotate", annotate);

// ============================================================================
// Tests
// ============================================================================

test "annotate pass - literal" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const result = try annotate(testing.allocator, &lit);

    try testing.expect(result.modified);
    try testing.expectEqual(&lit, result.output.ir);
    try testing.expect(result.output.ty == null);
    try testing.expectEqual(ir_types.Quantity.many, result.output.quantity);

    // Clean up
    testing.allocator.destroy(@constCast(result.output));
}

test "annotate pass - addition" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const add = Ir{ .add = .{ .left = &lit1, .right = &lit2 } };

    const result = try annotate(testing.allocator, &add);

    try testing.expect(result.modified);
    try testing.expectEqual(&add, result.output.ir);
    try testing.expectEqual(@as(usize, 2), result.output.children.len);

    // Clean up children and node
    for (result.output.children) |child| {
        testing.allocator.destroy(@constCast(child));
    }
    testing.allocator.free(result.output.children);
    testing.allocator.destroy(@constCast(result.output));
}
