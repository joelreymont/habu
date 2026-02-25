//! Erasure Pass
//!
//! Removes zero-quantity (type-level only) terms from IR.
//! In QTT, 0-quantity variables exist only for type computation.
//!
//! Input: TypedIr
//! Output: Ir (with erased terms replaced by nil)

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const Ir = @import("../ir.zig").Ir;
const ir_types = @import("ir_types.zig");
const TypedIr = ir_types.TypedIr;
const Quantity = ir_types.Quantity;
const Value = @import("../../runtime/value.zig").Value;

/// Erase pass - removes zero-quantity nodes
pub fn erase(allocator: std.mem.Allocator, input: *const TypedIr) PassError!PassResult(*const Ir) {
    // Walk TypedIr tree, collect erased variable names
    var erased_set = std.StringHashMap(void).init(allocator);
    defer erased_set.deinit();

    try collectErasedVars(input, &erased_set);

    // If nothing to erase, return original IR
    if (erased_set.count() == 0) {
        return PassResult(*const Ir).unchanged(input.ir);
    }

    // Transform IR, replacing erased vars with nil
    const result = try eraseIr(allocator, input.ir, &erased_set);

    if (result == input.ir) {
        return PassResult(*const Ir).unchanged(input.ir);
    } else {
        return PassResult(*const Ir).changed(result);
    }
}

/// Collect variable names that should be erased
fn collectErasedVars(typed: *const TypedIr, erased_set: *std.StringHashMap(void)) !void {
    // Check if this node is a variable binding with zero quantity
    if (typed.quantity == .zero) {
        // If it's a variable reference, mark it
        if (typed.ir.* == .@"var") {
            try erased_set.put(typed.ir.@"var".name, {});
        }
        // If it's a let binding, mark bound vars
        if (typed.ir.* == .let) {
            for (typed.ir.let.bindings) |b| {
                try erased_set.put(b.name, {});
            }
        }
        // If it's a lambda, mark parameters
        if (typed.ir.* == .lambda) {
            for (typed.ir.lambda.params) |param_name| {
                try erased_set.put(param_name, {});
            }
        }
    }

    // Recurse into children
    for (typed.children) |child| {
        try collectErasedVars(child, erased_set);
    }
}

/// Transform IR, replacing erased variable references with nil
fn eraseIr(allocator: std.mem.Allocator, ir: *const Ir, erased_set: *const std.StringHashMap(void)) !*const Ir {
    switch (ir.*) {
        .@"var" => |v| {
            if (erased_set.contains(v.name)) {
                // Replace with nil literal
                const nil_ir = try allocator.create(Ir);
                nil_ir.* = .{ .lit = Value.nil };
                return nil_ir;
            }
            return ir;
        },
        .lit, .quote_sym, .global_ref, .go => return ir,
        .add, .sub, .mul, .div, .mod => |binop| {
            const new_left = try eraseIr(allocator, binop.left, erased_set);
            const new_right = try eraseIr(allocator, binop.right, erased_set);
            if (new_left == binop.left and new_right == binop.right) {
                return ir;
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = ir.*;
            switch (new_ir.*) {
                .add => |*b| {
                    b.left = new_left;
                    b.right = new_right;
                },
                .sub => |*b| {
                    b.left = new_left;
                    b.right = new_right;
                },
                .mul => |*b| {
                    b.left = new_left;
                    b.right = new_right;
                },
                .div => |*b| {
                    b.left = new_left;
                    b.right = new_right;
                },
                .mod => |*b| {
                    b.left = new_left;
                    b.right = new_right;
                },
                else => unreachable,
            }
            return new_ir;
        },
        .@"if" => |i| {
            const new_cond = try eraseIr(allocator, i.cond, erased_set);
            const new_then = try eraseIr(allocator, i.then_branch, erased_set);
            const new_else = try eraseIr(allocator, i.else_branch, erased_set);
            if (new_cond == i.cond and new_then == i.then_branch and new_else == i.else_branch) {
                return ir;
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .@"if" = .{
                .cond = new_cond,
                .then_branch = new_then,
                .else_branch = new_else,
            } };
            return new_ir;
        },
        .let => |l| {
            var changed = false;
            var new_bindings = try allocator.alloc(Ir.Binding, l.bindings.len);
            for (l.bindings, 0..) |b, i| {
                const new_val = try eraseIr(allocator, b.value, erased_set);
                new_bindings[i] = .{ .name = b.name, .value = new_val, .index = b.index };
                if (new_val != b.value) changed = true;
            }
            const new_body = try eraseIr(allocator, l.body, erased_set);
            if (new_body != l.body) changed = true;
            if (!changed) {
                allocator.free(new_bindings);
                return ir;
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .let = .{ .bindings = new_bindings, .body = new_body } };
            return new_ir;
        },
        .call => |c| {
            var changed = false;
            const new_func = try eraseIr(allocator, c.func, erased_set);
            if (new_func != c.func) changed = true;
            var new_args = try allocator.alloc(*const Ir, c.args.len);
            for (c.args, 0..) |arg, i| {
                new_args[i] = try eraseIr(allocator, arg, erased_set);
                if (new_args[i] != arg) changed = true;
            }
            if (!changed) {
                allocator.free(new_args);
                return ir;
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .call = .{ .func = new_func, .args = new_args } };
            return new_ir;
        },
        // Add more cases as needed - for now return unchanged
        else => return ir,
    }
}

/// Create the erase pass
pub const pass = pass_mod.makePass(*const TypedIr, *const Ir, "erase", erase);

// ============================================================================
// Tests
// ============================================================================

test "erase pass - no erased vars" {
    const testing = std.testing;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const typed = TypedIr.init(&lit);

    const result = try erase(testing.allocator, &typed);

    try testing.expect(!result.modified);
    try testing.expectEqual(&lit, result.output);
}

test "erase pass - erased variable" {
    const testing = std.testing;

    const var_ref = Ir{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };

    // Create TypedIr with zero quantity (erased)
    const typed = TypedIr{
        .ir = &var_ref,
        .ty = null,
        .quantity = .zero,
        .children = &.{},
    };

    const result = try erase(testing.allocator, &typed);

    try testing.expect(result.modified);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.output.*));
    try testing.expect(result.output.lit.isNil());

    // Clean up
    testing.allocator.destroy(@constCast(result.output));
}
