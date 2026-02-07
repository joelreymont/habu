//! Constraint Propagation Pass
//!
//! Flow-sensitive constraint propagation that eliminates redundant type checks.
//! Runs after type inference (p07) and before type erasure (p08).
//!
//! At each `if` node with a type-predicate condition (consp, symbolp, etc.),
//! narrows the variable's type in the then-branch and adds negative constraints
//! in the else-branch. When an assert_* node's operand already has a proven
//! type, the assertion is replaced with an identity (the operand itself).
//!
//! Input: TypedIr (with types populated)
//! Output: TypedIr (with redundant assert_* nodes eliminated)

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const ir_mod = @import("../ir.zig");
const Ir = ir_mod.Ir;
const ir_types = @import("ir_types.zig");
const TypedIr = ir_types.TypedIr;
const type_adt = @import("../../types/type.zig");
const Type = type_adt.Type;
const constraint_mod = @import("constraint.zig");
const Constraint = constraint_mod.Constraint;
const ConstraintSet = constraint_mod.ConstraintSet;

/// Predicate-to-type mapping entry
const PredEntry = struct {
    tag: std.meta.Tag(Ir),
    ty: *const Type,
};

/// Table mapping type predicates to the types they prove
const pred_table = [_]PredEntry{
    .{ .tag = .consp, .ty = &type_adt.t_cons },
    .{ .tag = .symbolp, .ty = &type_adt.t_symbol },
    .{ .tag = .numberp, .ty = &type_adt.t_fixnum },
    .{ .tag = .integerp, .ty = &type_adt.t_fixnum },
    .{ .tag = .stringp, .ty = &type_adt.t_string },
    .{ .tag = .vectorp, .ty = &type_adt.t_vector },
    .{ .tag = .closurep, .ty = &type_adt.t_closure },
    .{ .tag = .keywordp, .ty = &type_adt.t_keyword },
    .{ .tag = .nilp, .ty = &type_adt.t_nil },
    .{ .tag = .characterp, .ty = &type_adt.t_char },
    .{ .tag = .floatp, .ty = &type_adt.t_float },
    .{ .tag = .listp, .ty = &type_adt.t_cons },
    .{ .tag = .rationalp, .ty = &type_adt.t_rational },
    .{ .tag = .complexp, .ty = &type_adt.t_complex },
};

/// Assert-to-type mapping entry
const AssertEntry = struct {
    tag: std.meta.Tag(Ir),
    ty: *const Type,
};

/// Table mapping assert_* nodes to the types they check
const assert_table = [_]AssertEntry{
    .{ .tag = .assert_fixnum, .ty = &type_adt.t_fixnum },
    .{ .tag = .assert_cons, .ty = &type_adt.t_cons },
    .{ .tag = .assert_symbol, .ty = &type_adt.t_symbol },
    .{ .tag = .assert_string, .ty = &type_adt.t_string },
    .{ .tag = .assert_vector, .ty = &type_adt.t_vector },
    .{ .tag = .assert_closure, .ty = &type_adt.t_closure },
    .{ .tag = .assert_non_nil, .ty = &type_adt.t_non_nil },
    .{ .tag = .assert_list, .ty = &type_adt.t_cons },
};

/// Result of constraint propagation on an IR tree
const PropResult = struct {
    ir: *const Ir,
    changed: bool,
};

/// Tags that are type predicates (UnaryOp)
const predicate_tags = .{
    .consp,     .symbolp,    .numberp,   .integerp, .realp,
    .stringp,   .vectorp,    .closurep,  .keywordp, .nilp,
    .characterp,.floatp,     .listp,     .atom,     .rationalp,
    .complexp,
};

/// Tags that are type assertions (UnaryOp)
const assert_tags = .{
    .assert_fixnum, .assert_cons,    .assert_symbol, .assert_string,
    .assert_vector, .assert_closure, .assert_non_nil,.assert_list,
};

/// Tags that are other unary ops we want to recurse into
const other_unary_tags = .{
    .car, .cdr, .not, .length,
};

/// Get operand pointer from a unary IR node.
fn getOperand(ir: *const Ir) ?*const Ir {
    return switch (ir.*) {
        inline .consp, .symbolp, .numberp, .integerp, .realp, .stringp, .vectorp, .closurep, .keywordp, .nilp, .characterp, .floatp, .listp, .atom, .rationalp, .complexp, .assert_fixnum, .assert_cons, .assert_symbol, .assert_string, .assert_vector, .assert_closure, .assert_non_nil, .assert_list, .car, .cdr, .not, .length, .type_of, .sym_name, .print, .hashtablep, .packagep, .symbol_package, .package_name, .find_package, .streamp, .input_stream_p, .output_stream_p, .make_string_input_stream, .get_output_stream_string, .vec_len, .vec_fill_ptr, .copy_structure, .str_len, .string_upcase, .string_downcase, .real_part, .imag_part, .numerator, .denominator, .method_qualifiers, .method_specializers, .method_function, .generic_function_methods, .generic_function_lambda_list, .generic_function_name, .make_box, .box_ref, .package_nicknames, .package_use_list, .package_used_by_list, .package_shadowing_symbols, .delete_package, .function_lambda_expression => |op| op.operand,
        .define => |d| d.value,
        else => null,
    };
}

/// Propagate constraints through an IR tree, eliminating redundant type checks.
fn propagate(allocator: std.mem.Allocator, ir: *const Ir, cs: *ConstraintSet) PassError!PropResult {
    const ir_tag = std.meta.activeTag(ir.*);

    // Check assert_* nodes: if constraints prove the type, eliminate the check
    for (assert_table) |entry| {
        if (ir_tag == entry.tag) {
            const operand = getOperand(ir) orelse return .{ .ir = ir, .changed = false };
            if (getVarName(operand)) |name| {
                if (cs.provesType(name, entry.ty)) {
                    return .{ .ir = operand, .changed = true };
                }
            }
            const new_operand = try propagate(allocator, operand, cs);
            if (new_operand.changed) {
                return .{ .ir = try rebuildUnary(allocator, ir, new_operand.ir), .changed = true };
            }
            return .{ .ir = ir, .changed = false };
        }
    }

    switch (ir.*) {
        .@"if" => |i| {
            const new_cond = try propagate(allocator, i.cond, cs);
            const cond_ir = new_cond.ir;
            const cond_tag = std.meta.activeTag(cond_ir.*);

            var then_cs = try cs.clone();
            defer then_cs.deinit();
            var else_cs = try cs.clone();
            defer else_cs.deinit();

            for (pred_table) |entry| {
                if (cond_tag == entry.tag) {
                    if (getOperand(cond_ir)) |pred_operand| {
                        if (getVarName(pred_operand)) |name| {
                            try then_cs.add(name, .{ .type_is = entry.ty });
                            try else_cs.add(name, .{ .type_not = entry.ty });
                        }
                    }
                    break;
                }
            }

            // Comparisons with constants → range constraints
            switch (cond_tag) {
                .lt, .le, .gt, .ge, .num_eq => {
                    extractComparisonConstraint(cond_ir, &then_cs, &else_cs) catch {};
                },
                else => {},
            }

            const new_then = try propagate(allocator, i.then_branch, &then_cs);
            const new_else = try propagate(allocator, i.else_branch, &else_cs);

            if (new_cond.changed or new_then.changed or new_else.changed) {
                const new_ir = try allocator.create(Ir);
                new_ir.* = .{ .@"if" = .{
                    .cond = new_cond.ir,
                    .then_branch = new_then.ir,
                    .else_branch = new_else.ir,
                } };
                return .{ .ir = new_ir, .changed = true };
            }
            return .{ .ir = ir, .changed = false };
        },

        .let => |l| {
            var changed = false;
            var new_bindings = try allocator.alloc(Ir.Binding, l.bindings.len);

            for (l.bindings, 0..) |b, idx| {
                const new_val = try propagate(allocator, b.value, cs);
                new_bindings[idx] = .{ .name = b.name, .value = new_val.ir, .index = b.index };
                if (new_val.changed) changed = true;

                if (b.value.* == .lit) {
                    try cs.add(b.name, .{ .eql = b.value.lit });
                }
            }

            const new_body = try propagate(allocator, l.body, cs);
            if (new_body.changed) changed = true;

            if (!changed) {
                allocator.free(new_bindings);
                return .{ .ir = ir, .changed = false };
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .let = .{ .bindings = new_bindings, .body = new_body.ir } };
            return .{ .ir = new_ir, .changed = true };
        },

        .progn => |exprs| {
            var changed = false;
            var new_exprs = try allocator.alloc(*const Ir, exprs.len);
            for (exprs, 0..) |e, idx| {
                const r = try propagate(allocator, e, cs);
                new_exprs[idx] = r.ir;
                if (r.changed) changed = true;
            }
            if (!changed) {
                allocator.free(new_exprs);
                return .{ .ir = ir, .changed = false };
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .progn = new_exprs };
            return .{ .ir = new_ir, .changed = true };
        },

        .call => |c| {
            var changed = false;
            const new_func = try propagate(allocator, c.func, cs);
            if (new_func.changed) changed = true;
            var new_args = try allocator.alloc(*const Ir, c.args.len);
            for (c.args, 0..) |arg, idx| {
                const r = try propagate(allocator, arg, cs);
                new_args[idx] = r.ir;
                if (r.changed) changed = true;
            }
            if (!changed) {
                allocator.free(new_args);
                return .{ .ir = ir, .changed = false };
            }
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .call = .{ .func = new_func.ir, .args = new_args } };
            return .{ .ir = new_ir, .changed = true };
        },

        .add, .sub, .mul, .div, .mod => |binop| {
            return try propagateBinop(allocator, ir, binop, cs);
        },

        .eq, .lt, .gt, .le, .ge, .num_eq => |cmp| {
            return try propagateBinop(allocator, ir, cmp, cs);
        },

        .lambda => |l| {
            var inner_cs = ConstraintSet.init(cs.allocator);
            defer inner_cs.deinit();
            const new_body = try propagate(allocator, l.body, &inner_cs);
            if (!new_body.changed) return .{ .ir = ir, .changed = false };
            const new_ir = try allocator.create(Ir);
            var new_lambda = l;
            new_lambda.body = new_body.ir;
            new_ir.* = .{ .lambda = new_lambda };
            return .{ .ir = new_ir, .changed = true };
        },

        .set => |s| {
            const new_val = try propagate(allocator, s.value, cs);
            if (!new_val.changed) return .{ .ir = ir, .changed = false };
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .set = .{
                .name = s.name,
                .value = new_val.ir,
                .depth = s.depth,
                .index = s.index,
            } };
            return .{ .ir = new_ir, .changed = true };
        },

        .block => |b| {
            const new_body = try propagate(allocator, b.body, cs);
            if (!new_body.changed) return .{ .ir = ir, .changed = false };
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .block = .{ .name = b.name, .body = new_body.ir } };
            return .{ .ir = new_ir, .changed = true };
        },

        .return_from => |r| {
            const new_val = try propagate(allocator, r.value, cs);
            if (!new_val.changed) return .{ .ir = ir, .changed = false };
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .return_from = .{ .name = r.name, .value = new_val.ir } };
            return .{ .ir = new_ir, .changed = true };
        },

        .loop => |l| {
            const new_cond = try propagate(allocator, l.cond, cs);
            const new_body = try propagate(allocator, l.body, cs);
            if (!new_cond.changed and !new_body.changed) return .{ .ir = ir, .changed = false };
            const new_ir = try allocator.create(Ir);
            new_ir.* = .{ .loop = .{ .cond = new_cond.ir, .body = new_body.ir } };
            return .{ .ir = new_ir, .changed = true };
        },

        .define => |d| {
            const new_val = try propagate(allocator, d.value, cs);
            if (!new_val.changed) return .{ .ir = ir, .changed = false };
            const new_ir = try allocator.create(Ir);
            var new_def = d;
            new_def.value = new_val.ir;
            new_ir.* = .{ .define = new_def };
            return .{ .ir = new_ir, .changed = true };
        },

        // Leaf nodes
        .lit, .quote_sym, .@"var", .global_ref, .go, .quote => {
            return .{ .ir = ir, .changed = false };
        },

        else => {
            // For unary ops, try to recurse into operand
            if (getOperand(ir)) |op_operand| {
                const new_operand = try propagate(allocator, op_operand, cs);
                if (!new_operand.changed) return .{ .ir = ir, .changed = false };
                return .{ .ir = try rebuildUnary(allocator, ir, new_operand.ir), .changed = true };
            }
            // Conservative: return unchanged
            return .{ .ir = ir, .changed = false };
        },
    }
}

/// Propagate through a binary operation.
fn propagateBinop(allocator: std.mem.Allocator, ir: *const Ir, binop: Ir.BinaryOp, cs: *ConstraintSet) PassError!PropResult {
    const new_left = try propagate(allocator, binop.left, cs);
    const new_right = try propagate(allocator, binop.right, cs);
    if (!new_left.changed and !new_right.changed) {
        return .{ .ir = ir, .changed = false };
    }
    return .{ .ir = try rebuildBinop(allocator, ir, new_left.ir, new_right.ir), .changed = true };
}

/// Rebuild a unary op with a new operand, preserving the tag.
fn rebuildUnary(allocator: std.mem.Allocator, orig: *const Ir, new_operand: *const Ir) !*const Ir {
    const new_ir = try allocator.create(Ir);
    new_ir.* = orig.*;
    // Use inline switch to set the operand on the correct variant
    switch (new_ir.*) {
        inline .consp, .symbolp, .numberp, .integerp, .realp, .stringp, .vectorp, .closurep, .keywordp, .nilp, .characterp, .floatp, .listp, .atom, .rationalp, .complexp, .assert_fixnum, .assert_cons, .assert_symbol, .assert_string, .assert_vector, .assert_closure, .assert_non_nil, .assert_list, .car, .cdr, .not, .length, .type_of, .sym_name, .print, .hashtablep, .packagep, .symbol_package, .package_name, .find_package, .streamp, .input_stream_p, .output_stream_p, .make_string_input_stream, .get_output_stream_string, .vec_len, .vec_fill_ptr, .copy_structure, .str_len, .string_upcase, .string_downcase, .real_part, .imag_part, .numerator, .denominator, .method_qualifiers, .method_specializers, .method_function, .generic_function_methods, .generic_function_lambda_list, .generic_function_name, .make_box, .box_ref, .package_nicknames, .package_use_list, .package_used_by_list, .package_shadowing_symbols, .delete_package, .function_lambda_expression => |*op| {
            op.operand = new_operand;
        },
        .define => |*d| {
            d.value = new_operand;
        },
        else => unreachable,
    }
    return new_ir;
}

/// Rebuild a binary op with new operands, preserving the tag.
fn rebuildBinop(allocator: std.mem.Allocator, orig: *const Ir, new_left: *const Ir, new_right: *const Ir) !*const Ir {
    const new_ir = try allocator.create(Ir);
    new_ir.* = orig.*;
    switch (new_ir.*) {
        inline .add, .sub, .mul, .div, .mod, .eq, .lt, .gt, .le, .ge, .num_eq => |*binop| {
            binop.left = new_left;
            binop.right = new_right;
        },
        else => unreachable,
    }
    return new_ir;
}

/// Extract comparison constraints from a comparison node.
fn extractComparisonConstraint(ir: *const Ir, then_cs: *ConstraintSet, else_cs: *ConstraintSet) !void {
    const cmp = switch (ir.*) {
        .lt, .le, .gt, .ge, .num_eq => |c| c,
        else => return,
    };

    const var_name = getVarName(cmp.left);
    const const_val = getConstInt(cmp.right);

    if (var_name != null and const_val != null) {
        const name = var_name.?;
        const val = const_val.?;

        switch (std.meta.activeTag(ir.*)) {
            .lt => {
                try then_cs.add(name, .{ .range = .{ .lo = null, .hi = val - 1 } });
                try else_cs.add(name, .{ .range = .{ .lo = val, .hi = null } });
            },
            .le => {
                try then_cs.add(name, .{ .range = .{ .lo = null, .hi = val } });
                try else_cs.add(name, .{ .range = .{ .lo = val + 1, .hi = null } });
            },
            .gt => {
                try then_cs.add(name, .{ .range = .{ .lo = val + 1, .hi = null } });
                try else_cs.add(name, .{ .range = .{ .lo = null, .hi = val } });
            },
            .ge => {
                try then_cs.add(name, .{ .range = .{ .lo = val, .hi = null } });
                try else_cs.add(name, .{ .range = .{ .lo = null, .hi = val - 1 } });
            },
            .num_eq => {
                try then_cs.add(name, .{ .range = .{ .lo = val, .hi = val } });
            },
            else => {},
        }
    }
}

/// Get the variable name from an IR node if it's a variable reference.
fn getVarName(ir: *const Ir) ?[]const u8 {
    return switch (ir.*) {
        .@"var" => |v| v.name,
        else => null,
    };
}

/// Get a constant integer value from an IR literal.
fn getConstInt(ir: *const Ir) ?i64 {
    return switch (ir.*) {
        .lit => |v| if (v.isFixnum()) v.toFixnum() else null,
        else => null,
    };
}

/// Top-level constraint propagation pass.
pub fn constrain(allocator: std.mem.Allocator, input: *const TypedIr) PassError!PassResult(*const TypedIr) {
    var cs = ConstraintSet.init(allocator);
    defer cs.deinit();

    const result = try propagate(allocator, input.ir, &cs);

    if (result.changed) {
        const new_typed = try allocator.create(TypedIr);
        new_typed.* = .{
            .ir = result.ir,
            .ty = input.ty,
            .quantity = input.quantity,
            .children = input.children,
        };
        return PassResult(*const TypedIr).changed(new_typed);
    }

    return PassResult(*const TypedIr).unchanged(input);
}

/// Pass registration
pub const pass = pass_mod.makePass(*const TypedIr, *const TypedIr, "constrain", constrain);

// ============================================================================
// Tests
// ============================================================================

const Value = @import("../../runtime/value.zig").Value;
const IrBuilder = ir_mod.IrBuilder;

test "constrain eliminates redundant assert after predicate" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);

    // (if (consp x) (assert_cons x) nil)
    const x = try builder.variable("x", 0, 0);
    const cond = try builder.consp(x);
    const x2 = try builder.variable("x", 0, 0);
    const assert_node = try builder.assertCons(x2);
    const else_node = try builder.lit(Value.nil);
    const if_node = try builder.ifExpr(cond, assert_node, else_node);

    const typed = TypedIr.init(if_node);
    const result = try constrain(testing.allocator, &typed);

    try testing.expect(result.modified);
    const out_ir = result.output.ir;
    try testing.expectEqual(std.meta.Tag(Ir).@"if", std.meta.activeTag(out_ir.*));
    const then_ir = out_ir.@"if".then_branch;
    try testing.expectEqual(std.meta.Tag(Ir).@"var", std.meta.activeTag(then_ir.*));

    if (result.modified) testing.allocator.destroy(@constCast(result.output));
}

test "constrain preserves non-redundant assert" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);

    const x = try builder.variable("x", 0, 0);
    const assert_node = try builder.assertCons(x);

    const typed = TypedIr.init(assert_node);
    const result = try constrain(testing.allocator, &typed);

    try testing.expect(!result.modified);
}

test "constrain handles nested if" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);

    // (if (consp x) (if (symbolp y) (assert_cons x) nil) nil)
    const x = try builder.variable("x", 0, 0);
    const cond1 = try builder.consp(x);
    const y = try builder.variable("y", 0, 1);
    const cond2 = try builder.symbolp(y);
    const x2 = try builder.variable("x", 0, 0);
    const inner_assert = try builder.assertCons(x2);
    const nil1 = try builder.lit(Value.nil);
    const inner_if = try builder.ifExpr(cond2, inner_assert, nil1);
    const nil2 = try builder.lit(Value.nil);
    const outer_if = try builder.ifExpr(cond1, inner_if, nil2);

    const typed = TypedIr.init(outer_if);
    const result = try constrain(testing.allocator, &typed);

    try testing.expect(result.modified);
    const out = result.output.ir;
    const then_branch = out.@"if".then_branch;
    const inner_then = then_branch.@"if".then_branch;
    try testing.expectEqual(std.meta.Tag(Ir).@"var", std.meta.activeTag(inner_then.*));

    if (result.modified) testing.allocator.destroy(@constCast(result.output));
}

test "constrain comparison adds range" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);

    // (if (< x 10) (assert_fixnum x) nil)
    const x = try builder.variable("x", 0, 0);
    const ten = try builder.lit(Value.makeFixnum(10));
    const cond = try builder.lt(x, ten);
    const x2 = try builder.variable("x", 0, 0);
    const assert_node = try builder.assertFixnum(x2);
    const nil_node = try builder.lit(Value.nil);
    const if_node = try builder.ifExpr(cond, assert_node, nil_node);

    const typed = TypedIr.init(if_node);
    const result = try constrain(testing.allocator, &typed);

    try testing.expect(result.modified);
    const then_ir = result.output.ir.@"if".then_branch;
    try testing.expectEqual(std.meta.Tag(Ir).@"var", std.meta.activeTag(then_ir.*));

    if (result.modified) testing.allocator.destroy(@constCast(result.output));
}
