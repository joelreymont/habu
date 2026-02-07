//! Specialization Pass
//!
//! Replaces generic IR nodes with specialized variants when types are
//! proven by preceding type assertions. This eliminates redundant
//! runtime type checks in the VM.
//!
//! Patterns recognized:
//!   (assert_fixnum x) in add/sub/mul operands → fixnum_add/sub/mul
//!   (assert_cons x) in car/cdr operand → unsafe_car/cdr
//!   (assert_vector x) in vec_ref operand → direct_aref
//!
//! Input: Ir (after erase)
//! Output: Ir (with specialized nodes)

const std = @import("std");
const Ir = @import("../ir.zig").Ir;

/// Check if an IR node is proven fixnum (asserted or literal fixnum).
fn isProvenFixnum(node: *const Ir) bool {
    return switch (node.*) {
        .assert_fixnum => true,
        .lit => |v| v.isFixnum(),
        else => false,
    };
}

/// Check if an IR node is proven cons (asserted).
fn isProvenCons(node: *const Ir) bool {
    return node.* == .assert_cons;
}

/// Check if an IR node is proven vector (asserted).
fn isProvenVector(node: *const Ir) bool {
    return node.* == .assert_vector;
}

/// Walk IR tree and replace generic ops with specialized variants.
pub fn specialize(allocator: std.mem.Allocator, node: *const Ir) !*const Ir {
    switch (node.*) {
        // Binary arithmetic: specialize when both operands proven fixnum
        .add => |op| {
            const left = try specialize(allocator, op.left);
            const right = try specialize(allocator, op.right);
            if (isProvenFixnum(left) and isProvenFixnum(right)) {
                const n = try allocator.create(Ir);
                n.* = .{ .fixnum_add = .{ .left = left, .right = right } };
                return n;
            }
            if (left != op.left or right != op.right) {
                const n = try allocator.create(Ir);
                n.* = .{ .add = .{ .left = left, .right = right } };
                return n;
            }
            return node;
        },
        .sub => |op| {
            const left = try specialize(allocator, op.left);
            const right = try specialize(allocator, op.right);
            if (isProvenFixnum(left) and isProvenFixnum(right)) {
                const n = try allocator.create(Ir);
                n.* = .{ .fixnum_sub = .{ .left = left, .right = right } };
                return n;
            }
            if (left != op.left or right != op.right) {
                const n = try allocator.create(Ir);
                n.* = .{ .sub = .{ .left = left, .right = right } };
                return n;
            }
            return node;
        },
        .mul => |op| {
            const left = try specialize(allocator, op.left);
            const right = try specialize(allocator, op.right);
            if (isProvenFixnum(left) and isProvenFixnum(right)) {
                const n = try allocator.create(Ir);
                n.* = .{ .fixnum_mul = .{ .left = left, .right = right } };
                return n;
            }
            if (left != op.left or right != op.right) {
                const n = try allocator.create(Ir);
                n.* = .{ .mul = .{ .left = left, .right = right } };
                return n;
            }
            return node;
        },

        // Car/cdr: specialize when operand proven cons
        .car => |op| {
            const operand = try specialize(allocator, op.operand);
            if (isProvenCons(operand)) {
                const n = try allocator.create(Ir);
                n.* = .{ .unsafe_car = .{ .operand = operand } };
                return n;
            }
            if (operand != op.operand) {
                const n = try allocator.create(Ir);
                n.* = .{ .car = .{ .operand = operand } };
                return n;
            }
            return node;
        },
        .cdr => |op| {
            const operand = try specialize(allocator, op.operand);
            if (isProvenCons(operand)) {
                const n = try allocator.create(Ir);
                n.* = .{ .unsafe_cdr = .{ .operand = operand } };
                return n;
            }
            if (operand != op.operand) {
                const n = try allocator.create(Ir);
                n.* = .{ .cdr = .{ .operand = operand } };
                return n;
            }
            return node;
        },

        // Vec_ref: specialize when vector proven and index proven fixnum
        .vec_ref => |op| {
            const left = try specialize(allocator, op.left);
            const right = try specialize(allocator, op.right);
            if (isProvenVector(left) and isProvenFixnum(right)) {
                const n = try allocator.create(Ir);
                n.* = .{ .direct_aref = .{ .left = left, .right = right } };
                return n;
            }
            if (left != op.left or right != op.right) {
                const n = try allocator.create(Ir);
                n.* = .{ .vec_ref = .{ .left = left, .right = right } };
                return n;
            }
            return node;
        },

        // Recurse into compound forms
        .@"if" => |i| {
            const cond = try specialize(allocator, i.cond);
            const then_b = try specialize(allocator, i.then_branch);
            const else_b = try specialize(allocator, i.else_branch);
            if (cond != i.cond or then_b != i.then_branch or else_b != i.else_branch) {
                const n = try allocator.create(Ir);
                n.* = .{ .@"if" = .{ .cond = cond, .then_branch = then_b, .else_branch = else_b } };
                return n;
            }
            return node;
        },
        .progn => |exprs| {
            var changed = false;
            const new_exprs = try allocator.alloc(*const Ir, exprs.len);
            for (exprs, 0..) |e, idx| {
                new_exprs[idx] = try specialize(allocator, e);
                if (new_exprs[idx] != e) changed = true;
            }
            if (changed) {
                const n = try allocator.create(Ir);
                n.* = .{ .progn = new_exprs };
                return n;
            }
            return node;
        },
        .let => |l| {
            var changed = false;
            const new_bindings = try allocator.alloc(Ir.Binding, l.bindings.len);
            for (l.bindings, 0..) |b, idx| {
                const new_val = try specialize(allocator, b.value);
                new_bindings[idx] = .{
                    .name = b.name,
                    .index = b.index,
                    .value = new_val,
                };
                if (new_val != b.value) changed = true;
            }
            const new_body = try specialize(allocator, l.body);
            if (new_body != l.body) changed = true;
            if (changed) {
                const n = try allocator.create(Ir);
                n.* = .{ .let = .{ .bindings = new_bindings, .body = new_body } };
                return n;
            }
            return node;
        },
        .set => |s| {
            const new_val = try specialize(allocator, s.value);
            if (new_val != s.value) {
                const n = try allocator.create(Ir);
                n.* = .{ .set = .{ .name = s.name, .depth = s.depth, .index = s.index, .value = new_val } };
                return n;
            }
            return node;
        },
        .define => |d| {
            const new_val = try specialize(allocator, d.value);
            if (new_val != d.value) {
                const n = try allocator.create(Ir);
                n.* = .{ .define = .{ .name = d.name, .index = d.index, .value = new_val } };
                return n;
            }
            return node;
        },
        .lambda => |lam| {
            const new_body = try specialize(allocator, lam.body);
            if (new_body != lam.body) {
                const n = try allocator.create(Ir);
                n.* = .{ .lambda = .{
                    .params = lam.params,
                    .optional_params = lam.optional_params,
                    .key_params = lam.key_params,
                    .allow_other_keys = lam.allow_other_keys,
                    .rest_param = lam.rest_param,
                    .captures = lam.captures,
                    .body = new_body,
                    .lambda_expr = lam.lambda_expr,
                    .name = lam.name,
                } };
                return n;
            }
            return node;
        },
        .call => |c| {
            var changed = false;
            const new_func = try specialize(allocator, c.func);
            if (new_func != c.func) changed = true;
            const new_args = try allocator.alloc(*const Ir, c.args.len);
            for (c.args, 0..) |a, idx| {
                new_args[idx] = try specialize(allocator, a);
                if (new_args[idx] != a) changed = true;
            }
            if (changed) {
                const n = try allocator.create(Ir);
                n.* = .{ .call = .{ .func = new_func, .args = new_args } };
                return n;
            }
            return node;
        },
        .tailcall => |c| {
            var changed = false;
            const new_func = try specialize(allocator, c.func);
            if (new_func != c.func) changed = true;
            const new_args = try allocator.alloc(*const Ir, c.args.len);
            for (c.args, 0..) |a, idx| {
                new_args[idx] = try specialize(allocator, a);
                if (new_args[idx] != a) changed = true;
            }
            if (changed) {
                const n = try allocator.create(Ir);
                n.* = .{ .tailcall = .{ .func = new_func, .args = new_args } };
                return n;
            }
            return node;
        },
        .loop => |l| {
            const new_cond = try specialize(allocator, l.cond);
            const new_body = try specialize(allocator, l.body);
            if (new_cond != l.cond or new_body != l.body) {
                const n = try allocator.create(Ir);
                n.* = .{ .loop = .{ .cond = new_cond, .body = new_body } };
                return n;
            }
            return node;
        },
        .block => |b| {
            const new_body = try specialize(allocator, b.body);
            if (new_body != b.body) {
                const n = try allocator.create(Ir);
                n.* = .{ .block = .{ .name = b.name, .body = new_body } };
                return n;
            }
            return node;
        },
        .return_from => |r| {
            const new_val = try specialize(allocator, r.value);
            if (new_val != r.value) {
                const n = try allocator.create(Ir);
                n.* = .{ .return_from = .{ .name = r.name, .value = new_val } };
                return n;
            }
            return node;
        },

        // Leaf nodes and everything else: return unchanged
        else => return node,
    }
}

// ============================================================================
// Tests
// ============================================================================

test "specialize - fixnum_add from assert_fixnum" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build: (add (assert_fixnum x) (assert_fixnum y))
    const x_var = try alloc.create(Ir);
    x_var.* = .{ .lit = Value.makeFixnum(1) };
    const y_var = try alloc.create(Ir);
    y_var.* = .{ .lit = Value.makeFixnum(2) };
    const ax = try alloc.create(Ir);
    ax.* = .{ .assert_fixnum = .{ .operand = x_var } };
    const ay = try alloc.create(Ir);
    ay.* = .{ .assert_fixnum = .{ .operand = y_var } };
    const add_node = try alloc.create(Ir);
    add_node.* = .{ .add = .{ .left = ax, .right = ay } };

    const result = try specialize(alloc, add_node);
    try testing.expect(result.* == .fixnum_add);
    // Operands should be the assert_fixnum nodes
    try testing.expect(result.fixnum_add.left.* == .assert_fixnum);
    try testing.expect(result.fixnum_add.right.* == .assert_fixnum);
}

test "specialize - unsafe_car from assert_cons" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build: (car (assert_cons x))
    const x_var = try alloc.create(Ir);
    x_var.* = .{ .lit = Value.nil };
    const ac = try alloc.create(Ir);
    ac.* = .{ .assert_cons = .{ .operand = x_var } };
    const car_node = try alloc.create(Ir);
    car_node.* = .{ .car = .{ .operand = ac } };

    const result = try specialize(alloc, car_node);
    try testing.expect(result.* == .unsafe_car);
}

test "specialize - no change without assertion" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build: (add (lit 1) (var x)) — no assertion, should not specialize
    const x_lit = try alloc.create(Ir);
    x_lit.* = .{ .lit = Value.makeFixnum(1) };
    const y_var = try alloc.create(Ir);
    y_var.* = .{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };
    const add_node = try alloc.create(Ir);
    add_node.* = .{ .add = .{ .left = x_lit, .right = y_var } };

    const result = try specialize(alloc, add_node);
    // Left is a literal fixnum, but right is not proven — should stay as .add
    try testing.expect(result.* == .add);
}

test "specialize - fixnum literal + fixnum literal" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build: (add (lit 1) (lit 2)) — both literals, should specialize
    const a = try alloc.create(Ir);
    a.* = .{ .lit = Value.makeFixnum(1) };
    const b = try alloc.create(Ir);
    b.* = .{ .lit = Value.makeFixnum(2) };
    const add_node = try alloc.create(Ir);
    add_node.* = .{ .add = .{ .left = a, .right = b } };

    const result = try specialize(alloc, add_node);
    try testing.expect(result.* == .fixnum_add);
}
