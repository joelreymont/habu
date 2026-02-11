//! Specialization Pass
//!
//! Replaces generic IR nodes with specialized variants when types are
//! proven. Uses flow-sensitive type propagation through let bindings:
//!
//!   (let ((i 0) (acc 0))        ; i, acc proven fixnum (literal)
//!     (while (< i n)            ; → fixnum_lt if both proven
//!       (setq acc (+ acc i))    ; → fixnum_add, acc stays fixnum
//!       (setq i (+ i 1))))      ; → fixnum_add, i stays fixnum
//!
//! Patterns recognized:
//!   - let bindings with fixnum literal/assert/fixnum_op → variable proven fixnum
//!   - set with fixnum-producing value → variable stays proven fixnum
//!   - assert_fixnum wrapping var → variable proven fixnum
//!   - assert_fixnum wrapping arithmetic → specialized arithmetic
//!   - assert_cons wrapping car/cdr → unsafe_car/cdr
//!   - assert_vector + assert_fixnum in vec_ref → direct_aref
//!
//! Input: Ir (after erase)
//! Output: Ir (with specialized nodes)

const std = @import("std");
const Ir = @import("../ir.zig").Ir;

/// Known type for a variable slot.
const VarType = enum { fixnum, cons, vector, unknown };

/// Flow-sensitive type environment.
/// Maps variable indices to their known types.
const TypeEnv = struct {
    /// Stack of known types indexed by local variable index.
    /// null = unknown type. Grows as needed.
    types: std.AutoHashMapUnmanaged(u16, VarType),

    fn init() TypeEnv {
        return .{ .types = .{} };
    }

    fn deinit(self: *TypeEnv, allocator: std.mem.Allocator) void {
        self.types.deinit(allocator);
    }

    fn clone(self: *const TypeEnv, allocator: std.mem.Allocator) !TypeEnv {
        return .{ .types = try self.types.clone(allocator) };
    }

    fn setType(self: *TypeEnv, allocator: std.mem.Allocator, index: u16, ty: VarType) !void {
        try self.types.put(allocator, index, ty);
    }

    fn getType(self: *const TypeEnv, index: u16) VarType {
        return self.types.get(index) orelse .unknown;
    }
};

/// Check if an IR node is proven fixnum given the current type environment.
fn isProvenFixnum(node: *const Ir, env: *const TypeEnv) bool {
    return switch (node.*) {
        .assert_fixnum => true,
        .lit => |v| v.isFixnum(),
        .@"var" => |v| env.getType(v.index) == .fixnum,
        // Results of specialized fixnum ops are fixnum
        .fixnum_add, .fixnum_sub, .fixnum_mul => true,
        else => false,
    };
}

/// Check if an IR node is proven cons given the current type environment.
fn isProvenCons(node: *const Ir, env: *const TypeEnv) bool {
    return switch (node.*) {
        .assert_cons => true,
        .@"var" => |v| env.getType(v.index) == .cons,
        .cons => true,
        else => false,
    };
}

/// Check if an IR node is proven vector given the current type environment.
fn isProvenVector(node: *const Ir, env: *const TypeEnv) bool {
    return switch (node.*) {
        .assert_vector => true,
        .@"var" => |v| env.getType(v.index) == .vector,
        else => false,
    };
}

/// Determine the result type of a specialized IR node.
fn resultType(node: *const Ir, env: *const TypeEnv) VarType {
    return switch (node.*) {
        .lit => |v| if (v.isFixnum()) .fixnum else .unknown,
        .assert_fixnum => .fixnum,
        .assert_cons => .cons,
        .assert_vector => .vector,
        .fixnum_add, .fixnum_sub, .fixnum_mul => .fixnum,
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq => .unknown, // returns t/nil
        .@"var" => |v| env.getType(v.index),
        .cons => .cons,
        else => .unknown,
    };
}

/// Strip assert wrapper, returning the inner operand.
/// Used when the specialized op already guarantees the type.
fn stripAssert(node: *const Ir) *const Ir {
    return switch (node.*) {
        .assert_fixnum => |af| af.operand,
        .assert_cons => |ac| ac.operand,
        .assert_vector => |av| av.operand,
        .assert_symbol => |a| a.operand,
        .assert_string => |a| a.operand,
        else => node,
    };
}

/// Walk IR tree and replace generic ops with specialized variants.
/// Uses flow-sensitive type environment to track variable types through
/// let bindings and set mutations.
pub fn specialize(allocator: std.mem.Allocator, node: *const Ir) !*const Ir {
    var env = TypeEnv.init();
    defer env.deinit(allocator);
    return specializeWithEnv(allocator, node, &env);
}

fn specializeWithEnv(allocator: std.mem.Allocator, node: *const Ir, env: *TypeEnv) std.mem.Allocator.Error!*const Ir {
    switch (node.*) {
        // Binary arithmetic: specialize when both operands proven fixnum
        .add => |op| {
            const left = try specializeWithEnv(allocator, op.left, env);
            const right = try specializeWithEnv(allocator, op.right, env);
            if (isProvenFixnum(left, env) and isProvenFixnum(right, env)) {
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
            const left = try specializeWithEnv(allocator, op.left, env);
            const right = try specializeWithEnv(allocator, op.right, env);
            if (isProvenFixnum(left, env) and isProvenFixnum(right, env)) {
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
            const left = try specializeWithEnv(allocator, op.left, env);
            const right = try specializeWithEnv(allocator, op.right, env);
            if (isProvenFixnum(left, env) and isProvenFixnum(right, env)) {
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
            const operand = try specializeWithEnv(allocator, op.operand, env);
            if (isProvenCons(operand, env)) {
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
            const operand = try specializeWithEnv(allocator, op.operand, env);
            if (isProvenCons(operand, env)) {
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
            const left = try specializeWithEnv(allocator, op.left, env);
            const right = try specializeWithEnv(allocator, op.right, env);
            if (isProvenVector(left, env) and isProvenFixnum(right, env)) {
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

        // Comparisons: specialize when both operands proven fixnum
        .le => |op| return try specializeCmp(allocator, .fixnum_le, .le, op.left, op.right, env) orelse node,
        .lt => |op| return try specializeCmp(allocator, .fixnum_lt, .lt, op.left, op.right, env) orelse node,
        .gt => |op| return try specializeCmp(allocator, .fixnum_gt, .gt, op.left, op.right, env) orelse node,
        .ge => |op| return try specializeCmp(allocator, .fixnum_ge, .ge, op.left, op.right, env) orelse node,
        .num_eq => |op| return try specializeCmp(allocator, .fixnum_eq, .num_eq, op.left, op.right, env) orelse node,

        // Compound forms with type environment propagation
        .@"if" => |i| {
            const cond = try specializeWithEnv(allocator, i.cond, env);
            const then_b = try specializeWithEnv(allocator, i.then_branch, env);
            const else_b = try specializeWithEnv(allocator, i.else_branch, env);
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
                new_exprs[idx] = try specializeWithEnv(allocator, e, env);
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
            // Flow-sensitive: track binding types
            var changed = false;
            const new_bindings = try allocator.alloc(Ir.Binding, l.bindings.len);
            for (l.bindings, 0..) |b, idx| {
                const new_val = try specializeWithEnv(allocator, b.value, env);
                new_bindings[idx] = .{
                    .name = b.name,
                    .index = b.index,
                    .value = new_val,
                };
                if (new_val != b.value) changed = true;

                // Record the type of this binding
                const ty = resultType(new_val, env);
                if (ty != .unknown) {
                    try env.setType(allocator, b.index, ty);
                }
            }
            const new_body = try specializeWithEnv(allocator, l.body, env);
            if (new_body != l.body) changed = true;
            if (changed) {
                const n = try allocator.create(Ir);
                n.* = .{ .let = .{ .bindings = new_bindings, .body = new_body } };
                return n;
            }
            return node;
        },
        .set => |s| {
            const new_val = try specializeWithEnv(allocator, s.value, env);

            // Update type environment for the mutated variable
            const ty = resultType(new_val, env);
            try env.setType(allocator, s.index, ty);

            if (new_val != s.value) {
                const n = try allocator.create(Ir);
                n.* = .{ .set = .{ .name = s.name, .depth = s.depth, .index = s.index, .value = new_val } };
                return n;
            }
            return node;
        },
        .define => |d| {
            const new_val = try specializeWithEnv(allocator, d.value, env);
            if (new_val != d.value) {
                const n = try allocator.create(Ir);
                n.* = .{ .define = .{ .name = d.name, .index = d.index, .value = new_val } };
                return n;
            }
            return node;
        },
        .lambda => |lam| {
            // Lambda gets a fresh type environment (params are unknown)
            var inner_env = TypeEnv.init();
            defer inner_env.deinit(allocator);
            const new_body = try specializeWithEnv(allocator, lam.body, &inner_env);
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
                    .speed = lam.speed,
                    .safety = lam.safety,
                } };
                return n;
            }
            return node;
        },
        .call => |c| {
            var changed = false;
            const new_func = try specializeWithEnv(allocator, c.func, env);
            if (new_func != c.func) changed = true;
            const new_args = try allocator.alloc(*const Ir, c.args.len);
            for (c.args, 0..) |a, idx| {
                new_args[idx] = try specializeWithEnv(allocator, a, env);
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
            const new_func = try specializeWithEnv(allocator, c.func, env);
            if (new_func != c.func) changed = true;
            const new_args = try allocator.alloc(*const Ir, c.args.len);
            for (c.args, 0..) |a, idx| {
                new_args[idx] = try specializeWithEnv(allocator, a, env);
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
            // For loops, we specialize cond and body with the current env.
            // The set operations inside the body will update the env.
            // This is a single-pass approximation — for fixnum counting loops
            // it works because the init binding sets the type before the loop.
            const new_cond = try specializeWithEnv(allocator, l.cond, env);
            const new_body = try specializeWithEnv(allocator, l.body, env);
            if (new_cond != l.cond or new_body != l.body) {
                const n = try allocator.create(Ir);
                n.* = .{ .loop = .{ .cond = new_cond, .body = new_body } };
                return n;
            }
            return node;
        },
        .block => |b| {
            const new_body = try specializeWithEnv(allocator, b.body, env);
            if (new_body != b.body) {
                const n = try allocator.create(Ir);
                n.* = .{ .block = .{ .name = b.name, .body = new_body } };
                return n;
            }
            return node;
        },
        .return_from => |r| {
            const new_val = try specializeWithEnv(allocator, r.value, env);
            if (new_val != r.value) {
                const n = try allocator.create(Ir);
                n.* = .{ .return_from = .{ .name = r.name, .value = new_val } };
                return n;
            }
            return node;
        },

        // assert_fixnum wrapping arithmetic: specialize the inner op.
        // (the fixnum (+ x y)) → assert_fixnum(add(x, y))
        // Since the programmer declared the result is fixnum, we trust it
        // and use the fixnum-specialized op (which skips type dispatch).
        .assert_fixnum => |af| {
            const inner = try specializeWithEnv(allocator, af.operand, env);
            switch (inner.*) {
                .add => |op| {
                    const n = try allocator.create(Ir);
                    n.* = .{ .fixnum_add = .{ .left = op.left, .right = op.right } };
                    return n;
                },
                .sub => |op| {
                    const n = try allocator.create(Ir);
                    n.* = .{ .fixnum_sub = .{ .left = op.left, .right = op.right } };
                    return n;
                },
                .mul => |op| {
                    const n = try allocator.create(Ir);
                    n.* = .{ .fixnum_mul = .{ .left = op.left, .right = op.right } };
                    return n;
                },
                else => {
                    // Not arithmetic — keep the assert_fixnum with specialized inner
                    if (inner != af.operand) {
                        const n = try allocator.create(Ir);
                        n.* = .{ .assert_fixnum = .{ .operand = inner } };
                        return n;
                    }
                    return node;
                },
            }
        },

        // Unary ops: recurse into operand
        .not, .nilp, .consp, .abs => |op| {
            const new_operand = try specializeWithEnv(allocator, op.operand, env);
            if (new_operand != op.operand) {
                const n = try allocator.create(Ir);
                const tag = node.*;
                n.* = switch (tag) {
                    .not => .{ .not = .{ .operand = new_operand } },
                    .nilp => .{ .nilp = .{ .operand = new_operand } },
                    .consp => .{ .consp = .{ .operand = new_operand } },
                    .abs => .{ .abs = .{ .operand = new_operand } },
                    else => unreachable,
                };
                return n;
            }
            return node;
        },

        // Leaf nodes and everything else: return unchanged
        else => return node,
    }
}

/// Specialize a comparison operation. Returns null if unchanged.
fn specializeCmp(
    allocator: std.mem.Allocator,
    comptime spec_tag: std.meta.Tag(Ir),
    comptime gen_tag: std.meta.Tag(Ir),
    left_orig: *const Ir,
    right_orig: *const Ir,
    env: *TypeEnv,
) std.mem.Allocator.Error!?*const Ir {
    const left = try specializeWithEnv(allocator, left_orig, env);
    const right = try specializeWithEnv(allocator, right_orig, env);
    if (isProvenFixnum(left, env) and isProvenFixnum(right, env)) {
        const n = try allocator.create(Ir);
        n.* = @unionInit(Ir, @tagName(spec_tag), .{ .left = left, .right = right });
        return n;
    }
    if (left != left_orig or right != right_orig) {
        const n = try allocator.create(Ir);
        n.* = @unionInit(Ir, @tagName(gen_tag), .{ .left = left, .right = right });
        return n;
    }
    return null; // unchanged
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
    // Operands should be the assert_fixnum nodes (runtime checks preserved for safety)
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

    // Build: (add (lit 1) (var x)) — no assertion, x type unknown
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

test "specialize - flow-sensitive let binding" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build: (let ((i 0)) (+ i 1))
    // i is bound to fixnum literal 0, so (+ i 1) should specialize to fixnum_add
    const zero = try alloc.create(Ir);
    zero.* = .{ .lit = Value.makeFixnum(0) };
    const one = try alloc.create(Ir);
    one.* = .{ .lit = Value.makeFixnum(1) };
    const i_var = try alloc.create(Ir);
    i_var.* = .{ .@"var" = .{ .name = "i", .depth = 0, .index = 0 } };
    const add_node = try alloc.create(Ir);
    add_node.* = .{ .add = .{ .left = i_var, .right = one } };
    const let_node = try alloc.create(Ir);
    let_node.* = .{ .let = .{
        .bindings = &[_]Ir.Binding{.{ .name = "i", .index = 0, .value = zero }},
        .body = add_node,
    } };

    const result = try specialize(alloc, let_node);
    try testing.expect(result.* == .let);
    // Body should be specialized to fixnum_add
    try testing.expect(result.let.body.* == .fixnum_add);
}
