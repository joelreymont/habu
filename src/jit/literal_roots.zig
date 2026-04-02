const std = @import("std");
const compiler_mod = @import("../compiler/compiler.zig");
const interp = @import("../interp/interp.zig");
const runtime = @import("../runtime/runtime.zig");
const jit_backend = @import("backend_api.zig");

const Ir = compiler_mod.ir.Ir;
const Vm = interp.Vm;
const Heap = runtime.Heap;
const Value = runtime.Value;

pub const LiteralRoots = jit_backend.LiteralRoots;

pub fn shouldRootJitLiteral(v: Value) bool {
    return v.isPointer() and !v.isMagicSymbol();
}

pub fn rootLiteral(vm: *Vm, ir_node: *const Ir, roots: *LiteralRoots, v: Value) !void {
    if (!shouldRootJitLiteral(v)) return;
    const key = @intFromPtr(ir_node);
    if (roots.contains(key)) return;
    const slot = try vm.registerJitLiteral(v);
    try roots.put(key, slot);
}

pub fn rootValue(vm: *Vm, ir_node: *const Ir, roots: *LiteralRoots, v: Value) !void {
    const key = @intFromPtr(ir_node);
    if (roots.contains(key)) return;
    const slot = try vm.registerJitLiteral(v);
    try roots.put(key, slot);
}

pub fn ensureCoverage(ir_node: *const Ir, roots: *const LiteralRoots) !void {
    if (jit_backend.IrTranslator.firstUnsupportedTagWithLiteralRoots(ir_node, roots) != null) {
        return error.UnsupportedIrNode;
    }
}

pub fn collect(ir_node: *const Ir, roots: *LiteralRoots, ctx: anytype, comptime ops: anytype) !void {
    switch (ir_node.*) {
        .lit => |v| try ops.onLit(ctx, ir_node, roots, v),
        .global_ref => |gr| try ops.onGlobalRef(ctx, ir_node, roots, gr.name),
        .lambda => |lam| {
            if (lam.captures.len != 0) return error.UnsupportedIrNode;
            if (lam.optional_params.len != 0) return error.UnsupportedIrNode;
            if (lam.key_params.len != 0) return error.UnsupportedIrNode;
            if (lam.rest_param != null) return error.UnsupportedIrNode;
            try ops.onLambda(ctx, ir_node, roots, lam);
            try collect(lam.body, roots, ctx, ops);
        },
        .fixnum_add,
        .fixnum_sub,
        .add,
        .sub,
        .fixnum_le,
        .fixnum_lt,
        .fixnum_gt,
        .fixnum_ge,
        .fixnum_eq,
        .le,
        .lt,
        .gt,
        .ge,
        .num_eq,
        .fixnum_mul,
        .mul,
        .eq,
        .cons,
        .logand,
        .mod,
        .rem,
        .append,
        .assoc,
        .make_string,
        .position,
        .position_eq,
        .position_equal,
        => |op| {
            try collect(op.left, roots, ctx, ops);
            try collect(op.right, roots, ctx, ops);
        },
        .@"if" => |f| {
            try collect(f.cond, roots, ctx, ops);
            try collect(f.then_branch, roots, ctx, ops);
            try collect(f.else_branch, roots, ctx, ops);
        },
        .block => |b| try collect(b.body, roots, ctx, ops),
        .progn => |exprs| {
            for (exprs) |expr| try collect(expr, roots, ctx, ops);
        },
        .list, .list_star => |elements| {
            for (elements) |element| try collect(element, roots, ctx, ops);
        },
        .let => |l| {
            for (l.bindings) |binding| try collect(binding.value, roots, ctx, ops);
            try collect(l.body, roots, ctx, ops);
        },
        .set => |s| try collect(s.value, roots, ctx, ops),
        .loop => |l| {
            try collect(l.cond, roots, ctx, ops);
            try collect(l.body, roots, ctx, ops);
        },
        .progv => |p| {
            try collect(p.symbols, roots, ctx, ops);
            try collect(p.values, roots, ctx, ops);
            try collect(p.body, roots, ctx, ops);
        },
        .assert_fixnum => |op| try collect(op.operand, roots, ctx, ops),
        .call => |c| {
            try collect(c.func, roots, ctx, ops);
            for (c.args) |arg| try collect(arg, roots, ctx, ops);
        },
        .tailcall => |tc| {
            try collect(tc.func, roots, ctx, ops);
            for (tc.args) |arg| try collect(arg, roots, ctx, ops);
        },
        .nilp,
        .not,
        .consp,
        .abs,
        .zerop,
        .oddp,
        .evenp,
        .car,
        .cdr,
        .unsafe_car,
        .unsafe_cdr,
        .length,
        .sqrt,
        .round,
        .intern,
        .vec_len,
        .str_len,
        => |op| try collect(op.operand, roots, ctx, ops),
        .hash_count => |h| try collect(h.operand, roots, ctx, ops),
        .hash_capacity => |h| try collect(h.operand, roots, ctx, ops),
        .hash_clear => |h| try collect(h.operand, roots, ctx, ops),
        .hash_test => |h| try collect(h.operand, roots, ctx, ops),
        .hash_keys => |h| try collect(h.operand, roots, ctx, ops),
        .hash_alist => |h| try collect(h.operand, roots, ctx, ops),
        .make_hash => {},
        .vec_new => |v| {
            try collect(v.size, roots, ctx, ops);
            if (v.init) |init_val| try collect(init_val, roots, ctx, ops);
        },
        .vec_ref, .str_ref, .str_concat => |op| {
            try collect(op.left, roots, ctx, ops);
            try collect(op.right, roots, ctx, ops);
        },
        .vec_set => |v| {
            try collect(v.vec, roots, ctx, ops);
            try collect(v.index, roots, ctx, ops);
            try collect(v.value, roots, ctx, ops);
        },
        .hash_get => |h| {
            try collect(h.table, roots, ctx, ops);
            try collect(h.key, roots, ctx, ops);
            if (h.default) |d| try collect(d, roots, ctx, ops);
        },
        .hash_set => |h| {
            try collect(h.table, roots, ctx, ops);
            try collect(h.key, roots, ctx, ops);
            try collect(h.value, roots, ctx, ops);
        },
        .hash_rem => |h| {
            try collect(h.table, roots, ctx, ops);
            try collect(h.key, roots, ctx, ops);
        },
        .format => |f| {
            if (f.args.len > 1) return error.UnsupportedIrNode;
            try collect(f.dest, roots, ctx, ops);
            try collect(f.control, roots, ctx, ops);
            for (f.args) |arg| try collect(arg, roots, ctx, ops);
        },
        .str_set => |s| {
            try collect(s.str, roots, ctx, ops);
            try collect(s.index, roots, ctx, ops);
            try collect(s.value, roots, ctx, ops);
        },
        .substring => |s| {
            try collect(s.str, roots, ctx, ops);
            try collect(s.start, roots, ctx, ops);
            try collect(s.end, roots, ctx, ops);
        },
        .arr_new => |a| {
            if (a.dimensions.len > 8) return error.UnsupportedIrNode;
            for (a.dimensions) |dim| try collect(dim, roots, ctx, ops);
            if (a.init) |init_ir| try collect(init_ir, roots, ctx, ops);
        },
        .arr_new_dyn => |a| {
            try collect(a.dimensions, roots, ctx, ops);
            if (a.init) |init_ir| try collect(init_ir, roots, ctx, ops);
        },
        .arr_ref => |a| {
            if (a.subscripts.len < 1 or a.subscripts.len > 2) return error.UnsupportedIrNode;
            try collect(a.array, roots, ctx, ops);
            for (a.subscripts) |sub| try collect(sub, roots, ctx, ops);
        },
        .arr_set => |a| {
            if (a.subscripts.len < 1 or a.subscripts.len > 2) return error.UnsupportedIrNode;
            try collect(a.array, roots, ctx, ops);
            for (a.subscripts) |sub| try collect(sub, roots, ctx, ops);
            try collect(a.value, roots, ctx, ops);
        },
        else => return error.UnsupportedIrNode,
    }
}

test "ensureCoverage catches missing nested literal roots" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var vm = try interp.Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = compiler_mod.IrBuilder.init(alloc);
    const lit = try builder.lit(try heap.allocBaseString("x"));
    const prog = try builder.progn(&[_]*const Ir{lit});
    const blk = try builder.block(Value.nil, prog);

    var roots = LiteralRoots.init(testing.allocator);
    defer roots.deinit();

    try testing.expectError(error.UnsupportedIrNode, ensureCoverage(blk, &roots));

    const Ctx = struct { vm: *Vm };
    const ops = struct {
        fn onLit(ctx: Ctx, ir_node: *const Ir, roots_: *LiteralRoots, v: Value) !void {
            try rootLiteral(ctx.vm, ir_node, roots_, v);
        }
        fn onGlobalRef(_: Ctx, _: *const Ir, _: *LiteralRoots, _: []const u8) !void {
            return error.UnsupportedIrNode;
        }
        fn onLambda(_: Ctx, _: *const Ir, _: *LiteralRoots, _: anytype) !void {
            return error.UnsupportedIrNode;
        }
    };

    try collect(blk, &roots, Ctx{ .vm = &vm }, ops);
    try ensureCoverage(blk, &roots);
}
