//! JIT runtime call helpers

const ctx = @import("ctx.zig");
const runtime = @import("../runtime/runtime.zig");
const arith = @import("../runtime/primitives/arith.zig");

const Value = runtime.Value;
const std = @import("std");

const BinaryOp = *const fn (*runtime.Heap, Value, Value) arith.Error!Value;

fn collectJitGarbage(c: *ctx.JitContext, extra: []Value) !void {
    const heap = c.heap;
    var roots = std.ArrayList(Value){};
    defer roots.deinit(heap.backing_allocator);

    const stack_len_bytes = @intFromPtr(c.sp) - @intFromPtr(c.frame_base);
    const stack_len: usize = @intCast(@divExact(stack_len_bytes, @sizeOf(Value)));
    const stack_vals = c.frame_base[0..stack_len];
    try roots.appendSlice(heap.backing_allocator, stack_vals);

    const const_vals = c.const_pool[0..c.const_count];
    try roots.appendSlice(heap.backing_allocator, const_vals);

    try roots.appendSlice(heap.backing_allocator, extra);

    _ = try heap.collectGarbage(roots.items);

    var idx: usize = 0;
    for (stack_vals) |*v| {
        v.* = roots.items[idx];
        idx += 1;
    }
    for (const_vals) |*v| {
        v.* = roots.items[idx];
        idx += 1;
    }
    for (extra) |*v| {
        v.* = roots.items[idx];
        idx += 1;
    }
}

fn callBinaryWithGc(c: *ctx.JitContext, a: Value, b: Value, func: BinaryOp) arith.Error!Value {
    var args = [_]Value{ a, b };
    return func(c.heap, args[0], args[1]) catch |err| switch (err) {
        error.OutOfMemory => blk: {
            try collectJitGarbage(c, &args);
            break :blk try func(c.heap, args[0], args[1]);
        },
        else => return err,
    };
}

pub fn add(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.add);
}

pub fn sub(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.sub);
}

pub fn mul(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.mul);
}

pub fn div(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try callBinaryWithGc(c, a, b, arith.div);
}

pub fn neg(c: *ctx.JitContext, a: Value) arith.Error!Value {
    _ = c;
    return try arith.negate(a);
}

pub fn numberp(c: *ctx.JitContext, a: Value) arith.Error!Value {
    _ = c;
    return if (a.isNumber()) Value.t else Value.nil;
}

pub fn lt(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.lt(a, b)) Value.t else Value.nil;
}

pub fn gt(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.gt(a, b)) Value.t else Value.nil;
}

pub fn le(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.le(a, b)) Value.t else Value.nil;
}

pub fn ge(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    _ = c;
    return if (try arith.ge(a, b)) Value.t else Value.nil;
}

test "rt add returns error union" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var dummy = [_]Value{Value.nil};
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .frame_base = &dummy,
        .stack_end = &dummy,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
    };

    try testing.expectError(error.TypeMismatch, add(&c, Value.nil, Value.nil));

    const res = try add(&c, Value.makeFixnum(1), Value.makeFixnum(2));
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 3), res.toFixnum());
}

test "rt neg returns error union" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var dummy = [_]Value{Value.nil};
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .frame_base = &dummy,
        .stack_end = &dummy,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
    };

    try testing.expectError(error.TypeMismatch, neg(&c, Value.nil));
    const res = try neg(&c, Value.makeFixnum(5));
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, -5), res.toFixnum());
}
