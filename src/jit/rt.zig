//! JIT runtime call helpers

const ctx = @import("ctx.zig");
const runtime = @import("../runtime/runtime.zig");
const arith = @import("../runtime/primitives/arith.zig");

const Value = runtime.Value;
const std = @import("std");

pub fn add(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try arith.add(c.heap, a, b);
}

pub fn sub(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try arith.sub(c.heap, a, b);
}

pub fn mul(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try arith.mul(c.heap, a, b);
}

pub fn div(c: *ctx.JitContext, a: Value, b: Value) arith.Error!Value {
    return try arith.div(c.heap, a, b);
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
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .frame_base = &dummy,
        .stack_end = &dummy,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
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
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .frame_base = &dummy,
        .stack_end = &dummy,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
    };

    try testing.expectError(error.TypeMismatch, neg(&c, Value.nil));
    const res = try neg(&c, Value.makeFixnum(5));
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, -5), res.toFixnum());
}
