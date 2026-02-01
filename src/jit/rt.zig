//! JIT runtime call helpers

const ctx = @import("ctx.zig");
const runtime = @import("../runtime/runtime.zig");
const arith = @import("../runtime/primitives/arith.zig");

const Value = runtime.Value;
const std = @import("std");

pub const Err = enum(u32) {
    ok = 0,
    type_mismatch = 1,
    div_zero = 2,
    out_of_memory = 3,
    invalid_argument = 4,
};

fn setOk(c: *ctx.JitContext) void {
    c.err = @intFromEnum(Err.ok);
}

fn setArithErr(c: *ctx.JitContext, err: arith.Error) void {
    c.err = @intFromEnum(switch (err) {
        error.TypeMismatch => Err.type_mismatch,
        error.DivisionByZero => Err.div_zero,
        error.OutOfMemory => Err.out_of_memory,
        error.InvalidArgument => Err.invalid_argument,
    });
}

pub fn add(c: *ctx.JitContext, a: Value, b: Value) callconv(.c) Value {
    const res = arith.add(c.heap, a, b) catch |err| {
        setArithErr(c, err);
        return Value.nil;
    };
    setOk(c);
    return res;
}

pub fn sub(c: *ctx.JitContext, a: Value, b: Value) callconv(.c) Value {
    const res = arith.sub(c.heap, a, b) catch |err| {
        setArithErr(c, err);
        return Value.nil;
    };
    setOk(c);
    return res;
}

pub fn mul(c: *ctx.JitContext, a: Value, b: Value) callconv(.c) Value {
    const res = arith.mul(c.heap, a, b) catch |err| {
        setArithErr(c, err);
        return Value.nil;
    };
    setOk(c);
    return res;
}

pub fn div(c: *ctx.JitContext, a: Value, b: Value) callconv(.c) Value {
    const res = arith.div(c.heap, a, b) catch |err| {
        setArithErr(c, err);
        return Value.nil;
    };
    setOk(c);
    return res;
}

pub fn numberp(c: *ctx.JitContext, a: Value) callconv(.c) Value {
    setOk(c);
    return if (a.isNumber()) Value.t else Value.nil;
}

test "rt add sets err" {
    const testing = std.testing;

    var heap = try runtime.Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var dummy = [_]Value{Value.nil};
    var c = ctx.JitContext{
        .sp = &dummy,
        .const_pool = &dummy,
        .heap = &heap,
        .err = 0,
    };

    _ = add(&c, Value.nil, Value.nil);
    try testing.expectEqual(@intFromEnum(Err.type_mismatch), c.err);

    const res = add(&c, Value.makeFixnum(1), Value.makeFixnum(2));
    try testing.expectEqual(@intFromEnum(Err.ok), c.err);
    try testing.expect(res.isFixnum());
    try testing.expectEqual(@as(i64, 3), res.toFixnum());
}
