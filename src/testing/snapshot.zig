const std = @import("std");

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Repl = @import("../interp/repl.zig").Repl;
const harness = @import("harness.zig");
const OhSnap = @import("ohsnap");

pub fn asString(val: Value) ![]const u8 {
    switch (val.typeKind()) {
        .string => return val.toPtr(runtime.String).bytes(),
        else => return error.TypeMismatch,
    }
}

pub fn expectValue(
    comptime src: std.builtin.SourceLocation,
    val: Value,
    comptime expected: []const u8,
) !void {
    const got = try asString(val);
    const oh = OhSnap{};
    try oh.snap(src, expected).diff(got, true);
}

pub fn expectEval(
    comptime src: std.builtin.SourceLocation,
    repl: *Repl,
    expr: []const u8,
    comptime expected: []const u8,
) !void {
    const wrapped = try std.fmt.allocPrint(repl.allocator, "(write-to-string {s})", .{expr});
    defer repl.allocator.free(wrapped);

    const got = try repl.eval(wrapped);
    try expectValue(src, got, expected);
}

pub fn expectHarnessEval(
    comptime src: std.builtin.SourceLocation,
    allocator: std.mem.Allocator,
    heap: *Heap,
    expr: []const u8,
    comptime expected: []const u8,
) !void {
    const wrapped = try std.fmt.allocPrint(allocator, "(write-to-string {s})", .{expr});
    defer allocator.free(wrapped);

    const got = try harness.eval(allocator, heap, wrapped);
    try expectValue(src, got, expected);
}
