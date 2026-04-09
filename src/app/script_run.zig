const std = @import("std");
const runtime = @import("../runtime/runtime.zig");
const Heap = runtime.Heap;
const repl_mod = @import("../interp/repl.zig");
const Repl = repl_mod.Repl;

pub fn run(allocator: std.mem.Allocator, heap_size: usize, args: []const []const u8, writer: anytype) !void {
    if (args.len <= 1) return error.InvalidArgument;

    var heap = try Heap.init(allocator, .{ .total_size = heap_size });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    try repl.loadFile("lib/stdlib.habu", writer);
    try publishArgs(&repl, args);

    const script_path = args[1];
    try repl.addTrustedLoadRootForFile(script_path);
    try repl.loadFile(script_path, writer);
}

fn publishArgs(repl: *Repl, args: []const []const u8) !void {
    const script_args = if (args.len > 2) args[2..] else &.{};
    try repl.publishCommandLineArgs(script_args, args);
}
