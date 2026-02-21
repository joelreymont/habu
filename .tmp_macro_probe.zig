const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const Repl = @import("src/interp/repl.zig").Repl;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var heap = try runtime.Heap.init(allocator, .{ .total_size = 512 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(load \"lib/stdlib.habu\")");
    _ = try repl.eval("(load \"../maxima/src/commac.lisp\")");
    _ = try repl.eval("(load \"../maxima/src/transm.lisp\")");

    const expanded = try repl.eval("(macroexpand-1 '(def%tr $eval_when (form) 'ok))");

    var buf: [8192]u8 = undefined;
    var out = std.fs.File.stdout().writer(&buf);
    const w = &out.interface;
    try repl.printValue(expanded, w);
    try w.writeAll("\n");
    try w.flush();
}
