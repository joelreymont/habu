const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const repl_mod = @import("src/interp/repl.zig");

pub fn main() !void {
    const gpa = std.heap.page_allocator;
    var heap = try runtime.Heap.init(gpa, .{ .total_size = 128 * 1024 * 1024 });
    defer heap.deinit();

    var repl: repl_mod.Repl = undefined;
    try repl.init(gpa, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const exprs = [_][]const u8{
        "(load \"lib/maxima-loader.lisp\")",
        "(setq *maxima-files* '(\"lmdcls\" \"letmac\" \"clmacs\" \"commac\" \"mormac\" \"globals\" \"compat\" \"defcal\" \"maxmac\" \"mopers\" \"mforma\"))",
        "(maxima-load-all)",
        "(in-package :maxima)",
        "(macroexpand-1 '(mformat-dispatch-on-char -c))",
    };

    for (exprs, 0..) |expr, i| {
        const v = repl.eval(expr) catch |err| {
            std.debug.print("expr[{d}] ERR {s}\n", .{ i, @errorName(err) });
            return;
        };
        std.debug.print("expr[{d}] OK kind={s} raw=0x{x}\n", .{ i, @tagName(v.typeKind()), v.raw });
    }
}
