const std = @import("std");
const repl_mod = @import("src/interp/repl.zig");
const runtime = @import("src/runtime/runtime.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    var heap = try runtime.Heap.init(alloc, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: repl_mod.Repl = undefined;
    try repl.init(alloc, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const stdout = std.fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var stdout_writer = stdout.writer(&buf);
    const writer = &stdout_writer.interface;

    const files = [_][]const u8{
        "lib/stdlib.habu",
        "/private/tmp/habu-ansi/ansi-test/init.lsp",
        "/private/tmp/habu-ansi/ansi-test/gclload1.lsp",
        "/private/tmp/habu-ansi/ansi-test/compile-and-load.lsp",
        "/private/tmp/habu-ansi/ansi-test/rt-package.lsp",
    };

    for (files, 0..) |path, i| {
        repl.loadFilePublic(path, writer) catch |err| {
            std.debug.print("load[{d}] ERR {s} path={s}\n", .{ i + 1, @errorName(err), path });
            return;
        };
        std.debug.print("load[{d}] OK path={s}\n", .{ i + 1, path });
    }

    const checks = [_][]const u8{
        "(in-package :regression-test)",
        "(fboundp 'truename)",
        "(fboundp 'ignore-errors)",
        "(fboundp 'values)",
        "(ignore-errors (truename #P\"sandbox/\"))",
        "(defparameter *sandbox-path* (ignore-errors (truename #P\"sandbox/\")))",
    };

    for (checks, 0..) |expr, i| {
        const r = repl.eval(expr) catch |err| {
            std.debug.print("check[{d}] ERR {s} expr={s}\n", .{ i + 1, @errorName(err), expr });
            continue;
        };
        std.debug.print("check[{d}] OK kind={s} raw=0x{x} expr={s}\n", .{ i + 1, @tagName(r.typeKind()), r.raw, expr });
    }
}
