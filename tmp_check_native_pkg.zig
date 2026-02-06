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

    const out_file = std.fs.File.stdout();
    var out_buf: [4096]u8 = undefined;
    var out_writer = out_file.writer(&out_buf);
    const out = &out_writer.interface;

    _ = try repl.loadFilePublic("lib/stdlib.habu", out);

    const cl = heap.cl_package.?;
    const clu = heap.cl_user_package.?;
    std.debug.print("cl has ignore-errors? {any}\n", .{cl.symbols.get("IGNORE-ERRORS") != null});
    std.debug.print("clu has ignore-errors? {any}\n", .{clu.symbols.get("IGNORE-ERRORS") != null});
    std.debug.print("cl has truename? {any}\n", .{cl.symbols.get("TRUENAME") != null});
    std.debug.print("clu has truename? {any}\n", .{clu.symbols.get("TRUENAME") != null});
}
