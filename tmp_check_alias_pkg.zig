const std = @import("std");
const repl_mod = @import("src/interp/repl.zig");
const runtime = @import("src/runtime/runtime.zig");

fn show(heap: *runtime.Heap, label: []const u8) void {
    const cl = heap.cl_package;
    const clu = heap.cl_user_package;
    const kw = heap.keyword_package;
    const alias_cl = heap.package_aliases.get("CL");
    const pkg_cl = heap.packages.get("COMMON-LISP");
    const cur = heap.current_package;

    std.debug.print("== {s} ==\n", .{label});
    std.debug.print("cl={any} clu={any} kw={any}\n", .{
        if (cl) |p| @intFromPtr(p) else @as(usize, 0),
        if (clu) |p| @intFromPtr(p) else @as(usize, 0),
        if (kw) |p| @intFromPtr(p) else @as(usize, 0),
    });
    std.debug.print("pkg[COMMON-LISP]={any} alias[CL]={any} current={any}\n", .{
        if (pkg_cl) |p| @intFromPtr(p) else @as(usize, 0),
        if (alias_cl) |p| @intFromPtr(p) else @as(usize, 0),
        if (cur) |p| @intFromPtr(p) else @as(usize, 0),
    });
}

fn load(repl: *repl_mod.Repl, path: []const u8, writer: anytype) !void {
    _ = try repl.loadFilePublic(path, writer);
}

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
    var w = stdout.writer(&buf);
    const writer = &w.interface;

    show(&heap, "start");
    try load(&repl, "lib/stdlib.habu", writer);
    show(&heap, "after stdlib");

    try load(&repl, "/tmp/habu-ansi/ansi-test/compile-and-load.lsp", writer);
    show(&heap, "after compile-and-load");

    try load(&repl, "/tmp/habu-ansi/ansi-test/rt-package.lsp", writer);
    show(&heap, "after rt-package");

    _ = try repl.eval("(compile-and-load \"/tmp/habu-ansi/ansi-test/rt.lsp\")");
    show(&heap, "after compile-and-load rt");

    try load(&repl, "/tmp/habu-ansi/ansi-test/cl-test-package.lsp", writer);
    show(&heap, "after cl-test-package");

    _ = try repl.eval("(in-package :cl-test)");
    show(&heap, "after in-package cl-test");

    _ = try repl.eval("(compile-and-load* \"/tmp/habu-ansi/ansi-test/auxiliary/ansi-aux-macros.lsp\")");
    show(&heap, "after compile-and-load* ansi-aux-macros");

    _ = try repl.eval("(find-package \"CL\")");
    show(&heap, "after find-package CL");
}
