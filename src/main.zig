//! Habu Lisp - Main Entry Point
//!
//! A Lisp implementation in Zig with:
//! - Bytecode compiler
//! - Stack-based VM (WASM compatible)
//! - Copy-and-patch JIT (native platforms)
//! - Gradual typing with occurrence typing

const std = @import("std");
const fs = std.fs;
const runtime = @import("runtime/runtime.zig");
const Heap = runtime.Heap;
const repl_mod = @import("interp/repl.zig");
const Repl = repl_mod.Repl;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize heap (64MB)
    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    // Print banner
    const stdout = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var stdout_writer = stdout.writer(&buf);
    const writer = &stdout_writer.interface;

    try writer.print("🐍 Habu Lisp v0.1.0\n", .{});
    try writer.print("Type expressions to evaluate, :h for help, :q to quit\n\n", .{});
    try writer.flush();

    // Run REPL
    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    // Auto-load stdlib.habu
    repl.loadFilePublic("stdlib.habu", writer) catch |err| {
        try writer.print("; Warning: Could not load stdlib.habu: {s}\n", .{@errorName(err)});
    };
    try writer.flush();

    // Load files from command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    for (args[1..]) |arg| {
        repl.loadFilePublic(arg, writer) catch |err| {
            try writer.print("Error loading {s}: {s}\n", .{ arg, @errorName(err) });
        };
        try writer.flush();
    }

    try repl.runWithFiles(fs.File.stdin(), stdout);
}
