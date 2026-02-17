//! Habu Lisp - Main Entry Point
//!
//! A Lisp implementation in Zig with:
//! - Bytecode compiler
//! - Stack-based VM (WASM compatible)
//! - Hoist SSA JIT (native platforms)
//! - Gradual typing with occurrence typing

const std = @import("std");
const fs = std.fs;
const runtime = @import("runtime/runtime.zig");
const Heap = runtime.Heap;
const repl_mod = @import("interp/repl.zig");
const Repl = repl_mod.Repl;

fn resolveHeapSize() usize {
    const default_size = 256 * 1024 * 1024;
    const heap_mb_c = std.posix.getenv("HABU_HEAP_MB") orelse return default_size;
    const heap_mb = std.fmt.parseUnsigned(usize, std.mem.sliceTo(heap_mb_c, 0), 10) catch return default_size;
    if (heap_mb == 0) return default_size;
    return std.math.mul(usize, heap_mb, 1024 * 1024) catch return default_size;
}

pub fn main() !void {
    // Run on a thread with 64MB stack to handle deep recursive parsing/compilation
    // of large Lisp files (e.g., Maxima's 3500-line simp.lisp).
    const thread = try std.Thread.spawn(.{ .stack_size = 512 * 1024 * 1024 }, mainImpl, .{});
    thread.join();
}

fn mainImpl() void {
    mainInner() catch |err| {
        const stderr = fs.File.stderr();
        var buf: [4096]u8 = undefined;
        var stderr_writer = stderr.writer(&buf);
        stderr_writer.interface.print("Fatal error: {s}\n", .{@errorName(err)}) catch {};
        stderr_writer.interface.flush() catch {};
    };
}

fn mainInner() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Initialize heap (256MB default, overridable via HABU_HEAP_MB)
    var heap = try Heap.init(allocator, .{ .total_size = resolveHeapSize() });
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
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Auto-load stdlib.habu
    if (repl.loadFilePublic("lib/stdlib.habu", writer)) |_| {} else |err| {
        try writer.print("; Warning: Could not load lib/stdlib.habu: {s}\n", .{@errorName(err)});
        if (err == error.FileNotFound) {
            try writer.print("; Hint: Run from project root directory\n", .{});
        }
        try writer.flush();
        return err;
    }
    try writer.flush();

    // Load files from command line arguments
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    const has_files = args.len > 1;
    for (args[1..]) |arg| {
        if (repl.loadFilePublic(arg, writer)) |_| {} else |err| {
            try writer.print("Error loading {s}: {s}\n", .{ arg, @errorName(err) });
            try writer.flush();
            return err;
        }
        try writer.flush();
    }

    // Only run interactive REPL if no files were loaded
    if (!has_files) {
        try repl.runWithFiles(fs.File.stdin(), stdout);
    }
}
