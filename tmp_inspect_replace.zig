const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const repl_mod = @import("src/interp/repl.zig");
const disasm = @import("src/bytecode/disasm.zig");

pub fn main() !void {
    var repl: repl_mod.Repl = undefined;
    var heap = try runtime.Heap.init(std.heap.page_allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    try repl.init(std.heap.page_allocator, &heap, .{});
    defer repl.deinit();

    const fn_val = try repl.eval("(symbol-function 'replace)");
    if (!fn_val.isClosure()) {
        std.debug.print("replace is not closure: {s}\n", .{@tagName(fn_val.typeKind())});
        return;
    }

    const cl = fn_val.toPtr(runtime.Closure);
    if (!cl.code.isChunk()) {
        std.debug.print("replace closure code kind: {s}\n", .{@tagName(cl.code.typeKind())});
        return;
    }

    const chunk = cl.code.toPtr(runtime.Chunk);
    std.debug.print("name={s} arity={d} opt={d} key={d} rest={d} locals={d} allow_other_keys={d}\n", .{
        if (chunk.name.isSymbol()) chunk.name.toPtr(runtime.Symbol).getName() else "<non-symbol>",
        chunk.arity,
        chunk.opt_count,
        chunk.key_count,
        chunk.has_rest,
        chunk.num_locals,
        chunk.allow_other_keys,
    });

    std.debug.print("allowed_keywords={s}\n", .{@tagName(chunk.allowed_keywords.typeKind())});

    const stdout = std.fs.File.stdout();
    var buf: [8192]u8 = undefined;
    var w = stdout.writer(&buf);
    const out = &w.interface;
    try disasm.disassembleRuntime(chunk, out);
    try out.flush();
}
