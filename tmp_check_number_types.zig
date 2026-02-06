const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const repl_mod = @import("src/interp/repl.zig");

fn dump(name: []const u8, v: runtime.Value) void {
    std.debug.print("{s}: kind={s}", .{ name, @tagName(v.typeKind()) });
    switch (v.typeKind()) {
        .fixnum => std.debug.print(" val={d}", .{v.toFixnum()}),
        .float => std.debug.print(" val={d}", .{v.toFloat()}),
        .bignum => {
            const b = v.toPtr(runtime.Bignum);
            std.debug.print(" size={d} sign={d} limb0={d}", .{ b.size, b.sign, if (b.size != 0) b.limbs[0] else 0 });
        },
        .symbol => std.debug.print(" sym={s}", .{v.toPtr(runtime.Symbol).getName()}),
        else => {},
    }
    std.debug.print("\n", .{});
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    var heap = try runtime.Heap.init(alloc, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    const mpf = try repl_mod.evalString(alloc, &heap, "most-positive-fixnum");
    dump("mpf", mpf);
    const mnf = try repl_mod.evalString(alloc, &heap, "most-negative-fixnum");
    dump("mnf", mnf);
    const n = try repl_mod.evalString(alloc, &heap, "(- most-positive-fixnum most-negative-fixnum)");
    dump("n", n);
    const n2 = try repl_mod.evalString(alloc, &heap, "(* 1000 (- most-positive-fixnum most-negative-fixnum))");
    dump("n2", n2);
    const rr = try repl_mod.evalString(alloc, &heap, "(random (- most-positive-fixnum most-negative-fixnum))");
    dump("random_n", rr);
}
