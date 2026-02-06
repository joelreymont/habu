const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const Vm = @import("src/interp/vm.zig").Vm;
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;

fn headName(v: Value) []const u8 {
    if (!v.isCons()) return "<atom>";
    const c = v.toPtr(Cons);
    if (!c.car.isSymbol()) return "<non-sym>";
    return c.car.toPtr(Symbol).getName();
}

fn printVal(v: Value, depth: u8) void {
    if (depth == 0) { std.debug.print("...", .{}); return; }
    switch (v.typeKind()) {
        .nil => std.debug.print("nil", .{}),
        .fixnum => std.debug.print("{d}", .{v.toFixnum()}),
        .float => std.debug.print("<float:{d}>", .{v.toFloat()}),
        .bignum => {
            const b = v.toPtr(runtime.Bignum);
            const sz: usize = @intCast(if (b.size < 0) -b.size else b.size);
            const neg = b.size < 0;
            std.debug.print("<bignum size={d} neg={any} limb0={d}>", .{ sz, neg, if (sz != 0) b.limbs[0] else 0 });
        },
        .symbol => std.debug.print("{s}", .{v.toPtr(Symbol).getName()}),
        .cons => {
            std.debug.print("(", .{});
            var list = v;
            var first = true;
            var n: usize = 0;
            while (list.isCons() and n < 12) : (n += 1) {
                const cc = list.toPtr(Cons);
                if (!first) std.debug.print(" ", .{});
                first = false;
                printVal(cc.car, depth - 1);
                list = cc.cdr;
            }
            if (!list.isNil()) { std.debug.print(" . ", .{}); printVal(list, depth - 1); }
            std.debug.print(")", .{});
        },
        .string => std.debug.print("\"<str>\"", .{}),
        else => std.debug.print("<{s}>", .{@tagName(v.typeKind())}),
    }
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) return;

    const src = try std.fs.cwd().readFileAlloc(alloc, args[1], 256 * 1024 * 1024);
    defer alloc.free(src);

    var heap = try runtime.Heap.init(alloc, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(alloc, &heap);
    defer vm.deinit();

    var parser = try parser_mod.Parser.init(alloc, &heap, src, &vm.builtins);
    defer parser.deinit();

    var idx: usize = 0;
    while (parser.current.kind != .eof) {
        const expr = try parser.parse();
        idx += 1;
        if (idx >= 70 and idx <= 95) {
            std.debug.print("form {d}: {s} ", .{ idx, headName(expr) });
            printVal(expr, 8);
            std.debug.print("\n", .{});
        }
    }
}
