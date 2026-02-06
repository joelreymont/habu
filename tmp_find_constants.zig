const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const Vm = @import("src/interp/vm.zig").Vm;
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;

fn sym(v: Value) ?[]const u8 {
    return if (v.isSymbol()) v.toPtr(Symbol).getName() else null;
}

fn dump(v: Value) void {
    switch (v.typeKind()) {
        .fixnum => std.debug.print("fixnum:{d}", .{v.toFixnum()}),
        .float => std.debug.print("float:{d}", .{v.toFloat()}),
        .bignum => {
            const b = v.toPtr(runtime.Bignum);
            const sz: usize = @intCast(if (b.size < 0) -b.size else b.size);
            std.debug.print("bignum(size={d},neg={any},limb0={d})", .{ sz, b.size < 0, if (sz > 0) b.limbs[0] else 0 });
        },
        .cons => std.debug.print("cons", .{}),
        .symbol => std.debug.print("sym:{s}", .{v.toPtr(Symbol).getName()}),
        else => std.debug.print("{s}", .{@tagName(v.typeKind())}),
    }
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const src = try std.fs.cwd().readFileAlloc(alloc, "lib/stdlib.habu", 256 * 1024 * 1024);
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
        if (!expr.isCons()) continue;
        const c = expr.toPtr(Cons);
        if (!c.car.isSymbol()) continue;
        if (!std.mem.eql(u8, c.car.toPtr(Symbol).getName(), "DEFCONSTANT")) continue;
        if (!c.cdr.isCons()) continue;
        const name_val = c.cdr.toPtr(Cons).car;
        const nm = sym(name_val) orelse continue;
        if (std.mem.eql(u8, nm, "MOST-POSITIVE-FIXNUM") or std.mem.eql(u8, nm, "MOST-NEGATIVE-FIXNUM") or std.mem.eql(u8, nm, "MOST-POSITIVE-LONG-FLOAT") or std.mem.eql(u8, nm, "MOST-NEGATIVE-LONG-FLOAT")) {
            std.debug.print("form {d} {s} = ", .{ idx, nm });
            const val_cell = c.cdr.toPtr(Cons).cdr;
            if (val_cell.isCons()) {
                dump(val_cell.toPtr(Cons).car);
            } else {
                std.debug.print("<missing>", .{});
            }
            std.debug.print("\n", .{});
        }
    }
}
