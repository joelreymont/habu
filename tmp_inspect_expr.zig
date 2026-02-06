const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const Vm = @import("src/interp/vm.zig").Vm;
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;

fn symName(v: Value) []const u8 {
    if (v.typeKind() != .symbol) return "<not-symbol>";
    return v.toPtr(Symbol).getName();
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const src = "(let* ((x 1) (x (+ x 1))) x)\n";

    var heap = try runtime.Heap.init(alloc, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(alloc, &heap);
    defer vm.deinit();

    var parser = try parser_mod.Parser.init(alloc, &heap, src, &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    const c = expr.toPtr(Cons);
    const head = c.car;
    std.debug.print("head={s} raw=0x{x}\n", .{ symName(head), head.raw });

    const tail = c.cdr;
    const args = tail.toPtr(Cons);
    const binds = args.car;
    var bl = binds;
    var i: usize = 0;
    while (bl.isCons()) : (i += 1) {
        const b = bl.toPtr(Cons).car;
        if (b.isCons()) {
            const bc = b.toPtr(Cons);
            const name = bc.car;
            std.debug.print("bind[{d}] name={s} raw=0x{x}\n", .{ i, symName(name), name.raw });
            if (bc.cdr.isCons()) {
                const v = bc.cdr.toPtr(Cons).car;
                if (v.isCons()) {
                    const vc = v.toPtr(Cons);
                    if (vc.car.typeKind() == .symbol) {
                        std.debug.print("  rhs head={s}\n", .{symName(vc.car)});
                    }
                    if (vc.cdr.isCons()) {
                        const arg1 = vc.cdr.toPtr(Cons).car;
                        std.debug.print("  rhs arg1 kind={s} raw=0x{x} name={s}\n", .{@tagName(arg1.typeKind()), arg1.raw, symName(arg1)});
                    }
                }
            }
        }
        bl = bl.toPtr(Cons).cdr;
    }
}
