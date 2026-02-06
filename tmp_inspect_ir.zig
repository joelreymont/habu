const std = @import("std");
const compiler_mod = @import("src/compiler/compiler.zig");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const Vm = @import("src/interp/vm.zig").Vm;

const Ir = compiler_mod.Ir;
const Compiler = compiler_mod.Compiler;
const Env = compiler_mod.Env;

fn dump(node: *const Ir, depth: usize) void {
    const pad = "                                ";
    const p = pad[0..@min(depth * 2, pad.len)];
    switch (node.*) {
        .let => |l| {
            std.debug.print("{s}let\n", .{p});
            for (l.bindings, 0..) |b, i| {
                std.debug.print("{s}  bind[{d}] name={s} idx={d}\n", .{ p, i, b.name, b.index });
                dump(b.value, depth + 2);
            }
            dump(l.body, depth + 1);
        },
        .@"var" => |v| {
            std.debug.print("{s}var name={s} depth={d} idx={d}\n", .{ p, v.name, v.depth, v.index });
        },
        .add => |op| {
            std.debug.print("{s}add\n", .{p});
            dump(op.left, depth + 1);
            dump(op.right, depth + 1);
        },
        .lit => |v| {
            std.debug.print("{s}lit kind={s}\n", .{ p, @tagName(v.typeKind()) });
        },
        else => {
            std.debug.print("{s}{s}\n", .{ p, @tagName(node.*) });
        },
    }
}

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const src = "(lambda () (let* ((x 1) (x (+ x 1))) x))\n";

    var heap = try runtime.Heap.init(alloc, .{ .total_size = 128 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(alloc, &heap);
    defer vm.deinit();

    var parser = try parser_mod.Parser.init(alloc, &heap, src, &vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();

    var comp = try Compiler.initWithHeap(alloc, &vm);
    defer comp.deinit();

    var env = Env.init(alloc, null);
    defer env.deinit();

    const ir = try comp.compile(expr, &env);
    dump(ir, 0);
}
