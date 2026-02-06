const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const Vm = @import("src/interp/vm.zig").Vm;

fn dump(v: runtime.Value) void {
    var cur = v;
    std.debug.print("list=", .{});
    while (cur.isCons()) {
        const c = cur.toPtr(runtime.Cons);
        const x = c.car;
        switch (x.typeKind()) {
            .keyword => std.debug.print(":{s} ", .{x.toPtr(runtime.Keyword).getName()}),
            .symbol => std.debug.print("{s} ", .{x.toPtr(runtime.Symbol).getName()}),
            .string => std.debug.print("\"{s}\" ", .{x.toPtr(runtime.String).bytes()}),
            .nil => std.debug.print("NIL ", .{}),
            else => std.debug.print("<{s}> ", .{@tagName(x.typeKind())}),
        }
        cur = c.cdr;
    }
    if (!cur.isNil()) std.debug.print(" . <{s}>", .{@tagName(cur.typeKind())});
    std.debug.print("\n", .{});
}

pub fn main() !void {
    var heap = try runtime.Heap.init(std.heap.page_allocator, .{});
    defer heap.deinit();
    var vm = try Vm.init(std.heap.page_allocator, &heap);
    defer vm.deinit();

    var p = try parser_mod.Parser.init(std.heap.page_allocator, &heap, "'(#-wcl :cl #+wcl :lisp)", &vm.builtins);
    defer p.deinit();
    const expr = try p.parse();
    const q = expr.toPtr(runtime.Cons);
    const payload = q.cdr.toPtr(runtime.Cons).car;
    dump(payload);
}
