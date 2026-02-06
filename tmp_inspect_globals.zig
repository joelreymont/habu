const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const compiler_mod = @import("src/compiler/compiler.zig");
const Vm = @import("src/interp/vm.zig").Vm;

pub fn main() !void {
    const gpa = std.heap.page_allocator;
    var heap = try runtime.Heap.init(gpa, .{ .total_size = 128 * 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(gpa, &heap);
    defer vm.deinit();

    var comp = try compiler_mod.Compiler.initWithHeap(gpa, &vm);
    defer comp.deinit();
    var env = compiler_mod.Env.init(gpa, null);
    defer env.deinit();

    const src = "(defstruct (entry (:conc-name nil)) pend name props form test-function vals)\n";
    var parser = try parser_mod.Parser.init(gpa, &heap, src, &vm.builtins);
    defer parser.deinit();
    const expr = try parser.parse();
    _ = try comp.compile(expr, &env);

    var it = comp.globals.bindings.iterator();
    while (it.next()) |e| {
        const k = e.key_ptr.*;
        if (std.mem.indexOf(u8, k, "ENTRY") != null or std.mem.indexOf(u8, k, "entry") != null or std.mem.indexOf(u8, k, "COPY") != null) {
            std.debug.print("{s} -> {d}\n", .{ k, e.value_ptr.* });
        }
    }
}
