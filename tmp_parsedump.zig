const std = @import("std");
const runtime = @import("src/runtime/runtime.zig");
const parser_mod = @import("src/reader/parser.zig");
const Vm = @import("src/interp/vm.zig").Vm;

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) return;

    const src = try std.fs.cwd().readFileAlloc(alloc, args[1], 64 * 1024 * 1024);
    defer alloc.free(src);

    var heap = try runtime.Heap.init(alloc, .{ .total_size = 128 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(alloc, &heap);
    defer vm.deinit();

    var parser = try parser_mod.Parser.init(alloc, &heap, src, &vm.builtins);
    defer parser.deinit();

    var idx: usize = 0;
    while (parser.current.kind != .eof) {
        _ = parser.parse() catch |err| {
            const loc = parser.getErrorLocation();
            std.debug.print("parse_error form={d} err={s} line={d} col={d} token=<{s}> kind={s}\n", .{ idx + 1, @errorName(err), loc.line, loc.column, loc.text, @tagName(parser.current.kind) });
            return;
        };
        idx += 1;
    }
    std.debug.print("ok forms={d}\n", .{idx});
}
