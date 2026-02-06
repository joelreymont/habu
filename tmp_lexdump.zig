const std = @import("std");
const lex = @import("src/reader/lexer.zig");

pub fn main() !void {
    const alloc = std.heap.page_allocator;
    const args = try std.process.argsAlloc(alloc);
    defer std.process.argsFree(alloc, args);
    if (args.len < 2) return;
    const path = args[1];
    const src = try std.fs.cwd().readFileAlloc(alloc, path, 20 * 1024 * 1024);
    defer alloc.free(src);

    var l = lex.Lexer.init(src);
    var i: usize = 0;
    while (true) : (i += 1) {
        const t = l.next();
        if (t.kind == .err or t.kind == .eof) {
            std.debug.print("idx={d} kind={s} line={d} col={d} text=<{s}>\n", .{ i, @tagName(t.kind), t.line, t.column, t.text });
            break;
        }
    }
}
