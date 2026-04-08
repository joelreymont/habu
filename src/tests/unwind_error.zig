const std = @import("std");
const testing = std.testing;

const Repl = @import("../interp/repl.zig").Repl;
const Heap = @import("../runtime/heap.zig").Heap;
const Value = @import("../runtime/runtime.zig").Value;

fn loadStdlib(repl: *Repl) !void {
    const null_writer = std.io.null_writer;
    const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    defer file.close();
    const content = try file.readToEndAlloc(repl.allocator, 16 * 1024 * 1024);
    defer repl.allocator.free(content);
    try repl.evalFile(content, null_writer);
}

test "unwind-protect runs cleanup when protected form errors" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 32 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const ran = try repl.eval(
        \\(let ((cell (cons nil nil)))
        \\  (handler-case
        \\      (unwind-protect
        \\          (symbol-package 42)
        \\        (rplaca cell t))
        \\    (type-error (c)
        \\      (declare (ignore c))
        \\      (car cell))))
    );
    try testing.expect(ran.eq(Value.t));
}
