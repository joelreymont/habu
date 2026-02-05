const std = @import("std");
const Heap = @import("../runtime/heap.zig").Heap;
const Repl = @import("../interp/repl.zig").Repl;

test "stdlib.habu matches lib/stdlib.habu" {
    const testing = std.testing;

    const a = try std.fs.cwd().readFileAlloc(testing.allocator, "lib/stdlib.habu", 16 * 1024 * 1024);
    defer testing.allocator.free(a);

    const b = try std.fs.cwd().readFileAlloc(testing.allocator, "stdlib.habu", 16 * 1024 * 1024);
    defer testing.allocator.free(b);

    try testing.expectEqual(a.len, b.len);
    try testing.expect(std.mem.eql(u8, a, b));
}

fn loadStdlib(repl: *Repl) !void {
    const null_writer = std.io.null_writer;
    const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    defer file.close();
    const content = try file.readToEndAlloc(repl.allocator, 16 * 1024 * 1024);
    defer repl.allocator.free(content);
    try repl.evalFileContent(content, null_writer);
}

test "logical pathname translations load from host file" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const filename = "TESTHOST.translations.lisp";
    try std.fs.cwd().writeFile(.{
        .sub_path = filename,
        .data = "'((\"TEST;**;*.*\" \"/tmp/*.*\"))\n",
    });
    defer std.fs.cwd().deleteFile(filename) catch {};

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const roundtrip = try repl.eval(
        "(progn (set-logical-pathname-translations 'ROUNDTRIP '((\"A\" \"B\")))\n" ++
            "       (logical-pathname-translations 'ROUNDTRIP))",
    );
    try testing.expect(roundtrip.isCons());

    const loaded = try repl.eval(
        "(let ((ok (load-logical-pathname-translations 'TESTHOST))\n" ++
            "      (tr (logical-pathname-translations 'TESTHOST)))\n" ++
            "  (and ok (consp tr)))",
    );
    try testing.expect(!loaded.isNil());
}
