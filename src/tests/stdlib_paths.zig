const std = @import("std");

test "stdlib.habu matches lib/stdlib.habu" {
    const testing = std.testing;

    const a = try std.fs.cwd().readFileAlloc(testing.allocator, "lib/stdlib.habu", 16 * 1024 * 1024);
    defer testing.allocator.free(a);

    const b = try std.fs.cwd().readFileAlloc(testing.allocator, "stdlib.habu", 16 * 1024 * 1024);
    defer testing.allocator.free(b);

    try testing.expectEqual(a.len, b.len);
    try testing.expect(std.mem.eql(u8, a, b));
}

