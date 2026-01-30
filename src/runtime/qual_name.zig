const std = @import("std");
const objects = @import("objects.zig");
const heap_mod = @import("heap.zig");

pub const QualName = struct {
    name: []const u8,
    owned: bool,
};

pub fn qualName(allocator: ?std.mem.Allocator, pkg: []const u8, local: []const u8, buf: []u8) !QualName {
    const need = pkg.len + 1 + local.len;
    if (need <= buf.len) {
        std.mem.copyForwards(u8, buf[0..pkg.len], pkg);
        buf[pkg.len] = ':';
        std.mem.copyForwards(u8, buf[pkg.len + 1 .. need], local);
        return .{ .name = buf[0..need], .owned = false };
    }
    if (allocator) |alloc| {
        const name = try std.fmt.allocPrint(alloc, "{s}:{s}", .{ pkg, local });
        return .{ .name = name, .owned = true };
    }
    return error.NoSpaceLeft;
}

pub fn qualSym(allocator: ?std.mem.Allocator, sym: *const objects.Symbol, buf: []u8) !QualName {
    const pkg_ptr = sym.reserved;
    if (pkg_ptr == 0) return .{ .name = sym.getName(), .owned = false };
    const pkg: *const heap_mod.Package = @ptrFromInt(pkg_ptr);
    return qualName(allocator, pkg.name, sym.getName(), buf);
}

// ============================================================================
// Tests
// ============================================================================

test "qualName uses buffer when it fits" {
    const testing = std.testing;

    var buf: [16]u8 = undefined;
    const q = try qualName(null, "PKG", "SYM", &buf);
    try testing.expectEqualStrings("PKG:SYM", q.name);
    try testing.expect(!q.owned);
}

test "qualName allocates when buffer is too small" {
    const testing = std.testing;

    var buf: [4]u8 = undefined;
    const q = try qualName(testing.allocator, "LONGPKG", "NAME", &buf);
    defer if (q.owned) testing.allocator.free(q.name);
    try testing.expectEqualStrings("LONGPKG:NAME", q.name);
    try testing.expect(q.owned);
}

test "qualSym uses package name" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const sym = try heap.intern("foo");
    try testing.expect(heap.current_package != null);
    const pkg = heap.current_package.?;

    var buf: [64]u8 = undefined;
    const q = try qualSym(testing.allocator, sym.toPtr(objects.Symbol), &buf);
    defer if (q.owned) testing.allocator.free(q.name);

    const expected = try std.fmt.allocPrint(testing.allocator, "{s}:FOO", .{ pkg.name });
    defer testing.allocator.free(expected);

    try testing.expectEqualStrings(expected, q.name);
    try testing.expect(!q.owned);
}
