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
    return qualSymWithHeap(allocator, null, sym, buf);
}

pub fn qualSymWithHeap(
    allocator: ?std.mem.Allocator,
    heap: ?*const heap_mod.Heap,
    sym: *const objects.Symbol,
    buf: []u8,
) !QualName {
    const sym_addr = @intFromPtr(sym);
    const sym_name_len = std.math.cast(usize, sym.name_len) orelse {
        std.debug.print(
            "TRACE bad-qual-sym bad-len bits=0x{x} sym=0x{x} name_len={d}\n",
            .{ sym.reserved, sym_addr, sym.name_len },
        );
        @panic("corrupt symbol length in qualSymWithHeap");
    };
    const sym_name_ptr = @intFromPtr(sym.name_ptr);
    const sym_expected = sym_addr + @sizeOf(objects.Symbol);
    if (sym_name_ptr != sym_expected or sym_name_len > 1_048_576) {
        std.debug.print(
            "TRACE bad-qual-sym bad-sym bits=0x{x} sym=0x{x} name_ptr=0x{x} expected=0x{x} name_len={d}\n",
            .{ sym.reserved, sym_addr, sym_name_ptr, sym_expected, sym_name_len },
        );
        @panic("corrupt symbol object in qualSymWithHeap");
    }
    const local_name = sym.name_ptr[0..sym_name_len];

    if (heap) |h| {
        const pkg = h.symbolHomePkg(sym) orelse return .{ .name = local_name, .owned = false };
        const pkg_name_ptr = @intFromPtr(pkg.name.ptr);
        if (pkg_name_ptr < 0x1000 or pkg.name.len > 1_048_576) {
            std.debug.print(
                "TRACE bad-qual-sym bad-pkg sym={s} bits=0x{x} pkg=0x{x} name_ptr=0x{x} name_len={d}\n",
                .{ local_name, sym.reserved, @intFromPtr(pkg), pkg_name_ptr, pkg.name.len },
            );
            @panic("corrupt package in qualSymWithHeap");
        }
        return qualName(allocator, pkg.name, local_name, buf);
    }
    const pkg_bits = sym.reserved;
    // Symbol.reserved is either a *Package pointer (low-bit 0) or an uninterned uid (low-bit 1).
    if (pkg_bits == 0 or (pkg_bits & 1) != 0) return .{ .name = local_name, .owned = false };
    const pkg: *const heap_mod.Package = @ptrFromInt(pkg_bits);
    return qualName(allocator, pkg.name, local_name, buf);
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

test "qualSymWithHeap ignores stale package pointers" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const sym = try heap.allocSymbol("FOO");
    const sym_ptr = sym.toPtr(objects.Symbol);
    sym_ptr.reserved = 0x2000;

    var buf: [64]u8 = undefined;
    const q = try qualSymWithHeap(testing.allocator, &heap, sym_ptr, &buf);
    defer if (q.owned) testing.allocator.free(q.name);

    try testing.expectEqualStrings("FOO", q.name);
    try testing.expect(!q.owned);
}
