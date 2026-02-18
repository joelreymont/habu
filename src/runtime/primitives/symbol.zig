//! Symbol primitives
//!
//! gensym, symbol-name, symbol-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");
const list_prims = @import("list.zig");

const NameBuf = struct {
    name: []const u8,
    owned: bool,
};

fn formatName(allocator: std.mem.Allocator, prefix: []const u8, count: u64, buf: *[256]u8) !NameBuf {
    const needed = std.fmt.count("{s}{d}", .{ prefix, count });
    if (needed <= buf.len) {
        const name = try std.fmt.bufPrint(buf, "{s}{d}", .{ prefix, count });
        return .{ .name = name, .owned = false };
    }
    const name = try std.fmt.allocPrint(allocator, "{s}{d}", .{ prefix, count });
    return .{ .name = name, .owned = true };
}

/// Generate a unique uninterned symbol (gensym)
/// Returns a new uninterned symbol with name prefix + counter
pub fn gensym(heap: *Heap, prefix: ?Value) !Value {
    const counter = &heap.gensym_counter;
    const count = counter.*;
    counter.* = count + 1;

    const prefix_str = if (prefix) |p| blk: {
        switch (p.typeKind()) {
            .string => break :blk p.toPtr(objects.String).bytes(),
            .symbol => break :blk p.toPtr(objects.Symbol).getName(),
            else => break :blk "G",
        }
    } else "G";

    var buf: [256]u8 = undefined;
    const name_info = try formatName(heap.backing_allocator, prefix_str, count, &buf);
    defer if (name_info.owned) heap.backing_allocator.free(name_info.name);

    return try heap.allocSymbol(name_info.name);
}

/// Create uninterned symbol with given name
pub fn makeSymbol(heap: *Heap, name: Value) !Value {
    const name_bytes: []const u8 = switch (name.typeKind()) {
        .string => name.toPtr(objects.String).bytes(),
        .symbol => name.toPtr(objects.Symbol).getName(),
        .char => blk: {
            var buf: [4]u8 = undefined;
            const cp: u21 = @intCast(name.toCharacter());
            const n = try std.unicode.utf8Encode(cp, &buf);
            break :blk buf[0..n];
        },
        .array => blk: {
            const arr = name.toPtr(objects.Array);
            if (arr.rank != 1 or arr.total_size != 0) return error.TypeError;
            break :blk "";
        },
        else => return error.TypeError,
    };
    return try heap.allocSymbol(name_bytes);
}

/// Copy symbol optionally copying properties
pub fn copySymbol(heap: *Heap, sym: Value, copy_props: ?Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const orig = sym.toPtr(objects.Symbol);

    const new_sym = try heap.allocSymbol(orig.getName());

    if (copy_props != null and !copy_props.?.isNil()) {
        const new_sym_ptr = new_sym.toPtr(objects.Symbol);
        new_sym_ptr.plist = orig.plist;
    }

    return new_sym;
}

/// Generate temporary interned symbol with unique name
pub fn gentemp(heap: *Heap, prefix: ?Value, package: ?Value) !Value {
    _ = package;

    const counter = &heap.gentemp_counter;
    const count = counter.*;
    counter.* = count + 1;

    const prefix_str = if (prefix) |p| blk: {
        switch (p.typeKind()) {
            .string => break :blk p.toPtr(objects.String).bytes(),
            .symbol => break :blk p.toPtr(objects.Symbol).getName(),
            else => break :blk "T",
        }
    } else "T";

    const pkg = if (heap.current_package) |val| val else return error.InvalidArgument;
    var buf: [256]u8 = undefined;
    var attempt: u64 = 0;
    while (true) : (attempt += 1) {
        const name_count = count + attempt;
        if (name_count < count) return error.OutOfMemory;
        const name_info = try formatName(heap.backing_allocator, prefix_str, name_count, &buf);
        {
            defer if (name_info.owned) heap.backing_allocator.free(name_info.name);

            if (try pkg.findAccessible(name_info.name) != null) continue;
            const sym = try pkg.intern(heap, name_info.name);
            counter.* = name_count + 1;
            return sym;
        }
    }
}

/// Get symbol's home package
pub fn symbolPackage(sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    return s.package;
}

fn plistLooksLikeAList(plist: Value) bool {
    if (!plist.isCons()) return false;
    const first = plist.toPtr(objects.Cons);
    return first.car.isCons();
}

fn alistToFlat(heap: *Heap, plist: Value) !Value {
    var elems = std.ArrayList(Value){};
    defer elems.deinit(heap.backing_allocator);

    var cur = plist;
    while (cur.isCons()) {
        const node = cur.toPtr(objects.Cons);
        const entry = node.car;

        if (entry.isCons()) {
            const pair = entry.toPtr(objects.Cons);
            try elems.append(heap.backing_allocator, pair.car);
            try elems.append(heap.backing_allocator, pair.cdr);
        } else {
            try elems.append(heap.backing_allocator, entry);
            try elems.append(heap.backing_allocator, Value.nil);
        }

        cur = node.cdr;
    }

    if (!cur.isNil()) {
        try elems.append(heap.backing_allocator, cur);
        try elems.append(heap.backing_allocator, Value.nil);
    }

    var out = Value.nil;
    var i = elems.items.len;
    while (i > 0) {
        i -= 1;
        out = try heap.allocCons(elems.items[i], out);
    }
    return out;
}

fn flatToAList(heap: *Heap, plist: Value) !Value {
    var entries = std.ArrayList(Value){};
    defer entries.deinit(heap.backing_allocator);

    var cur = plist;
    while (cur.isCons()) {
        const ind_cell = cur.toPtr(objects.Cons);
        const indicator = ind_cell.car;

        var value = Value.nil;
        var next = Value.nil;
        if (ind_cell.cdr.isCons()) {
            const val_cell = ind_cell.cdr.toPtr(objects.Cons);
            value = val_cell.car;
            next = val_cell.cdr;
        }

        const pair = try heap.allocCons(indicator, value);
        try entries.append(heap.backing_allocator, pair);
        cur = next;
    }

    if (!cur.isNil()) {
        const pair = try heap.allocCons(cur, Value.nil);
        try entries.append(heap.backing_allocator, pair);
    }

    var out = Value.nil;
    var i = entries.items.len;
    while (i > 0) {
        i -= 1;
        out = try heap.allocCons(entries.items[i], out);
    }
    return out;
}

/// Get symbol's property list
pub fn symbolPlist(heap: *Heap, sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    const plist = s.plist;
    if (plist.isNil()) return Value.nil;
    if (plistLooksLikeAList(plist)) return try alistToFlat(heap, plist);
    return plist;
}

/// Get symbol's function binding
pub fn symbolFunction(sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    return s.function;
}

/// Get symbol's dynamic value binding
pub fn symbolValue(sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    return s.value;
}

/// Set symbol's value (for special variables)
pub fn setSymbolValue(sym: Value, val: Value) !void {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    s.value = val;
}

/// Set symbol's property list
pub fn setSymbolPlist(heap: *Heap, sym: Value, plist: Value) !void {
    if (!sym.isSymbol()) {
        if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null) {
            std.debug.print("TRACE setSymbolPlist type-mismatch kind={s} raw=0x{x} plist_raw=0x{x}\n", .{
                @tagName(sym.typeKind()),
                sym.raw,
                plist.raw,
            });
        }
        return error.TypeError;
    }
    const s = sym.toPtr(objects.Symbol);
    if (plist.isNil() or !plist.isCons()) {
        s.plist = plist;
        heap.writeBarrier(sym, plist);
        return;
    }

    if (plistLooksLikeAList(plist)) {
        s.plist = plist;
        heap.writeBarrier(sym, plist);
        return;
    }

    s.plist = try flatToAList(heap, plist);
    heap.writeBarrier(sym, s.plist);
}

/// Test if symbol has value binding
pub fn boundp(sym: Value) bool {
    if (!sym.isSymbol()) return false;
    const s = sym.toPtr(objects.Symbol);
    return !s.value.isNil();
}

// ============================================================================
// Tests
// ============================================================================

test "gensym uses full prefix for long names" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var prefix_buf: [300]u8 = undefined;
    for (&prefix_buf) |*b| b.* = 'A';
    const prefix_str = prefix_buf[0..];
    const prefix_val = try heap.allocBaseString(prefix_str);

    const sym1 = try gensym(&heap, prefix_val);
    const sym2 = try gensym(&heap, prefix_val);

    const name1 = sym1.toPtr(objects.Symbol).getName();
    const name2 = sym2.toPtr(objects.Symbol).getName();

    try testing.expect(std.mem.startsWith(u8, name1, prefix_str));
    try testing.expect(std.mem.startsWith(u8, name2, prefix_str));
    try testing.expect(!std.mem.eql(u8, name1, name2));
}

test "gentemp skips existing symbol names" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    _ = try heap.intern("T0");
    const sym = try gentemp(&heap, null, null);
    const name = sym.toPtr(objects.Symbol).getName();
    try testing.expect(!std.mem.eql(u8, name, "T0"));
}

test "gensym accepts symbol prefix" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const prefix_sym = try heap.intern("PRE");
    const sym = try gensym(&heap, prefix_sym);
    const name = sym.toPtr(objects.Symbol).getName();
    try testing.expect(std.mem.startsWith(u8, name, "PRE"));
}

test "symbol-plist exposes flat plist for CL consumers" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const sym = try heap.intern("PLIST-SYM");
    const k1 = try heap.intern("K1");
    const k2 = try heap.intern("K2");

    _ = try list_prims.put(&heap, sym, k1, Value.makeFixnum(11));
    _ = try list_prims.put(&heap, sym, k2, Value.makeFixnum(22));

    const plist = try symbolPlist(&heap, sym);
    try testing.expect(plist.isCons());
    const c0 = plist.toPtr(objects.Cons);
    try testing.expect(c0.car.eq(k2));
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 22), c1.car.toFixnum());
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(objects.Cons);
    try testing.expect(c2.car.eq(k1));
    try testing.expect(c2.cdr.isCons());
    const c3 = c2.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 11), c3.car.toFixnum());

    try setSymbolPlist(&heap, sym, plist);
    const got_k1 = try list_prims.get(sym, k1);
    const got_k2 = try list_prims.get(sym, k2);
    try testing.expectEqual(@as(i64, 11), got_k1.toFixnum());
    try testing.expectEqual(@as(i64, 22), got_k2.toFixnum());
}

/// Test if symbol has function binding
pub fn fboundp(sym: Value) bool {
    if (!sym.isSymbol()) return false;
    const s = sym.toPtr(objects.Symbol);
    return !s.function.isNil();
}

/// Remove symbol's value binding
pub fn makunbound(sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    s.value = Value.nil;
    return sym;
}

/// Remove symbol's function binding
pub fn fmakunbound(sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    s.function = Value.nil;
    return sym;
}

/// Return list of symbols whose names contain substring
pub fn aproposList(heap: *Heap, substring: Value, package: ?Value) !Value {
    _ = package;
    if (!substring.isString()) return error.TypeError;

    const search_str = substring.toPtr(objects.String).bytes();
    var result = Value.nil;

    // Search all packages
    var pkg_iter = heap.packages.iterator();
    while (pkg_iter.next()) |entry| {
        const pkg = entry.value_ptr.*;
        var sym_iter = pkg.symbols.iterator();
        while (sym_iter.next()) |sym_entry| {
            const name = sym_entry.key_ptr.*;
            const sym = sym_entry.value_ptr.*;
            if (std.mem.indexOf(u8, name, search_str) != null) {
                result = try heap.allocCons(sym, result);
            }
        }
    }

    return result;
}

/// Print symbols whose names contain substring (interactive)
pub fn apropos(heap: *Heap, substring: Value, package: ?Value) !Value {
    const matches = try aproposList(heap, substring, package);

    var curr = matches;
    var buf: [4096]u8 = undefined;
    const stdout = std.fs.File.stdout();
    var writer = stdout.writer(&buf);
    const w = &writer.interface;
    while (!curr.isNil()) {
        const pair = curr.toPtr(objects.Cons);
        const sym = pair.car;
        const sym_ptr = sym.toPtr(objects.Symbol);
        const name = sym_ptr.getName();

        try w.writeAll(name);
        try w.writeAll("\n");

        curr = pair.cdr;
    }
    try w.flush();

    return Value.nil;
}
