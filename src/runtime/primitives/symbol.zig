//! Symbol primitives
//!
//! gensym, symbol-name, symbol-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");
const list_prims = @import("list.zig");
const string_prims = @import("string.zig");

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
    var scratch: [256]u8 = undefined;
    const bytes = try string_prims.designatorBytes(heap.backing_allocator, name, scratch[0..]);
    defer bytes.deinit(heap.backing_allocator);
    return try heap.allocSymbol(bytes.slice);
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

const PlistEntry = struct {
    indicator: Value,
    value: Value,
    next: Value,
};

fn nextPlistEntry(cur: Value) ?PlistEntry {
    if (!cur.isCons()) return null;
    const cell = cur.toPtr(objects.Cons);

    if (cell.car.isCons()) {
        const pair = cell.car.toPtr(objects.Cons);
        return .{
            .indicator = pair.car,
            .value = pair.cdr,
            .next = cell.cdr,
        };
    }

    if (cell.cdr.isCons()) {
        const value_cell = cell.cdr.toPtr(objects.Cons);
        return .{
            .indicator = cell.car,
            .value = value_cell.car,
            .next = value_cell.cdr,
        };
    }

    return .{
        .indicator = cell.car,
        .value = Value.nil,
        .next = Value.nil,
    };
}

fn allocConsRooted(heap: *Heap, car_val: Value, cdr_val: Value, roots: []Value) !Value {
    const cons = try heap.allocWithGC(objects.Cons, roots);
    cons.* = objects.Cons.init(car_val, cdr_val);
    return Value.makeCons(cons);
}

fn reverseProperList(heap: *Heap, list: Value) Value {
    var prev = Value.nil;
    var cur = list;
    while (cur.isCons()) {
        const cell = cur.toPtr(objects.Cons);
        const next = cell.cdr;
        cell.cdr = prev;
        heap.writeBarrier(cur, prev);
        prev = cur;
        cur = next;
    }
    return prev;
}

fn plistToFlat(heap: *Heap, plist: Value) !Value {
    var cur = plist;
    var out = Value.nil;
    while (nextPlistEntry(cur)) |entry| {
        const indicator = entry.indicator;
        const value = entry.value;
        const next = entry.next;

        var roots = [_]Value{ cur, out, indicator, value };
        const ind_cell = try allocConsRooted(heap, roots[2], roots[1], roots[0..]);
        roots[1] = ind_cell;
        const val_cell = try allocConsRooted(heap, roots[3], roots[1], roots[0..]);
        out = val_cell;
        cur = next;
    }

    if (!cur.isNil()) {
        var roots = [_]Value{ cur, out };
        const ind_cell = try allocConsRooted(heap, roots[0], roots[1], roots[0..]);
        roots[1] = ind_cell;
        out = try allocConsRooted(heap, Value.nil, roots[1], roots[0..]);
    }

    return reverseProperList(heap, out);
}

fn flatToAList(heap: *Heap, plist: Value) !Value {
    var cur = plist;
    var out = Value.nil;
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

        var roots = [_]Value{ cur, out, indicator, value };
        const pair = try allocConsRooted(heap, roots[2], roots[3], roots[0..]);
        roots[1] = out;
        roots[2] = pair;
        out = try allocConsRooted(heap, roots[2], roots[1], roots[0..3]);
        cur = next;
    }

    if (!cur.isNil()) {
        var roots = [_]Value{ cur, out };
        const pair = try allocConsRooted(heap, roots[0], Value.nil, roots[0..]);
        roots[0] = pair;
        out = try allocConsRooted(heap, roots[0], roots[1], roots[0..]);
    }

    return reverseProperList(heap, out);
}

/// Get symbol's property list
pub fn symbolPlist(heap: *Heap, sym: Value) !Value {
    if (!sym.isSymbolLike()) return error.TypeError;
    const plist = if (sym.isNil())
        heap.nil_symbol_plist
    else if (sym.isT())
        heap.t_symbol_plist
    else if (sym.isKeyword())
        heap.getKeywordPlist(sym)
    else
        sym.toPtr(objects.Symbol).plist;
    if (plist.isNil()) return Value.nil;
    if (plist.isCons()) return try plistToFlat(heap, plist);
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
    if (!sym.isSymbolLike()) {
        if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null) {
            std.debug.print("TRACE setSymbolPlist type-mismatch kind={s} raw=0x{x} plist_raw=0x{x}\n", .{
                @tagName(sym.typeKind()),
                sym.raw,
                plist.raw,
            });
        }
        return error.TypeError;
    }
    if (sym.isKeyword()) {
        if (plist.isNil() or !plist.isCons() or plistLooksLikeAList(plist)) {
            try heap.setKeywordPlist(sym, plist);
        } else {
            const roots = [_]Value{ sym, plist };
            const alist = try flatToAList(heap, roots[1]);
            try heap.setKeywordPlist(roots[0], alist);
        }
        return;
    }

    const plist_ptr = if (sym.isNil())
        &heap.nil_symbol_plist
    else if (sym.isT())
        &heap.t_symbol_plist
    else
        &sym.toPtr(objects.Symbol).plist;

    if (plist.isNil() or !plist.isCons()) {
        plist_ptr.* = plist;
        if (sym.isSymbol()) heap.writeBarrier(sym, plist);
        return;
    }

    if (plistLooksLikeAList(plist)) {
        plist_ptr.* = plist;
        if (sym.isSymbol()) heap.writeBarrier(sym, plist);
        return;
    }

    const roots = [_]Value{ sym, plist };
    const alist = try flatToAList(heap, roots[1]);
    const sym_live = roots[0];
    const plist_ptr_live = if (sym_live.isNil())
        &heap.nil_symbol_plist
    else if (sym_live.isT())
        &heap.t_symbol_plist
    else
        &sym_live.toPtr(objects.Symbol).plist;
    plist_ptr_live.* = alist;
    if (sym_live.isSymbol()) heap.writeBarrier(sym_live, alist);
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
    const got_k1 = try list_prims.get(&heap, sym, k1);
    const got_k2 = try list_prims.get(&heap, sym, k2);
    try testing.expectEqual(@as(i64, 11), got_k1.toFixnum());
    try testing.expectEqual(@as(i64, 22), got_k2.toFixnum());
}

test "plist ops support NIL and T symbols" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const key = try heap.intern("PLIST-KEY");
    const kw_sym = try heap.internKeyword("extended-number");

    _ = try list_prims.put(&heap, Value.nil, key, Value.makeFixnum(7));
    _ = try list_prims.put(&heap, Value.t, key, Value.makeFixnum(9));
    _ = try list_prims.put(&heap, kw_sym, key, Value.makeFixnum(11));

    const nil_val = try list_prims.get(&heap, Value.nil, key);
    const t_val = try list_prims.get(&heap, Value.t, key);
    const kw_val = try list_prims.get(&heap, kw_sym, key);
    try testing.expectEqual(@as(i64, 7), nil_val.toFixnum());
    try testing.expectEqual(@as(i64, 9), t_val.toFixnum());
    try testing.expectEqual(@as(i64, 11), kw_val.toFixnum());

    try testing.expect((try list_prims.remprop(&heap, Value.nil, key)).isT());
    try testing.expect((try list_prims.remprop(&heap, Value.t, key)).isT());
    try testing.expect((try list_prims.remprop(&heap, kw_sym, key)).isT());
    try testing.expect((try list_prims.get(&heap, Value.nil, key)).isNil());
    try testing.expect((try list_prims.get(&heap, Value.t, key)).isNil());
    try testing.expect((try list_prims.get(&heap, kw_sym, key)).isNil());
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
