//! Symbol primitives
//!
//! gensym, symbol-name, symbol-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");

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
        if (p.isString()) {
            const s = p.toPtr(objects.String);
            break :blk s.bytes();
        } else if (p.isSymbol()) {
            const sym = p.toPtr(objects.Symbol);
            break :blk sym.getName();
        } else {
            break :blk "G";
        }
    } else "G";

    var buf: [256]u8 = undefined;
    const name_info = try formatName(heap.backing_allocator, prefix_str, count, &buf);
    defer if (name_info.owned) heap.backing_allocator.free(name_info.name);

    return try heap.allocSymbol(name_info.name);
}

/// Create uninterned symbol with given name
pub fn makeSymbol(heap: *Heap, name: Value) !Value {
    if (!name.isString()) return error.TypeError;
    const s = name.toPtr(objects.String);
    return try heap.allocSymbol(s.bytes());
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
        if (p.isString()) {
            const s = p.toPtr(objects.String);
            break :blk s.bytes();
        } else if (p.isSymbol()) {
            const sym_ptr = p.toPtr(objects.Symbol);
            break :blk sym_ptr.getName();
        } else {
            break :blk "T";
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

            if (pkg.findAccessible(name_info.name) != null) continue;
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

/// Get symbol's property list
pub fn symbolPlist(sym: Value) !Value {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    return s.plist;
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
pub fn setSymbolPlist(sym: Value, plist: Value) !void {
    if (!sym.isSymbol()) return error.TypeError;
    const s = sym.toPtr(objects.Symbol);
    s.plist = plist;
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
