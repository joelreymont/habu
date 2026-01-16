//! Symbol primitives
//!
//! gensym, symbol-name, symbol-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");

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
    const name = std.fmt.bufPrint(&buf, "{s}{d}", .{ prefix_str, count }) catch "GENSYM";

    return try heap.allocSymbol(name);
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

    var buf: [256]u8 = undefined;
    var attempt: usize = 0;
    while (attempt < 1000) : (attempt += 1) {
        const name = std.fmt.bufPrint(&buf, "{s}{d}", .{ prefix_str, count + attempt }) catch "TEMP";
        const sym = heap.intern(name);
        const sym_ptr = sym.toPtr(objects.Symbol);
        if (sym_ptr.value.isNil() and sym_ptr.function.isNil()) {
            counter.* = count + attempt + 1;
            return sym;
        }
    }

    return error.OutOfMemory;
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
