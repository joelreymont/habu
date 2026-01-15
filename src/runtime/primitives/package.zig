// Package system primitives
//
// make-package, find-package, intern, export, import, use-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

/// Create a new package
pub fn makePackage(heap: *Heap, name: Value, nicknames: ?Value, use_list: ?Value) !Value {
    if (!name.isString() and !name.isSymbol()) return error.TypeError;

    const pkg_ptr = try heap.alloc(@sizeOf(objects.Package));
    const pkg: *objects.Package = @ptrFromInt(pkg_ptr);

    pkg.* = .{
        .kind = .package,
        .name = name,
        .nicknames = nicknames orelse Value.nil(),
        .use_list = use_list orelse Value.nil(),
        .exports = Value.nil(), // TODO: create hash table
        .symbols = Value.nil(), // TODO: create hash table
        .shadowing = Value.nil(),
    };

    return Value.fromPtr(pkg);
}

/// Find a package by name or nickname
pub fn findPackage(heap: *Heap, name: Value) ?Value {
    _ = heap;
    _ = name;
    // TODO: lookup in global package registry
    return null;
}

/// Get package name
pub fn packageName(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.name;
}

/// Get package nicknames
pub fn packageNicknames(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.nicknames;
}

/// Get package use-list
pub fn packageUseList(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.use_list;
}

/// List all packages
pub fn listAllPackages(heap: *Heap) !Value {
    _ = heap;
    // TODO: return list from global registry
    return Value.nil();
}

/// Intern a symbol in a package
pub fn internSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    _ = heap;
    _ = name;
    _ = pkg;
    // TODO: implement symbol interning in package-specific table
    return Value.nil();
}

/// Find a symbol in a package
pub fn findSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    _ = heap;
    _ = name;
    _ = pkg;
    // TODO: search package and use-list
    return Value.nil();
}

/// Export symbols from a package
pub fn exportSymbols(symbols: Value, pkg: Value) !void {
    _ = symbols;
    _ = pkg;
    // TODO: add to export table
}

/// Import symbols into a package
pub fn importSymbols(symbols: Value, pkg: Value) !void {
    _ = symbols;
    _ = pkg;
    // TODO: add to internal table
}

/// Shadow symbols in a package
pub fn shadowSymbols(names: Value, pkg: Value) !void {
    _ = names;
    _ = pkg;
    // TODO: add to shadowing list
}

/// Shadowing import
pub fn shadowingImport(symbols: Value, pkg: Value) !void {
    _ = symbols;
    _ = pkg;
    // TODO: import + shadow
}

/// Add packages to use-list
pub fn usePackage(pkgs_to_use: Value, pkg: Value) !void {
    _ = pkgs_to_use;
    _ = pkg;
    // TODO: add to use-list, inherit external symbols
}

/// Remove packages from use-list
pub fn unusePackage(pkgs_to_unuse: Value, pkg: Value) !void {
    _ = pkgs_to_unuse;
    _ = pkg;
    // TODO: remove from use-list
}

/// Unexport symbols
pub fn unexportSymbols(symbols: Value, pkg: Value) !void {
    _ = symbols;
    _ = pkg;
    // TODO: remove from export table
}

/// Remove symbol from package
pub fn uninternSymbol(symbol: Value, pkg: Value) !bool {
    _ = symbol;
    _ = pkg;
    // TODO: remove from symbol table
    return false;
}

/// Delete a package
pub fn deletePackage(heap: *Heap, pkg: Value) !bool {
    _ = heap;
    _ = pkg;
    // TODO: remove from global registry
    return false;
}

/// Rename a package
pub fn renamePackage(pkg: Value, new_name: Value, new_nicknames: ?Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    p.name = new_name;
    if (new_nicknames) |nn| p.nicknames = nn;
    return pkg;
}
