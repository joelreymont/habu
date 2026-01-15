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
/// Returns (values symbol status) where status is :internal/:external/:inherited/nil
pub fn internSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Get string name
    const name_str = if (name.isString())
        name.toString()
    else if (name.isSymbol())
        name.toSymbol().getName()
    else
        return error.TypeError;

    // Check if symbol exists in internal table
    if (p.symbols.raw != Value.nil().raw) {
        const ht = p.symbols.toPtr(objects.HashTable);
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const entry = &ht.entries[i];
            if (entry.isEmpty() or entry.isDeleted()) continue;
            if (!entry.key.isSymbol()) continue;
            const sym = entry.key.toSymbol();
            const sym_name = sym.getName();
            if (std.mem.eql(u8, sym_name, name_str)) {
                // Found in internal table - check if exported
                if (p.exports.raw != Value.nil().raw) {
                    const exp = p.exports.toPtr(objects.HashTable);
                    var j: usize = 0;
                    while (j < exp.capacity) : (j += 1) {
                        const e = &exp.entries[j];
                        if (e.isEmpty() or e.isDeleted()) continue;
                        if (e.key.raw == entry.key.raw) {
                            const status = try heap.internKeyword("external");
                            return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil()));
                        }
                    }
                }
                const status = try heap.internKeyword("internal");
                return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil()));
            }
        }
    }

    // Check used packages for exported symbols
    var use = p.use_list;
    while (use.raw != Value.nil().raw) {
        if (!use.isCons()) break;
        const used_pkg = use.car();
        if (used_pkg.isPackage()) {
            const up = used_pkg.toPtr(objects.Package);
            if (up.exports.raw != Value.nil().raw) {
                const exp = up.exports.toPtr(objects.HashTable);
                var i: usize = 0;
                while (i < exp.capacity) : (i += 1) {
                    const entry = &exp.entries[i];
                    if (entry.isEmpty() or entry.isDeleted()) continue;
                    if (!entry.key.isSymbol()) continue;
                    const sym = entry.key.toSymbol();
                    const sym_name = sym.getName();
                    if (std.mem.eql(u8, sym_name, name_str)) {
                        const status = try heap.internKeyword("inherited");
                        return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil()));
                    }
                }
            }
        }
        use = use.cdr();
    }

    // Not found - create new internal symbol
    const new_sym = try heap.allocSymbol(name_str);

    // Add to symbols table (create if needed)
    if (p.symbols.raw == Value.nil().raw) {
        p.symbols = try createHashTable(heap, 16);
    }
    try insertHashTable(heap, p.symbols, new_sym, Value.t());

    const status = try heap.internKeyword("internal");
    return try heap.allocCons(new_sym, try heap.allocCons(status, Value.nil()));
}

fn createHashTable(heap: *Heap, capacity: usize) !Value {
    const size = @sizeOf(objects.HashTable) + capacity * @sizeOf(objects.HashEntry);
    const ptr = try heap.allocRaw(size);
    const ht: *objects.HashTable = @ptrCast(@alignCast(ptr));
    const entries: [*]objects.HashEntry = @ptrCast(ptr + @sizeOf(objects.HashTable));

    ht.* = .{
        .kind = .hash_table,
        .count = 0,
        .capacity = capacity,
        .entries = entries,
    };

    // Initialize all entries to empty
    var i: usize = 0;
    while (i < capacity) : (i += 1) {
        entries[i] = .{ .key = objects.HashEntry.EMPTY, .value = Value.nil() };
    }

    return Value.fromPtr(ht);
}

fn insertHashTable(heap: *Heap, table: Value, key: Value, value: Value) !void {
    _ = heap;
    if (!table.isHashTable()) return error.TypeError;
    const ht = table.toPtr(objects.HashTable);

    // Simple linear probing
    const hash = key.hash();
    var idx = hash % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const entry = &ht.entries[idx];
        if (entry.isEmpty() or entry.isDeleted() or entry.key.raw == key.raw) {
            const was_new = entry.isEmpty() or entry.isDeleted();
            entry.key = key;
            entry.value = value;
            if (was_new) ht.count += 1;
            return;
        }
        idx = (idx + 1) % ht.capacity;
    }
    return error.HashTableFull;
}

/// Find a symbol in a package
/// Returns (values symbol status) where status is :internal/:external/:inherited/nil
pub fn findSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Get string name
    const name_str = if (name.isString())
        name.toString()
    else if (name.isSymbol())
        name.toSymbol().getName()
    else
        return error.TypeError;

    // Check if symbol exists in internal table
    if (p.symbols.raw != Value.nil().raw) {
        const ht = p.symbols.toPtr(objects.HashTable);
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const entry = &ht.entries[i];
            if (entry.isEmpty() or entry.isDeleted()) continue;
            if (!entry.key.isSymbol()) continue;
            const sym = entry.key.toSymbol();
            const sym_name = sym.getName();
            if (std.mem.eql(u8, sym_name, name_str)) {
                // Found in internal table - check if exported
                if (p.exports.raw != Value.nil().raw) {
                    const exp = p.exports.toPtr(objects.HashTable);
                    var j: usize = 0;
                    while (j < exp.capacity) : (j += 1) {
                        const e = &exp.entries[j];
                        if (e.isEmpty() or e.isDeleted()) continue;
                        if (e.key.raw == entry.key.raw) {
                            const status = try heap.internKeyword("external");
                            return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil()));
                        }
                    }
                }
                const status = try heap.internKeyword("internal");
                return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil()));
            }
        }
    }

    // Check used packages for exported symbols
    var use = p.use_list;
    while (use.raw != Value.nil().raw) {
        if (!use.isCons()) break;
        const used_pkg = use.car();
        if (used_pkg.isPackage()) {
            const up = used_pkg.toPtr(objects.Package);
            if (up.exports.raw != Value.nil().raw) {
                const exp = up.exports.toPtr(objects.HashTable);
                var i: usize = 0;
                while (i < exp.capacity) : (i += 1) {
                    const entry = &exp.entries[i];
                    if (entry.isEmpty() or entry.isDeleted()) continue;
                    if (!entry.key.isSymbol()) continue;
                    const sym = entry.key.toSymbol();
                    const sym_name = sym.getName();
                    if (std.mem.eql(u8, sym_name, name_str)) {
                        const status = try heap.internKeyword("inherited");
                        return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil()));
                    }
                }
            }
        }
        use = use.cdr();
    }

    // Not found
    return try heap.allocCons(Value.nil(), try heap.allocCons(Value.nil(), Value.nil()));
}

/// Export symbols from a package
pub fn exportSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Create exports table if needed
    if (p.exports.raw == Value.nil().raw) {
        p.exports = try createHashTable(heap, 16);
    }

    // Handle single symbol or list
    if (symbols.isSymbol()) {
        try insertHashTable(heap, p.exports, symbols, Value.t());
    } else {
        var list = symbols;
        while (list.raw != Value.nil().raw) {
            if (!list.isCons()) return error.TypeError;
            const sym = list.car();
            if (!sym.isSymbol()) return error.TypeError;
            try insertHashTable(heap, p.exports, sym, Value.t());
            list = list.cdr();
        }
    }
}

/// Import symbols into a package
pub fn importSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Create symbols table if needed
    if (p.symbols.raw == Value.nil().raw) {
        p.symbols = try createHashTable(heap, 16);
    }

    // Handle single symbol or list
    if (symbols.isSymbol()) {
        try insertHashTable(heap, p.symbols, symbols, Value.t());
    } else {
        var list = symbols;
        while (list.raw != Value.nil().raw) {
            if (!list.isCons()) return error.TypeError;
            const sym = list.car();
            if (!sym.isSymbol()) return error.TypeError;
            try insertHashTable(heap, p.symbols, sym, Value.t());
            list = list.cdr();
        }
    }
}

/// Shadow symbols in a package
pub fn shadowSymbols(heap: *Heap, names: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Handle single name or list
    if (names.isSymbol() or names.isString()) {
        p.shadowing = try heap.allocCons(names, p.shadowing);
    } else {
        var list = names;
        while (list.raw != Value.nil().raw) {
            if (!list.isCons()) return error.TypeError;
            const name = list.car();
            if (!name.isSymbol() and !name.isString()) return error.TypeError;
            p.shadowing = try heap.allocCons(name, p.shadowing);
            list = list.cdr();
        }
    }
}

/// Shadowing import
pub fn shadowingImport(heap: *Heap, symbols: Value, pkg: Value) !void {
    try importSymbols(heap, symbols, pkg);
    try shadowSymbols(heap, symbols, pkg);
}

/// Add packages to use-list
pub fn usePackage(heap: *Heap, pkgs_to_use: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Handle single package or list
    if (pkgs_to_use.isPackage()) {
        p.use_list = try heap.allocCons(pkgs_to_use, p.use_list);
    } else {
        var list = pkgs_to_use;
        while (list.raw != Value.nil().raw) {
            if (!list.isCons()) return error.TypeError;
            const pkg_to_use = list.car();
            if (!pkg_to_use.isPackage()) return error.TypeError;
            p.use_list = try heap.allocCons(pkg_to_use, p.use_list);
            list = list.cdr();
        }
    }
}

/// Remove packages from use-list
pub fn unusePackage(heap: *Heap, pkgs_to_unuse: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Handle single package or list
    var to_remove = pkgs_to_unuse;
    if (pkgs_to_unuse.isPackage()) {
        to_remove = try heap.allocCons(pkgs_to_unuse, Value.nil());
    }

    // Filter use_list
    var new_use_list = Value.nil();
    var use = p.use_list;
    while (use.raw != Value.nil().raw) {
        if (!use.isCons()) break;
        const used_pkg = use.car();
        var should_keep = true;

        var rem = to_remove;
        while (rem.raw != Value.nil().raw) {
            if (!rem.isCons()) break;
            if (rem.car().raw == used_pkg.raw) {
                should_keep = false;
                break;
            }
            rem = rem.cdr();
        }

        if (should_keep) {
            new_use_list = try heap.allocCons(used_pkg, new_use_list);
        }
        use = use.cdr();
    }
    p.use_list = new_use_list;
}

/// Unexport symbols
pub fn unexportSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    if (p.exports.raw == Value.nil().raw) return;

    const ht = p.exports.toPtr(objects.HashTable);

    // Handle single symbol or list
    if (symbols.isSymbol()) {
        try removeFromHashTable(ht, symbols);
    } else {
        var list = symbols;
        while (list.raw != Value.nil().raw) {
            if (!list.isCons()) return error.TypeError;
            const sym = list.car();
            if (!sym.isSymbol()) return error.TypeError;
            try removeFromHashTable(ht, sym);
            list = list.cdr();
        }
    }
    _ = heap;
}

fn removeFromHashTable(ht: *objects.HashTable, key: Value) !void {
    const hash = key.hash();
    var idx = hash % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const entry = &ht.entries[idx];
        if (entry.isEmpty()) return;
        if (entry.key.raw == key.raw) {
            entry.key = objects.HashEntry.DELETED;
            entry.value = Value.nil();
            ht.count -= 1;
            return;
        }
        idx = (idx + 1) % ht.capacity;
    }
}

/// Remove symbol from package
pub fn uninternSymbol(symbol: Value, pkg: Value) !bool {
    if (!symbol.isSymbol()) return error.TypeError;
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    if (p.symbols.raw == Value.nil().raw) return false;

    const ht = p.symbols.toPtr(objects.HashTable);
    const hash = symbol.hash();
    var idx = hash % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const entry = &ht.entries[idx];
        if (entry.isEmpty()) return false;
        if (entry.key.raw == symbol.raw) {
            entry.key = objects.HashEntry.DELETED;
            entry.value = Value.nil();
            ht.count -= 1;
            return true;
        }
        idx = (idx + 1) % ht.capacity;
    }
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
