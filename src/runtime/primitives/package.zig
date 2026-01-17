// Package system primitives
//
// make-package, find-package, intern, export, import, use-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;
const hash = @import("hash.zig");
const Cons = objects.Cons;
const String = objects.String;
const Vector = objects.Vector;

/// Create a new package
pub fn makePackage(heap: *Heap, name: Value, nicknames: ?Value, use_list: ?Value) !Value {
    if (!name.isString() and !name.isSymbol()) return error.TypeError;

    const pkg = try heap.alloc(objects.Package);

    pkg.* = .{
        .kind = .package,
        .name = name,
        .nicknames = nicknames orelse Value.nil,
        .use_list = use_list orelse Value.nil,
        .exports = Value.nil,
        .symbols = Value.nil,
        .shadowing = Value.nil,
    };

    const pkg_val = Value.makePtr(pkg, .boxed);
    try heap.putLispPackage(name, pkg_val);

    // Register nicknames
    if (nicknames) |nns| {
        var nicks = nns;
        while (!nicks.isNil()) {
            if (!nicks.isCons()) break;
            const nick = nicks.toPtr(Cons).car;
            try heap.putLispPackage(nick, pkg_val);
            nicks = nicks.toPtr(Cons).cdr;
        }
    }

    return pkg_val;
}

/// Find a package by name or nickname
pub fn findPackage(heap: *Heap, name: Value) ?Value {
    return heap.findLispPackage(name);
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

/// Get list of packages that use this package
pub fn packageUsedByList(heap: *Heap, pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;

    var result = Value.nil;
    var it = heap.packages.valueIterator();
    while (it.next()) |zig_pkg| {
        const name_val = heap.allocString(zig_pkg.*.name) catch continue;
        if (heap.findLispPackage(name_val)) |pkg_val| {
            const p = pkg_val.toPtr(objects.Package);
            var use_curr = p.use_list;
            while (!use_curr.isNil()) {
                const pair = use_curr.toPtr(objects.Cons);
                if (pair.car.raw == pkg.raw) {
                    result = try heap.allocCons(pkg_val, result);
                    break;
                }
                use_curr = pair.cdr;
            }
        }
    }
    return result;
}

/// Get list of shadowing symbols in package
pub fn packageShadowingSymbols(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.shadowing;
}

/// Get package internal symbols hash table
pub fn packageSymbolsTable(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.symbols;
}

/// Get package exports hash table
pub fn packageExportsTable(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.exports;
}

/// Find symbol in all packages, return list
pub fn findAllSymbols(heap: *Heap, name: Value) !Value {
    const name_str = if (name.isString())
        name.toPtr(objects.String).bytes()
    else if (name.isSymbol())
        name.toPtr(objects.Symbol).getName()
    else
        return error.TypeError;

    var result = Value.nil;
    var it = heap.packages.valueIterator();
    while (it.next()) |zig_pkg| {
        const name_val = heap.allocString(zig_pkg.*.name) catch continue;
        if (heap.findLispPackage(name_val)) |pkg_val| {
            const p = pkg_val.toPtr(objects.Package);

            const sym_table = p.symbols;
            if (sym_table.isNil()) continue;
            const ht = sym_table.toPtr(objects.HashTable);
            if (hashTableLookup(heap, ht, name_str)) |sym| {
                result = try heap.allocCons(sym, result);
            }
        }
    }
    return result;
}

/// List all packages
pub fn listAllPackages(heap: *Heap) !Value {
    var result = Value.nil;
    var it = heap.packages.valueIterator();
    while (it.next()) |zig_pkg| {
        const name_val = try heap.allocString(zig_pkg.*.name);
        if (heap.findLispPackage(name_val)) |pkg_val| {
            result = try heap.allocCons(pkg_val, result);
        }
    }
    return result;
}

/// Intern a symbol in a package
/// Returns (values symbol status) where status is :internal/:external/:inherited/nil
pub fn internSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Get string name and create lookup symbol
    const name_str = if (name.isString())
        name.toPtr(objects.String).bytes()
    else if (name.isSymbol())
        name.toPtr(objects.Symbol).getName()
    else
        return error.TypeError;

    // Create temporary symbol for lookup
    const lookup_sym = try heap.allocSymbol(name_str);

    // Check if symbol exists in internal table using hash lookup
    if (p.symbols.raw != Value.nil.raw) {
        var found_sym: Value = Value.nil;
        const ht = p.symbols.toPtr(objects.HashTable);
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const entry = &ht.entries[i];
            if (entry.key.raw == objects.HashTable.EMPTY.raw or entry.key.raw == objects.HashTable.DELETED.raw) continue;
            if (!entry.key.isSymbol()) continue;
            const sym = entry.key.toPtr(objects.Symbol);
            if (std.mem.eql(u8, sym.getName(), name_str)) {
                found_sym = entry.key;
                break;
            }
        }
        if (found_sym.raw != Value.nil.raw) {
            // Found in internal table - check if exported
            if (p.exports.raw != Value.nil.raw) {
                const exported = hashTableLookup(p.exports, found_sym);
                if (exported.raw != Value.nil.raw) {
                    const status = try heap.internKeyword("external");
                    return try heap.allocCons(found_sym, try heap.allocCons(status, Value.nil));
                }
            }
            const status = try heap.internKeyword("internal");
            return try heap.allocCons(found_sym, try heap.allocCons(status, Value.nil));
        }
    }

    // Check used packages for exported symbols
    var use = p.use_list;
    while (use.raw != Value.nil.raw) {
        if (!use.isCons()) break;
        const used_pkg = use.toPtr(objects.Cons).car;
        if (used_pkg.isPackage()) {
            const up = used_pkg.toPtr(objects.Package);
            if (up.exports.raw != Value.nil.raw) {
                const found = hashTableLookup(up.exports, lookup_sym);
                if (found.raw != Value.nil.raw) {
                    const status = try heap.internKeyword("inherited");
                    return try heap.allocCons(found, try heap.allocCons(status, Value.nil));
                }
            }
        }
        use = use.toPtr(objects.Cons).cdr;
    }

    // Not found - use the symbol we created
    const new_sym = lookup_sym;

    // Add to symbols table (create if needed)
    if (p.symbols.raw == Value.nil.raw) {
        p.symbols = try createHashTable(heap, 16);
    }
    try insertHashTable(heap, p.symbols, new_sym, Value.t);

    const status = try heap.internKeyword("internal");
    return try heap.allocCons(new_sym, try heap.allocCons(status, Value.nil));
}

fn hashTableLookup(table: Value, key: Value) Value {
    if (!table.isHashTable()) return Value.nil;
    const ht = table.toPtr(objects.HashTable);
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    const test_type = ht.test_type;
    var idx = hashValueWithTest(key, test_type) & mask;

    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (objects.HashTable.isEmpty(entry)) {
            return Value.nil;
        }
        if (!objects.HashTable.isDeleted(entry) and hashKeyEqualWithTest(entry.key, key, test_type)) {
            return entry.key;
        }
        idx = (idx + 1) & mask;
    }
    return Value.nil;
}

fn hashValueWithTest(key: Value, test_type: objects.HashTest) u64 {
    return switch (test_type) {
        .eq => key.raw,
        .eql => key.raw,
        .equal => hash.hashValue(key),
    };
}

fn hashKeyEqualWithTest(a: Value, b: Value, test_type: objects.HashTest) bool {
    return switch (test_type) {
        .eq => a.raw == b.raw,
        .eql => valueEql(a, b),
        .equal => valueEqual(a, b, 0),
    };
}

const MAX_EQUAL_DEPTH = 1000;

fn valueEql(a: Value, b: Value) bool {
    if (a.isFloat() and b.isFloat()) {
        const af = a.toFloat();
        const bf = b.toFloat();
        if (std.math.isNan(af) or std.math.isNan(bf)) return false;
        return af == bf;
    }
    return a.raw == b.raw;
}

fn valueEqual(a: Value, b: Value, depth: usize) bool {
    if (depth > MAX_EQUAL_DEPTH) return false;
    if (a.isFloat() and b.isFloat()) return valueEql(a, b);
    if (a.raw == b.raw) return true;
    if (a.isFixnum() or b.isFixnum()) return false;
    if (a.isCharacter() or b.isCharacter()) return false;
    if (a.isFloat() or b.isFloat()) return false;

    const tag_a = a.raw & 0xF;
    const tag_b = b.raw & 0xF;
    if (tag_a != tag_b) return false;

    if (a.isCons()) {
        const ca = a.toPtr(Cons);
        const cb = b.toPtr(Cons);
        return valueEqual(ca.car, cb.car, depth + 1) and valueEqual(ca.cdr, cb.cdr, depth + 1);
    } else if (a.isString()) {
        const sa = a.toPtr(String);
        const sb = b.toPtr(String);
        return std.mem.eql(u8, sa.bytes(), sb.bytes());
    } else if (a.isVector()) {
        const va = a.toPtr(Vector);
        const vb = b.toPtr(Vector);
        if (va.length != vb.length) return false;
        for (va.items(), vb.items()) |ea, eb| {
            if (!valueEqual(ea, eb, depth + 1)) return false;
        }
        return true;
    }
    return false;
}

fn createHashTable(heap: *Heap, capacity: usize) !Value {
    const size = @sizeOf(objects.HashTable) + capacity * @sizeOf(objects.HashEntry);
    const ptr = try heap.allocRaw(size);
    const ht: *objects.HashTable = @ptrCast(@alignCast(ptr));
    const entries: [*]objects.HashEntry = @ptrCast(ptr + @sizeOf(objects.HashTable));

    ht.* = .{
        .kind = .hashtable,
        .count = 0,
        .capacity = capacity,
        .entries = entries,
        .test_type = .eql,
    };

    // Initialize all entries to empty
    var i: usize = 0;
    while (i < capacity) : (i += 1) {
        entries[i] = .{ .key = objects.HashTable.EMPTY, .value = Value.nil };
    }

    return Value.makePtr(ht, .boxed);
}

fn insertHashTable(heap: *Heap, table: Value, key: Value, value: Value) !void {
    _ = heap;
    if (!table.isHashTable()) return error.TypeError;
    const ht = table.toPtr(objects.HashTable);

    // Simple linear probing
    const h = key.raw;
    var idx = h % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const entry = &ht.entries[idx];
        if (entry.key.raw == objects.HashTable.EMPTY.raw or entry.key.raw == objects.HashTable.DELETED.raw or entry.key.raw == key.raw) {
            const was_new = entry.key.raw == objects.HashTable.EMPTY.raw or entry.key.raw == objects.HashTable.DELETED.raw;
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
        name.toPtr(objects.String).bytes()
    else if (name.isSymbol())
        name.toPtr(objects.Symbol).getName()
    else
        return error.TypeError;

    // Check if symbol exists in internal table
    if (p.symbols.raw != Value.nil.raw) {
        const ht = p.symbols.toPtr(objects.HashTable);
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const entry = &ht.entries[i];
            if (entry.key.raw == objects.HashTable.EMPTY.raw or entry.key.raw == objects.HashTable.DELETED.raw) continue;
            if (!entry.key.isSymbol()) continue;
            const sym = entry.key.toPtr(objects.Symbol);
            const sym_name = sym.getName();
            if (std.mem.eql(u8, sym_name, name_str)) {
                // Found in internal table - check if exported
                if (p.exports.raw != Value.nil.raw) {
                    const exp = p.exports.toPtr(objects.HashTable);
                    var j: usize = 0;
                    while (j < exp.capacity) : (j += 1) {
                        const e = &exp.entries[j];
                        if (e.key.raw == objects.HashTable.EMPTY.raw or e.key.raw == objects.HashTable.DELETED.raw) continue;
                        if (e.key.raw == entry.key.raw) {
                            const status = try heap.internKeyword("external");
                            return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil));
                        }
                    }
                }
                const status = try heap.internKeyword("internal");
                return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil));
            }
        }
    }

    // Check used packages for exported symbols
    var use = p.use_list;
    while (use.raw != Value.nil.raw) {
        if (!use.isCons()) break;
        const used_pkg = use.toPtr(objects.Cons).car;
        if (used_pkg.isPackage()) {
            const up = used_pkg.toPtr(objects.Package);
            if (up.exports.raw != Value.nil.raw) {
                const exp = up.exports.toPtr(objects.HashTable);
                var i: usize = 0;
                while (i < exp.capacity) : (i += 1) {
                    const entry = &exp.entries[i];
                    if (entry.key.raw == objects.HashTable.EMPTY.raw or entry.key.raw == objects.HashTable.DELETED.raw) continue;
                    if (!entry.key.isSymbol()) continue;
                    const sym = entry.key.toPtr(objects.Symbol);
                    const sym_name = sym.getName();
                    if (std.mem.eql(u8, sym_name, name_str)) {
                        const status = try heap.internKeyword("inherited");
                        return try heap.allocCons(entry.key, try heap.allocCons(status, Value.nil));
                    }
                }
            }
        }
        use = use.toPtr(objects.Cons).cdr;
    }

    // Not found
    return try heap.allocCons(Value.nil, try heap.allocCons(Value.nil, Value.nil));
}

/// Export symbols from a package
pub fn exportSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Create exports table if needed
    if (p.exports.raw == Value.nil.raw) {
        p.exports = try createHashTable(heap, 16);
    }

    // Handle single symbol or list
    if (symbols.isSymbol()) {
        try insertHashTable(heap, p.exports, symbols, Value.t);
    } else {
        var list = symbols;
        while (list.raw != Value.nil.raw) {
            if (!list.isCons()) return error.TypeError;
            const sym = list.toPtr(objects.Cons).car;
            if (!sym.isSymbol()) return error.TypeError;
            try insertHashTable(heap, p.exports, sym, Value.t);
            list = list.toPtr(objects.Cons).cdr;
        }
    }
}

/// Import symbols into a package
pub fn importSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    // Create symbols table if needed
    if (p.symbols.raw == Value.nil.raw) {
        p.symbols = try createHashTable(heap, 16);
    }

    // Handle single symbol or list
    if (symbols.isSymbol()) {
        try insertHashTable(heap, p.symbols, symbols, Value.t);
    } else {
        var list = symbols;
        while (list.raw != Value.nil.raw) {
            if (!list.isCons()) return error.TypeError;
            const sym = list.toPtr(objects.Cons).car;
            if (!sym.isSymbol()) return error.TypeError;
            try insertHashTable(heap, p.symbols, sym, Value.t);
            list = list.toPtr(objects.Cons).cdr;
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
        while (list.raw != Value.nil.raw) {
            if (!list.isCons()) return error.TypeError;
            const name = list.toPtr(objects.Cons).car;
            if (!name.isSymbol() and !name.isString()) return error.TypeError;
            p.shadowing = try heap.allocCons(name, p.shadowing);
            list = list.toPtr(objects.Cons).cdr;
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
        while (list.raw != Value.nil.raw) {
            if (!list.isCons()) return error.TypeError;
            const pkg_to_use = list.toPtr(objects.Cons).car;
            if (!pkg_to_use.isPackage()) return error.TypeError;
            p.use_list = try heap.allocCons(pkg_to_use, p.use_list);
            list = list.toPtr(objects.Cons).cdr;
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
        to_remove = try heap.allocCons(pkgs_to_unuse, Value.nil);
    }

    // Filter use_list
    var new_use_list = Value.nil;
    var use = p.use_list;
    while (use.raw != Value.nil.raw) {
        if (!use.isCons()) break;
        const used_pkg = use.toPtr(objects.Cons).car;
        var should_keep = true;

        var rem = to_remove;
        while (rem.raw != Value.nil.raw) {
            if (!rem.isCons()) break;
            if (rem.toPtr(objects.Cons).car.raw == used_pkg.raw) {
                should_keep = false;
                break;
            }
            rem = rem.toPtr(objects.Cons).cdr;
        }

        if (should_keep) {
            new_use_list = try heap.allocCons(used_pkg, new_use_list);
        }
        use = use.toPtr(objects.Cons).cdr;
    }
    p.use_list = new_use_list;
}

/// Unexport symbols
pub fn unexportSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    if (p.exports.raw == Value.nil.raw) return;

    const ht = p.exports.toPtr(objects.HashTable);

    // Handle single symbol or list
    if (symbols.isSymbol()) {
        try removeFromHashTable(ht, symbols);
    } else {
        var list = symbols;
        while (list.raw != Value.nil.raw) {
            if (!list.isCons()) return error.TypeError;
            const sym = list.toPtr(objects.Cons).car;
            if (!sym.isSymbol()) return error.TypeError;
            try removeFromHashTable(ht, sym);
            list = list.toPtr(objects.Cons).cdr;
        }
    }
    _ = heap;
}

fn removeFromHashTable(ht: *objects.HashTable, key: Value) !void {
    const h = key.raw;
    var idx = h % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const entry = &ht.entries[idx];
        if (entry.key.raw == objects.HashTable.EMPTY.raw) return;
        if (entry.key.raw == key.raw) {
            entry.key = objects.HashTable.DELETED;
            entry.value = Value.nil;
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
    if (p.symbols.raw == Value.nil.raw) return false;

    const ht = p.symbols.toPtr(objects.HashTable);
    const h = symbol.raw;
    var idx = h % ht.capacity;
    var i: usize = 0;
    while (i < ht.capacity) : (i += 1) {
        const entry = &ht.entries[idx];
        if (entry.key.raw == objects.HashTable.EMPTY.raw) return false;
        if (entry.key.raw == symbol.raw) {
            entry.key = objects.HashTable.DELETED;
            entry.value = Value.nil;
            ht.count -= 1;
            return true;
        }
        idx = (idx + 1) % ht.capacity;
    }
    return false;
}

/// Delete a package
pub fn deletePackage(heap: *Heap, pkg: Value) !bool {
    if (!pkg.isPackage()) return error.TypeError;

    const p = pkg.toPtr(objects.Package);

    // Remove from Lisp package registry
    _ = try heap.removeLispPackage(p.name);

    // Remove nicknames from registry
    var nicks = p.nicknames;
    while (!nicks.isNil()) {
        if (!nicks.isCons()) break;
        const nick = nicks.toPtr(Cons).car;
        _ = try heap.removeLispPackage(nick);
        nicks = nicks.toPtr(Cons).cdr;
    }

    // Clear package state
    p.symbols = Value.nil;
    p.exports = Value.nil;
    p.use_list = Value.nil;
    p.shadowing = Value.nil;

    return true;
}

/// Rename a package
pub fn renamePackage(heap: *Heap, pkg: Value, new_name: Value, new_nicknames: ?Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;

    const p = pkg.toPtr(objects.Package);

    // Remove old name from Lisp package registry
    _ = try heap.removeLispPackage(p.name);

    // Remove old nicknames
    var old_nicks = p.nicknames;
    while (!old_nicks.isNil()) {
        if (!old_nicks.isCons()) break;
        const nick = old_nicks.toPtr(Cons).car;
        _ = try heap.removeLispPackage(nick);
        old_nicks = old_nicks.toPtr(Cons).cdr;
    }

    // Update package name
    p.name = new_name;
    if (new_nicknames) |nn| p.nicknames = nn;

    // Add new name to package table
    try heap.putLispPackage(new_name, pkg);

    // Add new nicknames
    if (new_nicknames) |nns| {
        var nicks = nns;
        while (!nicks.isNil()) {
            if (!nicks.isCons()) break;
            const nick = nicks.toPtr(Cons).car;
            try heap.putLispPackage(nick, pkg);
            nicks = nicks.toPtr(Cons).cdr;
        }
    }

    return pkg;
}

test "package creation and lookup" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocString("MY-PKG");
    const pkg = try makePackage(&heap, name, null, null);

    try testing.expect(pkg.isPackage());
    const pkg_name = try packageName(pkg);
    try testing.expect(pkg_name.raw == name.raw);
}

test "intern and find symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocString("TEST-PKG");
    const pkg = try makePackage(&heap, name, null, null);

    const sym_name = try heap.allocString("FOO");
    const result = try internSymbol(&heap, sym_name, pkg);

    try testing.expect(result.isCons());
    const sym = result.toPtr(objects.Cons).car;
    try testing.expect(sym.isSymbol());

    const found = try findSymbol(&heap, sym_name, pkg);
    try testing.expect(found.isCons());
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == sym.raw);
}

test "export and import symbols" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name1 = try heap.allocString("PKG1");
    const pkg1 = try makePackage(&heap, name1, null, null);

    const sym_name = try heap.allocString("BAR");
    const result = try internSymbol(&heap, sym_name, pkg1);
    const sym = result.toPtr(objects.Cons).car;

    try exportSymbols(&heap, sym, pkg1);

    const name2 = try heap.allocString("PKG2");
    const pkg2 = try makePackage(&heap, name2, null, null);

    try importSymbols(&heap, sym, pkg2);

    const found = try findSymbol(&heap, sym_name, pkg2);
    try testing.expect(found.isCons());
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == sym.raw);
}

test "use-package and inherited symbols" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name1 = try heap.allocString("PKG1");
    const pkg1 = try makePackage(&heap, name1, null, null);

    const sym_name = try heap.allocString("BAZ");
    const result = try internSymbol(&heap, sym_name, pkg1);
    const sym = result.toPtr(objects.Cons).car;

    try exportSymbols(&heap, sym, pkg1);

    const name2 = try heap.allocString("PKG2");
    const pkg2 = try makePackage(&heap, name2, null, null);

    try usePackage(&heap, pkg1, pkg2);

    const found = try findSymbol(&heap, sym_name, pkg2);
    try testing.expect(found.isCons());
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == sym.raw);
}

test "intern returns correct status" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocString("TEST"), null, null);
    const sym_name = try heap.allocString("X");

    const result1 = try internSymbol(&heap, sym_name, pkg);
    const status1 = result1.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    try testing.expect(status1.isKeyword());
    const s1_str = status1.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s1_str, "internal"));

    const sym = result1.toPtr(objects.Cons).car;
    try exportSymbols(&heap, sym, pkg);

    const result2 = try internSymbol(&heap, sym_name, pkg);
    const status2 = result2.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    try testing.expect(status2.isKeyword());
    const s2_str = status2.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s2_str, "external"));
}

test "unexport removes from exports" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocString("TEST"), null, null);
    const sym_name = try heap.allocString("Y");
    const result = try internSymbol(&heap, sym_name, pkg);
    const sym = result.toPtr(objects.Cons).car;

    try exportSymbols(&heap, sym, pkg);
    const found1 = try findSymbol(&heap, sym_name, pkg);
    const status1 = found1.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    const s1_str = status1.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s1_str, "external"));

    try unexportSymbols(&heap, sym, pkg);
    const found2 = try findSymbol(&heap, sym_name, pkg);
    const status2 = found2.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    const s2_str = status2.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s2_str, "internal"));
}

test "unintern removes symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocString("TEST"), null, null);
    const sym_name = try heap.allocString("Z");
    const result = try internSymbol(&heap, sym_name, pkg);
    const sym = result.toPtr(objects.Cons).car;

    const removed = try uninternSymbol(sym, pkg);
    try testing.expect(removed);

    const found = try findSymbol(&heap, sym_name, pkg);
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == Value.nil.raw);
}

test "unuse-package removes from use-list" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg1 = try makePackage(&heap, try heap.allocString("PKG1"), null, null);
    const pkg2 = try makePackage(&heap, try heap.allocString("PKG2"), null, null);

    try usePackage(&heap, pkg1, pkg2);
    const use1 = try packageUseList(pkg2);
    try testing.expect(use1.isCons());

    try unusePackage(&heap, pkg1, pkg2);
    const use2 = try packageUseList(pkg2);
    try testing.expect(use2.raw == Value.nil.raw);
}

test "delete-package removes from system" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocString("TO-DELETE");
    const pkg = try makePackage(&heap, name, null, null);
    try testing.expect(pkg.isPackage());

    const found1 = findPackage(&heap, name);
    try testing.expect(found1 != null);

    const removed = try deletePackage(&heap, pkg);
    try testing.expect(removed);

    const found2 = findPackage(&heap, name);
    try testing.expect(found2 == null);
}

test "delete-package removes nicknames" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocString("FULL-NAME");
    const nick1 = try heap.allocString("SHORT");
    const nicks = try heap.allocCons(nick1, Value.nil);
    const pkg = try makePackage(&heap, name, nicks, null);

    const found_by_nick = findPackage(&heap, nick1);
    try testing.expect(found_by_nick != null);

    _ = try deletePackage(&heap, pkg);

    const after_del = findPackage(&heap, nick1);
    try testing.expect(after_del == null);
}

test "rename-package updates name" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const old_name = try heap.allocString("OLD");
    const pkg = try makePackage(&heap, old_name, null, null);

    const new_name = try heap.allocString("NEW");
    const renamed = try renamePackage(&heap, pkg, new_name, null);
    try testing.expect(renamed.raw == pkg.raw);

    const found_old = findPackage(&heap, old_name);
    try testing.expect(found_old == null);

    const found_new = findPackage(&heap, new_name);
    try testing.expect(found_new != null);
    try testing.expect(found_new.?.raw == pkg.raw);
}

test "rename-package updates nicknames" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const old_name = try heap.allocString("OLD");
    const old_nick = try heap.allocString("O");
    const old_nicks = try heap.allocCons(old_nick, Value.nil);
    const pkg = try makePackage(&heap, old_name, old_nicks, null);

    const new_name = try heap.allocString("NEW");
    const new_nick = try heap.allocString("N");
    const new_nicks = try heap.allocCons(new_nick, Value.nil);
    _ = try renamePackage(&heap, pkg, new_name, new_nicks);

    const found_old_nick = findPackage(&heap, old_nick);
    try testing.expect(found_old_nick == null);

    const found_new_nick = findPackage(&heap, new_nick);
    try testing.expect(found_new_nick != null);
    try testing.expect(found_new_nick.?.raw == pkg.raw);
}

test "shadow creates shadowing symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocString("TEST"), null, null);
    const name = try heap.allocString("SHADOWED");

    try shadowSymbols(&heap, name, pkg);

    const shadowing = try packageShadowingSymbols(pkg);
    try testing.expect(shadowing.isCons());
    const first = shadowing.toPtr(Cons).car;
    try testing.expect(first.raw == name.raw);
}

test "shadowing-import imports and shadows" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocString("TEST"), null, null);
    const sym_name = try heap.allocString("X");
    const result = try internSymbol(&heap, sym_name, pkg);
    const sym = result.toPtr(Cons).car;

    const pkg2 = try makePackage(&heap, try heap.allocString("PKG2"), null, null);
    try shadowingImport(&heap, sym, pkg2);

    const found = try findSymbol(&heap, sym_name, pkg2);
    const found_sym = found.toPtr(Cons).car;
    try testing.expect(found_sym.raw == sym.raw);

    const shadowing = try packageShadowingSymbols(pkg2);
    try testing.expect(shadowing.isCons());
}
