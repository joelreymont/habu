// Package system primitives
//
// make-package, find-package, intern, export, import, use-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const heap_mod = @import("../heap.zig");
const Heap = heap_mod.Heap;
const Cons = objects.Cons;
const String = objects.String;
const Vector = objects.Vector;

fn nameBytes(name: Value) ![]const u8 {
    return switch (name.typeKind()) {
        .string => name.toPtr(objects.String).bytes(),
        .symbol => name.toPtr(objects.Symbol).getName(),
        else => error.TypeError,
    };
}

fn nameBytesWithKeyword(name: Value) ![]const u8 {
    return switch (name.typeKind()) {
        .string => name.toPtr(objects.String).bytes(),
        .symbol => name.toPtr(objects.Symbol).getName(),
        .keyword => name.toPtr(objects.Keyword).getName(),
        else => error.TypeError,
    };
}

fn packageNameBytes(pkg: *objects.Package) ![]const u8 {
    return switch (pkg.name.typeKind()) {
        .symbol => pkg.name.toPtr(objects.Symbol).getName(),
        .string => pkg.name.toPtr(objects.String).bytes(),
        .keyword => pkg.name.toPtr(objects.Keyword).getName(),
        else => error.TypeError,
    };
}

fn packageNameFromValue(pkg_val: Value) ![]const u8 {
    return switch (pkg_val.typeKind()) {
        .package => packageNameBytes(pkg_val.toPtr(objects.Package)),
        .string, .symbol, .keyword => nameBytesWithKeyword(pkg_val),
        else => error.TypeError,
    };
}

fn resolvePkg(heap: *Heap, designator: Value) !Value {
    return switch (designator.typeKind()) {
        .package => designator,
        .symbol, .string, .keyword => if (try heap.findLispPackage(designator)) |pkg| pkg else return error.InvalidPackage,
        else => error.TypeError,
    };
}

fn listHasPkg(list: Value, pkg: Value) bool {
    var cur = list;
    while (cur.raw != Value.nil.raw) {
        if (!cur.isCons()) break;
        if (cur.toPtr(Cons).car.raw == pkg.raw) return true;
        cur = cur.toPtr(Cons).cdr;
    }
    return false;
}

fn resolvePkgList(heap: *Heap, pkgs: Value) !Value {
    return switch (pkgs.typeKind()) {
        .nil => Value.nil,
        .cons => {
            var result = Value.nil;
            var list = pkgs;
            while (list.raw != Value.nil.raw) {
                if (!list.isCons()) return error.TypeError;
                const pkg_val = try resolvePkg(heap, list.toPtr(Cons).car);
                if (!listHasPkg(result, pkg_val)) {
                    result = try heap.allocCons(pkg_val, result);
                }
                list = list.toPtr(Cons).cdr;
            }
            return result;
        },
        else => {
            const pkg_val = try resolvePkg(heap, pkgs);
            return try heap.allocCons(pkg_val, Value.nil);
        },
    };
}

fn filterPkgList(heap: *Heap, list: Value, remove_list: Value) !Value {
    var result = Value.nil;
    var cur = list;
    while (cur.raw != Value.nil.raw) {
        if (!cur.isCons()) return error.TypeError;
        const item = cur.toPtr(Cons).car;
        if (!listHasPkg(remove_list, item)) {
            result = try heap.allocCons(item, result);
        }
        cur = cur.toPtr(Cons).cdr;
    }
    return result;
}

fn nativePkgFor(heap: *Heap, pkg: Value) !*heap_mod.Package {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    const pkg_name = try packageNameBytes(p);
    return if (heap.findPackage(pkg_name)) |found| found else return error.InvalidPackage;
}

fn listContainsValue(list: Value, value: Value) bool {
    var cur = list;
    while (cur.raw != Value.nil.raw) {
        if (!cur.isCons()) break;
        if (cur.toPtr(Cons).car.raw == value.raw) return true;
        cur = cur.toPtr(Cons).cdr;
    }
    return false;
}

fn ensureLispSymbolTable(heap: *Heap, pkg: *objects.Package) !void {
    if (pkg.symbols.raw == Value.nil.raw) {
        pkg.symbols = try createHashTable(heap, 16);
        heap.writeBarrier(Value.makePackage(pkg), pkg.symbols);
    }
}

fn addShadowingSymbol(heap: *Heap, pkg: *objects.Package, sym: Value) !void {
    if (!listContainsValue(pkg.shadowing, sym)) {
        pkg.shadowing = try heap.allocCons(sym, pkg.shadowing);
        heap.writeBarrier(Value.makePackage(pkg), pkg.shadowing);
    }
}

fn ensureLocalShadowSymbol(heap: *Heap, native_pkg: *heap_mod.Package, name: []const u8) !Value {
    var upper_buf: [256]u8 = undefined;
    const upper = try heap_mod.upperNameAlloc(heap.backing_allocator, name, upper_buf[0..]);
    defer heap_mod.freeUpperName(heap.backing_allocator, upper);
    const upper_name = upper.slice;
    var replace_existing = false;

    if (native_pkg.symbols.get(upper_name)) |existing| {
        if (existing.isSymbol()) {
            // Keep an existing symbol only when it is already local to this package.
            // Inherited symbols can be present in the native symbol table; SHADOW
            // must replace those with a fresh local symbol.
            if (existing.toPtr(objects.Symbol).reserved == @intFromPtr(native_pkg)) {
                return existing;
            }
            replace_existing = true;
        } else {
            return existing;
        }
    }

    if (replace_existing) {
        _ = native_pkg.symbols.remove(upper_name);
    }

    const sym = try heap.allocSymbol(upper_name);
    sym.toPtr(objects.Symbol).reserved = @intFromPtr(native_pkg);
    try native_pkg.symbols.put(upper_name, sym);
    return sym;
}

fn nativeUseHas(list: []const *heap_mod.Package, pkg: *heap_mod.Package) bool {
    for (list) |item| {
        if (item == pkg) return true;
    }
    return false;
}

fn filterNativeUseList(list: *std.ArrayList(*heap_mod.Package), remove_items: []const *heap_mod.Package) void {
    var out: usize = 0;
    for (list.items) |item| {
        if (!nativeUseHas(remove_items, item)) {
            list.items[out] = item;
            out += 1;
        }
    }
    list.items = list.items[0..out];
}

fn purgeNativePackageEntries(heap: *Heap, native_pkg: *heap_mod.Package) !void {
    var pkg_keys = std.ArrayList([]u8){};
    defer {
        for (pkg_keys.items) |key| heap.backing_allocator.free(key);
        pkg_keys.deinit(heap.backing_allocator);
    }

    var pkg_it = heap.packages.iterator();
    while (pkg_it.next()) |entry| {
        if (entry.value_ptr.* == native_pkg) {
            const key_copy = try heap.backing_allocator.dupe(u8, entry.key_ptr.*);
            try pkg_keys.append(heap.backing_allocator, key_copy);
        }
    }
    for (pkg_keys.items) |key| {
        if (heap.packages.fetchRemove(key)) |removed| {
            heap.backing_allocator.free(removed.key);
        }
    }

    var alias_keys = std.ArrayList([]u8){};
    defer {
        for (alias_keys.items) |key| heap.backing_allocator.free(key);
        alias_keys.deinit(heap.backing_allocator);
    }

    var alias_it = heap.package_aliases.iterator();
    while (alias_it.next()) |entry| {
        if (entry.value_ptr.* == native_pkg) {
            const key_copy = try heap.backing_allocator.dupe(u8, entry.key_ptr.*);
            try alias_keys.append(heap.backing_allocator, key_copy);
        }
    }
    for (alias_keys.items) |key| {
        if (heap.package_aliases.fetchRemove(key)) |removed| {
            heap.backing_allocator.free(removed.key);
        }
    }
}

fn addNativeExport(pkg: *heap_mod.Package, name: []const u8) !void {
    if (pkg.exports.contains(name)) return;
    const key = try pkg.allocator.dupe(u8, name);
    errdefer pkg.allocator.free(key);
    try pkg.exports.put(pkg.allocator, key, {});
}

fn removeNativeExport(pkg: *heap_mod.Package, name: []const u8) void {
    if (pkg.exports.fetchRemove(name)) |removed| {
        pkg.allocator.free(removed.key);
    }
}

fn addNativeSymbol(pkg: *heap_mod.Package, sym: Value) !void {
    const sym_name = sym.toPtr(objects.Symbol).getName();
    if (pkg.symbols.get(sym_name)) |existing| {
        if (existing.raw != sym.raw) {
            return error.SymConflict;
        }
        return;
    }
    try pkg.symbols.put(sym_name, sym);
}

fn removeNativeSymbol(pkg: *heap_mod.Package, sym: Value) void {
    const sym_name = sym.toPtr(objects.Symbol).getName();
    _ = pkg.symbols.remove(sym_name);
}

fn detachHomeSymbol(heap: *Heap, native_pkg: *heap_mod.Package, sym: Value) !void {
    if (!sym.isSymbol()) return;
    const sym_obj = sym.toPtr(objects.Symbol);
    if (sym_obj.reserved != @intFromPtr(native_pkg)) return;
    try heap.retagUninterned(sym_obj);
}

/// Create a new package
pub fn makePackage(heap: *Heap, name: Value, nicknames: ?Value, use_list: ?Value) !Value {
    switch (name.typeKind()) {
        .string, .symbol, .keyword => {},
        else => return error.TypeError,
    }

    const pkg_name = try nameBytesWithKeyword(name);
    if (try heap.findLispPackage(name)) |_| return error.PackageExists;

    const existing_native = heap.findPackage(pkg_name);
    const direct_native = heap.packages.get(pkg_name);
    if (existing_native != null and direct_native == null) return error.PackageExists;
    const reused_native = if (existing_native) |native| native else null;

    if (nicknames) |nns| {
        var nicks = nns;
        while (nicks.raw != Value.nil.raw) {
            if (!nicks.isCons()) return error.TypeError;
            const nick = nicks.toPtr(Cons).car;
            switch (nick.typeKind()) {
                .string, .symbol, .keyword => {},
                else => return error.TypeError,
            }
            if (try heap.findLispPackage(nick)) |_| return error.PackageExists;
            const nick_name = try nameBytesWithKeyword(nick);
            if (heap.findPackage(nick_name)) |existing_nick_pkg| {
                if (reused_native) |native| {
                    if (existing_nick_pkg != native) return error.PackageExists;
                } else {
                    return error.PackageExists;
                }
            }
            nicks = nicks.toPtr(Cons).cdr;
        }
    }

    const resolved_use_list = if (use_list) |ul| try resolvePkgList(heap, ul) else Value.nil;
    const native_pkg = if (reused_native) |native| native else try heap.findOrCreatePackage(pkg_name);
    errdefer if (reused_native == null) {
        purgeNativePackageEntries(heap, native_pkg) catch {};
        native_pkg.deinit();
    };

    const pkg = try heap.alloc(objects.Package);

    pkg.* = .{
        .kind = .package,
        .name = name,
        .nicknames = nicknames orelse Value.nil,
        .use_list = resolved_use_list,
        .exports = Value.nil,
        .symbols = Value.nil,
        .shadowing = Value.nil,
    };

    const pkg_val = Value.makePtr(pkg, .boxed);
    const name_key = try heap.packageKey(name);
    try heap.putLispPackage(name, pkg_val);
    errdefer {
        _ = heap.removeLispPackageKey(name_key);
    }

    // Register nicknames
    if (nicknames) |nns| {
        var nicks = nns;
        while (!nicks.isNil()) {
            if (!nicks.isCons()) break;
            const nick = nicks.toPtr(Cons).car;
            const nick_key = try heap.packageKey(nick);
            try heap.putLispPackage(nick, pkg_val);
            errdefer {
                _ = heap.removeLispPackageKey(nick_key);
            }
            const nick_name = try nameBytesWithKeyword(nick);
            if (heap.package_aliases.get(nick_name)) |existing_alias| {
                if (existing_alias != native_pkg) return error.PackageExists;
            } else {
                const alias_key = try heap.backing_allocator.dupe(u8, nick_name);
                errdefer heap.backing_allocator.free(alias_key);
                try heap.package_aliases.put(heap.backing_allocator, alias_key, native_pkg);
                errdefer _ = heap.package_aliases.remove(alias_key);
            }
            nicks = nicks.toPtr(Cons).cdr;
        }
    }

    var use_cur = resolved_use_list;
    while (use_cur.raw != Value.nil.raw) {
        if (!use_cur.isCons()) break;
        const use_pkg = use_cur.toPtr(Cons).car;
        const native_use = try nativePkgFor(heap, use_pkg);
        if (!nativeUseHas(native_pkg.use_list.items, native_use)) {
            try native_pkg.usePackage(native_use);
        }
        use_cur = use_cur.toPtr(Cons).cdr;
    }

    return pkg_val;
}

/// Find a package by name or nickname
pub fn findPackage(heap: *Heap, name: Value) error{ OutOfMemory, TypeError }!?Value {
    return try heap.findLispPackage(name);
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
        const name_val = try heap.allocBaseString(zig_pkg.*.name);
        if (try heap.findLispPackage(name_val)) |pkg_val| {
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
pub fn packageSymbols(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.symbols;
}

/// Get package exports hash table
pub fn packageExports(pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);
    return p.exports;
}

/// Find symbol in all packages, return list
pub fn findAllSymbols(heap: *Heap, name: Value) !Value {
    const name_str = try nameBytesWithKeyword(name);
    var upper_buf: [256]u8 = undefined;
    const upper = try heap_mod.upperNameAlloc(heap.backing_allocator, name_str, upper_buf[0..]);
    defer heap_mod.freeUpperName(heap.backing_allocator, upper);
    const upper_name = upper.slice;

    var seen = std.AutoHashMap(u64, void).init(heap.backing_allocator);
    defer seen.deinit();

    var result = Value.nil;
    var it = heap.packages.valueIterator();
    while (it.next()) |native_pkg| {
        const pkg_ptr = native_pkg.*;
        if (pkg_ptr.findAccessibleUpper(upper_name)) |sym| {
            if (seen.get(sym.raw) == null) {
                try seen.put(sym.raw, {});
                result = try heap.allocCons(sym, result);
            }
        }
    }
    return result;
}

/// Find all symbols whose name contains the given substring
pub fn aproposSymbols(heap: *Heap, substring: Value) !Value {
    const substr = try nameBytes(substring);

    var result = Value.nil;
    var seen = std.AutoHashMap(u64, void).init(heap.backing_allocator);
    defer seen.deinit();

    // Iterate over all registered Lisp packages
    var it = heap.packages.valueIterator();
    while (it.next()) |zig_pkg| {
        const name_val = try heap.allocBaseString(zig_pkg.*.name);
        const pkg_opt = try heap.findLispPackage(name_val);
        if (pkg_opt == null) continue;
        const pkg = pkg_opt.?.toPtr(objects.Package);

        // Check internal symbols
        if (pkg.symbols.raw != Value.nil.raw) {
            const ht = pkg.symbols.toPtr(objects.HashTable);
            const cap: usize = @intCast(ht.capacity);
            for (0..cap) |i| {
                const entry_key = ht.getKey(i);
                if (objects.HashTable.isAvailableKey(entry_key)) continue;
                if (!entry_key.isSymbol()) continue;

                // Check if name contains substring
                const sym = entry_key.toPtr(objects.Symbol);
                const sym_name = sym.getName();
                if (std.mem.indexOf(u8, sym_name, substr) != null) {
                    // Avoid duplicates
                    if (seen.get(entry_key.raw) == null) {
                        try seen.put(entry_key.raw, {});
                        result = try heap.allocCons(entry_key, result);
                    }
                }
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
        const name_val = try heap.allocBaseString(zig_pkg.*.name);
        if (try heap.findLispPackage(name_val)) |pkg_val| {
            result = try heap.allocCons(pkg_val, result);
        }
    }
    return result;
}

/// Get all symbols in a package (from native SymbolTable)
/// Takes package name string, keyword, symbol, or Lisp package object
pub fn packageSymbolsList(heap: *Heap, pkg_val: Value) !Value {
    const pkg_name = try packageNameFromValue(pkg_val);

    // Find native package
    const native_pkg = if (heap.findPackage(pkg_name)) |val| val else return Value.nil;

    // Iterate over native SymbolTable
    var result = Value.nil;
    var it = native_pkg.symbols.iterator();
    while (it.next()) |entry| {
        result = try heap.allocCons(entry.value_ptr.*, result);
    }
    return result;
}

/// Get all exported symbols in a package (from native Package)
/// Takes package name string, keyword, symbol, or Lisp package object
pub fn packageExportsList(heap: *Heap, pkg_val: Value) !Value {
    const pkg_name = try packageNameFromValue(pkg_val);

    // Find native package
    const native_pkg = if (heap.findPackage(pkg_name)) |val| val else return Value.nil;

    // Iterate over exports hash map
    var result = Value.nil;
    var it = native_pkg.exports.keyIterator();
    while (it.next()) |export_name| {
        // Look up the symbol by name
        if (native_pkg.symbols.get(export_name.*)) |sym| {
            result = try heap.allocCons(sym, result);
        }
    }
    return result;
}

/// Intern a symbol in a package
/// Returns (values symbol status) where status is :internal/:external/:inherited/nil
pub fn internSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    const p = pkg.toPtr(objects.Package);

    const name_str = try nameBytesWithKeyword(name);
    const pkg_name = try packageNameBytes(p);
    const native_pkg = if (heap.findPackage(pkg_name)) |val| val else return error.InvalidPackage;

    if (try native_pkg.findAccessible(name_str)) |sym| {
        // Found in internal table - check if exported
        if (p.symbols.raw != Value.nil.raw) {
            const found_sym = hashTableLookup(p.symbols, sym);
            if (found_sym.raw != Value.nil.raw) {
                if (p.exports.raw != Value.nil.raw) {
                    const exported = hashTableLookup(p.exports, found_sym);
                    if (exported.raw != Value.nil.raw) {
                        const status = try heap.internKeyword("EXTERNAL");
                        return try heap.allocCons(found_sym, try heap.allocCons(status, Value.nil));
                    }
                }
                const status = try heap.internKeyword("INTERNAL");
                return try heap.allocCons(found_sym, try heap.allocCons(status, Value.nil));
            }
        }

        if (try findInheritedSymbol(heap, p, sym)) |found| {
            const status = try heap.internKeyword("INHERITED");
            return try heap.allocCons(found, try heap.allocCons(status, Value.nil));
        }
    }

    // Not found - intern new symbol in native package
    const new_sym = try native_pkg.intern(heap, name_str);

    // Add to symbols table (create if needed)
    if (p.symbols.raw == Value.nil.raw) {
        p.symbols = try createHashTable(heap, 16);
        heap.writeBarrier(pkg, p.symbols);
    }
    try insertHashTable(heap, p.symbols, new_sym, new_sym);

    const status = try heap.internKeyword("INTERNAL");
    return try heap.allocCons(new_sym, try heap.allocCons(status, Value.nil));
}

fn hashTableLookup(table: Value, key: Value) Value {
    if (!table.isHashTable()) return Value.nil;
    const ht = table.toPtr(objects.HashTable);
    return ht.get(key) orelse Value.nil;
}

fn createHashTable(heap: *Heap, capacity: usize) !Value {
    return heap.allocHashTable(capacity, .eql);
}

fn insertHashTable(heap: *Heap, table: Value, key: Value, value: Value) !void {
    if (!table.isHashTable()) return error.TypeError;
    const ht = table.toPtr(objects.HashTable);

    while (true) {
        ht.put(key, value) catch |err| switch (err) {
            error.HashTableNeedsGrowth, error.HashTableFull => {
                const new_cap = try std.math.mul(usize, @intCast(ht.capacity), 2);
                try heap.growHashTableInPlace(ht, new_cap);
                continue;
            },
            else => return err,
        };
        return;
    }
}

fn findInheritedSymbol(heap: *Heap, p: *objects.Package, sym: Value) !?Value {
    if (!sym.isSymbol()) return null;
    const sym_name = sym.toPtr(objects.Symbol).getName();
    var use = p.use_list;
    while (use.raw != Value.nil.raw) {
        if (!use.isCons()) break;
        const used_pkg = use.toPtr(objects.Cons).car;
        if (used_pkg.isPackage()) {
            const up = used_pkg.toPtr(objects.Package);
            if (up.exports.raw != Value.nil.raw) {
                const found = hashTableLookup(up.exports, sym);
                if (found.raw != Value.nil.raw) return found;
            }
            if (nativePkgFor(heap, used_pkg)) |native_use| {
                if (native_use.exports.get(sym_name) != null) return sym;
            } else |_| {}
        }
        use = use.toPtr(objects.Cons).cdr;
    }
    return null;
}

/// Find a symbol in a package
/// Returns (values symbol status) where status is :internal/:external/:inherited/nil
pub fn findSymbol(heap: *Heap, name: Value, pkg: Value) !Value {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);

    const name_str = try nameBytesWithKeyword(name);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);

    if (try native_pkg.findAccessible(name_str)) |sym| {
        // Check if symbol exists in internal table
        if (p.symbols.raw != Value.nil.raw) {
            const found_sym = hashTableLookup(p.symbols, sym);
            if (found_sym.raw != Value.nil.raw) {
                // Found in internal table - check if exported
                if (p.exports.raw != Value.nil.raw) {
                    const exported = hashTableLookup(p.exports, found_sym);
                    if (exported.raw != Value.nil.raw) {
                        const status = try heap.internKeyword("EXTERNAL");
                        return try heap.allocCons(found_sym, try heap.allocCons(status, Value.nil));
                    }
                }
                const status = try heap.internKeyword("INTERNAL");
                return try heap.allocCons(found_sym, try heap.allocCons(status, Value.nil));
            }
        }

        if (try findInheritedSymbol(heap, p, sym)) |found| {
            const status = try heap.internKeyword("INHERITED");
            return try heap.allocCons(found, try heap.allocCons(status, Value.nil));
        }
    }

    // Not found
    return try heap.allocCons(Value.nil, try heap.allocCons(Value.nil, Value.nil));
}

/// Export symbols from a package
pub fn exportSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);

    // Create exports table if needed
    if (p.exports.raw == Value.nil.raw) {
        p.exports = try createHashTable(heap, 16);
        heap.writeBarrier(resolved_pkg, p.exports);
    }

    // Handle single symbol or list
    switch (symbols.typeKind()) {
        .symbol => {
            try insertHashTable(heap, p.exports, symbols, symbols);
            try addNativeExport(native_pkg, symbols.toPtr(objects.Symbol).getName());
        },
        .nil => return,
        .cons => {
            var list = symbols;
            while (list.raw != Value.nil.raw) {
                if (!list.isCons()) return error.TypeError;
                const sym = list.toPtr(objects.Cons).car;
                if (!sym.isSymbol()) return error.TypeError;
                try insertHashTable(heap, p.exports, sym, sym);
                try addNativeExport(native_pkg, sym.toPtr(objects.Symbol).getName());
                list = list.toPtr(objects.Cons).cdr;
            }
        },
        else => return error.TypeError,
    }
}

/// Import symbols into a package
pub fn importSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);

    // Create symbols table if needed
    if (p.symbols.raw == Value.nil.raw) {
        p.symbols = try createHashTable(heap, 16);
        heap.writeBarrier(resolved_pkg, p.symbols);
    }

    // Handle single symbol or list
    switch (symbols.typeKind()) {
        .symbol => {
            try insertHashTable(heap, p.symbols, symbols, symbols);
            try addNativeSymbol(native_pkg, symbols);
        },
        .nil => return,
        .cons => {
            var list = symbols;
            while (list.raw != Value.nil.raw) {
                if (!list.isCons()) return error.TypeError;
                const sym = list.toPtr(objects.Cons).car;
                if (!sym.isSymbol()) return error.TypeError;
                try insertHashTable(heap, p.symbols, sym, sym);
                try addNativeSymbol(native_pkg, sym);
                list = list.toPtr(objects.Cons).cdr;
            }
        },
        else => return error.TypeError,
    }
}

/// Shadow symbols in a package
pub fn shadowSymbols(heap: *Heap, names: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);
    try ensureLispSymbolTable(heap, p);

    // Handle single name or list
    switch (names.typeKind()) {
        .symbol, .string, .keyword => {
            const name = try nameBytesWithKeyword(names);
            const sym = try ensureLocalShadowSymbol(heap, native_pkg, name);
            try insertHashTable(heap, p.symbols, sym, sym);
            try addShadowingSymbol(heap, p, sym);
        },
        .nil => return,
        .cons => {
            var list = names;
            while (list.raw != Value.nil.raw) {
                if (!list.isCons()) return error.TypeError;
                const name = list.toPtr(objects.Cons).car;
                switch (name.typeKind()) {
                    .symbol, .string, .keyword => {},
                    else => return error.TypeError,
                }
                const name_str = try nameBytesWithKeyword(name);
                const sym = try ensureLocalShadowSymbol(heap, native_pkg, name_str);
                try insertHashTable(heap, p.symbols, sym, sym);
                try addShadowingSymbol(heap, p, sym);
                list = list.toPtr(objects.Cons).cdr;
            }
        },
        else => return error.TypeError,
    }
}

/// Shadowing import
pub fn shadowingImport(heap: *Heap, symbols: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);
    try ensureLispSymbolTable(heap, p);

    const replace_local = struct {
        fn one(heap2: *Heap, pkg_obj: *objects.Package, native: *heap_mod.Package, sym: Value) !void {
            if (!sym.isSymbol()) return error.TypeError;
            const sym_name = sym.toPtr(objects.Symbol).getName();
            if (native.symbols.get(sym_name)) |existing| {
                if (existing.raw != sym.raw) {
                    if (pkg_obj.symbols.raw != Value.nil.raw) {
                        try removeFromHashTable(pkg_obj.symbols.toPtr(objects.HashTable), existing);
                    }
                    if (pkg_obj.exports.raw != Value.nil.raw) {
                        try removeFromHashTable(pkg_obj.exports.toPtr(objects.HashTable), existing);
                    }
                    removeNativeExport(native, sym_name);
                    removeNativeSymbol(native, existing);
                    try detachHomeSymbol(heap2, native, existing);
                } else {
                    return;
                }
            }
            try insertHashTable(heap2, pkg_obj.symbols, sym, sym);
            try addNativeSymbol(native, sym);
        }
    };

    switch (symbols.typeKind()) {
        .symbol => {
            try replace_local.one(heap, p, native_pkg, symbols);
            try addShadowingSymbol(heap, p, symbols);
        },
        .nil => return,
        .cons => {
            var list = symbols;
            while (list.raw != Value.nil.raw) {
                if (!list.isCons()) return error.TypeError;
                const sym = list.toPtr(objects.Cons).car;
                if (!sym.isSymbol()) return error.TypeError;
                try replace_local.one(heap, p, native_pkg, sym);
                try addShadowingSymbol(heap, p, sym);
                list = list.toPtr(objects.Cons).cdr;
            }
        },
        else => return error.TypeError,
    }
}

/// Add packages to use-list
pub fn usePackage(heap: *Heap, pkgs_to_use: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);
    const resolved = try resolvePkgList(heap, pkgs_to_use);

    var list = resolved;
    while (list.raw != Value.nil.raw) {
        if (!list.isCons()) return error.TypeError;
        const pkg_to_use = list.toPtr(objects.Cons).car;
        if (!listHasPkg(p.use_list, pkg_to_use)) {
            p.use_list = try heap.allocCons(pkg_to_use, p.use_list);
            heap.writeBarrier(resolved_pkg, p.use_list);
        }
        const native_use = try nativePkgFor(heap, pkg_to_use);
        if (!nativeUseHas(native_pkg.use_list.items, native_use)) {
            try native_pkg.usePackage(native_use);
        }
        list = list.toPtr(objects.Cons).cdr;
    }
}

/// Remove packages from use-list
pub fn unusePackage(heap: *Heap, pkgs_to_unuse: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);
    const to_remove = try resolvePkgList(heap, pkgs_to_unuse);
    if (to_remove.raw == Value.nil.raw) return;

    p.use_list = try filterPkgList(heap, p.use_list, to_remove);
    heap.writeBarrier(resolved_pkg, p.use_list);

    var native_rm = std.ArrayList(*heap_mod.Package){};
    defer native_rm.deinit(heap.backing_allocator);
    var rem = to_remove;
    while (rem.raw != Value.nil.raw) {
        if (!rem.isCons()) return error.TypeError;
        const rm_pkg = rem.toPtr(objects.Cons).car;
        const native_rm_pkg = try nativePkgFor(heap, rm_pkg);
        if (!nativeUseHas(native_rm.items, native_rm_pkg)) {
            try native_rm.append(heap.backing_allocator, native_rm_pkg);
        }
        rem = rem.toPtr(objects.Cons).cdr;
    }
    filterNativeUseList(&native_pkg.use_list, native_rm.items);
}

/// Unexport symbols
pub fn unexportSymbols(heap: *Heap, symbols: Value, pkg: Value) !void {
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    if (p.exports.raw == Value.nil.raw) return;
    const native_pkg = try nativePkgFor(heap, resolved_pkg);

    const ht = p.exports.toPtr(objects.HashTable);

    // Handle single symbol or list
    switch (symbols.typeKind()) {
        .symbol => {
            try removeFromHashTable(ht, symbols);
            removeNativeExport(native_pkg, symbols.toPtr(objects.Symbol).getName());
        },
        .nil => return,
        .cons => {
            var list = symbols;
            while (list.raw != Value.nil.raw) {
                if (!list.isCons()) return error.TypeError;
                const sym = list.toPtr(objects.Cons).car;
                if (!sym.isSymbol()) return error.TypeError;
                try removeFromHashTable(ht, sym);
                removeNativeExport(native_pkg, sym.toPtr(objects.Symbol).getName());
                list = list.toPtr(objects.Cons).cdr;
            }
        },
        else => return error.TypeError,
    }
}

fn removeFromHashTable(ht: *objects.HashTable, key: Value) !void {
    _ = ht.remove(key);
}

/// Remove symbol from package
pub fn uninternSymbol(heap: *Heap, symbol: Value, pkg: Value) !bool {
    if (!symbol.isSymbol()) return error.TypeError;
    const resolved_pkg = try resolvePkg(heap, pkg);
    const p = resolved_pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, resolved_pkg);
    if (p.symbols.raw == Value.nil.raw) return false;

    const ht = p.symbols.toPtr(objects.HashTable);
    if (!ht.remove(symbol)) return false;

    // Remove from exports if present
    if (p.exports.raw != Value.nil.raw) {
        try removeFromHashTable(p.exports.toPtr(objects.HashTable), symbol);
    }
    removeNativeExport(native_pkg, symbol.toPtr(objects.Symbol).getName());
    removeNativeSymbol(native_pkg, symbol);
    try detachHomeSymbol(heap, native_pkg, symbol);

    // Remove from shadowing list if present
    var new_shadowing = Value.nil;
    var curr = p.shadowing;
    while (curr.raw != Value.nil.raw) {
        if (!curr.isCons()) break;
        const item = curr.toPtr(Cons).car;
        if (item.raw != symbol.raw) {
            new_shadowing = try heap.allocCons(item, new_shadowing);
        }
        curr = curr.toPtr(Cons).cdr;
    }
    p.shadowing = new_shadowing;
    heap.writeBarrier(pkg, p.shadowing);

    return true;
}

/// Delete a package
pub fn deletePackage(heap: *Heap, designator: Value) !bool {
    const pkg = try resolvePkg(heap, designator);
    const p = pkg.toPtr(objects.Package);
    const pkg_name = try packageNameBytes(p);
    const native_pkg = if (heap.findPackage(pkg_name)) |found| found else return error.InvalidPackage;
    if ((heap.cl_package != null and native_pkg == heap.cl_package.?) or
        (heap.cl_user_package != null and native_pkg == heap.cl_user_package.?) or
        (heap.keyword_package != null and native_pkg == heap.keyword_package.?))
    {
        // Protect core packages; deleting them leaves stale compiler/runtime assumptions.
        return error.InvalidPackage;
    }
    const rm_list = try heap.allocCons(pkg, Value.nil);

    if (heap.lisp_packages.raw != Value.nil.raw) {
        const ht = heap.lisp_packages.toPtr(objects.HashTable);
        var seen = std.AutoHashMap(u64, void).init(heap.backing_allocator);
        defer seen.deinit();

        const cap: usize = @intCast(ht.capacity);
        for (0..cap) |i| {
            const key = ht.getKey(i);
            if (objects.HashTable.isAvailableKey(key)) continue;
            const pkg_val = ht.getValue(i);
            if (!pkg_val.isPackage()) continue;
            if (pkg_val.raw == pkg.raw) continue;
            if (seen.get(pkg_val.raw) != null) continue;
            try seen.put(pkg_val.raw, {});
            const other = pkg_val.toPtr(objects.Package);
            if (other.use_list.raw != Value.nil.raw) {
                other.use_list = try filterPkgList(heap, other.use_list, rm_list);
                heap.writeBarrier(pkg_val, other.use_list);
            }
        }
    }

    const native_rm = [_]*heap_mod.Package{native_pkg};
    var native_it = heap.packages.valueIterator();
    while (native_it.next()) |other| {
        const other_pkg = other.*;
        if (other_pkg == native_pkg) continue;
        if (other_pkg.use_list.items.len == 0) continue;
        filterNativeUseList(&other_pkg.use_list, native_rm[0..]);
    }

    var native_sym_it = native_pkg.symbols.iterator();
    while (native_sym_it.next()) |entry| {
        try detachHomeSymbol(heap, native_pkg, entry.value_ptr.*);
    }

    // Remove from Lisp package registry
    _ = try heap.removeLispPackage(p.name);

    // Remove nicknames from registry
    var nicks = p.nicknames;
    while (!nicks.isNil()) {
        if (!nicks.isCons()) break;
        const nick = nicks.toPtr(Cons).car;
        _ = try heap.removeLispPackage(nick);
        const nick_name = try nameBytesWithKeyword(nick);
        if (heap.package_aliases.fetchRemove(nick_name)) |removed| {
            heap.backing_allocator.free(removed.key);
        }
        nicks = nicks.toPtr(Cons).cdr;
    }

    // Clear package state
    p.symbols = Value.nil;
    p.exports = Value.nil;
    p.use_list = Value.nil;
    p.shadowing = Value.nil;

    if (heap.current_package) |cur| {
        if (cur == native_pkg) {
            if (heap.cl_user_package) |user| {
                heap.current_package = user;
            } else if (heap.cl_package) |cl| {
                heap.current_package = cl;
            } else {
                heap.current_package = null;
            }
        }
    }

    try purgeNativePackageEntries(heap, native_pkg);
    native_pkg.deinit();

    return true;
}

/// Rename a package
pub fn renamePackage(heap: *Heap, pkg: Value, new_name: Value, new_nicknames: ?Value) !Value {
    if (!pkg.isPackage()) return error.TypeError;
    switch (new_name.typeKind()) {
        .string, .symbol, .keyword => {},
        else => return error.TypeError,
    }

    const p = pkg.toPtr(objects.Package);
    const native_pkg = try nativePkgFor(heap, pkg);

    if (try heap.findLispPackage(new_name)) |existing| {
        if (existing.raw != pkg.raw) return error.PackageExists;
    }

    const new_name_bytes = try nameBytesWithKeyword(new_name);
    if (heap.findPackage(new_name_bytes)) |existing_native| {
        if (existing_native != native_pkg) return error.PackageExists;
    }

    if (new_nicknames) |nns| {
        var nicks = nns;
        while (nicks.raw != Value.nil.raw) {
            if (!nicks.isCons()) return error.TypeError;
            const nick = nicks.toPtr(Cons).car;
            switch (nick.typeKind()) {
                .string, .symbol, .keyword => {},
                else => return error.TypeError,
            }
            if (try heap.findLispPackage(nick)) |existing_pkg| {
                if (existing_pkg.raw != pkg.raw) return error.PackageExists;
            }
            const nick_name = try nameBytesWithKeyword(nick);
            if (heap.findPackage(nick_name)) |existing_native| {
                if (existing_native != native_pkg) return error.PackageExists;
            }
            nicks = nicks.toPtr(Cons).cdr;
        }
    }

    const old_name_bytes = try packageNameBytes(p);
    if (!std.mem.eql(u8, new_name_bytes, old_name_bytes)) {
        const new_key = try heap.backing_allocator.dupe(u8, new_name_bytes);
        try heap.packages.put(heap.backing_allocator, new_key, native_pkg);

        if (heap.packages.fetchRemove(old_name_bytes)) |removed| {
            heap.backing_allocator.free(removed.key);
        }

        const new_name_copy = try native_pkg.allocator.dupe(u8, new_name_bytes);
        const old_name_copy = native_pkg.name;
        native_pkg.name = new_name_copy;
        native_pkg.allocator.free(old_name_copy);
    }

    _ = try heap.removeLispPackage(p.name);
    try heap.putLispPackage(new_name, pkg);

    if (new_nicknames) |nns| {
        var old_nicks = p.nicknames;
        while (!old_nicks.isNil()) {
            if (!old_nicks.isCons()) break;
            const nick = old_nicks.toPtr(Cons).car;
            _ = try heap.removeLispPackage(nick);
            const nick_name = try nameBytesWithKeyword(nick);
            if (heap.package_aliases.fetchRemove(nick_name)) |removed| {
                heap.backing_allocator.free(removed.key);
            }
            old_nicks = old_nicks.toPtr(Cons).cdr;
        }

        p.nicknames = nns;
        heap.writeBarrier(pkg, p.nicknames);
        var nicks = nns;
        while (!nicks.isNil()) {
            if (!nicks.isCons()) break;
            const nick = nicks.toPtr(Cons).car;
            try heap.putLispPackage(nick, pkg);
            const nick_name = try nameBytesWithKeyword(nick);
            const alias_key = try heap.backing_allocator.dupe(u8, nick_name);
            try heap.package_aliases.put(heap.backing_allocator, alias_key, native_pkg);
            nicks = nicks.toPtr(Cons).cdr;
        }
    }

    p.name = new_name;
    heap.writeBarrier(pkg, p.name);
    return pkg;
}

test "package creation and lookup" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("MY-PKG");
    const pkg = try makePackage(&heap, name, null, null);

    try testing.expect(pkg.isPackage());
    const pkg_name = try packageName(pkg);
    try testing.expect(pkg_name.raw == name.raw);
}

test "findPackage accepts symbol and string designators" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("TEST-DESIGNATOR");
    const pkg = try makePackage(&heap, name, null, null);

    const sym = try heap.intern("TEST-DESIGNATOR");
    const found_sym = try findPackage(&heap, sym);
    try testing.expect(found_sym != null);
    try testing.expect(found_sym.?.eq(pkg));

    const found_str = try findPackage(&heap, name);
    try testing.expect(found_str != null);
    try testing.expect(found_str.?.eq(pkg));
}

test "intern and find symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("TEST-PKG");
    const pkg = try makePackage(&heap, name, null, null);

    const sym_name = try heap.allocBaseString("FOO");
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

    const name1 = try heap.allocBaseString("PKG1");
    const pkg1 = try makePackage(&heap, name1, null, null);

    const sym_name = try heap.allocBaseString("BAR");
    const result = try internSymbol(&heap, sym_name, pkg1);
    const sym = result.toPtr(objects.Cons).car;

    try exportSymbols(&heap, sym, pkg1);

    const name2 = try heap.allocBaseString("PKG2");
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

    const name1 = try heap.allocBaseString("PKG1");
    const pkg1 = try makePackage(&heap, name1, null, null);

    const sym_name = try heap.allocBaseString("BAZ");
    const result = try internSymbol(&heap, sym_name, pkg1);
    const sym = result.toPtr(objects.Cons).car;

    try exportSymbols(&heap, sym, pkg1);

    const name2 = try heap.allocBaseString("PKG2");
    const pkg2 = try makePackage(&heap, name2, null, null);

    try usePackage(&heap, pkg1, pkg2);

    const found = try findSymbol(&heap, sym_name, pkg2);
    try testing.expect(found.isCons());
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == sym.raw);
}

test "shadowing-import retags replaced local home symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg1 = try makePackage(&heap, try heap.allocBaseString("PKG1"), null, null);
    const pkg2 = try makePackage(&heap, try heap.allocBaseString("PKG2"), null, null);
    const sym_name = try heap.allocBaseString("SWAP");

    const intern1 = try internSymbol(&heap, sym_name, pkg1);
    const old_sym = intern1.toPtr(objects.Cons).car;
    const old_bits_before = old_sym.toPtr(objects.Symbol).reserved;
    try testing.expect(old_bits_before != 0 and (old_bits_before & 1) == 0);

    const intern2 = try internSymbol(&heap, sym_name, pkg2);
    const new_sym = intern2.toPtr(objects.Cons).car;

    try shadowingImport(&heap, new_sym, pkg1);

    const old_bits_after = old_sym.toPtr(objects.Symbol).reserved;
    try testing.expect(old_bits_after != 0 and (old_bits_after & 1) == 1);

    const found = try findSymbol(&heap, sym_name, pkg1);
    try testing.expect(found.isCons());
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == new_sym.raw);
}

test "findAllSymbols returns distinct symbols" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg1 = try makePackage(&heap, try heap.allocBaseString("PKG1"), null, null);
    const pkg2 = try makePackage(&heap, try heap.allocBaseString("PKG2"), null, null);

    const sym_name = try heap.allocBaseString("FOO");
    const result1 = try internSymbol(&heap, sym_name, pkg1);
    const sym1 = result1.toPtr(objects.Cons).car;
    const result2 = try internSymbol(&heap, sym_name, pkg2);
    const sym2 = result2.toPtr(objects.Cons).car;
    try testing.expect(sym1.raw != sym2.raw);

    const list = try findAllSymbols(&heap, sym_name);
    var count: usize = 0;
    var found1 = false;
    var found2 = false;
    var cur = list;
    while (cur.raw != Value.nil.raw) : (count += 1) {
        try testing.expect(cur.isCons());
        const sym = cur.toPtr(Cons).car;
        if (sym.raw == sym1.raw) found1 = true;
        if (sym.raw == sym2.raw) found2 = true;
        cur = cur.toPtr(Cons).cdr;
    }
    try testing.expect(found1);
    try testing.expect(found2);
    try testing.expectEqual(@as(usize, 2), count);
}

test "findAllSymbols dedupes inherited symbols" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg1 = try makePackage(&heap, try heap.allocBaseString("PKG1"), null, null);
    const pkg2 = try makePackage(&heap, try heap.allocBaseString("PKG2"), null, null);

    const sym_name = try heap.allocBaseString("BAR");
    const result = try internSymbol(&heap, sym_name, pkg1);
    const sym = result.toPtr(objects.Cons).car;
    try exportSymbols(&heap, sym, pkg1);
    try usePackage(&heap, pkg1, pkg2);

    const list = try findAllSymbols(&heap, sym_name);
    var count: usize = 0;
    var found = false;
    var cur = list;
    while (cur.raw != Value.nil.raw) : (count += 1) {
        try testing.expect(cur.isCons());
        const item = cur.toPtr(Cons).car;
        if (item.raw == sym.raw) found = true;
        cur = cur.toPtr(Cons).cdr;
    }
    try testing.expect(found);
    try testing.expectEqual(@as(usize, 1), count);
}

test "intern returns correct status" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST"), null, null);
    const sym_name = try heap.allocBaseString("X");

    const result1 = try internSymbol(&heap, sym_name, pkg);
    const status1 = result1.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    try testing.expect(status1.isKeyword());
    const s1_str = status1.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s1_str, "INTERNAL"));

    const sym = result1.toPtr(objects.Cons).car;
    try exportSymbols(&heap, sym, pkg);

    const result2 = try internSymbol(&heap, sym_name, pkg);
    const status2 = result2.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    try testing.expect(status2.isKeyword());
    const s2_str = status2.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s2_str, "EXTERNAL"));
}

test "unexport removes from exports" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST"), null, null);
    const sym_name = try heap.allocBaseString("Y");
    const result = try internSymbol(&heap, sym_name, pkg);
    const sym = result.toPtr(objects.Cons).car;

    try exportSymbols(&heap, sym, pkg);
    const found1 = try findSymbol(&heap, sym_name, pkg);
    const status1 = found1.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    const s1_str = status1.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s1_str, "EXTERNAL"));

    try unexportSymbols(&heap, sym, pkg);
    const found2 = try findSymbol(&heap, sym_name, pkg);
    const status2 = found2.toPtr(objects.Cons).cdr.toPtr(objects.Cons).car;
    const s2_str = status2.toPtr(objects.Keyword).getName();
    try testing.expect(std.mem.eql(u8, s2_str, "INTERNAL"));
}

test "unintern removes symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST"), null, null);
    const sym_name = try heap.allocBaseString("Z");
    const result = try internSymbol(&heap, sym_name, pkg);
    const sym = result.toPtr(objects.Cons).car;
    const home_bits_before = sym.toPtr(objects.Symbol).reserved;
    try testing.expect(home_bits_before != 0 and (home_bits_before & 1) == 0);

    const removed = try uninternSymbol(&heap, sym, pkg);
    try testing.expect(removed);

    const home_bits_after = sym.toPtr(objects.Symbol).reserved;
    try testing.expect(home_bits_after != 0 and (home_bits_after & 1) == 1);

    const found = try findSymbol(&heap, sym_name, pkg);
    const found_sym = found.toPtr(objects.Cons).car;
    try testing.expect(found_sym.raw == Value.nil.raw);
}

test "unuse-package removes from use-list" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg1 = try makePackage(&heap, try heap.allocBaseString("PKG1"), null, null);
    const pkg2 = try makePackage(&heap, try heap.allocBaseString("PKG2"), null, null);

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

    const name = try heap.allocBaseString("TO-DELETE");
    const pkg = try makePackage(&heap, name, null, null);
    try testing.expect(pkg.isPackage());
    const sym_name = try heap.allocBaseString("TO-DELETE-SYM");
    const interned = try internSymbol(&heap, sym_name, pkg);
    const sym = interned.toPtr(objects.Cons).car;
    const home_bits_before = sym.toPtr(objects.Symbol).reserved;
    try testing.expect(home_bits_before != 0 and (home_bits_before & 1) == 0);

    const found1 = try findPackage(&heap, name);
    try testing.expect(found1 != null);

    const removed = try deletePackage(&heap, pkg);
    try testing.expect(removed);

    const found2 = try findPackage(&heap, name);
    try testing.expect(found2 == null);

    const home_bits_after = sym.toPtr(objects.Symbol).reserved;
    try testing.expect(home_bits_after != 0 and (home_bits_after & 1) == 1);
}

test "delete-package removes nicknames" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("FULL-NAME");
    const nick1 = try heap.allocBaseString("SHORT");
    const nicks = try heap.allocCons(nick1, Value.nil);
    const pkg = try makePackage(&heap, name, nicks, null);

    const found_by_nick = try findPackage(&heap, nick1);
    try testing.expect(found_by_nick != null);

    _ = try deletePackage(&heap, pkg);

    const after_del = try findPackage(&heap, nick1);
    try testing.expect(after_del == null);
}

test "delete-package rejects protected packages" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const cl_name = try heap.allocBaseString("COMMON-LISP");
    const cl_user_name = try heap.allocBaseString("CL-USER");
    const kw_name = try heap.allocBaseString("KEYWORD");
    const cl_pkg = (try findPackage(&heap, cl_name)).?;
    const cl_user_pkg = (try findPackage(&heap, cl_user_name)).?;
    const kw_pkg = (try findPackage(&heap, kw_name)).?;

    try testing.expectError(error.InvalidPackage, deletePackage(&heap, cl_pkg));
    try testing.expectError(error.InvalidPackage, deletePackage(&heap, cl_user_pkg));
    try testing.expectError(error.InvalidPackage, deletePackage(&heap, kw_pkg));

    _ = try heap.intern("IF");
    _ = try heap.intern("NIL");
}

test "delete-package purges stray native aliases" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("P-ALIAS-TEST");
    const pkg = try makePackage(&heap, name, null, null);
    const native_pkg = try nativePkgFor(&heap, pkg);

    const alias_name = "PALIASX";
    const alias_key = try heap.backing_allocator.dupe(u8, alias_name);
    try heap.package_aliases.put(heap.backing_allocator, alias_key, native_pkg);

    _ = try deletePackage(&heap, pkg);
    try testing.expect(heap.findPackage(alias_name) == null);
}

test "rename-package updates name" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const old_name = try heap.allocBaseString("OLD");
    const pkg = try makePackage(&heap, old_name, null, null);

    const new_name = try heap.allocBaseString("NEW");
    const renamed = try renamePackage(&heap, pkg, new_name, null);
    try testing.expect(renamed.raw == pkg.raw);

    const found_old = try findPackage(&heap, old_name);
    try testing.expect(found_old == null);

    const found_new = try findPackage(&heap, new_name);
    try testing.expect(found_new != null);
    try testing.expect(found_new.?.raw == pkg.raw);
}

test "rename-package updates nicknames" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const old_name = try heap.allocBaseString("OLD");
    const old_nick = try heap.allocBaseString("O");
    const old_nicks = try heap.allocCons(old_nick, Value.nil);
    const pkg = try makePackage(&heap, old_name, old_nicks, null);

    const new_name = try heap.allocBaseString("NEW");
    const new_nick = try heap.allocBaseString("N");
    const new_nicks = try heap.allocCons(new_nick, Value.nil);
    _ = try renamePackage(&heap, pkg, new_name, new_nicks);

    const found_old_nick = try findPackage(&heap, old_nick);
    try testing.expect(found_old_nick == null);

    const found_new_nick = try findPackage(&heap, new_nick);
    try testing.expect(found_new_nick != null);
    try testing.expect(found_new_nick.?.raw == pkg.raw);
}

test "shadow creates shadowing symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST"), null, null);
    const name = try heap.allocBaseString("SHADOWED");

    try shadowSymbols(&heap, name, pkg);

    const shadowing = try packageShadowingSymbols(pkg);
    try testing.expect(shadowing.isCons());
    const first = shadowing.toPtr(Cons).car;
    try testing.expect(first.isSymbol());
    try testing.expectEqualStrings("SHADOWED", first.toPtr(objects.Symbol).getName());

    const found = try findSymbol(&heap, name, pkg);
    const found_sym = found.toPtr(Cons).car;
    try testing.expect(found_sym.raw == first.raw);
}

test "shadowing-import imports and shadows" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST"), null, null);
    const sym_name = try heap.allocBaseString("X");
    const result = try internSymbol(&heap, sym_name, pkg);
    const sym = result.toPtr(Cons).car;

    const pkg2 = try makePackage(&heap, try heap.allocBaseString("PKG2"), null, null);
    try shadowingImport(&heap, sym, pkg2);

    const found = try findSymbol(&heap, sym_name, pkg2);
    const found_sym = found.toPtr(Cons).car;
    try testing.expect(found_sym.raw == sym.raw);

    const shadowing = try packageShadowingSymbols(pkg2);
    try testing.expect(shadowing.isCons());
    const shadowed = shadowing.toPtr(Cons).car;
    try testing.expect(shadowed.raw == sym.raw);
}

test "shadow creates local symbol that masks inherited symbol" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const provider = try makePackage(&heap, try heap.allocBaseString("PROVIDER"), null, null);
    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST-MASK"), null, null);
    try usePackage(&heap, provider, pkg);

    const name = try heap.allocBaseString("MASKED");
    const provider_intern = try internSymbol(&heap, name, provider);
    const provider_sym = provider_intern.toPtr(Cons).car;
    try exportSymbols(&heap, provider_sym, provider);

    const before = try findSymbol(&heap, name, pkg);
    const inherited = before.toPtr(Cons).car;
    try testing.expect(inherited.isSymbol());
    const before_status = before.toPtr(Cons).cdr.toPtr(Cons).car;
    try testing.expect(before_status.isKeyword());
    try testing.expectEqualStrings("INHERITED", before_status.toPtr(objects.Keyword).getName());

    try shadowSymbols(&heap, name, pkg);

    const after = try findSymbol(&heap, name, pkg);
    const local_sym = after.toPtr(Cons).car;
    const status = after.toPtr(Cons).cdr.toPtr(Cons).car;
    try testing.expect(local_sym.isSymbol());
    try testing.expect(local_sym.raw != inherited.raw);
    try testing.expect(status.isKeyword());
    try testing.expectEqualStrings("INTERNAL", status.toPtr(objects.Keyword).getName());
}

test "shadow replaces inherited symbol entry in native table" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const provider = try makePackage(&heap, try heap.allocBaseString("PROVIDER-SHADOW"), null, null);
    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST-SHADOW-REPLACE"), null, null);
    try usePackage(&heap, provider, pkg);

    const sym_name = try heap.allocBaseString("MASKED");
    const provider_intern = try internSymbol(&heap, sym_name, provider);
    const provider_sym = provider_intern.toPtr(Cons).car;
    try exportSymbols(&heap, provider_sym, provider);

    const before = try findSymbol(&heap, sym_name, pkg);
    const inherited_sym = before.toPtr(Cons).car;
    const before_status = before.toPtr(Cons).cdr.toPtr(Cons).car;
    try testing.expect(inherited_sym.isSymbol());
    try testing.expect(before_status.isKeyword());
    try testing.expectEqualStrings("INHERITED", before_status.toPtr(objects.Keyword).getName());

    const native_pkg = try nativePkgFor(&heap, pkg);
    try native_pkg.symbols.put("MASKED", inherited_sym);

    try shadowSymbols(&heap, sym_name, pkg);

    const after = try findSymbol(&heap, sym_name, pkg);
    const local_sym = after.toPtr(Cons).car;
    const after_status = after.toPtr(Cons).cdr.toPtr(Cons).car;
    try testing.expect(local_sym.isSymbol());
    try testing.expect(local_sym.raw != inherited_sym.raw);
    try testing.expect(after_status.isKeyword());
    try testing.expectEqualStrings("INTERNAL", after_status.toPtr(objects.Keyword).getName());
}

test "findSymbol uses native exports for inherited status" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const provider = try makePackage(&heap, try heap.allocBaseString("PROVIDER-NATIVE"), null, null);
    const pkg = try makePackage(&heap, try heap.allocBaseString("TEST-NATIVE-INH"), null, null);
    try usePackage(&heap, provider, pkg);

    const name = try heap.allocBaseString("FOO");
    const provider_intern = try internSymbol(&heap, name, provider);
    const provider_sym = provider_intern.toPtr(Cons).car;
    try exportSymbols(&heap, provider_sym, provider);

    // Simulate package objects that do not mirror native exports exactly.
    provider.toPtr(objects.Package).exports = Value.nil;

    const found = try findSymbol(&heap, name, pkg);
    const found_sym = found.toPtr(Cons).car;
    const found_status = found.toPtr(Cons).cdr.toPtr(Cons).car;
    try testing.expect(found_sym.isSymbol());
    try testing.expect(found_sym.raw == provider_sym.raw);
    try testing.expect(found_status.isKeyword());
    try testing.expectEqualStrings("INHERITED", found_status.toPtr(objects.Keyword).getName());
}

test "packageSymbolsList accepts keyword name" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    _ = try makePackage(&heap, try heap.allocBaseString("PKG"), null, null);
    const kw = try heap.internKeyword("PKG");

    const syms = try packageSymbolsList(&heap, kw);
    try testing.expect(syms.isCons());
    var found_t = false;
    var found_nil = false;
    var cur = syms;
    while (cur.raw != Value.nil.raw) {
        try testing.expect(cur.isCons());
        const sym = cur.toPtr(Cons).car;
        if (sym.raw == Value.t.raw) found_t = true;
        if (sym.raw == Value.nil.raw) found_nil = true;
        cur = cur.toPtr(Cons).cdr;
    }
    try testing.expect(found_t);
    try testing.expect(found_nil);
}

test "makePackage rejects existing nickname" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const bar = try heap.allocBaseString("bar");
    _ = try makePackage(&heap, bar, null, null);

    const foo = try heap.allocBaseString("foo");
    const nick = try heap.allocBaseString("bar");
    const nick_list = try heap.allocCons(nick, Value.nil);

    try testing.expectError(error.PackageExists, makePackage(&heap, foo, nick_list, null));
    const found = try heap.findLispPackage(foo);
    try testing.expect(found == null);
}

test "makePackage supports keyword nicknames" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("KW-NICK-PKG");
    const nick = try heap.internKeyword("KW-NICK");
    const nick_list = try heap.allocCons(nick, Value.nil);
    const pkg = try makePackage(&heap, name, nick_list, null);
    try testing.expect(pkg.isPackage());

    const found = try findPackage(&heap, nick);
    try testing.expect(found != null);
    try testing.expect(found.?.raw == pkg.raw);
}

test "makePackage reuses native placeholder package" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const placeholder = try heap.findOrCreatePackage("REGRESSION-TEST");
    const name = try heap.allocBaseString("REGRESSION-TEST");
    const pkg = try makePackage(&heap, name, null, null);

    try testing.expect(pkg.isPackage());
    const native_pkg = try nativePkgFor(&heap, pkg);
    try testing.expect(native_pkg == placeholder);
}

test "resolvePkg invalid package" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const name = try heap.allocBaseString("MISSING");
    try testing.expectError(error.InvalidPackage, resolvePkg(&heap, name));
}
