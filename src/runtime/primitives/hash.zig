//! Hash table primitives

const std = @import("std");
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const runtime = @import("../runtime.zig");
const Heap = @import("../heap.zig").Heap;
const HashTable = objects.HashTable;
const HashTest = objects.HashTest;

/// (make-hash-table &key test size) - create hash table
/// test: eq (default), eql, equal, equalp
/// size: initial capacity (default 16)
pub fn primMakeHashTable(heap: *Heap, args: []const Value) !Value {
    // Intern keywords and symbols once
    const kw_test = heap.internKeyword("test");
    const kw_size = heap.internKeyword("size");
    const sym_eq = heap.intern("eq");
    const sym_eql = heap.intern("eql");
    const sym_equal = heap.intern("equal");
    const sym_equalp = heap.intern("equalp");

    var test_type = HashTest.eq;
    var size: usize = 16;

    // Parse keyword arguments
    var i: usize = 0;
    while (i < args.len) : (i += 2) {
        if (i + 1 >= args.len) return error.TypeMismatch;

        const key = args[i];
        const val = args[i + 1];

        if (!key.isKeyword()) return error.TypeMismatch;

        if (key.eq(kw_test)) {
            if (!val.isSymbol()) return error.TypeMismatch;

            if (val.eq(sym_eq)) {
                test_type = .eq;
            } else if (val.eq(sym_eql)) {
                test_type = .eql;
            } else if (val.eq(sym_equal)) {
                test_type = .equal;
            } else if (val.eq(sym_equalp)) {
                test_type = .equalp;
            } else {
                return error.TypeMismatch;
            }
        } else if (key.eq(kw_size)) {
            if (!val.isFixnum()) return error.TypeMismatch;
            const n = val.toFixnum();
            if (n < 0) return error.TypeMismatch;
            size = @intCast(n);
        }
    }

    return heap.allocHashTable(size, test_type);
}

/// (gethash key hash-table &optional default) - get value from hash table
pub fn primGethash(heap: *Heap, args: []const Value) !Value {
    if (args.len < 2 or args.len > 3) return error.TypeMismatch;

    const key = args[0];
    const ht_val = args[1];
    const default = if (args.len == 3) args[2] else Value.nil;

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    if (ht.get(heap, key)) |val| {
        return val;
    }
    return default;
}

/// (puthash key value hash-table) - set value in hash table
/// Returns value
pub fn primPuthash(_: *Heap, args: []const Value) !Value {
    if (args.len != 3) return error.TypeMismatch;

    const key = args[0];
    const value = args[1];
    const ht_val = args[2];

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    try ht.put(key, value);
    return value;
}

/// (remhash key hash-table) - remove key from hash table
/// Returns t if key was present, nil otherwise
pub fn primRemhash(_: *Heap, args: []const Value) !Value {
    if (args.len != 2) return error.TypeMismatch;

    const key = args[0];
    const ht_val = args[1];

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    const removed = ht.remove(key);
    return if (removed) Value.t else Value.nil;
}

/// (clrhash hash-table) - clear all entries
/// Returns the hash table
pub fn primClrhash(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    ht.clear();
    return ht_val;
}

/// (hash-table-count hash-table) - get number of entries
pub fn primHashTableCount(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    return Value.makeFixnum(@intCast(ht.count));
}

/// (hash-table-test hash-table) - get test function
pub fn primHashTableTest(heap: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    const test_name = switch (ht.test_type) {
        .eq => "eq",
        .eql => "eql",
        .equal => "equal",
        .equalp => "equalp",
    };
    return heap.intern(test_name);
}

/// (maphash function hash-table) - call function on each key-value pair
/// Returns nil
pub fn primMaphash(heap: *Heap, args: []const Value) !Value {
    if (args.len != 2) return error.TypeMismatch;

    const func = args[0];
    const ht_val = args[1];

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);

    // Iterate over entries
    for (ht.entries[0..ht.capacity]) |entry| {
        if (!HashTable.isAvailable(entry)) {
            // Call function with (key value)
            const call_args = [_]Value{ entry.key, entry.value };

            // Need to call the function - for now just return nil
            // Full implementation would need VM callback
            _ = func;
            _ = call_args;
            _ = heap;
        }
    }

    return Value.nil;
}

/// (sxhash object) - compute hash code for object
/// Returns non-negative fixnum
pub fn primSxhash(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;
    const obj = args[0];
    const h = hashValue(obj);
    const fixnum_h = @as(i63, @intCast(h & 0x3FFFFFFFFFFFFFFF));
    return Value.makeFixnum(fixnum_h);
}

pub fn hashValue(val: Value) u64 {
    return switch (val.typeKind()) {
        .nil => 0,
        .t => 1,
        .fixnum => @bitCast(@as(i64, val.toFixnum())),
        .float => @bitCast(val.toFloat()),
        .char => @intCast(val.toCharacter()),
        .cons => blk: {
            const c = val.toPtr(objects.Cons);
            const h1 = hashValue(c.car);
            const h2 = hashValue(c.cdr);
            break :blk h1 *% 31 +% h2;
        },
        .symbol, .keyword => val.raw,
        .string => blk: {
            const s = val.toPtr(objects.String);
            var h: u64 = 0;
            for (s.bytes()) |b| {
                h = h *% 31 +% b;
            }
            break :blk h;
        },
        .vector => blk: {
            const v = val.toPtr(objects.Vector);
            var h: u64 = 0;
            for (v.items()) |item| {
                h = h *% 31 +% hashValue(item);
            }
            break :blk h;
        },
        .closure, .hashtable, .rational, .complex, .stream, .bignum, .array, .pathname, .package, .chunk, .condition => val.raw,
    };
}
