//! Object layouts for Habu heap objects
//!
//! All objects are 16-byte aligned (required for 4-bit tag space).
//! Objects are allocated from a bump-pointer heap with Cheney GC.
//!
//! Layout conventions:
//! - First word is always the length/size (for GC traversal)
//! - Remaining words depend on object type
//! - All pointers stored are tagged Values

const std = @import("std");
const Value = @import("value.zig").Value;

/// Cons cell: (car . cdr)
/// Size: 16 bytes (2 words), 16-byte aligned for tagging
pub const Cons = extern struct {
    car: Value align(16),
    cdr: Value,

    pub fn init(car: Value, cdr: Value) Cons {
        return .{ .car = car, .cdr = cdr };
    }
};

/// Symbol: interned name with optional property list
/// Size: 32 bytes (4 words)
pub const Symbol = extern struct {
    /// Length of name in bytes
    name_len: u64,
    /// Pointer to name bytes (not tagged, just raw ptr)
    name_ptr: [*]const u8,
    /// Property list (tagged Value, nil or cons)
    plist: Value,
    /// Reserved for future use (hash, package, etc.)
    reserved: u64,

    pub fn getName(self: *const Symbol) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

/// Vector: growable array of values
/// Size: 24 bytes header + data
pub const Vector = extern struct {
    /// Number of elements
    length: u64,
    /// Capacity (for resizable vectors)
    capacity: u64,
    /// Pointer to element data (array of Values)
    data: [*]Value,

    pub fn get(self: *const Vector, index: usize) Value {
        std.debug.assert(index < self.length);
        return self.data[index];
    }

    pub fn set(self: *Vector, index: usize, val: Value) void {
        std.debug.assert(index < self.length);
        self.data[index] = val;
    }

    pub fn items(self: *const Vector) []Value {
        return self.data[0..self.length];
    }
};

/// String: mutable byte sequence (CL strings are mutable)
/// Size: 16 bytes header + data (inline for short strings)
pub const String = extern struct {
    /// Length in bytes
    length: u64,
    /// Pointer to byte data
    data: [*]u8,

    pub fn bytes(self: *const String) []const u8 {
        return self.data[0..self.length];
    }

    pub fn mutableBytes(self: *String) []u8 {
        return self.data[0..self.length];
    }
};

/// Closure: function + captured environment
/// Size: 32 bytes header + captures
pub const Closure = extern struct {
    /// Pointer to code (bytecode or JIT code address)
    code: *const anyopaque,
    /// Number of parameters
    arity: u32,
    /// Number of captured values
    num_captures: u32,
    /// Pointer to captured values array
    captures: [*]Value,

    pub fn getCapture(self: *const Closure, index: usize) Value {
        std.debug.assert(index < self.num_captures);
        return self.captures[index];
    }

    pub fn getCapturedValues(self: *const Closure) []Value {
        return self.captures[0..self.num_captures];
    }
};

/// Keyword: like symbol but self-evaluating, used for named arguments
/// Size: 24 bytes
pub const Keyword = extern struct {
    /// Length of name in bytes (without leading colon)
    name_len: u64,
    /// Pointer to name bytes
    name_ptr: [*]const u8,
    /// Hash for fast comparison (optional)
    hash: u64,

    pub fn getName(self: *const Keyword) []const u8 {
        return self.name_ptr[0..self.name_len];
    }
};

/// Hash table entry (key-value pair)
pub const HashEntry = extern struct {
    key: Value,
    value: Value,
};

/// Hash table test function type
pub const HashTest = enum(u8) {
    eq = 0, // Identity comparison
    eql = 1, // Identity + numeric equality (default)
    equal = 2, // Structural equality
};

/// Hash table: mutable key-value mapping
/// Uses open addressing with linear probing
/// Size: 32 bytes header + entries array
pub const HashTable = extern struct {
    /// Number of entries currently stored
    count: u64,
    /// Capacity (size of entries array)
    capacity: u64,
    /// Pointer to entries array
    entries: [*]HashEntry,
    /// Test function type (eq, eql, equal)
    test_type: HashTest,
    /// Padding for alignment
    _pad: [7]u8 = .{ 0, 0, 0, 0, 0, 0, 0 },

    /// Sentinel for empty entry - uses impossible character codepoint (> Unicode max 0x10FFFF)
    /// This ensures it won't collide with any valid fixnum, float, character, or pointer
    pub const EMPTY: Value = Value{ .raw = 0x80000000003FFFFE }; // char with codepoint 0x1FFFFF
    /// Sentinel for deleted entry - uses another impossible character codepoint
    pub const DELETED: Value = Value{ .raw = 0x80000000003FFFFC }; // char with codepoint 0x1FFFFE

    /// Check if an entry is empty
    pub fn isEmpty(entry: HashEntry) bool {
        return entry.key.raw == EMPTY.raw;
    }

    /// Check if an entry is deleted
    pub fn isDeleted(entry: HashEntry) bool {
        return entry.key.raw == DELETED.raw;
    }

    /// Check if an entry is available (empty or deleted)
    pub fn isAvailable(entry: HashEntry) bool {
        return isEmpty(entry) or isDeleted(entry);
    }

    /// Get entries slice
    pub fn getEntries(self: *const HashTable) []HashEntry {
        return self.entries[0..self.capacity];
    }
};

// ============================================================================
// Object size calculations (for GC)
// ============================================================================

/// Get the size of an object in bytes given its tag
/// This includes inline data (name bytes for symbols/keywords)
pub fn objectSize(val: Value) usize {
    const tag = val.getTag();
    return switch (tag) {
        .cons => @sizeOf(Cons),
        .symbol => blk: {
            const sym = val.toPtr(Symbol);
            // Header + inline name bytes (aligned to 8)
            break :blk @sizeOf(Symbol) + std.mem.alignForward(usize, sym.name_len, 8);
        },
        .vector => blk: {
            const vec = val.toPtr(Vector);
            // Header + data array
            break :blk @sizeOf(Vector) + vec.capacity * @sizeOf(Value);
        },
        .string => blk: {
            const str = val.toPtr(String);
            // Header + byte data (aligned)
            break :blk @sizeOf(String) + std.mem.alignForward(usize, str.length, 8);
        },
        .closure => blk: {
            const cls = val.toPtr(Closure);
            // Header + captures array
            break :blk @sizeOf(Closure) + cls.num_captures * @sizeOf(Value);
        },
        .keyword => blk: {
            const kw = val.toPtr(Keyword);
            // Header + inline name bytes (aligned to 8)
            break :blk @sizeOf(Keyword) + std.mem.alignForward(usize, kw.name_len, 8);
        },
        .hashtable => blk: {
            const ht = val.toPtr(HashTable);
            // Header + entries array
            break :blk @sizeOf(HashTable) + ht.capacity * @sizeOf(HashEntry);
        },
        .forwarding => @sizeOf(usize), // Just a pointer
    };
}

/// Iterate over all Values in an object (for GC root scanning)
pub fn forEachValue(val: Value, callback: *const fn (Value) void) void {
    const tag = val.getTag();
    switch (tag) {
        .cons => {
            const cons = val.toPtr(Cons);
            callback(cons.car);
            callback(cons.cdr);
        },
        .symbol => {
            const sym = val.toPtr(Symbol);
            callback(sym.plist);
        },
        .vector => {
            const vec = val.toPtr(Vector);
            for (vec.items()) |item| {
                callback(item);
            }
        },
        .string, .keyword => {
            // No internal Values to scan
        },
        .closure => {
            const cls = val.toPtr(Closure);
            for (cls.getCapturedValues()) |cap| {
                callback(cap);
            }
        },
        .hashtable => {
            const ht = val.toPtr(HashTable);
            for (ht.getEntries()) |entry| {
                if (!HashTable.isAvailable(entry)) {
                    callback(entry.key);
                    callback(entry.value);
                }
            }
        },
        .forwarding => {
            // Forwarding pointers shouldn't be scanned
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "cons layout" {
    const testing = std.testing;

    try testing.expectEqual(@as(usize, 16), @sizeOf(Cons));
    try testing.expectEqual(@as(usize, 16), @alignOf(Cons));
}

test "cons operations" {
    const testing = std.testing;

    var cons = Cons.init(Value.makeFixnum(1), Value.makeFixnum(2));
    try testing.expectEqual(@as(i64, 1), cons.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), cons.cdr.toFixnum());
}

test "symbol layout" {
    const testing = std.testing;

    // Symbol should be at least 32 bytes for alignment
    try testing.expect(@sizeOf(Symbol) >= 24);
}

test "vector layout" {
    const testing = std.testing;

    try testing.expect(@sizeOf(Vector) >= 24);
}

test "string layout" {
    const testing = std.testing;

    try testing.expectEqual(@as(usize, 16), @sizeOf(String));
}

test "closure layout" {
    const testing = std.testing;

    try testing.expect(@sizeOf(Closure) >= 24);
}
