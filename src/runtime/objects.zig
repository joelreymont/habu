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

/// String: immutable byte sequence
/// Size: 16 bytes header + data (inline for short strings)
pub const String = extern struct {
    /// Length in bytes
    length: u64,
    /// Pointer to byte data
    data: [*]const u8,

    pub fn bytes(self: *const String) []const u8 {
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

// ============================================================================
// Object size calculations (for GC)
// ============================================================================

/// Get the size of an object in bytes given its tag
pub fn objectSize(val: Value) usize {
    const tag = val.getTag();
    return switch (tag) {
        .cons => @sizeOf(Cons),
        .symbol => @sizeOf(Symbol),
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
        .keyword => @sizeOf(Keyword),
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
