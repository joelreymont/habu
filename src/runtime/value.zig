//! Tagged value representation for Habu
//!
//! 1+3 bit hybrid tagging scheme:
//!   bit0=1: fixnum (63-bit signed, value >> 1)
//!   bit0=0: pointer | tag in bits 1-3
//!     0: cons, 2: symbol, 4: vector, 6: string
//!     8: closure, 10: keyword, 14: forwarding (GC)
//!   Special: 0 = nil
//!   Character: bit63=1, codepoint in bits 0-20
//!
//! This scheme allows:
//! - Fast fixnum check: (v & 1) == 1
//! - Fast nil check: v == 0
//! - Fast character check: (v >> 63) == 1
//! - Pointer extraction: v & ~0xF (mask off low 4 bits)
//! - Tag extraction: v & 0xE (bits 1-3, ignoring bit 0)

const std = @import("std");

/// Runtime type tags (bits 1-3 of pointer, shifted left by 1)
pub const Tag = enum(u4) {
    cons = 0,
    symbol = 2,
    vector = 4,
    string = 6,
    closure = 8,
    keyword = 10,
    hashtable = 12,
    forwarding = 14, // GC forwarding pointer
};

/// A tagged Habu value (64-bit)
pub const Value = packed struct {
    raw: u64,

    // ========================================================================
    // Constants
    // ========================================================================

    pub const nil: Value = .{ .raw = 0 };
    pub const t: Value = Value.makeFixnum(1); // true = fixnum 1

    const TAG_MASK: u64 = 0xE; // bits 1-3
    const PTR_MASK: u64 = ~@as(u64, 0xF); // clear low 4 bits

    // ========================================================================
    // Type predicates
    // ========================================================================

    /// Check if value is nil (the empty list)
    pub inline fn isNil(self: Value) bool {
        return self.raw == 0;
    }

    /// Check if value is a fixnum (bit0 = 1)
    pub inline fn isFixnum(self: Value) bool {
        return (self.raw & 1) == 1;
    }

    /// Check if value is a pointer (bit0 = 0, bit63 = 0, not nil)
    pub inline fn isPointer(self: Value) bool {
        // bit0=0 and bit63=0 distinguishes pointers from fixnums and characters
        return self.raw != 0 and (self.raw & 1) == 0 and (self.raw >> 63) == 0;
    }

    /// Check if value is a cons cell
    pub inline fn isCons(self: Value) bool {
        return self.isPointer() and self.getTag() == .cons;
    }

    /// Check if value is a symbol
    pub inline fn isSymbol(self: Value) bool {
        return self.isPointer() and self.getTag() == .symbol;
    }

    /// Check if value is a vector
    pub inline fn isVector(self: Value) bool {
        return self.isPointer() and self.getTag() == .vector;
    }

    /// Check if value is a string
    pub inline fn isString(self: Value) bool {
        return self.isPointer() and self.getTag() == .string;
    }

    /// Check if value is a closure
    pub inline fn isClosure(self: Value) bool {
        return self.isPointer() and self.getTag() == .closure;
    }

    /// Check if value is a keyword
    pub inline fn isKeyword(self: Value) bool {
        return self.isPointer() and self.getTag() == .keyword;
    }

    /// Check if value is a hash table
    pub inline fn isHashTable(self: Value) bool {
        return self.isPointer() and self.getTag() == .hashtable;
    }

    /// Check if value is a forwarding pointer (GC)
    pub inline fn isForwarding(self: Value) bool {
        return self.isPointer() and self.getTag() == .forwarding;
    }

    /// Check if value is truthy (not nil)
    pub inline fn isTruthy(self: Value) bool {
        return !self.isNil();
    }

    /// Check if value is a character (bit63=1)
    pub inline fn isCharacter(self: Value) bool {
        return (self.raw >> 63) == 1;
    }

    // ========================================================================
    // Tag and pointer extraction
    // ========================================================================

    /// Get the tag of a pointer value
    pub inline fn getTag(self: Value) Tag {
        std.debug.assert(self.isPointer());
        return @enumFromInt(@as(u4, @truncate(self.raw & TAG_MASK)));
    }

    /// Get the raw pointer (with tag bits cleared)
    pub inline fn toPtr(self: Value, comptime T: type) *T {
        std.debug.assert(self.isPointer());
        return @ptrFromInt(self.raw & PTR_MASK);
    }

    /// Get the raw pointer as usize
    pub inline fn toPtrAddr(self: Value) usize {
        std.debug.assert(self.isPointer());
        return @intCast(self.raw & PTR_MASK);
    }

    // ========================================================================
    // Fixnum operations
    // ========================================================================

    /// Create a fixnum from an integer
    pub inline fn makeFixnum(n: i64) Value {
        // Shift left and set bit 0
        const unsigned: u64 = @bitCast(n);
        return .{ .raw = (unsigned << 1) | 1 };
    }

    /// Extract fixnum value
    pub inline fn toFixnum(self: Value) i64 {
        std.debug.assert(self.isFixnum());
        // Arithmetic right shift to preserve sign
        const signed: i64 = @bitCast(self.raw);
        return signed >> 1;
    }

    // ========================================================================
    // Character operations
    // ========================================================================

    const CHAR_TAG: u64 = 1 << 63;

    /// Create a character from a Unicode code point
    pub inline fn makeCharacter(codepoint: u21) Value {
        // Encoding: bit63=1, bit0=0, codepoint in bits 1-21
        // This ensures bit0=0 so it's not confused with fixnum
        return .{ .raw = CHAR_TAG | (@as(u64, codepoint) << 1) };
    }

    /// Extract character code point
    pub inline fn toCharacter(self: Value) u21 {
        std.debug.assert(self.isCharacter());
        return @truncate((self.raw >> 1) & 0x1FFFFF); // Bits 1-21
    }

    // ========================================================================
    // Pointer construction
    // ========================================================================

    /// Create a tagged pointer value
    pub inline fn makePtr(ptr: anytype, tag: Tag) Value {
        const addr: u64 = @intFromPtr(ptr);
        std.debug.assert(addr & 0xF == 0); // Must be 16-byte aligned
        return .{ .raw = addr | @as(u64, @intFromEnum(tag)) };
    }

    /// Create a cons cell value
    pub inline fn makeCons(ptr: anytype) Value {
        return makePtr(ptr, .cons);
    }

    /// Create a symbol value
    pub inline fn makeSymbol(ptr: anytype) Value {
        return makePtr(ptr, .symbol);
    }

    /// Create a vector value
    pub inline fn makeVector(ptr: anytype) Value {
        return makePtr(ptr, .vector);
    }

    /// Create a string value
    pub inline fn makeString(ptr: anytype) Value {
        return makePtr(ptr, .string);
    }

    /// Create a closure value
    pub inline fn makeClosure(ptr: anytype) Value {
        return makePtr(ptr, .closure);
    }

    /// Create a keyword value
    pub inline fn makeKeyword(ptr: anytype) Value {
        return makePtr(ptr, .keyword);
    }

    /// Create a hash table value
    pub inline fn makeHashTable(ptr: anytype) Value {
        return makePtr(ptr, .hashtable);
    }

    /// Create a forwarding pointer (GC)
    pub inline fn makeForwarding(ptr: anytype) Value {
        return makePtr(ptr, .forwarding);
    }

    // ========================================================================
    // Comparison
    // ========================================================================

    /// Check if two values are eq (same object or same fixnum)
    pub inline fn eq(self: Value, other: Value) bool {
        return self.raw == other.raw;
    }

    // ========================================================================
    // Debug formatting
    // ========================================================================

    pub fn format(
        self: Value,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.isNil()) {
            try writer.writeAll("nil");
        } else if (self.isFixnum()) {
            try writer.print("{d}", .{self.toFixnum()});
        } else if (self.isCharacter()) {
            const cp = self.toCharacter();
            if (cp < 128) {
                try writer.print("#\\{c}", .{@as(u8, @intCast(cp))});
            } else {
                try writer.print("#\\U+{X:0>4}", .{cp});
            }
        } else {
            const tag = self.getTag();
            const addr = self.toPtrAddr();
            try writer.print("<{s} @{x}>", .{ @tagName(tag), addr });
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "nil" {
    const testing = std.testing;

    try testing.expect(Value.nil.isNil());
    try testing.expect(!Value.nil.isFixnum());
    try testing.expect(!Value.nil.isPointer());
    try testing.expect(!Value.nil.isTruthy());
}

test "fixnum" {
    const testing = std.testing;

    const zero = Value.makeFixnum(0);
    try testing.expect(zero.isFixnum());
    try testing.expect(!zero.isNil());
    try testing.expectEqual(@as(i64, 0), zero.toFixnum());

    const pos = Value.makeFixnum(42);
    try testing.expect(pos.isFixnum());
    try testing.expectEqual(@as(i64, 42), pos.toFixnum());

    const neg = Value.makeFixnum(-17);
    try testing.expect(neg.isFixnum());
    try testing.expectEqual(@as(i64, -17), neg.toFixnum());

    // Large values
    const big = Value.makeFixnum(0x1FFFFFFFFFFFFFF); // Max 61-bit positive
    try testing.expectEqual(@as(i64, 0x1FFFFFFFFFFFFFF), big.toFixnum());
}

test "tagged pointer" {
    const testing = std.testing;

    // Use aligned address for test
    const addr: usize = 0x1000;
    const ptr: *u8 = @ptrFromInt(addr);

    const cons = Value.makeCons(ptr);
    try testing.expect(cons.isPointer());
    try testing.expect(cons.isCons());
    try testing.expect(!cons.isSymbol());
    try testing.expectEqual(addr, cons.toPtrAddr());

    const symbol = Value.makeSymbol(ptr);
    try testing.expect(symbol.isSymbol());
    try testing.expectEqual(addr, symbol.toPtrAddr());

    const string = Value.makeString(ptr);
    try testing.expect(string.isString());
    try testing.expectEqual(@as(u4, 6), @intFromEnum(string.getTag()));
}

test "eq comparison" {
    const testing = std.testing;

    const a = Value.makeFixnum(42);
    const b = Value.makeFixnum(42);
    const c = Value.makeFixnum(43);

    try testing.expect(a.eq(b));
    try testing.expect(!a.eq(c));
    try testing.expect(Value.nil.eq(Value.nil));
}

test "truthiness" {
    const testing = std.testing;

    try testing.expect(!Value.nil.isTruthy());
    try testing.expect(Value.makeFixnum(0).isTruthy()); // 0 is truthy (not nil)
    try testing.expect(Value.t.isTruthy());
}
