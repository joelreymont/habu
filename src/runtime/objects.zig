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
/// Size: 32 bytes header + data
pub const Vector = extern struct {
    /// Number of elements
    length: u64,
    /// Capacity (for resizable vectors)
    capacity: u64,
    /// Pointer to element data (array of Values)
    data: [*]Value,
    /// Optional fill-pointer (0xFFFFFFFFFFFFFFFF = none)
    /// Low 63 bits: fill-pointer value
    /// High bit: adjustable flag
    fill_pointer: u64,

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

    pub fn hasFillPointer(self: *const Vector) bool {
        return self.fill_pointer != 0xFFFFFFFFFFFFFFFF;
    }

    pub fn getFillPointer(self: *const Vector) ?u64 {
        if (self.fill_pointer == 0xFFFFFFFFFFFFFFFF) return null;
        return self.fill_pointer & 0x7FFFFFFFFFFFFFFF;
    }

    pub fn setFillPointer(self: *Vector, fp: ?u64) void {
        if (fp) |p| {
            const adj_bit = if (self.fill_pointer == 0xFFFFFFFFFFFFFFFF)
                0
            else
                self.fill_pointer & 0x8000000000000000;
            self.fill_pointer = (p & 0x7FFFFFFFFFFFFFFF) | adj_bit;
        } else {
            self.fill_pointer = 0xFFFFFFFFFFFFFFFF;
        }
    }

    pub fn isAdjustable(self: *const Vector) bool {
        return (self.fill_pointer & 0x8000000000000000) != 0;
    }

    pub fn setAdjustable(self: *Vector, adj: bool) void {
        if (adj) {
            self.fill_pointer |= 0x8000000000000000;
        } else {
            self.fill_pointer &= 0x7FFFFFFFFFFFFFFF;
        }
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

/// Boxed object kind - first word of all boxed objects
pub const BoxedKind = enum(u64) {
    hashtable = 0,
    rational = 1,
    complex = 2,
    stream = 3,
    bignum = 4,
    array = 5,
    pathname = 6,
    package = 7,
    chunk = 8,
};

/// Stream direction
pub const StreamDirection = enum(u8) {
    input = 0,
    output = 1,
};

/// Stream type
pub const StreamType = enum(u8) {
    string = 0,
    file = 1,
    stdin = 2,
    stdout = 3,
    stderr = 4,
};

/// Stream object for I/O operations
pub const Stream = extern struct {
    kind: BoxedKind, // Must be first - discriminator (= .stream)
    direction: StreamDirection,
    stream_type: StreamType,
    closed: bool,
    _padding: u8 = 0,
    /// For string streams: current read/write position
    position: u64,
    /// For string streams: pointer to string data (input) or ArrayList buffer (output)
    data_ptr: u64,
    /// For string streams: length of data (input) or capacity (output)
    length: u64,
    /// For file streams: file descriptor
    file_fd: i32,
    /// Pushback buffer for unread-char (0xFF = none)
    pushback_char: u8 = 0xFF,
    _padding2: [3]u8 = [_]u8{0} ** 3,
    /// For string streams: reference to source string (prevents GC)
    /// For file streams: Value.nil
    source_value: Value = Value.nil,

    pub fn isInput(self: *const Stream) bool {
        return self.direction == .input;
    }

    pub fn isOutput(self: *const Stream) bool {
        return self.direction == .output;
    }

    pub fn isClosed(self: *const Stream) bool {
        return self.closed;
    }

    pub fn make(direction: StreamDirection, stream_type: StreamType, file_fd: i32) Stream {
        return .{
            .kind = .stream,
            .direction = direction,
            .stream_type = stream_type,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = file_fd,
            .pushback_char = 0xFF,
        };
    }

    pub fn makeString(direction: StreamDirection, data_ptr: u64, length: u64) Stream {
        return .{
            .kind = .stream,
            .direction = direction,
            .stream_type = .string,
            .closed = false,
            .position = 0,
            .data_ptr = data_ptr,
            .length = length,
            .file_fd = -1,
            .pushback_char = 0xFF,
        };
    }
};

/// Rational number (p/q where gcd(p,q)=1, q>0)
pub const Rational = extern struct {
    kind: BoxedKind, // Must be first - discriminator
    numerator: i64,
    denominator: i64,

    pub fn make(num: i64, den: i64) Rational {
        const normalized = normalize(num, den);
        return .{ .kind = .rational, .numerator = normalized.num, .denominator = normalized.den };
    }

    fn normalize(num: i64, den: i64) struct { num: i64, den: i64 } {
        if (den == 0) return .{ .num = 0, .den = 1 };
        var n = num;
        var d = den;
        if (d < 0) {
            n = -n;
            d = -d;
        }
        const g = gcd(if (n < 0) -n else n, d);
        return .{ .num = @divTrunc(n, g), .den = @divTrunc(d, g) };
    }

    fn gcd(a: i64, b: i64) i64 {
        var x = a;
        var y = b;
        while (y != 0) {
            const t = y;
            y = @rem(x, y);
            x = t;
        }
        return x;
    }
};

/// Complex number (real + imag*i)
pub const Complex = extern struct {
    kind: BoxedKind, // Must be first - discriminator
    real: f64,
    imag: f64,

    pub fn make(real: f64, imag: f64) Complex {
        return .{ .kind = .complex, .real = real, .imag = imag };
    }

    pub fn add(a: Complex, b: Complex) Complex {
        return make(a.real + b.real, a.imag + b.imag);
    }

    pub fn sub(a: Complex, b: Complex) Complex {
        return make(a.real - b.real, a.imag - b.imag);
    }

    pub fn mul(a: Complex, b: Complex) Complex {
        // (a + bi)(c + di) = (ac - bd) + (ad + bc)i
        return make(
            a.real * b.real - a.imag * b.imag,
            a.real * b.imag + a.imag * b.real,
        );
    }

    pub fn div(a: Complex, b: Complex) Complex {
        // (a + bi)/(c + di) = ((ac + bd) + (bc - ad)i) / (c² + d²)
        const denom = b.real * b.real + b.imag * b.imag;
        if (denom == 0) return make(0, 0);
        return make(
            (a.real * b.real + a.imag * b.imag) / denom,
            (a.imag * b.real - a.real * b.imag) / denom,
        );
    }

    pub fn abs(self: Complex) f64 {
        return @sqrt(self.real * self.real + self.imag * self.imag);
    }

    pub fn conjugate(self: Complex) Complex {
        return make(self.real, -self.imag);
    }
};

/// Bignum: arbitrary-precision integer
/// Uses inline limbs array with sign bit
pub const Bignum = extern struct {
    kind: BoxedKind, // Must be first - discriminator
    /// Number of used limbs (negative = negative number)
    size: i64,
    /// Limbs array (least significant first)
    limbs: [8]u64,

    pub fn make(n: i64) Bignum {
        var result = Bignum{
            .kind = .bignum,
            .size = 0,
            .limbs = [_]u64{0} ** 8,
        };

        if (n == 0) return result;

        const abs_n: u64 = if (n < 0) @as(u64, @intCast(-n)) else @as(u64, @intCast(n));
        result.limbs[0] = abs_n;
        result.size = if (n < 0) -1 else 1;

        return result;
    }

    pub fn isNegative(self: *const Bignum) bool {
        return self.size < 0;
    }

    pub fn isZero(self: *const Bignum) bool {
        return self.size == 0;
    }
};

/// Multi-dimensional array
/// Supports up to 8 dimensions
pub const Array = extern struct {
    /// Boxed object discriminator (must be first)
    kind: BoxedKind = .array,
    /// Number of dimensions (1-8)
    rank: u8,
    /// Size of each dimension
    dimensions: [8]u64,
    /// Total number of elements (product of all dimensions)
    total_size: u64,
    /// Pointer to data array (Value elements)
    data_ptr: u64,
};

/// Uses open addressing with linear probing
/// Size: 40 bytes header + entries array
pub const HashTable = extern struct {
    /// Boxed object discriminator (must be first)
    kind: BoxedKind = .hashtable,
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

/// Package object for symbol namespace management
pub const Package = extern struct {
    kind: BoxedKind = .package,
    /// Package name (symbol)
    name: Value,
    /// List of nickname symbols
    nicknames: Value,
    /// List of packages this package uses
    use_list: Value,
    /// Hash table of exported symbols
    exports: Value,
    /// Hash table of internal symbols
    symbols: Value,
    /// List of shadowed symbol names
    shadowing: Value,
};

/// Pathname object for file path manipulation
/// Follows Common Lisp pathname component model
pub const Pathname = extern struct {
    /// Boxed object discriminator (must be first)
    kind: BoxedKind = .pathname,
    /// Host component (string or nil)
    host: Value,
    /// Device component (string or nil)
    device: Value,
    /// Directory component (list of strings, or :absolute/:relative as first element)
    directory: Value,
    /// Name component (string or nil)
    name: Value,
    /// Type/extension component (string or nil)
    type: Value,
    /// Version component (fixnum, :newest, :unspecific, or nil)
    version: Value,
};

/// Compiled bytecode chunk (GC-managed)
pub const Chunk = extern struct {
    /// Boxed object discriminator (must be first)
    kind: BoxedKind = .chunk,
    /// Number of constants in pool
    const_count: u32,
    /// Number of bytecode bytes
    code_len: u32,
    /// Function arity (required params)
    arity: u8,
    /// Optional parameter count
    opt_count: u8,
    /// Keyword parameter count
    key_count: u8,
    /// Has rest parameter
    has_rest: u8, // bool as u8 for alignment
    /// Number of local variables
    num_locals: u8,
    /// Padding for alignment
    _pad: [3]u8 = .{0} ** 3,
    /// Constant pool pointer (points to inline array after header)
    const_pool: [*]Value,
    /// Bytecode pointer (points to inline array after constants)
    code: [*]u8,

    pub fn getConstants(self: *const Chunk) []Value {
        return self.const_pool[0..self.const_count];
    }

    pub fn getCode(self: *const Chunk) []u8 {
        return self.code[0..self.code_len];
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
        .boxed => blk: {
            // Check discriminator to determine actual type
            const kind_ptr: *const BoxedKind = @ptrFromInt(val.raw & ~@as(u64, 0xF));
            break :blk switch (kind_ptr.*) {
                .hashtable => {
                    const ht = val.toPtr(HashTable);
                    // Header + entries array
                    break :blk @sizeOf(HashTable) + ht.capacity * @sizeOf(HashEntry);
                },
                .array => {
                    const arr = val.toPtr(Array);
                    // Header + data array
                    break :blk @sizeOf(Array) + arr.total_size * @sizeOf(Value);
                },
                .rational => @sizeOf(Rational),
                .complex => @sizeOf(Complex),
                .stream => @sizeOf(Stream),
                .bignum => @sizeOf(Bignum),
                .pathname => @sizeOf(Pathname),
                .package => @sizeOf(Package),
                .chunk => {
                    const chunk = val.toPtr(Chunk);
                    // Header + const pool + bytecode (both aligned to 8)
                    const const_size = chunk.const_count * @sizeOf(Value);
                    const code_size = std.mem.alignForward(usize, chunk.code_len, 8);
                    break :blk @sizeOf(Chunk) + const_size + code_size;
                },
            };
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
        .boxed => {
            // Check discriminator to determine actual type
            const kind_ptr: *const BoxedKind = @ptrFromInt(val.raw & ~@as(u64, 0xF));
            switch (kind_ptr.*) {
                .hashtable => {
                    const ht = val.toPtr(HashTable);
                    for (ht.getEntries()) |entry| {
                        if (!HashTable.isAvailable(entry)) {
                            callback(entry.key);
                            callback(entry.value);
                        }
                    }
                },
                .package => {
                    const pkg = val.toPtr(Package);
                    callback(pkg.name);
                    callback(pkg.nicknames);
                    callback(pkg.use_list);
                    callback(pkg.exports);
                    callback(pkg.symbols);
                    callback(pkg.shadowing);
                },
                .rational, .complex, .stream, .bignum, .pathname, .array => {
                    // No internal Values to scan
                },
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
