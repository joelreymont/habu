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

/// Check if arg_class is a subtype of specializer_class
/// This is true if specializer_class appears in arg_class's CPL
pub fn isSubtype(arg_class: *const Class, specializer_class: Value) bool {
    var cpl = arg_class.cpl;
    while (cpl.isCons()) {
        const cons = cpl.toPtr(Cons);
        if (cons.car.eq(specializer_class)) return true;
        cpl = cons.cdr;
    }
    return false;
}

/// Compare method specificity: returns true if m1 is more specific than m2
/// Two methods are compared by finding the leftmost argument where specializers differ,
/// then checking which specializer appears first in that argument's CPL
pub fn isMoreSpecific(m1_specs: []const Value, m2_specs: []const Value, arg_classes: []const *const Class) bool {
    const len = @min(m1_specs.len, m2_specs.len);
    for (0..len) |i| {
        if (m1_specs[i].eq(m2_specs[i])) continue;

        // Found first differing specializer - check CPL position
        var cpl = arg_classes[i].cpl;
        while (cpl.isCons()) {
            const cons = cpl.toPtr(Cons);
            if (cons.car.eq(m1_specs[i])) return true;
            if (cons.car.eq(m2_specs[i])) return false;
            cpl = cons.cdr;
        }
        return false;
    }
    return false;
}

/// C3 linearization: compute class precedence list
/// L[C(B1...BN)] = C + merge(L[B1]...L[BN], B1...BN)
pub fn computeCpl(allocator: std.mem.Allocator, class: Value, direct_supers: []const Value, get_cpl: *const fn (Value) []const Value) ![]Value {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const tmp = arena.allocator();

    // Build input lists: parent CPLs + direct supers list
    var lists = std.ArrayList(std.ArrayList(Value)){};
    defer {
        for (lists.items) |*l| l.deinit(tmp);
        lists.deinit(tmp);
    }

    for (direct_supers) |sup| {
        var parent_cpl = std.ArrayList(Value){};
        for (get_cpl(sup)) |c| try parent_cpl.append(tmp, c);
        try lists.append(tmp, parent_cpl);
    }

    var supers_list = std.ArrayList(Value){};
    for (direct_supers) |s| try supers_list.append(tmp, s);
    try lists.append(tmp, supers_list);

    // Result: class + merge
    var result = std.ArrayList(Value){};
    try result.append(tmp, class);

    // Merge loop
    while (true) {
        var all_empty = true;
        for (lists.items) |l| {
            if (l.items.len > 0) {
                all_empty = false;
                break;
            }
        }
        if (all_empty) break;

        // Find eligible head: appears as head but not in any tail
        var candidate: ?Value = null;
        for (lists.items) |l| {
            if (l.items.len == 0) continue;
            const head = l.items[0];

            var in_tail = false;
            for (lists.items) |other| {
                if (other.items.len <= 1) continue;
                for (other.items[1..]) |t| {
                    if (head.eq(t)) {
                        in_tail = true;
                        break;
                    }
                }
                if (in_tail) break;
            }

            if (!in_tail) {
                candidate = head;
                break;
            }
        }

        if (candidate) |c| {
            try result.append(tmp, c);
            // Remove from all lists
            for (lists.items) |*l| {
                var i: usize = 0;
                while (i < l.items.len) {
                    if (l.items[i].eq(c)) {
                        _ = l.orderedRemove(i);
                    } else {
                        i += 1;
                    }
                }
            }
        } else {
            return error.InconsistentPrecedence;
        }
    }

    return allocator.dupe(Value, result.items);
}

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
/// Note: This is "base-string" in CL terminology (8-bit characters)
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

/// String32: UTF-32 string for full Unicode support
/// Size: 24 bytes header + data
pub const String32 = extern struct {
    kind: BoxedKind = .string32, // Must be first - discriminator
    /// Length in codepoints
    length: u32,
    _pad: u32 = 0,
    /// Pointer to u32 codepoint data
    data: [*]u32,

    pub fn codepoints(self: *const String32) []const u32 {
        return self.data[0..self.length];
    }

    pub fn mutableCodepoints(self: *String32) []u32 {
        return self.data[0..self.length];
    }
};

/// Closure: function + captured environment
/// Size: 32 bytes header + captures
pub const Closure = extern struct {
    /// Chunk value (GC-managed) - stores bytecode or JIT code
    code: Value,
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
    condition = 9,
    class = 10,
    string32 = 11,
    slotdef = 12,
    generic_function = 13,
    method = 14,
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
    // Compound stream types (ANSI CL)
    broadcast = 5, // writes to multiple streams
    concatenated = 6, // reads from sequence of streams
    echo = 7, // echoes input to output
    synonym = 8, // delegates to symbol's value
    two_way = 9, // bidirectional: input + output
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

    /// Get value by key, returns null if not found
    pub fn get(self: *const HashTable, heap: anytype, key: Value) ?Value {
        _ = heap;
        const h = @import("../runtime/primitives/hash.zig").hashValue(key);
        var idx = h % self.capacity;
        var i: usize = 0;
        while (i < self.capacity) : (i += 1) {
            const entry = self.entries[idx];
            if (isEmpty(entry)) return null;
            if (!isDeleted(entry) and self.keysEqual(entry.key, key)) {
                return entry.value;
            }
            idx = (idx + 1) % self.capacity;
        }
        return null;
    }

    /// Put key-value pair, returns error if needs rehashing
    pub fn put(self: *HashTable, key: Value, value: Value) !void {
        // Check load factor, signal need to grow
        if (self.count * 4 >= self.capacity * 3) {
            return error.HashTableNeedsGrowth;
        }

        const h = @import("../runtime/primitives/hash.zig").hashValue(key);
        var idx = h % self.capacity;
        var i: usize = 0;
        var first_deleted: ?usize = null;

        while (i < self.capacity) : (i += 1) {
            const entry = &self.entries[idx];
            if (isEmpty(entry.*)) {
                // Use first deleted slot if found, otherwise use empty slot
                const target_idx = first_deleted orelse idx;
                self.entries[target_idx] = .{ .key = key, .value = value };
                self.count += 1;
                return;
            }
            if (isDeleted(entry.*) and first_deleted == null) {
                first_deleted = idx;
            } else if (!isDeleted(entry.*) and self.keysEqual(entry.key, key)) {
                // Update existing
                entry.value = value;
                return;
            }
            idx = (idx + 1) % self.capacity;
        }
        return error.HashTableFull;
    }

    /// Remove key, returns true if found
    pub fn remove(self: *HashTable, key: Value) bool {
        const h = @import("../runtime/primitives/hash.zig").hashValue(key);
        var idx = h % self.capacity;
        var i: usize = 0;
        while (i < self.capacity) : (i += 1) {
            const entry = &self.entries[idx];
            if (isEmpty(entry.*)) return false;
            if (!isDeleted(entry.*) and self.keysEqual(entry.key, key)) {
                entry.* = .{ .key = DELETED, .value = Value.nil };
                self.count -= 1;
                return true;
            }
            idx = (idx + 1) % self.capacity;
        }
        return false;
    }

    /// Clear all entries
    pub fn clear(self: *HashTable) void {
        for (0..self.capacity) |i| {
            self.entries[i] = .{ .key = EMPTY, .value = Value.nil };
        }
        self.count = 0;
    }

    /// Compare keys according to hash table test type
    fn keysEqual(self: *const HashTable, a: Value, b: Value) bool {
        return switch (self.test_type) {
            .eq => a.raw == b.raw,
            .eql => blk: {
                if (a.raw == b.raw) break :blk true;
                if (a.typeKind() != b.typeKind()) break :blk false;
                // eql: same for numbers with same value
                if (a.isFixnum() and b.isFixnum()) break :blk a.toFixnum() == b.toFixnum();
                if (a.isFloat() and b.isFloat()) break :blk a.toFloat() == b.toFloat();
                if (a.isCharacter() and b.isCharacter()) break :blk a.toCharacter() == b.toCharacter();
                break :blk false;
            },
            .equal => blk: {
                if (a.raw == b.raw) break :blk true;
                const tk_a = a.typeKind();
                const tk_b = b.typeKind();
                if (tk_a != tk_b) break :blk false;
                // equal: compares string contents
                if (tk_a == .string) {
                    const sa = a.toPtr(String);
                    const sb = b.toPtr(String);
                    break :blk std.mem.eql(u8, sa.bytes(), sb.bytes());
                }
                if (tk_a == .string32) {
                    const sa = a.toPtr(String32);
                    const sb = b.toPtr(String32);
                    break :blk std.mem.eql(u32, sa.codepoints(), sb.codepoints());
                }
                // Fall back to eql for other types
                break :blk self.keysEqual(a, b); // This would recurse with .eql
            },
        };
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

    pub fn readU8(self: *const Chunk, offset: usize) u8 {
        return self.getCode()[offset];
    }

    pub fn readU16(self: *const Chunk, offset: usize) u16 {
        const c = self.getCode();
        return @as(u16, c[offset]) | (@as(u16, c[offset + 1]) << 8);
    }

    pub fn readI16(self: *const Chunk, offset: usize) i16 {
        const c = self.getCode();
        const val = @as(u16, c[offset]) | (@as(u16, c[offset + 1]) << 8);
        return @bitCast(val);
    }

    pub fn readI32(self: *const Chunk, offset: usize) i32 {
        const c = self.getCode();
        const val = @as(u32, c[offset]) |
            (@as(u32, c[offset + 1]) << 8) |
            (@as(u32, c[offset + 2]) << 16) |
            (@as(u32, c[offset + 3]) << 24);
        return @bitCast(val);
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
                .string32 => {
                    const s32 = val.toPtr(String32);
                    // Header + u32 codepoint data (aligned to 8)
                    break :blk @sizeOf(String32) + std.mem.alignForward(usize, s32.length * 4, 8);
                },
                .rational => @sizeOf(Rational),
                .complex => @sizeOf(Complex),
                .stream => @sizeOf(Stream),
                .bignum => @sizeOf(Bignum),
                .pathname => @sizeOf(Pathname),
                .package => @sizeOf(Package),
                .condition => @sizeOf(Condition),
                .class => {
                    const cls = val.toPtr(Class);
                    break :blk @sizeOf(Class) + cls.num_shared * @sizeOf(Value);
                },
                .slotdef => @sizeOf(SlotDefinition),
                .generic_function => @sizeOf(GenericFunction),
                .method => @sizeOf(Method),
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
                .chunk => {
                    const chunk = val.toPtr(Chunk);
                    for (chunk.getConstants()) |c| {
                        callback(c);
                    }
                },
                .string32, .rational, .complex, .stream, .bignum, .pathname, .array => {
                    // No internal Values to scan
                },
                .class => {
                    const cls = val.toPtr(Class);
                    callback(cls.name);
                    for (cls.shared_slots[0..cls.num_shared]) |slot_val| {
                        callback(slot_val);
                    }
                },
                .condition => {
                    // No internal Values to scan
                },
                .slotdef => {
                    // No internal Values to scan
                },
                .generic_function => {
                    const gf = val.toPtr(GenericFunction);
                    callback(gf.name);
                    callback(gf.lambda_list);
                    callback(gf.methods);
                },
                .method => {
                    const method = val.toPtr(Method);
                    callback(method.qualifiers);
                    callback(method.specializers);
                    callback(method.lambda_list);
                    callback(method.function);
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

/// Condition: base for error/warning hierarchy
/// Size: 32 bytes (4 words)
pub const Condition = extern struct {
    kind: BoxedKind align(16),
    /// Condition type (interned symbol)
    type_sym: Value,
    /// Format control string (or nil)
    format_control: Value,
    /// Format arguments list (or nil)
    format_args: Value,
};

test "condition layout" {
    const testing = std.testing;
    try testing.expectEqual(@as(usize, 32), @sizeOf(Condition));
}

/// Class: CLOS class metaobject with shared slot storage
/// Size: 48 bytes + slot data
pub const Class = extern struct {
    kind: BoxedKind align(16),
    /// Class name (symbol)
    name: Value,
    /// Direct superclasses (list of Values)
    direct_supers: Value,
    /// Class precedence list (list of Values)
    cpl: Value,
    /// Direct slot definitions (list of SlotDefinition objects)
    direct_slots: Value,
    /// All slot definitions (list of SlotDefinition objects)
    slots: Value,
    /// Metaclass (class of this class)
    metaclass: Value,
    /// Number of shared slots
    num_shared: u32,
    _pad: u32 = 0,
    /// Pointer to shared slot values array
    shared_slots: [*]Value,
};

pub const SlotDefinition = extern struct {
    kind: BoxedKind align(16),
    /// Slot name (symbol)
    name: Value,
    /// Initform (s-expr or nil)
    initform: Value,
    /// Initargs (list of keywords)
    initargs: Value,
    /// Readers (list of symbols)
    readers: Value,
    /// Writers (list of symbols)
    writers: Value,
    /// Allocation type (symbol: :instance or :class)
    allocation: Value,
    /// Type specifier (type or t)
    slot_type: Value,
};

/// Generic function object
pub const GenericFunction = extern struct {
    kind: BoxedKind align(16),
    /// GF name (symbol)
    name: Value,
    /// Lambda list (list of symbols, may include &rest etc)
    lambda_list: Value,
    /// Methods (list of Method objects)
    methods: Value,
    /// Dispatcher function (closure that implements method dispatch)
    dispatcher: Value,
};

/// Method object
pub const Method = extern struct {
    kind: BoxedKind align(16),
    /// Qualifiers (list of symbols: :before, :after, :around)
    qualifiers: Value,
    /// Specializers (list of class names or (eql value))
    specializers: Value,
    /// Lambda list (list of symbols)
    lambda_list: Value,
    /// Function (closure)
    function: Value,
};

/// Instance: CLOS instance representation
/// Stored as Vector: #(class slot1 slot2 ...)
/// data[0] = class (Value pointing to Class object)
/// data[1..] = instance slot values (may be Value.unbound)
pub const Instance = Vector;
