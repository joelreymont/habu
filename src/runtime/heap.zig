//! Heap management for Habu
//!
//! Uses a semispace design:
//! - Two equally-sized spaces: from-space and to-space
//! - Bump pointer allocation in from-space
//! - Cheney copying GC copies live objects to to-space
//! - After GC, spaces are swapped
//!
//! All allocations are 16-byte aligned for the tagging scheme.

const std = @import("std");
const Value = @import("value.zig").Value;
const objects = @import("objects.zig");

pub const ALIGNMENT: usize = 16;

/// Interned symbol table for eq comparison
pub const SymbolTable = struct {
    /// Map from name to interned symbol Value
    map: std.StringHashMapUnmanaged(Value),
    /// Backing allocator for keys
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .map = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *SymbolTable) void {
        // Free all interned symbol keys
        var it = self.map.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.map.deinit(self.allocator);
    }

    pub fn get(self: *const SymbolTable, name: []const u8) ?Value {
        return self.map.get(name);
    }

    pub fn put(self: *SymbolTable, name: []const u8, sym: Value) !void {
        const key = try self.allocator.dupe(u8, name);
        try self.map.put(self.allocator, key, sym);
    }
};

/// Heap configuration
pub const Config = struct {
    /// Total heap size (both semispaces combined)
    total_size: usize = 64 * 1024 * 1024, // 64MB default
    /// GC threshold (trigger GC when from-space is this full)
    gc_threshold: f32 = 0.9,
};

/// Semispace heap with bump allocation
pub const Heap = struct {
    /// Backing memory (both semispaces)
    memory: []align(ALIGNMENT) u8,
    /// Size of each semispace
    space_size: usize,
    /// Current from-space start
    from_start: [*]align(ALIGNMENT) u8,
    /// Current to-space start
    to_start: [*]align(ALIGNMENT) u8,
    /// Bump pointer (next allocation point)
    alloc_ptr: [*]align(ALIGNMENT) u8,
    /// End of from-space
    from_end: [*]u8,
    /// GC threshold in bytes
    gc_threshold: usize,
    /// Backing allocator (for the memory buffer itself)
    backing_allocator: std.mem.Allocator,
    /// Statistics
    stats: Stats,
    /// Interned symbol table
    symbols: SymbolTable,

    pub const Stats = struct {
        allocations: usize = 0,
        bytes_allocated: usize = 0,
        gc_count: usize = 0,
        bytes_copied: usize = 0,
    };

    /// Initialize a new heap
    pub fn init(allocator: std.mem.Allocator, config: Config) !Heap {
        const space_size = config.total_size / 2;
        // Zig 0.15: alignment is an enum, .@"16" for 16-byte alignment
        const memory = try allocator.alignedAlloc(u8, .@"16", config.total_size);

        const from_start: [*]align(ALIGNMENT) u8 = @alignCast(memory.ptr);
        const to_start: [*]align(ALIGNMENT) u8 = @alignCast(memory.ptr + space_size);

        return .{
            .memory = memory,
            .space_size = space_size,
            .from_start = from_start,
            .to_start = to_start,
            .alloc_ptr = from_start,
            .from_end = memory.ptr + space_size,
            .gc_threshold = @intFromFloat(@as(f32, @floatFromInt(space_size)) * config.gc_threshold),
            .backing_allocator = allocator,
            .stats = .{},
            .symbols = SymbolTable.init(allocator),
        };
    }

    /// Deinitialize heap
    pub fn deinit(self: *Heap) void {
        self.symbols.deinit();
        self.backing_allocator.free(self.memory);
    }

    /// Get current allocation position
    pub fn getAllocPtr(self: *const Heap) usize {
        return @intFromPtr(self.alloc_ptr);
    }

    /// Get bytes used in from-space
    pub fn bytesUsed(self: *const Heap) usize {
        return @intFromPtr(self.alloc_ptr) - @intFromPtr(self.from_start);
    }

    /// Get bytes available in from-space
    pub fn bytesAvailable(self: *const Heap) usize {
        return self.space_size - self.bytesUsed();
    }

    /// Check if GC should be triggered
    pub fn shouldGC(self: *const Heap) bool {
        return self.bytesUsed() >= self.gc_threshold;
    }

    /// Allocate raw bytes (16-byte aligned)
    pub fn allocRaw(self: *Heap, size: usize) ?[*]align(ALIGNMENT) u8 {
        const aligned_size = std.mem.alignForward(usize, size, ALIGNMENT);

        const current = @intFromPtr(self.alloc_ptr);
        const end = @intFromPtr(self.from_end);

        if (current + aligned_size > end) {
            return null; // Out of memory, need GC
        }

        const result = self.alloc_ptr;
        self.alloc_ptr = @ptrFromInt(current + aligned_size);

        self.stats.allocations += 1;
        self.stats.bytes_allocated += aligned_size;

        return result;
    }

    /// Allocate an object of a specific type
    pub fn alloc(self: *Heap, comptime T: type) ?*T {
        const ptr = self.allocRaw(@sizeOf(T)) orelse return null;
        return @ptrCast(@alignCast(ptr));
    }

    /// Allocate a cons cell
    pub fn allocCons(self: *Heap, car: Value, cdr: Value) ?Value {
        const cons = self.alloc(objects.Cons) orelse return null;
        cons.* = objects.Cons.init(car, cdr);
        return Value.makeCons(cons);
    }

    /// Allocate a vector with given capacity
    pub fn allocVector(self: *Heap, length: usize, capacity: usize) ?Value {
        // Allocate header + data array together
        const data_size = capacity * @sizeOf(Value);
        const total_size = @sizeOf(objects.Vector) + data_size;

        const ptr = self.allocRaw(total_size) orelse return null;
        const vec: *objects.Vector = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]Value = @ptrCast(@alignCast(ptr + @sizeOf(objects.Vector)));

        // Initialize data to nil
        for (0..capacity) |i| {
            data_ptr[i] = Value.nil;
        }

        vec.* = .{
            .length = length,
            .capacity = capacity,
            .data = data_ptr,
        };

        return Value.makeVector(vec);
    }

    /// Allocate a string (copies the bytes)
    pub fn allocString(self: *Heap, bytes: []const u8) ?Value {
        const aligned_len = std.mem.alignForward(usize, bytes.len, 8);
        const total_size = @sizeOf(objects.String) + aligned_len;

        const ptr = self.allocRaw(total_size) orelse return null;
        const str: *objects.String = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

        // Copy bytes
        @memcpy(data_ptr[0..bytes.len], bytes);

        str.* = .{
            .length = bytes.len,
            .data = data_ptr,
        };

        return Value.makeString(str);
    }

    /// Allocate a closure
    pub fn allocClosure(self: *Heap, code: *const anyopaque, arity: u32, captures: []const Value) ?Value {
        const total_size = @sizeOf(objects.Closure) + captures.len * @sizeOf(Value);

        const ptr = self.allocRaw(total_size) orelse return null;
        const closure: *objects.Closure = @ptrCast(@alignCast(ptr));

        // Captures follow immediately after header
        const captures_ptr: [*]Value = @ptrCast(@alignCast(ptr + @sizeOf(objects.Closure)));

        // Copy captures
        for (captures, 0..) |cap, i| {
            captures_ptr[i] = cap;
        }

        closure.* = .{
            .code = code,
            .arity = arity,
            .num_captures = @intCast(captures.len),
            .captures = captures_ptr,
        };

        return Value.makeClosure(closure);
    }

    /// Allocate a hash table with given initial capacity
    pub fn allocHashTable(self: *Heap, capacity: usize) ?Value {
        const actual_capacity = if (capacity < 8) 8 else capacity;
        const total_size = @sizeOf(objects.HashTable) + actual_capacity * @sizeOf(objects.HashEntry);

        const ptr = self.allocRaw(total_size) orelse return null;
        const ht: *objects.HashTable = @ptrCast(@alignCast(ptr));

        // Entries follow immediately after header
        const entries_ptr: [*]objects.HashEntry = @ptrCast(@alignCast(ptr + @sizeOf(objects.HashTable)));

        // Initialize all entries to EMPTY
        for (0..actual_capacity) |i| {
            entries_ptr[i] = .{
                .key = objects.HashTable.EMPTY,
                .value = Value.nil,
            };
        }

        ht.* = .{
            .count = 0,
            .capacity = actual_capacity,
            .entries = entries_ptr,
        };

        return Value.makeHashTable(ht);
    }

    /// Allocate a symbol from a string
    pub fn allocSymbol(self: *Heap, name: []const u8) ?Value {
        const aligned_name_len = std.mem.alignForward(usize, name.len, 8);
        const total_size = @sizeOf(objects.Symbol) + aligned_name_len;

        const ptr = self.allocRaw(total_size) orelse return null;
        const sym: *objects.Symbol = @ptrCast(@alignCast(ptr));
        const name_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Symbol));

        @memcpy(name_ptr[0..name.len], name);

        sym.* = .{
            .name_len = name.len,
            .name_ptr = name_ptr,
            .plist = Value.nil,
            .reserved = 0,
        };

        return Value.makeSymbol(sym);
    }

    /// Intern a symbol (same name = same Value)
    /// Returns existing symbol if already interned, otherwise creates new one
    pub fn intern(self: *Heap, name: []const u8) ?Value {
        // Check for existing symbol
        if (self.symbols.get(name)) |existing| {
            return existing;
        }

        // Allocate new symbol
        const sym = self.allocSymbol(name) orelse return null;

        // Add to symbol table
        self.symbols.put(name, sym) catch return null;

        return sym;
    }

    /// Swap from-space and to-space
    pub fn swapSpaces(self: *Heap) void {
        const tmp = self.from_start;
        self.from_start = self.to_start;
        self.to_start = tmp;
        self.from_end = @ptrCast(@as([*]u8, self.from_start) + self.space_size);
        self.alloc_ptr = self.from_start;
    }

    /// Reset allocation pointer (used after GC)
    pub fn resetAllocPtr(self: *Heap, new_ptr: [*]align(ALIGNMENT) u8) void {
        self.alloc_ptr = new_ptr;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "heap init and deinit" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    try testing.expectEqual(@as(usize, 512 * 1024), heap.space_size);
    try testing.expectEqual(@as(usize, 0), heap.bytesUsed());
}

test "heap alloc cons" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const cons = heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2)) orelse return error.OutOfMemory;

    try testing.expect(cons.isCons());

    const ptr = cons.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), ptr.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), ptr.cdr.toFixnum());
}

test "heap alloc string" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = heap.allocString("hello") orelse return error.OutOfMemory;

    try testing.expect(str.isString());

    const ptr = str.toPtr(objects.String);
    try testing.expectEqualStrings("hello", ptr.bytes());
}

test "heap alloc vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = heap.allocVector(3, 8) orelse return error.OutOfMemory;

    try testing.expect(vec.isVector());

    const ptr = vec.toPtr(objects.Vector);
    try testing.expectEqual(@as(usize, 3), ptr.length);
    try testing.expectEqual(@as(usize, 8), ptr.capacity);

    // All elements should be nil
    for (ptr.items()) |item| {
        try testing.expect(item.isNil());
    }
}

test "heap space swap" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const original_from = heap.from_start;
    const original_to = heap.to_start;

    heap.swapSpaces();

    try testing.expectEqual(original_to, heap.from_start);
    try testing.expectEqual(original_from, heap.to_start);
}
