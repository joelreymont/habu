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
const Symbol = objects.Symbol;
const GC = @import("gc.zig").GC;

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
        errdefer self.allocator.free(key);
        try self.map.put(self.allocator, key, sym);
    }

    pub fn iterator(self: *const SymbolTable) std.StringHashMapUnmanaged(Value).Iterator {
        return self.map.iterator();
    }
};

/// Package: a namespace for symbols
pub const Package = struct {
    name: []const u8,
    symbols: SymbolTable,
    /// Packages whose exported symbols are accessible
    use_list: std.ArrayList(*Package),
    /// Symbols exported from this package
    exports: std.StringHashMapUnmanaged(void),
    allocator: std.mem.Allocator,
    /// If true, all interned symbols are automatically exported (for CL package)
    auto_export: bool,

    pub fn init(allocator: std.mem.Allocator, name: []const u8) !*Package {
        const pkg = try allocator.create(Package);
        pkg.* = .{
            .name = try allocator.dupe(u8, name),
            .symbols = SymbolTable.init(allocator),
            .use_list = std.ArrayList(*Package){},
            .exports = .{},
            .allocator = allocator,
            .auto_export = false,
        };
        return pkg;
    }

    pub fn deinit(self: *Package) void {
        self.symbols.deinit();
        self.use_list.deinit(self.allocator);
        self.exports.deinit(self.allocator);
        self.allocator.free(self.name);
        self.allocator.destroy(self);
    }

    pub fn intern(self: *Package, heap: *Heap, name: []const u8) error{OutOfMemory}!Value {
        // Check own symbols first
        if (self.symbols.get(name)) |existing| {
            return existing;
        }
        // Check used packages for exported symbols
        for (self.use_list.items) |used_pkg| {
            if (used_pkg.exports.contains(name) or used_pkg.auto_export) {
                if (used_pkg.symbols.get(name)) |sym| {
                    return sym;
                }
            }
        }
        // Allocate new symbol in this package
        const sym = try heap.allocSymbol(name);
        // Store package pointer in symbol's reserved field
        const sym_ptr = sym.toPtr(Symbol);
        sym_ptr.reserved = @intFromPtr(self);
        try self.symbols.put(name, sym);
        // Auto-export if flag is set (for CL package)
        if (self.auto_export) {
            try self.exports.put(self.allocator, name, {});
        }
        return sym;
    }

    pub fn exportSymbol(self: *Package, name: []const u8) !void {
        try self.exports.put(self.allocator, name, {});
    }

    pub fn usePackage(self: *Package, other: *Package) !void {
        try self.use_list.append(self.allocator, other);
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
    /// Interned symbol table (legacy, for backward compat - use packages)
    symbols: SymbolTable,
    /// Interned keyword table
    keywords: SymbolTable,
    /// Package registry (Zig packages for symbol interning)
    packages: std.StringHashMapUnmanaged(*Package),
    /// Current package for symbol interning
    current_package: ?*Package,
    /// The COMMON-LISP package (primitives)
    cl_package: ?*Package,
    /// The CL-USER package (default user package)
    cl_user_package: ?*Package,
    /// Gensym counter for unique symbol generation
    gensym_counter: u64,
    /// Gentemp counter for temporary symbol generation
    gentemp_counter: u64,
    /// The KEYWORD package
    keyword_package: ?*Package,
    /// Lisp-level package registry (hash table: name -> Package Value)
    lisp_packages: Value,
    /// Class metadata for CLOS slot-value lookup
    /// Maps class name to slot names array
    class_metadata: std.StringHashMapUnmanaged([]const []const u8),
    /// Readtable for reader macros
    /// Maps character (u8) to macro function and flags
    readtable: std.AutoHashMapUnmanaged(u8, ReadtableEntry),
    /// Dispatch macro readtable for #X dispatch
    /// Maps dispatch char (u8) to sub-char table (HashMap(u8, Value))
    dispatch_readtable: std.AutoHashMapUnmanaged(u8, std.AutoHashMapUnmanaged(u8, Value)),

    pub const ReadtableEntry = struct {
        function: Value,
        non_terminating: bool,
    };

    pub const SlotMeta = struct {
        name: []const u8,
        initform: ?Value, // Default value expression (evaluated lazily)
    };

    pub const ClassMeta = struct {
        slots: []const SlotMeta,
    };

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

        var heap = Heap{
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
            .keywords = SymbolTable.init(allocator),
            .packages = .{},
            .current_package = null,
            .cl_package = null,
            .cl_user_package = null,
            .gensym_counter = 0,
            .gentemp_counter = 0,
            .keyword_package = null,
            .lisp_packages = Value.nil,
            .class_metadata = .{},
            .readtable = .{},
            .dispatch_readtable = .{},
        };

        // Create Lisp package registry
        heap.lisp_packages = try heap.allocHashTable(16, .eql);

        // Create COMMON-LISP package (holds primitives, all symbols exported)
        heap.cl_package = Package.init(allocator, "COMMON-LISP") catch return error.OutOfMemory;
        heap.cl_package.?.auto_export = true; // All CL symbols are exported
        const cl_key = try allocator.dupe(u8, "COMMON-LISP");
        errdefer allocator.free(cl_key);
        heap.packages.put(allocator, cl_key, heap.cl_package.?) catch return error.OutOfMemory;
        // Also register as "CL" alias (shared package pointer, separate key)
        const cl_alias_key = try allocator.dupe(u8, "CL");
        errdefer allocator.free(cl_alias_key);
        heap.packages.put(allocator, cl_alias_key, heap.cl_package.?) catch return error.OutOfMemory;

        // Create CL-USER package (uses CL)
        heap.cl_user_package = Package.init(allocator, "CL-USER") catch return error.OutOfMemory;
        heap.cl_user_package.?.usePackage(heap.cl_package.?) catch return error.OutOfMemory;
        const cl_user_key = try allocator.dupe(u8, "CL-USER");
        errdefer allocator.free(cl_user_key);
        heap.packages.put(allocator, cl_user_key, heap.cl_user_package.?) catch return error.OutOfMemory;

        // Start in CL package so primitives get interned there
        // VM will switch to CL-USER after primitive registration
        heap.current_package = heap.cl_package;

        return heap;
    }

    /// Deinitialize heap
    pub fn deinit(self: *Heap) void {
        self.symbols.deinit();
        self.keywords.deinit();
        // Free all packages (dedup since CL is alias for COMMON-LISP)
        var seen = std.AutoHashMap(*Package, void).init(self.backing_allocator);
        defer seen.deinit();
        var pkg_iter = self.packages.iterator();
        while (pkg_iter.next()) |entry| {
            if (!seen.contains(entry.value_ptr.*)) {
                seen.put(entry.value_ptr.*, {}) catch {};
                entry.value_ptr.*.deinit();
            }
            self.backing_allocator.free(entry.key_ptr.*);
        }
        self.packages.deinit(self.backing_allocator);
        // Free class_metadata keys and slot name arrays
        var class_iter = self.class_metadata.iterator();
        while (class_iter.next()) |entry| {
            self.backing_allocator.free(entry.key_ptr.*);
            for (entry.value_ptr.*) |slot_name| {
                self.backing_allocator.free(slot_name);
            }
            self.backing_allocator.free(entry.value_ptr.*);
        }
        self.class_metadata.deinit(self.backing_allocator);
        self.readtable.deinit(self.backing_allocator);
        // Free dispatch_readtable nested hashmaps
        var disp_iter = self.dispatch_readtable.valueIterator();
        while (disp_iter.next()) |sub_table| {
            sub_table.deinit(self.backing_allocator);
        }
        self.dispatch_readtable.deinit(self.backing_allocator);
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
    pub fn allocRaw(self: *Heap, size: usize) error{OutOfMemory}![*]align(ALIGNMENT) u8 {
        const aligned_size = std.mem.alignForward(usize, size, ALIGNMENT);

        const current = @intFromPtr(self.alloc_ptr);
        const end = @intFromPtr(self.from_end);

        if (current + aligned_size > end) {
            return error.OutOfMemory;
        }

        const result = self.alloc_ptr;
        self.alloc_ptr = @ptrFromInt(current + aligned_size);

        self.stats.allocations += 1;
        self.stats.bytes_allocated += aligned_size;

        return result;
    }

    /// Allocate an object of a specific type
    pub fn alloc(self: *Heap, comptime T: type) error{OutOfMemory}!*T {
        const ptr = try self.allocRaw(@sizeOf(T));
        return @ptrCast(@alignCast(ptr));
    }

    /// Allocate a cons cell
    pub fn allocCons(self: *Heap, car: Value, cdr: Value) error{OutOfMemory}!Value {
        const cons = try self.alloc(objects.Cons);
        cons.* = objects.Cons.init(car, cdr);
        return Value.makeCons(cons);
    }

    /// Allocate a rational number
    pub fn allocRational(self: *Heap, num: i64, den: i64) error{OutOfMemory}!Value {
        const rat = try self.alloc(objects.Rational);
        rat.* = objects.Rational.make(num, den);
        return Value.makeRational(rat);
    }

    /// Allocate a complex number
    pub fn allocComplex(self: *Heap, real: f64, imag: f64) error{OutOfMemory}!Value {
        const cplx = try self.alloc(objects.Complex);
        cplx.* = objects.Complex.make(real, imag);
        return Value.makeComplex(cplx);
    }

    /// Allocate a bignum from an i64
    pub fn allocBignum(self: *Heap, n: i64) error{OutOfMemory}!Value {
        const bn = try self.alloc(objects.Bignum);
        bn.* = objects.Bignum.make(n);
        return Value.makeBignum(bn);
    }

    /// Allocate a stream
    pub fn allocStream(self: *Heap, direction: objects.StreamDirection, stream_type: objects.StreamType, file_fd: i32) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = objects.Stream.make(direction, stream_type, file_fd);
        return Value.makeStream(stream);
    }

    /// Allocate a bignum from limbs array
    pub fn allocBignumFromLimbs(self: *Heap, limbs: []const u64, negative: bool) error{OutOfMemory}!Value {
        const bn = try self.alloc(objects.Bignum);
        bn.* = .{
            .kind = .bignum,
            .size = 0,
            .limbs = [_]u64{0} ** 8,
        };

        // Determine actual number of significant limbs (trim leading zeros)
        var used_limbs: usize = limbs.len;
        while (used_limbs > 0 and limbs[used_limbs - 1] == 0) {
            used_limbs -= 1;
        }

        // Copy limbs (max 8 limbs)
        const copy_count = @min(used_limbs, 8);
        for (0..copy_count) |i| {
            bn.limbs[i] = limbs[i];
        }

        // Set size (negative if negative flag is set)
        bn.size = if (used_limbs == 0) 0 else if (negative) -@as(i64, @intCast(used_limbs)) else @as(i64, @intCast(used_limbs));

        return Value.makeBignum(bn);
    }

    /// Allocate a string input stream
    pub fn allocStringInputStream(self: *Heap, str: Value) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        const str_obj = str.toPtr(objects.String);
        stream.* = .{
            .kind = .stream,
            .direction = .input,
            .stream_type = .string,
            .closed = false,
            .position = 0,
            .data_ptr = @intFromPtr(str_obj.data),
            .length = str_obj.length,
            .file_fd = -1,
            .source_value = str,
        };
        return Value.makeStream(stream);
    }

    /// Allocate a string output stream
    pub fn allocStringOutputStream(self: *Heap) error{OutOfMemory}!Value {
        const buf = try self.backing_allocator.create(std.ArrayList(u8));
        buf.* = std.ArrayList(u8){};

        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .output,
            .stream_type = .string,
            .closed = false,
            .position = 0,
            .data_ptr = @intFromPtr(buf),
            .length = 0,
            .file_fd = -1,
        };
        return Value.makeStream(stream);
    }

    /// Allocate a file input stream
    pub fn allocFileInputStream(self: *Heap, fd: i32) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .input,
            .stream_type = .file,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = fd,
        };
        return Value.makeStream(stream);
    }

    /// Allocate a file output stream
    pub fn allocFileOutputStream(self: *Heap, fd: i32) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .output,
            .stream_type = .file,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = fd,
        };
        return Value.makeStream(stream);
    }

    /// Allocate a vector with given capacity
    pub fn allocVector(self: *Heap, length: usize, capacity: usize) error{OutOfMemory}!Value {
        // Allocate header + data array together
        const data_size = std.math.mul(usize, capacity, @sizeOf(Value)) catch return error.OutOfMemory;
        const total_size = std.math.add(usize, @sizeOf(objects.Vector), data_size) catch return error.OutOfMemory;

        const ptr = try self.allocRaw(total_size);
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
            .fill_pointer = 0xFFFFFFFFFFFFFFFF, // no fill-pointer by default
        };

        return Value.makeVector(vec);
    }

    /// Allocate a multi-dimensional array
    pub fn allocArray(self: *Heap, dimensions: []const u64) error{OutOfMemory}!Value {
        if (dimensions.len == 0 or dimensions.len > 8) return error.OutOfMemory;

        // Calculate total size (product of all dimensions)
        var total_size: u64 = 1;
        for (dimensions) |dim| {
            total_size = std.math.mul(u64, total_size, dim) catch return error.OutOfMemory;
        }

        // Allocate header + data array together
        const data_size = std.math.mul(u64, total_size, @sizeOf(Value)) catch return error.OutOfMemory;
        const header_size = @sizeOf(objects.Array);
        const alloc_size = std.math.add(u64, header_size, data_size) catch return error.OutOfMemory;

        const ptr = try self.allocRaw(alloc_size);
        const arr: *objects.Array = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]Value = @ptrCast(@alignCast(ptr + header_size));

        // Initialize data to nil
        for (0..total_size) |i| {
            data_ptr[i] = Value.nil;
        }

        // Initialize array header
        arr.* = .{
            .rank = @intCast(dimensions.len),
            .dimensions = [_]u64{0} ** 8,
            .total_size = total_size,
            .data_ptr = @intFromPtr(data_ptr),
        };

        // Copy dimensions
        for (dimensions, 0..) |dim, i| {
            arr.dimensions[i] = dim;
        }

        return Value.makeArray(arr);
    }

    /// Allocate a string (copies the bytes)
    pub fn allocString(self: *Heap, bytes: []const u8) error{OutOfMemory}!Value {
        const aligned_len = std.mem.alignForward(usize, bytes.len, 8);
        const total_size = std.math.add(usize, @sizeOf(objects.String), aligned_len) catch return error.OutOfMemory;

        const ptr = try self.allocRaw(total_size);
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

    /// Allocate an uninitialized string of given length
    pub fn allocStringUninitialized(self: *Heap, len: usize) error{ OutOfMemory, Overflow }!Value {
        const aligned_len = std.mem.alignForward(usize, len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String), aligned_len);

        const ptr = try self.allocRaw(total_size);
        const str: *objects.String = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

        str.* = .{
            .length = len,
            .data = data_ptr,
        };

        return Value.makeString(str);
    }

    /// Allocate a closure
    pub fn allocClosure(self: *Heap, code: Value, arity: u32, captures: []const Value) error{ OutOfMemory, Overflow }!Value {
        const captures_size = try std.math.mul(usize, captures.len, @sizeOf(Value));
        const total_size = try std.math.add(usize, @sizeOf(objects.Closure), captures_size);

        const ptr = try self.allocRaw(total_size);
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

    pub fn allocCondition(self: *Heap, type_sym: Value, format_control: Value, format_args: Value) !Value {
        const cond = try self.alloc(objects.Condition);
        cond.* = .{
            .kind = .condition,
            .type_sym = type_sym,
            .format_control = format_control,
            .format_args = format_args,
        };
        return Value.makeCondition(cond);
    }

    /// Round up to next power of two (for hash table capacity)
    fn nextPowerOfTwo(n: usize) usize {
        if (n == 0) return 1;
        var v = n - 1;
        v |= v >> 1;
        v |= v >> 2;
        v |= v >> 4;
        v |= v >> 8;
        v |= v >> 16;
        v |= v >> 32;
        return v + 1;
    }

    /// Allocate a hash table with given initial capacity (will be rounded to power of 2)
    pub fn allocHashTable(self: *Heap, capacity: usize, test_type: objects.HashTest) error{OutOfMemory}!Value {
        // Ensure power-of-two capacity for correct linear probing with mask
        const actual_capacity = nextPowerOfTwo(if (capacity < 8) 8 else capacity);
        const total_size = @sizeOf(objects.HashTable) + actual_capacity * @sizeOf(objects.HashEntry);

        const ptr = try self.allocRaw(total_size);
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
            .test_type = test_type,
        };

        return Value.makeHashTable(ht);
    }

    /// Allocate a bytecode chunk with constant pool and code
    pub fn allocChunk(
        self: *Heap,
        code: []const u8,
        constants: []const Value,
        arity: u8,
        opt_count: u8,
        key_count: u8,
        has_rest: bool,
        num_locals: u8,
    ) !Value {
        const const_size = constants.len * @sizeOf(Value);
        const code_size = std.mem.alignForward(usize, code.len, 8);
        const total = @sizeOf(objects.Chunk) + const_size + code_size;

        const ptr = try self.allocRaw(total);
        const chunk: *objects.Chunk = @ptrCast(@alignCast(ptr));

        const const_ptr: [*]Value = @ptrCast(@alignCast(ptr + @sizeOf(objects.Chunk)));
        const code_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Chunk) + const_size);

        @memcpy(code_ptr[0..code.len], code);
        @memcpy(const_ptr[0..constants.len], constants);

        chunk.* = .{
            .const_count = @intCast(constants.len),
            .code_len = @intCast(code.len),
            .arity = arity,
            .opt_count = opt_count,
            .key_count = key_count,
            .has_rest = if (has_rest) 1 else 0,
            .num_locals = num_locals,
            .const_pool = const_ptr,
            .code = code_ptr,
        };

        return Value.makeChunk(chunk);
    }

    /// Allocate a symbol from a string
    pub fn allocSymbol(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        const aligned_name_len = std.mem.alignForward(usize, name.len, 8);
        const total_size = @sizeOf(objects.Symbol) + aligned_name_len;

        const ptr = try self.allocRaw(total_size);
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

    /// Allocate a Package object on the heap
    pub fn allocPackage(self: *Heap, name: Value, nicknames: Value, use_list: Value, auto_export: bool) !Value {
        const pkg = try self.alloc(objects.Package);
        pkg.* = .{
            .name = name,
            .nicknames = nicknames,
            .use_list = use_list,
            .exports = Value.nil,
            .symbols = Value.nil,
            .shadowing = Value.nil,
        };
        _ = auto_export;
        return Value.makePackage(pkg);
    }

    /// Allocate a Pathname object on the heap
    pub fn allocPathname(
        self: *Heap,
        host: Value,
        device: Value,
        directory: Value,
        name: Value,
        ty: Value,
        version: Value,
    ) !Value {
        const pn = try self.alloc(objects.Pathname);
        pn.* = .{
            .host = host,
            .device = device,
            .directory = directory,
            .name = name,
            .type = ty,
            .version = version,
        };
        return Value.makePathname(pn);
    }

    /// Find a Lisp-level package by name
    pub fn findLispPackage(self: *Heap, name: Value) ?Value {
        if (!name.isString() and !name.isSymbol()) return null;
        if (self.lisp_packages.raw == Value.nil.raw) return null;
        const ht = self.lisp_packages.toPtr(objects.HashTable);

        const name_str = if (name.isString())
            name.toPtr(objects.String).bytes()
        else if (name.isSymbol())
            name.toPtr(objects.Symbol).getName()
        else
            return null;

        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const e = &ht.entries[i];
            if (e.key.raw == objects.HashTable.EMPTY.raw or e.key.raw == objects.HashTable.DELETED.raw) continue;

            const key_str = if (e.key.isString())
                e.key.toPtr(objects.String).bytes()
            else if (e.key.isSymbol())
                e.key.toPtr(objects.Symbol).getName()
            else
                continue;

            if (std.mem.eql(u8, key_str, name_str)) return e.value;
        }
        return null;
    }

    /// Register a Lisp package
    pub fn putLispPackage(self: *Heap, name: Value, pkg: Value) !void {
        if (self.lisp_packages.raw == Value.nil.raw) return error.RegistryNotInitialized;
        const ht = self.lisp_packages.toPtr(objects.HashTable);

        const name_str = if (name.isString())
            name.toPtr(objects.String).bytes()
        else if (name.isSymbol())
            name.toPtr(objects.Symbol).getName()
        else
            return error.TypeError;

        const hash = name.raw;
        var idx = hash % ht.capacity;
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const e = &ht.entries[idx];
            const is_empty = e.key.raw == objects.HashTable.EMPTY.raw;
            const is_deleted = e.key.raw == objects.HashTable.DELETED.raw;

            if (is_empty or is_deleted) {
                e.key = name;
                e.value = pkg;
                ht.count += 1;
                return;
            }

            // Check for existing key with same string content
            const key_str = if (e.key.isString())
                e.key.toPtr(objects.String).bytes()
            else if (e.key.isSymbol())
                e.key.toPtr(objects.Symbol).getName()
            else {
                idx = (idx + 1) % ht.capacity;
                continue;
            };

            if (std.mem.eql(u8, key_str, name_str)) {
                e.key = name;
                e.value = pkg;
                return;
            }

            idx = (idx + 1) % ht.capacity;
        }
        return error.HashTableFull;
    }

    /// Remove a Lisp package by name string
    pub fn removeLispPackage(self: *Heap, name: Value) !bool {
        if (self.lisp_packages.raw == Value.nil.raw) return false;
        const ht = self.lisp_packages.toPtr(objects.HashTable);

        const name_str = if (name.isString())
            name.toPtr(objects.String).bytes()
        else if (name.isSymbol())
            name.toPtr(objects.Symbol).getName()
        else
            return error.TypeError;

        const hash = name.raw;
        var idx = hash % ht.capacity;
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const e = &ht.entries[idx];
            if (e.key.raw == objects.HashTable.EMPTY.raw) return false;
            if (e.key.raw != objects.HashTable.DELETED.raw) {
                const key_str = if (e.key.isString())
                    e.key.toPtr(objects.String).bytes()
                else if (e.key.isSymbol())
                    e.key.toPtr(objects.Symbol).getName()
                else {
                    idx = (idx + 1) % ht.capacity;
                    continue;
                };

                if (std.mem.eql(u8, key_str, name_str)) {
                    e.key = objects.HashTable.DELETED;
                    e.value = Value.nil;
                    ht.count -= 1;
                    return true;
                }
            }
            idx = (idx + 1) % ht.capacity;
        }
        return false;
    }

    /// Intern a symbol (same name = same Value)
    /// Returns existing symbol if already interned, otherwise creates new one
    /// Uses current package if available, otherwise legacy global table
    pub fn intern(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        // Use current package if available
        if (self.current_package) |pkg| {
            return pkg.intern(self, name);
        }

        // Fallback to legacy global table
        if (self.symbols.get(name)) |existing| {
            return existing;
        }

        const sym = try self.allocSymbol(name);
        try self.symbols.put(name, sym);
        return sym;
    }

    /// Intern a symbol in a specific package by name
    pub fn internInPackage(self: *Heap, pkg_name: []const u8, sym_name: []const u8) !?Value {
        const pkg = self.findPackage(pkg_name) orelse return null;
        return try pkg.intern(self, sym_name);
    }

    /// Find a package by name
    pub fn findPackage(self: *Heap, name: []const u8) ?*Package {
        return self.packages.get(name);
    }

    /// Create or find a package
    pub fn findOrCreatePackage(self: *Heap, name: []const u8) error{OutOfMemory}!*Package {
        if (self.packages.get(name)) |existing| {
            return existing;
        }
        const pkg = try Package.init(self.backing_allocator, name);
        errdefer pkg.deinit();
        const key = try self.backing_allocator.dupe(u8, name);
        errdefer self.backing_allocator.free(key);
        try self.packages.put(self.backing_allocator, key, pkg);
        return pkg;
    }

    /// Set current package
    pub fn setCurrentPackage(self: *Heap, pkg: *Package) void {
        self.current_package = pkg;
    }

    /// Get current package name
    pub fn getCurrentPackageName(self: *const Heap) []const u8 {
        if (self.current_package) |pkg| {
            return pkg.name;
        }
        return "CL-USER";
    }

    /// Allocate a keyword in the heap
    /// FNV-1a hash for bytes
    fn fnvHash(bytes: []const u8) u64 {
        var hash: u64 = 0xcbf29ce484222325; // FNV offset basis
        for (bytes) |b| {
            hash ^= b;
            hash *%= 0x100000001b3; // FNV prime
        }
        return hash;
    }
    pub fn allocKeyword(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        const aligned_name_len = std.mem.alignForward(usize, name.len, 8);
        const total_size = @sizeOf(objects.Keyword) + aligned_name_len;

        const ptr = try self.allocRaw(total_size);
        const kw: *objects.Keyword = @ptrCast(@alignCast(ptr));
        const name_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Keyword));

        @memcpy(name_ptr[0..name.len], name);

        kw.* = .{
            .name_len = name.len,
            .name_ptr = name_ptr,
            .hash = fnvHash(name),
        };

        return Value.makeKeyword(kw);
    }

    /// Intern a keyword (same name = same Value)
    /// Returns existing keyword if already interned, otherwise creates new one
    pub fn internKeyword(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        // Check for existing keyword
        if (self.keywords.get(name)) |existing| {
            return existing;
        }

        // Allocate new keyword
        const kw = try self.allocKeyword(name);

        // Add to keyword table
        try self.keywords.put(name, kw);

        return kw;
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

    /// Run garbage collection with external roots (from VM stack, globals, etc.)
    /// Returns bytes reclaimed (space_used_before - space_used_after)
    pub fn collectGarbage(self: *Heap, external_roots: []Value) !usize {
        const before = self.bytesUsed();

        // Build root set: external roots + interned symbol/keyword values
        var all_roots = std.ArrayList(Value){};
        defer all_roots.deinit(self.backing_allocator);

        // Add external roots
        all_roots.appendSlice(self.backing_allocator, external_roots) catch return error.OutOfMemory;

        // Add symbol table values (the Values need to be updated after GC)
        var sym_it = self.symbols.map.valueIterator();
        while (sym_it.next()) |v| {
            all_roots.append(self.backing_allocator, v.*) catch return error.OutOfMemory;
        }

        // Add keyword table values
        var kw_it = self.keywords.map.valueIterator();
        while (kw_it.next()) |v| {
            all_roots.append(self.backing_allocator, v.*) catch return error.OutOfMemory;
        }

        // Add package symbol table values
        var pkg_it = self.packages.valueIterator();
        while (pkg_it.next()) |pkg| {
            var pkg_sym_it = pkg.*.symbols.map.valueIterator();
            while (pkg_sym_it.next()) |v| {
                all_roots.append(self.backing_allocator, v.*) catch return error.OutOfMemory;
            }
        }

        // Add readtable function values
        var rt_it = self.readtable.valueIterator();
        while (rt_it.next()) |entry| {
            all_roots.append(self.backing_allocator, entry.function) catch return error.OutOfMemory;
        }

        // Add dispatch readtable function values
        var drt_it = self.dispatch_readtable.valueIterator();
        while (drt_it.next()) |sub_table| {
            var sub_it = sub_table.valueIterator();
            while (sub_it.next()) |fn_val| {
                all_roots.append(self.backing_allocator, fn_val.*) catch return error.OutOfMemory;
            }
        }

        // Add Lisp package registry
        if (self.lisp_packages.raw != Value.nil.raw) {
            all_roots.append(self.backing_allocator, self.lisp_packages) catch return error.OutOfMemory;
        }

        // Run GC
        var gc = GC.init(self.backing_allocator, self);
        defer gc.deinit();
        _ = gc.collect(all_roots.items) catch return error.OutOfMemory;

        // Update symbol table with new locations
        const sym_count = self.symbols.map.count();
        const kw_count = self.keywords.map.count();
        const ext_count = external_roots.len;

        // Count package symbols
        var pkg_sym_count: usize = 0;
        var pkg_count_it = self.packages.valueIterator();
        while (pkg_count_it.next()) |pkg| {
            pkg_sym_count += pkg.*.symbols.map.count();
        }

        // Count readtable functions
        const rt_count = self.readtable.count();
        var drt_count: usize = 0;
        var drt_count_it = self.dispatch_readtable.valueIterator();
        while (drt_count_it.next()) |sub_table| {
            drt_count += sub_table.count();
        }

        // External roots are updated in-place by GC.collect
        // Copy external roots back (they were passed by value to ArrayList)
        for (external_roots, 0..) |_, i| {
            external_roots[i] = all_roots.items[i];
        }

        // Update symbol table values
        var sym_idx: usize = 0;
        var sym_update_it = self.symbols.map.valueIterator();
        while (sym_update_it.next()) |v| {
            v.* = all_roots.items[ext_count + sym_idx];
            sym_idx += 1;
        }

        // Update keyword table values
        var kw_idx: usize = 0;
        var kw_update_it = self.keywords.map.valueIterator();
        while (kw_update_it.next()) |v| {
            v.* = all_roots.items[ext_count + sym_count + kw_idx];
            kw_idx += 1;
        }

        // Update package symbol table values
        var pkg_sym_idx: usize = 0;
        var pkg_update_it = self.packages.valueIterator();
        while (pkg_update_it.next()) |pkg| {
            var pkg_sym_update_it = pkg.*.symbols.map.valueIterator();
            while (pkg_sym_update_it.next()) |v| {
                v.* = all_roots.items[ext_count + sym_count + kw_count + pkg_sym_idx];
                pkg_sym_idx += 1;
            }
        }
        std.debug.assert(pkg_sym_idx == pkg_sym_count);

        // Update readtable function values
        var rt_idx: usize = 0;
        var rt_update_it = self.readtable.valueIterator();
        while (rt_update_it.next()) |entry| {
            entry.function = all_roots.items[ext_count + sym_count + kw_count + pkg_sym_count + rt_idx];
            rt_idx += 1;
        }
        std.debug.assert(rt_idx == rt_count);

        // Update dispatch readtable function values
        var drt_idx: usize = 0;
        var drt_update_it = self.dispatch_readtable.valueIterator();
        while (drt_update_it.next()) |sub_table| {
            var sub_update_it = sub_table.valueIterator();
            while (sub_update_it.next()) |fn_val| {
                fn_val.* = all_roots.items[ext_count + sym_count + kw_count + pkg_sym_count + rt_count + drt_idx];
                drt_idx += 1;
            }
        }
        std.debug.assert(drt_idx == drt_count);

        // Update Lisp package registry
        if (self.lisp_packages.raw != Value.nil.raw) {
            self.lisp_packages = all_roots.items[ext_count + sym_count + kw_count + pkg_sym_count + rt_count + drt_count];
        }

        const after = self.bytesUsed();
        return if (before > after) before - after else 0;
    }

    /// Try to allocate, running GC if needed
    /// external_roots should contain VM stack, globals, etc.
    pub fn allocWithGC(self: *Heap, comptime T: type, external_roots: []Value) ?*T {
        // Try allocation first
        if (self.alloc(T)) |ptr| {
            return ptr;
        }

        // Run GC and retry
        _ = self.collectGarbage(external_roots);

        // Try again
        return self.alloc(T);
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
    // lisp_packages hash table is allocated during init
    try testing.expect(heap.bytesUsed() > 0);
}

test "heap alloc cons" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const cons = try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2));

    try testing.expect(cons.isCons());

    const ptr = cons.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), ptr.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), ptr.cdr.toFixnum());
}

test "heap alloc string" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocString("hello");

    try testing.expect(str.isString());

    const ptr = str.toPtr(objects.String);
    try testing.expectEqualStrings("hello", ptr.bytes());
}

test "heap alloc vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try heap.allocVector(3, 8);

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
