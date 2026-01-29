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

pub const UpperName = struct {
    slice: []const u8,
    owned: ?[]u8,
};

pub fn upperNameAlloc(allocator: std.mem.Allocator, name: []const u8, buf: []u8) !UpperName {
    if (name.len <= buf.len) {
        for (name, 0..) |c, i| {
            buf[i] = std.ascii.toUpper(c);
        }
        return .{ .slice = buf[0..name.len], .owned = null };
    }

    const out = try allocator.alloc(u8, name.len);
    for (name, 0..) |c, i| {
        out[i] = std.ascii.toUpper(c);
    }
    return .{ .slice = out, .owned = out };
}

pub fn freeUpperName(allocator: std.mem.Allocator, upper: UpperName) void {
    if (upper.owned) |mem| allocator.free(mem);
}

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
        const Stage = enum { none, name, pkg, ready };
        var stage: Stage = .none;
        var pkg: *Package = undefined;
        var name_copy: []u8 = undefined;

        defer switch (stage) {
            .none => {},
            .name => allocator.free(name_copy),
            .pkg => {
                allocator.destroy(pkg);
                allocator.free(name_copy);
            },
            .ready => pkg.deinit(),
        };

        name_copy = try allocator.dupe(u8, name);
        stage = .name;

        pkg = try allocator.create(Package);
        stage = .pkg;

        pkg.* = .{
            .name = name_copy,
            .symbols = SymbolTable.init(allocator),
            .use_list = std.ArrayList(*Package){},
            .exports = .{},
            .allocator = allocator,
            .auto_export = false,
        };
        stage = .ready;

        try pkg.symbols.put("T", Value.t);
        try pkg.symbols.put("NIL", Value.nil);

        stage = .none;
        return pkg;
    }

    pub fn deinit(self: *Package) void {
        self.symbols.deinit();
        self.use_list.deinit(self.allocator);
        // Free export keys
        var it = self.exports.keyIterator();
        while (it.next()) |key| {
            self.allocator.free(key.*);
        }
        self.exports.deinit(self.allocator);
        self.allocator.free(self.name);
        self.allocator.destroy(self);
    }

    fn findAccessibleUpper(self: *Package, upper_name: []const u8) ?Value {
        if (self.symbols.get(upper_name)) |existing| {
            return existing;
        }
        for (self.use_list.items) |used_pkg| {
            if (used_pkg.exports.contains(upper_name) or used_pkg.auto_export) {
                if (used_pkg.symbols.get(upper_name)) |sym| {
                    return sym;
                }
            }
        }
        return null;
    }

    pub fn findAccessible(self: *Package, name: []const u8) error{OutOfMemory}!?Value {
        var upper_buf: [256]u8 = undefined;
        const upper = try upperNameAlloc(self.allocator, name, upper_buf[0..]);
        defer freeUpperName(self.allocator, upper);
        return self.findAccessibleUpper(upper.slice);
    }

    pub fn intern(self: *Package, heap: *Heap, name: []const u8) error{OutOfMemory}!Value {
        // Upcase name per CL spec
        var upper_buf: [256]u8 = undefined;
        const upper = try upperNameAlloc(self.allocator, name, upper_buf[0..]);
        defer freeUpperName(self.allocator, upper);
        const upper_name = upper.slice;

        if (self.findAccessibleUpper(upper_name)) |existing| {
            return existing;
        }
        // Allocate new symbol in this package (already upcased in allocSymbol)
        const sym = try heap.allocSymbol(upper_name);
        // Store package pointer in symbol's reserved field
        const sym_ptr = sym.toPtr(Symbol);
        sym_ptr.reserved = @intFromPtr(self);
        try self.symbols.put(upper_name, sym);
        // Auto-export if flag is set (for CL package)
        if (self.auto_export) {
            const persistent_export_name = try self.allocator.dupe(u8, upper_name);
            try self.exports.put(self.allocator, persistent_export_name, {});
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
    /// Package nicknames (alias -> package)
    package_aliases: std.StringHashMapUnmanaged(*Package),
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
    /// Lisp-level class registry (hash table: name -> Class Value)
    lisp_classes: Value,
    /// Metaclass instances (chicken-egg bootstrap)
    standard_class: Value,
    built_in_class: Value,
    structure_class: Value,
    /// Class metadata for CLOS slot-value lookup
    /// Maps class symbol to slot symbol array
    class_metadata: std.AutoHashMapUnmanaged(Value, []const Value),
    /// Readtable for reader macros
    /// Maps character (u8) to macro function and flags
    readtable: std.AutoHashMapUnmanaged(u8, ReadtableEntry),
    /// Dispatch macro readtable for #X dispatch
    /// Maps dispatch char (u8) to sub-char table (HashMap(u8, Value))
    dispatch_readtable: std.AutoHashMapUnmanaged(u8, std.AutoHashMapUnmanaged(u8, Value)),
    /// Streams allocated in heap (for finalization)
    stream_list: std.ArrayList(*objects.Stream),

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
            .package_aliases = .{},
            .current_package = null,
            .cl_package = null,
            .cl_user_package = null,
            .gensym_counter = 0,
            .gentemp_counter = 0,
            .keyword_package = null,
            .lisp_packages = Value.nil,
            .lisp_classes = Value.nil,
            .standard_class = Value.nil,
            .built_in_class = Value.nil,
            .structure_class = Value.nil,
            .class_metadata = .{},
            .readtable = .{},
            .dispatch_readtable = .{},
            .stream_list = std.ArrayList(*objects.Stream){},
        };

        try heap.symbols.put("T", Value.t);
        try heap.symbols.put("NIL", Value.nil);

        // Create Lisp package registry
        heap.lisp_packages = try heap.allocHashTable(16, .eql);
        // Create Lisp class registry
        heap.lisp_classes = try heap.allocHashTable(128, .eql);

        // Create Lisp-visible Package objects for CL and CL-USER
        {
            // COMMON-LISP package
            const cl_name = try heap.allocBaseString("COMMON-LISP");
            const cl_nickname = try heap.allocBaseString("CL");
            const cl_nicknames = try heap.allocCons(cl_nickname, Value.nil);
            const cl_pkg = try heap.allocPackage(cl_name, cl_nicknames, Value.nil, true);
            try heap.putLispPackage(cl_name, cl_pkg);
            try heap.putLispPackage(cl_nickname, cl_pkg);

            // CL-USER package (uses CL)
            const cl_user_name = try heap.allocBaseString("CL-USER");
            const cl_user_uses = try heap.allocCons(cl_pkg, Value.nil);
            const cl_user_pkg = try heap.allocPackage(cl_user_name, Value.nil, cl_user_uses, false);
            try heap.putLispPackage(cl_user_name, cl_user_pkg);

            // KEYWORD package
            const kw_name = try heap.allocBaseString("KEYWORD");
            const kw_pkg = try heap.allocPackage(kw_name, Value.nil, Value.nil, true);
            try heap.putLispPackage(kw_name, kw_pkg);
        }

        // Create COMMON-LISP package first so metaclasses are interned in CL
        heap.cl_package = try Package.init(allocator, "COMMON-LISP");
        heap.cl_package.?.auto_export = true; // All CL symbols are exported
        const cl_key = try allocator.dupe(u8, "COMMON-LISP");
        errdefer allocator.free(cl_key);
        try heap.packages.put(allocator, cl_key, heap.cl_package.?);
        const cl_alias_key = try allocator.dupe(u8, "CL");
        errdefer allocator.free(cl_alias_key);
        try heap.package_aliases.put(allocator, cl_alias_key, heap.cl_package.?);
        // Create CL-USER package (uses CL)
        heap.cl_user_package = try Package.init(allocator, "CL-USER");
        try heap.cl_user_package.?.usePackage(heap.cl_package.?);
        const cl_user_key = try allocator.dupe(u8, "CL-USER");
        errdefer allocator.free(cl_user_key);
        try heap.packages.put(allocator, cl_user_key, heap.cl_user_package.?);

        // Start in CL package so primitives get interned there
        // VM will switch to CL-USER after primitive registration
        heap.current_package = heap.cl_package;

        // Create built-in classes for primitive types (must be after CL package exists)
        try heap.createBuiltInClasses();

        return heap;
    }

    /// Deinitialize heap
    pub fn deinit(self: *Heap) void {
        self.symbols.deinit();
        self.keywords.deinit();
        // Free all packages
        var pkg_iter = self.packages.iterator();
        while (pkg_iter.next()) |entry| {
            entry.value_ptr.*.deinit();
            self.backing_allocator.free(entry.key_ptr.*);
        }
        self.packages.deinit(self.backing_allocator);
        var alias_iter = self.package_aliases.iterator();
        while (alias_iter.next()) |entry| {
            self.backing_allocator.free(entry.key_ptr.*);
        }
        self.package_aliases.deinit(self.backing_allocator);
        // Free class_metadata slot arrays
        var class_iter = self.class_metadata.iterator();
        while (class_iter.next()) |entry| {
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
        for (self.stream_list.items) |stream| {
            stream.finalize();
        }
        self.stream_list.deinit(self.backing_allocator);
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

    fn trackStream(self: *Heap, stream: *objects.Stream) !void {
        try self.stream_list.append(self.backing_allocator, stream);
    }

    /// Allocate a cons cell
    pub fn allocCons(self: *Heap, car: Value, cdr: Value) error{OutOfMemory}!Value {
        const cons = try self.alloc(objects.Cons);
        cons.* = objects.Cons.init(car, cdr);
        return Value.makeCons(cons);
    }

    /// Build a list from a slice (preserves order)
    pub fn listFromSlice(self: *Heap, items: []const Value) error{OutOfMemory}!Value {
        var list = Value.nil;
        var i = items.len;
        while (i > 0) {
            i -= 1;
            list = try self.allocCons(items[i], list);
        }
        return list;
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
        try self.trackStream(stream);
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
        try self.trackStream(stream);
        return Value.makeStream(stream);
    }

    /// Allocate a string output stream
    pub fn allocStringOutputStream(self: *Heap) error{OutOfMemory}!Value {
        const buf = try self.backing_allocator.create(objects.OutputBuffer);
        buf.* = .{
            .list = std.ArrayList(u8){},
            .allocator = self.backing_allocator,
        };

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
        try self.trackStream(stream);
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
        try self.trackStream(stream);
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
        try self.trackStream(stream);
        return Value.makeStream(stream);
    }

    /// Allocate stdin stream
    pub fn allocStdin(self: *Heap) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .input,
            .stream_type = .stdin,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = 0, // stdin fd
        };
        return Value.makeStream(stream);
    }

    /// Allocate stdout stream
    pub fn allocStdout(self: *Heap) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .output,
            .stream_type = .stdout,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = 1, // stdout fd
        };
        return Value.makeStream(stream);
    }

    /// Allocate stderr stream
    pub fn allocStderr(self: *Heap) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .output,
            .stream_type = .stderr,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = 2, // stderr fd
        };
        return Value.makeStream(stream);
    }

    /// Allocate a broadcast stream (writes to multiple streams)
    /// streams_list is a list of output streams
    pub fn allocBroadcastStream(self: *Heap, streams_list: Value) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .output,
            .stream_type = .broadcast,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = -1,
            .source_value = streams_list, // list of streams
        };
        return Value.makeStream(stream);
    }

    /// Allocate a concatenated stream (reads from sequence of streams)
    /// streams_list is a list of input streams
    pub fn allocConcatenatedStream(self: *Heap, streams_list: Value) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .input,
            .stream_type = .concatenated,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = -1,
            .source_value = streams_list, // list of streams
        };
        return Value.makeStream(stream);
    }

    /// Allocate an echo stream (reads from input, echoes to output)
    /// components is a cons cell: (input-stream . output-stream)
    pub fn allocEchoStream(self: *Heap, input: Value, output: Value) error{OutOfMemory}!Value {
        const components = try self.allocCons(input, output);
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .input, // primarily an input stream
            .stream_type = .echo,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = -1,
            .source_value = components,
        };
        return Value.makeStream(stream);
    }

    /// Allocate a synonym stream (delegates to symbol's value)
    /// symbol_val is the symbol whose value is the target stream
    pub fn allocSynonymStream(self: *Heap, symbol_val: Value) error{OutOfMemory}!Value {
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .input, // direction determined dynamically
            .stream_type = .synonym,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = -1,
            .source_value = symbol_val, // the symbol
        };
        return Value.makeStream(stream);
    }

    /// Allocate a two-way stream (bidirectional: input + output)
    /// components is a cons cell: (input-stream . output-stream)
    pub fn allocTwoWayStream(self: *Heap, input: Value, output: Value) error{OutOfMemory}!Value {
        const components = try self.allocCons(input, output);
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .input, // can be used for both
            .stream_type = .two_way,
            .closed = false,
            .position = 0,
            .data_ptr = 0,
            .length = 0,
            .file_fd = -1,
            .source_value = components,
        };
        return Value.makeStream(stream);
    }

    /// Allocate a vector with given capacity
    pub fn allocVector(self: *Heap, length: usize, capacity: usize) error{OutOfMemory, Overflow}!Value {
        // Allocate header + data array together
        const data_size = try std.math.mul(usize, capacity, @sizeOf(Value));
        const total_size = try std.math.add(usize, @sizeOf(objects.Vector), data_size);

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
    pub fn allocArray(self: *Heap, dimensions: []const u64) error{OutOfMemory, Overflow}!Value {
        if (dimensions.len == 0 or dimensions.len > 8) return error.OutOfMemory;

        // Calculate total size (product of all dimensions)
        var total_size: u64 = 1;
        for (dimensions) |dim| {
            total_size = try std.math.mul(u64, total_size, dim);
        }

        // Allocate header + data array together
        const data_size = try std.math.mul(u64, total_size, @sizeOf(Value));
        const header_size = @sizeOf(objects.Array);
        const alloc_size = try std.math.add(u64, header_size, data_size);

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
    pub fn allocBaseString(self: *Heap, bytes: []const u8) error{OutOfMemory, Overflow}!Value {
        const aligned_len = std.mem.alignForward(usize, bytes.len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String), aligned_len);

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

    /// Allocate a String32 (UTF-32 string) from codepoints
    pub fn allocString32(self: *Heap, codepoints: []const u32) error{ OutOfMemory, Overflow }!Value {
        const byte_len = try std.math.mul(usize, codepoints.len, 4);
        const aligned_len = std.mem.alignForward(usize, byte_len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String32), aligned_len);

        const ptr = try self.allocRaw(total_size);
        const s32: *objects.String32 = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u32 = @ptrCast(@alignCast(ptr + @sizeOf(objects.String32)));

        // Copy codepoints
        @memcpy(data_ptr[0..codepoints.len], codepoints);

        s32.* = .{
            .length = @intCast(codepoints.len),
            .data = data_ptr,
        };

        return Value.makeString32(s32);
    }

    /// Allocate an uninitialized String32 of given codepoint length
    pub fn allocString32Uninitialized(self: *Heap, len: usize) error{ OutOfMemory, Overflow }!Value {
        const byte_len = try std.math.mul(usize, len, 4);
        const aligned_len = std.mem.alignForward(usize, byte_len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String32), aligned_len);

        const ptr = try self.allocRaw(total_size);
        const s32: *objects.String32 = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u32 = @ptrCast(@alignCast(ptr + @sizeOf(objects.String32)));

        s32.* = .{
            .length = @intCast(len),
            .data = data_ptr,
        };

        return Value.makeString32(s32);
    }

    /// Allocate a String32 from UTF-8 bytes, replacing invalid sequences with U+FFFD
    pub fn allocString32FromUtf8(self: *Heap, bytes: []const u8) error{ OutOfMemory, Overflow }!Value {
        // First pass: count codepoints
        var count: usize = 0;
        var i: usize = 0;
        while (i < bytes.len) {
            const cp_len = std.unicode.utf8ByteSequenceLength(bytes[i]) catch {
                count += 1; // Replacement character
                i += 1;
                continue;
            };
            if (i + cp_len > bytes.len) {
                count += 1; // Incomplete sequence, replacement character
                break;
            }
            _ = std.unicode.utf8Decode(bytes[i..][0..cp_len]) catch {
                count += 1; // Invalid sequence, replacement character
                i += 1;
                continue;
            };
            count += 1;
            i += cp_len;
        }

        // Allocate String32
        const byte_len = try std.math.mul(usize, count, 4);
        const aligned_len = std.mem.alignForward(usize, byte_len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String32), aligned_len);

        const ptr = try self.allocRaw(total_size);
        const s32: *objects.String32 = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u32 = @ptrCast(@alignCast(ptr + @sizeOf(objects.String32)));

        // Second pass: decode and store codepoints
        var out_idx: usize = 0;
        i = 0;
        while (i < bytes.len) {
            const cp_len = std.unicode.utf8ByteSequenceLength(bytes[i]) catch {
                data_ptr[out_idx] = 0xFFFD; // Replacement character
                out_idx += 1;
                i += 1;
                continue;
            };
            if (i + cp_len > bytes.len) {
                data_ptr[out_idx] = 0xFFFD; // Incomplete sequence
                out_idx += 1;
                break;
            }
            const cp = std.unicode.utf8Decode(bytes[i..][0..cp_len]) catch {
                data_ptr[out_idx] = 0xFFFD; // Invalid sequence
                out_idx += 1;
                i += 1;
                continue;
            };
            data_ptr[out_idx] = cp;
            out_idx += 1;
            i += cp_len;
        }

        s32.* = .{
            .length = @intCast(count),
            .data = data_ptr,
        };

        return Value.makeString32(s32);
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

    fn packageKey(self: *Heap, name: Value) error{OutOfMemory, TypeError}!Value {
        return switch (name.typeKind()) {
            .string => try self.internKeyword(name.toPtr(objects.String).bytes()),
            .symbol => try self.internKeyword(name.toPtr(objects.Symbol).getName()),
            .keyword => name,
            else => error.TypeError,
        };
    }

    /// Find a Lisp-level package by name
    pub fn findLispPackage(self: *Heap, name: Value) error{OutOfMemory, TypeError}!?Value {
        if (self.lisp_packages.raw == Value.nil.raw) return null;
        const ht = self.lisp_packages.toPtr(objects.HashTable);
        const key = try self.packageKey(name);
        return ht.get(self, key);
    }

    /// Register a Lisp package
    pub fn putLispPackage(self: *Heap, name: Value, pkg: Value) !void {
        if (self.lisp_packages.raw == Value.nil.raw) return error.RegistryNotInitialized;
        const ht = self.lisp_packages.toPtr(objects.HashTable);
        const key = try self.packageKey(name);
        try ht.put(key, pkg);
    }

    /// Remove a Lisp package by name string
    pub fn removeLispPackage(self: *Heap, name: Value) !bool {
        if (self.lisp_packages.raw == Value.nil.raw) return false;
        const ht = self.lisp_packages.toPtr(objects.HashTable);
        const key = try self.packageKey(name);
        return ht.remove(key);
    }

    /// Register a class in the global class registry
    pub fn putLispClass(self: *Heap, name: Value, class: Value) !void {
        if (self.lisp_classes.raw == Value.nil.raw) return error.RegistryNotInitialized;
        const ht = self.lisp_classes.toPtr(objects.HashTable);

        const hash = name.raw;
        var idx = hash % ht.capacity;
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const e = &ht.entries[idx];
            const is_empty = e.key.raw == objects.HashTable.EMPTY.raw;
            const is_deleted = e.key.raw == objects.HashTable.DELETED.raw;

            if (is_empty or is_deleted or e.key.raw == name.raw) {
                const was_new = is_empty or is_deleted;
                e.key = name;
                e.value = class;
                if (was_new) ht.count += 1;
                return;
            }

            idx = (idx + 1) % ht.capacity;
        }
        return error.HashTableFull;
    }

    /// Find a class by name in the global class registry
    pub fn findLispClass(self: *Heap, name: Value) ?Value {
        if (self.lisp_classes.raw == Value.nil.raw) return null;
        const ht = self.lisp_classes.toPtr(objects.HashTable);

        const hash = name.raw;
        var idx = hash % ht.capacity;
        var i: usize = 0;
        while (i < ht.capacity) : (i += 1) {
            const e = &ht.entries[idx];
            if (e.key.raw == objects.HashTable.EMPTY.raw) return null;
            if (e.key.raw != objects.HashTable.DELETED.raw and e.key.raw == name.raw) {
                return e.value;
            }
            idx = (idx + 1) % ht.capacity;
        }
        return null;
    }

    /// Intern a symbol (same name = same Value)
    /// Returns existing symbol if already interned, otherwise creates new one
    /// Uses current package if available, otherwise legacy global table
    pub fn intern(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        // Use current package if available
        if (self.current_package) |pkg| {
            return pkg.intern(self, name);
        }

        // Upcase name per CL spec
        var upper_buf: [256]u8 = undefined;
        const upper = try upperNameAlloc(self.backing_allocator, name, upper_buf[0..]);
        defer freeUpperName(self.backing_allocator, upper);
        const upper_name = upper.slice;

        // Fallback to legacy global table
        if (self.symbols.get(upper_name)) |existing| {
            return existing;
        }

        const sym = try self.allocSymbol(upper_name);
        try self.symbols.put(upper_name, sym);
        return sym;
    }

    /// Intern a symbol in a specific package by name
    pub fn internInPackage(self: *Heap, pkg_name: []const u8, sym_name: []const u8) !?Value {
        const pkg = if (self.findPackage(pkg_name)) |val| val else return null;
        return try pkg.intern(self, sym_name);
    }

    /// Find a package by name
    pub fn findPackage(self: *Heap, name: []const u8) ?*Package {
        if (self.packages.get(name)) |pkg| {
            return pkg;
        }
        return self.package_aliases.get(name);
    }

    /// Create or find a package
    pub fn findOrCreatePackage(self: *Heap, name: []const u8) error{OutOfMemory}!*Package {
        if (self.packages.get(name)) |existing| return existing;
        if (self.package_aliases.get(name)) |alias| return alias;
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
        // Upcase name per CL spec
        var upper_buf: [256]u8 = undefined;
        const upper = try upperNameAlloc(self.backing_allocator, name, upper_buf[0..]);
        defer freeUpperName(self.backing_allocator, upper);
        const upper_name = upper.slice;

        // Check for existing keyword with upcased name
        if (self.keywords.get(upper_name)) |existing| {
            return existing;
        }

        // Allocate new keyword (already upcased in allocKeyword)
        const kw = try self.allocKeyword(upper_name);

        // Add to keyword table with upcased name
        try self.keywords.put(upper_name, kw);

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
        try all_roots.appendSlice(self.backing_allocator, external_roots);

        // Add symbol table values (the Values need to be updated after GC)
        var sym_it = self.symbols.map.valueIterator();
        while (sym_it.next()) |v| {
            try all_roots.append(self.backing_allocator, v.*);
        }

        // Add keyword table values
        var kw_it = self.keywords.map.valueIterator();
        while (kw_it.next()) |v| {
            try all_roots.append(self.backing_allocator, v.*);
        }

        // Add package symbol table values
        var pkg_it = self.packages.valueIterator();
        while (pkg_it.next()) |pkg| {
            var pkg_sym_it = pkg.*.symbols.map.valueIterator();
            while (pkg_sym_it.next()) |v| {
                try all_roots.append(self.backing_allocator, v.*);
            }
        }

        // Add readtable function values
        var rt_it = self.readtable.valueIterator();
        while (rt_it.next()) |entry| {
            try all_roots.append(self.backing_allocator, entry.function);
        }

        // Add dispatch readtable function values
        var drt_it = self.dispatch_readtable.valueIterator();
        while (drt_it.next()) |sub_table| {
            var sub_it = sub_table.valueIterator();
            while (sub_it.next()) |fn_val| {
                try all_roots.append(self.backing_allocator, fn_val.*);
            }
        }

        // Add Lisp package registry
        if (self.lisp_packages.raw != Value.nil.raw) {
            try all_roots.append(self.backing_allocator, self.lisp_packages);
        }
        if (self.lisp_classes.raw != Value.nil.raw) {
            try all_roots.append(self.backing_allocator, self.lisp_classes);
        }

        // Add metaclass roots
        if (self.standard_class.raw != Value.nil.raw) {
            try all_roots.append(self.backing_allocator, self.standard_class);
        }
        if (self.built_in_class.raw != Value.nil.raw) {
            try all_roots.append(self.backing_allocator, self.built_in_class);
        }
        if (self.structure_class.raw != Value.nil.raw) {
            try all_roots.append(self.backing_allocator, self.structure_class);
        }

        // Run GC
        var gc = GC.init(self.backing_allocator, self);
        defer gc.deinit();
        _ = try gc.collect(all_roots.items);

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
        var idx = ext_count + sym_count + kw_count + pkg_sym_count + rt_count + drt_count;
        if (self.lisp_packages.raw != Value.nil.raw) {
            self.lisp_packages = all_roots.items[idx];
            idx += 1;
        }
        if (self.lisp_classes.raw != Value.nil.raw) {
            self.lisp_classes = all_roots.items[idx];
            idx += 1;
        }

        // Update metaclass roots
        if (self.standard_class.raw != Value.nil.raw) {
            self.standard_class = all_roots.items[idx];
            idx += 1;
        }
        if (self.built_in_class.raw != Value.nil.raw) {
            self.built_in_class = all_roots.items[idx];
            idx += 1;
        }
        if (self.structure_class.raw != Value.nil.raw) {
            self.structure_class = all_roots.items[idx];
            idx += 1;
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

    /// Bootstrap metaclass: chicken-egg solution where class-of(Class) = Class itself
    pub fn allocMetaclass(self: *Heap, name: []const u8, metaclass: Value) !Value {
        const name_sym = try self.intern(name);

        const class_ptr = try self.alloc(objects.Class);
        class_ptr.* = .{
            .kind = .class,
            .name = name_sym,
            .direct_supers = Value.nil,
            .cpl = Value.nil,
            .direct_slots = Value.nil,
            .slots = Value.nil,
            .num_shared = 0,
            .shared_slots = undefined,
            .metaclass = metaclass,
        };
        const class_val = Value.makeClass(class_ptr);

        // Metacircular: CPL = (standard-class) for standard-class
        const cpl_cons = try self.allocCons(class_val, Value.nil);
        class_ptr.cpl = cpl_cons;

        return class_val;
    }

    /// Create built-in classes for primitive types (fixnum, cons, symbol, string, vector, etc.)
    fn createBuiltInClasses(self: *Heap) !void {
        // Bootstrap metaclasses first (chicken-egg: class-of(standard-class) = standard-class)
        // Step 1: Create standard-class with nil metaclass temporarily
        const std_class_name = try self.intern("standard-class");
        const std_class_ptr = try self.alloc(objects.Class);
        std_class_ptr.* = .{
            .kind = .class,
            .name = std_class_name,
            .direct_supers = Value.nil,
            .cpl = Value.nil,
            .direct_slots = Value.nil,
            .slots = Value.nil,
            .metaclass = Value.nil,
            .num_shared = 0,
            .shared_slots = undefined,
        };
        self.standard_class = Value.makeClass(std_class_ptr);

        // Step 2: Set standard-class's metaclass to itself
        std_class_ptr.metaclass = self.standard_class;

        // Step 3: Set CPL = (standard-class)
        const std_cpl = try self.allocCons(self.standard_class, Value.nil);
        std_class_ptr.cpl = std_cpl;

        // Step 4: Create other metaclasses with standard-class as metaclass
        self.built_in_class = try self.allocMetaclass("built-in-class", self.standard_class);
        self.structure_class = try self.allocMetaclass("structure-class", self.standard_class);

        // Register metaclasses in class registry
        try self.putLispClass(std_class_name, self.standard_class);
        const bic_name = self.built_in_class.toPtr(objects.Class).name;
        try self.putLispClass(bic_name, self.built_in_class);
        const struct_class_name = self.structure_class.toPtr(objects.Class).name;
        try self.putLispClass(struct_class_name, self.structure_class);

        // Now create built-in classes for primitive types
        const type_names = [_][]const u8{
            "fixnum",
            "float",
            "character",
            "cons",
            "symbol",
            "string",
            "vector",
            "keyword",
            "closure",
            "hash-table",
            "rational",
            "complex",
            "stream",
            "bignum",
            "array",
            "pathname",
            "package",
            "chunk",
            "condition",
            "class",
            "generic-function",
            "method",
            "slot-definition",
        };

        for (type_names) |name| {
            const name_sym = try self.intern(name);
            const class_ptr = try self.alloc(objects.Class);
            class_ptr.* = .{
                .kind = .class,
                .name = name_sym,
                .direct_supers = Value.nil,
                .cpl = Value.nil,
                .direct_slots = Value.nil,
                .slots = Value.nil,
                .metaclass = self.built_in_class,
                .num_shared = 0,
                .shared_slots = undefined,
            };
            const class_val = Value.makeClass(class_ptr);

            // CPL = (type built-in-class t)
            const t_sym = Value.t;
            const bic_cpl_tail = try self.allocCons(self.built_in_class, try self.allocCons(t_sym, Value.nil));
            const cpl = try self.allocCons(class_val, bic_cpl_tail);
            class_ptr.cpl = cpl;

            // Register in class registry
            try self.putLispClass(name_sym, class_val);
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

fn fillLowercase(buf: []u8) void {
    for (buf) |*b| {
        b.* = 'a';
    }
}

test "heap init and deinit" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    try testing.expectEqual(@as(usize, 512 * 1024), heap.space_size);
    // lisp_packages hash table is allocated during init
    try testing.expect(heap.bytesUsed() > 0);
}

test "heap deinit finalizes output streams" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    _ = try heap.allocStringOutputStream();
    heap.deinit();
}

test "heap findPackage handles CL alias" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const pkg = heap.findPackage("CL");
    try testing.expect(pkg != null);
    try testing.expect(pkg.? == heap.cl_package.?);
}

test "heap intern handles t and nil in packages" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const t_sym = try heap.intern("t");
    const nil_sym = try heap.intern("nil");
    try testing.expectEqual(@as(u64, Value.t.raw), t_sym.raw);
    try testing.expectEqual(@as(u64, Value.nil.raw), nil_sym.raw);

    const pkg = try heap.findOrCreatePackage("FOO");
    const t_pkg = try pkg.intern(&heap, "t");
    const nil_pkg = try pkg.intern(&heap, "nil");
    try testing.expectEqual(@as(u64, Value.t.raw), t_pkg.raw);
    try testing.expectEqual(@as(u64, Value.nil.raw), nil_pkg.raw);
}

test "heap intern fallback uses symbol table" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    heap.current_package = null;
    defer heap.deinit();

    const sym1 = try heap.intern("foo");
    const sym2 = try heap.intern("FOO");
    try testing.expectEqual(@as(u64, sym1.raw), sym2.raw);
}

test "heap intern uppercases long names" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var name_buf: [300]u8 = undefined;
    fillLowercase(name_buf[0..]);

    const sym = try heap.intern(name_buf[0..]);
    const sym_name = sym.toPtr(objects.Symbol).getName();
    try testing.expectEqual(@as(usize, name_buf.len), sym_name.len);
    for (sym_name) |c| {
        try testing.expect(c == 'A');
    }
}

test "heap internKeyword uppercases long names" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var name_buf: [300]u8 = undefined;
    fillLowercase(name_buf[0..]);

    const kw = try heap.internKeyword(name_buf[0..]);
    const kw_name = kw.toPtr(objects.Keyword).getName();
    try testing.expectEqual(@as(usize, name_buf.len), kw_name.len);
    for (kw_name) |c| {
        try testing.expect(c == 'A');
    }
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

    const str = try heap.allocBaseString("hello");

    try testing.expect(str.isString());

    const ptr = str.toPtr(objects.String);
    try testing.expectEqualStrings("hello", ptr.bytes());
}

test "heap alloc string oom" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const big = try testing.allocator.alloc(u8, heap.space_size);
    defer testing.allocator.free(big);

    try testing.expectError(error.OutOfMemory, heap.allocBaseString(big));
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
