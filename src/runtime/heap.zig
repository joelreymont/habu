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
const roots_mod = @import("roots.zig");

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

    pub fn remove(self: *SymbolTable, name: []const u8) bool {
        if (self.map.fetchRemove(name)) |removed| {
            self.allocator.free(removed.key);
            return true;
        }
        return false;
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

    pub fn findAccessibleUpper(self: *Package, upper_name: []const u8) ?Value {
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
        var upper_buf: [256]u8 = undefined;
        const upper = try upperNameAlloc(self.allocator, name, upper_buf[0..]);
        defer freeUpperName(self.allocator, upper);
        if (self.exports.contains(upper.slice)) return;

        const key = try self.allocator.dupe(u8, upper.slice);
        errdefer self.allocator.free(key);
        try self.exports.put(self.allocator, key, {});
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
    /// Reusable buffer for building GC root slot lists.
    gc_slots: std.ArrayList(*Value),
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
    /// Unique id counter for uninterned symbols (stored in Symbol.reserved with low-bit tag)
    sym_uid_counter: u64,
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
    /// Warning handler for warn primitive (optional)
    warn_handler: ?WarnHandler,
    warn_ctx: ?*anyopaque,
    /// Cached condition symbols/keywords for primitives (avoid per-call interning)
    sym_simple_warning: Value,
    kw_format_control: Value,
    kw_format_arguments: Value,

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

    pub const WarnHandler = *const fn (Value, ?*anyopaque) anyerror!void;

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
            .gc_slots = std.ArrayList(*Value){},
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
            .sym_uid_counter = 1,
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
            .warn_handler = null,
            .warn_ctx = null,
            .sym_simple_warning = Value.nil,
            .kw_format_control = Value.nil,
            .kw_format_arguments = Value.nil,
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
            const cl_user_alias = try heap.allocBaseString("COMMON-LISP-USER");
            const cl_user_uses = try heap.allocCons(cl_pkg, Value.nil);
            const cl_user_pkg = try heap.allocPackage(cl_user_name, Value.nil, cl_user_uses, false);
            try heap.putLispPackage(cl_user_name, cl_user_pkg);
            try heap.putLispPackage(cl_user_alias, cl_user_pkg);

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
        const cl_user_alias_key = try allocator.dupe(u8, "COMMON-LISP-USER");
        errdefer allocator.free(cl_user_alias_key);
        try heap.package_aliases.put(allocator, cl_user_alias_key, heap.cl_user_package.?);

        // Start in CL package so primitives get interned there
        // VM will switch to CL-USER after primitive registration
        heap.current_package = heap.cl_package;

        // Create built-in classes for primitive types (must be after CL package exists)
        try heap.createBuiltInClasses();

        // Cache condition symbols/keywords while current_package is CL so CL-USER resolves them.
        heap.sym_simple_warning = try heap.intern("simple-warning");
        heap.kw_format_control = try heap.internKeyword("format-control");
        heap.kw_format_arguments = try heap.internKeyword("format-arguments");

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
            // Free string output stream buffers before finalize
            if (stream.stream_type == .string and stream.direction == .output and stream.data_ptr != 0 and stream.position > 0) {
                const buf: [*]u8 = @ptrFromInt(stream.data_ptr);
                std.heap.page_allocator.free(buf[0..stream.position]);
                stream.data_ptr = 0;
                stream.position = 0;
                stream.length = 0;
            }
            stream.finalize();
        }
        self.stream_list.deinit(self.backing_allocator);
        self.gc_slots.deinit(self.backing_allocator);
        self.backing_allocator.free(self.memory);
    }

    /// Get current allocation position
    pub fn getAllocPtr(self: *const Heap) usize {
        return @intFromPtr(self.alloc_ptr);
    }

    /// Get address of alloc_ptr field (for JIT inline cons).
    pub fn getAllocPtrAddr(self: *Heap) u64 {
        return @intFromPtr(&self.alloc_ptr);
    }

    /// Get address of from_end field (for JIT inline cons).
    pub fn getFromEndAddr(self: *Heap) u64 {
        return @intFromPtr(&self.from_end);
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

        const raw_current = @intFromPtr(self.alloc_ptr);
        const end = @intFromPtr(self.from_end);
        if (raw_current >= end) return error.OutOfMemory;
        if (raw_current > std.math.maxInt(usize) - (ALIGNMENT - 1)) return error.OutOfMemory;

        const current = std.mem.alignForward(usize, raw_current, ALIGNMENT);
        if (current < raw_current) return error.OutOfMemory;
        if (current > std.math.maxInt(usize) - aligned_size) {
            return error.OutOfMemory;
        }
        const next = current + aligned_size;

        if (next > end) {
            return error.OutOfMemory;
        }

        const result: [*]align(ALIGNMENT) u8 = @ptrFromInt(current);
        self.alloc_ptr = @ptrFromInt(next);

        self.stats.allocations +%= 1;
        self.stats.bytes_allocated +%= aligned_size;

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

    pub fn setWarnHandler(self: *Heap, handler: ?WarnHandler, ctx: ?*anyopaque) void {
        self.warn_handler = handler;
        self.warn_ctx = ctx;
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

        // Size must match stored limb count; Bignum currently stores up to 8 limbs.
        bn.size = if (copy_count == 0) 0 else if (negative) -@as(i64, @intCast(copy_count)) else @as(i64, @intCast(copy_count));

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
        const stream = try self.alloc(objects.Stream);
        stream.* = .{
            .kind = .stream,
            .direction = .output,
            .stream_type = .string,
            .closed = false,
            ._padding = 0,
            .position = 0, // capacity
            .data_ptr = 0, // raw buffer, allocated on first write
            .length = 0, // bytes written
            .file_fd = -1,
            .pushback_char = 0xFF,
            ._padding2 = [_]u8{0} ** 3,
            .source_value = Value.nil,
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

    const DecStatus = enum { ok, invalid, incomplete };
    const DecRes = struct {
        status: DecStatus,
        len: usize,
        cp: u32,
    };

    fn decUtf8(bytes: []const u8, i: usize) DecRes {
        const b0 = bytes[i];
        if (b0 < 0x80) {
            return .{ .status = .ok, .len = 1, .cp = b0 };
        }
        if (b0 < 0xC2) {
            return .{ .status = .invalid, .len = 1, .cp = 0 };
        }
        if (b0 < 0xE0) {
            if (i + 1 >= bytes.len) return .{ .status = .incomplete, .len = 1, .cp = 0 };
            const b1 = bytes[i + 1];
            if ((b1 & 0xC0) != 0x80) return .{ .status = .invalid, .len = 1, .cp = 0 };
            const cp = (@as(u32, b0 & 0x1F) << 6) | @as(u32, b1 & 0x3F);
            return .{ .status = .ok, .len = 2, .cp = cp };
        }
        if (b0 < 0xF0) {
            if (i + 2 >= bytes.len) return .{ .status = .incomplete, .len = 1, .cp = 0 };
            const b1 = bytes[i + 1];
            const b2 = bytes[i + 2];
            if ((b1 & 0xC0) != 0x80 or (b2 & 0xC0) != 0x80) return .{ .status = .invalid, .len = 1, .cp = 0 };
            if (b0 == 0xE0 and b1 < 0xA0) return .{ .status = .invalid, .len = 1, .cp = 0 };
            if (b0 == 0xED and b1 >= 0xA0) return .{ .status = .invalid, .len = 1, .cp = 0 };
            const cp = (@as(u32, b0 & 0x0F) << 12) | (@as(u32, b1 & 0x3F) << 6) | @as(u32, b2 & 0x3F);
            return .{ .status = .ok, .len = 3, .cp = cp };
        }
        if (b0 < 0xF5) {
            if (i + 3 >= bytes.len) return .{ .status = .incomplete, .len = 1, .cp = 0 };
            const b1 = bytes[i + 1];
            const b2 = bytes[i + 2];
            const b3 = bytes[i + 3];
            if ((b1 & 0xC0) != 0x80 or (b2 & 0xC0) != 0x80 or (b3 & 0xC0) != 0x80) {
                return .{ .status = .invalid, .len = 1, .cp = 0 };
            }
            if (b0 == 0xF0 and b1 < 0x90) return .{ .status = .invalid, .len = 1, .cp = 0 };
            if (b0 == 0xF4 and b1 >= 0x90) return .{ .status = .invalid, .len = 1, .cp = 0 };
            const cp = (@as(u32, b0 & 0x07) << 18) |
                (@as(u32, b1 & 0x3F) << 12) |
                (@as(u32, b2 & 0x3F) << 6) |
                @as(u32, b3 & 0x3F);
            return .{ .status = .ok, .len = 4, .cp = cp };
        }

        return .{ .status = .invalid, .len = 1, .cp = 0 };
    }

    /// Allocate a String32 from UTF-8 bytes, replacing invalid sequences with U+FFFD
    pub fn allocString32FromUtf8(self: *Heap, bytes: []const u8) error{ OutOfMemory, Overflow }!Value {
        // First pass: count codepoints
        var count: usize = 0;
        var i: usize = 0;
        while (i < bytes.len) {
            const dec = decUtf8(bytes, i);
            switch (dec.status) {
                .ok => {
                    count += 1;
                    i += dec.len;
                },
                .invalid => {
                    count += 1;
                    i += 1;
                },
                .incomplete => {
                    count += 1;
                    break;
                },
            }
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
            const dec = decUtf8(bytes, i);
            switch (dec.status) {
                .ok => {
                    data_ptr[out_idx] = dec.cp;
                    out_idx += 1;
                    i += dec.len;
                },
                .invalid => {
                    data_ptr[out_idx] = 0xFFFD;
                    out_idx += 1;
                    i += 1;
                },
                .incomplete => {
                    data_ptr[out_idx] = 0xFFFD;
                    out_idx += 1;
                    break;
                },
            }
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
    pub fn allocHashTable(self: *Heap, capacity: usize, test_type: objects.HashTest) error{ OutOfMemory, Overflow }!Value {
        // Ensure power-of-two capacity for correct linear probing with mask.
        const actual_capacity = nextPowerOfTwo(if (capacity < 8) 8 else capacity);

        const entries_len = try std.math.mul(usize, actual_capacity, 2);
        const entries_vec_val = try self.allocVector(entries_len, entries_len);
        const entries_vec = entries_vec_val.toPtr(objects.Vector);

        // Keys are in even slots. Values stay nil.
        for (0..actual_capacity) |i| {
            entries_vec.data[i * 2] = objects.HashTable.EMPTY;
        }

        const ht = try self.alloc(objects.HashTable);
        ht.* = .{
            .count = 0,
            .capacity = actual_capacity,
            .entries_vec = entries_vec_val,
            .test_type = test_type,
        };

        return Value.makeHashTable(ht);
    }

    pub fn growHashTableInPlace(
        self: *Heap,
        ht: *objects.HashTable,
        new_capacity: usize,
    ) error{ OutOfMemory, Overflow, HashTableNeedsGrowth, HashTableFull }!void {
        const old_cap: usize = @intCast(ht.capacity);
        const dbl = try std.math.mul(usize, old_cap, 2);
        const target = if (new_capacity < dbl) dbl else new_capacity;

        const actual_capacity = nextPowerOfTwo(if (target < 8) 8 else target);
        if (actual_capacity <= old_cap) return;

        const entries_len = try std.math.mul(usize, actual_capacity, 2);
        const entries_vec_val = try self.allocVector(entries_len, entries_len);
        const entries_vec = entries_vec_val.toPtr(objects.Vector);

        // Keys are in even slots. Values stay nil.
        for (0..actual_capacity) |i| {
            entries_vec.data[i * 2] = objects.HashTable.EMPTY;
        }

        const old_entries_vec = ht.entries_vec.toPtr(objects.Vector);

        var tmp: objects.HashTable = .{
            .count = 0,
            .capacity = actual_capacity,
            .entries_vec = entries_vec_val,
            .test_type = ht.test_type,
        };

        for (0..old_cap) |i| {
            const k = old_entries_vec.data[i * 2];
            if (objects.HashTable.isAvailableKey(k)) continue;
            const v = old_entries_vec.data[i * 2 + 1];
            try tmp.put(k, v);
        }

        ht.entries_vec = entries_vec_val;
        ht.capacity = actual_capacity;
        ht.count = tmp.count;
    }

    fn putHashTableAutoGrow(
        self: *Heap,
        ht: *objects.HashTable,
        key: Value,
        value: Value,
    ) error{ OutOfMemory, Overflow, HashTableNeedsGrowth, HashTableFull }!void {
        while (true) {
            ht.put(key, value) catch |err| switch (err) {
                error.HashTableNeedsGrowth, error.HashTableFull => {
                    const cap: usize = @intCast(ht.capacity);
                    const grown = try std.math.mul(usize, cap, 2);
                    try self.growHashTableInPlace(ht, grown);
                    continue;
                },
                else => return err,
            };
            return;
        }
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
            .allow_other_keys = 0,
            .num_locals = num_locals,
            .const_pool = const_ptr,
            .code = code_ptr,
        };

        return Value.makeChunk(chunk);
    }

    /// Allocate a chunk with optimize settings
    pub fn allocChunkWithOpt(
        self: *Heap,
        code: []const u8,
        constants: []const Value,
        arity: u8,
        opt_count: u8,
        key_count: u8,
        has_rest: bool,
        num_locals: u8,
        speed: u8,
        safety: u8,
    ) !Value {
        const val = try self.allocChunk(code, constants, arity, opt_count, key_count, has_rest, num_locals);
        const chunk = val.toPtr(objects.Chunk);
        chunk.speed = speed;
        chunk.safety = safety;
        return val;
    }

    pub fn allocMacroEnv(self: *Heap) !Value {
        const macros = try self.allocHashTable(16, .eq);
        const symbol_macros = try self.allocHashTable(16, .eq);
        const env = try self.alloc(objects.MacroEnv);
        env.* = .{
            .macros = macros,
            .symbol_macros = symbol_macros,
        };
        return Value.makeMacroEnv(env);
    }

    pub fn allocNativeCode(self: *Heap, entry: usize) error{OutOfMemory}!Value {
        const nc = try self.alloc(objects.NativeCode);
        nc.* = .{ .entry = entry };
        return Value.makeNativeCode(nc);
    }

    /// Allocate a symbol from a string
    pub fn allocSymbol(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        const aligned_name_len = std.mem.alignForward(usize, name.len, 8);
        const total_size = @sizeOf(objects.Symbol) + aligned_name_len;

        const ptr = try self.allocRaw(total_size);
        const sym: *objects.Symbol = @ptrCast(@alignCast(ptr));
        const name_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Symbol));

        @memcpy(name_ptr[0..name.len], name);

        // Uninterned symbols get a stable id in reserved (low-bit tagged).
        // Interned symbols overwrite reserved with a *Package pointer later.
        const uid = self.sym_uid_counter;
        if (uid == 0 or uid > (std.math.maxInt(u64) >> 1)) return error.OutOfMemory;
        self.sym_uid_counter = uid + 1;
        const reserved_uid = (uid << 1) | 1;

        sym.* = .{
            .name_len = name.len,
            .name_ptr = name_ptr,
            .plist = Value.nil,
            .reserved = reserved_uid,
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

    pub fn packageKey(self: *Heap, name: Value) error{OutOfMemory, TypeError}!Value {
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
        return ht.get(key);
    }

    /// Register a Lisp package
    pub fn putLispPackage(self: *Heap, name: Value, pkg: Value) !void {
        if (self.lisp_packages.raw == Value.nil.raw) return error.RegistryNotInitialized;
        const ht = self.lisp_packages.toPtr(objects.HashTable);
        const key = try self.packageKey(name);
        try self.putHashTableAutoGrow(ht, key, pkg);
    }

    /// Remove a Lisp package by name string
    pub fn removeLispPackage(self: *Heap, name: Value) !bool {
        if (self.lisp_packages.raw == Value.nil.raw) return false;
        const ht = self.lisp_packages.toPtr(objects.HashTable);
        const key = try self.packageKey(name);
        return ht.remove(key);
    }

    /// Remove a Lisp package by precomputed key
    pub fn removeLispPackageKey(self: *Heap, key: Value) bool {
        if (self.lisp_packages.raw == Value.nil.raw) return false;
        const ht = self.lisp_packages.toPtr(objects.HashTable);
        return ht.remove(key);
    }

    /// Register a class in the global class registry
    pub fn putLispClass(self: *Heap, name: Value, class: Value) !void {
        if (self.lisp_classes.raw == Value.nil.raw) return error.RegistryNotInitialized;
        const ht = self.lisp_classes.toPtr(objects.HashTable);
        try self.putHashTableAutoGrow(ht, name, class);
    }

    /// Remove a class from the global class registry
    pub fn removeLispClass(self: *Heap, name: Value) bool {
        if (self.lisp_classes.raw == Value.nil.raw) return false;
        const ht = self.lisp_classes.toPtr(objects.HashTable);
        return ht.remove(name);
    }

    /// Find a class by name in the global class registry
    pub fn findLispClass(self: *Heap, name: Value) ?Value {
        if (self.lisp_classes.raw == Value.nil.raw) return null;
        const ht = self.lisp_classes.toPtr(objects.HashTable);
        return ht.get(name);
    }

    /// Intern a symbol (same name = same Value)
    /// Returns existing symbol if already interned, otherwise creates new one
    /// Uses current package if available, otherwise legacy global table
    pub fn intern(self: *Heap, name: []const u8) error{OutOfMemory}!Value {
        // Use current package if available
        if (self.resolveCurrentPackageForIntern()) |pkg| {
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

    fn hasNativePackagePtr(self: *const Heap, pkg: *Package) bool {
        if (self.cl_package != null and self.cl_package.? == pkg) return true;
        if (self.cl_user_package != null and self.cl_user_package.? == pkg) return true;
        if (self.keyword_package != null and self.keyword_package.? == pkg) return true;

        var pkg_it = self.packages.valueIterator();
        while (pkg_it.next()) |entry| {
            if (entry.* == pkg) return true;
        }

        var alias_it = self.package_aliases.valueIterator();
        while (alias_it.next()) |entry| {
            if (entry.* == pkg) return true;
        }

        return false;
    }

    fn resolveCurrentPackageForIntern(self: *Heap) ?*Package {
        const pkg = self.current_package orelse return null;
        if (self.hasNativePackagePtr(pkg)) return pkg;

        std.log.err("stale current package pointer 0x{x}; resetting package context", .{@intFromPtr(pkg)});
        if (self.cl_user_package) |user_pkg| {
            self.current_package = user_pkg;
            return user_pkg;
        }
        if (self.cl_package) |cl_pkg| {
            self.current_package = cl_pkg;
            return cl_pkg;
        }
        self.current_package = null;
        return null;
    }

    /// Intern a symbol in a specific package by name
    pub fn internInPackage(self: *Heap, pkg_name: []const u8, sym_name: []const u8) !?Value {
        const pkg = if (self.findPackage(pkg_name)) |val| val else return null;
        return try pkg.intern(self, sym_name);
    }

    /// Create a new shadow symbol in a package, preventing inheritance of the
    /// same-named symbol from used packages.  If the symbol already exists
    /// locally it is returned unchanged.
    pub fn shadowInPackage(self: *Heap, pkg: *Package, name: []const u8) !Value {
        var upper_buf: [256]u8 = undefined;
        const upper = try upperNameAlloc(self.backing_allocator, name, upper_buf[0..]);
        defer freeUpperName(self.backing_allocator, upper);
        const upper_name = upper.slice;

        // If already present locally, nothing to do
        if (pkg.symbols.get(upper_name)) |existing| return existing;

        // Create a new symbol owned by this package
        const sym = try self.allocSymbol(upper_name);
        sym.toPtr(objects.Symbol).reserved = @intFromPtr(pkg);
        try pkg.symbols.put(upper_name, sym);
        return sym;
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
            if (self.hasNativePackagePtr(pkg)) return pkg.name;
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
        var ranges = [_]roots_mod.RootRange{.{ .ptr = external_roots.ptr, .len = external_roots.len }};
        return try self.collectGarbageRootSet(.{
            .ranges = ranges[0..],
            .slots = &[_]*Value{},
        });
    }

    /// Run garbage collection with a precise external root set (ranges + slots).
    /// Internal heap roots (symbols, packages, readtables, etc.) are always included.
    pub fn collectGarbageRootSet(self: *Heap, external_roots: roots_mod.RootSet) !usize {
        const before = self.bytesUsed();

        // Internal roots are tracked by slot address; external roots are passed as a range.
        self.gc_slots.clearRetainingCapacity();
        try self.gc_slots.appendSlice(self.backing_allocator, external_roots.slots);

        // Add symbol table values.
        var sym_it = self.symbols.map.valueIterator();
        while (sym_it.next()) |v| {
            try self.gc_slots.append(self.backing_allocator, v);
        }

        // Add keyword table values.
        var kw_it = self.keywords.map.valueIterator();
        while (kw_it.next()) |v| {
            try self.gc_slots.append(self.backing_allocator, v);
        }

        // Add package symbol table values.
        var pkg_it = self.packages.valueIterator();
        while (pkg_it.next()) |pkg| {
            var pkg_sym_it = pkg.*.symbols.map.valueIterator();
            while (pkg_sym_it.next()) |v| {
                try self.gc_slots.append(self.backing_allocator, v);
            }
        }

        // Add readtable function values.
        var rt_it = self.readtable.valueIterator();
        while (rt_it.next()) |entry| {
            try self.gc_slots.append(self.backing_allocator, &entry.function);
        }

        // Add dispatch readtable function values.
        var drt_it = self.dispatch_readtable.valueIterator();
        while (drt_it.next()) |sub_table| {
            var sub_it = sub_table.valueIterator();
            while (sub_it.next()) |fn_val| {
                try self.gc_slots.append(self.backing_allocator, fn_val);
            }
        }

        // Lisp package registry.
        if (self.lisp_packages.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.lisp_packages);
        }
        if (self.lisp_classes.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.lisp_classes);
        }

        // Metaclass roots.
        if (self.standard_class.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.standard_class);
        }
        if (self.built_in_class.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.built_in_class);
        }
        if (self.structure_class.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.structure_class);
        }

        // Cached condition symbols/keywords.
        if (self.sym_simple_warning.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.sym_simple_warning);
        }
        if (self.kw_format_control.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.kw_format_control);
        }
        if (self.kw_format_arguments.raw != Value.nil.raw) {
            try self.gc_slots.append(self.backing_allocator, &self.kw_format_arguments);
        }

        // Run GC
        var gc = GC.init(self.backing_allocator, self);
        defer gc.deinit();
        _ = try gc.collectRootSet(.{
            .ranges = external_roots.ranges,
            .slots = self.gc_slots.items,
        });

        const after = self.bytesUsed();
        return if (before > after) before - after else 0;
    }

    /// Try to allocate, running GC if needed
    /// external_roots should contain VM stack, globals, etc.
    pub fn allocWithGC(self: *Heap, comptime T: type, external_roots: []Value) error{OutOfMemory}!*T {
        // Try allocation first
        if (self.alloc(T)) |ptr| return ptr else |_| {}

        // Run GC and retry
        _ = try self.collectGarbage(external_roots);

        // Try again (propagate OOM if still full)
        return try self.alloc(T);
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

        var vector_class: Value = Value.nil;
        var generic_function_class: Value = Value.nil;
        var method_class: Value = Value.nil;
        var slot_definition_class: Value = Value.nil;

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

            if (std.mem.eql(u8, name, "vector")) vector_class = class_val;
            if (std.mem.eql(u8, name, "generic-function")) generic_function_class = class_val;
            if (std.mem.eql(u8, name, "method")) method_class = class_val;
            if (std.mem.eql(u8, name, "slot-definition")) slot_definition_class = class_val;
        }

        // ANSI CLOS names that are expected to exist in FIND-CLASS and DEFCLASS superlists.
        // Keep aliases to runtime classes where dedicated runtime objects are not distinct yet.
        const std_object_name = try self.intern("standard-object");
        const std_object_ptr = try self.alloc(objects.Class);
        const std_object = Value.makeClass(std_object_ptr);
        const std_object_supers = if (vector_class.raw != Value.nil.raw)
            try self.allocCons(vector_class, Value.nil)
        else
            Value.nil;
        const std_object_cpl = if (vector_class.raw != Value.nil.raw)
            try self.allocCons(std_object, vector_class.toPtr(objects.Class).cpl)
        else
            try self.allocCons(std_object, try self.allocCons(Value.t, Value.nil));
        std_object_ptr.* = .{
            .kind = .class,
            .name = std_object_name,
            .direct_supers = std_object_supers,
            .cpl = std_object_cpl,
            .direct_slots = Value.nil,
            .slots = Value.nil,
            .metaclass = self.standard_class,
            .num_shared = 0,
            .shared_slots = undefined,
        };
        try self.putLispClass(std_object_name, std_object);

        if (generic_function_class.raw != Value.nil.raw) {
            const std_gf_name = try self.intern("standard-generic-function");
            try self.putLispClass(std_gf_name, generic_function_class);
        }
        if (method_class.raw != Value.nil.raw) {
            const std_method_name = try self.intern("standard-method");
            try self.putLispClass(std_method_name, method_class);
        }
        if (slot_definition_class.raw != Value.nil.raw) {
            const std_slot_name = try self.intern("standard-slot-definition");
            try self.putLispClass(std_slot_name, slot_definition_class);
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

test "heap collectGarbage reuses gc_slots buffer" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var roots = [_]Value{};
    _ = try heap.collectGarbage(&roots);
    const cap1 = heap.gc_slots.capacity;
    try testing.expect(cap1 > 0);

    _ = try heap.collectGarbage(&roots);
    try testing.expectEqual(cap1, heap.gc_slots.capacity);
}

test "heap collectGarbageRootSet updates multi-range" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var r0 = [_]Value{try heap.allocCons(Value.makeFixnum(1), Value.nil)};
    var r1 = [_]Value{try heap.allocCons(Value.makeFixnum(2), Value.nil)};
    const raw0 = r0[0].raw;
    const raw1 = r1[0].raw;

    var ranges = [_]roots_mod.RootRange{
        .{ .ptr = r0[0..].ptr, .len = r0.len },
        .{ .ptr = r1[0..].ptr, .len = r1.len },
    };
    _ = try heap.collectGarbageRootSet(.{
        .ranges = ranges[0..],
        .slots = &[_]*Value{},
    });

    try testing.expect(r0[0].isCons());
    try testing.expect(r1[0].isCons());
    try testing.expect(r0[0].raw != raw0);
    try testing.expect(r1[0].raw != raw1);

    const c0 = r0[0].toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), c0.car.toFixnum());
    try testing.expect(c0.cdr.isNil());

    const c1 = r1[0].toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 2), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
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

test "gc forwarding ignores string length 14 header" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const bytes = "abcdefghijklmn"; // 14 bytes; matches forwarding tag bits
    const str = try heap.allocBaseString(bytes);
    var roots = [_]Value{str};
    _ = try heap.collectGarbage(roots[0..]);

    const moved = roots[0];
    try testing.expect(moved.isString());
    try testing.expectEqualStrings(bytes, moved.toPtr(objects.String).bytes());
}

test "heap alloc string32 from utf8 replaces invalid" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const bytes = [_]u8{ 0xC2, 0x20, 'A' };
    const str = try heap.allocString32FromUtf8(&bytes);
    try testing.expect(str.isString32());

    const ptr = str.toPtr(objects.String32);
    try testing.expectEqual(@as(u32, 3), ptr.length);
    try testing.expectEqual(@as(u32, 0xFFFD), ptr.data[0]);
    try testing.expectEqual(@as(u32, 0x20), ptr.data[1]);
    try testing.expectEqual(@as(u32, 'A'), ptr.data[2]);
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
