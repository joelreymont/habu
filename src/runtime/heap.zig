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
const Tag = @import("value.zig").Tag;
const objects = @import("objects.zig");
const Symbol = objects.Symbol;
const gc_mod = @import("gc.zig");
const GC = gc_mod.GC;
const roots_mod = @import("roots.zig");

pub const ALIGNMENT: usize = 16;
pub const CARD_SHIFT: u6 = 9; // 512-byte cards
pub const CARD_SIZE: usize = 1 << CARD_SHIFT;
pub const CARD_GRAIN_SHIFT: u6 = 6; // 64-byte card lanes
pub const CARD_GRAIN_SIZE: usize = 1 << CARD_GRAIN_SHIFT;
const CARD_GRAIN_N: usize = CARD_SIZE / CARD_GRAIN_SIZE;
comptime {
    if (CARD_SIZE % CARD_GRAIN_SIZE != 0) @compileError("CARD_GRAIN_SIZE must divide CARD_SIZE");
    if (CARD_GRAIN_N > 8) @compileError("CARD_GRAIN_N must fit in a u8 bitmask");
}
const ALLOC_SAMPLE_MASK: usize = 7; // Sample 1/8 allocations.
const NURSERY_HEADROOM_MIN: usize = 64 * 1024;
const NURSERY_HEADROOM_SHIFT: u6 = 3; // 12.5% live-set headroom.
const GC_DEBT_MIN_BYTES: usize = 64 * 1024;
const GC_DEBT_TARGET_SHIFT: u6 = 2; // 25% of nursery target.
const TENURED_FREE_BIN_N: usize = 20;
const TENURED_ALLOC_SCAN_BUDGET: usize = 64;
const TENURED_SPLIT_MIN_REMAINDER: usize = 64;

pub const AllocClass = enum(u8) {
    cons,
    symbol,
    keyword,
    vector,
    array,
    string,
    closure,
    stream,
    hash_table,
    chunk,
    package,
    pathname,
    condition,
    native_code,
    rational,
    complex,
    bignum,
    macro_env,
    other,
};

const ALLOC_CLASS_N: usize = std.meta.fields(AllocClass).len;
pub const ALLOC_SIZE_N: usize = 8;
pub const GC_AGE_N: usize = 8;

/// Interned symbol table for eq comparison
pub const SymbolTable = struct {
    /// Map from name to interned symbol Value
    map: std.StringHashMapUnmanaged(Value),
    /// Backing allocator for keys
    allocator: std.mem.Allocator,
    /// Monotonic mutation version for cache invalidation.
    version: u64,

    pub fn init(allocator: std.mem.Allocator) SymbolTable {
        return .{
            .map = .{},
            .allocator = allocator,
            .version = 0,
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
        self.version +%= 1;
    }

    pub fn remove(self: *SymbolTable, name: []const u8) bool {
        if (self.map.fetchRemove(name)) |removed| {
            self.allocator.free(removed.key);
            self.version +%= 1;
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

fn elapsedNsSince(start_ns: i128) u64 {
    const now_ns = std.time.nanoTimestamp();
    if (now_ns <= start_ns) return 0;
    return @intCast(now_ns - start_ns);
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
    total_size: usize = 256 * 1024 * 1024, // 256MB default
    /// GC threshold (trigger GC when from-space is this full)
    gc_threshold: f32 = 0.9,
    /// Layout mode (semispace today, generational scaffold for next steps).
    gc_layout: GcLayoutMode = .semispace,
    /// Generational scaffold sizing (used when gc_layout = .generational).
    generational: GenerationalConfig = .{},
};

pub const GcLayoutMode = enum {
    semispace,
    generational,
};

pub const GenerationalConfig = struct {
    /// Size of each nursery semispace (from/to).
    nursery_each: ?usize = null,
    /// Optional large-object-space size.
    los_size: ?usize = null,
    /// Allocate objects at/above this size in LOS (non-moving).
    los_threshold: usize = 32 * 1024,
    /// Promote pointer-free survivors at/above this size.
    promote_threshold: usize = 1024,
    /// Lower bound for adaptive promotion threshold.
    promote_threshold_min: ?usize = null,
    /// Upper bound for adaptive promotion threshold.
    promote_threshold_max: ?usize = null,
};

pub const Region = struct {
    start: [*]align(ALIGNMENT) u8,
    end: [*]u8,

    pub fn len(self: Region) usize {
        return @intFromPtr(self.end) - @intFromPtr(self.start);
    }
};

pub const CardRun = struct {
    start_idx: usize,
    end_idx: usize, // exclusive
};

pub const HeapLayout = struct {
    mode: GcLayoutMode,
    nursery_from: Region,
    nursery_to: Region,
    tenured: ?Region,
    los: ?Region,
};

/// Semispace heap with bump allocation
pub const Heap = struct {
    /// Backing memory (both semispaces)
    memory: []align(ALIGNMENT) u8,
    /// Reserved region layout (nursery/tenured/LOS scaffold).
    layout: HeapLayout,
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
    nursery_target_bytes: usize,
    nursery_min_bytes: usize,
    nursery_max_bytes: usize,
    nursery_target_pause_ns: u64,
    gc_debt_bytes: usize,
    gc_debt_threshold_bytes: usize,
    /// Reusable buffer for building GC root slot lists.
    gc_slots: std.ArrayList(*Value),
    /// Cached internal root slots (symbols/packages/readtables).
    gc_internal_slots: std.ArrayList(*Value),
    /// Structural signature of root-bearing tables.
    gc_root_sig: GcRootSig,
    gc_root_sig_valid: bool,
    /// Nursery survivor age maps (addr -> minor survival age).
    survivor_age_cur: std.AutoHashMapUnmanaged(usize, u8),
    survivor_age_next: std.AutoHashMapUnmanaged(usize, u8),
    /// Card table for old->young write barriers (generational scaffold).
    card_table: []u8,
    /// Tenured bump pointer (used by minor-GC promotion policy).
    tenured_alloc_ptr: ?[*]align(ALIGNMENT) u8,
    promote_threshold: usize,
    promote_threshold_min: usize,
    promote_threshold_max: usize,
    /// Metadata for promoted tenured objects (for remembered-set scans).
    tenured_objs: std.ArrayList(TenuredObj),
    /// Free spans reclaimed by tenured sweep (non-moving reuse).
    tenured_free: std.ArrayList(FreeSpan),
    /// Segregated tenured free bins for reuse fast-path allocation.
    tenured_free_bins: [TENURED_FREE_BIN_N]std.ArrayList(FreeSpan),
    /// LOS bump pointer and metadata.
    los_alloc_ptr: ?[*]align(ALIGNMENT) u8,
    los_threshold: usize,
    los_threshold_min: usize,
    los_threshold_max: usize,
    los_target_pause_ns: u64,
    los_objs: std.ArrayList(TenuredObj),
    los_free: std.ArrayList(FreeSpan),
    los_free_bins: [TENURED_FREE_BIN_N]std.ArrayList(FreeSpan),
    major_cycle_active: bool,
    /// Persistent collector state reused across GC cycles.
    gc: GC,
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
        alloc_sample_n: usize = 0,
        alloc_sample_bytes: usize = 0,
        alloc_sample_class: [ALLOC_CLASS_N]usize = [_]usize{0} ** ALLOC_CLASS_N,
        alloc_sample_size: [ALLOC_SIZE_N]usize = [_]usize{0} ** ALLOC_SIZE_N,
        gc_count: usize = 0,
        gc_minor_count: usize = 0,
        gc_major_count: usize = 0,
        bytes_copied: usize = 0,
        gc_minor_ns: u64 = 0,
        gc_major_ns: u64 = 0,
        gc_survive_n: usize = 0,
        gc_survive_bytes: usize = 0,
        gc_survive_class: [ALLOC_CLASS_N]usize = [_]usize{0} ** ALLOC_CLASS_N,
        gc_survive_size: [ALLOC_SIZE_N]usize = [_]usize{0} ** ALLOC_SIZE_N,
        gc_survive_age: [GC_AGE_N]usize = [_]usize{0} ** GC_AGE_N,
        gc_survive_age_class: [ALLOC_CLASS_N][GC_AGE_N]usize = [_][GC_AGE_N]usize{[_]usize{0} ** GC_AGE_N} ** ALLOC_CLASS_N,
        gc_promote_n: usize = 0,
        gc_promote_bytes: usize = 0,
        gc_promote_class: [ALLOC_CLASS_N]usize = [_]usize{0} ** ALLOC_CLASS_N,
        gc_promote_size: [ALLOC_SIZE_N]usize = [_]usize{0} ** ALLOC_SIZE_N,
        gc_promote_age: [GC_AGE_N]usize = [_]usize{0} ** GC_AGE_N,
        gc_promote_age_class: [ALLOC_CLASS_N][GC_AGE_N]usize = [_][GC_AGE_N]usize{[_]usize{0} ** GC_AGE_N} ** ALLOC_CLASS_N,
        gc_promote_success_n: usize = 0,
        gc_promote_success_bytes: usize = 0,
        gc_promote_success_class: [ALLOC_CLASS_N]usize = [_]usize{0} ** ALLOC_CLASS_N,
        gc_promote_success_age: [GC_AGE_N]usize = [_]usize{0} ** GC_AGE_N,
        gc_promote_success_age_class: [ALLOC_CLASS_N][GC_AGE_N]usize = [_][GC_AGE_N]usize{[_]usize{0} ** GC_AGE_N} ** ALLOC_CLASS_N,
        gc_promote_threshold: usize = 0,
        gc_promote_threshold_min: usize = 0,
        gc_promote_threshold_max: usize = 0,
        gc_promote_scale: f64 = 1.0,
        gc_promote_success_rate: f64 = 0.0,
        gc_promote_young_ratio: f64 = 0.0,
        gc_promote_mature_ratio: f64 = 0.0,
        gc_nursery_target: usize = 0,
        gc_nursery_scale: f64 = 1.0,
        gc_nursery_survival: f64 = 0.0,
        gc_nursery_pause_error: f64 = 0.0,
        gc_los_threshold: usize = 0,
        gc_los_threshold_min: usize = 0,
        gc_los_threshold_max: usize = 0,
        gc_los_scale: f64 = 1.0,
        gc_los_large_ratio: f64 = 0.0,
        gc_los_occupancy: f64 = 0.0,
        gc_los_pause_error: f64 = 0.0,
        gc_build_ns: u64 = 0,
        gc_root_ns: u64 = 0,
        gc_copy_ns: u64 = 0,
        gc_finalize_ns: u64 = 0,
        gc_root_vals: usize = 0,
        gc_remembered_scanned: usize = 0,
        gc_remembered_runs: usize = 0,
        gc_remembered_marked_cards: usize = 0,
        gc_major_cycle_n: usize = 0,
        gc_major_mark_steps: usize = 0,
        gc_major_sweep_tenured_steps: usize = 0,
        gc_major_sweep_los_steps: usize = 0,
        gc_major_swept_tenured: usize = 0,
        gc_major_swept_los: usize = 0,
        gc_major_max_tenured_slice: usize = 0,
        gc_major_max_los_slice: usize = 0,
        gc_debt_bytes: usize = 0,
        gc_debt_threshold: usize = 0,
        gc_debt_alloc_bytes: usize = 0,
        gc_debt_paydown_bytes: usize = 0,
        gc_debt_trigger_n: usize = 0,
        gc_debt_skip_n: usize = 0,
        gc_debt_score: f64 = 0.0,
        gc_debt_ratio: f64 = 0.0,
        gc_debt_occupancy: f64 = 0.0,
        gc_debt_survival: f64 = 0.0,
        gc_debt_pause_error: f64 = 0.0,
        wb_marks: usize = 0,
        gc_promoted_bytes: usize = 0,
    };

    pub const TenuredObj = struct {
        addr: usize,
        tag: Tag,
        size: usize,
        marked: bool = false,
        promoted_cycle: usize = 0,
        promoted_age: u8 = 0,
        promote_success_recorded: bool = false,
    };

    pub const SurvivorAgeEntry = struct {
        addr: usize,
        age: u8,
    };

    pub const FreeSpan = struct {
        addr: usize,
        size: usize,
    };

    pub const FreeSpanStats = struct {
        span_n: usize = 0,
        bytes: usize = 0,
        largest: usize = 0,
    };

    pub const SweepSliceResult = struct {
        done: bool,
        scanned: usize,
    };

    const GcRootSig = struct {
        symbols_ver: u64 = 0,
        keywords_ver: u64 = 0,
        packages: usize = 0,
        package_ptr_sum: usize = 0,
        pkg_symbols_ver: u64 = 0,
        readtable: usize = 0,
        dispatch_tables: usize = 0,
        dispatch_entries: usize = 0,

        fn eql(a: GcRootSig, b: GcRootSig) bool {
            return a.symbols_ver == b.symbols_ver and
                a.keywords_ver == b.keywords_ver and
                a.packages == b.packages and
                a.package_ptr_sum == b.package_ptr_sum and
                a.pkg_symbols_ver == b.pkg_symbols_ver and
                a.readtable == b.readtable and
                a.dispatch_tables == b.dispatch_tables and
                a.dispatch_entries == b.dispatch_entries;
        }
    };

    pub const WarnHandler = *const fn (Value, ?*anyopaque) anyerror!void;

    fn alignDown(size: usize) usize {
        return size & ~(ALIGNMENT - 1);
    }

    fn buildLayout(memory: []align(ALIGNMENT) u8, config: Config) !HeapLayout {
        const total = memory.len;
        const base: [*]align(ALIGNMENT) u8 = @alignCast(memory.ptr);

        switch (config.gc_layout) {
            .semispace => {
                const nursery_each = alignDown(total / 2);
                if (nursery_each == 0) return error.InvalidLayout;

                const from_start = base;
                const from_end = memory.ptr + nursery_each;
                const to_start: [*]align(ALIGNMENT) u8 = @alignCast(memory.ptr + nursery_each);
                const to_end = memory.ptr + nursery_each * 2;

                return .{
                    .mode = .semispace,
                    .nursery_from = .{ .start = from_start, .end = from_end },
                    .nursery_to = .{ .start = to_start, .end = to_end },
                    .tenured = null,
                    .los = null,
                };
            },
            .generational => {
                const nursery_each = alignDown(config.generational.nursery_each orelse (total / 8));
                const los_size = alignDown(config.generational.los_size orelse (total / 8));
                if (nursery_each == 0) return error.InvalidLayout;
                if (los_size >= total) return error.InvalidLayout;

                const nursery_total = nursery_each * 2;
                if (nursery_total >= total) return error.InvalidLayout;
                if (nursery_total > total - los_size) return error.InvalidLayout;

                const tenured_size = alignDown(total - nursery_total - los_size);
                if (tenured_size == 0) return error.InvalidLayout;

                const nursery_from_start = base;
                const nursery_from_end = memory.ptr + nursery_each;
                const nursery_to_start: [*]align(ALIGNMENT) u8 = @alignCast(memory.ptr + nursery_each);
                const nursery_to_end = memory.ptr + nursery_total;

                const tenured_start: [*]align(ALIGNMENT) u8 = @alignCast(memory.ptr + nursery_total);
                const tenured_end = memory.ptr + nursery_total + tenured_size;

                const los_start: [*]align(ALIGNMENT) u8 = @alignCast(tenured_end);
                const los_end = tenured_end + los_size;

                return .{
                    .mode = .generational,
                    .nursery_from = .{ .start = nursery_from_start, .end = nursery_from_end },
                    .nursery_to = .{ .start = nursery_to_start, .end = nursery_to_end },
                    .tenured = .{ .start = tenured_start, .end = tenured_end },
                    .los = .{ .start = los_start, .end = los_end },
                };
            },
        }
    }

    /// Initialize a new heap
    pub fn init(allocator: std.mem.Allocator, config: Config) !Heap {
        // Zig 0.15: alignment is an enum, .@"16" for 16-byte alignment
        const memory = try allocator.alignedAlloc(u8, .@"16", config.total_size);
        errdefer allocator.free(memory);
        const card_len = (config.total_size + CARD_SIZE - 1) / CARD_SIZE;
        const card_table = try allocator.alloc(u8, card_len);
        errdefer allocator.free(card_table);
        @memset(card_table, 0);

        const layout = try buildLayout(memory, config);
        const space_size = layout.nursery_from.len();
        const from_start = layout.nursery_from.start;
        const to_start = layout.nursery_to.start;
        const raw_threshold: usize = @intFromFloat(@as(f32, @floatFromInt(space_size)) * config.gc_threshold);
        const aligned_threshold = std.mem.alignForward(usize, raw_threshold, ALIGNMENT);
        const nursery_max = alignDown(space_size);
        const nursery_min_default = alignDown(@max(space_size / 8, @as(usize, 64 * 1024)));
        const nursery_min = if (layout.mode == .generational) nursery_min_default else nursery_max;
        var nursery_target = aligned_threshold;
        if (nursery_target < nursery_min) nursery_target = nursery_min;
        if (nursery_target > nursery_max) nursery_target = nursery_max;
        var debt_threshold = std.mem.alignForward(
            usize,
            @max(nursery_target >> GC_DEBT_TARGET_SHIFT, @as(usize, GC_DEBT_MIN_BYTES)),
            ALIGNMENT,
        );
        if (debt_threshold > nursery_target) debt_threshold = nursery_target;

        const base_promote = std.mem.alignForward(usize, @max(config.generational.promote_threshold, @as(usize, ALIGNMENT)), ALIGNMENT);
        const min_default = @max(base_promote / 8, @as(usize, 64));
        var promote_min = std.mem.alignForward(usize, config.generational.promote_threshold_min orelse min_default, ALIGNMENT);
        const max_default = if (layout.mode == .generational) @max(base_promote, nursery_max / 2) else base_promote;
        const promote_max = std.mem.alignForward(usize, config.generational.promote_threshold_max orelse max_default, ALIGNMENT);
        if (promote_min > promote_max) promote_min = promote_max;
        var promote_target = base_promote;
        if (promote_target < promote_min) promote_target = promote_min;
        if (promote_target > promote_max) promote_target = promote_max;

        const base_los_threshold = std.mem.alignForward(usize, @max(config.generational.los_threshold, @as(usize, ALIGNMENT)), ALIGNMENT);
        const los_min_default = @max(base_los_threshold / 4, @as(usize, 256));
        var los_threshold_min = std.mem.alignForward(usize, los_min_default, ALIGNMENT);
        const los_max_default = if (layout.mode == .generational) @max(base_los_threshold, nursery_max / 2) else base_los_threshold;
        const los_threshold_max = std.mem.alignForward(usize, los_max_default, ALIGNMENT);
        if (los_threshold_min > los_threshold_max) los_threshold_min = los_threshold_max;
        var los_threshold_target = base_los_threshold;
        if (los_threshold_target < los_threshold_min) los_threshold_target = los_threshold_min;
        if (los_threshold_target > los_threshold_max) los_threshold_target = los_threshold_max;

        var heap = Heap{
            .memory = memory,
            .layout = layout,
            .space_size = space_size,
            .from_start = from_start,
            .to_start = to_start,
            .alloc_ptr = from_start,
            .from_end = layout.nursery_from.end,
            .gc_threshold = nursery_target,
            .nursery_target_bytes = nursery_target,
            .nursery_min_bytes = nursery_min,
            .nursery_max_bytes = nursery_max,
            .nursery_target_pause_ns = 10_000_000,
            .gc_debt_bytes = 0,
            .gc_debt_threshold_bytes = debt_threshold,
            .gc_slots = std.ArrayList(*Value){},
            .gc_internal_slots = std.ArrayList(*Value){},
            .gc_root_sig = .{},
            .gc_root_sig_valid = false,
            .survivor_age_cur = .{},
            .survivor_age_next = .{},
            .card_table = card_table,
            .tenured_alloc_ptr = if (layout.tenured) |r| r.start else null,
            .promote_threshold = promote_target,
            .promote_threshold_min = promote_min,
            .promote_threshold_max = promote_max,
            .tenured_objs = std.ArrayList(TenuredObj){},
            .tenured_free = std.ArrayList(FreeSpan){},
            .tenured_free_bins = [_]std.ArrayList(FreeSpan){std.ArrayList(FreeSpan){}} ** TENURED_FREE_BIN_N,
            .los_alloc_ptr = if (layout.los) |r| r.start else null,
            .los_threshold = los_threshold_target,
            .los_threshold_min = los_threshold_min,
            .los_threshold_max = los_threshold_max,
            .los_target_pause_ns = 10_000_000,
            .los_objs = std.ArrayList(TenuredObj){},
            .los_free = std.ArrayList(FreeSpan){},
            .los_free_bins = [_]std.ArrayList(FreeSpan){std.ArrayList(FreeSpan){}} ** TENURED_FREE_BIN_N,
            .major_cycle_active = false,
            .gc = GC.init(allocator),
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
        heap.stats.gc_nursery_target = nursery_target;
        heap.stats.gc_debt_threshold = debt_threshold;
        heap.stats.gc_promote_threshold = promote_target;
        heap.stats.gc_promote_threshold_min = promote_min;
        heap.stats.gc_promote_threshold_max = promote_max;
        heap.stats.gc_los_threshold = los_threshold_target;
        heap.stats.gc_los_threshold_min = los_threshold_min;
        heap.stats.gc_los_threshold_max = los_threshold_max;

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
        self.gc_internal_slots.deinit(self.backing_allocator);
        self.survivor_age_cur.deinit(self.backing_allocator);
        self.survivor_age_next.deinit(self.backing_allocator);
        self.tenured_objs.deinit(self.backing_allocator);
        self.tenured_free.deinit(self.backing_allocator);
        for (&self.tenured_free_bins) |*bin| {
            bin.deinit(self.backing_allocator);
        }
        self.los_objs.deinit(self.backing_allocator);
        self.los_free.deinit(self.backing_allocator);
        for (&self.los_free_bins) |*bin| {
            bin.deinit(self.backing_allocator);
        }
        self.gc.deinit();
        self.backing_allocator.free(self.card_table);
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

    pub fn gcLayoutMode(self: *const Heap) GcLayoutMode {
        return self.layout.mode;
    }

    pub fn setMajorCycleActive(self: *Heap, active: bool) void {
        if (self.layout.mode != .generational) return;
        self.major_cycle_active = active;
    }

    pub fn isMajorCycleActive(self: *const Heap) bool {
        return self.layout.mode == .generational and self.major_cycle_active;
    }

    pub fn nurseryFromRegion(self: *const Heap) Region {
        return self.layout.nursery_from;
    }

    pub fn nurseryToRegion(self: *const Heap) Region {
        return self.layout.nursery_to;
    }

    pub fn tenuredRegion(self: *const Heap) ?Region {
        return self.layout.tenured;
    }

    pub fn losRegion(self: *const Heap) ?Region {
        return self.layout.los;
    }

    fn containsAddr(self: *const Heap, addr: usize) bool {
        const start = @intFromPtr(self.memory.ptr);
        const end = start + self.memory.len;
        return addr >= start and addr < end;
    }

    fn regionContains(r: Region, addr: usize) bool {
        return addr >= @intFromPtr(r.start) and addr < @intFromPtr(r.end);
    }

    pub fn isInNurseryAddr(self: *const Heap, addr: usize) bool {
        return regionContains(self.layout.nursery_from, addr) or regionContains(self.layout.nursery_to, addr);
    }

    fn cardIndexForAddr(self: *const Heap, addr: usize) ?usize {
        if (!self.containsAddr(addr)) return null;
        const base = @intFromPtr(self.memory.ptr);
        return (addr - base) >> CARD_SHIFT;
    }

    fn cardLaneBitForAddr(self: *const Heap, addr: usize) u8 {
        const base = @intFromPtr(self.memory.ptr);
        const lane = ((addr - base) & (CARD_SIZE - 1)) >> CARD_GRAIN_SHIFT;
        return @as(u8, 1) << @as(u3, @intCast(lane));
    }

    fn cardLaneMaskForRange(self: *const Heap, card_idx: usize, start_addr: usize, end_addr: usize) u8 {
        const card = self.cardRange(card_idx) orelse return 0;
        const card_start = @intFromPtr(card.start);
        const card_end = @intFromPtr(card.end);
        const lo = @max(start_addr, card_start);
        const hi = @min(end_addr, card_end);
        if (hi <= lo) return 0;

        const lane_lo = (lo - card_start) >> CARD_GRAIN_SHIFT;
        const lane_hi = (hi - 1 - card_start) >> CARD_GRAIN_SHIFT;
        var mask: u8 = 0;
        var lane = lane_lo;
        while (lane <= lane_hi) : (lane += 1) {
            mask |= @as(u8, 1) << @as(u3, @intCast(lane));
        }
        return mask;
    }

    pub fn clearCardTable(self: *Heap) void {
        @memset(self.card_table, 0);
    }

    pub fn isCardMarkedForAddr(self: *const Heap, addr: usize) bool {
        const idx = self.cardIndexForAddr(addr) orelse return false;
        const bit = self.cardLaneBitForAddr(addr);
        return (self.card_table[idx] & bit) != 0;
    }

    pub fn markedCardCount(self: *const Heap) usize {
        var n: usize = 0;
        for (self.card_table) |v| {
            if (v != 0) n += 1;
        }
        return n;
    }

    pub fn cardRange(self: *const Heap, card_idx: usize) ?Region {
        if (card_idx >= self.card_table.len) return null;
        const base = @intFromPtr(self.memory.ptr);
        const mem_end = base + self.memory.len;
        const start_addr = base + card_idx * CARD_SIZE;
        var end_addr = start_addr + CARD_SIZE;
        if (end_addr > mem_end) end_addr = mem_end;
        return .{
            .start = @ptrFromInt(start_addr),
            .end = @ptrFromInt(end_addr),
        };
    }

    pub fn appendMarkedCards(self: *const Heap, allocator: std.mem.Allocator, out: *std.ArrayList(usize)) !void {
        for (self.card_table, 0..) |v, idx| {
            if (v != 0) try out.append(allocator, idx);
        }
    }

    pub fn appendMarkedCardRuns(self: *const Heap, allocator: std.mem.Allocator, out: *std.ArrayList(CardRun)) !void {
        var idx: usize = 0;
        while (idx < self.card_table.len) {
            while (idx < self.card_table.len and self.card_table[idx] == 0) : (idx += 1) {}
            if (idx >= self.card_table.len) break;
            const start_idx = idx;
            idx += 1;
            while (idx < self.card_table.len and self.card_table[idx] != 0) : (idx += 1) {}
            try out.append(allocator, .{
                .start_idx = start_idx,
                .end_idx = idx,
            });
        }
    }

    pub fn appendMarkedCardRanges(self: *const Heap, allocator: std.mem.Allocator, out: *std.ArrayList(Region)) !void {
        for (self.card_table, 0..) |v, idx| {
            if (v == 0) continue;
            if (self.cardRange(idx)) |r| {
                try out.append(allocator, r);
            }
        }
    }

    pub fn clearMarkedCards(self: *Heap, cards: []const usize) void {
        for (cards) |idx| {
            if (idx < self.card_table.len) self.card_table[idx] = 0;
        }
    }

    pub fn writeBarrier(self: *Heap, owner: Value, stored: Value) void {
        if (self.layout.mode != .generational) return;
        if (!owner.isPointer() or !stored.isPointer()) return;

        const owner_addr = owner.toPtrAddr();
        const stored_addr = stored.toPtrAddr();
        if (!self.containsAddr(owner_addr) or !self.containsAddr(stored_addr)) return;
        if (self.isInNurseryAddr(owner_addr)) return;
        if (!self.major_cycle_active and !self.isInNurseryAddr(stored_addr)) return;

        const card_idx = self.cardIndexForAddr(owner_addr) orelse return;
        const bit = self.cardLaneBitForAddr(owner_addr);
        if ((self.card_table[card_idx] & bit) == 0) {
            self.card_table[card_idx] |= bit;
            self.stats.wb_marks +%= 1;
        }
    }

    pub fn markCardForOwnerAddr(self: *Heap, owner_addr: usize) void {
        if (self.layout.mode != .generational) return;
        if (!self.containsAddr(owner_addr)) return;
        if (self.isInNurseryAddr(owner_addr)) return;
        const card_idx = self.cardIndexForAddr(owner_addr) orelse return;
        const bit = self.cardLaneBitForAddr(owner_addr);
        if ((self.card_table[card_idx] & bit) == 0) {
            self.card_table[card_idx] |= bit;
        }
    }

    pub fn hasMarkedCardInAddrRange(self: *const Heap, start_addr: usize, end_addr: usize) bool {
        if (end_addr <= start_addr) return false;
        if (!self.containsAddr(start_addr)) return false;
        const last_addr = end_addr - 1;
        if (!self.containsAddr(last_addr)) return false;

        const start_idx = self.cardIndexForAddr(start_addr) orelse return false;
        const end_idx = self.cardIndexForAddr(last_addr) orelse return false;
        for (start_idx..end_idx + 1) |idx| {
            const mask = self.cardLaneMaskForRange(idx, start_addr, end_addr);
            if (mask != 0 and (self.card_table[idx] & mask) != 0) return true;
        }
        return false;
    }

    pub fn hasMarkedCardInAddrRangeRuns(self: *const Heap, start_addr: usize, end_addr: usize, runs: []const CardRun) bool {
        if (runs.len == 0) return false;
        if (end_addr <= start_addr) return false;
        if (!self.containsAddr(start_addr)) return false;
        const last_addr = end_addr - 1;
        if (!self.containsAddr(last_addr)) return false;

        const start_idx = self.cardIndexForAddr(start_addr) orelse return false;
        const end_idx = self.cardIndexForAddr(last_addr) orelse return false;
        for (runs) |run| {
            if (run.end_idx <= start_idx) continue;
            if (run.start_idx > end_idx) break;

            const card_lo = @max(run.start_idx, start_idx);
            const card_hi = @min(run.end_idx - 1, end_idx);
            var idx = card_lo;
            while (idx <= card_hi) : (idx += 1) {
                const mask = self.cardLaneMaskForRange(idx, start_addr, end_addr);
                if (mask != 0 and (self.card_table[idx] & mask) != 0) return true;
            }
        }
        return false;
    }

    fn freeBinIndex(size: usize) usize {
        var cap = ALIGNMENT;
        var idx: usize = 0;
        while (idx + 1 < TENURED_FREE_BIN_N and size > cap) : (idx += 1) {
            cap <<= 1;
        }
        return idx;
    }

    fn drainBinsToList(
        self: *Heap,
        bins: *[TENURED_FREE_BIN_N]std.ArrayList(FreeSpan),
        list: *std.ArrayList(FreeSpan),
    ) !void {
        var total: usize = 0;
        for (bins.*) |bin| {
            total +%= bin.items.len;
        }
        if (total == 0) return;

        try list.ensureUnusedCapacity(self.backing_allocator, total);
        for (bins) |*bin| {
            for (bin.items) |span| {
                list.appendAssumeCapacity(span);
            }
            bin.clearRetainingCapacity();
        }
    }

    fn rebuildBinsFromList(
        self: *Heap,
        bins: *[TENURED_FREE_BIN_N]std.ArrayList(FreeSpan),
        list: *std.ArrayList(FreeSpan),
    ) !void {
        var counts = [_]usize{0} ** TENURED_FREE_BIN_N;
        for (list.items) |span| {
            counts[freeBinIndex(span.size)] +%= 1;
        }

        for (bins, 0..) |*bin, idx| {
            bin.clearRetainingCapacity();
            if (counts[idx] == 0) continue;
            try bin.ensureUnusedCapacity(self.backing_allocator, counts[idx]);
        }

        for (list.items) |span| {
            bins[freeBinIndex(span.size)].appendAssumeCapacity(span);
        }
        list.clearRetainingCapacity();
    }

    fn allocFromBins(
        bins: *[TENURED_FREE_BIN_N]std.ArrayList(FreeSpan),
        aligned_size: usize,
    ) ?[*]align(ALIGNMENT) u8 {
        var best_bin: usize = 0;
        var best_idx: usize = 0;
        var best_waste: usize = 0;
        var have_best = false;
        var scanned: usize = 0;

        var bin_idx = freeBinIndex(aligned_size);
        search: while (bin_idx < TENURED_FREE_BIN_N) : (bin_idx += 1) {
            var i: usize = 0;
            while (i < bins[bin_idx].items.len) : (i += 1) {
                const span_size = bins[bin_idx].items[i].size;
                if (span_size < aligned_size) continue;

                scanned +%= 1;
                const waste = span_size - aligned_size;
                if (!have_best or waste < best_waste) {
                    have_best = true;
                    best_bin = bin_idx;
                    best_idx = i;
                    best_waste = waste;
                    if (waste == 0) break :search;
                }
                if (scanned >= TENURED_ALLOC_SCAN_BUDGET) break :search;
            }
        }
        if (!have_best) return null;

        const span = &bins[best_bin].items[best_idx];
        const out_addr = span.addr;
        if (best_waste < TENURED_SPLIT_MIN_REMAINDER) {
            _ = bins[best_bin].swapRemove(best_idx);
        } else {
            span.addr += aligned_size;
            span.size = best_waste;
        }
        return @ptrFromInt(out_addr);
    }

    fn allocFromList(list: *std.ArrayList(FreeSpan), aligned_size: usize) ?[*]align(ALIGNMENT) u8 {
        var best_idx: usize = 0;
        var best_waste: usize = 0;
        var have_best = false;
        var scanned: usize = 0;

        var i: usize = 0;
        while (i < list.items.len) : (i += 1) {
            const span_size = list.items[i].size;
            if (span_size < aligned_size) continue;

            scanned +%= 1;
            const waste = span_size - aligned_size;
            if (!have_best or waste < best_waste) {
                have_best = true;
                best_idx = i;
                best_waste = waste;
                if (waste == 0) break;
            }
            if (scanned >= TENURED_ALLOC_SCAN_BUDGET) break;
        }
        if (!have_best) return null;

        const span = &list.items[best_idx];
        const out_addr = span.addr;
        if (best_waste < TENURED_SPLIT_MIN_REMAINDER) {
            _ = list.swapRemove(best_idx);
        } else {
            span.addr += aligned_size;
            span.size = best_waste;
        }
        return @ptrFromInt(out_addr);
    }

    fn coalesceFreeList(list: *std.ArrayList(FreeSpan)) void {
        if (list.items.len < 2) return;
        std.mem.sort(FreeSpan, list.items, {}, struct {
            fn lessThan(_: void, a: FreeSpan, b: FreeSpan) bool {
                return a.addr < b.addr;
            }
        }.lessThan);

        var write: usize = 0;
        var cur = list.items[0];
        var i: usize = 1;
        while (i < list.items.len) : (i += 1) {
            const next = list.items[i];
            if (cur.addr + cur.size == next.addr) {
                cur.size += next.size;
                continue;
            }
            list.items[write] = cur;
            write += 1;
            cur = next;
        }
        list.items[write] = cur;
        list.items.len = write + 1;
    }

    fn drainTenuredBinsToList(self: *Heap) !void {
        try self.drainBinsToList(&self.tenured_free_bins, &self.tenured_free);
    }

    fn rebuildTenuredBinsFromList(self: *Heap) !void {
        try self.rebuildBinsFromList(&self.tenured_free_bins, &self.tenured_free);
    }

    fn allocTenuredFromBins(self: *Heap, aligned_size: usize) ?[*]align(ALIGNMENT) u8 {
        return allocFromBins(&self.tenured_free_bins, aligned_size);
    }

    fn allocTenuredFromPendingList(self: *Heap, aligned_size: usize) ?[*]align(ALIGNMENT) u8 {
        return allocFromList(&self.tenured_free, aligned_size);
    }

    pub fn allocTenuredRaw(self: *Heap, size: usize) error{OutOfMemory}![*]align(ALIGNMENT) u8 {
        const aligned_size = std.mem.alignForward(usize, size, ALIGNMENT);

        if (self.allocTenuredFromBins(aligned_size)) |reused| return reused;
        if (self.allocTenuredFromPendingList(aligned_size)) |pending| return pending;

        const tenured = self.layout.tenured orelse return error.OutOfMemory;
        const cur_ptr = self.tenured_alloc_ptr orelse return error.OutOfMemory;
        const cur = @intFromPtr(cur_ptr);
        const end = @intFromPtr(tenured.end);
        if (cur > end) return error.OutOfMemory;
        if (aligned_size > end - cur) return error.OutOfMemory;

        const out = cur_ptr;
        self.tenured_alloc_ptr = @ptrFromInt(cur + aligned_size);
        return out;
    }

    pub fn recordTenuredObject(self: *Heap, addr: usize, tag: Tag, size: usize, age: u8) !void {
        const item: TenuredObj = .{
            .addr = addr,
            .tag = tag,
            .size = size,
            .marked = false,
            .promoted_cycle = self.stats.gc_count,
            .promoted_age = age,
            .promote_success_recorded = false,
        };
        try self.tenured_objs.append(self.backing_allocator, item);
    }

    pub fn clearTenuredMarks(self: *Heap) void {
        for (self.tenured_objs.items) |*obj| {
            obj.marked = false;
        }
    }

    pub fn markTenuredObject(self: *Heap, addr: usize) MarkResult {
        const tenured = self.layout.tenured orelse return .none;
        if (addr < @intFromPtr(tenured.start) or addr >= @intFromPtr(tenured.end)) return .none;
        for (self.tenured_objs.items) |*obj| {
            if (obj.addr == addr) {
                if (obj.marked) return .already;
                obj.marked = true;
                return .newly;
            }
        }
        return .none;
    }

    fn coalesceTenuredFree(self: *Heap) !void {
        try self.drainTenuredBinsToList();
        coalesceFreeList(&self.tenured_free);
        try self.rebuildTenuredBinsFromList();
    }

    pub fn sweepTenured(self: *Heap) !void {
        if (self.layout.mode != .generational) return;

        var dead_count: usize = 0;
        for (self.tenured_objs.items) |obj| {
            if (!obj.marked) dead_count += 1;
        }
        if (dead_count > 0) {
            try self.tenured_free.ensureUnusedCapacity(self.backing_allocator, dead_count);
        }

        var write: usize = 0;
        for (self.tenured_objs.items) |obj| {
            if (obj.marked) {
                var live = obj;
                if (!live.promote_success_recorded and live.promoted_cycle < self.stats.gc_count) {
                    const cls = allocClassForTagAddr(live.tag, live.addr);
                    const age_bucket = allocAgeBucket(live.promoted_age);
                    self.stats.gc_promote_success_n +%= 1;
                    self.stats.gc_promote_success_bytes +%= live.size;
                    self.stats.gc_promote_success_class[@intFromEnum(cls)] +%= 1;
                    self.stats.gc_promote_success_age[age_bucket] +%= 1;
                    self.stats.gc_promote_success_age_class[@intFromEnum(cls)][age_bucket] +%= 1;
                    live.promote_success_recorded = true;
                }
                live.marked = false;
                self.tenured_objs.items[write] = live;
                write += 1;
                continue;
            }
            self.tenured_free.appendAssumeCapacity(.{ .addr = obj.addr, .size = obj.size });
        }
        self.tenured_objs.items.len = write;
        try self.coalesceTenuredFree();
    }

    pub fn sweepTenuredSlice(self: *Heap, cursor: *usize, budget: usize) !SweepSliceResult {
        if (self.layout.mode != .generational) return .{ .done = true, .scanned = 0 };
        if (budget == 0) return .{ .done = false, .scanned = 0 };
        if (self.tenured_objs.items.len == 0) {
            cursor.* = 0;
            return .{ .done = true, .scanned = 0 };
        }
        if (cursor.* >= self.tenured_objs.items.len) {
            cursor.* = 0;
        }

        const reserve_n = @min(budget, self.tenured_objs.items.len - cursor.*);
        try self.tenured_free.ensureUnusedCapacity(self.backing_allocator, reserve_n);

        var scanned: usize = 0;
        var left = budget;
        while (cursor.* < self.tenured_objs.items.len and left > 0) : (left -= 1) {
            scanned +%= 1;
            const idx = cursor.*;
            const obj = self.tenured_objs.items[idx];
            if (obj.marked) {
                var live = obj;
                if (!live.promote_success_recorded and live.promoted_cycle < self.stats.gc_count) {
                    const cls = allocClassForTagAddr(live.tag, live.addr);
                    const age_bucket = allocAgeBucket(live.promoted_age);
                    self.stats.gc_promote_success_n +%= 1;
                    self.stats.gc_promote_success_bytes +%= live.size;
                    self.stats.gc_promote_success_class[@intFromEnum(cls)] +%= 1;
                    self.stats.gc_promote_success_age[age_bucket] +%= 1;
                    self.stats.gc_promote_success_age_class[@intFromEnum(cls)][age_bucket] +%= 1;
                    live.promote_success_recorded = true;
                }
                live.marked = false;
                self.tenured_objs.items[idx] = live;
                cursor.* += 1;
                continue;
            }

            self.tenured_free.appendAssumeCapacity(.{ .addr = obj.addr, .size = obj.size });
            _ = self.tenured_objs.swapRemove(idx);
        }

        if (cursor.* < self.tenured_objs.items.len) return .{ .done = false, .scanned = scanned };
        cursor.* = 0;
        try self.coalesceTenuredFree();
        return .{ .done = true, .scanned = scanned };
    }

    fn shouldAllocLos(self: *const Heap, aligned_size: usize) bool {
        return self.layout.mode == .generational and
            self.layout.los != null and
            aligned_size >= self.los_threshold;
    }

    fn drainLosBinsToList(self: *Heap) !void {
        try self.drainBinsToList(&self.los_free_bins, &self.los_free);
    }

    fn rebuildLosBinsFromList(self: *Heap) !void {
        try self.rebuildBinsFromList(&self.los_free_bins, &self.los_free);
    }

    fn allocLosFromBins(self: *Heap, aligned_size: usize) ?[*]align(ALIGNMENT) u8 {
        return allocFromBins(&self.los_free_bins, aligned_size);
    }

    fn allocLosFromPendingList(self: *Heap, aligned_size: usize) ?[*]align(ALIGNMENT) u8 {
        return allocFromList(&self.los_free, aligned_size);
    }

    fn trimLosTailFromFreeList(self: *Heap) void {
        const los = self.layout.los orelse return;
        const los_start = @intFromPtr(los.start);
        const los_end = @intFromPtr(los.end);
        var cur = @intFromPtr(self.los_alloc_ptr orelse los.start);
        if (cur < los_start or cur > los_end) return;

        while (self.los_free.items.len > 0) {
            const last_idx = self.los_free.items.len - 1;
            const span = self.los_free.items[last_idx];
            if (span.addr + span.size != cur) break;
            cur = span.addr;
            self.los_free.items.len = last_idx;
        }

        self.los_alloc_ptr = @ptrFromInt(cur);
    }

    pub fn allocLosRaw(self: *Heap, size: usize) error{OutOfMemory}![*]align(ALIGNMENT) u8 {
        const aligned_size = std.mem.alignForward(usize, size, ALIGNMENT);

        if (self.allocLosFromBins(aligned_size)) |reused| {
            self.stats.allocations +%= 1;
            self.stats.bytes_allocated +%= aligned_size;
            self.recordAllocDebt(aligned_size);
            return reused;
        }
        if (self.allocLosFromPendingList(aligned_size)) |pending| {
            self.stats.allocations +%= 1;
            self.stats.bytes_allocated +%= aligned_size;
            self.recordAllocDebt(aligned_size);
            return pending;
        }

        const los = self.layout.los orelse return error.OutOfMemory;
        const cur_ptr = self.los_alloc_ptr orelse return error.OutOfMemory;
        const cur = @intFromPtr(cur_ptr);
        const end = @intFromPtr(los.end);
        if (cur > end) return error.OutOfMemory;
        if (aligned_size > end - cur) return error.OutOfMemory;

        const out = cur_ptr;
        self.los_alloc_ptr = @ptrFromInt(cur + aligned_size);
        self.stats.allocations +%= 1;
        self.stats.bytes_allocated +%= aligned_size;
        self.recordAllocDebt(aligned_size);
        return out;
    }

    pub fn recordLosObject(self: *Heap, addr: usize, tag: Tag, size: usize) !void {
        const item: TenuredObj = .{ .addr = addr, .tag = tag, .size = size, .marked = false };
        try self.los_objs.append(self.backing_allocator, item);
    }

    pub fn clearLosMarks(self: *Heap) void {
        for (self.los_objs.items) |*obj| {
            obj.marked = false;
        }
    }

    pub const MarkResult = enum {
        none,
        already,
        newly,
    };

    pub fn markLosObject(self: *Heap, addr: usize) MarkResult {
        const los = self.layout.los orelse return .none;
        if (addr < @intFromPtr(los.start) or addr >= @intFromPtr(los.end)) return .none;
        for (self.los_objs.items) |*obj| {
            if (obj.addr == addr) {
                if (obj.marked) return .already;
                obj.marked = true;
                return .newly;
            }
        }
        return .none;
    }

    fn coalesceLosFree(self: *Heap) !void {
        try self.drainLosBinsToList();
        coalesceFreeList(&self.los_free);
        self.trimLosTailFromFreeList();
        try self.rebuildLosBinsFromList();
    }

    pub fn sweepLos(self: *Heap) !void {
        if (self.layout.mode != .generational) return;

        var dead_count: usize = 0;
        for (self.los_objs.items) |obj| {
            if (!obj.marked) dead_count += 1;
        }
        if (dead_count == 0) {
            self.clearLosMarks();
            return;
        }

        try self.los_free.ensureUnusedCapacity(self.backing_allocator, dead_count);

        var write: usize = 0;
        for (self.los_objs.items) |obj| {
            if (obj.marked) {
                var live = obj;
                live.marked = false;
                self.los_objs.items[write] = live;
                write += 1;
                continue;
            }
            self.los_free.appendAssumeCapacity(.{ .addr = obj.addr, .size = obj.size });
        }
        self.los_objs.items.len = write;
        try self.coalesceLosFree();
    }

    pub fn sweepLosSlice(self: *Heap, cursor: *usize, budget: usize) !SweepSliceResult {
        if (self.layout.mode != .generational) return .{ .done = true, .scanned = 0 };
        if (budget == 0) return .{ .done = false, .scanned = 0 };
        if (self.los_objs.items.len == 0) {
            cursor.* = 0;
            return .{ .done = true, .scanned = 0 };
        }
        if (cursor.* >= self.los_objs.items.len) {
            cursor.* = 0;
        }

        const reserve_n = @min(budget, self.los_objs.items.len - cursor.*);
        try self.los_free.ensureUnusedCapacity(self.backing_allocator, reserve_n);

        var scanned: usize = 0;
        var left = budget;
        while (cursor.* < self.los_objs.items.len and left > 0) : (left -= 1) {
            scanned +%= 1;
            const idx = cursor.*;
            const obj = self.los_objs.items[idx];
            if (obj.marked) {
                var live = obj;
                live.marked = false;
                self.los_objs.items[idx] = live;
                cursor.* += 1;
                continue;
            }

            self.los_free.appendAssumeCapacity(.{ .addr = obj.addr, .size = obj.size });
            _ = self.los_objs.swapRemove(idx);
        }

        if (cursor.* < self.los_objs.items.len) return .{ .done = false, .scanned = scanned };
        cursor.* = 0;
        try self.coalesceLosFree();
        return .{ .done = true, .scanned = scanned };
    }

    pub fn tenuredBytesUsed(self: *const Heap) usize {
        const tenured = self.layout.tenured orelse return 0;
        const ptr = self.tenured_alloc_ptr orelse return 0;
        return @intFromPtr(ptr) - @intFromPtr(tenured.start);
    }

    pub fn tenuredFreeStats(self: *const Heap) FreeSpanStats {
        var out: FreeSpanStats = .{};

        for (self.tenured_free.items) |span| {
            out.span_n +%= 1;
            out.bytes +%= span.size;
            if (span.size > out.largest) out.largest = span.size;
        }
        for (self.tenured_free_bins) |bin| {
            for (bin.items) |span| {
                out.span_n +%= 1;
                out.bytes +%= span.size;
                if (span.size > out.largest) out.largest = span.size;
            }
        }

        return out;
    }

    pub fn tenuredFragmentation(self: *const Heap) f64 {
        const free_stats = self.tenuredFreeStats();
        if (free_stats.bytes == 0) return 0.0;
        const free_f = @as(f64, @floatFromInt(free_stats.bytes));
        const largest_f = @as(f64, @floatFromInt(free_stats.largest));
        return 1.0 - (largest_f / free_f);
    }

    pub fn losBytesUsed(self: *const Heap) usize {
        const los = self.layout.los orelse return 0;
        const ptr = self.los_alloc_ptr orelse return 0;
        return @intFromPtr(ptr) - @intFromPtr(los.start);
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

    fn deriveDebtThreshold(self: *const Heap) usize {
        const base = if (self.layout.mode == .generational) self.nursery_target_bytes else self.gc_threshold;
        var threshold = std.mem.alignForward(
            usize,
            @max(base >> GC_DEBT_TARGET_SHIFT, @as(usize, GC_DEBT_MIN_BYTES)),
            ALIGNMENT,
        );
        if (threshold > base) threshold = base;
        return threshold;
    }

    fn refreshDebtThreshold(self: *Heap) void {
        self.gc_debt_threshold_bytes = self.deriveDebtThreshold();
        self.stats.gc_debt_threshold = self.gc_debt_threshold_bytes;
    }

    fn recordAllocDebt(self: *Heap, bytes: usize) void {
        self.gc_debt_bytes +%= bytes;
        self.stats.gc_debt_alloc_bytes +%= bytes;
        self.stats.gc_debt_bytes = self.gc_debt_bytes;
    }

    pub fn shouldCollectDebt(self: *const Heap) bool {
        return self.gc_debt_bytes >= self.gc_debt_threshold_bytes;
    }

    pub fn shouldCollectDebtNow(self: *Heap) bool {
        const decision = gc_mod.deriveDebtTrigger(.{
            .debt_bytes = self.gc_debt_bytes,
            .debt_threshold = self.gc_debt_threshold_bytes,
            .nursery_used_bytes = self.bytesUsed(),
            .nursery_target_bytes = self.nursery_target_bytes,
            .survival_ratio = self.stats.gc_nursery_survival,
            .pause_error = self.stats.gc_nursery_pause_error,
        });
        self.stats.gc_debt_score = decision.score;
        self.stats.gc_debt_ratio = decision.debt_ratio;
        self.stats.gc_debt_occupancy = decision.occupancy_ratio;
        self.stats.gc_debt_survival = decision.survival_ratio;
        self.stats.gc_debt_pause_error = decision.pause_error;
        if (decision.should_collect) {
            self.stats.gc_debt_trigger_n +%= 1;
        } else {
            self.stats.gc_debt_skip_n +%= 1;
        }
        return decision.should_collect;
    }

    fn settleGcDebt(self: *Heap, copied_bytes: usize, reclaimed_bytes: usize) void {
        const paydown = @max(copied_bytes, reclaimed_bytes);
        const debt_before = self.gc_debt_bytes;
        const debt_paid = @min(paydown, debt_before);
        if (paydown >= debt_before) {
            self.gc_debt_bytes = 0;
        } else {
            self.gc_debt_bytes -= paydown;
        }
        self.stats.gc_debt_paydown_bytes +%= debt_paid;
        self.stats.gc_debt_bytes = self.gc_debt_bytes;
        self.refreshDebtThreshold();
    }

    fn nurseryLiveFloor(self: *const Heap) usize {
        if (self.layout.mode != .generational) return self.gc_threshold;
        const used = self.bytesUsed();
        const frac = used >> NURSERY_HEADROOM_SHIFT;
        const headroom = @max(frac, NURSERY_HEADROOM_MIN);
        var floor = used +| headroom;
        floor = std.mem.alignForward(usize, floor, ALIGNMENT);
        if (floor < self.nursery_min_bytes) floor = self.nursery_min_bytes;
        if (floor > self.nursery_max_bytes) floor = self.nursery_max_bytes;
        return floor;
    }

    pub fn setNurseryTarget(self: *Heap, target_bytes: usize, survival_ratio: f64, pause_error: f64, scale: f64) void {
        if (self.layout.mode != .generational) return;
        var target = target_bytes;
        if (target < self.nursery_min_bytes) target = self.nursery_min_bytes;
        if (target > self.nursery_max_bytes) target = self.nursery_max_bytes;
        const floor = self.nurseryLiveFloor();
        if (target < floor) target = floor;
        target = std.mem.alignForward(usize, target, ALIGNMENT);

        self.nursery_target_bytes = target;
        self.gc_threshold = target;
        self.stats.gc_nursery_target = target;
        self.stats.gc_nursery_scale = scale;
        self.stats.gc_nursery_survival = survival_ratio;
        self.stats.gc_nursery_pause_error = pause_error;
        self.refreshDebtThreshold();
    }

    pub fn setPromoteThreshold(self: *Heap, target_bytes: usize, scale: f64, success_rate: f64, young_ratio: f64, mature_ratio: f64) void {
        if (self.layout.mode != .generational) return;
        var target = target_bytes;
        if (target < self.promote_threshold_min) target = self.promote_threshold_min;
        if (target > self.promote_threshold_max) target = self.promote_threshold_max;
        target = std.mem.alignForward(usize, target, ALIGNMENT);

        self.promote_threshold = target;
        self.stats.gc_promote_threshold = target;
        self.stats.gc_promote_threshold_min = self.promote_threshold_min;
        self.stats.gc_promote_threshold_max = self.promote_threshold_max;
        self.stats.gc_promote_scale = scale;
        self.stats.gc_promote_success_rate = success_rate;
        self.stats.gc_promote_young_ratio = young_ratio;
        self.stats.gc_promote_mature_ratio = mature_ratio;
    }

    pub fn setLosThreshold(self: *Heap, target_bytes: usize, scale: f64, large_ratio: f64, occupancy: f64, pause_error: f64) void {
        if (self.layout.mode != .generational) return;
        var target = target_bytes;
        if (target < self.los_threshold_min) target = self.los_threshold_min;
        if (target > self.los_threshold_max) target = self.los_threshold_max;
        target = std.mem.alignForward(usize, target, ALIGNMENT);

        self.los_threshold = target;
        self.stats.gc_los_threshold = target;
        self.stats.gc_los_threshold_min = self.los_threshold_min;
        self.stats.gc_los_threshold_max = self.los_threshold_max;
        self.stats.gc_los_scale = scale;
        self.stats.gc_los_large_ratio = large_ratio;
        self.stats.gc_los_occupancy = occupancy;
        self.stats.gc_los_pause_error = pause_error;
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
        self.recordAllocDebt(aligned_size);

        return result;
    }

    fn allocClassForType(comptime T: type) AllocClass {
        if (T == objects.Cons) return .cons;
        if (T == objects.Symbol) return .symbol;
        if (T == objects.Keyword) return .keyword;
        if (T == objects.Vector) return .vector;
        if (T == objects.Array) return .array;
        if (T == objects.String or T == objects.String32) return .string;
        if (T == objects.Closure) return .closure;
        if (T == objects.Stream) return .stream;
        if (T == objects.HashTable) return .hash_table;
        if (T == objects.Chunk) return .chunk;
        if (T == objects.Package) return .package;
        if (T == objects.Pathname) return .pathname;
        if (T == objects.Condition) return .condition;
        if (T == objects.NativeCode) return .native_code;
        if (T == objects.Rational) return .rational;
        if (T == objects.Complex) return .complex;
        if (T == objects.Bignum) return .bignum;
        if (T == objects.MacroEnv) return .macro_env;
        return .other;
    }

    fn allocSizeBucket(size: usize) usize {
        if (size <= 32) return 0;
        if (size <= 64) return 1;
        if (size <= 128) return 2;
        if (size <= 256) return 3;
        if (size <= 512) return 4;
        if (size <= 1024) return 5;
        if (size <= 4096) return 6;
        return 7;
    }

    fn allocAgeBucket(age: u8) usize {
        if (age <= 1) return age;
        if (age == 2) return 2;
        if (age == 3) return 3;
        if (age <= 5) return 4;
        if (age <= 7) return 5;
        if (age <= 11) return 6;
        return 7;
    }

    pub fn nextSurvivorAge(self: *const Heap, addr: usize) u8 {
        const prev = self.survivor_age_cur.get(addr) orelse 0;
        if (prev == std.math.maxInt(u8)) return prev;
        return prev + 1;
    }

    pub fn rebuildSurvivorAges(self: *Heap, entries: []const SurvivorAgeEntry) !void {
        self.survivor_age_next.clearRetainingCapacity();
        try self.survivor_age_next.ensureTotalCapacity(self.backing_allocator, @intCast(entries.len));

        const nursery_start = @intFromPtr(self.from_start);
        const nursery_end = @intFromPtr(self.from_end);
        for (entries) |entry| {
            if (entry.addr < nursery_start or entry.addr >= nursery_end) continue;
            try self.survivor_age_next.put(self.backing_allocator, entry.addr, entry.age);
        }

        const tmp = self.survivor_age_cur;
        self.survivor_age_cur = self.survivor_age_next;
        self.survivor_age_next = tmp;
    }

    fn noteAllocSample(self: *Heap, cls: AllocClass, size: usize) void {
        if ((self.stats.allocations & ALLOC_SAMPLE_MASK) != 0) return;
        self.stats.alloc_sample_n +%= 1;
        self.stats.alloc_sample_bytes +%= size;
        self.stats.alloc_sample_class[@intFromEnum(cls)] +%= 1;
        self.stats.alloc_sample_size[allocSizeBucket(size)] +%= 1;
    }

    fn allocClassForBoxedKind(kind: objects.BoxedKind) AllocClass {
        return switch (kind) {
            .hashtable => .hash_table,
            .rational => .rational,
            .complex => .complex,
            .stream => .stream,
            .bignum => .bignum,
            .array => .array,
            .pathname => .pathname,
            .package => .package,
            .chunk => .chunk,
            .condition => .condition,
            .native_code => .native_code,
            .macro_env => .macro_env,
            else => .other,
        };
    }

    fn allocClassForTagAddr(tag: Tag, addr: usize) AllocClass {
        return switch (tag) {
            .cons => .cons,
            .symbol => .symbol,
            .keyword => .keyword,
            .vector => .vector,
            .string => .string,
            .closure => .closure,
            .boxed => blk: {
                const kind_ptr: *const objects.BoxedKind = @ptrFromInt(addr);
                break :blk allocClassForBoxedKind(kind_ptr.*);
            },
            else => .other,
        };
    }

    pub fn noteSurvival(self: *Heap, tag: Tag, addr: usize, size: usize, promoted: bool, age: u8) void {
        const cls = allocClassForTagAddr(tag, addr);
        const bucket = allocSizeBucket(size);
        const age_bucket = allocAgeBucket(age);
        self.stats.gc_survive_n +%= 1;
        self.stats.gc_survive_bytes +%= size;
        self.stats.gc_survive_class[@intFromEnum(cls)] +%= 1;
        self.stats.gc_survive_size[bucket] +%= 1;
        self.stats.gc_survive_age[age_bucket] +%= 1;
        self.stats.gc_survive_age_class[@intFromEnum(cls)][age_bucket] +%= 1;
        if (promoted) {
            self.stats.gc_promote_n +%= 1;
            self.stats.gc_promote_bytes +%= size;
            self.stats.gc_promote_class[@intFromEnum(cls)] +%= 1;
            self.stats.gc_promote_size[bucket] +%= 1;
            self.stats.gc_promote_age[age_bucket] +%= 1;
            self.stats.gc_promote_age_class[@intFromEnum(cls)][age_bucket] +%= 1;
        }
    }

    /// Allocate an object of a specific type
    pub fn alloc(self: *Heap, comptime T: type) error{OutOfMemory}!*T {
        const ptr = try self.allocRaw(@sizeOf(T));
        const aligned_size = std.mem.alignForward(usize, @sizeOf(T), ALIGNMENT);
        self.noteAllocSample(allocClassForType(T), aligned_size);
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
    pub fn allocVector(self: *Heap, length: usize, capacity: usize) error{ OutOfMemory, Overflow }!Value {
        // Allocate header + data array together
        const data_size = try std.math.mul(usize, capacity, @sizeOf(Value));
        const total_size = try std.math.add(usize, @sizeOf(objects.Vector), data_size);
        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
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

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .vector, aligned_size);
        }
        self.noteAllocSample(.vector, aligned_size);

        return Value.makeVector(vec);
    }

    /// Allocate a multi-dimensional array
    pub fn allocArray(self: *Heap, dimensions: []const u64) error{ OutOfMemory, Overflow }!Value {
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

        const aligned_size = std.mem.alignForward(usize, alloc_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(alloc_size);
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

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .boxed, aligned_size);
        }
        self.noteAllocSample(.array, aligned_size);

        return Value.makeArray(arr);
    }

    /// Allocate a string (copies the bytes)
    pub fn allocBaseString(self: *Heap, bytes: []const u8) error{ OutOfMemory, Overflow }!Value {
        const aligned_len = std.mem.alignForward(usize, bytes.len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String), aligned_len);

        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
        const str: *objects.String = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

        // Copy bytes
        @memcpy(data_ptr[0..bytes.len], bytes);

        str.* = .{
            .length = bytes.len,
            .data = data_ptr,
        };

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .string, aligned_size);
        }
        self.noteAllocSample(.string, aligned_size);

        return Value.makeString(str);
    }

    /// Allocate an uninitialized string of given length
    pub fn allocStringUninitialized(self: *Heap, len: usize) error{ OutOfMemory, Overflow }!Value {
        const aligned_len = std.mem.alignForward(usize, len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String), aligned_len);

        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
        const str: *objects.String = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

        str.* = .{
            .length = len,
            .data = data_ptr,
        };

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .string, aligned_size);
        }
        self.noteAllocSample(.string, aligned_size);

        return Value.makeString(str);
    }

    /// Allocate a String32 (UTF-32 string) from codepoints
    pub fn allocString32(self: *Heap, codepoints: []const u32) error{ OutOfMemory, Overflow }!Value {
        const byte_len = try std.math.mul(usize, codepoints.len, 4);
        const aligned_len = std.mem.alignForward(usize, byte_len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String32), aligned_len);

        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
        const s32: *objects.String32 = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u32 = @ptrCast(@alignCast(ptr + @sizeOf(objects.String32)));

        // Copy codepoints
        @memcpy(data_ptr[0..codepoints.len], codepoints);

        s32.* = .{
            .length = @intCast(codepoints.len),
            .data = data_ptr,
        };

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .boxed, aligned_size);
        }
        self.noteAllocSample(.string, aligned_size);

        return Value.makeString32(s32);
    }

    /// Allocate an uninitialized String32 of given codepoint length
    pub fn allocString32Uninitialized(self: *Heap, len: usize) error{ OutOfMemory, Overflow }!Value {
        const byte_len = try std.math.mul(usize, len, 4);
        const aligned_len = std.mem.alignForward(usize, byte_len, 8);
        const total_size = try std.math.add(usize, @sizeOf(objects.String32), aligned_len);

        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
        const s32: *objects.String32 = @ptrCast(@alignCast(ptr));

        // Data follows immediately after header
        const data_ptr: [*]u32 = @ptrCast(@alignCast(ptr + @sizeOf(objects.String32)));

        s32.* = .{
            .length = @intCast(len),
            .data = data_ptr,
        };

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .boxed, aligned_size);
        }
        self.noteAllocSample(.string, aligned_size);

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

        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
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

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .boxed, aligned_size);
        }
        self.noteAllocSample(.string, aligned_size);

        return Value.makeString32(s32);
    }

    /// Allocate a closure
    pub fn allocClosure(self: *Heap, code: Value, arity: u32, captures: []const Value) error{ OutOfMemory, Overflow }!Value {
        const captures_size = try std.math.mul(usize, captures.len, @sizeOf(Value));
        const total_size = try std.math.add(usize, @sizeOf(objects.Closure), captures_size);

        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total_size);
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

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .closure, aligned_size);
        }
        self.noteAllocSample(.closure, aligned_size);

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

        const aligned_size = std.mem.alignForward(usize, total, ALIGNMENT);
        const use_los = self.shouldAllocLos(aligned_size);
        const ptr = if (use_los) try self.allocLosRaw(aligned_size) else try self.allocRaw(total);
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

        if (use_los) {
            try self.recordLosObject(@intFromPtr(ptr), .boxed, aligned_size);
        }
        self.noteAllocSample(.chunk, aligned_size);

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
        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);

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

        self.noteAllocSample(.symbol, aligned_size);

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

    pub fn packageKey(self: *Heap, name: Value) error{ OutOfMemory, TypeError }!Value {
        return switch (name.typeKind()) {
            .string => try self.internKeyword(name.toPtr(objects.String).bytes()),
            .symbol => try self.internKeyword(name.toPtr(objects.Symbol).getName()),
            .keyword => name,
            else => error.TypeError,
        };
    }

    /// Find a Lisp-level package by name
    pub fn findLispPackage(self: *Heap, name: Value) error{ OutOfMemory, TypeError }!?Value {
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
        const aligned_size = std.mem.alignForward(usize, total_size, ALIGNMENT);

        const ptr = try self.allocRaw(total_size);
        const kw: *objects.Keyword = @ptrCast(@alignCast(ptr));
        const name_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.Keyword));

        @memcpy(name_ptr[0..name.len], name);

        kw.* = .{
            .name_len = name.len,
            .name_ptr = name_ptr,
            .hash = fnvHash(name),
        };

        self.noteAllocSample(.keyword, aligned_size);

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

    fn calcGcRootSig(self: *const Heap) GcRootSig {
        var pkg_symbols_ver: u64 = 0;
        var package_ptr_sum: usize = 0;
        var pkg_it = self.packages.valueIterator();
        while (pkg_it.next()) |pkg| {
            pkg_symbols_ver +%= pkg.*.symbols.version;
            package_ptr_sum +%= @intFromPtr(pkg.*);
        }

        var dispatch_entries: usize = 0;
        var drt_it = self.dispatch_readtable.valueIterator();
        while (drt_it.next()) |sub_table| {
            dispatch_entries +%= sub_table.count();
        }

        return .{
            .symbols_ver = self.symbols.version,
            .keywords_ver = self.keywords.version,
            .packages = self.packages.count(),
            .package_ptr_sum = package_ptr_sum,
            .pkg_symbols_ver = pkg_symbols_ver,
            .readtable = self.readtable.count(),
            .dispatch_tables = self.dispatch_readtable.count(),
            .dispatch_entries = dispatch_entries,
        };
    }

    fn refreshGcInternalSlots(self: *Heap) !void {
        const sig = self.calcGcRootSig();
        if (self.gc_root_sig_valid and GcRootSig.eql(self.gc_root_sig, sig)) return;

        self.gc_internal_slots.clearRetainingCapacity();

        // Symbol table values.
        var sym_it = self.symbols.map.valueIterator();
        while (sym_it.next()) |v| {
            try self.gc_internal_slots.append(self.backing_allocator, v);
        }

        // Keyword table values.
        var kw_it = self.keywords.map.valueIterator();
        while (kw_it.next()) |v| {
            try self.gc_internal_slots.append(self.backing_allocator, v);
        }

        // Package symbol table values.
        var pkg_it = self.packages.valueIterator();
        while (pkg_it.next()) |pkg| {
            var pkg_sym_it = pkg.*.symbols.map.valueIterator();
            while (pkg_sym_it.next()) |v| {
                try self.gc_internal_slots.append(self.backing_allocator, v);
            }
        }

        // Readtable function values.
        var rt_it = self.readtable.valueIterator();
        while (rt_it.next()) |entry| {
            try self.gc_internal_slots.append(self.backing_allocator, &entry.function);
        }

        // Dispatch readtable function values.
        var drt_it = self.dispatch_readtable.valueIterator();
        while (drt_it.next()) |sub_table| {
            var sub_it = sub_table.valueIterator();
            while (sub_it.next()) |fn_val| {
                try self.gc_internal_slots.append(self.backing_allocator, fn_val);
            }
        }

        // Lisp package/class registries and cached symbols.
        if (self.lisp_packages.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.lisp_packages);
        }
        if (self.lisp_classes.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.lisp_classes);
        }
        if (self.standard_class.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.standard_class);
        }
        if (self.built_in_class.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.built_in_class);
        }
        if (self.structure_class.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.structure_class);
        }
        if (self.sym_simple_warning.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.sym_simple_warning);
        }
        if (self.kw_format_control.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.kw_format_control);
        }
        if (self.kw_format_arguments.raw != Value.nil.raw) {
            try self.gc_internal_slots.append(self.backing_allocator, &self.kw_format_arguments);
        }

        self.gc_root_sig = sig;
        self.gc_root_sig_valid = true;
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
        const build_start = std.time.nanoTimestamp();

        // Internal roots are tracked by slot address; external roots are passed as a range.
        self.gc_slots.clearRetainingCapacity();
        try self.gc_slots.appendSlice(self.backing_allocator, external_roots.slots);
        try self.refreshGcInternalSlots();
        try self.gc_slots.appendSlice(self.backing_allocator, self.gc_internal_slots.items);

        self.stats.gc_build_ns +%= elapsedNsSince(build_start);

        // Run GC
        const copied = try self.gc.collectRootSet(self, .{
            .ranges = external_roots.ranges,
            .slots = self.gc_slots.items,
        });

        const after = self.bytesUsed();
        const reclaimed = if (before > after) before - after else 0;
        self.settleGcDebt(copied, reclaimed);
        return reclaimed;
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

test "heap init supports generational layout scaffold" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    try testing.expectEqual(GcLayoutMode.generational, heap.gcLayoutMode());
    try testing.expectEqual(@as(usize, 512 * 1024), heap.nurseryFromRegion().len());
    try testing.expectEqual(@as(usize, 512 * 1024), heap.nurseryToRegion().len());
    try testing.expect(heap.tenuredRegion() != null);
    try testing.expect(heap.losRegion() != null);

    const tenured = heap.tenuredRegion().?;
    const los = heap.losRegion().?;
    try testing.expect(tenured.len() > 0);
    try testing.expectEqual(@intFromPtr(tenured.end), @intFromPtr(los.start));
}

test "heap allocates large objects in LOS" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 256,
        },
    });
    defer heap.deinit();

    const los0 = heap.los_objs.items.len;
    const vec = try heap.allocVector(1, 64);
    try testing.expect(!heap.isInNurseryAddr(vec.toPtrAddr()));
    try testing.expect(heap.losBytesUsed() > 0);
    try testing.expectEqual(los0 + 1, heap.los_objs.items.len);
}

test "heap LOS reuses middle free spans from bins" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 256,
        },
    });
    defer heap.deinit();

    const first_size = std.mem.alignForward(usize, 0x510, ALIGNMENT);
    const middle_size = std.mem.alignForward(usize, 0x620, ALIGNMENT);
    const tail_size = std.mem.alignForward(usize, 0x590, ALIGNMENT);

    const first = try heap.allocLosRaw(first_size);
    const middle = try heap.allocLosRaw(middle_size);
    const tail = try heap.allocLosRaw(tail_size);
    try heap.recordLosObject(@intFromPtr(first), .boxed, first_size);
    try heap.recordLosObject(@intFromPtr(middle), .boxed, middle_size);
    try heap.recordLosObject(@intFromPtr(tail), .boxed, tail_size);
    const middle_addr = @intFromPtr(middle);

    for (heap.los_objs.items) |obj| {
        if (obj.addr == middle_addr) continue;
        _ = heap.markLosObject(obj.addr);
    }
    try heap.sweepLos();

    var los_bin_span_n: usize = 0;
    for (heap.los_free_bins) |bin| {
        los_bin_span_n +%= bin.items.len;
    }
    try testing.expect(los_bin_span_n > 0);
    try testing.expectEqual(@as(usize, 0), heap.los_free.items.len);

    const reused = try heap.allocLosRaw(middle_size);
    try testing.expectEqual(middle_addr, @intFromPtr(reused));
}

test "heap LOS sweep trims free tail and rewinds bump pointer" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 256,
        },
    });
    defer heap.deinit();

    const head_size = std.mem.alignForward(usize, 0x710, ALIGNMENT);
    const tail_size = std.mem.alignForward(usize, 0x7A0, ALIGNMENT);
    const head = try heap.allocLosRaw(head_size);
    const tail = try heap.allocLosRaw(tail_size);
    try heap.recordLosObject(@intFromPtr(head), .boxed, head_size);
    try heap.recordLosObject(@intFromPtr(tail), .boxed, tail_size);
    const head_addr = @intFromPtr(head);
    const tail_addr = @intFromPtr(tail);

    for (heap.los_objs.items) |obj| {
        if (obj.addr == tail_addr) continue;
        _ = heap.markLosObject(obj.addr);
    }
    try heap.sweepLos();

    try testing.expectEqual(tail_addr, @intFromPtr(heap.los_alloc_ptr.?));
    try testing.expect(heap.markLosObject(head_addr) != .none);
    try testing.expectEqual(@as(usize, 0), heap.los_free.items.len);

    var los_bin_span_n: usize = 0;
    for (heap.los_free_bins) |bin| {
        los_bin_span_n +%= bin.items.len;
    }
    try testing.expectEqual(@as(usize, 0), los_bin_span_n);

    const bumped = try heap.allocLosRaw(tail_size);
    try testing.expectEqual(tail_addr, @intFromPtr(bumped));
}

test "heap tenured free bins coalesce and reuse spans" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const span0_size = std.mem.alignForward(usize, 64, ALIGNMENT);
    const span1_size = std.mem.alignForward(usize, 160, ALIGNMENT);
    const span2_size = std.mem.alignForward(usize, 96, ALIGNMENT);

    const span0 = try heap.allocTenuredRaw(span0_size);
    const span1 = try heap.allocTenuredRaw(span1_size);
    const span2 = try heap.allocTenuredRaw(span2_size);

    try heap.tenured_free.append(heap.backing_allocator, .{
        .addr = @intFromPtr(span1),
        .size = span1_size,
    });
    try heap.tenured_free.append(heap.backing_allocator, .{
        .addr = @intFromPtr(span0),
        .size = span0_size,
    });
    try heap.coalesceTenuredFree();
    try testing.expectEqual(@as(usize, 0), heap.tenured_free.items.len);

    var free_span_n: usize = 0;
    for (heap.tenured_free_bins) |bin| {
        free_span_n += bin.items.len;
    }
    try testing.expectEqual(@as(usize, 1), free_span_n);

    const merged = try heap.allocTenuredRaw(span0_size + span1_size);
    try testing.expectEqual(@intFromPtr(span0), @intFromPtr(merged));

    try heap.tenured_free.append(heap.backing_allocator, .{
        .addr = @intFromPtr(span2),
        .size = span2_size,
    });
    const split_head_size = std.mem.alignForward(usize, 32, ALIGNMENT);
    const split_head = try heap.allocTenuredRaw(split_head_size);
    try testing.expectEqual(@intFromPtr(span2), @intFromPtr(split_head));

    try heap.coalesceTenuredFree();

    const split_tail = try heap.allocTenuredRaw(span2_size - split_head_size);
    try testing.expectEqual(@intFromPtr(span2) + split_head_size, @intFromPtr(split_tail));
}

test "heap tenured split policy avoids tiny tail fragments" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const span_size = std.mem.alignForward(usize, 96, ALIGNMENT);
    const req_size = std.mem.alignForward(usize, 80, ALIGNMENT);
    const span = try heap.allocTenuredRaw(span_size);

    try heap.tenured_free.append(heap.backing_allocator, .{
        .addr = @intFromPtr(span),
        .size = span_size,
    });
    try heap.coalesceTenuredFree();

    const alloc = try heap.allocTenuredRaw(req_size);
    try testing.expectEqual(@intFromPtr(span), @intFromPtr(alloc));

    var free_span_n: usize = 0;
    for (heap.tenured_free_bins) |bin| free_span_n += bin.items.len;
    try testing.expectEqual(@as(usize, 0), free_span_n);
    try testing.expectEqual(@as(usize, 0), heap.tenured_free.items.len);
}

test "heap tenured fragmentation metric reflects largest free span" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const span_size = std.mem.alignForward(usize, 64, ALIGNMENT);
    const span0 = try heap.allocTenuredRaw(span_size);
    _ = try heap.allocTenuredRaw(span_size);
    const span2 = try heap.allocTenuredRaw(span_size);

    try heap.tenured_free.append(heap.backing_allocator, .{
        .addr = @intFromPtr(span0),
        .size = span_size,
    });
    try heap.tenured_free.append(heap.backing_allocator, .{
        .addr = @intFromPtr(span2),
        .size = span_size,
    });
    try heap.coalesceTenuredFree();

    const free_stats = heap.tenuredFreeStats();
    try testing.expectEqual(@as(usize, 2), free_stats.span_n);
    try testing.expectEqual(@as(usize, span_size * 2), free_stats.bytes);
    try testing.expectEqual(@as(usize, span_size), free_stats.largest);
    try testing.expectApproxEqAbs(@as(f64, 0.5), heap.tenuredFragmentation(), 1e-9);
}

test "heap writeBarrier marks old-to-young cards in generational mode" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const tenured = heap.tenuredRegion().?;
    const owner_addr = @intFromPtr(tenured.start);
    const fake_owner = Value{ .raw = owner_addr };
    const young = try heap.allocCons(Value.makeFixnum(1), Value.nil);

    heap.clearCardTable();
    try testing.expect(!heap.isCardMarkedForAddr(owner_addr));
    heap.writeBarrier(fake_owner, young);
    try testing.expect(heap.isCardMarkedForAddr(owner_addr));
    try testing.expect(heap.stats.wb_marks > 0);
}

test "heap writeBarrier is disabled for semispace mode" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const owner = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    const young = try heap.allocCons(Value.makeFixnum(2), Value.nil);
    const owner_addr = owner.toPtrAddr();

    heap.clearCardTable();
    heap.writeBarrier(owner, young);
    try testing.expect(!heap.isCardMarkedForAddr(owner_addr));
}

test "heap writeBarrier marks old-to-old cards during major cycle" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const tenured = heap.tenuredRegion().?;
    const owner_addr = @intFromPtr(tenured.start);
    const other_addr = owner_addr + ALIGNMENT;
    const owner = Value{ .raw = owner_addr };
    const other_old = Value{ .raw = other_addr };

    heap.clearCardTable();
    heap.setMajorCycleActive(false);
    heap.writeBarrier(owner, other_old);
    try testing.expect(!heap.isCardMarkedForAddr(owner_addr));

    heap.setMajorCycleActive(true);
    heap.writeBarrier(owner, other_old);
    try testing.expect(heap.isCardMarkedForAddr(owner_addr));
}

test "heap remembered set APIs enumerate and clear marked cards" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const tenured = heap.tenuredRegion().?;
    const owner_a = Value{ .raw = @intFromPtr(tenured.start) };
    const owner_b = Value{ .raw = @intFromPtr(tenured.start) + CARD_SIZE * 2 };
    const young = try heap.allocCons(Value.makeFixnum(7), Value.nil);

    heap.clearCardTable();
    heap.writeBarrier(owner_a, young);
    heap.writeBarrier(owner_b, young);
    try testing.expectEqual(@as(usize, 2), heap.markedCardCount());

    var cards = std.ArrayList(usize){};
    defer cards.deinit(testing.allocator);
    try heap.appendMarkedCards(testing.allocator, &cards);
    try testing.expectEqual(@as(usize, 2), cards.items.len);

    var ranges = std.ArrayList(Region){};
    defer ranges.deinit(testing.allocator);
    try heap.appendMarkedCardRanges(testing.allocator, &ranges);
    try testing.expectEqual(@as(usize, 2), ranges.items.len);
    for (ranges.items) |r| {
        try testing.expect(r.len() > 0);
    }

    var runs = std.ArrayList(CardRun){};
    defer runs.deinit(testing.allocator);
    try heap.appendMarkedCardRuns(testing.allocator, &runs);
    try testing.expectEqual(@as(usize, 2), runs.items.len);
    try testing.expect(runs.items[0].end_idx <= runs.items[1].start_idx);

    heap.clearMarkedCards(cards.items);
    try testing.expectEqual(@as(usize, 0), heap.markedCardCount());
}

test "heap card lanes reduce same-card remembered false positives" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const tenured = heap.tenuredRegion().?;
    const base = @intFromPtr(tenured.start);
    const owner_a = Value{ .raw = base };
    const owner_b_addr = base + CARD_GRAIN_SIZE * 3;
    const owner_b = Value{ .raw = owner_b_addr };
    const young = try heap.allocCons(Value.makeFixnum(9), Value.nil);

    heap.clearCardTable();
    heap.writeBarrier(owner_a, young);
    try testing.expectEqual(@as(usize, 1), heap.markedCardCount());
    try testing.expect(heap.isCardMarkedForAddr(base));
    try testing.expect(!heap.isCardMarkedForAddr(owner_b_addr));

    heap.writeBarrier(owner_b, young);
    try testing.expectEqual(@as(usize, 1), heap.markedCardCount());
    try testing.expect(heap.isCardMarkedForAddr(owner_b_addr));

    try testing.expect(heap.hasMarkedCardInAddrRange(base, base + CARD_GRAIN_SIZE));
    try testing.expect(!heap.hasMarkedCardInAddrRange(base + CARD_GRAIN_SIZE, base + CARD_GRAIN_SIZE * 2));
    try testing.expect(heap.hasMarkedCardInAddrRange(owner_b_addr, owner_b_addr + CARD_GRAIN_SIZE));
}

test "heap debt tracks allocation pressure and GC paydown" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    const debt0 = heap.stats.gc_debt_bytes;
    _ = try heap.allocBaseString(payload);
    try testing.expect(heap.stats.gc_debt_bytes > debt0);
    try testing.expect(heap.stats.gc_debt_alloc_bytes > 0);
    try testing.expect(heap.stats.gc_debt_threshold > 0);

    const debt_before_gc = heap.stats.gc_debt_bytes;
    var roots = [_]Value{};
    _ = try heap.collectGarbage(&roots);
    try testing.expect(heap.stats.gc_debt_paydown_bytes > 0);
    try testing.expect(heap.stats.gc_debt_bytes <= debt_before_gc);
    try testing.expect(heap.stats.gc_debt_bytes <= heap.stats.gc_debt_threshold);
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

test "heap collectGarbage caches internal roots by signature" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var roots = [_]Value{};
    _ = try heap.collectGarbage(&roots);
    const sig0 = heap.gc_root_sig;
    const slot_len0 = heap.gc_internal_slots.items.len;
    try testing.expect(slot_len0 > 0);

    _ = try heap.collectGarbage(&roots);
    try testing.expectEqual(slot_len0, heap.gc_internal_slots.items.len);
    try testing.expectEqual(sig0.symbols_ver, heap.gc_root_sig.symbols_ver);
    try testing.expectEqual(sig0.pkg_symbols_ver, heap.gc_root_sig.pkg_symbols_ver);

    _ = try heap.intern("GC-CACHE-SIG-ROOT");
    _ = try heap.collectGarbage(&roots);
    try testing.expect(heap.gc_root_sig.pkg_symbols_ver > sig0.pkg_symbols_ver);
    try testing.expect(heap.gc_internal_slots.items.len >= slot_len0 + 1);
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

test "heap gc telemetry counters are monotonic" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
        },
    });
    defer heap.deinit();

    var roots = [_]Value{try heap.allocCons(Value.makeFixnum(3), Value.nil)};
    const c0 = heap.stats.gc_count;
    const m0 = heap.stats.gc_minor_count;
    const maj0 = heap.stats.gc_major_count;
    const b0 = heap.stats.gc_build_ns;
    const r0 = heap.stats.gc_root_ns;
    const cpy0 = heap.stats.gc_copy_ns;
    const f0 = heap.stats.gc_finalize_ns;
    const mn0 = heap.stats.gc_minor_ns;

    _ = try heap.collectGarbage(&roots);
    const c1 = heap.stats.gc_count;
    const m1 = heap.stats.gc_minor_count;
    const maj1 = heap.stats.gc_major_count;
    const b1 = heap.stats.gc_build_ns;
    const r1 = heap.stats.gc_root_ns;
    const cpy1 = heap.stats.gc_copy_ns;
    const f1 = heap.stats.gc_finalize_ns;
    const mn1 = heap.stats.gc_minor_ns;

    _ = try heap.collectGarbage(&roots);
    const c2 = heap.stats.gc_count;
    const m2 = heap.stats.gc_minor_count;
    const maj2 = heap.stats.gc_major_count;
    const b2 = heap.stats.gc_build_ns;
    const r2 = heap.stats.gc_root_ns;
    const cpy2 = heap.stats.gc_copy_ns;
    const f2 = heap.stats.gc_finalize_ns;
    const mn2 = heap.stats.gc_minor_ns;

    try testing.expectEqual(c0 + 1, c1);
    try testing.expectEqual(c1 + 1, c2);
    try testing.expectEqual(m0 + 1, m1);
    try testing.expectEqual(m1 + 1, m2);
    try testing.expectEqual(maj0, maj1);
    try testing.expectEqual(maj1, maj2);
    try testing.expectEqual(c2, m2 + maj2);
    try testing.expect(b1 >= b0 and b2 >= b1);
    try testing.expect(r1 >= r0 and r2 >= r1);
    try testing.expect(cpy1 >= cpy0 and cpy2 >= cpy1);
    try testing.expect(f1 >= f0 and f2 >= f1);
    try testing.expect(mn1 >= mn0 and mn2 >= mn1);
}

test "heap allocation sampling tracks classes and buckets" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    const sample_n0 = heap.stats.alloc_sample_n;
    const cons0 = heap.stats.alloc_sample_class[@intFromEnum(AllocClass.cons)];
    const size0 = heap.stats.alloc_sample_size;

    for (0..512) |_| {
        _ = try heap.allocCons(Value.makeFixnum(1), Value.nil);
    }

    const sample_n1 = heap.stats.alloc_sample_n;
    const cons1 = heap.stats.alloc_sample_class[@intFromEnum(AllocClass.cons)];
    const size1 = heap.stats.alloc_sample_size;

    const sample_delta = sample_n1 - sample_n0;
    const cons_delta = cons1 - cons0;
    try testing.expect(sample_delta > 0);
    try testing.expect(cons_delta > 0);

    var size_sum: usize = 0;
    for (size1, 0..) |n, i| {
        size_sum += n - size0[i];
    }
    try testing.expectEqual(sample_delta, size_sum);
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
