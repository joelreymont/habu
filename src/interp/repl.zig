//! Read-Eval-Print Loop for Habu
//!
//! Interactive REPL that ties together:
//! - Reader (parsing S-expressions)
//! - Compiler (S-expr → IR)
//! - Emitter (IR → bytecode)
//! - VM (bytecode execution)

const std = @import("std");
const reader = @import("../reader/reader.zig");
const Parser = reader.Parser;
const compiler = @import("../compiler/compiler.zig");
const Compiler = compiler.Compiler;
const Env = compiler.Env;
const ir = @import("../compiler/ir.zig");
const Ir = ir.Ir;
const IrBuilder = ir.IrBuilder;
const passes = @import("../compiler/passes/passes.zig");
const jit_backend = @import("../jit/backend_api.zig");
const jit_candidates = @import("../jit/candidates.zig");
const jit_literal_roots = @import("../jit/literal_roots.zig");
const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;
const Op = bytecode.Op;
const disasm = bytecode.disasm;
const vm_mod = @import("vm.zig");
const Vm = vm_mod.Vm;
const runtime = @import("../runtime/runtime.zig");
const qual_name = @import("../runtime/qual_name.zig");
const primitives = @import("../runtime/primitives/primitives.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const String = runtime.String;
const diagnostic = @import("../diagnostic.zig");
const lineedit = @import("lineedit.zig");
const LineEditor = lineedit.LineEditor;

/// Patch make_closure indices in a chunk to be absolute
fn patchChunkIndices(chunk: *runtime.objects.Chunk, base: u16) !void {
    const code = chunk.getCode();
    var i: usize = 0;
    while (i < code.len) {
        const insn = try bytecode.opcodes.decodeInstruction(code, i);
        if (insn.op == .make_closure) {
            const rel_idx = std.mem.readInt(u16, code[insn.operand_off..][0..2], .little);
            const abs_idx = try std.math.add(u16, rel_idx, base);
            std.mem.writeInt(u16, code[insn.operand_off..][0..2], abs_idx, .little);
        }
        i = insn.next_off;
    }
}

pub const ReplError = anyerror;
const load_form_root_name = "__HABU_INTERNAL_LOAD_FORM_ROOT__";
const load_form_root_stack_name = "__HABU_INTERNAL_LOAD_FORM_STACK__";
const load_form_root_tmp_name = "__HABU_INTERNAL_LOAD_FORM_TMP__";

/// REPL configuration
pub const Config = struct {
    /// Show disassembly before execution
    show_disasm: bool = false,
    /// Show bytecode bytes
    show_bytes: bool = false,
    /// Allow hoist JIT compilation for eligible lambdas.
    /// Internal/benchmark option: disabling keeps the same backend while
    /// forcing interpreter-only execution.
    enable_jit: bool = true,
    /// Prompt string
    prompt: []const u8 = "🐍 ",
    /// Continuation prompt (for multi-line input)
    cont_prompt: []const u8 = "   ",
};

const MacroEntry = struct {
    closure: Value,
    has_whole: bool,
    has_env: bool,
};

const VmRootCtx = struct {
    vm: *Vm,
    roots: *std.ArrayList(Value),
    prev: ?*VmRootCtx,
};

const CallOpCounts = struct {
    blr: usize = 0,
    bl: usize = 0,
};

fn countCallOps(code: []const u8) CallOpCounts {
    var counts = CallOpCounts{};
    var i: usize = 0;
    while (i + 4 <= code.len) : (i += 4) {
        const insn = std.mem.readInt(u32, code[i..][0..4], .little);
        if (insn & 0xFFFFFC1F == 0xD63F0000) counts.blr += 1;
        if (insn & 0xFC000000 == 0x94000000) counts.bl += 1;
    }
    return counts;
}

/// REPL state
pub const Repl = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: Vm,
    config: Config,
    /// Persistent compiler for global definitions
    compiler: Compiler,
    /// Persistent chunk pool for closures (boxed chunk roots)
    chunk_pool: std.ArrayList(Value),
    /// Macro definitions: symbol -> closure
    macros: std.AutoHashMap(Value, MacroEntry),
    /// GC roots that must survive across VM runs (macro definitions, etc).
    persistent_roots: std.ArrayList(Value),
    trusted_load_root: []u8,
    /// Line editor for interactive input
    line_editor: LineEditor,
    /// Current VM being used (for nested loads)
    current_vm: ?*Vm,
    /// Linked stack of active VM root contexts for nested eval/load.
    active_root_ctx: ?*VmRootCtx,
    /// Heap GC count when macro maps were last key-refreshed/rebuilt.
    macro_gc_synced: usize,
    /// Cached trace toggle for function resolver hot path.
    trace_fn_resolve: bool,

    fn traceFormIndexFromEnv() ?usize {
        const raw_c = std.posix.getenv("HABU_TRACE_FORM_INDEX") orelse return null;
        const raw: []const u8 = raw_c;
        if (raw.len == 0) return null;
        return std.fmt.parseInt(usize, raw, 10) catch null;
    }

    pub fn init(self: *Repl, allocator: std.mem.Allocator, heap: *Heap, config: Config) !void {
        // NOTE: Repl must be initialized in-place so Compiler subcomponents can
        // safely keep pointers into vm (builtins, etc) without a move.
        var repl_cfg = config;
        if (std.posix.getenv("HABU_DISABLE_JIT") != null) {
            repl_cfg.enable_jit = false;
        }
        self.* = .{
            .allocator = allocator,
            .heap = heap,
            .vm = undefined,
            .config = repl_cfg,
            .compiler = undefined,
            .chunk_pool = std.ArrayList(Value){},
            .macros = std.AutoHashMap(Value, MacroEntry).init(allocator),
            .persistent_roots = std.ArrayList(Value){},
            .trusted_load_root = undefined,
            .line_editor = LineEditor.init(allocator),
            .current_vm = null,
            .active_root_ctx = null,
            .macro_gc_synced = 0,
            .trace_fn_resolve = std.posix.getenv("HABU_TRACE_FN_RESOLVE") != null,
        };
        errdefer self.chunk_pool.deinit(allocator);
        errdefer self.macros.deinit();
        errdefer self.persistent_roots.deinit(allocator);
        errdefer allocator.free(self.trusted_load_root);
        errdefer self.line_editor.deinit();

        self.trusted_load_root = try std.process.getCwdAlloc(allocator);
        self.vm = try Vm.init(allocator, heap);
        errdefer self.vm.deinit();
        self.vm.setChunkPoolOwned(&self.chunk_pool);
        // Keep ext_roots backing storage stable while compiler/eval temporarily swap
        // vm.ext_roots to local arrays. Macro definitions can append roots during
        // those windows; reserving avoids dangling saved slices on restore.
        try self.persistent_roots.ensureTotalCapacity(allocator, 65_536);
        self.vm.setExtRootsOwned(&self.persistent_roots);
        self.macro_gc_synced = self.heap.stats.gc_count;

        self.compiler = try Compiler.initWithHeap(allocator, &self.vm);
        errdefer self.compiler.deinit();
    }

    fn isVmActive(self: *Repl, vm: *Vm) bool {
        if (vm == &self.vm) return true;
        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            if (ctx.vm == vm) return true;
            ctx_opt = ctx.prev;
        }
        return false;
    }

    fn activeVm(self: *Repl) *Vm {
        if (self.current_vm) |vm| {
            if (self.isVmActive(vm)) return vm;
        }
        if (self.active_root_ctx) |ctx| {
            return ctx.vm;
        }
        return &self.vm;
    }

    fn syncChunkPools(self: *Repl, vm: *Vm) void {
        const owner = &self.chunk_pool;
        // Keep the main VM in sync even when evaluating via nested VMs.
        self.vm.setChunkPoolOwned(owner);
        if (vm != &self.vm) {
            vm.setChunkPoolOwned(owner);
        }
        // If we are inside a nested load/eval, ensure that VM also sees the new slice.
        if (self.current_vm) |cur| {
            if (cur != &self.vm and cur != vm) {
                cur.setChunkPoolOwned(owner);
            }
        }
        // Keep all active VMs in nested runVmPreserveMacroState chains in sync too.
        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            if (ctx.vm != &self.vm and ctx.vm != vm) {
                ctx.vm.setChunkPoolOwned(owner);
            }
            ctx_opt = ctx.prev;
        }
    }

    fn appendChildChunksAndSync(self: *Repl, vm: *Vm, child_chunks: []const Value) !void {
        try self.chunk_pool.ensureUnusedCapacity(self.allocator, child_chunks.len);
        for (child_chunks) |child_chunk| {
            self.chunk_pool.appendAssumeCapacity(child_chunk);
        }
        self.syncChunkPools(vm);

        if (std.posix.getenv("HABU_TRAP_CHUNK_POOL_SANITY") != null) {
            var i: usize = 0;
            while (i < self.chunk_pool.items.len) : (i += 1) {
                const live = vm.resolveForwardedValue(self.chunk_pool.items[i]);
                self.chunk_pool.items[i] = live;
                if (live.isNil() or live.isChunk()) continue;
                std.debug.print(
                    "TRACE chunk-pool-corrupt idx={d} raw=0x{x} kind={s}\n",
                    .{ i, live.raw, @tagName(live.typeKind()) },
                );
                @panic("chunk pool contains non-chunk value");
            }
        }
    }

    fn appendToVmExtRootOwner(self: *Repl, vm: *Vm, vals: []const Value) !void {
        const owner = vm.ext_roots_owner orelse return;
        if (owner == &self.persistent_roots) return;

        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            if (ctx.vm == vm and ctx.roots == owner) {
                vm.setExtRootsOwned(owner);
                return;
            }
            ctx_opt = ctx.prev;
        }

        try owner.appendSlice(self.allocator, vals);
        vm.setExtRootsOwned(owner);
    }

    fn upsertRootPair(roots: *std.ArrayList(Value), key: Value, val: Value, allocator: std.mem.Allocator) !void {
        var i = roots.items.len;
        while (i >= 2) {
            i -= 2;
            const existing_key = roots.items[i];
            const existing_val = roots.items[i + 1];
            const same_kind = existing_val.isClosure() == val.isClosure();
            if (same_kind and existing_key.eq(key)) {
                roots.items[i] = key;
                roots.items[i + 1] = val;
                return;
            }
        }
        try roots.append(allocator, key);
        try roots.append(allocator, val);
    }

    fn upsertVmExtRootOwnerPair(self: *Repl, vm: *Vm, key: Value, val: Value) !void {
        const owner = vm.ext_roots_owner orelse return;
        if (owner == &self.persistent_roots) return;

        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            if (ctx.vm == vm and ctx.roots == owner) {
                vm.setExtRootsOwned(owner);
                return;
            }
            ctx_opt = ctx.prev;
        }

        try upsertRootPair(owner, key, val, self.allocator);
        vm.setExtRootsOwned(owner);
    }

    fn pinPersistentRoot(self: *Repl, val: Value) !void {
        const vm = self.activeVm();
        try self.persistent_roots.append(self.allocator, val);
        if (self.active_root_ctx == null) {
            const ext = self.vm.currentExtRoots();
            if (ext.len == 0 or (ext.len <= self.persistent_roots.items.len and ext.ptr == self.persistent_roots.items.ptr)) {
                self.vm.setExtRootsOwned(&self.persistent_roots);
            }
        }

        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            try ctx.roots.append(self.allocator, val);
            if (ctx.vm.ext_roots_owner == ctx.roots) {
                ctx.vm.setExtRootsOwned(ctx.roots);
            }
            ctx_opt = ctx.prev;
        }

        const val_buf = [_]Value{val};
        try self.appendToVmExtRootOwner(vm, &val_buf);
    }

    fn pinPersistentPair(self: *Repl, key: Value, val: Value) !void {
        const vm = self.activeVm();
        const key_live = vm.resolveForwardedValue(key);
        const val_live = vm.resolveForwardedValue(val);
        if (val_live.isClosure()) {
            const cls = val_live.toPtr(runtime.Closure);
            const max_caps = self.heap.space_size / @sizeOf(Value);
            if (cls.num_captures > max_caps) {
                std.debug.print(
                    "TRACE bad-persistent-closure key=0x{x} val=0x{x} captures={d} max={d}\n",
                    .{ key_live.raw, val_live.raw, cls.num_captures, max_caps },
                );
                @panic("invalid closure persisted");
            }
        }

        var i = self.persistent_roots.items.len;
        while (i >= 2) {
            i -= 2;
            const existing_key = self.persistent_roots.items[i];
            const existing_val = self.persistent_roots.items[i + 1];
            const same_kind = existing_val.isClosure() == val_live.isClosure();
            if (same_kind and existing_key.eq(key_live)) {
                self.persistent_roots.items[i] = key_live;
                self.persistent_roots.items[i + 1] = val_live;
                if (self.active_root_ctx == null) {
                    const ext = self.vm.currentExtRoots();
                    if (ext.len == 0 or (ext.len <= self.persistent_roots.items.len and ext.ptr == self.persistent_roots.items.ptr)) {
                        self.vm.setExtRootsOwned(&self.persistent_roots);
                    }
                }
                var update_ctx = self.active_root_ctx;
                while (update_ctx) |ctx| {
                    try upsertRootPair(ctx.roots, key_live, val_live, self.allocator);
                    if (ctx.vm.ext_roots_owner == ctx.roots) {
                        ctx.vm.setExtRootsOwned(ctx.roots);
                    }
                    update_ctx = ctx.prev;
                }
                try self.upsertVmExtRootOwnerPair(vm, key_live, val_live);
                if (std.posix.getenv("HABU_TRACE_PERSISTENT_ROOTS") != null) {
                    const owner_len = if (vm.ext_roots_owner) |owner| owner.items.len else 0;
                    std.debug.print(
                        "TRACE persistent-roots update total={d} owner-len={d} key=0x{x} closure={any}\n",
                        .{ self.persistent_roots.items.len, owner_len, key_live.raw, val_live.isClosure() },
                    );
                }
                return;
            }
        }

        try self.persistent_roots.append(self.allocator, key_live);
        try self.persistent_roots.append(self.allocator, val_live);
        if (self.active_root_ctx == null) {
            const ext = self.vm.currentExtRoots();
            if (ext.len == 0 or (ext.len <= self.persistent_roots.items.len and ext.ptr == self.persistent_roots.items.ptr)) {
                self.vm.setExtRootsOwned(&self.persistent_roots);
            }
        }

        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            try upsertRootPair(ctx.roots, key_live, val_live, self.allocator);
            if (ctx.vm.ext_roots_owner == ctx.roots) {
                ctx.vm.setExtRootsOwned(ctx.roots);
            }
            ctx_opt = ctx.prev;
        }
        try self.upsertVmExtRootOwnerPair(vm, key_live, val_live);
        if (std.posix.getenv("HABU_TRACE_PERSISTENT_ROOTS") != null) {
            const owner_len = if (vm.ext_roots_owner) |owner| owner.items.len else 0;
            std.debug.print(
                "TRACE persistent-roots add total={d} owner-len={d} key=0x{x} closure={any}\n",
                .{ self.persistent_roots.items.len, owner_len, key_live.raw, val_live.isClosure() },
            );
        }
    }

    const MacroMapPair = struct {
        key: Value,
        val: Value,
    };

    const ReplMacroPair = struct {
        key: Value,
        closure: Value,
        has_whole: bool,
        has_env: bool,
    };

    fn isLiveValue(self: *Repl, val: Value) bool {
        if (!val.isPointer()) return true;
        const addr = val.toPtrAddr();
        const start = @intFromPtr(self.heap.from_start);
        const end = @intFromPtr(self.heap.from_end);
        if (addr < start or addr >= end) return false;
        if ((addr & (runtime.heap.ALIGNMENT - 1)) != 0) return false;

        const align8 = struct {
            fn run(n: usize) ?usize {
                if (n > std.math.maxInt(usize) - 7) return null;
                const plus = n + 7;
                return plus & ~@as(usize, 7);
            }
        }.run;
        const within = struct {
            fn run(base: usize, size: usize, limit: usize) bool {
                const obj_end = std.math.add(usize, base, size) catch return false;
                return obj_end <= limit;
            }
        }.run;
        const mul = struct {
            fn run(a: anytype, b: usize) ?usize {
                return std.math.mul(usize, @as(usize, @intCast(a)), b) catch null;
            }
        }.run;

        switch (val.getTag()) {
            .cons => return within(addr, @sizeOf(runtime.Cons), end),
            .symbol => {
                if (!within(addr, @sizeOf(Symbol), end)) return false;
                const sym = val.toPtr(Symbol);
                const name_len: usize = @intCast(sym.name_len);
                const aligned_name = align8(name_len) orelse return false;
                const data_start = std.math.add(usize, addr, @sizeOf(Symbol)) catch return false;
                const data_end = std.math.add(usize, data_start, aligned_name) catch return false;
                if (data_end > end) return false;
                const name_ptr = @intFromPtr(sym.name_ptr);
                const name_end = std.math.add(usize, name_ptr, name_len) catch return false;
                return name_ptr >= data_start and name_end <= data_end;
            },
            .vector => {
                if (!within(addr, @sizeOf(runtime.Vector), end)) return false;
                const vec = val.toPtr(runtime.Vector);
                if (vec.length > vec.capacity) return false;
                const data_size = mul(vec.capacity, @sizeOf(Value)) orelse return false;
                const data_start = std.math.add(usize, addr, @sizeOf(runtime.Vector)) catch return false;
                const data_end = std.math.add(usize, data_start, data_size) catch return false;
                if (data_end > end) return false;
                return @intFromPtr(vec.data) == data_start;
            },
            .string => {
                if (!within(addr, @sizeOf(String), end)) return false;
                const str = val.toPtr(String);
                const data_size = align8(str.length) orelse return false;
                const data_start = std.math.add(usize, addr, @sizeOf(String)) catch return false;
                const data_end = std.math.add(usize, data_start, data_size) catch return false;
                if (data_end > end) return false;
                return @intFromPtr(str.data) == data_start;
            },
            .closure => {
                if (!within(addr, @sizeOf(runtime.Closure), end)) return false;
                const cls = val.toPtr(runtime.Closure);
                const cap_size = mul(cls.num_captures, @sizeOf(Value)) orelse return false;
                const cap_start = std.math.add(usize, addr, @sizeOf(runtime.Closure)) catch return false;
                const cap_end = std.math.add(usize, cap_start, cap_size) catch return false;
                if (cap_end > end) return false;
                return @intFromPtr(cls.captures) == cap_start;
            },
            .keyword => {
                if (!within(addr, @sizeOf(runtime.Keyword), end)) return false;
                const kw = val.toPtr(runtime.Keyword);
                const name_len: usize = @intCast(kw.name_len);
                const aligned_name = align8(name_len) orelse return false;
                const data_start = std.math.add(usize, addr, @sizeOf(runtime.Keyword)) catch return false;
                const data_end = std.math.add(usize, data_start, aligned_name) catch return false;
                if (data_end > end) return false;
                const name_ptr = @intFromPtr(kw.name_ptr);
                const name_end = std.math.add(usize, name_ptr, name_len) catch return false;
                return name_ptr >= data_start and name_end <= data_end;
            },
            .boxed => {
                if (!within(addr, @sizeOf(u64), end)) return false;
                const kind_raw = @as(*const u64, @ptrFromInt(addr)).*;
                if (kind_raw > @intFromEnum(runtime.BoxedKind.macro_env)) return false;
                const kind: runtime.BoxedKind = @enumFromInt(@as(u64, kind_raw));
                return switch (kind) {
                    .hashtable => within(addr, @sizeOf(runtime.HashTable), end),
                    .rational => within(addr, @sizeOf(runtime.Rational), end),
                    .complex => within(addr, @sizeOf(runtime.Complex), end),
                    .stream => within(addr, @sizeOf(runtime.Stream), end),
                    .bignum => within(addr, @sizeOf(runtime.Bignum), end),
                    .array => blk: {
                        if (!within(addr, @sizeOf(runtime.Array), end)) break :blk false;
                        const arr = val.toPtr(runtime.Array);
                        if (arr.rank == 0 or arr.rank > 8) break :blk false;
                        var expected_total: u64 = 1;
                        var i: usize = 0;
                        while (i < arr.rank) : (i += 1) {
                            expected_total = std.math.mul(u64, expected_total, arr.dimensions[i]) catch break :blk false;
                        }
                        if (arr.total_size != expected_total) break :blk false;
                        const data_size = std.math.mul(usize, @intCast(arr.total_size), @sizeOf(Value)) catch break :blk false;
                        const data_start = std.math.add(usize, addr, @sizeOf(runtime.Array)) catch break :blk false;
                        const data_end = std.math.add(usize, data_start, data_size) catch break :blk false;
                        if (data_end > end) break :blk false;
                        break :blk arr.data_ptr == data_start;
                    },
                    .pathname => within(addr, @sizeOf(runtime.Pathname), end),
                    .package => within(addr, @sizeOf(runtime.Package), end),
                    .chunk => blk: {
                        if (!within(addr, @sizeOf(runtime.Chunk), end)) break :blk false;
                        const chunk = val.toPtr(runtime.Chunk);
                        const const_size = std.math.mul(usize, chunk.const_count, @sizeOf(Value)) catch break :blk false;
                        const code_size = align8(chunk.code_len) orelse break :blk false;
                        const header_end = std.math.add(usize, addr, @sizeOf(runtime.Chunk)) catch break :blk false;
                        const code_start = std.math.add(usize, header_end, const_size) catch break :blk false;
                        const obj_end = std.math.add(usize, code_start, code_size) catch break :blk false;
                        if (obj_end > end) break :blk false;
                        if (@intFromPtr(chunk.const_pool) != header_end) break :blk false;
                        break :blk @intFromPtr(chunk.code) == code_start;
                    },
                    .condition => within(addr, @sizeOf(runtime.objects.Condition), end),
                    .class => within(addr, @sizeOf(runtime.Class), end),
                    .string32 => blk: {
                        if (!within(addr, @sizeOf(runtime.String32), end)) break :blk false;
                        const s32 = val.toPtr(runtime.String32);
                        const byte_size = std.math.mul(usize, s32.length, @sizeOf(u32)) catch break :blk false;
                        const data_size = align8(byte_size) orelse break :blk false;
                        const data_start = std.math.add(usize, addr, @sizeOf(runtime.String32)) catch break :blk false;
                        const data_end = std.math.add(usize, data_start, data_size) catch break :blk false;
                        if (data_end > end) break :blk false;
                        break :blk @intFromPtr(s32.data) == data_start;
                    },
                    .slotdef => within(addr, @sizeOf(runtime.objects.SlotDefinition), end),
                    .generic_function => within(addr, @sizeOf(runtime.objects.GenericFunction), end),
                    .method => within(addr, @sizeOf(runtime.objects.Method), end),
                    .native_code => within(addr, @sizeOf(runtime.NativeCode), end),
                    .structure => blk: {
                        if (!within(addr, @sizeOf(runtime.objects.Structure), end)) break :blk false;
                        const obj = val.toPtr(runtime.objects.Structure);
                        const data_size = std.math.mul(usize, @intCast(obj.length), @sizeOf(Value)) catch break :blk false;
                        const data_start = std.math.add(usize, addr, @sizeOf(runtime.objects.Structure)) catch break :blk false;
                        const data_end = std.math.add(usize, data_start, data_size) catch break :blk false;
                        if (data_end > end) break :blk false;
                        break :blk @intFromPtr(obj.slots) == data_start;
                    },
                    .macro_env => within(addr, @sizeOf(runtime.MacroEnv), end),
                };
            },
            .forwarding => return false,
        }
    }

    fn collectCompilerMacroPairs(self: *Repl, out: *std.ArrayList(MacroMapPair)) !void {
        var it = self.compiler.macro_table.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const val = entry.value_ptr.*;
            if (!key.isSymbol()) continue;
            if (!self.isLiveValue(key) or !self.isLiveValue(val)) continue;
            if (val.isClosure()) {
                const closure = val.toPtr(runtime.Closure);
                if (!closure.code.isChunk()) continue;
                if (!self.isLiveValue(closure.code)) continue;
            }
            try out.append(self.allocator, .{
                .key = key,
                .val = val,
            });
        }
    }

    fn collectSymbolMacroPairs(self: *Repl, out: *std.ArrayList(MacroMapPair)) !void {
        var it = self.compiler.symbol_macros.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const val = entry.value_ptr.*;
            if (!key.isSymbol()) continue;
            if (!self.isLiveValue(key) or !self.isLiveValue(val)) continue;
            try out.append(self.allocator, .{
                .key = key,
                .val = val,
            });
        }
    }

    /// Resolve a potentially-stale symbol Value to its current live address.
    /// After GC, symbols may have been moved and the old location has a
    /// forwarding pointer. We follow one level of forwarding, then look up
    /// the symbol by name in its home package (which IS updated by GC).
    fn resolveStaleSymbol(self: *Repl, sym_val: Value) Value {
        if (!sym_val.isSymbol()) return sym_val;
        if (self.isLiveValue(sym_val)) return sym_val; // Not stale

        // The symbol was moved by GC. The old location may have a forwarding
        // pointer. Follow it to find the live copy.
        const addr = sym_val.toPtrAddr();
        if (addr == 0) return sym_val;
        const stale_start = @intFromPtr(self.heap.to_start);
        const stale_end = stale_start + self.heap.space_size;
        if (addr < stale_start or addr >= stale_end) return sym_val;
        const first_word: *const Value = @ptrFromInt(addr);
        if (!first_word.isForwarding()) return sym_val;
        const new_addr = first_word.toPtrAddr();
        const forwarded_size_ptr: *const usize = @ptrFromInt(addr + @sizeOf(Value));
        const forwarded_size = forwarded_size_ptr.*;
        const forwarded_size_ok = forwarded_size > 0 and
            forwarded_size <= self.heap.space_size and
            std.mem.isAligned(forwarded_size, @import("../runtime/heap.zig").ALIGNMENT);
        const from_start = @intFromPtr(self.heap.from_start);
        const from_end = @intFromPtr(self.heap.from_end);
        const in_from = new_addr >= from_start and new_addr < from_end and forwarded_size <= from_end - new_addr;
        var in_tenured = false;
        if (self.heap.gcLayoutMode() == .generational) {
            if (self.heap.tenuredRegion()) |tenured| {
                const ten_start = @intFromPtr(tenured.start);
                const ten_used_end = if (self.heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                in_tenured = new_addr >= ten_start and new_addr < ten_used_end and forwarded_size <= ten_used_end - new_addr;
            }
        }
        if (!forwarded_size_ok or !(in_from or in_tenured)) return sym_val;
        const tag = sym_val.getTag();
        return .{ .raw = new_addr | @as(u64, @intFromEnum(tag)) };
        // No forwarding pointer — can't resolve (multi-GC scenario)
    }

    /// Refresh all macro map keys to their current GC-safe addresses.
    /// After VM execution (which may trigger GC), symbol addresses in our
    /// Zig-managed hash maps become stale. We resolve stale keys through
    /// forwarding pointers left by the most recent GC.
    fn refreshMacroKeys(self: *Repl) !void {
        // Refresh REPL macros
        {
            var updates = std.ArrayList(struct { old: Value, new: Value, entry: MacroEntry }){};
            defer updates.deinit(self.allocator);
            var it = self.macros.iterator();
            while (it.next()) |entry| {
                const key = entry.key_ptr.*;
                if (!key.isSymbol()) continue;
                const current = self.resolveStaleSymbol(key);
                if (current.raw != key.raw) {
                    try updates.append(self.allocator, .{
                        .old = key,
                        .new = current,
                        .entry = entry.value_ptr.*,
                    });
                }
            }
            for (updates.items) |upd| {
                _ = self.macros.remove(upd.old);
                try self.macros.put(upd.new, upd.entry);
            }
        }
        // Refresh compiler macros
        {
            var updates = std.ArrayList(struct { old: Value, new: Value, val: Value }){};
            defer updates.deinit(self.allocator);
            var it = self.compiler.macro_table.iterator();
            while (it.next()) |entry| {
                const key = entry.key_ptr.*;
                if (!key.isSymbol()) continue;
                const current = self.resolveStaleSymbol(key);
                if (current.raw != key.raw) {
                    try updates.append(self.allocator, .{
                        .old = key,
                        .new = current,
                        .val = entry.value_ptr.*,
                    });
                }
            }
            for (updates.items) |upd| {
                _ = self.compiler.macro_table.remove(upd.old);
                try self.compiler.macro_table.put(upd.new, upd.val);
            }
        }
        // Refresh symbol macros
        {
            var updates = std.ArrayList(struct { old: Value, new: Value, val: Value }){};
            defer updates.deinit(self.allocator);
            var it = self.compiler.symbol_macros.iterator();
            while (it.next()) |entry| {
                const key = entry.key_ptr.*;
                if (!key.isSymbol()) continue;
                const current = self.resolveStaleSymbol(key);
                if (current.raw != key.raw) {
                    try updates.append(self.allocator, .{
                        .old = key,
                        .new = current,
                        .val = entry.value_ptr.*,
                    });
                }
            }
            for (updates.items) |upd| {
                _ = self.compiler.symbol_macros.remove(upd.old);
                try self.compiler.symbol_macros.put(upd.new, upd.val);
            }
        }
    }

    fn collectReplMacroPairs(self: *Repl, out: *std.ArrayList(ReplMacroPair)) !void {
        var it = self.macros.iterator();
        while (it.next()) |entry| {
            const key = entry.key_ptr.*;
            const m = entry.value_ptr.*;
            if (!key.isSymbol()) continue;
            if (!self.isLiveValue(key) or !self.isLiveValue(m.closure)) continue;
            if (!m.closure.isClosure()) continue;
            const closure = m.closure.toPtr(runtime.Closure);
            if (!closure.code.isChunk()) continue;
            if (!self.isLiveValue(closure.code)) continue;
            try out.append(self.allocator, .{
                .key = key,
                .closure = m.closure,
                .has_whole = m.has_whole,
                .has_env = m.has_env,
            });
        }
    }

    fn restoreMacroMapsFromRoots(
        self: *Repl,
        roots: []const Value,
        compiler_macro_start: usize,
        compiler_macro_count: usize,
        symbol_macro_start: usize,
        symbol_macro_count: usize,
        repl_macro_start: usize,
        repl_macros: []const ReplMacroPair,
        live_compiler_macros: []const MacroMapPair,
        live_symbol_macros: []const MacroMapPair,
        live_repl_macros: []const ReplMacroPair,
    ) !void {
        self.compiler.macro_table.clearRetainingCapacity();
        var i: usize = 0;
        while (i < compiler_macro_count) : (i += 1) {
            const base = compiler_macro_start + (i * 2);
            const key = roots[base];
            const val = roots[base + 1];
            if (!key.isSymbol()) continue;
            if (!self.isLiveValue(key) or !self.isLiveValue(val)) continue;
            try self.compiler.macro_table.put(key, val);
        }
        for (live_compiler_macros) |pair| {
            // Keys already resolved by collectCompilerMacroPairs
            const gop = try self.compiler.macro_table.getOrPut(pair.key);
            if (!gop.found_existing) {
                gop.value_ptr.* = pair.val;
            }
        }

        self.compiler.symbol_macros.clearRetainingCapacity();
        i = 0;
        while (i < symbol_macro_count) : (i += 1) {
            const base = symbol_macro_start + (i * 2);
            const key = roots[base];
            const val = roots[base + 1];
            if (!key.isSymbol()) continue;
            if (!self.isLiveValue(key) or !self.isLiveValue(val)) continue;
            try self.compiler.symbol_macros.put(key, val);
        }
        for (live_symbol_macros) |pair| {
            // Keys already resolved by collectSymbolMacroPairs
            const gop = try self.compiler.symbol_macros.getOrPut(pair.key);
            if (!gop.found_existing) {
                gop.value_ptr.* = pair.val;
            }
        }

        self.macros.clearRetainingCapacity();
        i = 0;
        while (i < repl_macros.len) : (i += 1) {
            const base = repl_macro_start + (i * 2);
            const pair = repl_macros[i];
            const key = roots[base];
            const closure = roots[base + 1];
            if (!key.isSymbol()) continue;
            if (!self.isLiveValue(key) or !self.isLiveValue(closure)) continue;
            try self.macros.put(key, .{
                .closure = closure,
                .has_whole = pair.has_whole,
                .has_env = pair.has_env,
            });
        }
        for (live_repl_macros) |pair| {
            // Keys already resolved by collectReplMacroPairs
            const gop = try self.macros.getOrPut(pair.key);
            if (!gop.found_existing) {
                gop.value_ptr.* = .{
                    .closure = pair.closure,
                    .has_whole = pair.has_whole,
                    .has_env = pair.has_env,
                };
            }
        }
    }

    fn decodeCompilerMacroEntry(self: *Repl, value_raw: Value) ?MacroEntry {
        const value = self.vm.resolveForwardedValue(value_raw);
        if (!value.isCons()) return null;
        if (!self.isLiveValue(value)) return null;
        const c0 = value.toPtr(Cons);
        const closure = self.vm.resolveForwardedValue(c0.car);
        if (!closure.isClosure()) return null;
        const cdr0 = self.vm.resolveForwardedValue(c0.cdr);
        if (!cdr0.isCons() or !self.isLiveValue(cdr0)) return .{
            .closure = closure,
            .has_whole = false,
            .has_env = false,
        };
        const c1 = cdr0.toPtr(Cons);
        const flags_val = self.vm.resolveForwardedValue(c1.car);
        const flags: i64 = if (flags_val.isFixnum()) flags_val.toFixnum() else 0;
        if (flags < 0 or flags > 3) return null;
        return .{
            .closure = closure,
            .has_whole = (flags & 1) != 0,
            .has_env = (flags & 2) != 0,
        };
    }

    fn rebuildMacroMapsFromPersistentRoots(self: *Repl) !void {
        self.compiler.macro_table.clearRetainingCapacity();
        self.macros.clearRetainingCapacity();

        var i: usize = 0;
        while (i + 1 < self.persistent_roots.items.len) : (i += 2) {
            const key_raw = self.persistent_roots.items[i];
            const val_raw = self.persistent_roots.items[i + 1];
            const key = self.vm.resolveForwardedValue(key_raw);
            const val = self.vm.resolveForwardedValue(val_raw);

            self.persistent_roots.items[i] = key;
            self.persistent_roots.items[i + 1] = val;

            if (!key.isSymbol()) continue;

            if (self.decodeCompilerMacroEntry(val)) |entry| {
                try self.compiler.macro_table.put(key, val);
                try self.macros.put(key, entry);
                continue;
            }

            if (val.isClosure() and self.macros.get(key) == null) {
                try self.macros.put(key, .{
                    .closure = val,
                    .has_whole = false,
                    .has_env = false,
                });
            }
        }

        self.vm.setExtRootsOwned(&self.persistent_roots);
    }

    fn syncMacroMapsIfGcChanged(self: *Repl) !void {
        const gc_now = self.heap.stats.gc_count;
        if (gc_now == self.macro_gc_synced) return;
        try self.rebuildMacroMapsFromPersistentRoots();
        self.macro_gc_synced = gc_now;
    }

    fn runVmPreserveMacroState(self: *Repl, vm: *Vm, chunk_ptr: *runtime.objects.Chunk) !Value {
        try self.syncMacroMapsIfGcChanged();
        const gc_before = self.heap.stats.gc_count;
        const trace_vm_root_counts = std.posix.getenv("HABU_TRACE_VM_ROOT_COUNTS") != null;

        const saved_ext = try vm.saveExtRoots();
        const saved_ext_roots = saved_ext.roots;
        if (saved_ext_roots.len != 0) {
            for (saved_ext_roots) |*val| {
                val.* = vm.resolveForwardedValue(val.*);
            }
        }

        var compiler_macros = std.ArrayList(MacroMapPair){};
        defer compiler_macros.deinit(self.allocator);
        try self.collectCompilerMacroPairs(&compiler_macros);

        var symbol_macros = std.ArrayList(MacroMapPair){};
        defer symbol_macros.deinit(self.allocator);
        try self.collectSymbolMacroPairs(&symbol_macros);

        var repl_macros = std.ArrayList(ReplMacroPair){};
        defer repl_macros.deinit(self.allocator);
        try self.collectReplMacroPairs(&repl_macros);

        const total_roots = saved_ext_roots.len +
            (compiler_macros.items.len * 2) +
            (symbol_macros.items.len * 2) +
            (repl_macros.items.len * 2);
        if (trace_vm_root_counts) {
            std.debug.print(
                "TRACE vm-roots saved={d} compiler={d} symbol={d} repl={d} total={d} gc={d}\n",
                .{
                    saved_ext_roots.len,
                    compiler_macros.items.len,
                    symbol_macros.items.len,
                    repl_macros.items.len,
                    total_roots,
                    gc_before,
                },
            );
        }
        var roots = std.ArrayList(Value){};
        defer roots.deinit(self.allocator);
        try roots.ensureTotalCapacity(self.allocator, total_roots);

        if (saved_ext_roots.len > 0) {
            try roots.appendSlice(self.allocator, saved_ext_roots);
        }

        const compiler_macro_start = roots.items.len;
        for (compiler_macros.items) |entry| {
            try roots.append(self.allocator, entry.key);
            try roots.append(self.allocator, entry.val);
        }

        const symbol_macro_start = roots.items.len;
        for (symbol_macros.items) |entry| {
            try roots.append(self.allocator, entry.key);
            try roots.append(self.allocator, entry.val);
        }

        const repl_macro_start = roots.items.len;
        for (repl_macros.items) |entry| {
            try roots.append(self.allocator, entry.key);
            try roots.append(self.allocator, entry.closure);
        }

        vm.setExtRootsOwned(&roots);
        defer vm.restoreExtRootsSynced(saved_ext, roots.items, saved_ext_roots.len);
        var root_ctx = VmRootCtx{
            .vm = vm,
            .roots = &roots,
            .prev = self.active_root_ctx,
        };
        self.active_root_ctx = &root_ctx;
        defer self.active_root_ctx = root_ctx.prev;

        var live_compiler_macros = std.ArrayList(MacroMapPair){};
        defer live_compiler_macros.deinit(self.allocator);
        var live_symbol_macros = std.ArrayList(MacroMapPair){};
        defer live_symbol_macros.deinit(self.allocator);
        var live_repl_macros = std.ArrayList(ReplMacroPair){};
        defer live_repl_macros.deinit(self.allocator);

        const result = if (vm.isExecuting()) blk: {
            const chunk_val = Value.makeChunk(chunk_ptr);
            const closure = try vm.allocClosureWithGC(chunk_val, chunk_ptr.arity, &[_]Value{});
            const call_base = vm.sp;
            break :blk vm.callFromStackAt(call_base, closure, &[_]Value{}) catch |run_err| {
                const gc_after = self.heap.stats.gc_count;
                if (gc_after != gc_before) {
                    try self.refreshMacroKeys();
                    try self.collectCompilerMacroPairs(&live_compiler_macros);
                    try self.collectSymbolMacroPairs(&live_symbol_macros);
                    try self.collectReplMacroPairs(&live_repl_macros);
                    try self.restoreMacroMapsFromRoots(
                        roots.items,
                        compiler_macro_start,
                        compiler_macros.items.len,
                        symbol_macro_start,
                        symbol_macros.items.len,
                        repl_macro_start,
                        repl_macros.items,
                        live_compiler_macros.items,
                        live_symbol_macros.items,
                        live_repl_macros.items,
                    );
                    try self.rebuildMacroMapsFromPersistentRoots();
                    self.macro_gc_synced = gc_after;
                }
                return run_err;
            };
        } else vm.run(chunk_ptr) catch |run_err| {
            const gc_after = self.heap.stats.gc_count;
            if (gc_after != gc_before) {
                try self.refreshMacroKeys();
                try self.collectCompilerMacroPairs(&live_compiler_macros);
                try self.collectSymbolMacroPairs(&live_symbol_macros);
                try self.collectReplMacroPairs(&live_repl_macros);
                try self.restoreMacroMapsFromRoots(
                    roots.items,
                    compiler_macro_start,
                    compiler_macros.items.len,
                    symbol_macro_start,
                    symbol_macros.items.len,
                    repl_macro_start,
                    repl_macros.items,
                    live_compiler_macros.items,
                    live_symbol_macros.items,
                    live_repl_macros.items,
                );
                try self.rebuildMacroMapsFromPersistentRoots();
                self.macro_gc_synced = gc_after;
            }
            return run_err;
        };

        const gc_after = self.heap.stats.gc_count;
        if (gc_after != gc_before) {
            try self.refreshMacroKeys();
            try self.collectCompilerMacroPairs(&live_compiler_macros);
            try self.collectSymbolMacroPairs(&live_symbol_macros);
            try self.collectReplMacroPairs(&live_repl_macros);
            try self.restoreMacroMapsFromRoots(
                roots.items,
                compiler_macro_start,
                compiler_macros.items.len,
                symbol_macro_start,
                symbol_macros.items.len,
                repl_macro_start,
                repl_macros.items,
                live_compiler_macros.items,
                live_symbol_macros.items,
                live_repl_macros.items,
            );
            try self.rebuildMacroMapsFromPersistentRoots();
            self.macro_gc_synced = gc_after;
        }
        return result;
    }

    /// Wire up VM to compiler's global environment. Must be called after init.
    pub fn wireGlobalEnv(self: *Repl) !void {
        self.vm.setGlobalEnv(&self.compiler.globals);
        // Set up load callback
        self.vm.setLoadCallback(&loadCallback, @ptrCast(self));
        // Set up eval callback
        self.vm.setEvalCallback(&evalCallback, @ptrCast(self));
        // Set up macroexpand callbacks
        self.vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));
        self.vm.setMacroexpand1Callback(&macroexpand1Callback, @ptrCast(self));
        // Set up fboundp callback
        self.vm.setFboundpCallback(&fboundpCallback, @ptrCast(self));
        // Set up symbol-function/function-designator resolver callback
        self.vm.setFunctionResolveCallback(&functionResolveCallback, @ptrCast(self));
        // Set VM on compiler for macro expansion
        self.compiler.setVm(&self.vm);
        _ = try self.ensureLoadFormRootGlobal();
        _ = try self.ensureLoadFormRootStackGlobal();
        _ = try self.ensureLoadFormRootTmpGlobal();
        // Create *features* list
        try self.createFeaturesGlobal();
        // Create print control globals
        try self.createPrintGlobals();
        // Create standard stream globals
        try self.createStreamGlobals();
    }

    /// Helper to set a global and update num_globals
    fn setGlobal(self: *Repl, name: []const u8, value: Value) !void {
        const idx = try self.compiler.globals.define(name);
        if (idx >= self.vm.globals.len) return error.InvalidConstant;
        self.vm.globals[idx] = value;
        if (idx >= self.vm.num_globals) {
            self.vm.num_globals = idx + 1;
        }
    }

    fn ensureLoadFormRootGlobal(self: *Repl) !u16 {
        if (self.compiler.globals.lookup(load_form_root_name)) |idx| {
            if (idx >= self.vm.globals.len) return error.InvalidConstant;
            return idx;
        }
        const idx = try self.compiler.globals.define(load_form_root_name);
        if (idx >= self.vm.globals.len) return error.InvalidConstant;
        self.vm.globals[idx] = Value.nil;
        if (idx >= self.vm.num_globals) {
            self.vm.num_globals = idx + 1;
        }
        return idx;
    }

    fn ensureLoadFormRootStackGlobal(self: *Repl) !u16 {
        if (self.compiler.globals.lookup(load_form_root_stack_name)) |idx| {
            if (idx >= self.vm.globals.len) return error.InvalidConstant;
            return idx;
        }
        const idx = try self.compiler.globals.define(load_form_root_stack_name);
        if (idx >= self.vm.globals.len) return error.InvalidConstant;
        self.vm.globals[idx] = Value.nil;
        if (idx >= self.vm.num_globals) {
            self.vm.num_globals = idx + 1;
        }
        return idx;
    }

    fn ensureLoadFormRootTmpGlobal(self: *Repl) !u16 {
        if (self.compiler.globals.lookup(load_form_root_tmp_name)) |idx| {
            if (idx >= self.vm.globals.len) return error.InvalidConstant;
            return idx;
        }
        const idx = try self.compiler.globals.define(load_form_root_tmp_name);
        if (idx >= self.vm.globals.len) return error.InvalidConstant;
        self.vm.globals[idx] = Value.nil;
        if (idx >= self.vm.num_globals) {
            self.vm.num_globals = idx + 1;
        }
        return idx;
    }

    fn ensureVmGlobalRootSlots(vm: *Vm, root_idx: u16, stack_idx: u16, tmp_idx: u16) !void {
        if (root_idx >= vm.globals.len) return error.InvalidConstant;
        if (stack_idx >= vm.globals.len) return error.InvalidConstant;
        if (tmp_idx >= vm.globals.len) return error.InvalidConstant;
        if (root_idx >= vm.num_globals) vm.num_globals = root_idx + 1;
        if (stack_idx >= vm.num_globals) vm.num_globals = stack_idx + 1;
        if (tmp_idx >= vm.num_globals) vm.num_globals = tmp_idx + 1;
    }

    fn ensureVmGlobalRootStackSlots(vm: *Vm, root_idx: u16, stack_idx: u16) !void {
        if (root_idx >= vm.globals.len) return error.InvalidConstant;
        if (stack_idx >= vm.globals.len) return error.InvalidConstant;
        if (root_idx >= vm.num_globals) vm.num_globals = root_idx + 1;
        if (stack_idx >= vm.num_globals) vm.num_globals = stack_idx + 1;
    }

    fn allocRootCons(vm: *Vm, car: Value, cdr: Value, extra_roots: []const Value) !Value {
        return vm.heap.allocCons(car, cdr) catch |err| switch (err) {
            error.OutOfMemory => {
                var root_stack: [12]Value = undefined;
                var root_count: usize = 0;
                root_stack[root_count] = car;
                root_count += 1;
                root_stack[root_count] = cdr;
                root_count += 1;
                for (extra_roots) |v| {
                    if (root_count >= root_stack.len) break;
                    root_stack[root_count] = v;
                    root_count += 1;
                }
                _ = try vm.collectGarbageWithRoots(root_stack[0..root_count]);
                return try vm.heap.allocCons(root_stack[0], root_stack[1]);
            },
        };
    }

    fn pushRootValue(vm: *Vm, root_idx: u16, stack_idx: u16, root_val: Value) !void {
        try ensureVmGlobalRootStackSlots(vm, root_idx, stack_idx);
        const saved_root = vm.resolveForwardedValue(vm.globals[root_idx]);
        const saved_stack = vm.resolveForwardedValue(vm.globals[stack_idx]);
        const live_root = vm.resolveForwardedValue(root_val);
        vm.globals[root_idx] = live_root;
        errdefer {
            vm.globals[root_idx] = saved_root;
            vm.globals[stack_idx] = saved_stack;
        }
        const extra = [_]Value{live_root};
        vm.globals[stack_idx] = try allocRootCons(vm, saved_root, saved_stack, extra[0..]);
    }

    fn popRootValue(vm: *Vm, root_idx: u16, stack_idx: u16) void {
        if (root_idx >= vm.globals.len or stack_idx >= vm.globals.len) return;
        const stack = vm.resolveForwardedValue(vm.globals[stack_idx]);
        vm.globals[stack_idx] = stack;
        if (!stack.isCons()) {
            vm.globals[root_idx] = vm.resolveForwardedValue(vm.globals[root_idx]);
            vm.globals[stack_idx] = Value.nil;
            return;
        }
        const stack_addr = stack.toPtrAddr();
        if (!vm.heap.containsAddrForDebug(stack_addr)) {
            vm.globals[root_idx] = vm.resolveForwardedValue(vm.globals[root_idx]);
            vm.globals[stack_idx] = Value.nil;
            return;
        }
        const cell = stack.toPtr(Cons);
        vm.globals[root_idx] = vm.resolveForwardedValue(cell.car);
        vm.globals[stack_idx] = vm.resolveForwardedValue(cell.cdr);
    }

    fn compileExprRooted(self: *Repl, vm: *Vm, expr: Value, env: *const Env) !*Ir {
        const root_idx = try self.ensureLoadFormRootGlobal();
        const stack_idx = try self.ensureLoadFormRootStackGlobal();
        try pushRootValue(vm, root_idx, stack_idx, expr);
        defer popRootValue(vm, root_idx, stack_idx);
        const live_expr = vm.globals[root_idx];
        return self.compiler.compile(live_expr, env);
    }

    fn pushMacroCallRoots(
        vm: *Vm,
        root_idx: u16,
        stack_idx: u16,
        tmp_idx: u16,
        args: Value,
        whole_form: Value,
    ) !void {
        try ensureVmGlobalRootSlots(vm, root_idx, stack_idx, tmp_idx);
        const saved_root = vm.resolveForwardedValue(vm.globals[root_idx]);
        const saved_tmp = vm.resolveForwardedValue(vm.globals[tmp_idx]);
        const saved_stack = vm.resolveForwardedValue(vm.globals[stack_idx]);
        const args_live = vm.resolveForwardedValue(args);
        const whole_live = vm.resolveForwardedValue(whole_form);
        vm.globals[root_idx] = args_live;
        vm.globals[tmp_idx] = whole_live;
        errdefer {
            vm.globals[root_idx] = saved_root;
            vm.globals[tmp_idx] = saved_tmp;
        }
        const saved_pair = try allocRootCons(vm, saved_root, saved_tmp, &[_]Value{ args_live, whole_live });
        vm.globals[stack_idx] = try allocRootCons(vm, saved_pair, saved_stack, &[_]Value{ args_live, whole_live, saved_pair });
    }

    fn popMacroCallRoots(vm: *Vm, root_idx: u16, stack_idx: u16, tmp_idx: u16) void {
        if (root_idx >= vm.globals.len or stack_idx >= vm.globals.len or tmp_idx >= vm.globals.len) return;
        const stack = vm.resolveForwardedValue(vm.globals[stack_idx]);
        vm.globals[stack_idx] = stack;
        if (!stack.isCons()) {
            vm.globals[root_idx] = vm.resolveForwardedValue(vm.globals[root_idx]);
            vm.globals[tmp_idx] = vm.resolveForwardedValue(vm.globals[tmp_idx]);
            vm.globals[stack_idx] = Value.nil;
            return;
        }
        if (!vm.heap.containsAddrForDebug(stack.toPtrAddr())) {
            vm.globals[root_idx] = vm.resolveForwardedValue(vm.globals[root_idx]);
            vm.globals[tmp_idx] = vm.resolveForwardedValue(vm.globals[tmp_idx]);
            vm.globals[stack_idx] = Value.nil;
            return;
        }
        const stack_cell = stack.toPtr(Cons);
        const next_stack = vm.resolveForwardedValue(stack_cell.cdr);
        const saved_pair = vm.resolveForwardedValue(stack_cell.car);
        vm.globals[stack_idx] = next_stack;
        if (!saved_pair.isCons()) {
            vm.globals[root_idx] = vm.resolveForwardedValue(vm.globals[root_idx]);
            vm.globals[tmp_idx] = vm.resolveForwardedValue(vm.globals[tmp_idx]);
            return;
        }
        if (!vm.heap.containsAddrForDebug(saved_pair.toPtrAddr())) {
            vm.globals[root_idx] = vm.resolveForwardedValue(vm.globals[root_idx]);
            vm.globals[tmp_idx] = vm.resolveForwardedValue(vm.globals[tmp_idx]);
            return;
        }
        const pair = saved_pair.toPtr(Cons);
        vm.globals[root_idx] = vm.resolveForwardedValue(pair.car);
        vm.globals[tmp_idx] = vm.resolveForwardedValue(pair.cdr);
    }

    /// Helper to set a CL global: intern symbol in CL, define global, set value
    fn setClGlobal(self: *Repl, sym_name: []const u8, value: Value) !void {
        const saved_ext = try self.vm.saveExtRoots();
        const saved_ext_roots = saved_ext.roots;
        if (saved_ext_roots.len != 0) {
            for (saved_ext_roots) |*val| {
                val.* = self.vm.resolveForwardedValue(val.*);
            }
        }
        var roots = std.ArrayList(Value){};
        defer roots.deinit(self.allocator);
        try roots.ensureTotalCapacity(self.allocator, saved_ext_roots.len + 1);
        if (saved_ext_roots.len != 0) {
            try roots.appendSlice(self.allocator, saved_ext_roots);
        }
        const value_idx = roots.items.len;
        try roots.append(self.allocator, value);
        self.vm.setExtRootsOwned(&roots);
        defer self.vm.restoreExtRootsSynced(saved_ext, roots.items, saved_ext_roots.len);

        // Intern symbol in CL package so it's found when CL-USER code references it
        _ = try self.heap.internInPackage("COMMON-LISP", sym_name);
        const live_value = self.vm.resolveForwardedValue(roots.items[value_idx]);
        roots.items[value_idx] = live_value;
        // Define global with qualified name
        var buf: [256]u8 = undefined;
        const qname = try std.fmt.bufPrint(&buf, "COMMON-LISP:{s}", .{sym_name});
        try self.setGlobal(qname, live_value);
    }

    fn createFeaturesGlobal(self: *Repl) !void {
        const b = self.compiler.builtins.?;
        var features = Value.nil;
        features = try self.heap.allocCons(b.kw_habu, features);
        features = try self.heap.allocCons(b.kw_zig, features);
        const info = @import("builtin").os.tag;
        const os_kw = switch (info) {
            .windows => b.kw_windows,
            .macos => b.kw_darwin,
            else => b.kw_unix,
        };
        features = try self.heap.allocCons(os_kw, features);
        try self.setClGlobal("*FEATURES*", features);
    }

    fn createPrintGlobals(self: *Repl) !void {
        try self.setClGlobal("*PRINT-LENGTH*", Value.nil);
        try self.setClGlobal("*PRINT-LEVEL*", Value.nil);
    }

    fn createStreamGlobals(self: *Repl) !void {
        const saved_ext = try self.vm.saveExtRoots();
        const saved_ext_roots = saved_ext.roots;
        if (saved_ext_roots.len != 0) {
            for (saved_ext_roots) |*val| {
                val.* = self.vm.resolveForwardedValue(val.*);
            }
        }
        var roots = std.ArrayList(Value){};
        defer roots.deinit(self.allocator);
        try roots.ensureTotalCapacity(self.allocator, saved_ext_roots.len + 4);
        if (saved_ext_roots.len != 0) {
            try roots.appendSlice(self.allocator, saved_ext_roots);
        }
        const stdin_idx = roots.items.len;
        try roots.append(self.allocator, try self.heap.allocStdin());
        const stdout_idx = roots.items.len;
        try roots.append(self.allocator, try self.heap.allocStdout());
        const stderr_idx = roots.items.len;
        try roots.append(self.allocator, try self.heap.allocStderr());

        self.vm.setExtRootsOwned(&roots);
        defer self.vm.restoreExtRootsSynced(saved_ext, roots.items, saved_ext_roots.len);

        try self.setClGlobal("*STANDARD-INPUT*", roots.items[stdin_idx]);
        roots.items[stdin_idx] = self.vm.resolveForwardedValue(roots.items[stdin_idx]);

        try self.setClGlobal("*STANDARD-OUTPUT*", roots.items[stdout_idx]);
        roots.items[stdout_idx] = self.vm.resolveForwardedValue(roots.items[stdout_idx]);

        try self.setClGlobal("*ERROR-OUTPUT*", roots.items[stderr_idx]);
        roots.items[stderr_idx] = self.vm.resolveForwardedValue(roots.items[stderr_idx]);

        roots.items[stdin_idx] = self.vm.resolveForwardedValue(roots.items[stdin_idx]);
        roots.items[stdout_idx] = self.vm.resolveForwardedValue(roots.items[stdout_idx]);
        const term_idx = roots.items.len;
        try roots.append(self.allocator, try self.heap.allocTwoWayStream(roots.items[stdin_idx], roots.items[stdout_idx]));
        roots.items[term_idx] = self.vm.resolveForwardedValue(roots.items[term_idx]);

        roots.items[stdout_idx] = self.vm.resolveForwardedValue(roots.items[stdout_idx]);
        try self.setClGlobal("*TRACE-OUTPUT*", roots.items[stdout_idx]);
        try self.setClGlobal("*QUERY-IO*", roots.items[term_idx]);
        try self.setClGlobal("*DEBUG-IO*", roots.items[term_idx]);
        try self.setClGlobal("*TERMINAL-IO*", roots.items[term_idx]);
    }

    /// Callback for (load "filename") from VM
    fn loadCallback(filename: []const u8, context: *anyopaque) vm_mod.Error!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.load(filename);
    }

    /// Callback for (eval expr) from VM
    fn evalCallback(expr: Value, context: *anyopaque) vm_mod.Error!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        if (std.process.hasEnvVar(self.allocator, "HABU_TRACE_EVAL_EXPR") catch false) {
            if (expr.isCons()) {
                const head = expr.toPtr(Cons).car;
                if (head.isSymbol()) {
                    std.debug.print("EVAL expr head={s}\n", .{head.toPtr(Symbol).getName()});
                } else {
                    std.debug.print("EVAL expr head-kind={s}\n", .{@tagName(head.typeKind())});
                }
                if (expr.toPtr(Cons).cdr.isCons()) {
                    const arg0 = expr.toPtr(Cons).cdr.toPtr(Cons).car;
                    std.debug.print("EVAL expr arg0-kind={s}\n", .{@tagName(arg0.typeKind())});
                    if (arg0.isSymbol()) {
                        std.debug.print("EVAL expr arg0-symbol={s}\n", .{arg0.toPtr(Symbol).getName()});
                    } else if (arg0.isCons()) {
                        const inner_head = arg0.toPtr(Cons).car;
                        if (inner_head.isSymbol()) {
                            std.debug.print("EVAL expr arg0-head={s}\n", .{inner_head.toPtr(Symbol).getName()});
                        } else {
                            std.debug.print("EVAL expr arg0-head-kind={s}\n", .{@tagName(inner_head.typeKind())});
                        }
                    }
                }
            } else {
                std.debug.print("EVAL expr kind={s}\n", .{@tagName(expr.typeKind())});
            }
        }
        return self.evalExpr(expr);
    }

    /// Callback for (macroexpand expr) from VM
    fn macroexpandCallback(expr: Value, context: *anyopaque) vm_mod.Error!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.expandMacros(expr);
    }

    /// Callback for (macroexpand-1 expr) from VM
    fn macroexpand1Callback(expr: Value, context: *anyopaque) vm_mod.Error!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.expandMacrosOnce(expr);
    }

    /// Callback for (fboundp sym) from VM - checks if symbol has a function binding
    fn fboundpCallback(sym: Value, context: *anyopaque) vm_mod.Error!bool {
        const self: *Repl = @ptrCast(@alignCast(context));
        if (!sym.isSymbol()) return false;

        // Build qualified name using symbol's package
        // Check if it's a macro
        if (self.lookupMacroEntry(sym) != null) {
            return true;
        }
        if (try self.lookupCallableFunction(sym)) |_| {
            return true;
        }

        // Check if it's a builtin primitive
        if (self.compiler.builtins) |_| {
            const dispatch_sym = self.canonicalMacroSymbol(sym);
            if (self.compiler.isBuiltinFunctionRaw(dispatch_sym)) {
                return true;
            }
        }
        return false;
    }

    /// Callback for symbol-function/function designators from VM.
    /// Returns a callable Value for globals or lazily materialized primitive wrappers.
    fn functionResolveCallback(sym: Value, context: *anyopaque) vm_mod.Error!?Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        const live_sym = self.vm.resolveForwardedValue(sym);
        if (!live_sym.isSymbol()) return null;
        const trace_fn_resolve = self.trace_fn_resolve;
        if (trace_fn_resolve) {
            const name = self.safeSymbolName(live_sym) orelse "<invalid-symbol>";
            std.debug.print("TRACE fn-resolve sym={s}\n", .{name});
        }

        if (try self.lookupCallableFunction(live_sym)) |fn_val| {
            if (trace_fn_resolve) {
                std.debug.print("TRACE fn-resolve hit-global kind={s}\n", .{@tagName(fn_val.typeKind())});
            }
            return fn_val;
        }

        if (try self.tryAutoloadFunctionSymbol(live_sym)) {
            if (try self.lookupCallableFunction(live_sym)) |autoloaded_fn| {
                if (trace_fn_resolve) {
                    std.debug.print("TRACE fn-resolve hit-autoload kind={s}\n", .{@tagName(autoloaded_fn.typeKind())});
                }
                return autoloaded_fn;
            }
        }

        // Lazily materialize builtin primitive wrappers.
        if (self.compiler.builtins) |_| {
            const dispatch_sym = self.canonicalMacroSymbol(live_sym);
            const is_builtin = self.compiler.isBuiltinFunctionRaw(dispatch_sym);
            if (trace_fn_resolve) {
                const dispatch_name = self.safeSymbolName(dispatch_sym) orelse "<invalid-symbol>";
                std.debug.print(
                    "TRACE fn-resolve dispatch={s} builtin={}\n",
                    .{ dispatch_name, is_builtin },
                );
            }
            if (is_builtin) {
                const arity_opt = self.compiler.primitiveRefArity(dispatch_sym);
                if (trace_fn_resolve) {
                    if (arity_opt) |arity| {
                        std.debug.print("TRACE fn-resolve arity={d}\n", .{@intFromEnum(arity)});
                    } else {
                        std.debug.print("TRACE fn-resolve arity=null\n", .{});
                    }
                }
                if (arity_opt) |arity| {
                    const wrapper_form = try self.buildPrimitiveFunctionWrapper(dispatch_sym, arity);
                    const wrapper_val = try self.evalExpr(wrapper_form);
                    if (trace_fn_resolve) {
                        std.debug.print("TRACE fn-resolve wrapper kind={s}\n", .{@tagName(wrapper_val.typeKind())});
                    }
                    if (isCallableValue(wrapper_val)) return wrapper_val;
                } else if (self.builtinCallableTag(dispatch_sym)) |tag| {
                    const builtin_val = try self.heap.allocNativeCode(@intFromEnum(tag));
                    if (trace_fn_resolve) {
                        std.debug.print("TRACE fn-resolve native kind={s}\n", .{@tagName(builtin_val.typeKind())});
                    }
                    return builtin_val;
                }
            }
        }

        if (trace_fn_resolve) {
            std.debug.print("TRACE fn-resolve miss\n", .{});
        }
        return null;
    }

    fn isCallableValue(val: Value) bool {
        return switch (val.typeKind()) {
            .closure, .native_code, .generic_function => true,
            else => false,
        };
    }

    fn lookupFunctionCellValue(self: *Repl, sym: Value) !?Value {
        if (!sym.isSymbol()) return null;
        const key = try self.heap.intern("%FUNCTION-CELL");
        const cell = try primitives.list.get(self.heap, sym, key);
        if (!isCallableValue(cell)) return null;
        return cell;
    }

    fn tryAutoloadFunctionSymbol(self: *Repl, sym: Value) !bool {
        const live_sym = self.vm.resolveForwardedValue(sym);
        if (!live_sym.isSymbol()) return false;
        const sym_ptr = live_sym.toPtr(Symbol);
        const home_pkg = self.heap.symbolHomePkg(sym_ptr) orelse return false;

        const autoload_key = home_pkg.findAccessibleUpper("AUTOLOAD") orelse return false;
        const load_function_sym = home_pkg.findAccessibleUpper("LOAD-FUNCTION") orelse return false;
        const target_sym = live_sym;
        var autoload_val = try primitives.list.get(self.heap, target_sym, autoload_key);
        if (autoload_val.isNil()) return false;
        const builtins = self.compiler.builtins orelse return false;
        const quote_form = try self.listFromSlice(&[_]Value{ builtins.quote, target_sym });
        const form = try self.listFromSlice(&[_]Value{ load_function_sym, quote_form, Value.nil });
        _ = try self.evalExpr(form);
        return true;
    }

    fn buildPrimitiveFunctionWrapper(self: *Repl, sym: Value, arity: Compiler.PrimitiveRefArity) !Value {
        const builtins = self.compiler.builtins orelse return error.InvalidSyntax;
        const arg_count: usize = switch (arity) {
            .nullary => 0,
            .unary => 1,
            .binary => 2,
            .ternary => 3,
        };

        const sym_a = try self.heap.intern("A");
        const sym_b = try self.heap.intern("B");
        const sym_c = try self.heap.intern("C");
        const param_pool = [_]Value{ sym_a, sym_b, sym_c };

        var params_buf: [3]Value = undefined;
        var i: usize = 0;
        while (i < arg_count) : (i += 1) {
            params_buf[i] = param_pool[i];
        }
        const params_list = try self.listFromSlice(params_buf[0..arg_count]);

        var call_items: [4]Value = undefined;
        call_items[0] = sym;
        i = 0;
        while (i < arg_count) : (i += 1) {
            call_items[i + 1] = params_buf[i];
        }
        const call_list = try self.listFromSlice(call_items[0 .. arg_count + 1]);

        const lambda_items = [_]Value{ builtins.lambda, params_list, call_list };
        return self.listFromSlice(&lambda_items);
    }

    fn builtinCallableTag(self: *const Repl, sym: Value) ?vm_mod.BuiltinCallableTag {
        const b = self.compiler.builtins orelse return null;
        const Entry = struct { field: []const u8, tag: vm_mod.BuiltinCallableTag };
        const table = [_]Entry{
            .{ .field = "+", .tag = .add },
            .{ .field = "-", .tag = .sub },
            .{ .field = "*", .tag = .mul },
            .{ .field = "/", .tag = .div },
            .{ .field = "log", .tag = .log },
            .{ .field = "gensym", .tag = .gensym },
            .{ .field = "atan", .tag = .atan },
            .{ .field = "list", .tag = .list },
            .{ .field = "%make-broadcast-stream", .tag = .make_broadcast_stream },
            .{ .field = "%make-concatenated-stream", .tag = .make_concatenated_stream },
            .{ .field = "class-of", .tag = .class_of },
            .{ .field = "floor", .tag = .floor },
            .{ .field = "ceiling", .tag = .ceiling },
            .{ .field = "round", .tag = .round },
            .{ .field = "truncate", .tag = .truncate },
            .{ .field = "aref", .tag = .aref },
            .{ .field = "make-string", .tag = .make_string },
            .{ .field = "make-vector", .tag = .make_vector },
            .{ .field = "%svset", .tag = .svset },
            .{ .field = "%aset", .tag = .aset },
            .{ .field = "%set-slot-value", .tag = .set_slot_value },
            .{ .field = "%sset", .tag = .sset },
            .{ .field = "%make-unbound", .tag = .make_unbound },
            .{ .field = "%class-of", .tag = .class_of_internal },
            .{ .field = "make-array", .tag = .make_array },
            .{ .field = "char", .tag = .char },
            .{ .field = "schar", .tag = .schar },
            .{ .field = "format", .tag = .format },
            .{ .field = "print", .tag = .print },
            .{ .field = "princ", .tag = .princ },
            .{ .field = "encode-universal-time", .tag = .encode_universal_time },
            .{ .field = "%make-pathname", .tag = .make_pathname },
            .{ .field = "make-hash-table", .tag = .make_hash_table },
            .{ .field = "gethash", .tag = .gethash },
            .{ .field = "puthash", .tag = .puthash },
            .{ .field = "remhash", .tag = .remhash },
            .{ .field = "hash-table-count", .tag = .hash_table_count },
            .{ .field = "hash-table-capacity", .tag = .hash_table_capacity },
            .{ .field = "%open", .tag = .open },
            .{ .field = "%close", .tag = .close_internal },
            .{ .field = "close", .tag = .close },
            .{ .field = "%read-line", .tag = .read_line },
            .{ .field = "%write-line", .tag = .write_line },
            .{ .field = "%write-string", .tag = .write_string },
            .{ .field = "%read-byte", .tag = .read_byte },
            .{ .field = "%write-byte", .tag = .write_byte },
            .{ .field = "%file-position", .tag = .file_position },
            .{ .field = "%set-file-position", .tag = .set_file_position },
            .{ .field = "%file-length", .tag = .file_length },
            .{ .field = "%finish-output", .tag = .finish_output },
            .{ .field = "%force-output", .tag = .force_output },
            .{ .field = "%clear-input", .tag = .clear_input },
            .{ .field = "%clear-output", .tag = .clear_output },
            .{ .field = "class-direct-superclasses", .tag = .class_direct_superclasses },
            .{ .field = "class-precedence-list", .tag = .class_precedence_list },
            .{ .field = "class-direct-slots", .tag = .class_direct_slots },
            .{ .field = "class-slots", .tag = .class_slots },
            .{ .field = "slot-definition-name", .tag = .slot_definition_name },
            .{ .field = "slot-definition-initform", .tag = .slot_definition_initform },
            .{ .field = "slot-definition-initargs", .tag = .slot_definition_initargs },
            .{ .field = "slot-definition-readers", .tag = .slot_definition_readers },
            .{ .field = "slot-definition-writers", .tag = .slot_definition_writers },
            .{ .field = "slot-definition-allocation", .tag = .slot_definition_allocation },
            .{ .field = "slot-definition-type", .tag = .slot_definition_type },
            .{ .field = "%set-class-printer", .tag = .set_class_printer },
        };

        inline for (table) |entry| {
            if (sym.raw == @field(b, entry.field).raw) return entry.tag;
        }
        return null;
    }

    fn listFromSlice(self: *Repl, items: []const Value) !Value {
        var out = Value.nil;
        var i = items.len;
        while (i > 0) {
            i -= 1;
            out = try self.heap.allocCons(items[i], out);
        }
        return out;
    }

    fn lookupCallableFunction(self: *Repl, sym: Value) !?Value {
        const live_sym = self.vm.resolveForwardedValue(sym);
        if (!live_sym.isSymbol()) return null;
        const local_name = self.safeSymbolName(live_sym) orelse return null;
        const source_vm = self.activeVm();
        const s = live_sym.toPtr(Symbol);
        const trace = self.trace_fn_resolve;
        if (try self.lookupFunctionCellValue(live_sym)) |fn_cell| {
            if (trace) {
                std.debug.print("TRACE fn-lookup fn-cell={s}\n", .{local_name});
            }
            return fn_cell;
        }
        if (trace and std.mem.eql(u8, local_name, "MAPCAR")) {
            std.debug.print("TRACE fn-lookup source num_globals={d}\n", .{source_vm.num_globals});
        }

        var qbuf: [512]u8 = undefined;
        const q = try qual_name.qualSymWithHeap(self.allocator, self.heap, s, &qbuf);
        defer if (q.owned) self.allocator.free(q.name);

        if (self.compiler.globals.lookup(q.name)) |idx| {
            if (trace) {
                std.debug.print("TRACE fn-lookup qname={s} idx={d} kind={s}\n", .{
                    q.name,
                    idx,
                    globalKindName(source_vm, idx),
                });
            }
            if (self.lookupCallableAtGlobalIdx(source_vm, idx)) |callable| {
                return callable;
            }
        }
        return null;
    }

    fn globalKindName(vm: *const Vm, idx: usize) []const u8 {
        if (idx >= vm.num_globals or idx >= vm.globals.len) return "oob";
        return @tagName(vm.globals[idx].typeKind());
    }

    fn ptrInHeap(vm: *const Vm, ptr_addr: usize, need_bytes: usize) bool {
        const heap_start = @intFromPtr(vm.heap.memory.ptr);
        const heap_end = heap_start + vm.heap.memory.len;
        if (ptr_addr < heap_start or ptr_addr >= heap_end) return false;
        const end = ptr_addr + need_bytes;
        if (end < ptr_addr) return false;
        return end <= heap_end;
    }

    fn isCallableGlobalValue(vm: *const Vm, val: Value) bool {
        const raw = val.raw;
        if (raw == Value.nil.raw or raw == Value.t.raw or raw == Value.unbound.raw) return false;
        // Immediates are never callable.
        if ((raw & 1) == 1) return false; // fixnum
        if ((raw >> 63) == 1) return false; // char
        if (((raw >> 62) & 0x3) == 1) return false; // float

        const tag_bits: u4 = @truncate(raw & 0xF);
        switch (tag_bits) {
            0x8 => return true, // closure
            0xC => { // boxed: only generic-function and native-code are callable
                const ptr_addr = @as(usize, @intCast(raw & ~@as(u64, 0xF)));
                if (!ptrInHeap(vm, ptr_addr, @sizeOf(u64))) return false;
                const kind_raw = @as(*const u64, @ptrFromInt(ptr_addr)).*;
                return kind_raw == @intFromEnum(runtime.BoxedKind.generic_function) or
                    kind_raw == @intFromEnum(runtime.BoxedKind.native_code);
            },
            else => return false,
        }
    }

    fn lookupCallableInVm(vm: *const Vm, idx: usize) ?Value {
        if (idx >= vm.num_globals or idx >= vm.globals.len) return null;
        const val = vm.globals[idx];
        if (!isCallableGlobalValue(vm, val)) return null;
        return val;
    }

    fn lookupCallableAtGlobalIdx(self: *Repl, source_vm: *const Vm, idx: usize) ?Value {
        if (lookupCallableInVm(source_vm, idx)) |callable| return callable;
        if (source_vm != &self.vm) {
            if (lookupCallableInVm(&self.vm, idx)) |callable| return callable;
        }
        if (self.current_vm) |cur| {
            if (cur != source_vm and cur != &self.vm) {
                if (lookupCallableInVm(cur, idx)) |callable| return callable;
            }
        }
        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            const vm = ctx.vm;
            if (vm != source_vm and vm != &self.vm) {
                if (lookupCallableInVm(vm, idx)) |callable| return callable;
            }
            ctx_opt = ctx.prev;
        }
        return null;
    }

    /// Evaluate an expression in the active VM context.
    fn evalExpr(self: *Repl, expr: Value) !Value {
        // Use arena for compilation
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        const source_vm = self.activeVm();
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
        try pushRootValue(source_vm, form_root_idx, form_stack_idx, expr);
        defer popRootValue(source_vm, form_root_idx, form_stack_idx);

        // Save and set compiler state
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        const saved_compiler_vm = self.compiler.vm;
        self.compiler.setVm(source_vm);
        try self.compiler.refreshBuiltins();
        self.compiler.builder = ir.IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;
        defer {
            if (saved_compiler_vm) |saved_vm| {
                self.compiler.setVm(saved_vm);
            } else {
                self.compiler.vm = null;
            }
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
        }

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        // Let compiler-driven macro expansion run for eval forms. Pre-expanding
        // through the REPL macro table can leak stale macro bindings into eval.
        const normalized = source_vm.resolveForwardedValue(source_vm.globals[form_root_idx]);
        const ir_node = try self.compileExprRooted(source_vm, normalized, &env);
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        emitter.setRetainedValueLookup(Compiler.retainedValueLookup, &self.compiler);
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch child chunks to use absolute indices
        for (child_chunks) |c| {
            const chunk_ptr = c.toPtr(runtime.objects.Chunk);
            try patchChunkIndices(chunk_ptr, chunk_base);
        }

        // Store child chunks for closures and sync all VM views immediately.
        // tryHoistCompileLambdas and runtime eval can allocate/GC; chunk_pool
        // slices must already point at the current owner storage.
        try self.appendChildChunksAndSync(source_vm, child_chunks);

        // Run through the same VM using a nested call frame. This preserves
        // the caller VM's stack/locals across GC while evaluating runtime EVAL.
        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        try patchChunkIndices(chunk_ptr, chunk_base);
        const closure = try source_vm.allocClosureWithGC(chunk, chunk_ptr.arity, &[_]Value{});
        return try source_vm.callFromStackAt(source_vm.sp, closure, &[_]Value{});
    }

    /// Load a file and return the last value (for (load ...) primitive)
    fn load(self: *Repl, path: []const u8) !Value {
        const source_vm = self.activeVm();
        const resolved_path = try self.resolveLoadPath(source_vm, path);
        defer self.allocator.free(resolved_path);
        const trace_forms = std.process.hasEnvVar(self.allocator, "HABU_TRACE_FORMS") catch false;
        if (trace_forms) {
            std.debug.print("TRACE load: {s} [pkg={s}]\n", .{ resolved_path, self.heap.getCurrentPackageName() });
        }

        const content = try self.readFileContent(resolved_path);
        defer self.allocator.free(content);

        const load_path = try self.loadPathnameValue(resolved_path);
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();

        const load_bindings = try self.bindLoadGlobals(source_vm, load_path, form_root_idx, form_stack_idx);
        defer self.restoreLoadGlobals(source_vm, load_bindings, form_root_idx, form_stack_idx);

        // CL spec: *PACKAGE* is dynamically rebound by LOAD and restored after.
        const saved_pkg_global = self.globalValue(source_vm, "COMMON-LISP:*PACKAGE*") orelse Value.nil;
        try pushRootValue(source_vm, form_root_idx, form_stack_idx, saved_pkg_global);
        defer popRootValue(source_vm, form_root_idx, form_stack_idx);
        defer {
            const saved_pkg_live = source_vm.globals[form_root_idx];
            self.setGlobalValue(source_vm, "COMMON-LISP:*PACKAGE*", saved_pkg_live);
            self.syncReaderPackageFromVm(source_vm);
        }

        const result = try self.evalForms(content);
        if (trace_forms and (std.mem.endsWith(u8, resolved_path, "gclload1.lsp") or
            std.mem.endsWith(u8, resolved_path, "rt.lsp") or
            std.mem.endsWith(u8, resolved_path, "cl-test-package.lsp")))
        {
            std.debug.print(
                "TRACE load done: {s} cl-test-native={any} pkg={s}\n",
                .{ resolved_path, self.heap.findPackage("CL-TEST") != null, self.heap.getCurrentPackageName() },
            );
        }
        return result;
    }

    const LoadBindings = struct {
        idxs: [6]usize = [_]usize{0} ** 6,
        len: usize = 0,
    };

    fn loadPathnameValue(self: *Repl, path: []const u8) !Value {
        const resolved = if (std.fs.realpathAlloc(self.allocator, path)) |p| p else |_| blk: {
            if (std.fs.path.isAbsolute(path)) {
                break :blk try self.allocator.dupe(u8, path);
            }
            const cwd = try std.process.getCwdAlloc(self.allocator);
            defer self.allocator.free(cwd);
            break :blk try std.fs.path.join(self.allocator, &.{ cwd, path });
        };
        defer self.allocator.free(resolved);

        const path_str = try self.heap.allocBaseString(resolved);
        return try primitives.pathname.parseNamestring(self.allocator, self.heap, path_str);
    }

    fn bindLoadGlobals(
        self: *Repl,
        vm: *Vm,
        path: Value,
        root_idx: u16,
        stack_idx: u16,
    ) !LoadBindings {
        var bindings: LoadBindings = .{};
        const names = [_][]const u8{
            "COMMON-LISP:*LOAD-PATHNAME*",
            "COMMON-LISP:*LOAD-TRUENAME*",
            "COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS*",
        };
        for (names) |name| {
            const idx = self.compiler.globals.lookup(name) orelse continue;
            if (idx >= vm.globals.len) continue;
            const prev = if (idx < vm.num_globals) vm.globals[idx] else Value.nil;
            try pushRootValue(vm, root_idx, stack_idx, prev);
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
            std.debug.assert(bindings.len < bindings.idxs.len);
            bindings.idxs[bindings.len] = idx;
            bindings.len += 1;
        }
        return bindings;
    }

    fn restoreLoadGlobals(
        self: *Repl,
        vm: *Vm,
        bindings: LoadBindings,
        root_idx: u16,
        stack_idx: u16,
    ) void {
        _ = self;
        var i = bindings.len;
        while (i > 0) {
            i -= 1;
            const idx = bindings.idxs[i];
            if (idx >= vm.globals.len) {
                popRootValue(vm, root_idx, stack_idx);
                continue;
            }
            vm.globals[idx] = vm.globals[root_idx];
            popRootValue(vm, root_idx, stack_idx);
        }
    }

    fn readFileContent(self: *Repl, path: []const u8) ![]u8 {
        const file = if (std.fs.path.isAbsolute(path))
            try std.fs.openFileAbsolute(path, .{})
        else
            try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const stat = try file.stat();
        const max_bytes = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
        return try file.readToEndAlloc(self.allocator, max_bytes);
    }

    fn currentLoadTruename(self: *Repl, vm: *const Vm) ?Value {
        if (self.globalValue(@constCast(vm), "COMMON-LISP:*LOAD-TRUENAME*")) |val| {
            if (!val.isNil()) return val;
        }
        return null;
    }

    fn globalValue(self: *Repl, vm: *Vm, name: []const u8) ?Value {
        if (self.compiler.globals.lookup(name)) |idx| {
            if (idx < vm.num_globals) {
                const raw = vm.globals[idx];
                const val = vm.resolveForwardedValue(raw);
                if (val.raw != raw.raw) vm.globals[idx] = val;
                return val;
            }
        }
        return null;
    }

    fn setGlobalValue(self: *Repl, vm: *Vm, name: []const u8, val: Value) void {
        if (self.compiler.globals.lookup(name)) |idx| {
            if (idx >= vm.globals.len) return;
            vm.globals[idx] = val;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
    }

    fn currentPackageGlobal(self: *Repl, vm: *Vm) ?Value {
        if (self.globalValue(vm, "COMMON-LISP:*PACKAGE*")) |val| {
            if (val.isPackage()) return val;
        }
        return null;
    }

    fn setPackageGlobals(self: *Repl, vm: *Vm, pkg_val: Value) void {
        self.setGlobalValue(vm, "COMMON-LISP:*PACKAGE*", pkg_val);
    }

    fn syncReaderPackageFromVm(self: *Repl, vm: *Vm) void {
        const pkg_val = self.currentPackageGlobal(vm) orelse return;
        const pkg_name = self.packageNameBytesLive(vm, pkg_val) orelse return;
        if (self.heap.findPackage(pkg_name)) |native_pkg| {
            self.heap.setCurrentPackage(native_pkg);
        }
    }

    fn packageNameBytesLive(self: *Repl, vm: *Vm, pkg_val: Value) ?[]const u8 {
        const live_pkg = vm.resolveForwardedValue(pkg_val);
        if (!live_pkg.isPackage()) return null;
        const pkg_obj = live_pkg.toPtr(runtime.objects.Package);
        const raw_name = pkg_obj.name;
        const live_name = vm.resolveForwardedValue(raw_name);
        if (live_name.raw != raw_name.raw) {
            pkg_obj.name = live_name;
            self.heap.writeBarrier(live_pkg, live_name);
        }
        const name_bytes = switch (live_name.typeKind()) {
            .symbol => live_name.toPtr(runtime.Symbol).getName(),
            .string => live_name.toPtr(runtime.String).bytes(),
            .keyword => live_name.toPtr(runtime.Keyword).getName(),
            else => return null,
        };
        if (name_bytes.len == 0) return name_bytes;
        const name_ptr = @intFromPtr(name_bytes.ptr);
        if (!self.heap.containsAddrForDebug(name_ptr)) {
            if (std.posix.getenv("HABU_TRACE_BAD_PACKAGE_NAME") != null) {
                const live_addr = if (live_name.isPointer()) live_name.toPtrAddr() else 0;
                const stale_start = @intFromPtr(self.heap.to_start);
                const stale_end = stale_start + self.heap.space_size;
                const from_start = @intFromPtr(self.heap.from_start);
                const from_end = @intFromPtr(self.heap.from_end);
                var fw_raw: u64 = 0;
                var w1: usize = 0;
                if (live_addr != 0 and self.heap.containsAddrForDebug(live_addr)) {
                    const fw: *const Value = @ptrFromInt(live_addr);
                    fw_raw = fw.raw;
                    const w1_ptr: *const usize = @ptrFromInt(live_addr + @sizeOf(Value));
                    w1 = w1_ptr.*;
                }
                std.debug.print(
                    "TRACE bad package-name ptr pkg=0x{x} name=0x{x} kind={s} ptr=0x{x} len={d} live=0x{x} fw=0x{x} w1=0x{x} stale=[0x{x},0x{x}) from=[0x{x},0x{x})\n",
                    .{
                        live_pkg.raw,
                        live_name.raw,
                        @tagName(live_name.typeKind()),
                        name_ptr,
                        name_bytes.len,
                        live_addr,
                        fw_raw,
                        w1,
                        stale_start,
                        stale_end,
                        from_start,
                        from_end,
                    },
                );
            }
            @panic("invalid package name pointer");
        }
        return name_bytes;
    }

    fn resolveLoadPath(self: *Repl, vm: *const Vm, path: []const u8) ![]u8 {
        if (std.fs.path.isAbsolute(path)) {
            return try self.allocator.dupe(u8, path);
        }
        if (self.currentDefaultPathname(vm)) |defaults| {
            if (try self.pathnameDesignatorToOwnedString(defaults)) |base| {
                defer self.allocator.free(base);
                if (try self.resolveRelativeLoadPath(base, path)) |resolved| {
                    return resolved;
                }
            }
        }
        if (self.currentLoadTruename(vm)) |load_true| {
            if (try self.pathnameDesignatorToOwnedString(load_true)) |base| {
                defer self.allocator.free(base);
                if (try self.resolveRelativeLoadPath(base, path)) |resolved| {
                    return resolved;
                }
            }
        }
        if (try self.resolveRelativeLoadPath(self.trusted_load_root, path)) |resolved| {
            return resolved;
        }
        return error.FileNotFound;
    }

    fn resolveRelativeLoadPath(self: *Repl, base: []const u8, path: []const u8) !?[]u8 {
        const candidates = [_][]const u8{
            base,
            std.fs.path.dirname(base) orelse "",
        };
        for (candidates) |root| {
            if (root.len == 0) continue;
            if (try self.resolveTrustedPath(root, path)) |resolved| return resolved;
        }
        return null;
    }

    fn resolveTrustedPath(self: *Repl, root: []const u8, path: []const u8) !?[]u8 {
        const root_abs = try self.resolveBasePath(root);
        defer self.allocator.free(root_abs);
        const candidate = try std.fs.path.resolve(self.allocator, &.{ root_abs, path });
        errdefer self.allocator.free(candidate);
        if (!self.pathWithinRoot(root_abs, candidate)) return null;
        if (!try self.fileExists(candidate)) return null;
        return candidate;
    }

    fn resolveBasePath(self: *Repl, base: []const u8) ![]u8 {
        if (std.fs.path.isAbsolute(base)) {
            return try std.fs.path.resolve(self.allocator, &.{base});
        }
        return try std.fs.path.resolve(self.allocator, &.{ self.trusted_load_root, base });
    }

    fn pathWithinRoot(self: *Repl, root: []const u8, path: []const u8) bool {
        _ = self;
        if (!std.mem.startsWith(u8, path, root)) return false;
        if (path.len == root.len) return true;
        if (root.len == 0) return false;
        const sep = std.fs.path.sep;
        return root[root.len - 1] == sep or path[root.len] == sep;
    }

    fn fileExists(self: *Repl, path: []const u8) !bool {
        _ = self;
        if (!std.fs.path.isAbsolute(path)) return false;
        std.fs.accessAbsolute(path, .{}) catch |err| switch (err) {
            error.FileNotFound => return false,
            else => return err,
        };
        return true;
    }

    fn pathnameDesignatorToOwnedString(self: *Repl, designator: Value) !?[]u8 {
        return switch (designator.typeKind()) {
            .string => try self.allocator.dupe(u8, designator.toPtr(String).bytes()),
            .pathname => blk: {
                const ns = try primitives.pathname.namestring(self.allocator, self.heap, &self.vm.builtins, designator);
                if (!ns.isString()) break :blk null;
                break :blk try self.allocator.dupe(u8, ns.toPtr(String).bytes());
            },
            else => null,
        };
    }

    fn currentDefaultPathname(self: *Repl, vm: *const Vm) ?Value {
        if (self.globalValue(@constCast(vm), "COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS*")) |val| {
            if (!val.isNil()) return val;
        }
        return null;
    }

    const ReadEvalCtx = struct {
        repl: *Repl,
        vm: *Vm,
    };

    const DispatchMacroCtx = struct {
        vm: *Vm,
    };

    fn parseWithHookError(parser: *Parser) anyerror!Value {
        return parser.parse() catch |parse_err| {
            if (parse_err == error.UnexpectedToken) {
                if (parser.takeHookError()) |hook_err| return hook_err;
            }
            return parse_err;
        };
    }

    fn parserReadEval(ctx: *anyopaque, expr: Value) anyerror!Value {
        const hook: *ReadEvalCtx = @ptrCast(@alignCast(ctx));
        var arena = std.heap.ArenaAllocator.init(hook.repl.allocator);
        defer arena.deinit();
        const eval_alloc = arena.allocator();
        return hook.repl.evalParsedWithVm(expr, hook.vm, eval_alloc, null) catch |err| {
            if (std.posix.getenv("HABU_TRACE_READ_EVAL") != null) {
                if (expr.isCons()) {
                    const head = expr.toPtr(Cons).car;
                    if (head.isSymbol()) {
                        std.debug.print("TRACE read-eval error: {s} head={s}\n", .{ @errorName(err), head.toPtr(Symbol).getName() });
                    } else {
                        std.debug.print("TRACE read-eval error: {s} head-kind={s}\n", .{ @errorName(err), @tagName(head.typeKind()) });
                    }
                } else {
                    std.debug.print("TRACE read-eval error: {s} expr-kind={s}\n", .{ @errorName(err), @tagName(expr.typeKind()) });
                }
            }
            return err;
        };
    }

    fn parserDispatchMacro(
        ctx: *anyopaque,
        function: Value,
        disp_char: u8,
        sub_char: u8,
        arg: ?u32,
        stream: Value,
    ) anyerror!Value {
        _ = disp_char;
        const hook: *DispatchMacroCtx = @ptrCast(@alignCast(ctx));
        const arg_val = if (arg) |n| Value.makeFixnum(@intCast(n)) else Value.nil;
        const args = [_]Value{ stream, Value.makeCharacter(sub_char), arg_val };
        return hook.vm.callFromStackAt(hook.vm.sp, function, &args) catch |err| {
            if (std.posix.getenv("HABU_TRACE_DISPATCH_MACRO") != null) {
                const fn_kind = @tagName(function.typeKind());
                const has_resolver = hook.vm.function_resolve_callback != null;
                if (function.isSymbol()) {
                    std.debug.print(
                        "TRACE dispatch-macro error={s} fn={s} kind={s} sub={c} resolver={}\n",
                        .{ @errorName(err), function.toPtr(Symbol).getName(), fn_kind, sub_char, has_resolver },
                    );
                } else {
                    std.debug.print(
                        "TRACE dispatch-macro error={s} kind={s} sub={c} resolver={}\n",
                        .{ @errorName(err), fn_kind, sub_char, has_resolver },
                    );
                }
            }
            return err;
        };
    }

    /// Evaluate multiple forms in the active VM context.
    /// Nested execution is handled through callFromStackAt in runVmPreserveMacroState.
    fn evalForms(self: *Repl, content: []const u8) !Value {
        var last_value = Value.nil;
        const trace_forms = std.process.hasEnvVar(self.allocator, "HABU_TRACE_FORMS") catch false;
        const trace_form_timing = std.process.hasEnvVar(self.allocator, "HABU_TRACE_FORM_TIMING") catch false;
        var form_idx: usize = 0;
        var had_deftest_state: ?bool = null;

        // Evaluate forms in the current active VM to keep GC roots coherent
        // across nested LOAD/EVAL callback recursion.
        const source_vm = self.activeVm();
        const saved_current_vm = self.current_vm;
        self.current_vm = source_vm;
        defer self.current_vm = saved_current_vm;

        self.syncChunkPools(source_vm);
        // Keep reader package aligned with runtime *PACKAGE* for this VM.
        self.syncReaderPackageFromVm(source_vm);

        var parse_arena = std.heap.ArenaAllocator.init(self.allocator);
        defer parse_arena.deinit();
        const parse_alloc = parse_arena.allocator();

        var parser = try Parser.init(parse_alloc, self.heap, content, &self.vm.builtins);
        defer parser.deinit();
        var read_eval_ctx = ReadEvalCtx{ .repl = self, .vm = source_vm };
        var dispatch_ctx = DispatchMacroCtx{ .vm = source_vm };
        parser.setReadEvalHook(@ptrCast(&read_eval_ctx), parserReadEval);
        parser.setDispatchMacroHook(@ptrCast(&dispatch_ctx), parserDispatchMacro);

        while (parser.current.kind != .eof) {
            self.syncReaderPackageFromVm(source_vm);
            const expr = parseWithHookError(&parser) catch |err| {
                if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null or trace_forms) {
                    const loc = parser.getErrorLocation();
                    var load_name: []const u8 = "<unknown>";
                    if (self.currentLoadTruename(source_vm)) |truename| {
                        const ns = primitives.pathname.namestring(
                            self.allocator,
                            self.heap,
                            &self.vm.builtins,
                            truename,
                        ) catch Value.nil;
                        if (ns.isString()) load_name = ns.toPtr(runtime.String).bytes();
                    }
                    std.debug.print(
                        "TRACE parse error: {s} file={s} at {d}:{d} token={s} kind={s}\n",
                        .{
                            @errorName(err),
                            load_name,
                            loc.line,
                            loc.column,
                            loc.text,
                            @tagName(parser.current.kind),
                        },
                    );
                }
                return err;
            };
            form_idx += 1;
            if (trace_forms) {
                std.debug.print("TRACE form {d} pkg={s}\n", .{ form_idx, self.heap.getCurrentPackageName() });
                if (expr.isCons()) {
                    const head = expr.toPtr(Cons).car;
                    if (head.isSymbol()) {
                        const head_name = head.toPtr(Symbol).getName();
                        std.debug.print("TRACE form {d}: {s}\n", .{ form_idx, head_name });
                        if (std.mem.eql(u8, head_name, "DEFTEST")) {
                            const tail = expr.toPtr(Cons).cdr;
                            if (tail.isCons()) {
                                const test_name = tail.toPtr(Cons).car;
                                if (test_name.isSymbol()) {
                                    std.debug.print("TRACE deftest {d}: {s}\n", .{ form_idx, test_name.toPtr(Symbol).getName() });
                                } else {
                                    std.debug.print("TRACE deftest {d}: {s}\n", .{ form_idx, @tagName(test_name.typeKind()) });
                                }
                            }
                        } else if (std.mem.eql(u8, head_name, "DEFUN")) {
                            const tail = expr.toPtr(Cons).cdr;
                            if (tail.isCons()) {
                                const fn_name = tail.toPtr(Cons).car;
                                switch (fn_name.typeKind()) {
                                    .symbol => std.debug.print("TRACE defun {d}: {s}\n", .{
                                        form_idx,
                                        fn_name.toPtr(Symbol).getName(),
                                    }),
                                    .cons => {
                                        const name_head = fn_name.toPtr(Cons).car;
                                        if (name_head.isSymbol()) {
                                            std.debug.print("TRACE defun {d}: ({s} ...)\n", .{
                                                form_idx,
                                                name_head.toPtr(Symbol).getName(),
                                            });
                                        } else {
                                            std.debug.print("TRACE defun {d}: cons\n", .{form_idx});
                                        }
                                    },
                                    else => std.debug.print("TRACE defun {d}: {s}\n", .{
                                        form_idx,
                                        @tagName(fn_name.typeKind()),
                                    }),
                                }
                            }
                        }
                    } else {
                        std.debug.print("TRACE form {d}: {s}\n", .{ form_idx, @tagName(head.typeKind()) });
                    }
                } else {
                    std.debug.print("TRACE form {d}: {s}\n", .{ form_idx, @tagName(expr.typeKind()) });
                }
            }
            if (std.posix.getenv("HABU_TRACE_MACRO_LOOKUP") != null and expr.isCons()) {
                const head = expr.toPtr(Cons).car;
                if (head.isSymbol()) {
                    const head_name = head.toPtr(Symbol).getName();
                    if (std.mem.eql(u8, head_name, "DEFTEST")) {
                        const repl_hit = self.lookupMacroEntry(head) != null;
                        const direct_hit = self.macros.get(head) != null;
                        const compiler_hit = self.compiler.macro_table.get(head) != null;
                        var named_hit = false;
                        var it = self.macros.iterator();
                        while (it.next()) |entry| {
                            const key = entry.key_ptr.*;
                            if (!key.isSymbol()) continue;
                            if (std.mem.eql(u8, key.toPtr(Symbol).getName(), "DEFTEST")) {
                                named_hit = true;
                                break;
                            }
                        }
                        std.debug.print(
                            "TRACE macro DEFTEST: repl={any} direct={any} compiler={any} named={any} counts repl={d} compiler={d}\n",
                            .{
                                repl_hit,
                                direct_hit,
                                compiler_hit,
                                named_hit,
                                self.macros.count(),
                                self.compiler.macro_table.count(),
                            },
                        );
                    }
                }
            }
            var eval_arena = std.heap.ArenaAllocator.init(self.allocator);
            defer eval_arena.deinit();
            const eval_alloc = eval_arena.allocator();
            const form_root_idx = try self.ensureLoadFormRootGlobal();
            const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
            try pushRootValue(source_vm, form_root_idx, form_stack_idx, expr);
            defer popRootValue(source_vm, form_root_idx, form_stack_idx);

            const live_form = source_vm.globals[form_root_idx];
            const form_start_ns: i128 = if (trace_form_timing) std.time.nanoTimestamp() else 0;
            last_value = self.evalParsedWithVm(live_form, source_vm, eval_alloc, form_idx) catch |err| {
                if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null or trace_forms) {
                    std.debug.print("TRACE load eval error: {s} form={d}\n", .{ @errorName(err), form_idx });
                }
                return err;
            };
            if (trace_form_timing) {
                const elapsed_ns: i128 = std.time.nanoTimestamp() - form_start_ns;
                const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
                var load_name: []const u8 = "<unknown>";
                if (self.currentLoadTruename(source_vm)) |truename| {
                    const ns = primitives.pathname.namestring(
                        self.allocator,
                        self.heap,
                        &self.vm.builtins,
                        truename,
                    ) catch Value.nil;
                    if (ns.isString()) load_name = ns.toPtr(runtime.String).bytes();
                }
                std.debug.print(
                    "TRACE form-time file={s} form={d} ms={d:.3}\n",
                    .{ load_name, form_idx, elapsed_ms },
                );
            }
            if (std.posix.getenv("HABU_TRACE_DEFMACRO") != null) {
                var has_def = false;
                var it = self.macros.iterator();
                while (it.next()) |entry| {
                    const key = entry.key_ptr.*;
                    if (!key.isSymbol()) continue;
                    if (std.mem.eql(u8, key.toPtr(Symbol).getName(), "DEFTEST")) {
                        has_def = true;
                        break;
                    }
                }
                if (had_deftest_state == null or had_deftest_state.? != has_def) {
                    std.debug.print(
                        "TRACE macro DEFTEST state: present={any} form={d} pkg={s} repl={d} compiler={d}\n",
                        .{
                            has_def,
                            form_idx,
                            self.heap.getCurrentPackageName(),
                            self.macros.count(),
                            self.compiler.macro_table.count(),
                        },
                    );
                }
                had_deftest_state = has_def;
            }
        }

        return last_value;
    }

    /// Evaluate with a specific VM instance
    fn evalWithVm(self: *Repl, source: []const u8, vm: *Vm) !Value {
        // Use arena for IR nodes
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Parse
        var parser = try Parser.init(arena_alloc, self.heap, source, &self.vm.builtins);
        defer parser.deinit();
        var read_eval_ctx = ReadEvalCtx{ .repl = self, .vm = vm };
        var dispatch_ctx = DispatchMacroCtx{ .vm = vm };
        parser.setReadEvalHook(@ptrCast(&read_eval_ctx), parserReadEval);
        parser.setDispatchMacroHook(@ptrCast(&dispatch_ctx), parserDispatchMacro);
        const expr = try parseWithHookError(&parser);

        return self.evalParsedWithVm(expr, vm, arena_alloc, null);
    }

    fn evalParsedWithVm(self: *Repl, parsed_expr: Value, vm: *Vm, arena_alloc: std.mem.Allocator, form_index: ?usize) !Value {
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
        try pushRootValue(vm, form_root_idx, form_stack_idx, parsed_expr);
        defer popRootValue(vm, form_root_idx, form_stack_idx);

        var expr = vm.resolveForwardedValue(vm.globals[form_root_idx]);
        const saved_compiler_vm = self.compiler.vm;
        self.compiler.setVm(vm);
        try self.compiler.refreshBuiltins();
        // Macro expansion during compile may execute already-defined macro
        // closures; keep VM chunk pool in sync before any compile-time eval.
        self.syncChunkPools(vm);
        try self.syncMacroMapsIfGcChanged();
        expr = vm.resolveForwardedValue(vm.globals[form_root_idx]);
        defer {
            if (saved_compiler_vm) |saved_vm| {
                self.compiler.setVm(saved_vm);
            } else {
                self.compiler.vm = null;
            }
        }

        if (std.posix.getenv("HABU_TRACE_DEFMACRO_DISPATCH") != null and expr.isCons()) {
            const head = expr.toPtr(Cons).car;
            if (head.isSymbol()) {
                const head_name = head.toPtr(Symbol).getName();
                if (std.mem.eql(u8, head_name, "DEFMACRO")) {
                    const b = self.compiler.builtins.?;
                    const canonical = self.canonicalMacroSymbol(head);
                    std.debug.print(
                        "TRACE defmacro-dispatch pkg={s} head=0x{x} canon=0x{x} builtin=0x{x} is={any}\n",
                        .{
                            self.heap.getCurrentPackageName(),
                            head.raw,
                            canonical.raw,
                            b.defmacro.raw,
                            canonical.raw == b.defmacro.raw,
                        },
                    );
                }
            }
        }

        // Check for defmacro - handle specially like main eval
        if (self.isDefmacro(expr)) {
            return self.handleDefmacro(expr, arena_alloc);
        }

        // Package forms must execute immediately so subsequent reader forms
        // use the updated package context during file loads.
        if (self.isDefpackage(expr) or self.isInPackage(expr)) {
            return try self.evalPackageForm(expr, arena_alloc);
        }

        // Check for eval-when - compile-time evaluation
        if (self.isEvalWhen(expr)) {
            const result = try self.handleEvalWhen(expr, arena_alloc);
            // If eval-when returned nil (only :compile-toplevel), we're done
            if (result.isNil()) return Value.nil;
            // Otherwise compile the returned progn for runtime
            vm.globals[form_root_idx] = result;
            expr = result;
        }

        // Let the compiler drive macro expansion with form-aware contexts.
        // Pre-expanding here is context-free and can incorrectly expand symbols
        // in binding positions (e.g. LET vars) as macro calls.

        const trace_form_idx = traceFormIndexFromEnv();
        const trace_selected_form = if (trace_form_idx) |want|
            if (form_index) |idx| idx == want else false
        else
            false;
        if (trace_selected_form) {
            var load_name: []const u8 = "<unknown>";
            if (self.currentLoadTruename(vm)) |truename| {
                const ns = primitives.pathname.namestring(
                    self.allocator,
                    self.heap,
                    &self.vm.builtins,
                    truename,
                ) catch Value.nil;
                if (ns.isString()) load_name = ns.toPtr(runtime.String).bytes();
            }
            std.debug.print("TRACE selected form idx={d} pkg={s}\n", .{
                form_index.?,
                self.heap.getCurrentPackageName(),
            });
            std.debug.print("TRACE selected form file={s}\n", .{load_name});
            var form_buf = std.ArrayList(u8){};
            defer form_buf.deinit(self.allocator);
            self.printValue(expr, form_buf.writer(self.allocator)) catch {};
            std.debug.print("TRACE selected form expr: {s}\n", .{form_buf.items});
        }

        // Compile
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;
        defer {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
        }

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = if (self.compileExprRooted(vm, expr, &env)) |node| node else |err| {
            return err;
        };
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        emitter.setRetainedValueLookup(Compiler.retainedValueLookup, &self.compiler);
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch child chunks to use absolute indices
        for (child_chunks) |child_chunk| {
            try patchChunkIndices(child_chunk.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Store chunks persistently and sync owner-backed VM slices before any
        // operation that may allocate/GC (JIT, patching, disasm, run).
        try self.appendChildChunksAndSync(vm, child_chunks);

        // Try hoist SSA JIT compilation for eligible lambda nodes
        _ = try self.tryHoistCompileLambdas(specialized, child_chunks, chunk_base);

        // Patch main chunk to use absolute chunk indices
        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        try patchChunkIndices(chunk_ptr, chunk_base);

        const trace_form_disasm = std.posix.getenv("HABU_TRACE_FORM_DISASM") != null;
        if (trace_form_disasm and (trace_form_idx == null or trace_selected_form) and (!vm.isExecuting() or trace_selected_form)) {
            std.debug.print("TRACE form disasm begin\n", .{});
            var buf = std.ArrayList(u8){};
            defer buf.deinit(self.allocator);
            disasm.disassembleRuntime(chunk_ptr, buf.writer(self.allocator)) catch |err| {
                std.debug.print("TRACE form disasm error={s}\n", .{@errorName(err)});
                return err;
            };
            std.debug.print("{s}", .{buf.items});
            if (trace_selected_form and chunk_ptr.const_count > 0) {
                const consts = chunk_ptr.getConstants();
                var ci: usize = 0;
                while (ci < consts.len) : (ci += 1) {
                    var cbuf = std.ArrayList(u8){};
                    defer cbuf.deinit(self.allocator);
                    self.printValue(consts[ci], cbuf.writer(self.allocator)) catch {};
                    std.debug.print("TRACE form const[{d}]={s}\n", .{ ci, cbuf.items });
                }
            }
            std.debug.print("TRACE form disasm end\n", .{});
        }

        return try self.runVmPreserveMacroState(vm, chunk_ptr);
    }

    pub fn deinit(self: *Repl) void {
        self.vm.deinit();
        self.line_editor.deinit();
        self.compiler.deinit();
        self.chunk_pool.deinit(self.allocator);
        self.macros.deinit();
        self.persistent_roots.deinit(self.allocator);
        self.allocator.free(self.trusted_load_root);
    }

    /// Run specialization pass on IR, replacing generic ops with
    /// type-specialized variants where types are proven by assertions.
    /// E.g., (add (assert_fixnum x) (assert_fixnum y)) → fixnum_add
    fn specializeIr(self: *Repl, ir_node: *const Ir) !*const Ir {
        return try passes.specialize.specialize(self.compiler.allocator, ir_node);
    }

    fn lookupGlobalRefSymbol(self: *Repl, qname: []const u8) !?Value {
        if (std.mem.indexOf(u8, qname, "::")) |sep| {
            return try self.lookupPackageSymbol(qname[0..sep], qname[sep + 2 ..]);
        }
        if (std.mem.indexOfScalar(u8, qname, ':')) |sep| {
            return try self.lookupPackageSymbol(qname[0..sep], qname[sep + 1 ..]);
        }
        return self.heap.symbols.get(qname);
    }

    fn lookupPackageSymbol(self: *Repl, pkg_name: []const u8, sym_name: []const u8) !?Value {
        if (sym_name.len == 0) return null;
        if (try self.heap.lookupInPackage(pkg_name, sym_name)) |sym| return sym;

        if (pkg_name.len <= 128 and sym_name.len <= 256) {
            var pkg_buf: [128]u8 = undefined;
            var sym_buf: [256]u8 = undefined;
            for (pkg_name, 0..) |ch, i| pkg_buf[i] = std.ascii.toUpper(ch);
            for (sym_name, 0..) |ch, i| sym_buf[i] = std.ascii.toUpper(ch);
            return try self.heap.lookupInPackage(pkg_buf[0..pkg_name.len], sym_buf[0..sym_name.len]);
        }
        return null;
    }

    const LiteralRootCtx = struct {
        repl: *Repl,
    };

    const literal_root_ops = struct {
        pub fn onLit(ctx: LiteralRootCtx, ir_node: *const Ir, roots: *jit_backend.LiteralRoots, v: Value) !void {
            try jit_literal_roots.rootLiteral(&ctx.repl.vm, ir_node, roots, v);
        }

        pub fn onGlobalRef(ctx: LiteralRootCtx, ir_node: *const Ir, roots: *jit_backend.LiteralRoots, qname: []const u8) !void {
            const sym = (try ctx.repl.lookupGlobalRefSymbol(qname)) orelse return error.UnsupportedIrNode;
            try jit_literal_roots.rootValue(&ctx.repl.vm, ir_node, roots, sym);
        }

        pub fn onLambda(ctx: LiteralRootCtx, ir_node: *const Ir, roots: *jit_backend.LiteralRoots, lam: anytype) !void {
            if (lam.lambda_expr.isNil()) return error.UnsupportedIrNode;

            const key = @intFromPtr(ir_node);
            if (roots.contains(key)) return;

            const closure_val = try ctx.repl.evalExpr(lam.lambda_expr);
            if (!closure_val.isClosure()) return error.TypeMismatch;
            try jit_literal_roots.rootValue(&ctx.repl.vm, ir_node, roots, closure_val);
        }
    };

    fn tryHoistCompileLambdas(
        self: *Repl,
        ir_node: *const Ir,
        child_chunks: []const Value,
        chunk_base: u16,
    ) !bool {
        if (!self.config.enable_jit) return false;
        const trace = std.posix.getenv("HABU_TRACE_JIT") != null;
        var candidates = std.ArrayList(jit_candidates.LambdaCandidate){};
        defer {
            jit_candidates.freeLambdaCandidates(self.allocator, candidates.items);
            candidates.deinit(self.allocator);
        }
        try jit_candidates.collectLambdaCandidates(self.allocator, ir_node, &candidates);
        if (candidates.items.len == 0) {
            if (trace) std.debug.print("JIT: no lambda candidates in top-level IR ({s})\n", .{@tagName(ir_node.*)});
            return false;
        }

        const used_chunks = try self.allocator.alloc(bool, child_chunks.len);
        defer self.allocator.free(used_chunks);
        @memset(used_chunks, false);
        const live_chunks = try self.allocator.alloc(Value, child_chunks.len);
        defer self.allocator.free(live_chunks);
        const chunk_base_usize: usize = chunk_base;

        var compiled_any = false;
        for (candidates.items) |candidate| {
            for (child_chunks, 0..) |child_chunk, idx| {
                const pool_idx = chunk_base_usize + idx;
                const pooled = if (pool_idx < self.vm.chunk_pool.len) self.vm.chunk_pool[pool_idx] else child_chunk;
                live_chunks[idx] = self.vm.resolveForwardedValue(pooled);
            }
            const live_name_sym = self.vm.resolveForwardedValue(candidate.name_sym);
            const compile_name = if (live_name_sym.isSymbol())
                live_name_sym.toPtr(Symbol).getName()
            else
                candidate.name;

            self.vm.jit_adm.cand += 1;
            const lambda_ir = candidate.lambda_ir;
            if (jit_candidates.ineligibleReason(lambda_ir)) |reason| {
                switch (reason) {
                    .not_lambda => self.vm.jit_adm.fail_other += 1,
                    .speed => self.vm.jit_adm.sk_speed += 1,
                    .safety => self.vm.jit_adm.sk_safety += 1,
                    .assert_fixnum_body => self.vm.jit_adm.sk_assert += 1,
                    .captures => self.vm.jit_adm.sk_caps += 1,
                    .optional_params => self.vm.jit_adm.sk_opt += 1,
                    .key_params => self.vm.jit_adm.sk_key += 1,
                    .rest_param => self.vm.jit_adm.sk_rest += 1,
                }
                if (trace and lambda_ir.* == .lambda) {
                    const lambda = lambda_ir.lambda;
                    std.debug.print("JIT: skip '{s}' reason={s} speed={d} safety={d} captures={d} opt={d} key={d} rest={}\n", .{
                        compile_name,
                        jit_candidates.reasonLabel(reason),
                        lambda.speed,
                        lambda.safety,
                        lambda.captures.len,
                        lambda.optional_params.len,
                        lambda.key_params.len,
                        lambda.rest_param != null,
                    });
                }
                continue;
            }

            const chunk_ptr = jit_candidates.findMatchingChunk(&candidate, live_name_sym, live_chunks, used_chunks) orelse {
                self.vm.jit_adm.sk_chunk += 1;
                if (trace) {
                    std.debug.print("JIT: no matching chunk for '{s}' local={s}\n", .{ compile_name, candidate.local_name });
                }
                continue;
            };
            self.vm.jit_adm.elig += 1;

            switch (try self.vm.jitCompileStatus(chunk_ptr)) {
                .compiled => {
                    compiled_any = true;
                    self.vm.jit_adm.comp += 1;
                    self.vm.jit_adm.cache_comp += 1;
                    if (trace) {
                        std.debug.print("JIT: cache hit compiled '{s}' chunk=0x{x}\n", .{
                            compile_name,
                            @intFromPtr(chunk_ptr),
                        });
                    }
                    continue;
                },
                .unsupported => {
                    self.vm.jit_adm.fail_unsupported += 1;
                    self.vm.jit_adm.cache_unsupported += 1;
                    if (trace) {
                        std.debug.print("JIT: cache hit unsupported '{s}' chunk=0x{x}\n", .{
                            compile_name,
                            @intFromPtr(chunk_ptr),
                        });
                    }
                    continue;
                },
                .failed => {
                    self.vm.jit_adm.fail_other += 1;
                    self.vm.jit_adm.cache_failed += 1;
                    if (trace) {
                        std.debug.print("JIT: cache hit failed '{s}' chunk=0x{x}\n", .{
                            compile_name,
                            @intFromPtr(chunk_ptr),
                        });
                    }
                    continue;
                },
                .none => {},
            }

            if (trace and lambda_ir.* == .lambda) {
                const lambda = lambda_ir.lambda;
                std.debug.print("JIT: considering '{s}' speed={d} safety={d} captures={d} opt={d} key={d} rest={} chunk=0x{x}\n", .{
                    compile_name,
                    lambda.speed,
                    lambda.safety,
                    lambda.captures.len,
                    lambda.optional_params.len,
                    lambda.key_params.len,
                    lambda.rest_param != null,
                    @intFromPtr(chunk_ptr),
                });
            }

            const compile_result = self.doHoistCompile(lambda_ir, compile_name, chunk_ptr);
            try self.vm.noteJitCompileStatus(chunk_ptr, switch (compile_result) {
                .compiled => .compiled,
                .unsupported => .unsupported,
                .failed => .failed,
            });
            switch (compile_result) {
                .compiled => {
                    compiled_any = true;
                    self.vm.jit_adm.comp += 1;
                },
                .unsupported => self.vm.jit_adm.fail_unsupported += 1,
                .failed => self.vm.jit_adm.fail_other += 1,
            }
        }

        return compiled_any;
    }

    fn populateKnownFns(self: *Repl, known_fns: *std.StringHashMap(jit_backend.KnownFn)) !void {
        for (self.vm.jit_fns.items) |entry| {
            const cfn = entry.compiled;
            var callee_sym_raw: u64 = 0;
            const live_chunk_val = self.vm.resolveForwardedValue(entry.chunk);
            if (live_chunk_val.isChunk()) {
                const chunk = live_chunk_val.toPtr(runtime.objects.Chunk);
                const live_name = self.vm.resolveForwardedValue(chunk.name);
                if (live_name.isSymbol()) {
                    callee_sym_raw = live_name.raw;
                }
            }
            try known_fns.put(cfn.name, .{
                .fn_ptr = @intFromPtr(cfn.fn_ptr),
                .arity = cfn.arity,
                .ir_body = cfn.ir_body,
                .param_names = cfn.param_names,
                .callee_name = cfn.name,
                .callee_sym_raw = callee_sym_raw,
            });
        }
    }

    /// Inner function that propagates errors to allow try usage.
    fn doHoistCompile(
        self: *Repl,
        lambda_ir: *const Ir,
        name: []const u8,
        chunk_ptr: *const runtime.objects.Chunk,
    ) enum { compiled, unsupported, failed } {
        // Build known_fns map from existing hoist-compiled functions
        var known_fns = std.StringHashMap(jit_backend.KnownFn).init(self.allocator);
        defer known_fns.deinit();
        self.populateKnownFns(&known_fns) catch return .failed;

        const trace = std.posix.getenv("HABU_TRACE_JIT") != null;
        const body_ir = if (lambda_ir.* == .lambda) lambda_ir.lambda.body else lambda_ir;
        var literal_roots = jit_backend.LiteralRoots.init(self.allocator);
        defer literal_roots.deinit();
        if (lambda_ir.* == .lambda) {
            jit_literal_roots.collect(
                lambda_ir.lambda.body,
                &literal_roots,
                LiteralRootCtx{ .repl = self },
                literal_root_ops,
            ) catch |err| {
                if (trace) {
                    std.debug.print("JIT: literal root prep failed for '{s}': {s}\n", .{ name, @errorName(err) });
                }
                if (err == error.UnsupportedIrNode) {
                    if (jit_backend.IrTranslator.firstUnsupportedTagWithLiteralRoots(body_ir, null)) |tag| {
                        self.vm.noteUnsupportedTag(tag);
                    }
                    return .unsupported;
                }
                return .failed;
            };
            jit_literal_roots.ensureCoverage(lambda_ir.lambda.body, &literal_roots) catch |err| {
                if (trace) {
                    std.debug.print("JIT: literal root coverage failed for '{s}': {s}\n", .{ name, @errorName(err) });
                }
                if (err == error.UnsupportedIrNode) {
                    if (jit_backend.IrTranslator.firstUnsupportedTagWithLiteralRoots(body_ir, &literal_roots)) |tag| {
                        self.vm.noteUnsupportedTag(tag);
                    }
                    return .unsupported;
                }
                return .failed;
            };
        }
        const literal_roots_ptr: ?*const jit_backend.LiteralRoots = if (literal_roots.count() > 0)
            &literal_roots
        else
            null;
        if (std.posix.getenv("HABU_TRACE_JIT_LIT_ROOTS") != null) {
            std.debug.print("JIT_LIT_ROOTS fn={s} count={d}\n", .{ name, literal_roots.count() });
        }

        var compiled = jit_backend.compileIrWithKnownFnsAndLiteralRoots(self.allocator, lambda_ir, name, &known_fns, literal_roots_ptr) catch |err| {
            if (trace) {
                std.debug.print("JIT: hoist compile failed for '{s}': {s}\n", .{ name, @errorName(err) });
                if (err == error.UnsupportedIrNode) {
                    if (jit_backend.IrTranslator.firstUnsupportedTagWithLiteralRoots(body_ir, literal_roots_ptr)) |tag| {
                        std.debug.print("JIT: first unsupported tag for '{s}' is {s}\n", .{ name, @tagName(tag) });
                    }
                }
            }
            if (err == error.UnsupportedIrNode) {
                if (jit_backend.IrTranslator.firstUnsupportedTagWithLiteralRoots(body_ir, literal_roots_ptr)) |tag| {
                    self.vm.noteUnsupportedTag(tag);
                }
                return .unsupported;
            }
            return .failed;
        };
        const persistent = self.allocator.create(jit_backend.CompiledFn) catch {
            compiled.deinit();
            return .failed;
        };
        persistent.* = compiled;

        // Deep-copy IR body for potential inlining by callers.
        // The original IR lives on a temporary arena that will be freed.
        if (lambda_ir.* == .lambda) {
            const lambda = lambda_ir.lambda;
            // Create a dedicated arena for the IR copy
            const ir_arena = self.allocator.create(std.heap.ArenaAllocator) catch {
                persistent.deinit();
                self.allocator.destroy(persistent);
                return .failed;
            };
            ir_arena.* = std.heap.ArenaAllocator.init(self.allocator);
            const ir_alloc = ir_arena.allocator();
            var copy_ok = false;
            copy_blk: {
                const body_copy = ir.deepCopyIr(ir_alloc, lambda.body) catch |err| {
                    if (trace) {
                        std.debug.print("JIT: IR copy skipped for '{s}' body: {s}\n", .{ name, @errorName(err) });
                    }
                    break :copy_blk;
                };
                const params_copy = ir_alloc.alloc([]const u8, lambda.params.len) catch |err| {
                    if (trace) {
                        std.debug.print("JIT: IR copy skipped for '{s}' params: {s}\n", .{ name, @errorName(err) });
                    }
                    break :copy_blk;
                };
                for (lambda.params, 0..) |p, pi| {
                    params_copy[pi] = ir_alloc.dupe(u8, p) catch |err| {
                        if (trace) {
                            std.debug.print("JIT: IR copy skipped for '{s}' param[{d}]: {s}\n", .{ name, pi, @errorName(err) });
                        }
                        break :copy_blk;
                    };
                }
                persistent.ir_arena = ir_arena;
                persistent.ir_body = body_copy;
                persistent.param_names = params_copy;
                copy_ok = true;
            }
            if (!copy_ok) {
                ir_arena.deinit();
                self.allocator.destroy(ir_arena);
                persistent.ir_arena = null;
                persistent.ir_body = null;
                persistent.param_names = null;
            }
        }

        // Always register on the primary VM - activeVm() may return a context VM
        // that gets destroyed after file loading, losing the registration.
        self.vm.registerJitFn(chunk_ptr, persistent) catch {
            persistent.deinit();
            self.allocator.destroy(persistent);
            return .failed;
        };

        // Post-registration: patch cross-calls from BLR to BL (direct call).
        // Now that this function is registered, patch any of its cross-calls
        // to known functions with direct BL instructions.
        {
            const jit_mem = persistent.mem;
            const code_ptr = jit_mem.ptr;
            const code_len = jit_mem.used;
            const fn_base = @intFromPtr(code_ptr);
            const trace_patch = std.posix.getenv("HABU_TRACE_JIT_PATCH") != null;
            // Make writable for patching
            jit_mem.setExec(false) catch |err| {
                if (trace) {
                    std.debug.print("JIT: setExec(false) failed for '{s}': {s}\n", .{ name, @errorName(err) });
                }
                _ = self.vm.unregisterJitFn(chunk_ptr);
                persistent.deinit();
                self.allocator.destroy(persistent);
                return .failed;
            };
            var patched_bl: usize = 0;
            const call_counts_before = if (trace_patch)
                countCallOps(code_ptr[0..code_len])
            else
                CallOpCounts{};
            if (std.posix.getenv("HABU_NO_PATCH_CROSS_BL") == null) {
                patched_bl = jit_backend.patchCrossCallsToBL(code_ptr, code_len, fn_base);
            }
            const call_counts_after = if (trace_patch)
                countCallOps(code_ptr[0..code_len])
            else
                CallOpCounts{};
            if (trace_patch) {
                std.debug.print(
                    "JIT: cross-bl patch '{s}' patched={d} blr={d}->{d} bl={d}->{d}\n",
                    .{
                        name,
                        patched_bl,
                        call_counts_before.blr,
                        call_counts_after.blr,
                        call_counts_before.bl,
                        call_counts_after.bl,
                    },
                );
            }
            // Flush icache and restore exec permission
            jit_mem.flushCacheRange(code_ptr, code_len);
            jit_mem.setExec(true) catch |err| {
                if (trace) {
                    std.debug.print("JIT: setExec(true) failed for '{s}': {s}\n", .{ name, @errorName(err) });
                }
                _ = self.vm.unregisterJitFn(chunk_ptr);
                persistent.deinit();
                self.allocator.destroy(persistent);
                return .failed;
            };
        }

        if (std.posix.getenv("HABU_TRACE_JIT") != null) {
            std.debug.print("JIT: hoist compiled '{s}' OK (arity={d}, fn_ptr={*}, chunk=0x{x}, reg_count={d})\n", .{
                name,                   compiled.arity,            compiled.fn_ptr,
                @intFromPtr(chunk_ptr), self.vm.jit_fns.items.len,
            });
        }
        return .compiled;
    }

    /// Run the REPL loop with File-based I/O
    pub fn runWithFiles(self: *Repl, stdin: std.fs.File, stdout: std.fs.File) !void {
        _ = stdin; // Line editor reads directly from stdin

        var out_buf: [4096]u8 = undefined;
        var out_writer = stdout.writer(&out_buf);
        const writer = &out_writer.interface;

        // Input accumulator for multi-line expressions
        var input_buf = std.ArrayList(u8){};
        defer input_buf.deinit(self.allocator);

        while (true) {
            // Get appropriate prompt
            const prompt = if (input_buf.items.len == 0) self.config.prompt else self.config.cont_prompt;

            // Read line with editing
            const line = (try self.handleReadlineResult(
                self.line_editor.readline(prompt),
                &input_buf,
                writer,
            )) orelse {
                // EOF (Ctrl-D on empty line)
                if (input_buf.items.len > 0) {
                    try self.evalPrint(input_buf.items, writer);
                }
                return;
            };

            const trimmed = std.mem.trim(u8, line, " \t\r\n");

            // Empty line on fresh input: skip
            if (trimmed.len == 0 and input_buf.items.len == 0) continue;

            // Handle commands only on fresh input that doesn't contain S-expressions
            // If the line contains '(', it's likely code with keywords, not a REPL command
            if (input_buf.items.len == 0 and trimmed.len > 0 and trimmed[0] == ':' and std.mem.indexOf(u8, trimmed, "(") == null) {
                try self.handleCommand(trimmed, writer);
                try writer.flush();
                continue;
            }

            // Accumulate input
            if (input_buf.items.len > 0) {
                try input_buf.append(self.allocator, '\n');
            }
            try input_buf.appendSlice(self.allocator, line);

            // Check if parens are balanced
            const balance = countParenBalance(input_buf.items);
            if (balance < 0) {
                // Too many closing parens - error
                try writer.writeAll("\x1b[1;31merror\x1b[0m: unexpected ')'\n");
                try writer.flush();
                input_buf.clearRetainingCapacity();
                continue;
            }
            if (balance > 0) {
                // Incomplete - continue reading
                continue;
            }

            // Parens balanced - evaluate
            const trimmed_input = std.mem.trim(u8, input_buf.items, " \t\r\n");
            if (trimmed_input.len > 0) {
                self.evalPrint(trimmed_input, writer) catch |err| {
                    // Evaluation errors (user error, unhandled throw, type mismatch, etc.)
                    // are already printed by evalPrint. Continue the REPL.
                    // Only propagate fatal errors.
                    if (err == error.OutOfMemory) return err;
                    // Non-fatal: continue REPL
                };
                try writer.flush();
            }
            input_buf.clearRetainingCapacity();
        }
    }

    fn handleReadlineResult(
        self: *Repl,
        result: anyerror!?[]const u8,
        input_buf: *std.ArrayList(u8),
        writer: anytype,
    ) !?[]const u8 {
        return if (result) |line| line else |err| {
            if (input_buf.items.len > 0) {
                try self.evalPrint(input_buf.items, writer);
            }
            return err;
        };
    }

    /// Count paren balance: positive = open parens, negative = too many close parens
    fn countParenBalance(input: []const u8) i32 {
        var balance: i32 = 0;
        var in_string = false;
        var in_comment = false;
        var i: usize = 0;

        while (i < input.len) : (i += 1) {
            const c = input[i];

            if (in_comment) {
                if (c == '\n') in_comment = false;
                continue;
            }

            if (in_string) {
                if (c == '\\' and i + 1 < input.len) {
                    i += 1; // Skip escaped char
                } else if (c == '"') {
                    in_string = false;
                }
                continue;
            }

            switch (c) {
                '"' => in_string = true,
                ';' => in_comment = true,
                '(' => balance += 1,
                ')' => {
                    balance -= 1;
                    if (balance < 0) return balance;
                },
                else => {},
            }
        }

        // If in string, consider incomplete
        if (in_string) return 1;

        return balance;
    }

    /// Run the REPL loop (for testing with anytype readers)
    pub fn run(self: *Repl, in_reader: anytype, writer: anytype) !void {
        _ = self;
        _ = in_reader;
        _ = writer;
        // This version is for tests only - use runWithFiles for actual REPL
    }

    /// Error information for better diagnostics
    pub const ErrorInfo = struct {
        kind: ErrorKind,
        line: u32,
        column: u32,
        text: []const u8,
    };

    pub const ErrorKind = enum {
        parse_unexpected_token,
        parse_unterminated_list,
        parse_invalid_number,
        parse_invalid_array,
        compile_unbound_variable,
        compile_invalid_syntax,
        runtime_type_mismatch,
        runtime_user_error,
        other,
    };

    /// Evaluate a string and print the result, with nice error messages
    pub fn evalPrint(self: *Repl, source: []const u8, writer: anytype) !void {
        var err_info: ?ErrorInfo = null;
        const result = if (self.evalCapturingError(source, &err_info)) |value| value else |err| {
            if (err_info) |info| {
                // For user errors / unhandled throws, try to show the actual message
                if (info.kind == .runtime_user_error and !self.vm.last_error_value.isNil()) {
                    const msg = self.vm.last_error_value;
                    self.vm.last_error_value = Value.nil;
                    if (msg.isString()) {
                        try writer.print("\x1b[1;31merror\x1b[0m: ", .{});
                        try self.printValue(msg, writer);
                        try writer.writeAll("\n");
                    } else {
                        try writer.print("\x1b[1;31merror\x1b[0m: ", .{});
                        try self.printValue(msg, writer);
                        try writer.writeAll("\n");
                    }
                } else {
                    try self.printDiagnostic(source, info, writer);
                }
            } else {
                try writer.print("Error: {s}\n", .{@errorName(err)});
            }
            return err;
        };
        try self.printValue(result, writer);
        try writer.writeAll("\n");
        if (self.vm.secondary_values_count > 0) {
            var i: usize = 0;
            while (i < self.vm.secondary_values_count) : (i += 1) {
                try self.printValue(self.vm.secondary_values[i], writer);
                try writer.writeAll("\n");
            }
            self.vm.secondary_values_count = 0;
        }
    }

    fn printDiagnostic(self: *Repl, source: []const u8, info: ErrorInfo, writer: anytype) !void {
        _ = self;
        // Format: error: message at line:column
        const msg = switch (info.kind) {
            .parse_unexpected_token => "unexpected token",
            .parse_unterminated_list => "unterminated list",
            .parse_invalid_number => "invalid number",
            .parse_invalid_array => "invalid array literal",
            .compile_unbound_variable => "unbound variable",
            .compile_invalid_syntax => "invalid syntax",
            .runtime_type_mismatch => "type mismatch",
            .runtime_user_error => "user error",
            .other => "error",
        };

        try writer.print("\x1b[1;31merror\x1b[0m: {s}\n", .{msg});
        try writer.print("  \x1b[1;34m-->\x1b[0m <repl>:{d}:{d}\n", .{ info.line, info.column });
        try writer.print("   \x1b[1;34m|\x1b[0m\n", .{});

        // Print the source line
        var line_num: u32 = 1;
        var line_start: usize = 0;
        for (source, 0..) |c, i| {
            if (line_num == info.line) {
                // Find end of line
                var line_end = i;
                while (line_end < source.len and source[line_end] != '\n') line_end += 1;
                try writer.print("\x1b[1;34m{d:>3} |\x1b[0m {s}\n", .{ line_num, source[line_start..line_end] });
                break;
            }
            if (c == '\n') {
                line_num += 1;
                line_start = i + 1;
            }
        } else {
            // Single line input
            try writer.print("\x1b[1;34m  1 |\x1b[0m {s}\n", .{source});
        }

        // Print caret pointing to error
        try writer.print("   \x1b[1;34m|\x1b[0m ", .{});
        var col: u32 = 1;
        while (col < info.column) : (col += 1) {
            try writer.writeAll(" ");
        }
        try writer.print("\x1b[1;31m^\x1b[0m", .{});
        if (info.text.len > 1) {
            for (info.text[1..]) |_| {
                try writer.print("\x1b[1;31m^\x1b[0m", .{});
            }
        }
        try writer.print(" {s}\n", .{info.text});
    }

    /// Evaluate a string, capture error info for diagnostics
    fn evalCapturingError(self: *Repl, source: []const u8, err_info: *?ErrorInfo) !Value {
        self.compiler.setVm(&self.vm);
        try self.compiler.refreshBuiltins();

        // Use arena for IR nodes to simplify cleanup
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Parse
        var parser = try Parser.init(arena_alloc, self.heap, source, &self.vm.builtins);
        defer parser.deinit();
        var read_eval_ctx = ReadEvalCtx{ .repl = self, .vm = &self.vm };
        var dispatch_ctx = DispatchMacroCtx{ .vm = &self.vm };
        parser.setReadEvalHook(@ptrCast(&read_eval_ctx), parserReadEval);
        parser.setDispatchMacroHook(@ptrCast(&dispatch_ctx), parserDispatchMacro);

        var expr = if (parseWithHookError(&parser)) |parsed| parsed else |err| {
            const loc = parser.getErrorLocation();
            err_info.* = .{
                .kind = switch (err) {
                    error.UnexpectedToken => .parse_unexpected_token,
                    error.UnterminatedList => .parse_unterminated_list,
                    error.InvalidNumber => .parse_invalid_number,
                    error.InvalidCharacter => .parse_invalid_number,
                    error.Overflow => .parse_invalid_number,
                    error.InvalidArray => .parse_invalid_array,
                    else => .other,
                },
                .line = loc.line,
                .column = loc.column,
                .text = loc.text,
            };
            return err;
        };

        // Check for defmacro
        if (self.isDefmacro(expr)) {
            return self.handleDefmacro(expr, arena_alloc);
        }

        // Check for defpackage/in-package - these need to execute immediately
        if (self.isDefpackage(expr) or self.isInPackage(expr)) {
            return try self.evalPackageForm(expr, arena_alloc);
        }

        // Check for eval-when - compile-time evaluation
        if (self.isEvalWhen(expr)) {
            const result = try self.handleEvalWhen(expr, arena_alloc);
            if (result.isNil()) return Value.nil;
            expr = result;
        }

        // Let the compiler drive macro expansion with form-aware contexts.
        // Pre-expanding here is context-free and can incorrectly expand symbols
        // in binding positions (e.g. LET vars) as macro calls.

        // Compile - use persistent compiler for globals, but temp builder/allocator
        // Save and restore since they use arena allocator
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;
        defer {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
        }

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = if (self.compileExprRooted(&self.vm, expr, &env)) |node| node else |err| {
            err_info.* = .{
                .kind = if (err == error.UnboundVariable) .compile_unbound_variable else .compile_invalid_syntax,
                .line = 1,
                .column = 1,
                .text = "",
            };
            return err;
        };
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode (with heap for symbol interning)
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        emitter.setRetainedValueLookup(Compiler.retainedValueLookup, &self.compiler);
        defer emitter.deinit();

        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch wrapper chunk AND child chunks to use absolute indices
        const wrapper_chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        try patchChunkIndices(wrapper_chunk_ptr, chunk_base);
        for (child_chunks) |c| {
            try patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Store child chunks persistently and sync VM chunk-pool slices before
        // any JIT/GC-capable work.
        try self.appendChildChunksAndSync(&self.vm, child_chunks);

        // Try hoist SSA JIT compilation for eligible lambda nodes
        _ = try self.tryHoistCompileLambdas(specialized, child_chunks, chunk_base);

        // Free child chunk array (now owned by persistent storage)
        self.allocator.free(child_chunks);

        const result = if (self.runVmPreserveMacroState(&self.vm, chunk.toPtr(runtime.objects.Chunk))) |value| value else |err| {
            const kind: ErrorKind = if (err == error.UserError or err == error.UnhandledThrow)
                .runtime_user_error
            else
                .runtime_type_mismatch;
            err_info.* = .{
                .kind = kind,
                .line = 1,
                .column = 1,
                .text = "",
            };
            return err;
        };
        return result;
    }

    /// Evaluate a string, return the result
    pub fn eval(self: *Repl, source: []const u8) !Value {
        return self.evalWithVm(source, &self.vm);
    }

    /// Evaluate with a specific VM instance
    /// Print a value in Lisp notation
    pub fn printValue(self: *Repl, val: Value, writer: anytype) anyerror!void {
        const io = runtime.primitives.io;
        switch (val.typeKind()) {
            .nil => try writer.writeAll("nil"),
            .t => try writer.writeAll("t"),
            .unbound => try writer.writeAll("#<unbound>"),
            .fixnum => try io.writeFixnumTo(val.toFixnum(), writer),
            .float => try io.writeFloatTo(val.toFloat(), writer),
            .char => {
                const cp = val.toCharacter();
                if (cp == ' ') {
                    try writer.writeAll("#\\space");
                } else if (cp == '\n') {
                    try writer.writeAll("#\\newline");
                } else if (cp == '\t') {
                    try writer.writeAll("#\\tab");
                } else if (cp == '\r') {
                    try writer.writeAll("#\\return");
                } else if (cp >= 32 and cp < 127) {
                    try writer.print("#\\{c}", .{@as(u8, @intCast(cp))});
                } else {
                    try writer.print("#\\U+{X:0>4}", .{cp});
                }
            },
            .cons => try self.printList(val, writer),
            .symbol => {
                if (val.isUnbound()) {
                    try writer.writeAll("#<unbound>");
                } else {
                    try writer.writeAll(val.toPtr(Symbol).getName());
                }
            },
            .string => try writer.print("\"{s}\"", .{val.toPtr(String).bytes()}),
            .string32 => {
                // Convert UTF-32 to UTF-8 for output
                try writer.writeByte('"');
                const s32 = val.toPtr(runtime.String32);
                var utf8_buf: [4]u8 = undefined;
                for (s32.codepoints()) |cp| {
                    const len = try std.unicode.utf8Encode(@intCast(cp), &utf8_buf);
                    try writer.writeAll(utf8_buf[0..len]);
                }
                try writer.writeByte('"');
            },
            .keyword => {
                try writer.writeByte(':');
                try writer.writeAll(val.toPtr(runtime.Keyword).getName());
            },
            .closure => try writer.writeAll("#<closure>"),
            .vector => try self.printVector(val, writer),
            .structure => try writer.writeAll("#<structure>"),
            .hashtable => try writer.print("#<hash-table count={d}>", .{val.toPtr(runtime.HashTable).count}),
            .rational => {
                const rat = val.toPtr(runtime.Rational);
                try writer.print("{d}/{d}", .{ rat.numerator, rat.denominator });
            },
            .complex => {
                const cplx = val.toPtr(runtime.Complex);
                try writer.print("#C({d} {d})", .{ cplx.real, cplx.imag });
            },
            .stream => {
                const stream = val.toPtr(runtime.Stream);
                const dir = switch (stream.direction) {
                    .input => "input",
                    .output => "output",
                    .io => "io",
                };
                const kind = switch (stream.stream_type) {
                    .string => "string",
                    .file => "file",
                    .stdin => "stdin",
                    .stdout => "stdout",
                    .stderr => "stderr",
                    .byte => "byte",
                    .broadcast => "broadcast",
                    .concatenated => "concatenated",
                    .echo => "echo",
                    .synonym => "synonym",
                    .two_way => "two-way",
                };
                try writer.print("#<{s}-{s}-stream>", .{ kind, dir });
            },
            .bignum => {
                const bn = val.toPtr(runtime.Bignum);
                try writer.print("#<bignum size={d}>", .{bn.size});
            },
            .array => {
                const arr = val.toPtr(runtime.Array);
                try writer.print("#<array rank={d}>", .{arr.rank});
            },
            .pathname => try writer.writeAll("#<pathname>"),
            .package => {
                const pkg = val.toPtr(runtime.Package);
                const name_sym = pkg.name.toPtr(Symbol);
                try writer.print("#<package {s}>", .{name_sym.getName()});
            },
            .chunk => try writer.writeAll("#<chunk>"),
            .condition => try writer.writeAll("#<condition>"),
            .class => try writer.writeAll("#<class>"),
            .slotdef => try writer.writeAll("#<slot-definition>"),
            .generic_function => try writer.writeAll("#<generic-function>"),
            .method => try writer.writeAll("#<method>"),
            .native_code => {
                const nc = val.toPtr(runtime.NativeCode);
                try writer.print("#<native-code 0x{x}>", .{nc.entry});
            },
            .macro_env => try writer.writeAll("#<macro-env>"),
        }
    }

    fn printList(self: *Repl, val: Value, writer: anytype) anyerror!void {
        try writer.writeAll("(");

        var current = val;
        var first = true;

        while (current.isCons()) {
            if (!first) try writer.writeAll(" ");
            first = false;

            const cons = current.toPtr(Cons);
            try self.printValue(cons.car, writer);
            current = cons.cdr;
        }

        // Handle improper list
        if (!current.isNil()) {
            try writer.writeAll(" . ");
            try self.printValue(current, writer);
        }

        try writer.writeAll(")");
    }

    fn printVector(self: *Repl, val: Value, writer: anytype) anyerror!void {
        const vec = val.toPtr(runtime.Vector);
        try writer.writeAll("#(");

        for (0..vec.length) |i| {
            if (i > 0) try writer.writeAll(" ");
            try self.printValue(vec.data[i], writer);
        }

        try writer.writeAll(")");
    }

    const Command = struct {
        short: []const u8,
        long: []const u8,
        has_arg: bool,
        help: []const u8,
    };

    const commands = [_]Command{
        .{ .short = ":q", .long = ":quit", .has_arg = false, .help = "Exit REPL" },
        .{ .short = ":d", .long = ":disasm", .has_arg = false, .help = "Toggle disassembly display" },
        .{ .short = ":l", .long = ":load", .has_arg = true, .help = "Load and evaluate a file" },
        .{ .short = ":t", .long = ":type", .has_arg = true, .help = "Show inferred type of expression" },
        .{ .short = ":h", .long = ":help", .has_arg = false, .help = "Show this help" },
    };

    fn matchCommand(cmd: []const u8) ?struct { idx: usize, arg: []const u8 } {
        inline for (commands, 0..) |c, i| {
            if (c.has_arg) {
                // Commands with args: ":x " or ":long "
                if (std.mem.startsWith(u8, cmd, c.short ++ " ")) {
                    return .{ .idx = i, .arg = std.mem.trim(u8, cmd[c.short.len + 1 ..], " \t") };
                }
                if (std.mem.startsWith(u8, cmd, c.long ++ " ")) {
                    return .{ .idx = i, .arg = std.mem.trim(u8, cmd[c.long.len + 1 ..], " \t") };
                }
            } else {
                // Commands without args: exact match
                if (std.mem.eql(u8, cmd, c.short) or std.mem.eql(u8, cmd, c.long)) {
                    return .{ .idx = i, .arg = "" };
                }
            }
        }
        return null;
    }

    fn handleCommand(self: *Repl, cmd: []const u8, writer: anytype) !void {
        const match = matchCommand(cmd) orelse {
            try writer.print("Unknown command: {s}\n", .{cmd});
            return;
        };

        switch (match.idx) {
            0 => std.process.exit(0), // :q
            1 => { // :d
                self.config.show_disasm = !self.config.show_disasm;
                try writer.print("Disassembly: {s}\n", .{if (self.config.show_disasm) "on" else "off"});
            },
            2 => { // :l
                if (self.loadFile(match.arg, writer)) |_| {} else |err| {
                    try writer.print("Load error: {s}\n", .{@errorName(err)});
                    return err;
                }
            },
            3 => { // :t
                if (self.showType(match.arg, writer)) |_| {} else |err| {
                    try writer.print("Type error: {s}\n", .{@errorName(err)});
                    return err;
                }
            },
            4 => { // :h
                try writer.writeAll("Commands:\n");
                inline for (commands) |c| {
                    try writer.print("  {s: <3} {s: <8} {s}\n", .{ c.short, c.long, c.help });
                }
            },
            else => unreachable,
        }
    }

    /// Load and evaluate a file.
    pub fn loadFile(self: *Repl, path: []const u8, writer: anytype) !void {
        _ = if (self.load(path)) |value| value else |err| {
            try writer.print("Cannot open '{s}': {s}\n", .{ path, @errorName(err) });
            return err;
        };
        try writer.print("; loaded {s}\n", .{path});
    }

    /// Evaluate multiple forms from source content.
    pub fn evalFile(self: *Repl, content: []const u8, writer: anytype) !void {
        _ = writer;
        _ = try self.evalForms(content);
    }

    // ========================================================================
    // Macro support
    // ========================================================================

    /// Check if expression is (defmacro name (args) body)
    fn isDefmacro(self: *Repl, expr: Value) bool {
        if (!expr.isCons()) return false;
        const cons = expr.toPtr(Cons);
        if (!cons.car.isSymbol()) return false;

        const b = self.compiler.builtins.?;
        const dispatch_head = self.canonicalMacroSymbol(cons.car);
        return dispatch_head.raw == b.defmacro.raw;
    }

    /// Check if expression is (in-package ...)
    fn isInPackage(self: *Repl, expr: Value) bool {
        if (!expr.isCons()) return false;
        const cons = expr.toPtr(Cons);
        if (!cons.car.isSymbol()) return false;

        const dispatch_head = self.canonicalMacroSymbol(cons.car);
        return dispatch_head.eq(self.vm.builtins.sym_in_package);
    }

    /// Check if expression is (defpackage ...)
    fn isDefpackage(self: *Repl, expr: Value) bool {
        if (!expr.isCons()) return false;
        const cons = expr.toPtr(Cons);
        if (!cons.car.isSymbol()) return false;

        const b = self.compiler.builtins.?;
        const dispatch_head = self.canonicalMacroSymbol(cons.car);
        return dispatch_head.raw == b.defpackage.raw;
    }

    fn findAccessibleCaseFolded(pkg: *const runtime.heap.Package, name: []const u8) ?Value {
        if (pkg.findAccessibleUpper(name)) |canonical| return canonical;

        var needs_upper = false;
        for (name) |ch| {
            if (ch >= 'a' and ch <= 'z') {
                needs_upper = true;
                break;
            }
        }
        if (!needs_upper or name.len > 256) return null;

        var upper_buf: [256]u8 = undefined;
        for (name, 0..) |ch, i| upper_buf[i] = std.ascii.toUpper(ch);
        return pkg.findAccessibleUpper(upper_buf[0..name.len]);
    }

    fn safeSymbolName(self: *Repl, sym: Value) ?[]const u8 {
        const live = self.vm.resolveForwardedValue(sym);
        if (!live.isSymbol()) return null;

        const addr = live.toPtrAddr();
        if (!self.heap.containsAddrForDebug(addr)) return null;
        const sym_ptr = live.toPtr(Symbol);

        const name_len = std.math.cast(usize, sym_ptr.name_len) orelse return null;
        const data_start = std.math.add(usize, addr, @sizeOf(Symbol)) catch |err| switch (err) {
            error.Overflow => return null,
        };
        const data_size = std.mem.alignForward(usize, name_len, 8);
        const data_end = std.math.add(usize, data_start, data_size) catch |err| switch (err) {
            error.Overflow => return null,
        };
        const name_ptr = @intFromPtr(sym_ptr.name_ptr);
        const name_end = std.math.add(usize, name_ptr, name_len) catch |err| switch (err) {
            error.Overflow => return null,
        };
        if (name_ptr < data_start or name_end > data_end) return null;

        return sym_ptr.getName();
    }

    fn canonicalMacroSymbol(self: *Repl, sym: Value) Value {
        const live_sym = self.vm.resolveForwardedValue(sym);
        if (!live_sym.isSymbol()) return live_sym;
        const name = self.safeSymbolName(live_sym) orelse return live_sym;

        if (self.heap.cl_package) |cl_pkg| {
            if (findAccessibleCaseFolded(cl_pkg, name)) |canonical| return canonical;
        }
        if (self.heap.cl_user_package) |cl_user_pkg| {
            if (findAccessibleCaseFolded(cl_user_pkg, name)) |canonical| return canonical;
        }

        return live_sym;
    }

    fn symbolPackage(self: *const Repl, sym: *const Symbol) ?*const runtime.heap.Package {
        return self.heap.symbolHomePkg(sym);
    }

    fn lookupMacroEntry(self: *Repl, sym: Value) ?MacroEntry {
        if (!sym.isSymbol()) return null;
        const sym_ptr = sym.toPtr(Symbol);
        const name = sym_ptr.getName();
        const trace_lookup = blk: {
            const raw = std.posix.getenv("HABU_TRACE_MACRO_LOOKUP_NAME") orelse break :blk false;
            const target = std.mem.sliceTo(raw, 0);
            break :blk std.ascii.eqlIgnoreCase(name, target);
        };

        if (self.macros.get(sym)) |entry| {
            if (trace_lookup) {
                std.debug.print(
                    "TRACE macro-lookup hit-direct name={s} pkg={s} count={d}\n",
                    .{ name, self.heap.getCurrentPackageName(), self.macros.count() },
                );
            }
            return entry;
        }
        if (trace_lookup) {
            std.debug.print(
                "TRACE macro-lookup miss-direct name={s} pkg={s} count={d}\n",
                .{ name, self.heap.getCurrentPackageName(), self.macros.count() },
            );
        }

        if (self.symbolPackage(sym_ptr)) |_| {
            // Respect package-qualified symbols: do not fall back to current package
            // or package-name lookup, which can hijack CL:FOO with local FOO macros.
            const canonical = self.canonicalMacroSymbol(sym);
            if (canonical.raw == sym.raw) {
                return null;
            }
            if (self.macros.get(canonical)) |entry| {
                if (trace_lookup) std.debug.print("TRACE macro-lookup hit-canonical name={s}\n", .{name});
                return entry;
            }
            if (trace_lookup) std.debug.print("TRACE macro-lookup miss-canonical name={s}\n", .{name});
            return null;
        }

        const canonical = self.canonicalMacroSymbol(sym);
        if (canonical.raw != sym.raw) {
            if (self.macros.get(canonical)) |entry| {
                if (trace_lookup) std.debug.print("TRACE macro-lookup hit-canonical2 name={s}\n", .{name});
                return entry;
            }
        }

        if (trace_lookup) std.debug.print("TRACE macro-lookup miss-name name={s}\n", .{name});
        return null;
    }

    fn normalizeMacroParams(self: *Repl, params_val: Value) !struct { params: Value, has_whole: bool, has_env: bool } {
        const b = self.compiler.builtins.?; // must exist during repl use
        var params = params_val;

        var whole_var: ?Value = null;
        if (params.isCons()) {
            const first_cons = params.toPtr(Cons);
            if (first_cons.car.raw == b.@"&whole".raw) {
                if (first_cons.cdr.isCons()) {
                    const rest = first_cons.cdr.toPtr(Cons);
                    whole_var = rest.car;
                    params = rest.cdr;
                }
            }
        }

        var env_var: ?Value = null;
        var new_params = Value.nil;
        var param_tail: ?*Cons = null;
        var p = params;
        while (p.isCons()) {
            const pc = p.toPtr(Cons);
            if (pc.car.raw == b.@"&environment".raw) {
                if (pc.cdr.isCons()) {
                    const env_rest = pc.cdr.toPtr(Cons);
                    env_var = env_rest.car;
                    p = env_rest.cdr;
                } else {
                    p = pc.cdr;
                }
                continue;
            }

            const new_cell = try self.heap.allocCons(pc.car, Value.nil);
            const new_cons = new_cell.toPtr(Cons);
            if (param_tail) |t| {
                t.cdr = new_cell;
                self.heap.writeBarrier(Value.makeCons(t), new_cell);
            } else {
                new_params = new_cell;
            }
            param_tail = new_cons;
            p = pc.cdr;
        }

        // Preserve dotted/symbol tail in macro lambda lists.
        if (!p.isNil()) {
            if (param_tail) |tail| {
                tail.cdr = p;
                self.heap.writeBarrier(Value.makeCons(tail), p);
            } else {
                new_params = p;
            }
        }

        var final_params = new_params;
        if (env_var) |ev| {
            final_params = try self.heap.allocCons(ev, final_params);
        }
        if (whole_var) |wv| {
            final_params = try self.heap.allocCons(wv, final_params);
        }

        return .{
            .params = final_params,
            .has_whole = whole_var != null,
            .has_env = env_var != null,
        };
    }

    /// Handle defmacro: compile the macro body and store the closure
    /// (defmacro name (args...) body...) -> stores (lambda (args...) body...) as macro
    fn handleDefmacro(self: *Repl, expr: Value, arena_alloc: std.mem.Allocator) !Value {
        // Extract: (defmacro name (args...) body...)
        const cons1 = expr.toPtr(Cons);
        const rest1 = cons1.cdr;
        if (!rest1.isCons()) return error.CompileError;

        const cons2 = rest1.toPtr(Cons);
        if (!cons2.car.isSymbol()) return error.CompileError;
        // Save the macro name immediately; later compile/eval steps may trigger GC,
        // and parsed form Values are not patched in-place by those collections.
        const macro_name_saved = try self.allocator.dupe(u8, cons2.car.toPtr(runtime.Symbol).getName());
        defer self.allocator.free(macro_name_saved);
        const rest2 = cons2.cdr;
        if (!rest2.isCons()) return error.CompileError;

        // Transform destructured params before building lambda
        const transformed_rest2 = try self.compiler.transformDestructuredParams(rest2);
        if (!transformed_rest2.isCons()) return error.CompileError;

        const def_cons = transformed_rest2.toPtr(Cons);
        const raw_params = def_cons.car;
        const body_list = def_cons.cdr;

        const macro_params = try self.normalizeMacroParams(raw_params);
        if (std.posix.getenv("HABU_TRACE_DEFMACRO_PARAMS") != null and cons2.car.isSymbol()) {
            const dumpParamShape = struct {
                fn run(params: Value) struct { fixed: usize, tail: Value } {
                    var fixed: usize = 0;
                    var p = params;
                    while (p.isCons()) {
                        fixed += 1;
                        p = p.toPtr(Cons).cdr;
                    }
                    return .{ .fixed = fixed, .tail = p };
                }
            }.run;
            const raw_info = dumpParamShape(raw_params);
            const norm_info = dumpParamShape(macro_params.params);
            std.debug.print(
                "TRACE defmacro params name={s} raw-fixed={d} raw-tail={s} norm-fixed={d} norm-tail={s} whole={any} env={any}\n",
                .{
                    macro_name_saved,
                    raw_info.fixed,
                    @tagName(raw_info.tail.typeKind()),
                    norm_info.fixed,
                    @tagName(norm_info.tail.typeKind()),
                    macro_params.has_whole,
                    macro_params.has_env,
                },
            );
            if (raw_info.tail.isSymbol()) {
                std.debug.print("  raw-tail-sym={s}\n", .{raw_info.tail.toPtr(Symbol).getName()});
            }
            if (norm_info.tail.isSymbol()) {
                std.debug.print("  norm-tail-sym={s}\n", .{norm_info.tail.toPtr(Symbol).getName()});
            }
        }
        const runtime_rest2 = try self.heap.allocCons(macro_params.params, body_list);

        // Build (lambda (args...) body...) to evaluate
        const lambda_sym = try self.heap.intern("lambda");
        const lambda_expr = try self.heap.allocCons(lambda_sym, runtime_rest2);

        // Don't expand macros in defmacro body - they'll be expanded when the macro is called
        // Expanding here can cause issues with forward references and recursive macros
        // const expanded_lambda = self.expandMacros(lambda_expr) catch return error.CompileError;
        const expanded_lambda = lambda_expr;

        // Compile and evaluate the lambda to get a closure
        const source_vm = self.activeVm();
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;
        defer {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
        }

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = if (self.compileExprRooted(source_vm, expanded_lambda, &env)) |node| node else |err| {
            std.debug.print("Compile error: {s}\n", .{@errorName(err)});
            return err;
        };
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        emitter.setRetainedValueLookup(Compiler.retainedValueLookup, &self.compiler);
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch wrapper chunk AND child chunks to use absolute indices
        const wrapper_chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        try patchChunkIndices(wrapper_chunk_ptr, chunk_base);
        for (child_chunks) |c| {
            try patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Add child chunks and sync VM chunk-pool slices immediately.
        try self.appendChildChunksAndSync(source_vm, child_chunks);
        const saved_current_vm = self.current_vm;
        self.current_vm = source_vm;
        defer self.current_vm = saved_current_vm;
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
        const form_tmp_idx = try self.ensureLoadFormRootTmpGlobal();
        try pushMacroCallRoots(
            source_vm,
            form_root_idx,
            form_stack_idx,
            form_tmp_idx,
            Value.nil,
            transformed_rest2,
        );
        defer popMacroCallRoots(source_vm, form_root_idx, form_stack_idx, form_tmp_idx);

        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const closure = try self.runVmPreserveMacroState(source_vm, chunk_ptr);
        const closure_live = source_vm.resolveForwardedValue(closure);
        source_vm.globals[form_root_idx] = closure_live;
        const transformed_rest2_live = source_vm.resolveForwardedValue(source_vm.globals[form_tmp_idx]);
        source_vm.globals[form_tmp_idx] = transformed_rest2_live;

        if (!closure_live.isClosure()) return error.CompileError;

        if (std.posix.getenv("HABU_TRACE_DEFMACRO_CLOSURE") != null) {
            const cl = closure_live.toPtr(runtime.Closure);
            if (cl.code.isChunk()) {
                const ch = cl.code.toPtr(runtime.objects.Chunk);
                std.debug.print(
                    "TRACE defmacro closure name={s} arity={d} opt={d} key={d} rest={any} code-len={d} consts={d}\n",
                    .{
                        macro_name_saved,
                        ch.arity,
                        ch.opt_count,
                        ch.key_count,
                        ch.has_rest != 0,
                        ch.code_len,
                        ch.const_count,
                    },
                );
            } else {
                std.debug.print(
                    "TRACE defmacro closure name={s} non-chunk-code kind={s}\n",
                    .{ macro_name_saved, @tagName(cl.code.typeKind()) },
                );
            }
        }

        // Re-resolve the macro symbol from the package using the saved name.
        // After VM execution (which may trigger GC), the original parsed symbol
        // Values are stale.
        var macro_sym: Value = undefined;
        if (self.heap.current_package) |pkg_cur| {
            const pkg = self.heap.findPackage(pkg_cur.name) orelse pkg_cur;
            if (pkg.symbols.get(macro_name_saved)) |local_sym| {
                macro_sym = local_sym;
            } else {
                // Symbol not in current package — intern it
                macro_sym = try pkg.intern(self.heap, macro_name_saved);
            }
        } else {
            macro_sym = try self.heap.intern(macro_name_saved);
        }

        // Store closure metadata in compiler macro table as:
        // (closure flags transformed-def), where flags bit0=&whole bit1=&environment.
        var macro_flags: i64 = 0;
        if (macro_params.has_whole) macro_flags |= 1;
        if (macro_params.has_env) macro_flags |= 2;
        const compiler_macro_entry_items = [_]Value{
            closure_live,
            Value.makeFixnum(macro_flags),
            transformed_rest2_live,
        };
        const compiler_macro_entry = try self.listFromSlice(&compiler_macro_entry_items);

        // Store the closure in REPL macro table for pre-compilation macro expansion
        // and the compiled entry in compiler macro table for compile-time expansion.
        try self.macros.put(macro_sym, .{
            .closure = closure_live,
            .has_whole = macro_params.has_whole,
            .has_env = macro_params.has_env,
        });
        try self.compiler.macro_table.put(macro_sym, compiler_macro_entry);
        try self.pinPersistentPair(macro_sym, closure_live);
        try self.pinPersistentPair(macro_sym, compiler_macro_entry);

        // Also store closure on symbol plist under MACRO-FUNCTION so that
        // (macro-function 'name) returns the closure per CL spec.
        if (macro_sym.isSymbol()) {
            const mf_key = try self.heap.intern("MACRO-FUNCTION");
            const entry_key = try self.heap.intern("%HABU-MACRO-ENTRY");
            const sym_ptr = macro_sym.toPtr(runtime.Symbol);
            // Build new plist entry (MACRO-FUNCTION . closure)
            const mf_entry = try self.heap.allocCons(mf_key, closure_live);
            const meta_entry = try self.heap.allocCons(entry_key, compiler_macro_entry);
            // Prepend to existing plist
            const old_plist = sym_ptr.plist;
            const with_meta = try self.heap.allocCons(meta_entry, old_plist);
            const new_plist = try self.heap.allocCons(mf_entry, with_meta);
            sym_ptr.plist = new_plist;
            self.heap.writeBarrier(macro_sym, new_plist);
        }

        if (std.posix.getenv("HABU_TRACE_DEFMACRO_DEFINE") != null and macro_sym.isSymbol()) {
            std.debug.print(
                "TRACE defmacro-define pkg={s} name={s} plist_nil={any} repl={d} compiler={d}\n",
                .{
                    self.heap.getCurrentPackageName(),
                    macro_sym.toPtr(Symbol).getName(),
                    macro_sym.toPtr(Symbol).plist.isNil(),
                    self.macros.count(),
                    self.compiler.macro_table.count(),
                },
            );
        }

        if (std.posix.getenv("HABU_TRACE_DEFMACRO") != null and macro_sym.isSymbol()) {
            const macro_name = macro_sym.toPtr(Symbol).getName();
            if (std.mem.eql(u8, macro_name, "DEFTEST")) {
                std.debug.print(
                    "TRACE defmacro DEFTEST defined: repl={d} compiler={d} pkg={s}\n",
                    .{
                        self.macros.count(),
                        self.compiler.macro_table.count(),
                        self.heap.getCurrentPackageName(),
                    },
                );
            }
        }

        // Return the macro name as a symbol
        return macro_sym;
    }

    /// Check if expression is (eval-when (situations...) body...)
    fn isEvalWhen(self: *Repl, expr: Value) bool {
        if (!expr.isCons()) return false;
        const cons = expr.toPtr(Cons);
        if (!cons.car.isSymbol()) return false;

        const b = self.compiler.builtins.?;
        const dispatch_head = self.canonicalMacroSymbol(cons.car);
        return dispatch_head.raw == b.@"eval-when".raw;
    }

    /// Handle eval-when: evaluate at compile time if :compile-toplevel
    /// (eval-when (situations...) body...) -> evaluates body based on situations
    fn handleEvalWhen(self: *Repl, expr: Value, arena_alloc: std.mem.Allocator) ReplError!Value {
        // Extract: (eval-when (situations...) body...)
        const cons1 = expr.toPtr(Cons);
        const rest1 = cons1.cdr;
        if (!rest1.isCons()) return error.CompileError;

        const cons2 = rest1.toPtr(Cons);
        const situations = cons2.car;
        const body = cons2.cdr;

        // Check situations for :compile-toplevel and :execute
        var compile_toplevel = false;
        var execute = false;

        var sit = situations;
        while (sit.isCons()) {
            const sit_cons = sit.toPtr(Cons);
            const situation = sit_cons.car;

            if (situation.isKeyword()) {
                const b = self.compiler.builtins.?;
                if (situation.raw == b.@"kw_compile-toplevel".raw) {
                    compile_toplevel = true;
                } else if (situation.raw == b.kw_execute.raw or
                    situation.raw == b.@"kw_load-toplevel".raw)
                {
                    execute = true;
                }
            }
            sit = sit_cons.cdr;
        }

        // If :compile-toplevel, evaluate each form now
        // Use a single shared VM for the entire body so closures from earlier
        // forms are available when called by later forms (e.g., defun + setf macro-function).
        if (compile_toplevel) {
            try self.evalCompileToplevel(body, arena_alloc);
        }

        // If :execute, return progn of body for runtime execution
        if (execute) {
            const progn_sym = try self.heap.intern("progn");
            return try self.heap.allocCons(progn_sym, body);
        }

        // Neither - return nil
        return Value.nil;
    }

    /// Evaluate all forms in compile-toplevel context.
    /// Each form is evaluated individually using evalSingleExpr.
    fn evalCompileToplevel(self: *Repl, body: Value, arena_alloc: std.mem.Allocator) ReplError!void {
        const source_vm = self.activeVm();
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
        try pushRootValue(source_vm, form_root_idx, form_stack_idx, body);
        defer popRootValue(source_vm, form_root_idx, form_stack_idx);

        while (true) {
            const form_live = source_vm.resolveForwardedValue(source_vm.globals[form_root_idx]);
            source_vm.globals[form_root_idx] = form_live;
            if (!form_live.isCons()) {
                if (!form_live.isNil()) return error.CompileError;
                return;
            }

            const form_cons = form_live.toPtr(Cons);
            const expr = form_cons.car;
            // Root the tail before evaluating this form. evalSingleExpr can
            // allocate/GC and move the body list between iterations.
            source_vm.globals[form_root_idx] = form_cons.cdr;
            _ = try self.evalSingleExpr(expr, arena_alloc);
        }
    }

    /// Evaluate a single expression (used by eval-when for compile-time evaluation)
    fn evalSingleExpr(self: *Repl, expr_val: Value, arena_alloc: std.mem.Allocator) ReplError!Value {
        var expr = expr_val;
        try self.compiler.refreshBuiltins();
        const trace_eval_single = std.posix.getenv("HABU_TRACE_EVAL_SINGLE") != null;
        const source_vm = self.activeVm();
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
        try pushRootValue(source_vm, form_root_idx, form_stack_idx, expr);
        defer popRootValue(source_vm, form_root_idx, form_stack_idx);
        expr = source_vm.resolveForwardedValue(source_vm.globals[form_root_idx]);

        // Check for defmacro
        if (self.isDefmacro(expr)) {
            return self.handleDefmacro(expr, arena_alloc);
        }

        // Check for defpackage/in-package - these need to execute immediately
        if (self.isDefpackage(expr) or self.isInPackage(expr)) {
            return try self.evalPackageForm(expr, arena_alloc);
        }

        // Check for nested eval-when
        if (self.isEvalWhen(expr)) {
            return self.handleEvalWhen(expr, arena_alloc);
        }

        // Expand macros
        expr = try self.expandMacros(expr);
        source_vm.globals[form_root_idx] = expr;
        expr = source_vm.resolveForwardedValue(source_vm.globals[form_root_idx]);

        if (trace_eval_single and expr.isCons()) {
            const head = expr.toPtr(Cons).car;
            if (head.isSymbol()) {
                std.debug.print("TRACE eval-single head={s}\n", .{head.toPtr(Symbol).getName()});
            } else {
                std.debug.print("TRACE eval-single head-kind={s}\n", .{@tagName(head.typeKind())});
            }
        }

        // Compile
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;
        defer {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
        }

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const ir_node = if (self.compileExprRooted(source_vm, expr, &env)) |node| node else |err| {
            std.debug.print("Compile error: {s}\n", .{@errorName(err)});
            return err;
        };

        // Compile-time EVAL-WHEN execution should follow dynamic REPL semantics.
        // Running nanopass inference here rejects valid ANSI helper forms.
        // But we still run specialization for performance.
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        emitter.setRetainedValueLookup(Compiler.retainedValueLookup, &self.compiler);
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch child chunks AND main chunk to use absolute indices
        for (child_chunks) |c| {
            try patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
        }
        try patchChunkIndices(chunk.toPtr(runtime.objects.Chunk), chunk_base);

        // Add child chunks and sync VM chunk-pool slices immediately.
        try self.appendChildChunksAndSync(source_vm, child_chunks);

        const saved_current_vm = self.current_vm;
        self.current_vm = source_vm;
        defer self.current_vm = saved_current_vm;

        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const result = try self.runVmPreserveMacroState(source_vm, chunk_ptr);
        self.compiler.noteCompileTimeUnspecial(expr);
        return result;
    }

    /// Desugar an expression (let* → let, cond → if, etc.)
    fn desugarExpr(self: *Repl, expr: Value) ReplError!Value {
        var desugarer = passes.Desugarer.init(self.allocator, self.heap, &self.vm.builtins);
        return try desugarer.desugar(expr);
    }

    /// Expand macros in an expression (recursive)
    fn expandMacros(self: *Repl, expr: Value) ReplError!Value {
        return self.expandMacrosWithDepth(expr, 0);
    }

    /// Expand macros once (non-recursive, for macroexpand-1)
    fn expandMacrosOnce(self: *Repl, expr: Value) ReplError!Value {
        // Non-list: no expansion
        if (!expr.isCons()) return expr;

        const cons = expr.toPtr(Cons);
        const head = cons.car;

        // Check if head is a macro
        if (head.isSymbol()) {
            if (self.lookupMacroEntry(head)) |macro_entry| {
                // Expand macro once and return without recursive expansion
                return try self.callMacro(macro_entry, expr, cons.cdr, head);
            }
        }

        // Not a macro call, return unchanged
        return expr;
    }

    fn expandMacrosWithDepth(self: *Repl, expr: Value, depth: u32) ReplError!Value {
        if (depth > 10000) return error.MacroExpansionTooDeep;

        // Non-list: no expansion
        if (!expr.isCons()) return expr;

        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check if head is a macro or special form
        if (head.isSymbol()) {
            const dispatch_head = self.canonicalMacroSymbol(head);
            // Handle eval-when during macro expansion (using symbol identity)
            const is_eval_when = dispatch_head.raw == self.compiler.builtins.?.@"eval-when".raw;

            if (is_eval_when) {
                // Use arena for compile-time evaluation
                var arena = std.heap.ArenaAllocator.init(self.allocator);
                defer arena.deinit();
                const arena_alloc = arena.allocator();

                const result = try self.handleEvalWhen(expr, arena_alloc);
                // If eval-when returned a progn (has :execute), expand it too
                if (!result.isNil()) {
                    return self.expandMacrosWithDepth(result, depth + 1);
                }
                // Only :compile-toplevel - return nil
                return Value.nil;
            }

            // Skip special forms that shouldn't be expanded
            if (self.compiler.builtins) |b| {
                if (dispatch_head.raw == b.quote.raw or dispatch_head.raw == b.quasiquote.raw) {
                    return expr; // Don't expand inside quoted forms
                }
                if (dispatch_head.raw == b.lambda.raw) {
                    // Expand lambda body forms, but keep parameter list untouched.
                    const lambda_args = tail;
                    if (!lambda_args.isCons()) return expr;
                    const args_cons = lambda_args.toPtr(Cons);
                    const params = args_cons.car;
                    const body = args_cons.cdr;
                    const expanded_body = try self.expandMacroListWithDepth(body, depth + 1);
                    if (expanded_body.raw == body.raw) return expr;
                    const rebuilt_args = try self.heap.allocCons(params, expanded_body);
                    return try self.heap.allocCons(head, rebuilt_args);
                }
                // For setf, don't expand the place (first arg) but do expand value (second arg)
                if (dispatch_head.raw == b.setf.raw) {
                    const args = tail;
                    if (args.isCons()) {
                        const args_cons = args.toPtr(Cons);
                        const place = args_cons.car; // Keep place unexpanded
                        const rest = args_cons.cdr;
                        if (rest.isCons()) {
                            const rest_cons = rest.toPtr(Cons);
                            const value_expr = rest_cons.car;
                            const rest_tail = rest_cons.cdr;
                            const expanded_value = try self.expandMacrosWithDepth(value_expr, depth + 1);
                            const new_rest = try self.heap.allocCons(expanded_value, rest_tail);
                            const new_args = try self.heap.allocCons(place, new_rest);
                            return try self.heap.allocCons(head, new_args);
                        }
                    }
                    return expr;
                }

                // handler-case: expand only the protected expression,
                // NOT the handler clause lambda lists (they contain variable names
                // that might shadow user macros like (defmacro e ...))
                if (dispatch_head.raw == b.@"handler-case".raw) {
                    const args = tail;
                    if (args.isCons()) {
                        const args_cons = args.toPtr(Cons);
                        const protected_expr = args_cons.car;
                        const clauses = args_cons.cdr;
                        // Expand the protected expression
                        const expanded_protected = try self.expandMacrosWithDepth(protected_expr, depth + 1);
                        // Expand handler clause bodies but NOT lambda lists
                        const expanded_clauses = try self.expandHandlerCaseClauses(clauses, depth + 1);
                        const new_args = try self.heap.allocCons(expanded_protected, expanded_clauses);
                        return try self.heap.allocCons(head, new_args);
                    }
                    return expr;
                }
            }

            if (self.lookupMacroEntry(head)) |macro_entry| {
                if (std.posix.getenv("HABU_TRACE_MACRO_DEPTH") != null and depth >= 480) {
                    const sym = head.toPtr(runtime.Symbol);
                    if (self.symbolPackage(sym)) |pkg| {
                        std.debug.print("TRACE macro-depth {d}: {s} [{s}]\n", .{ depth, sym.getName(), pkg.name });
                    } else {
                        std.debug.print("TRACE macro-depth {d}: {s} [uninterned]\n", .{ depth, sym.getName() });
                    }
                }
                // Expand macro: call the closure with the args
                const expansion = try self.callMacro(macro_entry, expr, tail, head);
                if (expansion.raw == expr.raw) {
                    // No-op expansion: stop to avoid unbounded recursion on self-expanding macros.
                    return expansion;
                }
                if (depth > 512 and expansion.isCons()) {
                    const exp_head = expansion.toPtr(Cons).car;
                    if (exp_head.isSymbol()) {
                        const exp_dispatch = self.canonicalMacroSymbol(exp_head);
                        if (exp_dispatch.raw == dispatch_head.raw) return error.MacroExpansionTooDeep;
                    }
                }
                // Recursively expand the result
                return self.expandMacrosWithDepth(expansion, depth + 1);
            }
        }

        // Non-macro form: recursively expand subforms.
        const expanded_head = try self.expandMacrosWithDepth(head, depth + 1);
        const expanded_tail = try self.expandMacroListWithDepth(tail, depth + 1);
        if (expanded_head.raw == head.raw and expanded_tail.raw == tail.raw) {
            return expr;
        }
        return try self.heap.allocCons(expanded_head, expanded_tail);
    }

    /// Expand macros in a list (for cdr of cons)
    fn expandMacroList(self: *Repl, list: Value) ReplError!Value {
        return self.expandMacroListWithDepth(list, 0);
    }

    /// Expand handler-case clauses, preserving lambda lists (variable bindings)
    /// Each clause is (type (var) body...) — only expand body forms, not (var).
    fn expandHandlerCaseClauses(self: *Repl, clauses: Value, depth: u32) ReplError!Value {
        if (!clauses.isCons()) return clauses;
        const cons = clauses.toPtr(Cons);
        const clause = cons.car;
        const rest = cons.cdr;

        const expanded_clause = blk: {
            if (!clause.isCons()) break :blk clause;
            const cc = clause.toPtr(Cons);
            const cond_type = cc.car;
            const clause_rest = cc.cdr;
            if (!clause_rest.isCons()) break :blk clause;
            const cr = clause_rest.toPtr(Cons);
            const lambda_list = cr.car; // (var) — DO NOT expand
            const body = cr.cdr;
            // Only expand body forms
            const expanded_body = try self.expandMacroListWithDepth(body, depth);
            if (expanded_body.raw == body.raw) break :blk clause;
            const new_rest = try self.heap.allocCons(lambda_list, expanded_body);
            break :blk try self.heap.allocCons(cond_type, new_rest);
        };

        const expanded_rest = try self.expandHandlerCaseClauses(rest, depth);
        if (expanded_clause.raw == clause.raw and expanded_rest.raw == rest.raw) return clauses;
        return try self.heap.allocCons(expanded_clause, expanded_rest);
    }

    fn expandMacroListWithDepth(self: *Repl, list: Value, depth: u32) ReplError!Value {
        if (depth > 10000) return error.MacroExpansionTooDeep;
        if (!list.isCons()) return list;

        const cons = list.toPtr(Cons);
        const car = cons.car;
        const cdr = cons.cdr;
        const expanded_car = try self.expandMacrosWithDepth(car, depth + 1);
        const expanded_cdr = try self.expandMacroListWithDepth(cdr, depth);

        if (expanded_car.raw != car.raw or expanded_cdr.raw != cdr.raw) {
            return try self.heap.allocCons(expanded_car, expanded_cdr);
        }
        return list;
    }

    /// Call a macro closure with arguments (as a list)
    fn callMacro(self: *Repl, macro: MacroEntry, whole_form: Value, args: Value, macro_name: Value) ReplError!Value {
        const source_vm = self.activeVm();
        const form_root_idx = try self.ensureLoadFormRootGlobal();
        const form_stack_idx = try self.ensureLoadFormRootStackGlobal();
        const form_tmp_idx = try self.ensureLoadFormRootTmpGlobal();
        try pushMacroCallRoots(
            source_vm,
            form_root_idx,
            form_stack_idx,
            form_tmp_idx,
            args,
            whole_form,
        );
        defer popMacroCallRoots(source_vm, form_root_idx, form_stack_idx, form_tmp_idx);

        const closure = macro.closure;
        if (!closure.isClosure()) {
            if (std.posix.getenv("HABU_TRACE_MACRO_CALLS") != null and macro_name.isSymbol()) {
                std.debug.print(
                    "TRACE macro call: {s} invalid-closure type={}\n",
                    .{ macro_name.toPtr(Symbol).getName(), closure.typeKind() },
                );
            }
            return error.RuntimeError;
        }
        var call_args = source_vm.globals[form_root_idx];
        if (macro.has_env) {
            const env_val = try self.heap.allocMacroEnv();
            source_vm.globals[form_root_idx] = try self.heap.allocCons(env_val, source_vm.globals[form_root_idx]);
            call_args = source_vm.globals[form_root_idx];
        }
        if (macro.has_whole) {
            source_vm.globals[form_root_idx] = try self.heap.allocCons(source_vm.globals[form_tmp_idx], source_vm.globals[form_root_idx]);
            call_args = source_vm.globals[form_root_idx];
        }
        source_vm.globals[form_tmp_idx] = closure;
        // Count args
        var argc: usize = 0;
        var arg_list = source_vm.globals[form_root_idx];
        while (arg_list.isCons()) {
            argc += 1;
            arg_list = arg_list.toPtr(Cons).cdr;
        }
        if (std.posix.getenv("HABU_TRACE_MACRO_CALLS") != null and macro_name.isSymbol()) {
            std.debug.print(
                "TRACE macro call: {s} argc={d} whole={any} env={any}\n",
                .{ macro_name.toPtr(Symbol).getName(), argc, macro.has_whole, macro.has_env },
            );
        }

        if (argc > std.math.maxInt(u8)) return error.InvalidArgument;
        const byte_count = std.math.add(usize, 9, std.math.mul(usize, argc, 4) catch return error.InvalidArgument) catch return error.InvalidArgument;

        // Build bytecode to call closure with args
        var code = std.ArrayList(u8){};
        defer code.deinit(self.allocator);
        try code.ensureTotalCapacity(self.allocator, byte_count);

        // push_const for closure (const 0)
        var op_buf: [2]u8 = undefined;
        var idx_buf: [2]u8 = undefined;
        std.mem.writeInt(u16, &op_buf, @intFromEnum(Op.push_const), .little);
        std.mem.writeInt(u16, &idx_buf, 0, .little);
        try code.appendSlice(self.allocator, &op_buf);
        try code.appendSlice(self.allocator, &idx_buf);

        // push each arg as constant
        var const_idx: u16 = 1;
        arg_list = call_args;
        while (arg_list.isCons()) {
            std.mem.writeInt(u16, &op_buf, @intFromEnum(Op.push_const), .little);
            std.mem.writeInt(u16, &idx_buf, const_idx, .little);
            try code.appendSlice(self.allocator, &op_buf);
            try code.appendSlice(self.allocator, &idx_buf);
            const_idx += 1;
            arg_list = arg_list.toPtr(Cons).cdr;
        }

        // call
        std.mem.writeInt(u16, &op_buf, @intFromEnum(Op.call), .little);
        try code.appendSlice(self.allocator, &op_buf);
        try code.append(self.allocator, @intCast(argc));

        // ret
        std.mem.writeInt(u16, &op_buf, @intFromEnum(Op.ret), .little);
        try code.appendSlice(self.allocator, &op_buf);

        // Build constants: [0]=closure, [1..]=args
        var constants = std.ArrayList(Value){};
        defer constants.deinit(self.allocator);
        try constants.append(self.allocator, source_vm.globals[form_tmp_idx]);
        arg_list = source_vm.globals[form_root_idx];
        while (arg_list.isCons()) {
            try constants.append(self.allocator, arg_list.toPtr(Cons).car);
            arg_list = arg_list.toPtr(Cons).cdr;
        }

        const chunk = try self.heap.allocChunk(code.items, constants.items, 0, 0, 0, false, 0);

        self.syncChunkPools(source_vm);

        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const saved_current_vm = self.current_vm;
        self.current_vm = source_vm;
        defer self.current_vm = saved_current_vm;

        return self.runVmPreserveMacroState(source_vm, chunk_ptr) catch |err| {
            if (std.posix.getenv("HABU_TRACE_MACRO_CALLS") != null and macro_name.isSymbol()) {
                std.debug.print(
                    "TRACE macro call error: {s} err={s}\n",
                    .{ macro_name.toPtr(Symbol).getName(), @errorName(err) },
                );
            }
            return err;
        };
    }

    /// Handle package forms (defpackage/in-package) - execute them immediately
    fn evalPackageForm(self: *Repl, expr: Value, arena_alloc: std.mem.Allocator) !Value {
        if (self.isInPackage(expr)) {
            if (!expr.isCons()) return error.InvalidSyntax;
            const form_cons = expr.toPtr(Cons);
            if (!form_cons.cdr.isCons()) return error.InvalidSyntax;
            const arg_cons = form_cons.cdr.toPtr(Cons);
            if (!arg_cons.cdr.isNil()) return error.InvalidSyntax;

            const pkg_val = (try primitives.package.findPackage(self.heap, arg_cons.car)) orelse {
                if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null) {
                    const arg = arg_cons.car;
                    const pkg_name_opt = switch (arg.typeKind()) {
                        .symbol => arg.toPtr(runtime.Symbol).getName(),
                        .string => arg.toPtr(runtime.String).bytes(),
                        .keyword => arg.toPtr(runtime.Keyword).getName(),
                        else => null,
                    };
                    const dbg_vm = self.current_vm orelse &self.vm;
                    const vm_pkg_name = if (self.currentPackageGlobal(dbg_vm)) |cur_pkg|
                        (self.packageNameBytesLive(dbg_vm, cur_pkg) orelse "<invalid>")
                    else
                        "<none>";
                    if (pkg_name_opt) |pkg_name| {
                        const native_hit = self.heap.findPackage(pkg_name) != null;
                        std.debug.print(
                            "TRACE in-package miss: designator={s} native_hit={any} vm_pkg={s} heap_pkg={s}\n",
                            .{
                                pkg_name,
                                native_hit,
                                vm_pkg_name,
                                self.heap.getCurrentPackageName(),
                            },
                        );
                    } else {
                        std.debug.print(
                            "TRACE in-package miss: designator-kind={s} vm_pkg={s} heap_pkg={s}\n",
                            .{
                                @tagName(arg.typeKind()),
                                vm_pkg_name,
                                self.heap.getCurrentPackageName(),
                            },
                        );
                    }
                }
                return error.InvalidPackage;
            };
            if (!pkg_val.isPackage()) return error.TypeMismatch;

            const target_vm = self.current_vm orelse &self.vm;
            const pkg_name = self.packageNameBytesLive(target_vm, pkg_val) orelse return error.TypeMismatch;
            const native_pkg = self.heap.findPackage(pkg_name) orelse return error.InvalidPackage;
            self.heap.setCurrentPackage(native_pkg);
            self.setPackageGlobals(target_vm, pkg_val);
            self.syncReaderPackageFromVm(target_vm);
            return pkg_val;
        }

        // For package forms, just compile and execute inline
        // The compiler will call heap.setCurrentPackage which affects future reads
        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        const saved_compiler_vm = self.compiler.vm;
        const target_vm = self.current_vm orelse &self.vm;
        self.compiler.setVm(target_vm);
        self.compiler.builder = ir.IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;
        defer {
            if (saved_compiler_vm) |saved_vm| {
                self.compiler.setVm(saved_vm);
            } else {
                self.compiler.vm = null;
            }
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
        }

        const ir_node = try self.compileExprRooted(target_vm, expr, &env);
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        emitter.setRetainedValueLookup(Compiler.retainedValueLookup, &self.compiler);
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();

        // Execute package side effects in the active VM context.
        // During nested LOAD/EVAL this must be the nested VM so subsequent
        // reader sync sees updated *PACKAGE* from that VM's globals.
        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const result = try self.runVmPreserveMacroState(target_vm, chunk_ptr);
        self.syncReaderPackageFromVm(target_vm);

        // The package is now set in self.heap.current_package for future reads
        // Return the result (package name as symbol)
        return result;
    }

    // ========================================================================
    // Type inference
    // ========================================================================

    /// Show the inferred type of an expression
    fn showType(self: *Repl, expr_str: []const u8, writer: anytype) !void {
        // Parse expression
        var parser = try @import("../reader/parser.zig").Parser.init(self.allocator, self.heap, expr_str, &self.vm.builtins);
        defer parser.deinit();
        var dispatch_ctx = DispatchMacroCtx{ .vm = &self.vm };
        parser.setDispatchMacroHook(@ptrCast(&dispatch_ctx), parserDispatchMacro);
        const expr = try parseWithHookError(&parser);
        if (expr.isNil()) {
            try writer.writeAll("Empty expression\n");
            return;
        }

        // Use arena allocator for compilation
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        // Save and set up compiler state
        const saved_builder = self.compiler.builder;
        const saved_allocator = self.compiler.allocator;
        self.compiler.builder = IrBuilder.init(arena_alloc);
        self.compiler.allocator = arena_alloc;

        var env = Env.init(arena_alloc, null);
        defer env.deinit();

        // Compile to IR
        const ir_node = if (self.compileExprRooted(&self.vm, expr, &env)) |node| node else |err| {
            self.compiler.builder = saved_builder;
            self.compiler.allocator = saved_allocator;
            try writer.print("Compile error: {s}\n", .{@errorName(err)});
            return err;
        };
        self.compiler.builder = saved_builder;
        self.compiler.allocator = saved_allocator;

        // Run type inference
        const inferred = if (self.compiler.typeInfer(ir_node)) |ty| ty else |err| {
            try writer.print("Type inference failed: {s}\n", .{@errorName(err)});
            return err;
        };

        // Print the inferred type using custom format method
        try inferred.format("", .{}, writer);
        try writer.writeAll("\n");
    }
};

test "repl init wires compiler pointers to repl vm" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{});
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    try testing.expect(repl.compiler.vm == &repl.vm);
    try testing.expect(repl.compiler.bi_checker.builtins == &repl.vm.builtins);
}

/// Convenience function to evaluate a string
pub fn evalString(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var repl: Repl = undefined;
    try repl.init(allocator, heap, .{});
    defer repl.deinit();
    return repl.eval(source);
}

const FailAfterAllocator = struct {
    const Self = @This();

    backing: std.mem.Allocator,
    fail_after: usize,
    op_count: usize = 0,

    fn init(backing: std.mem.Allocator, fail_after: usize) Self {
        return .{ .backing = backing, .fail_after = fail_after };
    }

    fn allocator(self: *Self) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .remap = remap,
                .free = free,
            },
        };
    }

    fn shouldFail(self: *Self) bool {
        const fail = self.op_count >= self.fail_after;
        self.op_count += 1;
        return fail;
    }

    fn alloc(ctx: *anyopaque, n: usize, alignment: std.mem.Alignment, ra: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        if (self.shouldFail()) return null;
        return self.backing.rawAlloc(n, alignment, ra);
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));
        if (self.shouldFail()) return false;
        return self.backing.rawResize(memory, alignment, new_len, ra);
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ra: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));
        if (self.shouldFail()) return null;
        return self.backing.rawRemap(memory, alignment, new_len, ra);
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ra: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));
        self.backing.rawFree(memory, alignment, ra);
    }
};

fn fakeJitTarget() callconv(.c) u64 {
    return Value.nil.raw;
}

// ============================================================================
// Tests
// ============================================================================

test "eval fixnum" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "42");
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "nil");
    try testing.expect(result.isNil());
}

test "eval arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(+ 10 20)");
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "eval nested arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(+ (* 3 4) (- 10 5))");
    try testing.expectEqual(@as(i64, 17), result.toFixnum());
}

test "lambda keyword defaults handle omitted trailing keys" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const call_with_key = try repl.eval(
        \\(progn
        \\  (defun kw-probe (&key (a 1) (b 10) (c 20))
        \\    (+ a b c))
        \\  (kw-probe :a 2))
    );
    try testing.expect(call_with_key.isFixnum());
    try testing.expectEqual(@as(i64, 32), call_with_key.toFixnum());

    const call_defaults = try repl.eval("(kw-probe)");
    try testing.expect(call_defaults.isFixnum());
    try testing.expectEqual(@as(i64, 31), call_defaults.toFixnum());
}

test "loop for-in supports by step function" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);

    const result = try repl.eval(
        "(equal (loop for x in '(1 2 3 4 5 6) by #'cddr collect x) '(1 3 5))",
    );
    try testing.expect(!result.isNil());
}

test "loop when else-when else-do chain is accepted" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);

    const result = try repl.eval(
        "(equal (loop for v in '(1 \"x\" 2) when (numberp v) collecting v into tem else when (stringp v) collecting (length v) into tem else do (return :bad) finally (return tem)) '(1 1 2))",
    );
    try testing.expect(!result.isNil());
}

test "setf supports composed list places" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);

    const result = try repl.eval(
        \\(progn
        \\  (setq xs (list 10 20 30 40 50))
        \\  (setf (second xs) 21)
        \\  (setf (caddr xs) 32)
        \\  (setf (fourth xs) 43)
        \\  (setf (cdddr xs) '(44 55))
        \\  (setq pair (list (cons 1 2)))
        \\  (setf (caar pair) 9)
        \\  (setf (cdar pair) 8)
        \\  (and (equal xs '(10 21 32 44 55))
        \\       (= (caar pair) 9)
        \\       (= (cdar pair) 8)))
    );
    try testing.expect(!result.isNil());
}

test "dolist supports early return without corrupting iteration state" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);

    const result = try repl.eval(
        "(equal (progn (setq seen nil) (list (dolist (x '(1 2 a 3) 'done) (if (numberp x) (push x seen) (return :stop))) (nreverse seen))) '(:stop (1 2)))",
    );
    try testing.expect(!result.isNil());
}

test "lookupCallableFunction uses exact package-qualified global" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const fn_val = try repl.eval("(lambda () 99)");
    try testing.expect(Repl.isCallableValue(fn_val));

    const sym = try repl.heap.intern("HABU-EXACT-LOOKUP-TEST-FN");
    const idx_exact = try repl.compiler.globals.define("COMMON-LISP-USER:HABU-EXACT-LOOKUP-TEST-FN");
    repl.vm.globals[idx_exact] = fn_val;
    if (idx_exact >= repl.vm.num_globals) repl.vm.num_globals = idx_exact + 1;

    const idx_other = try repl.compiler.globals.define("PKG-A:HABU-EXACT-LOOKUP-TEST-FN");
    repl.vm.globals[idx_other] = Value.nil;
    if (idx_other >= repl.vm.num_globals) repl.vm.num_globals = idx_other + 1;

    const first = try repl.lookupCallableFunction(sym);
    try testing.expect(first != null);
    try testing.expect(first.?.eq(fn_val));

    repl.vm.globals[idx_other] = fn_val;
    const second = try repl.lookupCallableFunction(sym);
    try testing.expect(second != null);
    try testing.expect(second.?.eq(fn_val));
}

test "function resolver autoload is package-generic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);

    _ = try repl.eval("(defpackage \"AUTO-PKG\" (:use \"COMMON-LISP\") (:export \"AUTO-TARGET\"))");
    _ = try repl.eval("(in-package \"AUTO-PKG\")");
    _ = try repl.eval(
        "(defun load-function (fn mexprp) " ++
            "(declare (ignore mexprp)) " ++
            "(setf (symbol-function fn) (lambda () 42)) " ++
            "t)",
    );
    _ = try repl.eval("(setf (get 'auto-target 'autoload) '(\"auto-pkg-loader\"))");
    _ = try repl.eval("(in-package \"CL-USER\")");

    const sym = (try repl.heap.lookupInPackage("AUTO-PKG", "AUTO-TARGET")) orelse return error.TestUnexpectedResult;
    try testing.expect(try repl.lookupCallableFunction(sym) == null);

    const resolved = try Repl.functionResolveCallback(sym, @ptrCast(&repl));
    try testing.expect(resolved != null);
    try testing.expect(Repl.isCallableValue(resolved.?));

    const result = try repl.eval("(auto-pkg:auto-target)");
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval parse error sets error info" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var info: ?Repl.ErrorInfo = null;
    try testing.expectError(error.UnterminatedList, repl.evalCapturingError("(", &info));
    try testing.expect(info != null);
    if (info) |got| {
        try testing.expectEqual(Repl.ErrorKind.parse_unterminated_list, got.kind);
        try testing.expectEqual(@as(u32, 1), got.line);
        try testing.expect(got.column >= 1);
    }
}

test "loadFile missing file errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.FileNotFound, repl.loadFile("nope-nope.habu", stream.writer()));
}

test "loadFile aborts on first form error" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("load-abort.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(defun load-ok () 1)\n" ++
                "(cdr 1)\n" ++
                "(defun load-after-error () 2)\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "load-abort.lsp" });
    defer allocator.free(script_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.TypeMismatch, repl.loadFile(script_abs, stream.writer()));

    const before = try repl.eval("(fboundp 'load-ok)");
    try testing.expect(before.isT());
    const after = try repl.eval("(fboundp 'load-after-error)");
    try testing.expect(after.isNil());
}

test "handler-case around load catches once and aborts file" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("load-handler-abort.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(setq *load-handler-probe* 0)\n" ++
                "(error \"first\")\n" ++
                "(setq *load-handler-probe* 1)\n" ++
                "(error \"second\")\n" ++
                "(defun load-after-handler () 2)\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "load-handler-abort.lsp" });
    defer allocator.free(script_abs);

    _ = try repl.eval("(setq *load-handler-count* 0)");
    const caught_expr = try std.fmt.allocPrint(
        allocator,
        "(eq (handler-case (load \"{s}\") (error (c) (declare (ignore c)) (setq *load-handler-count* (+ *load-handler-count* 1)) :caught)) :caught)",
        .{script_abs},
    );
    defer allocator.free(caught_expr);
    const caught = try repl.eval(caught_expr);
    try testing.expect(caught.isT());

    const count = try repl.eval("*load-handler-count*");
    try testing.expect(count.isFixnum());
    try testing.expectEqual(@as(i64, 1), count.toFixnum());

    const probe = try repl.eval("*load-handler-probe*");
    try testing.expect(probe.isFixnum());
    try testing.expectEqual(@as(i64, 0), probe.toFixnum());

    const after = try repl.eval("(fboundp 'load-after-handler)");
    try testing.expect(after.isNil());
}

test "script handler-case load does not resume failed file" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 32 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var err_file = try tmp.dir.createFile("errfile.lsp", .{});
        defer err_file.close();
        try err_file.writeAll(
            "(setq *script-load-probe* 0)\n" ++
                "(error \"e1\")\n" ++
                "(setq *script-load-probe* 1)\n" ++
                "(error \"e2\")\n" ++
                "(setq *script-load-probe* 2)\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const err_abs = try std.fs.path.join(allocator, &.{ base, "errfile.lsp" });
    defer allocator.free(err_abs);

    const wrapper_src = try std.fmt.allocPrint(
        allocator,
        "(setq *script-load-catches* 0)\n" ++
            "(handler-case\n" ++
            "    (load \"{s}\")\n" ++
            "  (condition (e)\n" ++
            "    (declare (ignore e))\n" ++
            "    (setq *script-load-catches* (+ *script-load-catches* 1))))\n" ++
            "(setq *script-load-after* 42)\n",
        .{err_abs},
    );
    defer allocator.free(wrapper_src);

    {
        var wrapper = try tmp.dir.createFile("wrapper.lsp", .{});
        defer wrapper.close();
        try wrapper.writeAll(wrapper_src);
    }

    const wrapper_abs = try std.fs.path.join(allocator, &.{ base, "wrapper.lsp" });
    defer allocator.free(wrapper_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile(wrapper_abs, stream.writer());

    const catches = try repl.eval("*script-load-catches*");
    try testing.expect(catches.isFixnum());
    try testing.expectEqual(@as(i64, 1), catches.toFixnum());

    const probe = try repl.eval("*script-load-probe*");
    try testing.expect(probe.isFixnum());
    try testing.expectEqual(@as(i64, 0), probe.toFixnum());

    const after = try repl.eval("*script-load-after*");
    try testing.expect(after.isFixnum());
    try testing.expectEqual(@as(i64, 42), after.toFixnum());
}

test "handler-case load aborts file after first signaled condition" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 32 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("load-signal-abort.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(setq *load-signal-probe* 0)\n" ++
                "(signal 'type-error nil)\n" ++
                "(setq *load-signal-probe* 1)\n" ++
                "(signal 'type-error nil)\n" ++
                "(setq *load-signal-probe* 2)\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const err_abs = try std.fs.path.join(allocator, &.{ base, "load-signal-abort.lsp" });
    defer allocator.free(err_abs);

    const wrapper_src = try std.fmt.allocPrint(
        allocator,
        "(setq *load-signal-catches* 0)\n" ++
            "(handler-case\n" ++
            "    (load \"{s}\")\n" ++
            "  (condition (e)\n" ++
            "    (declare (ignore e))\n" ++
            "    (setq *load-signal-catches* (+ *load-signal-catches* 1))))\n" ++
            "(setq *load-signal-after* 7)\n",
        .{err_abs},
    );
    defer allocator.free(wrapper_src);

    {
        var wrapper = try tmp.dir.createFile("wrapper-signal.lsp", .{});
        defer wrapper.close();
        try wrapper.writeAll(wrapper_src);
    }

    const wrapper_abs = try std.fs.path.join(allocator, &.{ base, "wrapper-signal.lsp" });
    defer allocator.free(wrapper_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile(wrapper_abs, stream.writer());

    const catches = try repl.eval("*load-signal-catches*");
    try testing.expect(catches.isFixnum());
    try testing.expectEqual(@as(i64, 1), catches.toFixnum());

    const probe = try repl.eval("*load-signal-probe*");
    try testing.expect(probe.isFixnum());
    try testing.expectEqual(@as(i64, 0), probe.toFixnum());

    const after = try repl.eval("*load-signal-after*");
    try testing.expect(after.isFixnum());
    try testing.expectEqual(@as(i64, 7), after.toFixnum());
}

test "load preserves package global across generational GC pressure" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{
        .total_size = 64 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 2 * 1024 * 1024,
            .los_size = 8 * 1024 * 1024,
            .los_threshold = 16 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const cl_name = try repl.heap.intern("COMMON-LISP");
    const cl_pkg = (try repl.heap.findLispPackage(cl_name)) orelse return error.TestUnexpectedResult;
    try repl.setClGlobal("*PACKAGE*", cl_pkg);

    const pkg_before = repl.currentPackageGlobal(&repl.vm) orelse return error.TestUnexpectedResult;
    try testing.expect(pkg_before.isPackage());
    const pkg_before_name = repl.packageNameBytesLive(&repl.vm, pkg_before) orelse return error.TestUnexpectedResult;
    const pkg_name_before = try allocator.dupe(u8, pkg_before_name);
    defer allocator.free(pkg_name_before);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("load-package-gc.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(in-package \"CL-USER\")\n" ++
                "(let ((i 0))\n" ++
                "  (while (< i 400000)\n" ++
                "    (cons i i)\n" ++
                "    (setq i (+ i 1))))\n" ++
                "17\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "load-package-gc.lsp" });
    defer allocator.free(script_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile(script_abs, stream.writer());

    const pkg_after_load = repl.currentPackageGlobal(&repl.vm) orelse return error.TestUnexpectedResult;
    try testing.expect(pkg_after_load.isPackage());
    const pkg_after_load_name = repl.packageNameBytesLive(&repl.vm, pkg_after_load) orelse return error.TestUnexpectedResult;
    try testing.expect(std.mem.eql(u8, pkg_name_before, pkg_after_load_name));

    _ = try repl.vm.collectGarbage();
    _ = try repl.vm.collectGarbage();

    const pkg_after_gc = repl.currentPackageGlobal(&repl.vm) orelse return error.TestUnexpectedResult;
    try testing.expect(pkg_after_gc.isPackage());
    const pkg_after_gc_name = repl.packageNameBytesLive(&repl.vm, pkg_after_gc) orelse return error.TestUnexpectedResult;
    try testing.expect(std.mem.eql(u8, pkg_name_before, pkg_after_gc_name));
}

test "load restores load pathname globals under generational GC pressure" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{
        .total_size = 64 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 2 * 1024 * 1024,
            .los_size = 8 * 1024 * 1024,
            .los_threshold = 16 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const load_globals = [_][]const u8{
        "COMMON-LISP:*LOAD-PATHNAME*",
        "COMMON-LISP:*LOAD-TRUENAME*",
        "COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS*",
    };

    const sentinel = try repl.loadPathnameValue("/tmp/habu-load-sentinel.lsp");
    const sentinel_ns_val = try primitives.pathname.namestring(
        allocator,
        repl.heap,
        &repl.vm.builtins,
        sentinel,
    );
    try testing.expect(sentinel_ns_val.isString());
    const sentinel_ns = try allocator.dupe(u8, sentinel_ns_val.toPtr(runtime.String).bytes());
    defer allocator.free(sentinel_ns);

    for (load_globals) |name| {
        const idx = if (repl.compiler.globals.lookup(name)) |i| i else try repl.compiler.globals.define(name);
        repl.vm.globals[idx] = sentinel;
        if (idx >= repl.vm.num_globals) repl.vm.num_globals = idx + 1;
    }

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("load-pathname-gc.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(let ((i 0))\n" ++
                "  (while (< i 400000)\n" ++
                "    (cons i i)\n" ++
                "    (setq i (+ i 1))))\n" ++
                "17\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "load-pathname-gc.lsp" });
    defer allocator.free(script_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile(script_abs, stream.writer());

    _ = try repl.vm.collectGarbage();
    _ = try repl.vm.collectGarbage();

    for (load_globals) |name| {
        const idx = repl.compiler.globals.lookup(name) orelse continue;
        if (idx >= repl.vm.num_globals) return error.TestUnexpectedResult;
        const got = repl.vm.globals[idx];
        try testing.expect(got.isPathname());
        const got_ns = try primitives.pathname.namestring(
            allocator,
            repl.heap,
            &repl.vm.builtins,
            got,
        );
        try testing.expect(got_ns.isString());
        try testing.expect(std.mem.eql(u8, sentinel_ns, got_ns.toPtr(runtime.String).bytes()));
    }
}

test "nested load restores package and binds default pathname defaults" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 32 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var inner = try tmp.dir.createFile("inner.lsp", .{});
        defer inner.close();
        try inner.writeAll(
            "(in-package \"COMMON-LISP\")\n" ++
                "17\n",
        );
    }
    {
        var outer = try tmp.dir.createFile("outer.lsp", .{});
        defer outer.close();
        try outer.writeAll(
            "(in-package \"CL-USER\")\n" ++
                "(list (progn (load \"inner.lsp\") (package-name *package*))\n" ++
                "      (pathnamep *default-pathname-defaults*))\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const outer_abs = try std.fs.path.join(allocator, &.{ base, "outer.lsp" });
    defer allocator.free(outer_abs);

    const form = try std.fmt.allocPrint(allocator, "(load \"{s}\")", .{outer_abs});
    defer allocator.free(form);
    const result = try repl.eval(form);
    try testing.expect(result.isCons());
    const c1 = result.toPtr(runtime.Cons);
    try testing.expect(c1.car.isString());
    try testing.expectEqualStrings("COMMON-LISP-USER", c1.car.toPtr(runtime.String).bytes());
    const c2 = c1.cdr.toPtr(runtime.Cons);
    try testing.expect(!c2.car.isNil());
}

test "load rejects repeated directory prefix fallback" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    const load_globals = [_][]const u8{
        "COMMON-LISP:*LOAD-PATHNAME*",
        "COMMON-LISP:*LOAD-TRUENAME*",
    };
    for (load_globals) |name| {
        const idx = try repl.compiler.globals.define(name);
        repl.vm.globals[idx] = Value.nil;
        if (idx >= repl.vm.num_globals) repl.vm.num_globals = idx + 1;
    }

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.makePath("printer/format");

    {
        var file = try tmp.dir.createFile("printer/format/load.lsp", .{});
        defer file.close();
        try file.writeAll("(load \"format/format-c.lsp\")\n");
    }
    {
        var file = try tmp.dir.createFile("printer/format/format-c.lsp", .{});
        defer file.close();
        try file.writeAll("42\n");
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const load_abs = try std.fs.path.join(allocator, &.{ base, "printer", "format", "load.lsp" });
    defer allocator.free(load_abs);

    var out_buf: [256]u8 = undefined;
    var stream = std.io.fixedBufferStream(&out_buf);
    try testing.expectError(error.FileNotFound, repl.loadFile(load_abs, stream.writer()));
}

test "showType reports compile error" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    var saw_err = false;
    if (repl.showType("(if)", stream.writer())) |_| {} else |_| {
        saw_err = true;
    }
    try testing.expect(saw_err);
}

test "handleReadlineResult evals buffered input on error" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var out_buf: [256]u8 = undefined;
    var out_stream = std.io.fixedBufferStream(&out_buf);

    var input_buf = std.ArrayList(u8){};
    defer input_buf.deinit(allocator);
    try input_buf.appendSlice(allocator, "42");

    const ReadErr = error{ReadFailed};
    try testing.expectError(ReadErr.ReadFailed, repl.handleReadlineResult(ReadErr.ReadFailed, &input_buf, out_stream.writer()));

    const written = out_buf[0..out_stream.pos];
    try testing.expect(std.mem.indexOf(u8, written, "42") != null);
}
test "eval cons" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(car (cons 1 2))");
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "eval if true" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(if t 1 2)");
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "eval if false" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(if nil 1 2)");
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "eval comparison" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(< 5 10)");
    try testing.expect(result.eq(Value.t));

    const result2 = try evalString(allocator, &heap, "(> 5 10)");
    try testing.expect(result2.isNil());
}

test "evalPrint prints multiple values on separate lines" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var out = std.ArrayList(u8){};
    defer out.deinit(allocator);

    try repl.evalPrint("(values 1 2 3)", out.writer(allocator));
    try testing.expectEqualStrings("1\n2\n3\n", out.items);
}

test "evalPrint clears secondary values between evaluations" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var out = std.ArrayList(u8){};
    defer out.deinit(allocator);

    try repl.evalPrint("(values 10 20)", out.writer(allocator));
    out.clearRetainingCapacity();
    try repl.evalPrint("42", out.writer(allocator));
    try testing.expectEqualStrings("42\n", out.items);
}

test "eval expands macro forms" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    _ = try repl.eval("(defmacro eval-macro (x) x)");
    const result = try repl.eval("(eval '(eval-macro 42))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval type predicate" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalString(allocator, &heap, "(consp (cons 1 2))");
    try testing.expect(result.eq(Value.t));

    const result2 = try evalString(allocator, &heap, "(null nil)");
    try testing.expect(result2.eq(Value.t));
}

test "eval parse error propagates" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    try testing.expectError(error.UnterminatedList, evalString(allocator, &heap, "("));
}

test "eval compile error propagates" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    try testing.expectError(error.InvalidLambda, evalString(allocator, &heap, "(lambda 1)"));
}

test "showType parse error propagates" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var buf = std.ArrayList(u8){};
    defer buf.deinit(allocator);

    try testing.expectError(error.UnterminatedList, repl.showType("(", buf.writer(allocator)));
}

test "repl chunk pool survives GC between evals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(define mk (lambda (x) (lambda () x)))");

    // Regress: GC between evals used to see a dangling chunk_pool slice.
    _ = try repl.vm.collectGarbage();

    _ = try repl.eval("(define f (mk 42))");
    const result = try repl.eval("(f)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "nested eval keeps transient chunk rooted across GC" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 2 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval(
        "(defun hold-and-eval (n acc)\n" ++
            "  (if (= n 0)\n" ++
            "      (eval '(+ 20 22))\n" ++
            "      (hold-and-eval (- n 1) (cons n acc))))",
    );

    const result1 = try repl.eval("(hold-and-eval 2000 nil)");
    try testing.expect(result1.isFixnum());
    try testing.expectEqual(@as(i64, 42), result1.toFixnum());

    const result2 = try repl.eval("(hold-and-eval 2000 nil)");
    try testing.expect(result2.isFixnum());
    try testing.expectEqual(@as(i64, 42), result2.toFixnum());
}

test "handler-case catches type-error from symbol-package" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // symbol-package on a fixnum should signal type-error, caught by handler-case
    const result = try repl.eval(
        "(handler-case (symbol-package 42) (type-error (c) (declare (ignore c)) :caught))",
    );
    try testing.expect(result.isKeyword());

    const kw = result.toPtr(@import("../runtime/objects.zig").Keyword);
    try testing.expectEqualStrings("CAUGHT", kw.getName());
}

test "symbol-package on keyword returns KEYWORD package" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval("(package-name (symbol-package :test))");
    try testing.expect(result.isString());
    const str = result.toPtr(@import("../runtime/objects.zig").String);
    try testing.expectEqualStrings("KEYWORD", str.bytes());
}

test "specialize pass produces fixnum_add for (the fixnum) operands" {
    // Verify that (+ (the fixnum a) (the fixnum b)) uses fixnum_add at runtime
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // (the fixnum x) wraps operands with assert_fixnum, enabling fixnum_add specialization
    const result = try repl.eval("(+ (the fixnum 10) (the fixnum 20))");
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "specialize pass produces fixnum_sub for (the fixnum) operands" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval("(- (the fixnum 50) (the fixnum 20))");
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "specialize pass - literal fixnum addition specializes" {
    // Literal fixnums are also recognized as proven fixnum by the specialize pass
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval("(+ 3 4)");
    try testing.expectEqual(@as(i64, 7), result.toFixnum());
}

test "populateKnownFns: no-preseed control does not fail" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var failing = FailAfterAllocator.init(allocator, 0);
    repl.allocator = failing.allocator();

    var known_fns = std.StringHashMap(jit_backend.KnownFn).init(repl.allocator);
    defer known_fns.deinit();

    try repl.populateKnownFns(&known_fns);
    try testing.expectEqual(@as(usize, 0), known_fns.count());
}

test "doHoistCompile: preseeded known_fns first insert failure returns failed" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    defer repl.vm.jit_fns.clearRetainingCapacity();

    var seeded: jit_backend.CompiledFn = .{
        .mem = undefined,
        .fn_ptr = @ptrCast(&fakeJitTarget),
        .arity = 0,
        .allocator = allocator,
        .name = "PRESEEDED",
    };
    try repl.vm.jit_fns.append(allocator, .{ .chunk = Value.nil, .compiled = &seeded });
    const before = repl.vm.jit_fns.items.len;

    var failing = FailAfterAllocator.init(allocator, 0);
    repl.allocator = failing.allocator();

    var body: Ir = .{ .lit = Value.makeFixnum(1) };
    var lambda_ir: Ir = .{ .lambda = .{
        .params = &.{},
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = &body,
        .speed = 3,
        .safety = 0,
    } };

    const result = repl.doHoistCompile(&lambda_ir, "OOM-KNOWN-FNS", repl.vm.chunk);
    try testing.expect(result == .failed);
    try testing.expectEqual(before, repl.vm.jit_fns.items.len);
}

test "populateKnownFns: partial-map failure after successful inserts" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    defer repl.vm.jit_fns.clearRetainingCapacity();

    var seeded: [16]jit_backend.CompiledFn = undefined;
    var name_bufs: [16][16]u8 = undefined;
    for (0..seeded.len) |i| {
        const name = try std.fmt.bufPrint(&name_bufs[i], "PRESEEDED-{d}", .{i});
        seeded[i] = .{
            .mem = undefined,
            .fn_ptr = @ptrCast(&fakeJitTarget),
            .arity = 0,
            .allocator = allocator,
            .name = name,
        };
        try repl.vm.jit_fns.append(allocator, .{ .chunk = Value.nil, .compiled = &seeded[i] });
    }

    var failing = FailAfterAllocator.init(allocator, 1);
    repl.allocator = failing.allocator();

    var known_fns = std.StringHashMap(jit_backend.KnownFn).init(repl.allocator);
    defer known_fns.deinit();

    try testing.expectError(error.OutOfMemory, repl.populateKnownFns(&known_fns));
    try testing.expect(known_fns.count() > 0);
    try testing.expect(failing.op_count > 1);
}

test "repl stream globals survive repeated gc" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const names = [_][]const u8{
        "COMMON-LISP:*QUERY-IO*",
        "COMMON-LISP:*DEBUG-IO*",
        "COMMON-LISP:*TRACE-OUTPUT*",
        "COMMON-LISP:*TERMINAL-IO*",
    };
    var idxs: [names.len]u16 = undefined;
    for (names, 0..) |name, i| {
        idxs[i] = repl.compiler.globals.lookup(name) orelse return error.TestUnexpectedResult;
    }

    var cycle: usize = 0;
    while (cycle < 4) : (cycle += 1) {
        _ = try repl.vm.collectGarbage();
        for (idxs) |idx| {
            const val = repl.vm.globals[idx];
            try testing.expect(val.isStream());
            const kind_raw = @as(*const u64, @ptrFromInt(val.toPtrAddr())).*;
            try testing.expectEqual(@as(u64, @intFromEnum(runtime.objects.BoxedKind.stream)), kind_raw);
        }
    }
}
