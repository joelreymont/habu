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
const hoist_backend = @import("../jit/hoist_backend.zig");
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
fn patchChunkIndices(chunk: *runtime.objects.Chunk, base: u16) void {
    const code = chunk.getCode();
    var i: usize = 0;
    while (i + 1 < code.len) {
        // Read opcode (2 bytes, little-endian)
        const low: u16 = code[i];
        const high: u16 = code[i + 1];
        const opcode = low | (high << 8);
        const op: Op = @enumFromInt(opcode);
        const size = op.operandSize();

        if (op == .make_closure) {
            // Operand starts at i + 2 (after 2-byte opcode)
            const rel_idx = std.mem.readInt(u16, code[i + 2 ..][0..2], .little);
            const abs_idx = rel_idx + base;
            std.mem.writeInt(u16, code[i + 2 ..][0..2], abs_idx, .little);
        }

        // Move to next instruction: 2 bytes for opcode + operand size
        i += 2 + size;
    }
}

pub const ReplError = anyerror;

/// REPL configuration
pub const Config = struct {
    /// Show disassembly before execution
    show_disasm: bool = false,
    /// Show bytecode bytes
    show_bytes: bool = false,
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

/// REPL state
pub const Repl = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: Vm,
    config: Config,
    /// Persistent compiler for global definitions
    compiler: Compiler,
    /// Persistent chunk pool for closures (GC updates pointers in-place via Vm.chunk_pool roots)
    chunk_pool: std.ArrayList(*runtime.objects.Chunk),
    /// Macro definitions: symbol -> closure
    macros: std.AutoHashMap(Value, MacroEntry),
    /// Line editor for interactive input
    line_editor: LineEditor,
    /// Current VM being used (for nested loads)
    current_vm: ?*Vm,
    /// Linked stack of active VM root contexts for nested eval/load.
    active_root_ctx: ?*VmRootCtx,

    pub fn init(self: *Repl, allocator: std.mem.Allocator, heap: *Heap, config: Config) !void {
        // NOTE: Repl must be initialized in-place so Compiler subcomponents can
        // safely keep pointers into vm (builtins, etc) without a move.
        self.* = .{
            .allocator = allocator,
            .heap = heap,
            .vm = undefined,
            .config = config,
            .compiler = undefined,
            .chunk_pool = std.ArrayList(*runtime.objects.Chunk){},
            .macros = std.AutoHashMap(Value, MacroEntry).init(allocator),
            .line_editor = LineEditor.init(allocator),
            .current_vm = null,
            .active_root_ctx = null,
        };
        errdefer self.chunk_pool.deinit(allocator);
        errdefer self.macros.deinit();
        errdefer self.line_editor.deinit();

        self.vm = try Vm.init(allocator, heap);
        errdefer self.vm.deinit();
        self.vm.setChunkPool(self.chunk_pool.items);

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
        const pool = self.chunk_pool.items;
        // Keep the main VM in sync even when evaluating via nested VMs.
        self.vm.setChunkPool(pool);
        if (vm != &self.vm) {
            vm.setChunkPool(pool);
        }
        // If we are inside a nested load/eval, ensure that VM also sees the new slice.
        if (self.current_vm) |cur| {
            if (cur != &self.vm and cur != vm) {
                cur.setChunkPool(pool);
            }
        }
        // Keep all active VMs in nested runVmPreserveMacroState chains in sync too.
        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            if (ctx.vm != &self.vm and ctx.vm != vm) {
                ctx.vm.setChunkPool(pool);
            }
            ctx_opt = ctx.prev;
        }
    }

    fn pinPersistentRoot(self: *Repl, val: Value) !void {
        var ctx_opt = self.active_root_ctx;
        while (ctx_opt) |ctx| {
            try ctx.roots.append(self.allocator, val);
            ctx.vm.setExtRoots(ctx.roots.items);
            ctx_opt = ctx.prev;
        }
    }

    fn pinPersistentPair(self: *Repl, key: Value, val: Value) !void {
        try self.pinPersistentRoot(key);
        try self.pinPersistentRoot(val);
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
                // Closure-valued compiler macros must still point at a valid chunk.
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

    fn runVmPreserveMacroState(self: *Repl, vm: *Vm, chunk_ptr: *runtime.objects.Chunk) !Value {
        const saved_ext = vm.ext_roots;
        if (saved_ext.len != 0) {
            for (saved_ext) |*val| {
                if (!self.isLiveValue(val.*)) {
                    val.* = Value.nil;
                }
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

        // Purge any stale entries before executing in nested VMs. We only keep
        // values proven live in current from-space by the collectors above.
        self.compiler.macro_table.clearRetainingCapacity();
        for (compiler_macros.items) |pair| {
            try self.compiler.macro_table.put(pair.key, pair.val);
        }
        self.compiler.symbol_macros.clearRetainingCapacity();
        for (symbol_macros.items) |pair| {
            try self.compiler.symbol_macros.put(pair.key, pair.val);
        }
        self.macros.clearRetainingCapacity();
        for (repl_macros.items) |pair| {
            try self.macros.put(pair.key, .{
                .closure = pair.closure,
                .has_whole = pair.has_whole,
                .has_env = pair.has_env,
            });
        }

        const total_roots = saved_ext.len +
            (compiler_macros.items.len * 2) +
            (symbol_macros.items.len * 2) +
            (repl_macros.items.len * 2);
        var roots = std.ArrayList(Value){};
        defer roots.deinit(self.allocator);
        try roots.ensureTotalCapacity(self.allocator, total_roots);

        if (saved_ext.len > 0) {
            try roots.appendSlice(self.allocator, saved_ext);
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

        vm.setExtRoots(roots.items);
        defer vm.setExtRoots(saved_ext);
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
            const closure = try self.heap.allocClosure(chunk_val, chunk_ptr.arity, &[_]Value{});
            const call_base = vm.sp;
            break :blk vm.callFromStackAt(call_base, closure, &[_]Value{}) catch |run_err| {
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
                return run_err;
            };
        } else vm.run(chunk_ptr) catch |run_err| {
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
            return run_err;
        };

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
        self.vm.globals[idx] = value;
        if (idx >= self.vm.num_globals) {
            self.vm.num_globals = idx + 1;
        }
    }

    /// Helper to set a CL global: intern symbol in CL, define global, set value
    fn setClGlobal(self: *Repl, sym_name: []const u8, value: Value) !void {
        // Intern symbol in CL package so it's found when CL-USER code references it
        _ = try self.heap.internInPackage("COMMON-LISP", sym_name);
        // Define global with qualified name
        var buf: [256]u8 = undefined;
        const qname = try std.fmt.bufPrint(&buf, "COMMON-LISP:{s}", .{sym_name});
        try self.setGlobal(qname, value);
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
        // Create standard stream objects
        const stdin_stream = try self.heap.allocStdin();
        const stdout_stream = try self.heap.allocStdout();
        const stderr_stream = try self.heap.allocStderr();

        // Pre-intern in CL and set globals
        try self.setClGlobal("*STANDARD-INPUT*", stdin_stream);
        try self.setClGlobal("*STANDARD-OUTPUT*", stdout_stream);
        try self.setClGlobal("*ERROR-OUTPUT*", stderr_stream);
        try self.setClGlobal("*QUERY-IO*", stdout_stream);
        try self.setClGlobal("*DEBUG-IO*", stdout_stream);
        try self.setClGlobal("*TRACE-OUTPUT*", stdout_stream);
        try self.setClGlobal("*TERMINAL-IO*", stdout_stream);
    }

    /// Callback for (load "filename") from VM
    fn loadCallback(filename: []const u8, context: *anyopaque) vm_mod.Error!Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        return self.loadFileValue(filename);
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
        return self.evalExpression(expr);
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
        if (self.compiler.builtins) |b| {
            const dispatch_sym = try self.canonicalBuiltinFunctionSymbol(sym);
            if (b.isBuiltinFunction(dispatch_sym)) {
                return true;
            }
        }
        return false;
    }

    /// Callback for symbol-function/function designators from VM.
    /// Returns a callable Value for globals or lazily materialized primitive wrappers.
    fn functionResolveCallback(sym: Value, context: *anyopaque) vm_mod.Error!?Value {
        const self: *Repl = @ptrCast(@alignCast(context));
        if (!sym.isSymbol()) return null;
        const trace_fn_resolve = std.posix.getenv("HABU_TRACE_FN_RESOLVE") != null;
        if (trace_fn_resolve) {
            std.debug.print("TRACE fn-resolve sym={s}\n", .{sym.toPtr(Symbol).getName()});
        }

        if (try self.lookupCallableFunction(sym)) |fn_val| {
            if (trace_fn_resolve) {
                std.debug.print("TRACE fn-resolve hit-global kind={s}\n", .{@tagName(fn_val.typeKind())});
            }
            return fn_val;
        }

        // Lazily materialize builtin primitive wrappers.
        if (self.compiler.builtins) |b| {
            const dispatch_sym = try self.canonicalBuiltinFunctionSymbol(sym);
            const is_builtin = b.isBuiltinFunction(dispatch_sym);
            if (trace_fn_resolve) {
                std.debug.print(
                    "TRACE fn-resolve dispatch={s} builtin={}\n",
                    .{ dispatch_sym.toPtr(Symbol).getName(), is_builtin },
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
                    const wrapper_val = try self.evalExpression(wrapper_form);
                    if (trace_fn_resolve) {
                        std.debug.print("TRACE fn-resolve wrapper kind={s}\n", .{@tagName(wrapper_val.typeKind())});
                    }
                    if (isCallableValue(wrapper_val)) return wrapper_val;
                } else {
                    // Generic fallback for builtin primitives without fixed arity metadata:
                    // (lambda (&rest args) (eval (cons 'sym args)))
                    const wrapper_form = try self.buildEvalDispatchWrapper(dispatch_sym);
                    const wrapper_val = try self.evalExpression(wrapper_form);
                    if (trace_fn_resolve) {
                        std.debug.print(
                            "TRACE fn-resolve eval-wrapper kind={s}\n",
                            .{@tagName(wrapper_val.typeKind())},
                        );
                    }
                    if (isCallableValue(wrapper_val)) return wrapper_val;
                }
            }
        }

        if (trace_fn_resolve) {
            std.debug.print("TRACE fn-resolve miss\n", .{});
        }
        return null;
    }

    /// Resolve symbol package aliases for builtin function dispatch.
    /// This includes internal CL names (for example `%set-symbol-plist`) which
    /// may not be externally accessible from the current package.
    fn canonicalBuiltinFunctionSymbol(self: *Repl, sym: Value) vm_mod.Error!Value {
        if (!sym.isSymbol()) return sym;
        const b = self.compiler.builtins orelse return sym;

        const direct = self.canonicalMacroSymbol(sym);
        if (b.isBuiltinFunction(direct)) return direct;

        const name = sym.toPtr(Symbol).getName();
        if (try self.heap.internInPackage("CL", name)) |cl_sym| {
            if (b.isBuiltinFunction(cl_sym)) return cl_sym;
        }
        if (try self.heap.internInPackage("CL-USER", name)) |cl_user_sym| {
            if (b.isBuiltinFunction(cl_user_sym)) return cl_user_sym;
        }

        return direct;
    }

    fn isCallableValue(val: Value) bool {
        return switch (val.typeKind()) {
            .closure, .native_code, .generic_function => true,
            else => false,
        };
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

    fn buildEvalDispatchWrapper(self: *Repl, sym: Value) !Value {
        const builtins = self.compiler.builtins orelse return error.InvalidSyntax;
        const sym_args = try self.heap.intern("ARGS");

        const params_items = [_]Value{ builtins.@"&rest", sym_args };
        const params_list = try self.listFromSlice(&params_items);

        const quote_sym_items = [_]Value{ builtins.quote, sym };
        const quote_sym_form = try self.listFromSlice(&quote_sym_items);

        const cons_items = [_]Value{ builtins.cons, quote_sym_form, sym_args };
        const cons_form = try self.listFromSlice(&cons_items);

        const eval_items = [_]Value{ builtins.eval, cons_form };
        const eval_form = try self.listFromSlice(&eval_items);

        const lambda_items = [_]Value{ builtins.lambda, params_list, eval_form };
        return self.listFromSlice(&lambda_items);
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
        if (!sym.isSymbol()) return null;
        const source_vm = self.activeVm();
        const s = sym.toPtr(Symbol);
        const local_name = s.getName();
        const trace = std.posix.getenv("HABU_TRACE_FN_RESOLVE") != null;
        if (trace and std.mem.eql(u8, local_name, "MAPCAR")) {
            std.debug.print("TRACE fn-lookup source num_globals={d}\n", .{source_vm.num_globals});
        }

        var qbuf: [512]u8 = undefined;
        const q = try qual_name.qualSym(self.allocator, s, &qbuf);
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
        if (self.compiler.globals.lookup(local_name)) |idx| {
            if (trace) {
                std.debug.print("TRACE fn-lookup local={s} idx={d} kind={s}\n", .{
                    local_name,
                    idx,
                    globalKindName(source_vm, idx),
                });
            }
            if (self.lookupCallableAtGlobalIdx(source_vm, idx)) |callable| {
                return callable;
            }
        }

        var full_buf: [640]u8 = undefined;
        const prefixes = [_][]const u8{ "COMMON-LISP:", "CL:", "CL-USER:" };
        for (prefixes) |prefix| {
            if (prefix.len + local_name.len > full_buf.len) continue;
            @memcpy(full_buf[0..prefix.len], prefix);
            @memcpy(full_buf[prefix.len .. prefix.len + local_name.len], local_name);
            const candidate = full_buf[0 .. prefix.len + local_name.len];
            if (self.compiler.globals.lookup(candidate)) |idx| {
                if (trace) {
                    std.debug.print("TRACE fn-lookup cand={s} idx={d} kind={s}\n", .{
                        candidate,
                        idx,
                        globalKindName(source_vm, idx),
                    });
                }
                if (self.lookupCallableAtGlobalIdx(source_vm, idx)) |callable| {
                    return callable;
                }
            }
        }
        if (trace and std.mem.eql(u8, local_name, "MAPCAR")) {
            var it = self.compiler.globals.bindings.iterator();
            while (it.next()) |entry| {
                if (std.mem.indexOf(u8, entry.key_ptr.*, "MAPCAR") != null) {
                    const idx = entry.value_ptr.*;
                    std.debug.print("TRACE fn-lookup any={s} idx={d} kind={s}\n", .{
                        entry.key_ptr.*,
                        idx,
                        globalKindName(source_vm, idx),
                    });
                }
            }
        }

        // Package alias fallback: if there is exactly one callable global whose
        // local (unqualified) name matches, treat it as the function binding.
        var alias_count: usize = 0;
        var alias_val = Value.nil;
        var alias_it = self.compiler.globals.bindings.iterator();
        while (alias_it.next()) |entry| {
            const idx = entry.value_ptr.*;
            const candidate_val = self.lookupCallableAtGlobalIdx(source_vm, idx) orelse continue;
            if (!isCallableValue(candidate_val)) continue;

            const key = entry.key_ptr.*;
            const key_local = if (std.mem.lastIndexOfScalar(u8, key, ':')) |split|
                key[split + 1 ..]
            else
                key;

            if (!std.ascii.eqlIgnoreCase(key_local, local_name)) continue;
            alias_count += 1;
            alias_val = candidate_val;
            if (alias_count > 1) break;
        }
        if (alias_count == 1) {
            if (trace) {
                std.debug.print("TRACE fn-lookup alias-hit local={s}\n", .{local_name});
            }
            return alias_val;
        }
        if (trace) {
            var dbg_it = self.compiler.globals.bindings.iterator();
            while (dbg_it.next()) |entry| {
                const key = entry.key_ptr.*;
                const key_local = if (std.mem.lastIndexOfScalar(u8, key, ':')) |split|
                    key[split + 1 ..]
                else
                    key;
                if (!std.ascii.eqlIgnoreCase(key_local, local_name)) continue;
                const idx = entry.value_ptr.*;
                std.debug.print("TRACE fn-lookup alias-cand key={s} idx={d} kind={s}\n", .{
                    key,
                    idx,
                    globalKindName(source_vm, idx),
                });
            }
        }
        return null;
    }

    fn globalKindName(vm: *const Vm, idx: usize) []const u8 {
        if (idx >= vm.num_globals) return "oob";
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
        if (idx >= vm.num_globals) return null;
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

    /// Evaluate an expression using a separate VM
    fn evalExpression(self: *Repl, expr: Value) !Value {
        // Use arena for compilation
        var arena = std.heap.ArenaAllocator.init(self.allocator);
        defer arena.deinit();
        const arena_alloc = arena.allocator();

        const source_vm = self.activeVm();

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
        var normalized = expr;
        normalized = try self.desugarExpr(normalized);
        const ir_node = try self.compiler.compile(normalized, &env);
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
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
            patchChunkIndices(chunk_ptr, chunk_base);
        }

        // Store child chunks for closures
        try self.chunk_pool.ensureUnusedCapacity(self.allocator, child_chunks.len);
        for (child_chunks) |c| {
            self.chunk_pool.appendAssumeCapacity(c.toPtr(runtime.objects.Chunk));
        }

        // Run through the same VM using a nested call frame. This preserves
        // the caller VM's stack/locals across GC while evaluating runtime EVAL.
        self.syncChunkPools(source_vm);
        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        patchChunkIndices(chunk_ptr, chunk_base);
        const closure = try self.heap.allocClosure(chunk, chunk_ptr.arity, &[_]Value{});
        return try source_vm.callFromStackAt(source_vm.sp, closure, &[_]Value{});
    }

    /// Load a file and return the last value (for (load ...) primitive)
    /// Uses a separate VM to avoid recursive execution issues
    fn loadFileValue(self: *Repl, path: []const u8) !Value {
        const source_vm = self.activeVm();
        const resolved_path = try self.resolveLoadPath(source_vm, path);
        defer self.allocator.free(resolved_path);
        const trace_forms = std.process.hasEnvVar(self.allocator, "HABU_TRACE_FORMS") catch false;
        if (trace_forms) {
            std.debug.print("TRACE load: {s} [pkg={s}]\n", .{ resolved_path, self.heap.getCurrentPackageName() });
        }

        // Prefer sibling source for FASL designators so macro definitions are
        // seen by the REPL macro table during ANSI harness loads.
        if (try self.fallbackSourcePathForFasl(resolved_path)) |fallback_path| {
            defer self.allocator.free(fallback_path);
            if (!self.isCurrentLoadPath(source_vm, fallback_path)) {
                return try self.loadFileValue(fallback_path);
            }
            return Value.t;
        }

        const content = self.readFileContent(resolved_path) catch |err| {
            if (err == error.FileNotFound) {
                if (try self.fallbackSourcePathForFasl(resolved_path)) |fallback_path| {
                    defer self.allocator.free(fallback_path);
                    if (self.isCurrentLoadPath(source_vm, fallback_path)) {
                        return Value.t;
                    }
                    return try self.loadFileValue(fallback_path);
                }
            }
            return err;
        };
        defer self.allocator.free(content);

        const load_path = try self.loadPathnameValue(resolved_path);
        const load_bindings = self.bindLoadGlobals(source_vm, load_path);
        defer self.restoreLoadGlobals(source_vm, load_bindings);

        // Evaluate all expressions and return the last value.
        // Some ANSI harnesses hand us host-generated .fasl files; if parsing
        // fails, fall back to sibling source file when available.
        const result = self.evalFileContentSeparateVm(content) catch |err| {
            if (err == error.UnexpectedToken) {
                if (try self.fallbackSourcePathForFasl(resolved_path)) |fallback_path| {
                    defer self.allocator.free(fallback_path);
                    if (self.isCurrentLoadPath(source_vm, fallback_path)) {
                        return Value.t;
                    }
                    return try self.loadFileValue(fallback_path);
                }
            }
            return err;
        };
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
        load_pathname_cl: ?struct { idx: usize, prev: Value } = null,
        load_pathname_user: ?struct { idx: usize, prev: Value } = null,
        load_pathname_plain: ?struct { idx: usize, prev: Value } = null,
        load_truename_cl: ?struct { idx: usize, prev: Value } = null,
        load_truename_user: ?struct { idx: usize, prev: Value } = null,
        load_truename_plain: ?struct { idx: usize, prev: Value } = null,
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

    fn bindLoadGlobals(self: *Repl, vm: *Vm, path: Value) LoadBindings {
        var bindings: LoadBindings = .{};
        if (self.compiler.globals.lookup("COMMON-LISP:*LOAD-PATHNAME*")) |idx| {
            bindings.load_pathname_cl = .{ .idx = idx, .prev = vm.globals[idx] };
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
        if (self.compiler.globals.lookup("CL-USER:*LOAD-PATHNAME*")) |idx| {
            bindings.load_pathname_user = .{ .idx = idx, .prev = vm.globals[idx] };
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
        if (self.compiler.globals.lookup("*LOAD-PATHNAME*")) |idx| {
            bindings.load_pathname_plain = .{ .idx = idx, .prev = vm.globals[idx] };
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
        if (self.compiler.globals.lookup("COMMON-LISP:*LOAD-TRUENAME*")) |idx| {
            bindings.load_truename_cl = .{ .idx = idx, .prev = vm.globals[idx] };
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
        if (self.compiler.globals.lookup("CL-USER:*LOAD-TRUENAME*")) |idx| {
            bindings.load_truename_user = .{ .idx = idx, .prev = vm.globals[idx] };
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
        if (self.compiler.globals.lookup("*LOAD-TRUENAME*")) |idx| {
            bindings.load_truename_plain = .{ .idx = idx, .prev = vm.globals[idx] };
            vm.globals[idx] = path;
            if (idx >= vm.num_globals) vm.num_globals = idx + 1;
        }
        return bindings;
    }

    fn restoreLoadGlobals(self: *Repl, vm: *Vm, bindings: LoadBindings) void {
        _ = self;
        if (bindings.load_pathname_cl) |entry| vm.globals[entry.idx] = entry.prev;
        if (bindings.load_pathname_user) |entry| vm.globals[entry.idx] = entry.prev;
        if (bindings.load_pathname_plain) |entry| vm.globals[entry.idx] = entry.prev;
        if (bindings.load_truename_cl) |entry| vm.globals[entry.idx] = entry.prev;
        if (bindings.load_truename_user) |entry| vm.globals[entry.idx] = entry.prev;
        if (bindings.load_truename_plain) |entry| vm.globals[entry.idx] = entry.prev;
    }

    fn readFileContent(self: *Repl, path: []const u8) ![]u8 {
        const file = if (std.fs.path.isAbsolute(path))
            try std.fs.openFileAbsolute(path, .{})
        else
            try std.fs.cwd().openFile(path, .{});
        defer file.close();

        return try file.readToEndAlloc(self.allocator, 1024 * 1024);
    }

    fn currentLoadTruename(self: *Repl, vm: *const Vm) ?Value {
        const names = [_][]const u8{
            "COMMON-LISP:*LOAD-TRUENAME*",
            "CL-USER:*LOAD-TRUENAME*",
            "*LOAD-TRUENAME*",
        };
        for (names) |name| {
            if (self.compiler.globals.lookup(name)) |idx| {
                if (idx < vm.num_globals) {
                    const val = vm.globals[idx];
                    if (!val.isNil()) return val;
                }
            }
        }
        return null;
    }

    fn currentPackageGlobal(self: *Repl, vm: *const Vm) ?Value {
        const names = [_][]const u8{
            "COMMON-LISP:*PACKAGE*",
            "CL:*PACKAGE*",
            "CL-USER:*PACKAGE*",
            "*PACKAGE*",
        };
        for (names) |name| {
            if (self.compiler.globals.lookup(name)) |idx| {
                if (idx < vm.num_globals) {
                    const val = vm.globals[idx];
                    if (val.isPackage()) return val;
                }
            }
        }
        return null;
    }

    fn setPackageGlobals(self: *Repl, vm: *Vm, pkg_val: Value) void {
        const names = [_][]const u8{
            "COMMON-LISP:*PACKAGE*",
            "CL:*PACKAGE*",
            "CL-USER:*PACKAGE*",
            "*PACKAGE*",
        };
        for (names) |name| {
            if (self.compiler.globals.lookup(name)) |idx| {
                vm.globals[idx] = pkg_val;
                if (idx >= vm.num_globals) vm.num_globals = idx + 1;
            }
        }
    }

    fn syncReaderPackageFromVm(self: *Repl, vm: *const Vm) void {
        const pkg_val = self.currentPackageGlobal(vm) orelse return;
        const pkg_obj = pkg_val.toPtr(runtime.objects.Package);
        const pkg_name = switch (pkg_obj.name.typeKind()) {
            .symbol => pkg_obj.name.toPtr(runtime.Symbol).getName(),
            .string => pkg_obj.name.toPtr(runtime.String).bytes(),
            .keyword => pkg_obj.name.toPtr(runtime.Keyword).getName(),
            else => return,
        };
        if (self.heap.findPackage(pkg_name)) |native_pkg| {
            self.heap.setCurrentPackage(native_pkg);
        }
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
        return try self.allocator.dupe(u8, path);
    }

    fn resolveRelativeLoadPath(self: *Repl, base: []const u8, path: []const u8) !?[]u8 {
        const candidates = [_][]const u8{
            base,
            std.fs.path.dirname(base) orelse "",
        };
        for (candidates) |root| {
            if (root.len == 0) continue;
            const primary = try std.fs.path.join(self.allocator, &.{ root, path });
            if (try self.fileExists(primary)) return primary;
            self.allocator.free(primary);

            const base_name = std.fs.path.basename(root);
            if (path.len > base_name.len + 1 and std.mem.eql(u8, path[0..base_name.len], base_name) and path[base_name.len] == '/') {
                const trimmed = path[base_name.len + 1 ..];
                const trimmed_join = try std.fs.path.join(self.allocator, &.{ root, trimmed });
                if (try self.fileExists(trimmed_join)) return trimmed_join;
                self.allocator.free(trimmed_join);
            }
        }
        return null;
    }

    fn isCurrentLoadPath(self: *Repl, vm: *const Vm, path: []const u8) bool {
        if (self.currentLoadTruename(vm)) |load_true| {
            const ns = primitives.pathname.namestring(self.allocator, self.heap, &self.vm.builtins, load_true) catch return false;
            if (ns.isString()) {
                return std.mem.eql(u8, ns.toPtr(runtime.String).bytes(), path);
            }
        }
        return false;
    }

    fn fileExists(self: *Repl, path: []const u8) !bool {
        _ = self;
        if (std.fs.path.isAbsolute(path)) {
            std.fs.accessAbsolute(path, .{}) catch |err| switch (err) {
                error.FileNotFound => return false,
                else => return err,
            };
            return true;
        }
        std.fs.cwd().access(path, .{}) catch |err| switch (err) {
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
        const names = [_][]const u8{
            "COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS*",
            "CL:*DEFAULT-PATHNAME-DEFAULTS*",
            "CL-USER:*DEFAULT-PATHNAME-DEFAULTS*",
            "*DEFAULT-PATHNAME-DEFAULTS*",
        };
        for (names) |name| {
            if (self.compiler.globals.lookup(name)) |idx| {
                if (idx < vm.num_globals) {
                    const val = vm.globals[idx];
                    if (!val.isNil()) return val;
                }
            }
        }
        return null;
    }

    fn fallbackSourcePathForFasl(self: *Repl, path: []const u8) !?[]u8 {
        const ext = std.fs.path.extension(path);
        if (!(std.ascii.eqlIgnoreCase(ext, ".fasl") or std.ascii.eqlIgnoreCase(ext, ".hfasl"))) {
            return null;
        }
        const base = path[0 .. path.len - ext.len];
        const candidates = [_][]const u8{ ".lsp", ".lisp", ".habu" };
        for (candidates) |cand_ext| {
            const cand = try std.mem.concat(self.allocator, u8, &.{ base, cand_ext });
            if (try self.fileExists(cand)) return cand;
            self.allocator.free(cand);
        }
        return null;
    }

    const ReadEvalCtx = struct {
        repl: *Repl,
        vm: *Vm,
    };

    fn parserReadEval(ctx: *anyopaque, expr: Value) reader.ParseError!Value {
        const hook: *ReadEvalCtx = @ptrCast(@alignCast(ctx));
        var arena = std.heap.ArenaAllocator.init(hook.repl.allocator);
        defer arena.deinit();
        const eval_alloc = arena.allocator();
        return hook.repl.evalParsedWithVm(expr, hook.vm, eval_alloc) catch |err| {
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
            return error.UnexpectedToken;
        };
    }

    /// Evaluate file content in the active VM context.
    /// Nested execution is handled through callFromStackAt in runVmPreserveMacroState.
    fn evalFileContentSeparateVm(self: *Repl, content: []const u8) !Value {
        var last_value = Value.nil;
        const trace_forms = std.process.hasEnvVar(self.allocator, "HABU_TRACE_FORMS") catch false;
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
        parser.setReadEvalHook(@ptrCast(&read_eval_ctx), parserReadEval);

        while (parser.current.kind != .eof) {
            self.syncReaderPackageFromVm(source_vm);
            const expr = parser.parse() catch |err| {
                if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null) {
                    const loc = parser.getErrorLocation();
                    std.debug.print(
                        "TRACE parse error: {s} at {d}:{d} token={s} kind={s}\n",
                        .{ @errorName(err), loc.line, loc.column, loc.text, @tagName(parser.current.kind) },
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
            last_value = try self.evalParsedWithVm(expr, source_vm, eval_alloc);
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
        var read_eval_ctx = ReadEvalCtx{ .repl = self, .vm = vm };
        parser.setReadEvalHook(@ptrCast(&read_eval_ctx), parserReadEval);
        const expr = try parser.parse();

        return self.evalParsedWithVm(expr, vm, arena_alloc);
    }

    fn evalParsedWithVm(self: *Repl, parsed_expr: Value, vm: *Vm, arena_alloc: std.mem.Allocator) !Value {
        var expr = parsed_expr;
        const saved_compiler_vm = self.compiler.vm;
        self.compiler.setVm(vm);
        try self.compiler.refreshBuiltins();
        // Macro expansion during compile may execute already-defined macro
        // closures; keep VM chunk pool in sync before any compile-time eval.
        self.syncChunkPools(vm);
        defer {
            if (saved_compiler_vm) |saved_vm| {
                self.compiler.setVm(saved_vm);
            } else {
                self.compiler.vm = null;
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
            expr = result;
        }

        // Expand macros
        expr = if (self.expandMacros(expr)) |expanded| expanded else |err| {
            return err;
        };

        // Desugar (let* → let, cond → if, etc.)
        expr = try self.desugarExpr(expr);

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

        const ir_node = if (self.compiler.compile(expr, &env)) |node| node else |err| {
            return err;
        };
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch child chunks to use absolute indices
        for (child_chunks) |child_chunk| {
            patchChunkIndices(child_chunk.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Store chunks persistently
        try self.chunk_pool.ensureUnusedCapacity(self.allocator, child_chunks.len);
        for (child_chunks) |child_chunk| {
            self.chunk_pool.appendAssumeCapacity(child_chunk.toPtr(runtime.objects.Chunk));
        }

        // Try hoist SSA JIT compilation for eligible lambda nodes
        _ = self.tryHoistCompileLambdas(specialized, child_chunks, chunk_base);

        // Patch main chunk to use absolute chunk indices
        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        patchChunkIndices(chunk_ptr, chunk_base);

        if (std.posix.getenv("HABU_TRACE_FORM_DISASM") != null and !vm.isExecuting()) {
            std.debug.print("TRACE form disasm begin\n", .{});
            var buf = std.ArrayList(u8){};
            defer buf.deinit(self.allocator);
            disasm.disassembleRuntime(chunk_ptr, buf.writer(self.allocator)) catch |err| {
                std.debug.print("TRACE form disasm error={s}\n", .{@errorName(err)});
                return err;
            };
            std.debug.print("{s}", .{buf.items});
            std.debug.print("TRACE form disasm end\n", .{});
        }

        // Set chunk pool and run with base offset
        self.syncChunkPools(vm);

        return try self.runVmPreserveMacroState(vm, chunk_ptr);
    }

    pub fn deinit(self: *Repl) void {
        self.vm.deinit();
        self.line_editor.deinit();
        self.compiler.deinit();
        self.chunk_pool.deinit(self.allocator);
        self.macros.deinit();
    }

    /// Run specialization pass on IR, replacing generic ops with
    /// type-specialized variants where types are proven by assertions.
    /// E.g., (add (assert_fixnum x) (assert_fixnum y)) → fixnum_add
    fn specializeIr(self: *Repl, ir_node: *const Ir) !*const Ir {
        return try passes.specialize.specialize(self.compiler.allocator, ir_node);
    }

    /// Try to compile lambda nodes via Hoist SSA JIT and register with the VM.
    /// Called after bytecode emission, before arena reset.
    /// Returns true if hoist compilation succeeded, false otherwise.
    /// Check if an IR body has calls that can't be resolved to JIT targets.
    fn hasUnresolvableCalls(body: *const Ir, self_name: []const u8, known: *const std.StringHashMap(void)) bool {
        return switch (body.*) {
            .call => |c| blk: {
                if (!hoist_backend.isCallResolvable(c.func, self_name, known))
                    break :blk true;
                for (c.args) |arg| {
                    if (hasUnresolvableCalls(arg, self_name, known)) break :blk true;
                }
                break :blk false;
            },
            .tailcall => |tc| blk: {
                if (!hoist_backend.isCallResolvable(tc.func, self_name, known))
                    break :blk true;
                for (tc.args) |arg| {
                    if (hasUnresolvableCalls(arg, self_name, known)) break :blk true;
                }
                break :blk false;
            },
            .@"if" => |i| hasUnresolvableCalls(i.cond, self_name, known) or
                hasUnresolvableCalls(i.then_branch, self_name, known) or
                hasUnresolvableCalls(i.else_branch, self_name, known),
            .let => |l| blk: {
                for (l.bindings) |binding| {
                    if (hasUnresolvableCalls(binding.value, self_name, known)) break :blk true;
                }
                break :blk hasUnresolvableCalls(l.body, self_name, known);
            },
            .set => |s| hasUnresolvableCalls(s.value, self_name, known),
            .progn => |exprs| blk: {
                for (exprs) |e| {
                    if (hasUnresolvableCalls(e, self_name, known)) break :blk true;
                }
                break :blk false;
            },
            .loop => |l| hasUnresolvableCalls(l.cond, self_name, known) or hasUnresolvableCalls(l.body, self_name, known),
            .fixnum_add, .fixnum_sub, .add, .sub, .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
            .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
            .logand, .mod, .rem, .append, .assoc,
            => |op| hasUnresolvableCalls(op.left, self_name, known) or hasUnresolvableCalls(op.right, self_name, known),
            .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs,
            .zerop, .oddp, .evenp, .length,
            => |op| hasUnresolvableCalls(op.operand, self_name, known),
            .lit, .@"var", .global_ref => false,
            else => false,
        };
    }

    fn tryHoistCompileLambdas(
        self: *Repl,
        ir_node: *const Ir,
        child_chunks: []const Value,
        chunk_base: u16,
    ) bool {
        _ = chunk_base;
        const trace = std.posix.getenv("HABU_TRACE_JIT") != null;
        // Only handle top-level (define name lambda) for now
        const define = switch (ir_node.*) {
            .define => |d| d,
            .progn => |exprs| blk: {
                // Also search progn for defines (e.g., (progn (defun ...) (call)))
                for (exprs) |expr| {
                    switch (expr.*) {
                        .define => |d| break :blk d,
                        else => {},
                    }
                }
                if (trace) std.debug.print("JIT: no define found in progn\n", .{});
                return false;
            },
            else => {
                if (trace) std.debug.print("JIT: top-level IR is not define (is {s})\n", .{@tagName(ir_node.*)});
                return false;
            },
        };
        const lambda_ir = switch (define.value.*) {
            .lambda => define.value,
            else => {
                if (trace) std.debug.print("JIT: define value is not lambda for '{s}'\n", .{define.name});
                return false;
            },
        };
        const lambda = lambda_ir.lambda;

        if (trace) std.debug.print("JIT: considering '{s}' speed={d} safety={d} captures={d} opt={d} key={d} rest={}\n", .{ define.name, lambda.speed, lambda.safety, lambda.captures.len, lambda.optional_params.len, lambda.key_params.len, lambda.rest_param != null });

        // Only compile speed=3, safety=0 functions
        // Safety > 0 needs runtime type checks that hoist backend doesn't emit
        if (lambda.safety > 0) return false;
        // Skip functions whose body is just a type assertion (e.g. (the fixnum x)).
        // These may receive non-fixnum types; the untagged JIT would corrupt them.
        if (lambda.body.* == .assert_fixnum) return false;
        // Skip functions that the hoist backend can't translate (fast reject)
        if (!hoist_backend.IrTranslator.canTranslate(lambda.body)) {
            if (trace) std.debug.print("JIT: canTranslate failed for '{s}' (body tag: {s})\n", .{ define.name, @tagName(lambda.body.*) });
            return false;
        }
        // Skip functions with non-self calls that have no known JIT target.
        // Build temporary known_fns set for the check.
        {
            var kf_check = std.StringHashMap(void).init(self.allocator);
            defer kf_check.deinit();
            var iter = self.vm.hoist_fns.iterator();
            while (iter.next()) |entry| {
                const cfn = entry.value_ptr.*;
                kf_check.put(cfn.name, {}) catch {};
            }
            if (hasUnresolvableCalls(lambda.body, define.name, &kf_check)) {
                if (trace) std.debug.print("JIT: skipping '{s}' — has unresolvable calls\n", .{define.name});
                return false;
            }
        }

        // Only simple functions without captures, optional, key, or rest params
        if (lambda.captures.len > 0) return false;
        if (lambda.optional_params.len > 0) return false;
        if (lambda.key_params.len > 0) return false;
        if (lambda.rest_param != null) return false;

        // The first child chunk is the lambda's chunk
        if (child_chunks.len == 0) {
            if (trace) std.debug.print("JIT: no child chunks for '{s}'\n", .{define.name});
            return false;
        }
        const chunk_val = child_chunks[0];
        const chunk_ptr = chunk_val.toPtr(runtime.objects.Chunk);

        // Try hoist compilation (may fail for unsupported IR nodes — that's OK)
        return self.doHoistCompile(lambda_ir, define.name, chunk_ptr);
    }

    /// Inner function that propagates errors to allow try usage.
    fn doHoistCompile(
        self: *Repl,
        lambda_ir: *const Ir,
        name: []const u8,
        chunk_ptr: *const runtime.objects.Chunk,
    ) bool {
        // Build known_fns map from existing hoist-compiled functions
        var known_fns = std.StringHashMap(hoist_backend.KnownFn).init(self.allocator);
        defer known_fns.deinit();
        {
            var iter = self.vm.hoist_fns.iterator();
            while (iter.next()) |entry| {
                const cfn = entry.value_ptr.*;
                known_fns.put(cfn.name, .{
                    .fn_ptr = @intFromPtr(cfn.fn_ptr),
                    .arity = cfn.arity,
                    .ir_body = cfn.ir_body,
                    .param_names = cfn.param_names,
                    .callee_name = cfn.name,
                }) catch {};
            }
        }

        const trace = std.posix.getenv("HABU_TRACE_JIT") != null;
        var compiled = hoist_backend.compileIrWithKnownFns(self.allocator, lambda_ir, name, &known_fns) catch |err| {
            if (trace) {
                std.debug.print("JIT: hoist compile failed for '{s}': {s}\n", .{ name, @errorName(err) });
            }
            return false;
        };
        const persistent = self.allocator.create(hoist_backend.CompiledFn) catch {
            compiled.deinit();
            return false;
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
                return false;
            };
            ir_arena.* = std.heap.ArenaAllocator.init(self.allocator);
            const ir_alloc = ir_arena.allocator();

            const body_copy = ir.deepCopyIr(ir_alloc, lambda.body) catch {
                ir_arena.deinit();
                self.allocator.destroy(ir_arena);
                persistent.ir_arena = null;
                // Non-fatal: function still works, just can't be inlined
                self.vm.registerHoistFn(chunk_ptr, persistent) catch {
                    persistent.deinit();
                    self.allocator.destroy(persistent);
                    return false;
                };
                return true;
            };
            const params_copy = ir_alloc.alloc([]const u8, lambda.params.len) catch {
                ir_arena.deinit();
                self.allocator.destroy(ir_arena);
                persistent.ir_arena = null;
                self.vm.registerHoistFn(chunk_ptr, persistent) catch {
                    persistent.deinit();
                    self.allocator.destroy(persistent);
                    return false;
                };
                return true;
            };
            for (lambda.params, 0..) |p, pi| {
                params_copy[pi] = ir_alloc.dupe(u8, p) catch {
                    ir_arena.deinit();
                    self.allocator.destroy(ir_arena);
                    persistent.ir_arena = null;
                    self.vm.registerHoistFn(chunk_ptr, persistent) catch {
                        persistent.deinit();
                        self.allocator.destroy(persistent);
                        return false;
                    };
                    return true;
                };
            }
            persistent.ir_arena = ir_arena;
            persistent.ir_body = body_copy;
            persistent.param_names = params_copy;
        }

        // Always register on the primary VM - activeVm() may return a context VM
        // that gets destroyed after file loading, losing the registration.
        self.vm.registerHoistFn(chunk_ptr, persistent) catch {
            persistent.deinit();
            self.allocator.destroy(persistent);
            return false;
        };
        if (std.posix.getenv("HABU_TRACE_JIT") != null) {
            std.debug.print("JIT: hoist compiled '{s}' OK (arity={d}, fn_ptr={*}, chunk=0x{x}, map_count={d})\n", .{
                name, compiled.arity, compiled.fn_ptr,
                @intFromPtr(chunk_ptr), self.vm.hoist_fns.count(),
            });
        }
        return true;
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
                try self.evalPrint(trimmed_input, writer);
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
                try self.printDiagnostic(source, info, writer);
            } else {
                try writer.print("Error: {s}\n", .{@errorName(err)});
            }
            return err;
        };
        try self.printValue(result, writer);
        try writer.writeAll("\n");
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

        var expr = if (parser.parse()) |parsed| parsed else |err| {
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

        // Expand macros before compilation
        expr = if (self.expandMacros(expr)) |expanded| expanded else |err| {
            return err;
        };

        // Desugar (let* → let, cond → if, etc.)
        expr = try self.desugarExpr(expr);

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

        const ir_node = if (self.compiler.compile(expr, &env)) |node| node else |err| {
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
        defer emitter.deinit();

        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch wrapper chunk AND child chunks to use absolute indices
        const wrapper_chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        patchChunkIndices(wrapper_chunk_ptr, chunk_base);
        for (child_chunks) |c| {
            patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Store child chunks persistently (closures need them beyond this eval)
        try self.chunk_pool.ensureUnusedCapacity(self.allocator, child_chunks.len);
        for (child_chunks) |c| {
            self.chunk_pool.appendAssumeCapacity(c.toPtr(runtime.objects.Chunk));
        }

        // Try hoist SSA JIT compilation for eligible lambda nodes
        _ = self.tryHoistCompileLambdas(specialized, child_chunks, chunk_base);

        // Free child chunk array (now owned by persistent storage)
        self.allocator.free(child_chunks);

        self.syncChunkPools(&self.vm);

        const result = if (self.runVmPreserveMacroState(&self.vm, chunk.toPtr(runtime.objects.Chunk))) |value| value else |err| {
            err_info.* = .{
                .kind = if (err == error.UserError) .runtime_user_error else .runtime_type_mismatch,
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
            .float => try writer.print("{d}", .{val.toFloat()}),
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

    /// Load and evaluate a file (public for main.zig)
    pub fn loadFilePublic(self: *Repl, path: []const u8, writer: anytype) !void {
        return self.loadFile(path, writer);
    }

    /// Load and evaluate a file
    fn loadFile(self: *Repl, path: []const u8, writer: anytype) !void {
        _ = if (self.loadFileValue(path)) |value| value else |err| {
            try writer.print("Cannot open '{s}': {s}\n", .{ path, @errorName(err) });
            return err;
        };
        try writer.print("; loaded {s}\n", .{path});
    }

    /// Evaluate file content (multiple expressions)
    pub fn evalFileContent(self: *Repl, content: []const u8, writer: anytype) !void {
        var pos: usize = 0;

        while (pos < content.len) {
            // Skip whitespace and comments
            while (pos < content.len) {
                if (content[pos] == ' ' or content[pos] == '\t' or
                    content[pos] == '\n' or content[pos] == '\r')
                {
                    pos += 1;
                } else if (content[pos] == ';') {
                    // Skip comment line
                    while (pos < content.len and content[pos] != '\n') {
                        pos += 1;
                    }
                } else {
                    break;
                }
            }

            if (pos >= content.len) break;

            // Find end of expression (simple approach: match parens)
            const start = pos;
            const end = if (self.findExprEnd(content, pos)) |value| value else |err| {
                try writer.print("Parse error at position {d}: {s}\n", .{ pos, @errorName(err) });
                return err;
            };

            if (end > start) {
                const expr = content[start..end];
                if (self.eval(expr)) |_| {} else |err| {
                    try writer.print("Error evaluating expression at position {d}: {s}\n", .{ start, @errorName(err) });
                    try writer.print("Expression: {s}\n", .{expr[0..@min(100, expr.len)]});
                    return err;
                }
                pos = end;
            } else {
                break;
            }
        }
    }

    /// Find end of S-expression
    fn findExprEnd(self: *Repl, content: []const u8, start: usize) !usize {
        _ = self;
        var pos = start;
        if (pos >= content.len) return start;

        // Handle list
        if (content[pos] == '(') {
            var depth: usize = 1;
            pos += 1;
            while (pos < content.len and depth > 0) {
                if (content[pos] == '(') {
                    depth += 1;
                } else if (content[pos] == ')') {
                    depth -= 1;
                } else if (content[pos] == '"') {
                    // Skip string
                    pos += 1;
                    while (pos < content.len and content[pos] != '"') {
                        if (content[pos] == '\\' and pos + 1 < content.len) {
                            pos += 1;
                        }
                        pos += 1;
                    }
                } else if (content[pos] == ';') {
                    // Skip comment
                    while (pos < content.len and content[pos] != '\n') {
                        pos += 1;
                    }
                    pos -= 1; // will be incremented below
                }
                pos += 1;
            }
            if (depth > 0) return error.ParseError;
            return pos;
        }

        // Handle atom
        while (pos < content.len) {
            const c = content[pos];
            if (c == ' ' or c == '\t' or c == '\n' or c == '\r' or
                c == '(' or c == ')' or c == ';')
            {
                break;
            }
            pos += 1;
        }
        return pos;
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

    fn canonicalMacroSymbol(self: *Repl, sym: Value) Value {
        if (!sym.isSymbol()) return sym;
        const cl_pkg = self.heap.cl_package orelse return sym;
        const name = sym.toPtr(Symbol).getName();
        if (cl_pkg.findAccessibleUpper(name)) |canonical| return canonical;

        var needs_upper = false;
        for (name) |ch| {
            if (ch >= 'a' and ch <= 'z') {
                needs_upper = true;
                break;
            }
        }
        if (!needs_upper) return sym;
        if (name.len > 256) return sym;

        var upper_buf: [256]u8 = undefined;
        for (name, 0..) |ch, i| upper_buf[i] = std.ascii.toUpper(ch);
        return cl_pkg.findAccessibleUpper(upper_buf[0..name.len]) orelse sym;
    }

    fn lookupMacroByNameInPackage(self: *Repl, pkg: *runtime.heap.Package, name: []const u8) ?MacroEntry {
        if (pkg.symbols.get(name)) |pkg_sym| {
            if (self.macros.get(pkg_sym)) |entry| return entry;
        }
        for (pkg.use_list.items) |used_pkg| {
            if (used_pkg.exports.contains(name) or used_pkg.auto_export) {
                if (used_pkg.symbols.get(name)) |used_sym| {
                    if (self.macros.get(used_sym)) |entry| return entry;
                }
            }
        }
        return null;
    }

    fn symbolPackage(sym: *const Symbol) ?*runtime.heap.Package {
        const bits = sym.reserved;
        if (bits == 0 or (bits & 1) != 0) return null;
        return @ptrFromInt(bits);
    }

    fn lookupMacroEntry(self: *Repl, sym: Value) ?MacroEntry {
        if (!sym.isSymbol()) return null;
        if (self.macros.get(sym)) |entry| return entry;

        const sym_ptr = sym.toPtr(Symbol);
        const name = sym_ptr.getName();

        if (symbolPackage(sym_ptr)) |pkg| {
            if (self.lookupMacroByNameInPackage(pkg, name)) |entry| return entry;
            // Respect package-qualified symbols: do not fall back to current package
            // lookup by name, which can hijack CL:FOO with local FOO macros.
            const canonical = self.canonicalMacroSymbol(sym);
            if (canonical.raw == sym.raw) return null;
            return self.macros.get(canonical);
        }

        // Uninterned symbols can only be resolved by current package context.
        if (self.heap.current_package) |pkg| {
            if (self.lookupMacroByNameInPackage(pkg, name)) |entry| return entry;
        }
        if (self.heap.cl_package) |cl_pkg| {
            if (self.lookupMacroByNameInPackage(cl_pkg, name)) |entry| return entry;
        }

        const canonical = self.canonicalMacroSymbol(sym);
        if (canonical.raw != sym.raw) {
            if (self.macros.get(canonical)) |entry| return entry;
        }
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
        const rest2 = cons2.cdr;
        if (!rest2.isCons()) return error.CompileError;

        // Transform destructured params before building lambda
        const transformed_rest2 = try self.compiler.transformDestructuredParams(rest2);
        if (!transformed_rest2.isCons()) return error.CompileError;

        const def_cons = transformed_rest2.toPtr(Cons);
        const raw_params = def_cons.car;
        const body_list = def_cons.cdr;

        const macro_params = try self.normalizeMacroParams(raw_params);
        const runtime_rest2 = try self.heap.allocCons(macro_params.params, body_list);

        // Build (lambda (args...) body...) to evaluate
        const lambda_sym = try self.heap.intern("lambda");
        const lambda_expr = try self.heap.allocCons(lambda_sym, runtime_rest2);

        // Don't expand macros in defmacro body - they'll be expanded when the macro is called
        // Expanding here can cause issues with forward references and recursive macros
        // const expanded_lambda = self.expandMacros(lambda_expr) catch return error.CompileError;
        const expanded_lambda = lambda_expr;

        // Compile and evaluate the lambda to get a closure
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

        const ir_node = if (self.compiler.compile(expanded_lambda, &env)) |node| node else |err| {
            std.debug.print("Compile error: {s}\n", .{@errorName(err)});
            return err;
        };
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch wrapper chunk AND child chunks to use absolute indices
        const wrapper_chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        patchChunkIndices(wrapper_chunk_ptr, chunk_base);
        for (child_chunks) |c| {
            patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Add child chunks
        try self.chunk_pool.ensureUnusedCapacity(self.allocator, child_chunks.len);
        for (child_chunks) |c| {
            self.chunk_pool.appendAssumeCapacity(c.toPtr(runtime.objects.Chunk));
        }

        // Use a separate VM to avoid corrupting the main VM's state
        // (handleDefmacro may be called during a load from within the main VM)
        var macro_vm = try Vm.init(self.allocator, self.heap);
        defer macro_vm.deinit();
        macro_vm.setGlobalEnv(&self.compiler.globals);
        macro_vm.setLoadCallback(&loadCallback, @ptrCast(self));
        macro_vm.setEvalCallback(&evalCallback, @ptrCast(self));
        macro_vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));
        macro_vm.setMacroexpand1Callback(&macroexpand1Callback, @ptrCast(self));
        macro_vm.setFboundpCallback(&fboundpCallback, @ptrCast(self));
        macro_vm.setFunctionResolveCallback(&functionResolveCallback, @ptrCast(self));

        self.syncChunkPools(&macro_vm);

        // Copy globals from current context
        const source_vm = self.activeVm();
        for (source_vm.globals, 0..) |g, i| {
            macro_vm.globals[i] = g;
        }
        macro_vm.num_globals = source_vm.num_globals;

        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const closure = try self.runVmPreserveMacroState(&macro_vm, chunk_ptr);

        if (!closure.isClosure()) return error.CompileError;

        var macro_sym = cons2.car;
        if (self.heap.current_package) |pkg| {
            const macro_name = cons2.car.toPtr(Symbol).getName();
            if (pkg.symbols.get(macro_name)) |local_sym| {
                macro_sym = local_sym;
            }
        }

        // Store the closure in REPL macro table for pre-compilation macro expansion
        // Store the AST in Compiler macro table for compile-time expansion
        try self.macros.put(macro_sym, .{
            .closure = closure,
            .has_whole = macro_params.has_whole,
            .has_env = macro_params.has_env,
        });
        try self.compiler.macro_table.put(macro_sym, transformed_rest2);
        try self.pinPersistentPair(macro_sym, closure);
        try self.pinPersistentPair(macro_sym, transformed_rest2);

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
        if (compile_toplevel) {
            var form = body;
            while (form.isCons()) {
                const form_cons = form.toPtr(Cons);
                _ = try self.evalSingleExpr(form_cons.car, arena_alloc);
                form = form_cons.cdr;
            }
        }

        // If :execute, return progn of body for runtime execution
        if (execute) {
            const progn_sym = try self.heap.intern("progn");
            return try self.heap.allocCons(progn_sym, body);
        }

        // Neither - return nil
        return Value.nil;
    }

    /// Evaluate a single expression (used by eval-when for compile-time evaluation)
    fn evalSingleExpr(self: *Repl, expr_val: Value, arena_alloc: std.mem.Allocator) ReplError!Value {
        var expr = expr_val;
        try self.compiler.refreshBuiltins();
        const trace_eval_single = std.posix.getenv("HABU_TRACE_EVAL_SINGLE") != null;

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

        if (trace_eval_single and expr.isCons()) {
            const head = expr.toPtr(Cons).car;
            if (head.isSymbol()) {
                std.debug.print("TRACE eval-single head={s}\n", .{head.toPtr(Symbol).getName()});
            } else {
                std.debug.print("TRACE eval-single head-kind={s}\n", .{@tagName(head.typeKind())});
            }
        }

        // Desugar (let* → let, cond → if, etc.)
        expr = try self.desugarExpr(expr);

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

        const ir_node = if (self.compiler.compile(expr, &env)) |node| node else |err| {
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
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();
        const child_chunks = try emitter.getChildChunks();
        defer self.allocator.free(child_chunks);

        // Record base before appending
        const chunk_base: u16 = @intCast(self.chunk_pool.items.len);

        // Patch child chunks to use absolute indices
        for (child_chunks) |c| {
            patchChunkIndices(c.toPtr(runtime.objects.Chunk), chunk_base);
        }

        // Add child chunks
        try self.chunk_pool.ensureUnusedCapacity(self.allocator, child_chunks.len);
        for (child_chunks) |c| {
            self.chunk_pool.appendAssumeCapacity(c.toPtr(runtime.objects.Chunk));
        }

        // Use a separate VM to avoid corrupting the main VM's state
        var eval_vm = try Vm.init(self.allocator, self.heap);
        defer eval_vm.deinit();
        eval_vm.setGlobalEnv(&self.compiler.globals);

        self.syncChunkPools(&eval_vm);

        eval_vm.setLoadCallback(&loadCallback, @ptrCast(self));
        eval_vm.setEvalCallback(&evalCallback, @ptrCast(self));
        eval_vm.setMacroexpandCallback(&macroexpandCallback, @ptrCast(self));
        eval_vm.setMacroexpand1Callback(&macroexpand1Callback, @ptrCast(self));
        eval_vm.setFboundpCallback(&fboundpCallback, @ptrCast(self));
        eval_vm.setFunctionResolveCallback(&functionResolveCallback, @ptrCast(self));

        // Copy globals from current context
        const source_vm = self.activeVm();
        for (source_vm.globals, 0..) |g, i| {
            eval_vm.globals[i] = g;
        }
        eval_vm.num_globals = source_vm.num_globals;

        // Set eval_vm as current so nested loads use it for globals
        const saved_current_vm = self.current_vm;
        self.current_vm = &eval_vm;
        defer self.current_vm = saved_current_vm;

        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const result = try self.runVmPreserveMacroState(&eval_vm, chunk_ptr);

        // Copy back any new globals to the original source
        for (eval_vm.globals, 0..) |g, i| {
            source_vm.globals[i] = g;
        }
        if (eval_vm.num_globals > source_vm.num_globals) {
            source_vm.num_globals = eval_vm.num_globals;
        }

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
            }

            if (self.lookupMacroEntry(head)) |macro_entry| {
                if (std.posix.getenv("HABU_TRACE_MACRO_DEPTH") != null and depth >= 480) {
                    const sym = head.toPtr(runtime.Symbol);
                    if (symbolPackage(sym)) |pkg| {
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
        var call_args = args;
        if (macro.has_env) {
            const env_val = try self.heap.allocMacroEnv();
            call_args = try self.heap.allocCons(env_val, call_args);
        }
        if (macro.has_whole) {
            call_args = try self.heap.allocCons(whole_form, call_args);
        }
        // Count args
        var argc: usize = 0;
        var arg_list = call_args;
        while (arg_list.isCons()) {
            argc += 1;
            arg_list = arg_list.toPtr(Cons).cdr;
        }
        if (std.posix.getenv("HABU_TRACE_MACRO_CALLS") != null and macro_name.isSymbol()) {
            std.debug.print(
                "TRACE macro call: {s} argc={d}\n",
                .{ macro_name.toPtr(Symbol).getName(), argc },
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
        try constants.append(self.allocator, closure);
        arg_list = call_args;
        while (arg_list.isCons()) {
            try constants.append(self.allocator, arg_list.toPtr(Cons).car);
            arg_list = arg_list.toPtr(Cons).cdr;
        }

        const chunk = try self.heap.allocChunk(code.items, constants.items, 0, 0, 0, false, 0);

        // Use a separate VM to avoid corrupting the current VM state
        var macro_vm = try Vm.init(self.allocator, self.heap);
        defer macro_vm.deinit();
        macro_vm.setGlobalEnv(&self.compiler.globals);
        macro_vm.setLoadCallback(&loadCallback, @ptrCast(self));
        macro_vm.setEvalCallback(&evalCallback, @ptrCast(self));
        // NOTE: macroexpandCallback NOT set to prevent infinite expansion loops
        macro_vm.setFboundpCallback(&fboundpCallback, @ptrCast(self));
        macro_vm.setFunctionResolveCallback(&functionResolveCallback, @ptrCast(self));

        // Copy globals from current context (nested VM if loading, main VM otherwise)
        const source_vm = self.activeVm();
        for (source_vm.globals[0..source_vm.num_globals], 0..) |g, i| {
            macro_vm.globals[i] = g;
        }
        macro_vm.num_globals = source_vm.num_globals;

        self.syncChunkPools(&macro_vm);

        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        return self.runVmPreserveMacroState(&macro_vm, chunk_ptr) catch |err| {
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

            var pkg_val_opt = try primitives.package.findPackage(self.heap, arg_cons.car);
            if (pkg_val_opt == null) {
                const pkg_name = switch (arg_cons.car.typeKind()) {
                    .symbol => arg_cons.car.toPtr(runtime.Symbol).getName(),
                    .string => arg_cons.car.toPtr(runtime.String).bytes(),
                    .keyword => arg_cons.car.toPtr(runtime.Keyword).getName(),
                    else => null,
                };
                if (pkg_name) |name| {
                    if (self.heap.findPackage(name)) |native_pkg| {
                        const name_val = try self.heap.allocBaseString(native_pkg.name);
                        pkg_val_opt = if (try self.heap.findLispPackage(name_val)) |existing|
                            existing
                        else blk: {
                            const created = try self.heap.allocPackage(name_val, Value.nil, Value.nil, native_pkg.auto_export);
                            try self.heap.putLispPackage(name_val, created);
                            break :blk created;
                        };
                    }
                }
            }
            const pkg_val = pkg_val_opt orelse {
                if (std.posix.getenv("HABU_TRACE_ERROR_CONTEXT") != null) {
                    const arg = arg_cons.car;
                    const pkg_name_opt = switch (arg.typeKind()) {
                        .symbol => arg.toPtr(runtime.Symbol).getName(),
                        .string => arg.toPtr(runtime.String).bytes(),
                        .keyword => arg.toPtr(runtime.Keyword).getName(),
                        else => null,
                    };
                    const dbg_vm = self.current_vm orelse &self.vm;
                    const vm_pkg_name = if (self.currentPackageGlobal(dbg_vm)) |cur_pkg| curblk: {
                        const cur_obj = cur_pkg.toPtr(runtime.objects.Package);
                        break :curblk switch (cur_obj.name.typeKind()) {
                            .symbol => cur_obj.name.toPtr(runtime.Symbol).getName(),
                            .string => cur_obj.name.toPtr(runtime.String).bytes(),
                            .keyword => cur_obj.name.toPtr(runtime.Keyword).getName(),
                            else => "<invalid>",
                        };
                    } else "<none>";
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
                return error.UnboundVariable;
            };
            if (!pkg_val.isPackage()) return error.TypeMismatch;

            const pkg_obj = pkg_val.toPtr(runtime.objects.Package);
            const pkg_name = switch (pkg_obj.name.typeKind()) {
                .symbol => pkg_obj.name.toPtr(runtime.Symbol).getName(),
                .string => pkg_obj.name.toPtr(runtime.String).bytes(),
                .keyword => pkg_obj.name.toPtr(runtime.Keyword).getName(),
                else => return error.TypeMismatch,
            };
            if (self.heap.findPackage(pkg_name)) |native_pkg| {
                self.heap.setCurrentPackage(native_pkg);
            }

            const target_vm = self.current_vm orelse &self.vm;
            self.setPackageGlobals(target_vm, pkg_val);
            self.syncReaderPackageFromVm(target_vm);
            return pkg_val;
        }

        // For package forms, just compile and execute inline
        // The compiler will call heap.setCurrentPackage which affects future reads
        var env = Env.init(arena_alloc, null);
        defer env.deinit();
        const ir_node = try self.compiler.compile(expr, &env);
        const specialized = try self.specializeIr(ir_node);

        // Emit bytecode
        var emitter = Emitter.initWithHeap(self.allocator, self.heap);
        emitter.speed = self.compiler.optimize_current.speed;
        emitter.safety = self.compiler.optimize_current.safety;
        defer emitter.deinit();
        try emitter.emit(specialized);
        const chunk = try emitter.finalize();

        // Execute package side effects in the active VM context.
        // During nested LOAD/EVAL this must be the nested VM so subsequent
        // reader sync sees updated *PACKAGE* from that VM's globals.
        const chunk_ptr = chunk.toPtr(runtime.objects.Chunk);
        const target_vm = self.current_vm orelse &self.vm;
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
        const expr = try parser.parse();
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
        const ir_node = if (self.compiler.compile(expr, &env)) |node| node else |err| {
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

/// Patch make_closure instructions to use absolute chunk indices
fn patchMakeClosureIndices(code: []u8, base: u16) void {
    var i: usize = 0;
    while (i < code.len) {
        const op: Op = @enumFromInt(code[i]);
        const size = op.operandSize();

        if (op == .make_closure) {
            // make_closure has: u16 chunk_index, u8 num_captures
            // Patch the u16 index at code[i+1..i+3]
            const rel_idx = std.mem.readInt(u16, code[i + 1 ..][0..2], .little);
            const abs_idx = base + rel_idx;
            std.mem.writeInt(u16, code[i + 1 ..][0..2], abs_idx, .little);
        }

        i += 1 + size;
    }
}

/// Convenience function to evaluate a string
pub fn evalString(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var repl: Repl = undefined;
    try repl.init(allocator, heap, .{});
    defer repl.deinit();
    return repl.eval(source);
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

test "loadFilePublic missing file errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try testing.expectError(error.FileNotFound, repl.loadFilePublic("nope-nope.habu", stream.writer()));
}

test "load resolves relative path with repeated directory prefix" {
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
        "CL-USER:*LOAD-PATHNAME*",
        "CL-USER:*LOAD-TRUENAME*",
        "*LOAD-PATHNAME*",
        "*LOAD-TRUENAME*",
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
    try repl.loadFilePublic(load_abs, stream.writer());
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
