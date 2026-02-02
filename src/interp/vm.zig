//! Bytecode Virtual Machine for Habu
//!
//! Stack-based interpreter that executes bytecode.
//! Designed for portability (WASM target).

comptime {
    @setEvalBranchQuota(5000);
}

const std = @import("std");
const builtin = @import("builtin");
const bytecode = @import("../bytecode/bytecode.zig");
const disasm = @import("../bytecode/disasm.zig");
const opcodes = @import("../bytecode/opcodes.zig");
const Op = bytecode.Op;
const Chunk = bytecode.Chunk;
const runtime = @import("../runtime/runtime.zig");
const primitives = @import("../runtime/primitives/primitives.zig");
const qual_name = @import("../runtime/qual_name.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const String = runtime.String;
const Symbol = runtime.Symbol;
const arith = @import("../runtime/primitives/arith.zig");
const io = @import("../runtime/primitives/io.zig");
const stringPrims = @import("../runtime/primitives/string.zig");
const char_primitives = @import("../runtime/primitives/char.zig");
const hash_prims = @import("../runtime/primitives/hash.zig");
const type_mod = @import("../runtime/primitives/type.zig");
const HashTable = runtime.HashTable;
const Vector = runtime.Vector;
const compiler = @import("../compiler/compiler.zig");
const GlobalEnv = compiler.GlobalEnv;
const Parser = @import("../reader/parser.zig").Parser;
const BuiltinSymbols = @import("../runtime/builtins.zig").BuiltinSymbols;

pub const Error = anyerror;

/// Catch frame for exception handling
pub const CatchFrame = struct {
    /// Tag value to match against
    tag: Value,
    /// Chunk to return to
    chunk: *const Chunk,
    /// IP to jump to when throw is caught
    catch_ip: usize,
    /// Stack pointer to restore
    catch_sp: usize,
    /// Frame pointer to restore
    catch_fp: usize,
};

/// Unwind frame for unwind-protect
pub const UnwindFrame = struct {
    /// Chunk containing the cleanup code
    chunk: *const Chunk,
    /// IP of cleanup code start
    cleanup_ip: usize,
    /// Stack pointer when push_unwind was executed
    unwind_sp: usize,
    /// Frame pointer when push_unwind was executed
    unwind_fp: usize,
};

/// Block frame for block/return-from (lexical non-local exit)
pub const BlockFrame = struct {
    /// Block name (interned symbol raw value for identity comparison)
    name_raw: Value,
    /// Chunk to return to
    chunk: *const Chunk,
    /// IP to jump to after block exits (past the block body)
    exit_ip: usize,
    /// Stack pointer to restore
    block_sp: usize,
    /// Frame pointer to restore
    block_fp: usize,
};

/// Restart frame for restart-case
pub const RestartFrame = struct {
    /// Restart name (interned symbol)
    name: Value,
    /// Chunk containing the restart handler
    chunk: *const Chunk,
    /// IP of restart handler code
    handler_ip: usize,
    /// Stack pointer to restore when restart is invoked
    restart_sp: usize,
    /// Frame pointer to restore when restart is invoked
    restart_fp: usize,
    /// Catch stack depth to restore
    catch_depth: usize,
    /// Unwind stack depth to restore
    unwind_depth: usize,
};

/// Handler frame for handler-bind
pub const HandlerFrame = struct {
    /// Condition type (symbol or list of types)
    condition_type: Value,
    /// Handler function (closure)
    handler_fn: Value,
};

/// Progv frame for dynamic variable binding
pub const ProgvFrame = struct {
    /// Saved global values (list of (symbol . old-value) pairs)
    saved_bindings: Value,
};

/// Call frame for function calls
pub const Frame = struct {
    /// Return address (chunk + ip)
    chunk: *const Chunk,
    return_ip: usize,
    /// Base pointer (stack index of first local)
    bp: usize,
    /// Current closure (for accessing captures)
    closure: ?*const runtime.Closure,
    /// Argument count passed to this function call
    argc: u8,
};

/// Let scope frame for nested let bindings
/// Tracks where this scope's locals start on the stack
pub const Scope = struct {
    /// Base pointer for this scope's locals (stack index)
    bp: usize,
    /// Number of locals in this scope
    num_locals: u8,
};

/// Maximum nested let scopes
const MAX_SCOPES = 256;

/// Saved execution state for nested calls
/// Used by callClosure to save/restore state atomically
const State = struct {
    chunk: *const Chunk,
    ip: usize,
    fp: usize,
    sp: usize,
    scope_sp: usize,
    chunk_pool: []*Chunk,
    chunk_base: usize,

    fn save(vm: *const Vm) State {
        return .{
            .chunk = vm.chunk,
            .ip = vm.ip,
            .fp = vm.fp,
            .sp = vm.sp,
            .scope_sp = vm.scope_sp,
            .chunk_pool = vm.chunk_pool,
            .chunk_base = vm.chunk_base,
        };
    }

    fn restore(self: State, vm: *Vm) void {
        vm.chunk = self.chunk;
        vm.ip = self.ip;
        vm.fp = self.fp;
        vm.sp = self.sp;
        vm.scope_sp = self.scope_sp;
        vm.chunk_pool = self.chunk_pool;
        vm.chunk_base = self.chunk_base;
    }
};

/// Virtual Machine
pub const Vm = struct {
    /// Value stack
    stack: [STACK_SIZE]Value,
    /// Stack pointer (next free slot)
    sp: usize,

    /// Call stack
    frames: [MAX_FRAMES]Frame,
    /// Frame pointer (current frame index)
    fp: usize,

    /// Current chunk being executed
    chunk: *const Chunk,
    /// Instruction pointer
    ip: usize,

    /// Heap for allocations
    heap: *Heap,

    /// Allocator
    allocator: std.mem.Allocator,

    /// Global variables (indexed by constant pool index)
    globals: [MAX_GLOBALS]Value,
    /// Number of defined globals
    num_globals: usize,

    /// Current package (special variable)
    current_package: Value,

    /// Chunk pool for closures (pointers to individually allocated chunks)
    chunk_pool: []*Chunk,
    /// Base offset for current eval's chunks
    chunk_base: usize,

    /// Catch stack for exception handling
    catch_stack: [MAX_CATCHES]CatchFrame,
    /// Catch stack pointer
    catch_sp: usize,

    /// Unwind stack for unwind-protect
    unwind_stack: [MAX_UNWINDS]UnwindFrame,
    /// Unwind stack pointer
    unwind_sp: usize,

    /// Restart stack for restart-case
    restart_stack: [MAX_RESTARTS]RestartFrame,
    /// Restart stack pointer
    restart_sp: usize,

    /// Block stack for block/return-from
    block_stack: [MAX_BLOCKS]BlockFrame,
    /// Block stack pointer
    block_sp: usize,

    /// Handler stack for handler-bind
    handler_stack: [MAX_HANDLERS]HandlerFrame,
    /// Handler stack pointer
    handler_sp: usize,

    /// Progv stack for dynamic variable binding
    progv_stack: [MAX_PROGVS]ProgvFrame,
    /// Progv stack pointer
    progv_sp: usize,

    /// Let scope stack for nested let bindings
    scope_stack: [MAX_SCOPES]Scope,
    /// Scope stack pointer (number of active scopes)
    scope_sp: usize,

    /// Saved throw state for unwinding through unwind-protect
    pending_throw_tag: Value,
    pending_throw_value: Value,
    pending_error: ?anyerror,
    is_unwinding: bool,

    /// Saved return-from state for unwinding through unwind-protect
    pending_block_name: Value,
    pending_block_value: Value,
    is_returning_from_block: bool,

    /// Random number generator state
    prng: std.Random.DefaultPrng,
    prng_seeded: bool,

    /// Secondary values buffer for multiple-value-bind
    secondary_values: [MAX_SECONDARY_VALUES]Value,
    /// Number of secondary values currently available
    secondary_values_count: usize,

    /// Global environment for boundp/fboundp lookups
    global_env: ?*const GlobalEnv,

    /// Callback for (load "filename") - set by REPL
    load_callback: ?*const fn ([]const u8, *anyopaque) Error!Value,
    load_context: ?*anyopaque,

    /// Callback for (eval expr) - set by REPL
    eval_callback: ?*const fn (Value, *anyopaque) Error!Value,
    eval_context: ?*anyopaque,

    /// Callback for (macroexpand expr) - set by REPL
    macroexpand_callback: ?*const fn (Value, *anyopaque) Error!Value,
    macroexpand_context: ?*anyopaque,

    /// Callback for (macroexpand-1 expr) - set by REPL
    macroexpand_1_callback: ?*const fn (Value, *anyopaque) Error!Value,
    macroexpand_1_context: ?*anyopaque,

    /// Callback for fboundp - checks if symbol is a function (macro, primitive, or defun)
    fboundp_callback: ?*const fn (Value, *anyopaque) Error!bool,
    fboundp_context: ?*anyopaque,

    /// Counter for gensym
    gensym_counter: u64,

    /// Current closure for load_capture when fp=0 (used by callClosure)
    current_closure: ?*const runtime.Closure,

    /// Current argc for load_argc when fp=0 (used by callClosure)
    current_argc: u8,

    /// Pre-interned builtin symbols for fast dispatch
    builtins: BuiltinSymbols,

    /// Pre-interned type symbols for runtime type dispatch
    type_syms: type_mod.TypeSymbols,

    const STACK_SIZE = 4096;
    const MAX_SECONDARY_VALUES = 20;
    const MAX_FRAMES = 256;
    const MAX_GLOBALS = 2048;
    const MAX_CATCHES = 32;
    const MAX_UNWINDS = 32;
    const MAX_RESTARTS = 64;
    const MAX_BLOCKS = 64;
    const MAX_PROGVS = 32;
    const MAX_HANDLERS = 64;

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) !Vm {
        var vm = Vm{
            .stack = undefined,
            .sp = 0,
            .frames = undefined,
            .fp = 0,
            .chunk = undefined,
            .ip = 0,
            .heap = heap,
            .allocator = allocator,
            .globals = undefined,
            .num_globals = 0,
            .current_package = Value.nil,
            .chunk_pool = &[_]*Chunk{},
            .chunk_base = 0,
            .catch_stack = undefined,
            .catch_sp = 0,
            .unwind_stack = undefined,
            .unwind_sp = 0,
            .restart_stack = undefined,
            .restart_sp = 0,
            .block_stack = undefined,
            .block_sp = 0,
            .handler_stack = undefined,
            .handler_sp = 0,
            .progv_stack = undefined,
            .progv_sp = 0,
            .scope_stack = undefined,
            .scope_sp = 0,
            .pending_throw_tag = Value.nil,
            .pending_throw_value = Value.nil,
            .pending_error = null,
            .is_unwinding = false,
            .pending_block_name = Value.nil,
            .pending_block_value = Value.nil,
            .is_returning_from_block = false,
            .prng = std.Random.DefaultPrng.init(0),
            .prng_seeded = false,
            .secondary_values = undefined,
            .secondary_values_count = 0,
            .global_env = null,
            .load_callback = null,
            .load_context = null,
            .eval_callback = null,
            .eval_context = null,
            .macroexpand_callback = null,
            .macroexpand_context = null,
            .macroexpand_1_callback = null,
            .macroexpand_1_context = null,
            .fboundp_callback = null,
            .fboundp_context = null,
            .gensym_counter = 0,
            .current_closure = null,
            .current_argc = 0,
            .builtins = try BuiltinSymbols.init(heap),
            .type_syms = try type_mod.TypeSymbols.init(heap),
        };
        // Initialize globals to nil
        for (&vm.globals) |*g| {
            g.* = Value.nil;
        }
        return vm;
    }

    /// Set the chunk pool for closures with a base offset (deprecated)
    pub fn setChunkPoolWithBase(self: *Vm, chunks: []*Chunk, base: usize) void {
        self.chunk_pool = chunks;
        self.chunk_base = base;
    }

    /// Set the chunk pool for closures (indices are absolute)
    pub fn setChunkPool(self: *Vm, chunks: []*Chunk) void {
        self.chunk_pool = chunks;
        self.chunk_base = 0;
    }

    /// Set the global environment for boundp/fboundp lookups
    pub fn setGlobalEnv(self: *Vm, env: *const GlobalEnv) void {
        self.global_env = env;
    }

    /// Set the load callback for (load "filename")
    pub fn setLoadCallback(self: *Vm, callback: *const fn ([]const u8, *anyopaque) Error!Value, context: *anyopaque) void {
        self.load_callback = callback;
        self.load_context = context;
    }

    /// Set the eval callback for (eval expr)
    pub fn setEvalCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!Value, context: *anyopaque) void {
        self.eval_callback = callback;
        self.eval_context = context;
    }

    /// Set the macroexpand callback for (macroexpand expr)
    pub fn setMacroexpandCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!Value, context: *anyopaque) void {
        self.macroexpand_callback = callback;
        self.macroexpand_context = context;
    }

    /// Set the macroexpand-1 callback for (macroexpand-1 expr)
    pub fn setMacroexpand1Callback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!Value, context: *anyopaque) void {
        self.macroexpand_1_callback = callback;
        self.macroexpand_1_context = context;
    }

    /// Set the fboundp callback for checking function bindings
    pub fn setFboundpCallback(self: *Vm, callback: *const fn (Value, *anyopaque) Error!bool, context: *anyopaque) void {
        self.fboundp_callback = callback;
        self.fboundp_context = context;
    }

    /// Allocate a cons cell, running GC if needed
    pub fn allocCons(self: *Vm, car: Value, cdr: Value) error{OutOfMemory}!Value {
        return if (self.heap.allocCons(car, cdr)) |val| val else |_| {
            _ = try self.collectGarbage();
            return try self.heap.allocCons(car, cdr);
        };
    }

    /// Allocate a vector, running GC if needed
    pub fn allocVector(self: *Vm, length: usize, capacity: usize) error{OutOfMemory, Overflow}!Value {
        return if (self.heap.allocVector(length, capacity)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbage();
                return try self.heap.allocVector(length, capacity);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate a string, running GC if needed
    pub fn allocString(self: *Vm, data: []const u8) error{OutOfMemory, Overflow}!Value {
        return if (self.heap.allocBaseString(data)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbage();
                return try self.heap.allocBaseString(data);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate an uninitialized string, running GC if needed
    pub fn allocStringUninitialized(self: *Vm, length: usize) error{ OutOfMemory, Overflow }!Value {
        return if (self.heap.allocStringUninitialized(length)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbage();
                return try self.heap.allocStringUninitialized(length);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate a symbol (uninterned), running GC if needed
    pub fn allocSymbol(self: *Vm, name: []const u8) error{OutOfMemory}!Value {
        return if (self.heap.allocSymbol(name)) |val| val else |_| {
            _ = try self.collectGarbage();
            return try self.heap.allocSymbol(name);
        };
    }

    /// Allocate a closure, running GC if needed
    pub fn allocClosureWithGC(self: *Vm, code: Value, arity: u32, captures: []const Value) error{ OutOfMemory, Overflow }!Value {
        return if (self.heap.allocClosure(code, arity, captures)) |val| val else |err| switch (err) {
            error.OutOfMemory => {
                _ = try self.collectGarbage();
                return try self.heap.allocClosure(code, arity, captures);
            },
            error.Overflow => return error.Overflow,
        };
    }

    /// Allocate a hash table, running GC if needed
    pub fn allocHashTable(self: *Vm, capacity: usize, test_type: runtime.HashTest) error{OutOfMemory}!Value {
        return if (self.heap.allocHashTable(capacity, test_type)) |val| val else |_| {
            _ = try self.collectGarbage();
            return try self.heap.allocHashTable(capacity, test_type);
        };
    }

    /// Intern a symbol, running GC if needed
    pub fn intern(self: *Vm, name: []const u8) error{OutOfMemory}!Value {
        return if (self.heap.intern(name)) |val| val else |_| {
            _ = try self.collectGarbage();
            return try self.heap.intern(name);
        };
    }

    /// Run garbage collection, using VM state as roots
    /// Returns bytes reclaimed
    fn syncPrintGlobals(self: *Vm) !void {
        if (self.global_env) |env| {
            if (env.lookup("*print-length*")) |idx| {
                const val = self.globals[idx];
                if (val.isFixnum()) {
                    const len: usize = @intCast(val.toFixnum());
                    io.print_length = len;
                } else {
                    io.print_length = null;
                }
            }
            if (env.lookup("*print-level*")) |idx| {
                const val = self.globals[idx];
                if (val.isFixnum()) {
                    const lvl: usize = @intCast(val.toFixnum());
                    io.print_level = lvl;
                } else {
                    io.print_level = null;
                }
            }
        }
    }

    fn handleSpecialVarLoad(self: *Vm, idx: u16) !Value {
        if (self.global_env) |env| {
            if (env.lookup("*print-escape*")) |esc_idx| {
                if (idx == esc_idx) return io.getPrintEscape();
            }
            if (env.lookup("*print-case*")) |case_idx| {
                if (idx == case_idx) return try io.getPrintCase(self.heap);
            }
            if (env.lookup("*print-readably*")) |read_idx| {
                if (idx == read_idx) return io.getPrintReadably();
            }
            if (env.lookup("*print-base*")) |base_idx| {
                if (idx == base_idx) return io.getPrintBase();
            }
            if (env.lookup("*print-radix*")) |radix_idx| {
                if (idx == radix_idx) return io.getPrintRadix();
            }
            if (env.lookup("*print-gensym*")) |gensym_idx| {
                if (idx == gensym_idx) return io.getPrintGensym();
            }
            if (env.lookup("*print-array*")) |array_idx| {
                if (idx == array_idx) return io.getPrintArray();
            }
        }
        return self.globals[idx];
    }

    fn handleSpecialVarStore(self: *Vm, idx: u16, val: Value) !void {
        if (self.global_env) |env| {
            if (env.lookup("*print-escape*")) |esc_idx| {
                if (idx == esc_idx) {
                    io.setPrintEscape(val);
                    return;
                }
            }
            if (env.lookup("*print-case*")) |case_idx| {
                if (idx == case_idx) {
                    try io.setPrintCase(&self.builtins, val);
                    return;
                }
            }
            if (env.lookup("*print-readably*")) |read_idx| {
                if (idx == read_idx) {
                    io.setPrintReadably(val);
                    return;
                }
            }
            if (env.lookup("*print-base*")) |base_idx| {
                if (idx == base_idx) {
                    try io.setPrintBase(val);
                    return;
                }
            }
            if (env.lookup("*print-radix*")) |radix_idx| {
                if (idx == radix_idx) {
                    io.setPrintRadix(val);
                    return;
                }
            }
            if (env.lookup("*print-gensym*")) |gensym_idx| {
                if (idx == gensym_idx) {
                    io.setPrintGensym(val);
                    return;
                }
            }
            if (env.lookup("*print-array*")) |array_idx| {
                if (idx == array_idx) {
                    io.setPrintArray(val);
                    return;
                }
            }
            if (env.lookup("*print-length*")) |len_idx| {
                if (idx == len_idx) {
                    if (val.isFixnum()) {
                        const len: usize = @intCast(val.toFixnum());
                        io.print_length = len;
                    } else {
                        io.print_length = null;
                    }
                    return;
                }
            }
            if (env.lookup("*print-level*")) |lvl_idx| {
                if (idx == lvl_idx) {
                    if (val.isFixnum()) {
                        const lvl: usize = @intCast(val.toFixnum());
                        io.print_level = lvl;
                    } else {
                        io.print_level = null;
                    }
                    return;
                }
            }
        }
    }

    pub fn loadGlobal(self: *Vm, idx: u16) Error!Value {
        if (idx >= MAX_GLOBALS) return error.InvalidConstant;
        return try self.handleSpecialVarLoad(idx);
    }

    pub fn storeGlobal(self: *Vm, idx: u16, val: Value) Error!void {
        if (idx >= MAX_GLOBALS) return error.InvalidConstant;
        self.globals[idx] = val;
        if (idx >= self.num_globals) {
            self.num_globals = idx + 1;
        }
        try self.handleSpecialVarStore(idx, val);
    }

    pub fn collectGarbage(self: *Vm) !usize {
        // Gather roots from VM state
        var roots = std.ArrayList(Value){};
        defer roots.deinit(self.allocator);

        // Stack values
        try roots.appendSlice(self.allocator, self.stack[0..self.sp]);

        // Global values
        try roots.appendSlice(self.allocator, self.globals[0..self.num_globals]);

        // Catch frame return values
        for (self.catch_stack[0..self.catch_sp]) |frame| {
            try roots.append(self.allocator, frame.tag);
        }

        // Frame closures - must trace closures in call frames
        for (self.frames[0..self.fp]) |frame| {
            if (frame.closure) |c| {
                try roots.append(self.allocator, Value.makeClosure(c));
            }
        }

        // current_closure for callClosure's fp=0 case
        if (self.current_closure) |c| {
            try roots.append(self.allocator, Value.makeClosure(c));
        }

        // Pending throw values
        try roots.append(self.allocator, self.pending_throw_tag);
        try roots.append(self.allocator, self.pending_throw_value);

        // Pending block value
        try roots.append(self.allocator, self.pending_block_value);

        // Current package
        try roots.append(self.allocator, self.current_package);

        // Restart frames
        for (self.restart_stack[0..self.restart_sp]) |frame| {
            try roots.append(self.allocator, frame.name);
        }

        // Handler frames
        for (self.handler_stack[0..self.handler_sp]) |frame| {
            try roots.append(self.allocator, frame.condition_type);
            try roots.append(self.allocator, frame.handler_fn);
        }

        // Secondary values
        try roots.appendSlice(self.allocator, self.secondary_values[0..self.secondary_values_count]);

        // Current chunk constants
        const current_chunk_start = roots.items.len;
        for (self.chunk.getConstants()) |c| {
            try roots.append(self.allocator, c);
        }

        // Active frame chunks (from closures)
        var frame_chunk_starts = std.ArrayList(usize){};
        defer frame_chunk_starts.deinit(self.allocator);
        for (self.frames[0..self.fp]) |frame| {
            if (frame.closure) |closure| {
                const chunk: *const Chunk = closure.code.toPtr(Chunk);
                try frame_chunk_starts.append(self.allocator, roots.items.len);
                for (chunk.getConstants()) |c| {
                    try roots.append(self.allocator, c);
                }
            }
        }

        // Chunk constant pools - track start index for each chunk
        var chunk_const_starts = std.ArrayList(usize){};
        defer chunk_const_starts.deinit(self.allocator);
        for (self.chunk_pool) |chunk| {
            try chunk_const_starts.append(self.allocator, roots.items.len);
            for (chunk.getConstants()) |c| {
                try roots.append(self.allocator, c);
            }
        }

        // Run GC
        const reclaimed = self.heap.collectGarbage(roots.items);

        // Update VM state with new locations
        var idx: usize = 0;

        // Update stack
        for (self.stack[0..self.sp]) |*v| {
            v.* = roots.items[idx];
            idx += 1;
        }

        // Update globals
        for (self.globals[0..self.num_globals]) |*v| {
            v.* = roots.items[idx];
            idx += 1;
        }

        // Update catch frame tags
        for (self.catch_stack[0..self.catch_sp]) |*frame| {
            frame.tag = roots.items[idx];
            idx += 1;
        }

        // Update frame closures with relocated addresses
        for (self.frames[0..self.fp]) |*frame| {
            if (frame.closure != null) {
                const relocated = roots.items[idx];
                frame.closure = relocated.toPtr(runtime.Closure);
                idx += 1;
            }
        }

        // Update current_closure
        if (self.current_closure != null) {
            const relocated = roots.items[idx];
            self.current_closure = relocated.toPtr(runtime.Closure);
            idx += 1;
        }

        // Update pending throw values
        self.pending_throw_tag = roots.items[idx];
        idx += 1;
        self.pending_throw_value = roots.items[idx];
        idx += 1;

        // Update pending block value
        self.pending_block_value = roots.items[idx];
        idx += 1;

        // Update current package
        self.current_package = roots.items[idx];
        idx += 1;

        // Update restart frames
        for (self.restart_stack[0..self.restart_sp]) |*frame| {
            frame.name = roots.items[idx];
            idx += 1;
        }

        // Update handler frames
        for (self.handler_stack[0..self.handler_sp]) |*frame| {
            frame.condition_type = roots.items[idx];
            idx += 1;
            frame.handler_fn = roots.items[idx];
            idx += 1;
        }

        // Update secondary values
        for (self.secondary_values[0..self.secondary_values_count]) |*v| {
            v.* = roots.items[idx];
            idx += 1;
        }

        // Update current chunk constants
        for (self.chunk.getConstants(), 0..) |*c, i| {
            c.* = roots.items[current_chunk_start + i];
        }
        idx = current_chunk_start + self.chunk.getConstants().len;

        // Update active frame chunk constants
        var frame_idx: usize = 0;
        for (self.frames[0..self.fp]) |frame| {
            if (frame.closure) |closure| {
                const chunk: *const Chunk = closure.code.toPtr(Chunk);
                const start = frame_chunk_starts.items[frame_idx];
                for (chunk.getConstants(), 0..) |*c, i| {
                    c.* = roots.items[start + i];
                }
                frame_idx += 1;
            }
        }
        if (frame_chunk_starts.items.len > 0) {
            idx = frame_chunk_starts.items[frame_chunk_starts.items.len - 1];
            if (self.frames[self.fp - 1].closure) |last_closure| {
                const last_chunk: *const Chunk = last_closure.code.toPtr(Chunk);
                idx += last_chunk.getConstants().len;
            }
        }

        // Update chunk constant pools with relocated values
        for (self.chunk_pool, 0..) |chunk, chunk_idx| {
            const start = chunk_const_starts.items[chunk_idx];
            for (chunk.getConstants(), 0..) |_, const_idx| {
                chunk.getConstants()[const_idx] = roots.items[start + const_idx];
            }
        }

        return reclaimed;
    }

    /// Call a closure with arguments already on stack
    /// Expects args to be pushed already at positions [0..argc)
    pub fn callClosure(self: *Vm, closure: *const runtime.Closure, argc: u8) anyerror!Value {
        // Save state - will be restored on both success and error
        const saved_state = State.save(self);
        defer saved_state.restore(self);

        // Set up to execute the closure's chunk directly (like vm.run)
        const closure_chunk: *const Chunk = closure.code.toPtr(Chunk);
        self.chunk = closure_chunk;
        self.ip = 0;
        self.fp = 0; // No frame - ret at fp=0 returns immediately
        self.scope_sp = 0; // Reset let scope stack

        // Args are already on stack as locals (at positions 0..argc)
        // Reset sp to argc (in case it was different)
        self.sp = argc;
        // If closure needs more locals, push nil for them
        while (self.sp < closure_chunk.num_locals) {
            try self.push(Value.nil);
        }

        // Store closure and argc for load_capture/load_argc when fp=0
        self.current_closure = closure;
        self.current_argc = argc;

        // Execute until return
        return try self.execute();
    }

    /// Run a chunk to completion
    pub fn run(self: *Vm, chunk: *const Chunk) Error!Value {
        self.chunk = chunk;
        self.ip = 0;
        self.sp = 0;
        self.fp = 0;
        self.scope_sp = 0;

        // Reserve space for locals
        var i: usize = 0;
        while (i < chunk.num_locals) : (i += 1) {
            try self.push(Value.nil);
        }

        return self.execute();
    }

    fn execute(self: *Vm) Error!Value {
        while (true) {
            // Bounds check before reading opcode to prevent read past end of chunk
            if (self.ip >= self.chunk.getCode().len) return error.InvalidOpcode;
            const op = self.readOp();

            // Execute opcode with error handling
            if (self.executeOp(op)) |_| {} else |err| {
                if (err == error.Halt) {
                    // Program terminated - return result from stack
                    std.debug.assert(self.sp > 0);
                    return try self.pop();
                }
                return self.doError(err);
            }
        }
    }

    /// Execute a single opcode
    fn executeOp(self: *Vm, op: Op) anyerror!void {
        switch (op) {
            // Stack manipulation
            .push_nil => try self.push(Value.nil),
            .push_t => try self.push(Value.t),
            .push_i32 => {
                const n = self.readI32();
                try self.push(Value.makeFixnum(n));
            },
            .push_const => {
                const idx = self.readU16();
                if (idx >= self.chunk.getConstants().len) return error.InvalidConstant;
                try self.push(self.chunk.getConstants()[idx]);
            },
            .dup => {
                const val = try self.peek(0);
                try self.push(val);
            },
            .pop => _ = try self.pop(),
            .swap => {
                const a = try self.pop();
                const b = try self.pop();
                try self.push(a);
                try self.push(b);
            },

            // Variable access - always use frame's bp (not scope's bp)
            // Indices are frame-relative, assigned by compiler
            .load_local => {
                const idx = self.readU8();
                const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
                const stack_idx = bp + idx;
                if (stack_idx >= STACK_SIZE or stack_idx >= self.sp) return error.InvalidOpcode;
                const val = self.stack[stack_idx];
                try self.push(val);
            },
            .store_local => {
                const idx = self.readU8();
                const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
                const stack_idx = bp + idx;
                if (stack_idx >= STACK_SIZE or stack_idx >= self.sp) return error.InvalidOpcode;
                self.stack[stack_idx] = try self.pop();
            },
            .enter_scope => {
                const num_locals = self.readU8();
                if (self.scope_sp >= MAX_SCOPES) return error.StackOverflow;
                // Record current sp as base for this scope
                self.scope_stack[self.scope_sp] = .{
                    .bp = self.sp,
                    .num_locals = num_locals,
                };
                self.scope_sp += 1;
                // Reserve slots by pushing nil placeholders
                for (0..num_locals) |_| {
                    try self.push(Value.nil);
                }
            },
            .exit_scope => {
                const num_locals = self.readU8();
                if (self.scope_sp == 0) return error.InvalidOpcode;
                self.scope_sp -= 1;
                // Result is on top of stack, locals are below
                // Stack: [locals...] result
                // We need: result
                if (num_locals > 0) {
                    const result = try self.pop();
                    // Pop the locals
                    for (0..num_locals) |_| {
                        _ = try self.pop();
                    }
                    try self.push(result);
                }
            },
            .load_capture => {
                const idx = self.readU8();
                // Get current closure from frame, or from current_closure if fp=0
                const closure = if (self.fp > 0)
                    self.frames[self.fp - 1].closure
                else
                    self.current_closure;

                if (closure) |c| {
                    if (idx < c.num_captures) {
                        try self.push(c.getCapture(idx));
                    } else {
                        return error.InvalidConstant;
                    }
                } else {
                    return error.InvalidConstant;
                }
            },
            .load_upvalue => {
                _ = self.readU8(); // depth (unused with flat closures)
                const index = self.readU8();
                // Get current closure from frame, or from current_closure if fp=0
                const closure = if (self.fp > 0)
                    self.frames[self.fp - 1].closure
                else
                    self.current_closure;

                if (closure) |c| {
                    if (index < c.num_captures) {
                        try self.push(c.getCapture(index));
                    } else {
                        return error.InvalidConstant;
                    }
                } else {
                    return error.TypeMismatch; // No closure
                }
            },
            .store_upvalue => {
                _ = self.readU8(); // depth (unused with flat closures)
                const index = self.readU8();
                const val = try self.pop();
                // Get current closure from frame, or from current_closure if fp=0
                const closure = if (self.fp > 0)
                    self.frames[self.fp - 1].closure
                else
                    self.current_closure;

                if (closure) |c| {
                    if (index < c.num_captures) {
                        // Note: captures array is mutable
                        const captures: [*]Value = @constCast(c.captures);
                        captures[index] = val;
                    } else {
                        return error.InvalidConstant;
                    }
                } else {
                    return error.TypeMismatch;
                }
            },
            .load_global => {
                const idx = self.readU16();
                const val = try self.loadGlobal(idx);
                try self.push(val);
            },
            .store_global => {
                const idx = self.readU16();
                const val = try self.pop();
                try self.storeGlobal(idx, val);
            },
            .load_argc => {
                // Get argc from current frame, or from current_argc if fp=0 (callClosure)
                const frame_argc = if (self.fp > 0) self.frames[self.fp - 1].argc else self.current_argc;
                try self.push(Value.makeFixnum(frame_argc));
            },
            .find_key => {
                // Get keyword to search for from constant pool
                const kw_idx = self.readU16();
                if (kw_idx >= self.chunk.getConstants().len) return error.InvalidConstant;
                const keyword = self.chunk.getConstants()[kw_idx];

                // Get current frame info
                const frame = if (self.fp > 0) &self.frames[self.fp - 1] else null;
                if (frame) |f| {
                    const chunk: *const Chunk = f.closure.?.code.toPtr(Chunk);
                    // Layout: [positional] [key params] [keyword pairs]
                    // Keyword pairs start after positional + key param slots
                    const max_positional = chunk.arity + chunk.opt_count;
                    const kw_pair_start: usize = max_positional + chunk.key_count;
                    const frame_argc = f.argc;
                    const positional_count = @min(frame_argc, max_positional);
                    const kw_pair_count = frame_argc - positional_count;

                    // Scan keyword-value pairs
                    var found = false;
                    var found_value = Value.nil;
                    var idx: usize = 0;
                    while (idx + 1 < kw_pair_count) : (idx += 2) {
                        const stack_idx = f.bp + kw_pair_start + idx;
                        // Bounds check before accessing stack
                        if (stack_idx + 1 >= self.sp) break;
                        const kw = self.stack[stack_idx];
                        if (kw.raw == keyword.raw) {
                            found = true;
                            found_value = self.stack[stack_idx + 1];
                            break;
                        }
                    }

                    // Push (found_flag, value)
                    try self.push(if (found) Value.t else Value.nil);
                    try self.push(found_value);
                } else {
                    // No frame - push (nil, nil)
                    try self.push(Value.nil);
                    try self.push(Value.nil);
                }
            },

            // Arithmetic (with float contagion)
            .add => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.add(self.heap, a, b));
            },
            .sub => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.sub(self.heap, a, b));
            },
            .mul => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.mul(self.heap, a, b));
            },
            .div => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(try primitives.arith.div(self.heap, a, b));
            },
            .mod => try self.binaryOp(binaryMod),
            .quot => try self.binaryOp(binaryQuot),
            .rem => try self.binaryOp(binaryRem),
            .neg => {
                const a = try self.pop();
                if (!a.isFixnum()) return error.TypeMismatch;
                const n = a.toFixnum();
                // -minInt(i64) overflows
                if (n == std.math.minInt(i64)) return error.TypeMismatch;
                try self.push(Value.makeFixnum(-n));
            },

            // Comparison
            .eq => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (a.eq(b)) Value.t else Value.nil);
            },
            .lt => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.lt(a, b)) Value.t else Value.nil);
            },
            .gt => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.gt(a, b)) Value.t else Value.nil);
            },
            .le => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.le(a, b)) Value.t else Value.nil);
            },
            .ge => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (try primitives.arith.ge(a, b)) Value.t else Value.nil);
            },
            .num_eq => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (primitives.arith.numEq(a, b)) Value.t else Value.nil);
            },
            .not => {
                const a = try self.pop();
                try self.push(if (a.isNil()) Value.t else Value.nil);
            },

            // List operations
            .cons => {
                const cdr = try self.pop();
                const car = try self.pop();
                const cell = try self.allocCons(car, cdr);
                try self.push(cell);
            },
            .car => {
                const pair = try self.pop();
                switch (pair.typeKind()) {
                    .nil => try self.push(Value.nil), // CL: (car nil) => nil
                    .cons => try self.push(pair.toPtr(Cons).car),
                    else => return error.TypeMismatch,
                }
            },
            .cdr => {
                const pair = try self.pop();
                switch (pair.typeKind()) {
                    .nil => try self.push(Value.nil), // CL: (cdr nil) => nil
                    .cons => try self.push(pair.toPtr(Cons).cdr),
                    else => return error.TypeMismatch,
                }
            },
            .make_list => {
                const count = self.readU8();
                // Bounds check: need at least count items on stack
                if (count > self.sp) return error.StackUnderflow;
                // Build list by popping elements from top of stack (reverse order)
                // This avoids the double-reverse pattern
                var list = Value.nil;
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const elem = self.stack[self.sp - 1 - i];
                    list = try self.allocCons(elem, list);
                }
                self.sp -= count;
                try self.push(list);
            },
            .append_lists => {
                const list2 = try self.pop();
                const list1 = try self.pop();
                // Append list1 to list2: (append '(a b) '(c d)) -> (a b c d)
                switch (list1.typeKind()) {
                    .nil => try self.push(list2),
                    .cons => {
                        // Single-pass copy: build copy of list1, link tail to list2
                        var head: ?Value = null;
                        var tail: ?*Cons = null;
                        var curr = list1;
                        while (curr.isCons()) {
                            const c = curr.toPtr(Cons);
                            const new_cell = try self.allocCons(c.car, Value.nil);
                            if (tail) |t| {
                                t.cdr = new_cell;
                            } else {
                                head = new_cell;
                            }
                            tail = new_cell.toPtr(Cons);
                            curr = c.cdr;
                        }
                        // Link tail to list2
                        if (tail) |t| {
                            t.cdr = list2;
                        }
                        try self.push(head orelse list2);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .list_length => {
                const seq = try self.pop();
                switch (seq.typeKind()) {
                    .nil => try self.push(Value.makeFixnum(0)),
                    .cons => {
                        var len: i64 = 0;
                        var curr = seq;
                        while (curr.isCons()) {
                            len += 1;
                            curr = curr.toPtr(Cons).cdr;
                        }
                        try self.push(Value.makeFixnum(len));
                    },
                    .vector => {
                        const vec = seq.toPtr(runtime.Vector);
                        try self.push(Value.makeFixnum(@intCast(vec.length)));
                    },
                    .string => {
                        const str = seq.toPtr(runtime.String);
                        try self.push(Value.makeFixnum(@intCast(str.length)));
                    },
                    else => return error.TypeMismatch,
                }
            },

            .list_reverse => {
                const list = try self.pop();
                var reversed = Value.nil;
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    reversed = try self.allocCons(c.car, reversed);
                    curr = c.cdr;
                }
                try self.push(reversed);
            },

            .list_nth => {
                const list = try self.pop();
                const n_val = try self.pop();
                if (!n_val.isFixnum()) return error.TypeMismatch;
                const n = n_val.toFixnum();
                if (n < 0) return error.TypeMismatch;
                var idx: i64 = 0;
                var curr = list;
                while (curr.isCons()) {
                    if (idx == n) {
                        try self.push(curr.toPtr(Cons).car);
                        break;
                    }
                    idx += 1;
                    curr = curr.toPtr(Cons).cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_nthcdr => {
                const list = try self.pop();
                const n_val = try self.pop();
                if (!n_val.isFixnum()) return error.TypeMismatch;
                const n = n_val.toFixnum();
                if (n < 0) return error.TypeMismatch;
                var idx: i64 = 0;
                var curr = list;
                while (idx < n and curr.isCons()) {
                    idx += 1;
                    curr = curr.toPtr(Cons).cdr;
                }
                try self.push(curr);
            },

            .rplaca => {
                const new_car = try self.pop();
                const cons_val = try self.pop();
                if (!cons_val.isCons()) return error.TypeMismatch;
                const c = cons_val.toPtr(Cons);
                c.car = new_car;
                try self.push(new_car);
            },

            .rplacd => {
                const new_cdr = try self.pop();
                const cons_val = try self.pop();
                if (!cons_val.isCons()) return error.TypeMismatch;
                const c = cons_val.toPtr(Cons);
                c.cdr = new_cdr;
                try self.push(new_cdr);
            },

            .error_user => {
                const msg_val = try self.pop();
                // Accept any value (not just strings) for flexibility
                _ = msg_val;
                return error.UserError;
            },

            .list_last => {
                const list = try self.pop();
                switch (list.typeKind()) {
                    .nil => try self.push(Value.nil),
                    .cons => {
                        var curr = list;
                        while (curr.isCons()) {
                            const c = curr.toPtr(Cons);
                            if (!c.cdr.isCons()) {
                                try self.push(curr);
                                break;
                            }
                            curr = c.cdr;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },

            .list_member => {
                const list = try self.pop();
                const item = try self.pop();
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.raw == item.raw) {
                        try self.push(curr);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_member_eql => {
                // member with eql test (compares numbers by value)
                const list = try self.pop();
                const item = try self.pop();
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .eql)) {
                        try self.push(curr);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_member_equal => {
                // member with equal test (deep equality)
                const list = try self.pop();
                const item = try self.pop();
                var curr = list;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .equal)) {
                        try self.push(curr);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            // Type predicates
            .consp => {
                const a = try self.pop();
                try self.push(if (a.isCons()) Value.t else Value.nil);
            },
            .symbolp => {
                const a = try self.pop();
                // nil and t are also symbols in CL
                try self.push(if (a.isSymbolLike()) Value.t else Value.nil);
            },
            .numberp => {
                const a = try self.pop();
                try self.push(if (a.isNumber()) Value.t else Value.nil);
            },
            .stringp => {
                const a = try self.pop();
                try self.push(if (a.isString()) Value.t else Value.nil);
            },
            .vectorp => {
                const a = try self.pop();
                try self.push(if (a.isVector()) Value.t else Value.nil);
            },
            .closurep => {
                const a = try self.pop();
                try self.push(if (a.isClosure()) Value.t else Value.nil);
            },
            .keywordp => {
                const a = try self.pop();
                try self.push(if (a.isKeyword()) Value.t else Value.nil);
            },
            .method_qualifiers => {
                const method_val = try self.pop();
                const args = try self.heap.allocCons(method_val, Value.nil);
                const result = try primitives.methodQualifiers(self.heap, args);
                try self.push(result);
            },
            .method_specializers => {
                const method_val = try self.pop();
                const args = try self.heap.allocCons(method_val, Value.nil);
                const result = try primitives.methodSpecializers(self.heap, args);
                try self.push(result);
            },
            .method_function => {
                const method_val = try self.pop();
                const args = try self.heap.allocCons(method_val, Value.nil);
                const result = try primitives.methodFunction(self.heap, args);
                try self.push(result);
            },
            .generic_function_methods => {
                const gf_val = try self.pop();
                const args = try self.heap.allocCons(gf_val, Value.nil);
                const result = try primitives.genericFunctionMethods(self.heap, args);
                try self.push(result);
            },
            .generic_function_lambda_list => {
                const gf_val = try self.pop();
                const args = try self.heap.allocCons(gf_val, Value.nil);
                const result = try primitives.genericFunctionLambdaList(self.heap, args);
                try self.push(result);
            },
            .generic_function_name => {
                const gf_val = try self.pop();
                const args = try self.heap.allocCons(gf_val, Value.nil);
                const result = try primitives.genericFunctionName(self.heap, args);
                try self.push(result);
            },
            .nilp => {
                const a = try self.pop();
                try self.push(if (a.isNil()) Value.t else Value.nil);
            },

            // Vector operations
            .make_vec => {
                _ = self.readU16(); // Size operand (unused, size from stack)
                const init_val = try self.pop();
                const size_val = try self.pop();
                if (!size_val.isFixnum()) return error.TypeMismatch;
                const size_signed = size_val.toFixnum();
                if (size_signed < 0) return error.TypeMismatch;
                const size: usize = @intCast(size_signed);
                const vec = try self.allocVector(size, size);
                // Fill with init value (nil or specified)
                const vec_obj = vec.toPtr(Vector);
                for (0..size) |i| {
                    vec_obj.data[i] = init_val;
                }
                try self.push(vec);
            },
            .make_vec_n => {
                const count = self.readU8();
                const vec = try self.allocVector(count, count);
                const vec_obj = vec.toPtr(Vector);
                // Pop elements in reverse order (last element pushed first)
                var i: usize = count;
                while (i > 0) {
                    i -= 1;
                    vec_obj.data[i] = try self.pop();
                }
                try self.push(vec);
            },
            .vec_ref => {
                const idx_val = try self.pop();
                const vec_val = try self.pop();
                if (!vec_val.isVector() or !idx_val.isFixnum()) return error.TypeMismatch;
                const vec = vec_val.toPtr(runtime.Vector);
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                if (idx >= vec.length) return error.TypeMismatch;
                try self.push(vec.get(idx));
            },
            .vec_set => {
                const val = try self.pop();
                const idx_val = try self.pop();
                const vec_val = try self.pop();
                if (!vec_val.isVector() or !idx_val.isFixnum()) return error.TypeMismatch;
                const vec = vec_val.toPtr(runtime.Vector);
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                if (idx >= vec.length) return error.TypeMismatch;
                vec.set(idx, val);
                try self.push(val); // Return the value that was set
            },
            .elt_set => {
                // Polymorphic: set element in vector or list
                const val = try self.pop();
                const idx_val = try self.pop();
                const seq_val = try self.pop();
                if (!idx_val.isFixnum()) return error.TypeMismatch;
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);

                switch (seq_val.typeKind()) {
                    .vector => {
                        const vec = seq_val.toPtr(runtime.Vector);
                        if (idx >= vec.length) return error.TypeMismatch;
                        vec.set(idx, val);
                        try self.push(val);
                    },
                    .cons, .nil => {
                        // For lists, use nthcdr then rplaca
                        var list = seq_val;
                        var i: usize = 0;
                        while (i < idx) : (i += 1) {
                            if (!list.isCons()) return error.TypeMismatch;
                            list = list.toPtr(runtime.Cons).cdr;
                        }
                        if (!list.isCons()) return error.TypeMismatch;
                        list.toPtr(runtime.Cons).car = val;
                        try self.push(val);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .vec_len => {
                const vec_val = try self.pop();
                if (!vec_val.isVector()) return error.TypeMismatch;
                const vec = vec_val.toPtr(runtime.Vector);
                try self.push(Value.makeFixnum(@intCast(vec.length)));
            },

            .vec_fill_ptr => {
                const vec_val = try self.pop();
                if (!vec_val.isVector()) return error.TypeMismatch;
                const fp = primitives.vector.fillPointer(vec_val);
                if (fp) |p| {
                    try self.push(Value.makeFixnum(p));
                } else {
                    try self.push(Value.nil);
                }
            },

            .vec_push => {
                const elem = try self.pop();
                const vec_val = try self.pop();
                const result = primitives.vector.vectorPush(vec_val, elem);
                try self.push(Value.makeFixnum(result));
            },

            .vec_push_ext => {
                const ext = try self.pop();
                const elem = try self.pop();
                const vec_val = try self.pop();
                if (!ext.isFixnum()) return error.TypeMismatch;
                const result = try primitives.vector.vectorPushExtend(self.heap, vec_val, elem, @intCast(ext.toFixnum()));
                try self.push(Value.makeFixnum(result));
            },

            .vec_pop => {
                const vec_val = try self.pop();
                const result = primitives.vector.vectorPop(vec_val);
                try self.push(result);
            },

            .vec_set_fill_ptr => {
                const fp_val = try self.pop();
                const vec_val = try self.pop();
                if (!fp_val.isFixnum()) return error.TypeMismatch;
                const ok = primitives.vector.setFillPointer(vec_val, fp_val.toFixnum());
                try self.push(if (ok) Value.t else Value.nil);
            },

            .vec_set_adjustable => {
                const bool_val = try self.pop();
                const vec_val = try self.pop();
                const ok = primitives.vector.setAdjustable(vec_val, !bool_val.isNil());
                try self.push(if (ok) Value.t else Value.nil);
            },

            .vec_adjust => {
                const fill_val = try self.pop();
                const new_size_val = try self.pop();
                const vec_val = try self.pop();
                if (!new_size_val.isFixnum()) return error.TypeMismatch;
                const new_size: u64 = @intCast(new_size_val.toFixnum());
                const result = try primitives.vector.adjustArray(self.heap, vec_val, new_size, fill_val);
                try self.push(result);
            },

            // CLOS operations
            .slot_value => {
                const slot_name_val = try self.pop();
                const obj = try self.pop();
                const args = try self.heap.allocCons(obj, try self.heap.allocCons(slot_name_val, Value.nil));
                const result = try primitives.slotValue(self.heap, args);
                try self.push(result);
            },

            .set_slot_value => {
                const value = try self.pop();
                const slot_name_val = try self.pop();
                const obj = try self.pop();
                const args = try self.heap.allocCons(obj, try self.heap.allocCons(slot_name_val, try self.heap.allocCons(value, Value.nil)));
                const result = try primitives.clos.setSlotValue(self.heap, args);
                try self.push(result);
            },
            .class_of => {
                const obj = try self.pop();
                const args = try self.heap.allocCons(obj, Value.nil);
                const result = try primitives.classOf(self.heap, args);
                try self.push(result);
            },
            .find_class => {
                const name = try self.pop();
                const args = try self.heap.allocCons(name, Value.nil);
                const result = try primitives.findClass(self.heap, args);
                try self.push(result);
            },
            .class_name => {
                const class_val = try self.pop();
                const args = try self.heap.allocCons(class_val, Value.nil);
                const result = try primitives.className(self.heap, args);
                try self.push(result);
            },
            .class_direct_superclasses => {
                const class_val = try self.pop();
                const args = try self.heap.allocCons(class_val, Value.nil);
                const result = try primitives.classDirectSuperclasses(self.heap, args);
                try self.push(result);
            },
            .class_precedence_list => {
                const class_val = try self.pop();
                const args = try self.heap.allocCons(class_val, Value.nil);
                const result = try primitives.classPrecedenceList(self.heap, args);
                try self.push(result);
            },
            .class_direct_slots => {
                const class_val = try self.pop();
                const args = try self.heap.allocCons(class_val, Value.nil);
                const result = try primitives.classDirectSlots(self.heap, args);
                try self.push(result);
            },
            .class_slots => {
                const class_val = try self.pop();
                const args = try self.heap.allocCons(class_val, Value.nil);
                const result = try primitives.classSlots(self.heap, args);
                try self.push(result);
            },
            .slot_definition_name => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionName(self.heap, args);
                try self.push(result);
            },
            .slot_definition_initform => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionInitform(self.heap, args);
                try self.push(result);
            },
            .slot_definition_initargs => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionInitargs(self.heap, args);
                try self.push(result);
            },
            .slot_definition_readers => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionReaders(self.heap, args);
                try self.push(result);
            },
            .slot_definition_writers => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionWriters(self.heap, args);
                try self.push(result);
            },
            .slot_definition_allocation => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionAllocation(self.heap, args);
                try self.push(result);
            },
            .slot_definition_type => {
                const slot_def = try self.pop();
                const args = try self.heap.allocCons(slot_def, Value.nil);
                const result = try primitives.slotDefinitionType(self.heap, args);
                try self.push(result);
            },
            .make_generic_function => {
                const lambda_list = try self.pop();
                const name = try self.pop();
                const args = try self.heap.allocCons(name, try self.heap.allocCons(lambda_list, Value.nil));
                const result = try primitives.makeGenericFunction(self.heap, args);
                try self.push(result);
            },
            .make_method => {
                const function = try self.pop();
                const lambda_list = try self.pop();
                const specializers = try self.pop();
                const qualifiers = try self.pop();
                const args = try self.heap.allocCons(qualifiers, try self.heap.allocCons(specializers, try self.heap.allocCons(lambda_list, try self.heap.allocCons(function, Value.nil))));
                const result = try primitives.makeMethod(self.heap, args);
                try self.push(result);
            },
            .set_gf_dispatcher => {
                const dispatcher = try self.pop();
                const gf_val = try self.pop();
                if (!gf_val.isGenericFunction()) return error.TypeMismatch;
                const gf = gf_val.toPtr(runtime.objects.GenericFunction);
                gf.dispatcher = dispatcher;
                try self.push(gf_val);
            },
            .add_method => {
                const method_val = try self.pop();
                const gf_val = try self.pop();
                const args = try self.heap.allocCons(gf_val, try self.heap.allocCons(method_val, Value.nil));
                const result = try primitives.addMethod(self.heap, args);
                try self.push(result);
            },
            .make_unbound => {
                try self.push(Value.unbound);
            },
            .slot_boundp => {
                const slot_name_val = try self.pop();
                const obj = try self.pop();
                const args = try self.heap.allocCons(obj, try self.heap.allocCons(slot_name_val, Value.nil));
                const result = try primitives.slotBoundp(self.heap, args);
                try self.push(result);
            },
            .slot_makunbound => {
                const slot_name_val = try self.pop();
                const obj = try self.pop();
                const args = try self.heap.allocCons(obj, try self.heap.allocCons(slot_name_val, Value.nil));
                const result = try primitives.slotMakunbound(self.heap, args);
                try self.push(result);
            },

            // Box operations (mutable cells for closures)
            .make_box => {
                const val = try self.pop();
                // Allocate a 1-element vector as a box
                const box = try self.allocVector(1, 1);
                const vec = box.toPtr(runtime.Vector);
                vec.set(0, val);
                try self.push(box);
            },
            .box_ref => {
                const box = try self.pop();
                if (!box.isVector()) return error.TypeMismatch;
                const vec = box.toPtr(runtime.Vector);
                if (vec.length < 1) return error.TypeMismatch;
                try self.push(vec.get(0));
            },
            .box_set => {
                const val = try self.pop();
                const box = try self.pop();
                if (!box.isVector()) return error.TypeMismatch;
                const vec = box.toPtr(runtime.Vector);
                if (vec.length < 1) return error.TypeMismatch;
                vec.set(0, val);
                try self.push(val); // Return the value written
            },

            // String operations
            .str_ref => {
                const idx_val = try self.pop();
                const str_val = try self.pop();
                if (!str_val.isString() or !idx_val.isFixnum()) return error.TypeMismatch;
                const str = str_val.toPtr(runtime.String);
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                if (idx >= str.length) return error.TypeMismatch;
                try self.push(Value.makeFixnum(str.bytes()[idx]));
            },
            .str_len => {
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;
                const str = str_val.toPtr(runtime.String);
                try self.push(Value.makeFixnum(@intCast(str.length)));
            },
            .str_set => {
                const char_val = try self.pop();
                const idx_val = try self.pop();
                const str_val = try self.pop();
                if (!str_val.isString() or !idx_val.isFixnum()) return error.TypeMismatch;
                const str = str_val.toPtr(runtime.String);
                const idx_signed = idx_val.toFixnum();
                if (idx_signed < 0) return error.TypeMismatch;
                const idx: usize = @intCast(idx_signed);
                if (idx >= str.length) return error.TypeMismatch;
                const char_int = switch (char_val.typeKind()) {
                    .fixnum => char_val.toFixnum(),
                    .char => @as(i64, @intCast(char_val.toCharacter())),
                    else => return error.TypeMismatch,
                };
                if (char_int < 0 or char_int > 255) return error.TypeMismatch;
                str.mutableBytes()[idx] = @intCast(char_int);
                try self.push(str_val);
            },
            .str_concat => {
                const s2 = try self.pop();
                const s1 = try self.pop();
                if (!s1.isString() or !s2.isString()) return error.TypeMismatch;
                const str1 = s1.toPtr(runtime.String);
                const str2 = s2.toPtr(runtime.String);
                // Allocate new string with combined length
                const new_len = str1.length + str2.length;
                const result = try self.allocStringUninitialized(new_len);
                const result_str = result.toPtr(runtime.String);
                const dest = result_str.mutableBytes();
                @memcpy(dest[0..str1.length], str1.bytes());
                @memcpy(dest[str1.length..new_len], str2.bytes());
                try self.push(result);
            },

            // Control flow
            .jmp => {
                const offset = self.readI16();
                // Use isize to handle the full range of usize safely
                const new_ip = @as(isize, @intCast(self.ip)) + offset;
                if (new_ip < 0) return error.InvalidOpcode;
                self.ip = @intCast(new_ip);
            },
            .jmp_nil => {
                const offset = self.readI16();
                const val = try self.pop();
                if (val.isNil()) {
                    const new_ip = @as(isize, @intCast(self.ip)) + offset;
                    if (new_ip < 0) return error.InvalidOpcode;
                    self.ip = @intCast(new_ip);
                }
            },
            .jmp_not_nil => {
                const offset = self.readI16();
                const val = try self.pop();
                if (!val.isNil()) {
                    const new_ip = @as(isize, @intCast(self.ip)) + offset;
                    if (new_ip < 0) return error.InvalidOpcode;
                    self.ip = @intCast(new_ip);
                }
            },

            // Function calls
            .call => {
                const argc = self.readU8();
                //const start_idx = if (self.sp > argc + 3) self.sp - argc - 3 else 0;
                //for (start_idx..self.sp) |i| {
                //}
                try self.doCall(argc, false);
            },
            .tail_call => {
                const argc = self.readU8();
                try self.doCall(argc, true);
            },
            .apply => {
                try self.doApply();
            },
            .ret => {
                const result = try self.pop();
                if (self.fp == 0) {
                    // Top level return - push result and halt
                    try self.push(result);
                    return error.Halt;
                }
                // Restore caller state
                self.fp -= 1;
                const frame = self.frames[self.fp];
                self.sp = frame.bp;
                self.chunk = frame.chunk;
                self.ip = frame.return_ip;
                try self.push(result);
            },
            .make_closure => {
                const chunk_idx = self.readU16();
                const num_captures = self.readU8();

                // Get the chunk from the pool (offset by base for this eval)
                const abs_idx = self.chunk_base + chunk_idx;
                if (abs_idx >= self.chunk_pool.len) return error.InvalidConstant;
                const closure_chunk = self.chunk_pool[abs_idx];

                // Collect captures from stack
                var captures: [64]Value = undefined;
                if (num_captures > 64) return error.StackOverflow;
                var i: usize = num_captures;
                while (i > 0) {
                    i -= 1;
                    captures[i] = try self.pop();
                }

                // Create closure - wrap chunk pointer in a Value
                const chunk_val = Value.makeChunk(closure_chunk);
                const closure = try self.allocClosureWithGC(
                    chunk_val,
                    closure_chunk.arity,
                    captures[0..num_captures],
                );

                try self.push(closure);
            },

            // I/O
            .write => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.write(val, Value.nil);
                try self.push(result);
            },
            .print => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.print(val, Value.nil);
                try self.push(result);
            },
            .princ => {
                const val = try self.pop();
                try self.syncPrintGlobals();
                const result = try io.princ(val, Value.nil);
                try self.push(result);
            },
            .terpri => {
                try io.sysNewline();
                try self.push(Value.nil);
            },
            .write_char => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                if (cp < 128) {
                    try io.sysWriteChar(@intCast(cp));
                } else {
                    // UTF-8 encode for non-ASCII
                    var buf: [4]u8 = undefined;
                    const len = try std.unicode.utf8Encode(@intCast(cp), &buf);
                    try io.sysWriteBytes(buf[0..len]);
                }
                try self.push(val);
            },
            .char_upcase => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const upper = if (cp >= 'a' and cp <= 'z') cp - 32 else cp;
                try self.push(Value.makeCharacter(upper));
            },
            .char_downcase => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const lower = if (cp >= 'A' and cp <= 'Z') cp + 32 else cp;
                try self.push(Value.makeCharacter(lower));
            },
            .digit_char_p => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const is_digit = cp >= '0' and cp <= '9';
                try self.push(if (is_digit) Value.t else Value.nil);
            },
            .alpha_char_p => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                const is_alpha = (cp >= 'A' and cp <= 'Z') or (cp >= 'a' and cp <= 'z');
                try self.push(if (is_alpha) Value.t else Value.nil);
            },
            .parse_integer => {
                const val = try self.pop();
                if (!val.isString()) return error.TypeMismatch;
                const str = val.toPtr(String);
                const bytes = str.bytes();
                // Skip leading whitespace
                var start: usize = 0;
                while (start < bytes.len and (bytes[start] == ' ' or bytes[start] == '\t' or bytes[start] == '\n' or bytes[start] == '\r')) : (start += 1) {}
                // Parse integer from string
                var result: i64 = 0;
                var negative = false;
                var i: usize = start;
                var overflow = false;
                if (i < bytes.len and bytes[i] == '-') {
                    negative = true;
                    i += 1;
                }
                while (i < bytes.len) : (i += 1) {
                    const c = bytes[i];
                    if (c >= '0' and c <= '9') {
                        // Use checked arithmetic to detect overflow
                        const mul_result = @mulWithOverflow(result, 10);
                        if (mul_result[1] != 0) {
                            overflow = true;
                            break;
                        }
                        const add_result = @addWithOverflow(mul_result[0], c - '0');
                        if (add_result[1] != 0) {
                            overflow = true;
                            break;
                        }
                        result = add_result[0];
                    } else if (c == ' ' or c == '\t' or c == '\n' or c == '\r') {
                        break; // Stop at trailing whitespace
                    } else {
                        break;
                    }
                }
                if (overflow) {
                    try self.push(Value.nil);
                } else {
                    if (negative) result = -result;
                    try self.push(Value.makeFixnum(result));
                }
            },
            .write_to_string => {
                const val = try self.pop();
                // Convert value to string representation
                var buf: [256]u8 = undefined;
                var fbs = std.io.fixedBufferStream(&buf);
                try io.writeValueToBuffer(val, fbs.writer().any());
                const written = fbs.getWritten();
                const result = try self.allocString(written);
                try self.push(result);
            },
            .logand => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
                const result = a.toFixnum() & b.toFixnum();
                try self.push(Value.makeFixnum(result));
            },
            .logior => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
                const result = a.toFixnum() | b.toFixnum();
                try self.push(Value.makeFixnum(result));
            },
            .logxor => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
                const result = a.toFixnum() ^ b.toFixnum();
                try self.push(Value.makeFixnum(result));
            },
            .lognot => {
                const a = try self.pop();
                if (!a.isFixnum()) return error.TypeMismatch;
                const result = ~a.toFixnum();
                try self.push(Value.makeFixnum(result));
            },
            .ash => {
                const count_val = try self.pop();
                const n_val = try self.pop();
                if (!n_val.isFixnum() or !count_val.isFixnum()) return error.TypeMismatch;
                const n = n_val.toFixnum();
                const count = count_val.toFixnum();
                const result = if (count >= 0)
                    n << @intCast(@min(count, 63))
                else if (count == std.math.minInt(i64))
                    // -minInt overflows; shift by 63+ zeros everything
                    if (n >= 0) @as(i64, 0) else @as(i64, -1)
                else
                    n >> @intCast(@min(-count, 63));
                try self.push(Value.makeFixnum(result));
            },
            .lognand => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.lognand(a, b);
                try self.push(result);
            },
            .lognor => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.lognor(a, b);
                try self.push(result);
            },
            .logandc1 => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logandc1(a, b);
                try self.push(result);
            },
            .logandc2 => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logandc2(a, b);
                try self.push(result);
            },
            .logeqv => {
                const b = try self.pop();
                const a = try self.pop();
                const result = try arith.logeqv(a, b);
                try self.push(result);
            },
            .logbitp => {
                const n = try self.pop();
                const index = try self.pop();
                const result = try arith.logbitp(index, n);
                try self.push(if (result) Value.t else Value.nil);
            },
            .logcount => {
                const n = try self.pop();
                const result = try arith.logcount(n);
                try self.push(result);
            },
            .integer_length => {
                const n = try self.pop();
                const result = try arith.integer_length(n);
                try self.push(result);
            },
            .read_file => {
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                const result = try io.readFile(self.heap, path_str.bytes());
                try self.push(result);
            },
            .write_file => {
                const content_val = try self.pop();
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                try io.writeFile(path_str.bytes(), content_val);
                try self.push(Value.nil);
            },
            .delete_file => {
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                try io.deleteFile(path_str.bytes());
                try self.push(Value.nil);
            },
            .rename_file => {
                const new_path_val = try self.pop();
                const old_path_val = try self.pop();
                if (!old_path_val.isString()) return error.TypeMismatch;
                if (!new_path_val.isString()) return error.TypeMismatch;
                const old_path_str = old_path_val.toPtr(String);
                const new_path_str = new_path_val.toPtr(String);
                try io.renameFile(old_path_str.bytes(), new_path_str.bytes());
                try self.push(Value.nil);
            },
            .probe_file => {
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                if (try io.probeFile(path_str.bytes())) {
                    // Return the path as truename
                    try self.push(path_val);
                } else {
                    try self.push(Value.nil);
                }
            },
            .file_write_date => {
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                const timestamp = try io.fileWriteDate(path_str.bytes());
                try self.push(Value.makeFixnum(timestamp));
            },
            .file_author => {
                const path_val = try self.pop();
                // Accept string or pathname
                if (!path_val.isString() and !path_val.isPathname()) return error.TypeMismatch;
                // Unix-like systems don't track file author, return nil
                try self.push(Value.nil);
            },
            .get_universal_time => {
                const timestamp = io.getUniversalTime();
                try self.push(Value.makeFixnum(timestamp));
            },
            .get_internal_real_time => {
                const timestamp = io.getInternalRealTime();
                try self.push(Value.makeFixnum(timestamp));
            },
            .get_internal_run_time => {
                const timestamp = try io.getInternalRunTime();
                try self.push(Value.makeFixnum(timestamp));
            },
            .get_decoded_time => {
                const ut = io.getUniversalTime();
                const dt = io.decodeUniversalTime(ut, null);
                // Return 9 values: second, minute, hour, date, month, year, dow, dst-p, zone
                try self.push(Value.makeFixnum(dt.second));
                self.secondary_values[0] = Value.makeFixnum(dt.minute);
                self.secondary_values[1] = Value.makeFixnum(dt.hour);
                self.secondary_values[2] = Value.makeFixnum(dt.date);
                self.secondary_values[3] = Value.makeFixnum(dt.month);
                self.secondary_values[4] = Value.makeFixnum(dt.year);
                self.secondary_values[5] = Value.makeFixnum(dt.day_of_week);
                self.secondary_values[6] = if (dt.daylight_p) Value.t else Value.nil;
                self.secondary_values[7] = Value.makeFixnum(dt.zone);
                self.secondary_values_count = 8;
            },
            .decode_universal_time => {
                const ut_val = try self.pop();
                if (!ut_val.isFixnum()) return error.TypeMismatch;
                const ut = ut_val.toFixnum();
                const dt = io.decodeUniversalTime(ut, null);
                // Return 9 values: second, minute, hour, date, month, year, dow, dst-p, zone
                try self.push(Value.makeFixnum(dt.second));
                self.secondary_values[0] = Value.makeFixnum(dt.minute);
                self.secondary_values[1] = Value.makeFixnum(dt.hour);
                self.secondary_values[2] = Value.makeFixnum(dt.date);
                self.secondary_values[3] = Value.makeFixnum(dt.month);
                self.secondary_values[4] = Value.makeFixnum(dt.year);
                self.secondary_values[5] = Value.makeFixnum(dt.day_of_week);
                self.secondary_values[6] = if (dt.daylight_p) Value.t else Value.nil;
                self.secondary_values[7] = Value.makeFixnum(dt.zone);
                self.secondary_values_count = 8;
            },
            .encode_universal_time => {
                const argc = self.readU8();
                // Pop args in reverse: year, month, date, hour, minute, second, [zone]
                var zone: ?i64 = null;
                if (argc == 7) {
                    const zone_val = try self.pop();
                    if (zone_val.isFixnum()) {
                        zone = zone_val.toFixnum();
                    }
                }
                const year_val = try self.pop();
                const month_val = try self.pop();
                const date_val = try self.pop();
                const hour_val = try self.pop();
                const minute_val = try self.pop();
                const second_val = try self.pop();
                if (!second_val.isFixnum() or !minute_val.isFixnum() or
                    !hour_val.isFixnum() or !date_val.isFixnum() or
                    !month_val.isFixnum() or !year_val.isFixnum())
                {
                    return error.TypeMismatch;
                }
                const ut = io.encodeUniversalTime(
                    second_val.toFixnum(),
                    minute_val.toFixnum(),
                    hour_val.toFixnum(),
                    date_val.toFixnum(),
                    month_val.toFixnum(),
                    year_val.toFixnum(),
                    zone,
                );
                try self.push(Value.makeFixnum(ut));
            },
            .room => {
                // Print memory statistics
                const stats = self.heap.stats;
                try io.room(stats.allocations, stats.bytes_allocated, stats.gc_count, stats.bytes_copied);
                try self.push(Value.nil);
            },
            .lisp_implementation_type => {
                const str = try self.heap.allocBaseString("Habu");
                try self.push(str);
            },
            .lisp_implementation_version => {
                const str = try self.heap.allocBaseString("0.1.0");
                try self.push(str);
            },
            .software_type => {
                const str = try self.heap.allocBaseString(@tagName(builtin.os.tag));
                try self.push(str);
            },
            .machine_type => {
                const str = try self.heap.allocBaseString(@tagName(builtin.cpu.arch));
                try self.push(str);
            },
            .machine_instance => {
                // Return hostname or nil
                try self.push(Value.nil);
            },
            .machine_version => {
                // Return nil - no specific hardware version info available
                try self.push(Value.nil);
            },
            .software_version => {
                // Return nil - could be expanded to return OS version
                try self.push(Value.nil);
            },
            .short_site_name => {
                // Site names are installation-specific; return nil
                try self.push(Value.nil);
            },
            .long_site_name => {
                // Site names are installation-specific; return nil
                try self.push(Value.nil);
            },
            .user_homedir_pathname => {
                const result = try primitives.pathname.userHomedirPathname(self.allocator, self.heap);
                try self.push(result);
            },
            .make_string => {
                const char_val = try self.pop();
                const len_val = try self.pop();
                if (!len_val.isFixnum()) return error.TypeMismatch;
                const len_signed = len_val.toFixnum();
                if (len_signed < 0) return error.TypeMismatch;
                const len: usize = @intCast(len_signed);
                // If char provided, use it; otherwise use space
                const fill_char: u8 = if (char_val.isCharacter()) blk: {
                    const cp = char_val.toCharacter();
                    if (cp > 255) return error.TypeMismatch;
                    break :blk @intCast(cp);
                } else if (char_val == Value.nil)
                    ' '
                else
                    return error.TypeMismatch;
                const str = try self.allocStringUninitialized(len);
                const str_obj = str.toPtr(String);
                @memset(str_obj.data[0..len], fill_char);
                try self.push(str);
            },
            .math_ext => {
                const sub_op_byte = self.readU8();
                const sub_op: opcodes.MathExtOp = @enumFromInt(sub_op_byte);
                switch (sub_op) {
                    .asin => {
                        const val = try self.pop();
                        const result = try arith.asin_val(val);
                        try self.push(result);
                    },
                    .acos => {
                        const val = try self.pop();
                        const result = try arith.acos_val(val);
                        try self.push(result);
                    },
                    .atan => {
                        const val = try self.pop();
                        const result = try arith.atan_val(val);
                        try self.push(result);
                    },
                    .atan2 => {
                        const x = try self.pop();
                        const y = try self.pop();
                        const result = try arith.atan2_val(y, x);
                        try self.push(result);
                    },
                    .sinh => {
                        const val = try self.pop();
                        const result = try arith.sinh_val(val);
                        try self.push(result);
                    },
                    .cosh => {
                        const val = try self.pop();
                        const result = try arith.cosh_val(val);
                        try self.push(result);
                    },
                    .tanh => {
                        const val = try self.pop();
                        const result = try arith.tanh_val(val);
                        try self.push(result);
                    },
                    .asinh => {
                        const val = try self.pop();
                        const result = try arith.asinh_val(val);
                        try self.push(result);
                    },
                    .acosh => {
                        const val = try self.pop();
                        const result = try arith.acosh_val(val);
                        try self.push(result);
                    },
                    .atanh => {
                        const val = try self.pop();
                        const result = try arith.atanh_val(val);
                        try self.push(result);
                    },
                }
            },
            .list_to_string => {
                const list_val = try self.pop();
                // Count length first
                var len: usize = 0;
                var p = list_val;
                while (p != Value.nil) {
                    if (!p.isCons()) return error.TypeMismatch;
                    const c = p.toPtr(Cons);
                    // Accept either characters or fixnums (char codes)
                    if (!c.car.isCharacter() and !c.car.isFixnum()) return error.TypeMismatch;
                    len += 1;
                    p = c.cdr;
                }
                // Allocate and fill
                const str = try self.allocStringUninitialized(len);
                const str_obj = str.toPtr(String);
                var i: usize = 0;
                p = list_val;
                while (p != Value.nil) {
                    const c = p.toPtr(Cons);
                    const cp: i64 = if (c.car.isCharacter())
                        @intCast(c.car.toCharacter())
                    else
                        c.car.toFixnum();
                    // Only ASCII/Latin-1 characters fit in a byte
                    if (cp < 0 or cp > 255) return error.TypeMismatch;
                    str_obj.data[i] = @intCast(cp);
                    i += 1;
                    p = c.cdr;
                }
                try self.push(str);
            },
            .string_upcase => {
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;
                const src = str_val.toPtr(String);
                const src_bytes = src.bytes();
                const result = try self.allocStringUninitialized(src_bytes.len);
                const dst = result.toPtr(String);
                for (src_bytes, 0..) |c, i| {
                    dst.data[i] = std.ascii.toUpper(c);
                }
                try self.push(result);
            },
            .string_downcase => {
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;
                const src = str_val.toPtr(String);
                const src_bytes = src.bytes();
                const result = try self.allocStringUninitialized(src_bytes.len);
                const dst = result.toPtr(String);
                for (src_bytes, 0..) |c, i| {
                    dst.data[i] = std.ascii.toLower(c);
                }
                try self.push(result);
            },
            .random => {
                const n = try self.pop();
                const result = try arith.random(&self.prng, &self.prng_seeded, n);
                try self.push(result);
            },
            .random_seed => {
                const seed = try self.pop();
                const result = try arith.randomSeed(&self.prng, &self.prng_seeded, seed);
                try self.push(result);
            },
            .intern => {
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;
                const str = str_val.toPtr(String);
                const sym = try self.intern(str.bytes());
                try self.push(sym);
            },
            .substring => {
                const end_val = try self.pop();
                const start_val = try self.pop();
                const str_val = try self.pop();
                if (!end_val.isFixnum() or !start_val.isFixnum()) return error.TypeMismatch;
                const start_signed = start_val.toFixnum();
                const end_signed = end_val.toFixnum();
                if (start_signed < 0 or end_signed < 0) return error.TypeMismatch;
                const start: usize = @intCast(start_signed);
                const end: usize = @intCast(end_signed);
                const result = try stringPrims.substring(self.heap, str_val, start, end);
                try self.push(result);
            },
            .sym_name => {
                const sym_val = try self.pop();
                const name_str = switch (sym_val.typeKind()) {
                    .nil => try self.allocString("nil"),
                    .t => try self.allocString("t"),
                    .symbol => blk: {
                        const sym = sym_val.toPtr(Symbol);
                        break :blk try self.allocString(sym.getName());
                    },
                    else => return error.TypeMismatch,
                };
                try self.push(name_str);
            },
            .copy_symbol => {
                const copy_props = try self.pop();
                const sym_val = try self.pop();
                const sym_kind = sym_val.typeKind();
                // Get the symbol name
                const name = switch (sym_kind) {
                    .nil => "nil",
                    .t => "t",
                    .symbol => sym_val.toPtr(Symbol).getName(),
                    else => return error.TypeMismatch,
                };
                // Create new uninterned symbol
                const new_sym = try self.heap.allocSymbol(name);
                // Copy properties if requested (only plist is stored in Symbol)
                if (!copy_props.isNil() and sym_kind == .symbol) {
                    const orig = sym_val.toPtr(Symbol);
                    const new = new_sym.toPtr(Symbol);
                    // Copy plist
                    if (orig.plist != Value.nil) {
                        new.plist = orig.plist;
                    }
                }
                try self.push(new_sym);
            },
            .makunbound => {
                const sym_val = try self.pop();
                switch (sym_val.typeKind()) {
                    .nil, .t => {
                        // nil and t cannot be made unbound
                        return error.InvalidArgument;
                    },
                    .symbol => {
                        const sym = sym_val.toPtr(Symbol);
                        const local_name = sym.getName();
                        // Build qualified name using symbol's package
                        var qual_buf: [512]u8 = undefined;
                        const q = try qual_name.qualSym(self.allocator, sym, &qual_buf);
                        defer if (q.owned) self.allocator.free(q.name);
                        const qname = q.name;
                        // Look up symbol in global environment (qualified or local)
                        if (self.global_env) |env| {
                            const idx = env.lookup(qname) orelse env.lookup(local_name);
                            if (idx) |i| {
                                if (i < MAX_GLOBALS) {
                                    self.globals[i] = Value.unbound;
                                }
                            }
                        }
                        try self.push(sym_val);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .set_sym_val => {
                const val = try self.pop();
                const sym_val = try self.pop();
                switch (sym_val.typeKind()) {
                    .nil, .t => {
                        // nil and t cannot be set
                        return error.InvalidArgument;
                    },
                    .symbol => {
                        const sym = sym_val.toPtr(Symbol);
                        const local_name = sym.getName();
                        // Build qualified name using symbol's package
                        var qual_buf: [512]u8 = undefined;
                        const q = try qual_name.qualSym(self.allocator, sym, &qual_buf);
                        defer if (q.owned) self.allocator.free(q.name);
                        const qname = q.name;
                        // Look up symbol in global environment (qualified or local)
                        if (self.global_env) |env| {
                            const idx = env.lookup(qname) orelse env.lookup(local_name);
                            if (idx) |i| {
                                if (i < MAX_GLOBALS) {
                                    self.globals[i] = val;
                                    if (i >= self.num_globals) {
                                        self.num_globals = i + 1;
                                    }
                                }
                            }
                        }
                        try self.push(val);
                    },
                    else => return error.TypeMismatch,
                }
            },
            .type_of => {
                const val = try self.pop();
                const type_spec = try primitives.ty.typeOf(self.heap, val);
                try self.push(type_spec);
            },
            .str_eq => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringEqual(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_lt => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringLt(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_gt => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringGt(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_le => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringLe(a, b)) Value.t else Value.nil;
                try self.push(result);
            },
            .str_ge => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringGe(a, b)) Value.t else Value.nil;
                try self.push(result);
            },

            // Type assertions (gradual typing)
            .check_fixnum => {
                const val = try self.peek(0);
                if (!val.isFixnum()) return error.TypeMismatch;
            },
            .check_cons => {
                const val = try self.peek(0);
                if (!val.isCons()) return error.TypeMismatch;
            },
            .check_symbol => {
                const val = try self.peek(0);
                if (!val.isSymbol()) return error.TypeMismatch;
            },
            .check_string => {
                const val = try self.peek(0);
                if (!val.isString()) return error.TypeMismatch;
            },
            .check_vector => {
                const val = try self.peek(0);
                if (!val.isVector()) return error.TypeMismatch;
            },
            .check_closure => {
                const val = try self.peek(0);
                if (!val.isClosure()) return error.TypeMismatch;
            },
            .check_non_nil => {
                const val = try self.peek(0);
                if (val.isNil()) return error.TypeMismatch;
            },
            .check_list => {
                const val = try self.peek(0);
                if (!val.isNil() and !val.isCons()) return error.TypeMismatch;
            },
            .check_or => {
                // Read constant pool index for type vector
                const type_vec_idx = self.readU16();
                if (type_vec_idx >= self.chunk.getConstants().len) return error.InvalidConstant;
                const type_vec = self.chunk.getConstants()[type_vec_idx];

                // Get value to check
                const val = try self.peek(0);
                const val_kind = val.typeKind();
                const is_list = val_kind == .nil or val_kind == .cons;
                const is_atom = val_kind != .cons;
                const is_non_nil = val_kind != .nil;

                // Check if value matches any type in the list
                var matched = false;
                var current = type_vec;
                while (current.isCons()) {
                    const cons = current.toPtr(Cons);
                    const type_sym = cons.car;
                    const kind_match = switch (val_kind) {
                        .fixnum => type_sym.eq(self.type_syms.fixnum),
                        .cons => type_sym.eq(self.type_syms.cons),
                        .symbol => type_sym.eq(self.type_syms.symbol),
                        .string => type_sym.eq(self.type_syms.string),
                        .vector => type_sym.eq(self.type_syms.vector),
                        .closure => type_sym.eq(self.type_syms.closure),
                        .keyword => type_sym.eq(self.type_syms.keyword),
                        .nil => type_sym.eq(self.type_syms.nil),
                        else => false,
                    };
                    if (kind_match) {
                        matched = true;
                        break;
                    }
                    if (type_sym.eq(self.type_syms.list) and is_list) {
                        matched = true;
                        break;
                    }
                    if (type_sym.eq(self.type_syms.atom) and is_atom) {
                        matched = true;
                        break;
                    }
                    if (type_sym.eq(self.type_syms.t) and is_non_nil) {
                        matched = true;
                        break;
                    }
                    current = cons.cdr;
                }

                if (!matched) return error.TypeMismatch;
            },
            .check_refine => {
                // Stack: [value, predicate-result] -> [value]
                // Pop predicate result, check it's truthy, leave value
                const pred_result = try self.pop();
                if (pred_result.isNil()) return error.TypeMismatch;
                // Value is already on stack
            },

            // Catch/throw exception handling
            .push_catch => {
                const offset = self.readI16();
                const tag = try self.pop();
                // Calculate absolute jump target
                const catch_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                // Push catch frame
                if (self.catch_sp >= MAX_CATCHES) return error.StackOverflow;
                self.catch_stack[self.catch_sp] = .{
                    .tag = tag,
                    .chunk = self.chunk,
                    .catch_ip = catch_ip,
                    .catch_sp = self.sp,
                    .catch_fp = self.fp,
                };
                self.catch_sp += 1;
            },

            .pop_catch => {
                if (self.catch_sp == 0) return error.StackUnderflow;
                self.catch_sp -= 1;
            },

            .throw => {
                const value = try self.pop();
                const tag = try self.pop();
                try self.doThrow(tag, value);
            },

            .push_progv => {
                const values = try self.pop();
                const symbols = try self.pop();
                try self.pushProgvFrame(symbols, values);
            },

            .pop_progv => {
                try self.popProgvFrame();
            },

            // Block/return-from (lexical non-local exit)
            .push_block => {
                const offset = self.readI16();
                // Calculate exit_ip relative to current IP (after offset bytes, before name_idx)
                const exit_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                const name_idx = self.readU16();
                const name_raw = self.chunk.getConstants()[name_idx];
                if (self.block_sp >= MAX_BLOCKS) return error.StackOverflow;
                self.block_stack[self.block_sp] = .{
                    .name_raw = name_raw,
                    .chunk = self.chunk,
                    .exit_ip = exit_ip,
                    .block_sp = self.sp,
                    .block_fp = self.fp,
                };
                self.block_sp += 1;
            },

            .pop_block => {
                if (self.block_sp == 0) return error.StackUnderflow;
                self.block_sp -= 1;
            },

            .return_from => {
                const name_idx = self.readU16();
                const name_raw = self.chunk.getConstants()[name_idx];
                const value = try self.pop();
                try self.doReturnFrom(name_raw, value);
            },

            .push_unwind => {
                const offset = self.readI16();
                const cleanup_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                if (self.unwind_sp >= MAX_UNWINDS) return error.StackOverflow;
                self.unwind_stack[self.unwind_sp] = .{
                    .chunk = self.chunk,
                    .cleanup_ip = cleanup_ip,
                    .unwind_sp = self.sp,
                    .unwind_fp = self.fp,
                };
                self.unwind_sp += 1;
            },

            .pop_unwind => {
                _ = self.readI16(); // Skip unused operand
                // For normal exit, pop the unwind frame (doThrow/doError/doReturnFrom already popped for unwind case)
                if (!self.is_unwinding and !self.is_returning_from_block and self.unwind_sp > 0) {
                    self.unwind_sp -= 1;
                }
                // If we're unwinding (cleanup ran due to throw, error, or return-from), re-invoke
                if (self.is_unwinding) {
                    self.is_unwinding = false;

                    // Check if unwinding due to error
                    if (self.pending_error) |err| {
                        self.pending_error = null;
                        return err;
                    }

                    // Otherwise unwinding due to throw
                    const tag = self.pending_throw_tag;
                    const value = self.pending_throw_value;
                    self.pending_throw_tag = Value.nil;
                    self.pending_throw_value = Value.nil;
                    try self.doThrow(tag, value);
                } else if (self.is_returning_from_block) {
                    self.is_returning_from_block = false;

                    // Continue return-from
                    const name_raw = self.pending_block_name;
                    const value = self.pending_block_value;
                    self.pending_block_name = Value.nil;
                    self.pending_block_value = Value.nil;
                    try self.doReturnFrom(name_raw, value);
                }
            },

            // Restart handling
            .push_restart => {
                const offset = self.readI16();
                const name = try self.pop();
                const handler_ip = @as(usize, @intCast(@as(isize, @intCast(self.ip)) + offset));
                if (self.restart_sp >= MAX_RESTARTS) return error.StackOverflow;
                self.restart_stack[self.restart_sp] = .{
                    .name = name,
                    .chunk = self.chunk,
                    .handler_ip = handler_ip,
                    .restart_sp = self.sp,
                    .restart_fp = self.fp,
                    .catch_depth = self.catch_sp,
                    .unwind_depth = self.unwind_sp,
                };
                self.restart_sp += 1;
            },

            .pop_restarts => {
                const count = self.readU8();
                if (self.restart_sp < count) return error.StackUnderflow;
                self.restart_sp -= count;
            },

            .invoke_restart => {
                const value = try self.pop();
                const name = try self.pop();
                try self.doInvokeRestart(name, value);
            },

            .find_restart => {
                const name = try self.pop();
                // Search for restart by name
                var found = false;
                var i = self.restart_sp;
                while (i > 0) {
                    i -= 1;
                    if (self.restart_stack[i].name.raw == name.raw) {
                        found = true;
                        break;
                    }
                }
                try self.push(if (found) Value.t else Value.nil);
            },

            .handler_bind => {
                const handlers_alist = try self.pop();
                const body_fn = try self.pop();
                try self.doHandlerBind(body_fn, handlers_alist);
            },

            // Multiple values
            .values => {
                const count = self.readU8();
                if (count == 0) {
                    // (values) returns nil
                    try self.push(Value.nil);
                    self.secondary_values_count = 0;
                } else {
                    // Pop all values, store secondary values, push primary
                    // Values are on stack in order: v1 v2 ... vN (vN on top)
                    // We want: primary=v1, secondary=[v2, v3, ...]
                    const secondary_count = count - 1;
                    if (secondary_count > MAX_SECONDARY_VALUES) return error.StackOverflow;

                    // Pop secondary values in reverse order
                    var i: usize = 0;
                    while (i < secondary_count) : (i += 1) {
                        const idx = secondary_count - 1 - i;
                        self.secondary_values[idx] = try self.pop();
                    }
                    self.secondary_values_count = secondary_count;
                    // Primary value remains on stack
                }
            },

            .mv_bind => {
                const count = self.readU8();
                const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;

                // Primary value is already on stack - store to local 0
                const primary = try self.pop();
                self.stack[bp] = primary;

                // Store secondary values (or nil if not enough) to locals 1..count-1
                var i: usize = 1;
                while (i < count) : (i += 1) {
                    const val = if (i - 1 < self.secondary_values_count)
                        self.secondary_values[i - 1]
                    else
                        Value.nil;
                    self.stack[bp + i] = val;
                }

                // Clear secondary values
                self.secondary_values_count = 0;
            },

            .mv_list => {
                // Primary value is on stack, secondaries in buffer
                // Create a list of all values: (primary secondary1 secondary2 ...)
                const primary = try self.pop();

                // Build list in reverse: start with nil, cons secondaries, then cons primary
                var result = Value.nil;

                // Add secondary values in reverse order
                var i: usize = self.secondary_values_count;
                while (i > 0) : (i -= 1) {
                    result = try self.allocCons(self.secondary_values[i - 1], result);
                }

                // Add primary at front
                result = try self.allocCons(primary, result);

                // Clear secondary values
                self.secondary_values_count = 0;

                try self.push(result);
            },

            .values_list => {
                // Pop a list from stack, return its elements as multiple values
                const list = try self.pop();

                switch (list.typeKind()) {
                    .nil => {
                        // Empty list -> return nil with no secondary values
                        self.secondary_values_count = 0;
                        try self.push(Value.nil);
                    },
                    .cons => {
                        // Walk the list, extract elements
                        var first = Value.nil;
                        var count: usize = 0;
                        var current = list;

                        while (current.isCons()) {
                            const cons = current.toPtr(runtime.Cons);
                            if (count == 0) {
                                first = cons.car;
                            } else if (count - 1 < self.secondary_values.len) {
                                self.secondary_values[count - 1] = cons.car;
                            }
                            count += 1;
                            current = cons.cdr;
                        }

                        self.secondary_values_count = if (count > 1) count - 1 else 0;
                        try self.push(first);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .format => {
                const argc = self.readU8();
                if (argc > 32) return error.InvalidArgument;
                // Stack: dest, control-string, arg1, ..., argN (argN on top)
                // Pop args in reverse order
                var args: [32]Value = undefined;
                var arg_idx: usize = argc;
                while (arg_idx > 0) : (arg_idx -= 1) {
                    args[arg_idx - 1] = try self.pop();
                }
                const control = try self.pop();
                const dest = try self.pop();

                if (!control.isString()) return error.TypeMismatch;

                const result = try self.doFormat(dest, control, args[0..argc]);
                try self.push(result);
            },

            // Hash table operations
            .make_hash => {
                const capacity = self.readU16();
                const test_byte = self.readU8();
                const test_type: runtime.HashTest = @enumFromInt(test_byte);
                const ht = try self.allocHashTable(capacity, test_type);
                try self.push(ht);
            },
            .hash_get => {
                const key = try self.pop();
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                const result = hashTableGet(ht, key);
                try self.push(result);
            },
            .sxhash => {
                const obj = try self.pop();
                const result = try hash_prims.primSxhash(self.heap, &[_]Value{obj});
                try self.push(result);
            },
            .hash_set => {
                const value = try self.pop();
                const key = try self.pop();
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);

                // Try to insert, resize if needed
                if (!hashTableSet(ht, key, value)) {
                    // Resize in place - updates ht's entries pointer
                    try hashTableResizeInPlace(self, ht);
                    // Now insert should succeed
                    _ = hashTableSet(ht, key, value);
                }
                // Only push the value (CL setf gethash semantics)
                try self.push(value);
            },
            .hash_rem => {
                const key = try self.pop();
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                const removed = hashTableRemove(ht, key);
                try self.push(if (removed) Value.t else Value.nil);
            },
            .hash_count => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                try self.push(Value.makeFixnum(@intCast(ht.count)));
            },
            .hash_capacity => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                try self.push(Value.makeFixnum(@intCast(ht.capacity)));
            },
            .hashtablep => {
                const val = try self.pop();
                try self.push(if (val.isHashTable()) Value.t else Value.nil);
            },
            .packagep => {
                const val = try self.pop();
                try self.push(if (val.isPackage()) Value.t else Value.nil);
            },
            .symbol_package => {
                const val = try self.pop();
                // Handle special symbols nil and t
                switch (val.typeKind()) {
                    .nil, .t => {
                        // nil and t are in the CL package
                        const cl_name = try self.heap.allocBaseString("CL");
                        if (try self.heap.findLispPackage(cl_name)) |pkg| {
                            try self.push(pkg);
                        } else {
                            try self.push(Value.nil);
                        }
                    },
                    .symbol => {
                        const sym = val.toPtr(Symbol);
                        const pkg_ptr = sym.reserved;
                        if (pkg_ptr != 0) {
                            // Get the Zig package struct
                            const zig_pkg: *const runtime.heap.Package = @ptrFromInt(pkg_ptr);
                            // Look up the Lisp package object by name
                            const name_val = try self.heap.allocBaseString(zig_pkg.name);
                            if (try self.heap.findLispPackage(name_val)) |pkg| {
                                try self.push(pkg);
                            } else {
                                try self.push(Value.nil);
                            }
                        } else {
                            try self.push(Value.nil);
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },
            .package_name => {
                const pkg = try self.pop();
                const result = try primitives.package.packageName(pkg);
                try self.push(result);
            },
            .package_nicknames => {
                const pkg = try self.pop();
                const result = try primitives.package.packageNicknames(pkg);
                try self.push(result);
            },
            .package_use_list => {
                const pkg = try self.pop();
                const result = try primitives.package.packageUseList(pkg);
                try self.push(result);
            },
            .package_used_by_list => {
                const pkg = try self.pop();
                const result = try primitives.package.packageUsedByList(self.heap, pkg);
                try self.push(result);
            },
            .package_shadowing_symbols => {
                const pkg = try self.pop();
                const result = try primitives.package.packageShadowingSymbols(pkg);
                try self.push(result);
            },
            .list_all_packages => {
                const result = try primitives.package.listAllPackages(self.heap);
                try self.push(result);
            },
            .compute_restarts => {
                // Build list of active restart names from restart stack
                var result = Value.nil;
                var i: usize = self.restart_sp;
                while (i > 0) {
                    i -= 1;
                    const restart_sym = self.restart_stack[i].name;
                    result = try self.heap.allocCons(restart_sym, result);
                }
                try self.push(result);
            },
            .restart_name => {
                // In our simplified implementation, restarts are just symbols
                const restart = try self.pop();
                // Return the symbol itself as the name
                try self.push(restart);
            },
            .directory => {
                const pathname = try self.pop();
                const result = try io.listDirectory(self.heap, pathname);
                try self.push(result);
            },
            .pathname_match_p => {
                const wildcard = try self.pop();
                const pathname = try self.pop();
                const result = try io.pathnameMatchP(pathname, wildcard);
                try self.push(result);
            },
            .enough_namestring => {
                // enough-namestring returns shortest namestring to identify pathname
                // In our simplified implementation, returns full namestring
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;

                const pn = pn_val.toPtr(runtime.Pathname);

                var result = std.ArrayList(u8){};
                defer result.deinit(self.allocator);

                // Process directory
                if (pn.directory != Value.nil) {
                    var dir_list = pn.directory;
                    if (dir_list.isCons()) {
                        const first = dir_list.toPtr(runtime.Cons).car;
                        if (first.raw == self.builtins.kw_absolute.raw) {
                            try result.append(self.allocator, '/');
                            dir_list = dir_list.toPtr(runtime.Cons).cdr;
                        } else if (first.raw == self.builtins.kw_relative.raw) {
                            dir_list = dir_list.toPtr(runtime.Cons).cdr;
                        }
                    }
                    while (dir_list != Value.nil) {
                        if (!dir_list.isCons()) break;
                        const cons = dir_list.toPtr(runtime.Cons);
                        const component = cons.car;
                        if (component.isString()) {
                            const comp_str = component.toPtr(runtime.String);
                            try result.appendSlice(self.allocator, comp_str.bytes());
                            try result.append(self.allocator, '/');
                        }
                        dir_list = cons.cdr;
                    }
                }

                // Add name
                if (pn.name != Value.nil and pn.name.isString()) {
                    const name_str = pn.name.toPtr(runtime.String);
                    try result.appendSlice(self.allocator, name_str.bytes());
                }

                // Add type (extension)
                if (pn.type != Value.nil and pn.type.isString()) {
                    try result.append(self.allocator, '.');
                    const type_str = pn.type.toPtr(runtime.String);
                    try result.appendSlice(self.allocator, type_str.bytes());
                }

                const str = try self.heap.allocBaseString(result.items);
                try self.push(str);
            },
            .decode_float => {
                const val = try self.pop();
                const result = try arith.decodeFloat(self.heap, val);
                try self.push(result);
            },
            .integer_decode_float => {
                const val = try self.pop();
                const result = try arith.integerDecodeFloat(self.heap, val);
                try self.push(result);
            },
            .float_radix => {
                const val = try self.pop();
                const result = try arith.floatRadix(val);
                try self.push(result);
            },
            .float_digits => {
                const val = try self.pop();
                const result = try arith.floatDigits(val);
                try self.push(result);
            },
            .find_package => {
                const name = try self.pop();
                if (try primitives.package.findPackage(self.heap, name)) |pkg| {
                    try self.push(pkg);
                } else {
                    try self.push(Value.nil);
                }
            },
            .delete_package => {
                const pkg = try self.pop();
                const deleted = try primitives.package.deletePackage(self.heap, pkg);
                try self.push(if (deleted) Value.t else Value.nil);
            },
            .pkg_import => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.importSymbols(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_unexport => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.unexportSymbols(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_shadow => {
                const pkg = try self.pop();
                const names = try self.pop();
                try primitives.package.shadowSymbols(self.heap, names, pkg);
                try self.push(Value.t);
            },
            .pkg_shadowing_import => {
                const pkg = try self.pop();
                const symbols = try self.pop();
                try primitives.package.shadowingImport(self.heap, symbols, pkg);
                try self.push(Value.t);
            },
            .pkg_unuse_package => {
                const pkg = try self.pop();
                const packages = try self.pop();
                try primitives.package.unusePackage(self.heap, packages, pkg);
                try self.push(Value.t);
            },
            .pkg_unintern => {
                const pkg = try self.pop();
                const symbol = try self.pop();
                const removed = try primitives.package.uninternSymbol(self.heap, symbol, pkg);
                try self.push(if (removed) Value.t else Value.nil);
            },
            .pkg_find_symbol => {
                const pkg = try self.pop();
                const name = try self.pop();
                const result = try primitives.package.findSymbol(self.heap, name, pkg);
                // Returns (symbol . (status . nil)), push symbol then status
                if (result.isCons()) {
                    const c1 = result.toPtr(runtime.Cons);
                    try self.push(c1.car); // symbol
                    if (c1.cdr.isCons()) {
                        const c2 = c1.cdr.toPtr(runtime.Cons);
                        try self.push(c2.car); // status
                    } else {
                        try self.push(Value.nil);
                    }
                } else {
                    try self.push(Value.nil);
                    try self.push(Value.nil);
                }
            },
            .pkg_find_all_symbols => {
                const name = try self.pop();
                const result = try primitives.package.findAllSymbols(self.heap, name);
                try self.push(result);
            },
            .apropos_list => {
                const substring = try self.pop();
                const result = try primitives.package.aproposSymbols(self.heap, substring);
                try self.push(result);
            },
            .read_char_no_hang => {
                const stream = try self.pop();
                const result = try io.readCharNoHang(stream);
                try self.push(result);
            },
            .pkg_make_package => {
                const use_list = try self.pop();
                const nicknames = try self.pop();
                const name = try self.pop();
                const result = try primitives.package.makePackage(
                    self.heap,
                    name,
                    if (nicknames.isNil()) null else nicknames,
                    if (use_list.isNil()) null else use_list,
                );
                try self.push(result);
            },
            .pkg_rename_package => {
                const new_nicknames = try self.pop();
                const new_name = try self.pop();
                const pkg = try self.pop();
                const result = try primitives.package.renamePackage(
                    self.heap,
                    pkg,
                    new_name,
                    if (new_nicknames.isNil()) null else new_nicknames,
                );
                try self.push(result);
            },
            .hash_clear => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                hashTableClear(ht);
                try self.push(ht_val);
            },
            .hash_test => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                const test_name = switch (ht.test_type) {
                    .eq => "eq",
                    .eql => "eql",
                    .equal => "equal",
                };
                const sym = try self.heap.intern(test_name);
                try self.push(sym);
            },
            .hash_keys => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                // Build list of keys from hash table entries
                var result = Value.nil;
                const entries = ht.getEntries();
                for (entries) |entry| {
                    if (!HashTable.isAvailable(entry)) {
                        result = try self.allocCons(entry.key, result);
                    }
                }
                try self.push(result);
            },
            .hash_alist => {
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                // Build alist of (key . value) pairs from hash table entries
                var result = Value.nil;
                const entries = ht.getEntries();
                for (entries) |entry| {
                    if (!HashTable.isAvailable(entry)) {
                        const pair = try self.allocCons(entry.key, entry.value);
                        result = try self.allocCons(pair, result);
                    }
                }
                try self.push(result);
            },
            .rationalp => {
                const val = try self.pop();
                try self.push(if (val.isRational()) Value.t else Value.nil);
            },
            .complexp => {
                const val = try self.pop();
                try self.push(if (val.isComplex()) Value.t else Value.nil);
            },
            .make_complex => {
                const imag = try self.pop();
                const real = try self.pop();
                const real_f = switch (real.typeKind()) {
                    .float => real.toFloat(),
                    .fixnum => @as(f64, @floatFromInt(real.toFixnum())),
                    else => return error.TypeMismatch,
                };
                const imag_f = switch (imag.typeKind()) {
                    .float => imag.toFloat(),
                    .fixnum => @as(f64, @floatFromInt(imag.toFixnum())),
                    else => return error.TypeMismatch,
                };
                const cplx = try self.heap.allocComplex(real_f, imag_f);
                try self.push(cplx);
            },
            .real_part => {
                const val = try self.pop();
                if (!val.isComplex()) return error.TypeMismatch;
                const cplx = val.toPtr(runtime.Complex);
                try self.push(Value.makeFloat(cplx.real));
            },
            .imag_part => {
                const val = try self.pop();
                if (!val.isComplex()) return error.TypeMismatch;
                const cplx = val.toPtr(runtime.Complex);
                try self.push(Value.makeFloat(cplx.imag));
            },

            .numerator => {
                const val = try self.pop();
                if (!val.isRational()) return error.TypeMismatch;
                const rat = val.toPtr(runtime.Rational);
                try self.push(Value.makeFixnum(rat.numerator));
            },
            .denominator => {
                const val = try self.pop();
                if (!val.isRational()) return error.TypeMismatch;
                const rat = val.toPtr(runtime.Rational);
                try self.push(Value.makeFixnum(rat.denominator));
            },
            .rational => {
                const val = try self.pop();
                const result = if (val.isFloat())
                    try primitives.rational.floatToRational(self.heap, val.toFloat())
                else
                    val; // Already rational or integer
                try self.push(result);
            },
            .get => {
                const indicator = try self.pop();
                const sym = try self.pop();
                const result = try primitives.list.get(sym, indicator);
                try self.push(result);
            },
            .put => {
                const value = try self.pop();
                const indicator = try self.pop();
                const sym = try self.pop();
                const result = try primitives.list.put(self.heap, sym, indicator, value);
                try self.push(result);
            },
            .remprop => {
                const indicator = try self.pop();
                const sym = try self.pop();
                const result = try primitives.list.remprop(sym, indicator);
                try self.push(result);
            },
            // Stream operations
            .streamp => {
                const val = try self.pop();
                try self.push(if (val.isStream()) Value.t else Value.nil);
            },
            .input_stream_p => {
                const val = try self.pop();
                if (!val.isStream()) {
                    try self.push(Value.nil);
                } else {
                    const stream = val.toPtr(runtime.Stream);
                    try self.push(if (stream.isInput()) Value.t else Value.nil);
                }
            },
            .output_stream_p => {
                const val = try self.pop();
                if (!val.isStream()) {
                    try self.push(Value.nil);
                } else {
                    const stream = val.toPtr(runtime.Stream);
                    try self.push(if (stream.isOutput()) Value.t else Value.nil);
                }
            },
            .open_stream_p => {
                const val = try self.pop();
                if (!val.isStream()) {
                    try self.push(Value.nil);
                } else {
                    const stream = val.toPtr(runtime.Stream);
                    try self.push(if (!stream.isClosed()) Value.t else Value.nil);
                }
            },
            .interactive_stream_p => {
                const val = try self.pop();
                if (!val.isStream()) {
                    try self.push(Value.nil);
                } else {
                    const stream = val.toPtr(runtime.Stream);
                    // stdin, stdout, stderr are interactive
                    const is_interactive = stream.stream_type == .stdin or
                        stream.stream_type == .stdout or
                        stream.stream_type == .stderr;
                    try self.push(if (is_interactive) Value.t else Value.nil);
                }
            },
            .stream_element_type => {
                const val = try self.pop();
                if (!val.isStream()) return error.TypeMismatch;
                // Our streams are character streams
                try self.push(try self.heap.intern("character"));
            },
            .stream_external_format => {
                const val = try self.pop();
                if (!val.isStream()) return error.TypeMismatch;
                // Return :default as external format
                try self.push(try self.heap.internKeyword("default"));
            },
            .make_broadcast_stream => {
                // Pop count streams from stack, create broadcast stream
                const count = self.chunk.code[self.ip];
                self.ip += 1;
                var streams_list = Value.nil;
                // Build list in reverse order (stack order)
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const stream = try self.pop();
                    if (!stream.isStream()) return error.TypeMismatch;
                    streams_list = try self.heap.allocCons(stream, streams_list);
                }
                const result = try self.heap.allocBroadcastStream(streams_list);
                try self.push(result);
            },
            .make_concatenated_stream => {
                // Pop count streams from stack, create concatenated stream
                const count = self.chunk.code[self.ip];
                self.ip += 1;
                var streams_list = Value.nil;
                // Build list in reverse order (stack order)
                var i: usize = 0;
                while (i < count) : (i += 1) {
                    const stream = try self.pop();
                    if (!stream.isStream()) return error.TypeMismatch;
                    streams_list = try self.heap.allocCons(stream, streams_list);
                }
                const result = try self.heap.allocConcatenatedStream(streams_list);
                try self.push(result);
            },
            .make_echo_stream => {
                const output_stream = try self.pop();
                const input_stream = try self.pop();
                if (!input_stream.isStream() or !output_stream.isStream()) return error.TypeMismatch;
                const result = try self.heap.allocEchoStream(input_stream, output_stream);
                try self.push(result);
            },
            .make_synonym_stream => {
                const symbol = try self.pop();
                if (!symbol.isSymbol()) return error.TypeMismatch;
                const result = try self.heap.allocSynonymStream(symbol);
                try self.push(result);
            },
            .make_two_way_stream => {
                const output_stream = try self.pop();
                const input_stream = try self.pop();
                if (!input_stream.isStream() or !output_stream.isStream()) return error.TypeMismatch;
                const result = try self.heap.allocTwoWayStream(input_stream, output_stream);
                try self.push(result);
            },
            .broadcast_stream_streams => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .broadcast) return error.TypeMismatch;
                try self.push(stream.source_value); // the list of streams
            },
            .concatenated_stream_streams => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .concatenated) return error.TypeMismatch;
                try self.push(stream.source_value); // the list of streams
            },
            .echo_stream_input_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .echo) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.car);
            },
            .echo_stream_output_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .echo) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.cdr);
            },
            .synonym_stream_symbol => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .synonym) return error.TypeMismatch;
                try self.push(stream.source_value); // the symbol
            },
            .two_way_stream_input_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .two_way) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.car);
            },
            .two_way_stream_output_stream => {
                const stream_val = try self.pop();
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .two_way) return error.TypeMismatch;
                // source_value is (input . output)
                const pair = stream.source_value.toPtr(runtime.Cons);
                try self.push(pair.cdr);
            },
            .make_broadcast_stream_list => {
                // Pop list of streams, create broadcast stream
                const streams_list = try self.pop();
                // Validate all elements are streams
                var cursor = streams_list;
                while (cursor.isCons()) {
                    const cons = cursor.toPtr(runtime.Cons);
                    if (!cons.car.isStream()) return error.TypeMismatch;
                    cursor = cons.cdr;
                }
                const result = try self.heap.allocBroadcastStream(streams_list);
                try self.push(result);
            },
            .make_concatenated_stream_list => {
                // Pop list of streams, create concatenated stream
                const streams_list = try self.pop();
                // Validate all elements are streams
                var cursor = streams_list;
                while (cursor.isCons()) {
                    const cons = cursor.toPtr(runtime.Cons);
                    if (!cons.car.isStream()) return error.TypeMismatch;
                    cursor = cons.cdr;
                }
                const result = try self.heap.allocConcatenatedStream(streams_list);
                try self.push(result);
            },
            .disassemble => {
                const func_val = try self.pop();
                if (!func_val.isClosure()) return error.TypeMismatch;
                const closure = func_val.toPtr(runtime.Closure);
                // Get the chunk from the closure's code field
                if (!closure.code.isChunk()) return error.TypeMismatch;
                const chunk = closure.code.toPtr(runtime.Chunk);
                // Write disassembly to stdout using runtime variant
                var buf: [4096]u8 = undefined;
                const stdout = std.fs.File.stdout();
                var bw = stdout.writer(&buf);
                const writer = &bw.interface;
                try disasm.disassembleRuntime(chunk, writer);
                try writer.flush();
                try self.push(Value.nil);
            },
            .read_char_stream => {
                const stream = try self.pop();
                const result = try io.readChar(stream, null, null);
                try self.push(result);
            },
            .peek_char_stream => {
                const stream = try self.pop();
                const result = try io.peekChar(null, stream);
                try self.push(result);
            },
            .open_file => {
                const direction = try self.pop();
                const filename = try self.pop();
                const result = try io.openFile(self.heap, filename, direction, null, null);
                try self.push(result);
            },
            .close_stream => {
                const stream = try self.pop();
                try io.closeStream(stream, null);
                try self.push(Value.nil);
            },
            .make_string_input_stream => {
                const str = try self.pop();
                if (!str.isString()) return error.TypeMismatch;
                const stream = try self.heap.allocStringInputStream(str);
                try self.push(stream);
            },
            .make_string_output_stream => {
                const stream = try self.heap.allocStringOutputStream();
                try self.push(stream);
            },
            .get_output_stream_string => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primGetOutputStreamString(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },
            .write_to_stream => {
                const stream_val = try self.pop();
                const str_val = try self.pop();
                const result = try primitives.stream.primWriteString(self.heap, &[_]Value{ str_val, stream_val });
                try self.push(result);
            },
            .pathname_host => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameHost(path);
                try self.push(result);
            },
            .pathname_device => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameDevice(path);
                try self.push(result);
            },
            .pathname_directory => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameDirectory(path);
                try self.push(result);
            },
            .pathname_name => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameName(path);
                try self.push(result);
            },
            .pathname_type => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameType(path);
                try self.push(result);
            },
            .pathname_version => {
                const path = try self.pop();
                const result = try primitives.pathname.pathnameVersion(path);
                try self.push(result);
            },
            .truename => {
                const path = try self.pop();
                const result = try primitives.pathname.truename(self.allocator, self.heap, &self.builtins, path);
                try self.push(result);
            },
            .ensure_directories_exist => {
                const path = try self.pop();
                const result = try primitives.pathname.ensureDirectoriesExist(self.allocator, self.heap, &self.builtins, path);
                try self.push(result.pathname);
            },
            .package_symbols_table => {
                const pkg = try self.pop();
                const result = try primitives.package.packageSymbols(pkg);
                try self.push(result);
            },
            .package_exports_table => {
                const pkg = try self.pop();
                const result = try primitives.package.packageExports(pkg);
                try self.push(result);
            },
            .package_symbols_list => {
                const pkg_name = try self.pop();
                const result = try primitives.package.packageSymbolsList(self.heap, pkg_name);
                try self.push(result);
            },
            .package_exports_list => {
                const pkg_name = try self.pop();
                const result = try primitives.package.packageExportsList(self.heap, pkg_name);
                try self.push(result);
            },
            .unintern => {
                const pkg = try self.pop();
                const sym = try self.pop();
                const result = try primitives.package.uninternSymbol(self.heap, sym, pkg);
                try self.push(if (result) Value.t else Value.nil);
            },
            .find_symbol => {
                const pkg = try self.pop();
                const name = try self.pop();
                const result = try primitives.package.findSymbol(self.heap, name, pkg);
                try self.push(result);
            },
            .open => {
                const mode_val = try self.pop();
                const path_val = try self.pop();
                const result = try primitives.stream.primOpen(self.heap, &[_]Value{ path_val, mode_val }, &self.builtins);
                try self.push(result);
            },
            .close => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primClose(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .read_line => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primReadLine(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .write_line => {
                const text_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primWriteLine(self.heap, &[_]Value{ stream_val, text_val });
                try self.push(result);
            },

            .write_string => {
                const text_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primWriteString(self.heap, &[_]Value{ text_val, stream_val });
                try self.push(result);
            },

            .read_byte => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primReadByte(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .write_byte => {
                const byte_val = try self.pop();
                const stream_val = try self.pop();
                const result = try primitives.stream.primWriteByte(self.heap, &[_]Value{ stream_val, byte_val });
                try self.push(result);
            },

            .file_position => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primFilePosition(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .file_length => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primFileLength(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .finish_output => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primFinishOutput(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .force_output => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primForceOutput(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .clear_input => {
                const stream_val = try self.pop();
                // Clear input is a no-op since we don't buffer input at the Lisp level
                // Accept nil or a stream, return nil
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                try self.push(Value.nil);
            },

            .clear_output => {
                const stream_val = try self.pop();
                // Clear output is a no-op since we don't buffer output at the Lisp level
                // Accept nil or a stream, return nil
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                try self.push(Value.nil);
            },

            .sleep => {
                const seconds = try self.pop();
                try primitives.io.sleepSeconds(seconds);
                try self.push(Value.nil);
            },

            .make_array => {
                const operand = self.readU8();
                const rank: u8 = operand >> 1;
                const has_initial: bool = (operand & 1) == 1;

                if (rank == 0 or rank > 8) return error.TypeMismatch;

                // Pop initial-element if present
                const initial_element = if (has_initial) try self.pop() else Value.nil;

                // Pop dimensions from stack (in reverse order)
                var dimensions: [8]u64 = [_]u64{0} ** 8;
                var total_size: u64 = 1;
                var i: usize = rank;
                while (i > 0) {
                    i -= 1;
                    const dim_val = try self.pop();
                    if (!dim_val.isFixnum()) return error.TypeMismatch;
                    const dim_signed = dim_val.toFixnum();
                    if (dim_signed < 0) return error.TypeMismatch;
                    const dim: u64 = @intCast(dim_signed);
                    dimensions[i] = dim;
                    total_size *= dim;
                }

                // Allocate array object + data storage together
                const total_bytes = @sizeOf(runtime.Array) + total_size * @sizeOf(Value);
                const ptr = try self.heap.allocRaw(total_bytes);
                const arr: *runtime.Array = @ptrCast(@alignCast(ptr));

                // Data follows immediately after header
                const data_ptr: [*]Value = @ptrCast(@alignCast(ptr + @sizeOf(runtime.Array)));

                arr.* = .{
                    .kind = .array,
                    .rank = rank,
                    .dimensions = dimensions,
                    .total_size = total_size,
                    .data_ptr = @intFromPtr(data_ptr),
                };

                // Initialize with initial-element
                for (0..total_size) |idx| {
                    data_ptr[idx] = initial_element;
                }

                try self.push(Value.makeArray(arr));
            },

            .aref => {
                const sub_count = self.readU8();
                if (sub_count == 0 or sub_count > 8) return error.TypeMismatch;

                // Pop subscripts from stack (in reverse order)
                var subscripts: [8]u64 = [_]u64{0} ** 8;
                var j: usize = sub_count;
                while (j > 0) {
                    j -= 1;
                    const sub_val = try self.pop();
                    if (!sub_val.isFixnum()) return error.TypeMismatch;
                    const sub_signed = sub_val.toFixnum();
                    if (sub_signed < 0) return error.TypeMismatch;
                    subscripts[j] = @intCast(sub_signed);
                }

                // Pop array or vector
                const arr_val = try self.pop();

                switch (arr_val.typeKind()) {
                    .vector => {
                        // Handle vectors (1D case)
                        if (sub_count != 1) return error.TypeMismatch;
                        const vec = arr_val.toPtr(Vector);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= vec.length) return error.TypeMismatch;
                        try self.push(vec.get(idx));
                    },
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);

                        // Verify rank matches
                        if (arr.rank != sub_count) return error.TypeMismatch;

                        // Calculate linear index using row-major order
                        var index: u64 = 0;
                        for (0..sub_count) |k| {
                            // Bounds check
                            if (subscripts[k] >= arr.dimensions[k]) return error.TypeMismatch;

                            // Calculate stride (product of remaining dimensions)
                            var stride: u64 = 1;
                            for (k + 1..sub_count) |m| {
                                stride *= arr.dimensions[m];
                            }
                            index += subscripts[k] * stride;
                        }

                        // Access element
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        try self.push(data[index]);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .aset => {
                const sub_count = self.readU8();
                if (sub_count == 0 or sub_count > 8) return error.TypeMismatch;

                // Pop new value
                const new_val = try self.pop();

                // Pop subscripts from stack (in reverse order)
                var subscripts: [8]u64 = [_]u64{0} ** 8;
                var j: usize = sub_count;
                while (j > 0) {
                    j -= 1;
                    const sub_val = try self.pop();
                    if (!sub_val.isFixnum()) return error.TypeMismatch;
                    const sub_signed = sub_val.toFixnum();
                    if (sub_signed < 0) return error.TypeMismatch;
                    subscripts[j] = @intCast(sub_signed);
                }

                // Pop array or vector
                const arr_val = try self.pop();

                switch (arr_val.typeKind()) {
                    .vector => {
                        // Handle vectors (1D case)
                        if (sub_count != 1) return error.TypeMismatch;
                        const vec = arr_val.toPtr(Vector);
                        const idx: usize = @intCast(subscripts[0]);
                        if (idx >= vec.length) return error.TypeMismatch;
                        vec.set(idx, new_val);
                        try self.push(new_val);
                    },
                    .array => {
                        const arr = arr_val.toPtr(runtime.Array);

                        // Verify rank matches
                        if (arr.rank != sub_count) return error.TypeMismatch;

                        // Calculate linear index using row-major order
                        var index: u64 = 0;
                        for (0..sub_count) |k| {
                            // Bounds check
                            if (subscripts[k] >= arr.dimensions[k]) return error.TypeMismatch;

                            // Calculate stride (product of remaining dimensions)
                            var stride: u64 = 1;
                            for (k + 1..sub_count) |m| {
                                stride *= arr.dimensions[m];
                            }
                            index += subscripts[k] * stride;
                        }

                        // Set element
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        data[index] = new_val;

                        // Return the value (Common Lisp setf semantics)
                        try self.push(new_val);
                    },
                    else => return error.TypeMismatch,
                }
            },

            .array_dimension => {
                const axis_val = try self.pop();
                const arr_val = try self.pop();

                if (!arr_val.isArray()) return error.TypeMismatch;
                if (!axis_val.isFixnum()) return error.TypeMismatch;

                const arr = arr_val.toPtr(runtime.Array);
                const axis_signed = axis_val.toFixnum();
                if (axis_signed < 0) return error.TypeMismatch;
                const axis: usize = @intCast(axis_signed);

                if (axis >= arr.rank) return error.TypeMismatch;

                const dimension: i64 = @intCast(arr.dimensions[axis]);
                try self.push(Value.makeFixnum(dimension));
            },

            .array_dimensions => {
                const arr_val = try self.pop();

                if (!arr_val.isArray()) return error.TypeMismatch;

                const arr = arr_val.toPtr(runtime.Array);

                // Build list of dimensions from right to left
                var result = Value.nil;
                var i: usize = arr.rank;
                while (i > 0) {
                    i -= 1;
                    const dim: i64 = @intCast(arr.dimensions[i]);
                    result = try self.allocCons(Value.makeFixnum(dim), result);
                }

                try self.push(result);
            },

            // Pathname operations
            .make_pathname => {
                // Operand flags: bit 0=host, 1=device, 2=directory, 3=name, 4=type, 5=version
                const flags = self.readU8();

                // Pop components from stack (in reverse order of bits)
                const version = if ((flags & 0x20) != 0) try self.pop() else Value.nil;
                const type_comp = if ((flags & 0x10) != 0) try self.pop() else Value.nil;
                const name = if ((flags & 0x08) != 0) try self.pop() else Value.nil;
                const directory = if ((flags & 0x04) != 0) try self.pop() else Value.nil;
                const device = if ((flags & 0x02) != 0) try self.pop() else Value.nil;
                const host = if ((flags & 0x01) != 0) try self.pop() else Value.nil;

                // Allocate pathname object
                const bytes = try self.heap.allocRaw(@sizeOf(runtime.Pathname));
                const pn: *runtime.Pathname = @ptrCast(@alignCast(bytes));

                pn.* = .{
                    .kind = .pathname,
                    .host = host,
                    .device = device,
                    .directory = directory,
                    .name = name,
                    .type = type_comp,
                    .version = version,
                };

                try self.push(Value.makePathname(pn));
            },

            .pathname => {
                const pathspec = try self.pop();
                const result = try primitives.pathname.pathname(self.allocator, self.heap, pathspec);
                try self.push(result);
            },

            .parse_namestring => {
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;

                const str = str_val.toPtr(runtime.String);
                const path = str.bytes();

                // Parse the path into components
                const host = Value.nil;
                const device = Value.nil;
                var directory = Value.nil;
                var name = Value.nil;
                var type_comp = Value.nil;
                const version = Value.nil;

                if (path.len == 0) {
                    // Empty path - all components nil
                } else {
                    // Check if absolute (starts with /)
                    const is_absolute = path[0] == '/';
                    const start_idx: usize = if (is_absolute) 1 else 0;

                    // Split path by '/'
                    var components = std.ArrayList(Value){};
                    defer components.deinit(self.allocator);

                    var i: usize = start_idx;
                    while (i < path.len) {
                        var j = i;
                        while (j < path.len and path[j] != '/') : (j += 1) {}

                        if (j > i) {
                            const component = path[i..j];
                            const comp_str = try self.allocString(component);
                            try components.append(self.allocator, comp_str);
                        }

                        i = j + 1;
                    }

                    // Last component is the filename
                    if (components.items.len > 0) {
                        const filename_val = components.pop().?;
                        const filename = filename_val.toPtr(runtime.String);
                        const fname = filename.bytes();

                        // Split filename into name and type (extension)
                        if (std.mem.lastIndexOf(u8, fname, ".")) |dot_pos| {
                            if (dot_pos > 0) {
                                // Has extension
                                name = try self.allocString(fname[0..dot_pos]);
                                type_comp = try self.allocString(fname[dot_pos + 1 ..]);
                            } else {
                                // Starts with dot (hidden file on Unix)
                                name = filename_val;
                            }
                        } else {
                            // No extension
                            name = filename_val;
                        }
                    }

                    // Build directory list
                    if (components.items.len > 0 or is_absolute) {
                        // Start with :absolute or :relative keyword
                        const dir_type = if (is_absolute)
                            try self.heap.intern("absolute")
                        else
                            try self.heap.intern("relative");

                        var dir_list = Value.nil;
                        // Add components in reverse order to build list
                        var k: usize = components.items.len;
                        while (k > 0) {
                            k -= 1;
                            dir_list = try self.allocCons(components.items[k], dir_list);
                        }
                        // Add directory type at front
                        directory = try self.allocCons(dir_type, dir_list);
                    }
                }

                // Allocate pathname object
                const bytes = try self.heap.allocRaw(@sizeOf(runtime.Pathname));
                const pn: *runtime.Pathname = @ptrCast(@alignCast(bytes));

                pn.* = .{
                    .kind = .pathname,
                    .host = host,
                    .device = device,
                    .directory = directory,
                    .name = name,
                    .type = type_comp,
                    .version = version,
                };

                try self.push(Value.makePathname(pn));
            },

            .namestring => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;

                const pn = pn_val.toPtr(runtime.Pathname);

                // Build namestring from components
                var result = std.ArrayList(u8){};
                defer result.deinit(self.allocator);

                // Process directory
                if (pn.directory != Value.nil) {
                    var dir_list = pn.directory;

                    // Skip first element if it's :absolute or :relative keyword
                    if (dir_list.isCons()) {
                        const first = dir_list.toPtr(runtime.Cons).car;
                        if (first.raw == self.builtins.kw_absolute.raw) {
                            try result.append(self.allocator, '/');
                            dir_list = dir_list.toPtr(runtime.Cons).cdr;
                        } else if (first.raw == self.builtins.kw_relative.raw) {
                            dir_list = dir_list.toPtr(runtime.Cons).cdr;
                        }
                    }

                    // Add directory components
                    while (dir_list != Value.nil) {
                        if (!dir_list.isCons()) break;
                        const cons = dir_list.toPtr(runtime.Cons);
                        const component = cons.car;

                        if (component.isString()) {
                            const comp_str = component.toPtr(runtime.String);
                            try result.appendSlice(self.allocator, comp_str.bytes());
                            try result.append(self.allocator, '/');
                        }

                        dir_list = cons.cdr;
                    }
                }

                // Add name component
                if (pn.name != Value.nil and pn.name.isString()) {
                    const name_str = pn.name.toPtr(runtime.String);
                    try result.appendSlice(self.allocator, name_str.bytes());
                }

                // Add type component (extension)
                if (pn.type != Value.nil and pn.type.isString()) {
                    try result.append(self.allocator, '.');
                    const type_str = pn.type.toPtr(runtime.String);
                    try result.appendSlice(self.allocator, type_str.bytes());
                }

                // Create string from result
                const result_str = try self.allocString(result.items);
                try self.push(result_str);
            },

            .directory_namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.directoryNamestring(self.allocator, self.heap, &self.builtins, pn_val);
                try self.push(result);
            },

            .file_namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.fileNamestring(self.allocator, self.heap, pn_val);
                try self.push(result);
            },

            .host_namestring => {
                const pn_val = try self.pop();
                const result = try primitives.pathname.hostNamestring(self.allocator, self.heap, pn_val);
                try self.push(result);
            },

            .wild_pathname_p => {
                const pn_val = try self.pop();
                const result = primitives.pathname.wildPathnameP(&self.builtins, pn_val, null);
                try self.push(if (result) Value.t else Value.nil);
            },

            .merge_pathnames => {
                const default_val = try self.pop();
                const pn_val = try self.pop();

                if (!pn_val.isPathname()) return error.TypeMismatch;
                if (!default_val.isPathname()) return error.TypeMismatch;

                const pn = pn_val.toPtr(runtime.Pathname);
                const default_pn = default_val.toPtr(runtime.Pathname);

                // Create new pathname with merged components
                const bytes = try self.heap.allocRaw(@sizeOf(runtime.Pathname));
                const result: *runtime.Pathname = @ptrCast(@alignCast(bytes));

                // Fill nil components from defaults
                result.* = .{
                    .kind = .pathname,
                    .host = if (pn.host != Value.nil) pn.host else default_pn.host,
                    .device = if (pn.device != Value.nil) pn.device else default_pn.device,
                    .directory = if (pn.directory != Value.nil) pn.directory else default_pn.directory,
                    .name = if (pn.name != Value.nil) pn.name else default_pn.name,
                    .type = if (pn.type != Value.nil) pn.type else default_pn.type,
                    .version = if (pn.version != Value.nil) pn.version else default_pn.version,
                };

                try self.push(Value.makePathname(result));
            },

            .set_macro_character => {
                const non_term = try self.pop(); // &optional non-terminating-p
                const function = try self.pop();
                const char_val = try self.pop();

                if (!char_val.isCharacter()) return error.TypeMismatch;
                const char_code = char_val.toCharacter();
                if (char_code > 255) return error.TypeMismatch; // Only ASCII supported for now

                const byte: u8 = @intCast(char_code);
                const entry = runtime.Heap.ReadtableEntry{
                    .function = function,
                    .non_terminating = !non_term.isNil(),
                };
                try self.heap.readtable.put(self.allocator, byte, entry);
                try self.push(Value.nil);
            },

            .get_macro_character => {
                const char_val = try self.pop();

                if (!char_val.isCharacter()) return error.TypeMismatch;
                const char_code = char_val.toCharacter();
                if (char_code > 255) {
                    try self.push(Value.nil);
                    try self.push(Value.nil);
                } else {
                    const byte: u8 = @intCast(char_code);
                    const entry = self.heap.readtable.get(byte);
                    if (entry) |e| {
                        try self.push(e.function);
                        try self.push(if (e.non_terminating) Value.t else Value.nil);
                    } else {
                        try self.push(Value.nil);
                        try self.push(Value.nil);
                    }
                }
            },

            .set_dispatch_macro_character => {
                const function = try self.pop();
                const sub_char_val = try self.pop();
                const disp_char_val = try self.pop();

                if (!disp_char_val.isCharacter()) return error.TypeMismatch;
                if (!sub_char_val.isCharacter()) return error.TypeMismatch;

                const disp_code = disp_char_val.toCharacter();
                const sub_code = sub_char_val.toCharacter();

                if (disp_code > 255 or sub_code > 255) return error.TypeMismatch;

                const disp_byte: u8 = @intCast(disp_code);
                const sub_byte: u8 = @intCast(sub_code);

                // Get or create sub-table for dispatch character
                const gop = try self.heap.dispatch_readtable.getOrPut(self.allocator, disp_byte);
                if (!gop.found_existing) {
                    gop.value_ptr.* = .{};
                }

                // Store function in sub-table
                try gop.value_ptr.put(self.allocator, sub_byte, function);
                try self.push(Value.nil);
            },

            .get_dispatch_macro_character => {
                const sub_char_val = try self.pop();
                const disp_char_val = try self.pop();

                if (!disp_char_val.isCharacter()) return error.TypeMismatch;
                if (!sub_char_val.isCharacter()) return error.TypeMismatch;

                const disp_code = disp_char_val.toCharacter();
                const sub_code = sub_char_val.toCharacter();

                if (disp_code > 255 or sub_code > 255) {
                    try self.push(Value.nil);
                } else {
                    const disp_byte: u8 = @intCast(disp_code);
                    const sub_byte: u8 = @intCast(sub_code);

                    const sub_table = self.heap.dispatch_readtable.get(disp_byte);
                    if (sub_table) |table| {
                        const func = table.get(sub_byte);
                        try self.push(func orelse Value.nil);
                    } else {
                        try self.push(Value.nil);
                    }
                }
            },

            // Character operations
            .characterp => {
                const val = try self.pop();
                try self.push(if (val.isCharacter()) Value.t else Value.nil);
            },
            .floatp => {
                const val = try self.pop();
                try self.push(if (val.isFloat()) Value.t else Value.nil);
            },
            .listp => {
                const val = try self.pop();
                // listp: nil or cons
                try self.push(if (val == Value.nil or val.isCons()) Value.t else Value.nil);
            },
            .atom => {
                const val = try self.pop();
                // atom: not a cons (everything except cons)
                try self.push(if (!val.isCons()) Value.t else Value.nil);
            },
            .assoc => {
                const alist = try self.pop();
                const key = try self.pop();
                var curr = alist;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    // Each element should be a cons (key . value)
                    if (c.car.isCons()) {
                        const pair = c.car.toPtr(Cons);
                        if (pair.car.raw == key.raw) {
                            try self.push(c.car);
                            break;
                        }
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },
            .assoc_eql => {
                // assoc with eql test (compares numbers by value)
                const alist = try self.pop();
                const key = try self.pop();
                var curr = alist;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.isCons()) {
                        const pair = c.car.toPtr(Cons);
                        if (hashKeyEqualWithTest(pair.car, key, .eql)) {
                            try self.push(c.car);
                            break;
                        }
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },
            .assoc_equal => {
                // assoc with equal test (deep equality)
                const alist = try self.pop();
                const key = try self.pop();
                var curr = alist;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.isCons()) {
                        const pair = c.car.toPtr(Cons);
                        if (hashKeyEqualWithTest(pair.car, key, .equal)) {
                            try self.push(c.car);
                            break;
                        }
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_find => {
                // find with eql test (default) - works on lists, strings, vectors
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.findInSeq(item, seq, .eql));
            },
            .list_find_eq => {
                // find with eq test (identity) - works on lists, strings, vectors
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.findInSeq(item, seq, .eq));
            },
            .list_find_equal => {
                // find with equal test (structural) - works on lists, strings, vectors
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.findInSeq(item, seq, .equal));
            },

            .list_position => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.positionInSeq(item, seq, .eql));
            },

            .list_count => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.countInSeq(item, seq, .eql));
            },
            .list_count_eq => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.countInSeq(item, seq, .eq));
            },
            .list_count_equal => {
                const seq = try self.pop();
                const item = try self.pop();
                try self.push(try self.countInSeq(item, seq, .equal));
            },

            .list_remove => {
                // remove with eql test (default) - builds new list
                const seq = try self.pop();
                const item = try self.pop();
                var result = Value.nil;
                var tail_val = Value.nil;
                var curr = seq;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (!hashKeyEqualWithTest(c.car, item, .eql)) {
                        const new_cons = try self.allocCons(c.car, Value.nil);
                        if (tail_val.isCons()) {
                            tail_val.toPtr(Cons).cdr = new_cons;
                        } else {
                            result = new_cons;
                        }
                        tail_val = new_cons;
                    }
                    curr = c.cdr;
                }
                try self.push(result);
            },
            .list_remove_eq => {
                // remove with eq test (identity) - builds new list
                const seq = try self.pop();
                const item = try self.pop();
                var result = Value.nil;
                var tail_val = Value.nil;
                var curr = seq;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.raw != item.raw) {
                        const new_cons = try self.allocCons(c.car, Value.nil);
                        if (tail_val.isCons()) {
                            tail_val.toPtr(Cons).cdr = new_cons;
                        } else {
                            result = new_cons;
                        }
                        tail_val = new_cons;
                    }
                    curr = c.cdr;
                }
                try self.push(result);
            },
            .list_remove_equal => {
                // remove with equal test (structural) - builds new list
                const seq = try self.pop();
                const item = try self.pop();
                var result = Value.nil;
                var tail_val = Value.nil;
                var curr = seq;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (!hashKeyEqualWithTest(c.car, item, .equal)) {
                        const new_cons = try self.allocCons(c.car, Value.nil);
                        if (tail_val.isCons()) {
                            tail_val.toPtr(Cons).cdr = new_cons;
                        } else {
                            result = new_cons;
                        }
                        tail_val = new_cons;
                    }
                    curr = c.cdr;
                }
                try self.push(result);
            },

            .equal => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (valueEqual(a, b)) Value.t else Value.nil);
            },
            .eql => {
                const b = try self.pop();
                const a = try self.pop();
                // eql: eq for most types, but numeric equality for numbers
                // Floats need special handling: 0.0 == -0.0, NaN != NaN
                if (a.isFloat() and b.isFloat()) {
                    try self.push(if (floatEql(a.toFloat(), b.toFloat())) Value.t else Value.nil);
                } else {
                    try self.push(if (a.eq(b)) Value.t else Value.nil);
                }
            },
            .char_code => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                try self.push(Value.makeFixnum(@intCast(cp)));
            },
            .code_char => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                const n = val.toFixnum();
                if (n < 0 or n > 0x10FFFF) return error.InvalidArgument;
                try self.push(Value.makeCharacter(@intCast(@as(u64, @bitCast(n)))));
            },
            .char_eq => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
                try self.push(if (a.raw == b.raw) Value.t else Value.nil);
            },
            .char_lt => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
                try self.push(if (a.toCharacter() < b.toCharacter()) Value.t else Value.nil);
            },
            .char_gt => {
                const b = try self.pop();
                const a = try self.pop();
                if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
                try self.push(if (a.toCharacter() > b.toCharacter()) Value.t else Value.nil);
            },

            .read_char => {
                const ch = try io.sysReadChar();
                if (ch < 0) {
                    try self.push(Value.nil);
                } else {
                    try self.push(Value.makeCharacter(@intCast(ch)));
                }
            },
            .peek_char => {
                const ch = try io.sysPeekChar();
                if (ch < 0) {
                    try self.push(Value.nil);
                } else {
                    try self.push(Value.makeCharacter(@intCast(ch)));
                }
            },

            .read => {
                // Read a complete S-expression from stdin
                var buffer: [4096]u8 = undefined;
                const len = try io.sysReadSexp(&buffer);
                if (len == 0) {
                    try self.push(Value.nil);
                    return;
                }

                // Parse the S-expression
                var parser = try Parser.init(self.allocator, self.heap, buffer[0..len], &self.builtins);
                const result = try parser.parse();
                try self.push(result);
            },

            .load => {
                // Load and evaluate a file
                const filename_val = try self.pop();
                if (!filename_val.isString()) return error.TypeMismatch;

                const str = filename_val.toPtr(String);
                const filename = str.bytes();

                // Call the load callback if set
                if (self.load_callback) |callback| {
                    const result = try callback(filename, self.load_context.?);
                    try self.push(result);
                } else {
                    // No callback set - return nil
                    try self.push(Value.nil);
                }
            },

            .read_from_string => {
                // Parse a string into a Lisp value
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;

                const str = str_val.toPtr(String);
                var parser = try Parser.init(self.allocator, self.heap, str.bytes(), &self.builtins);
                defer parser.deinit();
                const result = try parser.parse();
                try self.push(result);
            },

            .eval => {
                // Evaluate expression at runtime
                const expr = try self.pop();

                // Call the eval callback if set
                if (self.eval_callback) |callback| {
                    const result = try callback(expr, self.eval_context.?);
                    try self.push(result);
                } else {
                    // No callback set - return nil
                    try self.push(Value.nil);
                }
            },

            .gensym => {
                // Pop prefix argument (nil if nullary)
                const prefix_arg = try self.pop();
                const sym = try primitives.symbol.gensym(self.heap, if (!prefix_arg.isNil()) prefix_arg else null);
                try self.push(sym);
            },

            .macroexpand => {
                // Expand macros fully
                const expr = try self.pop();

                // Call the macroexpand callback if set
                if (self.macroexpand_callback) |callback| {
                    const result = try callback(expr, self.macroexpand_context.?);
                    try self.push(result);
                } else {
                    // No callback set - return the expression unchanged
                    try self.push(expr);
                }
            },

            .macroexpand_1 => {
                // Expand macros once
                const expr = try self.pop();

                // Call the macroexpand-1 callback if set
                if (self.macroexpand_1_callback) |callback| {
                    const result = try callback(expr, self.macroexpand_1_context.?);
                    try self.push(result);
                } else {
                    // No callback set - return the expression unchanged
                    try self.push(expr);
                }
            },

            .unread_char => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                io.sysUnreadChar(@intCast(val.toCharacter()));
                try self.push(Value.nil);
            },

            .listen => {
                const stream_val = try self.pop();
                // Accept nil or a stream
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                if (stream_val.isNil()) {
                    try self.push(Value.nil);
                } else {
                    const result = try io.listen(stream_val);
                    try self.push(result);
                }
            },

            .upgraded_complex_part_type => {
                _ = try self.pop(); // typespec (ignored)
                // Our complex numbers use double-float for both parts
                try self.push(try self.heap.intern("real"));
            },

            .file_string_length => {
                const string_val = try self.pop();
                const stream_val = try self.pop();
                // Accept nil or stream for first arg
                if (!stream_val.isNil() and !stream_val.isStream()) return error.TypeMismatch;
                if (!string_val.isString()) return error.TypeMismatch;
                const str = string_val.toPtr(runtime.String);
                // Return byte length of string (assuming 1 byte per char for external format)
                try self.push(Value.makeFixnum(@intCast(str.bytes().len)));
            },

            .boundp => {
                const val = try self.pop();
                if (!val.isSymbol()) return error.TypeMismatch;
                const sym = val.toPtr(Symbol);
                const local_name = sym.getName();
                // Build qualified name using symbol's package
                var qual_buf: [512]u8 = undefined;
                const q = try qual_name.qualSym(self.allocator, sym, &qual_buf);
                defer if (q.owned) self.allocator.free(q.name);
                const qname = q.name;
                // Check if symbol exists in global environment and is not unbound
                const is_bound = if (self.global_env) |env| blk: {
                    const idx = env.lookup(qname) orelse env.lookup(local_name);
                    if (idx) |i| {
                        if (i < MAX_GLOBALS) {
                            // Check if value is unbound marker
                            break :blk self.globals[i].raw != Value.unbound.raw;
                        }
                    }
                    break :blk false;
                } else false;
                try self.push(if (is_bound) Value.t else Value.nil);
            },

            .fboundp => {
                const val = try self.pop();
                if (!val.isSymbol()) return error.TypeMismatch;
                // Use callback to check for function binding (macro, primitive, or defun)
                const is_fbound = if (self.fboundp_callback) |cb|
                    try cb(val, self.fboundp_context.?)
                else if (self.global_env) |env| blk: {
                    const sym = val.toPtr(Symbol);
                    const local_name = sym.getName();
                    // Build qualified name using symbol's package
                    var qbuf: [512]u8 = undefined;
                    const q = try qual_name.qualSym(self.allocator, sym, &qbuf);
                    defer if (q.owned) self.allocator.free(q.name);
                    const qname = q.name;
                    // Check qualified name first, then local name
                    break :blk (env.lookup(qname) orelse env.lookup(local_name)) != null;
                } else false;
                try self.push(if (is_fbound) Value.t else Value.nil);
            },

            .symbol_value, .symbol_function => {
                const val = try self.pop();
                // Handle magic symbols nil and t
                switch (val.typeKind()) {
                    .nil => try self.push(Value.nil),
                    .t => try self.push(Value.t),
                    .symbol => {
                        const sym = val.toPtr(Symbol);
                        const local_name = sym.getName();
                        // Build qualified name using symbol's package
                        var qual_buf: [512]u8 = undefined;
                        const q = try qual_name.qualSym(self.allocator, sym, &qual_buf);
                        defer if (q.owned) self.allocator.free(q.name);
                        const qname = q.name;
                        // Look up symbol in global environment
                        if (self.global_env) |env| {
                            // Try qualified name first, then local name
                            const idx = env.lookup(qname) orelse env.lookup(local_name);
                            if (idx) |i| {
                                try self.push(self.globals[i]);
                            } else {
                                return error.UnboundSymbol;
                            }
                        } else {
                            return error.UnboundSymbol;
                        }
                    },
                    else => return error.TypeMismatch,
                }
            },

            .typep => {
                const type_spec = try self.pop();
                const obj = try self.pop();
                const result = try primitives.typep(self.heap, &self.type_syms, obj, type_spec);
                try self.push(if (result) Value.t else Value.nil);
            },

            .subtypep => {
                const type2 = try self.pop();
                const type1 = try self.pop();
                const result = try primitives.subtypep(self.heap, type1, type2);
                try self.push(result);
            },

            // Numeric predicates
            .abs => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                const n = val.toFixnum();
                // abs(minInt) overflows
                if (n == std.math.minInt(i64)) return error.TypeMismatch;
                try self.push(Value.makeFixnum(if (n < 0) -n else n));
            },
            .zerop => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (val.toFixnum() == 0) Value.t else Value.nil);
            },
            .plusp => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (val.toFixnum() > 0) Value.t else Value.nil);
            },
            .minusp => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (val.toFixnum() < 0) Value.t else Value.nil);
            },
            .evenp => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (@mod(val.toFixnum(), 2) == 0) Value.t else Value.nil);
            },
            .oddp => {
                const val = try self.pop();
                if (!val.isFixnum()) return error.TypeMismatch;
                try self.push(if (@mod(val.toFixnum(), 2) != 0) Value.t else Value.nil);
            },

            // Math functions
            .sqrt => {
                const val = try self.pop();
                const f = try valToFloat(val);
                if (f < 0) {
                    const cplx = try self.heap.allocComplex(0.0, @sqrt(-f));
                    try self.push(cplx);
                } else {
                    try self.push(Value.makeFloat(@sqrt(f)));
                }
            },
            .sin => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@sin(f)));
            },
            .cos => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@cos(f)));
            },
            .tan => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@tan(f)));
            },
            .exp => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@exp(f)));
            },
            .log => {
                const val = try self.pop();
                const f = try valToFloat(val);
                try self.push(Value.makeFloat(@log(f)));
            },
            .floor => {
                const val = try self.pop();
                const f = try valToFloat(val);
                const floored: i64 = @intFromFloat(@floor(f));
                try self.push(Value.makeFixnum(floored));
            },
            .ceiling => {
                const val = try self.pop();
                const f = try valToFloat(val);
                const ceiled: i64 = @intFromFloat(@ceil(f));
                try self.push(Value.makeFixnum(ceiled));
            },
            .round => {
                const val = try self.pop();
                const f = try valToFloat(val);
                const rounded: i64 = @intFromFloat(@round(f));
                try self.push(Value.makeFixnum(rounded));
            },

            .halt => return error.Halt,
        }

        // Clear stale secondary values after each op (except ops that set them)
        if (op != .values and op != .get_decoded_time and op != .decode_universal_time) {
            self.secondary_values_count = 0;
        }
    }

    // ========================================================================
    // Exception handling
    // ========================================================================

    fn doThrow(self: *Vm, tag: Value, value: Value) Error!void {
        // First, check if there's an unwind-protect that needs cleanup
        // Unwind frames take precedence - we must run cleanup before continuing
        if (self.unwind_sp > 0) {
            // Pop the unwind frame
            self.unwind_sp -= 1;
            const unwind_frame = self.unwind_stack[self.unwind_sp];

            // Save throw state for after cleanup
            self.pending_throw_tag = tag;
            self.pending_throw_value = value;
            self.is_unwinding = true;

            // Jump to cleanup code with saved stack/frame state
            self.chunk = unwind_frame.chunk;
            self.ip = unwind_frame.cleanup_ip;
            // Restore sp/fp to the state when push_unwind was executed
            // so cleanup runs with the correct stack context
            // Validate before restore to guard against corruption
            if (unwind_frame.unwind_sp > STACK_SIZE or unwind_frame.unwind_fp > MAX_FRAMES) {
                return error.InvalidOpcode;
            }
            self.sp = unwind_frame.unwind_sp;
            self.fp = unwind_frame.unwind_fp;
            // pop_unwind will re-throw after cleanup completes
            return;
        }

        // No unwind frames - search for matching catch frame
        while (self.catch_sp > 0) {
            self.catch_sp -= 1;
            const frame = self.catch_stack[self.catch_sp];

            // Check if tag matches (using eq comparison)
            if (tag.raw == frame.tag.raw) {
                // Found matching catch - restore state and jump
                // Validate before restore to guard against corruption
                if (frame.catch_sp > STACK_SIZE or frame.catch_fp > MAX_FRAMES) {
                    return error.InvalidOpcode;
                }
                self.chunk = frame.chunk;
                self.ip = frame.catch_ip;
                self.sp = frame.catch_sp;
                self.fp = frame.catch_fp;
                // Push the thrown value as result
                try self.push(value);
                return;
            }
        }
        // No matching catch found
        return error.UnhandledThrow;
    }

    /// Handle return-from by searching for matching block frame and jumping to it
    fn pushProgvFrame(self: *Vm, symbols: Value, values: Value) Error!void {
        if (self.progv_sp >= MAX_PROGVS) return error.StackOverflow;

        // Build list of (symbol . old-value) pairs
        var saved_bindings = Value.nil;
        var sym_list = symbols;
        var val_list = values;

        while (sym_list.isCons()) {
            const sym_cons = sym_list.toPtr(Cons);
            const symbol = sym_cons.car;

            if (!symbol.isSymbol()) return error.TypeMismatch;

            // Get old value (or nil if unbound)
            const sym_ptr = symbol.toPtr(Symbol);
            const sym_name = sym_ptr.getName();
            const global_idx = if (self.global_env) |env| env.lookup(sym_name) else null;
            const old_value = if (global_idx) |idx| blk: {
                if (idx < self.num_globals) break :blk self.globals[idx] else break :blk Value.nil;
            } else Value.nil;

            // Set new value
            const new_value = if (val_list.isCons()) val_list.toPtr(Cons).car else Value.nil;
            if (global_idx) |idx| {
                if (idx < MAX_GLOBALS) {
                    self.globals[idx] = new_value;
                    if (idx >= self.num_globals) {
                        self.num_globals = idx + 1;
                    }
                }
            }

            // Save (symbol . old-value) pair
            const pair = try self.heap.allocCons(symbol, old_value);
            saved_bindings = try self.heap.allocCons(pair, saved_bindings);

            // Advance lists
            sym_list = sym_cons.cdr;
            if (val_list.isCons()) {
                val_list = val_list.toPtr(Cons).cdr;
            }
        }

        self.progv_stack[self.progv_sp] = .{ .saved_bindings = saved_bindings };
        self.progv_sp += 1;
    }

    fn popProgvFrame(self: *Vm) Error!void {
        if (self.progv_sp == 0) return error.StackUnderflow;
        self.progv_sp -= 1;

        const frame = self.progv_stack[self.progv_sp];
        var bindings = frame.saved_bindings;

        // Restore old values
        while (bindings.isCons()) {
            const binding_cons = bindings.toPtr(Cons);
            const pair = binding_cons.car;

            if (pair.isCons()) {
                const pair_cons = pair.toPtr(Cons);
                const symbol = pair_cons.car;
                const old_value = pair_cons.cdr;
                const sym_ptr = symbol.toPtr(Symbol);
                const sym_name = sym_ptr.getName();
                if (self.global_env) |env| {
                    if (env.lookup(sym_name)) |idx| {
                        if (idx < MAX_GLOBALS) {
                            self.globals[idx] = old_value;
                        }
                    }
                }
            }

            bindings = binding_cons.cdr;
        }
    }

    fn doReturnFrom(self: *Vm, name_raw: Value, value: Value) Error!void {
        // First, check if there's an unwind-protect that needs cleanup
        // Unwind frames take precedence - we must run cleanup before continuing
        if (self.unwind_sp > 0) {
            // Pop the unwind frame
            self.unwind_sp -= 1;
            const unwind_frame = self.unwind_stack[self.unwind_sp];

            // Save return-from state for after cleanup
            self.pending_block_name = name_raw;
            self.pending_block_value = value;
            self.is_returning_from_block = true;

            // Jump to cleanup code with saved stack/frame state
            self.chunk = unwind_frame.chunk;
            self.ip = unwind_frame.cleanup_ip;
            if (unwind_frame.unwind_sp > STACK_SIZE or unwind_frame.unwind_fp > MAX_FRAMES) {
                return error.InvalidOpcode;
            }
            self.sp = unwind_frame.unwind_sp;
            self.fp = unwind_frame.unwind_fp;
            // pop_unwind will re-invoke return-from after cleanup completes
            return;
        }

        // No unwind frames - search for matching block frame
        while (self.block_sp > 0) {
            self.block_sp -= 1;
            const frame = self.block_stack[self.block_sp];

            // Check if name matches (using raw value identity)
            if (name_raw == frame.name_raw) {
                // Found matching block - restore state and jump
                if (frame.block_sp > STACK_SIZE or frame.block_fp > MAX_FRAMES) {
                    return error.InvalidOpcode;
                }
                self.chunk = frame.chunk;
                self.ip = frame.exit_ip;
                self.sp = frame.block_sp;
                self.fp = frame.block_fp;
                // Push the return value as result
                try self.push(value);
                return;
            }
        }
        // No matching block found
        return error.NoMatchingBlock;
    }

    /// Handle an error by running unwind-protect cleanup if needed
    fn doError(self: *Vm, err: anyerror) Error {
        // Check if there's an unwind-protect that needs cleanup
        if (self.unwind_sp > 0) {
            // Pop the unwind frame
            self.unwind_sp -= 1;
            const unwind_frame = self.unwind_stack[self.unwind_sp];

            // Save error for after cleanup
            self.pending_error = err;
            self.is_unwinding = true;

            // Jump to cleanup code with saved stack/frame state
            self.chunk = unwind_frame.chunk;
            self.ip = unwind_frame.cleanup_ip;

            // Validate before restore to guard against corruption
            if (unwind_frame.unwind_sp > STACK_SIZE or unwind_frame.unwind_fp > MAX_FRAMES) {
                return error.InvalidOpcode;
            }
            self.sp = unwind_frame.unwind_sp;
            self.fp = unwind_frame.unwind_fp;

            // Return appropriate error
            return self.mapError(err);
        }

        // No unwind frames - propagate error normally
        return self.mapError(err);
    }

    fn mapError(_: *Vm, err: anyerror) Error {
        return err;
    }

    fn doInvokeRestart(self: *Vm, name: Value, value: Value) Error!void {
        // Search for restart by name (most recent first)
        var i = self.restart_sp;
        while (i > 0) {
            i -= 1;
            const frame = self.restart_stack[i];
            if (frame.name.raw == name.raw) {
                // Found matching restart - restore state
                // First, restore catch/unwind stack depths
                self.catch_sp = frame.catch_depth;
                self.unwind_sp = frame.unwind_depth;
                // Pop this restart and all more recent ones
                self.restart_sp = i;

                // Restore execution state
                if (frame.restart_sp > STACK_SIZE or frame.restart_fp > MAX_FRAMES) {
                    return error.InvalidOpcode;
                }
                self.chunk = frame.chunk;
                self.ip = frame.handler_ip;
                self.sp = frame.restart_sp;
                self.fp = frame.restart_fp;
                // Push the value as result of restart handler
                try self.push(value);
                return;
            }
        }
        // No matching restart found
        return error.RestartNotFound;
    }

    // ========================================================================
    // Handler-bind support
    // ========================================================================

    fn doHandlerBind(self: *Vm, body_fn: Value, handlers_alist: Value) Error!void {
        // Push handlers onto handler stack
        const depth_before = self.handler_sp;

        // Walk alist of (condition-type . handler-fn) pairs
        var curr = handlers_alist;
        while (!curr.isNil()) {
            if (!curr.isCons()) return error.TypeMismatch;
            const pair = curr.toPtr(runtime.Cons);
            const car = pair.car;

            if (!car.isCons()) return error.TypeMismatch;
            const binding = car.toPtr(runtime.Cons);
            const condition_type = binding.car;
            const handler_fn = binding.cdr;

            if (self.handler_sp >= MAX_HANDLERS) {
                return error.StackOverflow;
            }

            self.handler_stack[self.handler_sp] = HandlerFrame{
                .condition_type = condition_type,
                .handler_fn = handler_fn,
            };
            self.handler_sp += 1;

            curr = pair.cdr;
        }

        // Call body function
        if (!body_fn.isClosure()) return error.TypeMismatch;
        const closure = body_fn.toPtr(runtime.Closure);
        _ = try self.callClosure(closure, 0);

        // Pop handlers
        self.handler_sp = depth_before;
    }

    // ========================================================================
    // Format string support
    // ========================================================================

    fn doFormat(self: *Vm, dest: Value, control: Value, args: []const Value) Error!Value {
        const control_str = control.toPtr(runtime.String);
        const fmt = control_str.bytes();

        // Build result string
        var result = std.ArrayList(u8){};
        defer result.deinit(self.allocator);

        var arg_idx: usize = 0;
        var i: usize = 0;

        while (i < fmt.len) {
            if (fmt[i] == '~' and i + 1 < fmt.len) {
                // Scan ahead to find directive char (might have params/modifiers before it)
                var scan_idx = i + 1;
                while (scan_idx < fmt.len) {
                    const ch = fmt[scan_idx];
                    if (ch >= '0' and ch <= '9') {
                        scan_idx += 1;
                    } else if (ch == ',') {
                        scan_idx += 1;
                    } else if (ch == '\'' and scan_idx + 1 < fmt.len) {
                        scan_idx += 2; // Skip quote and next char
                    } else if (ch == ':' or ch == '@') {
                        scan_idx += 1;
                    } else {
                        break;
                    }
                }
                const directive = if (scan_idx < fmt.len) fmt[scan_idx] else fmt[i + 1];
                switch (directive) {
                    'A', 'a' => {
                        // Aesthetic - print without quotes
                        if (arg_idx < args.len) {
                            try self.formatValueAesthetic(args[arg_idx], &result);
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'S', 's' => {
                        // Standard - print with quotes for strings
                        if (arg_idx < args.len) {
                            try self.formatValueStandard(args[arg_idx], &result);
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'D', 'd' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 10, &result);
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'X', 'x' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 16, &result);
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'B', 'b' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 2, &result);
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'O', 'o' => {
                        if (arg_idx < args.len) {
                            try self.formatFixnumBase(args[arg_idx], 8, &result);
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'C', 'c' => {
                        // Character
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isCharacter()) {
                                const cp = val.toCharacter();
                                if (cp < 128) {
                                    try result.append(self.allocator, @intCast(cp));
                                } else {
                                    // UTF-8 encode
                                    var buf: [4]u8 = undefined;
                                    const len = try std.unicode.utf8Encode(cp, &buf);
                                    try result.appendSlice(self.allocator, buf[0..len]);
                                }
                            }
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    '%' => {
                        // Newline
                        try result.append(self.allocator, '\n');
                        i += 2;
                    },
                    '&' => {
                        // Fresh line - newline only if not at start of line
                        if (result.items.len > 0 and result.items[result.items.len - 1] != '\n') {
                            try result.append(self.allocator, '\n');
                        }
                        i += 2;
                    },
                    'T', 't' => {
                        // Tabulate - insert spaces to reach column
                        // Parse optional parameters: ~mincolT or ~mincol,colincT
                        var mincol: usize = 1; // Default: tab to next column
                        var colinc: usize = 1; // Default: increment by 1

                        const j = i + 2;
                        // Check for numeric parameters before T
                        var param_start = i + 1;
                        while (param_start > 0 and fmt[param_start - 1] >= '0' and fmt[param_start - 1] <= '9') {
                            param_start -= 1;
                        }

                        if (param_start < i + 1) {
                            // Parse mincol
                            const param_str = fmt[param_start .. i + 1];
                            if (std.mem.indexOf(u8, param_str, ",")) |comma_pos| {
                                // Both mincol and colinc
                                if (comma_pos > 0) {
                                    mincol = try std.fmt.parseInt(usize, param_str[0..comma_pos], 10);
                                }
                                if (comma_pos + 1 < param_str.len) {
                                    colinc = try std.fmt.parseInt(usize, param_str[comma_pos + 1 ..], 10);
                                }
                            } else {
                                // Just mincol
                                if (param_str.len > 0) {
                                    mincol = try std.fmt.parseInt(usize, param_str, 10);
                                }
                            }
                        }

                        // Calculate current column position (chars since last newline)
                        var col: usize = 0;
                        var k = result.items.len;
                        while (k > 0) {
                            k -= 1;
                            if (result.items[k] == '\n') break;
                            col += 1;
                        }

                        // Insert spaces to reach target column
                        var target_col = mincol;
                        if (col < mincol) {
                            target_col = mincol;
                        } else {
                            // Round up to next multiple of colinc
                            target_col = col + colinc - ((col - mincol) % colinc);
                        }

                        const spaces_needed = if (target_col > col) target_col - col else 0;
                        var space_idx: usize = 0;
                        while (space_idx < spaces_needed) : (space_idx += 1) {
                            try result.append(self.allocator, ' ');
                        }

                        i = j;
                    },
                    '~' => {
                        // Literal tilde
                        try result.append(self.allocator, '~');
                        i += 2;
                    },
                    '*' => {
                        // Argument navigation
                        // Parse optional count parameter: ~n*
                        var skip_count: usize = 1; // Default: skip 1 arg

                        // Look backwards for numeric parameter
                        var param_start = i + 1;
                        while (param_start > 0 and fmt[param_start - 1] >= '0' and fmt[param_start - 1] <= '9') {
                            param_start -= 1;
                        }

                        if (param_start < i + 1) {
                            const param_str = fmt[param_start .. i + 1];
                            if (param_str.len > 0) {
                                skip_count = try std.fmt.parseInt(usize, param_str, 10);
                            }
                        }

                        // Skip forward
                        arg_idx += skip_count;
                        // Bounds check not needed - just clamps at end
                        if (arg_idx > args.len) {
                            arg_idx = args.len;
                        }

                        i += 2;
                    },
                    'P', 'p' => {
                        // Plural - print 's' if arg != 1, else nothing
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            var should_plural = false;

                            if (val.isFixnum()) {
                                const n = val.toFixnum();
                                should_plural = (n != 1);
                            } else {
                                // Non-numbers are considered plural
                                should_plural = true;
                            }

                            if (should_plural) {
                                try result.append(self.allocator, 's');
                            }

                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    '(' => {
                        // Case conversion: ~(...~)
                        // Find matching ~)
                        const start = i + 2;
                        var depth: usize = 1;
                        var end = start;
                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '(') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == ')') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }
                        if (depth != 0) {
                            // Unmatched ~(, skip it
                            i += 2;
                            continue;
                        }

                        // Process the body and apply downcase
                        const body = fmt[start..end];
                        const body_start = result.items.len;

                        // Recursively format the body
                        var j: usize = 0;
                        while (j < body.len) {
                            if (body[j] == '~' and j + 1 < body.len) {
                                // Handle nested directives (simplified - just copy for now)
                                try result.append(self.allocator, body[j]);
                                j += 1;
                            } else {
                                try result.append(self.allocator, body[j]);
                                j += 1;
                            }
                        }

                        // Apply downcase to the added segment
                        const segment = result.items[body_start..];
                        for (segment) |*c| {
                            if (c.* >= 'A' and c.* <= 'Z') {
                                c.* = c.* + ('a' - 'A');
                            }
                        }

                        i = end + 2; // Skip past ~)
                    },
                    ')' => {
                        // End of case conversion - should not be reached at top level
                        i += 2;
                    },
                    '{' => {
                        // Iteration: ~{...~} processes a list
                        // Find matching ~}
                        const start = i + 2;
                        var depth: usize = 1;
                        var end = start;
                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '{') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == '}') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }
                        if (depth != 0) {
                            // Unmatched ~{, skip it
                            i += 2;
                            continue;
                        }
                        const body = fmt[start..end];
                        // Get list argument
                        if (arg_idx < args.len) {
                            const list_arg = args[arg_idx];
                            arg_idx += 1;
                            // Iterate over list
                            try self.formatIteration(list_arg, body, &result);
                        }
                        i = end + 2; // Skip past ~}
                    },
                    '}' => {
                        // End of iteration - should not be reached at top level
                        i += 2;
                    },
                    '^' => {
                        // Escape from iteration - only valid inside ~{...~}
                        // At top level, just skip it
                        i += 2;
                    },
                    ':' => {
                        // Check for ~:[ (boolean conditional)
                        if (i + 2 < fmt.len and fmt[i + 2] == '[') {
                            // Handle as boolean conditional - parse same as ~[ but interpret as nil/non-nil
                            const start = i + 3;
                            var depth: usize = 1;
                            var end = start;
                            while (end < fmt.len and depth > 0) {
                                if (end + 1 < fmt.len and fmt[end] == '~') {
                                    if (fmt[end + 1] == '[') {
                                        depth += 1;
                                        end += 2;
                                    } else if (fmt[end + 1] == ']') {
                                        depth -= 1;
                                        if (depth == 0) break;
                                        end += 2;
                                    } else {
                                        end += 1;
                                    }
                                } else {
                                    end += 1;
                                }
                            }
                            if (depth != 0) {
                                i += 3;
                                continue;
                            }
                            const body = fmt[start..end];
                            // Split into exactly 2 clauses by ~;
                            var clauses = std.ArrayList([]const u8){};
                            defer clauses.deinit(self.allocator);
                            var clause_start: usize = 0;
                            var j: usize = 0;
                            var clause_depth: usize = 0;
                            while (j < body.len) {
                                if (j + 1 < body.len and body[j] == '~') {
                                    if (body[j + 1] == '[') {
                                        clause_depth += 1;
                                        j += 2;
                                    } else if (body[j + 1] == ']') {
                                        if (clause_depth > 0) clause_depth -= 1;
                                        j += 2;
                                    } else if (body[j + 1] == ';' and clause_depth == 0) {
                                        try clauses.append(self.allocator, body[clause_start..j]);
                                        clause_start = j + 2;
                                        j += 2;
                                    } else {
                                        j += 1;
                                    }
                                } else {
                                    j += 1;
                                }
                            }
                            try clauses.append(self.allocator, body[clause_start..]);
                            // Get selector for boolean conditional
                            if (arg_idx < args.len) {
                                const selector = args[arg_idx];
                                arg_idx += 1;
                                // nil = clause 0, non-nil = clause 1
                                const clause_idx: usize = if (selector.isNil()) 0 else 1;
                                if (clause_idx < clauses.items.len) {
                                    try result.appendSlice(self.allocator, clauses.items[clause_idx]);
                                }
                            }
                            i = end + 2;
                        } else {
                            // Unknown :X directive, skip
                            i += 2;
                        }
                    },
                    '[' => {
                        // Conditional: ~[clause0~;clause1~;...~] or ~:[false~;true~]
                        // Find matching ~]
                        const start = i + 2;
                        var depth: usize = 1;
                        var end = start;
                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '[') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == ']') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }
                        if (depth != 0) {
                            i += 2;
                            continue;
                        }
                        const body = fmt[start..end];
                        // Split clauses by ~;
                        var clauses = std.ArrayList([]const u8){};
                        defer clauses.deinit(self.allocator);
                        var clause_start: usize = 0;
                        var j: usize = 0;
                        var clause_depth: usize = 0;
                        while (j < body.len) {
                            if (j + 1 < body.len and body[j] == '~') {
                                if (body[j + 1] == '[') {
                                    clause_depth += 1;
                                    j += 2;
                                } else if (body[j + 1] == ']') {
                                    if (clause_depth > 0) clause_depth -= 1;
                                    j += 2;
                                } else if (body[j + 1] == ';' and clause_depth == 0) {
                                    try clauses.append(self.allocator, body[clause_start..j]);
                                    clause_start = j + 2;
                                    j += 2;
                                } else {
                                    j += 1;
                                }
                            } else {
                                j += 1;
                            }
                        }
                        try clauses.append(self.allocator, body[clause_start..]);
                        // Get selector
                        if (arg_idx < args.len) {
                            const selector = args[arg_idx];
                            arg_idx += 1;
                            var clause_idx: usize = 0;
                            switch (selector.typeKind()) {
                                .fixnum => {
                                    const n = selector.toFixnum();
                                    if (n >= 0) clause_idx = @intCast(n);
                                },
                                .nil => {
                                    clause_idx = 0; // For ~:[false~;true~], nil selects first
                                },
                                else => {
                                    clause_idx = 1; // Non-nil selects second (for boolean conditional)
                                },
                            }
                            if (clause_idx < clauses.items.len) {
                                // Append the selected clause text directly
                                // (for full CL compat, would need recursive format processing)
                                try result.appendSlice(self.allocator, clauses.items[clause_idx]);
                            }
                        }
                        i = end + 2;
                    },
                    ']' => {
                        // End of conditional - should not be reached at top level
                        i += 2;
                    },
                    ';' => {
                        // Clause separator - should not be reached at top level
                        i += 2;
                    },
                    '<' => {
                        // Justification: ~mincol,colinc,minpad,'padchar:@<...~>
                        var mincol: usize = 0;
                        var colinc: usize = 1;
                        var minpad: usize = 0;
                        var padchar: u8 = ' ';
                        var colon = false;
                        var at = false;

                        const param_str = fmt[i + 1 .. scan_idx];
                        var pidx: usize = 0;
                        var param_num: usize = 0;

                        while (pidx < param_str.len) {
                            const pch = param_str[pidx];
                            if (pch == ':') {
                                colon = true;
                                pidx += 1;
                            } else if (pch == '@') {
                                at = true;
                                pidx += 1;
                            } else if (pch >= '0' and pch <= '9') {
                                var num: usize = 0;
                                while (pidx < param_str.len and param_str[pidx] >= '0' and param_str[pidx] <= '9') {
                                    num = num * 10 + (param_str[pidx] - '0');
                                    pidx += 1;
                                }
                                if (param_num == 0) {
                                    mincol = num;
                                } else if (param_num == 1) {
                                    colinc = num;
                                } else if (param_num == 2) {
                                    minpad = num;
                                }
                                param_num += 1;

                                if (pidx < param_str.len and param_str[pidx] == ',') {
                                    pidx += 1;
                                }
                            } else if (pch == '\'') {
                                pidx += 1;
                                if (pidx < param_str.len) {
                                    padchar = param_str[pidx];
                                    pidx += 1;
                                    param_num += 1;
                                    if (pidx < param_str.len and param_str[pidx] == ',') {
                                        pidx += 1;
                                    }
                                }
                            } else if (pch == ',') {
                                param_num += 1;
                                pidx += 1;
                            } else {
                                pidx += 1;
                            }
                        }

                        // Find matching ~>
                        const start = scan_idx + 1;
                        var depth: usize = 1;
                        var end = start;

                        while (end < fmt.len and depth > 0) {
                            if (end + 1 < fmt.len and fmt[end] == '~') {
                                if (fmt[end + 1] == '<') {
                                    depth += 1;
                                    end += 2;
                                } else if (fmt[end + 1] == '>') {
                                    depth -= 1;
                                    if (depth == 0) break;
                                    end += 2;
                                } else {
                                    end += 1;
                                }
                            } else {
                                end += 1;
                            }
                        }

                        if (depth != 0) {
                            i += 2;
                            continue;
                        }

                        const body = fmt[start..end];

                        // Split into segments by ~;
                        var segments = std.ArrayList([]const u8){};
                        defer segments.deinit(self.allocator);
                        var seg_start: usize = 0;
                        var j: usize = 0;
                        var seg_depth: usize = 0;

                        while (j < body.len) {
                            if (j + 1 < body.len and body[j] == '~') {
                                if (body[j + 1] == '<') {
                                    seg_depth += 1;
                                    j += 2;
                                } else if (body[j + 1] == '>') {
                                    if (seg_depth > 0) seg_depth -= 1;
                                    j += 2;
                                } else if (body[j + 1] == ';' and seg_depth == 0) {
                                    try segments.append(self.allocator, body[seg_start..j]);
                                    seg_start = j + 2;
                                    j += 2;
                                } else {
                                    j += 1;
                                }
                            } else {
                                j += 1;
                            }
                        }
                        try segments.append(self.allocator, body[seg_start..]);

                        // Process segments recursively
                        var seg_texts = std.ArrayList([]const u8){};
                        defer {
                            for (seg_texts.items) |s| self.allocator.free(s);
                            seg_texts.deinit(self.allocator);
                        }

                        var total_len: usize = 0;
                        for (segments.items) |seg| {
                            var seg_result = std.ArrayList(u8){};
                            defer seg_result.deinit(self.allocator);

                            var seg_idx: usize = 0;
                            while (seg_idx < seg.len) {
                                if (seg[seg_idx] == '~' and seg_idx + 1 < seg.len) {
                                    const dir = seg[seg_idx + 1];
                                    switch (dir) {
                                        'A', 'a' => {
                                            if (arg_idx < args.len) {
                                                try self.formatValueAesthetic(args[arg_idx], &seg_result);
                                                arg_idx += 1;
                                            }
                                            seg_idx += 2;
                                        },
                                        'S', 's' => {
                                            if (arg_idx < args.len) {
                                                try self.formatValueStandard(args[arg_idx], &seg_result);
                                                arg_idx += 1;
                                            }
                                            seg_idx += 2;
                                        },
                                        'D', 'd' => {
                                            if (arg_idx < args.len) {
                                                const val = args[arg_idx];
                                                if (val.isFixnum()) {
                                                    var buf: [32]u8 = undefined;
                                                    const num_str = try std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()});
                                                    try seg_result.appendSlice(self.allocator, num_str);
                                                }
                                                arg_idx += 1;
                                            }
                                            seg_idx += 2;
                                        },
                                        else => {
                                            try seg_result.append(self.allocator, seg[seg_idx]);
                                            seg_idx += 1;
                                        },
                                    }
                                } else {
                                    try seg_result.append(self.allocator, seg[seg_idx]);
                                    seg_idx += 1;
                                }
                            }

                            const owned = try self.allocator.dupe(u8, seg_result.items);
                            try seg_texts.append(self.allocator, owned);
                            total_len += owned.len;
                        }

                        // Calculate width with colinc
                        const base_width = total_len + minpad * @max(1, seg_texts.items.len - 1);
                        var width = mincol;
                        if (base_width > mincol) {
                            const k = (base_width - mincol + colinc - 1) / colinc;
                            width = mincol + k * colinc;
                        }

                        const pad_total = if (width > total_len) width - total_len else 0;

                        if (seg_texts.items.len == 1) {
                            // Single segment - apply modifiers
                            if (colon and at) {
                                // Center
                                const left_pad = pad_total / 2;
                                const right_pad = pad_total - left_pad;
                                var k: usize = 0;
                                while (k < left_pad) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                                try result.appendSlice(self.allocator, seg_texts.items[0]);
                                k = 0;
                                while (k < right_pad) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                            } else if (at) {
                                // Right justify
                                var k: usize = 0;
                                while (k < pad_total) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                                try result.appendSlice(self.allocator, seg_texts.items[0]);
                            } else {
                                // Left justify
                                try result.appendSlice(self.allocator, seg_texts.items[0]);
                                var k: usize = 0;
                                while (k < pad_total) : (k += 1) {
                                    try result.append(self.allocator, padchar);
                                }
                            }
                        } else {
                            // Multiple segments - distribute padding
                            const n_gaps = seg_texts.items.len - 1 + @intFromBool(colon) + @intFromBool(at);
                            if (n_gaps == 0) {
                                for (seg_texts.items) |s| {
                                    try result.appendSlice(self.allocator, s);
                                }
                            } else {
                                const pad_per_gap = pad_total / n_gaps;
                                const extra_pads = pad_total % n_gaps;

                                var gap_idx: usize = 0;

                                // Leading pad if colon
                                if (colon) {
                                    const this_pad = pad_per_gap + @intFromBool(gap_idx < extra_pads);
                                    var k: usize = 0;
                                    while (k < this_pad + minpad) : (k += 1) {
                                        try result.append(self.allocator, padchar);
                                    }
                                    gap_idx += 1;
                                }

                                for (seg_texts.items, 0..) |s, idx| {
                                    try result.appendSlice(self.allocator, s);
                                    if (idx < seg_texts.items.len - 1) {
                                        const this_pad = pad_per_gap + @intFromBool(gap_idx < extra_pads);
                                        var k: usize = 0;
                                        while (k < this_pad + minpad) : (k += 1) {
                                            try result.append(self.allocator, padchar);
                                        }
                                        gap_idx += 1;
                                    }
                                }

                                // Trailing pad if at
                                if (at) {
                                    const this_pad = pad_per_gap + @intFromBool(gap_idx < extra_pads);
                                    var k: usize = 0;
                                    while (k < this_pad + minpad) : (k += 1) {
                                        try result.append(self.allocator, padchar);
                                    }
                                }
                            }
                        }

                        i = end + 2;
                    },
                    else => {
                        // Unknown directive, output as-is
                        try result.append(self.allocator, fmt[i]);
                        i += 1;
                    },
                }
            } else {
                try result.append(self.allocator, fmt[i]);
                i += 1;
            }
        }

        // Handle destination
        if (dest.isNil()) {
            // Return as string
            return try self.allocString(result.items);
        } else {
            // Print to stdout (dest = t)
            const stdout_file = std.fs.File.stdout();
            var buf: [4096]u8 = undefined;
            var file_writer = stdout_file.writer(&buf);
            const w = &file_writer.interface;
            try w.writeAll(result.items);
            try w.flush();
            return Value.nil;
        }
    }

    fn formatValueAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        switch (val.typeKind()) {
            .nil => try result.appendSlice(self.allocator, "nil"),
            .t => try result.appendSlice(self.allocator, "t"),
            .unbound => try result.appendSlice(self.allocator, "#<unbound>"),
            .fixnum => {
                var buf: [32]u8 = undefined;
                const num_str = try std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()});
                try result.appendSlice(self.allocator, num_str);
            },
            .float => {
                var buf: [64]u8 = undefined;
                const num_str = try std.fmt.bufPrint(&buf, "{d}", .{val.toFloat()});
                try result.appendSlice(self.allocator, num_str);
            },
            .char => {
                const cp = val.toCharacter();
                if (cp < 128) {
                    try result.append(self.allocator, @as(u8, @intCast(cp)));
                }
            },
            .string, .string32 => try result.appendSlice(self.allocator, val.toPtr(runtime.String).bytes()),
            .symbol => try result.appendSlice(self.allocator, val.toPtr(Symbol).getName()),
            .keyword => {
                try result.append(self.allocator, ':');
                try result.appendSlice(self.allocator, val.toPtr(runtime.Keyword).getName());
            },
            .cons => try self.formatListAesthetic(val, result),
            .closure => try result.appendSlice(self.allocator, "#<closure>"),
            .vector => try result.appendSlice(self.allocator, "#<vector>"),
            .hashtable => try result.appendSlice(self.allocator, "#<hash-table>"),
            .rational => {
                const rat = val.toPtr(runtime.Rational);
                var buf: [64]u8 = undefined;
                const num_str = try std.fmt.bufPrint(&buf, "{d}/{d}", .{ rat.numerator, rat.denominator });
                try result.appendSlice(self.allocator, num_str);
            },
            .complex => {
                const cplx = val.toPtr(runtime.Complex);
                var buf: [128]u8 = undefined;
                const cplx_str = try std.fmt.bufPrint(&buf, "#C({d} {d})", .{ cplx.real, cplx.imag });
                try result.appendSlice(self.allocator, cplx_str);
            },
            .stream => try result.appendSlice(self.allocator, "#<stream>"),
            .bignum => try result.appendSlice(self.allocator, "#<bignum>"),
            .array => try result.appendSlice(self.allocator, "#<array>"),
            .pathname => try result.appendSlice(self.allocator, "#<pathname>"),
            .package => try result.appendSlice(self.allocator, "#<package>"),
            .chunk => try result.appendSlice(self.allocator, "#<chunk>"),
            .condition => try result.appendSlice(self.allocator, "#<condition>"),
            .class => try result.appendSlice(self.allocator, "#<class>"),
            .slotdef => try result.appendSlice(self.allocator, "#<slot-definition>"),
            .generic_function => try result.appendSlice(self.allocator, "#<generic-function>"),
            .method => try result.appendSlice(self.allocator, "#<method>"),
        }
    }

    fn formatValueStandard(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        if (val.isString()) {
            // Strings get quoted
            try result.append(self.allocator, '"');
            const str = val.toPtr(runtime.String);
            try result.appendSlice(self.allocator, str.bytes());
            try result.append(self.allocator, '"');
        } else {
            // Everything else same as aesthetic
            try self.formatValueAesthetic(val, result);
        }
    }

    fn formatFixnumBase(self: *Vm, val: Value, comptime base: u8, result: *std.ArrayList(u8)) Error!void {
        if (!val.isFixnum()) return;
        const n = val.toFixnum();
        var buf: [80]u8 = undefined;
        const spec = comptime switch (base) {
            2 => "{b}",
            8 => "{o}",
            10 => "{d}",
            16 => "{X}",
            else => unreachable,
        };
        const num_str = if (n >= 0)
            try std.fmt.bufPrint(&buf, spec, .{@as(u64, @intCast(n))})
        else
            try std.fmt.bufPrint(&buf, "-" ++ spec, .{@as(u64, @intCast(-n))});
        try result.appendSlice(self.allocator, num_str);
    }

    const MAX_FORMAT_DEPTH = 1000;

    fn formatListAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        try result.append(self.allocator, '(');
        var current = val;
        var first = true;
        var depth: usize = 0;
        while (current.isCons()) {
            // Prevent infinite loop on circular lists
            depth += 1;
            if (depth > MAX_FORMAT_DEPTH) {
                try result.appendSlice(self.allocator, "...");
                break;
            }
            if (!first) try result.append(self.allocator, ' ');
            first = false;
            const cons = current.toPtr(runtime.Cons);
            try self.formatValueAesthetic(cons.car, result);
            current = cons.cdr;
        }
        if (!current.isNil() and depth <= MAX_FORMAT_DEPTH) {
            try result.appendSlice(self.allocator, " . ");
            try self.formatValueAesthetic(current, result);
        }
        try result.append(self.allocator, ')');
    }

    /// Format iteration: process body for each element of a list
    /// Handles ~^ (escape) directive within the body
    fn formatIteration(self: *Vm, list: Value, body: []const u8, result: *std.ArrayList(u8)) Error!void {
        var current = list;
        var depth: usize = 0;

        while (current.isCons()) {
            depth += 1;
            if (depth > MAX_FORMAT_DEPTH) break;

            const cons = current.toPtr(runtime.Cons);
            const elem = cons.car;
            const remaining = cons.cdr;

            // Process body for this element
            var i: usize = 0;
            while (i < body.len) {
                if (body[i] == '~' and i + 1 < body.len) {
                    const directive = body[i + 1];
                    switch (directive) {
                        'A', 'a' => {
                            try self.formatValueAesthetic(elem, result);
                            i += 2;
                        },
                        'S', 's' => {
                            try self.formatValueStandard(elem, result);
                            i += 2;
                        },
                        'D', 'd' => {
                            if (elem.isFixnum()) {
                                var buf: [32]u8 = undefined;
                                const num_str = try std.fmt.bufPrint(&buf, "{d}", .{elem.toFixnum()});
                                try result.appendSlice(self.allocator, num_str);
                            }
                            i += 2;
                        },
                        '%' => {
                            try result.append(self.allocator, '\n');
                            i += 2;
                        },
                        '~' => {
                            try result.append(self.allocator, '~');
                            i += 2;
                        },
                        '^' => {
                            // Escape: exit iteration if no more elements
                            if (remaining.isNil()) {
                                return; // Exit iteration
                            }
                            i += 2;
                        },
                        else => {
                            try result.append(self.allocator, body[i]);
                            i += 1;
                        },
                    }
                } else {
                    try result.append(self.allocator, body[i]);
                    i += 1;
                }
            }

            current = remaining;
        }
    }

    // ========================================================================
    // Function call support
    // ========================================================================

    fn doCall(self: *Vm, argc: u8, tail: bool) Error!void {
        // Bounds check: need at least argc + 1 items on stack (args + function)
        if (self.sp < @as(usize, argc) + 1) return error.StackUnderflow;

        // Get function value (below args on stack)
        var fn_val = self.stack[self.sp - argc - 1];

        // If calling a generic function, delegate to its dispatcher
        if (fn_val.isGenericFunction()) {
            const gf = fn_val.toPtr(runtime.objects.GenericFunction);
            if (gf.dispatcher.isNil()) {
                std.debug.print("doCall: generic function has no dispatcher! name={any}\n", .{gf.name});
                return error.TypeMismatch;
            }
            fn_val = gf.dispatcher;
            // Update function slot on stack
            self.stack[self.sp - argc - 1] = fn_val;
        }

        if (!fn_val.isClosure()) {
            std.debug.print("doCall: fn_val is not a closure! type={}, argc={}\n", .{ fn_val.typeKind(), argc });
            return error.TypeMismatch;
        }

        const closure = fn_val.toPtr(runtime.Closure);
        const callee_chunk: *const Chunk = closure.code.toPtr(Chunk);
        const arity = callee_chunk.arity;
        const opt_count = callee_chunk.opt_count;
        const key_count = callee_chunk.key_count;
        const max_positional = arity + opt_count;

        // Find where keyword args actually start by scanning for first keyword
        // This handles cases like (foo req :k v) where optional is omitted
        var actual_positional = argc;
        if (key_count > 0 and argc > arity) {
            const arg_base = self.sp - argc;
            // Scan from required args position onwards for keyword objects
            var i: u8 = arity;
            while (i < argc) : (i += 2) {
                if (self.stack[arg_base + i].isKeyword()) {
                    actual_positional = i;
                    break;
                }
            }
        }

        // Check arity
        if (callee_chunk.has_rest != 0) {
            // Variadic: need at least required args
            if (argc < arity) {
                return error.TypeMismatch;
            }
        } else if (key_count > 0) {
            // Has keyword params: need at least required args
            if (argc < arity) {
                return error.TypeMismatch;
            }
            // Keyword args must come in pairs (after actual positional args)
            const kw_arg_count = argc - actual_positional;
            if (kw_arg_count % 2 != 0) {
                return error.TypeMismatch;
            }
        } else if (opt_count > 0) {
            // Has optional params: argc must be in [arity, arity + opt_count]
            if (argc < arity or argc > max_positional) {
                std.debug.print("doCall: optional arity mismatch! argc={}, arity={}, max_positional={}\n", .{ argc, arity, max_positional });
                return error.TypeMismatch;
            }
        } else {
            // Fixed: need exact arity
            if (argc != arity) {
                std.debug.print("doCall: arity mismatch! argc={}, arity={}, opt={}, key={}, rest={}\n", .{ argc, arity, opt_count, key_count, callee_chunk.has_rest });
                return error.TypeMismatch;
            }
        }

        // Build rest list if variadic (before we modify the stack)
        // Rest list contains args beyond required + optional + key params
        var rest_list = Value.nil;
        if (callee_chunk.has_rest != 0 and argc > max_positional) {
            // Build list from extra args (in reverse since we pop from end)
            const extra_count = argc - max_positional;
            var i: u8 = 0;
            while (i < extra_count) : (i += 1) {
                const idx = self.sp - 1 - i;
                rest_list = try self.allocCons(self.stack[idx], rest_list);
            }
            // Pop the extra args
            self.sp -= extra_count;
        }

        // Determine how many args to copy as locals
        // For keyword args, we need to keep ALL args for find_key to scan
        const actual_argc: u8 = if (key_count > 0) argc else @min(argc, max_positional);

        if (tail) {
            // Tail call: reuse current frame
            // Move arguments to start of current frame
            const current_bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
            const arg_start = self.sp - actual_argc;

            if (key_count > 0) {
                // For key args, layout is:
                // [required + optional args] [key params (nil)] [keyword pairs]
                // Use actual_positional to handle omitted optionals before keywords
                const positional_args: u8 = @min(actual_positional, max_positional);
                const kw_pair_count: u8 = argc - actual_positional;
                const key_slot_start = max_positional;
                const kw_pair_start = max_positional + key_count;

                // First, move keyword pairs to their slots
                var i: usize = kw_pair_count;
                while (i > 0) {
                    i -= 1;
                    self.stack[current_bp + kw_pair_start + i] = self.stack[arg_start + positional_args + i];
                }

                // Copy positional args
                for (0..positional_args) |j| {
                    self.stack[current_bp + j] = self.stack[arg_start + j];
                }

                // Initialize key param slots to nil
                for (0..key_count) |k| {
                    self.stack[current_bp + key_slot_start + k] = Value.nil;
                }

                self.sp = current_bp + kw_pair_start + kw_pair_count;
            } else {
                // Copy args to current frame's base
                for (0..actual_argc) |i| {
                    self.stack[current_bp + i] = self.stack[arg_start + i];
                }
                self.sp = current_bp + actual_argc;
            }

            // If variadic, push rest list as next local (after required + optional)
            if (callee_chunk.has_rest != 0) {
                try self.push(rest_list);
            }

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Update closure and argc in current frame
            if (self.fp > 0) {
                self.frames[self.fp - 1].closure = closure;
                self.frames[self.fp - 1].argc = argc;
            }

            // Reserve space for additional locals (after args + rest)
            const used_locals: usize = actual_argc + @as(u8, if (callee_chunk.has_rest != 0) 1 else 0);
            var i: usize = used_locals;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
        } else {
            // Regular call: push new frame
            if (self.fp >= MAX_FRAMES) {
                return error.StackOverflow;
            }

            // Save current state
            self.frames[self.fp] = .{
                .chunk = self.chunk,
                .return_ip = self.ip,
                .bp = self.sp - actual_argc - 1, // -1 for function value
                .closure = closure,
                .argc = argc,
            };
            self.fp += 1;

            // The arguments are already on stack above the function value
            // We need to set bp to point to first arg (overwriting fn_val slot)
            const new_bp = self.sp - actual_argc - 1;

            if (key_count > 0) {
                // For key args, layout is:
                // [required + optional args] [key params (nil)] [keyword pairs]
                // Use actual_positional to handle omitted optionals before keywords
                const positional_args: u8 = @min(actual_positional, max_positional);
                const kw_pair_count: u8 = argc - actual_positional;
                const key_slot_start = max_positional;
                const kw_pair_start = max_positional + key_count;

                // First, move keyword pairs to their slots (from the end to avoid overlap)
                // Keyword pairs are the last kw_pair_count args
                var i: usize = kw_pair_count;
                while (i > 0) {
                    i -= 1;
                    self.stack[new_bp + kw_pair_start + i] = self.stack[new_bp + 1 + positional_args + i];
                }

                // Copy positional args to their slots
                for (0..positional_args) |j| {
                    self.stack[new_bp + j] = self.stack[new_bp + 1 + j];
                }

                // Initialize key param slots to nil
                for (0..key_count) |k| {
                    self.stack[new_bp + key_slot_start + k] = Value.nil;
                }

                self.sp = new_bp + kw_pair_start + kw_pair_count;
            } else {
                // Normal case: copy args to slots [0, argc)
                for (0..actual_argc) |i| {
                    self.stack[new_bp + i] = self.stack[new_bp + 1 + i];
                }
                self.sp = new_bp + actual_argc;
            }

            // If variadic, push rest list as next local (after required + optional)
            if (callee_chunk.has_rest != 0) {
                try self.push(rest_list);
            }

            // Update frame bp
            self.frames[self.fp - 1].bp = new_bp;

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Reserve space for additional locals (after args + rest)
            const used: usize = actual_argc + @as(u8, if (callee_chunk.has_rest != 0) 1 else 0);
            var i: usize = used;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
        }
    }

    fn doApply(self: *Vm) Error!void {
        // Stack: ... fn args-list
        const args_list = try self.pop();
        const fn_val = try self.pop();

        if (!fn_val.isClosure()) {
            return error.TypeMismatch;
        }

        // Count and push args from list
        var count: u8 = 0;
        var list = args_list;
        while (list.isCons()) {
            if (count >= 255) return error.StackOverflow;
            const cons = list.toPtr(runtime.Cons);
            try self.push(cons.car);
            count += 1;
            list = cons.cdr;
        }

        // Validate list ended with nil (not improper list)
        if (!list.isNil()) return error.TypeMismatch;

        // Push function before args on stack
        // Current stack: ... arg1 arg2 ... argN
        // Need: ... fn arg1 arg2 ... argN
        // So we shift args up and insert fn
        if (count > 0) {
            // Bounds check before shuffling
            if (count > self.sp) return error.StackUnderflow;
            // Make room by moving args up one slot
            var i: usize = count;
            while (i > 0) {
                i -= 1;
                self.stack[self.sp - count + i + 1] = self.stack[self.sp - count + i];
            }
            self.stack[self.sp - count] = fn_val;
            self.sp += 1;
        } else {
            try self.push(fn_val);
        }

        // Now call with unpacked args
        try self.doCall(count, false);
    }

    // ========================================================================
    // Stack operations
    // ========================================================================

    pub fn push(self: *Vm, val: Value) Error!void {
        if (self.sp >= STACK_SIZE) return error.StackOverflow;
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    fn pop(self: *Vm) Error!Value {
        if (self.sp == 0) return error.StackUnderflow;
        self.sp -= 1;
        return self.stack[self.sp];
    }

    fn peek(self: *Vm, distance: usize) Error!Value {
        if (distance >= self.sp) return error.StackUnderflow;
        return self.stack[self.sp - 1 - distance];
    }

    fn strCharCode(item: Value) Error!u8 {
        switch (item.typeKind()) {
            .char => {
                const cp = item.toCharacter();
                if (cp > 255) return error.InvalidArgument;
                return @intCast(cp);
            },
            .fixnum => {
                const n = item.toFixnum();
                if (n < 0 or n > 255) return error.InvalidArgument;
                return @intCast(n);
            },
            else => return error.InvalidArgument,
        }
    }

    /// Position search for lists, strings, and vectors
    fn positionInSeq(self: *Vm, item: Value, seq: Value, cmp: runtime.HashTest) Error!Value {
        _ = self;
        // String: item should be a character or fixnum (char code)
        if (seq.isString()) {
            const str_obj = seq.toPtr(runtime.String);
            const str_bytes = str_obj.bytes();
            const char_code = try strCharCode(item);
            const needle = Value.makeCharacter(@intCast(char_code));
            for (str_bytes, 0..) |c, i| {
                const elem = Value.makeCharacter(@intCast(c));
                if (hashKeyEqualWithTest(needle, elem, cmp)) {
                    return Value.makeFixnum(@intCast(i));
                }
            }
            return Value.nil;
        }
        // Vector: search elements
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (hashKeyEqualWithTest(item, elem, cmp)) {
                    return Value.makeFixnum(@intCast(i));
                }
            }
            return Value.nil;
        }
        // List: iterate through cons cells
        var curr = seq;
        var idx: i64 = 0;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (hashKeyEqualWithTest(item, c.car, cmp)) {
                return Value.makeFixnum(idx);
            }
            curr = c.cdr;
            idx += 1;
        }
        return Value.nil;
    }

    /// Find search for lists, strings, and vectors
    fn findInSeq(self: *Vm, item: Value, seq: Value, cmp: runtime.HashTest) Error!Value {
        _ = self;
        // String: item should be a character or fixnum (char code)
        if (seq.isString()) {
            const str_obj = seq.toPtr(runtime.String);
            const str_bytes = str_obj.bytes();
            const char_code = try strCharCode(item);
            const needle = Value.makeCharacter(@intCast(char_code));
            for (str_bytes) |c| {
                const elem = Value.makeCharacter(@intCast(c));
                if (hashKeyEqualWithTest(needle, elem, cmp)) {
                    return elem;
                }
            }
            return Value.nil;
        }
        // Vector: search elements
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (hashKeyEqualWithTest(item, elem, cmp)) {
                    return elem;
                }
            }
            return Value.nil;
        }
        // List: iterate through cons cells
        var curr = seq;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (hashKeyEqualWithTest(item, c.car, cmp)) {
                return c.car;
            }
            curr = c.cdr;
        }
        return Value.nil;
    }

    /// Count occurrences for lists, strings, and vectors
    fn countInSeq(self: *Vm, item: Value, seq: Value, cmp: runtime.HashTest) Error!Value {
        _ = self;
        // String: item should be a character or fixnum (char code)
        if (seq.isString()) {
            const str_obj = seq.toPtr(runtime.String);
            const str_bytes = str_obj.bytes();
            const char_code = try strCharCode(item);
            const needle = Value.makeCharacter(@intCast(char_code));
            var n: i64 = 0;
            for (str_bytes) |c| {
                const elem = Value.makeCharacter(@intCast(c));
                if (hashKeyEqualWithTest(needle, elem, cmp)) {
                    n += 1;
                }
            }
            return Value.makeFixnum(n);
        }
        // Vector: search elements
        if (seq.isVector()) {
            const vec = seq.toPtr(runtime.Vector);
            var n: i64 = 0;
            for (0..vec.length) |i| {
                const elem = vec.get(i);
                if (hashKeyEqualWithTest(item, elem, cmp)) {
                    n += 1;
                }
            }
            return Value.makeFixnum(n);
        }
        // List: iterate through cons cells
        var curr = seq;
        var n: i64 = 0;
        while (curr.isCons()) {
            const c = curr.toPtr(Cons);
            if (hashKeyEqualWithTest(item, c.car, cmp)) {
                n += 1;
            }
            curr = c.cdr;
        }
        return Value.makeFixnum(n);
    }

    // ========================================================================
    // Bytecode reading
    // ========================================================================

    fn readOp(self: *Vm) Op {
        const code = self.chunk.getCode();
        const low: u16 = code[self.ip];
        const high: u16 = code[self.ip + 1];
        self.ip += 2;
        const opcode = low | (high << 8);
        return @enumFromInt(opcode);
    }

    fn readU8(self: *Vm) u8 {
        const byte = self.chunk.getCode()[self.ip];
        self.ip += 1;
        return byte;
    }

    fn readU16(self: *Vm) u16 {
        const val = self.chunk.readU16(self.ip);
        self.ip += 2;
        return val;
    }

    fn readI16(self: *Vm) i16 {
        const val = self.chunk.readI16(self.ip);
        self.ip += 2;
        return val;
    }

    fn readI32(self: *Vm) i32 {
        const val = self.chunk.readI32(self.ip);
        self.ip += 4;
        return val;
    }

    // ========================================================================
    // Binary operations
    // ========================================================================

    fn binaryOp(self: *Vm, comptime op: fn (i64, i64) Error!Value) Error!void {
        const b = try self.pop();
        const a = try self.pop();
        if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
        try self.push(try op(a.toFixnum(), b.toFixnum()));
    }

    fn binaryAdd(a: i64, b: i64) Error!Value {
        const result = @addWithOverflow(a, b);
        if (result[1] != 0) return error.TypeMismatch; // Overflow
        return Value.makeFixnum(result[0]);
    }

    fn binarySub(a: i64, b: i64) Error!Value {
        const result = @subWithOverflow(a, b);
        if (result[1] != 0) return error.TypeMismatch; // Overflow
        return Value.makeFixnum(result[0]);
    }

    fn binaryMul(a: i64, b: i64) Error!Value {
        const result = @mulWithOverflow(a, b);
        if (result[1] != 0) return error.TypeMismatch; // Overflow
        return Value.makeFixnum(result[0]);
    }

    fn binaryDiv(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        // minInt / -1 overflows
        if (a == std.math.minInt(i64) and b == -1) return error.TypeMismatch;
        return Value.makeFixnum(@divTrunc(a, b));
    }

    fn binaryMod(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        return Value.makeFixnum(@mod(a, b));
    }

    fn binaryQuot(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        if (a == std.math.minInt(i64) and b == -1) return error.TypeMismatch; // overflow
        return Value.makeFixnum(@divTrunc(a, b));
    }

    fn binaryRem(a: i64, b: i64) Error!Value {
        if (b == 0) return error.DivisionByZero;
        return Value.makeFixnum(@rem(a, b));
    }

    fn valToFloat(val: Value) Error!f64 {
        if (val.isFixnum()) return @floatFromInt(val.toFixnum());
        if (val.isFloat()) return val.toFloat();
        if (val.isRational()) {
            const rat = val.toPtr(runtime.Rational);
            const num: f64 = @floatFromInt(rat.numerator);
            const den: f64 = @floatFromInt(rat.denominator);
            return num / den;
        }
        return error.TypeMismatch;
    }

    fn binaryLt(a: i64, b: i64) Error!Value {
        return if (a < b) Value.t else Value.nil;
    }

    fn binaryGt(a: i64, b: i64) Error!Value {
        return if (a > b) Value.t else Value.nil;
    }

    fn binaryLe(a: i64, b: i64) Error!Value {
        return if (a <= b) Value.t else Value.nil;
    }

    fn binaryGe(a: i64, b: i64) Error!Value {
        return if (a >= b) Value.t else Value.nil;
    }

    fn binaryNumEq(a: i64, b: i64) Error!Value {
        return if (a == b) Value.t else Value.nil;
    }
};

// ============================================================================
// Equality helpers
// ============================================================================

/// Float equality per CL semantics: NaN != NaN, 0.0 == -0.0
fn floatEql(fa: f64, fb: f64) bool {
    // NaN is never equal to anything, including itself
    if (std.math.isNan(fa) or std.math.isNan(fb)) return false;
    // 0.0 and -0.0 are equal
    return fa == fb;
}

/// Structural equality (equal in Lisp)
/// Returns true if two values are structurally equal
fn valueEqual(a: Value, b: Value) bool {
    return valueEqualWithDepth(a, b, 0);
}

const MAX_EQUAL_DEPTH = 1000;

fn valueEqualWithDepth(a: Value, b: Value, depth: usize) bool {
    // Prevent stack overflow on circular structures
    if (depth > MAX_EQUAL_DEPTH) return false;

    // Handle floats with proper CL semantics (NaN != NaN, 0.0 == -0.0)
    if (a.isFloat() and b.isFloat()) {
        return floatEql(a.toFloat(), b.toFloat());
    }

    // Fast path: identical values (handles most cases)
    if (a.raw == b.raw) return true;

    // Both must be same type for structural equality
    // Fixnums are immediate, so if they're not identical, they're not equal
    if (a.isFixnum() or b.isFixnum()) return false;

    // Characters are immediate
    if (a.isCharacter() or b.isCharacter()) return false;

    // Floats already handled above
    if (a.isFloat() or b.isFloat()) return false;

    // Check tag type
    const tag_a = a.raw & 0xF;
    const tag_b = b.raw & 0xF;
    if (tag_a != tag_b) return false;

    // Both are pointers of same type
    switch (a.typeKind()) {
        .cons => {
            // Recursively compare car and cdr
            const cons_a = a.toPtr(Cons);
            const cons_b = b.toPtr(Cons);
            return valueEqualWithDepth(cons_a.car, cons_b.car, depth + 1) and
                valueEqualWithDepth(cons_a.cdr, cons_b.cdr, depth + 1);
        },
        .string => {
            // Compare strings character by character
            const str_a = a.toPtr(String);
            const str_b = b.toPtr(String);
            return std.mem.eql(u8, str_a.bytes(), str_b.bytes());
        },
        .vector => {
            // Compare vectors element by element
            const vec_a = a.toPtr(Vector);
            const vec_b = b.toPtr(Vector);
            if (vec_a.length != vec_b.length) return false;
            for (vec_a.items(), vec_b.items()) |ea, eb| {
                if (!valueEqualWithDepth(ea, eb, depth + 1)) return false;
            }
            return true;
        },
        else => return false,
    }
}

// ============================================================================
// Hash table helpers (open addressing with linear probing)
// ============================================================================

/// FNV-1a hash for bytes
fn fnvHash(bytes: []const u8) u64 {
    var hash: u64 = 0xcbf29ce484222325; // FNV offset basis
    for (bytes) |b| {
        hash ^= b;
        hash *%= 0x100000001b3; // FNV prime
    }
    return hash;
}

/// FNV-1a hash for a u64
fn fnvHashU64(val: u64) u64 {
    var hash: u64 = 0xcbf29ce484222325;
    var v = val;
    for (0..8) |_| {
        hash ^= @as(u8, @truncate(v));
        hash *%= 0x100000001b3;
        v >>= 8;
    }
    return hash;
}

/// Normalize float for hashing under eql semantics
/// 0.0 and -0.0 should hash the same; NaN hashes consistently
fn normalizeFloatForHash(f: f64) u64 {
    if (std.math.isNan(f)) return 0x7FF8000000000000; // Canonical NaN
    if (f == 0.0) return 0; // Normalize -0.0 to 0.0
    return @bitCast(f);
}

/// Hash a Value for use in hash table lookup
fn hashValueWithTest(val: Value, test_type: runtime.HashTest) u64 {
    switch (test_type) {
        .eq => {
            // eq: pure identity - hash by raw value
            // Symbols/keywords are interned, so hash by name for GC stability
            return switch (val.typeKind()) {
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                else => fnvHashU64(val.raw),
            };
        },
        .eql => {
            // eql: identity for most, but floats need normalization
            // Symbols/keywords are interned, so hash by name for GC stability
            return switch (val.typeKind()) {
                .float => fnvHashU64(normalizeFloatForHash(val.toFloat())),
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                else => fnvHashU64(val.raw),
            };
        },
        .equal => {
            // equal: content-based hashing
            return switch (val.typeKind()) {
                .nil, .t, .unbound, .fixnum, .char => fnvHashU64(val.raw),
                .float => fnvHashU64(normalizeFloatForHash(val.toFloat())),
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                .string, .string32 => fnvHash(val.toPtr(runtime.String).bytes()),
                // Reference types: hash address (NOT stable across GC)
                .cons, .vector, .closure, .hashtable, .rational, .complex, .stream, .bignum, .array, .pathname, .package, .chunk, .condition, .class, .slotdef, .generic_function, .method => fnvHashU64(val.raw),
            };
        },
    }
}

/// Check if two Values are equal for hash table purposes
fn hashKeyEqualWithTest(a: Value, b: Value, test_type: runtime.HashTest) bool {
    switch (test_type) {
        .eq => {
            // eq: pure identity comparison
            return a.raw == b.raw;
        },
        .eql => {
            // eql: identity, but floats use numeric equality
            if (a.raw == b.raw) return true;
            if (a.isFloat() and b.isFloat()) {
                return floatEql(a.toFloat(), b.toFloat());
            }
            return false;
        },
        .equal => {
            // equal: structural equality
            return valueEqual(a, b);
        },
    }
}

/// Get value from hash table, returns nil if not found
fn hashTableGet(ht: *HashTable, key: Value) Value {
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    const test_type = ht.test_type;
    var idx = hashValueWithTest(key, test_type) & mask;

    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (HashTable.isEmpty(entry)) {
            return Value.nil; // Not found
        }
        if (!HashTable.isDeleted(entry) and hashKeyEqualWithTest(entry.key, key, test_type)) {
            return entry.value; // Found
        }
        idx = (idx + 1) & mask; // Linear probe
    }
    return Value.nil; // Table full and key not found
}

/// Resize hash table in place by updating its entries pointer
fn hashTableResizeInPlace(vm: *Vm, ht: *HashTable) Error!void {
    const new_capacity = ht.capacity * 2;
    // Preserve the test_type from the original hash table
    const new_ht_val = try vm.allocHashTable(new_capacity, ht.test_type);
    const new_ht = new_ht_val.toPtr(HashTable);

    // Copy all entries from old to new
    for (ht.getEntries()) |entry| {
        if (!HashTable.isEmpty(entry) and !HashTable.isDeleted(entry)) {
            _ = hashTableSet(new_ht, entry.key, entry.value);
        }
    }

    // Update original hash table to use new entries
    ht.entries = new_ht.entries;
    ht.capacity = new_ht.capacity;
    // count stays the same (new_ht.count would be same as ht.count)
}

/// Set value in hash table (insert or update)
/// Returns true on success, false if table is full and needs resize
fn hashTableSet(ht: *HashTable, key: Value, value: Value) bool {
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    const test_type = ht.test_type;
    var idx = hashValueWithTest(key, test_type) & mask;

    var first_deleted: ?usize = null;
    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (HashTable.isEmpty(entry)) {
            // Check load factor before inserting (max 75%)
            if (ht.count * 4 >= ht.capacity * 3) {
                return false; // Table needs resize
            }
            // Insert at first deleted slot if we found one, else here
            const insert_idx = first_deleted orelse idx;
            entries[insert_idx] = .{ .key = key, .value = value };
            ht.count += 1;
            return true;
        }
        if (HashTable.isDeleted(entry)) {
            if (first_deleted == null) first_deleted = idx;
        } else if (hashKeyEqualWithTest(entry.key, key, test_type)) {
            // Update existing
            entries[idx].value = value;
            return true;
        }
        idx = (idx + 1) & mask;
    }
    // Table full - insert at first deleted if available
    if (first_deleted) |del_idx| {
        if (ht.count * 4 >= ht.capacity * 3) {
            return false; // Table needs resize
        }
        entries[del_idx] = .{ .key = key, .value = value };
        ht.count += 1;
        return true;
    }
    // Table is truly full
    return false;
}

/// Remove key from hash table, returns true if removed
fn hashTableRemove(ht: *HashTable, key: Value) bool {
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    const test_type = ht.test_type;
    var idx = hashValueWithTest(key, test_type) & mask;

    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (HashTable.isEmpty(entry)) {
            return false; // Not found
        }
        if (!HashTable.isDeleted(entry) and hashKeyEqualWithTest(entry.key, key, test_type)) {
            // Mark as deleted
            entries[idx].key = HashTable.DELETED;
            ht.count -= 1;
            return true;
        }
        idx = (idx + 1) & mask;
    }
    return false;
}

fn hashTableClear(ht: *HashTable) void {
    const entries = ht.getEntries();
    for (entries[0..ht.capacity]) |*entry| {
        entry.key = HashTable.EMPTY;
        entry.value = Value.nil;
    }
    ht.count = 0;
}

// ============================================================================
// Tests
// ============================================================================

test "vm push and return" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const code = [_]u8{
        0x02, 0x00, // push_i32
        42, 0, 0, 0, // i32 value: 42
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm callClosure runs and restores" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_op & 0xFF), @truncate(push_op >> 8),
        42, 0, 0, 0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_val = try heap.allocChunk(&code, &.{}, 0, 0, 0, false, 0);
    const closure_val = try heap.allocClosure(chunk_val, 0, &.{});
    const closure = closure_val.toPtr(runtime.Closure);

    const result = try vm.callClosure(closure, 0);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm allocVector propagates overflow" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const huge = std.math.maxInt(usize);
    try testing.expectError(error.Overflow, vm.allocVector(1, huge));
}

test "vm arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // (+ 10 20) = 30
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const add_op: u16 = @intFromEnum(Op.add);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 10,                       0,                      0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 20,                       0,                      0, 0,
        @truncate(add_op & 0xFF),      @truncate(add_op >> 8),      @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "vm make_complex" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const consts = [_]Value{
        Value.makeFloat(1.5),
    };

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_complex_op: u16 = @intFromEnum(Op.make_complex);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0,                       0,
        @truncate(push_i32_op & 0xFF),   @truncate(push_i32_op >> 8),   2,                       0, 0, 0,
        @truncate(make_complex_op & 0xFF), @truncate(make_complex_op >> 8),
        @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expect(result.typeKind() == .complex);
    const cplx = result.toPtr(runtime.Complex);
    try testing.expectApproxEqAbs(@as(f64, 1.5), cplx.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 2.0), cplx.imag, 0.0001);
}

test "vm sym_name" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_t_op: u16 = @intFromEnum(Op.push_t);
    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const sym_name_op: u16 = @intFromEnum(Op.sym_name);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code_nil = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(sym_name_op & 0xFF), @truncate(sym_name_op >> 8),
        @truncate(ret_op & 0xFF),      @truncate(ret_op >> 8),
    };
    const chunk_nil = Chunk{
        .code = @constCast(&code_nil),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_nil.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    const res_nil = try vm.run(&chunk_nil);
    try testing.expect(res_nil.isString());
    try testing.expectEqualStrings("nil", res_nil.toPtr(runtime.String).bytes());

    const code_t = [_]u8{
        @truncate(push_t_op & 0xFF), @truncate(push_t_op >> 8),
        @truncate(sym_name_op & 0xFF), @truncate(sym_name_op >> 8),
        @truncate(ret_op & 0xFF),    @truncate(ret_op >> 8),
    };
    const chunk_t = Chunk{
        .code = @constCast(&code_t),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_t.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    const res_t = try vm.run(&chunk_t);
    try testing.expect(res_t.isString());
    try testing.expectEqualStrings("t", res_t.toPtr(runtime.String).bytes());

    const sym = try heap.intern("foo");
    const consts = [_]Value{sym};
    const code_sym = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(sym_name_op & 0xFF),   @truncate(sym_name_op >> 8),
        @truncate(ret_op & 0xFF),        @truncate(ret_op >> 8),
    };
    const chunk_sym = Chunk{
        .code = @constCast(&code_sym),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_sym.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    const res_sym = try vm.run(&chunk_sym);
    try testing.expect(res_sym.isString());
    try testing.expectEqualStrings("FOO", res_sym.toPtr(runtime.String).bytes());
}

test "vm cons car cdr" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // (car (cons 1 2)) = 1
    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x40, 0x00, // cons
        0x41, 0x00, // car
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "vm list last" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 3, 0, 0, 0, // push_i32 3
        0x43, 0x00, 3, // make_list 3
        0x48, 0x00, // list_last
        0x41, 0x00, // car
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "vm elt_set list" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const make_list_op: u16 = @intFromEnum(Op.make_list);
    const list_nth_op: u16 = @intFromEnum(Op.list_nth);
    const elt_set_op: u16 = @intFromEnum(Op.elt_set);
    const pop_op: u16 = @intFromEnum(Op.pop);

    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 3, 0, 0, 0, // push_i32 3
        @truncate(make_list_op & 0xFF), @truncate(make_list_op >> 8), 3, // make_list 3
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        0x02, 0x00, 99, 0, 0, 0, // push_i32 99 (value)
        @truncate(elt_set_op & 0xFF), @truncate(elt_set_op >> 8),
        @truncate(pop_op & 0xFF), @truncate(pop_op >> 8),
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        0x10, 0x00, 0, // load_local 0
        @truncate(list_nth_op & 0xFF), @truncate(list_nth_op >> 8),
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "vm symbol_package nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const symbol_package_op: u16 = @intFromEnum(Op.symbol_package);
    const package_name_op: u16 = @intFromEnum(Op.package_name);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(symbol_package_op & 0xFF), @truncate(symbol_package_op >> 8),
        @truncate(package_name_op & 0xFF), @truncate(package_name_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expect(result.isString());
    try testing.expectEqualStrings("COMMON-LISP", result.toPtr(runtime.String).bytes());
}

test "vm aref aset vector" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const make_vec_n_op: u16 = @intFromEnum(Op.make_vec_n);
    const aref_op: u16 = @intFromEnum(Op.aref);
    const aset_op: u16 = @intFromEnum(Op.aset);
    const pop_op: u16 = @intFromEnum(Op.pop);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code = [_]u8{
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 3, 0, 0, 0, // push_i32 3
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8), 3, // make_vec_n 3
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        0x02, 0x00, 99, 0, 0, 0, // push_i32 99 (value)
        @truncate(aset_op & 0xFF), @truncate(aset_op >> 8), 1, // aset sub_count=1
        @truncate(pop_op & 0xFF), @truncate(pop_op >> 8),
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1 (index)
        @truncate(aref_op & 0xFF), @truncate(aref_op >> 8), 1, // aref sub_count=1
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "vm symbol_value specials" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_t_op: u16 = @intFromEnum(Op.push_t);
    const symbol_value_op: u16 = @intFromEnum(Op.symbol_value);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code_nil = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(symbol_value_op & 0xFF), @truncate(symbol_value_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_nil = Chunk{
        .code = @constCast(&code_nil),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_nil.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_nil = try vm.run(&chunk_nil);
    try testing.expect(res_nil.isNil());

    const code_t = [_]u8{
        @truncate(push_t_op & 0xFF), @truncate(push_t_op >> 8),
        @truncate(symbol_value_op & 0xFF), @truncate(symbol_value_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_t = Chunk{
        .code = @constCast(&code_t),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_t.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_t = try vm.run(&chunk_t);
    try testing.expect(res_t.isT());
}

test "vm format selector" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const format_op: u16 = @intFromEnum(Op.format);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const control = try heap.allocBaseString("~[no~;yes~]");
    const consts = [_]Value{control};

    const code_nil = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(format_op & 0xFF), @truncate(format_op >> 8), 1,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_nil = Chunk{
        .code = @constCast(&code_nil),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_nil.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_nil = try vm.run(&chunk_nil);
    try testing.expect(res_nil.isString());
    try testing.expectEqualStrings("no", res_nil.toPtr(runtime.String).bytes());

    const code_one = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 1, 0, 0, 0,
        @truncate(format_op & 0xFF), @truncate(format_op >> 8), 1,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_one = Chunk{
        .code = @constCast(&code_one),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_one.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_one = try vm.run(&chunk_one);
    try testing.expect(res_one.isString());
    try testing.expectEqualStrings("yes", res_one.toPtr(runtime.String).bytes());
}

test "vm list_position count string" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const code_char_op: u16 = @intFromEnum(Op.code_char);
    const list_position_op: u16 = @intFromEnum(Op.list_position);
    const list_count_op: u16 = @intFromEnum(Op.list_count);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const str = try heap.allocBaseString("abca");
    const consts = [_]Value{str};

    const code_pos = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 98, 0, 0, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(list_position_op & 0xFF), @truncate(list_position_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_pos = Chunk{
        .code = @constCast(&code_pos),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_pos.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_pos = try vm.run(&chunk_pos);
    try testing.expectEqual(@as(i64, 1), res_pos.toFixnum());

    const code_count = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 97, 0, 0, 0,
        @truncate(code_char_op & 0xFF), @truncate(code_char_op >> 8),
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(list_count_op & 0xFF), @truncate(list_count_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_count = Chunk{
        .code = @constCast(&code_count),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_count.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_count = try vm.run(&chunk_count);
    try testing.expectEqual(@as(i64, 2), res_count.toFixnum());
}

test "vm list_find string returns character" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const list_find_op: u16 = @intFromEnum(Op.list_find);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const str = try heap.allocBaseString("abca");
    const consts = [_]Value{str};

    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 97, 0, 0, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(list_find_op & 0xFF), @truncate(list_find_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res = try vm.run(&chunk);
    try testing.expect(res.isCharacter());
    try testing.expectEqual(@as(u21, 97), res.toCharacter());
}

test "vm list_position string invalid item errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const list_position_op: u16 = @intFromEnum(Op.list_position);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const item_str = try heap.allocBaseString("x");
    const seq_str = try heap.allocBaseString("abca");
    const consts = [_]Value{ item_str, seq_str };

    const code = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 1, 0,
        @truncate(list_position_op & 0xFF), @truncate(list_position_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    try testing.expectError(error.InvalidArgument, vm.run(&chunk));
}

test "vm equal vector string" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_vec_n_op: u16 = @intFromEnum(Op.make_vec_n);
    const store_local_op: u16 = @intFromEnum(Op.store_local);
    const load_local_op: u16 = @intFromEnum(Op.load_local);
    const equal_op: u16 = @intFromEnum(Op.equal);
    const ret_op: u16 = @intFromEnum(Op.ret);

    const code_vec = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 2, 0, 0, 0,
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8), 2,
        @truncate(store_local_op & 0xFF), @truncate(store_local_op >> 8), 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 2, 0, 0, 0,
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8), 2,
        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8), 0,
        @truncate(equal_op & 0xFF), @truncate(equal_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_vec = Chunk{
        .code = @constCast(&code_vec),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code_vec.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const res_vec = try vm.run(&chunk_vec);
    try testing.expect(res_vec.isT());

    const str_a = try heap.allocBaseString("hi");
    const str_b = try heap.allocBaseString("hi");
    const consts = [_]Value{ str_a, str_b };

    const code_str = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 1, 0,
        @truncate(equal_op & 0xFF), @truncate(equal_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };

    const chunk_str = Chunk{
        .code = @constCast(&code_str),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = consts.len,
        .code_len = code_str.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const res_str = try vm.run(&chunk_str);
    try testing.expect(res_str.isT());
}

test "vm conditional" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // (if nil 1 2) = 2
    const code = [_]u8{
        0x00, 0x00, // push_nil
        0x81, 0x00, 10, 0, // jmp_nil 10
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x80, 0x00, 6, 0, // jmp 6
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "vm locals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Store 42 in local 0, load it back
    const code = [_]u8{
        0x02, 0x00, 42, 0, 0, 0, // push_i32 42
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm hash table" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Create hash table, set key 42 to value 100, get key 42
    // Use local 0 to store ht
    const code = [_]u8{
        0xA4, 0x00, 16, 0, 1, // make_hash cap=16 type=eql
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 42, 0, 0, 0, // push_i32 42
        0x02, 0x00, 100, 0, 0, 0, // push_i32 100
        0x96, 0x00, // hash_set
        0x05, 0x00, // pop
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 42, 0, 0, 0, // push_i32 42
        0xA5, 0x00, // hash_get
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 100), result.toFixnum());
}

test "vm hash table count and remove" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Create hash table, set 2 keys, get count
    const code = [_]u8{
        0xA4, 0x00, 16, 0, 1, // make_hash cap=16 type=eql
        0x11, 0x00, 0, // store_local 0
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 1, 0, 0, 0, // push_i32 1
        0x02, 0x00, 10, 0, 0, 0, // push_i32 10
        0x96, 0x00, // hash_set
        0x05, 0x00, // pop
        0x10, 0x00, 0, // load_local 0
        0x02, 0x00, 2, 0, 0, 0, // push_i32 2
        0x02, 0x00, 20, 0, 0, 0, // push_i32 20
        0x96, 0x00, // hash_set
        0x05, 0x00, // pop
        0x10, 0x00, 0, // load_local 0
        0x98, 0x00, // hash_count
        0x92, 0x00, // ret
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 1,
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "vm read_from_string propagates parse errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const bad = try heap.allocBaseString("(");
    try vm.push(bad);
    try testing.expectError(error.UnterminatedList, vm.executeOp(.read_from_string));
}

test "vm typep propagates invalid type spec" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    try vm.push(Value.makeFixnum(1));
    try vm.push(Value.makeFixnum(2));
    try testing.expectError(error.InvalidTypeSpecifier, vm.executeOp(.typep));
}

test "vm write_to_string propagates buffer errors" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    var bytes: [300]u8 = undefined;
    @memset(&bytes, 'a');
    const str = try heap.allocBaseString(&bytes);
    try vm.push(str);

    try testing.expectError(error.NoSpaceLeft, vm.executeOp(.write_to_string));
}
