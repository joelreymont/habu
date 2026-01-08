//! Bytecode Virtual Machine for Habu
//!
//! Stack-based interpreter that executes bytecode.
//! Designed for portability (WASM target).

comptime {
    @setEvalBranchQuota(5000);
}

const std = @import("std");
const bytecode = @import("../bytecode/bytecode.zig");
const Op = bytecode.Op;
const Chunk = bytecode.Chunk;
const runtime = @import("../runtime/runtime.zig");
const primitives = @import("../runtime/primitives/primitives.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const String = runtime.String;
const Symbol = runtime.Symbol;
const arith = @import("../runtime/primitives/arith.zig");
const io = @import("../runtime/primitives/io.zig");
const stringPrims = @import("../runtime/primitives/string.zig");
const HashTable = runtime.HashTable;
const Vector = runtime.Vector;
const compiler = @import("../compiler/compiler.zig");
const GlobalEnv = compiler.GlobalEnv;
const Parser = @import("../reader/parser.zig").Parser;

pub const Error = error{
    StackOverflow,
    StackUnderflow,
    TypeMismatch,
    DivisionByZero,
    InvalidOpcode,
    InvalidConstant,
    InvalidArgument,
    OutOfMemory,
    Halt,
    UnhandledThrow,
    UnboundSymbol,
    UserError,
    RestartNotFound,
};

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

    /// Let scope stack for nested let bindings
    scope_stack: [MAX_SCOPES]Scope,
    /// Scope stack pointer (number of active scopes)
    scope_sp: usize,

    /// Saved throw state for unwinding through unwind-protect
    pending_throw_tag: Value,
    pending_throw_value: Value,
    pending_error: ?Error,
    is_unwinding: bool,

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

    /// Counter for gensym
    gensym_counter: u64,

    /// Current closure for load_capture when fp=0 (used by callClosure)
    current_closure: ?*const runtime.Closure,

    /// Current argc for load_argc when fp=0 (used by callClosure)
    current_argc: u8,

    /// Pre-interned type symbols for runtime type dispatch
    type_syms: TypeSymbols,

    /// Type symbols for runtime typep dispatch
    const TypeSymbols = struct {
        fixnum: Value,
        number: Value, // CL alias for numeric types
        cons: Value,
        symbol: Value,
        string: Value,
        vector: Value,
        closure: Value,
        function: Value,
        keyword: Value,
        character: Value,
        @"hash-table": Value,
        nil: Value,
        null: Value,
        list: Value,
        atom: Value,
        t: Value,

        fn init(heap: *Heap) !TypeSymbols {
            return .{
                .fixnum = try heap.intern("fixnum"),
                .number = try heap.intern("number"),
                .cons = try heap.intern("cons"),
                .symbol = try heap.intern("symbol"),
                .string = try heap.intern("string"),
                .vector = try heap.intern("vector"),
                .closure = try heap.intern("closure"),
                .function = try heap.intern("function"),
                .keyword = try heap.intern("keyword"),
                .character = try heap.intern("character"),
                .@"hash-table" = try heap.intern("hash-table"),
                .nil = try heap.intern("nil"),
                .null = try heap.intern("null"),
                .list = try heap.intern("list"),
                .atom = try heap.intern("atom"),
                .t = try heap.intern("t"),
            };
        }
    };

    const STACK_SIZE = 4096;
    const MAX_SECONDARY_VALUES = 20;
    const MAX_FRAMES = 256;
    const MAX_GLOBALS = 2048;
    const MAX_CATCHES = 32;
    const MAX_UNWINDS = 32;
    const MAX_RESTARTS = 64;

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
            .chunk_pool = &[_]*Chunk{},
            .chunk_base = 0,
            .catch_stack = undefined,
            .catch_sp = 0,
            .unwind_stack = undefined,
            .unwind_sp = 0,
            .restart_stack = undefined,
            .restart_sp = 0,
            .scope_stack = undefined,
            .scope_sp = 0,
            .pending_throw_tag = Value.nil,
            .pending_throw_value = Value.nil,
            .pending_error = null,
            .is_unwinding = false,
            .secondary_values = undefined,
            .secondary_values_count = 0,
            .global_env = null,
            .load_callback = null,
            .load_context = null,
            .eval_callback = null,
            .eval_context = null,
            .macroexpand_callback = null,
            .macroexpand_context = null,
            .gensym_counter = 0,
            .current_closure = null,
            .current_argc = 0,
            .type_syms = try TypeSymbols.init(heap),
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

    /// Allocate a cons cell, running GC if needed
    pub fn allocCons(self: *Vm, car: Value, cdr: Value) error{OutOfMemory}!Value {
        return self.heap.allocCons(car, cdr) catch {
            _ = self.collectGarbage();
            return try self.heap.allocCons(car, cdr);
        };
    }

    /// Allocate a vector, running GC if needed
    pub fn allocVector(self: *Vm, length: usize, capacity: usize) error{OutOfMemory}!Value {
        return self.heap.allocVector(length, capacity) catch {
            _ = self.collectGarbage();
            return try self.heap.allocVector(length, capacity);
        };
    }

    /// Allocate a string, running GC if needed
    pub fn allocString(self: *Vm, data: []const u8) error{OutOfMemory}!Value {
        return self.heap.allocString(data) catch {
            _ = self.collectGarbage();
            return try self.heap.allocString(data);
        };
    }

    /// Allocate an uninitialized string, running GC if needed
    pub fn allocStringUninitialized(self: *Vm, length: usize) error{OutOfMemory}!Value {
        return self.heap.allocStringUninitialized(length) catch {
            _ = self.collectGarbage();
            return try self.heap.allocStringUninitialized(length);
        };
    }

    /// Allocate a symbol (uninterned), running GC if needed
    pub fn allocSymbol(self: *Vm, name: []const u8) error{OutOfMemory}!Value {
        return self.heap.allocSymbol(name) catch {
            _ = self.collectGarbage();
            return try self.heap.allocSymbol(name);
        };
    }

    /// Allocate a closure, running GC if needed
    pub fn allocClosureWithGC(self: *Vm, code: *const anyopaque, arity: u32, captures: []const Value) error{OutOfMemory}!Value {
        return self.heap.allocClosure(code, arity, captures) catch {
            _ = self.collectGarbage();
            return try self.heap.allocClosure(code, arity, captures);
        };
    }

    /// Allocate a hash table, running GC if needed
    pub fn allocHashTable(self: *Vm, capacity: usize, test_type: runtime.HashTest) error{OutOfMemory}!Value {
        return self.heap.allocHashTable(capacity, test_type) catch {
            _ = self.collectGarbage();
            return try self.heap.allocHashTable(capacity, test_type);
        };
    }

    /// Intern a symbol, running GC if needed
    pub fn intern(self: *Vm, name: []const u8) error{OutOfMemory}!Value {
        return self.heap.intern(name) catch {
            _ = self.collectGarbage();
            return try self.heap.intern(name);
        };
    }

    /// Run garbage collection, using VM state as roots
    /// Returns bytes reclaimed
    pub fn collectGarbage(self: *Vm) usize {
        // Gather roots from VM state
        var roots = std.ArrayList(Value){};
        defer roots.deinit(self.allocator);

        // Stack values
        roots.appendSlice(self.allocator, self.stack[0..self.sp]) catch return 0;

        // Global values
        roots.appendSlice(self.allocator, self.globals[0..self.num_globals]) catch return 0;

        // Catch frame return values
        for (self.catch_stack[0..self.catch_sp]) |frame| {
            roots.append(self.allocator, frame.tag) catch return 0;
        }

        // Frame closures - must trace closures in call frames
        for (self.frames[0..self.fp]) |frame| {
            if (frame.closure) |c| {
                roots.append(self.allocator, Value.makeClosure(c)) catch return 0;
            }
        }

        // current_closure for callClosure's fp=0 case
        if (self.current_closure) |c| {
            roots.append(self.allocator, Value.makeClosure(c)) catch return 0;
        }

        // Pending throw values
        roots.append(self.allocator, self.pending_throw_tag) catch return 0;
        roots.append(self.allocator, self.pending_throw_value) catch return 0;

        // Secondary values
        roots.appendSlice(self.allocator, self.secondary_values[0..self.secondary_values_count]) catch return 0;

        // Chunk constant pools - track start index for each chunk
        var chunk_const_starts = std.ArrayList(usize){};
        defer chunk_const_starts.deinit(self.allocator);
        for (self.chunk_pool) |chunk| {
            chunk_const_starts.append(self.allocator, roots.items.len) catch return 0;
            for (chunk.constants) |c| {
                roots.append(self.allocator, Value{ .raw = c }) catch return 0;
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

        // Update secondary values
        for (self.secondary_values[0..self.secondary_values_count]) |*v| {
            v.* = roots.items[idx];
            idx += 1;
        }

        // Update chunk constant pools with relocated values
        for (self.chunk_pool, 0..) |chunk, chunk_idx| {
            const start = chunk_const_starts.items[chunk_idx];
            for (chunk.constants, 0..) |_, const_idx| {
                chunk.constants[const_idx] = roots.items[start + const_idx].raw;
            }
        }

        return reclaimed;
    }

    /// Call a closure with arguments already on stack
    /// Expects args to be pushed already at positions [0..argc)
    pub fn callClosure(self: *Vm, closure: *const runtime.Closure, argc: u8) Error!Value {
        // Save state - will be restored on both success and error
        const saved_state = State.save(self);

        // Set up to execute the closure's chunk directly (like vm.run)
        const closure_chunk: *const Chunk = @ptrCast(@alignCast(closure.code));
        self.chunk = closure_chunk;
        self.ip = 0;
        self.fp = 0; // No frame - ret at fp=0 returns immediately
        self.scope_sp = 0; // Reset let scope stack

        // Args are already on stack as locals (at positions 0..argc)
        // Reset sp to argc (in case it was different)
        self.sp = argc;
        // If closure needs more locals, push nil for them
        while (self.sp < closure_chunk.num_locals) {
            self.push(Value.nil) catch |err| {
                saved_state.restore(self);
                return err;
            };
        }

        // Store closure and argc for load_capture/load_argc when fp=0
        self.current_closure = closure;
        self.current_argc = argc;

        // Execute until return
        const result = self.execute() catch |err| {
            saved_state.restore(self);
            return err;
        };

        saved_state.restore(self);
        return result;
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
            if (self.ip >= self.chunk.code.len) return error.InvalidOpcode;
            const op = self.readOp();

            // Execute opcode with error handling
            self.executeOp(op) catch |err| {
                if (err == error.Halt) {
                    // Program terminated - return result from stack
                    return self.pop() catch Value.nil;
                }
                return self.doError(err);
            };
        }
    }

    /// Execute a single opcode
    fn executeOp(self: *Vm, op: Op) Error!void {
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
                if (idx >= self.chunk.constants.len) return error.InvalidConstant;
                try self.push(.{ .raw = self.chunk.constants[idx] });
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
                try self.push(self.stack[stack_idx]);
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
                if (idx >= MAX_GLOBALS) return error.InvalidConstant;
                try self.push(self.globals[idx]);
            },
            .store_global => {
                const idx = self.readU16();
                if (idx >= MAX_GLOBALS) return error.InvalidConstant;
                self.globals[idx] = try self.pop();
                if (idx >= self.num_globals) {
                    self.num_globals = idx + 1;
                }
            },
            .load_argc => {
                // Get argc from current frame, or from current_argc if fp=0 (callClosure)
                const frame_argc = if (self.fp > 0) self.frames[self.fp - 1].argc else self.current_argc;
                try self.push(Value.makeFixnum(frame_argc));
            },
            .find_key => {
                // Get keyword to search for from constant pool
                const kw_idx = self.readU16();
                if (kw_idx >= self.chunk.constants.len) return error.InvalidConstant;
                const keyword = Value{ .raw = self.chunk.constants[kw_idx] };

                // Get current frame info
                const frame = if (self.fp > 0) &self.frames[self.fp - 1] else null;
                if (frame) |f| {
                    const chunk: *const Chunk = @ptrCast(@alignCast(f.closure.?.code));
                    // Layout: [positional] [key params] [keyword pairs]
                    // Keyword pairs start after positional + key param slots
                    const max_positional = chunk.arity + chunk.optional_count;
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
                try self.push(if (primitives.arith.lt(a, b)) Value.t else Value.nil);
            },
            .gt => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (primitives.arith.gt(a, b)) Value.t else Value.nil);
            },
            .le => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (primitives.arith.le(a, b)) Value.t else Value.nil);
            },
            .ge => {
                const b = try self.pop();
                const a = try self.pop();
                try self.push(if (primitives.arith.ge(a, b)) Value.t else Value.nil);
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
                if (!pair.isCons()) return error.TypeMismatch;
                try self.push(pair.toPtr(Cons).car);
            },
            .cdr => {
                const pair = try self.pop();
                if (!pair.isCons()) return error.TypeMismatch;
                try self.push(pair.toPtr(Cons).cdr);
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
                if (list1.isNil()) {
                    try self.push(list2);
                } else if (!list1.isCons()) {
                    return error.TypeMismatch;
                } else {
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
                }
            },

            .list_length => {
                const list = try self.pop();
                var len: i64 = 0;
                var curr = list;
                while (curr.isCons()) {
                    len += 1;
                    curr = curr.toPtr(Cons).cdr;
                }
                try self.push(Value.makeFixnum(len));
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
                if (list.isNil()) {
                    try self.push(Value.nil);
                } else if (!list.isCons()) {
                    return error.TypeMismatch;
                } else {
                    var curr = list;
                    while (curr.isCons()) {
                        const c = curr.toPtr(Cons);
                        if (!c.cdr.isCons()) {
                            try self.push(curr);
                            break;
                        }
                        curr = c.cdr;
                    }
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
                try self.push(if (a.isFixnum()) Value.t else Value.nil);
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
            .vec_len => {
                const vec_val = try self.pop();
                if (!vec_val.isVector()) return error.TypeMismatch;
                const vec = vec_val.toPtr(runtime.Vector);
                try self.push(Value.makeFixnum(@intCast(vec.length)));
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

                // Create closure
                const closure = try self.allocClosureWithGC(
                    @ptrCast(closure_chunk),
                    closure_chunk.arity,
                    captures[0..num_captures],
                );

                try self.push(closure);
            },

            // I/O
            .print => {
                const val = try self.pop();
                io.printValue(val) catch return error.Halt;
                io.sysNewline() catch return error.Halt;
                try self.push(val); // Return the printed value
            },
            .princ => {
                const val = try self.pop();
                io.princValue(val) catch return error.Halt;
                // Note: no newline for princ
                try self.push(val); // Return the printed value
            },
            .terpri => {
                io.sysNewline() catch return error.Halt;
                try self.push(Value.nil);
            },
            .write_char => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                const cp = val.toCharacter();
                if (cp < 128) {
                    io.sysWriteChar(@intCast(cp)) catch return error.Halt;
                } else {
                    // UTF-8 encode for non-ASCII
                    var buf: [4]u8 = undefined;
                    const len = std.unicode.utf8Encode(@intCast(cp), &buf) catch 0;
                    io.sysWriteBytes(buf[0..len]) catch return error.Halt;
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
                // Parse integer from string
                var result: i64 = 0;
                var negative = false;
                var i: usize = 0;
                var overflow = false;
                if (bytes.len > 0 and bytes[0] == '-') {
                    negative = true;
                    i = 1;
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
                io.writeValueToBuffer(val, fbs.writer().any()) catch {
                    try self.push(Value.nil);
                    return;
                };
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
            .read_file => {
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                const result = io.readFile(self.heap, path_str.bytes()) catch {
                    try self.push(Value.nil);
                    return;
                };
                try self.push(result);
            },
            .write_file => {
                const content_val = try self.pop();
                const path_val = try self.pop();
                if (!path_val.isString()) return error.TypeMismatch;
                const path_str = path_val.toPtr(String);
                io.writeFile(path_str.bytes(), content_val) catch {
                    try self.push(Value.nil);
                    return;
                };
                try self.push(Value.nil);
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
            .string_to_list => {
                const str_val = try self.pop();
                if (!str_val.isString()) return error.TypeMismatch;
                const str = str_val.toPtr(String);
                const bytes = str.bytes();
                // Build list in reverse, then result is in correct order
                var result = Value.nil;
                var i: usize = bytes.len;
                while (i > 0) {
                    i -= 1;
                    const char = Value.makeCharacter(bytes[i]);
                    result = try self.allocCons(char, result);
                }
                try self.push(result);
            },
            .list_to_string => {
                const list_val = try self.pop();
                // Count length first
                var len: usize = 0;
                var p = list_val;
                while (p != Value.nil) {
                    if (!p.isCons()) return error.TypeMismatch;
                    const c = p.toPtr(Cons);
                    if (!c.car.isCharacter()) return error.TypeMismatch;
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
                    const cp = c.car.toCharacter();
                    // Only ASCII/Latin-1 characters fit in a byte
                    if (cp > 255) return error.TypeMismatch;
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
                const result = arith.random(n) catch return error.InvalidArgument;
                try self.push(result);
            },
            .random_seed => {
                const seed = try self.pop();
                const result = arith.randomSeed(seed) catch return error.TypeMismatch;
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
                const result = stringPrims.substring(self.heap, str_val, start, end) catch return error.OutOfMemory;
                try self.push(result);
            },
            .sym_name => {
                const sym_val = try self.pop();
                // Handle magic symbols nil and t
                if (sym_val.isNil()) {
                    const name_str = try self.allocString("nil");
                    try self.push(name_str);
                } else if (sym_val.isT()) {
                    const name_str = try self.allocString("t");
                    try self.push(name_str);
                } else if (sym_val.isSymbol()) {
                    const sym = sym_val.toPtr(Symbol);
                    const name_str = try self.allocString(sym.getName());
                    try self.push(name_str);
                } else {
                    return error.TypeMismatch;
                }
            },
            .type_of => {
                const val = try self.pop();
                // Return symbol naming the type
                const type_name: []const u8 = if (val.isNil())
                    "nil"
                else if (val.isFixnum())
                    "fixnum"
                else if (val.isCharacter())
                    "character"
                else if (val.isCons())
                    "cons"
                else if (val.isSymbol())
                    "symbol"
                else if (val.isVector())
                    "vector"
                else if (val.isString())
                    "string"
                else if (val.isClosure())
                    "closure"
                else if (val.isKeyword())
                    "keyword"
                else if (val.isHashTable())
                    "hash-table"
                else
                    "unknown";
                const type_sym = try self.heap.intern(type_name);
                try self.push(type_sym);
            },
            .str_eq => {
                const b = try self.pop();
                const a = try self.pop();
                const result = if (stringPrims.stringEqual(a, b)) Value.t else Value.nil;
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
                // For normal exit, pop the unwind frame (doThrow/doError already popped for unwind case)
                if (!self.is_unwinding and self.unwind_sp > 0) {
                    self.unwind_sp -= 1;
                }
                // If we're unwinding (cleanup ran due to throw or error), re-throw/re-error
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

                // Primary value is already on stack
                // Now push secondary values (or nil if not enough)
                var i: usize = 1;
                while (i < count) : (i += 1) {
                    if (i - 1 < self.secondary_values_count) {
                        try self.push(self.secondary_values[i - 1]);
                    } else {
                        try self.push(Value.nil);
                    }
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
            .hash_get_default => {
                const default = try self.pop();
                const key = try self.pop();
                const ht_val = try self.pop();
                if (!ht_val.isHashTable()) return error.TypeMismatch;
                const ht = ht_val.toPtr(HashTable);
                const result = hashTableGet(ht, key);
                try self.push(if (result.isNil()) default else result);
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
                    if (!hashTableResizeInPlace(self, ht)) return error.OutOfMemory;
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
            .hashtablep => {
                const val = try self.pop();
                try self.push(if (val.isHashTable()) Value.t else Value.nil);
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
                const real_f = if (real.isFloat()) real.toFloat() else if (real.isFixnum()) @as(f64, @floatFromInt(real.toFixnum())) else return error.TypeMismatch;
                const imag_f = if (imag.isFloat()) imag.toFloat() else if (imag.isFixnum()) @as(f64, @floatFromInt(imag.toFixnum())) else return error.TypeMismatch;
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
                if (!stream_val.isStream()) return error.TypeMismatch;
                const stream = stream_val.toPtr(runtime.Stream);
                if (stream.stream_type != .string or stream.direction != .output) {
                    return error.TypeMismatch;
                }
                // For string output streams, we need to get accumulated data
                // Currently returns empty string since we don't have write operations yet
                const result = try self.heap.allocString("");
                try self.push(result);
            },
            .open => {
                const mode_val = try self.pop();
                const path_val = try self.pop();
                const result = primitives.stream.primOpen(self.heap, &[_]Value{ path_val, mode_val }) catch return error.UserError;
                try self.push(result);
            },
            .close => {
                const stream_val = try self.pop();
                const result = try primitives.stream.primClose(self.heap, &[_]Value{stream_val});
                try self.push(result);
            },

            .read_line => {
                const stream_val = try self.pop();
                const result = primitives.stream.primReadLine(self.heap, &[_]Value{stream_val}) catch return error.UserError;
                try self.push(result);
            },

            .write_line => {
                const text_val = try self.pop();
                const stream_val = try self.pop();
                const result = primitives.stream.primWriteLine(self.heap, &[_]Value{ stream_val, text_val }) catch return error.UserError;
                try self.push(result);
            },

            .read_byte => {
                const stream_val = try self.pop();
                const result = primitives.stream.primReadByte(self.heap, &[_]Value{stream_val}) catch return error.UserError;
                try self.push(result);
            },

            .write_byte => {
                const byte_val = try self.pop();
                const stream_val = try self.pop();
                const result = primitives.stream.primWriteByte(self.heap, &[_]Value{ stream_val, byte_val }) catch return error.UserError;
                try self.push(result);
            },

            .file_position => {
                const stream_val = try self.pop();
                const result = primitives.stream.primFilePosition(self.heap, &[_]Value{stream_val}) catch return error.UserError;
                try self.push(result);
            },

            .file_length => {
                const stream_val = try self.pop();
                const result = primitives.stream.primFileLength(self.heap, &[_]Value{stream_val}) catch return error.UserError;
                try self.push(result);
            },

            .finish_output => {
                const stream_val = try self.pop();
                const result = primitives.stream.primFinishOutput(self.heap, &[_]Value{stream_val}) catch return error.UserError;
                try self.push(result);
            },

            .force_output => {
                const stream_val = try self.pop();
                const result = primitives.stream.primForceOutput(self.heap, &[_]Value{stream_val}) catch return error.UserError;
                try self.push(result);
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

                // Pop array
                const arr_val = try self.pop();
                if (!arr_val.isArray()) return error.TypeMismatch;
                const arr = arr_val.toPtr(runtime.Array);

                // Verify rank matches
                if (arr.rank != sub_count) return error.TypeMismatch;

                // Calculate linear index using row-major order
                // index = s0*d1*d2*... + s1*d2*d3*... + ... + sN
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

                // Pop array
                const arr_val = try self.pop();
                if (!arr_val.isArray()) return error.TypeMismatch;
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

                // If already a pathname, return it
                if (pathspec.isPathname()) {
                    try self.push(pathspec);
                } else if (pathspec.isString()) {
                    // Parse string as pathname
                    // For now, just use the string as the name component
                    const bytes = try self.heap.allocRaw(@sizeOf(runtime.Pathname));
                    const pn: *runtime.Pathname = @ptrCast(@alignCast(bytes));

                    pn.* = .{
                        .kind = .pathname,
                        .host = Value.nil,
                        .device = Value.nil,
                        .directory = Value.nil,
                        .name = pathspec,
                        .type = Value.nil,
                        .version = Value.nil,
                    };

                    try self.push(Value.makePathname(pn));
                } else if (pathspec.isStream()) {
                    // Return stream's pathname (not implemented yet)
                    // For now, return nil
                    try self.push(Value.nil);
                } else {
                    return error.TypeMismatch;
                }
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
                            const comp_str = try self.heap.allocString(component);
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
                                name = try self.heap.allocString(fname[0..dot_pos]);
                                type_comp = try self.heap.allocString(fname[dot_pos + 1 ..]);
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
                        if (first.isKeyword()) {
                            const kw = first.toPtr(runtime.Keyword);
                            const kw_name = kw.getName();
                            if (std.mem.eql(u8, kw_name, "absolute")) {
                                try result.append(self.allocator, '/');
                            }
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
                const result_str = try self.heap.allocString(result.items);
                try self.push(result_str);
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

            .pathname_host => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;
                const pn = pn_val.toPtr(runtime.Pathname);
                try self.push(pn.host);
            },

            .pathname_device => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;
                const pn = pn_val.toPtr(runtime.Pathname);
                try self.push(pn.device);
            },

            .pathname_directory => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;
                const pn = pn_val.toPtr(runtime.Pathname);
                try self.push(pn.directory);
            },

            .pathname_name => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;
                const pn = pn_val.toPtr(runtime.Pathname);
                try self.push(pn.name);
            },

            .pathname_type => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;
                const pn = pn_val.toPtr(runtime.Pathname);
                try self.push(pn.type);
            },

            .pathname_version => {
                const pn_val = try self.pop();
                if (!pn_val.isPathname()) return error.TypeMismatch;
                const pn = pn_val.toPtr(runtime.Pathname);
                try self.push(pn.version);
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
                // find with eql test (default)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .eql)) {
                        try self.push(c.car);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },
            .list_find_eq => {
                // find with eq test (identity)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.raw == item.raw) {
                        try self.push(c.car);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },
            .list_find_equal => {
                // find with equal test (structural)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .equal)) {
                        try self.push(c.car);
                        break;
                    }
                    curr = c.cdr;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_position => {
                // position with eql test (default)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                var idx: i64 = 0;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .eql)) {
                        try self.push(Value.makeFixnum(idx));
                        break;
                    }
                    curr = c.cdr;
                    idx += 1;
                } else {
                    try self.push(Value.nil);
                }
            },
            .list_position_eq => {
                // position with eq test (identity)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                var idx: i64 = 0;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.raw == item.raw) {
                        try self.push(Value.makeFixnum(idx));
                        break;
                    }
                    curr = c.cdr;
                    idx += 1;
                } else {
                    try self.push(Value.nil);
                }
            },
            .list_position_equal => {
                // position with equal test (structural)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                var idx: i64 = 0;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .equal)) {
                        try self.push(Value.makeFixnum(idx));
                        break;
                    }
                    curr = c.cdr;
                    idx += 1;
                } else {
                    try self.push(Value.nil);
                }
            },

            .list_count => {
                // count with eql test (default)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                var n: i64 = 0;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .eql)) {
                        n += 1;
                    }
                    curr = c.cdr;
                }
                try self.push(Value.makeFixnum(n));
            },
            .list_count_eq => {
                // count with eq test (identity)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                var n: i64 = 0;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (c.car.raw == item.raw) {
                        n += 1;
                    }
                    curr = c.cdr;
                }
                try self.push(Value.makeFixnum(n));
            },
            .list_count_equal => {
                // count with equal test (structural)
                const seq = try self.pop();
                const item = try self.pop();
                var curr = seq;
                var n: i64 = 0;
                while (curr.isCons()) {
                    const c = curr.toPtr(Cons);
                    if (hashKeyEqualWithTest(c.car, item, .equal)) {
                        n += 1;
                    }
                    curr = c.cdr;
                }
                try self.push(Value.makeFixnum(n));
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
                const ch = io.sysReadChar() catch -1;
                if (ch < 0) {
                    try self.push(Value.makeFixnum(-1));
                } else {
                    try self.push(Value.makeCharacter(@intCast(ch)));
                }
            },
            .peek_char => {
                const ch = io.sysPeekChar() catch -1;
                if (ch < 0) {
                    try self.push(Value.makeFixnum(-1));
                } else {
                    try self.push(Value.makeCharacter(@intCast(ch)));
                }
            },

            .read => {
                // Read a complete S-expression from stdin
                var buffer: [4096]u8 = undefined;
                const len = io.sysReadSexp(&buffer) catch {
                    // EOF or error - return nil
                    try self.push(Value.nil);
                    return;
                };

                // Parse the S-expression
                var parser = Parser.init(self.allocator, self.heap, buffer[0..len]);
                const result = parser.parse() catch {
                    try self.push(Value.nil);
                    return;
                };
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
                var parser = Parser.init(self.allocator, self.heap, str.bytes());
                const result = parser.parse() catch {
                    try self.push(Value.nil);
                    return;
                };
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
                // Generate a unique symbol
                var buf: [32]u8 = undefined;
                const name = std.fmt.bufPrint(&buf, "G{d}", .{self.gensym_counter}) catch {
                    try self.push(Value.nil);
                    return;
                };
                self.gensym_counter = std.math.add(u64, self.gensym_counter, 1) catch {
                    return error.OutOfMemory; // Overflow after 2^64 gensyms
                };
                const sym = try self.allocSymbol(name);
                try self.push(sym);
            },

            .macroexpand => {
                // Expand macros in expression
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

            .unread_char => {
                const val = try self.pop();
                if (!val.isCharacter()) return error.TypeMismatch;
                io.sysUnreadChar(@intCast(val.toCharacter()));
                try self.push(Value.nil);
            },

            .boundp, .fboundp => {
                const val = try self.pop();
                if (!val.isSymbol()) return error.TypeMismatch;
                const sym = val.toPtr(Symbol);
                const name = sym.getName();
                // Check if symbol exists in global environment
                const is_bound = if (self.global_env) |env|
                    env.lookup(name) != null
                else
                    false;
                try self.push(if (is_bound) Value.t else Value.nil);
            },

            .symbol_value, .symbol_function => {
                const val = try self.pop();
                if (!val.isSymbol()) return error.TypeMismatch;
                const sym = val.toPtr(Symbol);
                const name = sym.getName();
                // Look up symbol in global environment
                if (self.global_env) |env| {
                    if (env.lookup(name)) |idx| {
                        try self.push(self.globals[idx]);
                    } else {
                        return error.UnboundSymbol;
                    }
                } else {
                    return error.UnboundSymbol;
                }
            },

            .typep => {
                const type_spec = try self.pop();
                const obj = try self.pop();
                if (!type_spec.isSymbol()) return error.TypeMismatch;

                // Use symbol identity for type dispatch (no string comparison)
                const ts = self.type_syms;
                const matches = if (type_spec.raw == ts.fixnum.raw or type_spec.raw == ts.number.raw)
                    obj.isFixnum()
                else if (type_spec.raw == ts.cons.raw)
                    obj.isCons()
                else if (type_spec.raw == ts.symbol.raw)
                    obj.isSymbol()
                else if (type_spec.raw == ts.string.raw)
                    obj.isString()
                else if (type_spec.raw == ts.vector.raw)
                    obj.isVector()
                else if (type_spec.raw == ts.closure.raw or type_spec.raw == ts.function.raw)
                    obj.isClosure()
                else if (type_spec.raw == ts.keyword.raw)
                    obj.isKeyword()
                else if (type_spec.raw == ts.character.raw)
                    obj.isCharacter()
                else if (type_spec.raw == ts.@"hash-table".raw)
                    obj.isHashTable()
                else if (type_spec.raw == ts.nil.raw or type_spec.raw == ts.null.raw)
                    obj.isNil()
                else if (type_spec.raw == ts.list.raw)
                    obj.isNil() or obj.isCons()
                else if (type_spec.raw == ts.atom.raw)
                    !obj.isCons()
                else if (type_spec.raw == ts.t.raw)
                    true // Everything is of type t
                else blk: {
                    // Check for user-defined struct types
                    // Structs are vectors with type name as first element
                    if (obj.isVector()) {
                        const vec = obj.toPtr(runtime.Vector);
                        if (vec.length > 0) {
                            const type_tag = vec.data[0];
                            break :blk type_tag.raw == type_spec.raw;
                        }
                    }
                    break :blk false;
                };

                try self.push(if (matches) Value.t else Value.nil);
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

            .halt => return error.Halt,
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

    /// Handle an error by running unwind-protect cleanup if needed
    fn doError(self: *Vm, err: Error) Error {
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

            // Return original error to signal cleanup is running
            // pop_unwind will re-throw the error after cleanup
            return err;
        }

        // No unwind frames - propagate error normally
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
                const directive = fmt[i + 1];
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
                        // Decimal integer
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isFixnum()) {
                                var buf: [32]u8 = undefined;
                                const num_str = std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()}) catch return error.OutOfMemory;
                                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
                            }
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'X', 'x' => {
                        // Hexadecimal integer
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isFixnum()) {
                                var buf: [32]u8 = undefined;
                                const n = val.toFixnum();
                                const num_str = if (n >= 0)
                                    std.fmt.bufPrint(&buf, "{X}", .{@as(u64, @intCast(n))}) catch return error.OutOfMemory
                                else
                                    std.fmt.bufPrint(&buf, "-{X}", .{@as(u64, @intCast(-n))}) catch return error.OutOfMemory;
                                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
                            }
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'B', 'b' => {
                        // Binary integer
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isFixnum()) {
                                var buf: [80]u8 = undefined;
                                const n = val.toFixnum();
                                const num_str = if (n >= 0)
                                    std.fmt.bufPrint(&buf, "{b}", .{@as(u64, @intCast(n))}) catch return error.OutOfMemory
                                else
                                    std.fmt.bufPrint(&buf, "-{b}", .{@as(u64, @intCast(-n))}) catch return error.OutOfMemory;
                                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
                            }
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    'O', 'o' => {
                        // Octal integer
                        if (arg_idx < args.len) {
                            const val = args[arg_idx];
                            if (val.isFixnum()) {
                                var buf: [32]u8 = undefined;
                                const n = val.toFixnum();
                                const num_str = if (n >= 0)
                                    std.fmt.bufPrint(&buf, "{o}", .{@as(u64, @intCast(n))}) catch return error.OutOfMemory
                                else
                                    std.fmt.bufPrint(&buf, "-{o}", .{@as(u64, @intCast(-n))}) catch return error.OutOfMemory;
                                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
                            }
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
                                    result.append(self.allocator, @intCast(cp)) catch return error.OutOfMemory;
                                } else {
                                    // UTF-8 encode
                                    var buf: [4]u8 = undefined;
                                    const len = std.unicode.utf8Encode(cp, &buf) catch 0;
                                    result.appendSlice(self.allocator, buf[0..len]) catch return error.OutOfMemory;
                                }
                            }
                            arg_idx += 1;
                        }
                        i += 2;
                    },
                    '%' => {
                        // Newline
                        result.append(self.allocator, '\n') catch return error.OutOfMemory;
                        i += 2;
                    },
                    '&' => {
                        // Fresh line - newline only if not at start of line
                        if (result.items.len > 0 and result.items[result.items.len - 1] != '\n') {
                            result.append(self.allocator, '\n') catch return error.OutOfMemory;
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
                                mincol = std.fmt.parseInt(usize, param_str[0..comma_pos], 10) catch 1;
                                colinc = std.fmt.parseInt(usize, param_str[comma_pos + 1 ..], 10) catch 1;
                            } else {
                                // Just mincol
                                mincol = std.fmt.parseInt(usize, param_str, 10) catch 1;
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
                            result.append(self.allocator, ' ') catch return error.OutOfMemory;
                        }

                        i = j;
                    },
                    '~' => {
                        // Literal tilde
                        result.append(self.allocator, '~') catch return error.OutOfMemory;
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
                            skip_count = std.fmt.parseInt(usize, param_str, 10) catch 1;
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
                                result.append(self.allocator, 's') catch return error.OutOfMemory;
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
                                result.append(self.allocator, body[j]) catch return error.OutOfMemory;
                                j += 1;
                            } else {
                                result.append(self.allocator, body[j]) catch return error.OutOfMemory;
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
                                        clauses.append(self.allocator, body[clause_start..j]) catch return error.OutOfMemory;
                                        clause_start = j + 2;
                                        j += 2;
                                    } else {
                                        j += 1;
                                    }
                                } else {
                                    j += 1;
                                }
                            }
                            clauses.append(self.allocator, body[clause_start..]) catch return error.OutOfMemory;
                            // Get selector for boolean conditional
                            if (arg_idx < args.len) {
                                const selector = args[arg_idx];
                                arg_idx += 1;
                                // nil = clause 0, non-nil = clause 1
                                const clause_idx: usize = if (selector.isNil()) 0 else 1;
                                if (clause_idx < clauses.items.len) {
                                    result.appendSlice(self.allocator, clauses.items[clause_idx]) catch return error.OutOfMemory;
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
                                    clauses.append(self.allocator, body[clause_start..j]) catch return error.OutOfMemory;
                                    clause_start = j + 2;
                                    j += 2;
                                } else {
                                    j += 1;
                                }
                            } else {
                                j += 1;
                            }
                        }
                        clauses.append(self.allocator, body[clause_start..]) catch return error.OutOfMemory;
                        // Get selector
                        if (arg_idx < args.len) {
                            const selector = args[arg_idx];
                            arg_idx += 1;
                            var clause_idx: usize = 0;
                            if (selector.isFixnum()) {
                                const n = selector.toFixnum();
                                if (n >= 0) clause_idx = @intCast(n);
                            } else if (selector.isNil()) {
                                clause_idx = 0; // For ~:[false~;true~], nil selects first
                            } else {
                                clause_idx = 1; // Non-nil selects second (for boolean conditional)
                            }
                            if (clause_idx < clauses.items.len) {
                                // Append the selected clause text directly
                                // (for full CL compat, would need recursive format processing)
                                result.appendSlice(self.allocator, clauses.items[clause_idx]) catch return error.OutOfMemory;
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
                        // Justification: ~<...~>
                        // Basic implementation: just output content (no justification yet)
                        const start = i + 2;
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
                            // Unmatched ~<, skip it
                            i += 2;
                            continue;
                        }

                        // For now, just output the content without justification
                        const content = fmt[start..end];
                        result.appendSlice(self.allocator, content) catch return error.OutOfMemory;
                        i = end + 2; // Skip past ~>
                    },
                    else => {
                        // Unknown directive, output as-is
                        result.append(self.allocator, fmt[i]) catch return error.OutOfMemory;
                        i += 1;
                    },
                }
            } else {
                result.append(self.allocator, fmt[i]) catch return error.OutOfMemory;
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
            w.writeAll(result.items) catch return error.OutOfMemory;
            w.flush() catch return error.OutOfMemory;
            return Value.nil;
        }
    }

    fn formatValueAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        switch (val.typeKind()) {
            .nil => result.appendSlice(self.allocator, "nil") catch return error.OutOfMemory,
            .t => result.appendSlice(self.allocator, "t") catch return error.OutOfMemory,
            .fixnum => {
                var buf: [32]u8 = undefined;
                const num_str = std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()}) catch return error.OutOfMemory;
                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
            },
            .float => {
                var buf: [64]u8 = undefined;
                const num_str = std.fmt.bufPrint(&buf, "{d}", .{val.toFloat()}) catch return error.OutOfMemory;
                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
            },
            .char => {
                const cp = val.toCharacter();
                if (cp < 128) {
                    result.append(self.allocator, @as(u8, @intCast(cp))) catch return error.OutOfMemory;
                }
            },
            .string => result.appendSlice(self.allocator, val.toPtr(runtime.String).bytes()) catch return error.OutOfMemory,
            .symbol => result.appendSlice(self.allocator, val.toPtr(Symbol).getName()) catch return error.OutOfMemory,
            .keyword => {
                result.append(self.allocator, ':') catch return error.OutOfMemory;
                result.appendSlice(self.allocator, val.toPtr(runtime.Keyword).getName()) catch return error.OutOfMemory;
            },
            .cons => try self.formatListAesthetic(val, result),
            .closure => result.appendSlice(self.allocator, "#<closure>") catch return error.OutOfMemory,
            .vector => result.appendSlice(self.allocator, "#<vector>") catch return error.OutOfMemory,
            .hashtable => result.appendSlice(self.allocator, "#<hash-table>") catch return error.OutOfMemory,
            .rational => {
                const rat = val.toPtr(runtime.Rational);
                var buf: [64]u8 = undefined;
                const num_str = std.fmt.bufPrint(&buf, "{d}/{d}", .{ rat.numerator, rat.denominator }) catch return error.OutOfMemory;
                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
            },
            .complex => {
                const cplx = val.toPtr(runtime.Complex);
                var buf: [128]u8 = undefined;
                const cplx_str = std.fmt.bufPrint(&buf, "#C({d} {d})", .{ cplx.real, cplx.imag }) catch return error.OutOfMemory;
                result.appendSlice(self.allocator, cplx_str) catch return error.OutOfMemory;
            },
            .stream => result.appendSlice(self.allocator, "#<stream>") catch return error.OutOfMemory,
            .bignum => result.appendSlice(self.allocator, "#<bignum>") catch return error.OutOfMemory,
            .array => result.appendSlice(self.allocator, "#<array>") catch return error.OutOfMemory,
            .pathname => result.appendSlice(self.allocator, "#<pathname>") catch return error.OutOfMemory,
        }
    }

    fn formatValueStandard(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        if (val.isString()) {
            // Strings get quoted
            result.append(self.allocator, '"') catch return error.OutOfMemory;
            const str = val.toPtr(runtime.String);
            result.appendSlice(self.allocator, str.bytes()) catch return error.OutOfMemory;
            result.append(self.allocator, '"') catch return error.OutOfMemory;
        } else {
            // Everything else same as aesthetic
            try self.formatValueAesthetic(val, result);
        }
    }

    const MAX_FORMAT_DEPTH = 1000;

    fn formatListAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) Error!void {
        result.append(self.allocator, '(') catch return error.OutOfMemory;
        var current = val;
        var first = true;
        var depth: usize = 0;
        while (current.isCons()) {
            // Prevent infinite loop on circular lists
            depth += 1;
            if (depth > MAX_FORMAT_DEPTH) {
                result.appendSlice(self.allocator, "...") catch return error.OutOfMemory;
                break;
            }
            if (!first) result.append(self.allocator, ' ') catch return error.OutOfMemory;
            first = false;
            const cons = current.toPtr(runtime.Cons);
            try self.formatValueAesthetic(cons.car, result);
            current = cons.cdr;
        }
        if (!current.isNil() and depth <= MAX_FORMAT_DEPTH) {
            result.appendSlice(self.allocator, " . ") catch return error.OutOfMemory;
            try self.formatValueAesthetic(current, result);
        }
        result.append(self.allocator, ')') catch return error.OutOfMemory;
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
                                const num_str = std.fmt.bufPrint(&buf, "{d}", .{elem.toFixnum()}) catch return error.OutOfMemory;
                                result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
                            }
                            i += 2;
                        },
                        '%' => {
                            result.append(self.allocator, '\n') catch return error.OutOfMemory;
                            i += 2;
                        },
                        '~' => {
                            result.append(self.allocator, '~') catch return error.OutOfMemory;
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
                            result.append(self.allocator, body[i]) catch return error.OutOfMemory;
                            i += 1;
                        },
                    }
                } else {
                    result.append(self.allocator, body[i]) catch return error.OutOfMemory;
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
        const fn_val = self.stack[self.sp - argc - 1];

        if (!fn_val.isClosure()) {
            return error.TypeMismatch;
        }

        const closure = fn_val.toPtr(runtime.Closure);
        const callee_chunk: *const Chunk = @ptrCast(@alignCast(closure.code));
        const arity = callee_chunk.arity;
        const optional_count = callee_chunk.optional_count;
        const key_count = callee_chunk.key_count;
        const max_positional = arity + optional_count;

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
        if (callee_chunk.has_rest) {
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
        } else if (optional_count > 0) {
            // Has optional params: argc must be in [arity, arity + optional_count]
            if (argc < arity or argc > max_positional) {
                return error.TypeMismatch;
            }
        } else {
            // Fixed: need exact arity
            if (argc != arity) {
                return error.TypeMismatch;
            }
        }

        // Build rest list if variadic (before we modify the stack)
        // Rest list contains args beyond required + optional + key params
        var rest_list = Value.nil;
        if (callee_chunk.has_rest and argc > max_positional) {
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
            if (callee_chunk.has_rest) {
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
            const used_locals: usize = actual_argc + @as(u8, if (callee_chunk.has_rest) 1 else 0);
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
            if (callee_chunk.has_rest) {
                try self.push(rest_list);
            }

            // Update frame bp
            self.frames[self.fp - 1].bp = new_bp;

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Reserve space for additional locals (after args + rest)
            const used: usize = actual_argc + @as(u8, if (callee_chunk.has_rest) 1 else 0);
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

    // ========================================================================
    // Bytecode reading
    // ========================================================================

    fn readOp(self: *Vm) Op {
        const byte = self.chunk.code[self.ip];
        self.ip += 1;
        return @enumFromInt(byte);
    }

    fn readU8(self: *Vm) u8 {
        const byte = self.chunk.code[self.ip];
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
    if (a.isCons()) {
        // Recursively compare car and cdr
        const cons_a = a.toPtr(Cons);
        const cons_b = b.toPtr(Cons);
        return valueEqualWithDepth(cons_a.car, cons_b.car, depth + 1) and
            valueEqualWithDepth(cons_a.cdr, cons_b.cdr, depth + 1);
    } else if (a.isString()) {
        // Compare strings character by character
        const str_a = a.toPtr(String);
        const str_b = b.toPtr(String);
        return std.mem.eql(u8, str_a.bytes(), str_b.bytes());
    } else if (a.isVector()) {
        // Compare vectors element by element
        const vec_a = a.toPtr(Vector);
        const vec_b = b.toPtr(Vector);
        if (vec_a.length != vec_b.length) return false;
        for (vec_a.items(), vec_b.items()) |ea, eb| {
            if (!valueEqualWithDepth(ea, eb, depth + 1)) return false;
        }
        return true;
    }
    // Symbols, closures, keywords: use eq
    return false;
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
                .nil, .t, .fixnum, .char => fnvHashU64(val.raw),
                .float => fnvHashU64(normalizeFloatForHash(val.toFloat())),
                .symbol => fnvHash(val.toPtr(runtime.Symbol).getName()),
                .keyword => fnvHash(val.toPtr(runtime.Keyword).getName()),
                .string => fnvHash(val.toPtr(runtime.String).bytes()),
                // Reference types: hash address (NOT stable across GC)
                .cons, .vector, .closure, .hashtable, .rational, .complex, .stream, .bignum, .array, .pathname => fnvHashU64(val.raw),
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
/// Returns true on success, false if allocation failed
fn hashTableResizeInPlace(vm: *Vm, ht: *HashTable) bool {
    const new_capacity = ht.capacity * 2;
    // Preserve the test_type from the original hash table
    const new_ht_val = vm.allocHashTable(new_capacity, ht.test_type) catch return false;
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

    return true;
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

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    const code = [_]u8{
        @intFromEnum(Op.push_i32),
        42,
        0,
        0,
        0,
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm arithmetic" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // (+ 10 20) = 30
    const code = [_]u8{
        @intFromEnum(Op.push_i32), 10,                   0, 0, 0,
        @intFromEnum(Op.push_i32), 20,                   0, 0, 0,
        @intFromEnum(Op.add),      @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "vm cons car cdr" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // (car (cons 1 2)) = 1
    const code = [_]u8{
        @intFromEnum(Op.push_i32), 1,                    0,                    0, 0,
        @intFromEnum(Op.push_i32), 2,                    0,                    0, 0,
        @intFromEnum(Op.cons),     @intFromEnum(Op.car), @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "vm conditional" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // (if nil 1 2) = 2
    const code = [_]u8{
        @intFromEnum(Op.push_nil),
        @intFromEnum(Op.jmp_nil),  8, 0, // Jump 8 bytes if nil
        @intFromEnum(Op.push_i32), 1, 0,
        0,                         0,
        @intFromEnum(Op.jmp),      5, 0, // Jump 5 bytes over else
        @intFromEnum(Op.push_i32), 2, 0,
        0,                         0, @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 0,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "vm locals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Store 42 in local 0, load it back
    const code = [_]u8{
        @intFromEnum(Op.push_i32),    42, 0,                           0, 0,
        @intFromEnum(Op.store_local), 0,  @intFromEnum(Op.load_local), 0, @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 1,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "vm hash table" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Create hash table, set key 42 to value 100, get key 42
    // Use local 0 to store ht
    const code = [_]u8{
        // make_hash with capacity 16, test_type eql (1), store in local 0
        @intFromEnum(Op.make_hash),   16,                        0,                           1,
        @intFromEnum(Op.store_local), 0,
        // load ht, push key (42), push value (100), hash_set
                                @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.push_i32),    42,                        0,                           0,
        0,                            @intFromEnum(Op.push_i32), 100,                         0,
        0,                            0,
        @intFromEnum(Op.hash_set), // pushes value back
        @intFromEnum(Op.pop), // discard returned value
        // load ht, push key, hash_get
        @intFromEnum(Op.load_local),
        0,
        @intFromEnum(Op.push_i32),
        42,
        0,
        0,
        0,
        @intFromEnum(Op.hash_get),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 1,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 100), result.toFixnum());
}

test "vm hash table count and remove" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);

    // Create hash table, set 2 keys, get count
    const code = [_]u8{
        // make_hash with capacity 16, test_type eql (1)
        @intFromEnum(Op.make_hash),   16,                        0,                           1,
        // store in local 0
        @intFromEnum(Op.store_local), 0,
        // Set key 1 -> 10
                                @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.push_i32),    1,                         0,                           0,
        0,                            @intFromEnum(Op.push_i32), 10,                          0,
        0,                            0,                         @intFromEnum(Op.hash_set),
        @intFromEnum(Op.pop), // discard returned value
        // Set key 2 -> 20
        @intFromEnum(Op.load_local),
        0,
        @intFromEnum(Op.push_i32),
        2,
        0,
        0,
        0,
        @intFromEnum(Op.push_i32),
        20,
        0,
        0,
        0,
        @intFromEnum(Op.hash_set),
        @intFromEnum(Op.pop), // discard returned value
        // Get count (should be 2)
        @intFromEnum(Op.load_local),
        0,
        @intFromEnum(Op.hash_count),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .optional_count = 0,
        .key_count = 0,
        .has_rest = false,
        .num_locals = 1,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}
