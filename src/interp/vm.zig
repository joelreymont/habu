//! Bytecode Virtual Machine for Habu
//!
//! Stack-based interpreter that executes bytecode.
//! Designed for portability (WASM target).

const std = @import("std");
const bytecode = @import("../bytecode/bytecode.zig");
const Op = bytecode.Op;
const Chunk = bytecode.Chunk;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const String = runtime.String;
const Symbol = runtime.Symbol;
const arith = @import("../runtime/primitives/arith.zig");
const io = @import("../runtime/primitives/io.zig");
const stringPrims = @import("../runtime/primitives/string.zig");
const HashTable = runtime.HashTable;
const compiler = @import("../compiler/compiler.zig");
const GlobalEnv = compiler.GlobalEnv;
const Parser = @import("../reader/parser.zig").Parser;

pub const VmError = error{
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

/// Call frame for function calls
pub const Frame = struct {
    /// Return address (chunk + ip)
    chunk: *const Chunk,
    return_ip: usize,
    /// Base pointer (stack index of first local)
    bp: usize,
    /// Current closure (for accessing captures)
    closure: ?*const runtime.Closure,
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

    /// Saved throw state for unwinding through unwind-protect
    pending_throw_tag: Value,
    pending_throw_value: Value,
    is_unwinding: bool,

    /// Secondary values buffer for multiple-value-bind
    secondary_values: [MAX_SECONDARY_VALUES]Value,
    /// Number of secondary values currently available
    secondary_values_count: usize,

    /// Global environment for boundp/fboundp lookups
    global_env: ?*const GlobalEnv,

    /// Callback for (load "filename") - set by REPL
    load_callback: ?*const fn ([]const u8, *anyopaque) VmError!Value,
    load_context: ?*anyopaque,

    /// Callback for (eval expr) - set by REPL
    eval_callback: ?*const fn (Value, *anyopaque) VmError!Value,
    eval_context: ?*anyopaque,

    /// Callback for (macroexpand expr) - set by REPL
    macroexpand_callback: ?*const fn (Value, *anyopaque) VmError!Value,
    macroexpand_context: ?*anyopaque,

    /// Counter for gensym
    gensym_counter: u64,

    const STACK_SIZE = 1024;
    const MAX_SECONDARY_VALUES = 20;
    const MAX_FRAMES = 64;
    const MAX_GLOBALS = 256;
    const MAX_CATCHES = 32;
    const MAX_UNWINDS = 32;

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) Vm {
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
            .pending_throw_tag = Value.nil,
            .pending_throw_value = Value.nil,
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
    pub fn setLoadCallback(self: *Vm, callback: *const fn ([]const u8, *anyopaque) VmError!Value, context: *anyopaque) void {
        self.load_callback = callback;
        self.load_context = context;
    }

    /// Set the eval callback for (eval expr)
    pub fn setEvalCallback(self: *Vm, callback: *const fn (Value, *anyopaque) VmError!Value, context: *anyopaque) void {
        self.eval_callback = callback;
        self.eval_context = context;
    }

    /// Set the macroexpand callback for (macroexpand expr)
    pub fn setMacroexpandCallback(self: *Vm, callback: *const fn (Value, *anyopaque) VmError!Value, context: *anyopaque) void {
        self.macroexpand_callback = callback;
        self.macroexpand_context = context;
    }

    /// Run a chunk to completion
    pub fn run(self: *Vm, chunk: *const Chunk) VmError!Value {
        self.chunk = chunk;
        self.ip = 0;
        self.sp = 0;
        self.fp = 0;

        // Reserve space for locals
        var i: usize = 0;
        while (i < chunk.num_locals) : (i += 1) {
            try self.push(Value.nil);
        }

        return self.execute();
    }

    fn execute(self: *Vm) VmError!Value {
        while (true) {
            const op = self.readOp();

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

                // Variable access
                .load_local => {
                    const idx = self.readU8();
                    const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
                    try self.push(self.stack[bp + idx]);
                },
                .store_local => {
                    const idx = self.readU8();
                    const bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
                    self.stack[bp + idx] = try self.pop();
                },
                .load_capture => {
                    const idx = self.readU8();
                    // Get current closure from frame
                    if (self.fp > 0) {
                        if (self.frames[self.fp - 1].closure) |closure| {
                            if (idx < closure.num_captures) {
                                try self.push(closure.getCapture(idx));
                            } else {
                                return error.InvalidConstant;
                            }
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
                    // Get current frame's closure
                    if (self.fp > 0) {
                        if (self.frames[self.fp - 1].closure) |closure| {
                            if (index < closure.num_captures) {
                                try self.push(closure.getCapture(index));
                            } else {
                                return error.InvalidConstant;
                            }
                        } else {
                            return error.TypeMismatch; // No closure in frame
                        }
                    } else {
                        return error.TypeMismatch; // No frame
                    }
                },
                .store_upvalue => {
                    _ = self.readU8(); // depth (unused with flat closures)
                    const index = self.readU8();
                    const val = try self.pop();
                    // Get current frame's closure
                    if (self.fp > 0) {
                        if (self.frames[self.fp - 1].closure) |closure| {
                            if (index < closure.num_captures) {
                                // Note: captures array is mutable
                                const captures: [*]Value = @constCast(closure.captures);
                                captures[index] = val;
                            } else {
                                return error.InvalidConstant;
                            }
                        } else {
                            return error.TypeMismatch;
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

                // Arithmetic
                .add => try self.binaryOp(binaryAdd),
                .sub => try self.binaryOp(binarySub),
                .mul => try self.binaryOp(binaryMul),
                .div => try self.binaryOp(binaryDiv),
                .mod => try self.binaryOp(binaryMod),
                .neg => {
                    const a = try self.pop();
                    if (!a.isFixnum()) return error.TypeMismatch;
                    try self.push(Value.makeFixnum(-a.toFixnum()));
                },

                // Comparison
                .eq => {
                    const b = try self.pop();
                    const a = try self.pop();
                    try self.push(if (a.eq(b)) Value.t else Value.nil);
                },
                .lt => try self.binaryOp(binaryLt),
                .gt => try self.binaryOp(binaryGt),
                .le => try self.binaryOp(binaryLe),
                .ge => try self.binaryOp(binaryGe),
                .num_eq => try self.binaryOp(binaryNumEq),
                .not => {
                    const a = try self.pop();
                    try self.push(if (a.isNil()) Value.t else Value.nil);
                },

                // List operations
                .cons => {
                    const cdr = try self.pop();
                    const car = try self.pop();
                    const cell = self.heap.allocCons(car, cdr) orelse return error.OutOfMemory;
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
                    var list = Value.nil;
                    var i: usize = 0;
                    while (i < count) : (i += 1) {
                        // Pop from end to build list in order
                        const elem = self.stack[self.sp - count + i];
                        list = self.heap.allocCons(elem, list) orelse return error.OutOfMemory;
                    }
                    self.sp -= count;
                    // Reverse the list to get correct order
                    var reversed = Value.nil;
                    while (list.isCons()) {
                        const c = list.toPtr(Cons);
                        reversed = self.heap.allocCons(c.car, reversed) orelse return error.OutOfMemory;
                        list = c.cdr;
                    }
                    try self.push(reversed);
                },
                .append_lists => {
                    const list2 = try self.pop();
                    const list1 = try self.pop();
                    // Append list1 to list2: (append '(a b) '(c d)) -> (a b c d)
                    // Copy list1, set last cdr to list2
                    if (list1.isNil()) {
                        try self.push(list2);
                    } else if (!list1.isCons()) {
                        return error.TypeMismatch;
                    } else {
                        // Build reversed copy of list1
                        var reversed = Value.nil;
                        var curr = list1;
                        while (curr.isCons()) {
                            const c = curr.toPtr(Cons);
                            reversed = self.heap.allocCons(c.car, reversed) orelse return error.OutOfMemory;
                            curr = c.cdr;
                        }
                        // Build result by consing reversed elements onto list2
                        var result = list2;
                        while (reversed.isCons()) {
                            const c = reversed.toPtr(Cons);
                            result = self.heap.allocCons(c.car, result) orelse return error.OutOfMemory;
                            reversed = c.cdr;
                        }
                        try self.push(result);
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
                        reversed = self.heap.allocCons(c.car, reversed) orelse return error.OutOfMemory;
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

                // Type predicates
                .consp => {
                    const a = try self.pop();
                    try self.push(if (a.isCons()) Value.t else Value.nil);
                },
                .symbolp => {
                    const a = try self.pop();
                    try self.push(if (a.isSymbol()) Value.t else Value.nil);
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
                    const size_val = try self.pop();
                    if (!size_val.isFixnum()) return error.TypeMismatch;
                    const size: usize = @intCast(size_val.toFixnum());
                    const vec = self.heap.allocVector(size, size) orelse return error.OutOfMemory;
                    try self.push(vec);
                },
                .vec_ref => {
                    const idx_val = try self.pop();
                    const vec_val = try self.pop();
                    if (!vec_val.isVector() or !idx_val.isFixnum()) return error.TypeMismatch;
                    const vec = vec_val.toPtr(runtime.Vector);
                    const idx: usize = @intCast(idx_val.toFixnum());
                    if (idx >= vec.length) return error.TypeMismatch;
                    try self.push(vec.get(idx));
                },
                .vec_set => {
                    const val = try self.pop();
                    const idx_val = try self.pop();
                    const vec_val = try self.pop();
                    if (!vec_val.isVector() or !idx_val.isFixnum()) return error.TypeMismatch;
                    const vec = vec_val.toPtr(runtime.Vector);
                    const idx: usize = @intCast(idx_val.toFixnum());
                    if (idx >= vec.length) return error.TypeMismatch;
                    vec.set(idx, val);
                },
                .vec_len => {
                    const vec_val = try self.pop();
                    if (!vec_val.isVector()) return error.TypeMismatch;
                    const vec = vec_val.toPtr(runtime.Vector);
                    try self.push(Value.makeFixnum(@intCast(vec.length)));
                },

                // Box operations (mutable cells for closures)
                .make_box => {
                    const val = try self.pop();
                    // Allocate a 1-element vector as a box
                    const box = self.heap.allocVector(1, 1) orelse return error.OutOfMemory;
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
                    const idx: usize = @intCast(idx_val.toFixnum());
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
                    const result = self.heap.allocStringUninitialized(new_len) orelse return error.OutOfMemory;
                    const result_str = result.toPtr(runtime.String);
                    const dest = result_str.mutableBytes();
                    @memcpy(dest[0..str1.length], str1.bytes());
                    @memcpy(dest[str1.length..new_len], str2.bytes());
                    try self.push(result);
                },

                // Control flow
                .jmp => {
                    const offset = self.readI16();
                    self.ip = @intCast(@as(i32, @intCast(self.ip)) + offset);
                },
                .jmp_nil => {
                    const offset = self.readI16();
                    const val = try self.pop();
                    if (val.isNil()) {
                        self.ip = @intCast(@as(i32, @intCast(self.ip)) + offset);
                    }
                },
                .jmp_not_nil => {
                    const offset = self.readI16();
                    const val = try self.pop();
                    if (!val.isNil()) {
                        self.ip = @intCast(@as(i32, @intCast(self.ip)) + offset);
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
                        return result;
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
                    const closure = self.heap.allocClosure(
                        @ptrCast(closure_chunk),
                        closure_chunk.arity,
                        captures[0..num_captures],
                    ) orelse return error.OutOfMemory;

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
                    if (bytes.len > 0 and bytes[0] == '-') {
                        negative = true;
                        i = 1;
                    }
                    while (i < bytes.len) : (i += 1) {
                        const c = bytes[i];
                        if (c >= '0' and c <= '9') {
                            result = result * 10 + (c - '0');
                        } else {
                            break;
                        }
                    }
                    if (negative) result = -result;
                    try self.push(Value.makeFixnum(result));
                },
                .write_to_string => {
                    const val = try self.pop();
                    // Convert value to string representation
                    var buf: [256]u8 = undefined;
                    var fbs = std.io.fixedBufferStream(&buf);
                    io.writeValueToBuffer(val, fbs.writer().any()) catch {
                        try self.push(Value.nil);
                        continue;
                    };
                    const written = fbs.getWritten();
                    const result = self.heap.allocString(written) orelse return error.OutOfMemory;
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
                        continue;
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
                        continue;
                    };
                    try self.push(Value.nil);
                },
                .random => {
                    const n = try self.pop();
                    const result = arith.random(n);
                    try self.push(result);
                },
                .intern => {
                    const str_val = try self.pop();
                    if (!str_val.isString()) return error.TypeMismatch;
                    const str = str_val.toPtr(String);
                    const sym = self.heap.allocSymbol(str.bytes()) orelse return error.OutOfMemory;
                    try self.push(sym);
                },
                .substring => {
                    const end_val = try self.pop();
                    const start_val = try self.pop();
                    const str_val = try self.pop();
                    if (!end_val.isFixnum() or !start_val.isFixnum()) return error.TypeMismatch;
                    const start: usize = @intCast(start_val.toFixnum());
                    const end: usize = @intCast(end_val.toFixnum());
                    const result = stringPrims.substring(self.heap, str_val, start, end) catch return error.OutOfMemory;
                    try self.push(result);
                },
                .sym_name => {
                    const sym_val = try self.pop();
                    if (!sym_val.isSymbol()) return error.TypeMismatch;
                    const sym = sym_val.toPtr(Symbol);
                    const name_str = self.heap.allocString(sym.getName()) orelse return error.OutOfMemory;
                    try self.push(name_str);
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
                    const type_sym = self.heap.intern(type_name) orelse return error.OutOfMemory;
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
                    // For normal exit, pop the unwind frame (doThrow already popped for throw case)
                    if (!self.is_unwinding and self.unwind_sp > 0) {
                        self.unwind_sp -= 1;
                    }
                    // If we're unwinding (cleanup ran due to throw), re-throw
                    if (self.is_unwinding) {
                        self.is_unwinding = false;
                        const tag = self.pending_throw_tag;
                        const value = self.pending_throw_value;
                        self.pending_throw_tag = Value.nil;
                        self.pending_throw_value = Value.nil;
                        try self.doThrow(tag, value);
                    }
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
                        result = self.heap.allocCons(self.secondary_values[i - 1], result) orelse return error.OutOfMemory;
                    }

                    // Add primary at front
                    result = self.heap.allocCons(primary, result) orelse return error.OutOfMemory;

                    // Clear secondary values
                    self.secondary_values_count = 0;

                    try self.push(result);
                },

                .format => {
                    const argc = self.readU8();
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
                    const ht = self.heap.allocHashTable(capacity) orelse return error.OutOfMemory;
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
                .hash_set => {
                    const value = try self.pop();
                    const key = try self.pop();
                    const ht_val = try self.pop();
                    if (!ht_val.isHashTable()) return error.TypeMismatch;
                    const ht = ht_val.toPtr(HashTable);
                    hashTableSet(ht, key, value);
                    try self.push(value); // Return the set value (CL convention)
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

                // Character operations
                .characterp => {
                    const val = try self.pop();
                    try self.push(if (val.isCharacter()) Value.t else Value.nil);
                },
                .floatp => {
                    const val = try self.pop();
                    try self.push(if (val.isFloat()) Value.t else Value.nil);
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
                        continue;
                    };

                    // Parse the S-expression
                    var parser = Parser.init(self.allocator, self.heap, buffer[0..len]);
                    const result = parser.parse() catch {
                        try self.push(Value.nil);
                        continue;
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
                        continue;
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
                        continue;
                    };
                    self.gensym_counter += 1;
                    const sym = self.heap.allocSymbol(name) orelse return error.OutOfMemory;
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
                    const type_sym = type_spec.toPtr(Symbol);
                    const type_name = type_sym.getName();

                    const matches = if (std.mem.eql(u8, type_name, "fixnum"))
                        obj.isFixnum()
                    else if (std.mem.eql(u8, type_name, "cons"))
                        obj.isCons()
                    else if (std.mem.eql(u8, type_name, "symbol"))
                        obj.isSymbol()
                    else if (std.mem.eql(u8, type_name, "string"))
                        obj.isString()
                    else if (std.mem.eql(u8, type_name, "vector"))
                        obj.isVector()
                    else if (std.mem.eql(u8, type_name, "closure") or std.mem.eql(u8, type_name, "function"))
                        obj.isClosure()
                    else if (std.mem.eql(u8, type_name, "keyword"))
                        obj.isKeyword()
                    else if (std.mem.eql(u8, type_name, "character"))
                        obj.isCharacter()
                    else if (std.mem.eql(u8, type_name, "hash-table"))
                        obj.isHashTable()
                    else if (std.mem.eql(u8, type_name, "nil") or std.mem.eql(u8, type_name, "null"))
                        obj.isNil()
                    else if (std.mem.eql(u8, type_name, "list"))
                        obj.isNil() or obj.isCons()
                    else if (std.mem.eql(u8, type_name, "atom"))
                        !obj.isCons()
                    else if (std.mem.eql(u8, type_name, "t"))
                        true // Everything is of type t
                    else
                        false; // Unknown type

                    try self.push(if (matches) Value.t else Value.nil);
                },

                // Numeric predicates
                .abs => {
                    const val = try self.pop();
                    if (!val.isFixnum()) return error.TypeMismatch;
                    const n = val.toFixnum();
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
    }

    // ========================================================================
    // Exception handling
    // ========================================================================

    fn doThrow(self: *Vm, tag: Value, value: Value) VmError!void {
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

            // Jump to cleanup code
            self.chunk = unwind_frame.chunk;
            self.ip = unwind_frame.cleanup_ip;
            // Note: stack is NOT restored - cleanup runs with current stack
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

    // ========================================================================
    // Format string support
    // ========================================================================

    fn doFormat(self: *Vm, dest: Value, control: Value, args: []const Value) VmError!Value {
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
                    '%' => {
                        // Newline
                        result.append(self.allocator, '\n') catch return error.OutOfMemory;
                        i += 2;
                    },
                    '~' => {
                        // Literal tilde
                        result.append(self.allocator, '~') catch return error.OutOfMemory;
                        i += 2;
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
            return self.heap.allocString(result.items) orelse return error.OutOfMemory;
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

    fn formatValueAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) VmError!void {
        if (val.isNil()) {
            result.appendSlice(self.allocator, "nil") catch return error.OutOfMemory;
        } else if (val.isFixnum()) {
            var buf: [32]u8 = undefined;
            const num_str = std.fmt.bufPrint(&buf, "{d}", .{val.toFixnum()}) catch return error.OutOfMemory;
            result.appendSlice(self.allocator, num_str) catch return error.OutOfMemory;
        } else if (val.isString()) {
            const str = val.toPtr(runtime.String);
            result.appendSlice(self.allocator, str.bytes()) catch return error.OutOfMemory;
        } else if (val.isSymbol()) {
            const sym = val.toPtr(Symbol);
            result.appendSlice(self.allocator, sym.getName()) catch return error.OutOfMemory;
        } else if (val.isKeyword()) {
            const kw = val.toPtr(runtime.Keyword);
            result.append(self.allocator, ':') catch return error.OutOfMemory;
            result.appendSlice(self.allocator, kw.getName()) catch return error.OutOfMemory;
        } else if (val.isCons()) {
            try self.formatListAesthetic(val, result);
        } else if (val.isClosure()) {
            result.appendSlice(self.allocator, "#<closure>") catch return error.OutOfMemory;
        } else if (val.isVector()) {
            result.appendSlice(self.allocator, "#<vector>") catch return error.OutOfMemory;
        } else if (val.isHashTable()) {
            result.appendSlice(self.allocator, "#<hash-table>") catch return error.OutOfMemory;
        } else {
            result.appendSlice(self.allocator, "#<unknown>") catch return error.OutOfMemory;
        }
    }

    fn formatValueStandard(self: *Vm, val: Value, result: *std.ArrayList(u8)) VmError!void {
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

    fn formatListAesthetic(self: *Vm, val: Value, result: *std.ArrayList(u8)) VmError!void {
        result.append(self.allocator, '(') catch return error.OutOfMemory;
        var current = val;
        var first = true;
        while (current.isCons()) {
            if (!first) result.append(self.allocator, ' ') catch return error.OutOfMemory;
            first = false;
            const cons = current.toPtr(runtime.Cons);
            try self.formatValueAesthetic(cons.car, result);
            current = cons.cdr;
        }
        if (!current.isNil()) {
            result.appendSlice(self.allocator, " . ") catch return error.OutOfMemory;
            try self.formatValueAesthetic(current, result);
        }
        result.append(self.allocator, ')') catch return error.OutOfMemory;
    }

    // ========================================================================
    // Function call support
    // ========================================================================

    fn doCall(self: *Vm, argc: u8, tail: bool) VmError!void {
        // Get function value (below args on stack)
        const fn_val = self.stack[self.sp - argc - 1];

        if (!fn_val.isClosure()) {
            return error.TypeMismatch;
        }

        const closure = fn_val.toPtr(runtime.Closure);
        const callee_chunk: *const Chunk = @ptrCast(@alignCast(closure.code));
        const arity = callee_chunk.arity;

        // Check arity
        if (callee_chunk.has_rest) {
            // Variadic: need at least required args
            if (argc < arity) {
                return error.TypeMismatch;
            }
        } else {
            // Fixed: need exact arity
            if (argc != arity) {
                return error.TypeMismatch;
            }
        }

        // Build rest list if variadic (before we modify the stack)
        var rest_list = Value.nil;
        if (callee_chunk.has_rest and argc > arity) {
            // Build list from extra args (in reverse since we pop from end)
            const extra_count = argc - arity;
            var i: u8 = 0;
            while (i < extra_count) : (i += 1) {
                const idx = self.sp - 1 - i;
                rest_list = self.heap.allocCons(self.stack[idx], rest_list) orelse return error.OutOfMemory;
            }
            // Pop the extra args
            self.sp -= extra_count;
        }

        // After popping extra args, we have exactly `arity` args on stack
        const required_argc = arity;

        if (tail) {
            // Tail call: reuse current frame
            // Move arguments to start of current frame
            const current_bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
            const arg_start = self.sp - required_argc;

            // Copy required args to current frame's base
            for (0..required_argc) |i| {
                self.stack[current_bp + i] = self.stack[arg_start + i];
            }

            // Reset stack pointer
            self.sp = current_bp + required_argc;

            // If variadic, push rest list as next local
            if (callee_chunk.has_rest) {
                try self.push(rest_list);
            }

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Update closure in current frame for captures
            if (self.fp > 0) {
                self.frames[self.fp - 1].closure = closure;
            }

            // Reserve space for additional locals (after required + rest)
            const used_locals: usize = required_argc + @as(u8, if (callee_chunk.has_rest) 1 else 0);
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
                .bp = self.sp - required_argc - 1, // -1 for function value
                .closure = closure,
            };
            self.fp += 1;

            // The arguments are already on stack above the function value
            // We need to set bp to point to first arg (overwriting fn_val slot)
            const new_bp = self.sp - required_argc - 1;

            // Copy args down to overwrite fn_val (args are now locals 0..arity-1)
            for (0..required_argc) |i| {
                self.stack[new_bp + i] = self.stack[new_bp + 1 + i];
            }
            self.sp = new_bp + required_argc;

            // If variadic, push rest list as next local (at index `arity`)
            if (callee_chunk.has_rest) {
                try self.push(rest_list);
            }

            // Update frame bp
            self.frames[self.fp - 1].bp = new_bp;

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Reserve space for additional locals (after required + rest)
            const used: usize = required_argc + @as(u8, if (callee_chunk.has_rest) 1 else 0);
            var i: usize = used;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
        }
    }

    fn doApply(self: *Vm) VmError!void {
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

        // Push function before args on stack
        // Current stack: ... arg1 arg2 ... argN
        // Need: ... fn arg1 arg2 ... argN
        // So we shift args up and insert fn
        if (count > 0) {
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

    fn push(self: *Vm, val: Value) VmError!void {
        if (self.sp >= STACK_SIZE) return error.StackOverflow;
        self.stack[self.sp] = val;
        self.sp += 1;
    }

    fn pop(self: *Vm) VmError!Value {
        if (self.sp == 0) return error.StackUnderflow;
        self.sp -= 1;
        return self.stack[self.sp];
    }

    fn peek(self: *Vm, distance: usize) VmError!Value {
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

    fn binaryOp(self: *Vm, comptime op: fn (i64, i64) VmError!Value) VmError!void {
        const b = try self.pop();
        const a = try self.pop();
        if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
        try self.push(try op(a.toFixnum(), b.toFixnum()));
    }

    fn binaryAdd(a: i64, b: i64) VmError!Value {
        return Value.makeFixnum(a + b);
    }

    fn binarySub(a: i64, b: i64) VmError!Value {
        return Value.makeFixnum(a - b);
    }

    fn binaryMul(a: i64, b: i64) VmError!Value {
        return Value.makeFixnum(a * b);
    }

    fn binaryDiv(a: i64, b: i64) VmError!Value {
        if (b == 0) return error.DivisionByZero;
        return Value.makeFixnum(@divTrunc(a, b));
    }

    fn binaryMod(a: i64, b: i64) VmError!Value {
        if (b == 0) return error.DivisionByZero;
        return Value.makeFixnum(@mod(a, b));
    }

    fn binaryLt(a: i64, b: i64) VmError!Value {
        return if (a < b) Value.t else Value.nil;
    }

    fn binaryGt(a: i64, b: i64) VmError!Value {
        return if (a > b) Value.t else Value.nil;
    }

    fn binaryLe(a: i64, b: i64) VmError!Value {
        return if (a <= b) Value.t else Value.nil;
    }

    fn binaryGe(a: i64, b: i64) VmError!Value {
        return if (a >= b) Value.t else Value.nil;
    }

    fn binaryNumEq(a: i64, b: i64) VmError!Value {
        return if (a == b) Value.t else Value.nil;
    }
};

// ============================================================================
// Hash table helpers (open addressing with linear probing)
// ============================================================================

/// Hash a Value for use in hash table lookup
fn hashValue(val: Value) u64 {
    // Simple FNV-1a style hash on the raw value
    // For fixnums, this gives good distribution
    // For pointers, mixing helps distribute
    var hash: u64 = 0xcbf29ce484222325; // FNV offset basis
    const raw = val.raw;
    hash ^= raw & 0xFF;
    hash *%= 0x100000001b3; // FNV prime
    hash ^= (raw >> 8) & 0xFF;
    hash *%= 0x100000001b3;
    hash ^= (raw >> 16) & 0xFF;
    hash *%= 0x100000001b3;
    hash ^= (raw >> 24) & 0xFF;
    hash *%= 0x100000001b3;
    hash ^= (raw >> 32) & 0xFF;
    hash *%= 0x100000001b3;
    hash ^= (raw >> 40) & 0xFF;
    hash *%= 0x100000001b3;
    hash ^= (raw >> 48) & 0xFF;
    hash *%= 0x100000001b3;
    hash ^= (raw >> 56) & 0xFF;
    hash *%= 0x100000001b3;
    return hash;
}

/// Get value from hash table, returns nil if not found
fn hashTableGet(ht: *HashTable, key: Value) Value {
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    var idx = hashValue(key) & mask;

    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (HashTable.isEmpty(entry)) {
            return Value.nil; // Not found
        }
        if (!HashTable.isDeleted(entry) and entry.key.raw == key.raw) {
            return entry.value; // Found
        }
        idx = (idx + 1) & mask; // Linear probe
    }
    return Value.nil; // Table full and key not found
}

/// Set value in hash table (insert or update)
fn hashTableSet(ht: *HashTable, key: Value, value: Value) void {
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    var idx = hashValue(key) & mask;

    var first_deleted: ?usize = null;
    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (HashTable.isEmpty(entry)) {
            // Insert at first deleted slot if we found one, else here
            const insert_idx = first_deleted orelse idx;
            entries[insert_idx] = .{ .key = key, .value = value };
            ht.count += 1;
            return;
        }
        if (HashTable.isDeleted(entry)) {
            if (first_deleted == null) first_deleted = idx;
        } else if (entry.key.raw == key.raw) {
            // Update existing
            entries[idx].value = value;
            return;
        }
        idx = (idx + 1) & mask;
    }
    // Table full - insert at first deleted if available
    if (first_deleted) |del_idx| {
        entries[del_idx] = .{ .key = key, .value = value };
        ht.count += 1;
    }
    // Otherwise table is truly full, silently fail (should resize in practice)
}

/// Remove key from hash table, returns true if removed
fn hashTableRemove(ht: *HashTable, key: Value) bool {
    const entries = ht.getEntries();
    const mask = ht.capacity - 1;
    var idx = hashValue(key) & mask;

    var probes: usize = 0;
    while (probes < ht.capacity) : (probes += 1) {
        const entry = entries[idx];
        if (HashTable.isEmpty(entry)) {
            return false; // Not found
        }
        if (!HashTable.isDeleted(entry) and entry.key.raw == key.raw) {
            // Mark as deleted
            entries[idx].key = HashTable.DELETED;
            ht.count -= 1;
            return true;
        }
        idx = (idx + 1) & mask;
    }
    return false;
}

// ============================================================================
// Tests
// ============================================================================

test "vm push and return" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = Heap.init(allocator, .{ .total_size = 1024 * 1024 }) catch unreachable;
    defer heap.deinit();

    var vm = Vm.init(allocator, &heap);

    const code = [_]u8{
        @intFromEnum(Op.push_i32),
        42, 0, 0, 0,
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
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

    var vm = Vm.init(allocator, &heap);

    // (+ 10 20) = 30
    const code = [_]u8{
        @intFromEnum(Op.push_i32), 10, 0, 0, 0,
        @intFromEnum(Op.push_i32), 20, 0, 0, 0,
        @intFromEnum(Op.add),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
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

    var vm = Vm.init(allocator, &heap);

    // (car (cons 1 2)) = 1
    const code = [_]u8{
        @intFromEnum(Op.push_i32), 1, 0, 0, 0,
        @intFromEnum(Op.push_i32), 2, 0, 0, 0,
        @intFromEnum(Op.cons),
        @intFromEnum(Op.car),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
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

    var vm = Vm.init(allocator, &heap);

    // (if nil 1 2) = 2
    const code = [_]u8{
        @intFromEnum(Op.push_nil),
        @intFromEnum(Op.jmp_nil), 8, 0, // Jump 8 bytes if nil
        @intFromEnum(Op.push_i32), 1, 0, 0, 0,
        @intFromEnum(Op.jmp), 5, 0, // Jump 5 bytes over else
        @intFromEnum(Op.push_i32), 2, 0, 0, 0,
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
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

    var vm = Vm.init(allocator, &heap);

    // Store 42 in local 0, load it back
    const code = [_]u8{
        @intFromEnum(Op.push_i32), 42, 0, 0, 0,
        @intFromEnum(Op.store_local), 0,
        @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
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

    var vm = Vm.init(allocator, &heap);

    // Create hash table, set key 42 to value 100, get key 42
    // Use local 0 to store ht
    const code = [_]u8{
        // make_hash with capacity 16, store in local 0
        @intFromEnum(Op.make_hash), 16, 0,
        @intFromEnum(Op.store_local), 0,
        // load ht, push key (42), push value (100), hash_set
        @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.push_i32), 42, 0, 0, 0,
        @intFromEnum(Op.push_i32), 100, 0, 0, 0,
        @intFromEnum(Op.hash_set), // pushes value back
        @intFromEnum(Op.pop), // discard returned value
        // load ht, push key, hash_get
        @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.push_i32), 42, 0, 0, 0,
        @intFromEnum(Op.hash_get),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
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

    var vm = Vm.init(allocator, &heap);

    // Create hash table, set 2 keys, get count
    const code = [_]u8{
        // make_hash with capacity 16
        @intFromEnum(Op.make_hash), 16, 0,
        // store in local 0
        @intFromEnum(Op.store_local), 0,
        // Set key 1 -> 10
        @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.push_i32), 1, 0, 0, 0,
        @intFromEnum(Op.push_i32), 10, 0, 0, 0,
        @intFromEnum(Op.hash_set),
        @intFromEnum(Op.pop), // discard returned value
        // Set key 2 -> 20
        @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.push_i32), 2, 0, 0, 0,
        @intFromEnum(Op.push_i32), 20, 0, 0, 0,
        @intFromEnum(Op.hash_set),
        @intFromEnum(Op.pop), // discard returned value
        // Get count (should be 2)
        @intFromEnum(Op.load_local), 0,
        @intFromEnum(Op.hash_count),
        @intFromEnum(Op.ret),
    };

    const chunk = Chunk{
        .code = @constCast(&code),
        .constants = &[_]u64{},
        .arity = 0,
        .has_rest = false,
        .num_locals = 1,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}
