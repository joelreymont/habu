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
const arith = @import("../runtime/primitives/arith.zig");
const io = @import("../runtime/primitives/io.zig");

pub const VmError = error{
    StackOverflow,
    StackUnderflow,
    TypeMismatch,
    DivisionByZero,
    InvalidOpcode,
    InvalidConstant,
    OutOfMemory,
    Halt,
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

    const STACK_SIZE = 1024;
    const MAX_FRAMES = 64;
    const MAX_GLOBALS = 256;

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
                .load_upvalue, .store_upvalue => {
                    // TODO: Implement nested closures
                    _ = self.readU8();
                    _ = self.readU8();
                    try self.push(Value.nil);
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
                    // TODO: Implement string concatenation
                    _ = try self.pop();
                    _ = try self.pop();
                    try self.push(Value.nil);
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

                .halt => return error.Halt,
            }
        }
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

        // Check arity
        if (argc != callee_chunk.arity) {
            return error.TypeMismatch;
        }

        if (tail) {
            // Tail call: reuse current frame
            // Move arguments to start of current frame
            const current_bp = if (self.fp > 0) self.frames[self.fp - 1].bp else 0;
            const arg_start = self.sp - argc;

            // Copy args to current frame's base
            for (0..argc) |i| {
                self.stack[current_bp + i] = self.stack[arg_start + i];
            }

            // Reset stack pointer
            self.sp = current_bp + argc;

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Update closure in current frame for captures
            if (self.fp > 0) {
                self.frames[self.fp - 1].closure = closure;
            }

            // Reserve space for additional locals
            var i: usize = argc;
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
                .bp = self.sp - argc - 1, // -1 for function value
                .closure = closure,
            };
            self.fp += 1;

            // The arguments are already on stack above the function value
            // We need to set bp to point to first arg (overwriting fn_val slot)
            const new_bp = self.sp - argc - 1;

            // Copy args down to overwrite fn_val (args are now locals 0..argc-1)
            for (0..argc) |i| {
                self.stack[new_bp + i] = self.stack[new_bp + 1 + i];
            }
            self.sp = new_bp + argc;

            // Update frame bp
            self.frames[self.fp - 1].bp = new_bp;

            // Switch to callee
            self.chunk = callee_chunk;
            self.ip = 0;

            // Reserve space for additional locals
            var i: usize = argc;
            while (i < callee_chunk.num_locals) : (i += 1) {
                try self.push(Value.nil);
            }
        }
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
        .num_locals = 1,
        .name = "test",
    };

    const result = try vm.run(&chunk);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}
