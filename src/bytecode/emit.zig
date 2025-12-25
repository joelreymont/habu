//! Bytecode emitter - compiles IR to bytecode
//!
//! Walks the IR tree and emits stack-based bytecode.
//! Handles:
//! - Forward jump patching
//! - Constant pool management
//! - Closure capture indices

const std = @import("std");
const ir = @import("../compiler/ir.zig");
const Ir = ir.Ir;
const opcodes = @import("opcodes.zig");
const Op = opcodes.Op;
const Chunk = opcodes.Chunk;
const Value = @import("../runtime/value.zig").Value;

pub const EmitError = error{
    OutOfMemory,
    TooManyConstants,
    TooManyLocals,
    JumpTooLong,
    InvalidIr,
};

/// Bytecode emitter
pub const Emitter = struct {
    allocator: std.mem.Allocator,
    /// Bytecode buffer
    code: std.ArrayList(u8),
    /// Constant pool (raw u64 values)
    constants: std.ArrayList(u64),
    /// Child chunks (for lambdas)
    child_chunks: std.ArrayList(Chunk),
    /// Number of local variables
    num_locals: u8,
    /// Function arity
    arity: u8,
    /// Function name
    name: []const u8,
    /// Captured variables (for inner lambdas)
    captures: []const Ir.Capture,

    pub fn init(allocator: std.mem.Allocator) Emitter {
        return .{
            .allocator = allocator,
            .code = std.ArrayList(u8){},
            .constants = std.ArrayList(u64){},
            .child_chunks = std.ArrayList(Chunk){},
            .num_locals = 0,
            .arity = 0,
            .name = "",
            .captures = &[_]Ir.Capture{},
        };
    }

    pub fn deinit(self: *Emitter) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
        // Free child chunks
        for (self.child_chunks.items) |chunk| {
            self.allocator.free(chunk.code);
            self.allocator.free(chunk.constants);
        }
        self.child_chunks.deinit(self.allocator);
    }

    /// Emit bytecode for an IR node
    pub fn emit(self: *Emitter, node: *const Ir) EmitError!void {
        switch (node.*) {
            .lit => |v| try self.emitLiteral(v),
            .quote_sym => |name| try self.emitQuoteSym(name),
            .quote => |inner| try self.emitQuote(inner),
            .@"var" => |v| try self.emitVar(v.depth, v.index),
            .set => |s| try self.emitSet(s),
            .global_ref => |g| try self.emitGlobalRef(g.index),
            .define => |d| try self.emitDefine(d),
            .let => |l| try self.emitLet(l),
            .lambda => |lam| try self.emitLambda(lam),
            .@"if" => |i| try self.emitIf(i),
            .progn => |exprs| try self.emitProgn(exprs),
            .loop => |l| try self.emitLoop(l),
            .call => |c| try self.emitCall(c, false),
            .tailcall => |c| try self.emitCall(c, true),

            // Arithmetic
            .add => |op| try self.emitBinaryOp(op, .add),
            .sub => |op| try self.emitBinaryOp(op, .sub),
            .mul => |op| try self.emitBinaryOp(op, .mul),
            .div => |op| try self.emitBinaryOp(op, .div),
            .mod => |op| try self.emitBinaryOp(op, .mod),

            // Comparison
            .eq => |op| try self.emitBinaryOp(op, .eq),
            .lt => |op| try self.emitBinaryOp(op, .lt),
            .gt => |op| try self.emitBinaryOp(op, .gt),
            .le => |op| try self.emitBinaryOp(op, .le),
            .ge => |op| try self.emitBinaryOp(op, .ge),
            .num_eq => |op| try self.emitBinaryOp(op, .num_eq),

            // Logic
            .not => |op| try self.emitUnaryOp(op.operand, .not),

            // List operations
            .cons => |op| try self.emitBinaryOp(op, .cons),
            .car => |op| try self.emitUnaryOp(op.operand, .car),
            .cdr => |op| try self.emitUnaryOp(op.operand, .cdr),
            .list => |elements| try self.emitList(elements),

            // Type predicates
            .consp => |op| try self.emitUnaryOp(op.operand, .consp),
            .symbolp => |op| try self.emitUnaryOp(op.operand, .symbolp),
            .numberp => |op| try self.emitUnaryOp(op.operand, .numberp),
            .stringp => |op| try self.emitUnaryOp(op.operand, .stringp),
            .vectorp => |op| try self.emitUnaryOp(op.operand, .vectorp),
            .closurep => |op| try self.emitUnaryOp(op.operand, .closurep),
            .keywordp => |op| try self.emitUnaryOp(op.operand, .keywordp),
            .nilp => |op| try self.emitUnaryOp(op.operand, .nilp),

            // Vector operations
            .vec_new => |v| try self.emitVecNew(v),
            .vec_ref => |op| try self.emitBinaryOp(op, .vec_ref),
            .vec_set => |v| try self.emitVecSet(v),
            .vec_len => |op| try self.emitUnaryOp(op.operand, .vec_len),

            // String operations
            .str_ref => |op| try self.emitBinaryOp(op, .str_ref),
            .str_len => |op| try self.emitUnaryOp(op.operand, .str_len),
            .str_concat => |op| try self.emitBinaryOp(op, .str_concat),
            .substring => |op| {
                try self.emit(op.str);
                try self.emit(op.start);
                try self.emit(op.end);
                try self.emitOp(.substring);
            },

            // I/O
            .print => |op| try self.emitUnaryOp(op.operand, .print),
            .random => |op| try self.emitUnaryOp(op.operand, .random),
            .intern => |op| try self.emitUnaryOp(op.operand, .intern),

            // Type assertions
            .assert_fixnum => |op| try self.emitUnaryOp(op.operand, .check_fixnum),
            .assert_cons => |op| try self.emitUnaryOp(op.operand, .check_cons),
            .assert_symbol => |op| try self.emitUnaryOp(op.operand, .check_symbol),
            .assert_string => |op| try self.emitUnaryOp(op.operand, .check_string),
            .assert_vector => |op| try self.emitUnaryOp(op.operand, .check_vector),
            .assert_closure => |op| try self.emitUnaryOp(op.operand, .check_closure),
            .assert_non_nil => |op| try self.emitUnaryOp(op.operand, .check_non_nil),
        }
    }

    /// Finalize and return the chunk
    pub fn finalize(self: *Emitter) EmitError!Chunk {
        // Add implicit return if not present
        if (self.code.items.len == 0) {
            // Empty function returns nil
            try self.emitOp(.push_nil);
            try self.emitOp(.ret);
        } else if (self.code.items[self.code.items.len - 1] != @intFromEnum(Op.ret)) {
            // Non-empty function: just add ret (value already on stack)
            try self.emitOp(.ret);
        }

        return Chunk{
            .code = try self.allocator.dupe(u8, self.code.items),
            .constants = try self.allocator.dupe(u64, self.constants.items),
            .arity = self.arity,
            .num_locals = self.num_locals,
            .name = self.name,
        };
    }

    /// Get child chunks (caller takes ownership via duped slice)
    pub fn getChildChunks(self: *Emitter) ![]Chunk {
        const chunks = try self.allocator.dupe(Chunk, self.child_chunks.items);
        // Clear the list so deinit doesn't free the chunk contents
        self.child_chunks.items.len = 0;
        return chunks;
    }

    // ========================================================================
    // Emission helpers
    // ========================================================================

    fn emitOp(self: *Emitter, op: Op) EmitError!void {
        self.code.append(self.allocator, @intFromEnum(op)) catch
            return error.OutOfMemory;
    }

    fn emitU8(self: *Emitter, val: u8) EmitError!void {
        self.code.append(self.allocator, val) catch
            return error.OutOfMemory;
    }

    fn emitU16(self: *Emitter, val: u16) EmitError!void {
        self.code.append(self.allocator, @truncate(val)) catch
            return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(val >> 8)) catch
            return error.OutOfMemory;
    }

    fn emitI16(self: *Emitter, val: i16) EmitError!void {
        try self.emitU16(@bitCast(val));
    }

    fn emitI32(self: *Emitter, val: i32) EmitError!void {
        const u: u32 = @bitCast(val);
        self.code.append(self.allocator, @truncate(u)) catch return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(u >> 8)) catch return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(u >> 16)) catch return error.OutOfMemory;
        self.code.append(self.allocator, @truncate(u >> 24)) catch return error.OutOfMemory;
    }

    /// Add constant to pool, return index
    fn addConstant(self: *Emitter, val: u64) EmitError!u16 {
        // Check if already in pool
        for (self.constants.items, 0..) |c, i| {
            if (c == val) return @intCast(i);
        }

        if (self.constants.items.len >= 65535) {
            return error.TooManyConstants;
        }

        const idx: u16 = @intCast(self.constants.items.len);
        self.constants.append(self.allocator, val) catch return error.OutOfMemory;
        return idx;
    }

    /// Get current code offset
    fn currentOffset(self: *const Emitter) usize {
        return self.code.items.len;
    }

    /// Emit placeholder jump, return offset to patch
    fn emitJump(self: *Emitter, op: Op) EmitError!usize {
        try self.emitOp(op);
        const offset = self.currentOffset();
        try self.emitI16(0); // Placeholder
        return offset;
    }

    /// Patch jump at offset to current position
    fn patchJump(self: *Emitter, offset: usize) EmitError!void {
        const target = self.currentOffset();
        const distance = @as(i32, @intCast(target)) - @as(i32, @intCast(offset + 2));

        if (distance > 32767 or distance < -32768) {
            return error.JumpTooLong;
        }

        const val: i16 = @intCast(distance);
        const u: u16 = @bitCast(val);
        self.code.items[offset] = @truncate(u);
        self.code.items[offset + 1] = @truncate(u >> 8);
    }

    // ========================================================================
    // IR emission
    // ========================================================================

    fn emitLiteral(self: *Emitter, val: Value) EmitError!void {
        if (val.isNil()) {
            try self.emitOp(.push_nil);
        } else if (val.eq(Value.t)) {
            try self.emitOp(.push_t);
        } else if (val.isFixnum()) {
            const n = val.toFixnum();
            if (n >= std.math.minInt(i32) and n <= std.math.maxInt(i32)) {
                try self.emitOp(.push_i32);
                try self.emitI32(@intCast(n));
            } else {
                // Large fixnum - use constant pool
                const idx = try self.addConstant(val.raw);
                try self.emitOp(.push_const);
                try self.emitU16(idx);
            }
        } else {
            // Other values go in constant pool
            const idx = try self.addConstant(val.raw);
            try self.emitOp(.push_const);
            try self.emitU16(idx);
        }
    }

    fn emitQuoteSym(self: *Emitter, _: []const u8) EmitError!void {
        // TODO: Intern symbol and add to constant pool
        // For now, just push nil as placeholder
        try self.emitOp(.push_nil);
    }

    fn emitQuote(self: *Emitter, inner: *const Ir) EmitError!void {
        // Quote just emits the literal value
        try self.emit(inner);
    }

    fn emitVar(self: *Emitter, depth: u16, index: u16) EmitError!void {
        if (depth == 0) {
            // Local variable
            if (index > 255) return error.TooManyLocals;
            try self.emitOp(.load_local);
            try self.emitU8(@intCast(index));
        } else {
            // Check if this is a captured variable
            // Captures are stored with depth from enclosing function's perspective
            // IR nodes have depth from this function's perspective (so +1)
            for (self.captures, 0..) |cap, i| {
                if (cap.depth + 1 == depth and cap.index == index) {
                    // Load from capture array
                    try self.emitOp(.load_capture);
                    try self.emitU8(@intCast(i));
                    return;
                }
            }
            // Upvalue (nested closure case)
            if (depth > 255 or index > 255) return error.TooManyLocals;
            try self.emitOp(.load_upvalue);
            try self.emitU8(@intCast(depth));
            try self.emitU8(@intCast(index));
        }
    }

    fn emitGlobalRef(self: *Emitter, index: u16) EmitError!void {
        try self.emitOp(.load_global);
        try self.emitU16(index);
    }

    fn emitDefine(self: *Emitter, d: anytype) EmitError!void {
        // Emit value
        try self.emit(d.value);
        // Store to global
        try self.emitOp(.store_global);
        try self.emitU16(d.index);
        // Define leaves value on stack (for REPL)
        try self.emitOp(.load_global);
        try self.emitU16(d.index);
    }

    fn emitSet(self: *Emitter, s: anytype) EmitError!void {
        // Emit value first
        try self.emit(s.value);

        // Then store
        if (s.depth == 0) {
            if (s.index > 255) return error.TooManyLocals;
            try self.emitOp(.store_local);
            try self.emitU8(@intCast(s.index));
        } else {
            if (s.depth > 255 or s.index > 255) return error.TooManyLocals;
            try self.emitOp(.store_upvalue);
            try self.emitU8(@intCast(s.depth));
            try self.emitU8(@intCast(s.index));
        }
    }

    fn emitLet(self: *Emitter, l: anytype) EmitError!void {
        // Emit binding values
        for (l.bindings) |b| {
            try self.emit(b.value);
            // Values are now on stack as locals
        }

        // Emit body
        try self.emit(l.body);

        // TODO: Pop locals when let scope ends
    }

    fn emitLambda(self: *Emitter, lam: anytype) EmitError!void {
        // Create nested emitter for lambda body
        var lambda_emitter = Emitter.init(self.allocator);

        lambda_emitter.arity = @intCast(lam.params.len);
        lambda_emitter.num_locals = @intCast(lam.params.len);
        // Pass captures so emitVar knows which variables to load from capture array
        lambda_emitter.captures = lam.captures;

        // Emit body
        lambda_emitter.emit(lam.body) catch {
            lambda_emitter.deinit();
            return error.InvalidIr;
        };

        // Finalize lambda chunk
        const chunk = lambda_emitter.finalize() catch {
            lambda_emitter.deinit();
            return error.OutOfMemory;
        };

        // Collect any child chunks from the lambda
        for (lambda_emitter.child_chunks.items) |child_chunk| {
            self.child_chunks.append(self.allocator, child_chunk) catch {
                lambda_emitter.deinit();
                return error.OutOfMemory;
            };
        }
        lambda_emitter.child_chunks.items.len = 0; // Prevent double-free
        lambda_emitter.deinit();

        // Store chunk in child_chunks, get its index
        const chunk_idx: u16 = @intCast(self.child_chunks.items.len);
        self.child_chunks.append(self.allocator, chunk) catch return error.OutOfMemory;

        // Emit captures (if any)
        for (lam.captures) |cap| {
            // Load the captured value from upvalue
            if (cap.depth == 0) {
                try self.emitOp(.load_local);
                try self.emitU8(@intCast(cap.index));
            } else {
                try self.emitOp(.load_upvalue);
                try self.emitU8(@intCast(cap.depth - 1));
                try self.emitU8(@intCast(cap.index));
            }
        }

        // Emit make_closure: u16 chunk_index, u8 num_captures
        try self.emitOp(.make_closure);
        try self.emitU16(chunk_idx);
        try self.emitU8(@intCast(lam.captures.len));
    }

    fn emitIf(self: *Emitter, i: anytype) EmitError!void {
        // Emit test
        try self.emit(i.cond);

        // Jump to else if nil
        const else_jump = try self.emitJump(.jmp_nil);

        // Emit then branch
        try self.emit(i.then_branch);

        // Jump over else
        const end_jump = try self.emitJump(.jmp);

        // Patch else jump
        try self.patchJump(else_jump);

        // Emit else branch
        try self.emit(i.else_branch);

        // Patch end jump
        try self.patchJump(end_jump);
    }

    fn emitProgn(self: *Emitter, exprs: []const *const Ir) EmitError!void {
        if (exprs.len == 0) {
            try self.emitOp(.push_nil);
            return;
        }

        for (exprs, 0..) |expr, i| {
            try self.emit(expr);
            // Pop intermediate values, keep last
            if (i < exprs.len - 1) {
                try self.emitOp(.pop);
            }
        }
    }

    fn emitLoop(self: *Emitter, l: anytype) EmitError!void {
        // Loop start
        const loop_start = self.currentOffset();

        // Emit test
        try self.emit(l.cond);

        // Jump out if nil
        const exit_jump = try self.emitJump(.jmp_nil);

        // Emit body
        try self.emit(l.body);
        try self.emitOp(.pop); // Discard body result

        // Jump back to start
        const back_distance = @as(i32, @intCast(loop_start)) -
            @as(i32, @intCast(self.currentOffset() + 3)); // +3 for jmp instruction
        if (back_distance > 32767 or back_distance < -32768) {
            return error.JumpTooLong;
        }
        try self.emitOp(.jmp);
        try self.emitI16(@intCast(back_distance));

        // Patch exit
        try self.patchJump(exit_jump);

        // Push nil as loop result
        try self.emitOp(.push_nil);
    }

    fn emitCall(self: *Emitter, c: anytype, tail: bool) EmitError!void {
        // Emit function
        try self.emit(c.func);

        // Emit arguments
        for (c.args) |arg| {
            try self.emit(arg);
        }

        // Emit call
        if (c.args.len > 255) return error.TooManyLocals;
        try self.emitOp(if (tail) .tail_call else .call);
        try self.emitU8(@intCast(c.args.len));
    }

    fn emitBinaryOp(self: *Emitter, op: Ir.BinaryOp, opcode: Op) EmitError!void {
        try self.emit(op.left);
        try self.emit(op.right);
        try self.emitOp(opcode);
    }

    fn emitUnaryOp(self: *Emitter, operand: *const Ir, opcode: Op) EmitError!void {
        try self.emit(operand);
        try self.emitOp(opcode);
    }

    fn emitList(self: *Emitter, elements: []const *const Ir) EmitError!void {
        // Emit elements in order
        for (elements) |elem| {
            try self.emit(elem);
        }

        // Make list
        if (elements.len > 255) return error.TooManyLocals;
        try self.emitOp(.make_list);
        try self.emitU8(@intCast(elements.len));
    }

    fn emitVecNew(self: *Emitter, v: anytype) EmitError!void {
        try self.emit(v.size);
        if (v.init) |init_val| {
            try self.emit(init_val);
            // TODO: Need vec_new_init opcode
        }
        try self.emitOp(.make_vec);
        try self.emitU16(0); // Size from stack
    }

    fn emitVecSet(self: *Emitter, v: anytype) EmitError!void {
        try self.emit(v.vec);
        try self.emit(v.index);
        try self.emit(v.value);
        try self.emitOp(.vec_set);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "emit literal nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.nil);
    defer allocator.destroy(node);

    try emitter.emit(node);
    try testing.expectEqual(@as(u8, @intFromEnum(Op.push_nil)), emitter.code.items[0]);
}

test "emit literal fixnum" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(node);

    try emitter.emit(node);
    try testing.expectEqual(@as(u8, @intFromEnum(Op.push_i32)), emitter.code.items[0]);
    // 42 in little-endian i32
    try testing.expectEqual(@as(u8, 42), emitter.code.items[1]);
}

test "emit add" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    // Use 10 and 20 to avoid push_t optimization (fixnum 1 == t)
    const left = try builder.lit(Value.makeFixnum(10));
    const right = try builder.lit(Value.makeFixnum(20));
    const node = try builder.add(left, right);
    defer {
        allocator.destroy(left);
        allocator.destroy(right);
        allocator.destroy(node);
    }

    try emitter.emit(node);

    // push_i32(5) + push_i32(5) + add(1) = 11 bytes
    try testing.expectEqual(@as(usize, 11), emitter.code.items.len);
    // Last byte should be add opcode
    try testing.expectEqual(@as(u8, @intFromEnum(Op.add)), emitter.code.items[emitter.code.items.len - 1]);
}

test "emit if" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const cond = try builder.lit(Value.t);
    const then_branch = try builder.lit(Value.makeFixnum(1));
    const else_branch = try builder.lit(Value.makeFixnum(0));
    const node = try builder.ifExpr(cond, then_branch, else_branch);
    defer {
        allocator.destroy(cond);
        allocator.destroy(then_branch);
        allocator.destroy(else_branch);
        allocator.destroy(node);
    }

    try emitter.emit(node);

    // Should contain jmp_nil and jmp opcodes
    var has_jmp_nil = false;
    var has_jmp = false;
    for (emitter.code.items) |byte| {
        if (byte == @intFromEnum(Op.jmp_nil)) has_jmp_nil = true;
        if (byte == @intFromEnum(Op.jmp)) has_jmp = true;
    }
    try testing.expect(has_jmp_nil);
    try testing.expect(has_jmp);
}

test "finalize adds return" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    const builder = ir.IrBuilder.init(allocator);
    const node = try builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(node);

    try emitter.emit(node);
    const chunk = try emitter.finalize();
    defer allocator.free(chunk.code);
    defer allocator.free(chunk.constants);

    // Last instruction should be ret
    try testing.expectEqual(@as(u8, @intFromEnum(Op.ret)), chunk.code[chunk.code.len - 1]);
}
