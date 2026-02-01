//! JIT compiler for Habu bytecode
//!
//! Compiles bytecode to native ARM64 code using copy-and-patch.
//! Each bytecode instruction is compiled by copying a pre-compiled
//! stencil and patching in the runtime values.

const std = @import("std");
const stencils = @import("stencils.zig");
const Stencil = stencils.Stencil;
const patch = @import("patch.zig");
const ctx = @import("ctx.zig");
const rt = @import("rt.zig");
const bytecode = @import("../bytecode/bytecode.zig");
const Op = bytecode.Op;
const Chunk = bytecode.Chunk;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;

pub const JitError = error{
    OutOfMemory,
    UnsupportedOpcode,
    CodeTooLarge,
    InvalidConstantIndex,
    BranchOutOfRange,
    InvalidJumpTarget,
    OffsetTooLarge,
    InvalidHoleType,
    InvalidImm,
    InsufficientPatchValues,
};

/// JIT runtime context
pub const JitContext = ctx.JitContext;

/// JIT-compiled function
pub const JitFn = *const fn (*JitContext) callconv(.c) u64;

/// JIT compiler state
pub const Jit = struct {
    allocator: std.mem.Allocator,
    /// Code buffer for JIT output
    code_buffer: patch.CodeBuffer,
    /// Offset where current function starts
    fn_start: usize,
    /// Label positions for forward references
    labels: std.AutoHashMap(usize, usize),
    /// Pending forward jumps to patch
    pending_jumps: std.ArrayList(PendingJump),

    const PendingJump = struct {
        /// Offset in code buffer where jump instruction is
        code_offset: usize,
        /// Bytecode offset this jump targets
        target_bc_offset: usize,
        /// Hole type for this jump
        hole_type: stencils.HoleType,
    };

    pub fn init(allocator: std.mem.Allocator, code_buf_size: usize) !Jit {
        return .{
            .allocator = allocator,
            .code_buffer = try patch.CodeBuffer.init(allocator, code_buf_size),
            .fn_start = 0,
            .labels = std.AutoHashMap(usize, usize).init(allocator),
            .pending_jumps = std.ArrayList(PendingJump){},
        };
    }

    pub fn deinit(self: *Jit) void {
        self.code_buffer.deinit();
        self.labels.deinit();
        self.pending_jumps.deinit(self.allocator);
    }

    /// Compile a bytecode chunk to native code
    pub fn compile(self: *Jit, chunk: *const Chunk) JitError!JitFn {
        self.fn_start = self.code_buffer.pos;
        self.labels.clearRetainingCapacity();
        self.pending_jumps.clearRetainingCapacity();

        _ = try patch.patchStencil(&self.code_buffer, stencils.prologue_stencil, &[_]patch.PatchValue{});

        var bc_offset: usize = 0;
        const code = chunk.getCode();
        while (bc_offset < code.len) {
            // Record label for this bytecode offset
            try self.labels.put(bc_offset, self.code_buffer.pos);

            const op: Op = @enumFromInt(code[bc_offset]);
            bc_offset += 1;

            try self.compileOp(op, chunk, &bc_offset);
        }

        // Patch forward jumps
        try self.patchPendingJumps();

        return self.code_buffer.getFnPtr(JitFn, self.fn_start);
    }

    fn compileOp(self: *Jit, op: Op, chunk: *const Chunk, bc_offset: *usize) JitError!void {
        switch (op) {
            .push_nil => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.push_nil_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .push_t => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.push_t_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .push_i32 => {
                const val = chunk.readI32(bc_offset.*);
                bc_offset.* += 4;
                // Create tagged fixnum
                const tagged = Value.makeFixnum(val);
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = tagged.raw },
                });
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .push_const => {
                const idx = chunk.readU16(bc_offset.*);
                bc_offset.* += 2;
                if (idx >= chunk.getConstants().len) return error.InvalidConstantIndex;
                const offset_bytes: u32 = @as(u32, idx) * @as(u32, @intCast(@sizeOf(Value)));
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_const, &[_]patch.PatchValue{
                    .{ .imm32 = offset_bytes },
                });
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .add => {
                try self.emitBinaryArith(stencils.add_fixnum, @intFromPtr(&rt.add));
            },

            .sub => {
                try self.emitBinaryArith(stencils.sub_fixnum, @intFromPtr(&rt.sub));
            },

            .mul => {
                try self.emitBinaryArith(stencils.mul_fixnum, @intFromPtr(&rt.mul));
            },

            .neg => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.neg_fixnum, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .div => {
                try self.emitBinaryCall(@intFromPtr(&rt.div));
            },

            .eq => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.eq_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .lt => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.lt_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .gt => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.gt_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .le => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.le_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .ge => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.ge_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .not => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.not_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .nilp => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.nilp_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .numberp => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                try self.emitCallUnary(@intFromPtr(&rt.numberp));
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .dup => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .pop => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
            },

            .swap => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.swap_stencil, &[_]patch.PatchValue{});
            },

            .ret => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.epilogue_stencil, &[_]patch.PatchValue{});
            },

            .jmp => {
                const offset = chunk.readI16(bc_offset.*);
                bc_offset.* += 2;
                const target_bc = @as(usize, @intCast(@as(i32, @intCast(bc_offset.*)) + offset));

                // Record pending jump
                const code_offset = self.code_buffer.pos;
                const inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + code_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
                    .{ .addr = inst_addr }, // Placeholder
                });

                try self.pending_jumps.append(self.allocator, .{
                    .code_offset = code_offset,
                    .target_bc_offset = target_bc,
                    .hole_type = .rel26,
                });
            },

            .jmp_nil => {
                const offset = chunk.readI16(bc_offset.*);
                bc_offset.* += 2;
                const target_bc = @as(usize, @intCast(@as(i32, @intCast(bc_offset.*)) + offset));

                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                const code_offset = self.code_buffer.pos;
                const inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + code_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_nil, &[_]patch.PatchValue{
                    .{ .addr = inst_addr },
                });

                try self.pending_jumps.append(self.allocator, .{
                    .code_offset = code_offset,
                    .target_bc_offset = target_bc,
                    .hole_type = .rel19,
                });
            },

            .jmp_not_nil => {
                const offset = chunk.readI16(bc_offset.*);
                bc_offset.* += 2;
                const target_bc = @as(usize, @intCast(@as(i32, @intCast(bc_offset.*)) + offset));

                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                const code_offset = self.code_buffer.pos;
                const inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + code_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_not_nil, &[_]patch.PatchValue{
                    .{ .addr = inst_addr },
                });

                try self.pending_jumps.append(self.allocator, .{
                    .code_offset = code_offset,
                    .target_bc_offset = target_bc,
                    .hole_type = .rel19,
                });
            },

            .car => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.car_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            .cdr => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.cdr_stencil, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
            },

            // Skip operands for unsupported ops
            .load_local, .store_local, .load_capture => {
                bc_offset.* += 1;
                return error.UnsupportedOpcode;
            },
            .load_upvalue, .store_upvalue => {
                bc_offset.* += 2;
                return error.UnsupportedOpcode;
            },
            .load_global, .store_global, .make_vec => {
                bc_offset.* += 2;
                return error.UnsupportedOpcode;
            },
            .call, .tail_call, .make_list => {
                bc_offset.* += 1;
                return error.UnsupportedOpcode;
            },
            .make_closure => {
                bc_offset.* += 3;
                return error.UnsupportedOpcode;
            },

            else => return error.UnsupportedOpcode,
        }
    }

    fn emitCallUnary(self: *Jit, addr: usize) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x0_x22, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.call_abs, &[_]patch.PatchValue{
            .{ .imm64 = addr },
        });
    }

    fn emitCallBinary(self: *Jit, addr: usize) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x2_x1, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x0_x22, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.call_abs, &[_]patch.PatchValue{
            .{ .imm64 = addr },
        });
    }

    fn emitGuardFixnumX0(self: *Jit) JitError!usize {
        const start = self.code_buffer.pos;
        _ = try patch.patchStencil(&self.code_buffer, stencils.guard_fixnum_x0, &[_]patch.PatchValue{});
        return start + 4;
    }

    fn emitGuardFixnumX1(self: *Jit) JitError!usize {
        const start = self.code_buffer.pos;
        _ = try patch.patchStencil(&self.code_buffer, stencils.guard_fixnum_x1, &[_]patch.PatchValue{});
        return start + 4;
    }

    fn emitBinaryCall(self: *Jit, addr: usize) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
        try self.emitCallBinary(addr);
        _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
    }

    fn emitBinaryArith(self: *Jit, fast: Stencil, slow_addr: usize) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});

        const guard_x0_branch = try self.emitGuardFixnumX0();
        const guard_x1_branch = try self.emitGuardFixnumX1();

        _ = try patch.patchStencil(&self.code_buffer, fast, &[_]patch.PatchValue{});

        const fast_branch_offset = self.code_buffer.pos;
        const fast_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + fast_branch_offset;
        _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
            .{ .addr = fast_inst_addr },
        });

        const slow_code_offset = self.code_buffer.pos;
        const slow_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + slow_code_offset;
        try self.emitCallBinary(slow_addr);

        const end_code_offset = self.code_buffer.pos;
        _ = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
        const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;

        patch.jitWriteProtect(false);
        defer patch.jitWriteProtect(true);
        try self.patchBranch(guard_x0_branch, slow_target_addr, .rel19);
        try self.patchBranch(guard_x1_branch, slow_target_addr, .rel19);
        try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn patchPendingJumps(self: *Jit) JitError!void {
        patch.jitWriteProtect(false);
        defer patch.jitWriteProtect(true);

        for (self.pending_jumps.items) |jump| {
            const target_code_addr = self.labels.get(jump.target_bc_offset) orelse
                return error.InvalidJumpTarget;

            const target_addr = @intFromPtr(self.code_buffer.memory.ptr) + target_code_addr;
            try self.patchBranch(jump.code_offset, target_addr, jump.hole_type);
        }

        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn patchBranch(self: *Jit, code_offset: usize, target_addr: usize, hole_type: stencils.HoleType) JitError!void {
        const inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + code_offset;
        const offset = @as(i64, @intCast(target_addr)) - @as(i64, @intCast(inst_addr));
        const word_offset_i64 = @divTrunc(offset, 4);
        const code_slice = self.code_buffer.memory[code_offset..];

        switch (hole_type) {
            .rel26 => {
                if (word_offset_i64 < -(1 << 25) or word_offset_i64 >= (1 << 25)) return error.BranchOutOfRange;
                const word_offset_i32: i32 = @intCast(word_offset_i64);
                const word_offset: u32 = @bitCast(word_offset_i32);
                patch.patchRel26(code_slice, word_offset);
            },
            .rel19 => {
                if (word_offset_i64 < -(1 << 18) or word_offset_i64 >= (1 << 18)) return error.BranchOutOfRange;
                const word_offset_i32: i32 = @intCast(word_offset_i64);
                const word_offset: u32 = @bitCast(word_offset_i32);
                patch.patchRel19(code_slice, word_offset);
            },
            else => return error.InvalidHoleType,
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "jit init" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    try testing.expectEqual(@as(usize, 0), jit.code_buffer.pos);
}

test "jit compile simple" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    // push_i32 42; ret
    const code = [_]u8{
        @intFromEnum(Op.push_i32), 42, 0, 0, 0,
        @intFromEnum(Op.ret),
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

    const fn_ptr = try jit.compile(&chunk);

    // Verify function was compiled (on ARM64, we could call it)
    try testing.expect(@intFromPtr(fn_ptr) != 0);
    try testing.expect(jit.labels.count() > 0);
    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit compile jump" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const code = [_]u8{
        @intFromEnum(Op.jmp), 0, 0,
        @intFromEnum(Op.ret),
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

    _ = try jit.compile(&chunk);

    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.branch_stencil.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit branch range check" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 4096);
    defer jit.deinit();

    const big_offset: usize = (@as(usize, 1) << 34) + 4;
    try jit.labels.put(0, big_offset);
    try jit.pending_jumps.append(allocator, .{
        .code_offset = 0,
        .target_bc_offset = 0,
        .hole_type = .rel26,
    });

    try testing.expectError(error.BranchOutOfRange, jit.patchPendingJumps());
}

test "jit compile numberp" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const code = [_]u8{
        @intFromEnum(Op.push_i32), 1, 0, 0, 0,
        @intFromEnum(Op.numberp),
        @intFromEnum(Op.ret),
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

    _ = try jit.compile(&chunk);

    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.mov_x1_x0.code.len +
        stencils.mov_x0_x22.code.len +
        stencils.call_abs.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit compile add" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const code = [_]u8{
        @intFromEnum(Op.push_i32), 1, 0, 0, 0,
        @intFromEnum(Op.push_i32), 2, 0, 0, 0,
        @intFromEnum(Op.add),
        @intFromEnum(Op.ret),
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

    _ = try jit.compile(&chunk);

    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len + stencils.stack_push.code.len +
        stencils.load_imm64.code.len + stencils.stack_push.code.len +
        stencils.stack_pop_x1.code.len +
        stencils.stack_pop.code.len +
        stencils.guard_fixnum_x0.code.len +
        stencils.guard_fixnum_x1.code.len +
        stencils.add_fixnum.code.len +
        stencils.branch_stencil.code.len +
        stencils.mov_x2_x1.code.len +
        stencils.mov_x1_x0.code.len +
        stencils.mov_x0_x22.code.len +
        stencils.call_abs.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}
