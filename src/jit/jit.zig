//! JIT compiler for Habu bytecode
//!
//! Compiles bytecode to native ARM64 code using copy-and-patch.
//! Each bytecode instruction is compiled by copying a pre-compiled
//! stencil and patching in the runtime values.

const std = @import("std");
const builtin = @import("builtin");
const stencils = @import("stencils.zig");
const Stencil = stencils.Stencil;
const patch = @import("patch.zig");
const ctx = @import("ctx.zig");
const rt = @import("rt.zig");
const vm_mod = @import("../interp/vm.zig");
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
    AccessDenied,
    Unexpected,
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
    /// Pending runtime error branches to patch
    err_branches: std.ArrayList(usize),
    /// Pending self-call BLs to patch with entry point after compilation
    self_call_patches: std.ArrayList(usize),
    /// Number of items on the JIT stack above the self-call function slot.
    /// Set when load_global detects the closure matches the chunk being compiled.
    /// Reset after a call/tail_call consumes it, or when any other op invalidates tracking.
    self_call_depth: ?u8 = null,

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
            .err_branches = std.ArrayList(usize){},
            .self_call_patches = std.ArrayList(usize){},
        };
    }

    pub fn deinit(self: *Jit) void {
        self.code_buffer.deinit();
        self.labels.deinit();
        self.pending_jumps.deinit(self.allocator);
        self.err_branches.deinit(self.allocator);
        self.self_call_patches.deinit(self.allocator);
    }

    /// Compile a bytecode chunk to native code
    /// Compile a bytecode chunk to native code.
    /// `globals` is the VM's globals array for resolving self-call patterns.
    pub fn compile(self: *Jit, chunk: *const Chunk, globals: []const Value) JitError!JitFn {
        const start_pos = self.code_buffer.pos;
        const reloc_start = self.code_buffer.relocs.items.len;
        var ok = false;
        defer {
            if (!ok) {
                self.code_buffer.pos = start_pos;
                self.code_buffer.relocs.items = self.code_buffer.relocs.items[0..reloc_start];
                self.labels.clearRetainingCapacity();
                self.pending_jumps.clearRetainingCapacity();
                self.err_branches.clearRetainingCapacity();
                if (self.code_buffer.setWritable(false)) |_| {} else |err| {
                    std.debug.panic("jit rollback failed: {s}", .{@errorName(err)});
                }
            }
        }
        self.fn_start = start_pos;
        self.labels.clearRetainingCapacity();
        self.pending_jumps.clearRetainingCapacity();
        self.err_branches.clearRetainingCapacity();
        self.self_call_patches.clearRetainingCapacity();
        self.self_call_depth = null;
        try self.code_buffer.setWritable(true);

        _ = try patch.patchStencil(&self.code_buffer, stencils.prologue_stencil, &[_]patch.PatchValue{});

        // Self-calls BL to entry (after prologue). The self-call code
        // directly sets x19/x23 and updates ctx before the BL, so the
        // prologue register loading is not needed. The BL/RET pair uses
        // the ARM64 stack for the link register (x30). We save/restore
        // x23/x19 around the BL ourselves.
        //
        // Note: We can't skip the prologue entirely because the function
        // body may contain `ret` which emits the full epilogue. The epilogue
        // restores registers from the prologue's saves. So we need to go
        // through the prologue to set up the matching save/restore pairs.
        const self_call_target = start_pos; // = fn_start (through prologue)

        // Can this function use self-call optimization?
        // Only for exact-arity functions (no rest, no optional, no keyword).
        const can_self_call = chunk.has_rest == 0 and chunk.opt_count == 0 and chunk.key_count == 0;

        var bc_offset: usize = 0;
        const code = chunk.getCode();
        while (bc_offset < code.len) {
            // Record label for this bytecode offset
            try self.labels.put(bc_offset, self.code_buffer.pos);

            const op_raw = chunk.readU16(bc_offset);
            const op: Op = @enumFromInt(op_raw);
            bc_offset += 2;

            try self.compileOpSelfCall(op, chunk, &bc_offset, globals, can_self_call);
        }

        // Patch forward jumps
        try self.patchPendingJumps();
        // Emit error handler and patch runtime error branches
        try self.emitErrorHandler();
        // Patch self-call BLs to branch to fn_start (prologue entry)
        try self.patchSelfCalls(self_call_target);
        try self.code_buffer.setWritable(false);

        ok = true;
        return self.code_buffer.getFnPtr(JitFn, self.fn_start);
    }

    /// Wrapper around compileOp that detects self-recursive call patterns.
    ///
    /// Self-call detection works as follows:
    /// 1. On `load_global X`: check if globals[X] is a closure whose chunk
    ///    matches the chunk being compiled. If so, set self_call_depth = 0.
    /// 2. On each push instruction: increment self_call_depth.
    /// 3. On `call N` or `tail_call N`: if self_call_depth == N, this is
    ///    a self-call. Emit optimized native self-call sequence.
    /// 4. On anything that invalidates tracking (jump, pop, etc.): reset.
    fn compileOpSelfCall(
        self: *Jit,
        op: Op,
        chunk: *const Chunk,
        bc_offset: *usize,
        globals: []const Value,
        can_self_call: bool,
    ) JitError!void {
        if (!can_self_call) {
            return self.compileOp(op, chunk, bc_offset);
        }

        switch (op) {
            .load_global => {
                // Check if this global holds a closure for the chunk being compiled.
                const idx = chunk.readU16(bc_offset.*);
                if (idx < globals.len and globals[idx].isClosure()) {
                    const cls = globals[idx].toPtr(runtime.Closure);
                    if (cls.code.isChunk()) {
                        const target_chunk = cls.code.toPtr(Chunk);
                        if (target_chunk == chunk) {
                            // This load_global pushes a self-referencing closure.
                            // Don't set depth yet — it's set after we emit the push.
                            try self.compileOp(op, chunk, bc_offset);
                            self.self_call_depth = 0;
                            return;
                        }
                    }
                }
                // Not a self-reference: reset tracking
                self.self_call_depth = null;
                return self.compileOp(op, chunk, bc_offset);
            },

            .call, .tail_call => {
                const argc = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;

                if (self.self_call_depth) |depth| {
                    if (depth == argc) {
                        // Self-call detected! Emit optimized native sequence.
                        try self.emitSelfCall(chunk, argc, op == .tail_call);
                        self.self_call_depth = null;
                        return;
                    }
                }

                // Not a self-call: emit normal call via callFast
                self.self_call_depth = null;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, argc) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_callFast));
                return;
            },

            // Instructions that push one value to the stack
            .push_nil, .push_t, .push_i32, .load_local, .push_const, .dup => {
                if (self.self_call_depth) |d| {
                    try self.compileOp(op, chunk, bc_offset);
                    self.self_call_depth = d + 1;
                    return;
                }
                return self.compileOp(op, chunk, bc_offset);
            },

            // Binary ops consume 2, push 1: net -1
            .add, .sub, .mul, .lt, .gt, .le, .ge, .num_eq, .eq, .eql => {
                if (self.self_call_depth) |d| {
                    try self.compileOp(op, chunk, bc_offset);
                    if (d > 0) {
                        self.self_call_depth = d - 1;
                    } else {
                        self.self_call_depth = null;
                    }
                    return;
                }
                return self.compileOp(op, chunk, bc_offset);
            },

            // Unary ops that keep stack depth unchanged
            .check_fixnum, .check_cons, .check_symbol, .check_string,
            .check_vector, .check_closure, .check_non_nil, .check_list,
            => {
                // These peek TOS, don't change depth
                return self.compileOp(op, chunk, bc_offset);
            },

            // Specialized binary ops: consume 2, push 1: net -1
            .fixnum_add, .fixnum_sub, .fixnum_mul => {
                if (self.self_call_depth) |d| {
                    try self.compileOp(op, chunk, bc_offset);
                    if (d > 0) {
                        self.self_call_depth = d - 1;
                    } else {
                        self.self_call_depth = null;
                    }
                    return;
                }
                return self.compileOp(op, chunk, bc_offset);
            },

            // Anything else invalidates self-call tracking
            else => {
                self.self_call_depth = null;
                return self.compileOp(op, chunk, bc_offset);
            },
        }
    }

    fn compileOp(self: *Jit, op: Op, chunk: *const Chunk, bc_offset: *usize) JitError!void {
        switch (op) {
            .push_nil => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.push_nil_stencil, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },

            .push_t => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.push_t_stencil, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },

            .push_i32 => {
                const val = chunk.readI32(bc_offset.*);
                bc_offset.* += 4;
                // Create tagged fixnum
                const tagged = Value.makeFixnum(val);
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = tagged.raw },
                });
                try self.emitStackPush();
            },

            .push_const => {
                const idx = chunk.readU16(bc_offset.*);
                bc_offset.* += 2;
                if (idx >= chunk.getConstants().len) return error.InvalidConstantIndex;
                const offset_bytes: u32 = @as(u32, idx) * @as(u32, @intCast(@sizeOf(Value)));
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_const, &[_]patch.PatchValue{
                    .{ .imm32 = offset_bytes },
                });
                try self.emitStackPush();
            },

            .add => {
                try self.emitBinaryArith(stencils.add_fixnum, @intFromPtr(rt.j_add));
            },

            .sub => {
                try self.emitBinaryArith(stencils.sub_fixnum, @intFromPtr(rt.j_sub));
            },

            .mul => {
                try self.emitBinaryMul(@intFromPtr(rt.j_mul));
            },

            .neg => {
                try self.emitUnaryNeg(@intFromPtr(rt.j_neg));
            },

            .div => {
                try self.emitBinaryCall(@intFromPtr(rt.j_div));
            },

            .mod => {
                try self.emitBinaryCall(@intFromPtr(rt.j_mod));
            },

            .eq => {
                try self.emitStackPopX1();
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.eq_stencil, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },
            .equal => {
                try self.emitBinaryCall(@intFromPtr(rt.j_equal));
            },
            .eql => {
                try self.emitBinaryCall(@intFromPtr(rt.j_eql));
            },
            .equalp => {
                try self.emitBinaryCall(@intFromPtr(rt.j_equalp));
            },

            .lt => {
                try self.emitBinaryCompare(stencils.lt_stencil, @intFromPtr(rt.j_lt));
            },

            .gt => {
                try self.emitBinaryCompare(stencils.gt_stencil, @intFromPtr(rt.j_gt));
            },

            .le => {
                try self.emitBinaryCompare(stencils.le_stencil, @intFromPtr(rt.j_le));
            },

            .ge => {
                try self.emitBinaryCompare(stencils.ge_stencil, @intFromPtr(rt.j_ge));
            },

            .num_eq => {
                try self.emitBinaryCompare(stencils.eq_stencil, @intFromPtr(rt.j_numEq));
            },

            .not => {
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.not_stencil, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },

            .nilp => {
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.nilp_stencil, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },

            .numberp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_numberp));
                try self.emitStackPush();
            },
            .integerp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_integerp));
                try self.emitStackPush();
            },
            .realp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_realp));
                try self.emitStackPush();
            },
            .consp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_consp));
                try self.emitStackPush();
            },
            .symbolp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_symbolp));
                try self.emitStackPush();
            },
            .stringp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_stringp));
                try self.emitStackPush();
            },
            .vectorp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_vectorp));
                try self.emitStackPush();
            },
            .closurep => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_closurep));
                try self.emitStackPush();
            },
            .keywordp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_keywordp));
                try self.emitStackPush();
            },
            .characterp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_characterp));
                try self.emitStackPush();
            },
            .floatp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_floatp));
                try self.emitStackPush();
            },
            .listp => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_listp));
                try self.emitStackPush();
            },
            .atom => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_atom));
                try self.emitStackPush();
            },
            .char_code => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_charCode));
                try self.emitStackPush();
            },
            .code_char => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_codeChar));
                try self.emitStackPush();
            },
            .char_eq => {
                try self.emitBinaryCall(@intFromPtr(rt.j_charEq));
            },
            .char_lt => {
                try self.emitBinaryCall(@intFromPtr(rt.j_charLt));
            },
            .char_gt => {
                try self.emitBinaryCall(@intFromPtr(rt.j_charGt));
            },
            .char_upcase => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_charUpcase));
                try self.emitStackPush();
            },
            .char_downcase => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_charDowncase));
                try self.emitStackPush();
            },
            .digit_char_p => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_digitCharP));
                try self.emitStackPush();
            },
            .alpha_char_p => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_alphaCharP));
                try self.emitStackPush();
            },
            .string_upcase => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_stringUpcase));
                try self.emitStackPush();
            },
            .string_downcase => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_stringDowncase));
                try self.emitStackPush();
            },
            .write_to_string => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_writeToString));
                try self.emitStackPush();
            },
            .str_ref => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strRef));
            },
            .str_len => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_strLen));
                try self.emitStackPush();
            },
            .str_set => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_strSet));
            },
            .str_concat => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strConcat));
            },
            .str_eq => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strEq));
            },
            .str_lt => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strLt));
            },
            .str_gt => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strGt));
            },
            .str_le => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strLe));
            },
            .str_ge => {
                try self.emitBinaryCall(@intFromPtr(rt.j_strGe));
            },
            .random => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_random));
                try self.emitStackPush();
            },
            .random_seed => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_randomSeed));
                try self.emitStackPush();
            },
            .write => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_write));
                try self.emitStackPush();
            },
            .print => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_print));
                try self.emitStackPush();
            },
            .princ => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_princ));
                try self.emitStackPush();
            },
            .terpri => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_terpri));
                try self.emitStackPush();
            },
            .write_char => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_writeChar));
                try self.emitStackPush();
            },

            .list_length => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_listLength));
                try self.emitStackPush();
            },
            .list_member => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listMember));
            },
            .list_member_eql => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listMemberEql));
            },
            .list_member_equal => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listMemberEqual));
            },
            .assoc => {
                try self.emitBinaryCall(@intFromPtr(rt.j_assoc));
            },
            .assoc_eql => {
                try self.emitBinaryCall(@intFromPtr(rt.j_assocEql));
            },
            .assoc_equal => {
                try self.emitBinaryCall(@intFromPtr(rt.j_assocEqual));
            },
            .list_find => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listFind));
            },
            .list_find_eq => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listFindEq));
            },
            .list_find_equal => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listFindEqual));
            },
            .list_position => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listPosition));
            },
            .list_count => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listCount));
            },
            .list_count_eq => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listCountEq));
            },
            .list_count_equal => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listCountEqual));
            },
            .list_remove => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listRemove));
            },
            .list_remove_eq => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listRemoveEq));
            },
            .list_remove_equal => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listRemoveEqual));
            },
            .list_last => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_listLast));
                try self.emitStackPush();
            },

            .list_reverse => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_listReverse));
                try self.emitStackPush();
            },

            .append_lists => {
                try self.emitBinaryCall(@intFromPtr(rt.j_appendLists));
            },

            .list_nth => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listNth));
            },

            .list_nthcdr => {
                try self.emitBinaryCall(@intFromPtr(rt.j_listNthcdr));
            },

            .rplaca => {
                try self.emitBinaryCall(@intFromPtr(rt.j_rplaca));
            },

            .rplacd => {
                try self.emitBinaryCall(@intFromPtr(rt.j_rplacd));
            },

            .dup => {
                try self.emitStackPop();
                try self.emitStackPush();
                try self.emitStackPush();
            },

            .pop => {
                try self.emitStackPop();
            },

            .swap => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.swap_stencil, &[_]patch.PatchValue{});
            },

            .ret => {
                try self.emitStackPop();
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

                try self.emitStackPop();
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

                try self.emitStackPop();
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

            .cons => {
                try self.emitBinaryCall(@intFromPtr(rt.j_cons));
            },

            .car => {
                try self.emitStackPop();
                const nil_branch_offset = self.code_buffer.pos;
                const nil_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + nil_branch_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_nil, &[_]patch.PatchValue{
                    .{ .addr = nil_inst_addr },
                });

                const guard_offset = try patch.patchStencil(&self.code_buffer, stencils.guard_cons_x0, &[_]patch.PatchValue{});
                const guard_branch_offset = guard_offset + stencils.guard_cons_x0_branch_offset;

                _ = try patch.patchStencil(&self.code_buffer, stencils.car_stencil, &[_]patch.PatchValue{});

                const fast_branch_offset = self.code_buffer.pos;
                const fast_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + fast_branch_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
                    .{ .addr = fast_inst_addr },
                });

                const nil_code_offset = self.code_buffer.pos;
                _ = try patch.patchStencil(&self.code_buffer, stencils.push_nil_stencil, &[_]patch.PatchValue{});

                const end_code_offset = self.code_buffer.pos;
                try self.emitStackPush();
                const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;
                const nil_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + nil_code_offset;

                try self.patchBranch(nil_branch_offset, nil_target_addr, .rel19);
                try self.patchBranch(guard_branch_offset, nil_target_addr, .rel19);
                try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
                patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
            },

            .cdr => {
                try self.emitStackPop();
                const nil_branch_offset = self.code_buffer.pos;
                const nil_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + nil_branch_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_nil, &[_]patch.PatchValue{
                    .{ .addr = nil_inst_addr },
                });

                try self.emitGuardConsX0();

                _ = try patch.patchStencil(&self.code_buffer, stencils.cdr_stencil, &[_]patch.PatchValue{});

                const fast_branch_offset = self.code_buffer.pos;
                const fast_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + fast_branch_offset;
                _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
                    .{ .addr = fast_inst_addr },
                });

                const end_code_offset = self.code_buffer.pos;
                try self.emitStackPush();
                const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;

                try self.patchBranch(nil_branch_offset, end_target_addr, .rel19);
                try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
                patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
            },

            .load_local => {
                const idx = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                const offset_bytes: u32 = @as(u32, idx) * @as(u32, @intCast(@sizeOf(Value)));
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_local, &[_]patch.PatchValue{
                    .{ .imm32 = offset_bytes },
                });
                try self.emitStackPush();
            },

            .store_local => {
                const idx = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                try self.emitStackPop();
                const offset_bytes: u32 = @as(u32, idx) * @as(u32, @intCast(@sizeOf(Value)));
                _ = try patch.patchStencil(&self.code_buffer, stencils.store_local, &[_]patch.PatchValue{
                    .{ .imm32 = offset_bytes },
                });
            },

            .load_capture => {
                const idx = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, idx) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_loadCapture));
                try self.emitStackPush();
            },
            .load_upvalue => {
                _ = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                const idx = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, idx) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_loadUpvalue));
                try self.emitStackPush();
            },
            .store_upvalue => {
                _ = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                const idx = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, idx) },
                });
                _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
                try self.emitStackPop();
                try self.emitCallBinary(@intFromPtr(rt.j_storeUpvalue));
            },
            .load_global => {
                const idx = chunk.readU16(bc_offset.*);
                bc_offset.* += 2;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, idx) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_loadGlobal));
                try self.emitStackPush();
            },
            .store_global => {
                const idx = chunk.readU16(bc_offset.*);
                bc_offset.* += 2;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, idx) },
                });
                _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
                try self.emitStackPop();
                try self.emitCallBinary(@intFromPtr(rt.j_storeGlobal));
            },
            .load_argc => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_loadArgc));
                try self.emitStackPush();
            },
            .make_vec => {
                _ = chunk.readU16(bc_offset.*);
                bc_offset.* += 2;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_makeVec));
            },
            .make_vec_n => {
                const count = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, count) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_makeVecN));
            },
            .vec_ref => {
                try self.emitBinaryCall(@intFromPtr(rt.j_vecRef));
            },
            .vec_set => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_vecSet));
            },
            .vec_len => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_vecLen));
                try self.emitStackPush();
            },
            .vec_fill_ptr => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_vecFillPtr));
                try self.emitStackPush();
            },
            .vec_push => {
                try self.emitBinaryCall(@intFromPtr(rt.j_vecPush));
            },
            .vec_push_ext => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_vecPushExt));
            },
            .vec_pop => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_vecPop));
                try self.emitStackPush();
            },
            .vec_set_fill_ptr => {
                try self.emitBinaryCall(@intFromPtr(rt.j_vecSetFillPtr));
            },
            .vec_set_adjustable => {
                try self.emitBinaryCall(@intFromPtr(rt.j_vecSetAdjustable));
            },
            .vec_adjust => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_vecAdjust));
            },
            .copy_structure => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_copyStructure));
                try self.emitStackPush();
            },
            .function_lambda_expression => {
                try self.emitStackPop();
                try self.emitCallUnary(@intFromPtr(rt.j_functionLambdaExpression));
                try self.emitStackPush();
            },
            .call, .tail_call => {
                const argc = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, argc) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_callFast));
            },
            .apply => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = 0 },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_apply));
            },
            .make_list => {
                const count = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, count) },
                });
                try self.emitCallUnary(@intFromPtr(rt.j_makeList));
            },
            .make_closure => {
                const chunk_idx = chunk.readU16(bc_offset.*);
                bc_offset.* += 2;
                const num_captures = chunk.readU8(bc_offset.*);
                bc_offset.* += 1;
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, num_captures) },
                });
                _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
                _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                    .{ .imm64 = @as(u64, chunk_idx) },
                });
                try self.emitCallBinary(@intFromPtr(rt.j_makeClosure));
            },

            // Specialized (type-proven) operations — no guards, no slow path
            .fixnum_add => {
                try self.emitStackPopX1();
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.spec_fixnum_add, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },
            .fixnum_sub => {
                try self.emitStackPopX1();
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.spec_fixnum_sub, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },
            .fixnum_mul => {
                try self.emitStackPopX1();
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.spec_fixnum_mul, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },
            .unsafe_car => {
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.spec_unsafe_car, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },
            .unsafe_cdr => {
                try self.emitStackPop();
                _ = try patch.patchStencil(&self.code_buffer, stencils.spec_unsafe_cdr, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },
            .direct_aref => {
                try self.emitStackPopX1(); // index
                try self.emitStackPop(); // vector
                _ = try patch.patchStencil(&self.code_buffer, stencils.spec_direct_aref, &[_]patch.PatchValue{});
                try self.emitStackPush();
            },

            // Type assertion checks: peek TOS, guard type, error if mismatch
            .check_fixnum => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const branch = try self.emitGuardFixnumX0();
                try self.err_branches.append(self.allocator, branch);
            },
            .check_cons => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_cons, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_cons_branch_offset);
            },
            .check_vector => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_vector, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_vector_branch_offset);
            },
            .check_symbol => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_symbol, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_symbol_branch_offset);
            },
            .check_string => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_string, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_string_branch_offset);
            },
            .check_closure => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_closure, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_closure_branch_offset);
            },
            .check_non_nil => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_non_nil, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_non_nil_branch_offset);
            },
            .check_list => {
                _ = try patch.patchStencil(&self.code_buffer, stencils.peek_tos, &[_]patch.PatchValue{});
                const start = try patch.patchStencil(&self.code_buffer, stencils.guard_check_list, &[_]patch.PatchValue{});
                try self.err_branches.append(self.allocator, start + stencils.guard_check_list_branch_offset);
            },

            else => return error.UnsupportedOpcode,
        }
    }

    fn emitRuntimeCheck(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.runtime_check, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.runtime_check_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    fn emitStackPush(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.stack_push, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.stack_push_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    fn emitStackPushX1(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.stack_push_x1, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.stack_push_x1_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    fn emitStackPop(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.stack_pop, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.stack_pop_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    fn emitLoadCtxSp(self: *Jit) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.load_ctx_sp, &[_]patch.PatchValue{});
    }

    fn emitStackPopX1(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.stack_pop_x1, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.stack_pop_x1_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    // C-ABI runtime check: only checks retbuf.err, result stays in x0.
    fn emitRuntimeCheckC(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.runtime_check_c, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.runtime_check_c_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    // C-ABI call: x0=ctx, x1=arg, return u64 in x0. Errors via retbuf.
    fn emitCallUnary(self: *Jit, addr: usize) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x0_x22, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.store_ctx_sp, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.clear_retbuf_err, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.call_abs, &[_]patch.PatchValue{
            .{ .imm64 = addr },
        });
        // Runtime calls may trigger GC; refresh ctx-derived state.
        try self.emitLoadCtxSp();
        try self.emitRuntimeCheckC();
    }

    // C-ABI call: x0=ctx, x1=arg0, x2=arg1, return u64 in x0. Errors via retbuf.
    fn emitCallBinary(self: *Jit, addr: usize) JitError!void {
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x2_x1, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x1_x0, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x0_x22, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.store_ctx_sp, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.clear_retbuf_err, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.call_abs, &[_]patch.PatchValue{
            .{ .imm64 = addr },
        });
        // Runtime calls may trigger GC; refresh ctx-derived state.
        try self.emitLoadCtxSp();
        try self.emitRuntimeCheckC();
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

    fn emitGuardConsX0(self: *Jit) JitError!void {
        const start = try patch.patchStencil(&self.code_buffer, stencils.guard_cons_x0, &[_]patch.PatchValue{});
        const branch_offset = start + stencils.guard_cons_x0_branch_offset;
        try self.err_branches.append(self.allocator, branch_offset);
    }

    fn emitMulOverflowCheck(self: *Jit) JitError!usize {
        const start = try patch.patchStencil(&self.code_buffer, stencils.mul_overflow_check, &[_]patch.PatchValue{});
        return start + stencils.mul_overflow_check_branch_offset;
    }

    fn emitFixnumRangeCheck(self: *Jit) JitError!usize {
        const start = try patch.patchStencil(&self.code_buffer, stencils.fixnum_range_check, &[_]patch.PatchValue{});
        return start + stencils.fixnum_range_check_branch_offset;
    }

    fn emitBinaryCall(self: *Jit, addr: usize) JitError!void {
        try self.emitStackPopX1();
        try self.emitStackPop();
        try self.emitCallBinary(addr);
        try self.emitStackPush();
    }

    fn emitBinaryCompare(self: *Jit, fast: Stencil, slow_addr: usize) JitError!void {
        try self.emitStackPopX1();
        try self.emitStackPop();

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
        try self.emitStackPush();
        const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;

        try self.patchBranch(guard_x0_branch, slow_target_addr, .rel19);
        try self.patchBranch(guard_x1_branch, slow_target_addr, .rel19);
        try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn emitBinaryArith(self: *Jit, fast: Stencil, slow_addr: usize) JitError!void {
        try self.emitStackPopX1();
        try self.emitStackPop();

        const guard_x0_branch = try self.emitGuardFixnumX0();
        const guard_x1_branch = try self.emitGuardFixnumX1();

        _ = try patch.patchStencil(&self.code_buffer, fast, &[_]patch.PatchValue{});
        const range_branch = try self.emitFixnumRangeCheck();

        const fast_branch_offset = self.code_buffer.pos;
        const fast_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + fast_branch_offset;
        _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
            .{ .addr = fast_inst_addr },
        });

        const slow_code_offset = self.code_buffer.pos;
        const slow_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + slow_code_offset;
        try self.emitCallBinary(slow_addr);

        const end_code_offset = self.code_buffer.pos;
        try self.emitStackPush();
        const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;

        try self.patchBranch(guard_x0_branch, slow_target_addr, .rel19);
        try self.patchBranch(guard_x1_branch, slow_target_addr, .rel19);
        try self.patchBranch(range_branch, slow_target_addr, .rel19);
        try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn emitBinaryMul(self: *Jit, slow_addr: usize) JitError!void {
        try self.emitStackPopX1();
        try self.emitStackPop();

        const guard_x0_branch = try self.emitGuardFixnumX0();
        const guard_x1_branch = try self.emitGuardFixnumX1();

        _ = try patch.patchStencil(&self.code_buffer, stencils.mul_fixnum, &[_]patch.PatchValue{});
        const overflow_branch = try self.emitMulOverflowCheck();
        const range_branch = try self.emitFixnumRangeCheck();

        const fast_branch_offset = self.code_buffer.pos;
        const fast_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + fast_branch_offset;
        _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
            .{ .addr = fast_inst_addr },
        });

        const slow_code_offset = self.code_buffer.pos;
        const slow_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + slow_code_offset;
        try self.emitCallBinary(slow_addr);

        const end_code_offset = self.code_buffer.pos;
        try self.emitStackPush();
        const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;

        try self.patchBranch(guard_x0_branch, slow_target_addr, .rel19);
        try self.patchBranch(guard_x1_branch, slow_target_addr, .rel19);
        try self.patchBranch(overflow_branch, slow_target_addr, .rel19);
        try self.patchBranch(range_branch, slow_target_addr, .rel19);
        try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn emitUnaryNeg(self: *Jit, slow_addr: usize) JitError!void {
        try self.emitStackPop();

        const guard_x0_branch = try self.emitGuardFixnumX0();

        _ = try patch.patchStencil(&self.code_buffer, stencils.neg_fixnum, &[_]patch.PatchValue{});
        const range_branch = try self.emitFixnumRangeCheck();

        const fast_branch_offset = self.code_buffer.pos;
        const fast_inst_addr = @intFromPtr(self.code_buffer.memory.ptr) + fast_branch_offset;
        _ = try patch.patchStencil(&self.code_buffer, stencils.branch_stencil, &[_]patch.PatchValue{
            .{ .addr = fast_inst_addr },
        });

        const slow_code_offset = self.code_buffer.pos;
        const slow_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + slow_code_offset;
        try self.emitCallUnary(slow_addr);

        const end_code_offset = self.code_buffer.pos;
        try self.emitStackPush();
        const end_target_addr = @intFromPtr(self.code_buffer.memory.ptr) + end_code_offset;

        try self.patchBranch(guard_x0_branch, slow_target_addr, .rel19);
        try self.patchBranch(range_branch, slow_target_addr, .rel19);
        try self.patchBranch(fast_branch_offset, end_target_addr, .rel26);
        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn patchPendingJumps(self: *Jit) JitError!void {
        for (self.pending_jumps.items) |jump| {
            const target_code_addr = self.labels.get(jump.target_bc_offset) orelse
                return error.InvalidJumpTarget;

            const target_addr = @intFromPtr(self.code_buffer.memory.ptr) + target_code_addr;
            try self.patchBranch(jump.code_offset, target_addr, jump.hole_type);
        }

        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn emitErrorHandler(self: *Jit) JitError!void {
        if (self.err_branches.items.len == 0) return;

        const handler_offset = self.code_buffer.pos;
        _ = try patch.patchStencil(&self.code_buffer, stencils.store_err, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.push_nil_stencil, &[_]patch.PatchValue{});
        _ = try patch.patchStencil(&self.code_buffer, stencils.epilogue_stencil, &[_]patch.PatchValue{});

        const handler_addr = @intFromPtr(self.code_buffer.memory.ptr) + handler_offset;
        for (self.err_branches.items) |branch_offset| {
            try self.patchBranch(branch_offset, handler_addr, .rel19);
        }
        patch.flushIcache(self.code_buffer.memory.ptr, self.code_buffer.pos);
    }

    fn patchBranch(self: *Jit, code_offset: usize, target_addr: usize, hole_type: stencils.HoleType) JitError!void {
        try self.code_buffer.setWritable(true);
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
        try self.code_buffer.recordReloc(code_offset, hole_type, target_addr);
        patch.flushIcache(self.code_buffer.memory.ptr + code_offset, 4);
    }
    /// Emit a self-recursive call sequence.
    ///
    /// Stack layout before call: [..., closure, arg0, ..., argN-1]
    ///                                 ^--- x19 points here after last arg
    ///
    /// VM doCall behavior (which we replicate):
    ///   new_bp = sp - argc - 1 (position of closure slot)
    ///   stack[new_bp + i] = stack[new_bp + 1 + i]  for i in 0..argc
    ///   (shifts args down by 1, overwriting closure)
    ///   push nil for extra locals
    ///   frame_base = &stack[new_bp]
    ///
    /// For non-tail calls:
    ///   1. Save caller's x23/x19 to ARM64 stack
    ///   2. Set x23 = x19 - (argc+1)*8 (new frame_base, at closure slot)
    ///   3. Shift args down by 1 slot (overwrite closure with arg0, etc.)
    ///   4. Initialize extra locals to nil
    ///   5. Update JitContext (frame_base, sp)
    ///   6. BL <fn_start> (through prologue to reload regs from ctx)
    ///   7. Restore caller's x23/x19, ctx.frame_base, refresh x20
    ///   8. Adjust x19: pop argc+1 slots, push result
    ///
    /// For tail calls:
    ///   Falls back to callFast (TODO: implement native tail self-call).
    fn emitSelfCall(self: *Jit, chunk: *const Chunk, argc: u8, is_tail: bool) JitError!void {
        const num_locals: u16 = chunk.num_locals;
        const extra_locals: u16 = num_locals -| argc;

        if (is_tail) {
            // Tail self-call: fall back to callFast for now.
            _ = try patch.patchStencil(&self.code_buffer, stencils.load_imm64, &[_]patch.PatchValue{
                .{ .imm64 = @as(u64, argc) },
            });
            try self.emitCallUnary(@intFromPtr(rt.j_callFast));
            return;
        }

        // Non-tail self-call:

        // Step 1: Save caller frame state to ARM64 stack
        // STP x23, x19, [ARM64_sp, #-16]!
        try self.emitRaw(&stencils.inst_bytes(stencils.stp_pre(stencils.X23, stencils.X19, stencils.SP, -2)));

        // Step 2: Set new frame_base = x19 - (argc+1)*8 (at closure slot position)
        const frame_offset: u12 = (@as(u12, argc) + 1) * 8;
        try self.emitRaw(&stencils.inst_bytes(stencils.sub_imm(stencils.X23, stencils.X19, frame_offset)));

        // Step 3: Shift args down by 1 slot (overwrite closure with arg0, etc.)
        // x23[i] = x23[i+1] for i in 0..argc
        // Use x9 as temp register
        var i: u8 = 0;
        while (i < argc) : (i += 1) {
            // LDR x9, [x23, #(i+1)*8]
            const src_off: u12 = (@as(u12, i) + 1) * 8;
            try self.emitRaw(&stencils.inst_bytes(stencils.ldr_imm(9, stencils.X23, src_off)));
            // STR x9, [x23, #i*8]
            const dst_off: u12 = @as(u12, i) * 8;
            try self.emitRaw(&stencils.inst_bytes(stencils.str_imm(9, stencils.X23, dst_off)));
        }

        // Step 4: Set x19 = x23 + argc*8 (past the shifted args)
        const args_bytes: u12 = @as(u12, argc) * 8;
        try self.emitRaw(&stencils.inst_bytes(stencils.add_imm(stencils.X19, stencils.X23, args_bytes)));

        // Push nil for extra locals (num_locals - argc)
        var j: u16 = 0;
        while (j < extra_locals) : (j += 1) {
            // STR xzr, [x19], #8
            try self.emitRaw(&stencils.inst_bytes(0xF800841F));
        }

        // Step 5: Update JitContext
        // STR x23, [x22, #16]  — ctx.frame_base
        try self.emitRaw(&stencils.inst_bytes(0xF9000AD7));
        // STR x19, [x22, #0]   — ctx.sp
        try self.emitRaw(&stencils.inst_bytes(0xF90002D3));

        // Step 6: Call through prologue
        // MOV x0, x22  (pass ctx)
        _ = try patch.patchStencil(&self.code_buffer, stencils.mov_x0_x22, &[_]patch.PatchValue{});
        // BL <fn_start> — patched later to actual entry
        const bl_offset = self.code_buffer.pos;
        try self.emitRaw(&stencils.inst_bytes(stencils.bl_placeholder()));
        try self.self_call_patches.append(self.allocator, bl_offset);

        // Step 7: Restore caller frame state
        // After return, x0 has the result value.
        // LDP x23, x19, [ARM64_sp], #16
        try self.emitRaw(&stencils.inst_bytes(stencils.ldp_post(stencils.X23, stencils.X19, stencils.SP, 2)));

        // Restore JitContext.frame_base for GC safety
        // STR x23, [x22, #16]
        try self.emitRaw(&stencils.inst_bytes(0xF9000AD7));

        // Refresh const_pool from ctx (GC may have moved it)
        // LDR x20, [x22, #8]
        try self.emitRaw(&stencils.inst_bytes(0xF94006D4));

        // Step 8: Pop closure+args from caller's stack, push result
        // SUB x19, x19, #((argc+1) * 8)  — pop closure + args
        const pop_bytes: u12 = (@as(u12, argc) + 1) * 8;
        try self.emitRaw(&stencils.inst_bytes(stencils.sub_imm(stencils.X19, stencils.X19, pop_bytes)));
        // STR x0, [x19], #8 — push result
        try self.emitRaw(&stencils.inst_bytes(0xF8008260));
    }

    /// Emit raw instruction bytes to the code buffer.
    fn emitRaw(self: *Jit, bytes: []const u8) JitError!void {
        if (self.code_buffer.pos + bytes.len > self.code_buffer.memory.len) return error.CodeTooLarge;
        @memcpy(self.code_buffer.memory[self.code_buffer.pos..][0..bytes.len], bytes);
        self.code_buffer.pos += bytes.len;
    }

    /// Patch all self-call BLs to branch to the entry point (after prologue).
    fn patchSelfCalls(self: *Jit, entry_after_prologue: usize) JitError!void {
        if (self.self_call_patches.items.len == 0) return;

        const entry_addr = @intFromPtr(self.code_buffer.memory.ptr) + entry_after_prologue;
        for (self.self_call_patches.items) |bl_offset| {
            try self.patchBranch(bl_offset, entry_addr, .rel26);
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

fn expectListVals(list: Value, expected: []const i64) !void {
    var cur = list;
    for (expected) |val| {
        try std.testing.expect(cur.isCons());
        const cons = cur.toPtr(runtime.Cons);
        try std.testing.expect(cons.car.isFixnum());
        try std.testing.expectEqual(val, cons.car.toFixnum());
        cur = cons.cdr;
    }
    try std.testing.expect(cur.isNil());
}

fn expectVecVals(vec_val: Value, expected: []const i64) !void {
    try std.testing.expect(vec_val.isVector());
    const vec = vec_val.toPtr(runtime.Vector);
    try std.testing.expectEqual(@as(u64, expected.len), vec.length);
    for (expected, 0..) |val, i| {
        const item = vec.data[i];
        try std.testing.expect(item.isFixnum());
        try std.testing.expectEqual(val, item.toFixnum());
    }
}

test "jit init" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    try testing.expectEqual(@as(usize, 0), jit.code_buffer.pos);
}

test "jit compile rollback on error" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const bad_op: u16 = @intFromEnum(Op.math_ext);
    const code = [_]u8{
        @truncate(bad_op & 0xFF), @truncate(bad_op >> 8),
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

    const start_pos = jit.code_buffer.pos;
    try testing.expectError(error.UnsupportedOpcode, jit.compile(&chunk, &.{}));
    try testing.expectEqual(start_pos, jit.code_buffer.pos);
    try testing.expect(!jit.code_buffer.writable);
}

test "jit compile simple" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    // push_i32 42; ret
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        42, 0, 0, 0,
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

    const fn_ptr = try jit.compile(&chunk, &.{});

    // Verify function was compiled (on ARM64, we could call it)
    try testing.expect(@intFromPtr(fn_ptr) != 0);
    try testing.expect(jit.labels.count() > 0);
    const err_len =
        stencils.store_err.code.len +
        stencils.push_nil_stencil.code.len +
        stencils.epilogue_stencil.code.len;
    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len +
        err_len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit compile jump" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const jmp_op: u16 = @intFromEnum(Op.jmp);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(jmp_op & 0xFF), @truncate(jmp_op >> 8),
        0, 0,
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

    _ = try jit.compile(&chunk, &.{});

    const err_len =
        stencils.store_err.code.len +
        stencils.push_nil_stencil.code.len +
        stencils.epilogue_stencil.code.len;
    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.branch_stencil.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len +
        err_len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);

    const branch_off = stencils.prologue_stencil.code.len;
    const inst = std.mem.readInt(u32, jit.code_buffer.memory[branch_off .. branch_off + 4], .little);
    const word_off = inst & 0x03FFFFFF;
    try testing.expectEqual(@as(u32, 1), word_off);
}

test "jit relocations reapply after move" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const jmp_nil_op: u16 = @intFromEnum(Op.jmp_nil);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const jmp_op: u16 = @intFromEnum(Op.jmp);
    const ret_op: u16 = @intFromEnum(Op.ret);

    // (push_nil)
    // (jmp_nil +10) ; jump to push_i32 2
    // (push_i32 1)
    // (jmp +6)      ; jump to ret
    // (push_i32 2)
    // (ret)
    const code = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(jmp_nil_op & 0xFF), @truncate(jmp_nil_op >> 8), 10, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 1, 0, 0, 0,
        @truncate(jmp_op & 0xFF), @truncate(jmp_op >> 8), 6, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 2, 0, 0, 0,
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

    const vm_res = try vm.run(&chunk);
    try testing.expect(vm_res.isFixnum());
    try testing.expectEqual(@as(i64, 2), vm_res.toFixnum());

    const fn_ptr = try jit.compile(&chunk, &.{});
    try testing.expect(jit.code_buffer.relocs.items.len > 0);

    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&[_]Value{})),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = 0,
        .err_trace = &trace,
        .vm = &vm,
    };

    const jit_raw = fn_ptr(&ctx_val);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
    const jit_res = Value{ .raw = jit_raw };
    try testing.expect(jit_res.isFixnum());
    try testing.expectEqual(@as(i64, 2), jit_res.toFixnum());

    var moved = try patch.CodeBuffer.init(allocator, 1024 * 1024);
    defer moved.deinit();
    moved.pos = jit.code_buffer.pos;
    try moved.setWritable(true);
    @memcpy(moved.memory[0..moved.pos], jit.code_buffer.memory[0..moved.pos]);
    try moved.relocs.appendSlice(moved.alloc, jit.code_buffer.relocs.items);
    try moved.reapplyRelocs();
    try moved.setWritable(false);

    var ctx_val2 = ctx_val;
    ctx_val2.sp = stack_buf[0..].ptr;
    ctx_val2.err = 0;
    ret_buf.err = 0;
    ret_buf.value = Value.nil;

    const moved_fn = moved.getFnPtr(JitFn, jit.fn_start);
    const moved_raw = moved_fn(&ctx_val2);
    try testing.expectEqual(@as(u16, 0), ctx_val2.err);
    const moved_res = Value{ .raw = moved_raw };
    try testing.expect(moved_res.isFixnum());
    try testing.expectEqual(@as(i64, 2), moved_res.toFixnum());
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

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const numberp_op: u16 = @intFromEnum(Op.numberp);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        1, 0, 0, 0,
        @truncate(numberp_op & 0xFF), @truncate(numberp_op >> 8),
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

    _ = try jit.compile(&chunk, &.{});

    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.mov_x1_x0.code.len +
        stencils.mov_x0_x22.code.len +
        stencils.store_ctx_sp.code.len +
        stencils.clear_retbuf_err.code.len +
        stencils.call_abs.code.len +
        stencils.load_ctx_sp.code.len +
        stencils.runtime_check_c.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len +
        stencils.store_err.code.len +
        stencils.push_nil_stencil.code.len +
        stencils.epilogue_stencil.code.len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit compile add" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const add_op: u16 = @intFromEnum(Op.add);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        2, 0, 0, 0,
        @truncate(add_op & 0xFF), @truncate(add_op >> 8),
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

    _ = try jit.compile(&chunk, &.{});

    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len + stencils.stack_push.code.len +
        stencils.load_imm64.code.len + stencils.stack_push.code.len +
        stencils.stack_pop_x1.code.len +
        stencils.stack_pop.code.len +
        stencils.guard_fixnum_x0.code.len +
        stencils.guard_fixnum_x1.code.len +
        stencils.add_fixnum.code.len +
        stencils.fixnum_range_check.code.len +
        stencils.branch_stencil.code.len +
        stencils.mov_x2_x1.code.len +
        stencils.mov_x1_x0.code.len +
        stencils.mov_x0_x22.code.len +
        stencils.store_ctx_sp.code.len +
        stencils.clear_retbuf_err.code.len +
        stencils.call_abs.code.len +
        stencils.load_ctx_sp.code.len +
        stencils.runtime_check_c.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len +
        stencils.store_err.code.len +
        stencils.push_nil_stencil.code.len +
        stencils.epilogue_stencil.code.len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit compile locals" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const store_local_op: u16 = @intFromEnum(Op.store_local);
    const load_local_op: u16 = @intFromEnum(Op.load_local);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        7, 0, 0, 0,
        @truncate(store_local_op & 0xFF), @truncate(store_local_op >> 8),
        0,
        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8),
        0,
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

    _ = try jit.compile(&chunk, &.{});

    const err_len =
        stencils.store_err.code.len +
        stencils.push_nil_stencil.code.len +
        stencils.epilogue_stencil.code.len;
    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_imm64.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.store_local.code.len +
        stencils.load_local.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len +
        err_len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);
}

test "jit compile push_const" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8),
        0, 0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{Value.makeFixnum(7)};
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

    _ = try jit.compile(&chunk, &.{});

    const err_len =
        stencils.store_err.code.len +
        stencils.push_nil_stencil.code.len +
        stencils.epilogue_stencil.code.len;
    const expected_len =
        stencils.prologue_stencil.code.len +
        stencils.load_const.code.len +
        stencils.stack_push.code.len +
        stencils.stack_pop.code.len +
        stencils.epilogue_stencil.code.len +
        err_len;
    try testing.expectEqual(expected_len, jit.code_buffer.pos);

    const load_off = stencils.prologue_stencil.code.len;
    const inst = std.mem.readInt(u32, jit.code_buffer.memory[load_off .. load_off + 4], .little);
    const rn: u32 = (inst >> 5) & 0x1F;
    try testing.expectEqual(@as(u32, 20), rn);
}

test "jit vm parity add" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const add_op: u16 = @intFromEnum(Op.add);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        2, 0, 0, 0,
        @truncate(add_op & 0xFF), @truncate(add_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(@as(u16, 0), ctx_val.err);
    try testing.expectEqual(vm_res.raw, jit_res.raw);
}

test "jit vm parity numberp" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const numberp_op: u16 = @intFromEnum(Op.numberp);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        7, 0, 0, 0,
        @truncate(numberp_op & 0xFF), @truncate(numberp_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(vm_res.raw, jit_res.raw);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity lt" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const lt_op: u16 = @intFromEnum(Op.lt);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        2, 0, 0, 0,
        @truncate(lt_op & 0xFF), @truncate(lt_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(vm_res.raw, jit_res.raw);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity global store/load" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const store_global_op: u16 = @intFromEnum(Op.store_global);
    const load_global_op: u16 = @intFromEnum(Op.load_global);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const idx: u16 = 0;
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        42, 0, 0, 0,
        @truncate(store_global_op & 0xFF), @truncate(store_global_op >> 8),
        @truncate(idx & 0xFF), @truncate(idx >> 8),
        @truncate(load_global_op & 0xFF), @truncate(load_global_op >> 8),
        @truncate(idx & 0xFF), @truncate(idx >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(vm_res.raw, jit_res.raw);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit car nil returns nil" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_nil_op: u16 = @intFromEnum(Op.push_nil);
    const car_op: u16 = @intFromEnum(Op.car);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_nil_op & 0xFF), @truncate(push_nil_op >> 8),
        @truncate(car_op & 0xFF), @truncate(car_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(vm_res.raw, jit_res.raw);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit car non-cons returns nil" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const car_op: u16 = @intFromEnum(Op.car);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        1, 0, 0, 0,
        @truncate(car_op & 0xFF), @truncate(car_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);
    try testing.expect(vm_res.isNil());

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(vm_res.raw, jit_res.raw);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity make_list" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_list_op: u16 = @intFromEnum(Op.make_list);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        2, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        3, 0, 0, 0,
        @truncate(make_list_op & 0xFF), @truncate(make_list_op >> 8),
        3,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    const expected = [_]i64{ 1, 2, 3 };
    try expectListVals(vm_res, &expected);
    try expectListVals(jit_res, &expected);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity make_vec_n" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_vec_n_op: u16 = @intFromEnum(Op.make_vec_n);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        10, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        20, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        30, 0, 0, 0,
        @truncate(make_vec_n_op & 0xFF), @truncate(make_vec_n_op >> 8),
        3,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    const expected = [_]i64{ 10, 20, 30 };
    try expectVecVals(vm_res, &expected);
    try expectVecVals(jit_res, &expected);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity make_vec" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_vec_op: u16 = @intFromEnum(Op.make_vec);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        3, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        7, 0, 0, 0,
        @truncate(make_vec_op & 0xFF), @truncate(make_vec_op >> 8),
        0, 0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    const expected = [_]i64{ 7, 7, 7 };
    try expectVecVals(vm_res, &expected);
    try expectVecVals(jit_res, &expected);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity call closure" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const closure_code = [_]u8{
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8),
        42, 0, 0, 0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const closure_consts = [_]Value{};
    var closure_chunk = Chunk{
        .code = @constCast(&closure_code),
        .const_pool = @ptrCast(@constCast(&closure_consts)),
        .const_count = 0,
        .code_len = closure_code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };
    var chunk_pool = [_]*Chunk{ &closure_chunk };
    vm.chunk_pool = chunk_pool[0..];
    vm.chunk_base = 0;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const make_closure_op: u16 = @intFromEnum(Op.make_closure);
    const call_op: u16 = @intFromEnum(Op.call);
    const code = [_]u8{
        @truncate(make_closure_op & 0xFF), @truncate(make_closure_op >> 8),
        0, 0,
        0,
        @truncate(call_op & 0xFF), @truncate(call_op >> 8),
        0,
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    // Use the VM's own stack for the JitContext, since callFast
    // converts frame-relative indices to absolute VM stack positions.
    const vm_stack = vm.stack[0..];
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = vm_stack.ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = vm_stack.ptr,
        .stack_end = vm_stack[vm_stack.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expect(vm_res.isFixnum());
    try testing.expectEqual(@as(i64, 42), vm_res.toFixnum());
    try testing.expect(jit_res.isFixnum());
    try testing.expectEqual(@as(i64, 42), jit_res.toFixnum());
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit vm parity apply" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    const load_local_op: u16 = @intFromEnum(Op.load_local);
    const add_op: u16 = @intFromEnum(Op.add);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const closure_code = [_]u8{
        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8), 0,
        @truncate(load_local_op & 0xFF), @truncate(load_local_op >> 8), 1,
        @truncate(add_op & 0xFF), @truncate(add_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const closure_consts = [_]Value{};
    var closure_chunk = Chunk{
        .code = @constCast(&closure_code),
        .const_pool = @ptrCast(@constCast(&closure_consts)),
        .const_count = 0,
        .code_len = closure_code.len,
        .arity = 2,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 2,
    };
    var chunk_pool = [_]*Chunk{ &closure_chunk };
    vm.chunk_pool = chunk_pool[0..];
    vm.chunk_base = 0;

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const make_closure_op: u16 = @intFromEnum(Op.make_closure);
    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const make_list_op: u16 = @intFromEnum(Op.make_list);
    const apply_op: u16 = @intFromEnum(Op.apply);
    const code = [_]u8{
        @truncate(make_closure_op & 0xFF), @truncate(make_closure_op >> 8),
        0, 0,
        0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 1, 0, 0, 0,
        @truncate(push_i32_op & 0xFF), @truncate(push_i32_op >> 8), 2, 0, 0, 0,
        @truncate(make_list_op & 0xFF), @truncate(make_list_op >> 8), 2,
        @truncate(apply_op & 0xFF), @truncate(apply_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    const vm_stack2 = vm.stack[0..];
    var trace_addrs2: [16]usize = undefined;
    var trace2 = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs2[0..] };
    var ret_buf2 = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val2 = ctx.JitContext{
        .sp = vm_stack2.ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = vm_stack2.ptr,
        .stack_end = vm_stack2[vm_stack2.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf2,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace2,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val2);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expect(vm_res.isFixnum());
    try testing.expectEqual(@as(i64, 3), vm_res.toFixnum());
    try testing.expect(jit_res.isFixnum());
    try testing.expectEqual(@as(i64, 3), jit_res.toFixnum());
    try testing.expectEqual(@as(u16, 0), ctx_val2.err);
}

test "jit vm parity lt float" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const lt_op: u16 = @intFromEnum(Op.lt);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const code = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8),
        0, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8),
        1, 0,
        @truncate(lt_op & 0xFF), @truncate(lt_op >> 8),
        @truncate(ret_op & 0xFF), @truncate(ret_op >> 8),
    };
    const consts = [_]Value{ Value.makeFloat(1.5), Value.makeFloat(2.5) };
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

    const vm_res = try vm.run(&chunk);

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(vm_res.raw, jit_res.raw);
    try testing.expectEqual(@as(u16, 0), ctx_val.err);
}

test "jit gc roots preserve stack" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{ .total_size = 128 * 1024 });
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const obj = try heap.allocBaseString("keep");
    const big1 = try heap.allocBignum(1 << 62);
    const big2 = try heap.allocBignum(1 << 62);

    while (true) {
        const res = heap.allocCons(Value.nil, Value.nil);
        if (res) |_| {} else |err| switch (err) {
            error.OutOfMemory => break,
        }
    }

    const push_const_op: u16 = @intFromEnum(Op.push_const);
    const add_op: u16 = @intFromEnum(Op.add);
    const swap_op: u16 = @intFromEnum(Op.swap);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const consts = [_]Value{ obj, big1, big2 };
    const code = [_]u8{
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 0, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 1, 0,
        @truncate(push_const_op & 0xFF), @truncate(push_const_op >> 8), 2, 0,
        @truncate(add_op & 0xFF), @truncate(add_op >> 8),
        @truncate(swap_op & 0xFF), @truncate(swap_op >> 8),
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

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [32]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };
    const jit_raw = fn_ptr(&ctx_val);
    const jit_res = Value{ .raw = jit_raw };

    try testing.expectEqual(@as(u16, 0), ctx_val.err);
    try testing.expect(jit_res.isString());
    try testing.expectEqualStrings("keep", jit_res.toPtr(runtime.String).bytes());
}

test "jit stack overflow sets err" {
    if (builtin.cpu.arch != .aarch64) return;

    const testing = std.testing;
    const allocator = testing.allocator;

    var heap = try runtime.Heap.init(allocator, .{});
    defer heap.deinit();

    var vm = try vm_mod.Vm.init(allocator, &heap);
    defer vm.deinit();

    var jit = try Jit.init(allocator, 1024 * 1024);
    defer jit.deinit();

    const push_i32_op: u16 = @intFromEnum(Op.push_i32);
    const ret_op: u16 = @intFromEnum(Op.ret);
    const push_count: usize = 5;
    var code: [push_count * 6 + 2]u8 = undefined;
    var idx: usize = 0;
    var i: usize = 0;
    while (i < push_count) : (i += 1) {
        code[idx] = @truncate(push_i32_op & 0xFF);
        code[idx + 1] = @truncate(push_i32_op >> 8);
        code[idx + 2] = @truncate(@as(u32, @intCast(i)));
        code[idx + 3] = 0;
        code[idx + 4] = 0;
        code[idx + 5] = 0;
        idx += 6;
    }
    code[idx] = @truncate(ret_op & 0xFF);
    code[idx + 1] = @truncate(ret_op >> 8);

    const consts = [_]Value{};
    const chunk = Chunk{
        .code = @constCast(&code),
        .const_pool = @ptrCast(@constCast(&consts)),
        .const_count = 0,
        .code_len = code.len,
        .arity = 0,
        .opt_count = 0,
        .key_count = 0,
        .has_rest = 0,
        .num_locals = 0,
    };

    const fn_ptr = try jit.compile(&chunk, &.{});
    var stack_buf: [4]Value = undefined;
    var trace_addrs: [16]usize = undefined;
    var trace = std.builtin.StackTrace{ .index = 0, .instruction_addresses = trace_addrs[0..] };
    var ret_buf = ctx.RetBuf{ .value = Value.nil, .err = 0 };
    var ctx_val = ctx.JitContext{
        .sp = stack_buf[0..].ptr,
        .const_pool = @ptrCast(@constCast(&consts)),
        .frame_base = stack_buf[0..].ptr,
        .stack_end = stack_buf[stack_buf.len..].ptr,
        .heap = &heap,
        .ret_buf = &ret_buf,
        .err = 0,
        .const_count = consts.len,
        .err_trace = &trace,
        .vm = &vm,
    };

    const jit_raw = fn_ptr(&ctx_val);
    try testing.expectEqual(Value.nil.raw, jit_raw);
    try testing.expectEqual(@as(u16, @intFromError(error.StackOverflow)), ctx_val.err);
}
