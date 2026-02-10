//! Hoist SSA JIT Backend
//!
//! Translates Habu compiler IR to Hoist SSA IR, which then goes through:
//!   Optimize → Lower (ISLE) → Register Allocate → Emit AArch64
//!
//! This replaces the stencil-based stack-machine JIT with a proper
//! SSA compiler that keeps values in registers.
//!
//! Pipeline: Lisp → IR (tree) → Hoist SSA → Optimize → Lower → RegAlloc → Native
//!
//! Supports recursive functions via call_indirect with self-pointer patching.
//! The function address placeholder 0x0BADF00DDEADBEEF is embedded as an iconst
//! and patched with the actual code address after compilation.

const std = @import("std");
const hoist = @import("hoist");
const Function = hoist.function.Function;
const Signature = hoist.signature.Signature;
const AbiParam = hoist.signature.AbiParam;
const HoistType = hoist.types.Type;
const ContextBuilder = hoist.context.ContextBuilder;
const InstructionData = hoist.instruction_data.InstructionData;
const FunctionBuilder = hoist.builder.FunctionBuilder;
const IntCC = hoist.condcodes.IntCC;
const JitMem = hoist.jit.memory.Mem;
const Block = hoist.entities.Block;
const HoistValue = hoist.entities.Value;
const FuncRef = hoist.entities.FuncRef;
const ValueList = hoist.value_list.ValueList;
const SigRef = hoist.entities.SigRef;
const ExternalName = hoist.extfunc.ExternalName;

const habu_ir = @import("../compiler/ir.zig");
const Ir = habu_ir.Ir;
const habu_value = @import("../runtime/value.zig");
const Symbol = @import("../runtime/objects.zig").Symbol;
const Value = habu_value.Value;

const I64 = HoistType.I64;
const I8 = HoistType.I8;

/// Result of compiling a Habu function to native code.
pub const CompiledFn = struct {
    /// Executable memory containing the compiled code.
    mem: *JitMem,
    /// Entry point as a function pointer.
    fn_ptr: *const anyopaque,
    /// Number of user-visible parameters.
    arity: u32,
    /// Allocator used (for cleanup).
    allocator: std.mem.Allocator,

    pub fn deinit(self: *CompiledFn) void {
        self.mem.deinit();
        self.allocator.destroy(self.mem);
    }

    /// Call with args from VM stack (Values → tagged i64 → native → Value).
    pub fn callFromValues(self: *const CompiledFn, args: []const Value) Value {
        return switch (self.arity) {
            0 => blk: {
                const f: *const fn () callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f());
            },
            1 => blk: {
                const f: *const fn (i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0])));
            },
            2 => blk: {
                const f: *const fn (i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1])));
            },
            3 => blk: {
                const f: *const fn (i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1]), @bitCast(args[2])));
            },
            else => @bitCast(@as(i64, 0)), // TODO: support more args
        };
    }

    /// Call with 1 tagged i64 arg, returns tagged i64.
    /// Call with 0 args, returns tagged i64.
    pub fn call0(self: *const CompiledFn) i64 {
        const f: *const fn () callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
        return f();
    }

    pub fn call1(self: *const CompiledFn, arg: i64) i64 {
        const f: *const fn (i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
        return f(arg);
    }

    /// Call with 2 tagged i64 args, returns tagged i64.
    pub fn call2(self: *const CompiledFn, a: i64, b: i64) i64 {
        const f: *const fn (i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
        return f(a, b);
    }

    /// Call with 3 tagged i64 args, returns tagged i64.
    pub fn call3(self: *const CompiledFn, a: i64, b: i64, c: i64) i64 {
        const f: *const fn (i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
        return f(a, b, c);
    }
};

/// Translates Habu IR to Hoist SSA IR and compiles to native code.
pub const IrTranslator = struct {
    allocator: std.mem.Allocator,
    func: *Function,
    b: *FunctionBuilder,

    /// Maps local variable index → current SSA value.
    locals: std.ArrayList(HoistValue),

    /// Whether we're compiling a self-recursive function.
    is_recursive: bool,

    /// Whether the function contains loops (while).
    has_loops: bool,

    /// Name of the function being compiled (for self-call detection).
    fn_name: []const u8,

    /// Number of user-visible parameters.
    user_arity: u32,

    /// Placeholder i64 value for self-pointer (patched after compilation).
    self_ptr_placeholder: i64,

    /// SigRef for the self-call signature (for call_indirect).
    self_sig_ref: SigRef,

    /// Cache for iconst values — reuse across blocks (LICM for constants).
    const_cache: std.AutoHashMap(i64, HoistValue),

    /// True when the function has nested self-calls (e.g., tak pattern)
    /// that require post-emission parallel copy fixup for call arguments.
    needs_call_spill: bool = false,

    pub fn init(allocator: std.mem.Allocator, func: *Function, builder: *FunctionBuilder) IrTranslator {
        return .{
            .allocator = allocator,
            .func = func,
            .b = builder,
            .locals = std.ArrayList(HoistValue){},
            .is_recursive = false,
            .has_loops = false,
            .fn_name = "",
            .user_arity = 0,
            .self_ptr_placeholder = 0x0BADF00DDEADBEEF,
            .self_sig_ref = SigRef.new(0),
            .const_cache = std.AutoHashMap(i64, HoistValue).init(allocator),
        };
    }

    pub fn deinit(self: *IrTranslator) void {
        self.locals.deinit(self.allocator);
        self.const_cache.deinit();
    }

    /// Emit an iconst, reusing a previously emitted value for the same constant.
    /// This provides LICM for loop-invariant constants: a constant emitted in the
    /// entry block is reusable in all subsequent blocks (SSA dominance).
    fn cachedIconst(self: *IrTranslator, val: i64) !HoistValue {
        if (self.const_cache.get(val)) |cached| return cached;
        const result = try self.b.iconst(I64, val);
        try self.const_cache.put(val, result);
        return result;
    }

    /// Translate a Habu IR node to Hoist SSA, returning the SSA value produced.
    pub fn translate(self: *IrTranslator, ir: *const Ir) anyerror!HoistValue {
        return switch (ir.*) {
            .lit => |v| try self.translateLit(v),
            .@"var" => |v| self.translateVar(v),
            // Specialized fixnum ops (from type specialize pass)
            .fixnum_add => |op| try self.translateFixnumAdd(op.left, op.right),
            .fixnum_sub => |op| try self.translateFixnumSub(op.left, op.right),
            .fixnum_le => |op| try self.translateFixnumCmp(.sle, op.left, op.right),
            .fixnum_lt => |op| try self.translateFixnumCmp(.slt, op.left, op.right),
            .fixnum_gt => |op| try self.translateFixnumCmp(.sgt, op.left, op.right),
            .fixnum_ge => |op| try self.translateFixnumCmp(.sge, op.left, op.right),
            .fixnum_eq => |op| try self.translateFixnumCmp(.eq, op.left, op.right),
            // Generic arithmetic ops (same semantics, just not type-proven)
            .add => |op| try self.translateFixnumAdd(op.left, op.right),
            .sub => |op| try self.translateFixnumSub(op.left, op.right),
            .le => |op| try self.translateFixnumCmp(.sle, op.left, op.right),
            .lt => |op| try self.translateFixnumCmp(.slt, op.left, op.right),
            .gt => |op| try self.translateFixnumCmp(.sgt, op.left, op.right),
            .ge => |op| try self.translateFixnumCmp(.sge, op.left, op.right),
            .num_eq => |op| try self.translateFixnumCmp(.eq, op.left, op.right),
            .@"if" => |if_node| try self.translateIf(if_node.cond, if_node.then_branch, if_node.else_branch),
            .progn => |exprs| try self.translateProgn(exprs),
            .let => |let_node| try self.translateLet(let_node.bindings, let_node.body),
            .set => |set_node| try self.translateSet(set_node.index, set_node.value),
            .loop => |loop_node| try self.translateLoop(loop_node.cond, loop_node.body),
            .assert_fixnum => |op| try self.translate(op.operand), // At safety 0, just pass through
            .global_ref => |_| try self.translateLit(Value.nil), // TODO: general global refs
            .call => |call_node| try self.translateCall(call_node.func, call_node.args),
            .tailcall => |tc| try self.translateCall(tc.func, tc.args),
            else => {
                return error.UnsupportedIrNode;
            },
        };
    }

    fn translateLit(self: *IrTranslator, val: Value) anyerror!HoistValue {
        return try self.cachedIconst(@as(i64, @bitCast(val.raw)));
    }

    fn translateVar(self: *IrTranslator, v: anytype) HoistValue {
        if (v.depth == 0) {
            return self.locals.items[v.index];
        }
        unreachable; // TODO: closure captures
    }

    /// Extract a constant tagged fixnum value from an IR node, if it's a literal.
    fn getFixnumLit(ir: *const Ir) ?i64 {
        return switch (ir.*) {
            .lit => |v| if (v.isFixnum()) @bitCast(v.raw) else null,
            else => null,
        };
    }

    fn translateFixnumAdd(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        // Tagged fixnum add: result_raw = l_raw + r_raw - 1
        // When one operand is a constant, fold: iadd(x, const - 1)
        if (getFixnumLit(right)) |r_const| {
            const l = try self.translate(left);
            const folded = try self.cachedIconst(r_const - 1);
            return try self.b.iadd(I64, l, folded);
        }
        if (getFixnumLit(left)) |l_const| {
            const r = try self.translate(right);
            const folded = try self.cachedIconst(l_const - 1);
            return try self.b.iadd(I64, r, folded);
        }
        const l = try self.translate(left);
        const r = try self.translate(right);
        const sum = try self.b.iadd(I64, l, r);
        const one = try self.cachedIconst(1);
        return try self.b.isub(I64, sum, one);
    }

    fn translateFixnumSub(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        // Tagged fixnum sub: result_raw = l_raw - r_raw + 1
        // When right is a constant, fold: isub(x, const - 1)
        if (getFixnumLit(right)) |r_const| {
            const l = try self.translate(left);
            const folded = try self.cachedIconst(r_const - 1);
            return try self.b.isub(I64, l, folded);
        }
        const l = try self.translate(left);
        const r = try self.translate(right);
        const diff = try self.b.isub(I64, l, r);
        const one = try self.cachedIconst(1);
        return try self.b.iadd(I64, diff, one);
    }

    fn translateFixnumCmp(self: *IrTranslator, cc: IntCC, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        const l = try self.translate(left);
        const r = try self.translate(right);
        return try self.b.icmp(I8, cc, l, r);
    }

    fn translateIf(self: *IrTranslator, cond: *const Ir, then_ir: *const Ir, else_ir: *const Ir) anyerror!HoistValue {
        const cond_val = try self.translate(cond);

        const then_blk = try self.b.createBlock();
        const else_blk = try self.b.createBlock();
        const merge_blk = try self.b.createBlock();

        // Merge block has one param: the phi result (I64)
        const merge_param = try self.b.appendBlockParam(merge_blk, I64);

        try self.b.brif(cond_val, then_blk, else_blk);

        // Then branch
        self.b.switchToBlock(then_blk);
        try self.b.sealBlock(then_blk);
        const then_val = try self.translate(then_ir);
        // If the then-branch already terminated (nested if that returned),
        // don't emit another jump
        if (self.b.current_block != null) {
            const then_ty = self.func.dfg.valueType(then_val) orelse I64;
            const then_i64 = if (then_ty.raw == I8.raw)
                try self.b.uextend(I64, then_val)
            else
                then_val;
            try self.b.jumpArgs(merge_blk, &.{then_i64});
        }

        // Else branch
        self.b.switchToBlock(else_blk);
        try self.b.sealBlock(else_blk);
        const else_val = try self.translate(else_ir);
        if (self.b.current_block != null) {
            const else_ty = self.func.dfg.valueType(else_val) orelse I64;
            const else_i64 = if (else_ty.raw == I8.raw)
                try self.b.uextend(I64, else_val)
            else
                else_val;
            try self.b.jumpArgs(merge_blk, &.{else_i64});
        }

        // Continue in merge block
        self.b.switchToBlock(merge_blk);
        try self.b.sealBlock(merge_blk);
        return merge_param;
    }

    fn translateLet(self: *IrTranslator, bindings: []const Ir.Binding, body: *const Ir) anyerror!HoistValue {
        // Evaluate each binding and add to locals
        for (bindings) |binding| {
            const val = try self.translate(binding.value);
            // Extend locals array if needed
            while (self.locals.items.len <= binding.index) {
                try self.locals.append(self.allocator, HoistValue.new(0)); // placeholder
            }
            self.locals.items[binding.index] = val;
        }
        return try self.translate(body);
    }

    fn translateSet(self: *IrTranslator, index: u16, value_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(value_ir);
        if (index < self.locals.items.len) {
            self.locals.items[index] = val;
        }
        return val;
    }

    /// Pre-emit all literal constants found in an IR tree into the current block.
    /// This effectively performs LICM for constants when called before entering a loop.
    fn preEmitConstants(self: *IrTranslator, ir: *const Ir) !void {
        switch (ir.*) {
            .lit => |v| {
                _ = try self.cachedIconst(@as(i64, @bitCast(v.raw)));
            },
            .fixnum_add, .fixnum_sub, .add, .sub => |op| {
                // Also pre-emit the folded constant for fixnum ops with literal operands
                if (getFixnumLit(op.right)) |r_const| {
                    _ = try self.cachedIconst(r_const - 1);
                } else if (getFixnumLit(op.left)) |l_const| {
                    _ = try self.cachedIconst(l_const - 1);
                }
                try self.preEmitConstants(op.left);
                try self.preEmitConstants(op.right);
            },
            .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq => |op| {
                try self.preEmitConstants(op.left);
                try self.preEmitConstants(op.right);
            },
            .le, .lt, .gt, .ge, .num_eq => |op| {
                try self.preEmitConstants(op.left);
                try self.preEmitConstants(op.right);
            },
            .@"if" => |n| {
                try self.preEmitConstants(n.cond);
                try self.preEmitConstants(n.then_branch);
                try self.preEmitConstants(n.else_branch);
            },
            .progn => |exprs| {
                for (exprs) |expr| try self.preEmitConstants(expr);
            },
            .set => |n| try self.preEmitConstants(n.value),
            .assert_fixnum => |n| try self.preEmitConstants(n.operand),
            else => {},
        }
    }

    fn translateLoop(self: *IrTranslator, cond_ir: *const Ir, body_ir: *const Ir) anyerror!HoistValue {
        // Collect all variable indices mutated inside the loop body
        var mutated_indices = std.ArrayList(u16){};
        defer mutated_indices.deinit(self.allocator);
        try collectMutatedVars(body_ir, &mutated_indices, self.allocator);

        const n_phis = mutated_indices.items.len;
        if (n_phis > 16) return error.TooManyLoopVars;

        // LICM: Pre-emit all constants from the loop condition and body in the
        // current (pre-loop) block. These dominate the loop and will be kept in
        // registers by the allocator, avoiding re-materialization each iteration.
        try self.preEmitConstants(cond_ir);
        try self.preEmitConstants(body_ir);

        // Create blocks using low-level API
        const header = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(header);
        const loop_body = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(loop_body);
        const loop_exit = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(loop_exit);

        // Add block params for header (phi nodes for mutated variables)
        // Save values immediately — they're stable SSA value indices.
        var phi_vals: [16]HoistValue = undefined;
        for (0..n_phis) |pi| {
            phi_vals[pi] = try self.func.dfg.appendBlockParam(header, I64);
        }

        // Jump from current block to header with initial values
        var init_vals: [16]HoistValue = undefined;
        for (mutated_indices.items, 0..) |idx, i| {
            init_vals[i] = if (idx < self.locals.items.len)
                self.locals.items[idx]
            else
                try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        }
        try self.b.jumpArgs(header, init_vals[0..n_phis]);

        // Header block: install phi values and evaluate condition
        self.b.switchToBlock(header);
        for (mutated_indices.items, 0..) |idx, i| {
            while (self.locals.items.len <= idx) {
                try self.locals.append(self.allocator, HoistValue.new(0));
            }
            self.locals.items[idx] = phi_vals[i];
        }

        const cond_val = try self.translate(cond_ir);
        try self.b.brif(cond_val, loop_body, loop_exit);

        // Body: execute body, then jump back to header with updated values
        self.b.switchToBlock(loop_body);
        _ = try self.translate(body_ir);

        var updated_vals: [16]HoistValue = undefined;
        for (mutated_indices.items, 0..) |idx, i| {
            updated_vals[i] = if (idx < self.locals.items.len)
                self.locals.items[idx]
            else
                try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        }
        try self.b.jumpArgs(header, updated_vals[0..n_phis]);

        // Exit block
        self.b.switchToBlock(loop_exit);

        // After loop, locals point to phi values (correct on exit)
        for (mutated_indices.items, 0..) |idx, i| {
            self.locals.items[idx] = phi_vals[i];
        }

        // Return nil (while loop doesn't produce a value in CL)
        return try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
    }

    fn translateCall(self: *IrTranslator, func_ir: *const Ir, args: []const *const Ir) anyerror!HoistValue {
        // Check for self-recursive call
        if (self.is_recursive and isCallTargetSelf(func_ir, self.fn_name)) {
            return try self.translateSelfCall(args);
        }

        // TODO: general function calls (via runtime)
        return try self.b.iconst(I64, 0); // placeholder nil
    }

    fn translateSelfCall(self: *IrTranslator, args: []const *const Ir) anyerror!HoistValue {
        // Translate user args first (while parameter registers are still valid)
        var translated_args: [16]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }

        // Emit self_ptr iconst (after arg evaluation to avoid register interference)
        const self_ptr = try self.b.iconst(I64, self.self_ptr_placeholder);

        // Build argument list: [target_ptr, arg0, arg1, ...]
        // target_ptr is consumed by call_indirect, actual args passed to callee
        var call_args = ValueList.default();
        try self.func.dfg.value_lists.push(&call_args, self_ptr);
        for (0..args.len) |i| {
            try self.func.dfg.value_lists.push(&call_args, translated_args[i]);
        }

        // Emit call_indirect instruction
        const call_data = InstructionData{
            .call_indirect = .{
                .opcode = .call_indirect,
                .sig_ref = self.self_sig_ref,
                .args = call_args,
            },
        };
        const call_inst = try self.func.dfg.makeInst(call_data);
        const call_result = try self.func.dfg.appendInstResult(call_inst, I64);
        const block = self.b.current_block orelse return error.NoCurrentBlock;
        try self.func.layout.appendInst(call_inst, block);

        return call_result;
    }



    fn translateProgn(self: *IrTranslator, exprs: []const *const Ir) anyerror!HoistValue {
        var result: HoistValue = undefined;
        for (exprs) |expr| {
            result = try self.translate(expr);
        }
        return result;
    }
};

/// Patch all occurrences of a 64-bit placeholder value in the code buffer.
/// On AArch64, a 64-bit constant is loaded via MOVZ+MOVK+MOVK+MOVK sequence.
fn patchPlaceholder(buf: []u8, placeholder: u64, target: u64) bool {
    var found = false;
    const ph_0 = @as(u16, @truncate(placeholder));
    const ph_1 = @as(u16, @truncate(placeholder >> 16));
    const ph_2 = @as(u16, @truncate(placeholder >> 32));
    const ph_3 = @as(u16, @truncate(placeholder >> 48));

    const tg_0 = @as(u16, @truncate(target));
    const tg_1 = @as(u16, @truncate(target >> 16));
    const tg_2 = @as(u16, @truncate(target >> 32));
    const tg_3 = @as(u16, @truncate(target >> 48));

    var i: usize = 0;
    while (i + 16 <= buf.len) : (i += 4) {
        const inst0 = std.mem.readInt(u32, buf[i..][0..4], .little);
        if ((inst0 & 0xFFE00000) == 0xD2800000) {
            const imm16_0 = @as(u16, @truncate((inst0 >> 5) & 0xFFFF));
            if (imm16_0 == ph_0 and i + 16 <= buf.len) {
                const inst1 = std.mem.readInt(u32, buf[i + 4 ..][0..4], .little);
                const inst2 = std.mem.readInt(u32, buf[i + 8 ..][0..4], .little);
                const inst3 = std.mem.readInt(u32, buf[i + 12 ..][0..4], .little);

                const imm16_1 = @as(u16, @truncate((inst1 >> 5) & 0xFFFF));
                const imm16_2 = @as(u16, @truncate((inst2 >> 5) & 0xFFFF));
                const imm16_3 = @as(u16, @truncate((inst3 >> 5) & 0xFFFF));

                if ((inst1 & 0xFFE00000) == 0xF2A00000 and imm16_1 == ph_1 and
                    (inst2 & 0xFFE00000) == 0xF2C00000 and imm16_2 == ph_2 and
                    (inst3 & 0xFFE00000) == 0xF2E00000 and imm16_3 == ph_3)
                {
                    const rd = inst0 & 0x1F;
                    std.mem.writeInt(u32, buf[i..][0..4], 0xD2800000 | (@as(u32, tg_0) << 5) | rd, .little);
                    std.mem.writeInt(u32, buf[i + 4 ..][0..4], 0xF2A00000 | (@as(u32, tg_1) << 5) | rd, .little);
                    std.mem.writeInt(u32, buf[i + 8 ..][0..4], 0xF2C00000 | (@as(u32, tg_2) << 5) | rd, .little);
                    std.mem.writeInt(u32, buf[i + 12 ..][0..4], 0xF2E00000 | (@as(u32, tg_3) << 5) | rd, .little);
                    found = true;
                    i += 16;
                    continue;
                }
            }
        }
    }
    return found;
}

/// Recursively collect all variable indices that are assigned (set) within an IR subtree.
fn collectMutatedVars(ir: *const Ir, indices: *std.ArrayList(u16), allocator: std.mem.Allocator) !void {
    switch (ir.*) {
        .set => |s| {
            // Add index if not already present
            for (indices.items) |existing| {
                if (existing == s.index) return;
            }
            try indices.append(allocator, s.index);
            try collectMutatedVars(s.value, indices, allocator);
        },
        .progn => |exprs| {
            for (exprs) |expr| {
                try collectMutatedVars(expr, indices, allocator);
            }
        },
        .@"if" => |f| {
            try collectMutatedVars(f.cond, indices, allocator);
            try collectMutatedVars(f.then_branch, indices, allocator);
            try collectMutatedVars(f.else_branch, indices, allocator);
        },
        .let => |l| {
            for (l.bindings) |binding| {
                try collectMutatedVars(binding.value, indices, allocator);
            }
            try collectMutatedVars(l.body, indices, allocator);
        },
        .loop => |l| {
            try collectMutatedVars(l.cond, indices, allocator);
            try collectMutatedVars(l.body, indices, allocator);
        },
        .fixnum_add, .fixnum_sub, .add, .sub => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
        .le, .lt, .gt, .ge, .num_eq => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
        .assert_fixnum => |op| try collectMutatedVars(op.operand, indices, allocator),
        .call => |c| {
            for (c.args) |arg| try collectMutatedVars(arg, indices, allocator);
        },
        else => {},
    }
}

/// Check if a call target matches the current function name.
/// Handles both global_ref (unit tests) and lit-symbol (REPL compiler).
/// For lit-symbol, the symbol name is unqualified ("MYCD") while the
/// function name is qualified ("CL-USER:MYCD"), so we check if the
/// qualified name ends with ":" + symbol_name.
fn isCallTargetSelf(func_ir: *const Ir, name: []const u8) bool {
    return switch (func_ir.*) {
        .global_ref => |gr| std.mem.eql(u8, gr.name, name),
        .lit => |v| blk: {
            if (!v.isSymbol()) break :blk false;
            if (v.isNil()) break :blk false;
            const sym_name = v.toPtr(Symbol).getName();
            // Exact match
            if (std.mem.eql(u8, sym_name, name)) break :blk true;
            // Qualified match: name = "PKG:SYM" and sym_name = "SYM"
            if (name.len > sym_name.len + 1) {
                const suffix_start = name.len - sym_name.len;
                if (name[suffix_start - 1] == ':' and
                    std.mem.eql(u8, name[suffix_start..], sym_name))
                    break :blk true;
            }
            break :blk false;
        },
        else => false,
    };
}

/// Detect if a self-call appears as an argument to another self-call.
/// This pattern (e.g., tak) causes segfaults due to hoist regalloc bug
/// with call_indirect spilling. Returns true if the pattern is found.
fn hasNestedSelfCalls(body: *const Ir, name: []const u8) bool {
    return switch (body.*) {
        .call => |c| blk: {
            if (isCallTargetSelf(c.func, name)) {
                for (c.args) |arg| {
                    if (detectSelfCalls(arg, name)) break :blk true;
                }
            }
            for (c.args) |arg| {
                if (hasNestedSelfCalls(arg, name)) break :blk true;
            }
            break :blk false;
        },
        .tailcall => |tc| blk: {
            if (isCallTargetSelf(tc.func, name)) {
                for (tc.args) |arg| {
                    if (detectSelfCalls(arg, name)) break :blk true;
                }
            }
            for (tc.args) |arg| {
                if (hasNestedSelfCalls(arg, name)) break :blk true;
            }
            break :blk false;
        },
        .@"if" => |if_node| hasNestedSelfCalls(if_node.cond, name) or
            hasNestedSelfCalls(if_node.then_branch, name) or
            hasNestedSelfCalls(if_node.else_branch, name),
        .fixnum_add, .fixnum_sub, .add, .sub => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .le, .lt, .gt, .ge, .num_eq => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .progn => |exprs| {
            for (exprs) |expr| {
                if (hasNestedSelfCalls(expr, name)) return true;
            }
            return false;
        },
        .assert_fixnum => |op| hasNestedSelfCalls(op.operand, name),
        else => false,
    };
}

/// Detect whether a function body contains self-recursive calls.
fn detectSelfCalls(body: *const Ir, name: []const u8) bool {
    return switch (body.*) {
        .call => |c| blk: {
            if (isCallTargetSelf(c.func, name)) break :blk true;
            for (c.args) |arg| {
                if (detectSelfCalls(arg, name)) break :blk true;
            }
            break :blk detectSelfCalls(c.func, name);
        },
        .tailcall => |tc| blk: {
            if (isCallTargetSelf(tc.func, name)) break :blk true;
            for (tc.args) |arg| {
                if (detectSelfCalls(arg, name)) break :blk true;
            }
            break :blk detectSelfCalls(tc.func, name);
        },
        .@"if" => |if_node| detectSelfCalls(if_node.cond, name) or
            detectSelfCalls(if_node.then_branch, name) or
            detectSelfCalls(if_node.else_branch, name),
        .fixnum_add, .fixnum_sub, .add, .sub => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .le, .lt, .gt, .ge, .num_eq => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .progn => |exprs| {
            for (exprs) |expr| {
                if (detectSelfCalls(expr, name)) return true;
            }
            return false;
        },
        .assert_fixnum => |op| detectSelfCalls(op.operand, name),
        else => false,
    };
}

/// Detect whether a function body contains loop constructs.
fn detectLoops(body: *const Ir) bool {
    return switch (body.*) {
        .loop => true,
        .@"if" => |n| detectLoops(n.cond) or detectLoops(n.then_branch) or detectLoops(n.else_branch),
        .progn => |exprs| {
            for (exprs) |expr| {
                if (detectLoops(expr)) return true;
            }
            return false;
        },
        .let => |n| detectLoops(n.body),
        .fixnum_add, .fixnum_sub, .add, .sub => |n| detectLoops(n.left) or detectLoops(n.right),
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq => |n| detectLoops(n.left) or detectLoops(n.right),
        .le, .lt, .gt, .ge, .num_eq => |n| detectLoops(n.left) or detectLoops(n.right),
        .call => |c| {
            for (c.args) |arg| {
                if (detectLoops(arg)) return true;
            }
            return false;
        },
        .tailcall => |tc| {
            for (tc.args) |arg| {
                if (detectLoops(arg)) return true;
            }
            return false;
        },
        .assert_fixnum => |n| detectLoops(n.operand),
        .set => |n| detectLoops(n.value),
        else => false,
    };
}

/// Compile a Habu IR lambda to native code via Hoist SSA.
/// Returns error.UnsupportedRecursiveCall for functions with recursive calls
/// (due to hoist regalloc limitation — use stencil JIT as fallback).
pub fn compileIr(
    allocator: std.mem.Allocator,
    ir: *const Ir,
    name: []const u8,
) !CompiledFn {
    const lambda = switch (ir.*) {
        .lambda => |l| l,
        else => return error.ExpectedLambda,
    };

    const arity: u32 = @intCast(lambda.params.len);

    // Build signature: all params are i64 (tagged values), return i64
    var sig = Signature.init(allocator, .system_v);
    var sig_owned = true;
    defer if (sig_owned) sig.deinit();
    for (0..arity) |_| {
        try sig.params.append(allocator, AbiParam.new(I64));
    }
    try sig.returns.append(allocator, AbiParam.new(I64));

    var func = try Function.init(allocator, name, sig);
    sig_owned = false; // Ownership transferred to func
    defer func.deinit();

    var b = try FunctionBuilder.init(allocator, &func);
    defer b.deinit();

    // Create entry block with params
    const entry = try b.createBlock();
    {
        var param_types: [16]HoistType = undefined;
        for (0..arity) |i| {
            param_types[i] = I64;
        }
        try func.dfg.setBlockParams(entry, param_types[0..arity]);
    }
    b.switchToBlock(entry);
    try b.sealBlock(entry);

    // Set up translator
    var translator = IrTranslator.init(allocator, &func, &b);
    defer translator.deinit();

    translator.fn_name = name;
    translator.user_arity = arity;
    translator.is_recursive = detectSelfCalls(lambda.body, name);
    translator.has_loops = detectLoops(lambda.body);

    // Enable call result spilling for nested self-calls (e.g., tak pattern)
    // to break parallel copy conflicts in the regalloc.
    if (translator.is_recursive and hasNestedSelfCalls(lambda.body, name)) {
        translator.needs_call_spill = true;
    }

    // For recursive functions, register the callee signature for call_indirect
    if (translator.is_recursive) {
        var indirect_sig = Signature.init(allocator, .system_v);
        for (0..arity) |_| {
            try indirect_sig.params.append(allocator, AbiParam.new(I64));
        }
        try indirect_sig.returns.append(allocator, AbiParam.new(I64));
        translator.self_sig_ref = try func.addSignature(indirect_sig);
    }

    // Map params to SSA values
    const block_params = func.dfg.blockParams(entry);
    try translator.locals.ensureTotalCapacity(allocator, arity);
    for (0..arity) |i| {
        try translator.locals.append(allocator, block_params[i]);
    }

    // Pre-emit all constants in the entry block so they dominate all uses.
    // Without this, cachedIconst can return values from wrong blocks.
    try translator.preEmitConstants(lambda.body);

    // Translate body
    const result = translator.translate(lambda.body) catch |err| {
        return err;
    };

    // Emit ret with the result value
    const result_ty = func.dfg.valueType(result) orelse I64;
    const result_i64 = if (result_ty.raw == I8.raw)
        try b.uextend(I64, result)
    else
        result;
    try b.retValues(&.{result_i64});

    // Compile with Hoist
    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    var ctx = ctx_builder
        .optLevel(if (translator.is_recursive or translator.has_loops) .none else .aggressive)
        .callConv(.system_v)
        .verification(true)
        .build();

    // Print function for debug
    if (std.posix.getenv("HABU_DUMP_HOIST") != null) {
        var pp_buf: [8192]u8 = undefined;
        var pp_fbs = std.io.fixedBufferStream(&pp_buf);
        hoist.ir_print.writeFunction(pp_fbs.writer(), &func, .{}) catch {};
        std.debug.print("[hoist-ir]\n{s}\n", .{pp_buf[0..pp_fbs.pos]});
    }

    var code = ctx.compileFunction(&func) catch |err| {
        return err;
    };
    defer code.deinit();

    // Debug: dump machine code
    if (std.posix.getenv("HABU_DUMP_HOIST") != null) {
        std.debug.print("[hoist-asm] {d} bytes:", .{code.code.items.len});
        for (code.code.items, 0..) |byte, i| {
            if (i % 4 == 0) std.debug.print(" ", .{});
            if (i % 16 == 0) std.debug.print("\n  {x:0>4}: ", .{i});
            std.debug.print("{x:0>2}", .{byte});
        }
        std.debug.print("\n", .{});
    }

    // Allocate executable memory
    var mem = try allocator.create(JitMem);
    mem.* = try JitMem.init(allocator, code.code.items.len);
    errdefer {
        mem.deinit();
        allocator.destroy(mem);
    }

    const buf = try mem.alloc(code.code.items.len, 16);

    // Patch self-pointer placeholder BEFORE writeExec so I-cache flush covers it.
    // On AArch64, D-cache writes are not visible to I-cache without explicit flush.
    if (translator.is_recursive) {
        const func_addr = @intFromPtr(buf.ptr);
        const placeholder: u64 = @bitCast(translator.self_ptr_placeholder);
        if (!patchPlaceholder(code.code.items, placeholder, func_addr)) {
            return error.SelfPointerPatchFailed;
        }
    }

    // Peephole: replace dead cset with NOP in fused cmp+cset+b.cc sequences.
    // The icmp emits cmp+cset, and fused brif emits b.cc using flags directly.
    // The cset result is dead but still executes.
    if (std.posix.getenv("HABU_NO_CSET_ELIM") == null) {
        eliminateDeadCset(code.code.items);
    }

    // Fix parallel copy conflicts in call argument setup.
    // Hoist's lowering emits sequential mov instructions for call arguments
    // which can clobber source registers before they're consumed.
    if (translator.needs_call_spill) {
        fixCallArgMoves(code.code.items);
        // Debug: dump patched machine code
        if (false) {
            std.debug.print("[hoist-asm-patched] {d} bytes: ", .{code.code.items.len});
            for (code.code.items, 0..) |byte, ii| {
                if (ii % 4 == 0) std.debug.print(" ", .{});
                if (ii % 16 == 0) std.debug.print("\n  {x:0>4}: ", .{ii});
                std.debug.print("{x:0>2}", .{byte});
            }
            std.debug.print("\n", .{});
        }
    }

    // Debug: dump final machine code before making executable
    if (std.posix.getenv("HABU_DUMP_HOIST") != null) {
        std.debug.print("[hoist-asm-final] {d} bytes:\n", .{code.code.items.len});
        var dbg_i: usize = 0;
        while (dbg_i + 4 <= code.code.items.len) : (dbg_i += 4) {
            const w = @as(u32, code.code.items[dbg_i]) |
                (@as(u32, code.code.items[dbg_i + 1]) << 8) |
                (@as(u32, code.code.items[dbg_i + 2]) << 16) |
                (@as(u32, code.code.items[dbg_i + 3]) << 24);
            std.debug.print("  {x:0>4}: {x:0>8}\n", .{ dbg_i, w });
        }
    }

    try mem.writeExec(buf, code.code.items);

    // Debug: verify self-pointer in executable buffer
    if (std.posix.getenv("HABU_DUMP_HOIST") != null) {
        std.debug.print("[hoist-exec] fn_ptr=0x{x}\n", .{@intFromPtr(buf.ptr)});
        // Read self-ptr from offset 0x38 in the code
        if (translator.is_recursive and code.code.items.len >= 0x48) {
            const w0 = std.mem.readInt(u32, code.code.items[0x38..0x3c], .little);
            const w1 = std.mem.readInt(u32, code.code.items[0x3c..0x40], .little);
            const w2 = std.mem.readInt(u32, code.code.items[0x40..0x44], .little);
            const w3 = std.mem.readInt(u32, code.code.items[0x44..0x48], .little);
            const imm0 = @as(u64, (w0 >> 5) & 0xFFFF);
            const imm1 = @as(u64, (w1 >> 5) & 0xFFFF) << 16;
            const imm2 = @as(u64, (w2 >> 5) & 0xFFFF) << 32;
            const imm3 = @as(u64, (w3 >> 5) & 0xFFFF) << 48;
            std.debug.print("[hoist-exec] self-ptr decoded: 0x{x}\n", .{imm0 | imm1 | imm2 | imm3});
        }
    }

    try mem.setExec(true);

    return .{
        .mem = mem,
        .fn_ptr = @ptrCast(buf.ptr),
        .arity = arity,
        .allocator = allocator,
    };
}


/// Replace dead CSET instructions with NOP when followed by a B.cond.
/// Pattern: CMP; CSET; B.cond → CMP; NOP; B.cond
/// The CSET result is dead because B.cond reads flags directly from CMP.
fn eliminateDeadCset(code: []u8) void {
    const n_insns = code.len / 4;
    if (n_insns < 3) return;

    var i: usize = 0;
    while (i + 2 < n_insns) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);
        const insn2 = readInsn(code, i + 2);

        // Check pattern: CMP Xn, Xm (subs xzr); CSET Wd, cc; B.cond
        const is_cmp = (insn0 & 0xFFE0FC1F) == 0xEB00001F; // CMP (shifted register)
        const is_cset = (insn1 & 0xFFE00C00) == 0x1A800000; // CSET/CSINC
        const is_bcond = (insn2 & 0xFF000010) == 0x54000000; // B.cond

        if (is_cmp and is_cset and is_bcond) {
            // Replace CSET with NOP (0xD503201F)
            writeInsn(code, i + 1, 0xD503201F);
        }
    }
}

/// Fix parallel copy conflicts in AArch64 call argument setup.
///
/// Scans for `blr` instructions and checks the preceding `mov` instructions
/// for conflicts where a source register is overwritten before it's consumed.
/// Resolves conflicts by reordering the mov instructions.
///
/// Example conflict:
///   mov x0, x23    ; overwrites x0
///   mov x1, x24
///   mov x2, x0     ; reads x0, but x0 was already overwritten!
///   blr x9
///
/// Fixed:
///   mov x2, x0     ; read x0 first (before it's overwritten)
///   mov x0, x23
///   mov x1, x24
///   blr x9
fn fixCallArgMoves(code: []u8) void {
    if (code.len < 8) return;
    const n_insns = code.len / 4;

    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);

        // Check for BLR instruction: 1101 0110 0011 1111 0000 00xx xxx0 0000
        if (insn & 0xFFFFFC1F != 0xD63F0000) continue;

        // Found a BLR. Scan backwards for mov instructions (up to 8).
        const MovInfo = struct { src: u5, dst: u5, pos: usize };
        var movs: [8]MovInfo = undefined;
        var n_movs: usize = 0;

        var j = i;
        while (j > 0 and n_movs < 8) {
            j -= 1;
            const prev = readInsn(code, j);
            // Check for MOV Xd, Xm (ORR Xd, XZR, Xm): 0xAA0003E0 mask 0xFFE0FFE0
            if (prev & 0xFFE0FFE0 == 0xAA0003E0) {
                const rd: u5 = @truncate(prev & 0x1F);
                const rm: u5 = @truncate((prev >> 16) & 0x1F);
                // Only include moves to x0-x7 (ABI argument registers)
                // in the parallel copy resolution.
                if (rd <= 7) {
                    movs[n_movs] = .{ .src = rm, .dst = rd, .pos = j };
                    n_movs += 1;
                } else {
                    break; // Non-argument move, stop scanning
                }
            } else {
                break; // Stop at non-mov instruction
            }
        }

        if (n_movs < 2) continue;

        // Check for conflicts: a mov reads from a register that's been
        // overwritten by an earlier (lower index) mov in the sequence.
        // Note: movs[] is in reverse order (movs[0] is closest to blr).
        // The execution order is movs[n-1], movs[n-2], ..., movs[0], blr.
        var has_conflict = false;
        for (0..n_movs) |a| {
            for (a + 1..n_movs) |b| {
                // movs[b] executes BEFORE movs[a] (farther from blr = earlier)
                // Check if movs[b] writes a register that movs[a] reads
                if (movs[b].dst == movs[a].src) {
                    has_conflict = true;
                    break;
                }
            }
            if (has_conflict) break;
        }

        if (!has_conflict) continue;

        // Reorder using topological sort on the dependency graph.
        // Edge: move A depends on move B if B's destination = A's source
        // (A must execute before B to read the value B overwrites).
        // A move is "ready" when its destination is NOT the source of any
        // remaining (un-emitted) move.
        var new_order: [8]MovInfo = undefined;
        var emitted: [8]bool = .{ false, false, false, false, false, false, false, false };
        var n_emitted: usize = 0;

        while (n_emitted < n_movs) {
            var found = false;
            for (0..n_movs) |a| {
                if (emitted[a]) continue;
                // Check if this move's DESTINATION is needed as SOURCE by any remaining move.
                // If no remaining move reads from our destination, we can emit safely.
                var dst_needed = false;
                for (0..n_movs) |b| {
                    if (a == b or emitted[b]) continue;
                    if (movs[b].src == movs[a].dst) {
                        dst_needed = true;
                        break;
                    }
                }
                if (!dst_needed) {
                    new_order[n_emitted] = movs[a];
                    emitted[a] = true;
                    n_emitted += 1;
                    found = true;
                    break; // restart scan
                }
            }
            if (!found) {
                // Cycle detected - emit remaining in original order
                for (0..n_movs) |a| {
                    if (!emitted[a]) {
                        new_order[n_emitted] = movs[a];
                        emitted[a] = true;
                        n_emitted += 1;
                    }
                }
            }
        }

        // Write reordered instructions back.
        // The positions in the code buffer are: movs[n-1].pos, movs[n-2].pos, ..., movs[0].pos
        // We need to write new_order[0..n_movs] into these positions (in execution order).
        // Execution order: position = movs[n_movs - 1 - k].pos for k-th emitted move.
        for (0..n_emitted) |k| {
            const pos = movs[n_movs - 1 - k].pos;
            const new_insn: u32 = 0xAA0003E0 |
                @as(u32, new_order[k].dst) |
                (@as(u32, new_order[k].src) << 16);
            writeInsn(code, pos, new_insn);
        }
    }
}

fn readInsn(code: []const u8, idx: usize) u32 {
    const off = idx * 4;
    return std.mem.readInt(u32, code[off..][0..4], .little);
}

fn writeInsn(code: []u8, idx: usize, val: u32) void {
    const off = idx * 4;
    std.mem.writeInt(u32, code[off..][0..4], val, .little);
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

/// Helper: build Hoist function, compile, load into JIT memory
fn compileAndLoad(allocator: std.mem.Allocator, func: *Function) !struct { fn_ptr: *const fn (i64) callconv(.c) i64, mem: *JitMem } {
    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    var ctx = ctx_builder.optLevel(.aggressive).callConv(.system_v).verification(true).build();

    var code = ctx.compileFunction(func) catch |err| {
        return err;
    };
    defer code.deinit();

    var mem = try allocator.create(JitMem);
    mem.* = try JitMem.init(allocator, code.code.items.len);

    const buf = try mem.alloc(code.code.items.len, 16);
    try mem.writeExec(buf, code.code.items);
    try mem.setExec(true);

    return .{
        .fn_ptr = mem.getFn(*const fn (i64) callconv(.c) i64),
        .mem = mem,
    };
}

test "hoist identity" {
    var sig = Signature.init(testing.allocator, .system_v);
    try sig.params.append(testing.allocator, AbiParam.new(I64));
    try sig.returns.append(testing.allocator, AbiParam.new(I64));

    var func = try Function.init(testing.allocator, "identity", sig);
    defer func.deinit();

    var b = try FunctionBuilder.init(testing.allocator, &func);
    defer b.deinit();

    const entry = try b.createBlock();
    try func.dfg.setBlockParams(entry, &.{I64});
    b.switchToBlock(entry);
    try b.sealBlock(entry);
    try b.retValues(&.{func.dfg.blockParams(entry)[0]});

    const r = try compileAndLoad(testing.allocator, &func);
    defer {
        r.mem.deinit();
        testing.allocator.destroy(r.mem);
    }
    try testing.expectEqual(@as(i64, 42), r.fn_ptr(42));
    try testing.expectEqual(@as(i64, 0), r.fn_ptr(0));
    try testing.expectEqual(@as(i64, -1), r.fn_ptr(-1));
}

test "hoist arithmetic (n*2+1)" {
    var sig = Signature.init(testing.allocator, .system_v);
    try sig.params.append(testing.allocator, AbiParam.new(I64));
    try sig.returns.append(testing.allocator, AbiParam.new(I64));

    var func = try Function.init(testing.allocator, "double1", sig);
    defer func.deinit();

    var b = try FunctionBuilder.init(testing.allocator, &func);
    defer b.deinit();

    const entry = try b.createBlock();
    try func.dfg.setBlockParams(entry, &.{I64});
    b.switchToBlock(entry);
    try b.sealBlock(entry);

    const n_raw = func.dfg.blockParams(entry)[0];
    const one = try b.iconst(I64, 1);
    const n = try b.sshr(I64, n_raw, one);
    const two = try b.iconst(I64, 2);
    const doubled = try b.imul(I64, n, two);
    const result = try b.iadd(I64, doubled, one);
    const shifted = try b.ishl(I64, result, one);
    const tagged = try b.bor(I64, shifted, one);
    try b.retValues(&.{tagged});

    const r = try compileAndLoad(testing.allocator, &func);
    defer {
        r.mem.deinit();
        testing.allocator.destroy(r.mem);
    }
    // f(5)=11: tagged (5<<1)|1=11 → (11<<1)|1=23
    try testing.expectEqual(@as(i64, 23), r.fn_ptr(11));
    try testing.expectEqual(@as(i64, 3), r.fn_ptr(1)); // f(0)=1
}

test "hoist branch" {
    var sig = Signature.init(testing.allocator, .system_v);
    try sig.params.append(testing.allocator, AbiParam.new(I64));
    try sig.returns.append(testing.allocator, AbiParam.new(I64));

    var func = try Function.init(testing.allocator, "branch", sig);
    defer func.deinit();

    var b = try FunctionBuilder.init(testing.allocator, &func);
    defer b.deinit();

    const entry = try b.createBlock();
    const t_blk = try b.createBlock();
    const f_blk = try b.createBlock();

    try func.dfg.setBlockParams(entry, &.{I64});
    b.switchToBlock(entry);
    try b.sealBlock(entry);

    const n = func.dfg.blockParams(entry)[0];
    const zero = try b.iconst(I64, 0);
    const cmp = try b.icmp(I8, .sgt, n, zero);
    try b.brif(cmp, t_blk, f_blk);

    b.switchToBlock(t_blk);
    try b.sealBlock(t_blk);
    try b.retValues(&.{try b.iconst(I64, 100)});

    b.switchToBlock(f_blk);
    try b.sealBlock(f_blk);
    try b.retValues(&.{try b.iconst(I64, 200)});

    const r = try compileAndLoad(testing.allocator, &func);
    defer {
        r.mem.deinit();
        testing.allocator.destroy(r.mem);
    }
    try testing.expectEqual(@as(i64, 100), r.fn_ptr(5));
    try testing.expectEqual(@as(i64, 200), r.fn_ptr(-1));
    try testing.expectEqual(@as(i64, 200), r.fn_ptr(0));
}

test "hoist IR translator: non-recursive if" {
    // (lambda (n) (if (<= n 1) n 42))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const var_n = try alloc.create(Ir);
    var_n.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
    const var_n2 = try alloc.create(Ir);
    var_n2.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
    const lit_1 = try alloc.create(Ir);
    lit_1.* = .{ .lit = Value.makeFixnum(1) };
    const lit_42 = try alloc.create(Ir);
    lit_42.* = .{ .lit = Value.makeFixnum(42) };

    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_le = .{ .left = var_n, .right = lit_1 } };
    const body = try alloc.create(Ir);
    body.* = .{ .@"if" = .{ .cond = cond, .then_branch = var_n2, .else_branch = lit_42 } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = body,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "test_if");
    defer compiled.deinit();

    // n=0 (tagged=1): 0<=1 → return 0 (tagged=1)
    try testing.expectEqual(@as(i64, 1), compiled.call1(1));
    // n=1 (tagged=3): 1<=1 → return 1 (tagged=3)
    try testing.expectEqual(@as(i64, 3), compiled.call1(3));
    // n=5 (tagged=11): 5>1 → return 42 (tagged=85)
    try testing.expectEqual(@as(i64, 85), compiled.call1(11));
}

test "hoist IR translator: nested if in expression" {
    // (lambda (n) (+ (if (<= n 1) n 100) 10))
    // n=0 (tagged 1): (+ 0 10) = 10, tagged 21
    // n=5 (tagged 11): (+ 100 10) = 110, tagged 221
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const var_n = try alloc.create(Ir);
    var_n.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
    const var_n2 = try alloc.create(Ir);
    var_n2.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
    const lit_1 = try alloc.create(Ir);
    lit_1.* = .{ .lit = Value.makeFixnum(1) };
    const lit_100 = try alloc.create(Ir);
    lit_100.* = .{ .lit = Value.makeFixnum(100) };
    const lit_10 = try alloc.create(Ir);
    lit_10.* = .{ .lit = Value.makeFixnum(10) };

    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_le = .{ .left = var_n, .right = lit_1 } };
    const if_node = try alloc.create(Ir);
    if_node.* = .{ .@"if" = .{ .cond = cond, .then_branch = var_n2, .else_branch = lit_100 } };
    const add_node = try alloc.create(Ir);
    add_node.* = .{ .fixnum_add = .{ .left = if_node, .right = lit_10 } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = add_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "nested_if");
    defer compiled.deinit();

    // n=0 (tagged=1): if true → 0, then 0+10=10, tagged 21
    const r0 = compiled.call1(1);
    std.debug.print("nested_if(0) = {d} (expected 21)\n", .{r0});
    try testing.expectEqual(@as(i64, 21), r0);
    // n=5 (tagged=11): if false → 100, then 100+10=110, tagged 221
    const r5 = compiled.call1(11);
    std.debug.print("nested_if(5) = {d} (expected 221)\n", .{r5});
    try testing.expectEqual(@as(i64, 221), r5);
}

test "hoist IR translator: double recursive call" {
    // (defun f (n) (if (<= n 1) n (+ (f (- n 1)) (f (- n 2)))))
    // This is fib, testing specifically the double-call pattern with merge blocks
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const mkVar = struct {
        fn f(a: std.mem.Allocator) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
            return v;
        }
    }.f;

    const var_n = try mkVar(alloc);
    const var_n2 = try mkVar(alloc);
    const var_n3 = try mkVar(alloc);
    const var_n4 = try mkVar(alloc);
    const lit_1 = try alloc.create(Ir);
    lit_1.* = .{ .lit = Value.makeFixnum(1) };
    const lit_2 = try alloc.create(Ir);
    lit_2.* = .{ .lit = Value.makeFixnum(2) };

    // f(n-1)
    const sub1 = try alloc.create(Ir);
    sub1.* = .{ .fixnum_sub = .{ .left = var_n, .right = lit_1 } };
    const self1 = try alloc.create(Ir);
    self1.* = .{ .global_ref = .{ .name = "f", .index = 0 } };
    const args1 = try alloc.alloc(*const Ir, 1);
    args1[0] = sub1;
    const call1 = try alloc.create(Ir);
    call1.* = .{ .call = .{ .func = self1, .args = args1 } };

    // f(n-2)
    const sub2 = try alloc.create(Ir);
    sub2.* = .{ .fixnum_sub = .{ .left = var_n3, .right = lit_2 } };
    const self2 = try alloc.create(Ir);
    self2.* = .{ .global_ref = .{ .name = "f", .index = 0 } };
    const args2 = try alloc.alloc(*const Ir, 1);
    args2[0] = sub2;
    const call2 = try alloc.create(Ir);
    call2.* = .{ .call = .{ .func = self2, .args = args2 } };

    // f(n-1) + f(n-2)
    const add_ir = try alloc.create(Ir);
    add_ir.* = .{ .fixnum_add = .{ .left = call1, .right = call2 } };

    // (if (<= n 1) n (f(n-1) + f(n-2)))
    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_le = .{ .left = var_n2, .right = lit_1 } };
    const body = try alloc.create(Ir);
    body.* = .{ .@"if" = .{ .cond = cond, .then_branch = var_n4, .else_branch = add_ir } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = body,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "f");
    defer compiled.deinit();

    // f(0) = 0, tagged: 1
    const r0 = compiled.call1(1);
    std.debug.print("f(0) = {d} (raw), untagged = {d}\n", .{ r0, @divTrunc(r0, 2) });
    try testing.expectEqual(@as(i64, 1), r0);
    // f(1) = 1, tagged: 3
    const r1 = compiled.call1(3);
    std.debug.print("f(1) = {d} (raw), untagged = {d}\n", .{ r1, @divTrunc(r1, 2) });
    try testing.expectEqual(@as(i64, 3), r1);
    // f(2) = 1, tagged: 3
    const r2 = compiled.call1(5);
    std.debug.print("f(2) = {d} (raw), untagged = {d}\n", .{ r2, @divTrunc(r2, 2) });
    try testing.expectEqual(@as(i64, 3), r2);
}

test "hoist IR translator: countdown recursive" {
    // (defun countdown (n) (if (<= n 0) 0 (countdown (- n 1))))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const mkVar = struct {
        fn f(a: std.mem.Allocator) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
            return v;
        }
    }.f;
    const mkLit = struct {
        fn f(a: std.mem.Allocator, n: i64) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .lit = Value.makeFixnum(n) };
            return v;
        }
    }.f;

    // (fixnum_le n 0)
    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_le = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 0) } };

    // (fixnum_sub n 1)
    const n_minus_1 = try alloc.create(Ir);
    n_minus_1.* = .{ .fixnum_sub = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 1) } };

    // (call countdown (- n 1))
    const ref = try alloc.create(Ir);
    ref.* = .{ .global_ref = .{ .name = "countdown", .index = 0 } };
    const call_args = try alloc.alloc(*const Ir, 1);
    call_args[0] = n_minus_1;
    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = ref, .args = call_args } };

    // (if cond 0 (countdown (- n 1)))
    const if_node = try alloc.create(Ir);
    if_node.* = .{ .@"if" = .{ .cond = cond, .then_branch = try mkLit(alloc, 0), .else_branch = call_node } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = if_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "countdown");
    defer compiled.deinit();

    // countdown(0) = 0, tagged: 1
    try testing.expectEqual(@as(i64, 1), compiled.call1(1));
    // countdown(1) = 0, tagged: 1
    try testing.expectEqual(@as(i64, 1), compiled.call1(3));
    // countdown(5) = 0, tagged: 1
    try testing.expectEqual(@as(i64, 1), compiled.call1(11));
}

test "hoist IR translator: fib recursive" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build fib IR: (lambda (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))
    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const mkVar = struct {
        fn f(a: std.mem.Allocator) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
            return v;
        }
    }.f;
    const mkLit = struct {
        fn f(a: std.mem.Allocator, n: i64) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .lit = Value.makeFixnum(n) };
            return v;
        }
    }.f;
    const mkFibRef = struct {
        fn f(a: std.mem.Allocator) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .global_ref = .{ .name = "fib", .index = 0 } };
            return v;
        }
    }.f;

    // (fixnum_le n 1)
    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_le = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 1) } };

    // (fixnum_sub n 1)
    const n_minus_1 = try alloc.create(Ir);
    n_minus_1.* = .{ .fixnum_sub = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 1) } };

    // (fixnum_sub n 2)
    const n_minus_2 = try alloc.create(Ir);
    n_minus_2.* = .{ .fixnum_sub = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 2) } };

    // (call fib (- n 1))
    const call1_args = try alloc.alloc(*const Ir, 1);
    call1_args[0] = n_minus_1;
    const call1 = try alloc.create(Ir);
    call1.* = .{ .call = .{ .func = try mkFibRef(alloc), .args = call1_args } };

    // (call fib (- n 2))
    const call2_args = try alloc.alloc(*const Ir, 1);
    call2_args[0] = n_minus_2;
    const call2 = try alloc.create(Ir);
    call2.* = .{ .call = .{ .func = try mkFibRef(alloc), .args = call2_args } };

    // (fixnum_add call1 call2)
    const add = try alloc.create(Ir);
    add.* = .{ .fixnum_add = .{ .left = call1, .right = call2 } };

    // (if cond n add)
    const if_node = try alloc.create(Ir);
    if_node.* = .{ .@"if" = .{ .cond = cond, .then_branch = try mkVar(alloc), .else_branch = add } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = if_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "fib");
    defer compiled.deinit();

    // fib(0) = 0, tagged: 1 → 1
    const fib0 = compiled.call1(1);
    std.debug.print("fib(0) = {d} (expected 1)\n", .{fib0});
    try testing.expectEqual(@as(i64, 1), fib0);
    // fib(1) = 1, tagged: 3 → 3
    const fib1 = compiled.call1(3);
    std.debug.print("fib(1) = {d} (expected 3)\n", .{fib1});
    try testing.expectEqual(@as(i64, 3), fib1);
    // fib(2) = 1, tagged: 5 → 3
    const fib2 = compiled.call1(5);
    std.debug.print("fib(2) = {d} (expected 3)\n", .{fib2});
    try testing.expectEqual(@as(i64, 3), fib2);
    // fib(3) = 2, tagged: 7 → 5
    const fib3 = compiled.call1(7);
    std.debug.print("fib(3) = {d} (expected 5)\n", .{fib3});
    try testing.expectEqual(@as(i64, 5), fib3);
    // fib(5) = 5, tagged: 11 → 11
    try testing.expectEqual(@as(i64, 11), compiled.call1(11));
    // fib(10) = 55, tagged: (10<<1)|1=21 → (55<<1)|1=111
    const result = compiled.call1(21);
    try testing.expectEqual(@as(i64, 55), @as(i64, result) >> 1);
}

test "hoist IR translator: two-arg add" {
    // (lambda (a b) (+ a b))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 2);
    params[0] = "a";
    params[1] = "b";

    const var_a = try alloc.create(Ir);
    var_a.* = .{ .@"var" = .{ .name = "a", .depth = 0, .index = 0 } };
    const var_b = try alloc.create(Ir);
    var_b.* = .{ .@"var" = .{ .name = "b", .depth = 0, .index = 1 } };
    const add = try alloc.create(Ir);
    add.* = .{ .fixnum_add = .{ .left = var_a, .right = var_b } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = add,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "add2");
    defer compiled.deinit();

    // (+ 3 4) = 7. Tagged: 3→7, 4→9, 7→15
    const r = compiled.call2(7, 9);
    std.debug.print("add2(3,4) = {d} (expected 15)\n", .{r});
    try testing.expectEqual(@as(i64, 15), r);

    // (+ 0 0) = 0. Tagged: 0→1, 0→1, 0→1
    try testing.expectEqual(@as(i64, 1), compiled.call2(1, 1));
}

test "hoist IR translator: ackermann 2-arg recursive" {
    // (lambda (m n)
    //   (if (= m 0) (+ n 1)
    //     (if (= n 0) (ack (- m 1) 1)
    //       (ack (- m 1) (ack m (- n 1))))))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 2);
    params[0] = "m";
    params[1] = "n";

    const var_m = try alloc.create(Ir);
    var_m.* = .{ .@"var" = .{ .name = "m", .depth = 0, .index = 0 } };
    const var_n = try alloc.create(Ir);
    var_n.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 1 } };
    const lit_0 = try alloc.create(Ir);
    lit_0.* = .{ .lit = Value.makeFixnum(0) };
    const lit_1 = try alloc.create(Ir);
    lit_1.* = .{ .lit = Value.makeFixnum(1) };

    // (= m 0)
    const m_eq_0 = try alloc.create(Ir);
    m_eq_0.* = .{ .fixnum_eq = .{ .left = var_m, .right = lit_0 } };

    // (+ n 1) — base case
    const n_plus_1 = try alloc.create(Ir);
    n_plus_1.* = .{ .fixnum_add = .{ .left = var_n, .right = lit_1 } };

    // (= n 0)
    const n_eq_0 = try alloc.create(Ir);
    n_eq_0.* = .{ .fixnum_eq = .{ .left = var_n, .right = lit_0 } };

    // (- m 1)
    const m_minus_1 = try alloc.create(Ir);
    m_minus_1.* = .{ .fixnum_sub = .{ .left = var_m, .right = lit_1 } };

    // (- n 1)
    const n_minus_1 = try alloc.create(Ir);
    n_minus_1.* = .{ .fixnum_sub = .{ .left = var_n, .right = lit_1 } };

    // Self-call references
    const self1 = try alloc.create(Ir);
    self1.* = .{ .global_ref = .{ .name = "ack", .index = 0 } };
    const self2 = try alloc.create(Ir);
    self2.* = .{ .global_ref = .{ .name = "ack", .index = 0 } };
    const self3 = try alloc.create(Ir);
    self3.* = .{ .global_ref = .{ .name = "ack", .index = 0 } };

    // (ack (- m 1) 1)
    const args1 = try alloc.alloc(*const Ir, 2);
    args1[0] = m_minus_1;
    args1[1] = lit_1;
    const call_ack1 = try alloc.create(Ir);
    call_ack1.* = .{ .call = .{ .func = self1, .args = args1 } };

    // Need fresh copies of (- m 1) and (- n 1) and var_m for inner calls
    const m_minus_1_2 = try alloc.create(Ir);
    m_minus_1_2.* = .{ .fixnum_sub = .{ .left = var_m, .right = lit_1 } };
    const n_minus_1_2 = try alloc.create(Ir);
    n_minus_1_2.* = .{ .fixnum_sub = .{ .left = var_n, .right = lit_1 } };

    // (ack m (- n 1))
    const args2 = try alloc.alloc(*const Ir, 2);
    args2[0] = var_m;
    args2[1] = n_minus_1_2;
    const call_ack2 = try alloc.create(Ir);
    call_ack2.* = .{ .call = .{ .func = self2, .args = args2 } };

    // (ack (- m 1) (ack m (- n 1)))
    const args3 = try alloc.alloc(*const Ir, 2);
    args3[0] = m_minus_1_2;
    args3[1] = call_ack2;
    const call_ack3 = try alloc.create(Ir);
    call_ack3.* = .{ .call = .{ .func = self3, .args = args3 } };

    // (if (= n 0) (ack (- m 1) 1) (ack (- m 1) (ack m (- n 1))))
    const inner_if = try alloc.create(Ir);
    inner_if.* = .{ .@"if" = .{
        .cond = n_eq_0,
        .then_branch = call_ack1,
        .else_branch = call_ack3,
    } };

    // (if (= m 0) (+ n 1) inner_if)
    const outer_if = try alloc.create(Ir);
    outer_if.* = .{ .@"if" = .{
        .cond = m_eq_0,
        .then_branch = n_plus_1,
        .else_branch = inner_if,
    } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = outer_if,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "ack");
    defer compiled.deinit();

    // ack(0, 0) = 1, tagged: (0→1, 0→1) → (1→3)
    const r00 = compiled.call2(1, 1);
    std.debug.print("ack(0,0) = {d} (expected 3)\n", .{r00});
    try testing.expectEqual(@as(i64, 3), r00);

    // ack(0, 5) = 6, tagged: (0→1, 5→11) → (6→13)
    try testing.expectEqual(@as(i64, 13), compiled.call2(1, 11));

    // ack(1, 0) = 2, tagged: (1→3, 0→1) → (2→5)
    const r10 = compiled.call2(3, 1);
    std.debug.print("ack(1,0) = {d} (expected 5)\n", .{r10});
    try testing.expectEqual(@as(i64, 5), r10);

    // ack(1, 1) = 3, tagged: (1→3, 1→3) → (3→7)
    const r11 = compiled.call2(3, 3);
    std.debug.print("ack(1,1) = {d} (expected 7)\n", .{r11});
    try testing.expectEqual(@as(i64, 7), r11);

    // ack(2, 3) = 9, tagged: (2→5, 3→7) → (9→19)
    const r23 = compiled.call2(5, 7);
    std.debug.print("ack(2,3) = {d} (expected 19)\n", .{r23});
    try testing.expectEqual(@as(i64, 19), r23);

    // ack(3, 3) = 61, tagged: (3→7, 3→7) → (61→123)
    const r33 = compiled.call2(7, 7);
    std.debug.print("ack(3,3) = {d} (expected 123)\n", .{r33});
    try testing.expectEqual(@as(i64, 123), r33);
}

test "hoist IR translator: fixnum arithmetic" {
    // (lambda (n) (fixnum_add (fixnum_sub n 10) 20))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const var_n = try alloc.create(Ir);
    var_n.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
    const lit_10 = try alloc.create(Ir);
    lit_10.* = .{ .lit = Value.makeFixnum(10) };
    const lit_20 = try alloc.create(Ir);
    lit_20.* = .{ .lit = Value.makeFixnum(20) };

    const sub = try alloc.create(Ir);
    sub.* = .{ .fixnum_sub = .{ .left = var_n, .right = lit_10 } };
    const add = try alloc.create(Ir);
    add.* = .{ .fixnum_add = .{ .left = sub, .right = lit_20 } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = add,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "arith_test");
    defer compiled.deinit();

    // f(30) = (30-10)+20 = 40. Tagged: (30<<1)|1=61 → (40<<1)|1=81
    try testing.expectEqual(@as(i64, 81), compiled.call1(61));
    // f(0) = (0-10)+20 = 10. Tagged: 1 → (10<<1)|1=21
    try testing.expectEqual(@as(i64, 21), compiled.call1(1));
}

test "hoist IR translator: simple loop (let + while + setq)" {
    // (defun f () (let ((i 0) (acc 0)) (while (< i 10) (setq acc (+ acc i)) (setq i (+ i 1))) acc))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 0);

    const mkVar = struct {
        fn f(a: std.mem.Allocator, name: []const u8, idx: u16) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = name, .depth = 0, .index = idx } };
            return v;
        }
    }.f;
    const mkLit = struct {
        fn f(a: std.mem.Allocator, n: i64) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .lit = Value.makeFixnum(n) };
            return v;
        }
    }.f;

    // (setq acc (+ acc i))
    const add_node = try alloc.create(Ir);
    add_node.* = .{ .fixnum_add = .{ .left = try mkVar(alloc, "acc", 1), .right = try mkVar(alloc, "i", 0) } };
    const set_acc = try alloc.create(Ir);
    set_acc.* = .{ .set = .{ .name = "acc", .depth = 0, .index = 1, .value = add_node } };

    // (setq i (+ i 1))
    const inc_node = try alloc.create(Ir);
    inc_node.* = .{ .fixnum_add = .{ .left = try mkVar(alloc, "i", 0), .right = try mkLit(alloc, 1) } };
    const set_i = try alloc.create(Ir);
    set_i.* = .{ .set = .{ .name = "i", .depth = 0, .index = 0, .value = inc_node } };

    // body: (progn (setq acc ...) (setq i ...))
    const body_exprs = try alloc.alloc(*const Ir, 2);
    body_exprs[0] = set_acc;
    body_exprs[1] = set_i;
    const body = try alloc.create(Ir);
    body.* = .{ .progn = body_exprs };

    // cond: (< i 10)
    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_lt = .{ .left = try mkVar(alloc, "i", 0), .right = try mkLit(alloc, 10) } };

    // loop: (while cond body)
    const loop_node = try alloc.create(Ir);
    loop_node.* = .{ .loop = .{ .cond = cond, .body = body } };

    // let body: (progn loop acc)
    const let_body_exprs = try alloc.alloc(*const Ir, 2);
    let_body_exprs[0] = loop_node;
    let_body_exprs[1] = try mkVar(alloc, "acc", 1);
    const let_body = try alloc.create(Ir);
    let_body.* = .{ .progn = let_body_exprs };

    // let bindings: i=0, acc=0
    const bindings = try alloc.alloc(Ir.Binding, 2);
    bindings[0] = .{ .name = "i", .value = try mkLit(alloc, 0), .index = 0 };
    bindings[1] = .{ .name = "acc", .value = try mkLit(alloc, 0), .index = 1 };

    const let_node = try alloc.create(Ir);
    let_node.* = .{ .let = .{ .bindings = bindings, .body = let_body } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = let_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "loop_test");
    defer compiled.deinit();

    // sum(0..9) = 45. Tagged: (45<<1)|1 = 91
    const result = compiled.call0();
    try testing.expectEqual(@as(i64, 91), result);
}

// NOTE: Nested self-calls (call result as arg to another self-call) cause
// segfaults due to hoist regalloc not properly spilling across call_indirect.
// This pattern occurs in tak but not fib (fib uses + on call results).
// TODO: Fix hoist regalloc for this pattern, then re-enable test.

test "hoist IR translator: countdown callFromValues" {
    // Verify callFromValues works the same as call1 for recursive function
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const mkVar = struct {
        fn f(a: std.mem.Allocator) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
            return v;
        }
    }.f;
    const mkLit = struct {
        fn f(a: std.mem.Allocator, n: i64) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .lit = Value.makeFixnum(n) };
            return v;
        }
    }.f;

    const cond = try alloc.create(Ir);
    cond.* = .{ .fixnum_le = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 0) } };
    const n_minus_1 = try alloc.create(Ir);
    n_minus_1.* = .{ .fixnum_sub = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 1) } };
    const ref = try alloc.create(Ir);
    ref.* = .{ .global_ref = .{ .name = "countdown", .index = 0 } };
    const call_args = try alloc.alloc(*const Ir, 1);
    call_args[0] = n_minus_1;
    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = ref, .args = call_args } };
    const if_node = try alloc.create(Ir);
    if_node.* = .{ .@"if" = .{ .cond = cond, .then_branch = try mkLit(alloc, 42), .else_branch = call_node } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = if_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "countdown");
    defer compiled.deinit();

    // Test via call1
    try testing.expectEqual(@as(i64, 85), compiled.call1(1));  // countdown(0) = 42
    try testing.expectEqual(@as(i64, 85), compiled.call1(3));  // countdown(1) = 42

    // Test via callFromValues (same path as VM)
    const args0 = [_]Value{Value.makeFixnum(0)};
    const result0 = compiled.callFromValues(&args0);
    try testing.expectEqual(@as(u64, 85), result0.raw);

    const args1 = [_]Value{Value.makeFixnum(1)};
    const result1 = compiled.callFromValues(&args1);
    try testing.expectEqual(@as(u64, 85), result1.raw);

    const args3 = [_]Value{Value.makeFixnum(3)};
    const result3 = compiled.callFromValues(&args3);
    try testing.expectEqual(@as(u64, 85), result3.raw);
}

test "hoist IR translator: generic countdown recursive" {
    // Same as countdown but using generic le/sub instead of fixnum_le/fixnum_sub
    // (defun countdown (n) (if (<= n 0) 42 (countdown (- n 1))))
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 1);
    params[0] = "n";

    const mkVar = struct {
        fn f(a: std.mem.Allocator) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = "n", .depth = 0, .index = 0 } };
            return v;
        }
    }.f;
    const mkLit = struct {
        fn f(a: std.mem.Allocator, n: i64) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .lit = Value.makeFixnum(n) };
            return v;
        }
    }.f;

    // Generic le: (<= n 0)
    const cond = try alloc.create(Ir);
    cond.* = .{ .le = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 0) } };

    // Generic sub: (- n 1)
    const n_minus_1 = try alloc.create(Ir);
    n_minus_1.* = .{ .sub = .{ .left = try mkVar(alloc), .right = try mkLit(alloc, 1) } };

    // (call countdown (- n 1))
    const ref = try alloc.create(Ir);
    ref.* = .{ .global_ref = .{ .name = "countdown", .index = 0 } };
    const call_args = try alloc.alloc(*const Ir, 1);
    call_args[0] = n_minus_1;
    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = ref, .args = call_args } };

    // (if cond 42 (countdown (- n 1)))
    const if_node = try alloc.create(Ir);
    if_node.* = .{ .@"if" = .{ .cond = cond, .then_branch = try mkLit(alloc, 42), .else_branch = call_node } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = if_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "countdown");
    defer compiled.deinit();

    // countdown(0) = 42, tagged: 85
    try testing.expectEqual(@as(i64, 85), compiled.call1(1));
    // countdown(1) = 42, tagged: 85
    try testing.expectEqual(@as(i64, 85), compiled.call1(3));
    // countdown(5) = 42, tagged: 85
    try testing.expectEqual(@as(i64, 85), compiled.call1(11));
}

test "hoist phi loop: dump codegen for debugging" {
    // Compile phi loop and dump machine code (don't execute — known infinite loop).
    const allocator = testing.allocator;

    var sig = Signature.init(allocator, .system_v);
    try sig.returns.append(allocator, AbiParam.new(I64));

    var func = try Function.init(allocator, "phi_sum", sig);
    defer func.deinit();
    var b = try FunctionBuilder.init(allocator, &func);
    defer b.deinit();

    const entry = try func.dfg.addBlock();
    try func.layout.appendBlock(entry);

    const header = try func.dfg.addBlock();
    try func.layout.appendBlock(header);
    const phi_acc = try func.dfg.appendBlockParam(header, I64);
    const phi_i = try func.dfg.appendBlockParam(header, I64);

    const body_blk = try func.dfg.addBlock();
    try func.layout.appendBlock(body_blk);
    const exit_blk = try func.dfg.addBlock();
    try func.layout.appendBlock(exit_blk);

    // Entry: acc=1(tagged 0), i=1(tagged 0)
    b.switchToBlock(entry);
    const zero_t = try b.iconst(I64, 1);
    try b.jumpArgs(header, &.{ zero_t, zero_t });

    // Header: if i < 21 then body else exit
    b.switchToBlock(header);
    const limit = try b.iconst(I64, 21);
    const cmp = try b.icmp(I8, .slt, phi_i, limit);
    try b.brif(cmp, body_blk, exit_blk);

    // Body: new_acc = acc+i-1, new_i = i+3-1 (fixnum tagged ops)
    b.switchToBlock(body_blk);
    const sum_raw = try b.iadd(I64, phi_acc, phi_i);
    const one_a = try b.iconst(I64, 1);
    const new_acc = try b.isub(I64, sum_raw, one_a);
    const three = try b.iconst(I64, 3);
    const inc_raw = try b.iadd(I64, phi_i, three);
    const one_b = try b.iconst(I64, 1);
    const new_i = try b.isub(I64, inc_raw, one_b);
    try b.jumpArgs(header, &.{ new_acc, new_i });

    // Exit: return acc
    b.switchToBlock(exit_blk);
    try b.retValues(&.{phi_acc});

    // Print IR (debug)
    if (false) {
        var pp_buf: [8192]u8 = undefined;
        var pp_fbs = std.io.fixedBufferStream(&pp_buf);
        hoist.ir_print.writeFunction(pp_fbs.writer(), &func, .{}) catch {};
        std.debug.print("[phi-ir]\n{s}\n", .{pp_buf[0..pp_fbs.pos]});
    }

    // Compile
    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    var ctx = ctx_builder.optLevel(.none).callConv(.system_v).verification(true).build();

    var code = ctx.compileFunction(&func) catch |err| {
        std.debug.print("Phi compile error: {s}\n", .{@errorName(err)});
        return err;
    };
    defer code.deinit();

    // Dump machine code (debug)
    if (false) {
        std.debug.print("[phi-asm] {d} bytes:", .{code.code.items.len});
        for (code.code.items, 0..) |byte, idx| {
            if (idx % 4 == 0) std.debug.print(" ", .{});
            if (idx % 16 == 0) std.debug.print("\n  {x:0>4}: ", .{idx});
            std.debug.print("{x:0>2}", .{byte});
        }
        std.debug.print("\n", .{});
    }

    // Execute the compiled code
    var mem = try allocator.create(JitMem);
    mem.* = try JitMem.init(allocator, code.code.items.len);
    defer {
        mem.deinit();
        allocator.destroy(mem);
    }

    const buf = try mem.alloc(code.code.items.len, 16);
    try mem.writeExec(buf, code.code.items);
    try mem.setExec(true);

    const f: *const fn () callconv(.c) i64 = @ptrCast(@alignCast(buf.ptr));
    const result = f();
    // sum(0..9) = 45, tagged = 91
    try testing.expectEqual(@as(i64, 91), result);
}
