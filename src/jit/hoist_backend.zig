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
        };
    }

    pub fn deinit(self: *IrTranslator) void {
        self.locals.deinit(self.allocator);
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
        return try self.b.iconst(I64, @as(i64, @bitCast(val.raw)));
    }

    fn translateVar(self: *IrTranslator, v: anytype) HoistValue {
        if (v.depth == 0) {
            return self.locals.items[v.index];
        }
        unreachable; // TODO: closure captures
    }

    fn translateFixnumAdd(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        const l = try self.translate(left);
        const r = try self.translate(right);
        // Tagged fixnum add: result_raw = l_raw + r_raw - 1
        const sum = try self.b.iadd(I64, l, r);
        const one = try self.b.iconst(I64, 1);
        return try self.b.isub(I64, sum, one);
    }

    fn translateFixnumSub(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        const l = try self.translate(left);
        const r = try self.translate(right);
        // Tagged fixnum sub: result_raw = l_raw - r_raw + 1
        const diff = try self.b.isub(I64, l, r);
        const one = try self.b.iconst(I64, 1);
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

        try self.b.brif(cond_val, then_blk, else_blk);

        // Then branch: emit ret directly
        self.b.switchToBlock(then_blk);
        try self.b.sealBlock(then_blk);
        const then_val = try self.translate(then_ir);
        const then_ty = self.func.dfg.valueType(then_val) orelse I64;
        const then_i64 = if (then_ty.raw == I8.raw)
            try self.b.uextend(I64, then_val)
        else
            then_val;
        try self.b.retValues(&.{then_i64});

        // Else branch: emit ret directly
        self.b.switchToBlock(else_blk);
        try self.b.sealBlock(else_blk);
        const else_val = try self.translate(else_ir);
        const else_ty = self.func.dfg.valueType(else_val) orelse I64;
        const else_i64 = if (else_ty.raw == I8.raw)
            try self.b.uextend(I64, else_val)
        else
            else_val;
        try self.b.retValues(&.{else_i64});

        // Both branches returned — clear current block
        self.b.current_block = null;
        return then_i64; // sentinel, won't be used
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

    const StackSlotData = hoist.stack_slot_data.StackSlotData;
    const StackSlot = hoist.entities.StackSlot;

    /// Emit a stack_store instruction (no result value).
    fn emitStackStore(self: *IrTranslator, val: HoistValue, slot: StackSlot) !void {
        const block = self.b.current_block orelse return error.NoCurrentBlock;
        const inst_data = InstructionData{ .stack_store = .{
            .opcode = .stack_store,
            .arg = val,
            .stack_slot = slot,
            .offset = 0,
        } };
        const inst = try self.func.dfg.makeInst(inst_data);
        try self.func.layout.appendInst(inst, block);
    }

    /// Emit a stack_load instruction (returns loaded value).
    fn emitStackLoad(self: *IrTranslator, slot: StackSlot) !HoistValue {
        const block = self.b.current_block orelse return error.NoCurrentBlock;
        const inst_data = InstructionData{ .stack_load = .{
            .opcode = .stack_load,
            .stack_slot = slot,
            .offset = 0,
        } };
        const inst = try self.func.dfg.makeInst(inst_data);
        try self.func.layout.appendInst(inst, block);
        return try self.func.dfg.appendInstResult(inst, I64);
    }

    fn translateLoop(self: *IrTranslator, cond_ir: *const Ir, body_ir: *const Ir) anyerror!HoistValue {
        // Collect all variable indices mutated inside the loop body
        var mutated_indices = std.ArrayList(u16){};
        defer mutated_indices.deinit(self.allocator);
        try collectMutatedVars(body_ir, &mutated_indices, self.allocator);

        const n_vars = mutated_indices.items.len;
        if (n_vars > 16) return error.TooManyLoopVars;

        // Workaround for hoist block-param phi bug (#4):
        // Use stack slots for mutable loop variables instead of block parameters.
        // Less optimal (memory load/store each iteration) but avoids the phi
        // codegen bug that produces wrong register assignments for I64 loops.

        // Create stack slot per mutable variable
        var slots: [16]StackSlot = undefined;
        for (0..n_vars) |i| {
            slots[i] = try self.func.stack_slots.push(
                StackSlotData.init(.explicit_slot, 8, 3), // 8 bytes, 8-byte aligned
            );
        }

        // Store initial values into stack slots
        for (mutated_indices.items, 0..) |idx, i| {
            const init_val = if (idx < self.locals.items.len)
                self.locals.items[idx]
            else
                try self.b.iconst(I64, @as(i64, @bitCast(Value.nil.raw)));
            try self.emitStackStore(init_val, slots[i]);
        }

        // Create loop blocks using low-level API (no sealing needed since we
        // use stack slots instead of the builder's SSA variable mechanism)
        const header = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(header);
        const loop_body = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(loop_body);
        const loop_exit = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(loop_exit);

        // Jump from current block to header
        try self.b.jump(header);

        // Header: load variables from stack, evaluate condition
        self.b.switchToBlock(header);

        // Load current variable values from stack slots
        for (mutated_indices.items, 0..) |idx, i| {
            while (self.locals.items.len <= idx) {
                try self.locals.append(self.allocator, HoistValue.new(0));
            }
            self.locals.items[idx] = try self.emitStackLoad(slots[i]);
        }

        const cond_val = try self.translate(cond_ir);
        try self.b.brif(cond_val, loop_body, loop_exit);

        // Body: execute body statements, store updated values, jump back to header
        self.b.switchToBlock(loop_body);
        _ = try self.translate(body_ir);

        // Store updated variable values to stack slots
        for (mutated_indices.items, 0..) |idx, i| {
            const updated_val = if (idx < self.locals.items.len)
                self.locals.items[idx]
            else
                try self.b.iconst(I64, @as(i64, @bitCast(Value.nil.raw)));
            try self.emitStackStore(updated_val, slots[i]);
        }

        try self.b.jump(header);

        // Exit block
        self.b.switchToBlock(loop_exit);

        // After loop, load final values from stack
        for (mutated_indices.items, 0..) |idx, i| {
            self.locals.items[idx] = try self.emitStackLoad(slots[i]);
        }

        // Return nil (while loop doesn't produce a value in CL)
        return try self.b.iconst(I64, @as(i64, @bitCast(Value.nil.raw)));
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

    // Bail out for nested self-calls (e.g., tak) which trigger hoist regalloc bug
    if (translator.is_recursive and hasNestedSelfCalls(lambda.body, name)) {
        return error.UnsupportedNestedSelfCalls;
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

    // Translate body
    const result = translator.translate(lambda.body) catch |err| {
        return err;
    };

    // If the body is an if-expression, it already emitted returns.
    if (b.current_block != null) {
        const result_ty = func.dfg.valueType(result) orelse I64;
        const result_i64 = if (result_ty.raw == I8.raw)
            try b.uextend(I64, result)
        else
            result;
        try b.retValues(&.{result_i64});
    }

    // Compile with Hoist
    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    var ctx = ctx_builder
        .optLevel(if (translator.is_recursive or translator.has_loops) .none else .aggressive)
        .callConv(.system_v)
        .verification(true)
        .build();

    // Print function for debug
    if (false) {
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
    if (false) {
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

    try mem.writeExec(buf, code.code.items);
    try mem.setExec(true);

    return .{
        .mem = mem,
        .fn_ptr = @ptrCast(buf.ptr),
        .arity = arity,
        .allocator = allocator,
    };
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
    try testing.expectEqual(@as(i64, 1), compiled.call1(1));
    // fib(1) = 1, tagged: 3 → 3
    try testing.expectEqual(@as(i64, 3), compiled.call1(3));
    // fib(5) = 5, tagged: 11 → 11
    try testing.expectEqual(@as(i64, 11), compiled.call1(11));
    // fib(10) = 55, tagged: (10<<1)|1=21 → (55<<1)|1=111
    const result = compiled.call1(21);
    try testing.expectEqual(@as(i64, 55), @as(i64, result) >> 1);
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
