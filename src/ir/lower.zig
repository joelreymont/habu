//! IR Lowering - Tree IR to Register IR
//!
//! Converts Habu's tree-based IR to register-based IR suitable for
//! register allocation and native code generation.

const std = @import("std");
const tree_ir = @import("../compiler/ir.zig");
const Ir = tree_ir.Ir;
const inst = @import("instruction.zig");
const Inst = inst.Inst;
const Op = inst.Op;
const Reg = inst.Reg;
const REG = inst.REG;
const Builder = inst.Builder;
const Function = inst.Function;
const Value = @import("../runtime/value.zig").Value;

pub const LowerError = error{
    OutOfMemory,
    TooManyLocals,
    TooManyRegisters,
    UnsupportedIr,
    InvalidArity,
};

/// IR lowering context
pub const Lowerer = struct {
    allocator: std.mem.Allocator,
    builder: Builder,
    /// Map variable names to local slots
    locals: std.StringHashMap(u16),
    /// Next local slot
    next_local: u16,
    /// Pending branch patches (for forward jumps)
    patches: std.ArrayList(PendingPatch),

    const PendingPatch = struct {
        inst_idx: usize,
        target_label: []const u8,
    };

    pub fn init(allocator: std.mem.Allocator) Lowerer {
        return .{
            .allocator = allocator,
            .builder = Builder.init(allocator),
            .locals = std.StringHashMap(u16).init(allocator),
            .next_local = 0,
            .patches = std.ArrayList(PendingPatch){},
        };
    }

    pub fn deinit(self: *Lowerer) void {
        self.builder.deinit();
        self.locals.deinit();
        self.patches.deinit(self.allocator);
    }

    /// Allocate a local variable slot
    fn allocLocal(self: *Lowerer, name: []const u8) !u16 {
        const slot = self.next_local;
        if (slot >= 256) return LowerError.TooManyLocals;
        self.next_local += 1;
        try self.locals.put(name, slot);
        return slot;
    }

    /// Get local variable slot
    fn getLocal(self: *Lowerer, name: []const u8) ?u16 {
        return self.locals.get(name);
    }

    /// Lower an IR node, returning the register containing the result
    pub fn lower(self: *Lowerer, node: *const Ir) LowerError!Reg {
        return switch (node.*) {
            // Literals
            .lit => |val| try self.lowerLit(val),

            // Variables
            .@"var" => |v| try self.lowerVar(v.name, v.index),
            .set => |s| try self.lowerSet(s),
            .global_ref => |g| try self.lowerGlobalRef(g.index),
            .define => |d| try self.lowerDefine(d),

            // Binding
            .let => |l| try self.lowerLet(l),
            .lambda => try self.lowerLambda(node),

            // Control flow
            .@"if" => |i| try self.lowerIf(i),
            .progn => |exprs| try self.lowerProgn(exprs),
            .loop => |l| try self.lowerLoop(l),

            // Function calls
            .call => |c| try self.lowerCall(c),
            .tailcall => |t| try self.lowerTailcall(t),

            // Arithmetic
            .add => |b| try self.lowerBinary(.add, b),
            .sub => |b| try self.lowerBinary(.sub, b),
            .mul => |b| try self.lowerBinary(.mul, b),
            .div => |b| try self.lowerBinary(.div, b),
            .mod => |b| try self.lowerBinary(.mod, b),

            // Comparison
            .eq => |b| try self.lowerBinary(.eq, b),
            .lt => |b| try self.lowerBinary(.lt, b),
            .gt => |b| try self.lowerBinary(.gt, b),
            .le => |b| try self.lowerBinary(.le, b),
            .ge => |b| try self.lowerBinary(.ge, b),
            .eql => |b| try self.lowerBinary(.eql, b),
            .equal => |b| try self.lowerBinary(.equal, b),

            // Logic
            .not => |u| try self.lowerUnary(.not, u),

            // List operations
            .cons => |b| try self.lowerBinary(.cons, b),
            .car => |u| try self.lowerUnary(.car, u),
            .cdr => |u| try self.lowerUnary(.cdr, u),
            .list => |items| try self.lowerList(items),
            .length => |u| try self.lowerUnary(.len, u),

            // Type predicates
            .consp => |u| try self.lowerUnary(.consp, u),
            .nilp => |u| try self.lowerUnary(.nullp, u),
            .atom => |u| try self.lowerUnary(.atomp, u),
            .symbolp => |u| try self.lowerUnary(.symbolp, u),
            .numberp => |u| try self.lowerUnary(.numberp, u),

            // Vector operations
            .vec => |items| try self.lowerVector(items),
            .vec_ref => |b| try self.lowerBinary(.vref, b),
            .vec_len => |u| try self.lowerUnary(.len, u),

            // String operations
            .str_ref => |b| try self.lowerBinary(.sref, b),
            .str_len => |u| try self.lowerUnary(.len, u),

            // I/O
            .print => |u| try self.lowerPrint(u),

            else => LowerError.UnsupportedIr,
        };
    }

    fn lowerLit(self: *Lowerer, val: Value) !Reg {
        const dest = self.builder.freshReg();
        const raw = val.raw;

        // Check if it fits in 16-bit signed immediate
        if (val.isFixnum()) {
            const n = val.toFixnum();
            if (n >= -32768 and n <= 32767) {
                try self.builder.emitImm(dest, n);
                return dest;
            }
        }

        // For larger values, use constant pool
        const idx = try self.builder.addConstant(raw);
        try self.builder.emit(Inst.iu(.load_global, dest, 0, idx));
        return dest;
    }

    fn lowerVar(self: *Lowerer, name: []const u8, index: u16) !Reg {
        _ = name;
        const dest = self.builder.freshReg();
        try self.builder.emit(Inst.iu(.load_local, dest, 0, index));
        return dest;
    }

    fn lowerSet(self: *Lowerer, s: anytype) !Reg {
        const val_reg = try self.lower(s.value);
        try self.builder.emit(Inst.iu(.store_local, 0, val_reg, s.index));
        return val_reg;
    }

    fn lowerGlobalRef(self: *Lowerer, index: u16) !Reg {
        const dest = self.builder.freshReg();
        try self.builder.emit(Inst.iu(.load_global, dest, 0, index));
        return dest;
    }

    fn lowerDefine(self: *Lowerer, d: anytype) !Reg {
        const val_reg = try self.lower(d.value);
        // Store to global (using constant pool index as slot)
        const idx = try self.builder.addConstant(0); // placeholder for global slot
        try self.builder.emit(Inst.iu(.store_local, 0, val_reg, idx));
        return val_reg;
    }

    fn lowerLet(self: *Lowerer, l: anytype) !Reg {
        // Evaluate bindings
        for (l.bindings) |binding| {
            const val_reg = try self.lower(binding.value);
            const slot = try self.allocLocal(binding.name);
            try self.builder.emit(Inst.iu(.store_local, 0, val_reg, slot));
        }
        // Evaluate body
        return try self.lower(l.body);
    }

    fn lowerLambda(self: *Lowerer, _: *const Ir) !Reg {
        // Lambda creates a closure - emit closure instruction
        const dest = self.builder.freshReg();
        // For now, use a placeholder
        try self.builder.emit(Inst.iu(.closure, dest, 0, 0));
        return dest;
    }

    fn lowerIf(self: *Lowerer, i: anytype) !Reg {
        // Evaluate condition
        const cond_reg = try self.lower(i.cond);

        // Jump to else if nil
        const else_jump = self.builder.here();
        try self.builder.emitJz(cond_reg, 0); // placeholder

        // Then branch
        const then_reg = try self.lower(i.then_branch);
        const result = self.builder.freshReg();
        try self.builder.emitMov(result, then_reg);

        // Jump over else
        const end_jump = self.builder.here();
        try self.builder.emitJmp(0); // placeholder

        // Patch else jump
        self.builder.patchBranch(else_jump);

        // Else branch
        const else_reg = try self.lower(i.else_branch);
        try self.builder.emitMov(result, else_reg);

        // Patch end jump
        self.builder.patchBranch(end_jump);

        return result;
    }

    fn lowerProgn(self: *Lowerer, exprs: []const *const Ir) !Reg {
        var result: Reg = REG.zero;
        for (exprs) |expr| {
            result = try self.lower(expr);
        }
        return result;
    }

    fn lowerLoop(self: *Lowerer, l: anytype) !Reg {
        const loop_start = self.builder.here();

        // Evaluate condition
        const cond_reg = try self.lower(l.cond);

        // Exit if nil
        const exit_jump = self.builder.here();
        try self.builder.emitJz(cond_reg, 0); // placeholder

        // Loop body
        _ = try self.lower(l.body);

        // Jump back to start
        const offset: i16 = @intCast(@as(isize, @intCast(loop_start)) - @as(isize, @intCast(self.builder.here())) - 1);
        try self.builder.emitJmp(offset);

        // Patch exit
        self.builder.patchBranch(exit_jump);

        // Loops return nil
        const result = self.builder.freshReg();
        try self.builder.emitImm(result, 0);
        return result;
    }

    fn lowerCall(self: *Lowerer, c: anytype) !Reg {
        // Evaluate function
        const func_reg = try self.lower(c.func);

        // Evaluate arguments into consecutive registers
        const argc: u8 = @intCast(c.args.len);
        for (c.args, 0..) |arg, i| {
            const arg_reg = try self.lower(arg);
            // Move to argument register position
            const target: Reg = @intCast(i);
            if (arg_reg != target) {
                try self.builder.emitMov(target, arg_reg);
            }
        }

        // Emit call
        const result = self.builder.freshReg();
        try self.builder.emitCall(result, func_reg, argc);
        return result;
    }

    fn lowerTailcall(self: *Lowerer, t: anytype) !Reg {
        // Similar to call but use tailcall opcode
        const func_reg = try self.lower(t.func);
        const argc: u8 = @intCast(t.args.len);

        for (t.args, 0..) |arg, i| {
            const arg_reg = try self.lower(arg);
            const target: Reg = @intCast(i);
            if (arg_reg != target) {
                try self.builder.emitMov(target, arg_reg);
            }
        }

        // Tailcall doesn't return
        try self.builder.emit(Inst.iu(.tailcall, 0, func_reg, argc));
        return REG.zero;
    }

    fn lowerBinary(self: *Lowerer, op: Op, b: anytype) !Reg {
        const left = try self.lower(b.left);
        const right = try self.lower(b.right);
        const dest = self.builder.freshReg();
        try self.builder.emit(Inst.r(op, dest, left, right));
        return dest;
    }

    fn lowerUnary(self: *Lowerer, op: Op, u: anytype) !Reg {
        const src = try self.lower(u.operand);
        const dest = self.builder.freshReg();
        try self.builder.emit(Inst.r(op, dest, src, 0));
        return dest;
    }

    fn lowerList(self: *Lowerer, items: []const *const Ir) !Reg {
        if (items.len == 0) {
            // Empty list = nil
            const dest = self.builder.freshReg();
            try self.builder.emitImm(dest, 0);
            return dest;
        }

        // Build list from right to left: (cons a (cons b (cons c nil)))
        var result = self.builder.freshReg();
        try self.builder.emitImm(result, 0); // nil

        var i: usize = items.len;
        while (i > 0) {
            i -= 1;
            const elem = try self.lower(items[i]);
            const new_result = self.builder.freshReg();
            try self.builder.emitCons(new_result, elem, result);
            result = new_result;
        }

        return result;
    }

    fn lowerVector(self: *Lowerer, items: []const *const Ir) !Reg {
        // Make vector of size N
        const size_reg = self.builder.freshReg();
        try self.builder.emitImm(size_reg, @intCast(items.len));

        const init_reg = self.builder.freshReg();
        try self.builder.emitImm(init_reg, 0); // nil init

        const vec_reg = self.builder.freshReg();
        try self.builder.emit(Inst.r(.mkvec, vec_reg, size_reg, init_reg));

        // Set each element
        for (items, 0..) |item, idx| {
            const val_reg = try self.lower(item);
            const idx_reg = self.builder.freshReg();
            try self.builder.emitImm(idx_reg, @intCast(idx));
            try self.builder.emit(Inst.r(.vset, val_reg, vec_reg, idx_reg));
        }

        return vec_reg;
    }

    fn lowerPrint(self: *Lowerer, u: anytype) !Reg {
        const arg = try self.lower(u.operand);
        // Print is a native call
        try self.builder.emit(Inst.iu(.callz, arg, arg, 0)); // native function 0 = print
        return arg;
    }

    /// Lower a complete function
    pub fn lowerFunction(self: *Lowerer, name: []const u8, params: []const []const u8, body: *const Ir) !Function {
        // Allocate parameter slots
        for (params) |param| {
            _ = try self.allocLocal(param);
        }

        // Lower body
        const result_reg = try self.lower(body);

        // Return result
        try self.builder.emitRet(result_reg);

        return try self.builder.build(name, @intCast(params.len), @intCast(self.next_local));
    }
};

/// Lower tree IR to register IR function
pub fn lowerToFunction(allocator: std.mem.Allocator, name: []const u8, params: []const []const u8, body: *const Ir) !Function {
    var lowerer = Lowerer.init(allocator);
    defer lowerer.deinit();
    return try lowerer.lowerFunction(name, params, body);
}

// ============================================================================
// Tests
// ============================================================================

test "lower literal" {
    const testing = std.testing;
    var lowerer = Lowerer.init(testing.allocator);
    defer lowerer.deinit();

    // Create a literal IR node
    const lit_ir = Ir{ .lit = Value.makeFixnum(42) };
    const result = try lowerer.lower(&lit_ir);

    try testing.expect(result < 32);
    try testing.expectEqual(@as(usize, 1), lowerer.builder.code.items.len);
    try testing.expectEqual(Op.movi, lowerer.builder.code.items[0].op);
}

test "lower binary add" {
    const testing = std.testing;
    var lowerer = Lowerer.init(testing.allocator);
    defer lowerer.deinit();

    // (+ 1 2)
    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const add_ir = Ir{ .add = .{ .left = &lit1, .right = &lit2 } };

    const result = try lowerer.lower(&add_ir);

    try testing.expect(result < 32);
    // Should have: movi, movi, add
    try testing.expectEqual(@as(usize, 3), lowerer.builder.code.items.len);
    try testing.expectEqual(Op.add, lowerer.builder.code.items[2].op);
}

test "lower if" {
    const testing = std.testing;
    var lowerer = Lowerer.init(testing.allocator);
    defer lowerer.deinit();

    // (if t 1 2)
    const cond = Ir{ .lit = Value.t };
    const then_br = Ir{ .lit = Value.makeFixnum(1) };
    const else_br = Ir{ .lit = Value.makeFixnum(2) };
    const if_ir = Ir{ .@"if" = .{ .cond = &cond, .then_branch = &then_br, .else_branch = &else_br } };

    const result = try lowerer.lower(&if_ir);

    try testing.expect(result < 32);
    // Should have: movi (cond), jz, movi (then), mov, jmp, movi (else), mov
    try testing.expect(lowerer.builder.code.items.len >= 6);
}

test "lower list" {
    const testing = std.testing;
    var lowerer = Lowerer.init(testing.allocator);
    defer lowerer.deinit();

    // (list 1 2 3)
    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const lit3 = Ir{ .lit = Value.makeFixnum(3) };
    const items = [_]*const Ir{ &lit1, &lit2, &lit3 };
    const list_ir = Ir{ .list = &items };

    const result = try lowerer.lower(&list_ir);

    try testing.expect(result < 32);
    // Should have: movi (nil), movi, cons, movi, cons, movi, cons
    try testing.expect(lowerer.builder.code.items.len >= 7);
}

test "lower progn" {
    const testing = std.testing;
    var lowerer = Lowerer.init(testing.allocator);
    defer lowerer.deinit();

    // (progn 1 2 3)
    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const lit3 = Ir{ .lit = Value.makeFixnum(3) };
    const exprs = [_]*const Ir{ &lit1, &lit2, &lit3 };
    const progn_ir = Ir{ .progn = &exprs };

    const result = try lowerer.lower(&progn_ir);

    // Result should be the last expression's register
    try testing.expect(result < 32);
    // Should have: movi, movi, movi
    try testing.expectEqual(@as(usize, 3), lowerer.builder.code.items.len);
}
