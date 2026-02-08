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
//! KNOWN LIMITATION: Hoist's linear scan register allocator does not save
//! caller-saved registers across call/call_indirect instructions. This means
//! recursive functions produce incorrect code. Until hoist's regalloc is fixed,
//! recursive functions should use the stencil JIT instead.

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

    /// Call with 1 tagged i64 arg, returns tagged i64.
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

    /// Name of the function being compiled (for self-call detection).
    fn_name: []const u8,

    /// Number of user-visible parameters (excludes hidden self_ptr).
    user_arity: u32,

    pub fn init(allocator: std.mem.Allocator, func: *Function, builder: *FunctionBuilder) IrTranslator {
        return .{
            .allocator = allocator,
            .func = func,
            .b = builder,
            .locals = std.ArrayList(HoistValue){},
            .is_recursive = false,
            .fn_name = "",
            .user_arity = 0,
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
            .fixnum_add => |op| try self.translateFixnumAdd(op.left, op.right),
            .fixnum_sub => |op| try self.translateFixnumSub(op.left, op.right),
            .fixnum_le => |op| try self.translateFixnumCmp(.sle, op.left, op.right),
            .fixnum_lt => |op| try self.translateFixnumCmp(.slt, op.left, op.right),
            .fixnum_gt => |op| try self.translateFixnumCmp(.sgt, op.left, op.right),
            .fixnum_ge => |op| try self.translateFixnumCmp(.sge, op.left, op.right),
            .fixnum_eq => |op| try self.translateFixnumCmp(.eq, op.left, op.right),
            .@"if" => |if_node| try self.translateIf(if_node.cond, if_node.then_branch, if_node.else_branch),
            .progn => |exprs| try self.translateProgn(exprs),
            .assert_fixnum => |op| try self.translate(op.operand), // At safety 0, just pass through
            .global_ref => |_| try self.translateLit(Value.nil), // TODO: general global refs
            .call, .tailcall => {
                // Recursive calls require hoist regalloc to be fixed first.
                // For now, return error to signal that this function can't be
                // compiled with hoist and should use the stencil JIT instead.
                return error.UnsupportedRecursiveCall;
            },
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

    fn translateProgn(self: *IrTranslator, exprs: []const *const Ir) anyerror!HoistValue {
        var result: HoistValue = undefined;
        for (exprs) |expr| {
            result = try self.translate(expr);
        }
        return result;
    }
};

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
    errdefer sig.deinit();
    for (0..arity) |_| {
        try sig.params.append(allocator, AbiParam.new(I64));
    }
    try sig.returns.append(allocator, AbiParam.new(I64));

    var func = try Function.init(allocator, name, sig);
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
        .optLevel(.aggressive)
        .callConv(.system_v)
        .verification(true)
        .build();

    var code = ctx.compileFunction(&func) catch |err| {
        return err;
    };
    defer code.deinit();

    // Allocate executable memory
    var mem = try allocator.create(JitMem);
    mem.* = try JitMem.init(allocator, code.code.items.len);
    errdefer {
        mem.deinit();
        allocator.destroy(mem);
    }

    const buf = try mem.alloc(code.code.items.len, 16);
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
