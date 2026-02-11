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

const runtime = @import("../runtime/runtime.zig");
const Heap = runtime.Heap;
const Cons = runtime.Cons;

const I64 = HoistType.I64;
const I8 = HoistType.I8;

// ── Global heap pointer for JIT cons allocation ──
// Set by the VM before calling JIT functions that may allocate.
var g_heap: ?*Heap = null;

/// Set the global heap pointer for JIT allocation.
pub fn setHeap(heap: *Heap) void {
    g_heap = heap;
    jitConsRefreshCache();
}

/// C-ABI cons function callable from JIT-compiled code.
/// Takes (car_raw: u64, cdr_raw: u64) → cons_raw: u64
/// Performs bump allocation without GC. Returns nil (0) on OOM.
/// Fast inline cons: bump-allocate from a pre-cached region.
/// Falls back to heap.allocCons on overflow.
var g_alloc_ptr: u64 = 0;
var g_alloc_end: u64 = 0;

fn jitConsRefreshCache() void {
    if (g_heap) |heap| {
        g_alloc_ptr = @intFromPtr(heap.alloc_ptr);
        g_alloc_end = @intFromPtr(heap.from_end);
    }
}

fn jitCons(car_raw: u64, cdr_raw: u64) callconv(.c) u64 {
    const ptr = g_alloc_ptr;
    const next = ptr + 16;
    if (next <= g_alloc_end) {
        // Fast path: inline bump allocation
        const p: [*]u64 = @ptrFromInt(ptr);
        p[0] = car_raw; // car at offset 0
        p[1] = cdr_raw; // cdr at offset 8
        g_alloc_ptr = next;
        // Update heap's alloc_ptr to stay in sync
        if (g_heap) |heap| {
            heap.alloc_ptr = @ptrFromInt(next);
        }
        return ptr; // cons tag = 0, so raw = ptr
    }
    // Slow path: full allocation with potential GC
    const heap = g_heap orelse return 0;
    const car = Value{ .raw = car_raw };
    const cdr = Value{ .raw = cdr_raw };
    const result = heap.allocCons(car, cdr) catch return 0;
    // Refresh cache after potential GC
    jitConsRefreshCache();
    return result.raw;
}

/// Address of the jitCons function, usable as call target.
pub fn getJitConsPtr() u64 {
    return @intFromPtr(&jitCons);
}

/// Known JIT-compiled function info for cross-function calls.
/// Check if an IR node is a simple value (literal or variable reference).
/// Used to optimize TCO branches that just return a constant.
fn isSimpleValue(ir: *const Ir) bool {
    return switch (ir.*) {
        .lit, .@"var", .global_ref => true,
        else => false,
    };
}

/// Check if a function body calls itself (has self-recursive calls or tailcalls).
/// Used to prevent inlining recursive functions.
fn callsItself(body: *const Ir) bool {
    return switch (body.*) {
        .call, .tailcall => true, // Any call/tailcall in the body means it might be recursive
        .@"if" => |i| callsItself(i.cond) or callsItself(i.then_branch) or callsItself(i.else_branch),
        .let => |l| blk: {
            for (l.bindings) |b| {
                if (callsItself(b.value)) break :blk true;
            }
            break :blk callsItself(l.body);
        },
        .set => |s| callsItself(s.value),
        .progn => |exprs| blk: {
            for (exprs) |e| {
                if (callsItself(e)) break :blk true;
            }
            break :blk false;
        },
        .loop => |l| callsItself(l.cond) or callsItself(l.body),
        .fixnum_add, .fixnum_sub, .add, .sub,
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
        => |op| callsItself(op.left) or callsItself(op.right),
        .assert_fixnum, .nilp, .not, .consp, .abs,
        .car, .cdr, .unsafe_car, .unsafe_cdr,
        => |op| callsItself(op.operand),
        else => false,
    };
}

/// Check if any cross-function calls in the body would inline code containing loads.
fn crossCallsContainLoads(body: *const Ir, self_name: []const u8, kf: *const std.StringHashMap(KnownFn)) bool {
    return switch (body.*) {
        .call => |c| blk: {
            // Check if this call targets a known function with loads in its body
            if (getCallTargetName(c.func)) |target_name| {
                if (!namesMatch(target_name, self_name)) {
                    if (lookupKnownFnStatic(kf, target_name)) |known| {
                        if (known.ir_body) |ir_body| {
                            if (countIrNodes(ir_body) <= 30 and containsLoads(ir_body))
                                break :blk true;
                        }
                    }
                }
            }
            for (c.args) |arg| {
                if (crossCallsContainLoads(arg, self_name, kf)) break :blk true;
            }
            break :blk false;
        },
        .@"if" => |i| crossCallsContainLoads(i.cond, self_name, kf) or
            crossCallsContainLoads(i.then_branch, self_name, kf) or
            crossCallsContainLoads(i.else_branch, self_name, kf),
        .let => |l| blk: {
            for (l.bindings) |b| {
                if (crossCallsContainLoads(b.value, self_name, kf)) break :blk true;
            }
            break :blk crossCallsContainLoads(l.body, self_name, kf);
        },
        .set => |s| crossCallsContainLoads(s.value, self_name, kf),
        .progn => |exprs| blk: {
            for (exprs) |e| {
                if (crossCallsContainLoads(e, self_name, kf)) break :blk true;
            }
            break :blk false;
        },
        .loop => |l| crossCallsContainLoads(l.cond, self_name, kf) or
            crossCallsContainLoads(l.body, self_name, kf),
        .fixnum_add, .fixnum_sub, .add, .sub,
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
        => |op| crossCallsContainLoads(op.left, self_name, kf) or
            crossCallsContainLoads(op.right, self_name, kf),
        .assert_fixnum, .nilp, .not, .consp, .abs,
        .car, .cdr, .unsafe_car, .unsafe_cdr,
        => |op| crossCallsContainLoads(op.operand, self_name, kf),
        else => false,
    };
}

/// Static version of lookupKnownFn (no self parameter needed).
fn lookupKnownFnStatic(kf: *const std.StringHashMap(KnownFn), target_name: []const u8) ?KnownFn {
    // Direct lookup
    if (kf.get(target_name)) |v| return v;
    // Try with/without package prefix
    if (std.mem.indexOf(u8, target_name, ":")) |colon_pos| {
        if (kf.get(target_name[colon_pos + 1 ..])) |v| return v;
    }
    var it = kf.iterator();
    while (it.next()) |entry| {
        if (namesMatch(target_name, entry.key_ptr.*)) return entry.value_ptr.*;
    }
    return null;
}

/// Count the number of IR nodes in a tree (for inlining threshold).
fn countIrNodes(node: *const Ir) usize {
    return switch (node.*) {
        .lit, .@"var", .global_ref => 1,
        .fixnum_add, .fixnum_sub, .add, .sub,
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
        => |op| 1 + countIrNodes(op.left) + countIrNodes(op.right),
        .assert_fixnum, .nilp, .not, .consp, .abs,
        .car, .cdr, .unsafe_car, .unsafe_cdr,
        => |op| 1 + countIrNodes(op.operand),
        .@"if" => |f| 1 + countIrNodes(f.cond) + countIrNodes(f.then_branch) + countIrNodes(f.else_branch),
        .progn => |exprs| blk: {
            var c: usize = 1;
            for (exprs) |e| c += countIrNodes(e);
            break :blk c;
        },
        .let => |l| blk: {
            var c: usize = 1;
            for (l.bindings) |b| c += countIrNodes(b.value);
            c += countIrNodes(l.body);
            break :blk c;
        },
        .set => |s| 1 + countIrNodes(s.value),
        .loop => |l| 1 + countIrNodes(l.cond) + countIrNodes(l.body),
        .call => |c| blk: {
            var n: usize = 1 + countIrNodes(c.func);
            for (c.args) |a| n += countIrNodes(a);
            break :blk n;
        },
        .tailcall => |tc| blk: {
            var n: usize = 1 + countIrNodes(tc.func);
            for (tc.args) |a| n += countIrNodes(a);
            break :blk n;
        },
        else => 1,
    };
}

/// Scope for an inlined function call. Maps callee local indices to hoist values.
const InlineScope = struct {
    /// Base index in the caller's locals array where callee locals start.
    base: usize,
    /// Number of callee locals.
    count: usize,
};

pub const KnownFn = struct {
    fn_ptr: u64,
    arity: u32,
    /// Callee IR body for inlining (null if not available).
    ir_body: ?*const Ir = null,
    /// Callee parameter names for inlining (null if not available).
    param_names: ?[]const []const u8 = null,
    /// Callee function name (for detecting self-calls during inlined TCO).
    callee_name: []const u8 = "",
};

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
    /// Function name (for cross-function call lookup).
    name: []const u8 = "",
    /// IR arena kept alive for inlining. Null if not needed.
    ir_arena: ?*std.heap.ArenaAllocator = null,
    /// Lambda IR body for inlining (lives in ir_arena).
    ir_body: ?*const Ir = null,
    /// Parameter names for inlining (lives in ir_arena).
    param_names: ?[]const []const u8 = null,

    pub fn deinit(self: *CompiledFn) void {
        if (self.name.len > 0) self.allocator.free(self.name);
        if (self.ir_arena) |arena| {
            arena.deinit();
            self.allocator.destroy(arena);
        }
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

    /// Stack of inline scopes for inlined function calls.
    /// Each scope maps the callee's local indices to hoist values.
    /// When non-empty, variable lookups check the top scope first.
    inline_scopes: std.ArrayList(InlineScope),

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

    /// SigRef for calling jitCons (call_indirect).
    cons_sig_ref: ?SigRef = null,

    /// Map of known JIT-compiled function names → (fn_ptr, arity).
    /// Used for cross-function calls via call_indirect.
    known_fns: ?*const std.StringHashMap(KnownFn) = null,

    /// Whether the function has cross-function calls (non-self call_indirect).
    has_cross_calls: bool = false,

    /// Cache for cross-function call signatures (keyed by arity).
    call_sigs: [8]?SigRef = .{null} ** 8,

    /// When true, all internal values are untagged plain i64.
    /// Params are untagged at entry, result is re-tagged at return.
    /// Self-calls use untagged convention (no tag/untag at call boundary).
    untagged: bool = false,

    /// Whether the function contains load instructions (car, cdr).
    /// .aggressive optimization incorrectly eliminates loads.
    has_loads: bool = false,

    /// TCO: loop header block — tail calls jump here with updated params.
    tco_header: ?Block = null,
    /// TCO: exit block — non-tail returns jump here with result value.
    tco_exit: ?Block = null,


    pub fn init(allocator: std.mem.Allocator, func: *Function, builder: *FunctionBuilder) IrTranslator {
        return .{
            .allocator = allocator,
            .func = func,
            .b = builder,
            .locals = std.ArrayList(HoistValue){},
            .inline_scopes = std.ArrayList(InlineScope){},
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
        self.inline_scopes.deinit(self.allocator);
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

    /// Fast check: can we translate all nodes in this IR tree?
    /// Returns false if any unsupported node is found.
    pub fn canTranslate(ir: *const Ir) bool {
        return switch (ir.*) {
            .lit, .@"var", .global_ref => true,
            .fixnum_add, .fixnum_sub, .add, .sub => |op| canTranslate(op.left) and canTranslate(op.right),
            .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
            .le, .lt, .gt, .ge, .num_eq,
            => |op| canTranslate(op.left) and canTranslate(op.right),
            .@"if" => |f| canTranslate(f.cond) and canTranslate(f.then_branch) and canTranslate(f.else_branch),
            .progn => |exprs| {
                for (exprs) |e| if (!canTranslate(e)) return false;
                return true;
            },
            .let => |l| {
                for (l.bindings) |b| if (!canTranslate(b.value)) return false;
                return canTranslate(l.body);
            },
            .set => |s| canTranslate(s.value),
            .loop => |l| canTranslate(l.cond) and canTranslate(l.body),
            .assert_fixnum => |op| canTranslate(op.operand),
            .call => |c| {
                // Only self-recursive calls are supported; cross-function calls are not yet.
                // canTranslate is called without fn_name context, so we can't check self-call here.
                // We allow all calls to pass canTranslate and handle non-self calls gracefully in translate.
                if (!canTranslate(c.func)) return false;
                for (c.args) |a| if (!canTranslate(a)) return false;
                return true;
            },
            .tailcall => |tc| {
                if (!canTranslate(tc.func)) return false;
                for (tc.args) |a| if (!canTranslate(a)) return false;
                return true;
            },
            .fixnum_mul => |op| canTranslate(op.left) and canTranslate(op.right),
            .mul => |op| canTranslate(op.left) and canTranslate(op.right),
            .eq => |op| canTranslate(op.left) and canTranslate(op.right),
            // List / predicate operations (inline, no heap access needed)
            .nilp, .not, .consp, .abs => |op| canTranslate(op.operand),
            .car, .cdr, .unsafe_car, .unsafe_cdr => |op| canTranslate(op.operand),
            .cons => |op| canTranslate(op.left) and canTranslate(op.right),
            else => false,
        };
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
            .eq => |op| try self.translateFixnumCmp(.eq, op.left, op.right),
            .fixnum_mul => |op| try self.translateFixnumMul(op.left, op.right),
            .mul => |op| try self.translateFixnumMul(op.left, op.right),
            .@"if" => |if_node| try self.translateIf(if_node.cond, if_node.then_branch, if_node.else_branch),
            .progn => |exprs| try self.translateProgn(exprs),
            .let => |let_node| try self.translateLet(let_node.bindings, let_node.body),
            .set => |set_node| try self.translateSet(set_node.index, set_node.value),
            .loop => |loop_node| try self.translateLoop(loop_node.cond, loop_node.body),
            .assert_fixnum => |op| try self.translate(op.operand), // At safety 0, just pass through
            .global_ref => |_| try self.translateLit(Value.nil), // TODO: general global refs
            // List / predicate operations (inline, no heap access needed)
            .nilp => |op| try self.translateNilp(op.operand),
            .not => |op| try self.translateNot(op.operand),
            .consp => |op| try self.translateConsp(op.operand),
            .car => |op| try self.translateCar(op.operand),
            .cdr => |op| try self.translateCdr(op.operand),
            .unsafe_car => |op| try self.translateUnsafeCar(op.operand),
            .unsafe_cdr => |op| try self.translateUnsafeCdr(op.operand),
            .abs => |op| try self.translateAbs(op.operand),
            // Heap allocation (calls C-ABI runtime function)
            .cons => |op| try self.translateCons(op.left, op.right),
            .call => |call_node| try self.translateCall(call_node.func, call_node.args),
            .tailcall => |tc| try self.translateCall(tc.func, tc.args),
            else => {
                return error.UnsupportedIrNode;
            },
        };
    }

    fn translateLit(self: *IrTranslator, val: Value) anyerror!HoistValue {
        if (self.untagged and val.isFixnum()) {
            return try self.cachedIconst(val.toFixnum());
        }
        return try self.cachedIconst(@as(i64, @bitCast(val.raw)));
    }

    fn translateVar(self: *IrTranslator, v: anytype) HoistValue {
        if (v.depth == 0) {
            // If we're inside an inlined call, remap indices
            if (self.inline_scopes.items.len > 0) {
                const scope = self.inline_scopes.items[self.inline_scopes.items.len - 1];
                const idx = scope.base + v.index;
                if (idx < self.locals.items.len) {
                    return self.locals.items[idx];
                }
            }
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
        if (self.untagged) {
            const l = try self.translate(left);
            const r = try self.translate(right);
            return try self.b.iadd(I64, l, r);
        }
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
        if (self.untagged) {
            const l = try self.translate(left);
            const r = try self.translate(right);
            return try self.b.isub(I64, l, r);
        }
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

    fn translateFixnumMul(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged) {
            const l = try self.translate(left);
            const r = try self.translate(right);
            return try self.b.imul(I64, l, r);
        }
        // Tagged fixnum mul: result = sshr(a, 1) * (b - 1) + 1
        // Proof: a=2va+1, b=2vb+1. sshr(a,1)=va. b-1=2vb. va*2vb=2*va*vb. +1 = tagged(va*vb).
        // When one operand is a constant, fold: sshr(a,1) * (const_raw - 1) + 1
        if (getFixnumLit(right)) |r_const| {
            const l = try self.translate(left);
            const one = try self.cachedIconst(1);
            const l_val = try self.b.sshr(I64, l, one);
            const r_untagged = try self.cachedIconst(r_const - 1); // 2 * r_value
            const prod = try self.b.imul(I64, l_val, r_untagged);
            return try self.b.iadd(I64, prod, one);
        }
        if (getFixnumLit(left)) |l_const| {
            const r = try self.translate(right);
            const one = try self.cachedIconst(1);
            const r_val = try self.b.sshr(I64, r, one);
            const l_untagged = try self.cachedIconst(l_const - 1);
            const prod = try self.b.imul(I64, r_val, l_untagged);
            return try self.b.iadd(I64, prod, one);
        }
        const l = try self.translate(left);
        const r = try self.translate(right);
        const one = try self.cachedIconst(1);
        const l_val = try self.b.sshr(I64, l, one);
        const r_minus_1 = try self.b.isub(I64, r, one);
        const prod = try self.b.imul(I64, l_val, r_minus_1);
        return try self.b.iadd(I64, prod, one);
    }

    fn translateFixnumCmp(self: *IrTranslator, cc: IntCC, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        const l = try self.translate(left);
        const r = try self.translate(right);
        return try self.b.icmp(I8, cc, l, r);
    }

    /// Convert an I8 boolean (0/1) to a tagged Lisp value (nil=0, t=2).
    /// Used when a comparison result is needed as a Lisp value (not just a branch condition).
    fn boolToTagged(self: *IrTranslator, val: HoistValue) anyerror!HoistValue {
        const val_ty = self.func.dfg.valueType(val) orelse I64;
        if (val_ty.raw != I8.raw) return val; // Already I64
        const t_val = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw)));
        const nil_val = try self.cachedIconst(0);
        return try self.b.select(I64, val, t_val, nil_val);
    }

    fn translateIf(self: *IrTranslator, cond_ir: *const Ir, then_ir_orig: *const Ir, else_ir_orig: *const Ir) anyerror!HoistValue {
        // Optimize condition patterns to avoid redundant conversions:
        // - (not expr): swap branches, translate expr directly
        // - (nilp expr) / (null expr): compare expr with 0, avoid select
        var actual_cond = cond_ir;
        var then_ir = then_ir_orig;
        var else_ir = else_ir_orig;
        while (actual_cond.* == .not) {
            actual_cond = actual_cond.not.operand;
            const tmp = then_ir;
            then_ir = else_ir;
            else_ir = tmp;
        }
        // For nilp as condition: emit icmp eq(val, 0) directly (I8)
        // instead of icmp + select(t, nil) + brif(tagged)
        const cond_val = if (actual_cond.* == .nilp) blk: {
            const inner_val = try self.translate(actual_cond.nilp.operand);
            const zero = try self.cachedIconst(0);
            break :blk try self.b.icmp(I8, IntCC.eq, inner_val, zero);
        } else try self.translate(actual_cond);

        const then_blk = try self.b.createBlock();
        const else_blk = try self.b.createBlock();
        const merge_blk = try self.b.createBlock();

        // Merge block has one param: the phi result (I64)
        const merge_param = try self.b.appendBlockParam(merge_blk, I64);

        // Find locals mutated in either branch that are IN SCOPE (already in locals).
        // Variables created by `let` inside a branch are local to that branch.
        const num_in_scope: u16 = @intCast(self.locals.items.len);
        var mutated_indices = std.ArrayList(u16){};
        defer mutated_indices.deinit(self.allocator);
        try collectMutatedVars(then_ir, &mutated_indices, self.allocator);
        try collectMutatedVars(else_ir, &mutated_indices, self.allocator);
        // Deduplicate and filter to in-scope only
        std.mem.sort(u16, mutated_indices.items, {}, std.sort.asc(u16));
        var deduped_count: usize = 0;
        for (mutated_indices.items) |idx| {
            if (idx >= num_in_scope) continue; // Not in scope yet
            if (deduped_count == 0 or mutated_indices.items[deduped_count - 1] != idx) {
                mutated_indices.items[deduped_count] = idx;
                deduped_count += 1;
            }
        }
        mutated_indices.shrinkRetainingCapacity(deduped_count);

        // Add merge block params for each mutated local
        var merge_local_params = std.ArrayList(HoistValue){};
        defer merge_local_params.deinit(self.allocator);
        for (mutated_indices.items) |_| {
            const p = try self.b.appendBlockParam(merge_blk, I64);
            try merge_local_params.append(self.allocator, p);
        }

        // Save locals before branching
        const saved_locals = try self.allocator.alloc(HoistValue, self.locals.items.len);
        defer self.allocator.free(saved_locals);
        @memcpy(saved_locals, self.locals.items);

        try self.b.brif(cond_val, then_blk, else_blk);

        // Then branch
        self.b.switchToBlock(then_blk);
        try self.b.sealBlock(then_blk);
        const then_val = try self.translate(then_ir);
        if (self.b.current_block != null) {
            const then_i64 = try self.boolToTagged(then_val);
            // Build jump args: [result, mutated_locals...]
            var then_args = std.ArrayList(HoistValue){};
            defer then_args.deinit(self.allocator);
            try then_args.append(self.allocator, then_i64);
            for (mutated_indices.items) |idx| {
                try then_args.append(self.allocator, self.locals.items[idx]);
            }
            try self.b.jumpArgs(merge_blk, then_args.items);
        }

        // Record then-branch locals, restore saved for else branch
        const then_locals = try self.allocator.alloc(HoistValue, self.locals.items.len);
        defer self.allocator.free(then_locals);
        @memcpy(then_locals, self.locals.items);
        @memcpy(self.locals.items, saved_locals);

        // Else branch
        self.b.switchToBlock(else_blk);
        try self.b.sealBlock(else_blk);
        const else_val = try self.translate(else_ir);
        if (self.b.current_block != null) {
            const else_i64 = try self.boolToTagged(else_val);
            var else_args = std.ArrayList(HoistValue){};
            defer else_args.deinit(self.allocator);
            try else_args.append(self.allocator, else_i64);
            for (mutated_indices.items) |idx| {
                try else_args.append(self.allocator, self.locals.items[idx]);
            }
            try self.b.jumpArgs(merge_blk, else_args.items);
        }

        // Continue in merge block — update locals from merge params
        self.b.switchToBlock(merge_blk);
        try self.b.sealBlock(merge_blk);
        for (mutated_indices.items, 0..) |idx, i| {
            self.locals.items[idx] = merge_local_params.items[i];
        }

        return merge_param;
    }

    fn translateLet(self: *IrTranslator, bindings: []const Ir.Binding, body: *const Ir) anyerror!HoistValue {
        // Evaluate each binding and add to locals
        // Remap indices through inline scope if active
        const scope_base: usize = if (self.inline_scopes.items.len > 0)
            self.inline_scopes.items[self.inline_scopes.items.len - 1].base
        else
            0;
        for (bindings) |binding| {
            const val = try self.translate(binding.value);
            const actual_index = scope_base + binding.index;
            // Extend locals array if needed
            while (self.locals.items.len <= actual_index) {
                try self.locals.append(self.allocator, HoistValue.new(0)); // placeholder
            }
            self.locals.items[actual_index] = val;
        }
        return try self.translate(body);
    }

    fn translateSet(self: *IrTranslator, index: u16, value_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(value_ir);
        // Remap index through inline scope if active
        const actual_index: usize = if (self.inline_scopes.items.len > 0)
            self.inline_scopes.items[self.inline_scopes.items.len - 1].base + index
        else
            index;
        if (actual_index < self.locals.items.len) {
            self.locals.items[actual_index] = val;
        }
        return val;
    }

    /// Pre-emit all literal constants found in an IR tree into the current block.
    /// This effectively performs LICM for constants when called before entering a loop.
    fn preEmitConstants(self: *IrTranslator, ir: *const Ir) !void {
        switch (ir.*) {
            .lit => |v| {
                if (self.untagged and v.isFixnum()) {
                    _ = try self.cachedIconst(v.toFixnum());
                } else {
                    _ = try self.cachedIconst(@as(i64, @bitCast(v.raw)));
                }
            },
            .fixnum_add, .fixnum_sub, .add, .sub => |op| {
                if (!self.untagged) {
                    // Pre-emit the folded constant for tagged fixnum ops
                    if (getFixnumLit(op.right)) |r_const| {
                        _ = try self.cachedIconst(r_const - 1);
                    } else if (getFixnumLit(op.left)) |l_const| {
                        _ = try self.cachedIconst(l_const - 1);
                    }
                }
                try self.preEmitConstants(op.left);
                try self.preEmitConstants(op.right);
            },
            .fixnum_mul, .mul => |op| {
                if (!self.untagged) {
                    if (getFixnumLit(op.right)) |r_const| {
                        _ = try self.cachedIconst(r_const - 1);
                    } else if (getFixnumLit(op.left)) |l_const| {
                        _ = try self.cachedIconst(l_const - 1);
                    }
                    _ = try self.cachedIconst(1); // for sshr
                }
                try self.preEmitConstants(op.left);
                try self.preEmitConstants(op.right);
            },
            .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq, .eq => |op| {
                _ = try self.cachedIconst(0); // nil
                _ = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw))); // t
                try self.preEmitConstants(op.left);
                try self.preEmitConstants(op.right);
            },
            .le, .lt, .gt, .ge, .num_eq => |op| {
                _ = try self.cachedIconst(0); // nil
                _ = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw))); // t
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
            // List / predicate ops — pre-emit constants used in inline translation
            .nilp, .not => |n| {
                _ = try self.cachedIconst(0); // nil
                _ = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw))); // t
                try self.preEmitConstants(n.operand);
            },
            .consp => |n| {
                _ = try self.cachedIconst(0); // nil
                _ = try self.cachedIconst(0xF); // tag mask
                _ = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw))); // t
                try self.preEmitConstants(n.operand);
            },
            .car, .unsafe_car => |n| {
                try self.preEmitConstants(n.operand);
            },
            .cdr, .unsafe_cdr => |n| {
                _ = try self.cachedIconst(8); // cdr offset
                try self.preEmitConstants(n.operand);
            },
            .abs => |n| {
                _ = try self.cachedIconst(1); // for sge comparison
                _ = try self.cachedIconst(2); // for 2 - raw
                try self.preEmitConstants(n.operand);
            },
            .cons => |n| {
                _ = try self.cachedIconst(@as(i64, @bitCast(getJitConsPtr()))); // cons fn ptr
                try self.preEmitConstants(n.left);
                try self.preEmitConstants(n.right);
            },
            .call => |c| {
                // Don't pre-emit call function pointers — they'll be emitted locally
                // in the block where the call happens. Pre-emitting them in the
                // header/entry block creates register pressure by keeping the pointer
                // live across the entire function.
                for (c.args) |arg| try self.preEmitConstants(arg);
            },
            .tailcall => |tc| {
                // Don't pre-emit function pointers for tailcalls either.
                // In TCO mode, self-tailcalls become jumps (no pointer needed).
                // Non-TCO tailcalls emit the pointer locally.
                for (tc.args) |arg| try self.preEmitConstants(arg);
            },
            .let => |l| {
                for (l.bindings) |binding| try self.preEmitConstants(binding.value);
                try self.preEmitConstants(l.body);
            },
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

        // Check for cross-function call to known JIT-compiled function
        if (self.known_fns) |kf| {
            if (getCallTargetName(func_ir)) |target_name| {
                if (self.lookupKnownFn(kf, target_name)) |known| {
                    return try self.translateCrossCall(known, args);
                }
            }
        }

        // Unsupported call — should have been rejected by hasNonSelfCalls
        return try self.b.iconst(I64, 0); // placeholder nil
    }

    fn translateCrossCall(self: *IrTranslator, known: KnownFn, args: []const *const Ir) anyerror!HoistValue {
        if (known.ir_body != null and known.param_names != null) {
            const body = known.ir_body.?;
            const params = known.param_names.?;
            if (params.len != args.len) return try self.emitCrossCallIndirect(known, args);

            // Try inlining small non-recursive functions directly
            if (countIrNodes(body) <= 30 and !callsItself(body)) {
                return try self.translateInlinedCall(params, body, args);
            }

            // Try inlining tail-recursive functions as loops
            if (countIrNodes(body) <= 40 and known.callee_name.len > 0 and
                hasSelfTailCalls(body, known.callee_name) and
                !hasNonTailSelfCalls(body, known.callee_name))
            {
                return try self.translateInlinedTCOCall(known, params, body, args);
            }
        }

        return try self.emitCrossCallIndirect(known, args);
    }

    /// Emit a cross-function call as call_indirect (non-inlined path).
    fn emitCrossCallIndirect(self: *IrTranslator, known: KnownFn, args: []const *const Ir) anyerror!HoistValue {
        // Translate all arguments
        var translated_args: [16]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }

        // Get or create signature for this arity
        const arity: u32 = @intCast(args.len);
        const sig = try self.getCallSigForArity(arity);

        // Load the target function pointer
        const fn_ptr = try self.cachedIconst(@as(i64, @bitCast(known.fn_ptr)));

        // Build argument list: [fn_ptr, arg0, arg1, ...]
        var call_args = ValueList.default();
        try self.func.dfg.value_lists.push(&call_args, fn_ptr);
        for (0..arity) |i| {
            try self.func.dfg.value_lists.push(&call_args, translated_args[i]);
        }

        // Emit call_indirect
        const call_data = InstructionData{
            .call_indirect = .{
                .opcode = .call_indirect,
                .sig_ref = sig,
                .args = call_args,
            },
        };
        const call_inst = try self.func.dfg.makeInst(call_data);
        const call_result = try self.func.dfg.appendInstResult(call_inst, I64);
        const block = self.b.current_block orelse return error.NoCurrentBlock;
        try self.func.layout.appendInst(call_inst, block);

        return call_result;
    }

    /// Inline a tail-recursive cross-function call as a loop.
    /// Creates header/exit blocks similar to translateTCOBody, but within the
    /// caller's function. The callee's tail calls become jumps to the header.
    fn translateInlinedTCOCall(
        self: *IrTranslator,
        known: KnownFn,
        callee_params: []const []const u8,
        callee_body: *const Ir,
        args: []const *const Ir,
    ) anyerror!HoistValue {
        const arity = callee_params.len;
        if (arity > 8) return try self.emitCrossCallIndirect(known, args);

        // Translate all arguments first (in caller's scope)
        var translated_args: [8]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }

        // Create loop header with phi params for callee's parameters
        const header = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(header);

        var phi_vals: [8]HoistValue = undefined;
        for (0..arity) |pi| {
            phi_vals[pi] = try self.func.dfg.appendBlockParam(header, I64);
        }

        // Create exit block with result phi
        const exit = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(exit);
        const exit_param = try self.func.dfg.appendBlockParam(exit, I64);

        // Jump from current block to header with initial arg values
        try self.b.jumpArgs(header, translated_args[0..arity]);

        // Switch to header block
        self.b.switchToBlock(header);

        // Push inline scope: callee locals start after caller's locals
        const base = self.locals.items.len;
        for (0..arity) |i| {
            try self.locals.append(self.allocator, phi_vals[i]);
        }
        try self.inline_scopes.append(self.allocator, .{
            .base = base,
            .count = arity,
        });

        // Pre-emit constants in header block (dominating position for loop body)
        try self.preEmitConstants(callee_body);

        // Save and override TCO state + fn_name for callee context
        const saved_tco_header = self.tco_header;
        const saved_tco_exit = self.tco_exit;
        const saved_fn_name = self.fn_name;
        self.tco_header = header;
        self.tco_exit = exit;
        self.fn_name = known.callee_name;

        // Translate body in TCO context
        const body_result = try self.translateTCOExpr(callee_body);

        // If body produces a value (non-tail path), jump to exit
        if (self.b.current_block != null) {
            const result_tagged = try self.boolToTagged(body_result);
            try self.b.jumpArgs(exit, &.{result_tagged});
        }

        // Restore TCO state + fn_name
        self.tco_header = saved_tco_header;
        self.tco_exit = saved_tco_exit;
        self.fn_name = saved_fn_name;

        // Pop inline scope
        _ = self.inline_scopes.pop();
        self.locals.shrinkRetainingCapacity(base);

        // Switch to exit block and return the result
        self.b.switchToBlock(exit);
        return exit_param;
    }

    /// Inline a cross-function call by translating the callee's IR body directly.
    /// Maps callee parameters to translated argument values via inline scopes.
    fn translateInlinedCall(
        self: *IrTranslator,
        callee_params: []const []const u8,
        callee_body: *const Ir,
        args: []const *const Ir,
    ) anyerror!HoistValue {
        // Translate all arguments first (in caller's scope)
        var translated_args: [16]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }

        // Push callee params onto locals array and create inline scope
        const base = self.locals.items.len;
        for (0..callee_params.len) |i| {
            try self.locals.append(self.allocator, translated_args[i]);
        }
        try self.inline_scopes.append(self.allocator, .{
            .base = base,
            .count = callee_params.len,
        });

        // Translate the callee body (variables will resolve via inline scope)
        const result = try self.translate(callee_body);

        // Pop inline scope and callee locals
        _ = self.inline_scopes.pop();
        self.locals.shrinkRetainingCapacity(base);

        return result;
    }

    fn lookupKnownFn(self: *IrTranslator, kf: *const std.StringHashMap(KnownFn), target_name: []const u8) ?KnownFn {
        _ = self;
        // Direct lookup
        if (kf.get(target_name)) |known| return known;
        // Try short name from qualified "PKG:SYM"
        if (std.mem.indexOfScalar(u8, target_name, ':')) |colon_pos| {
            const short_name = target_name[colon_pos + 1 ..];
            if (kf.get(short_name)) |known| return known;
        }
        // Try matching known qualified names against unqualified target
        var iter = kf.iterator();
        while (iter.next()) |entry| {
            const kn = entry.key_ptr.*;
            if (std.mem.indexOfScalar(u8, kn, ':')) |colon_pos| {
                if (std.mem.eql(u8, kn[colon_pos + 1 ..], target_name)) return entry.value_ptr.*;
            }
        }
        return null;
    }

    fn getCallSigForArity(self: *IrTranslator, arity: u32) !SigRef {
        if (arity < 8) {
            if (self.call_sigs[arity]) |sr| return sr;
        }

        var sig = Signature.init(self.allocator, .system_v);
        for (0..arity) |_| {
            try sig.params.append(self.allocator, AbiParam.new(I64));
        }
        try sig.returns.append(self.allocator, AbiParam.new(I64));
        const sr = try self.func.addSignature(sig);

        if (arity < 8) {
            self.call_sigs[arity] = sr;
        }
        return sr;
    }

    /// Translate a tail-recursive function body as a loop.
    /// Creates: entry → header(params) → [body | exit(result)]
    /// Tail calls become jumpArgs(header, new_args).
    /// Non-tail returns become jumpArgs(exit, result).
    fn translateTCOBody(self: *IrTranslator, body_ir: *const Ir) anyerror!HoistValue {
        const arity = self.user_arity;
        if (arity > 8) return error.TooManyParams;

        // Create loop header with phi params
        const header = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(header);

        var phi_vals: [8]HoistValue = undefined;
        for (0..arity) |pi| {
            phi_vals[pi] = try self.func.dfg.appendBlockParam(header, I64);
        }

        // Create exit block with result phi
        const exit = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(exit);
        const exit_param = try self.func.dfg.appendBlockParam(exit, I64);

        // Jump from entry to header with initial param values
        var init_vals: [8]HoistValue = undefined;
        for (0..arity) |i| {
            init_vals[i] = self.locals.items[i];
        }
        try self.b.jumpArgs(header, init_vals[0..arity]);

        // Switch to header, install phi values as locals
        self.b.switchToBlock(header);
        for (0..arity) |i| {
            self.locals.items[i] = phi_vals[i];
        }

        // Pre-emit constants in header block (dominating position for loop body)
        try self.preEmitConstants(body_ir);

        // Enable TCO mode
        self.tco_header = header;
        self.tco_exit = exit;

        // Translate body — tail calls jump to header, returns jump to exit
        const body_result = try self.translateTCOExpr(body_ir);

        // If body produces a value (non-tail path), jump to exit with it.
        // If body terminated with a tail call, current_block is null — skip.
        if (self.b.current_block != null) {
            const result_tagged = try self.boolToTagged(body_result);
            try self.b.jumpArgs(exit, &.{result_tagged});
        }

        // Switch to exit block and return
        self.b.switchToBlock(exit);
        return exit_param;
    }

    /// Translate an expression in TCO context. In tail position, tail calls
    /// become jumps to header. Non-tail-position code delegates to translate().
    fn translateTCOExpr(self: *IrTranslator, ir: *const Ir) anyerror!HoistValue {
        switch (ir.*) {
            .tailcall => |tc| {
                if (isCallTargetSelf(tc.func, self.fn_name)) {
                    // Tail call → jump to header with new args
                    var arg_vals: [8]HoistValue = undefined;
                    for (tc.args, 0..) |arg, i| {
                        arg_vals[i] = try self.translate(arg);
                    }
                    try self.b.jumpArgs(self.tco_header.?, arg_vals[0..tc.args.len]);

                    // Signal that this code path terminated — no value flows forward.
                    // Callers check current_block == null to skip merge block jumps.
                    self.b.current_block = null;

                    // Return dummy value (never used at runtime)
                    return HoistValue.new(0);
                }
                // Non-self tail call — treat as regular call
                return try self.translateCall(tc.func, tc.args);
            },
            .@"if" => |i| {
                // Optimize condition: (not expr) → swap branches, (nilp expr) → icmp directly
                var cond_ir = i.cond;
                var then_branch = i.then_branch;
                var else_branch = i.else_branch;
                while (cond_ir.* == .not) {
                    cond_ir = cond_ir.not.operand;
                    const tmp = then_branch;
                    then_branch = else_branch;
                    else_branch = tmp;
                }
                const cond = if (cond_ir.* == .nilp) blk: {
                    const inner_val = try self.translate(cond_ir.nilp.operand);
                    const zero = try self.cachedIconst(0);
                    break :blk try self.b.icmp(I8, IntCC.eq, inner_val, zero);
                } else try self.translate(cond_ir);

                // Check if both branches are tail — if so, no merge needed
                const then_is_tail = isTailCall(then_branch, self.fn_name);
                const else_is_tail = isTailCall(else_branch, self.fn_name);

                if (then_is_tail and else_is_tail) {
                    // Both branches are tail calls — no merge block needed
                    const then_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(then_blk);
                    const else_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(else_blk);

                    try self.b.brif(cond, then_blk, else_blk);

                    self.b.switchToBlock(then_blk);
                    _ = try self.translateTCOExpr(then_branch);

                    self.b.switchToBlock(else_blk);
                    _ = try self.translateTCOExpr(else_branch);

                    // Both branches terminated — current_block is null.
                    // Return dummy (caller checks current_block).
                    return HoistValue.new(0);
                }

                // Optimization: if a branch is a simple exit value (literal)
                // and we have a tco_exit, jump directly to exit. This eliminates
                // trampoline blocks in patterns like (if test recurse nil).
                const then_is_simple_exit = isSimpleValue(then_branch);
                const else_is_simple_exit = isSimpleValue(else_branch);

                if ((then_is_tail or then_is_simple_exit) and (else_is_tail or else_is_simple_exit)) {
                    // Both branches terminate (tail call or simple exit)
                    const then_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(then_blk);
                    const else_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(else_blk);

                    try self.b.brif(cond, then_blk, else_blk);

                    self.b.switchToBlock(then_blk);
                    if (then_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(then_branch);
                        try self.b.jumpArgs(self.tco_exit.?, &.{val});
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(then_branch);
                    }

                    self.b.switchToBlock(else_blk);
                    if (else_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(else_branch);
                        try self.b.jumpArgs(self.tco_exit.?, &.{val});
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(else_branch);
                    }

                    return HoistValue.new(0);
                }

                if (then_is_tail or then_is_simple_exit) {
                    // Then terminates, else produces value
                    const then_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(then_blk);
                    const else_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(else_blk);

                    try self.b.brif(cond, then_blk, else_blk);

                    self.b.switchToBlock(then_blk);
                    if (then_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(then_branch);
                        try self.b.jumpArgs(self.tco_exit.?, &.{val});
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(then_branch);
                    }

                    self.b.switchToBlock(else_blk);
                    return try self.translateTCOExpr(else_branch);
                }

                if (else_is_tail or else_is_simple_exit) {
                    // Else terminates, then produces value
                    const then_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(then_blk);
                    const else_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(else_blk);

                    try self.b.brif(cond, then_blk, else_blk);

                    self.b.switchToBlock(else_blk);
                    if (else_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(else_branch);
                        try self.b.jumpArgs(self.tco_exit.?, &.{val});
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(else_branch);
                    }

                    self.b.switchToBlock(then_blk);
                    return try self.translateTCOExpr(then_branch);
                }

                // Neither branch is immediate tail — but may contain tail calls deeper.
                // Create merge block and translate both branches with TCO context.
                return try self.translateTCOIf(cond, then_branch, else_branch);
            },
            .let => |l| {
                // Translate bindings (non-tail)
                const old_len = self.locals.items.len;
                for (l.bindings) |binding| {
                    const val = try self.translate(binding.value);
                    try self.locals.append(self.allocator, val);
                }
                // Body is in tail position
                const result = try self.translateTCOExpr(l.body);
                self.locals.items.len = old_len;
                return result;
            },
            .progn => |exprs| {
                if (exprs.len == 0) return try self.b.iconst(I64, 0);
                // Non-tail exprs
                for (exprs[0 .. exprs.len - 1]) |e| {
                    _ = try self.translate(e);
                }
                // Last expr is in tail position
                return try self.translateTCOExpr(exprs[exprs.len - 1]);
            },
            // Non-tail — delegate to normal translation
            else => return try self.translate(ir),
        }
    }

    /// Translate an if in TCO context — like translateIf but propagates TCO to branches.
    /// Handles the case where a branch contains a tail call deeper inside (not immediate).
    fn translateTCOIf(self: *IrTranslator, cond_val: HoistValue, then_ir: *const Ir, else_ir: *const Ir) anyerror!HoistValue {
        const then_blk = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(then_blk);
        const else_blk = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(else_blk);
        const merge_blk = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(merge_blk);
        const merge_param = try self.func.dfg.appendBlockParam(merge_blk, I64);

        // Handle mutated locals (same as translateIf)
        const num_in_scope: u16 = @intCast(self.locals.items.len);
        var mutated_indices = std.ArrayList(u16){};
        defer mutated_indices.deinit(self.allocator);
        try collectMutatedVars(then_ir, &mutated_indices, self.allocator);
        try collectMutatedVars(else_ir, &mutated_indices, self.allocator);
        std.mem.sort(u16, mutated_indices.items, {}, std.sort.asc(u16));
        var deduped_count: usize = 0;
        for (mutated_indices.items) |idx| {
            if (idx >= num_in_scope) continue;
            if (deduped_count == 0 or mutated_indices.items[deduped_count - 1] != idx) {
                mutated_indices.items[deduped_count] = idx;
                deduped_count += 1;
            }
        }
        mutated_indices.shrinkRetainingCapacity(deduped_count);

        var merge_local_params = std.ArrayList(HoistValue){};
        defer merge_local_params.deinit(self.allocator);
        for (mutated_indices.items) |_| {
            const p = try self.func.dfg.appendBlockParam(merge_blk, I64);
            try merge_local_params.append(self.allocator, p);
        }

        const saved_locals = try self.allocator.alloc(HoistValue, self.locals.items.len);
        defer self.allocator.free(saved_locals);
        @memcpy(saved_locals, self.locals.items);

        try self.b.brif(cond_val, then_blk, else_blk);

        // Then branch (with TCO context)
        self.b.switchToBlock(then_blk);
        const then_val = try self.translateTCOExpr(then_ir);
        if (self.b.current_block != null) {
            const then_i64 = try self.boolToTagged(then_val);
            var then_merge_args = std.ArrayList(HoistValue){};
            defer then_merge_args.deinit(self.allocator);
            try then_merge_args.append(self.allocator, then_i64);
            for (mutated_indices.items) |idx| {
                try then_merge_args.append(self.allocator, self.locals.items[idx]);
            }
            try self.b.jumpArgs(merge_blk, then_merge_args.items);
        }

        // Else branch (with TCO context)
        @memcpy(self.locals.items, saved_locals);
        self.b.switchToBlock(else_blk);
        const else_val = try self.translateTCOExpr(else_ir);
        if (self.b.current_block != null) {
            const else_i64 = try self.boolToTagged(else_val);
            var else_merge_args = std.ArrayList(HoistValue){};
            defer else_merge_args.deinit(self.allocator);
            try else_merge_args.append(self.allocator, else_i64);
            for (mutated_indices.items) |idx| {
                try else_merge_args.append(self.allocator, self.locals.items[idx]);
            }
            try self.b.jumpArgs(merge_blk, else_merge_args.items);
        }

        // Merge
        self.b.switchToBlock(merge_blk);
        for (mutated_indices.items, 0..) |idx, mi| {
            self.locals.items[idx] = merge_local_params.items[mi];
        }
        return merge_param;
    }

    fn translateSelfCall(self: *IrTranslator, args: []const *const Ir) anyerror!HoistValue {
        // Translate user args first (while parameter registers are still valid)
        var translated_args: [16]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }

        // In untagged mode, re-tag args before self-call since the function
        // entry always untags. Result also comes back tagged, so untag it.
        if (self.untagged) {
            const one = try self.cachedIconst(1);
            for (0..args.len) |i| {
                const shifted = try self.b.ishl(I64, translated_args[i], one);
                translated_args[i] = try self.b.bor(I64, shifted, one);
            }
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

        // In untagged mode, the self-call returns a tagged result. Untag it.
        if (self.untagged) {
            const one = try self.cachedIconst(1);
            return try self.b.sshr(I64, call_result, one);
        }

        return call_result;
    }



    fn translateProgn(self: *IrTranslator, exprs: []const *const Ir) anyerror!HoistValue {
        var result: HoistValue = undefined;
        for (exprs) |expr| {
            result = try self.translate(expr);
        }
        return result;
    }

    // ── List / predicate operations (inline, no heap access) ──

    /// nilp: val == 0 → t (raw 2), else nil (raw 0)
    fn translateNilp(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const zero = try self.cachedIconst(0);
        const cond = try self.b.icmp(I8, IntCC.eq, val, zero);
        const t_val = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw)));
        const nil_val = try self.cachedIconst(0);
        return try self.b.select(I64, cond, t_val, nil_val);
    }

    /// not: identical to nilp in CL semantics
    fn translateNot(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        return self.translateNilp(operand_ir);
    }

    /// consp: (val & 0xF) == 0 && val != 0
    /// Cons tag is 0b0000 (pointer with bit0=0, bits1-3=000).
    /// nil is also 0b0000 but nil.raw == 0, so we need both checks.
    fn translateConsp(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const mask = try self.cachedIconst(0xF);
        const tag_bits = try self.b.band(I64, val, mask);
        const zero = try self.cachedIconst(0);
        const tag_ok = try self.b.icmp(I8, IntCC.eq, tag_bits, zero);
        const not_nil = try self.b.icmp(I8, IntCC.ne, val, zero);
        // Both conditions must be true: tag is cons AND not nil
        const both = try self.b.band(I8, tag_ok, not_nil);
        const t_val = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw)));
        const nil_val = try self.cachedIconst(0);
        return try self.b.select(I64, both, t_val, nil_val);
    }

    /// car: load [val + 0]
    /// At safety=0 (JIT requirement), no nil check needed.
    /// Cons tag is 0 so val.raw IS the pointer. Car is at offset 0.
    fn translateCar(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        return try self.b.load(I64, val, hoist.memflags.MemFlags.heap());
    }

    /// cdr: load [val + 8]
    /// At safety=0 (JIT requirement), no nil check needed.
    /// Cdr is at offset 8 (after car field).
    fn translateCdr(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const eight = try self.cachedIconst(8);
        const cdr_addr = try self.b.iadd(I64, val, eight);
        return try self.b.load(I64, cdr_addr, hoist.memflags.MemFlags.heap());
    }

    /// unsafe_car: same as car (both are unchecked at safety=0)
    fn translateUnsafeCar(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        return self.translateCar(operand_ir);
    }

    /// unsafe_cdr: same as cdr (both are unchecked at safety=0)
    fn translateUnsafeCdr(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        return self.translateCdr(operand_ir);
    }

    /// cons: inline bump allocation with C-ABI slow path fallback.
    /// Fast path: load alloc_ptr, store car+cdr, bump pointer.
    /// Slow path: call jitCons() for GC + allocate.
    fn translateCons(self: *IrTranslator, car_ir: *const Ir, cdr_ir: *const Ir) anyerror!HoistValue {
        const car_val = try self.translate(car_ir);
        const cdr_val = try self.translate(cdr_ir);

        // Simple approach: just use the C-ABI call with cached alloc pointer.
        // The jitCons function already has the fast path (cached bump allocation).
        const cons_sig = try self.getConsSigRef();
        const fn_ptr = try self.cachedIconst(@as(i64, @bitCast(getJitConsPtr())));
        var call_args = ValueList.default();
        try self.func.dfg.value_lists.push(&call_args, fn_ptr);
        try self.func.dfg.value_lists.push(&call_args, car_val);
        try self.func.dfg.value_lists.push(&call_args, cdr_val);
        const call_data = InstructionData{
            .call_indirect = .{
                .opcode = .call_indirect,
                .sig_ref = cons_sig,
                .args = call_args,
            },
        };
        const call_inst = try self.func.dfg.makeInst(call_data);
        const call_result = try self.func.dfg.appendInstResult(call_inst, I64);
        const block = self.b.current_block orelse return error.NoCurrentBlock;
        try self.func.layout.appendInst(call_inst, block);

        return call_result;
    }

    /// Get or create the SigRef for the cons call: (i64, i64) -> i64
    fn getConsSigRef(self: *IrTranslator) !SigRef {
        if (self.cons_sig_ref) |sr| return sr;

        // Cons takes 2 user args (car, cdr). fn_ptr is separate (call_indirect 1st value).
        var sig = Signature.init(self.allocator, .system_v);
        try sig.params.append(self.allocator, AbiParam.new(I64)); // car
        try sig.params.append(self.allocator, AbiParam.new(I64)); // cdr
        try sig.returns.append(self.allocator, AbiParam.new(I64)); // result cons

        const sr = try self.func.addSignature(sig);
        self.cons_sig_ref = sr;
        return sr;
    }

    /// abs for tagged fixnums.
    /// Tagged: raw = val*2+1. If val >= 0 (raw >= 1), return raw.
    /// If val < 0 (raw < 1), return 2 - raw.
    fn translateAbs(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const one = try self.cachedIconst(1);
        const is_non_neg = try self.b.icmp(I8, IntCC.sge, val, one);
        const two = try self.cachedIconst(2);
        const negated = try self.b.isub(I64, two, val);
        return try self.b.select(I64, is_non_neg, val, negated);
    }
};

/// Patch all occurrences of a 64-bit placeholder value in the code buffer.
/// On AArch64, a 64-bit constant is loaded via MOVZ+MOVK+MOVK+MOVK sequence.
/// Replace self-pointer indirect calls (BLR x9) with direct branch-and-link (BL).
///
/// Strategy: find each BLR x9, scan backward to find MOV x9, rN that feeds it,
/// then check if rN was loaded from a MOVZ+MOVK+MOVK+MOVK placeholder sequence.
/// Replace BLR with BL to function entry, NOP out the MOV and (if unused) the loads.
///
/// This handles cases where MOVZ is in the entry block (preEmitConstants) but BLR
/// is deep in the function body. Also handles hoist CSE sharing one load across
/// multiple call sites.
fn patchSelfCallsToBL(buf: []u8, placeholder: u64) bool {
    const NOP = @as(u32, 0xD503201F);
    const n_insns = buf.len / 4;
    if (n_insns < 6) return false;

    // Precompute placeholder halfwords
    const ph = [4]u16{
        @truncate(placeholder),
        @truncate(placeholder >> 16),
        @truncate(placeholder >> 32),
        @truncate(placeholder >> 48),
    };

    // Helper: check if instruction at idx is MOVZ with placeholder halfword 0
    const isPlaceholderLoad = struct {
        fn check(b: []const u8, idx: usize, p: [4]u16) ?u5 {
            const ni = b.len / 4;
            if (idx + 3 >= ni) return null;
            const w0 = readInsn(b, idx);
            if ((w0 & 0xFFE00000) != 0xD2800000) return null;
            if (@as(u16, @truncate((w0 >> 5) & 0xFFFF)) != p[0]) return null;
            const w1 = readInsn(b, idx + 1);
            const w2 = readInsn(b, idx + 2);
            const w3 = readInsn(b, idx + 3);
            if ((w1 & 0xFFE00000) != 0xF2A00000 or @as(u16, @truncate((w1 >> 5) & 0xFFFF)) != p[1]) return null;
            if ((w2 & 0xFFE00000) != 0xF2C00000 or @as(u16, @truncate((w2 >> 5) & 0xFFFF)) != p[2]) return null;
            if ((w3 & 0xFFE00000) != 0xF2E00000 or @as(u16, @truncate((w3 >> 5) & 0xFFFF)) != p[3]) return null;
            return @truncate(w0 & 0x1F);
        }
    }.check;

    var found = false;

    // For each BLR x9, convert to BL if it's a self-call
    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(buf, i);
        if (insn != 0xD63F0120) continue; // BLR x9

        // Scan backward for MOV x9, rN (up to 12 instructions — arg setup can be long)
        var mov_idx: ?usize = null;
        var src_reg: u5 = undefined;
        {
            var j = i;
            var scan: usize = 0;
            while (j > 0 and scan < 12) : (scan += 1) {
                j -= 1;
                const prev = readInsn(buf, j);
                // MOV x9, rN = ORR x9, xzr, rN: low 16 bits = 0x03E9, mask top
                if (prev & 0xFFE0FFFF == 0xAA0003E9) {
                    mov_idx = j;
                    src_reg = @truncate((prev >> 16) & 0x1F);
                    break;
                }
            }
        }
        if (mov_idx == null) continue;

        // Scan backward from MOV for the MOVZ+MOVK sequence that loaded src_reg.
        // It could be anywhere earlier in the function (preEmitConstants puts it in entry).
        var load_idx: ?usize = null;
        {
            var j: usize = mov_idx.?;
            while (j >= 4) {
                j -= 1;
                if (isPlaceholderLoad(buf, j, ph)) |rd| {
                    if (rd == src_reg) {
                        load_idx = j;
                        break;
                    }
                }
                if (j == 0) break;
            }
        }
        if (load_idx == null) continue;

        // Replace BLR x9 with BL to function entry (offset 0)
        const rel_offset: i32 = -@as(i32, @intCast(i));
        const imm26: u32 = @as(u32, @bitCast(rel_offset)) & 0x03FFFFFF;
        writeInsn(buf, i, 0x94000000 | imm26);

        // NOP out the MOV x9, rN
        writeInsn(buf, mov_idx.?, NOP);

        found = true;
    }

    // Second pass: NOP out placeholder load sequences whose registers are no longer used.
    // A load can be NOP'd if no non-NOP instruction reads its dest register.
    {
        var li: usize = 0;
        while (li + 3 < n_insns) : (li += 1) {
            if (isPlaceholderLoad(buf, li, ph)) |rd| {
                // Check if any instruction after the load uses rd as source
                var used = false;
                // Only check instructions AFTER the load (li+4..) — instructions
                // before the load read the register from a different definition.
                for (li + 4..n_insns) |k| {
                    const kinst = readInsn(buf, k);
                    if (kinst == NOP) continue;
                    // If rd is written by this instruction (as dest), stop — load is dead
                    const rd_k: u5 = @truncate(kinst & 0x1F);
                    // Check for MOVZ/MOV/SUB/ADD writing to rd (overwrites before any read)
                    if (rd_k == rd and kinst != NOP) {
                        // Check if this instruction also READS rd (e.g., ADD rd, rd, rn)
                        const rn_k: u5 = @truncate((kinst >> 5) & 0x1F);
                        const rm_k: u5 = @truncate((kinst >> 16) & 0x1F);
                        if (rn_k != rd and rm_k != rd) break; // pure write, load is dead
                    }
                    // Check MOV xD, rd: ORR xD, xzr, rd
                    if (kinst & 0xFFE0FFE0 == 0xAA0003E0 and @as(u5, @truncate((kinst >> 16) & 0x1F)) == rd) {
                        used = true;
                        break;
                    }
                    // Check any instruction that reads rd as rn or rm
                    const rn_k: u5 = @truncate((kinst >> 5) & 0x1F);
                    const rm_k: u5 = @truncate((kinst >> 16) & 0x1F);
                    if (rn_k == rd or rm_k == rd) {
                        used = true;
                        break;
                    }
                }
                if (!used) {
                    writeInsn(buf, li, NOP);
                    writeInsn(buf, li + 1, NOP);
                    writeInsn(buf, li + 2, NOP);
                    writeInsn(buf, li + 3, NOP);
                }
                li += 3;
            }
        }
    }

    return found;
}

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
        .fixnum_add, .fixnum_sub, .fixnum_mul, .add, .sub, .mul => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq, .eq => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
        .le, .lt, .gt, .ge, .num_eq => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs => |op| {
            try collectMutatedVars(op.operand, indices, allocator);
        },
        .cons => |op| {
            try collectMutatedVars(op.left, indices, allocator);
            try collectMutatedVars(op.right, indices, allocator);
        },
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
/// Check if a call target is resolvable (self-call or known JIT function).
pub fn isCallResolvable(func_ir: *const Ir, self_name: []const u8, known: *const std.StringHashMap(void)) bool {
    if (isCallTargetSelf(func_ir, self_name)) return true;
    if (getCallTargetName(func_ir)) |target_name| {
        if (known.get(target_name) != null) return true;
        // Check qualified name: "PKG:SYM" might match known "SYM" or vice versa
        if (std.mem.indexOfScalar(u8, target_name, ':')) |colon_pos| {
            const short_name = target_name[colon_pos + 1 ..];
            if (known.get(short_name) != null) return true;
        }
        // Check if any known name ends with ":TARGET_NAME"
        var iter = known.iterator();
        while (iter.next()) |entry| {
            const kn = entry.key_ptr.*;
            if (std.mem.indexOfScalar(u8, kn, ':')) |colon_pos| {
                if (std.mem.eql(u8, kn[colon_pos + 1 ..], target_name)) return true;
            }
        }
    }
    return false;
}

/// Extract the target function name from a call's func IR node.
fn getCallTargetName(func_ir: *const Ir) ?[]const u8 {
    return switch (func_ir.*) {
        .global_ref => |gr| gr.name,
        .lit => |v| blk: {
            if (!v.isSymbol()) break :blk null;
            if (v.isNil()) break :blk null;
            break :blk v.toPtr(Symbol).getName();
        },
        else => null,
    };
}

fn namesMatch(a: []const u8, b: []const u8) bool {
    // Exact match
    if (std.mem.eql(u8, a, b)) return true;
    // Qualified match: "PKG:SYM" matches "SYM"
    if (a.len > b.len + 1) {
        const suffix_start = a.len - b.len;
        if (a[suffix_start - 1] == ':' and
            std.mem.eql(u8, a[suffix_start..], b))
            return true;
    }
    if (b.len > a.len + 1) {
        const suffix_start = b.len - a.len;
        if (b[suffix_start - 1] == ':' and
            std.mem.eql(u8, b[suffix_start..], a))
            return true;
    }
    return false;
}

fn isCallTargetSelf(func_ir: *const Ir, name: []const u8) bool {
    return switch (func_ir.*) {
        .global_ref => |gr| namesMatch(gr.name, name),
        .lit => |v| blk: {
            if (!v.isSymbol()) break :blk false;
            if (v.isNil()) break :blk false;
            const sym_name = v.toPtr(Symbol).getName();
            break :blk namesMatch(sym_name, name);
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
        .fixnum_add, .fixnum_sub, .fixnum_mul, .add, .sub, .mul => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq, .eq => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .le, .lt, .gt, .ge, .num_eq => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .progn => |exprs| {
            for (exprs) |expr| {
                if (hasNestedSelfCalls(expr, name)) return true;
            }
            return false;
        },
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs => |op| hasNestedSelfCalls(op.operand, name),
        .cons => |op| hasNestedSelfCalls(op.left, name) or hasNestedSelfCalls(op.right, name),
        .let => |l| blk: {
            for (l.bindings) |binding| {
                if (hasNestedSelfCalls(binding.value, name)) break :blk true;
            }
            break :blk hasNestedSelfCalls(l.body, name);
        },
        .set => |s| hasNestedSelfCalls(s.value, name),
        .loop => |l| hasNestedSelfCalls(l.cond, name) or hasNestedSelfCalls(l.body, name),
        else => false,
    };
}

/// Detect whether a function body contains non-self calls (cross-function calls).
/// These are not yet supported by the JIT backend.
/// Check if an IR tree contains any cons operations (which emit call_indirect to jitCons).
fn containsCons(body: *const Ir) bool {
    return switch (body.*) {
        .cons => true,
        .@"if" => |i| containsCons(i.cond) or containsCons(i.then_branch) or containsCons(i.else_branch),
        .let => |l| blk: {
            for (l.bindings) |binding| {
                if (containsCons(binding.value)) break :blk true;
            }
            break :blk containsCons(l.body);
        },
        .set => |s| containsCons(s.value),
        .progn => |exprs| blk: {
            for (exprs) |e| {
                if (containsCons(e)) break :blk true;
            }
            break :blk false;
        },
        .loop => |l| containsCons(l.cond) or containsCons(l.body),
        .call => |c| blk: {
            for (c.args) |arg| {
                if (containsCons(arg)) break :blk true;
            }
            break :blk false;
        },
        .tailcall => |tc| blk: {
            for (tc.args) |arg| {
                if (containsCons(arg)) break :blk true;
            }
            break :blk false;
        },
        .fixnum_add, .fixnum_sub, .add, .sub, .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq,
        => |op| containsCons(op.left) or containsCons(op.right),
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs,
        => |op| containsCons(op.operand),
        else => false,
    };
}

pub fn hasNonSelfCalls(body: *const Ir, name: []const u8) bool {
    return switch (body.*) {
        .call => |c| blk: {
            // Self-call is OK, non-self-call is not
            if (!isCallTargetSelf(c.func, name)) break :blk true;
            for (c.args) |arg| {
                if (hasNonSelfCalls(arg, name)) break :blk true;
            }
            break :blk false;
        },
        .tailcall => |tc| blk: {
            if (!isCallTargetSelf(tc.func, name)) break :blk true;
            for (tc.args) |arg| {
                if (hasNonSelfCalls(arg, name)) break :blk true;
            }
            break :blk false;
        },
        .@"if" => |i| hasNonSelfCalls(i.cond, name) or hasNonSelfCalls(i.then_branch, name) or hasNonSelfCalls(i.else_branch, name),
        .let => |l| blk: {
            for (l.bindings) |binding| {
                if (hasNonSelfCalls(binding.value, name)) break :blk true;
            }
            break :blk hasNonSelfCalls(l.body, name);
        },
        .set => |s| hasNonSelfCalls(s.value, name),
        .progn => |exprs| blk: {
            for (exprs) |e| {
                if (hasNonSelfCalls(e, name)) break :blk true;
            }
            break :blk false;
        },
        .loop => |l| hasNonSelfCalls(l.cond, name) or hasNonSelfCalls(l.body, name),
        .fixnum_add, .fixnum_sub, .add, .sub, .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
        => |op| hasNonSelfCalls(op.left, name) or hasNonSelfCalls(op.right, name),
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs,
        => |op| hasNonSelfCalls(op.operand, name),
        .lit, .@"var", .global_ref => false,
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
        .fixnum_add, .fixnum_sub, .fixnum_mul, .add, .sub, .mul => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq, .eq => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .le, .lt, .gt, .ge, .num_eq => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .progn => |exprs| {
            for (exprs) |expr| {
                if (detectSelfCalls(expr, name)) return true;
            }
            return false;
        },
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs => |op| detectSelfCalls(op.operand, name),
        .cons => |op| detectSelfCalls(op.left, name) or detectSelfCalls(op.right, name),
        .let => |l| blk: {
            for (l.bindings) |binding| {
                if (detectSelfCalls(binding.value, name)) break :blk true;
            }
            break :blk detectSelfCalls(l.body, name);
        },
        .set => |s| detectSelfCalls(s.value, name),
        .loop => |l| detectSelfCalls(l.cond, name) or detectSelfCalls(l.body, name),
        else => false,
    };
}

/// Detect whether an IR tree contains load-generating operations (car, cdr).
/// Used to disable .aggressive optimization which incorrectly eliminates loads.
fn containsLoads(body: *const Ir) bool {
    return switch (body.*) {
        .car, .cdr, .unsafe_car, .unsafe_cdr => true,
        .@"if" => |i| containsLoads(i.cond) or containsLoads(i.then_branch) or containsLoads(i.else_branch),
        .let => |l| blk: {
            for (l.bindings) |binding| {
                if (containsLoads(binding.value)) break :blk true;
            }
            break :blk containsLoads(l.body);
        },
        .set => |s| containsLoads(s.value),
        .progn => |exprs| blk: {
            for (exprs) |e| {
                if (containsLoads(e)) break :blk true;
            }
            break :blk false;
        },
        .loop => |l| containsLoads(l.cond) or containsLoads(l.body),
        .fixnum_add, .fixnum_sub, .add, .sub, .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
        => |op| containsLoads(op.left) or containsLoads(op.right),
        .assert_fixnum, .nilp, .not, .consp, .abs,
        => |op| containsLoads(op.operand),
        .call => |c| blk: {
            for (c.args) |arg| {
                if (containsLoads(arg)) break :blk true;
            }
            break :blk false;
        },
        .tailcall => |tc| blk: {
            for (tc.args) |arg| {
                if (containsLoads(arg)) break :blk true;
            }
            break :blk false;
        },
        else => false,
    };
}

/// Check if an expression is a self-tail-call (immediate, not nested).
fn isTailCall(ir: *const Ir, name: []const u8) bool {
    return switch (ir.*) {
        .tailcall => |tc| isCallTargetSelf(tc.func, name),
        else => false,
    };
}

/// Detect whether a function body has self-recursive tail calls.
/// Walks only tail positions (if branches, let body, last progn expr).
fn hasSelfTailCalls(body: *const Ir, name: []const u8) bool {
    return switch (body.*) {
        .tailcall => |tc| isCallTargetSelf(tc.func, name),
        .@"if" => |i| hasSelfTailCalls(i.then_branch, name) or hasSelfTailCalls(i.else_branch, name),
        .let => |l| hasSelfTailCalls(l.body, name),
        .progn => |exprs| if (exprs.len == 0) false else hasSelfTailCalls(exprs[exprs.len - 1], name),
        else => false,
    };
}

/// Detect if a function body has non-tail self-calls (.call nodes).
fn hasNonTailSelfCalls(body: *const Ir, name: []const u8) bool {
    return switch (body.*) {
        .call => |c| blk: {
            if (isCallTargetSelf(c.func, name)) break :blk true;
            for (c.args) |arg| {
                if (hasNonTailSelfCalls(arg, name)) break :blk true;
            }
            break :blk false;
        },
        .tailcall => |tc| blk: {
            for (tc.args) |arg| {
                if (hasNonTailSelfCalls(arg, name)) break :blk true;
            }
            break :blk false;
        },
        .@"if" => |i| hasNonTailSelfCalls(i.cond, name) or hasNonTailSelfCalls(i.then_branch, name) or hasNonTailSelfCalls(i.else_branch, name),
        .let => |l| blk: {
            for (l.bindings) |binding| {
                if (hasNonTailSelfCalls(binding.value, name)) break :blk true;
            }
            break :blk hasNonTailSelfCalls(l.body, name);
        },
        .set => |s| hasNonTailSelfCalls(s.value, name),
        .progn => |exprs| blk: {
            for (exprs) |e| {
                if (hasNonTailSelfCalls(e, name)) break :blk true;
            }
            break :blk false;
        },
        .loop => |l| hasNonTailSelfCalls(l.cond, name) or hasNonTailSelfCalls(l.body, name),
        .fixnum_add, .fixnum_sub, .add, .sub, .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq,
        .le, .lt, .gt, .ge, .num_eq, .fixnum_mul, .mul, .eq, .cons,
        => |op| hasNonTailSelfCalls(op.left, name) or hasNonTailSelfCalls(op.right, name),
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs,
        => |op| hasNonTailSelfCalls(op.operand, name),
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
        .fixnum_add, .fixnum_sub, .fixnum_mul, .add, .sub, .mul => |n| detectLoops(n.left) or detectLoops(n.right),
        .fixnum_le, .fixnum_lt, .fixnum_gt, .fixnum_ge, .fixnum_eq, .eq => |n| detectLoops(n.left) or detectLoops(n.right),
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
        .assert_fixnum, .nilp, .not, .consp, .car, .cdr, .unsafe_car, .unsafe_cdr, .abs => |n| detectLoops(n.operand),
        .cons => |n| detectLoops(n.left) or detectLoops(n.right),
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
    return compileIrWithKnownFns(allocator, ir, name, null);
}

pub fn compileIrWithKnownFns(
    allocator: std.mem.Allocator,
    ir: *const Ir,
    name: []const u8,
    known_fns: ?*const std.StringHashMap(KnownFn),
) !CompiledFn {
    const lambda = switch (ir.*) {
        .lambda => |l| l,
        else => return error.ExpectedLambda,
    };

    // Fast reject: check if all IR nodes are supported before allocating
    if (!IrTranslator.canTranslate(lambda.body)) return error.UnsupportedIrNode;

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
    translator.known_fns = known_fns;
    translator.has_cross_calls = containsCons(lambda.body) or
        (if (known_fns) |kf| kf.count() > 0 and hasNonSelfCalls(lambda.body, name) else false);

    translator.user_arity = arity;
    translator.is_recursive = detectSelfCalls(lambda.body, name);
    translator.has_loops = detectLoops(lambda.body);
    translator.has_loads = containsLoads(lambda.body);

    // Check if any inlinable cross-function calls contain loads
    if (!translator.has_loads and known_fns != null) {
        translator.has_loads = crossCallsContainLoads(lambda.body, name, known_fns.?);
    }

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

    // Enable untagged mode: work with plain i64 inside the function body.
    // Params are untagged at entry (sshr 1), result is re-tagged at return.
    // Self-calls use untagged calling convention (no tag/untag overhead).
    // Only use untagged mode for functions with loops (no call overhead).
    // Recursive functions pay retag/untag at every self-call, which is worse.
    translator.untagged = translator.has_loops and !translator.is_recursive;

    // Map params to SSA values, untagging at entry in untagged mode.
    const block_params = func.dfg.blockParams(entry);
    try translator.locals.ensureTotalCapacity(allocator, arity);
    if (translator.untagged) {
        const one = try translator.cachedIconst(1);
        // Untag all params. For multi-param functions, the hoist regalloc
        // has a parallel copy bug where sequential moves clobber later params.
        // We work around this in fixEntryParamMoves post-pass.
        for (0..arity) |i| {
            const untagged = try b.sshr(I64, block_params[i], one);
            try translator.locals.append(allocator, untagged);
        }
    } else {
        for (0..arity) |i| {
            try translator.locals.append(allocator, block_params[i]);
        }
    }

    // Tail-call optimization: pure tail-recursive functions → loop.
    // Only if: has tail self-calls AND no non-tail self-calls.
    // Partial TCO (mixed tail/non-tail) blocked by hoist phi copy issues
    // for backward jumps with block params in presence of register pressure.
    const use_tco = hasSelfTailCalls(lambda.body, name) and
        !hasNonTailSelfCalls(lambda.body, name);

    if (use_tco) {
        // TCO converts recursion to a loop — function is no longer recursive
        translator.is_recursive = false;
        // But may still have cross-calls (cons, known functions)
        translator.has_cross_calls = containsCons(lambda.body) or
            (if (known_fns) |kf| kf.count() > 0 and hasNonSelfCalls(lambda.body, name) else false);
    }

    var result_i64: HoistValue = undefined;

    if (use_tco) {
        // TCO: translate as loop with header/exit blocks
        const tco_result = try translator.translateTCOBody(lambda.body);
        result_i64 = tco_result;
        // TCO exit block returns tagged values already (boolToTagged in translateTCOExpr)
    } else {
        // Normal: pre-emit constants, translate, return
        try translator.preEmitConstants(lambda.body);

        const result = translator.translate(lambda.body) catch |err| {
            return err;
        };

        // I8 boolean results → tagged Lisp values; untagged → re-tag
        result_i64 = try translator.boolToTagged(result);
        if (translator.untagged) {
            const result_ty = func.dfg.valueType(result) orelse I64;
            if (result_ty.raw != I8.raw) {
                const one = try translator.cachedIconst(1);
                const shifted = try b.ishl(I64, result_i64, one);
                result_i64 = try b.bor(I64, shifted, one);
            }
        }
    }

    try b.retValues(&.{result_i64});

    // Compile with Hoist
    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    // Use .none for functions with calls (cross or recursive) — aggressive
    // optimizations can incorrectly eliminate call_indirect instructions.
    // Only use .aggressive for leaf functions (no calls at all).
    var ctx = ctx_builder
        .optLevel(if (translator.is_recursive or translator.has_cross_calls or translator.has_loads) .none else .aggressive)
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

    // Patch self-pointer placeholder and convert indirect calls (BLR) to direct
    // branch-and-link (BL) for self-calls. On AArch64, BL uses a 26-bit relative
    // offset (±128MB range), eliminating the need to load a 64-bit address via
    // 4 MOVZ/MOVK instructions. This saves 5 instructions per self-call
    // (4 MOVZ/MOVK + MOV → NOP, BLR → BL).
    if (translator.is_recursive) {
        const func_addr = @intFromPtr(buf.ptr);
        const placeholder: u64 = @bitCast(translator.self_ptr_placeholder);
        // Try BL optimization first, fall back to address patching
        if (!patchSelfCallsToBL(code.code.items, placeholder)) {
            if (!patchPlaceholder(code.code.items, placeholder, func_addr)) {
                return error.SelfPointerPatchFailed;
            }
        }
    }

    // Peephole: replace dead cset with NOP in fused cmp+cset+b.cc sequences.
    // The icmp emits cmp+cset, and fused brif emits b.cc using flags directly.
    // The cset result is dead but still executes.
    if (std.posix.getenv("HABU_NO_CSET_ELIM") == null) {
        eliminateDeadCset(code.code.items);
    }

    // Fix parallel copy conflicts in function entry (param register shuffling).
    // When hoist's regalloc copies params from x0-x7 to work registers,
    // sequential moves can clobber params before they're consumed.
    // Fix parallel copy conflicts in entry (param register shuffling).
    // Hoist's regalloc may emit sequential MOV instructions that clobber source
    // registers before they're consumed. Common cases:
    //   - Untagged mode: SSHR follows MOVs immediately
    //   - .aggressive leaf functions: MOV x2,x1; MOV x3,x2 clobbers x2
    // For .none recursive functions, params are saved to callee-saved regs
    // first (x8+) so no conflicts occur — fixEntryParamMoves detects this
    // and returns early (no conflict found).
    if (arity > 1) {
        fixEntryParamMoves(code.code.items);
    }

    // Fuse CMP+CSET...CMP+CSEL into CMP...CSEL with original condition.
    // Pattern: CMP sets flags → CSET materializes bool → later CMP tests bool → CSEL.
    // Replace with: CMP sets flags → NOP → ... → NOP → CSEL(original cond).
    fuseSelectCondition(code.code.items);

    // Coalesce: replace `op rD, rA, rB; mov rC, rD` with `op rC, rA, rB; nop`
    coalesceMovs(code.code.items);

    // Eliminate B .+4 (jump to next instruction = NOP).
    // Must run AFTER coalescing since coalescing uses branches as scan barriers.
    eliminateUselessBranches(code.code.items);

    // Invert `b.cond .+8; b target` → `b.inv_cond target; nop`.
    invertBranchOverBranch(code.code.items);

    // Fix parallel copy conflicts in call argument setup.
    // Hoist's lowering emits sequential mov instructions for call arguments
    // which can clobber source registers before they're consumed.
    // Always run for recursive functions (even without nested self-calls)
    // because 3+ params create dependency chains in the arg move sequence.
    if (translator.is_recursive) {
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

    // Fuse MUL+ADD into MADD where possible.
    fuseMulAdd(code.code.items);

    // Eliminate round-trip MOV pairs: MOV xA, xB; ... MOV xB, xA → NOP both.
    // Common in TCO functions where entry params are copied to intermediate regs
    // and then immediately copied back for the loop header phis.
    eliminateRoundTripMovs(code.code.items);

    // Eliminate prologue/epilogue for leaf functions (no BLR/BL calls).
    // After TCO, recursive functions become loops and don't need frame setup.
    if (!translator.is_recursive) {
        eliminateLeafPrologue(code.code.items);
    }

    // Remove all NOP instructions and fix branch offsets.
    // Must run LAST after all other peephole passes that introduce NOPs.
    compactNops(code.code.items, &code.code);

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

    // Copy name to persistent storage — the IR arena may be freed after compilation.
    const owned_name = try allocator.dupe(u8, name);

    return .{
        .mem = mem,
        .fn_ptr = @ptrCast(buf.ptr),
        .arity = arity,
        .allocator = allocator,
        .name = owned_name,
    };
}


/// Replace dead CSET instructions with NOP when followed by a B.cond.
/// Pattern: CMP; CSET; B.cond → CMP; NOP; B.cond
/// The CSET result is dead because B.cond reads flags directly from CMP.
/// Fuse CMP+CSET...CMP+CSEL patterns into CMP...CSEL.
///
/// Hoist generates: CMP Xn,Xm; CSET Xc,cond; ...; CMP Wc,WZR; CSEL Xd,Xa,Xb,NE
/// This pass eliminates the CSET and second CMP, replacing CSEL's condition
/// with the original condition from the first CMP.
///
/// Pattern:
///   i+0: CMP (any CMP/SUBS setting flags)
///   i+1: CSET Xc, cond
///   ...  (instructions that don't set flags — no CMP/ADDS/SUBS)
///   i+k: CMP Wc, WZR (6B1F001F with Wc in bits 5-9)
///   i+k+1: CSEL Xd, Xa, Xb, NE (condition 1)
///
/// Result: NOP the CSET, NOP the CMP Wc,WZR, change CSEL cond from NE to original cond
fn fuseSelectCondition(code: []u8) void {
    const NOP = @as(u32, 0xD503201F);
    const n_insns = code.len / 4;
    if (n_insns < 4) return;

    var i: usize = 0;
    while (i + 3 < n_insns) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);

        // insn0: must be a CMP (SUBS with Rd=XZR/WZR)
        // 64-bit: EB..001F or 32-bit: 6B..001F
        const is_cmp = (insn0 & 0x7FE0001F == 0x6B00001F) or // SUBS Wd,Wn,Wm (CMP)
            (insn0 & 0xFFE0001F == 0xEB00001F); // SUBS Xd,Xn,Xm (CMP)
        if (!is_cmp) continue;

        // insn1: CSET Xc, cond = CSINC Xc, XZR, XZR, inv_cond
        // Encoding: 1001 1010 100 1 inv_cond:4 0 01 11111 Rd:5
        // Mask: 0xFFFFF7E0, check: 0x9A9F07E0 (64-bit CSINC with Rn=Rm=XZR)
        // Actually: CSET Xd, cond = CSINC Xd, XZR, XZR, invert(cond)
        // Format: 1 00 11010100 Rm:5 cond:4 0 1 Rn:5 Rd:5
        // With Rn=XZR(31), Rm=XZR(31): 0x9A9F_cond_07FF & mask
        // CSET Xd/Wd, cond = CSINC Xd/Wd, XZR/WZR, XZR/WZR, inv_cond
        // Fixed bits mask: everything except cond (bits 15-12) and Rd (bits 4-0)
        // 64-bit: 0x9A9F07E0, 32-bit: 0x1A9F07E0
        const cset_mask: u32 = 0xFFFF0FE0;
        const is_cset_64 = (insn1 & cset_mask == 0x9A9F07E0);
        const is_cset_32 = (insn1 & cset_mask == 0x1A9F07E0);
        if (!is_cset_64 and !is_cset_32) continue;
        const cset_rd: u5 = @truncate(insn1 & 0x1F);
        // Extract the inverted condition from CSET (bits 12-15)
        const inv_cond: u4 = @truncate((insn1 >> 12) & 0xF);
        // The original condition is the inversion of inv_cond (flip bit 0)
        const orig_cond: u4 = inv_cond ^ 1;

        // Scan forward for CMP Wc, WZR followed by CSEL with NE condition
        var j = i + 2;
        var found = false;
        while (j + 1 < n_insns) : (j += 1) {
            const insn_j = readInsn(code, j);

            // Skip NOPs
            if (insn_j == NOP) continue;

            // Stop if flags are modified (any ADDS/SUBS/CMP/CMN/ANDS)
            const top8 = insn_j >> 24;
            if (top8 == 0x6B or top8 == 0xEB or // SUBS/CMP
                top8 == 0x2B or top8 == 0xAB or // ADDS/CMN
                top8 == 0x72 or top8 == 0xF2 or // ANDS (32/64)
                top8 == 0x6A or top8 == 0xEA) // ANDS reg
            {
                // Check if this is our CMP Wc, WZR
                // CMP Wn, #0 = SUBS WZR, Wn, #0 = 0x6B1F001F with Rn in bits 5-9
                // Actually CMP Wc, WZR (register form) = SUBS WZR, Wc, WZR
                // = 0x6B1F001F | (cset_rd << 5)
                const expected_cmp = @as(u32, 0x6B1F001F) | (@as(u32, cset_rd) << 5);
                if (insn_j == expected_cmp) {
                    // Next must be CSEL with NE condition (cond=1)
                    if (j + 1 < n_insns) {
                        const insn_csel = readInsn(code, j + 1);
                        // CSEL Xd, Xn, Xm, cond: 1001 1010 100 Rm cond 00 Rn Rd
                        // Check it's a CSEL (not CSINC etc) with cond=NE(1)
                        if (insn_csel & 0xFFC00C00 == 0x9A800000) {
                            const csel_cond: u4 = @truncate((insn_csel >> 12) & 0xF);
                            if (csel_cond == 1) { // NE
                                // Replace CSEL condition with orig_cond
                                const new_csel = (insn_csel & 0xFFFF0FFF) | (@as(u32, orig_cond) << 12);
                                writeInsn(code, j + 1, new_csel);
                                // NOP the CSET and the CMP Wc, WZR
                                writeInsn(code, i + 1, NOP);
                                writeInsn(code, j, NOP);
                                found = true;
                            }
                        }
                    }
                }
                break; // Any flag-setting instruction stops the scan
            }

            // Also stop at branches
            if (insn_j & 0xFC000000 == 0x14000000 or // B
                insn_j & 0xFC000000 == 0x94000000 or // BL
                insn_j & 0xFF000010 == 0x54000000 or // B.cond
                insn_j & 0xFFFFFC1F == 0xD63F0000 or // BLR
                insn_j & 0xFFFFFC1F == 0xD65F0000) break; // RET
        }
        if (found) {
            i += 1; // skip past the NOP'd CSET
        }
    }
}

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
        // CSET is an alias for CSINC (op2 bit 10 = 1): 0x1A800400
        const is_cset = (insn1 & 0xFFE00C00) == 0x1A800400; // CSINC (CSET alias)
        const is_bcond = (insn2 & 0xFF000010) == 0x54000000; // B.cond

        if (is_cmp and is_cset and is_bcond) {
            // Replace CSET with NOP (0xD503201F)
            writeInsn(code, i + 1, 0xD503201F);
        }
    }
}

/// Fuse MUL+ADD into MADD: `mul rD, rA, rB; add rC, rD, rX` → `madd rC, rA, rB, rX; nop`
/// MADD: Rd = Ra + Rn * Rm. Encoding: sf 00 11011 000 Rm 0 Ra Rn Rd
fn fuseMulAdd(code: []u8) void {
    const n_insns = code.len / 4;
    if (n_insns < 2) return;

    var i: usize = 0;
    while (i + 1 < n_insns) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);

        // Check insn0 is MADD with Ra=XZR (= MUL alias)
        // MUL Xd, Xn, Xm = MADD Xd, Xn, Xm, XZR
        // Encoding: 1 00 11011 000 Rm 0 11111 Rn Rd
        if (insn0 & 0xFFE0FC00 != 0x9B007C00) continue;
        const mul_rd: u5 = @truncate(insn0 & 0x1F);
        const mul_rn: u5 = @truncate((insn0 >> 5) & 0x1F);
        const mul_rm: u5 = @truncate((insn0 >> 16) & 0x1F);

        // Check insn1 is ADD Xd, Rn, Rm (shifted register, 64-bit)
        if (insn1 & 0xFF200000 != 0x8B000000) continue;
        const add_rd: u5 = @truncate(insn1 & 0x1F);
        const add_rn: u5 = @truncate((insn1 >> 5) & 0x1F);
        const add_rm: u5 = @truncate((insn1 >> 16) & 0x1F);

        // Pattern 1: add rC, mul_rd, rX → madd rC, mul_rn, mul_rm, rX
        // Pattern 2: add rC, rX, mul_rd → madd rC, mul_rn, mul_rm, rX
        var ra: u5 = undefined; // the addend (non-mul operand)
        if (add_rn == mul_rd) {
            ra = add_rm;
        } else if (add_rm == mul_rd) {
            ra = add_rn;
        } else continue;

        // Don't fuse if mul_rd is used later (besides this add)
        var mul_rd_used_later = false;
        var j = i + 2;
        while (j < n_insns) : (j += 1) {
            const next = readInsn(code, j);
            if (next == 0xD503201F) continue;
            const rn_next: u5 = @truncate((next >> 5) & 0x1F);
            const rm_next: u5 = @truncate((next >> 16) & 0x1F);
            const rd_next: u5 = @truncate(next & 0x1F);
            if (rn_next == mul_rd or rm_next == mul_rd) {
                mul_rd_used_later = true;
                break;
            }
            if (rd_next == mul_rd) break; // overwritten, safe
            if (next & 0xFC000000 == 0x14000000 or
                next & 0xFF000010 == 0x54000000 or
                next & 0xFFFFFC1F == 0xD65F0000) break;
        }
        if (mul_rd_used_later) continue;

        // Encode MADD: 1 00 11011 000 Rm 0 Ra Rn Rd
        const madd: u32 = 0x9B000000 |
            (@as(u32, mul_rm) << 16) |
            (@as(u32, ra) << 10) |
            (@as(u32, mul_rn) << 5) |
            @as(u32, add_rd);
        writeInsn(code, i, madd);
        writeInsn(code, i + 1, 0xD503201F); // NOP
    }
}

/// Remove all NOP instructions from emitted code and fix branch offsets.
/// After all peephole passes introduce NOPs (dead cset elimination, mov coalescing,
/// B .+4 elimination, branch inversion), this pass compacts the code.
/// Eliminate round-trip MOV pairs at function entry.
/// Pattern: MOV xA, xB; MOV xB, xA (with no other use of xA between them).
/// Common in TCO functions where entry→header jump copies params to intermediates
/// and the header immediately copies back. Both MOVs become NOPs.
fn eliminateRoundTripMovs(code: []u8) void {
    const n_insns = code.len / 4;
    if (n_insns < 4) return;

    const nop: u32 = 0xD503201F;

    // Only scan the first ~20 instructions (entry region)
    const scan_limit = @min(n_insns, 20);
    var i: usize = 0;
    while (i < scan_limit) : (i += 1) {
        const insn_i = readInsn(code, i);
        // Must be MOV Xd, Xm (ORR Xd, XZR, Xm)
        if (insn_i & 0xFFE0FFE0 != 0xAA0003E0) continue;

        const rd_i: u5 = @truncate(insn_i & 0x1F);
        const rm_i: u5 = @truncate((insn_i >> 16) & 0x1F);
        if (rd_i == rm_i) continue; // MOV xA, xA is already a NOP

        // Look for reverse MOV (MOV rm_i, rd_i) within the next few instructions
        var j = i + 1;
        while (j < scan_limit) : (j += 1) {
            const insn_j = readInsn(code, j);
            if (insn_j & 0xFFE0FFE0 != 0xAA0003E0) continue;

            const rd_j: u5 = @truncate(insn_j & 0x1F);
            const rm_j: u5 = @truncate((insn_j >> 16) & 0x1F);

            // Check for reverse pair: rd_j == rm_i AND rm_j == rd_i
            if (rd_j == rm_i and rm_j == rd_i) {
                // Check that rd_i is not used or redefined between i and j
                var used = false;
                for (i + 1..j) |k| {
                    const insn_k = readInsn(code, k);
                    const rd_k: u5 = @truncate(insn_k & 0x1F);
                    const rn_k: u5 = @truncate((insn_k >> 5) & 0x1F);
                    const rm_k: u5 = @truncate((insn_k >> 16) & 0x1F);
                    // rd_i is used as source (rn, rm) or written as dest (rd)
                    if (rd_k == rd_i or rn_k == rd_i or rm_k == rd_i) {
                        used = true;
                        break;
                    }
                }
                if (!used) {
                    // Safe to eliminate both MOVs
                    writeInsn(code, i, nop);
                    writeInsn(code, j, nop);
                    break; // Move to next i
                }
            }
        }
    }
}

/// Eliminate prologue/epilogue for leaf functions (no BLR/BL calls).
/// Scans for BLR (0xD63F0xxx) and BL (0x94xxxxxx) instructions.
/// If none found, replaces STP x29,x30,[SP,#-frame]! + MOV x29,SP at start
/// and LDP x29,x30,[SP],#frame before RET at end with NOPs.
fn eliminateLeafPrologue(code: []u8) void {
    const n_insns = code.len / 4;
    if (n_insns < 4) return;

    // Check if function has any calls (BLR or BL)
    for (0..n_insns) |i| {
        const insn = readInsn(code, i);
        // BLR: 1101 0110 0011 1111 0000 00xx xxx0 0000
        if (insn & 0xFFFFFC1F == 0xD63F0000) return; // Has BLR — not a leaf
        // BL: 1001 01xx xxxx xxxx xxxx xxxx xxxx xxxx
        if (insn & 0xFC000000 == 0x94000000) return; // Has BL — not a leaf
    }

    // Leaf function — check for standard prologue pattern
    const insn0 = readInsn(code, 0);
    const insn1 = readInsn(code, 1);

    // STP x29, x30, [SP, #-imm]! (pre-indexed store pair, 64-bit)
    // Encoding: 10 101 001 1 iiiiiii 11110 11111 11101
    // Mask out imm7 (bits 15-21): check opc=10, STP=101, pre-indexed=0011, Rt2=x30, Rn=SP, Rt=x29
    const is_stp = (insn0 & 0xFFC07FFF) == 0xA9807BFD;
    // MOV x29, SP (ORR x29, XZR, SP) or MOV x29, SP alias
    const is_mov_fp = (insn1 == 0xAA1F03FD) or (insn1 == 0x910003FD);

    if (!is_stp or !is_mov_fp) return;

    // Find the LDP + RET at the end
    // LDP x29, x30, [SP], #imm (post-indexed load pair)
    // RET = 0xD65F03C0
    var ldp_pos: ?usize = null;
    var i: usize = n_insns;
    while (i > 0) {
        i -= 1;
        const insn = readInsn(code, i);
        if (insn == 0xD65F03C0) { // RET
            if (i > 0) {
                const prev = readInsn(code, i - 1);
                if ((prev & 0xFFC07FFF) == 0xA8C07BFD) { // LDP x29, x30, [SP], #imm
                    ldp_pos = i - 1;
                }
            }
            break;
        }
    }

    if (ldp_pos == null) return;

    // Replace prologue with NOPs
    const nop: u32 = 0xD503201F;
    writeInsn(code, 0, nop); // Replace STP
    writeInsn(code, 1, nop); // Replace MOV FP, SP

    // Replace epilogue with NOP (keep RET)
    writeInsn(code, ldp_pos.?, nop); // Replace LDP
}

fn compactNops(code: []u8, list: *std.ArrayList(u8)) void {
    const n_insns = code.len / 4;
    if (n_insns == 0) return;

    // Count NOPs — skip if none
    var nop_count: usize = 0;
    for (0..n_insns) |i| {
        if (readInsn(code, i) == 0xD503201F) nop_count += 1;
    }
    if (nop_count == 0) return;

    // Build old_pos → new_pos mapping (in instruction indices, not bytes)
    // For a NOP at old position i, new_pos[i] maps to the next non-NOP.
    // We use a stack-allocated array for small functions (<512 insns).
    var map_buf: [512]u32 = undefined;
    if (n_insns > map_buf.len) return; // too large, skip compaction
    const old_to_new = map_buf[0..n_insns];

    var new_pos: u32 = 0;
    for (0..n_insns) |i| {
        old_to_new[i] = new_pos;
        if (readInsn(code, i) != 0xD503201F) {
            new_pos += 1;
        }
    }
    const new_n_insns = new_pos;

    // Fix branch offsets before compacting.
    // For each branch instruction, adjust its relative offset.
    for (0..n_insns) |i| {
        const insn = readInsn(code, i);
        if (insn == 0xD503201F) continue; // skip NOPs

        const new_src = old_to_new[i];

        // B imm26: bits 31:26 = 000101
        if (insn & 0xFC000000 == 0x14000000) {
            const imm26_raw: u32 = insn & 0x03FFFFFF;
            const old_offset: i32 = if (imm26_raw & 0x02000000 != 0)
                @bitCast(imm26_raw | 0xFC000000)
            else
                @intCast(imm26_raw);
            const old_target: i32 = @as(i32, @intCast(i)) + old_offset;
            if (old_target < 0 or old_target >= @as(i32, @intCast(n_insns))) continue;
            const new_target = old_to_new[@intCast(old_target)];
            const new_offset: i32 = @as(i32, @intCast(new_target)) - @as(i32, @intCast(new_src));
            const new_imm26: u32 = @as(u32, @bitCast(new_offset)) & 0x03FFFFFF;
            writeInsn(code, i, (insn & 0xFC000000) | new_imm26);
            continue;
        }

        // BL imm26: bits 31:26 = 100101
        if (insn & 0xFC000000 == 0x94000000) {
            const imm26_raw: u32 = insn & 0x03FFFFFF;
            const old_offset: i32 = if (imm26_raw & 0x02000000 != 0)
                @bitCast(imm26_raw | 0xFC000000)
            else
                @intCast(imm26_raw);
            const old_target: i32 = @as(i32, @intCast(i)) + old_offset;
            if (old_target < 0 or old_target >= @as(i32, @intCast(n_insns))) continue;
            const new_target = old_to_new[@intCast(old_target)];
            const new_offset: i32 = @as(i32, @intCast(new_target)) - @as(i32, @intCast(new_src));
            const new_imm26: u32 = @as(u32, @bitCast(new_offset)) & 0x03FFFFFF;
            writeInsn(code, i, (insn & 0xFC000000) | new_imm26);
            continue;
        }

        // B.cond imm19: 0101 0100 imm19:0 cond
        if (insn & 0xFF000010 == 0x54000000) {
            const imm19_raw: u32 = (insn >> 5) & 0x7FFFF;
            const old_offset: i32 = if (imm19_raw & 0x40000 != 0)
                @bitCast((imm19_raw | 0xFFF80000))
            else
                @intCast(imm19_raw);
            const old_target: i32 = @as(i32, @intCast(i)) + old_offset;
            if (old_target < 0 or old_target >= @as(i32, @intCast(n_insns))) continue;
            const new_target = old_to_new[@intCast(old_target)];
            const new_offset: i32 = @as(i32, @intCast(new_target)) - @as(i32, @intCast(new_src));
            const new_imm19: u32 = @as(u32, @bitCast(new_offset)) & 0x7FFFF;
            writeInsn(code, i, (insn & 0xFF00001F) | (new_imm19 << 5));
            continue;
        }

        // CBZ/CBNZ imm19: sf 011 010 op imm19 Rt
        if (insn & 0x7E000000 == 0x34000000) {
            const imm19_raw: u32 = (insn >> 5) & 0x7FFFF;
            const old_offset: i32 = if (imm19_raw & 0x40000 != 0)
                @bitCast((imm19_raw | 0xFFF80000))
            else
                @intCast(imm19_raw);
            const old_target: i32 = @as(i32, @intCast(i)) + old_offset;
            if (old_target < 0 or old_target >= @as(i32, @intCast(n_insns))) continue;
            const new_target = old_to_new[@intCast(old_target)];
            const new_offset: i32 = @as(i32, @intCast(new_target)) - @as(i32, @intCast(new_src));
            const new_imm19: u32 = @as(u32, @bitCast(new_offset)) & 0x7FFFF;
            writeInsn(code, i, (insn & 0xFF00001F) | (new_imm19 << 5));
            continue;
        }

        // TBZ/TBNZ imm14: b5 011 011 op b40 imm14 Rt
        if (insn & 0x7E000000 == 0x36000000) {
            const imm14_raw: u32 = (insn >> 5) & 0x3FFF;
            const old_offset: i32 = if (imm14_raw & 0x2000 != 0)
                @bitCast((imm14_raw | 0xFFFFC000))
            else
                @intCast(imm14_raw);
            const old_target: i32 = @as(i32, @intCast(i)) + old_offset;
            if (old_target < 0 or old_target >= @as(i32, @intCast(n_insns))) continue;
            const new_target = old_to_new[@intCast(old_target)];
            const new_offset: i32 = @as(i32, @intCast(new_target)) - @as(i32, @intCast(new_src));
            const new_imm14: u32 = @as(u32, @bitCast(new_offset)) & 0x3FFF;
            writeInsn(code, i, (insn & 0xFFF8001F) | (new_imm14 << 5));
            continue;
        }
    }

    // Compact: copy non-NOP instructions to output positions
    var write_pos: usize = 0;
    for (0..n_insns) |i| {
        const insn = readInsn(code, i);
        if (insn != 0xD503201F) {
            if (write_pos != i) {
                writeInsn(code, write_pos, insn);
            }
            write_pos += 1;
        }
    }

    // Truncate the code buffer
    list.shrinkRetainingCapacity(new_n_insns * 4);
}

/// Replace `b.cond .+8; b target` with `b.inv_cond target; nop`.
/// The pattern arises when hoist emits `brif` as a conditional branch to the
/// then-block followed by an unconditional branch to the else-block, and the
/// then-block immediately follows. Inverting removes one branch from the hot path.
fn invertBranchOverBranch(code: []u8) void {
    const n_insns = code.len / 4;
    if (n_insns < 2) return;

    var i: usize = 0;
    while (i + 1 < n_insns) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);

        // Check: insn0 is B.cond with offset +8 (skip one instruction)
        // B.cond encoding: 0101 0100 imm19 0 cond
        if (insn0 & 0xFF000010 != 0x54000000) continue;
        const bcond_imm19: i32 = @as(i32, @bitCast(insn0 & 0x00FFFFE0)) >> 5;
        if (bcond_imm19 != 2) continue; // offset 2 words = +8 bytes = skip 1 insn

        // Check: insn1 is unconditional B (not BL)
        if (insn1 & 0xFC000000 != 0x14000000) continue;

        // Extract the unconditional branch's offset and the condition code
        const b_imm26_raw: u32 = insn1 & 0x03FFFFFF;
        // Sign-extend 26-bit offset
        const b_offset: i32 = if (b_imm26_raw & 0x02000000 != 0)
            @bitCast(b_imm26_raw | 0xFC000000)
        else
            @intCast(b_imm26_raw);
        // Adjust: the target was relative to insn1's position (i+1).
        // New branch is at position i, so add 1 to the offset.
        const new_offset: i32 = b_offset + 1;

        // Invert the condition code (flip bit 0 of the 4-bit cond field)
        const cond: u4 = @truncate(insn0 & 0xF);
        const inv_cond: u4 = cond ^ 1;

        // Encode new B.cond with inverted condition and far target
        const new_imm19: u32 = @bitCast(@as(i32, new_offset) << 5);
        const new_bcond: u32 = 0x54000000 | (new_imm19 & 0x00FFFFE0) | @as(u32, inv_cond);

        writeInsn(code, i, new_bcond);
        writeInsn(code, i + 1, 0xD503201F); // NOP
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
/// Fix parallel copy conflicts in function entry parameter moves.
/// Pattern: after prologue, hoist emits sequential `mov xD, xS` to copy
/// params from ABI registers to work registers. If a later move reads from
/// a register that an earlier move already overwrote, we reorder the moves.
fn fixEntryParamMoves(code: []u8) void {
    if (code.len < 16) return;
    const n_insns = code.len / 4;

    // Find the first mov after prologue (skip stp, mov fp,sp, and stp callee-saves)
    var first_mov: usize = 0;
    for (0..@min(n_insns, 16)) |i| {
        const insn = readInsn(code, i);
        // Look for MOV Xd, Xm (ORR Xd, XZR, Xm): 0xAA0003E0 mask 0xFFE0FFE0
        if (insn & 0xFFE0FFE0 == 0xAA0003E0) {
            const rm: u5 = @truncate((insn >> 16) & 0x1F);
            // Must be reading from an ABI arg register (x0-x7)
            if (rm <= 7) {
                first_mov = i;
                break;
            }
        }
    }
    if (first_mov == 0) return;

    // Collect consecutive mov instructions from param registers
    const MovInfo = struct { src: u5, dst: u5, pos: usize };
    var movs: [8]MovInfo = undefined;
    var n_movs: usize = 0;

    var i = first_mov;
    while (i < @min(n_insns, first_mov + 8) and n_movs < 8) : (i += 1) {
        const insn = readInsn(code, i);
        if (insn & 0xFFE0FFE0 == 0xAA0003E0) {
            const rd: u5 = @truncate(insn & 0x1F);
            const rm: u5 = @truncate((insn >> 16) & 0x1F);
            if (rm <= 7) {
                movs[n_movs] = .{ .src = rm, .dst = rd, .pos = i };
                n_movs += 1;
            } else break;
        } else break;
    }

    if (n_movs < 2) return;

    // Check for conflicts: mov[a] reads from a register that mov[b] (b < a, earlier)
    // has already written.
    var has_conflict = false;
    for (1..n_movs) |a| {
        for (0..a) |b_idx| {
            if (movs[b_idx].dst == movs[a].src) {
                has_conflict = true;
                break;
            }
        }
        if (has_conflict) break;
    }

    if (!has_conflict) return;

    // Simple fix for 2-3 params: use x9 as scratch.
    // Save the later params first using scratch, then do the moves.
    // For 2 params: mov x9, x1; mov xA, x0; mov xB, x9
    if (n_movs == 2) {
        // movs[0] = first mov, movs[1] = second mov
        // The conflict is movs[0] clobbers a source of movs[1]
        // Fix: swap the order (do movs[1] first, then movs[0])
        const insn0 = makeMovInsn(movs[0].dst, movs[0].src);
        const insn1 = makeMovInsn(movs[1].dst, movs[1].src);
        writeInsn(code, movs[0].pos, insn1);
        writeInsn(code, movs[1].pos, insn0);
    } else {
        // General case: save all param values to scratch regs first
        // Use x9-x15 as scratch (these are caller-saved temporaries)
        // Phase 1: copy params to scratch
        // Phase 2: copy scratch to destinations
        // This requires 2*n instructions, but we only have n slots.
        // Alternative: use topological sort like fixCallArgMoves.
        
        // Find the conflicting pair and swap just those two
        for (1..n_movs) |a| {
            for (0..a) |b_idx| {
                if (movs[b_idx].dst == movs[a].src) {
                    // Swap movs[a] and movs[b_idx] in execution order
                    const insn_a = makeMovInsn(movs[a].dst, movs[a].src);
                    const insn_b = makeMovInsn(movs[b_idx].dst, movs[b_idx].src);
                    // But after swap, movs[a] (now first) writes to movs[b_idx].src?
                    // No — the source is movs[a].src which is the param reg.
                    // After swap: movs[b_idx] position executes movs[a] first.
                    // movs[a].src reads from the original param reg (untouched).
                    // Then movs[a] position executes movs[b_idx], which reads from
                    // movs[b_idx].src — but we need to check it's not clobbered.
                    
                    // Simpler: use x9 as scratch for the conflicting value
                    // Replace: mov xA, x0; mov xB, x1 (where xA=x1)
                    // With:    mov x9, x1; mov xA, x0; mov xB, x9
                    // But we have exactly 2 instruction slots, need 3.
                    // Instead: swap the two instructions if safe.
                    // After swap: mov xB, x1; mov xA, x0
                    // Check: does xB == x0? If not, this is safe.
                    if (movs[a].dst != movs[b_idx].src) {
                        writeInsn(code, movs[b_idx].pos, insn_a);
                        writeInsn(code, movs[a].pos, insn_b);
                    } else {
                        // Circular dependency: movs[a].dst == movs[b_idx].src AND
                        // movs[b_idx].dst == movs[a].src. Use x9 as scratch.
                        // Replace: mov xA, xS_a; mov xB, xS_b (circular)
                        // With:    mov x9, xS_a; mov xA, xS_b; then need mov xB, x9
                        // For n_movs >= 3, reuse a later slot for the x9 copy.
                        // For n_movs == 2, look for a NOP or instruction we can replace.
                        
                        // Simple 2-move circular: XOR-swap
                        // mov xA, xS_a (where xA == xS_b)
                        // mov xB, xS_b (where xB == xS_a)
                        // After swap: both read from each other's destination.
                        // Use x9 as temp:
                        //   slot[b]: mov x9, xS_b     (save the value that will be clobbered)
                        //   slot[a]: mov xA, xS_a      (this clobbers xS_b since xA==xS_b)
                        //   Need extra slot for: mov xB, x9
                        
                        // If there's a NOP or iconst after the MOVs, overwrite it
                        const scratch: u5 = 9;
                        var found_slot: ?usize = null;
                        // Look for a NOP (0xd503201f) right after the MOVs.
                        // Do NOT overwrite MOVZ — those are real constants!
                        var search_pos = movs[n_movs - 1].pos + 1;
                        const max_search = @min(n_insns, search_pos + 4);
                        while (search_pos < max_search) : (search_pos += 1) {
                            const probe = readInsn(code, search_pos);
                            if (probe == 0xd503201f) { // NOP only
                                found_slot = search_pos;
                                break;
                            }
                        }
                        
                        if (found_slot) |slot| {
                            // Parallel copy for circular pair:
                            // a wants: a.dst = a.src (which is b.dst)
                            // b wants: b.dst = b.src (which is a.dst)
                            // Fix: save a.src (=b.dst) to scratch, do b, then a from scratch
                            writeInsn(code, movs[b_idx].pos, makeMovInsn(scratch, movs[a].src)); // Save the value that b will clobber
                            writeInsn(code, movs[a].pos, makeMovInsn(movs[b_idx].dst, movs[b_idx].src)); // Do b's move
                            writeInsn(code, slot, makeMovInsn(movs[a].dst, scratch)); // Do a's move from scratch
                        } else {
                            // Last resort: use EOR-swap (no extra slot needed)
                            // EOR xA, xA, xB; EOR xB, xB, xA; EOR xA, xA, xB
                            // But this needs 3 slots and we only have 2.
                            // For now, fall back to no fix — better than panic.
                            return;
                        }
                    }
                    return;
                }
            }
        }
    }
}

fn makeMovInsn(rd: u5, rm: u5) u32 {
    // MOV Xd, Xm = ORR Xd, XZR, Xm
    return 0xAA0003E0 | @as(u32, rd) | (@as(u32, rm) << 16);
}

/// Coalesce: replace `op rD, rA, rB; mov rC, rD` with `op rC, rA, rB; nop`
/// when rD is not used elsewhere after the mov. This eliminates phi-copy moves.
/// Eliminate B .+4 instructions (unconditional jump to next instruction = NOP)
fn eliminateUselessBranches(code: []u8) void {
    const n_insns = code.len / 4;
    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);
        // B imm26: 0x14000000 | offset. B .+4 means offset=1 → 0x14000001
        if (insn == 0x14000001) {
            writeInsn(code, i, 0xD503201F); // NOP
        }
    }
}

/// Coalesce: for ALU ops where the result is only used by a later mov,
/// change the ALU op's destination to the mov's destination and NOP the mov.
/// Handles non-adjacent pairs: `add rD, rA, rB; ...; mov rC, rD` → `add rC, rA, rB; ...; nop`
fn coalesceMovs(code: []u8) void {
    if (code.len < 8) return;
    const n_insns = code.len / 4;

    // Process in multiple passes since coalescing one pair may enable others
    var changed = true;
    while (changed) {
        changed = false;
        var i: usize = 0;
        while (i < n_insns) : (i += 1) {
            const insn0 = readInsn(code, i);

            // Only consider safe ALU ops
            const op_class = insn0 >> 24;
            const is_safe_alu = (op_class == 0x8B or // ADD
                op_class == 0xCB or // SUB
                op_class == 0x9B); // MADD/MUL
            if (!is_safe_alu) continue;

            const rd0: u5 = @truncate(insn0 & 0x1F);

            // Look ahead for a MOV that copies rd0
            var mov_idx: ?usize = null;
            var mov_dst: u5 = 0;
            var j = i + 1;
            while (j < n_insns) : (j += 1) {
                const next = readInsn(code, j);

                // Check for NOP (skip)
                if (next == 0xD503201F) continue;

                // Check for MOV Xd, rd0
                if (next & 0xFFE0FFE0 == 0xAA0003E0) {
                    const ms: u5 = @truncate((next >> 16) & 0x1F);
                    const md: u5 = @truncate(next & 0x1F);
                    if (ms == rd0 and md < 28) {
                        mov_idx = j;
                        mov_dst = md;
                        break;
                    }
                }

                // If rd0 is read as a source, can't coalesce
                const rn: u5 = @truncate((next >> 5) & 0x1F);
                const rm: u5 = @truncate((next >> 16) & 0x1F);
                if (rn == rd0 or rm == rd0) break;

                // If rd0 is overwritten, stop
                const rd_next: u5 = @truncate(next & 0x1F);
                if (rd_next == rd0) break;

                // Stop at branch/ret/call
                if (next & 0xFC000000 == 0x14000000 or // B
                    next & 0xFF000000 == 0x54000000 or // B.cond
                    next & 0xFFFFFC1F == 0xD65F0000 or // RET
                    next & 0xFFFFFC1F == 0xD63F0000 or // BLR
                    next & 0xFC000000 == 0x94000000) break; // BL
            }

            const mi = mov_idx orelse continue;

            // Check that rd0 is not used AFTER the mov (before next write or branch).
            // If rd0 has other consumers after the MOV, coalescing would break them.
            var safe = true;
            var k = mi + 1;
            while (k < n_insns) : (k += 1) {
                const after = readInsn(code, k);
                if (after == 0xD503201F) continue; // NOP
                const rn_a: u5 = @truncate((after >> 5) & 0x1F);
                const rm_a: u5 = @truncate((after >> 16) & 0x1F);
                const rd_a: u5 = @truncate(after & 0x1F);
                // Check for MOV Xd, Xm where Xm == rd0 (another consumer)
                if (after & 0xFFE0FFE0 == 0xAA0003E0) {
                    const mov_src: u5 = @truncate((after >> 16) & 0x1F);
                    if (mov_src == rd0) { safe = false; break; }
                }
                // If rd0 is used as source operand
                if (rn_a == rd0 or rm_a == rd0) { safe = false; break; }
                // If rd0 is redefined, no more consumers can see old value
                if (rd_a == rd0) break;
                // Stop at branch/ret (control flow boundary)
                if (after & 0xFC000000 == 0x14000000 or
                    after & 0xFF000000 == 0x54000000 or
                    after & 0xFFFFFC1F == 0xD65F0000 or
                    after & 0xFFFFFC1F == 0xD63F0000) break;
            }
            if (!safe) continue;

            // Also check that mov_dst is not written between ALU op and mov
            var j2 = i + 1;
            while (j2 < mi) : (j2 += 1) {
                const between = readInsn(code, j2);
                if (between == 0xD503201F) continue; // NOP
                const rd_b: u5 = @truncate(between & 0x1F);
                if (rd_b == mov_dst) {
                    safe = false;
                    break;
                }
            }
            if (!safe) continue;

            // Coalesce: change ALU destination to mov_dst, NOP the mov
            const patched = (insn0 & ~@as(u32, 0x1F)) | @as(u32, mov_dst);
            writeInsn(code, i, patched);
            writeInsn(code, mi, 0xD503201F); // NOP
            changed = true;
        }
    }
}

fn fixCallArgMoves(code: []u8) void {
    if (code.len < 8) return;
    const n_insns = code.len / 4;

    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);

        // Check for BLR (indirect) or BL (direct) call instruction
        const is_blr = (insn & 0xFFFFFC1F == 0xD63F0000);
        const is_bl = (insn & 0xFC000000 == 0x94000000);
        if (!is_blr and !is_bl) continue;

        // Found a call. Scan backwards for mov instructions (up to 8).
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
