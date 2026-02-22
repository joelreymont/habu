//! JIT Backend (ARM64 via Hoist SSA)
//!
//! Translates Habu compiler IR to Hoist SSA IR, then compiles to native code:
//!   IR (tree) → Hoist SSA → Optimize → Lower (ISLE) → RegAlloc → AArch64
//!
//! Supports recursive functions via call_indirect with self-pointer patching.
//! The placeholder 0x0BADF00DDEADBEEF is patched with the actual code address.

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
const StackSlot = hoist.entities.StackSlot;
const ExternalName = hoist.extfunc.ExternalName;
const StackLoadData = hoist.instruction_data.StackLoadData;
const MemFlags = hoist.memflags.MemFlags;
const createStackSlot = hoist.stackslots.createStackSlot;

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
pub const LiteralRoots = std.AutoHashMap(usize, *Value);

// ── Global heap pointer for JIT cons allocation ──
// Set by the VM before calling JIT functions that may allocate.
var g_heap: ?*Heap = null;
pub const CallBridge = struct {
    context: *anyopaque,
    call0: *const fn (*anyopaque, u64) callconv(.c) u64,
    call1: *const fn (*anyopaque, u64, u64) callconv(.c) u64,
    call2: *const fn (*anyopaque, u64, u64, u64) callconv(.c) u64,
    call3: *const fn (*anyopaque, u64, u64, u64, u64) callconv(.c) u64,
    call4: *const fn (*anyopaque, u64, u64, u64, u64, u64) callconv(.c) u64,
    call5: *const fn (*anyopaque, u64, u64, u64, u64, u64, u64) callconv(.c) u64,
    call6: *const fn (*anyopaque, u64, u64, u64, u64, u64, u64, u64) callconv(.c) u64,
    call7: *const fn (*anyopaque, u64, u64, u64, u64, u64, u64, u64, u64) callconv(.c) u64,
};
var g_call_bridge: ?CallBridge = null;
var g_bridge_err: bool = false;

/// Set the global heap pointer for JIT allocation.
pub fn setHeap(heap: *Heap) void {
    g_heap = heap;
    g_safepoint_batch_ops = 0;
    jitConsRefreshCache();
}

pub fn setCallBridge(bridge: CallBridge) void {
    g_call_bridge = bridge;
}

pub fn clearBridgeError() void {
    g_bridge_err = false;
}

pub fn markBridgeError() void {
    g_bridge_err = true;
}

pub fn bridgeErrorPending() bool {
    return g_bridge_err;
}

/// Sync heap.alloc_ptr from the JIT global g_alloc_ptr.
/// Must be called after JIT execution to keep heap state consistent
/// (inline cons updates g_alloc_ptr but not heap.alloc_ptr directly).
pub fn syncHeapFromGlobal(heap: *Heap) void {
    if (g_alloc_ptr != 0) {
        const heap_ptr = @intFromPtr(heap.alloc_ptr);
        if (g_alloc_ptr > heap_ptr) {
            heap.alloc_ptr = @ptrFromInt(g_alloc_ptr);
        } else {
            // Helper allocations can advance heap.alloc_ptr directly.
            // Never rewind the VM allocator to a stale inline-cons cursor.
            g_alloc_ptr = heap_ptr;
        }
    }
}

/// C-ABI cons function callable from JIT-compiled code.
/// Takes (car_raw: u64, cdr_raw: u64) → cons_raw: u64
/// Performs bump allocation without GC. Returns nil (0) on OOM.
/// Fast inline cons: bump-allocate from a pre-cached region.
/// Falls back to heap.allocCons on overflow.
var g_alloc_ptr: u64 = 0;
var g_alloc_end: u64 = 0;
var g_safepoint_batch_ops: usize = 0;
const JIT_SAFEPOINT_BATCH_OPS: usize = 8;

pub fn allocPtrRaw() u64 {
    return g_alloc_ptr;
}

fn jitConsRefreshCache() void {
    if (g_heap) |heap| {
        g_alloc_ptr = @intFromPtr(heap.alloc_ptr);
        g_alloc_end = @intFromPtr(heap.from_end);
    }
}

fn elapsedNsSince(start_ns: i128) u64 {
    const now_ns = std.time.nanoTimestamp();
    if (now_ns <= start_ns) return 0;
    return @intCast(now_ns - start_ns);
}

fn jitSafepointBeforeAlloc() void {
    if (g_heap) |heap| {
        g_safepoint_batch_ops +%= 1;
        const should_poll = heap.shouldCollectDebt() or
            g_safepoint_batch_ops >= JIT_SAFEPOINT_BATCH_OPS;
        if (!should_poll) return;
        g_safepoint_batch_ops = 0;

        const profile = heap.profileMutatorEnabled();
        const start_ns: i128 = if (profile) std.time.nanoTimestamp() else 0;

        // Keep alloc cache coherent before any slow-path allocator/GC entry.
        g_alloc_ptr = @intFromPtr(heap.alloc_ptr);
        g_alloc_end = @intFromPtr(heap.from_end);

        if (profile) {
            heap.noteSafepointJit(elapsedNsSince(start_ns));
        }
    }
}

fn jitWriteBarrier(owner_raw: u64, stored_raw: u64) void {
    const heap = g_heap orelse return;
    const stored = Value{ .raw = stored_raw };
    if (!stored.isPointer()) return;
    const profile = heap.profileMutatorEnabled();
    const start_ns: i128 = if (profile) std.time.nanoTimestamp() else 0;
    heap.writeBarrier(Value{ .raw = owner_raw }, stored);
    if (profile) {
        heap.noteJitWriteBarrier(elapsedNsSince(start_ns));
    }
}

fn jitResolveForwarded(val: Value) Value {
    if (!val.isPointer()) return val;
    const heap = g_heap orelse return val;

    var cur = val;
    var resolved_live: ?Value = null;
    var hops: u8 = 0;
    while (hops < 8 and cur.isPointer()) : (hops += 1) {
        const addr = cur.toPtrAddr();
        if (!heap.containsAddrForDebug(addr)) break;

        const first_word: *const Value = @ptrFromInt(addr);
        if (!first_word.isForwarding()) break;

        const new_addr = first_word.toPtrAddr();
        const forwarded_size_ptr: *const usize = @ptrFromInt(addr + @sizeOf(Value));
        const forwarded_size = forwarded_size_ptr.*;
        const forwarded_size_ok = forwarded_size > 0 and
            forwarded_size <= heap.space_size and
            std.mem.isAligned(forwarded_size, runtime.heap.ALIGNMENT);

        const from_start = @intFromPtr(heap.from_start);
        const from_live_end = @intFromPtr(heap.alloc_ptr);
        const in_from = new_addr >= from_start and new_addr < from_live_end and
            forwarded_size <= from_live_end - new_addr;

        const stale_start = @intFromPtr(heap.to_start);
        const stale_end = stale_start + heap.space_size;
        const in_stale = new_addr >= stale_start and new_addr < stale_end and
            forwarded_size <= stale_end - new_addr;

        var in_tenured = false;
        if (heap.gcLayoutMode() == .generational) {
            if (heap.tenuredRegion()) |tenured| {
                const ten_start = @intFromPtr(tenured.start);
                const ten_used_end = if (heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                in_tenured = new_addr >= ten_start and new_addr < ten_used_end and
                    forwarded_size <= ten_used_end - new_addr;
            }
        }

        if (!forwarded_size_ok or !(in_from or in_stale or in_tenured)) break;

        const next = Value{ .raw = new_addr | @as(u64, @intFromEnum(cur.getTag())) };
        if (next.raw == cur.raw) break;
        cur = next;
        if (in_from or in_tenured) {
            resolved_live = cur;
        }
    }

    return resolved_live orelse val;
}

/// Takes (cdr, car) order to avoid register swap when nesting cons calls.
/// Inner cons result stays in x0 (arg0=cdr position) naturally.
fn jitCons(cdr_raw: u64, car_raw: u64) callconv(.c) u64 {
    const car = jitResolveForwarded(Value{ .raw = car_raw });
    const cdr = jitResolveForwarded(Value{ .raw = cdr_raw });
    const ptr = g_alloc_ptr;
    const next = ptr + 16;
    if (next <= g_alloc_end) {
        // Fast path: inline bump allocation
        const p: [*]u64 = @ptrFromInt(ptr);
        p[0] = car.raw; // car at offset 0
        p[1] = cdr.raw; // cdr at offset 8
        g_alloc_ptr = next;
        // Update heap's alloc_ptr to stay in sync
        if (g_heap) |heap| {
            heap.alloc_ptr = @ptrFromInt(next);
        }
        return ptr; // cons tag = 0, so raw = ptr
    }
    // Slow path: full allocation with potential GC
    const heap = g_heap orelse return 0;
    jitSafepointBeforeAlloc();
    const result = heap.allocCons(car, cdr) catch return 0;
    // Refresh cache after potential GC
    jitConsRefreshCache();
    return result.raw;
}

// ── Global-state C-ABI wrappers for runtime primitives ──
// These can be called from JIT code via call_indirect without JitContext.

/// GCD of two tagged fixnums. Returns tagged fixnum.
fn jitGcd(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil.raw;
    var x = @abs(a.toFixnum());
    var y = @abs(b.toFixnum());
    while (y != 0) {
        const t = y;
        y = x % y;
        x = t;
    }
    return Value.makeFixnum(@intCast(x)).raw;
}

/// nreverse: destructively reverse a list. Returns tagged value.
fn jitNreverse(list_raw: u64) callconv(.c) u64 {
    var prev = Value.nil;
    var curr = jitResolveForwarded(Value{ .raw = list_raw });
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        const next = jitResolveForwarded(cell.cdr);
        cell.cdr = prev;
        jitWriteBarrier(curr.raw, prev.raw);
        prev = curr;
        curr = next;
    }
    return prev.raw;
}

/// append two lists. Returns tagged value. Allocates new cons cells.
fn jitAppend(list1_raw: u64, list2_raw: u64) callconv(.c) u64 {
    const list1 = jitResolveForwarded(Value{ .raw = list1_raw });
    const list2 = jitResolveForwarded(Value{ .raw = list2_raw });
    if (!list1.isCons()) return list2.raw;
    // Build reversed copy of list1, then reverse-cons onto list2
    // jitCons takes (cdr_raw, car_raw) — reversed parameter order!
    var rev = Value.nil;
    var curr = list1;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        const car = jitResolveForwarded(cell.car);
        const new_cell = jitCons(rev.raw, car.raw);
        if (new_cell == 0) return 0; // OOM
        rev = Value{ .raw = new_cell };
        curr = jitResolveForwarded(cell.cdr);
    }
    // Now rev is reversed list1. Reverse-cons onto list2.
    var result = list2;
    curr = rev;
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        const car = jitResolveForwarded(cell.car);
        const new_cell = jitCons(result.raw, car.raw);
        if (new_cell == 0) return 0; // OOM
        result = Value{ .raw = new_cell };
        curr = jitResolveForwarded(cell.cdr);
    }
    return result.raw;
}

/// assoc: lookup in alist by eq. Returns tagged value (pair or nil).
fn jitAssoc(key_raw: u64, alist_raw: u64) callconv(.c) u64 {
    var curr = Value{ .raw = alist_raw };
    while (curr.isCons()) {
        const cell = curr.toPtr(runtime.Cons);
        if (cell.car.isCons()) {
            const pair = cell.car.toPtr(runtime.Cons);
            if (pair.car.raw == key_raw) return cell.car.raw;
        }
        curr = cell.cdr;
    }
    return Value.nil.raw;
}

fn jitCall0(fn_raw: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call0(bridge.context, fn_raw);
}

fn jitCall1(fn_raw: u64, arg0: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call1(bridge.context, fn_raw, arg0);
}

fn jitCall2(fn_raw: u64, arg0: u64, arg1: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call2(bridge.context, fn_raw, arg0, arg1);
}

fn jitCall3(fn_raw: u64, arg0: u64, arg1: u64, arg2: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call3(bridge.context, fn_raw, arg0, arg1, arg2);
}

fn jitCall4(fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call4(bridge.context, fn_raw, arg0, arg1, arg2, arg3);
}

fn jitCall5(fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call5(bridge.context, fn_raw, arg0, arg1, arg2, arg3, arg4);
}

fn jitCall6(fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64, arg5: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call6(bridge.context, fn_raw, arg0, arg1, arg2, arg3, arg4, arg5);
}

fn jitCall7(fn_raw: u64, arg0: u64, arg1: u64, arg2: u64, arg3: u64, arg4: u64, arg5: u64, arg6: u64) callconv(.c) u64 {
    if (g_bridge_err) return Value.nil.raw;
    const bridge = g_call_bridge orelse std.debug.panic("jit call bridge not set", .{});
    return bridge.call7(bridge.context, fn_raw, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}

fn jitToFloat(v: Value) ?f64 {
    if (v.isFixnum()) return @floatFromInt(v.toFixnum());
    if (v.isFloat()) return v.toFloat();
    if (v.isRational()) {
        const rat = v.toPtr(runtime.Rational);
        const num: f64 = @floatFromInt(rat.numerator);
        const den: f64 = @floatFromInt(rat.denominator);
        return num / den;
    }
    return null;
}

fn jitRequireHeap() *Heap {
    return g_heap orelse std.debug.panic("jit helper missing heap", .{});
}

fn jitFloatCast(a_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const f = jitToFloat(a) orelse std.debug.panic(
        "jit float failed: type={s}",
        .{@tagName(a.typeKind())},
    );
    return Value.makeFloat(f).raw;
}

fn jitAddNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };

    if (a.isFloat() or b.isFloat()) {
        const out = runtime.primitives.arith.addFloat(a, b) catch |err| {
            std.debug.panic("jit add-float failed: {s}", .{@errorName(err)});
        };
        return out.raw;
    }
    if (a.isFixnum() and b.isFixnum()) {
        const av = a.toFixnum();
        const bv = b.toFixnum();
        const sum = @addWithOverflow(av, bv);
        const max_fixnum: i64 = (1 << 62) - 1;
        const min_fixnum: i64 = -(1 << 62);
        if (sum[1] == 0 and sum[0] <= max_fixnum and sum[0] >= min_fixnum) {
            return Value.makeFixnum(sum[0]).raw;
        }
    }

    const out = runtime.primitives.arith.add(jitRequireHeap(), a, b) catch |err| {
        std.debug.panic("jit add failed: {s}", .{@errorName(err)});
    };
    return out.raw;
}

fn jitSubNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };

    if (a.isFloat() or b.isFloat()) {
        const out = runtime.primitives.arith.subFloat(a, b) catch |err| {
            std.debug.panic("jit sub-float failed: {s}", .{@errorName(err)});
        };
        return out.raw;
    }
    if (a.isFixnum() and b.isFixnum()) {
        const av = a.toFixnum();
        const bv = b.toFixnum();
        const diff = @subWithOverflow(av, bv);
        const max_fixnum: i64 = (1 << 62) - 1;
        const min_fixnum: i64 = -(1 << 62);
        if (diff[1] == 0 and diff[0] <= max_fixnum and diff[0] >= min_fixnum) {
            return Value.makeFixnum(diff[0]).raw;
        }
    }

    const out = runtime.primitives.arith.sub(jitRequireHeap(), a, b) catch |err| {
        if (std.posix.getenv("HABU_TRACE_JIT_NUM") != null) {
            std.debug.print(
                "JIT_NUM sub fail err={s} a=0x{x} b=0x{x} a_fix={} b_fix={} a_float={} b_float={} a_ptr={} b_ptr={}\n",
                .{
                    @errorName(err),
                    a.raw,
                    b.raw,
                    a.isFixnum(),
                    b.isFixnum(),
                    a.isFloat(),
                    b.isFloat(),
                    a.isPointer(),
                    b.isPointer(),
                },
            );
        }
        std.debug.panic("jit sub failed: {s}", .{@errorName(err)});
    };
    return out.raw;
}

fn jitMulNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };

    if (a.isFloat() or b.isFloat()) {
        const out = runtime.primitives.arith.mulFloat(a, b) catch |err| {
            std.debug.panic("jit mul-float failed: {s}", .{@errorName(err)});
        };
        return out.raw;
    }
    if (a.isFixnum() and b.isFixnum()) {
        const av = a.toFixnum();
        const bv = b.toFixnum();
        const prod = @mulWithOverflow(av, bv);
        const max_fixnum: i64 = (1 << 62) - 1;
        const min_fixnum: i64 = -(1 << 62);
        if (prod[1] == 0 and prod[0] <= max_fixnum and prod[0] >= min_fixnum) {
            return Value.makeFixnum(prod[0]).raw;
        }
    }

    const out = runtime.primitives.arith.mul(jitRequireHeap(), a, b) catch |err| {
        std.debug.panic("jit mul failed: {s}", .{@errorName(err)});
    };
    return out.raw;
}

fn jitLtNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };
    const ok = runtime.primitives.arith.lt(a, b) catch |err| {
        std.debug.panic("jit lt failed: {s}", .{@errorName(err)});
    };
    return if (ok) Value.t.raw else Value.nil.raw;
}

fn jitGtNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };
    const ok = runtime.primitives.arith.gt(a, b) catch |err| {
        std.debug.panic("jit gt failed: {s}", .{@errorName(err)});
    };
    return if (ok) Value.t.raw else Value.nil.raw;
}

fn jitLeNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };
    if (std.posix.getenv("HABU_TRACE_JIT_NUM") != null) {
        std.debug.print(
            "JIT_NUM le a=0x{x} b=0x{x} a_fix={} b_fix={} a_float={} b_float={} a_ptr={} b_ptr={}\n",
            .{
                a.raw,
                b.raw,
                a.isFixnum(),
                b.isFixnum(),
                a.isFloat(),
                b.isFloat(),
                a.isPointer(),
                b.isPointer(),
            },
        );
    }
    const ok = runtime.primitives.arith.le(a, b) catch |err| {
        std.debug.panic("jit le failed: {s}", .{@errorName(err)});
    };
    return if (ok) Value.t.raw else Value.nil.raw;
}

fn jitGeNum(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };
    const ok = runtime.primitives.arith.ge(a, b) catch |err| {
        std.debug.panic("jit ge failed: {s}", .{@errorName(err)});
    };
    return if (ok) Value.t.raw else Value.nil.raw;
}

fn jitNumEq(a_raw: u64, b_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const b = Value{ .raw = b_raw };
    return if (runtime.primitives.arith.numEq(a, b)) Value.t.raw else Value.nil.raw;
}

fn jitSqrt(a_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    const f = jitToFloat(a) orelse std.debug.panic(
        "jit sqrt failed: type={s}",
        .{@tagName(a.typeKind())},
    );
    if (f < 0) {
        const cplx = jitRequireHeap().allocComplex(0.0, @sqrt(-f)) catch |err| {
            std.debug.panic("jit sqrt complex alloc failed: {s}", .{@errorName(err)});
        };
        return cplx.raw;
    }
    return Value.makeFloat(@sqrt(f)).raw;
}

fn jitRound(a_raw: u64) callconv(.c) u64 {
    const a = Value{ .raw = a_raw };
    if (a.isFixnum()) return a.raw;
    const f = jitToFloat(a) orelse std.debug.panic(
        "jit round failed: type={s}",
        .{@tagName(a.typeKind())},
    );

    // CL round: ties to even.
    const rounded = blk: {
        const r = @round(f);
        if (@abs(f - r) == 0.5) {
            const ri: i64 = @intFromFloat(r);
            if (@mod(ri, @as(i64, 2)) != 0) {
                break :blk if (f > 0) r - 1.0 else r + 1.0;
            }
        }
        break :blk r;
    };
    const q: i64 = @intFromFloat(rounded);
    return Value.makeFixnum(q).raw;
}

fn jitMakeHash(capacity_raw: u64, test_raw: u64) callconv(.c) u64 {
    const cap_val = Value{ .raw = capacity_raw };
    const test_val = Value{ .raw = test_raw };
    if (!cap_val.isFixnum() or !test_val.isFixnum()) return Value.nil.raw;
    const cap_i = cap_val.toFixnum();
    if (cap_i < 0) return Value.nil.raw;
    const cap: usize = @intCast(cap_i);
    const test_i = test_val.toFixnum();
    const test_type: runtime.HashTest = switch (test_i) {
        0 => .eq,
        1 => .eql,
        2 => .equal,
        3 => .equalp,
        else => return Value.nil.raw,
    };
    const heap = g_heap orelse return Value.nil.raw;
    const ht = heap.allocHashTable(cap, test_type) catch return Value.nil.raw;
    return ht.raw;
}

fn jitHashGet(table_raw: u64, key_raw: u64, default_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    const key = Value{ .raw = key_raw };
    if (ht.get(key)) |v| return v.raw;
    return default_raw;
}

fn jitHashSet(table_raw: u64, key_raw: u64, value_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    const key = Value{ .raw = key_raw };
    const value = Value{ .raw = value_raw };
    const heap = g_heap orelse return Value.nil.raw;

    while (true) {
        ht.put(key, value) catch |err| switch (err) {
            error.HashTableNeedsGrowth, error.HashTableFull => {
                const cap: usize = @intCast(ht.capacity);
                const new_cap = std.math.mul(usize, cap, 2) catch return Value.nil.raw;
                heap.growHashTableInPlace(ht, new_cap) catch return Value.nil.raw;
                continue;
            },
        };
        jitWriteBarrier(table_raw, key_raw);
        jitWriteBarrier(table_raw, value_raw);
        return value_raw;
    }
}

fn jitHashCount(table_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    if (ht.count > std.math.maxInt(i64)) return Value.nil.raw;
    return Value.makeFixnum(@intCast(ht.count)).raw;
}

fn jitHashRem(table_raw: u64, key_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    const key = Value{ .raw = key_raw };
    return if (ht.remove(key)) Value.t.raw else Value.nil.raw;
}

fn jitHashCapacity(table_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    if (ht.capacity > std.math.maxInt(i64)) return Value.nil.raw;
    return Value.makeFixnum(@intCast(ht.capacity)).raw;
}

fn jitHashClear(table_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    ht.clear();
    return table_raw;
}

fn jitHashTest(table_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    const test_name = switch (ht.test_type) {
        .eq => "eq",
        .eql => "eql",
        .equal => "equal",
        .equalp => "equalp",
    };
    const heap = g_heap orelse return Value.nil.raw;
    const sym = heap.intern(test_name) catch return Value.nil.raw;
    return sym.raw;
}

fn jitHashKeys(table_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    const heap = g_heap orelse return Value.nil.raw;

    var result = Value.nil;
    var i: u64 = 0;
    while (i < ht.capacity) : (i += 1) {
        const key = ht.getKey(@intCast(i));
        if (runtime.HashTable.isAvailableKey(key)) continue;
        result = heap.allocCons(key, result) catch return Value.nil.raw;
    }
    return result.raw;
}

fn jitHashAlist(table_raw: u64) callconv(.c) u64 {
    const table = Value{ .raw = table_raw };
    if (!table.isHashTable()) return Value.nil.raw;
    const ht = table.toPtr(runtime.HashTable);
    const heap = g_heap orelse return Value.nil.raw;

    var result = Value.nil;
    var i: u64 = 0;
    while (i < ht.capacity) : (i += 1) {
        const key = ht.getKey(@intCast(i));
        if (runtime.HashTable.isAvailableKey(key)) continue;
        const val = ht.getValue(@intCast(i));
        const pair = heap.allocCons(key, val) catch return Value.nil.raw;
        result = heap.allocCons(pair, result) catch return Value.nil.raw;
    }
    return result.raw;
}

fn jitDecodeFixnumIndex(raw: u64) ?usize {
    const idx_val = Value{ .raw = raw };
    if (!idx_val.isFixnum()) return null;
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return null;
    const idx_u64: u64 = @intCast(idx_signed);
    if (idx_u64 > std.math.maxInt(usize)) return null;
    return @intCast(idx_u64);
}

fn jitMakeVector(size_raw: u64, init_raw: u64) callconv(.c) u64 {
    const size = jitDecodeFixnumIndex(size_raw) orelse return Value.nil.raw;
    const heap = g_heap orelse return Value.nil.raw;
    const vec = heap.allocVector(size, size) catch return Value.nil.raw;
    const init_val = Value{ .raw = init_raw };
    const vec_obj = vec.toPtr(runtime.Vector);
    for (0..size) |i| {
        vec_obj.data[i] = init_val;
    }
    return vec.raw;
}

fn jitVecRef(vec_raw: u64, idx_raw: u64) callconv(.c) u64 {
    const vec_val = Value{ .raw = vec_raw };
    const idx = jitDecodeFixnumIndex(idx_raw) orelse return Value.nil.raw;
    if (!vec_val.isVector()) return Value.nil.raw;
    const vec = vec_val.toPtr(runtime.Vector);
    if (idx >= vec.length) return Value.nil.raw;
    return vec.get(idx).raw;
}

fn jitVecSet(vec_raw: u64, idx_raw: u64, value_raw: u64) callconv(.c) u64 {
    const vec_val = Value{ .raw = vec_raw };
    const idx = jitDecodeFixnumIndex(idx_raw) orelse return Value.nil.raw;
    if (!vec_val.isVector()) return Value.nil.raw;
    const vec = vec_val.toPtr(runtime.Vector);
    if (idx >= vec.length) return Value.nil.raw;
    vec.set(idx, Value{ .raw = value_raw });
    jitWriteBarrier(vec_raw, value_raw);
    return value_raw;
}

fn jitVecLen(vec_raw: u64) callconv(.c) u64 {
    const vec_val = Value{ .raw = vec_raw };
    if (!vec_val.isVector()) return Value.nil.raw;
    const vec = vec_val.toPtr(runtime.Vector);
    if (vec.length > std.math.maxInt(i64)) return Value.nil.raw;
    return Value.makeFixnum(@intCast(vec.length)).raw;
}

fn jitMakeString(len_raw: u64, char_raw: u64) callconv(.c) u64 {
    const len_val = Value{ .raw = len_raw };
    const char_val = Value{ .raw = char_raw };
    if (!len_val.isFixnum()) return Value.nil.raw;
    const len_signed = len_val.toFixnum();
    if (len_signed < 0) return Value.nil.raw;
    const len: usize = @intCast(len_signed);
    const fill_char: u8 = if (char_val.isCharacter()) blk: {
        const cp = char_val.toCharacter();
        if (cp > 255) return Value.nil.raw;
        break :blk @intCast(cp);
    } else if (char_val.isNil()) ' ' else return Value.nil.raw;

    const heap = g_heap orelse return Value.nil.raw;
    const str = heap.allocStringUninitialized(len) catch return Value.nil.raw;
    const str_obj = str.toPtr(runtime.String);
    @memset(str_obj.data[0..len], fill_char);
    return str.raw;
}

fn jitInternName(name: []const u8) u64 {
    const heap = g_heap orelse return Value.nil.raw;
    if (std.posix.getenv("HABU_TRACE_JIT_INTERN") != null) {
        std.debug.print("JIT_INTERN name=\"{s}\"\n", .{name});
    }
    const sym = heap.intern(name) catch return Value.nil.raw;
    return sym.raw;
}

fn jitIntern(name_raw: u64) callconv(.c) u64 {
    const name_val = Value{ .raw = name_raw };
    switch (name_val.typeKind()) {
        .string => return jitInternName(name_val.toPtr(runtime.String).bytes()),
        .symbol => return jitInternName(name_val.toPtr(Symbol).getName()),
        .keyword => return jitInternName(name_val.toPtr(runtime.Keyword).getName()),
        .nil => return jitInternName("nil"),
        .t => return jitInternName("t"),
        .fixnum => {
            const byte = [_]u8{@intCast(@mod(name_val.toFixnum(), 256))};
            return jitInternName(&byte);
        },
        else => return Value.nil.raw,
    }
}

fn jitMakeArray1(dim_raw: u64, init_raw: u64) callconv(.c) u64 {
    const dim_val = Value{ .raw = dim_raw };
    if (!dim_val.isFixnum()) return Value.nil.raw;
    const dim_signed = dim_val.toFixnum();
    if (dim_signed < 0) return Value.nil.raw;
    const dim: u64 = @intCast(dim_signed);
    const heap = g_heap orelse return Value.nil.raw;
    const dims = [_]u64{dim};
    const arr_val = heap.allocArray(&dims) catch return Value.nil.raw;
    const arr = arr_val.toPtr(runtime.Array);
    const data: [*]Value = @ptrFromInt(arr.data_ptr);
    const init_val = Value{ .raw = init_raw };
    for (0..arr.total_size) |i| data[i] = init_val;
    return arr_val.raw;
}

fn jitAref1(arr_raw: u64, idx_raw: u64) callconv(.c) u64 {
    const arr_val = Value{ .raw = arr_raw };
    const idx_val = Value{ .raw = idx_raw };
    if (!idx_val.isFixnum()) return Value.nil.raw;
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return Value.nil.raw;
    const idx: usize = @intCast(idx_signed);

    return switch (arr_val.typeKind()) {
        .vector => blk: {
            const vec = arr_val.toPtr(runtime.Vector);
            if (idx >= vec.length) break :blk Value.nil.raw;
            break :blk vec.get(idx).raw;
        },
        .string => blk: {
            const str = arr_val.toPtr(runtime.String);
            if (idx >= str.length) break :blk Value.nil.raw;
            break :blk Value.makeCharacter(@intCast(str.bytes()[idx])).raw;
        },
        .string32 => blk: {
            const str32 = arr_val.toPtr(runtime.String32);
            if (idx >= str32.length) break :blk Value.nil.raw;
            break :blk Value.makeCharacter(@intCast(str32.codepoints()[idx])).raw;
        },
        .array => blk: {
            const arr = arr_val.toPtr(runtime.Array);
            if (arr.rank != 1) break :blk Value.nil.raw;
            if (@as(u64, @intCast(idx)) >= arr.dimensions[0]) break :blk Value.nil.raw;
            const data: [*]Value = @ptrFromInt(arr.data_ptr);
            break :blk data[idx].raw;
        },
        else => Value.nil.raw,
    };
}

fn jitAref2(arr_raw: u64, s0_raw: u64, s1_raw: u64) callconv(.c) u64 {
    return jitArefN(
        arr_raw,
        Value.makeFixnum(2).raw,
        s0_raw,
        s1_raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
    );
}

fn jitArefN(
    arr_raw: u64,
    count_raw: u64,
    s0_raw: u64,
    s1_raw: u64,
    s2_raw: u64,
    s3_raw: u64,
    s4_raw: u64,
    s5_raw: u64,
    s6_raw: u64,
    s7_raw: u64,
) callconv(.c) u64 {
    const count_val = Value{ .raw = count_raw };
    if (!count_val.isFixnum()) return Value.nil.raw;
    const count_signed = count_val.toFixnum();
    if (count_signed < 0 or count_signed > 8) return Value.nil.raw;
    const sub_count: usize = @intCast(count_signed);

    const raw_subs = [_]u64{ s0_raw, s1_raw, s2_raw, s3_raw, s4_raw, s5_raw, s6_raw, s7_raw };
    var subs: [8]u64 = [_]u64{0} ** 8;
    var i: usize = 0;
    while (i < sub_count) : (i += 1) {
        const idx = jitDecodeFixnumIndex(raw_subs[i]) orelse return Value.nil.raw;
        subs[i] = @intCast(idx);
    }

    const arr_val = Value{ .raw = arr_raw };
    return switch (arr_val.typeKind()) {
        .vector => blk: {
            if (sub_count != 1) break :blk Value.nil.raw;
            const idx = subs[0];
            const vec = arr_val.toPtr(runtime.Vector);
            if (idx >= vec.length) break :blk Value.nil.raw;
            break :blk vec.get(@intCast(idx)).raw;
        },
        .string => blk: {
            if (sub_count != 1) break :blk Value.nil.raw;
            const idx = subs[0];
            const str = arr_val.toPtr(runtime.String);
            if (idx >= str.length) break :blk Value.nil.raw;
            break :blk Value.makeCharacter(@intCast(str.bytes()[@intCast(idx)])).raw;
        },
        .string32 => blk: {
            if (sub_count != 1) break :blk Value.nil.raw;
            const idx = subs[0];
            const str32 = arr_val.toPtr(runtime.String32);
            if (idx >= str32.length) break :blk Value.nil.raw;
            const cp = str32.codepoints()[@intCast(idx)];
            if (cp > std.math.maxInt(u21)) break :blk Value.nil.raw;
            break :blk Value.makeCharacter(@intCast(cp)).raw;
        },
        .array => blk: {
            const arr = arr_val.toPtr(runtime.Array);
            if (arr.rank != sub_count) break :blk Value.nil.raw;
            var index: u64 = 0;
            for (0..sub_count) |k| {
                if (subs[k] >= arr.dimensions[k]) break :blk Value.nil.raw;
                var stride: u64 = 1;
                for (k + 1..sub_count) |m| {
                    stride = std.math.mul(u64, stride, arr.dimensions[m]) catch return Value.nil.raw;
                }
                const term = std.math.mul(u64, subs[k], stride) catch return Value.nil.raw;
                index = std.math.add(u64, index, term) catch return Value.nil.raw;
            }
            if (index >= arr.total_size) break :blk Value.nil.raw;
            const data: [*]Value = @ptrFromInt(arr.data_ptr);
            break :blk data[@intCast(index)].raw;
        },
        else => Value.nil.raw,
    };
}

fn jitAsetN(
    arr_raw: u64,
    count_raw: u64,
    s0_raw: u64,
    s1_raw: u64,
    s2_raw: u64,
    s3_raw: u64,
    s4_raw: u64,
    s5_raw: u64,
    s6_raw: u64,
    s7_raw: u64,
    value_raw: u64,
) callconv(.c) u64 {
    const count_val = Value{ .raw = count_raw };
    if (!count_val.isFixnum()) return Value.nil.raw;
    const count_signed = count_val.toFixnum();
    if (count_signed < 0 or count_signed > 8) return Value.nil.raw;
    const sub_count: usize = @intCast(count_signed);

    const raw_subs = [_]u64{ s0_raw, s1_raw, s2_raw, s3_raw, s4_raw, s5_raw, s6_raw, s7_raw };
    var subs: [8]u64 = [_]u64{0} ** 8;
    var i: usize = 0;
    while (i < sub_count) : (i += 1) {
        const idx = jitDecodeFixnumIndex(raw_subs[i]) orelse return Value.nil.raw;
        subs[i] = @intCast(idx);
    }

    const new_val = Value{ .raw = value_raw };
    const arr_val = Value{ .raw = arr_raw };
    switch (arr_val.typeKind()) {
        .vector => {
            if (sub_count != 1) return Value.nil.raw;
            const idx = subs[0];
            const vec = arr_val.toPtr(runtime.Vector);
            if (idx >= vec.length) return Value.nil.raw;
            vec.set(@intCast(idx), new_val);
            jitWriteBarrier(arr_raw, value_raw);
            return value_raw;
        },
        .string => {
            if (sub_count != 1 or !new_val.isCharacter()) return Value.nil.raw;
            const idx = subs[0];
            const str = arr_val.toPtr(runtime.String);
            if (idx >= str.length) return Value.nil.raw;
            const cp = new_val.toCharacter();
            if (cp > 255) return Value.nil.raw;
            str.mutableBytes()[@intCast(idx)] = @intCast(cp);
            return value_raw;
        },
        .string32 => {
            if (sub_count != 1 or !new_val.isCharacter()) return Value.nil.raw;
            const idx = subs[0];
            const str32 = arr_val.toPtr(runtime.String32);
            if (idx >= str32.length) return Value.nil.raw;
            str32.mutableCodepoints()[@intCast(idx)] = new_val.toCharacter();
            return value_raw;
        },
        .array => {
            const arr = arr_val.toPtr(runtime.Array);
            if (arr.rank != sub_count) return Value.nil.raw;
            var index: u64 = 0;
            for (0..sub_count) |k| {
                if (subs[k] >= arr.dimensions[k]) return Value.nil.raw;
                var stride: u64 = 1;
                for (k + 1..sub_count) |m| {
                    stride = std.math.mul(u64, stride, arr.dimensions[m]) catch return Value.nil.raw;
                }
                const term = std.math.mul(u64, subs[k], stride) catch return Value.nil.raw;
                index = std.math.add(u64, index, term) catch return Value.nil.raw;
            }
            if (index >= arr.total_size) return Value.nil.raw;
            const data: [*]Value = @ptrFromInt(arr.data_ptr);
            data[@intCast(index)] = new_val;
            jitWriteBarrier(arr_raw, value_raw);
            return value_raw;
        },
        else => return Value.nil.raw,
    }
}

fn jitAset1(arr_raw: u64, s0_raw: u64, value_raw: u64) callconv(.c) u64 {
    return jitAsetN(
        arr_raw,
        Value.makeFixnum(1).raw,
        s0_raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        value_raw,
    );
}

fn jitAset2(arr_raw: u64, s0_raw: u64, s1_raw: u64, value_raw: u64) callconv(.c) u64 {
    return jitAsetN(
        arr_raw,
        Value.makeFixnum(2).raw,
        s0_raw,
        s1_raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        Value.nil.raw,
        value_raw,
    );
}

fn jitAllocArrayFromDims(heap: *Heap, rank: u8, dims: [8]u64, init_val: Value) error{ OutOfMemory, Overflow }!u64 {
    var total_size: u64 = 1;
    for (0..rank) |i| {
        total_size = try std.math.mul(u64, total_size, dims[i]);
    }
    if (total_size > std.math.maxInt(usize)) return error.Overflow;
    const elem_count: usize = @intCast(total_size);
    const bytes_data = try std.math.mul(usize, elem_count, @sizeOf(Value));
    const total_bytes = try std.math.add(usize, @sizeOf(runtime.Array), bytes_data);
    const ptr = try heap.allocRaw(total_bytes);
    const arr: *runtime.Array = @ptrCast(@alignCast(ptr));
    const data_ptr: [*]Value = @ptrCast(@alignCast(ptr + @sizeOf(runtime.Array)));
    arr.* = .{
        .kind = .array,
        .rank = rank,
        .dimensions = dims,
        .total_size = total_size,
        .data_ptr = @intFromPtr(data_ptr),
    };

    for (0..elem_count) |idx| {
        data_ptr[idx] = init_val;
    }
    return Value.makeArray(arr).raw;
}

fn jitMakeArrayDynamic(dims_raw: u64, init_raw: u64) callconv(.c) u64 {
    const dims_val = Value{ .raw = dims_raw };
    var dims: [8]u64 = [_]u64{0} ** 8;
    var rank: u8 = 0;
    switch (dims_val.typeKind()) {
        .fixnum => {
            const dim_signed = dims_val.toFixnum();
            if (dim_signed < 0) return Value.nil.raw;
            dims[0] = @intCast(dim_signed);
            rank = 1;
        },
        .nil, .cons => {
            var cur = dims_val;
            var idx: usize = 0;
            while (cur.isCons()) {
                if (idx >= dims.len) return Value.nil.raw;
                const pair = cur.toPtr(runtime.Cons);
                if (!pair.car.isFixnum()) return Value.nil.raw;
                const dim_signed = pair.car.toFixnum();
                if (dim_signed < 0) return Value.nil.raw;
                dims[idx] = @intCast(dim_signed);
                idx += 1;
                cur = pair.cdr;
            }
            if (!cur.isNil()) return Value.nil.raw;
            rank = @intCast(idx);
        },
        else => return Value.nil.raw,
    }
    const heap = g_heap orelse return Value.nil.raw;
    return jitAllocArrayFromDims(heap, rank, dims, Value{ .raw = init_raw }) catch return Value.nil.raw;
}

fn jitStrRef(str_raw: u64, idx_raw: u64) callconv(.c) u64 {
    const str_val = Value{ .raw = str_raw };
    const idx = jitDecodeFixnumIndex(idx_raw) orelse return Value.nil.raw;
    return switch (str_val.typeKind()) {
        .string => blk: {
            const str = str_val.toPtr(runtime.String);
            if (idx >= str.length) break :blk Value.nil.raw;
            break :blk Value.makeCharacter(@intCast(str.bytes()[idx])).raw;
        },
        .string32 => blk: {
            const str32 = str_val.toPtr(runtime.String32);
            if (idx >= str32.length) break :blk Value.nil.raw;
            const cp = str32.codepoints()[idx];
            if (cp > std.math.maxInt(u21)) break :blk Value.nil.raw;
            break :blk Value.makeCharacter(@intCast(cp)).raw;
        },
        else => Value.nil.raw,
    };
}

fn jitStrLen(str_raw: u64) callconv(.c) u64 {
    const str_val = Value{ .raw = str_raw };
    return switch (str_val.typeKind()) {
        .string => blk: {
            const str = str_val.toPtr(runtime.String);
            if (str.length > std.math.maxInt(i64)) break :blk Value.nil.raw;
            break :blk Value.makeFixnum(@intCast(str.length)).raw;
        },
        .string32 => blk: {
            const str32 = str_val.toPtr(runtime.String32);
            if (str32.length > std.math.maxInt(i64)) break :blk Value.nil.raw;
            break :blk Value.makeFixnum(@intCast(str32.length)).raw;
        },
        else => Value.nil.raw,
    };
}

fn jitStrConcat(s1_raw: u64, s2_raw: u64) callconv(.c) u64 {
    const s1 = Value{ .raw = s1_raw };
    const s2 = Value{ .raw = s2_raw };
    const s1_is_base = s1.isString();
    const s1_is_utf32 = s1.isString32();
    const s2_is_base = s2.isString();
    const s2_is_utf32 = s2.isString32();
    if (!(s1_is_base or s1_is_utf32) or !(s2_is_base or s2_is_utf32)) return Value.nil.raw;

    const heap = g_heap orelse return Value.nil.raw;
    if (s1_is_base and s2_is_base) {
        const len1 = s1.toPtr(runtime.String).length;
        const len2 = s2.toPtr(runtime.String).length;
        const new_len = std.math.add(usize, len1, len2) catch return Value.nil.raw;
        const result = heap.allocStringUninitialized(new_len) catch return Value.nil.raw;
        const result_str = result.toPtr(runtime.String);
        const dest = result_str.mutableBytes();
        const str1 = s1.toPtr(runtime.String);
        const str2 = s2.toPtr(runtime.String);
        @memcpy(dest[0..len1], str1.bytes());
        @memcpy(dest[len1..new_len], str2.bytes());
        return result.raw;
    }

    const len1 = if (s1_is_utf32) s1.toPtr(runtime.String32).length else s1.toPtr(runtime.String).length;
    const len2 = if (s2_is_utf32) s2.toPtr(runtime.String32).length else s2.toPtr(runtime.String).length;
    const new_len = std.math.add(usize, len1, len2) catch return Value.nil.raw;
    const result = heap.allocString32Uninitialized(new_len) catch return Value.nil.raw;
    const dest = result.toPtr(runtime.String32).mutableCodepoints();

    var out_idx: usize = 0;
    if (s1_is_utf32) {
        const cps = s1.toPtr(runtime.String32).codepoints();
        @memcpy(dest[0..cps.len], cps);
        out_idx = cps.len;
    } else {
        const bytes = s1.toPtr(runtime.String).bytes();
        for (bytes, 0..) |b, i| dest[i] = @intCast(b);
        out_idx = bytes.len;
    }

    if (s2_is_utf32) {
        const cps = s2.toPtr(runtime.String32).codepoints();
        @memcpy(dest[out_idx .. out_idx + cps.len], cps);
    } else {
        const bytes = s2.toPtr(runtime.String).bytes();
        for (bytes, 0..) |b, i| dest[out_idx + i] = @intCast(b);
    }
    return result.raw;
}

fn jitSubstring(str_raw: u64, start_raw: u64, end_raw: u64) callconv(.c) u64 {
    const start_idx = jitDecodeFixnumIndex(start_raw) orelse return Value.nil.raw;
    const end_idx = jitDecodeFixnumIndex(end_raw) orelse return Value.nil.raw;
    const heap = g_heap orelse return Value.nil.raw;
    const result = runtime.primitives.string.substring(
        heap,
        Value{ .raw = str_raw },
        start_idx,
        end_idx,
    ) catch return Value.nil.raw;
    return result.raw;
}

fn jitStrSet(str_raw: u64, idx_raw: u64, char_raw: u64) callconv(.c) u64 {
    const str_val = Value{ .raw = str_raw };
    const idx_val = Value{ .raw = idx_raw };
    const char_val = Value{ .raw = char_raw };
    if (!idx_val.isFixnum()) return Value.nil.raw;
    const idx_signed = idx_val.toFixnum();
    if (idx_signed < 0) return Value.nil.raw;
    const idx: usize = @intCast(idx_signed);
    const char_int: i64 = switch (char_val.typeKind()) {
        .fixnum => char_val.toFixnum(),
        .char => @as(i64, @intCast(char_val.toCharacter())),
        else => return Value.nil.raw,
    };
    if (char_int < 0) return Value.nil.raw;

    switch (str_val.typeKind()) {
        .string => {
            const str = str_val.toPtr(runtime.String);
            if (idx >= str.length) return Value.nil.raw;
            if (char_int > 255) return Value.nil.raw;
            str.mutableBytes()[idx] = @intCast(char_int);
            return str_val.raw;
        },
        .string32 => {
            const str32 = str_val.toPtr(runtime.String32);
            if (idx >= str32.length) return Value.nil.raw;
            if (char_int > std.math.maxInt(u21)) return Value.nil.raw;
            str32.mutableCodepoints()[idx] = @intCast(char_int);
            return str_val.raw;
        },
        else => return Value.nil.raw,
    }
}

fn jitPosition(item_raw: u64, seq_raw: u64) callconv(.c) u64 {
    const item = Value{ .raw = item_raw };
    const seq = Value{ .raw = seq_raw };

    switch (seq.typeKind()) {
        .string => {
            const cp: u21 = switch (item.typeKind()) {
                .char => item.toCharacter(),
                .fixnum => blk: {
                    const n = item.toFixnum();
                    if (n < 0 or n > std.math.maxInt(u21)) return Value.nil.raw;
                    break :blk @intCast(n);
                },
                else => return Value.nil.raw,
            };
            if (cp > 255) return Value.nil.raw;
            const needle: u8 = @intCast(cp);
            const str = seq.toPtr(runtime.String);
            for (str.bytes(), 0..) |b, i| {
                if (b == needle) return Value.makeFixnum(@intCast(i)).raw;
            }
            return Value.nil.raw;
        },
        .string32 => {
            const cp: u21 = switch (item.typeKind()) {
                .char => item.toCharacter(),
                .fixnum => blk: {
                    const n = item.toFixnum();
                    if (n < 0 or n > std.math.maxInt(u21)) return Value.nil.raw;
                    break :blk @intCast(n);
                },
                else => return Value.nil.raw,
            };
            const str32 = seq.toPtr(runtime.String32);
            for (str32.codepoints(), 0..) |c, i| {
                if (c == cp) return Value.makeFixnum(@intCast(i)).raw;
            }
            return Value.nil.raw;
        },
        .nil, .cons => {
            var cur = seq;
            var idx: i64 = 0;
            while (cur.isCons()) {
                const cell = cur.toPtr(runtime.Cons);
                if (cell.car.raw == item_raw) return Value.makeFixnum(idx).raw;
                idx += 1;
                cur = cell.cdr;
            }
            return Value.nil.raw;
        },
        else => return Value.nil.raw,
    }
}

fn jitLength(seq_raw: u64) callconv(.c) u64 {
    const seq = Value{ .raw = seq_raw };
    return switch (seq.typeKind()) {
        .nil => Value.makeFixnum(0).raw,
        .cons => blk: {
            var len: i64 = 0;
            var curr = seq;
            while (curr.isCons()) {
                len += 1;
                curr = curr.toPtr(runtime.Cons).cdr;
            }
            break :blk Value.makeFixnum(len).raw;
        },
        .vector => blk: {
            const vec = seq.toPtr(runtime.Vector);
            break :blk Value.makeFixnum(@intCast(vec.length)).raw;
        },
        .string => blk: {
            const str = seq.toPtr(runtime.String);
            break :blk Value.makeFixnum(@intCast(str.length)).raw;
        },
        .string32 => blk: {
            const str32 = seq.toPtr(runtime.String32);
            break :blk Value.makeFixnum(@intCast(str32.length)).raw;
        },
        .array => blk: {
            const arr = seq.toPtr(runtime.Array);
            if (arr.rank != 1) break :blk Value.nil.raw;
            if (arr.total_size > std.math.maxInt(i64)) break :blk Value.nil.raw;
            break :blk Value.makeFixnum(@intCast(arr.total_size)).raw;
        },
        else => Value.nil.raw,
    };
}

fn jitFormatSimple(dest_raw: u64, control_raw: u64, arg_raw: u64, argc_raw: u64) callconv(.c) u64 {
    const dest = Value{ .raw = dest_raw };
    const control = Value{ .raw = control_raw };
    const argc_val = Value{ .raw = argc_raw };
    if (!dest.isNil()) return Value.nil.raw;
    if (!control.isString()) return Value.nil.raw;
    if (!argc_val.isFixnum()) return Value.nil.raw;
    const argc = argc_val.toFixnum();
    if (argc < 0 or argc > 1) return Value.nil.raw;

    const heap = g_heap orelse return Value.nil.raw;
    const bytes = control.toPtr(runtime.String).bytes();

    // Fast path: one plain ~D/~d directive with literal prefix/suffix.
    // This avoids per-call dynamic ArrayList allocation in hot format loops.
    if (argc == 1) {
        const arg = Value{ .raw = arg_raw };
        if (arg.isFixnum()) {
            var directive_pos: ?usize = null;
            var idx: usize = 0;
            var simple = true;
            while (idx < bytes.len) : (idx += 1) {
                if (bytes[idx] != '~') continue;
                if (directive_pos != null or idx + 1 >= bytes.len) {
                    simple = false;
                    break;
                }
                const directive = bytes[idx + 1];
                if (directive != 'd' and directive != 'D') {
                    simple = false;
                    break;
                }
                directive_pos = idx;
                idx += 1;
            }

            if (simple and directive_pos != null) {
                const pos = directive_pos.?;
                var num_buf: [64]u8 = undefined;
                const printed = std.fmt.bufPrint(&num_buf, "{d}", .{arg.toFixnum()}) catch return Value.nil.raw;
                const suffix = bytes[pos + 2 ..];
                const total_len = pos + printed.len + suffix.len;
                if (total_len <= 512) {
                    var out_buf: [512]u8 = undefined;
                    @memcpy(out_buf[0..pos], bytes[0..pos]);
                    @memcpy(out_buf[pos .. pos + printed.len], printed);
                    @memcpy(out_buf[pos + printed.len .. total_len], suffix);
                    const result = heap.allocBaseString(out_buf[0..total_len]) catch return Value.nil.raw;
                    return result.raw;
                }
            }
        }
    }

    var out = std.ArrayList(u8){};
    defer out.deinit(heap.backing_allocator);

    var i: usize = 0;
    var arg_used = false;
    while (i < bytes.len) {
        if (bytes[i] != '~') {
            out.append(heap.backing_allocator, bytes[i]) catch return Value.nil.raw;
            i += 1;
            continue;
        }
        i += 1;
        if (i >= bytes.len) return Value.nil.raw;

        var width: ?usize = null;
        var pad: u8 = ' ';

        const width_start = i;
        while (i < bytes.len and std.ascii.isDigit(bytes[i])) : (i += 1) {}
        if (i > width_start) {
            width = std.fmt.parseInt(usize, bytes[width_start..i], 10) catch return Value.nil.raw;
        }

        if (i + 2 < bytes.len and bytes[i] == ',' and bytes[i + 1] == '\'') {
            pad = bytes[i + 2];
            i += 3;
        }
        if (i >= bytes.len) return Value.nil.raw;

        const directive = std.ascii.toLower(bytes[i]);
        i += 1;
        switch (directive) {
            'd' => {
                if (argc != 1 or arg_used) return Value.nil.raw;
                const arg = Value{ .raw = arg_raw };
                if (!arg.isFixnum()) return Value.nil.raw;
                const n = arg.toFixnum();

                var num_buf: [64]u8 = undefined;
                const printed = std.fmt.bufPrint(&num_buf, "{d}", .{n}) catch return Value.nil.raw;
                if (width) |w| {
                    if (printed.len < w) {
                        const pad_count = w - printed.len;
                        if (pad == '0' and printed[0] == '-') {
                            out.append(heap.backing_allocator, '-') catch return Value.nil.raw;
                            for (0..pad_count) |_| out.append(heap.backing_allocator, '0') catch return Value.nil.raw;
                            out.appendSlice(heap.backing_allocator, printed[1..]) catch return Value.nil.raw;
                        } else {
                            for (0..pad_count) |_| out.append(heap.backing_allocator, pad) catch return Value.nil.raw;
                            out.appendSlice(heap.backing_allocator, printed) catch return Value.nil.raw;
                        }
                    } else {
                        out.appendSlice(heap.backing_allocator, printed) catch return Value.nil.raw;
                    }
                } else {
                    out.appendSlice(heap.backing_allocator, printed) catch return Value.nil.raw;
                }
                arg_used = true;
            },
            else => return Value.nil.raw,
        }
    }

    const result = heap.allocBaseString(out.items) catch return Value.nil.raw;
    return result.raw;
}

/// Strip package prefix from a qualified name.
/// "COMMON-LISP:GCD" → "GCD", "CL-USER:FOO" → "FOO", "GCD" → "GCD"
fn stripPackagePrefix(name: []const u8) []const u8 {
    if (std.mem.indexOf(u8, name, ":")) |colon_pos| {
        return name[colon_pos + 1 ..];
    }
    return name;
}

/// Get function pointer for runtime primitive by name.
fn getJitPrimitivePtr(name: []const u8) ?u64 {
    return getJitPrimitivePtrWithArity(name, null);
}

fn getJitPrimitivePtrWithArity(name: []const u8, arity: ?usize) ?u64 {
    const bare = stripPackagePrefix(name);
    const Entry = struct { n: []const u8, p: *const anyopaque, a: usize };
    const table = [_]Entry{
        .{ .n = "NREVERSE", .p = @ptrCast(&jitNreverse), .a = 1 },
        .{ .n = "GCD", .p = @ptrCast(&jitGcd), .a = 2 },
        .{ .n = "GCD2", .p = @ptrCast(&jitGcd), .a = 2 },
        .{ .n = "APPEND", .p = @ptrCast(&jitAppend), .a = 2 },
        .{ .n = "%APPEND2", .p = @ptrCast(&jitAppend), .a = 2 },
        .{ .n = "ASSOC", .p = @ptrCast(&jitAssoc), .a = 2 },
        .{ .n = "FLOAT", .p = @ptrCast(&jitFloatCast), .a = 1 },
    };
    for (table) |entry| {
        if (std.mem.eql(u8, bare, entry.n)) {
            if (arity) |a| {
                if (a != entry.a) continue; // arity mismatch
            }
            return @intFromPtr(entry.p);
        }
    }
    return null;
}

/// Known JIT-compiled function info for cross-function calls.
/// Check if an IR node is a simple value (literal or variable reference).
/// Used to optimize TCO branches that just return a constant.
fn isSimpleValue(ir: *const Ir) bool {
    // Note: expanding this to include fixnum_add/sub causes incorrect results
    // due to hoist regalloc bug: block parameter register assignment fails
    // when jumping from a computation block directly to exit.
    return switch (ir.*) {
        .lit, .@"var", .global_ref => true,
        else => false,
    };
}

/// Check if a function body calls itself (has self-recursive calls or tailcalls).
/// Used to prevent inlining recursive functions.
// ── Generic IR tree walker ──
// Most analysis functions need the same recursive descent over IR nodes.
// This generic walker handles the structural recursion; callers only supply
// a predicate for leaf/call nodes.

/// Walk an IR tree depth-first, returning true if `pred` returns true for any node.
/// The predicate receives each node; structural children are walked automatically.
fn irAny(ir: *const Ir, pred: anytype) bool {
    if (pred.check(ir)) return true;
    return switch (ir.*) {
        .@"if" => |n| irAny(n.cond, pred) or irAny(n.then_branch, pred) or irAny(n.else_branch, pred),
        .block => |n| irAny(n.body, pred),
        .let => |n| blk: {
            for (n.bindings) |b| if (irAny(b.value, pred)) break :blk true;
            break :blk irAny(n.body, pred);
        },
        .set => |n| irAny(n.value, pred),
        .progn => |exprs| blk: {
            for (exprs) |e| if (irAny(e, pred)) break :blk true;
            break :blk false;
        },
        .loop => |n| irAny(n.cond, pred) or irAny(n.body, pred),
        .call => |c| blk: {
            for (c.args) |a| if (irAny(a, pred)) break :blk true;
            break :blk false;
        },
        .tailcall => |tc| blk: {
            for (tc.args) |a| if (irAny(a, pred)) break :blk true;
            break :blk false;
        },
        // Binary ops
        .fixnum_add,
        .fixnum_sub,
        .add,
        .sub,
        .fixnum_le,
        .fixnum_lt,
        .fixnum_gt,
        .fixnum_ge,
        .fixnum_eq,
        .le,
        .lt,
        .gt,
        .ge,
        .num_eq,
        .fixnum_mul,
        .mul,
        .eq,
        .cons,
        .logand,
        .mod,
        .rem,
        .append,
        .assoc,
        => |op| irAny(op.left, pred) or irAny(op.right, pred),
        // Unary ops
        .assert_fixnum,
        .nilp,
        .not,
        .consp,
        .abs,
        .car,
        .cdr,
        .unsafe_car,
        .unsafe_cdr,
        .zerop,
        .oddp,
        .evenp,
        .length,
        => |op| irAny(op.operand, pred),
        else => false,
    };
}

/// Check if a function body calls itself (has any call/tailcall nodes).
fn callsItself(body: *const Ir) bool {
    return irAny(body, struct {
        fn check(_: @This(), ir: *const Ir) bool {
            return ir.* == .call or ir.* == .tailcall;
        }
    }{});
}

/// Check if any cross-function calls in the body would inline code containing loads.
fn crossCallsContainLoads(body: *const Ir, self_name: []const u8, kf: *const std.StringHashMap(KnownFn)) bool {
    return irAny(body, struct {
        name: []const u8,
        known: *const std.StringHashMap(KnownFn),
        fn check(self: @This(), ir: *const Ir) bool {
            if (ir.* != .call) return false;
            const c = ir.call;
            const target_name = getCallTargetName(c.func) orelse return false;
            if (namesMatch(target_name, self.name)) return false;
            const kfn = lookupKnownFnByName(self.known, target_name) orelse return false;
            const ir_body = kfn.ir_body orelse return false;
            return countIrNodes(ir_body) <= 30 and containsLoads(ir_body);
        }
    }{ .name = self_name, .known = kf });
}

/// Lookup a KnownFn by name, handling package-qualified names.
fn lookupKnownFnByName(kf: *const std.StringHashMap(KnownFn), target_name: []const u8) ?KnownFn {
    if (kf.get(target_name)) |v| return v;
    if (std.mem.indexOfScalar(u8, target_name, ':')) |colon_pos| {
        if (kf.get(target_name[colon_pos + 1 ..])) |v| return v;
    }
    var it = kf.iterator();
    while (it.next()) |entry| {
        if (namesMatch(target_name, entry.key_ptr.*)) return entry.value_ptr.*;
    }
    return null;
}

/// Count IR nodes (for inlining threshold decisions).
fn countIrNodes(node: *const Ir) usize {
    var count: usize = 1;
    switch (node.*) {
        .@"if" => |n| count += countIrNodes(n.cond) + countIrNodes(n.then_branch) + countIrNodes(n.else_branch),
        .block => |n| count += countIrNodes(n.body),
        .let => |n| {
            for (n.bindings) |b| count += countIrNodes(b.value);
            count += countIrNodes(n.body);
        },
        .set => |n| count += countIrNodes(n.value),
        .progn => |exprs| for (exprs) |e| {
            count += countIrNodes(e);
        },
        .loop => |n| count += countIrNodes(n.cond) + countIrNodes(n.body),
        .call => |c| {
            count += countIrNodes(c.func);
            for (c.args) |a| count += countIrNodes(a);
        },
        .tailcall => |tc| {
            count += countIrNodes(tc.func);
            for (tc.args) |a| count += countIrNodes(a);
        },
        .fixnum_add,
        .fixnum_sub,
        .add,
        .sub,
        .fixnum_le,
        .fixnum_lt,
        .fixnum_gt,
        .fixnum_ge,
        .fixnum_eq,
        .le,
        .lt,
        .gt,
        .ge,
        .num_eq,
        .fixnum_mul,
        .mul,
        .eq,
        .cons,
        .logand,
        .mod,
        .rem,
        .append,
        .assoc,
        => |op| count += countIrNodes(op.left) + countIrNodes(op.right),
        .assert_fixnum,
        .nilp,
        .not,
        .consp,
        .abs,
        .car,
        .cdr,
        .unsafe_car,
        .unsafe_cdr,
        .zerop,
        .oddp,
        .evenp,
        .length,
        => |op| count += countIrNodes(op.operand),
        else => {},
    }
    return count;
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
    /// Continuation stack buffer (heap-allocated, used by cont-stack JIT).
    cont_buf: ?[]align(8) u8 = null,

    pub fn deinit(self: *CompiledFn) void {
        if (self.name.len > 0) self.allocator.free(self.name);
        if (self.ir_arena) |arena| {
            arena.deinit();
            self.allocator.destroy(arena);
        }
        if (self.cont_buf) |buf| self.allocator.free(buf);
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
            4 => blk: {
                const f: *const fn (i64, i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1]), @bitCast(args[2]), @bitCast(args[3])));
            },
            5 => blk: {
                const f: *const fn (i64, i64, i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1]), @bitCast(args[2]), @bitCast(args[3]), @bitCast(args[4])));
            },
            6 => blk: {
                const f: *const fn (i64, i64, i64, i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1]), @bitCast(args[2]), @bitCast(args[3]), @bitCast(args[4]), @bitCast(args[5])));
            },
            7 => blk: {
                const f: *const fn (i64, i64, i64, i64, i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1]), @bitCast(args[2]), @bitCast(args[3]), @bitCast(args[4]), @bitCast(args[5]), @bitCast(args[6])));
            },
            8 => blk: {
                const f: *const fn (i64, i64, i64, i64, i64, i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
                break :blk @bitCast(f(@bitCast(args[0]), @bitCast(args[1]), @bitCast(args[2]), @bitCast(args[3]), @bitCast(args[4]), @bitCast(args[5]), @bitCast(args[6]), @bitCast(args[7])));
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

    /// Call with 4 tagged i64 args, returns tagged i64.
    pub fn call4(self: *const CompiledFn, a: i64, b: i64, c: i64, d: i64) i64 {
        const f: *const fn (i64, i64, i64, i64) callconv(.c) i64 = @ptrCast(@alignCast(self.fn_ptr));
        return f(a, b, c, d);
    }
};

/// Translates Habu IR to Hoist SSA IR and compiles to native code.
const CseOp = enum(u8) { iadd, isub, imul, band, bor };
const CseKey = struct {
    op: CseOp,
    lhs: u32,
    rhs: u32,
};

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

    /// Whether all self-calls were converted to jumps (continuation stack).
    /// When true, no call_indirect was emitted, so self-pointer patching is skipped.
    all_calls_converted: bool,

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

    /// Simple CSE cache: (op, lhs.val, rhs.val) → result.
    /// Only caches within the same block (cleared on block switch).
    /// Eliminates duplicate computations like (+ i 1) in loop bodies.
    cse_cache: std.AutoHashMap(CseKey, HoistValue),

    /// True when the function has nested self-calls (e.g., tak pattern)
    /// that require post-emission parallel copy fixup for call arguments.
    needs_call_spill: bool = false,

    /// Map of known JIT-compiled function names → (fn_ptr, arity).
    /// Used for cross-function calls via call_indirect.
    known_fns: ?*const std.StringHashMap(KnownFn) = null,

    /// Maps IR node pointer -> GC-rooted literal slot for pointer literals.
    literal_roots: ?*const LiteralRoots = null,

    /// Whether the function has cross-function calls (non-self call_indirect).
    has_cross_calls: bool = false,

    /// Cache for cross-function call signatures (keyed by arity).
    call_sigs: [8]?SigRef = .{null} ** 8,

    /// When true, all internal values are untagged plain i64.
    /// Params are untagged at entry, result is re-tagged at return.
    /// Self-calls use untagged convention (no tag/untag at call boundary).
    untagged: bool = false,
    /// Allow recursive fixnum-fast lowering (safety=0 only).
    fixnum_fast: bool = false,

    /// When true, emit fresh small constants in blocks containing calls.
    /// Hoist's LICM moves cached constants to block0, forcing callee-saved regs.
    /// We still cache in the loop header (hot path), but emit fresh copies in
    /// blocks that contain call_indirect to avoid cross-call liveness.
    local_consts: bool = false,

    /// Tracks whether the current block will contain a call_indirect.
    /// Set before translating a block that contains non-tail self-calls.
    in_call_block: bool = false,
    /// Set during translateLoop's LICM preEmitConstants to enable hoisting
    /// of inline cons constants (g_alloc_ptr, 16, 8) out of loops.
    in_loop_preemit: bool = false,

    /// Whether the function contains load instructions (car, cdr).
    /// .aggressive optimization incorrectly eliminates loads.
    has_loads: bool = false,

    /// TCO: loop header block — tail calls jump here with updated params.
    tco_header: ?Block = null,
    /// TCO: exit block — non-tail returns jump here with result value.
    tco_exit: ?Block = null,

    /// Continuation stack: eliminates non-tail self-calls by converting them
    /// to explicit stack pushes. The continuation stack depth is a phi param
    /// on the header block. Stores/loads use cont_base + depth * 8.
    cont_depth_phi: ?HoistValue = null,
    cont_base: ?HoistValue = null,
    // cont_slot removed — depth passed as exit block param after hoist parallel copy fix
    /// Number of continuation values pushed per self-call (e.g., 1 for ack's m-1).
    cont_width: u32 = 0,
    /// Heap-allocated continuation stack buffer (ownership transferred to CompiledFn).
    cont_buf_alloc: ?[]align(8) u8 = null,

    pub fn init(allocator: std.mem.Allocator, func: *Function, builder: *FunctionBuilder) IrTranslator {
        return .{
            .allocator = allocator,
            .func = func,
            .b = builder,
            .locals = std.ArrayList(HoistValue){},
            .inline_scopes = std.ArrayList(InlineScope){},
            .is_recursive = false,
            .all_calls_converted = false,
            .has_loops = false,
            .fn_name = "",
            .user_arity = 0,
            .self_ptr_placeholder = 0x0BADF00DDEADBEEF,
            .self_sig_ref = SigRef.new(0),
            .const_cache = std.AutoHashMap(i64, HoistValue).init(allocator),
            .cse_cache = std.AutoHashMap(CseKey, HoistValue).init(allocator),
        };
    }

    pub fn deinit(self: *IrTranslator) void {
        self.locals.deinit(self.allocator);
        self.inline_scopes.deinit(self.allocator);
        self.const_cache.deinit();
        self.cse_cache.deinit();
    }

    /// Check CSE cache for a binary operation result.
    fn cseLookup(self: *IrTranslator, op: CseOp, lhs: HoistValue, rhs: HoistValue) ?HoistValue {
        return self.cse_cache.get(.{ .op = op, .lhs = lhs.index, .rhs = rhs.index });
    }

    /// Record a binary operation result in the CSE cache.
    fn cseRecord(self: *IrTranslator, op: CseOp, lhs: HoistValue, rhs: HoistValue, result: HoistValue) void {
        self.cse_cache.put(.{ .op = op, .lhs = lhs.index, .rhs = rhs.index }, result) catch {};
    }

    /// Switch to a new block, clearing the CSE cache (SSA values don't dominate across blocks).
    fn switchBlock(self: *IrTranslator, blk: anytype) void {
        self.b.switchToBlock(blk);
        self.cse_cache.clearRetainingCapacity();
        if (self.local_consts) {
            // local_consts mode intentionally keeps constants block-local to avoid
            // long live ranges across calls and to preserve SSA dominance.
            self.const_cache.clearRetainingCapacity();
        }
    }

    /// Emit an iconst, reusing a previously emitted value for the same constant.
    /// This provides LICM for loop-invariant constants: a constant emitted in the
    /// entry block is reusable in all subsequent blocks (SSA dominance).
    ///
    /// When `local_consts` is set (functions with calls), small constants
    /// (fitting in a single MOV immediate) are emitted fresh at each use-site
    /// to avoid hoist LICM hoisting them to block0 and forcing callee-saved regs.
    /// Large constants (function pointers, 3+ instruction MOVZ/MOVK sequences)
    /// are still cached since saving/restoring is cheaper than rematerializing.
    fn cachedIconst(self: *IrTranslator, val: i64) !HoistValue {
        // When local_consts is set, don't cache small constants at all.
        // This ensures each block gets its own iconst that the regalloc
        // can handle locally, avoiding callee-saved register pressure.
        // Large constants (function pointers: >16-bit) are still cached
        // since 3-instruction MOVZ+MOVK+MOVK is expensive to rematerialize.
        if (self.local_consts) {
            const uval: u64 = @bitCast(val);
            if (uval <= 0xFFFF or (~uval) <= 0xFFFF) {
                return try self.b.iconst(I64, val);
            }
        }
        if (self.const_cache.get(val)) |cached| return cached;
        const result = try self.b.iconst(I64, val);
        try self.const_cache.put(val, result);
        return result;
    }

    fn hasLiteralRoot(literal_roots: ?*const LiteralRoots, ir: *const Ir) bool {
        if (literal_roots) |roots| {
            return roots.contains(@intFromPtr(ir));
        }
        return false;
    }

    fn litNeedsRoot(v: Value) bool {
        return v.isPointer() and !v.isMagicSymbol();
    }

    /// Fast check: can we translate all nodes in this IR tree?
    /// Returns false if any unsupported node is found.
    pub fn canTranslate(ir: *const Ir) bool {
        return canTranslateWithLiteralRoots(ir, null);
    }

    pub fn canTranslateWithLiteralRoots(ir: *const Ir, literal_roots: ?*const LiteralRoots) bool {
        return switch (ir.*) {
            .lit => |v| blk: {
                if (!litNeedsRoot(v)) break :blk true;
                if (literal_roots == null) break :blk true;
                break :blk hasLiteralRoot(literal_roots, ir);
            },
            .@"var", .global_ref => true,
            .lambda => |lam| blk: {
                if (!hasLiteralRoot(literal_roots, ir)) break :blk false;
                if (lam.captures.len != 0) break :blk false;
                if (lam.optional_params.len != 0) break :blk false;
                if (lam.key_params.len != 0) break :blk false;
                if (lam.rest_param != null) break :blk false;
                break :blk canTranslateWithLiteralRoots(lam.body, literal_roots);
            },
            .fixnum_add, .fixnum_sub, .add, .sub => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .fixnum_le,
            .fixnum_lt,
            .fixnum_gt,
            .fixnum_ge,
            .fixnum_eq,
            .le,
            .lt,
            .gt,
            .ge,
            .num_eq,
            => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .@"if" => |f| canTranslateWithLiteralRoots(f.cond, literal_roots) and canTranslateWithLiteralRoots(f.then_branch, literal_roots) and canTranslateWithLiteralRoots(f.else_branch, literal_roots),
            .block => |b| canTranslateWithLiteralRoots(b.body, literal_roots),
            .progn => |exprs| {
                for (exprs) |e| if (!canTranslateWithLiteralRoots(e, literal_roots)) return false;
                return true;
            },
            .let => |l| {
                for (l.bindings) |b| if (!canTranslateWithLiteralRoots(b.value, literal_roots)) return false;
                return canTranslateWithLiteralRoots(l.body, literal_roots);
            },
            .set => |s| canTranslateWithLiteralRoots(s.value, literal_roots),
            .loop => |l| canTranslateWithLiteralRoots(l.cond, literal_roots) and canTranslateWithLiteralRoots(l.body, literal_roots),
            .assert_fixnum => |op| canTranslateWithLiteralRoots(op.operand, literal_roots),
            .call => |c| {
                if (!canTranslateWithLiteralRoots(c.func, literal_roots)) return false;
                for (c.args) |a| if (!canTranslateWithLiteralRoots(a, literal_roots)) return false;
                return true;
            },
            .tailcall => |tc| {
                if (!canTranslateWithLiteralRoots(tc.func, literal_roots)) return false;
                for (tc.args) |a| if (!canTranslateWithLiteralRoots(a, literal_roots)) return false;
                return true;
            },
            .fixnum_mul => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .mul => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .eq => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            // List / predicate operations (inline, no heap access needed)
            .nilp, .not, .consp, .abs => |op| canTranslateWithLiteralRoots(op.operand, literal_roots),
            .zerop, .oddp, .evenp => |op| canTranslateWithLiteralRoots(op.operand, literal_roots),
            .car, .cdr, .unsafe_car, .unsafe_cdr => |op| canTranslateWithLiteralRoots(op.operand, literal_roots),
            .length => |op| canTranslateWithLiteralRoots(op.operand, literal_roots),
            .cons => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .sqrt, .round, .intern, .vec_len, .str_len => |op| canTranslateWithLiteralRoots(op.operand, literal_roots),
            .vec_new => |v| blk: {
                if (!canTranslateWithLiteralRoots(v.size, literal_roots)) break :blk false;
                if (v.init) |init_val| break :blk canTranslateWithLiteralRoots(init_val, literal_roots);
                break :blk true;
            },
            .vec_ref, .str_ref, .str_concat => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .vec_set => |v| canTranslateWithLiteralRoots(v.vec, literal_roots) and canTranslateWithLiteralRoots(v.index, literal_roots) and canTranslateWithLiteralRoots(v.value, literal_roots),
            .hash_count => |h| canTranslateWithLiteralRoots(h.operand, literal_roots),
            .hash_capacity => |h| canTranslateWithLiteralRoots(h.operand, literal_roots),
            .hash_clear => |h| canTranslateWithLiteralRoots(h.operand, literal_roots),
            .hash_test => |h| canTranslateWithLiteralRoots(h.operand, literal_roots),
            .hash_keys => |h| canTranslateWithLiteralRoots(h.operand, literal_roots),
            .hash_alist => |h| canTranslateWithLiteralRoots(h.operand, literal_roots),
            .make_hash => true,
            .hash_get => |h| canTranslateWithLiteralRoots(h.table, literal_roots) and canTranslateWithLiteralRoots(h.key, literal_roots) and
                (if (h.default) |d| canTranslateWithLiteralRoots(d, literal_roots) else true),
            .hash_set => |h| canTranslateWithLiteralRoots(h.table, literal_roots) and canTranslateWithLiteralRoots(h.key, literal_roots) and canTranslateWithLiteralRoots(h.value, literal_roots),
            .hash_rem => |h| canTranslateWithLiteralRoots(h.table, literal_roots) and canTranslateWithLiteralRoots(h.key, literal_roots),
            .format => |f| blk: {
                if (f.args.len > 1) break :blk false;
                if (!canTranslateWithLiteralRoots(f.dest, literal_roots) or !canTranslateWithLiteralRoots(f.control, literal_roots)) break :blk false;
                for (f.args) |a| if (!canTranslateWithLiteralRoots(a, literal_roots)) break :blk false;
                break :blk true;
            },
            .make_string => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .str_set => |s| canTranslateWithLiteralRoots(s.str, literal_roots) and canTranslateWithLiteralRoots(s.index, literal_roots) and canTranslateWithLiteralRoots(s.value, literal_roots),
            .substring => |s| canTranslateWithLiteralRoots(s.str, literal_roots) and canTranslateWithLiteralRoots(s.start, literal_roots) and canTranslateWithLiteralRoots(s.end, literal_roots),
            .position, .position_eq, .position_equal => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .arr_new => |a| blk: {
                if (a.dimensions.len > 8) break :blk false;
                for (a.dimensions) |d| if (!canTranslateWithLiteralRoots(d, literal_roots)) break :blk false;
                if (a.init) |v| break :blk canTranslateWithLiteralRoots(v, literal_roots);
                break :blk true;
            },
            .arr_new_dyn => |a| blk: {
                if (!canTranslateWithLiteralRoots(a.dimensions, literal_roots)) break :blk false;
                if (a.init) |v| break :blk canTranslateWithLiteralRoots(v, literal_roots);
                break :blk true;
            },
            .arr_ref => |a| blk: {
                if (a.subscripts.len < 1 or a.subscripts.len > 2) break :blk false;
                if (!canTranslateWithLiteralRoots(a.array, literal_roots)) break :blk false;
                for (a.subscripts) |s| if (!canTranslateWithLiteralRoots(s, literal_roots)) break :blk false;
                break :blk true;
            },
            .arr_set => |a| blk: {
                if (a.subscripts.len < 1 or a.subscripts.len > 2) break :blk false;
                if (!canTranslateWithLiteralRoots(a.array, literal_roots)) break :blk false;
                if (!canTranslateWithLiteralRoots(a.value, literal_roots)) break :blk false;
                for (a.subscripts) |s| if (!canTranslateWithLiteralRoots(s, literal_roots)) break :blk false;
                break :blk true;
            },
            .logand => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .mod, .rem => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .append => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            .assoc => |op| canTranslateWithLiteralRoots(op.left, literal_roots) and canTranslateWithLiteralRoots(op.right, literal_roots),
            else => false,
        };
    }

    /// Return the first unsupported node tag in depth-first order.
    /// Useful for JIT coverage diagnostics when canTranslate() rejects a body.
    pub fn firstUnsupportedTag(ir: *const Ir) ?std.meta.Tag(Ir) {
        return firstUnsupportedTagWithLiteralRoots(ir, null);
    }

    pub fn firstUnsupportedTagWithLiteralRoots(ir: *const Ir, literal_roots: ?*const LiteralRoots) ?std.meta.Tag(Ir) {
        return switch (ir.*) {
            .lit => |v| blk: {
                if (!litNeedsRoot(v)) break :blk null;
                if (literal_roots == null) break :blk null;
                if (!hasLiteralRoot(literal_roots, ir)) break :blk .lit;
                break :blk null;
            },
            .@"var", .global_ref => null,
            .lambda => |lam| blk: {
                if (!hasLiteralRoot(literal_roots, ir)) break :blk .lambda;
                if (lam.captures.len != 0) break :blk .lambda;
                if (lam.optional_params.len != 0) break :blk .lambda;
                if (lam.key_params.len != 0) break :blk .lambda;
                if (lam.rest_param != null) break :blk .lambda;
                break :blk firstUnsupportedTagWithLiteralRoots(lam.body, literal_roots);
            },
            .fixnum_add,
            .fixnum_sub,
            .add,
            .sub,
            .fixnum_le,
            .fixnum_lt,
            .fixnum_gt,
            .fixnum_ge,
            .fixnum_eq,
            .le,
            .lt,
            .gt,
            .ge,
            .num_eq,
            .fixnum_mul,
            .mul,
            .eq,
            .cons,
            .logand,
            .mod,
            .rem,
            .append,
            .assoc,
            => |op| firstUnsupportedTagWithLiteralRoots(op.left, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(op.right, literal_roots),
            .@"if" => |f| firstUnsupportedTagWithLiteralRoots(f.cond, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(f.then_branch, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(f.else_branch, literal_roots),
            .block => |b| firstUnsupportedTagWithLiteralRoots(b.body, literal_roots),
            .progn => |exprs| blk: {
                for (exprs) |e| if (firstUnsupportedTagWithLiteralRoots(e, literal_roots)) |tag| break :blk tag;
                break :blk null;
            },
            .let => |l| blk: {
                for (l.bindings) |b| if (firstUnsupportedTagWithLiteralRoots(b.value, literal_roots)) |tag| break :blk tag;
                break :blk firstUnsupportedTagWithLiteralRoots(l.body, literal_roots);
            },
            .set => |s| firstUnsupportedTagWithLiteralRoots(s.value, literal_roots),
            .loop => |l| firstUnsupportedTagWithLiteralRoots(l.cond, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(l.body, literal_roots),
            .assert_fixnum => |op| firstUnsupportedTagWithLiteralRoots(op.operand, literal_roots),
            .call => |c| blk: {
                if (firstUnsupportedTagWithLiteralRoots(c.func, literal_roots)) |tag| break :blk tag;
                for (c.args) |a| if (firstUnsupportedTagWithLiteralRoots(a, literal_roots)) |tag| break :blk tag;
                break :blk null;
            },
            .tailcall => |tc| blk: {
                if (firstUnsupportedTagWithLiteralRoots(tc.func, literal_roots)) |tag| break :blk tag;
                for (tc.args) |a| if (firstUnsupportedTagWithLiteralRoots(a, literal_roots)) |tag| break :blk tag;
                break :blk null;
            },
            .nilp,
            .not,
            .consp,
            .abs,
            .zerop,
            .oddp,
            .evenp,
            .car,
            .cdr,
            .unsafe_car,
            .unsafe_cdr,
            .length,
            .sqrt,
            .round,
            .intern,
            .vec_len,
            .str_len,
            => |op| firstUnsupportedTagWithLiteralRoots(op.operand, literal_roots),
            .vec_new => |v| blk: {
                if (firstUnsupportedTagWithLiteralRoots(v.size, literal_roots)) |tag| break :blk tag;
                if (v.init) |init_val| break :blk firstUnsupportedTagWithLiteralRoots(init_val, literal_roots);
                break :blk null;
            },
            .vec_ref, .str_ref, .str_concat => |op| firstUnsupportedTagWithLiteralRoots(op.left, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(op.right, literal_roots),
            .vec_set => |v| firstUnsupportedTagWithLiteralRoots(v.vec, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(v.index, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(v.value, literal_roots),
            .hash_count => |h| firstUnsupportedTagWithLiteralRoots(h.operand, literal_roots),
            .hash_capacity => |h| firstUnsupportedTagWithLiteralRoots(h.operand, literal_roots),
            .hash_clear => |h| firstUnsupportedTagWithLiteralRoots(h.operand, literal_roots),
            .hash_test => |h| firstUnsupportedTagWithLiteralRoots(h.operand, literal_roots),
            .hash_keys => |h| firstUnsupportedTagWithLiteralRoots(h.operand, literal_roots),
            .hash_alist => |h| firstUnsupportedTagWithLiteralRoots(h.operand, literal_roots),
            .make_hash => null,
            .hash_get => |h| firstUnsupportedTagWithLiteralRoots(h.table, literal_roots) orelse
                firstUnsupportedTagWithLiteralRoots(h.key, literal_roots) orelse
                (if (h.default) |d| firstUnsupportedTagWithLiteralRoots(d, literal_roots) else null),
            .hash_set => |h| firstUnsupportedTagWithLiteralRoots(h.table, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(h.key, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(h.value, literal_roots),
            .hash_rem => |h| firstUnsupportedTagWithLiteralRoots(h.table, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(h.key, literal_roots),
            .format => |f| blk: {
                if (f.args.len > 1) break :blk .format;
                if (firstUnsupportedTagWithLiteralRoots(f.dest, literal_roots)) |tag| break :blk tag;
                if (firstUnsupportedTagWithLiteralRoots(f.control, literal_roots)) |tag| break :blk tag;
                for (f.args) |a| if (firstUnsupportedTagWithLiteralRoots(a, literal_roots)) |tag| break :blk tag;
                break :blk null;
            },
            .make_string => |op| firstUnsupportedTagWithLiteralRoots(op.left, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(op.right, literal_roots),
            .str_set => |s| firstUnsupportedTagWithLiteralRoots(s.str, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(s.index, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(s.value, literal_roots),
            .substring => |s| firstUnsupportedTagWithLiteralRoots(s.str, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(s.start, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(s.end, literal_roots),
            .position, .position_eq, .position_equal => |op| firstUnsupportedTagWithLiteralRoots(op.left, literal_roots) orelse firstUnsupportedTagWithLiteralRoots(op.right, literal_roots),
            .arr_new => |a| blk: {
                if (a.dimensions.len > 8) break :blk .arr_new;
                for (a.dimensions) |d| if (firstUnsupportedTagWithLiteralRoots(d, literal_roots)) |tag| break :blk tag;
                if (a.init) |v| break :blk firstUnsupportedTagWithLiteralRoots(v, literal_roots);
                break :blk null;
            },
            .arr_new_dyn => |a| blk: {
                if (firstUnsupportedTagWithLiteralRoots(a.dimensions, literal_roots)) |tag| break :blk tag;
                if (a.init) |v| break :blk firstUnsupportedTagWithLiteralRoots(v, literal_roots);
                break :blk null;
            },
            .arr_ref => |a| blk: {
                if (a.subscripts.len < 1 or a.subscripts.len > 2) break :blk .arr_ref;
                if (firstUnsupportedTagWithLiteralRoots(a.array, literal_roots)) |tag| break :blk tag;
                for (a.subscripts) |s| if (firstUnsupportedTagWithLiteralRoots(s, literal_roots)) |tag| break :blk tag;
                break :blk null;
            },
            .arr_set => |a| blk: {
                if (a.subscripts.len < 1 or a.subscripts.len > 2) break :blk .arr_set;
                if (firstUnsupportedTagWithLiteralRoots(a.array, literal_roots)) |tag| break :blk tag;
                if (firstUnsupportedTagWithLiteralRoots(a.value, literal_roots)) |tag| break :blk tag;
                for (a.subscripts) |s| if (firstUnsupportedTagWithLiteralRoots(s, literal_roots)) |tag| break :blk tag;
                break :blk null;
            },
            else => std.meta.activeTag(ir.*),
        };
    }

    /// Translate a Habu IR node to Hoist SSA, returning the SSA value produced.
    pub fn translate(self: *IrTranslator, ir: *const Ir) anyerror!HoistValue {
        return switch (ir.*) {
            .lit => |v| try self.translateLit(ir, v),
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
            .add => |op| try self.translateAdd(op.left, op.right),
            .sub => |op| try self.translateSub(op.left, op.right),
            .le => |op| try self.translateLe(op.left, op.right),
            .lt => |op| try self.translateLt(op.left, op.right),
            .gt => |op| try self.translateGt(op.left, op.right),
            .ge => |op| try self.translateGe(op.left, op.right),
            .num_eq => |op| try self.translateNumEq(op.left, op.right),
            .eq => |op| try self.translateFixnumCmp(.eq, op.left, op.right),
            .fixnum_mul => |op| try self.translateFixnumMul(op.left, op.right),
            .mul => |op| try self.translateMul(op.left, op.right),
            .block => |b| try self.translate(b.body),
            .@"if" => |if_node| try self.translateIf(if_node.cond, if_node.then_branch, if_node.else_branch),
            .progn => |exprs| try self.translateProgn(exprs),
            .let => |let_node| try self.translateLet(let_node.bindings, let_node.body),
            .set => |set_node| try self.translateSet(set_node.index, set_node.value),
            .loop => |loop_node| try self.translateLoop(loop_node.cond, loop_node.body),
            .assert_fixnum => |op| try self.translate(op.operand), // At safety 0, just pass through
            .global_ref => |_| try self.translateLit(ir, Value.nil), // TODO: general global refs
            .lambda => |_| try self.translateLambdaLiteral(ir),
            // List / predicate operations (inline, no heap access needed)
            .nilp => |op| try self.translateNilp(op.operand),
            .not => |op| try self.translateNot(op.operand),
            .consp => |op| try self.translateConsp(op.operand),
            .car => |op| try self.translateCar(op.operand),
            .cdr => |op| try self.translateCdr(op.operand),
            .unsafe_car => |op| try self.translateUnsafeCar(op.operand),
            .unsafe_cdr => |op| try self.translateUnsafeCdr(op.operand),
            .abs => |op| try self.translateAbs(op.operand),
            .zerop => |op| try self.translateZerop(op.operand),
            .oddp => |op| try self.translateOddp(op.operand),
            .evenp => |op| try self.translateEvenp(op.operand),
            .length => |op| try self.translateLength(op.operand),
            .sqrt => |op| try self.translateSqrt(op.operand),
            .round => |op| try self.translateRound(op.operand),
            .vec_new => |v| try self.translateVecNew(v.size, v.init),
            .vec_ref => |op| try self.translateVecRef(op.left, op.right),
            .vec_set => |v| try self.translateVecSet(v.vec, v.index, v.value),
            .vec_len => |op| try self.translateVecLen(op.operand),
            .make_hash => |h| try self.translateMakeHash(h.capacity, h.test_type),
            .hash_get => |h| try self.translateHashGet(h.table, h.key, h.default),
            .hash_set => |h| try self.translateHashSet(h.table, h.key, h.value),
            .hash_rem => |h| try self.translateHashRem(h.table, h.key),
            .hash_count => |h| try self.translateHashCount(h.operand),
            .hash_capacity => |h| try self.translateHashCapacity(h.operand),
            .hash_clear => |h| try self.translateHashClear(h.operand),
            .hash_test => |h| try self.translateHashTest(h.operand),
            .hash_keys => |h| try self.translateHashKeys(h.operand),
            .hash_alist => |h| try self.translateHashAlist(h.operand),
            .format => |f| try self.translateFormat(f.dest, f.control, f.args),
            .make_string => |op| try self.translateMakeString(op.left, op.right),
            .str_ref => |op| try self.translateStrRef(op.left, op.right),
            .str_len => |op| try self.translateStrLen(op.operand),
            .str_set => |s| try self.translateStrSet(s.str, s.index, s.value),
            .str_concat => |op| try self.translateStrConcat(op.left, op.right),
            .substring => |s| try self.translateSubstring(s.str, s.start, s.end),
            .position, .position_eq, .position_equal => |op| try self.translatePosition(op.left, op.right),
            .intern => |op| try self.translateIntern(op.operand),
            .arr_new => |a| try self.translateArrNew(a.dimensions, a.init),
            .arr_new_dyn => |a| try self.translateArrNewDynamic(a.dimensions, a.init),
            .arr_ref => |a| try self.translateArrRef(a.array, a.subscripts),
            .arr_set => |a| try self.translateArrSet(a.array, a.subscripts, a.value),
            // Binary: bitwise/modular
            .logand => |op| try self.translateLogand(op.left, op.right),
            .mod => |op| try self.translateMod(op.left, op.right),
            .rem => |op| try self.translateRem(op.left, op.right),
            // Heap allocation (calls C-ABI runtime function)
            .cons => |op| try self.translateCons(op.left, op.right),
            .append => |op| try self.translateAppend(op.left, op.right),
            .assoc => |op| try self.translateAssoc(op.left, op.right),
            .call => |call_node| try self.translateCall(call_node.func, call_node.args),
            .tailcall => |tc| try self.translateCall(tc.func, tc.args),
            else => {
                return error.UnsupportedIrNode;
            },
        };
    }

    fn translateLit(self: *IrTranslator, ir: *const Ir, val: Value) anyerror!HoistValue {
        if (self.untagged and val.isFixnum()) {
            return try self.cachedIconst(val.toFixnum());
        }
        if (litNeedsRoot(val)) {
            if (self.literal_roots) |roots| {
                const slot = roots.get(@intFromPtr(ir)) orelse return error.UnsupportedIrNode;
                const slot_addr = try self.cachedIconst(@as(i64, @bitCast(@intFromPtr(slot))));
                return try self.b.load(I64, slot_addr, MemFlags.default());
            }
        }
        return try self.cachedIconst(@as(i64, @bitCast(val.raw)));
    }

    fn translateLambdaLiteral(self: *IrTranslator, ir: *const Ir) anyerror!HoistValue {
        const roots = self.literal_roots orelse return error.UnsupportedIrNode;
        const slot = roots.get(@intFromPtr(ir)) orelse return error.UnsupportedIrNode;
        const slot_addr = try self.cachedIconst(@as(i64, @bitCast(@intFromPtr(slot))));
        return try self.b.load(I64, slot_addr, MemFlags.default());
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

    fn translateAdd(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumAdd(left, right);
        const l = try self.translate(left);
        const r = try self.translate(right);
        const args = [_]HoistValue{ l, r };
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAddNum)));
        return try self.emitPrimitiveCallValues(prim_ptr, &args);
    }

    fn translateSub(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumSub(left, right);
        const l = try self.translate(left);
        const r = try self.translate(right);
        const args = [_]HoistValue{ l, r };
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitSubNum)));
        return try self.emitPrimitiveCallValues(prim_ptr, &args);
    }

    fn translateMul(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumMul(left, right);
        const l = try self.translate(left);
        const r = try self.translate(right);
        const args = [_]HoistValue{ l, r };
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMulNum)));
        return try self.emitPrimitiveCallValues(prim_ptr, &args);
    }

    fn translateCmpTagged(self: *IrTranslator, prim_ptr: u64, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        const l = try self.translate(left);
        const r = try self.translate(right);
        const args = [_]HoistValue{ l, r };
        const tagged = try self.emitPrimitiveCallValues(prim_ptr, &args);
        const zero = try self.cachedIconst(0);
        return try self.b.icmp(I8, IntCC.ne, tagged, zero);
    }

    fn translateLt(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumCmp(.slt, left, right);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitLtNum)));
        return try self.translateCmpTagged(prim_ptr, left, right);
    }

    fn translateGt(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumCmp(.sgt, left, right);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitGtNum)));
        return try self.translateCmpTagged(prim_ptr, left, right);
    }

    fn translateLe(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumCmp(.sle, left, right);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitLeNum)));
        return try self.translateCmpTagged(prim_ptr, left, right);
    }

    fn translateGe(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumCmp(.sge, left, right);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitGeNum)));
        return try self.translateCmpTagged(prim_ptr, left, right);
    }

    fn translateNumEq(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged or (self.is_recursive and self.fixnum_fast)) return self.translateFixnumCmp(.eq, left, right);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitNumEq)));
        return try self.translateCmpTagged(prim_ptr, left, right);
    }

    fn translateFixnumAdd(self: *IrTranslator, left: *const Ir, right: *const Ir) anyerror!HoistValue {
        if (self.untagged) {
            const l = try self.translate(left);
            const r = try self.translate(right);
            if (self.cseLookup(.iadd, l, r)) |cached| return cached;
            const result = try self.b.iadd(I64, l, r);
            self.cseRecord(.iadd, l, r, result);
            return result;
        }
        // Tagged fixnum add: result_raw = l_raw + r_raw - 1
        // When one operand is a constant, fold: iadd(x, const - 1)
        if (getFixnumLit(right)) |r_const| {
            const l = try self.translate(left);
            const folded = try self.cachedIconst(r_const - 1);
            if (self.cseLookup(.iadd, l, folded)) |cached| return cached;
            const result = try self.b.iadd(I64, l, folded);
            self.cseRecord(.iadd, l, folded, result);
            return result;
        }
        if (getFixnumLit(left)) |l_const| {
            const r = try self.translate(right);
            const folded = try self.cachedIconst(l_const - 1);
            if (self.cseLookup(.iadd, r, folded)) |cached| return cached;
            const result = try self.b.iadd(I64, r, folded);
            self.cseRecord(.iadd, r, folded, result);
            return result;
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
            // Try strength reduction for multiply-by-constant (untagged)
            // In untagged mode, getFixnumLit returns raw tagged value;
            // the actual numeric value is raw >> 1 (fixnum tag is bit 0).
            if (getFixnumLit(right)) |r_raw| {
                const val = @as(i64, @intCast(r_raw >> 1));
                if (try self.mulByConst(l, val)) |result| return result;
            }
            if (getFixnumLit(left)) |l_raw| {
                const val = @as(i64, @intCast(l_raw >> 1));
                if (try self.mulByConst(r, val)) |result| return result;
            }
            return try self.b.imul(I64, l, r);
        }
        // Tagged fixnum mul: result = sshr(a, 1) * (b - 1) + 1
        // Proof: a=2va+1, b=2vb+1. sshr(a,1)=va. b-1=2vb. va*2vb=2*va*vb. +1 = tagged(va*vb).
        // When one operand is a constant, fold: sshr(a,1) * (const_raw - 1) + 1
        if (getFixnumLit(right)) |r_const| {
            const l = try self.translate(left);
            const one = try self.cachedIconst(1);
            const l_val = try self.b.sshr(I64, l, one);
            const r_untagged: i64 = @intCast(r_const - 1); // 2 * r_value
            // Try strength reduction: l_val * r_untagged using shift-add
            if (try self.mulByConst(l_val, r_untagged)) |prod| {
                return try self.b.iadd(I64, prod, one);
            }
            const r_iconst = try self.cachedIconst(r_untagged);
            const prod = try self.b.imul(I64, l_val, r_iconst);
            return try self.b.iadd(I64, prod, one);
        }
        if (getFixnumLit(left)) |l_const| {
            const r = try self.translate(right);
            const one = try self.cachedIconst(1);
            const r_val = try self.b.sshr(I64, r, one);
            const l_untagged: i64 = @intCast(l_const - 1);
            if (try self.mulByConst(r_val, l_untagged)) |prod| {
                return try self.b.iadd(I64, prod, one);
            }
            const l_iconst = try self.cachedIconst(l_untagged);
            const prod = try self.b.imul(I64, r_val, l_iconst);
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

    /// Multiply-by-constant strength reduction: replace imul with shift-add sequences.
    /// Returns null if the constant doesn't match a known pattern.
    /// Hoist's ISLE lowers iadd(x, ishl(y, K)) → ADD Xd, Xn, Xm, LSL #K (1 cycle).
    /// This replaces MADD (3-cycle latency on Apple Silicon).
    fn mulByConst(self: *IrTranslator, x: HoistValue, k: i64) anyerror!?HoistValue {
        // Power of 2 → single shift
        if (k > 0 and k & (k - 1) == 0) {
            const shift = @ctz(@as(u64, @bitCast(k)));
            if (shift == 0) return x; // multiply by 1
            const s = try self.cachedIconst(@intCast(shift));
            return try self.b.ishl(I64, x, s);
        }
        // k = 2^n + 1 → ADD Xd, Xn, Xm, LSL #n (single insn via ISLE)
        if (k > 2 and (k - 1) & (k - 2) == 0) {
            const shift = @ctz(@as(u64, @bitCast(k - 1)));
            const s = try self.cachedIconst(@intCast(shift));
            const shifted = try self.b.ishl(I64, x, s);
            return try self.b.iadd(I64, x, shifted);
        }
        // k = 2^n - 1 → RSB-like: (x << n) - x
        if (k > 2 and (k + 1) & k == 0) {
            const shift = @ctz(@as(u64, @bitCast(k + 1)));
            const s = try self.cachedIconst(@intCast(shift));
            const shifted = try self.b.ishl(I64, x, s);
            return try self.b.isub(I64, shifted, x);
        }
        // k = 2^n (already handled above as power of 2)
        // k = 2 * (2^n + 1) → (x + x << n) << 1 = 2 insns
        if (k > 4 and k & 1 == 0) {
            const half = @divExact(k, 2);
            if (half > 2 and (half - 1) & (half - 2) == 0) {
                const shift = @ctz(@as(u64, @bitCast(half - 1)));
                const s = try self.cachedIconst(@intCast(shift));
                const shifted = try self.b.ishl(I64, x, s);
                const sum = try self.b.iadd(I64, x, shifted);
                const one_val = try self.cachedIconst(1);
                return try self.b.ishl(I64, sum, one_val);
            }
        }
        return null;
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
        // For predicate conditions, emit direct I8 comparisons instead of
        // the full tagged select + brif chain.
        const cond_val = if (actual_cond.* == .nilp) blk: {
            // nilp: icmp eq(val, 0)
            const inner_val = try self.translate(actual_cond.nilp.operand);
            const zero = try self.cachedIconst(0);
            break :blk try self.b.icmp(I8, IntCC.eq, inner_val, zero);
        } else if (actual_cond.* == .oddp) blk: {
            // oddp: band(val, 2) != 0 → test bit 1 of tagged value
            const inner_val = try self.translate(actual_cond.oddp.operand);
            const two = try self.cachedIconst(2);
            const bit = try self.b.band(I64, inner_val, two);
            const zero = try self.cachedIconst(0);
            break :blk try self.b.icmp(I8, IntCC.ne, bit, zero);
        } else if (actual_cond.* == .evenp) blk: {
            // evenp: band(val, 2) == 0 → test bit 1 of tagged value
            const inner_val = try self.translate(actual_cond.evenp.operand);
            const two = try self.cachedIconst(2);
            const bit = try self.b.band(I64, inner_val, two);
            const zero = try self.cachedIconst(0);
            break :blk try self.b.icmp(I8, IntCC.eq, bit, zero);
        } else if (actual_cond.* == .zerop) blk: {
            // zerop: val == tagged_zero (1)
            const inner_val = try self.translate(actual_cond.zerop.operand);
            const tagged_zero = try self.cachedIconst(1);
            break :blk try self.b.icmp(I8, IntCC.eq, inner_val, tagged_zero);
        } else if (actual_cond.* == .consp) blk: {
            // consp: (val & 0xF == 0) && val != 0
            // For branch condition, just check val != 0 && low bits clear
            const inner_val = try self.translate(actual_cond.consp.operand);
            const mask = try self.cachedIconst(0xF);
            const tag_bits = try self.b.band(I64, inner_val, mask);
            const zero = try self.cachedIconst(0);
            const is_tagged_zero = try self.b.icmp(I8, IntCC.eq, tag_bits, zero);
            const is_non_nil = try self.b.icmp(I8, IntCC.ne, inner_val, zero);
            break :blk try self.b.band(I8, is_tagged_zero, is_non_nil);
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
        self.switchBlock(then_blk);
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
        self.switchBlock(else_blk);
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
        self.switchBlock(merge_blk);
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
            .block => |b| {
                try self.preEmitConstants(b.body);
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
            .zerop => |n| {
                _ = try self.cachedIconst(1); // tagged 0
                _ = try self.cachedIconst(0); // nil
                _ = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw))); // t
                try self.preEmitConstants(n.operand);
            },
            .oddp, .evenp => |n| {
                _ = try self.cachedIconst(2); // bit mask for bit 1
                _ = try self.cachedIconst(0); // nil/zero
                _ = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw))); // t
                try self.preEmitConstants(n.operand);
            },
            .length => |n| {
                _ = try self.cachedIconst(0); // nil
                _ = try self.cachedIconst(1); // tagged 0
                _ = try self.cachedIconst(2); // tagged increment (+1 raw)
                _ = try self.cachedIconst(8); // cdr offset
                try self.preEmitConstants(n.operand);
            },
            .logand => |n| {
                try self.preEmitConstants(n.left);
                try self.preEmitConstants(n.right);
            },
            .mod, .rem => |n| {
                _ = try self.cachedIconst(1); // for shift
                try self.preEmitConstants(n.left);
                try self.preEmitConstants(n.right);
            },
            .cons => |n| {
                // Pre-emit inline cons constants for loop LICM only on safety=0
                // fixnum-fast paths. Safe-mode functions stay on helper calls.
                if (self.fixnum_fast and self.in_loop_preemit and !self.is_recursive) {
                    _ = try self.cachedIconst(@as(i64, @bitCast(@intFromPtr(&g_alloc_ptr))));
                    _ = try self.cachedIconst(16);
                    _ = try self.cachedIconst(8);
                }
                try self.preEmitConstants(n.left);
                try self.preEmitConstants(n.right);
            },
            .append => |n| {
                // Append calls jitAppend — fn ptr emitted locally by translateAppend
                try self.preEmitConstants(n.left);
                try self.preEmitConstants(n.right);
            },
            .assoc => |n| {
                // Assoc calls jitAssoc — fn ptr emitted locally
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
        self.in_loop_preemit = true;
        try self.preEmitConstants(cond_ir);
        try self.preEmitConstants(body_ir);
        self.in_loop_preemit = false;

        // Standard header-tested loop structure.
        // Loop rotation was attempted but hoist's phi coalescing doesn't
        // eliminate the copy MOVs on the back-edge, making it slower.

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
        self.switchBlock(header);
        for (mutated_indices.items, 0..) |idx, i| {
            while (self.locals.items.len <= idx) {
                try self.locals.append(self.allocator, HoistValue.new(0));
            }
            self.locals.items[idx] = phi_vals[i];
        }

        const cond_val = try self.translate(cond_ir);
        try self.b.brif(cond_val, loop_body, loop_exit);

        // Body: execute body, then jump back to header with updated values
        self.switchBlock(loop_body);
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
        self.switchBlock(loop_exit);

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

        // Check for known runtime primitive (gcd, nreverse, append, assoc, etc.)
        if (getCallTargetName(func_ir)) |target_name| {
            if (getJitPrimitivePtrWithArity(target_name, args.len)) |prim_ptr| {
                return try self.emitPrimitiveCall(prim_ptr, args);
            }
        }

        return try self.translateGenericCall(func_ir, args);
    }

    fn translateGenericCall(self: *IrTranslator, func_ir: *const Ir, args: []const *const Ir) anyerror!HoistValue {
        // Keep helper calls register-only: helper receives (fn + args), so max
        // user args is 7 to stay within 8 integer argument registers.
        if (args.len > 7) return error.UnsupportedCallTarget;

        var call_args: [8]HoistValue = undefined;
        call_args[0] = try self.translateCallDesignator(func_ir);
        for (args, 0..) |arg, i| {
            call_args[i + 1] = try self.translate(arg);
        }

        const helper_ptr: u64 = switch (args.len) {
            0 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall0))),
            1 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall1))),
            2 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall2))),
            3 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall3))),
            4 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall4))),
            5 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall5))),
            6 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall6))),
            7 => @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCall7))),
            else => unreachable,
        };
        const fn_ptr = try self.b.iconst(I64, @as(i64, @bitCast(helper_ptr)));
        const result = try self.emitIndirectCallValues(fn_ptr, call_args[0 .. args.len + 1]);
        return result;
    }

    fn translateCallDesignator(self: *IrTranslator, func_ir: *const Ir) anyerror!HoistValue {
        return switch (func_ir.*) {
            .global_ref => blk: {
                const roots = self.literal_roots orelse return error.UnsupportedCallTarget;
                const slot = roots.get(@intFromPtr(func_ir)) orelse return error.UnsupportedCallTarget;
                const slot_addr = try self.cachedIconst(@as(i64, @bitCast(@intFromPtr(slot))));
                break :blk try self.b.load(I64, slot_addr, MemFlags.default());
            },
            else => try self.translate(func_ir),
        };
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

            // Inline TCO disabled: hoist regalloc phi copy bug for nested loops.
            // Cross-call works correctly with ~5% overhead.
            if (false and countIrNodes(body) <= 40 and known.callee_name.len > 0 and
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
        // Cross-call fn pointers are cached (hoistable); primitive ptrs are local
        const fn_ptr = try self.cachedIconst(@as(i64, @bitCast(known.fn_ptr)));
        return try self.emitIndirectCall(fn_ptr, args);
    }

    fn emitPrimitiveCall(self: *IrTranslator, prim_ptr: u64, args: []const *const Ir) anyerror!HoistValue {
        // Primitive fn pointers are emitted locally (not pre-emitted/cached)
        const fn_ptr = try self.b.iconst(I64, @as(i64, @bitCast(prim_ptr)));
        return try self.emitIndirectCall(fn_ptr, args);
    }

    fn emitPrimitiveCallValues(self: *IrTranslator, prim_ptr: u64, args: []const HoistValue) anyerror!HoistValue {
        const fn_ptr = try self.b.iconst(I64, @as(i64, @bitCast(prim_ptr)));
        return try self.emitIndirectCallValues(fn_ptr, args);
    }

    /// Emit call_indirect with a pre-loaded function pointer and IR argument list.
    fn emitIndirectCall(self: *IrTranslator, fn_ptr: HoistValue, args: []const *const Ir) anyerror!HoistValue {
        var translated_args: [16]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }
        return try self.emitIndirectCallValues(fn_ptr, translated_args[0..args.len]);
    }

    fn emitIndirectCallValues(self: *IrTranslator, fn_ptr: HoistValue, args: []const HoistValue) anyerror!HoistValue {
        // Hoist stack-arg lowering for indirect calls is currently unsafe with
        // frame-based prologues. Keep JIT indirect calls register-only.
        if (args.len > 8) return error.UnsupportedCallTarget;
        const arity: u32 = @intCast(args.len);
        const sig = try self.getCallSigForArity(arity);

        var call_args = ValueList.default();
        try self.func.dfg.value_lists.push(&call_args, fn_ptr);
        for (0..arity) |i| {
            try self.func.dfg.value_lists.push(&call_args, args[i]);
        }

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

        // Pre-emit constants BEFORE jump to header (in caller's current block).
        // Constants dominate the loop body but aren't re-executed each iteration.
        try self.preEmitConstants(callee_body);

        // Jump from current block to header with initial arg values
        try self.b.jumpArgs(header, translated_args[0..arity]);

        // Switch to header block
        self.switchBlock(header);

        // Push inline scope: callee locals start after caller's locals
        const base = self.locals.items.len;
        for (0..arity) |i| {
            try self.locals.append(self.allocator, phi_vals[i]);
        }
        try self.inline_scopes.append(self.allocator, .{
            .base = base,
            .count = arity,
        });

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
        self.switchBlock(exit);
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

    fn lookupKnownFn(_: *IrTranslator, kf: *const std.StringHashMap(KnownFn), target_name: []const u8) ?KnownFn {
        return lookupKnownFnByName(kf, target_name);
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
    ///
    /// Continuation stack optimization: when a tail self-call has an arg that
    /// is itself a non-tail self-call (e.g., ack's (ack (- m 1) (ack m (- n 1)))),
    /// we eliminate the non-tail call by pushing continuation state onto an
    /// explicit stack and jumping to the header. At exit, pending continuations
    /// are popped and the loop continues. This eliminates all BL+prologue+epilogue
    /// overhead for non-tail self-calls.
    fn translateTCOBody(self: *IrTranslator, body_ir: *const Ir) anyerror!HoistValue {
        const arity = self.user_arity;
        if (arity > 8) return error.TooManyParams;

        // Detect continuation pattern: does the body have a tail self-call
        // where exactly ONE arg contains a non-tail self-call?
        // Multiple inner self-calls (like tak) are not supported by cont stack.
        const use_cont_stack = hasNonTailSelfCalls(body_ir, self.fn_name) and
            hasSelfTailCalls(body_ir, self.fn_name) and
            hasSingleInnerSelfCall(body_ir, self.fn_name);

        // Determine continuation width: number of values to save per continuation.
        // For ack pattern: tail_call(self, arg0, inner_call) → save arg0 = 1 value.
        // For general: save all non-inner-call args = arity - 1 values.
        const cont_width: u32 = if (use_cont_stack) arity - 1 else 0;

        // Create loop header with phi params + optional cont_depth param
        const header = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(header);

        var phi_vals: [8]HoistValue = undefined;
        for (0..arity) |pi| {
            phi_vals[pi] = try self.func.dfg.appendBlockParam(header, I64);
        }

        // Add continuation depth phi parameter
        var cont_depth_phi: ?HoistValue = null;
        if (use_cont_stack) {
            cont_depth_phi = try self.func.dfg.appendBlockParam(header, I64);
        }

        // Create exit block with result phi + optional depth param
        const exit = try self.func.dfg.addBlock();
        try self.func.layout.appendBlock(exit);
        const exit_param = try self.func.dfg.appendBlockParam(exit, I64);
        var exit_depth_param: ?HoistValue = null;
        if (use_cont_stack) {
            exit_depth_param = try self.func.dfg.appendBlockParam(exit, I64);
        }

        // Pre-emit constants in ENTRY block (before jump to header).
        if (!self.local_consts) {
            try self.preEmitConstants(body_ir);
        }

        // Allocate continuation stack as a runtime constant (heap-allocated buffer).
        // Can't use stack_slot because ack(3,11) needs ~16K entries (128KB).
        // Ownership transfers to CompiledFn for cleanup.
        var cont_base: ?HoistValue = null;
        if (use_cont_stack) {
            // Allocate 128KB buffer for continuation stack (16384 entries * 8 bytes)
            const cont_buf_size: usize = 16384 * @as(usize, cont_width) * 8;
            const cont_buf = try self.allocator.alignedAlloc(u8, .@"8", cont_buf_size);
            self.cont_buf_alloc = cont_buf;
            // Store the buffer pointer as a constant in the IR
            const buf_ptr = @intFromPtr(cont_buf.ptr);
            cont_base = try self.b.iconst(I64, @as(i64, @bitCast(buf_ptr)));
        }

        // Jump from entry to header with initial param values + cont_depth=0
        var init_vals: [9]HoistValue = undefined;
        for (0..arity) |i| {
            init_vals[i] = self.locals.items[i];
        }
        if (use_cont_stack) {
            init_vals[arity] = try self.b.iconst(I64, 0); // depth = 0
        }
        const init_count: usize = if (use_cont_stack) arity + 1 else arity;
        try self.b.jumpArgs(header, init_vals[0..init_count]);

        // Switch to header, install phi values as locals
        self.switchBlock(header);
        for (0..arity) |i| {
            self.locals.items[i] = phi_vals[i];
        }

        // Enable TCO mode
        self.tco_header = header;
        self.tco_exit = exit;

        // Store continuation stack state
        if (use_cont_stack) {
            self.cont_depth_phi = cont_depth_phi;
            self.cont_base = cont_base;
            self.cont_width = cont_width;
            // All self-calls are converted to jumps (both tail and non-tail),
            // so no call_indirect is emitted — skip self-pointer patching.
            self.all_calls_converted = true;
        }

        // Translate body — tail calls jump to header, returns jump to exit
        const body_result = try self.translateTCOExpr(body_ir);

        // If body produces a value (non-tail path), jump to exit with it.
        // If body terminated with a tail call, current_block is null — skip.
        if (self.b.current_block != null) {
            const result_tagged = try self.boolToTagged(body_result);
            try self.jumpToTCOExit(result_tagged);
        }

        // Switch to exit block
        self.switchBlock(exit);

        if (use_cont_stack) {
            // Check if continuation stack is non-empty: depth > 0
            const depth = exit_depth_param.?;
            const zero = try self.b.iconst(I64, 0);
            const has_cont = try self.b.icmp(I8, IntCC.ne, depth, zero);

            const pop_blk = try self.func.dfg.addBlock();
            try self.func.layout.appendBlock(pop_blk);
            const ret_blk = try self.func.dfg.addBlock();
            try self.func.layout.appendBlock(ret_blk);

            try self.b.brif(has_cont, pop_blk, ret_blk);

            // Pop block: load continuation args, jump to header
            self.switchBlock(pop_blk);
            const one = try self.b.iconst(I64, 1);
            const new_depth = try self.b.isub(I64, depth, one);
            const eight = try self.b.iconst(I64, 8);
            const base = cont_base.?;

            // Load continuation values from stack
            // For ack (cont_width=1): load one value (the saved m-1)
            // The non-inner-call arg is always arg[0] for the 2-arg case.
            // The inner call result (exit_param) becomes the last arg.
            var pop_header_args: [9]HoistValue = undefined;
            for (0..cont_width) |ci| {
                // Address = base + (new_depth * cont_width + ci) * 8
                var offset = try self.b.imul(I64, new_depth, eight);
                if (cont_width > 1) {
                    const ci_val = try self.b.iconst(I64, @as(i64, @intCast(ci * 8)));
                    offset = try self.b.iadd(I64, offset, ci_val);
                }
                const addr = try self.b.iadd(I64, base, offset);
                const loaded = try self.b.load(I64, addr, MemFlags.default());
                pop_header_args[ci] = loaded;
            }
            // The inner call result (exit_param) fills the remaining arg slot
            pop_header_args[cont_width] = exit_param;
            pop_header_args[cont_width + 1] = new_depth; // continuation depth
            try self.b.jumpArgs(header, pop_header_args[0 .. arity + 1]);

            // Return block: actually return
            self.switchBlock(ret_blk);
            return exit_param;
        }

        return exit_param;
    }

    /// Translate an expression in TCO context. In tail position, tail calls
    /// Jump to the TCO exit block, passing the result value and (if active)
    /// the continuation depth phi parameter.
    /// Jump to the TCO exit block, passing the result value and (if active)
    /// the current continuation depth.
    fn jumpToTCOExit(self: *IrTranslator, val: HoistValue) !void {
        if (self.cont_depth_phi != null) {
            try self.b.jumpArgs(self.tco_exit.?, &.{ val, self.cont_depth_phi.? });
        } else {
            try self.b.jumpArgs(self.tco_exit.?, &.{val});
        }
    }

    /// become jumps to header. Non-tail-position code delegates to translate().
    fn translateTCOExpr(self: *IrTranslator, ir: *const Ir) anyerror!HoistValue {
        switch (ir.*) {
            .block => |b| return self.translateTCOExpr(b.body),
            .tailcall => |tc| {
                if (isCallTargetSelf(tc.func, self.fn_name)) {
                    // Check for continuation stack mode: if active and an arg
                    // contains a self-call, replace call_indirect with push+jump.
                    if (self.cont_depth_phi != null) {
                        var call_arg_idx: ?usize = null;
                        for (tc.args, 0..) |arg, i| {
                            if (detectSelfCalls(arg, self.fn_name)) {
                                call_arg_idx = i;
                                break;
                            }
                        }

                        if (call_arg_idx) |cai| {
                            // Continuation stack optimization:
                            // Instead of: call_indirect(inner), then tail-jump(result)
                            // Do: push non-call args, jump to header with inner's args

                            // 1. Compute and store non-call args (continuation state)
                            const depth = self.cont_depth_phi.?;
                            const base = self.cont_base.?;
                            const eight = try self.b.iconst(I64, 8);
                            const store_offset = try self.b.imul(I64, depth, eight);

                            var cont_idx: u32 = 0;
                            for (tc.args, 0..) |arg, i| {
                                if (i == cai) continue;
                                const val = try self.translate(arg);
                                var addr = try self.b.iadd(I64, base, store_offset);
                                if (cont_idx > 0) {
                                    const extra = try self.b.iconst(I64, @as(i64, @intCast(cont_idx * 8)));
                                    addr = try self.b.iadd(I64, addr, extra);
                                }
                                try self.b.store(val, addr, MemFlags.default());
                                cont_idx += 1;
                            }

                            // 2. Extract the inner self-call's args
                            const inner_call = tc.args[cai];
                            const inner_args = switch (inner_call.*) {
                                .call => |c| c.args,
                                else => return error.ExpectedSelfCall,
                            };

                            // 3. Translate inner call args and jump to header
                            var header_args: [9]HoistValue = undefined;
                            for (inner_args, 0..) |arg, i| {
                                header_args[i] = try self.translate(arg);
                            }
                            // Append new continuation depth (depth + 1)
                            const one = try self.b.iconst(I64, 1);
                            const new_depth = try self.b.iadd(I64, depth, one);
                            header_args[inner_args.len] = new_depth;
                            try self.b.jumpArgs(self.tco_header.?, header_args[0 .. inner_args.len + 1]);

                            self.b.current_block = null;
                            return HoistValue.new(0);
                        }
                    }

                    // Standard tail call path (no continuation stack, or no inner self-call)
                    if (self.local_consts) {
                        for (tc.args) |arg| {
                            if (detectSelfCalls(arg, self.fn_name)) {
                                self.in_call_block = true;
                                break;
                            }
                        }
                    }

                    var arg_vals: [9]HoistValue = undefined;
                    var has_call = [_]bool{false} ** 9;
                    for (tc.args, 0..) |arg, i| {
                        has_call[i] = detectSelfCalls(arg, self.fn_name);
                    }
                    for (tc.args, 0..) |arg, i| {
                        if (has_call[i]) {
                            arg_vals[i] = try self.translate(arg);
                        }
                    }
                    self.in_call_block = false;
                    for (tc.args, 0..) |arg, i| {
                        if (!has_call[i]) {
                            arg_vals[i] = try self.translate(arg);
                        }
                    }

                    // Append continuation depth if in cont mode
                    const arg_count = if (self.cont_depth_phi != null) blk: {
                        arg_vals[tc.args.len] = self.cont_depth_phi.?;
                        break :blk tc.args.len + 1;
                    } else tc.args.len;

                    try self.b.jumpArgs(self.tco_header.?, arg_vals[0..arg_count]);

                    self.b.current_block = null;

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

                    self.switchBlock(then_blk);
                    _ = try self.translateTCOExpr(then_branch);

                    self.switchBlock(else_blk);
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

                    self.switchBlock(then_blk);
                    if (then_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(then_branch);
                        try self.jumpToTCOExit(val);
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(then_branch);
                    }

                    self.switchBlock(else_blk);
                    if (else_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(else_branch);
                        try self.jumpToTCOExit(val);
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

                    self.switchBlock(then_blk);
                    if (then_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(then_branch);
                        try self.jumpToTCOExit(val);
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(then_branch);
                    }

                    self.switchBlock(else_blk);
                    return try self.translateTCOExpr(else_branch);
                }

                if (else_is_tail or else_is_simple_exit) {
                    // Else terminates, then produces value
                    const then_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(then_blk);
                    const else_blk = try self.func.dfg.addBlock();
                    try self.func.layout.appendBlock(else_blk);

                    try self.b.brif(cond, then_blk, else_blk);

                    self.switchBlock(else_blk);
                    if (else_is_simple_exit and self.tco_exit != null) {
                        const val = try self.translate(else_branch);
                        try self.jumpToTCOExit(val);
                        self.b.current_block = null;
                    } else {
                        _ = try self.translateTCOExpr(else_branch);
                    }

                    self.switchBlock(then_blk);
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
        self.switchBlock(then_blk);
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
        self.switchBlock(else_blk);
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
        self.switchBlock(merge_blk);
        for (mutated_indices.items, 0..) |idx, mi| {
            self.locals.items[idx] = merge_local_params.items[mi];
        }
        return merge_param;
    }

    fn translateSelfCall(self: *IrTranslator, args: []const *const Ir) anyerror!HoistValue {
        // Mark this block as containing a call so cachedIconst emits fresh
        // small constants instead of reusing cached values from loop header.
        const was_in_call = self.in_call_block;
        if (self.local_consts) self.in_call_block = true;

        // Translate user args first (while parameter registers are still valid)
        var translated_args: [16]HoistValue = undefined;
        for (args, 0..) |arg, i| {
            translated_args[i] = try self.translate(arg);
        }

        self.in_call_block = was_in_call;

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
    /// Uses load with offset=8 to emit LDR [base, #8] directly when not in
    /// TCO mode (backward phi copies have a hoist regalloc issue with offsets).
    fn translateCdr(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        // Always use iadd + load to prevent hoist LDP merging with car load.
        // Hoist's LDP register assignment has a bug: Rt2 destination doesn't
        // match the regalloc's expected register for the second value.
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

    /// cons allocation:
    /// - safety=0 fast path: inline bump allocation
    /// - safe path: helper call (preserves full runtime invariants)
    fn translateCons(self: *IrTranslator, car_ir: *const Ir, cdr_ir: *const Ir) anyerror!HoistValue {
        var car_val = try self.translate(car_ir);
        var cdr_val = try self.translate(cdr_ir);

        // In untagged mode, cons cells must store TAGGED values (they're runtime
        // objects read by the interpreter and other functions). Re-tag fixnum
        // arguments: tagged_raw = (untagged << 1) | 1.
        if (self.untagged) {
            if (producesFixnum(car_ir)) {
                const one = try self.cachedIconst(1);
                const shifted = try self.b.ishl(I64, car_val, one);
                car_val = try self.b.bor(I64, shifted, one);
            }
            if (producesFixnum(cdr_ir)) {
                const one = try self.cachedIconst(1);
                const shifted = try self.b.ishl(I64, cdr_val, one);
                cdr_val = try self.b.bor(I64, shifted, one);
            }
        }

        if (self.fixnum_fast) {
            // Inline bump allocation directly in hoist IR to avoid call_indirect
            // register swap issues. Loads g_alloc_ptr, stores car+cdr, bumps pointer.
            // No GC check — relies on sufficient heap space (same as jitCons fast path).
            const alloc_ptr_addr = try self.cachedIconst(@as(i64, @bitCast(@intFromPtr(&g_alloc_ptr))));
            const sixteen = try self.cachedIconst(16);
            const eight = try self.cachedIconst(8);

            const mf = hoist.memflags.MemFlags.default();

            // Load current allocation pointer
            const ptr = try self.b.load(I64, alloc_ptr_addr, mf);
            // Store car at [ptr+0]
            try self.b.store(car_val, ptr, mf);
            // Store cdr at [ptr+8]
            const ptr_plus_8 = try self.b.iadd(I64, ptr, eight);
            try self.b.store(cdr_val, ptr_plus_8, mf);
            // Bump allocation pointer
            const new_ptr = try self.b.iadd(I64, ptr, sixteen);
            try self.b.store(new_ptr, alloc_ptr_addr, mf);

            // Return ptr as cons value (cons tag = 0, so raw = ptr)
            // In untagged mode, the cons pointer is NOT a fixnum — it's already
            // a tagged cons (tag=0, raw=ptr). Don't untag it.
            return ptr;
        }

        const args = [_]HoistValue{ cdr_val, car_val };
        return try self.emitPrimitiveCallValues(@intFromPtr(&jitCons), &args);
    }

    /// Check if an IR expression produces a fixnum value (needs retagging in untagged mode).
    fn producesFixnum(ir: *const Ir) bool {
        return switch (ir.*) {
            .lit => |v| v.isFixnum(),
            .fixnum_add, .fixnum_sub, .fixnum_mul, .add, .sub, .mul => true,
            .@"var" => true, // Variables in untagged mode hold untagged fixnums
            .car, .unsafe_car => false, // car returns tagged values from cons cells
            .cdr, .unsafe_cdr => false, // cdr returns tagged values from cons cells
            .length => true, // length is a fixnum
            .cons => false, // cons returns a pointer
            else => true, // Conservative: assume fixnum
        };
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

    /// zerop: (= n 0) → tagged 0 is raw 1
    fn translateZerop(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const tagged_zero = try self.cachedIconst(1); // tagged 0 = (0<<1)|1 = 1
        const cond = try self.b.icmp(I8, IntCC.eq, val, tagged_zero);
        const t_val = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw)));
        const nil_val = try self.cachedIconst(0);
        return try self.b.select(I64, cond, t_val, nil_val);
    }

    /// oddp: test bit 1 of tagged fixnum. Tagged (n<<1)|1, so bit 1 = n's LSB.
    fn translateOddp(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const two = try self.cachedIconst(2); // bit mask for bit 1
        const bit = try self.b.band(I64, val, two);
        const zero = try self.cachedIconst(0);
        const cond = try self.b.icmp(I8, IntCC.ne, bit, zero);
        const t_val = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw)));
        const nil_val = try self.cachedIconst(0);
        return try self.b.select(I64, cond, t_val, nil_val);
    }

    /// evenp: inverse of oddp
    fn translateEvenp(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const val = try self.translate(operand_ir);
        const two = try self.cachedIconst(2);
        const bit = try self.b.band(I64, val, two);
        const zero = try self.cachedIconst(0);
        const cond = try self.b.icmp(I8, IntCC.eq, bit, zero);
        const t_val = try self.cachedIconst(@as(i64, @bitCast(Value.t.raw)));
        const nil_val = try self.cachedIconst(0);
        return try self.b.select(I64, cond, t_val, nil_val);
    }

    /// logand: bitwise AND. For tagged fixnums (n<<1)|1, AND preserves the tag bit
    /// since both operands have bit 0 = 1.
    fn translateLogand(self: *IrTranslator, left_ir: *const Ir, right_ir: *const Ir) anyerror!HoistValue {
        const left = try self.translate(left_ir);
        const right = try self.translate(right_ir);
        return try self.b.band(I64, left, right);
    }

    /// mod: modulus with floor division semantics (result sign matches divisor).
    /// Untag both operands, SDIV, MSUB for remainder, adjust sign, retag.
    /// For tagged fixnums: a_val = a_raw >> 1, b_val = b_raw >> 1
    /// result_raw = ((a_val mod b_val) << 1) | 1
    fn translateMod(self: *IrTranslator, left_ir: *const Ir, right_ir: *const Ir) anyerror!HoistValue {
        const a = try self.translate(left_ir);
        const b = try self.translate(right_ir);
        const one = try self.cachedIconst(1);
        // Arithmetic shift right by 1 to untag
        const a_val = try self.b.sshr(I64, a, one);
        const b_val = try self.b.sshr(I64, b, one);
        // SDIV + MSUB = remainder (truncating division)
        const quot = try self.b.sdiv(I64, a_val, b_val);
        const prod = try self.b.imul(I64, quot, b_val);
        const rem_val = try self.b.isub(I64, a_val, prod);
        // Floor mod adjustment: if rem != 0 and sign(rem) != sign(b), add b
        const zero = try self.cachedIconst(0);
        const rem_ne_zero = try self.b.icmp(I8, IntCC.ne, rem_val, zero);
        // XOR signs: if (rem ^ b) < 0, signs differ
        const xor_signs = try self.b.bxor(I64, rem_val, b_val);
        const signs_differ = try self.b.icmp(I8, IntCC.slt, xor_signs, zero);
        // need_adjust = rem != 0 && signs differ
        const need_adjust = try self.b.band(I8, rem_ne_zero, signs_differ);
        // adjusted = rem + b (if needed), else rem
        const adjusted = try self.b.iadd(I64, rem_val, b_val);
        const mod_val = try self.b.select(I64, need_adjust, adjusted, rem_val);
        // Retag: (mod_val << 1) | 1
        const shifted = try self.b.ishl(I64, mod_val, one);
        return try self.b.bor(I64, shifted, one);
    }

    /// rem: truncating remainder (result sign matches dividend).
    /// Simpler than mod — no sign adjustment needed.
    fn translateRem(self: *IrTranslator, left_ir: *const Ir, right_ir: *const Ir) anyerror!HoistValue {
        const a = try self.translate(left_ir);
        const b = try self.translate(right_ir);
        const one = try self.cachedIconst(1);
        const a_val = try self.b.sshr(I64, a, one);
        const b_val = try self.b.sshr(I64, b, one);
        const quot = try self.b.sdiv(I64, a_val, b_val);
        const prod = try self.b.imul(I64, quot, b_val);
        const rem_val = try self.b.isub(I64, a_val, prod);
        // Retag: (rem_val << 1) | 1
        const shifted = try self.b.ishl(I64, rem_val, one);
        return try self.b.bor(I64, shifted, one);
    }

    /// append: call jitAppend runtime function (allocates new cons cells)
    fn translateAppend(self: *IrTranslator, left_ir: *const Ir, right_ir: *const Ir) anyerror!HoistValue {
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAppend)));
        const args = [_]*const Ir{ left_ir, right_ir };
        return try self.emitPrimitiveCall(prim_ptr, &args);
    }

    /// assoc: call jitAssoc runtime function (linear search)
    fn translateAssoc(self: *IrTranslator, left_ir: *const Ir, right_ir: *const Ir) anyerror!HoistValue {
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAssoc)));
        const args = [_]*const Ir{ left_ir, right_ir };
        return try self.emitPrimitiveCall(prim_ptr, &args);
    }

    fn translateSqrt(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const v = try self.translate(operand_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitSqrt)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{v});
    }

    fn translateRound(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const v = try self.translate(operand_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitRound)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{v});
    }

    fn translateMakeHash(self: *IrTranslator, capacity: u16, test_type: runtime.HashTest) anyerror!HoistValue {
        const cap = try self.cachedIconst(@as(i64, @bitCast(Value.makeFixnum(@intCast(capacity)).raw)));
        const test_val = try self.cachedIconst(@as(i64, @bitCast(Value.makeFixnum(@intFromEnum(test_type)).raw)));
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMakeHash)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ cap, test_val });
    }

    fn translateVecNew(self: *IrTranslator, size_ir: *const Ir, init_ir: ?*const Ir) anyerror!HoistValue {
        const size = try self.translate(size_ir);
        const init_val = if (init_ir) |v|
            try self.translate(v)
        else
            try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMakeVector)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ size, init_val });
    }

    fn translateVecRef(self: *IrTranslator, vec_ir: *const Ir, idx_ir: *const Ir) anyerror!HoistValue {
        const vec = try self.translate(vec_ir);
        const idx = try self.translate(idx_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitVecRef)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ vec, idx });
    }

    fn translateVecSet(self: *IrTranslator, vec_ir: *const Ir, idx_ir: *const Ir, value_ir: *const Ir) anyerror!HoistValue {
        const vec = try self.translate(vec_ir);
        const idx = try self.translate(idx_ir);
        const value = try self.translate(value_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitVecSet)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ vec, idx, value });
    }

    fn translateVecLen(self: *IrTranslator, vec_ir: *const Ir) anyerror!HoistValue {
        const vec = try self.translate(vec_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitVecLen)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{vec});
    }

    fn translateHashGet(self: *IrTranslator, table_ir: *const Ir, key_ir: *const Ir, default_ir: ?*const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const key = try self.translate(key_ir);
        const def = if (default_ir) |d|
            try self.translate(d)
        else
            try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashGet)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ table, key, def });
    }

    fn translateHashSet(self: *IrTranslator, table_ir: *const Ir, key_ir: *const Ir, value_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const key = try self.translate(key_ir);
        const value = try self.translate(value_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashSet)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ table, key, value });
    }

    fn translateHashRem(self: *IrTranslator, table_ir: *const Ir, key_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const key = try self.translate(key_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashRem)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ table, key });
    }

    fn translateHashCount(self: *IrTranslator, table_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashCount)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{table});
    }

    fn translateHashCapacity(self: *IrTranslator, table_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashCapacity)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{table});
    }

    fn translateHashClear(self: *IrTranslator, table_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashClear)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{table});
    }

    fn translateHashTest(self: *IrTranslator, table_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashTest)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{table});
    }

    fn translateHashKeys(self: *IrTranslator, table_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashKeys)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{table});
    }

    fn translateHashAlist(self: *IrTranslator, table_ir: *const Ir) anyerror!HoistValue {
        const table = try self.translate(table_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitHashAlist)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{table});
    }

    fn translateFormat(self: *IrTranslator, dest_ir: *const Ir, control_ir: *const Ir, args: []const *const Ir) anyerror!HoistValue {
        if (args.len > 1) return error.UnsupportedIrNode;
        const dest = try self.translate(dest_ir);
        const control = try self.translate(control_ir);
        const arg = if (args.len == 1)
            try self.translate(args[0])
        else
            try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        const argc = try self.cachedIconst(@as(i64, @bitCast(Value.makeFixnum(@intCast(args.len)).raw)));
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitFormatSimple)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ dest, control, arg, argc });
    }

    fn translateMakeString(self: *IrTranslator, len_ir: *const Ir, char_ir: *const Ir) anyerror!HoistValue {
        const len = try self.translate(len_ir);
        const ch = try self.translate(char_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMakeString)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ len, ch });
    }

    fn translateStrRef(self: *IrTranslator, str_ir: *const Ir, idx_ir: *const Ir) anyerror!HoistValue {
        const s = try self.translate(str_ir);
        const idx = try self.translate(idx_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitStrRef)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ s, idx });
    }

    fn translateStrLen(self: *IrTranslator, str_ir: *const Ir) anyerror!HoistValue {
        const s = try self.translate(str_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitStrLen)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{s});
    }

    fn translateStrSet(self: *IrTranslator, str_ir: *const Ir, idx_ir: *const Ir, value_ir: *const Ir) anyerror!HoistValue {
        const s = try self.translate(str_ir);
        const idx = try self.translate(idx_ir);
        const v = try self.translate(value_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitStrSet)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ s, idx, v });
    }

    fn translateStrConcat(self: *IrTranslator, left_ir: *const Ir, right_ir: *const Ir) anyerror!HoistValue {
        const left = try self.translate(left_ir);
        const right = try self.translate(right_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitStrConcat)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ left, right });
    }

    fn translateSubstring(self: *IrTranslator, str_ir: *const Ir, start_ir: *const Ir, end_ir: *const Ir) anyerror!HoistValue {
        const s = try self.translate(str_ir);
        const start = try self.translate(start_ir);
        const end = try self.translate(end_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitSubstring)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ s, start, end });
    }

    fn translateIntern(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const name = try self.translate(operand_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitIntern)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{name});
    }

    fn translateArrNew(self: *IrTranslator, dimensions: []const *const Ir, init_ir: ?*const Ir) anyerror!HoistValue {
        if (dimensions.len > 8) return error.UnsupportedIrNode;
        const init_val = if (init_ir) |v|
            try self.translate(v)
        else
            try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));

        if (dimensions.len == 1) {
            const dim = try self.translate(dimensions[0]);
            const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMakeArray1)));
            return try self.emitPrimitiveCallValues(prim_ptr, &.{ dim, init_val });
        }

        // Build list dims for rank 0 and rank>=2 to keep call arity <= 8.
        const nil_val = try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        const cons_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitCons)));
        var dims_list = nil_val;
        var i: usize = dimensions.len;
        while (i > 0) {
            i -= 1;
            const dim = try self.translate(dimensions[i]);
            dims_list = try self.emitPrimitiveCallValues(cons_ptr, &.{ dims_list, dim });
        }

        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMakeArrayDynamic)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ dims_list, init_val });
    }

    fn translateArrNewDynamic(self: *IrTranslator, dimensions_ir: *const Ir, init_ir: ?*const Ir) anyerror!HoistValue {
        const dims = try self.translate(dimensions_ir);
        const init_val = if (init_ir) |v|
            try self.translate(v)
        else
            try self.cachedIconst(@as(i64, @bitCast(Value.nil.raw)));
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitMakeArrayDynamic)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ dims, init_val });
    }

    fn translateArrRef(self: *IrTranslator, array_ir: *const Ir, subscripts: []const *const Ir) anyerror!HoistValue {
        const arr = try self.translate(array_ir);
        return switch (subscripts.len) {
            1 => blk: {
                const s0 = try self.translate(subscripts[0]);
                const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAref1)));
                break :blk try self.emitPrimitiveCallValues(prim_ptr, &.{ arr, s0 });
            },
            2 => blk: {
                const s0 = try self.translate(subscripts[0]);
                const s1 = try self.translate(subscripts[1]);
                const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAref2)));
                break :blk try self.emitPrimitiveCallValues(prim_ptr, &.{ arr, s0, s1 });
            },
            else => error.UnsupportedIrNode,
        };
    }

    fn translateArrSet(self: *IrTranslator, array_ir: *const Ir, subscripts: []const *const Ir, value_ir: *const Ir) anyerror!HoistValue {
        const arr = try self.translate(array_ir);
        const value = try self.translate(value_ir);
        return switch (subscripts.len) {
            1 => blk: {
                const s0 = try self.translate(subscripts[0]);
                const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAset1)));
                break :blk try self.emitPrimitiveCallValues(prim_ptr, &.{ arr, s0, value });
            },
            2 => blk: {
                const s0 = try self.translate(subscripts[0]);
                const s1 = try self.translate(subscripts[1]);
                const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitAset2)));
                break :blk try self.emitPrimitiveCallValues(prim_ptr, &.{ arr, s0, s1, value });
            },
            else => error.UnsupportedIrNode,
        };
    }

    fn translatePosition(self: *IrTranslator, item_ir: *const Ir, seq_ir: *const Ir) anyerror!HoistValue {
        const item = try self.translate(item_ir);
        const seq = try self.translate(seq_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitPosition)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{ item, seq });
    }

    /// Generic sequence length helper for list/vector/string/string32/1d-array.
    fn translateLength(self: *IrTranslator, operand_ir: *const Ir) anyerror!HoistValue {
        const seq = try self.translate(operand_ir);
        const prim_ptr = @intFromPtr(@as(*const anyopaque, @ptrCast(&jitLength)));
        return try self.emitPrimitiveCallValues(prim_ptr, &.{seq});
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

/// Patch cross-function call_indirect (BLR) to direct BL when the target address
/// is within BL range (±128MB). This eliminates the MOVZ/MOVK/MOVK + MOV + BLR
/// sequence (6 instructions) and replaces with NOP...NOP + BL (1 instruction).
/// Called from the REPL after all functions in a batch are compiled.
pub fn patchCrossCallsToBL(code_ptr: [*]u8, code_len: usize, caller_base: usize) void {
    const buf = code_ptr[0..code_len];
    patchCrossCallsToBLSlice(buf, caller_base);
}

fn patchCrossCallsToBLSlice(buf: []u8, caller_base: usize) void {
    const NOP: u32 = 0xD503201F;
    if (buf.len < 24) return;
    const n_insns = buf.len / 4;

    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(buf, i);
        // Look for BLR xN (any register, not just x9)
        if (insn & 0xFFFFFC1F != 0xD63F0000) continue;
        const blr_reg: u5 = @truncate((insn >> 5) & 0x1F);

        // Scan backward for MOV blr_reg, rN
        var mov_idx: ?usize = null;
        var src_reg: u5 = undefined;
        {
            var j = i;
            var scan: usize = 0;
            while (j > 0 and scan < 8) : (scan += 1) {
                j -= 1;
                const prev = readInsn(buf, j);
                if (prev == NOP) continue;
                // MOV blr_reg, rN = ORR blr_reg, xzr, rN
                const expected_low: u32 = 0xAA0003E0 | @as(u32, blr_reg);
                if (prev & 0xFFE0FFFF == expected_low) {
                    mov_idx = j;
                    src_reg = @truncate((prev >> 16) & 0x1F);
                    break;
                }
                // If we hit an instruction that writes to blr_reg, stop
                if (@as(u5, @truncate(prev & 0x1F)) == blr_reg) break;
            }
        }
        if (mov_idx == null) continue;

        // Scan backward from MOV for MOVZ rN, #imm16 + MOVK + MOVK (3-instruction 48-bit load)
        var load_idx: ?usize = null;
        var loaded_addr: u64 = 0;
        {
            // Scan backward from MOV looking for MOVZ+MOVK+MOVK loading src_reg.
            // The MOVZ/MOVK sequence may be in the function prologue (pre-emitted constants).
            const start_j = mov_idx.?;
            var scan_count: usize = 0;
            var j = start_j;
            while (j > 0 and scan_count < 50) : (scan_count += 1) {
                j -= 1;
                const w0 = readInsn(buf, j);
                if (w0 == NOP) continue;
                // Check if this is MOVZ with hw=0 writing to src_reg
                const is_movz = (w0 & 0xFFE00000) == 0xD2800000;
                const w0_rd: u5 = @truncate(w0 & 0x1F);
                if (is_movz and w0_rd == src_reg and j + 2 < start_j) {
                    const w1 = readInsn(buf, j + 1);
                    const w2 = readInsn(buf, j + 2);
                    if ((w1 & 0xFFE00000) == 0xF2A00000 and @as(u5, @truncate(w1 & 0x1F)) == src_reg and
                        (w2 & 0xFFE00000) == 0xF2C00000 and @as(u5, @truncate(w2 & 0x1F)) == src_reg)
                    {
                        const imm0 = @as(u64, (w0 >> 5) & 0xFFFF);
                        const imm1 = @as(u64, (w1 >> 5) & 0xFFFF) << 16;
                        const imm2 = @as(u64, (w2 >> 5) & 0xFFFF) << 32;
                        loaded_addr = imm0 | imm1 | imm2;
                        load_idx = j;
                        break;
                    }
                }
                // Stop on non-MOVZ/non-MOVK write to src_reg
                const is_movk = (w0 & 0xFF800000) == 0xF2800000;
                if (!is_movz and !is_movk and w0_rd == src_reg) break;
            }
        }
        if (load_idx == null or loaded_addr == 0) continue;

        // Check BL range: ±128MB
        const blr_addr = caller_base + i * 4;
        const target = loaded_addr;
        const diff = @as(i64, @intCast(target)) - @as(i64, @intCast(blr_addr));
        if (diff < -128 * 1024 * 1024 or diff > 128 * 1024 * 1024) continue;

        // Patch: NOP the MOVZ/MOVK/MOVK sequence, NOP the MOV, replace BLR with BL
        const rel_offset: i32 = @intCast(@divExact(diff, 4));
        const imm26: u32 = @as(u32, @bitCast(rel_offset)) & 0x03FFFFFF;
        writeInsn(buf, load_idx.?, NOP);
        writeInsn(buf, load_idx.? + 1, NOP);
        writeInsn(buf, load_idx.? + 2, NOP);
        writeInsn(buf, mov_idx.?, NOP);
        writeInsn(buf, i, 0x94000000 | imm26);
    }
}

/// Recursively collect all variable indices that are assigned (set) within an IR subtree.
fn collectMutatedVars(ir: *const Ir, indices: *std.ArrayList(u16), allocator: std.mem.Allocator) !void {
    switch (ir.*) {
        .block => |b| {
            try collectMutatedVars(b.body, indices, allocator);
        },
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
        .assert_fixnum,
        .nilp,
        .not,
        .consp,
        .car,
        .cdr,
        .unsafe_car,
        .unsafe_cdr,
        .abs,
        .zerop,
        .oddp,
        .evenp,
        .length,
        => |op| {
            try collectMutatedVars(op.operand, indices, allocator);
        },
        .cons, .logand, .mod, .rem, .append, .assoc => |op| {
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
/// Check if IR tree contains calls to known runtime primitives (gcd, nreverse, etc.)
/// Check if IR tree contains calls to known runtime primitives (gcd, nreverse, etc.)
fn containsPrimitiveCalls(body: *const Ir, self_name: []const u8) bool {
    return irAny(body, struct {
        name: []const u8,
        fn check(self: @This(), ir: *const Ir) bool {
            const func_ir = switch (ir.*) {
                .call => |c| c.func,
                .tailcall => |tc| tc.func,
                else => return false,
            };
            if (isCallTargetSelf(func_ir, self.name)) return false;
            const target = getCallTargetName(func_ir) orelse return false;
            return getJitPrimitivePtr(target) != null;
        }
    }{ .name = self_name });
}

/// Detect if a self-call appears as an argument to another self-call.
/// This pattern (e.g., tak) causes segfaults due to hoist regalloc bug.
fn hasNestedSelfCalls(body: *const Ir, name: []const u8) bool {
    return irAny(body, struct {
        name: []const u8,
        fn check(self: @This(), ir: *const Ir) bool {
            const func_ir, const args = switch (ir.*) {
                .call => |c| .{ c.func, c.args },
                .tailcall => |tc| .{ tc.func, tc.args },
                else => return false,
            };
            if (!isCallTargetSelf(func_ir, self.name)) return false;
            for (args) |arg| {
                if (detectSelfCalls(arg, self.name)) return true;
            }
            return false;
        }
    }{ .name = name });
}

/// Check if IR tree contains cons/append operations.
fn containsCons(body: *const Ir) bool {
    return irAny(body, struct {
        fn check(_: @This(), ir: *const Ir) bool {
            return ir.* == .cons or ir.* == .append;
        }
    }{});
}

/// Detect IR nodes lowered to C-ABI helper calls in this backend.
/// When `fixnum_inline` is true, generic numeric ops are lowered inline and
/// should not force helper-call classification.
fn containsHelperCalls(body: *const Ir, fixnum_inline: bool) bool {
    return irAny(body, struct {
        fixnum_inline: bool,
        fn check(self: @This(), ir: *const Ir) bool {
            return switch (ir.*) {
                .add, .sub, .mul, .lt, .gt, .le, .ge, .num_eq => !self.fixnum_inline,
                .sqrt,
                .round,
                .vec_new,
                .vec_ref,
                .vec_set,
                .vec_len,
                .make_hash,
                .hash_get,
                .hash_set,
                .hash_rem,
                .hash_count,
                .hash_capacity,
                .hash_clear,
                .hash_test,
                .hash_keys,
                .hash_alist,
                .make_string,
                .str_ref,
                .str_len,
                .intern,
                .arr_new,
                .arr_new_dyn,
                .arr_ref,
                .arr_set,
                .str_set,
                .str_concat,
                .substring,
                .position,
                .position_eq,
                .position_equal,
                .format,
                => true,
                else => false,
            };
        }
    }{ .fixnum_inline = fixnum_inline });
}

/// Conservative gate for untagged mode: only pure fixnum arithmetic/control.
fn isUntaggedSafeExpr(ir: *const Ir) bool {
    return switch (ir.*) {
        .lit => |v| v.isFixnum(),
        .@"var" => true,
        .fixnum_add,
        .fixnum_sub,
        .fixnum_mul,
        .add,
        .sub,
        .mul,
        .fixnum_le,
        .fixnum_lt,
        .fixnum_gt,
        .fixnum_ge,
        .fixnum_eq,
        .le,
        .lt,
        .gt,
        .ge,
        .num_eq,
        .eq,
        .logand,
        .mod,
        .rem,
        => |op| isUntaggedSafeExpr(op.left) and isUntaggedSafeExpr(op.right),
        .assert_fixnum => |op| isUntaggedSafeExpr(op.operand),
        .@"if" => |n| isUntaggedSafeExpr(n.cond) and isUntaggedSafeExpr(n.then_branch) and isUntaggedSafeExpr(n.else_branch),
        .block => |n| isUntaggedSafeExpr(n.body),
        .progn => |exprs| blk: {
            for (exprs) |e| if (!isUntaggedSafeExpr(e)) break :blk false;
            break :blk true;
        },
        .let => |n| blk: {
            for (n.bindings) |b| if (!isUntaggedSafeExpr(b.value)) break :blk false;
            break :blk isUntaggedSafeExpr(n.body);
        },
        .set => |n| isUntaggedSafeExpr(n.value),
        .loop => |n| isUntaggedSafeExpr(n.cond) and isUntaggedSafeExpr(n.body),
        else => false,
    };
}

/// Detect whether a function body contains unresolvable non-self calls.
pub fn hasNonSelfCalls(body: *const Ir, name: []const u8) bool {
    return irAny(body, struct {
        name: []const u8,
        fn check(self: @This(), ir: *const Ir) bool {
            const func_ir = switch (ir.*) {
                .call => |c| c.func,
                .tailcall => |tc| tc.func,
                else => return false,
            };
            if (isCallTargetSelf(func_ir, self.name)) return false;
            const target = getCallTargetName(func_ir) orelse return true;
            return getJitPrimitivePtr(target) == null;
        }
    }{ .name = name });
}

/// Detect whether a function body contains any non-self call.
pub fn hasAnyNonSelfCalls(body: *const Ir, name: []const u8) bool {
    return irAny(body, struct {
        name: []const u8,
        fn check(self: @This(), ir: *const Ir) bool {
            const func_ir = switch (ir.*) {
                .call => |c| c.func,
                .tailcall => |tc| tc.func,
                else => return false,
            };
            return !isCallTargetSelf(func_ir, self.name);
        }
    }{ .name = name });
}

/// Detect whether a function body contains self-recursive calls.
fn detectSelfCalls(body: *const Ir, name: []const u8) bool {
    return irAny(body, struct {
        name: []const u8,
        fn check(self: @This(), ir: *const Ir) bool {
            return switch (ir.*) {
                .call => |c| isCallTargetSelf(c.func, self.name),
                .tailcall => |tc| isCallTargetSelf(tc.func, self.name),
                else => false,
            };
        }
    }{ .name = name });
}

/// Detect whether an IR tree contains load-generating operations (car, cdr, length).
fn containsLoads(body: *const Ir) bool {
    return irAny(body, struct {
        fn check(_: @This(), ir: *const Ir) bool {
            return switch (ir.*) {
                .car, .cdr, .unsafe_car, .unsafe_cdr, .length => true,
                else => false,
            };
        }
    }{});
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
        .block => |b| hasSelfTailCalls(b.body, name),
        .@"if" => |i| hasSelfTailCalls(i.then_branch, name) or hasSelfTailCalls(i.else_branch, name),
        .let => |l| hasSelfTailCalls(l.body, name),
        .progn => |exprs| if (exprs.len == 0) false else hasSelfTailCalls(exprs[exprs.len - 1], name),
        else => false,
    };
}

/// Check if the continuation stack pattern applies: exactly one tail-call arg
/// contains a self-call. Multiple self-call args (like tak) aren't supported.
fn hasSingleInnerSelfCall(body: *const Ir, name: []const u8) bool {
    return switch (body.*) {
        .tailcall => |tc| blk: {
            if (!isCallTargetSelf(tc.func, name)) break :blk false;
            var count: u32 = 0;
            for (tc.args) |arg| {
                if (detectSelfCalls(arg, name)) count += 1;
            }
            break :blk count == 1;
        },
        .block => |b| hasSingleInnerSelfCall(b.body, name),
        .@"if" => |i| hasSingleInnerSelfCall(i.then_branch, name) or hasSingleInnerSelfCall(i.else_branch, name),
        .let => |l| hasSingleInnerSelfCall(l.body, name),
        .progn => |exprs| if (exprs.len == 0) false else hasSingleInnerSelfCall(exprs[exprs.len - 1], name),
        else => false,
    };
}

/// Detect if a function body has non-tail self-calls (.call nodes targeting self).
fn hasNonTailSelfCalls(body: *const Ir, name: []const u8) bool {
    return irAny(body, struct {
        name: []const u8,
        fn check(self: @This(), ir: *const Ir) bool {
            return switch (ir.*) {
                .call => |c| isCallTargetSelf(c.func, self.name),
                else => false,
            };
        }
    }{ .name = name });
}

/// Detect whether a function body contains loop constructs.
fn detectLoops(body: *const Ir) bool {
    return irAny(body, struct {
        fn check(_: @This(), ir: *const Ir) bool {
            return ir.* == .loop;
        }
    }{});
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

pub fn compileIr(
    allocator: std.mem.Allocator,
    ir: *const Ir,
    name: []const u8,
) !CompiledFn {
    return compileIrWithKnownFnsAndLiteralRoots(allocator, ir, name, null, null);
}

pub fn compileIrWithKnownFns(
    allocator: std.mem.Allocator,
    ir: *const Ir,
    name: []const u8,
    known_fns: ?*const std.StringHashMap(KnownFn),
) !CompiledFn {
    return compileIrWithKnownFnsAndLiteralRoots(allocator, ir, name, known_fns, null);
}

pub fn compileIrWithKnownFnsAndLiteralRoots(
    allocator: std.mem.Allocator,
    ir: *const Ir,
    name: []const u8,
    known_fns: ?*const std.StringHashMap(KnownFn),
    literal_roots: ?*const LiteralRoots,
) !CompiledFn {
    const lambda = switch (ir.*) {
        .lambda => |l| l,
        else => return error.ExpectedLambda,
    };

    // Fast reject: check if all IR nodes are supported before allocating
    if (!IrTranslator.canTranslateWithLiteralRoots(lambda.body, literal_roots)) return error.UnsupportedIrNode;

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
    translator.literal_roots = literal_roots;
    translator.user_arity = arity;
    translator.is_recursive = detectSelfCalls(lambda.body, name);
    translator.has_loops = detectLoops(lambda.body);
    translator.has_loads = containsLoads(lambda.body);
    translator.fixnum_fast = lambda.safety == 0;
    translator.untagged = translator.fixnum_fast and translator.has_loops and !translator.is_recursive and
        isUntaggedSafeExpr(lambda.body);

    const fixnum_inline = translator.untagged or (translator.is_recursive and translator.fixnum_fast);
    translator.has_cross_calls = containsCons(lambda.body) or
        containsHelperCalls(lambda.body, fixnum_inline) or
        containsPrimitiveCalls(lambda.body, name) or
        hasAnyNonSelfCalls(lambda.body, name);

    if (std.posix.getenv("HABU_TRACE_JIT_FLAGS") != null) {
        std.debug.print(
            "JIT_FLAGS fn={s} arity={d} rec={} cross={} loops={} loads={} known={d}\n",
            .{ name, arity, translator.is_recursive, translator.has_cross_calls, translator.has_loops, translator.has_loads, if (known_fns) |kf| kf.count() else 0 },
        );
    }

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

    // Tail-call optimization: functions with self-tail-calls → loop.
    // Partial TCO: tail self-calls become jumps to header, non-tail self-calls
    // remain as call_indirect. This eliminates prologue/epilogue overhead for
    // tail calls while keeping non-tail calls correct.
    const use_tco = hasSelfTailCalls(lambda.body, name);

    if (use_tco) {
        // Partial TCO: tail self-calls become jumps. If there are still non-tail
        // self-calls, the function remains recursive (needs self-pointer).
        if (!hasNonTailSelfCalls(lambda.body, name)) {
            translator.is_recursive = false;
        }
        // May still have cross-calls (cons, known functions)
        const tco_fixnum_inline = translator.untagged or (translator.is_recursive and translator.fixnum_fast);
        translator.has_cross_calls = containsCons(lambda.body) or
            containsHelperCalls(lambda.body, tco_fixnum_inline) or
            containsPrimitiveCalls(lambda.body, name) or
            hasAnyNonSelfCalls(lambda.body, name);
    }

    // Emit small constants locally (not cached) when a TCO function has
    // non-tail self-calls. Hoist's optimizer moves cached constants to block0,
    // forcing callee-saved registers since their live ranges span call sites.
    // Only enabled for TCO functions where the loop header creates the problem.
    translator.local_consts = use_tco and hasNonTailSelfCalls(lambda.body, name);

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
    // Use .none for functions with calls (cross or recursive) — hoist optimizer
    // hangs or produces incorrect results for call_indirect at any opt level > none.
    // Use .aggressive for leaf functions (no calls at all).
    const use_aggressive = !(translator.is_recursive or translator.has_cross_calls or translator.has_loads);
    var ctx = ctx_builder
        .optLevel(if (use_aggressive) .aggressive else .none)
        .callConv(.system_v)
        .verification(true)
        .build();
    defer ctx.deinit();

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
    if (translator.is_recursive and !translator.all_calls_converted) {
        const func_addr = @intFromPtr(buf.ptr);
        const placeholder: u64 = @bitCast(translator.self_ptr_placeholder);
        // Try BL optimization first, fall back to address patching
        if (!patchSelfCallsToBL(code.code.items, placeholder)) {
            if (!patchPlaceholder(code.code.items, placeholder, func_addr)) {
                return error.SelfPointerPatchFailed;
            }
        }
    }

    const dump_passes = std.posix.getenv("HABU_DUMP_HOIST_PASSES") != null;
    if (dump_passes) dumpAsmPass("start", code.code.items);

    // Peephole: replace dead cset with NOP in fused cmp+cset+b.cc sequences.
    // The icmp emits cmp+cset, and fused brif emits b.cc using flags directly.
    // The cset result is dead but still executes.
    if (std.posix.getenv("HABU_NO_CSET_ELIM") == null) {
        eliminateDeadCset(code.code.items);
    }
    if (dump_passes) dumpAsmPass("after eliminateDeadCset", code.code.items);

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
        try fixEntryParamMovesAlloc(allocator, &code.code);
    }
    if (dump_passes) dumpAsmPass("after fixEntryParamMoves", code.code.items);

    // Fuse MOVZ+CMP into CMP immediate (eliminates MOVZ for small constants)
    fuseCmpImmediate(code.code.items);
    if (dump_passes) dumpAsmPass("after fuseCmpImmediate", code.code.items);

    // Fuse CMP+CSET...CMP+CSEL into CMP...CSEL with original condition.
    // Pattern: CMP sets flags → CSET materializes bool → later CMP tests bool → CSEL.
    // Replace with: CMP sets flags → NOP → ... → NOP → CSEL(original cond).
    fuseSelectCondition(code.code.items);
    if (dump_passes) dumpAsmPass("after fuseSelectCondition", code.code.items);

    // Coalesce: replace `op rD, rA, rB; mov rC, rD` with `op rC, rA, rB; nop`
    coalesceMovs(code.code.items);
    if (dump_passes) dumpAsmPass("after coalesceMovs", code.code.items);

    // Eliminate B .+4 (jump to next instruction = NOP).
    // Must run AFTER coalescing since coalescing uses branches as scan barriers.
    eliminateUselessBranches(code.code.items);
    if (dump_passes) dumpAsmPass("after eliminateUselessBranches", code.code.items);

    // Invert `b.cond .+8; b target` → `b.inv_cond target; nop`.
    invertBranchOverBranch(code.code.items);
    if (dump_passes) dumpAsmPass("after invertBranchOverBranch", code.code.items);

    // Eliminate dead MOV before unconditional B when MOV dest is never read.
    // Pattern: MOV Xd, Xs; B target where target is a chain ending at epilogue.
    // The MOV is a phi copy from the trampoline that's never consumed.
    eliminateDeadMovBeforeBranch(code.code.items);
    if (dump_passes) dumpAsmPass("after eliminateDeadMovBeforeBranch", code.code.items);

    // Note: LSL+ADD fusion (ADD Xd,Xn,Xm,LSL #K) was tested but is SLOWER
    // on Apple M-series (~10% regression). The wide OoO engine dispatches
    // separate LSL+ADD to parallel units faster than a single shifted-ADD.

    // Repair BLR target register clobbers in call setup windows where the
    // chosen target register (often x9) is overwritten before BLR executes.
    fixBlrTargetClobber(code.code.items);
    if (dump_passes) dumpAsmPass("after fixBlrTargetClobber", code.code.items);

    // Fix parallel copy conflicts in call argument setup.
    // Hoist's lowering emits sequential mov instructions for call arguments
    // which can clobber source registers before they're consumed.
    // Always run for recursive functions (even without nested self-calls)
    // because 3+ params create dependency chains in the arg move sequence.
    if (translator.is_recursive or translator.has_cross_calls) {
        if (!fixCallArgMoves(code.code.items)) {
            return error.UnsupportedIrNode;
        }
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
    if (dump_passes) dumpAsmPass("after fixCallArgMoves", code.code.items);

    // Fuse MUL+ADD into MADD where possible.
    fuseMulAdd(code.code.items);
    if (dump_passes) dumpAsmPass("after fuseMulAdd", code.code.items);

    // Fuse MOVZ+ALU into ALU-immediate and MOVZ+MOV into MOVZ-retarget.
    fuseMovzAlu(code.code.items);
    if (dump_passes) dumpAsmPass("after fuseMovzAlu#1", code.code.items);

    // Eliminate round-trip MOV pairs: MOV xA, xB; ... MOV xB, xA → NOP both.
    // Common in TCO functions where entry params are copied to intermediate regs
    // and then immediately copied back for the loop header phis.
    eliminateRoundTripMovs(code.code.items);
    if (dump_passes) dumpAsmPass("after eliminateRoundTripMovs", code.code.items);

    // Eliminate prologue/epilogue for leaf functions (no BLR/BL calls).
    // After TCO, recursive functions become loops and don't need frame setup.
    if (!translator.is_recursive or translator.all_calls_converted) {
        eliminateLeafPrologue(code.code.items);
    }
    if (dump_passes) dumpAsmPass("after eliminateLeafPrologue", code.code.items);

    // Eliminate dead MOVZ instructions where the dest is overwritten before read.
    eliminateDeadMovz(code.code.items);
    if (dump_passes) dumpAsmPass("after eliminateDeadMovz#1", code.code.items);

    // Remove all NOP instructions and fix branch offsets.
    // Must run LAST after all other peephole passes that introduce NOPs.
    compactNops(code.code.items, &code.code);
    if (dump_passes) dumpAsmPass("after compactNops#1", code.code.items);

    // After NOP compaction, new adjacent MOVZ+ALU pairs may emerge.
    // Also dead MOVZ may be exposed.
    fuseMovzAlu(code.code.items);
    if (dump_passes) dumpAsmPass("after fuseMovzAlu#2", code.code.items);
    eliminateDeadMovz(code.code.items);
    if (dump_passes) dumpAsmPass("after eliminateDeadMovz#2", code.code.items);
    compactNops(code.code.items, &code.code);
    if (dump_passes) dumpAsmPass("after compactNops#2", code.code.items);

    // Eliminate dead MOV x29, xzr in prologue (hoist clears frame pointer
    // after saving it, but we don't use x29 as frame pointer).
    eliminateDeadFramePointerClear(code.code.items);
    if (dump_passes) dumpAsmPass("after eliminateDeadFramePointerClear", code.code.items);

    // Final pass: compact any new NOPs and simplify branch chains.
    compactNops(code.code.items, &code.code);
    eliminateUselessBranches(code.code.items);
    if (dump_passes) dumpAsmPass("after final compact/branch", code.code.items);

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
        .cont_buf = translator.cont_buf_alloc,
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
/// Fuse MOVZ+CMP register into CMP immediate.
/// Pattern: MOVZ xN, #imm; ...; CMP xM, xN → CMP xM, #imm; NOP (MOVZ if dead)
/// ARM64 CMP immediate: SUBS XZR, Xn, #imm12 = 0xF100_0000 | (imm12 << 10) | (Rn << 5) | 0x1F
fn fuseCmpImmediate(code: []u8) void {
    const NOP = @as(u32, 0xD503201F);
    const n_insns = code.len / 4;
    if (n_insns < 2) return;

    var i: usize = 0;
    while (i + 1 < n_insns) : (i += 1) {
        const insn_cmp = readInsn(code, i);

        // Match CMP Xn, Xm (64-bit): 0xEB00001F with Rm in [20:16], Rn in [9:5]
        if (insn_cmp & 0xFFE0001F != 0xEB00001F) continue;
        const rn: u5 = @truncate((insn_cmp >> 5) & 0x1F);
        const rm: u5 = @truncate((insn_cmp >> 16) & 0x1F);
        if (rm == 31) continue; // CMP with XZR is already immediate-like

        // Scan backward for MOVZ rm, #imm where imm fits in 12 bits
        var found = false;
        var j: usize = i;
        while (j > 0) {
            j -= 1;
            const insn_j = readInsn(code, j);
            if (insn_j == NOP) continue;

            // MOVZ Xd, #imm16: 0xD2800000 | (imm16 << 5) | Rd
            if (insn_j & 0xFFE00000 == 0xD2800000) {
                const movz_rd: u5 = @truncate(insn_j & 0x1F);
                if (movz_rd != rm) {
                    // Different register — not our target, keep scanning
                    continue;
                }
                const movz_imm: u16 = @truncate((insn_j >> 5) & 0xFFFF);
                if (movz_imm > 4095) break; // Doesn't fit in CMP imm12

                // Check that rm is not used between MOVZ and CMP (other than by the CMP)
                var rm_used_between = false;
                var k = j + 1;
                while (k < i) : (k += 1) {
                    const insn_k = readInsn(code, k);
                    if (insn_k == NOP) continue;
                    // Check if rm appears as Rn (bits 9-5) or Rm (bits 20-16)
                    const k_rn: u5 = @truncate((insn_k >> 5) & 0x1F);
                    const k_rm: u5 = @truncate((insn_k >> 16) & 0x1F);
                    if (k_rn == rm or k_rm == rm) {
                        rm_used_between = true;
                        break;
                    }
                }

                // Replace CMP register with CMP immediate
                // CMP Xn, #imm12 = SUBS XZR, Xn, #imm12{shift=0}
                // Encoding: 1 1 1 1 0001 00 imm12 Rn 11111
                const cmp_imm: u32 = 0xF100001F | (@as(u32, movz_imm) << 10) | (@as(u32, rn) << 5);
                writeInsn(code, i, cmp_imm);

                // NOP the MOVZ if rm is not used elsewhere
                if (!rm_used_between) {
                    // Use CFG-aware liveness so branch-reachable uses of rm keep
                    // the MOVZ alive.
                    const dead_after = isRegDeadAfter(code, i, rm);
                    if (std.posix.getenv("HABU_TRACE_CMP_FUSE") != null) {
                        std.debug.print(
                            "TRACE cmp-fuse cmp_idx={d} movz_idx={d} rm=x{d} dead_after={any}\n",
                            .{ i, j, rm, dead_after },
                        );
                    }
                    if (dead_after) {
                        writeInsn(code, j, NOP);
                    }
                }
                found = true;
                break;
            }

            // Any non-MOVZ instruction that writes rm stops the scan
            // (MOVZ is handled above)
            if (insn_j & 0xFFE00000 != 0xD2800000) {
                const j_rd: u5 = @truncate(insn_j & 0x1F);
                if (j_rd == rm) break;
            }

            // Branches stop the scan
            if (insn_j & 0xFC000000 == 0x14000000 or // B
                insn_j & 0xFC000000 == 0x94000000 or // BL
                insn_j & 0xFF000010 == 0x54000000 or // B.cond
                insn_j & 0xFFFFFC1F == 0xD63F0000 or // BLR
                insn_j & 0xFFFFFC1F == 0xD65F0000) break; // RET
        }
        if (found) {}
    }
}

fn fuseSelectCondition(code: []u8) void {
    const NOP = @as(u32, 0xD503201F);
    const n_insns = code.len / 4;
    if (n_insns < 4) return;

    var i: usize = 0;
    while (i + 3 < n_insns) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);

        // insn0: must be a CMP (SUBS with Rd=XZR/WZR)
        // Register: 64-bit EB..001F, 32-bit 6B..001F
        // Immediate: 64-bit F1..001F, 32-bit 71..001F
        const is_cmp = (insn0 & 0x7FE0001F == 0x6B00001F) or // SUBS reg Wd/Xd
            (insn0 & 0xFFE0001F == 0xEB00001F) or // SUBS reg Xd
            (insn0 & 0x7F80001F == 0x7100001F) or // SUBS imm Wd/Xd
            (insn0 & 0xFF80001F == 0xF100001F); // SUBS imm Xd
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

    // Find the first non-prologue/non-MOV instruction to determine entry region end.
    // Don't eliminate round-trips in the entry param copy region — those are
    // hoist's broken parallel copies for parameter shuffles (swap attempts).
    var entry_end: usize = 0;
    for (0..@min(n_insns, 16)) |idx| {
        const insn = readInsn(code, idx);
        const is_stp = (insn & 0xFFC07FFF) == 0xA9807BFD;
        const is_mov_fp = (insn == 0xAA1F03FD) or (insn == 0x910003FD);
        const is_mov = (insn & 0xFFE0FFE0 == 0xAA0003E0);
        const is_stp_callee = (insn & 0xFFC003E0) == 0xA90003E0; // STP pair callee-saved
        if (is_stp or is_mov_fp or is_mov or is_stp_callee) {
            entry_end = idx + 1;
        } else break;
    }

    // Only scan after the entry region
    const scan_limit = @min(n_insns, 20);
    var i: usize = entry_end;
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
                // Safe elimination requires:
                // 1) rd_i is neither read nor written between i and j
                // 2) rm_i is not overwritten between i and j
                // 3) rd_i is dead after j on all paths
                var safe = true;
                for (i + 1..j) |k| {
                    const insn_k = readInsn(code, k);
                    if (insn_k == nop) continue;

                    // Control-flow between the pair can make local reasoning invalid.
                    if (insn_k & 0xFC000000 == 0x14000000 or // B
                        insn_k & 0xFF000000 == 0x54000000 or // B.cond
                        insn_k & 0xFFFFFC1F == 0xD65F0000 or // RET
                        insn_k & 0xFFFFFC1F == 0xD63F0000 or // BLR
                        insn_k & 0xFC000000 == 0x94000000) // BL
                    {
                        safe = false;
                        break;
                    }

                    if (insnReadsReg(insn_k, rd_i) or insnWritesReg(insn_k, rd_i)) {
                        safe = false;
                        break;
                    }
                    if (insnWritesReg(insn_k, rm_i)) {
                        safe = false;
                        break;
                    }
                }
                if (!safe) continue;
                if (!isRegDeadAfter(code, j, rd_i)) continue;

                writeInsn(code, i, nop);
                writeInsn(code, j, nop);
                break; // Move to next i
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
/// Eliminate dead MOV x29, xzr (clear frame pointer) in prologue.
/// Hoist emits this after STP x29,x30,[sp,...] but we don't use x29.
/// Eliminate dead MOV x29, xzr in prologue. Hoist clears the frame pointer after
/// saving it with STP, but we never use x29 as a frame pointer. The epilogue LDP
/// restores x29, so the MOV is dead. Safe because our generated code never
/// references x29 between prologue and epilogue.
fn eliminateDeadFramePointerClear(code: []u8) void {
    const n = code.len / 4;
    if (n < 2) return;
    // Look for MOV x29, xzr (= ORR x29, xzr, xzr = 0xAA1F03FD) in first 4 insns
    const limit = @min(n, 4);
    for (0..limit) |i| {
        if (readInsn(code, i) == 0xAA1F03FD) {
            // Verify x29 is not used as source in any instruction (excluding prologue/epilogue)
            var used = false;
            for (0..n) |j| {
                const insn = readInsn(code, j);
                // Skip STP/LDP (prologue/epilogue save/restore)
                if (insn & 0x7F000000 == 0x29000000 or // STP/LDP signed offset
                    insn & 0x7F000000 == 0x28000000 or // STP/LDP post-index
                    insn & 0x7F800000 == 0x29800000) // STP/LDP pre-index
                    continue;
                if (insn == 0xAA1F03FD) continue; // the MOV itself
                if (insn == 0xD503201F) continue; // NOP
                // Check if any other instruction reads x29
                if (insnReadsReg(insn, 29)) {
                    used = true;
                    break;
                }
            }
            if (!used) {
                writeInsn(code, i, 0xD503201F); // NOP
            }
            return;
        }
    }
}

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

/// Eliminate dead MOV instructions that appear before unconditional branches.
/// Pattern: `MOV Xd, Xs; B target` where target is:
/// Check if MOVZ dest reg is safe to eliminate (dead after consumer).
/// Uses deep analysis first, falls back to basic-block-local check.
fn isMovzRegDead(code: []const u8, alu_idx: usize, reg: u5) bool {
    return isRegDeadAfter(code, alu_idx, reg);
}

fn hasControlFlowBeforeWrite(code: []const u8, start_idx: usize, reg: u5) bool {
    const n = code.len / 4;
    var j = start_idx + 1;
    while (j < n) : (j += 1) {
        const insn = readInsn(code, j);
        if (insn == 0xD503201F) continue;
        if (insnWritesReg(insn, reg)) return false;
        if (insn & 0xFC000000 == 0x14000000 or // B
            insn & 0xFF000000 == 0x54000000 or // B.cond
            insn & 0xFC000000 == 0x94000000 or // BL
            insn & 0xFFFFFC1F == 0xD63F0000 or // BLR
            insn & 0xFFFFFC1F == 0xD65F0000) // RET
            return true;
    }
    return false;
}

/// Fuse adjacent MOVZ+ALU pairs into ALU-immediate form.
/// Patterns:
///   MOVZ Rn, #imm; ADD Rd, Rm, Rn  → ADD Rd, Rm, #imm; NOP
///   MOVZ Rn, #imm; SUB Rd, Rm, Rn  → SUB Rd, Rm, #imm; NOP
/// Only when:
///   - imm fits in 12 bits (0..4095)
///   - Rn (MOVZ dest) is dead after the ALU op (not read before overwrite)
fn fuseMovzAlu(code: []u8) void {
    const NOP: u32 = 0xD503201F;
    const n = code.len / 4;
    if (n < 2) return;

    var i: usize = 0;
    while (i + 1 < n) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);

        // Check insn0 is MOVZ Xd, #imm16 (64-bit): 1_10_100101_00_imm16_Rd
        if (insn0 & 0xFF800000 != 0xD2800000) continue;
        const movz_rd: u5 = @truncate(insn0 & 0x1F);
        const imm16: u32 = (insn0 >> 5) & 0xFFFF;

        // Only fuse small immediates that fit in 12-bit immediate field
        if (imm16 > 4095) continue;

        // Pattern 1: ADD Xd, Xn, Xm where Xm == movz_rd
        // ADD (shifted reg): 1_00_01011_sh_0_Rm_imm6_Rn_Rd
        if (insn1 & 0xFF200000 == 0x8B000000) {
            const alu_rm: u5 = @truncate((insn1 >> 16) & 0x1F);
            const alu_rn: u5 = @truncate((insn1 >> 5) & 0x1F);
            const alu_rd: u5 = @truncate(insn1 & 0x1F);
            const shift_imm6 = (insn1 >> 10) & 0x3F;
            if (alu_rm == movz_rd and shift_imm6 == 0) {
                // Check movz_rd is dead after this ADD
                if (isMovzRegDead(code, i + 1, movz_rd)) {
                    // ADD Xd, Xn, #imm12: 1_00_100010_0_imm12_Rn_Rd
                    const new_insn: u32 = 0x91000000 | (imm16 << 10) | (@as(u32, alu_rn) << 5) | @as(u32, alu_rd);
                    writeInsn(code, i, NOP);
                    writeInsn(code, i + 1, new_insn);
                    i += 1;
                    continue;
                }
            }
            // Also check Xn == movz_rd (commutative): ADD Xd, Xmovz, Xother
            if (alu_rn == movz_rd and shift_imm6 == 0 and alu_rm != movz_rd) {
                if (isMovzRegDead(code, i + 1, movz_rd)) {
                    const new_insn: u32 = 0x91000000 | (imm16 << 10) | (@as(u32, alu_rm) << 5) | @as(u32, alu_rd);
                    writeInsn(code, i, NOP);
                    writeInsn(code, i + 1, new_insn);
                    i += 1;
                    continue;
                }
            }
        }

        // Pattern 2: SUB Xd, Xn, Xm where Xm == movz_rd
        // SUB (shifted reg): 1_10_01011_sh_0_Rm_imm6_Rn_Rd
        if (insn1 & 0xFF200000 == 0xCB000000) {
            const alu_rm: u5 = @truncate((insn1 >> 16) & 0x1F);
            const alu_rn: u5 = @truncate((insn1 >> 5) & 0x1F);
            const alu_rd: u5 = @truncate(insn1 & 0x1F);
            const shift_imm6 = (insn1 >> 10) & 0x3F;
            if (alu_rm == movz_rd and shift_imm6 == 0) {
                if (isMovzRegDead(code, i + 1, movz_rd)) {
                    // SUB Xd, Xn, #imm12: 1_10_100010_0_imm12_Rn_Rd
                    const new_insn: u32 = 0xD1000000 | (imm16 << 10) | (@as(u32, alu_rn) << 5) | @as(u32, alu_rd);
                    writeInsn(code, i, NOP);
                    writeInsn(code, i + 1, new_insn);
                    i += 1;
                    continue;
                }
            }
        }

        // Pattern 3: CMP Xn, Xm (SUBS XZR, Xn, Xm) where Xm == movz_rd
        // SUBS (shifted reg): 1_11_01011_sh_0_Rm_imm6_Rn_Rd
        if (insn1 & 0xFF200000 == 0xEB000000) {
            const alu_rm: u5 = @truncate((insn1 >> 16) & 0x1F);
            const alu_rn: u5 = @truncate((insn1 >> 5) & 0x1F);
            const alu_rd: u5 = @truncate(insn1 & 0x1F);
            const shift_imm6 = (insn1 >> 10) & 0x3F;
            if (alu_rm == movz_rd and shift_imm6 == 0) {
                if (isMovzRegDead(code, i + 1, movz_rd)) {
                    // SUBS Xd, Xn, #imm12: 1_11_100010_0_imm12_Rn_Rd
                    const new_insn: u32 = 0xF1000000 | (imm16 << 10) | (@as(u32, alu_rn) << 5) | @as(u32, alu_rd);
                    writeInsn(code, i, NOP);
                    writeInsn(code, i + 1, new_insn);
                    i += 1;
                    continue;
                }
            }
        }

        // Pattern 4: MOV Rd, Rn where Rn == movz_rd → MOVZ Rd, #imm
        // MOV is ORR Rd, XZR, Rm: 1_01_01010_00_0_Rm_000000_11111_Rd
        if (insn1 & 0xFFE0FFE0 == 0xAA0003E0) {
            const mov_rm: u5 = @truncate((insn1 >> 16) & 0x1F);
            const mov_rd: u5 = @truncate(insn1 & 0x1F);
            if (mov_rm == movz_rd) {
                if (hasControlFlowBeforeWrite(code, i + 1, movz_rd)) continue;
                if (isMovzRegDead(code, i + 1, movz_rd)) {
                    // MOVZ Xd, #imm16: 1_10_100101_00_imm16_Rd
                    const new_insn: u32 = 0xD2800000 | (imm16 << 5) | @as(u32, mov_rd);
                    writeInsn(code, i, NOP);
                    writeInsn(code, i + 1, new_insn);
                    i += 1;
                    continue;
                }
            }
        }
    }
}

/// Check if register `reg` is dead after instruction at `idx`.
/// Scans forward following branches up to depth limit.
/// Returns true if reg is overwritten before any read.
fn isRegDeadAfter(code: []const u8, idx: usize, reg: u5) bool {
    const n = code.len / 4;
    if (n == 0 or idx + 1 >= n) return true;

    const max_insns = 4096;
    if (n > max_insns) return false;

    var memo: [max_insns]u8 = [_]u8{0} ** max_insns;
    return isRegDeadFrom(code, idx + 1, reg, memo[0..n]);
}

/// Check if register `reg` is dead within the current basic block after `idx`.
/// Does NOT follow branches — only scans to next branch/call/ret.
/// Returns true if reg is overwritten or unused within the block.
fn isRegDeadInBlock(code: []const u8, idx: usize, reg: u5) bool {
    const n = code.len / 4;
    var j: usize = idx + 1;
    while (j < n) : (j += 1) {
        const insn = readInsn(code, j);
        if (insn == 0xD503201F) continue; // NOP
        // RET reads x0 as the return value register.
        if (insn == 0xD65F03C0) {
            return reg != 0;
        }
        // Stop at any branch/call.
        if (insn & 0xFC000000 == 0x14000000 or // B
            insn & 0xFF000000 == 0x54000000 or // B.cond
            insn & 0xFC000000 == 0x94000000 or // BL
            insn & 0xFFFFFC1F == 0xD63F0000) // BLR
        {
            return true; // reached end of basic block without reading reg
        }
        if (insnReadsReg(insn, reg)) return false;
        if (insnWritesReg(insn, reg)) return true;
    }
    return true; // end of function
}

/// Check if register `reg` is dead starting at `start_idx`.
/// Memoized DFS over control flow: dead only if ALL reachable paths overwrite
/// or terminate before any read.
fn isRegDeadFrom(code: []const u8, start_idx: usize, reg: u5, memo: []u8) bool {
    const n = code.len / 4;
    if (start_idx >= n) return true;

    const status = memo[start_idx];
    if (status == 2) return true;
    if (status == 3) return false;
    if (status == 1) return false; // Loop/recurrence: conservatively live.
    memo[start_idx] = 1;

    const insn = readInsn(code, start_idx);
    if (insn == 0xD503201F) {
        const dead = isRegDeadFrom(code, start_idx + 1, reg, memo);
        memo[start_idx] = if (dead) 2 else 3;
        return dead;
    }

    // Calls/returns/branches first (control-flow boundaries).
    if (insn & 0xFC000000 == 0x94000000) { // BL
        const dead = if (reg <= 8) false else if (reg <= 17) true else false;
        memo[start_idx] = if (dead) 2 else 3;
        return dead;
    }
    if (insn & 0xFFFFFC1F == 0xD63F0000) { // BLR
        const target_reg: u5 = @truncate((insn >> 5) & 0x1F);
        const dead = if (reg == target_reg) false else if (reg <= 8) false else if (reg <= 17) true else false;
        memo[start_idx] = if (dead) 2 else 3;
        return dead;
    }
    if (insn == 0xD65F03C0) { // RET
        // RET reads x0 as the function result register.
        const dead = reg != 0;
        memo[start_idx] = if (dead) 2 else 3;
        return dead;
    }
    if (insn & 0xFC000000 == 0x14000000) { // B
        const imm26_raw = insn & 0x3FFFFFF;
        const imm26: i32 = if (imm26_raw & 0x2000000 != 0)
            @as(i32, @intCast(imm26_raw)) - 0x4000000
        else
            @intCast(imm26_raw);
        const target: i32 = @as(i32, @intCast(start_idx)) + imm26;
        if (target < 0 or target >= @as(i32, @intCast(n))) {
            memo[start_idx] = 3;
            return false;
        }
        const dead = isRegDeadFrom(code, @intCast(target), reg, memo);
        memo[start_idx] = if (dead) 2 else 3;
        return dead;
    }
    if (insn & 0xFF000000 == 0x54000000) { // B.cond
        const imm19_raw = (insn >> 5) & 0x7FFFF;
        const imm19: i32 = if (imm19_raw & 0x40000 != 0)
            @as(i32, @intCast(imm19_raw)) - 0x80000
        else
            @intCast(imm19_raw);
        const taken_target: i32 = @as(i32, @intCast(start_idx)) + imm19;
        if (taken_target < 0 or taken_target >= @as(i32, @intCast(n))) {
            memo[start_idx] = 3;
            return false;
        }
        const taken_dead = isRegDeadFrom(code, @intCast(taken_target), reg, memo);
        const fall_dead = if (start_idx + 1 < n)
            isRegDeadFrom(code, start_idx + 1, reg, memo)
        else
            true;
        const dead = taken_dead and fall_dead;
        memo[start_idx] = if (dead) 2 else 3;
        return dead;
    }

    if (insnReadsReg(insn, reg)) {
        memo[start_idx] = 3;
        return false;
    }
    if (insnWritesReg(insn, reg)) {
        memo[start_idx] = 2;
        return true;
    }

    const dead = isRegDeadFrom(code, start_idx + 1, reg, memo);
    memo[start_idx] = if (dead) 2 else 3;
    return dead;
}

/// Check if an ARM64 instruction writes to a specific register.
fn isLoadStoreImmediate(insn: u32) bool {
    // Unsigned offset loads/stores (includes 8/16/32/64/128-bit families).
    if (insn & 0x3B000000 == 0x39000000) return true;

    // Unscaled/pre/post-index loads/stores.
    const mode = insn & 0x3B200C00;
    return mode == 0x38000000 or // STUR
        mode == 0x38400000 or // LDUR
        mode == 0x38000400 or // STR post-index
        mode == 0x38400400 or // LDR post-index
        mode == 0x38000C00 or // STR pre-index
        mode == 0x38400C00; // LDR pre-index
}

fn isStoreImmediate(insn: u32) bool {
    if (!isLoadStoreImmediate(insn)) return false;
    const opc: u2 = @truncate((insn >> 22) & 0x3);
    return opc == 0; // store
}

fn isLoadImmediate(insn: u32) bool {
    if (!isLoadStoreImmediate(insn)) return false;
    const opc: u2 = @truncate((insn >> 22) & 0x3);
    return opc != 0; // load / load+extend
}

fn hasLoadStoreWriteback(insn: u32) bool {
    const mode = insn & 0x3B200C00;
    return mode == 0x38000400 or // post-index
        mode == 0x38400400 or // post-index
        mode == 0x38000C00 or // pre-index
        mode == 0x38400C00; // pre-index
}

fn insnWritesReg(insn: u32, reg: u5) bool {
    // Most data-processing instructions write to Rd (bits 4:0)
    const rd: u5 = @truncate(insn & 0x1F);

    // MOVZ/MOVK/MOVN: writes Rd
    if (insn & 0x1F800000 == 0x12800000) return rd == reg; // MOVN
    if (insn & 0x1F800000 == 0x12000000) return rd == reg; // ORR-imm (used for MOV bitmask)
    if (insn & 0xFF800000 == 0xD2800000) return rd == reg; // MOVZ 64-bit
    if (insn & 0xFF800000 == 0xF2800000) return rd == reg; // MOVK 64-bit

    // ADD/SUB (shifted reg and immediate): writes Rd
    if (insn & 0x1F000000 == 0x0B000000) return rd == reg; // ADD/SUB shifted
    if (insn & 0x1F000000 == 0x11000000) return rd == reg; // ADD/SUB immediate

    // Logical (shifted reg): writes Rd
    if (insn & 0x1F000000 == 0x0A000000) return rd == reg; // AND/ORR/EOR/etc (includes MOV)

    // MUL/MADD/MSUB: writes Rd
    if (insn & 0x1F800000 == 0x1B000000) return rd == reg;

    // Load/store immediate classes (unsigned + unscaled/pre/post-index).
    if (isLoadImmediate(insn)) {
        const rt: u5 = @truncate(insn & 0x1F);
        if (rt == reg) return true;
        // Pre/post-index forms update the base register as well.
        if (hasLoadStoreWriteback(insn)) {
            const rn: u5 = @truncate((insn >> 5) & 0x1F);
            if (rn == reg) return true;
        }
        return false;
    }
    if (isStoreImmediate(insn) and hasLoadStoreWriteback(insn)) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        return rn == reg;
    }

    // LDP: writes Rt (bits 4:0) AND Rt2 (bits 14:10)
    if (insn & 0x7FC00000 == 0xA9400000 or insn & 0x7FE00000 == 0xA8C00000 or
        insn & 0x7FC00000 == 0xA9C00000)
    {
        const rt: u5 = @truncate(insn & 0x1F);
        const rt2: u5 = @truncate((insn >> 10) & 0x1F);
        return rt == reg or rt2 == reg;
    }

    // CSEL/CSINC/CSNEG: writes Rd
    if (insn & 0x1FE00000 == 0x1A800000) return rd == reg;

    return false;
}

/// Check if an ARM64 instruction reads a specific register.
fn insnReadsReg(insn: u32, reg: u5) bool {
    // ADD/SUB (shifted reg): reads Rn (bits 9:5) and Rm (bits 20:16)
    if (insn & 0x1F000000 == 0x0B000000) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        const rm: u5 = @truncate((insn >> 16) & 0x1F);
        return rn == reg or rm == reg;
    }

    // ADD/SUB (immediate): reads Rn (bits 9:5)
    if (insn & 0x1F000000 == 0x11000000) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        return rn == reg;
    }

    // Logical (shifted reg): reads Rn and Rm
    if (insn & 0x1F000000 == 0x0A000000) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        const rm: u5 = @truncate((insn >> 16) & 0x1F);
        return rn == reg or rm == reg;
    }

    // MUL/MADD/MSUB: reads Rn, Rm, Ra
    if (insn & 0x1F800000 == 0x1B000000) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        const rm: u5 = @truncate((insn >> 16) & 0x1F);
        const ra: u5 = @truncate((insn >> 10) & 0x1F);
        return rn == reg or rm == reg or ra == reg;
    }

    // Store immediate classes: reads Rt (data) and Rn (base)
    if (isStoreImmediate(insn)) {
        const rt: u5 = @truncate(insn & 0x1F);
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        return rt == reg or rn == reg;
    }

    // STP: reads Rt, Rt2, Rn
    if (insn & 0x7FC00000 == 0xA9000000 or insn & 0x7FE00000 == 0xA9800000) {
        const rt: u5 = @truncate(insn & 0x1F);
        const rt2: u5 = @truncate((insn >> 10) & 0x1F);
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        return rt == reg or rt2 == reg or rn == reg;
    }

    // CSEL/CSINC/CSNEG: reads Rn and Rm
    if (insn & 0x1FE00000 == 0x1A800000) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        const rm: u5 = @truncate((insn >> 16) & 0x1F);
        return rn == reg or rm == reg;
    }

    // Load immediate classes: reads Rn (base)
    if (isLoadImmediate(insn)) {
        const rn: u5 = @truncate((insn >> 5) & 0x1F);
        return rn == reg;
    }

    // MOVZ/MOVK/MOVN: only writes, doesn't read (except MOVK reads Rd implicitly)
    if (insn & 0xFF800000 == 0xF2800000) { // MOVK reads Rd
        const rd: u5 = @truncate(insn & 0x1F);
        return rd == reg;
    }

    return false;
}

/// Eliminate dead MOVZ instructions where the dest register is overwritten
/// before being read. Common when hoist materializes constants that CMP
/// already uses as immediates.
fn eliminateDeadMovz(code: []u8) void {
    const NOP: u32 = 0xD503201F;
    const n = code.len / 4;

    var i: usize = 0;
    while (i < n) : (i += 1) {
        const insn = readInsn(code, i);
        // Check for MOVZ Xd, #imm16: 1_10_100101_00_imm16_Rd
        if (insn & 0xFF800000 != 0xD2800000) continue;
        const rd: u5 = @truncate(insn & 0x1F);
        if (isRegDeadAfter(code, i, rd)) {
            writeInsn(code, i, NOP);
        }
    }
}

///   1. The epilogue (LDP; LDP; RET) — Xd is dead
///   2. Another `MOV Xd2, Xs2; B target2` chain — Xd is dead if Xd != Xs2
/// These arise from hoist's trampoline blocks that insert unnecessary phi copies.
fn eliminateDeadMovBeforeBranch(code: []u8) void {
    const NOP: u32 = 0xD503201F;
    const n = code.len / 4;
    if (n < 2) return;

    var i: usize = 0;
    while (i + 1 < n) : (i += 1) {
        const insn0 = readInsn(code, i);
        const insn1 = readInsn(code, i + 1);

        // Check insn0 is MOV Xd, Xm: ORR Xd, XZR, Xm
        // Encoding: 1_01_01010_00_0_Rm_000000_11111_Rd
        if (insn0 & 0xFFE0FFE0 != 0xAA0003E0) continue;
        const rd: u5 = @truncate(insn0 & 0x1F);

        // Check insn1 is unconditional B
        if (insn1 & 0xFC000000 != 0x14000000) continue;

        // Resolve the B target
        const imm26_raw = insn1 & 0x3FFFFFF;
        const imm26: i32 = if (imm26_raw & 0x2000000 != 0)
            @as(i32, @intCast(imm26_raw)) - 0x4000000
        else
            @intCast(imm26_raw);
        const target_idx: i32 = @intCast(i + 1);
        const target: i32 = target_idx + imm26;
        if (target < 0 or target >= @as(i32, @intCast(n))) continue;
        const tidx: usize = @intCast(target);

        // Check if rd is dead at target:
        // 1. Target is epilogue (starts with LDP)
        const target_insn = readInsn(code, tidx);
        const is_ldp = (target_insn & 0xFFC00000 == 0xA9400000) or // LDP signed offset
            (target_insn & 0xFFE00000 == 0xA8C00000); // LDP post-index

        if (is_ldp) {
            // Epilogue: rd is dead (only x0 matters for return)
            if (rd != 0) { // don't NOP if rd is x0 (return value)
                writeInsn(code, i, NOP);
            }
            continue;
        }

        // 2. Target is another MOV; B chain
        if (tidx + 1 < n) {
            const t0 = readInsn(code, tidx);
            const t1 = readInsn(code, tidx + 1);
            if (t0 & 0xFFE0FFE0 == 0xAA0003E0 and t1 & 0xFC000000 == 0x14000000) {
                // Target is MOV Xd2, Xs2; B
                const xs2: u5 = @truncate((t0 >> 16) & 0x1F);
                if (rd != xs2 and rd != 0) {
                    // rd is not read at target (target reads xs2, writes xd2)
                    writeInsn(code, i, NOP);
                }
            }
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
/// Fix parallel copy conflicts in function entry parameter moves.
/// Hoist emits sequential `MOV xD, xS` to copy params from ABI registers (x0-x7)
/// to work registers. If a later MOV reads a register that an earlier MOV already
/// wrote, we have a conflict. For circular dependencies (swap), we insert an extra
/// instruction using x9 as scratch.
fn fixEntryParamMovesAlloc(allocator: std.mem.Allocator, code_list: *std.ArrayList(u8)) !void {
    const code = code_list.items;
    if (code.len < 8) return;
    const n_insns = code.len / 4;

    // Find first entry MOV that reads an ABI argument register.
    var first_mov: ?usize = null;
    for (0..@min(n_insns, 24)) |insn_idx| {
        const insn = readInsn(code, insn_idx);
        if (insn & 0xFFE0FFE0 != 0xAA0003E0) continue;
        const src: u5 = @truncate((insn >> 16) & 0x1F);
        if (src <= 7) {
            first_mov = insn_idx;
            break;
        }
    }
    const start = first_mov orelse return;

    // Collect consecutive MOVs in the entry copy region.
    const MovInfo = struct { src: u5, dst: u5, pos: usize };
    const max_movs = 12;
    var movs: [max_movs]MovInfo = undefined;
    var n_movs: usize = 0;
    var idx = start;
    while (idx < @min(n_insns, start + max_movs) and n_movs < max_movs) : (idx += 1) {
        const insn = readInsn(code, idx);
        if (insn & 0xFFE0FFE0 != 0xAA0003E0) break;
        movs[n_movs] = .{
            .src = @truncate((insn >> 16) & 0x1F),
            .dst = @truncate(insn & 0x1F),
            .pos = idx,
        };
        n_movs += 1;
    }
    if (n_movs < 2) return;

    // Quick reject: no overwritten source.
    var has_conflict = false;
    for (1..n_movs) |a| {
        for (0..a) |b| {
            if (movs[b].dst == movs[a].src) {
                has_conflict = true;
                break;
            }
        }
        if (has_conflict) break;
    }
    if (!has_conflict) return;

    // Symbolically execute the original move chain so we preserve semantics of
    // dependent chains such as "mov x5, x1; mov x2, x5" (x2 must get x1).
    var state: [32]u5 = undefined;
    for (0..state.len) |r| state[r] = @intCast(r);
    for (0..n_movs) |mi| {
        state[movs[mi].dst] = state[movs[mi].src];
    }

    const Assign = struct { dst: u5, src: u5 };
    var assigns: [max_movs]Assign = undefined;
    var n_assigns: usize = 0;
    for (0..n_movs) |mi| {
        const dst = movs[mi].dst;
        var seen = false;
        for (0..n_assigns) |ai| {
            if (assigns[ai].dst == dst) {
                seen = true;
                break;
            }
        }
        if (seen) continue;

        const src = state[dst];
        if (src == dst) continue;
        assigns[n_assigns] = .{ .dst = dst, .src = src };
        n_assigns += 1;
    }
    if (n_assigns == 0) return;

    const regUsed = struct {
        fn f(assigns_slice: []const Assign, reg: u5) bool {
            for (assigns_slice) |a| {
                if (a.src == reg or a.dst == reg) return true;
            }
            return false;
        }
    }.f;

    var scratch: u5 = 9;
    while (scratch < 28 and regUsed(assigns[0..n_assigns], scratch)) : (scratch += 1) {}
    if (scratch >= 28) scratch = 9;

    // Resolve normalized assignments with a cycle-safe parallel-copy schedule.
    var emitted: [max_movs]bool = .{false} ** max_movs;
    var emitted_count: usize = 0;
    var result: [max_movs * 2]u32 = undefined;
    var n_result: usize = 0;

    while (emitted_count < n_assigns) {
        var progressed = false;

        for (0..n_assigns) |ai| {
            if (emitted[ai]) continue;
            const dst = assigns[ai].dst;
            var dst_needed = false;
            for (0..n_assigns) |other| {
                if (ai == other or emitted[other]) continue;
                if (assigns[other].src == dst) {
                    dst_needed = true;
                    break;
                }
            }
            if (!dst_needed) {
                result[n_result] = makeMovInsn(dst, assigns[ai].src);
                n_result += 1;
                emitted[ai] = true;
                emitted_count += 1;
                progressed = true;
            }
        }

        if (progressed) continue;

        // Break a cycle by preserving one source in scratch.
        var cycle_idx: ?usize = null;
        for (0..n_assigns) |ai| {
            if (!emitted[ai]) {
                cycle_idx = ai;
                break;
            }
        }
        const ci = cycle_idx orelse break;
        result[n_result] = makeMovInsn(scratch, assigns[ci].src);
        n_result += 1;
        assigns[ci].src = scratch;
    }

    // Replace original MOV slots with the resolved sequence.
    if (n_result <= n_movs) {
        for (0..n_movs) |mi| {
            if (mi < n_result) {
                writeInsn(code, movs[mi].pos, result[mi]);
            } else {
                writeInsn(code, movs[mi].pos, 0xD503201F);
            }
        }
    } else {
        for (0..n_movs) |mi| {
            writeInsn(code, movs[mi].pos, result[mi]);
        }
        const insert_byte_pos = (movs[n_movs - 1].pos + 1) * 4;
        for (n_movs..n_result) |ri| {
            const bytes: [4]u8 = @bitCast(result[ri]);
            try code_list.insertSlice(allocator, insert_byte_pos + (ri - n_movs) * 4, &bytes);
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
    const NOP: u32 = 0xD503201F;
    const n_insns = code.len / 4;
    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);
        // B .+4: branch to next instruction → NOP
        if (insn == 0x14000001) {
            writeInsn(code, i, NOP);
            continue;
        }
        // Branch chain: B target where target is also B target2 → B target2
        if (insn & 0xFC000000 == 0x14000000) {
            const imm26_raw = insn & 0x3FFFFFF;
            const imm26: i32 = if (imm26_raw & 0x2000000 != 0)
                @as(i32, @intCast(imm26_raw)) - 0x4000000
            else
                @intCast(imm26_raw);
            const target: i32 = @as(i32, @intCast(i)) + imm26;
            if (target < 0 or target >= @as(i32, @intCast(n_insns))) continue;
            const tidx: usize = @intCast(target);
            const target_insn = readInsn(code, tidx);
            // Skip NOPs at target
            if (target_insn == NOP and tidx + 1 < n_insns) {
                // Follow NOP to next instruction
                continue;
            }
            // Check if target is another unconditional B
            if (target_insn & 0xFC000000 == 0x14000000) {
                // Resolve target's offset
                const t_imm26_raw = target_insn & 0x3FFFFFF;
                const t_imm26: i32 = if (t_imm26_raw & 0x2000000 != 0)
                    @as(i32, @intCast(t_imm26_raw)) - 0x4000000
                else
                    @intCast(t_imm26_raw);
                const final_target: i32 = @as(i32, @intCast(tidx)) + t_imm26;
                // Compute new offset from i to final_target
                const new_offset: i32 = final_target - @as(i32, @intCast(i));
                if (new_offset >= -0x2000000 and new_offset < 0x2000000) {
                    const new_imm26: u32 = @as(u32, @bitCast(new_offset)) & 0x3FFFFFF;
                    writeInsn(code, i, 0x14000000 | new_imm26);
                }
            }
        }
    }
}

/// Coalesce: for ALU ops where the result is only used by a later mov,
/// change the ALU op's destination to the mov's destination and NOP the mov.
/// Handles non-adjacent pairs: `add rD, rA, rB; ...; mov rC, rD` → `add rC, rA, rB; ...; nop`
/// Return true if `reg` is used as a BLR target before being overwritten.
/// Coalescing into such a register can destroy indirect-call targets.
fn usedAsBlrTargetBeforeRedef(code: []u8, from: usize, reg: u5) bool {
    const n_insns = code.len / 4;
    var i = from;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);
        if (insn == 0xD503201F) continue; // NOP

        // BLR Xn: opcode mask 0xFFFFFC1F, register in bits [9:5]
        if (insn & 0xFFFFFC1F == 0xD63F0000) {
            const target: u5 = @truncate((insn >> 5) & 0x1F);
            return target == reg;
        }

        // Stop at control-flow boundaries.
        if (insn & 0xFC000000 == 0x14000000 or // B
            insn & 0xFF000000 == 0x54000000 or // B.cond
            insn & 0xFFFFFC1F == 0xD65F0000 or // RET
            insn & 0xFC000000 == 0x94000000) // BL
            return false;

        // If register is overwritten first, BLR target use is no longer relevant.
        const rd: u5 = @truncate(insn & 0x1F);
        if (rd == reg) return false;
    }
    return false;
}

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
            const is_safe_alu = (op_class == 0x8B or // ADD (shifted reg)
                op_class == 0xCB or // SUB (shifted reg)
                op_class == 0x91 or // ADD (immediate)
                op_class == 0xD1 or // SUB (immediate)
                op_class == 0x9B or // MADD/MUL
                op_class == 0x8A or // AND (shifted reg)
                op_class == 0x92 or // AND (immediate)
                op_class == 0xD3); // LSL/LSR/ASR (shift imm)
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

            // rd0 must be dead on all paths after the MOV copy.
            if (!isRegDeadAfter(code, mi, rd0)) continue;

            // Also check that mov_dst is not written between ALU op and mov
            var safe = true;
            var j2 = i + 1;
            while (j2 < mi) : (j2 += 1) {
                const between = readInsn(code, j2);
                if (between == 0xD503201F) continue; // NOP
                const rd_b: u5 = @truncate(between & 0x1F);
                const rn_b: u5 = @truncate((between >> 5) & 0x1F);
                const rm_b: u5 = @truncate((between >> 16) & 0x1F);
                // Moving the write to mov_dst earlier must not clobber a live value.
                if (rn_b == mov_dst or rm_b == mov_dst) {
                    safe = false;
                    break;
                }
                if (rd_b == mov_dst) {
                    safe = false;
                    break;
                }
            }
            if (!safe) continue;

            // Don't coalesce if mov_dst is the upcoming indirect-call target.
            if (usedAsBlrTargetBeforeRedef(code, mi + 1, mov_dst)) continue;

            // Coalesce: change ALU destination to mov_dst, NOP the mov
            const patched = (insn0 & ~@as(u32, 0x1F)) | @as(u32, mov_dst);
            writeInsn(code, i, patched);
            writeInsn(code, mi, 0xD503201F); // NOP
            changed = true;
        }
    }
}

fn pickScratchForRange(code: []const u8, start_idx: usize, end_idx: usize, avoid: u5) u5 {
    var reg: u5 = 9;
    while (reg < 28) : (reg += 1) {
        if (reg == avoid) continue;
        var used = false;
        var i = start_idx;
        while (i <= end_idx) : (i += 1) {
            const insn = readInsn(code, i);
            if (insn == 0xD503201F) continue;
            const rd: u5 = @truncate(insn & 0x1F);
            const rn: u5 = @truncate((insn >> 5) & 0x1F);
            const rm: u5 = @truncate((insn >> 16) & 0x1F);
            if (rd == reg or rn == reg or rm == reg) {
                used = true;
                break;
            }
        }
        if (!used) return reg;
    }
    return avoid;
}

fn fixBlrTargetClobber(code: []u8) void {
    if (code.len < 8) return;
    const n_insns = code.len / 4;

    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);
        if (insn & 0xFFFFFC1F != 0xD63F0000) continue; // BLR

        const target: u5 = @truncate((insn >> 5) & 0x1F);

        var load_pos: ?usize = null;
        var load_src: u5 = 0;

        var j = i;
        var steps: usize = 0;
        while (j > 0 and steps < 12) : (steps += 1) {
            j -= 1;
            const prev = readInsn(code, j);
            if (prev == 0xD503201F) continue;

            if (prev & 0xFFE0FFE0 == 0xAA0003E0) { // MOV Xd, Xm
                const rd: u5 = @truncate(prev & 0x1F);
                const rm: u5 = @truncate((prev >> 16) & 0x1F);
                if (rd == target) {
                    load_pos = j;
                    load_src = rm;
                    break;
                }
            }

            if (prev & 0xFC000000 == 0x14000000 or // B
                prev & 0xFF000000 == 0x54000000 or // B.cond
                prev & 0xFFFFFC1F == 0xD65F0000 or // RET
                prev & 0xFFFFFC1F == 0xD63F0000 or // BLR
                prev & 0xFC000000 == 0x94000000) // BL
                break;
        }

        const pos = load_pos orelse continue;

        var clobbered = false;
        var k = pos + 1;
        while (k < i) : (k += 1) {
            const mid = readInsn(code, k);
            if (mid == 0xD503201F) continue;
            const rd: u5 = @truncate(mid & 0x1F);
            if (rd == target) {
                clobbered = true;
                break;
            }
        }
        if (!clobbered) continue;

        const scratch = pickScratchForRange(code, pos, i, target);
        if (scratch == target) continue;

        writeInsn(code, pos, makeMovInsn(scratch, load_src));
        const patched_call = (insn & ~@as(u32, 0x3E0)) | (@as(u32, scratch) << 5);
        writeInsn(code, i, patched_call);
    }
}

fn fixCallArgMoves(code: []u8) bool {
    if (code.len < 8) return true;
    const n_insns = code.len / 4;
    const NOP: u32 = 0xD503201F;

    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);

        // Check for BLR (indirect) or BL (direct) call instruction
        const is_blr = (insn & 0xFFFFFC1F == 0xD63F0000);
        const is_bl = (insn & 0xFC000000 == 0x94000000);
        if (!is_blr and !is_bl) continue;

        const call_target: ?u5 = if (is_blr) @truncate((insn >> 5) & 0x1F) else null;

        // Found a call. Scan backwards for argument move setup (up to 24 instructions).
        // Allow interleaved call-target setup (mov/movz/movk for BLR target register).
        const MovInfo = struct { src: u5, dst: u5, pos: usize };
        var movs: [8]MovInfo = undefined;
        var n_movs: usize = 0;

        var j = i;
        var scan_steps: usize = 0;
        while (j > 0 and n_movs < 8 and scan_steps < 24) {
            j -= 1;
            scan_steps += 1;
            const prev = readInsn(code, j);
            if (prev == NOP) continue;

            // Check for MOV Xd, Xm (ORR Xd, XZR, Xm): 0xAA0003E0 mask 0xFFE0FFE0
            if (prev & 0xFFE0FFE0 == 0xAA0003E0) {
                const rd: u5 = @truncate(prev & 0x1F);
                const rm: u5 = @truncate((prev >> 16) & 0x1F);
                // Only include moves to x0-x7 (ABI argument registers)
                // in the parallel copy resolution.
                if (rd <= 7) {
                    movs[n_movs] = .{ .src = rm, .dst = rd, .pos = j };
                    n_movs += 1;
                    continue;
                }

                // Interleaved target setup before BLR call.
                if (call_target) |target| {
                    if (rd == target) continue;
                }

                break;
            }

            // movz/movk are commonly used for BLR target materialization.
            if ((prev & 0xFF800000 == 0xD2800000) or (prev & 0xFF800000 == 0xF2800000)) {
                const rd: u5 = @truncate(prev & 0x1F);
                if (call_target) |target| {
                    if (rd == target) continue;
                }
                break;
            }

            // Reached non-setup instruction.
            break;
        }

        if (n_movs < 2) continue;

        // Sort by instruction position so symbolic execution follows original order.
        var sidx: usize = 1;
        while (sidx < n_movs) : (sidx += 1) {
            var k = sidx;
            while (k > 0 and movs[k - 1].pos > movs[k].pos) : (k -= 1) {
                const tmp = movs[k];
                movs[k] = movs[k - 1];
                movs[k - 1] = tmp;
            }
        }

        // Build the intended parallel-copy mapping from the original mov window.
        // Hoist lowers call-arg parallel copies into a linear mov chain, so
        // each mov's source must be interpreted as the pre-window register state
        // (not the register state after earlier movs in the window).
        // If a destination appears multiple times, the last assignment wins.
        var last_src: [8]u5 = undefined;
        var dst_assigned = [_]bool{false} ** 8;
        for (0..n_movs) |mi| {
            const dst = movs[mi].dst;
            last_src[dst] = movs[mi].src;
            dst_assigned[dst] = true;
        }

        const Assign = struct { dst: u5, src: u5 };
        var assigns: [8]Assign = undefined;
        var n_assigns: usize = 0;
        for (0..8) |dst_idx| {
            if (!dst_assigned[dst_idx]) continue;
            const dst: u5 = @intCast(dst_idx);
            const src = last_src[dst];
            if (src == dst) continue;
            assigns[n_assigns] = .{ .dst = dst, .src = src };
            n_assigns += 1;
        }
        if (n_assigns < 2) continue;

        const regUsed = struct {
            fn f(assigns_slice: []const Assign, reg: u5) bool {
                for (assigns_slice) |a| {
                    if (a.src == reg or a.dst == reg) return true;
                }
                return false;
            }
        }.f;

        var scratch: u5 = 9;
        while (scratch < 28 and regUsed(assigns[0..n_assigns], scratch)) : (scratch += 1) {}
        if (scratch >= 28) scratch = 9;

        // Resolve parallel copy with cycle handling via scratch register.
        var emitted: [8]bool = .{ false, false, false, false, false, false, false, false };
        var emitted_count: usize = 0;
        var result: [9]u32 = undefined; // up to n_movs + 1 for one broken cycle
        var n_result: usize = 0;

        while (emitted_count < n_assigns) {
            var progressed = false;

            for (0..n_assigns) |ai| {
                if (emitted[ai]) continue;
                const dst = assigns[ai].dst;
                var dst_needed = false;
                for (0..n_assigns) |other| {
                    if (ai == other or emitted[other]) continue;
                    if (assigns[other].src == dst) {
                        dst_needed = true;
                        break;
                    }
                }
                if (!dst_needed) {
                    result[n_result] = makeMovInsn(dst, assigns[ai].src);
                    n_result += 1;
                    emitted[ai] = true;
                    emitted_count += 1;
                    progressed = true;
                }
            }

            if (progressed) continue;

            // Cycle: preserve one source in scratch, then continue.
            var cycle_idx: ?usize = null;
            for (0..n_assigns) |ai| {
                if (!emitted[ai]) {
                    cycle_idx = ai;
                    break;
                }
            }
            const ci = cycle_idx orelse break;
            if (n_result >= result.len) return false;
            result[n_result] = makeMovInsn(scratch, assigns[ci].src);
            n_result += 1;
            assigns[ci].src = scratch;
        }

        // Existing arg-move slots are written in original execution order.
        if (n_result <= n_movs) {
            for (0..n_movs) |k| {
                const pos = movs[k].pos;
                if (k < n_result) {
                    writeInsn(code, pos, result[k]);
                } else {
                    writeInsn(code, pos, 0xD503201F); // NOP
                }
            }
            continue;
        }

        // Need one extra slot for cycle break: reuse `mov x9, xT; blr x9`.
        if (n_result == n_movs + 1 and is_blr) {
            const call_target_reg = call_target orelse return false;
            const earliest_pos = movs[0].pos;
            if (call_target_reg == 9 and earliest_pos > 0) {
                const slot_pos = earliest_pos - 1;
                const slot_insn = readInsn(code, slot_pos);
                if (slot_insn & 0xFFE0FFE0 == 0xAA0003E0) {
                    const slot_dst: u5 = @truncate(slot_insn & 0x1F);
                    const slot_src: u5 = @truncate((slot_insn >> 16) & 0x1F);
                    if (slot_dst == 9) {
                        const patched_call = (insn & ~@as(u32, 0x3E0)) | (@as(u32, slot_src) << 5);
                        writeInsn(code, i, patched_call);
                        writeInsn(code, slot_pos, result[0]);
                        for (0..n_movs) |k| {
                            const pos = movs[k].pos;
                            writeInsn(code, pos, result[k + 1]);
                        }
                        continue;
                    }
                }
            }
        }

        return false;
    }

    return true;
}

fn readInsn(code: []const u8, idx: usize) u32 {
    const off = idx * 4;
    return std.mem.readInt(u32, code[off..][0..4], .little);
}

fn writeInsn(code: []u8, idx: usize, val: u32) void {
    const off = idx * 4;
    std.mem.writeInt(u32, code[off..][0..4], val, .little);
}

fn dumpAsmPass(label: []const u8, code: []const u8) void {
    std.debug.print("[hoist-pass] {s} bytes={d}\n", .{ label, code.len });
    var i: usize = 0;
    while (i + 4 <= code.len) : (i += 4) {
        const w = std.mem.readInt(u32, code[i..][0..4], .little);
        std.debug.print("  {x:0>4}: {x:0>8}\n", .{ i, w });
    }
}

// ============================================================================
// Tests
// ============================================================================

const testing = std.testing;

fn writeWords(words: []const u32, code: []u8) void {
    for (words, 0..) |word, idx| {
        const off = idx * 4;
        std.mem.writeInt(u32, code[off..][0..4], word, .little);
    }
}

fn simulateMovesUntilCall(code: []const u8, regs: *[32]u64) void {
    const n_insns = code.len / 4;
    var i: usize = 0;
    while (i < n_insns) : (i += 1) {
        const insn = readInsn(code, i);
        const is_blr = (insn & 0xFFFFFC1F == 0xD63F0000);
        const is_bl = (insn & 0xFC000000 == 0x94000000);
        if (is_blr or is_bl) break;

        if (insn & 0xFFE0FFE0 == 0xAA0003E0) {
            const rd: u5 = @truncate(insn & 0x1F);
            const rm: u5 = @truncate((insn >> 16) & 0x1F);
            regs[rd] = regs[rm];
            continue;
        }

        if (insn & 0xFF800000 == 0xD2800000) { // MOVZ
            const rd: u5 = @truncate(insn & 0x1F);
            const imm16: u64 = @as(u64, (insn >> 5) & 0xFFFF);
            const hw: u6 = @truncate((insn >> 21) & 0x3);
            regs[rd] = imm16 << (@as(u6, hw) * 16);
            continue;
        }

        if (insn & 0xFF800000 == 0xF2800000) { // MOVK
            const rd: u5 = @truncate(insn & 0x1F);
            const imm16: u64 = @as(u64, (insn >> 5) & 0xFFFF);
            const hw: u6 = @truncate((insn >> 21) & 0x3);
            const shift = @as(u6, hw) * 16;
            const mask: u64 = ~(@as(u64, 0xFFFF) << shift);
            regs[rd] = (regs[rd] & mask) | (imm16 << shift);
        }
    }
}

test "fixCallArgMoves handles target mov between arg setup and blr" {
    const words = [_]u32{
        makeMovInsn(0, 23),
        makeMovInsn(9, 22),
        makeMovInsn(1, 0),
        0xD63F0120, // BLR x9
    };
    var code: [words.len * 4]u8 = undefined;
    writeWords(&words, &code);

    try testing.expect(fixCallArgMoves(&code));

    var regs: [32]u64 = undefined;
    for (0..regs.len) |idx| regs[idx] = @as(u64, 1000 + idx);
    const old0 = regs[0];
    const old23 = regs[23];
    simulateMovesUntilCall(&code, &regs);

    try testing.expectEqual(old23, regs[0]);
    try testing.expectEqual(old0, regs[1]);
}

test "fixCallArgMoves scans through movz target setup" {
    const words = [_]u32{
        makeMovInsn(0, 23),
        0xD2800029, // MOVZ x9, #1
        makeMovInsn(1, 0),
        0xD63F0120, // BLR x9
    };
    var code: [words.len * 4]u8 = undefined;
    writeWords(&words, &code);

    try testing.expect(fixCallArgMoves(&code));

    var regs: [32]u64 = undefined;
    for (0..regs.len) |idx| regs[idx] = @as(u64, 2000 + idx);
    const old0 = regs[0];
    const old23 = regs[23];
    simulateMovesUntilCall(&code, &regs);

    try testing.expectEqual(old23, regs[0]);
    try testing.expectEqual(old0, regs[1]);
}

test "fixCallArgMoves uses target slot for 2-cycle" {
    const words = [_]u32{
        makeMovInsn(9, 22), // available slot before arg copies
        makeMovInsn(0, 1),
        makeMovInsn(1, 0),
        0xD63F0120, // BLR x9
    };
    var code: [words.len * 4]u8 = undefined;
    writeWords(&words, &code);

    try testing.expect(fixCallArgMoves(&code));

    const patched_call = readInsn(&code, 3);
    const call_target: u5 = @truncate((patched_call >> 5) & 0x1F);
    try testing.expectEqual(@as(u5, 22), call_target);

    var regs: [32]u64 = undefined;
    for (0..regs.len) |idx| regs[idx] = @as(u64, 3000 + idx);
    const old0 = regs[0];
    const old1 = regs[1];
    simulateMovesUntilCall(&code, &regs);

    try testing.expectEqual(old1, regs[0]);
    try testing.expectEqual(old0, regs[1]);
}

test "fixCallArgMoves preserves 3-cycle call-arg mapping" {
    const words = [_]u32{
        makeMovInsn(9, 22), // call target setup
        makeMovInsn(0, 1),
        makeMovInsn(1, 2),
        makeMovInsn(2, 0),
        0xD63F0120, // BLR x9
    };
    var code: [words.len * 4]u8 = undefined;
    writeWords(&words, &code);

    try testing.expect(fixCallArgMoves(&code));

    var regs: [32]u64 = undefined;
    for (0..regs.len) |idx| regs[idx] = @as(u64, 4000 + idx);
    const old0 = regs[0];
    const old1 = regs[1];
    const old2 = regs[2];
    simulateMovesUntilCall(&code, &regs);

    try testing.expectEqual(old1, regs[0]);
    try testing.expectEqual(old2, regs[1]);
    try testing.expectEqual(old0, regs[2]);
}

test "fixCallArgMoves stops at non-target movz setup" {
    const words = [_]u32{
        makeMovInsn(0, 1), // pre-window setup that must stay out of arg-copy solve
        0xD2800074, // MOVZ x20, #3
        makeMovInsn(9, 22),
        makeMovInsn(0, 19),
        makeMovInsn(1, 20),
        makeMovInsn(2, 0),
        0xD63F0120, // BLR x9
    };
    var code: [words.len * 4]u8 = undefined;
    writeWords(&words, &code);

    try testing.expect(fixCallArgMoves(&code));

    var regs: [32]u64 = undefined;
    for (0..regs.len) |idx| regs[idx] = @as(u64, 5000 + idx);
    const old1 = regs[1];
    const old19 = regs[19];
    simulateMovesUntilCall(&code, &regs);

    try testing.expectEqual(old19, regs[0]);
    try testing.expectEqual(@as(u64, 3), regs[1]);
    try testing.expectEqual(old1, regs[2]);
}

test "containsHelperCalls excludes numeric ops under fixnum inline lowering" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const var_x = try alloc.create(Ir);
    var_x.* = .{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };
    const one = try alloc.create(Ir);
    one.* = .{ .lit = Value.makeFixnum(1) };
    const add = try alloc.create(Ir);
    add.* = .{ .add = .{ .left = var_x, .right = one } };

    try testing.expect(containsHelperCalls(add, false));
    try testing.expect(!containsHelperCalls(add, true));
}

test "containsHelperCalls preserves true helper ops under fixnum inline lowering" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const one = try alloc.create(Ir);
    one.* = .{ .lit = Value.makeFixnum(1) };
    const sqrt = try alloc.create(Ir);
    sqrt.* = .{ .sqrt = .{ .operand = one } };

    try testing.expect(containsHelperCalls(sqrt, false));
    try testing.expect(containsHelperCalls(sqrt, true));
}

test "hoist IR translator canTranslate new data ops" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const var_a = try alloc.create(Ir);
    var_a.* = .{ .@"var" = .{ .name = "a", .depth = 0, .index = 0 } };
    const var_b = try alloc.create(Ir);
    var_b.* = .{ .@"var" = .{ .name = "b", .depth = 0, .index = 1 } };
    const var_c = try alloc.create(Ir);
    var_c.* = .{ .@"var" = .{ .name = "c", .depth = 0, .index = 2 } };
    const one = try alloc.create(Ir);
    one.* = .{ .lit = Value.makeFixnum(1) };
    const two = try alloc.create(Ir);
    two.* = .{ .lit = Value.makeFixnum(2) };

    const str_concat = try alloc.create(Ir);
    str_concat.* = .{ .str_concat = .{ .left = var_a, .right = var_b } };
    try testing.expect(IrTranslator.canTranslate(str_concat));

    const substring = try alloc.create(Ir);
    substring.* = .{ .substring = .{ .str = var_a, .start = one, .end = two } };
    try testing.expect(IrTranslator.canTranslate(substring));

    const arr_new_dyn = try alloc.create(Ir);
    arr_new_dyn.* = .{ .arr_new_dyn = .{ .dimensions = var_a, .init = var_b } };
    try testing.expect(IrTranslator.canTranslate(arr_new_dyn));

    const subs = try alloc.alloc(*const Ir, 2);
    subs[0] = one;
    subs[1] = two;
    const arr_set = try alloc.create(Ir);
    arr_set.* = .{ .arr_set = .{ .array = var_a, .subscripts = subs, .value = var_c } };
    try testing.expect(IrTranslator.canTranslate(arr_set));

    const hash_keys = try alloc.create(Ir);
    hash_keys.* = .{ .hash_keys = .{ .operand = var_a } };
    try testing.expect(IrTranslator.canTranslate(hash_keys));

    const hash_alist = try alloc.create(Ir);
    hash_alist.* = .{ .hash_alist = .{ .operand = var_a } };
    try testing.expect(IrTranslator.canTranslate(hash_alist));
}

test "hoist IR translator: vec_ref vec_set vec_len helpers" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 2);
    params[0] = "v";
    params[1] = "x";

    const var_v_set = try alloc.create(Ir);
    var_v_set.* = .{ .@"var" = .{ .name = "v", .depth = 0, .index = 0 } };
    const var_v_len = try alloc.create(Ir);
    var_v_len.* = .{ .@"var" = .{ .name = "v", .depth = 0, .index = 0 } };
    const var_v_ref = try alloc.create(Ir);
    var_v_ref.* = .{ .@"var" = .{ .name = "v", .depth = 0, .index = 0 } };
    const var_x = try alloc.create(Ir);
    var_x.* = .{ .@"var" = .{ .name = "x", .depth = 0, .index = 1 } };
    const idx = try alloc.create(Ir);
    idx.* = .{ .lit = Value.makeFixnum(1) };

    const vec_set = try alloc.create(Ir);
    vec_set.* = .{ .vec_set = .{ .vec = var_v_set, .index = idx, .value = var_x } };

    const vec_len = try alloc.create(Ir);
    vec_len.* = .{ .vec_len = .{ .operand = var_v_len } };

    const vec_ref = try alloc.create(Ir);
    vec_ref.* = .{ .vec_ref = .{ .left = var_v_ref, .right = idx } };

    const exprs = try alloc.alloc(*const Ir, 3);
    exprs[0] = vec_set;
    exprs[1] = vec_len;
    exprs[2] = vec_ref;
    const body = try alloc.create(Ir);
    body.* = .{ .progn = exprs };

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

    var compiled = try compileIr(testing.allocator, lambda, "jit_vec_ops");
    defer compiled.deinit();

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    const vec = try heap.allocVector(3, 3);
    const vec_obj = vec.toPtr(runtime.Vector);
    vec_obj.set(0, Value.makeFixnum(10));
    vec_obj.set(1, Value.makeFixnum(11));
    vec_obj.set(2, Value.makeFixnum(12));

    const new_val = Value.makeFixnum(42);
    const result = compiled.call2(@as(i64, @bitCast(vec.raw)), @as(i64, @bitCast(new_val.raw)));
    try testing.expectEqual(@as(i64, @bitCast(new_val.raw)), result);
    try testing.expectEqual(new_val.raw, vec_obj.get(1).raw);
}

test "hoist IR translator: multidim arr_set and arr_ref helpers" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 2);
    params[0] = "a";
    params[1] = "v";

    const var_a_set = try alloc.create(Ir);
    var_a_set.* = .{ .@"var" = .{ .name = "a", .depth = 0, .index = 0 } };
    const var_a_ref = try alloc.create(Ir);
    var_a_ref.* = .{ .@"var" = .{ .name = "a", .depth = 0, .index = 0 } };
    const var_v = try alloc.create(Ir);
    var_v.* = .{ .@"var" = .{ .name = "v", .depth = 0, .index = 1 } };

    const s1 = try alloc.create(Ir);
    s1.* = .{ .lit = Value.makeFixnum(1) };
    const s2 = try alloc.create(Ir);
    s2.* = .{ .lit = Value.makeFixnum(2) };
    const subs = try alloc.alloc(*const Ir, 2);
    subs[0] = s1;
    subs[1] = s2;

    const arr_set = try alloc.create(Ir);
    arr_set.* = .{ .arr_set = .{ .array = var_a_set, .subscripts = subs, .value = var_v } };

    const arr_ref = try alloc.create(Ir);
    arr_ref.* = .{ .arr_ref = .{ .array = var_a_ref, .subscripts = subs } };

    const exprs = try alloc.alloc(*const Ir, 2);
    exprs[0] = arr_set;
    exprs[1] = arr_ref;
    const body = try alloc.create(Ir);
    body.* = .{ .progn = exprs };

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

    var compiled = try compileIr(testing.allocator, lambda, "jit_arr_ops");
    defer compiled.deinit();

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    const dims = [_]u64{ 2, 3 };
    const arr = try heap.allocArray(&dims);
    const arr_obj = arr.toPtr(runtime.Array);
    const data: [*]Value = @ptrFromInt(arr_obj.data_ptr);

    const new_val = Value.makeFixnum(77);
    const result = compiled.call2(@as(i64, @bitCast(arr.raw)), @as(i64, @bitCast(new_val.raw)));
    try testing.expectEqual(@as(i64, @bitCast(new_val.raw)), result);
    try testing.expectEqual(new_val.raw, data[5].raw);
}

test "hoist IR translator: hash_rem and hash_capacity helpers" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 2);
    params[0] = "h";
    params[1] = "k";

    const var_h_rem = try alloc.create(Ir);
    var_h_rem.* = .{ .@"var" = .{ .name = "h", .depth = 0, .index = 0 } };
    const var_h_cap = try alloc.create(Ir);
    var_h_cap.* = .{ .@"var" = .{ .name = "h", .depth = 0, .index = 0 } };
    const var_k = try alloc.create(Ir);
    var_k.* = .{ .@"var" = .{ .name = "k", .depth = 0, .index = 1 } };

    const hash_rem = try alloc.create(Ir);
    hash_rem.* = .{ .hash_rem = .{ .table = var_h_rem, .key = var_k } };
    const hash_capacity = try alloc.create(Ir);
    hash_capacity.* = .{ .hash_capacity = .{ .operand = var_h_cap } };

    const exprs = try alloc.alloc(*const Ir, 2);
    exprs[0] = hash_rem;
    exprs[1] = hash_capacity;
    const body = try alloc.create(Ir);
    body.* = .{ .progn = exprs };

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

    var compiled = try compileIr(testing.allocator, lambda, "jit_hash_ops");
    defer compiled.deinit();

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    const ht = try heap.allocHashTable(8, .equal);
    const table = ht.toPtr(runtime.HashTable);
    const key = Value.makeFixnum(5);
    try table.put(key, Value.makeFixnum(99));

    const result = compiled.call2(@as(i64, @bitCast(ht.raw)), @as(i64, @bitCast(key.raw)));
    try testing.expectEqual(@as(usize, 0), table.count);
    try testing.expectEqual(@as(i64, @bitCast(Value.makeFixnum(@intCast(table.capacity)).raw)), result);
}

test "hoist IR translator: global_ref generic call requires literal root" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const fn_ref = try alloc.create(Ir);
    fn_ref.* = .{ .global_ref = .{ .name = "UNRESOLVED-CALLEE", .index = 0 } };
    const call_args = try alloc.alloc(*const Ir, 0);
    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = fn_ref, .args = call_args } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = &.{},
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = call_node,
        .speed = 3,
        .safety = 0,
    } };

    try testing.expectError(
        error.UnsupportedCallTarget,
        compileIr(testing.allocator, lambda, "global-ref-generic-call-no-root"),
    );
}

test "hoist IR translator: global_ref generic call loads rooted designator" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const fn_ref = try alloc.create(Ir);
    fn_ref.* = .{ .global_ref = .{ .name = "ROOTED-CALLEE", .index = 0 } };
    const call_args = try alloc.alloc(*const Ir, 0);
    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = fn_ref, .args = call_args } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = &.{},
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = call_node,
        .speed = 3,
        .safety = 0,
    } };

    const slot = try testing.allocator.create(Value);
    defer testing.allocator.destroy(slot);
    slot.* = Value.t;

    var roots = LiteralRoots.init(testing.allocator);
    defer roots.deinit();
    try roots.put(@intFromPtr(fn_ref), slot);

    const EchoBridge = struct {
        fn call0(_: *anyopaque, fn_raw: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call1(_: *anyopaque, fn_raw: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call2(_: *anyopaque, fn_raw: u64, _: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call3(_: *anyopaque, fn_raw: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call4(_: *anyopaque, fn_raw: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call5(_: *anyopaque, fn_raw: u64, _: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call6(_: *anyopaque, fn_raw: u64, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
        fn call7(_: *anyopaque, fn_raw: u64, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return fn_raw;
        }
    };

    var ctx_byte: u8 = 0;
    setCallBridge(.{
        .context = @ptrCast(&ctx_byte),
        .call0 = EchoBridge.call0,
        .call1 = EchoBridge.call1,
        .call2 = EchoBridge.call2,
        .call3 = EchoBridge.call3,
        .call4 = EchoBridge.call4,
        .call5 = EchoBridge.call5,
        .call6 = EchoBridge.call6,
        .call7 = EchoBridge.call7,
    });

    var compiled = try compileIrWithKnownFnsAndLiteralRoots(
        testing.allocator,
        lambda,
        "global-ref-generic-call-rooted",
        null,
        &roots,
    );
    defer compiled.deinit();

    const result = compiled.call0();
    try testing.expectEqual(@as(i64, @bitCast(Value.t.raw)), result);
}

test "hoist IR translator: generic call supports seven args" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const fn_ref = try alloc.create(Ir);
    fn_ref.* = .{ .global_ref = .{ .name = "ROOTED-CALLEE-7", .index = 0 } };

    const call_args = try alloc.alloc(*const Ir, 7);
    for (0..7) |i| {
        const lit = try alloc.create(Ir);
        lit.* = .{ .lit = Value.makeFixnum(@intCast(i + 1)) };
        call_args[i] = lit;
    }

    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = fn_ref, .args = call_args } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = &.{},
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = call_node,
        .speed = 3,
        .safety = 0,
    } };

    const slot = try testing.allocator.create(Value);
    defer testing.allocator.destroy(slot);
    slot.* = Value.t;

    var roots = LiteralRoots.init(testing.allocator);
    defer roots.deinit();
    try roots.put(@intFromPtr(fn_ref), slot);

    const EchoBridge = struct {
        fn call0(_: *anyopaque, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call1(_: *anyopaque, _: u64, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call2(_: *anyopaque, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call3(_: *anyopaque, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call4(_: *anyopaque, _: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call5(_: *anyopaque, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call6(_: *anyopaque, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64) callconv(.c) u64 {
            return Value.nil.raw;
        }
        fn call7(_: *anyopaque, fn_raw: u64, _: u64, _: u64, _: u64, _: u64, _: u64, _: u64, arg6: u64) callconv(.c) u64 {
            if (fn_raw == 0) return Value.nil.raw;
            return arg6;
        }
    };

    var ctx_byte: u8 = 0;
    setCallBridge(.{
        .context = @ptrCast(&ctx_byte),
        .call0 = EchoBridge.call0,
        .call1 = EchoBridge.call1,
        .call2 = EchoBridge.call2,
        .call3 = EchoBridge.call3,
        .call4 = EchoBridge.call4,
        .call5 = EchoBridge.call5,
        .call6 = EchoBridge.call6,
        .call7 = EchoBridge.call7,
    });

    var compiled = try compileIrWithKnownFnsAndLiteralRoots(
        testing.allocator,
        lambda,
        "global-ref-generic-call-seven-args",
        null,
        &roots,
    );
    defer compiled.deinit();

    const result = compiled.call0();
    try testing.expectEqual(@as(i64, @bitCast(Value.makeFixnum(7).raw)), result);
}

/// Helper: build Hoist function, compile, load into JIT memory
fn compileAndLoad(allocator: std.mem.Allocator, func: *Function) !struct { fn_ptr: *const fn (i64) callconv(.c) i64, mem: *JitMem } {
    var ctx_builder = ContextBuilder.init(allocator);
    _ = try ctx_builder.targetNative();
    var ctx = ctx_builder.optLevel(.aggressive).callConv(.system_v).verification(true).build();
    defer ctx.deinit();

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

test "fixEntryParamMovesAlloc preserves chained entry copies" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    // Problematic chain observed in NQUEENS-SAFE-P entry shuffle.
    try appendInsn(&code, testing.allocator, makeMovInsn(3, 0)); // x3 <- x0
    try appendInsn(&code, testing.allocator, makeMovInsn(5, 1)); // x5 <- x1
    try appendInsn(&code, testing.allocator, makeMovInsn(6, 2)); // x6 <- x2
    try appendInsn(&code, testing.allocator, makeMovInsn(0, 3)); // x0 <- x3
    try appendInsn(&code, testing.allocator, makeMovInsn(2, 5)); // x2 <- x5
    try appendInsn(&code, testing.allocator, makeMovInsn(4, 6)); // x4 <- x6
    try appendInsn(&code, testing.allocator, 0xF100005F); // cmp x2, #0

    var expected_regs: [32]u64 = undefined;
    for (0..expected_regs.len) |r| expected_regs[r] = r;
    const orig_moves = [_]struct { dst: u5, src: u5 }{
        .{ .dst = 3, .src = 0 },
        .{ .dst = 5, .src = 1 },
        .{ .dst = 6, .src = 2 },
        .{ .dst = 0, .src = 3 },
        .{ .dst = 2, .src = 5 },
        .{ .dst = 4, .src = 6 },
    };
    for (orig_moves) |m| {
        expected_regs[m.dst] = expected_regs[m.src];
    }

    try fixEntryParamMovesAlloc(testing.allocator, &code);

    var actual_regs: [32]u64 = undefined;
    for (0..actual_regs.len) |r| actual_regs[r] = r;
    const n_insns = code.items.len / 4;
    var idx: usize = 0;
    while (idx < n_insns) : (idx += 1) {
        const insn = readInsn(code.items, idx);
        if (insn & 0xFFE0FFE0 != 0xAA0003E0) break;
        const dst: u5 = @truncate(insn & 0x1F);
        const src: u5 = @truncate((insn >> 16) & 0x1F);
        actual_regs[dst] = actual_regs[src];
    }

    const checked_regs = [_]u5{ 0, 2, 3, 4, 5, 6 };
    for (checked_regs) |r| {
        try testing.expectEqual(expected_regs[r], actual_regs[r]);
    }
}

test "fuseMovzAlu keeps movz source register live across branch" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, 0xD2800074); // movz x20, #3
    try appendInsn(&code, testing.allocator, makeMovInsn(23, 20)); // mov x23, x20
    try appendInsn(&code, testing.allocator, 0x14000002); // b +2 (to idx 4)
    try appendInsn(&code, testing.allocator, 0xD503201F); // nop
    try appendInsn(&code, testing.allocator, makeMovInsn(2, 20)); // mov x2, x20 (later use)
    try appendInsn(&code, testing.allocator, 0xD65F03C0); // ret

    fuseMovzAlu(code.items);

    const insn0 = readInsn(code.items, 0);
    const insn1 = readInsn(code.items, 1);
    try testing.expectEqual(@as(u32, 0xD2800074), insn0);
    try testing.expectEqual(makeMovInsn(23, 20), insn1);
}

test "isRegDeadAfter keeps branch-reachable movz live" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    const insns = [_]u32{
        0xA9BF7BFD, // stp x29, x30, [sp, #-16]!
        0xAA1F03FD, // mov x29, sp
        0xAA0003E3, // mov x3, x0
        0xAA0103E5, // mov x5, x1
        0xAA0203E6, // mov x6, x2
        0xAA0303E0, // mov x0, x3
        0xAA0503E2, // mov x2, x5
        0xAA0603E4, // mov x4, x6
        0x14000001, // b +1
        0xD2800003, // movz x3, #0
        0xEB00005F, // cmp x2, xzr
        0x1A9F17E5, // cset x5, eq
        0x540000A0, // b.eq +5
        0x14000007, // b +7
        0xAA0503E0, // mov x0, x5
        0xA8C17BFD, // ldp x29, x30, [sp], #16
        0xD65F03C0, // ret
        0xD2800041, // movz x1, #2
        0xAA0103E5, // mov x5, x1
        0x17FFFFFB, // b -5
        0xF8400046, // ldr x6, [x2]
        0xEB0000DF, // cmp x6, x0
        0x1A9F17E7, // cset x7, eq
        0x54000040, // b.eq +2
        0x14000003, // b +3
        0xAA0303E5, // mov x5, x3  <-- x3 use
        0x17FFFFF4, // b -12
    };
    for (insns) |insn| try appendInsn(&code, testing.allocator, insn);

    // movz at idx 9 must stay live *after* the cmp at idx 10 because x3 is
    // still used at idx 25 on a branch-reachable path.
    try testing.expect(!isRegDeadAfter(code.items, 10, 3));
}

test "isRegDeadAfter keeps movz live in cset-elided graph" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    const insns = [_]u32{
        0xA9BF7BFD, 0xAA1F03FD, 0xAA0003E3, 0xAA0103E5,
        0xAA0203E6, 0xAA0303E0, 0xAA0503E2, 0xAA0603E4,
        0x14000001, 0xD2800003, 0xF100005F, 0xD503201F,
        0x540000A0, 0x14000007, 0xAA0503E0, 0xA8C17BFD,
        0xD65F03C0, 0xD2800041, 0xAA0103E5, 0x17FFFFFB,
        0xF8400046, 0xEB0000DF, 0xD503201F, 0x54000040,
        0x14000003, 0xAA0303E5, 0x17FFFFF4, 0xCB0000C7,
        0xD2800028, 0x8B0800EB, 0xF100057F, 0x1A9FB7E7,
        0xCB0B0028, 0x6B1F00FF, 0x9A881167, 0xEB0400FF,
        0xD503201F, 0x54000040, 0x14000003, 0xAA0303E5,
        0x17FFFFE6, 0xD2800103, 0x8B030045, 0xF84000A3,
        0x8B010085, 0xAA0303E2, 0xAA0503E4, 0x17FFFFDA,
    };
    for (insns) |insn| try appendInsn(&code, testing.allocator, insn);

    try testing.expect(!isRegDeadAfter(code.items, 10, 3));
}

test "isRegDeadAfter treats return register as live at ret" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, 0xD2800AA0); // movz x0, #85
    try appendInsn(&code, testing.allocator, 0xD65F03C0); // ret

    try testing.expect(!isRegDeadAfter(code.items, 0, 0));
}

test "eliminateDeadMovz keeps movz live for return register" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, 0xD2800AA0); // movz x0, #85
    try appendInsn(&code, testing.allocator, 0xD65F03C0); // ret

    eliminateDeadMovz(code.items);

    try testing.expectEqual(@as(u32, 0xD2800AA0), readInsn(code.items, 0));
}

test "eliminateDeadMovz keeps movz live for store data operands" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, 0xD28000A1); // movz x1, #5
    try appendInsn(&code, testing.allocator, 0xF80000C1); // str x1, [x6]
    try appendInsn(&code, testing.allocator, 0xD28000E1); // movz x1, #7 (dead)
    try appendInsn(&code, testing.allocator, 0xD65F03C0); // ret

    eliminateDeadMovz(code.items);

    try testing.expectEqual(@as(u32, 0xD28000A1), readInsn(code.items, 0));
    try testing.expectEqual(@as(u32, 0xD503201F), readInsn(code.items, 2));
}

test "eliminateDeadMovz keeps movz live for unscaled load base" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, 0xD2800063); // movz x3, #3
    try appendInsn(&code, testing.allocator, 0xF8400066); // ldr x6, [x3]
    try appendInsn(&code, testing.allocator, 0xD2800003); // movz x3, #0 (dead)
    try appendInsn(&code, testing.allocator, 0xD65F03C0); // ret

    eliminateDeadMovz(code.items);

    try testing.expectEqual(@as(u32, 0xD2800063), readInsn(code.items, 0));
    try testing.expectEqual(@as(u32, 0xD503201F), readInsn(code.items, 2));
}

test "coalesceMovs keeps source live across loop backedge" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, makeMovInsn(0, 5)); // reads x5 on loop header
    try appendInsn(&code, testing.allocator, 0x8B010085); // add x5, x4, x1
    try appendInsn(&code, testing.allocator, makeMovInsn(4, 5)); // mov x4, x5
    try appendInsn(&code, testing.allocator, 0x17FFFFFD); // b to idx 0

    coalesceMovs(code.items);

    try testing.expectEqual(@as(u32, 0x8B010085), readInsn(code.items, 1));
    try testing.expectEqual(makeMovInsn(4, 5), readInsn(code.items, 2));
}

test "coalesceMovs folds dead copy in straight line" {
    var code = std.ArrayList(u8){};
    defer code.deinit(testing.allocator);

    const appendInsn = struct {
        fn f(list: *std.ArrayList(u8), allocator: std.mem.Allocator, insn: u32) !void {
            const bytes: [4]u8 = @bitCast(insn);
            try list.appendSlice(allocator, &bytes);
        }
    }.f;

    try appendInsn(&code, testing.allocator, 0x8B010085); // add x5, x4, x1
    try appendInsn(&code, testing.allocator, makeMovInsn(4, 5)); // mov x4, x5
    try appendInsn(&code, testing.allocator, 0xD65F03C0); // ret

    coalesceMovs(code.items);

    try testing.expectEqual(@as(u32, 0x8B010084), readInsn(code.items, 0));
    try testing.expectEqual(@as(u32, 0xD503201F), readInsn(code.items, 1));
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

test "hoist IR translator: pointer literal requires root when provided" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var heap = try runtime.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    const sym = try heap.intern("jit-literal-symbol");

    const lit_sym = try alloc.create(Ir);
    lit_sym.* = .{ .lit = sym };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = &.{},
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = lit_sym,
        .speed = 3,
        .safety = 0,
    } };

    var roots = LiteralRoots.init(testing.allocator);
    defer roots.deinit();
    try testing.expect(!IrTranslator.canTranslateWithLiteralRoots(lambda.lambda.body, &roots));

    const slot = try testing.allocator.create(Value);
    defer testing.allocator.destroy(slot);
    slot.* = sym;
    try roots.put(@intFromPtr(lit_sym), slot);
    try testing.expect(IrTranslator.canTranslateWithLiteralRoots(lambda.lambda.body, &roots));

    var compiled = try compileIrWithKnownFnsAndLiteralRoots(testing.allocator, lambda, "jit_lit_sym", null, &roots);
    defer compiled.deinit();
    try testing.expectEqual(@as(i64, @bitCast(sym.raw)), compiled.call0());
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

test "hoist IR translator: primitive gcd call" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 0);

    const mkLit = struct {
        fn f(a: std.mem.Allocator, n: i64) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .lit = Value.makeFixnum(n) };
            return v;
        }
    }.f;

    const gcd_ref = try alloc.create(Ir);
    gcd_ref.* = .{ .global_ref = .{ .name = "GCD", .index = 0 } };
    const call_args = try alloc.alloc(*const Ir, 2);
    call_args[0] = try mkLit(alloc, 39);
    call_args[1] = try mkLit(alloc, 21);
    const call_node = try alloc.create(Ir);
    call_node.* = .{ .call = .{ .func = gcd_ref, .args = call_args } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = call_node,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "gcd_once");
    defer compiled.deinit();

    try testing.expectEqual(@as(i64, 7), compiled.call0());
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
    try testing.expectEqual(@as(i64, 85), compiled.call1(1)); // countdown(0) = 42
    try testing.expectEqual(@as(i64, 85), compiled.call1(3)); // countdown(1) = 42

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

test "hoist IR translator: callFromValues supports arity 4" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const params = try alloc.alloc([]const u8, 4);
    params[0] = "a";
    params[1] = "b";
    params[2] = "c";
    params[3] = "d";

    const mkVar = struct {
        fn f(a: std.mem.Allocator, name: []const u8, idx: u16) !*Ir {
            const v = try a.create(Ir);
            v.* = .{ .@"var" = .{ .name = name, .depth = 0, .index = idx } };
            return v;
        }
    }.f;

    const sum_ab = try alloc.create(Ir);
    sum_ab.* = .{ .fixnum_add = .{
        .left = try mkVar(alloc, "a", 0),
        .right = try mkVar(alloc, "b", 1),
    } };
    const sum_cd = try alloc.create(Ir);
    sum_cd.* = .{ .fixnum_add = .{
        .left = try mkVar(alloc, "c", 2),
        .right = try mkVar(alloc, "d", 3),
    } };
    const sum_all = try alloc.create(Ir);
    sum_all.* = .{ .fixnum_add = .{
        .left = sum_ab,
        .right = sum_cd,
    } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = params,
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = sum_all,
        .speed = 3,
        .safety = 0,
    } };

    var compiled = try compileIr(testing.allocator, lambda, "add4");
    defer compiled.deinit();

    const args = [_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
        Value.makeFixnum(4),
    };
    const result = compiled.callFromValues(&args);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10), result.toFixnum());

    const r4 = compiled.call4(
        @bitCast(Value.makeFixnum(1).raw),
        @bitCast(Value.makeFixnum(2).raw),
        @bitCast(Value.makeFixnum(3).raw),
        @bitCast(Value.makeFixnum(4).raw),
    );
    try testing.expectEqual(@as(u64, result.raw), @as(u64, @bitCast(r4)));
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

test "hoist IR translator: block wrapper compiles" {
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    const lit = try alloc.create(Ir);
    lit.* = .{ .lit = Value.makeFixnum(42) };

    const blk = try alloc.create(Ir);
    blk.* = .{ .block = .{
        .name = Value.nil,
        .body = lit,
    } };

    const lambda = try alloc.create(Ir);
    lambda.* = .{ .lambda = .{
        .params = &.{},
        .optional_params = &.{},
        .key_params = &.{},
        .rest_param = null,
        .captures = &.{},
        .body = blk,
        .speed = 3,
        .safety = 0,
    } };

    try testing.expect(IrTranslator.canTranslate(blk));
    try testing.expectEqual(@as(?std.meta.Tag(Ir), null), IrTranslator.firstUnsupportedTag(blk));

    var compiled = try compileIr(testing.allocator, lambda, "block-test");
    defer compiled.deinit();
    try testing.expectEqual(@as(i64, 85), compiled.call0());
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
    defer ctx.deinit();

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
