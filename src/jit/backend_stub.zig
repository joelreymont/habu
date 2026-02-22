const std = @import("std");
const ir_mod = @import("../compiler/ir.zig");
const runtime = @import("../runtime/runtime.zig");

const Ir = ir_mod.Ir;
const Value = runtime.Value;
const Heap = runtime.Heap;
pub const LiteralRoots = std.AutoHashMap(usize, *Value);

pub fn setHeap(_: *Heap) void {}

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

pub fn setCallBridge(_: CallBridge) void {}

pub fn syncHeapFromGlobal(_: *Heap) void {}

pub fn allocPtrRaw() u64 {
    return 0;
}

pub fn clearBridgeError() void {}

pub fn markBridgeError() void {}

pub fn bridgeErrorPending() bool {
    return false;
}

pub const KnownFn = struct {
    fn_ptr: u64 = 0,
    arity: u32 = 0,
    ir_body: ?*const Ir = null,
    param_names: ?[]const []const u8 = null,
    callee_name: []const u8 = "",
};

pub const StubMem = struct {
    ptr: [*]u8 = undefined,
    used: usize = 0,

    pub fn deinit(_: *StubMem) void {}
    pub fn setExec(_: *StubMem, _: bool) !void {}
    pub fn flushCacheRange(_: *StubMem, _: [*]u8, _: usize) void {}
};

pub const CompiledFn = struct {
    mem: *StubMem,
    fn_ptr: *const anyopaque,
    arity: u32,
    allocator: std.mem.Allocator,
    name: []const u8 = "",
    ir_arena: ?*std.heap.ArenaAllocator = null,
    ir_body: ?*const Ir = null,
    param_names: ?[]const []const u8 = null,
    cont_buf: ?[]align(8) u8 = null,

    pub fn deinit(_: *CompiledFn) void {}

    pub fn callFromValues(_: *const CompiledFn, _: []const Value) Value {
        return Value.nil;
    }
};

pub const IrTranslator = struct {
    pub fn canTranslate(_: *const Ir) bool {
        return false;
    }

    pub fn firstUnsupportedTag(_: *const Ir) ?std.meta.Tag(Ir) {
        return null;
    }
};

pub fn patchCrossCallsToBL(_: [*]u8, _: usize, _: usize) void {}

pub fn compileIr(
    _: std.mem.Allocator,
    _: *const Ir,
    _: []const u8,
) !CompiledFn {
    return error.JitDisabled;
}

pub fn compileIrWithKnownFns(
    _: std.mem.Allocator,
    _: *const Ir,
    _: []const u8,
    _: ?*const std.StringHashMap(KnownFn),
) !CompiledFn {
    return error.JitDisabled;
}

pub fn compileIrWithKnownFnsAndLiteralRoots(
    _: std.mem.Allocator,
    _: *const Ir,
    _: []const u8,
    _: ?*const std.StringHashMap(KnownFn),
    _: ?*const LiteralRoots,
) !CompiledFn {
    return error.JitDisabled;
}
