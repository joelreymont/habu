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
    push_progv: *const fn (*anyopaque, u64, u64) callconv(.c) u16,
    pop_progv: *const fn (*anyopaque) callconv(.c) u16,
};

var g_call_bridge: ?CallBridge = null;

pub fn setCallBridge(bridge: CallBridge) void {
    g_call_bridge = bridge;
}

pub fn clearCallBridge() void {
    g_call_bridge = null;
}

pub fn callBridgeContext() ?*anyopaque {
    if (g_call_bridge) |bridge| return bridge.context;
    return null;
}

pub const ErrorBridge = struct {
    context: *anyopaque,
    set_error: *const fn (*anyopaque, u16) callconv(.c) void,
};

var g_error_bridge: ?ErrorBridge = null;

pub fn setErrorBridge(bridge: ErrorBridge) void {
    g_error_bridge = bridge;
}

pub fn clearErrorBridge() void {
    g_error_bridge = null;
}

pub fn errorBridgeContext() ?*anyopaque {
    if (g_error_bridge) |bridge| return bridge.context;
    return null;
}

pub const GlobalBridge = struct {
    context: *anyopaque,
    load_global: *const fn (*anyopaque, u16) callconv(.c) u64,
};

var g_global_bridge: ?GlobalBridge = null;

pub fn setGlobalBridge(bridge: GlobalBridge) void {
    g_global_bridge = bridge;
}

pub fn clearGlobalBridge() void {
    g_global_bridge = null;
}

pub fn globalBridgeContext() ?*anyopaque {
    if (g_global_bridge) |bridge| return bridge.context;
    return null;
}

pub fn syncHeapFromGlobal(_: *Heap) void {}

pub fn allocPtrRaw() u64 {
    return 0;
}

pub const BridgeRunFn = *const fn (*anyopaque) callconv(.c) u64;

pub fn bridgeRun(func: BridgeRunFn, ctx: *anyopaque, out_raw: *u64) c_int {
    out_raw.* = func(ctx);
    return 0;
}

pub fn bridgeThrow() void {}

pub fn bridgeDepth() c_int {
    return 0;
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

    pub fn canTranslateWithLiteralRoots(_: *const Ir, _: ?*const LiteralRoots) bool {
        return false;
    }

    pub fn firstUnsupportedTag(_: *const Ir) ?std.meta.Tag(Ir) {
        return null;
    }

    pub fn firstUnsupportedTagWithLiteralRoots(_: *const Ir, _: ?*const LiteralRoots) ?std.meta.Tag(Ir) {
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
