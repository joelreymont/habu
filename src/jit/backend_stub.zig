const std = @import("std");
const ir_mod = @import("../compiler/ir.zig");
const runtime = @import("../runtime/runtime.zig");

const Ir = ir_mod.Ir;
const Value = runtime.Value;
const Heap = runtime.Heap;

pub fn setHeap(_: *Heap) void {}

pub fn syncHeapFromGlobal(_: *Heap) void {}

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

pub fn isCallResolvable(_: *const Ir, _: []const u8, _: *const std.StringHashMap(void)) bool {
    return false;
}

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
