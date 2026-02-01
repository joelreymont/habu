//! JIT runtime context

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;

pub const JitContext = extern struct {
    sp: [*]Value,
    const_pool: [*]Value,
    frame_base: [*]Value,
    stack_end: [*]Value,
    heap: *runtime.Heap,
    ret_buf: *RetBuf,
    err: u16,
    _pad: [6]u8 = .{0} ** 6,
};

pub const RetBuf = extern struct {
    value: Value,
    err: u16,
    _pad: [6]u8 = .{0} ** 6,
};

comptime {
    if (@offsetOf(JitContext, "stack_end") != 24) {
        @compileError("JitContext.stack_end offset mismatch");
    }
    if (@offsetOf(JitContext, "ret_buf") != 40) {
        @compileError("JitContext.ret_buf offset mismatch");
    }
    if (@offsetOf(JitContext, "err") != 48) {
        @compileError("JitContext.err offset mismatch");
    }
    if (@sizeOf(RetBuf) != 16) {
        @compileError("RetBuf size mismatch");
    }
    if (@offsetOf(RetBuf, "err") != 8) {
        @compileError("RetBuf.err offset mismatch");
    }
    if (@sizeOf(anyerror) != 2 or @alignOf(anyerror) != 2) {
        @compileError("anyerror ABI mismatch");
    }
}
