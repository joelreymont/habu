//! JIT runtime context

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;

pub const JitContext = extern struct {
    sp: [*]Value,
    const_pool: [*]Value,
    frame_base: [*]Value,
    heap: *runtime.Heap,
    err: u32,
    _pad: u32 = 0,
};
