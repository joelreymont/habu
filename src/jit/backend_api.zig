const build_options = @import("build_options");
const std = @import("std");

const impl = if (build_options.use_hoist)
    @import("backend.zig")
else
    @import("backend_stub.zig");

const hoist_contract = if (build_options.use_hoist)
    @import("hoist_contract.zig")
else
    struct {
        pub fn run(_: std.mem.Allocator) !void {}
    };

pub const setHeap = impl.setHeap;
pub const syncHeapFromGlobal = impl.syncHeapFromGlobal;
pub const KnownFn = impl.KnownFn;
pub const CompiledFn = impl.CompiledFn;
pub const IrTranslator = impl.IrTranslator;
pub const isCallResolvable = impl.isCallResolvable;
pub const patchCrossCallsToBL = impl.patchCrossCallsToBL;
pub const compileIr = impl.compileIr;
pub const compileIrWithKnownFns = impl.compileIrWithKnownFns;

test "hoist API contract probe" {
    if (!build_options.use_hoist) return;
    try hoist_contract.run(std.testing.allocator);
}
