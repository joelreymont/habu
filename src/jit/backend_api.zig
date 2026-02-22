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
pub const setCallBridge = impl.setCallBridge;
pub const syncHeapFromGlobal = impl.syncHeapFromGlobal;
pub const allocPtrRaw = impl.allocPtrRaw;
pub const BridgeRunFn = impl.BridgeRunFn;
pub const bridgeRun = impl.bridgeRun;
pub const bridgeThrow = impl.bridgeThrow;
pub const bridgeDepth = impl.bridgeDepth;
pub const CallBridge = impl.CallBridge;
pub const LiteralRoots = impl.LiteralRoots;
pub const KnownFn = impl.KnownFn;
pub const CompiledFn = impl.CompiledFn;
pub const IrTranslator = impl.IrTranslator;
pub const patchCrossCallsToBL = impl.patchCrossCallsToBL;
pub const compileIr = impl.compileIr;
pub const compileIrWithKnownFns = impl.compileIrWithKnownFns;
pub const compileIrWithKnownFnsAndLiteralRoots = impl.compileIrWithKnownFnsAndLiteralRoots;

test "hoist API contract probe" {
    if (!build_options.use_hoist) return;
    try hoist_contract.run(std.testing.allocator);
}
