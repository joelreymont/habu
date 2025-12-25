//! Habu interpreter module
//!
//! Exports interpreter components.

pub const vm = @import("vm.zig");
pub const Vm = vm.Vm;
pub const VmError = vm.VmError;
pub const Frame = vm.Frame;

pub const repl = @import("repl.zig");
pub const Repl = repl.Repl;
pub const evalString = repl.evalString;

test {
    _ = vm;
    _ = repl;
}
