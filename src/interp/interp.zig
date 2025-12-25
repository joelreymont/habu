//! Habu interpreter module
//!
//! Exports interpreter components.

pub const vm = @import("vm.zig");
pub const Vm = vm.Vm;
pub const VmError = vm.VmError;
pub const Frame = vm.Frame;

test {
    _ = vm;
}
