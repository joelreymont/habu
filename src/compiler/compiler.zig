//! Habu compiler module
//!
//! Exports all compiler components.

pub const ir = @import("ir.zig");
pub const Ir = ir.Ir;
pub const IrBuilder = ir.IrBuilder;

test {
    _ = ir;
}
