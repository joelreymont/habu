pub const integration = @import("integration.zig");
pub const property = @import("property.zig");
pub const oracle = @import("oracle.zig");
pub const cpl_test = @import("cpl_test.zig");
pub const stdlib_paths = @import("stdlib_paths.zig");
pub const jit_parity = @import("jit_parity.zig");
pub const jit_tiering = @import("jit_tiering.zig");
// pub const loop_tests = @import("loop_tests.zig"); // DISABLED: requires working stdlib

test {
    _ = integration;
    _ = property;
    _ = oracle;
    _ = cpl_test;
    _ = stdlib_paths;
    _ = jit_parity;
    _ = jit_tiering;
    // _ = loop_tests;
}
