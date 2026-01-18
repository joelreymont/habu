pub const integration = @import("integration.zig");
pub const property = @import("property.zig");
pub const oracle = @import("oracle.zig");
// pub const loop_tests = @import("loop_tests.zig"); // DISABLED: requires working stdlib

test {
    _ = integration;
    _ = property;
    _ = oracle;
    // _ = loop_tests;
}
