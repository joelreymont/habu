pub const integration = @import("integration.zig");
pub const property = @import("property.zig");
pub const oracle = @import("oracle.zig");
pub const cpl_test = @import("cpl_test.zig");
pub const stdlib_paths = @import("stdlib_paths.zig");
pub const unwind_error = @import("unwind_error.zig");
// pub const loop_tests = @import("loop_tests.zig"); // DISABLED: requires working stdlib

test {
    _ = integration;
    _ = property;
    _ = oracle;
    _ = cpl_test;
    _ = stdlib_paths;
    _ = unwind_error;
    // _ = loop_tests;
}
