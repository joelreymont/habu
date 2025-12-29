//! Test module exports

pub const integration = @import("integration.zig");
pub const property = @import("property.zig");
pub const oracle = @import("oracle.zig");

test {
    _ = integration;
    _ = property;
    _ = oracle;
}
