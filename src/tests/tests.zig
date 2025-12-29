//! Test module exports

pub const integration = @import("integration.zig");
pub const property = @import("property.zig");

test {
    _ = integration;
    _ = property;
}
