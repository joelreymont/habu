//! Test module exports

pub const integration = @import("integration.zig");

test {
    _ = integration;
}
