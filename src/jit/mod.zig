//! JIT compilation module
//!
//! Hoist SSA-based JIT compiler for ARM64. Translates Habu IR to
//! Hoist IR (SSA form), then compiles to native machine code.

pub const backend = @import("backend.zig");

test {
    _ = backend;
}
