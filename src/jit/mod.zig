//! JIT compilation module
//!
//! Copy-and-patch JIT for ARM64. Compiles bytecode to native
//! machine code using pre-compiled stencils.

pub const stencils = @import("stencils.zig");
pub const patch = @import("patch.zig");
pub const jit = @import("jit.zig");

pub const Jit = jit.Jit;
pub const JitFn = jit.JitFn;
pub const Stencil = stencils.Stencil;
pub const CodeBuffer = patch.CodeBuffer;

test {
    _ = stencils;
    _ = patch;
    _ = jit;
}
