//! Habu - A Zig-based Lisp with gradual typing
//!
//! Architecture:
//! - Types-first design with Racket-style contracts
//! - Bytecode interpreter (works on WASM)
//! - Copy-and-patch JIT (native platforms)
//! - Cheney copying GC

pub const diagnostic = @import("diagnostic.zig");
pub const types = @import("types/types.zig");

test {
    _ = diagnostic;
    _ = types;
}
