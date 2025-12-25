//! Habu bytecode module
//!
//! Exports bytecode components.

pub const opcodes = @import("opcodes.zig");
pub const Op = opcodes.Op;
pub const Chunk = opcodes.Chunk;

test {
    _ = opcodes;
}
