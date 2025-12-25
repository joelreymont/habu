//! Habu bytecode module
//!
//! Exports bytecode components.

pub const opcodes = @import("opcodes.zig");
pub const Op = opcodes.Op;
pub const Chunk = opcodes.Chunk;

pub const emit = @import("emit.zig");
pub const Emitter = emit.Emitter;
pub const EmitError = emit.EmitError;

test {
    _ = opcodes;
    _ = emit;
}
