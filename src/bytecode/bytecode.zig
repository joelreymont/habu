//! Habu bytecode module
//!
//! Exports bytecode components.

pub const opcodes = @import("opcodes.zig");
pub const Op = opcodes.Op;
// Use GC-managed Chunk from runtime
const runtime = @import("../runtime/runtime.zig");
pub const Chunk = runtime.Chunk;

pub const emit = @import("emit.zig");
pub const Emitter = emit.Emitter;
pub const Error = emit.Error;

pub const disasm = @import("disasm.zig");
pub const disassemble = disasm.disassemble;
pub const disassembleInstruction = disasm.disassembleInstruction;

test {
    _ = opcodes;
    _ = emit;
    _ = disasm;
}
