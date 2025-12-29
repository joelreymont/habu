//! Habu Register-based IR
//!
//! ARM64-inspired intermediate representation with:
//! - Fixed 32-bit instructions
//! - 32 virtual registers
//! - ~50 orthogonal opcodes
//! - Direct mapping to native code

pub const instruction = @import("instruction.zig");
pub const regalloc = @import("regalloc.zig");
pub const arm64 = @import("arm64.zig");

// Re-export commonly used types
pub const Inst = instruction.Inst;
pub const Op = instruction.Op;
pub const Reg = instruction.Reg;
pub const REG = instruction.REG;
pub const Builder = instruction.Builder;
pub const Function = instruction.Function;
pub const Program = instruction.Program;

// Register allocator
pub const GraphColorAlloc = regalloc.GraphColorAlloc;
pub const PhysReg = regalloc.PhysReg;
pub const PHYS = regalloc.PHYS;

// ARM64 codegen
pub const ARM64 = arm64.ARM64;
pub const Emitter = arm64.Emitter;
pub const generate = arm64.generate;

test {
    _ = instruction;
    _ = regalloc;
    _ = arm64;
}
