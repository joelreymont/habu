//! ARM64 JIT stencils for copy-and-patch compilation
//!
//! Each stencil is a pre-compiled machine code template with "holes"
//! that get patched at JIT time with actual values/addresses.
//!
//! Stencil format:
//! - code: []const u8 - raw ARM64 machine code
//! - holes: []const Hole - locations to patch
//!
//! Hole types:
//! - imm64: 64-bit immediate (for constants, addresses)
//! - imm32: 32-bit immediate
//! - rel26: 26-bit relative offset (for BL/B)
//! - rel19: 19-bit relative offset (for B.cond)

const std = @import("std");

/// Hole type for patching
pub const HoleType = enum {
    /// 64-bit immediate value (movz/movk sequence)
    imm64,
    /// 32-bit immediate value
    imm32,
    /// 26-bit relative branch offset (BL, B)
    rel26,
    /// 19-bit relative branch offset (B.cond, CBZ)
    rel19,
    /// 14-bit relative branch offset (TBZ, TBNZ)
    rel14,
};

/// A hole in a stencil that needs patching
pub const Hole = struct {
    /// Byte offset in the stencil
    offset: u32,
    /// Type of hole
    hole_type: HoleType,
    /// Symbolic name for the hole (for debugging)
    name: []const u8,
};

/// A stencil template
pub const Stencil = struct {
    /// Raw machine code
    code: []const u8,
    /// Holes to patch
    holes: []const Hole,
    /// Human-readable name
    name: []const u8,
};

// ============================================================================
// ARM64 Instruction Encoding Helpers
// ============================================================================

/// Encode MOVZ instruction: MOVZ Xd, #imm16, LSL #shift
/// shift must be 0, 16, 32, or 48
fn movz(rd: u5, imm16: u16, shift: u6) u32 {
    const hw: u2 = @intCast(shift / 16);
    return 0xD2800000 | (@as(u32, hw) << 21) | (@as(u32, imm16) << 5) | rd;
}

/// Encode MOVK instruction: MOVK Xd, #imm16, LSL #shift
fn movk(rd: u5, imm16: u16, shift: u6) u32 {
    const hw: u2 = @intCast(shift / 16);
    return 0xF2800000 | (@as(u32, hw) << 21) | (@as(u32, imm16) << 5) | rd;
}

/// Encode ADD instruction: ADD Xd, Xn, Xm
fn add_reg(rd: u5, rn: u5, rm: u5) u32 {
    return 0x8B000000 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode SUB instruction: SUB Xd, Xn, Xm
fn sub_reg(rd: u5, rn: u5, rm: u5) u32 {
    return 0xCB000000 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode ADD immediate: ADD Xd, Xn, #imm12
fn add_imm(rd: u5, rn: u5, imm12: u12) u32 {
    return 0x91000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

/// Encode SUB immediate: SUB Xd, Xn, #imm12
fn sub_imm(rd: u5, rn: u5, imm12: u12) u32 {
    return 0xD1000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

/// Encode LSR immediate: LSR Xd, Xn, #shift
fn lsr_imm(rd: u5, rn: u5, shift: u6) u32 {
    // UBFM Xd, Xn, #shift, #63
    return 0xD340FC00 | (@as(u32, shift) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode LSL immediate: LSL Xd, Xn, #shift
fn lsl_imm(rd: u5, rn: u5, shift: u6) u32 {
    // UBFM Xd, Xn, #(-shift mod 64), #(63-shift)
    const immr: u6 = @truncate(64 -% @as(u7, shift));
    const imms: u6 = 63 - shift;
    return 0xD3400000 | (@as(u32, immr) << 16) | (@as(u32, imms) << 10) | (@as(u32, rn) << 5) | rd;
}

/// Encode ORR immediate: ORR Xd, Xn, #imm (for setting bit0)
fn orr_imm_bit0(rd: u5, rn: u5) u32 {
    // ORR Xd, Xn, #1 - immediate bitmask encoding for 1
    // N=1, immr=0, imms=0 encodes #1
    return 0xB2400000 | (@as(u32, rn) << 5) | rd;
}

/// Encode AND immediate: AND Xd, Xn, #~1 (for clearing bit0)
fn and_not_bit0(rd: u5, rn: u5) u32 {
    // AND Xd, Xn, #~1 = AND Xd, Xn, #0xFFFFFFFFFFFFFFFE
    // N=1, immr=1, imms=62 encodes this mask
    return 0x92407C00 | (@as(u32, rn) << 5) | rd;
}

/// Encode RET instruction
fn ret() u32 {
    return 0xD65F03C0; // RET X30
}

/// Encode BL instruction with placeholder offset
fn bl_placeholder() u32 {
    return 0x94000000; // BL with offset 0 (to be patched)
}

/// Encode B instruction with placeholder offset
fn b_placeholder() u32 {
    return 0x14000000; // B with offset 0 (to be patched)
}

/// Encode CBZ: CBZ Xn, offset (branch if zero)
fn cbz_placeholder(rn: u5) u32 {
    return 0xB4000000 | @as(u32, rn); // CBZ with offset 0
}

/// Encode CBNZ: CBNZ Xn, offset (branch if not zero)
fn cbnz_placeholder(rn: u5) u32 {
    return 0xB5000000 | @as(u32, rn); // CBNZ with offset 0
}

/// Convert u32 instruction to bytes (little-endian)
fn inst_bytes(inst: u32) [4]u8 {
    return @bitCast(inst);
}

// ============================================================================
// Register Conventions
// ============================================================================

// x0: accumulator (result register)
// x1-x7: argument registers
// x8: indirect result location
// x9-x15: caller-saved temporaries
// x16-x17: intra-procedure-call scratch
// x18: platform register (reserved)
// x19-x28: callee-saved
// x29: frame pointer
// x30: link register
// sp: stack pointer

const X0: u5 = 0; // accumulator
const X1: u5 = 1; // temp/arg
const X2: u5 = 2; // temp/arg
const X9: u5 = 9; // scratch
const X10: u5 = 10; // scratch
const X30: u5 = 30; // link register

// ============================================================================
// Stencil Definitions
// ============================================================================

/// Load 64-bit immediate into x0
/// Holes: imm64 at offset 0 (4 instructions, 16 bytes)
pub const load_imm64 = Stencil{
    .name = "load_imm64",
    .code = &(inst_bytes(movz(X0, 0, 0)) ++
        inst_bytes(movk(X0, 0, 16)) ++
        inst_bytes(movk(X0, 0, 32)) ++
        inst_bytes(movk(X0, 0, 48))),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .imm64, .name = "value" },
    },
};

/// Add two fixnums: x0 = x0 + x1 (preserving tag)
/// Both inputs are tagged fixnums (bit0=1)
/// Result: untag both, add, retag
pub const add_fixnum = Stencil{
    .name = "add_fixnum",
    .code = &(
    // Untag x0: x9 = x0 >> 1
        inst_bytes(lsr_imm(X9, X0, 1)) ++
        // Untag x1: x10 = x1 >> 1
        inst_bytes(lsr_imm(X10, X1, 1)) ++
        // Add: x0 = x9 + x10
        inst_bytes(add_reg(X0, X9, X10)) ++
        // Retag: x0 = (x0 << 1) | 1
        inst_bytes(lsl_imm(X0, X0, 1)) ++
        inst_bytes(orr_imm_bit0(X0, X0))),
    .holes = &[_]Hole{},
};

/// Subtract two fixnums: x0 = x0 - x1 (preserving tag)
pub const sub_fixnum = Stencil{
    .name = "sub_fixnum",
    .code = &(
    // Untag x0: x9 = x0 >> 1
        inst_bytes(lsr_imm(X9, X0, 1)) ++
        // Untag x1: x10 = x1 >> 1
        inst_bytes(lsr_imm(X10, X1, 1)) ++
        // Sub: x0 = x9 - x10
        inst_bytes(sub_reg(X0, X9, X10)) ++
        // Retag: x0 = (x0 << 1) | 1
        inst_bytes(lsl_imm(X0, X0, 1)) ++
        inst_bytes(orr_imm_bit0(X0, X0))),
    .holes = &[_]Hole{},
};

/// Return from function (x0 already has result)
pub const ret_stencil = Stencil{
    .name = "ret",
    .code = &inst_bytes(ret()),
    .holes = &[_]Hole{},
};

/// Call function at address
/// Holes: rel26 at offset 0
pub const call_stencil = Stencil{
    .name = "call",
    .code = &inst_bytes(bl_placeholder()),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .rel26, .name = "target" },
    },
};

/// Unconditional branch
/// Holes: rel26 at offset 0
pub const branch_stencil = Stencil{
    .name = "branch",
    .code = &inst_bytes(b_placeholder()),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .rel26, .name = "target" },
    },
};

/// Branch if x0 is nil (zero)
/// Holes: rel19 at offset 0
pub const branch_nil = Stencil{
    .name = "branch_nil",
    .code = &inst_bytes(cbz_placeholder(X0)),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .rel19, .name = "target" },
    },
};

/// Branch if x0 is not nil (non-zero)
/// Holes: rel19 at offset 0
pub const branch_not_nil = Stencil{
    .name = "branch_not_nil",
    .code = &inst_bytes(cbnz_placeholder(X0)),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .rel19, .name = "target" },
    },
};

/// Cons cell allocation stub
/// This would call the runtime allocator
/// For now, just a call placeholder
pub const cons_stencil = Stencil{
    .name = "cons",
    .code = &inst_bytes(bl_placeholder()),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .rel26, .name = "alloc_cons" },
    },
};

/// Car: x0 = car(x0)
/// Assumes x0 is a tagged cons pointer (bit0=0, bits 1-3 = 0)
/// Car is at offset 0 from untagged pointer
pub const car_stencil = Stencil{
    .name = "car",
    .code = &(
    // Clear tag bits: x0 = x0 & ~0xF (pointer mask)
        inst_bytes(and_not_bit0(X0, X0)) ++ // clears bit0
        inst_bytes(0xD27E0000 | @as(u32, X0)) ++ // AND x0, x0, #~0xE (clear bits 1-3)
        // Load car: x0 = [x0]
        inst_bytes(0xF9400000 | @as(u32, X0) | (@as(u32, X0) << 5)) // LDR x0, [x0]
    ),
    .holes = &[_]Hole{},
};

/// Cdr: x0 = cdr(x0)
/// Cdr is at offset 8 from untagged pointer
pub const cdr_stencil = Stencil{
    .name = "cdr",
    .code = &(
    // Clear tag bits
        inst_bytes(and_not_bit0(X0, X0)) ++
        inst_bytes(0xD27E0000 | @as(u32, X0)) ++
        // Load cdr: x0 = [x0 + 8]
        inst_bytes(0xF9400400 | @as(u32, X0) | (@as(u32, X0) << 5)) // LDR x0, [x0, #8]
    ),
    .holes = &[_]Hole{},
};

// ============================================================================
// Tests
// ============================================================================

test "stencil sizes" {
    const testing = std.testing;

    // load_imm64 should be 16 bytes (4 instructions)
    try testing.expectEqual(@as(usize, 16), load_imm64.code.len);

    // add_fixnum should be 20 bytes (5 instructions)
    try testing.expectEqual(@as(usize, 20), add_fixnum.code.len);

    // ret should be 4 bytes (1 instruction)
    try testing.expectEqual(@as(usize, 4), ret_stencil.code.len);
}

test "instruction encoding" {
    const testing = std.testing;

    // MOVZ X0, #0 should encode to 0xD2800000
    try testing.expectEqual(@as(u32, 0xD2800000), movz(0, 0, 0));

    // RET should encode to 0xD65F03C0
    try testing.expectEqual(@as(u32, 0xD65F03C0), ret());

    // ADD X0, X1, X2 should encode to 0x8B020020
    try testing.expectEqual(@as(u32, 0x8B020020), add_reg(0, 1, 2));
}
