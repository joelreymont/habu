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

/// Encode AND instruction: AND Xd, Xn, Xm
fn and_reg(rd: u5, rn: u5, rm: u5) u32 {
    return 0x8A000000 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode CMP instruction: CMP Xn, Xm
fn cmp_reg(rn: u5, rm: u5) u32 {
    return 0xEB00001F | (@as(u32, rm) << 16) | (@as(u32, rn) << 5);
}

/// Encode conditional branch with placeholder offset
fn b_cond(cond: u4) u32 {
    return 0x54000000 | @as(u32, cond);
}

/// Encode ADD immediate: ADD Xd, Xn, #imm12
pub fn add_imm(rd: u5, rn: u5, imm12: u12) u32 {
    return 0x91000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

/// Encode SUB immediate: SUB Xd, Xn, #imm12
pub fn sub_imm(rd: u5, rn: u5, imm12: u12) u32 {
    return 0xD1000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | rd;
}

/// Encode LDR Xt, [Xn, #imm12] — unsigned offset, 8-byte aligned
/// imm12 is in bytes (must be multiple of 8, max 32760)
pub fn ldr_imm(rt: u5, rn: u5, byte_offset: u12) u32 {
    // LDR Xt, [Xn, #pimm] where pimm = byte_offset/8
    const scaled: u12 = byte_offset >> 3;
    return 0xF9400000 | (@as(u32, scaled) << 10) | (@as(u32, rn) << 5) | rt;
}

/// Encode STR Xt, [Xn, #imm12] — unsigned offset, 8-byte aligned
/// imm12 is in bytes (must be multiple of 8, max 32760)
pub fn str_imm(rt: u5, rn: u5, byte_offset: u12) u32 {
    // STR Xt, [Xn, #pimm] where pimm = byte_offset/8
    const scaled: u12 = byte_offset >> 3;
    return 0xF9000000 | (@as(u32, scaled) << 10) | (@as(u32, rn) << 5) | rt;
}

/// Encode LSR immediate: LSR Xd, Xn, #shift
fn lsr_imm(rd: u5, rn: u5, shift: u6) u32 {
    // UBFM Xd, Xn, #shift, #63
    return 0xD340FC00 | (@as(u32, shift) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode ASR immediate: ASR Xd, Xn, #shift
fn asr_imm(rd: u5, rn: u5, shift: u6) u32 {
    // SBFM Xd, Xn, #shift, #63
    return 0x9340FC00 | (@as(u32, shift) << 16) | (@as(u32, rn) << 5) | rd;
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

/// Encode MUL instruction: MUL Xd, Xn, Xm
fn mul_reg(rd: u5, rn: u5, rm: u5) u32 {
    return 0x9B007C00 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode SMULH instruction: SMULH Xd, Xn, Xm
fn smulh_reg(rd: u5, rn: u5, rm: u5) u32 {
    return 0x9B407C00 | (@as(u32, rm) << 16) | (@as(u32, rn) << 5) | rd;
}

/// Encode RET instruction
fn ret() u32 {
    return 0xD65F03C0; // RET X30
}

/// Encode BLR instruction
fn blr(rn: u5) u32 {
    return 0xD63F0000 | (@as(u32, rn) << 5);
}

/// Encode BL instruction with placeholder offset
pub fn bl_placeholder() u32 {
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

/// Encode CBZ (32-bit): CBZ Wn, offset
fn cbz_w_placeholder(rn: u5) u32 {
    return 0x34000000 | @as(u32, rn); // CBZ Wn with offset 0
}

/// Encode CBNZ: CBNZ Xn, offset
fn cbnz_placeholder(rn: u5) u32 {
    return 0xB5000000 | @as(u32, rn); // CBNZ with offset 0
}

/// Encode CBNZ (32-bit): CBNZ Wn, offset
fn cbnz_w_placeholder(rn: u5) u32 {
    return 0x35000000 | @as(u32, rn); // CBNZ Wn with offset 0
}

/// Encode STP (pre-index): STP Xt1, Xt2, [Xn, #imm7*8]!
pub fn stp_pre(rt1: u5, rt2: u5, rn: u5, offset: i7) u32 {
    const imm: u32 = @bitCast(@as(i32, offset) & 0x7F);
    return 0xA9800000 | (imm << 15) | (@as(u32, rt2) << 10) | (@as(u32, rn) << 5) | rt1;
}

/// Encode LDP (post-index): LDP Xt1, Xt2, [Xn], #imm7*8
pub fn ldp_post(rt1: u5, rt2: u5, rn: u5, offset: i7) u32 {
    const imm: u32 = @bitCast(@as(i32, offset) & 0x7F);
    return 0xA8C00000 | (imm << 15) | (@as(u32, rt2) << 10) | (@as(u32, rn) << 5) | rt1;
}

/// Encode TBZ: TBZ Xt, #bit, offset (branch if bit is zero)
fn tbz_placeholder(rt: u5, bit: u6) u32 {
    // TBZ: b5|011011|0|b40|imm14|Rt
    // b5 = bit[5], b40 = bit[4:0]
    const b5: u32 = @as(u32, bit >> 5) & 1;
    const b40: u32 = @as(u32, bit) & 0x1F;
    return (b5 << 31) | (0x36 << 24) | (b40 << 19) | @as(u32, rt);
}

/// Encode AND immediate: AND Xd, Xn, #0xF (extract low 4 bits)
fn and_low4(rd: u5, rn: u5) u32 {
    // AND Xd, Xn, #0xF
    // N=1, immr=0, imms=3 encodes #0xF
    return 0x92400C00 | (@as(u32, rn) << 5) | rd;
}

/// Encode CMP immediate: CMP Xn, #imm12
fn cmp_imm(rn: u5, imm12: u12) u32 {
    // SUBS XZR, Xn, #imm12
    return 0xF1000000 | (@as(u32, imm12) << 10) | (@as(u32, rn) << 5) | XZR;
}

/// Encode B.NE (branch if not equal): B.NE <offset>
fn bne_placeholder() u32 {
    return 0x54000001; // B.NE with offset 0
}

/// Convert u32 instruction to bytes (little-endian)
pub fn inst_bytes(inst: u32) [4]u8 {
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
const X3: u5 = 3; // temp/arg
const X8: u5 = 8; // sret mirror
const X9: u5 = 9; // scratch
const X10: u5 = 10; // scratch
const X11: u5 = 11; // scratch
const X12: u5 = 12; // scratch
const X13: u5 = 13; // scratch
const X16: u5 = 16; // scratch
pub const X19: u5 = 19; // stack pointer
pub const X20: u5 = 20; // const_pool base
pub const X21: u5 = 21; // ret_buf pointer
pub const X22: u5 = 22; // ctx pointer
pub const X23: u5 = 23; // frame base
pub const X24: u5 = 24; // saved scratch
const X25: u5 = 25; // callee-saved
const X26: u5 = 26; // callee-saved
const X27: u5 = 27; // callee-saved
const X28: u5 = 28; // callee-saved
pub const X29: u5 = 29; // fp
pub const X30: u5 = 30; // link register
const XZR: u5 = 31; // zero register
pub const SP: u5 = 31; // stack pointer

const COND_HS: u4 = 0x2; // unsigned >=
const COND_LS: u4 = 0x9; // unsigned <=

const ERR_STACK_OVERFLOW: u16 = @intFromError(error.StackOverflow);
const ERR_STACK_UNDERFLOW: u16 = @intFromError(error.StackUnderflow);
const ERR_TYPE_MISMATCH: u16 = @intFromError(error.TypeMismatch);

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

/// Add two fixnums: x9 = (x0 + x1) untagged
/// Both inputs are tagged fixnums (bit0=1)
pub const add_fixnum = Stencil{
    .name = "add_fixnum_raw",
    .code = &(
        // Untag x0: x9 = x0 >> 1 (arith)
        inst_bytes(asr_imm(X9, X0, 1)) ++
            // Untag x1: x10 = x1 >> 1 (arith)
            inst_bytes(asr_imm(X10, X1, 1)) ++
            // Add: x9 = x9 + x10
            inst_bytes(add_reg(X9, X9, X10))),
    .holes = &[_]Hole{},
};

/// Subtract two fixnums: x9 = (x0 - x1) untagged
pub const sub_fixnum = Stencil{
    .name = "sub_fixnum_raw",
    .code = &(
        // Untag x0: x9 = x0 >> 1 (arith)
        inst_bytes(asr_imm(X9, X0, 1)) ++
            // Untag x1: x10 = x1 >> 1 (arith)
            inst_bytes(asr_imm(X10, X1, 1)) ++
            // Sub: x9 = x9 - x10
            inst_bytes(sub_reg(X9, X9, X10))),
    .holes = &[_]Hole{},
};

/// Return from function (x0 already has result)
pub const ret_stencil = Stencil{
    .name = "ret",
    .code = &inst_bytes(ret()),
    .holes = &[_]Hole{},
};

/// Function prologue: save lr, save regs, load sp/const_pool/frame_base/ret_buf from ctx
pub const prologue_stencil = Stencil{
    .name = "prologue",
    .code = &(
        inst_bytes(stp_pre(X29, X30, SP, -2)) ++
            inst_bytes(add_imm(X29, SP, 0)) ++
            inst_bytes(stp_pre(X19, X20, SP, -2)) ++
            inst_bytes(stp_pre(X21, X22, SP, -2)) ++
            inst_bytes(stp_pre(X23, X24, SP, -2)) ++
            // MOV x22, x0 (ctx)
            inst_bytes(add_imm(X22, X0, 0)) ++
            // LDR x19, [x22, #0] (ctx.sp)
            inst_bytes(0xF94002D3) ++
            // LDR x20, [x22, #8] (ctx.const_pool)
            inst_bytes(0xF94006D4) ++
            // LDR x23, [x22, #16] (ctx.frame_base)
            inst_bytes(0xF9400AD7) ++
            // LDR x24, [x22, #24] (ctx.stack_end)
            inst_bytes(0xF9400ED8) ++
            // LDR x21, [x22, #40] (ctx.ret_buf)
            inst_bytes(0xF94016D5) ++
            // STRH wzr, [x22, #48] (clear ctx.err)
            inst_bytes(0x790062DF)),
    .holes = &[_]Hole{},
};

/// Function epilogue: restore regs, return
pub const epilogue_stencil = Stencil{
    .name = "epilogue",
    .code = &(
        inst_bytes(ldp_post(X23, X24, SP, 2)) ++
            inst_bytes(ldp_post(X21, X22, SP, 2)) ++
            inst_bytes(ldp_post(X19, X20, SP, 2)) ++
            inst_bytes(ldp_post(X29, X30, SP, 2)) ++
            inst_bytes(ret())),
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

/// Call absolute address via BLR
/// Holes: imm64 at offset 0
pub const call_abs = Stencil{
    .name = "call_abs",
    .code = &(
        inst_bytes(movz(X16, 0, 0)) ++
            inst_bytes(movk(X16, 0, 16)) ++
            inst_bytes(movk(X16, 0, 32)) ++
            inst_bytes(movk(X16, 0, 48)) ++
            inst_bytes(blr(X16))),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .imm64, .name = "target" },
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
        // Clear tag bits: x0 = (x0 >> 4) << 4
        inst_bytes(lsr_imm(X0, X0, 4)) ++
            inst_bytes(lsl_imm(X0, X0, 4)) ++
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
        // Clear tag bits: x0 = (x0 >> 4) << 4
        inst_bytes(lsr_imm(X0, X0, 4)) ++
            inst_bytes(lsl_imm(X0, X0, 4)) ++
            // Load cdr: x0 = [x0 + 8]
            inst_bytes(0xF9400400 | @as(u32, X0) | (@as(u32, X0) << 5)) // LDR x0, [x0, #8]
    ),
    .holes = &[_]Hole{},
};

/// Multiply two fixnums: x9 = low(x0 * x1) untagged, x11 = high
pub const mul_fixnum = Stencil{
    .name = "mul_fixnum_raw",
    .code = &(
        // Untag x0: x9 = x0 >> 1 (arith)
        inst_bytes(asr_imm(X9, X0, 1)) ++
            // Untag x1: x10 = x1 >> 1 (arith)
            inst_bytes(asr_imm(X10, X1, 1)) ++
            // High half: x11 = smulh(x9, x10)
            inst_bytes(smulh_reg(X11, X9, X10)) ++
            // Low half: x9 = x9 * x10
            inst_bytes(mul_reg(X9, X9, X10))),
    .holes = &[_]Hole{},
};

/// Negate fixnum: x9 = -x0 untagged
pub const neg_fixnum = Stencil{
    .name = "neg_fixnum_raw",
    .code = &(
        // Untag: x9 = x0 >> 1 (arith)
        inst_bytes(asr_imm(X9, X0, 1)) ++
            // Negate: x9 = 0 - x9
            inst_bytes(sub_reg(X9, XZR, X9))),
    .holes = &[_]Hole{},
};

/// Check mul overflow: branch to slow if high != sign(low)
/// Uses x9 (low), x11 (high), x12 temp
pub const mul_overflow_check = Stencil{
    .name = "mul_overflow_check",
    .code = &(
        // ASR x12, x9, #63 (sign of low)
        inst_bytes(asr_imm(X12, X9, 63)) ++
            // SUB x12, x11, x12
            inst_bytes(sub_reg(X12, X11, X12)) ++
            // CBNZ x12, <slow>
            inst_bytes(cbnz_placeholder(X12))),
    .holes = &[_]Hole{},
};
pub const mul_overflow_check_branch_offset: usize = 8;

/// Check fixnum range for untagged x9, tag into x0 if ok
/// Branches to slow path if out of range.
pub const fixnum_range_check = Stencil{
    .name = "fixnum_range_check",
    .code = &(
        // ASR x11, x9, #62
        inst_bytes(asr_imm(X11, X9, 62)) ++
            // CMP x11, #0
            inst_bytes(0xF100017F) ++
            // CSET w12, EQ
            inst_bytes(0x1A9F17EC) ++
            // CMP x11, #-1
            inst_bytes(0xB100057F) ++
            // CSET w13, EQ
            inst_bytes(0x1A9F17ED) ++
            // ORR w12, w12, w13
            inst_bytes(0x2A0D018C) ++
            // CBZ w12, <slow>
            inst_bytes(cbz_w_placeholder(X12)) ++
            // Tag: x0 = (x9 << 1) | 1
            inst_bytes(lsl_imm(X0, X9, 1)) ++
            inst_bytes(orr_imm_bit0(X0, X0))),
    .holes = &[_]Hole{},
};
pub const fixnum_range_check_branch_offset: usize = 24;

/// Push nil (zero) onto accumulator
pub const push_nil_stencil = Stencil{
    .name = "push_nil",
    .code = &inst_bytes(0xD2800000), // MOV x0, #0
    .holes = &[_]Hole{},
};

/// Push t (Value.t = 0x2) onto accumulator
pub const push_t_stencil = Stencil{
    .name = "push_t",
    .code = &inst_bytes(0xD2800040), // MOV x0, #2
    .holes = &[_]Hole{},
};

/// Compare equal (eq): x0 = (x0 == x1) ? t : nil
pub const eq_stencil = Stencil{
    .name = "eq",
    .code = &(
        // CMP x0, x1
        inst_bytes(0xEB01001F) ++
            // CSET x0, EQ (set to 1 if equal, 0 otherwise)
            inst_bytes(0x9A9F17E0) ++
            // Convert to tagged: x0 = x0 << 1 (gives 0 or 2)
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Compare less than: x0 = (x0 < x1) ? t : nil (for fixnums)
pub const lt_stencil = Stencil{
    .name = "lt",
    .code = &(
        // CMP x0, x1 (signed compare for tagged fixnums)
        inst_bytes(0xEB01001F) ++
            // CSET x0, LT
            inst_bytes(0x9A9FA7E0) ++
            // Convert to tagged
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Compare greater than: x0 = (x0 > x1) ? t : nil
pub const gt_stencil = Stencil{
    .name = "gt",
    .code = &(
        // CMP x0, x1
        inst_bytes(0xEB01001F) ++
            // CSET x0, GT
            inst_bytes(0x9A9FD7E0) ++
            // Convert to tagged
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Compare less than or equal: x0 = (x0 <= x1) ? t : nil
pub const le_stencil = Stencil{
    .name = "le",
    .code = &(
        // CMP x0, x1
        inst_bytes(0xEB01001F) ++
            // CSET x0, LE
            inst_bytes(0x9A9FC7E0) ++
            // Convert to tagged
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Compare greater than or equal: x0 = (x0 >= x1) ? t : nil
pub const ge_stencil = Stencil{
    .name = "ge",
    .code = &(
        // CMP x0, x1
        inst_bytes(0xEB01001F) ++
            // CSET x0, GE
            inst_bytes(0x9A9FB7E0) ++
            // Convert to tagged
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Stack push: store x0 to stack and advance sp
/// Uses x19 as stack pointer (callee-saved)
pub const stack_push = Stencil{
    .name = "stack_push",
    .code = &(
        // MOVZ x9, #err
        inst_bytes(movz(X9, ERR_STACK_OVERFLOW, 0)) ++
            // CMP x19, x24
            inst_bytes(cmp_reg(X19, X24)) ++
            // B.HS <err>
            inst_bytes(b_cond(COND_HS)) ++
            // STR x0, [x19], #8 (post-increment)
            inst_bytes(0xF8008660)),
    .holes = &[_]Hole{},
};
pub const stack_push_branch_offset: usize = 8;

/// Stack push: store x1 to stack and advance sp
/// Uses x19 as stack pointer (callee-saved)
pub const stack_push_x1 = Stencil{
    .name = "stack_push_x1",
    .code = &(
        // MOVZ x9, #err
        inst_bytes(movz(X9, ERR_STACK_OVERFLOW, 0)) ++
            // CMP x19, x24
            inst_bytes(cmp_reg(X19, X24)) ++
            // B.HS <err>
            inst_bytes(b_cond(COND_HS)) ++
            // STR x1, [x19], #8 (post-increment)
            inst_bytes(0xF8008661)),
    .holes = &[_]Hole{},
};
pub const stack_push_x1_branch_offset: usize = 8;

/// Stack pop: load from stack to x0 and decrement sp
pub const stack_pop = Stencil{
    .name = "stack_pop",
    .code = &(
        // MOVZ x9, #err
        inst_bytes(movz(X9, ERR_STACK_UNDERFLOW, 0)) ++
            // CMP x19, x23
            inst_bytes(cmp_reg(X19, X23)) ++
            // B.LS <err>
            inst_bytes(b_cond(COND_LS)) ++
            // LDR x0, [x19, #-8]! (pre-decrement)
            inst_bytes(0xF85F8E60)),
    .holes = &[_]Hole{},
};
pub const stack_pop_branch_offset: usize = 8;

/// Stack pop to x1 (for binary ops)
pub const stack_pop_x1 = Stencil{
    .name = "stack_pop_x1",
    .code = &(
        // MOVZ x9, #err
        inst_bytes(movz(X9, ERR_STACK_UNDERFLOW, 0)) ++
            // CMP x19, x23
            inst_bytes(cmp_reg(X19, X23)) ++
            // B.LS <err>
            inst_bytes(b_cond(COND_LS)) ++
            // LDR x1, [x19, #-8]!
            inst_bytes(0xF85F8E61)),
    .holes = &[_]Hole{},
};
pub const stack_pop_x1_branch_offset: usize = 8;

/// Move x1 <- x0
pub const mov_x1_x0 = Stencil{
    .name = "mov_x1_x0",
    .code = &inst_bytes(add_imm(X1, X0, 0)),
    .holes = &[_]Hole{},
};

/// Move x2 <- x0
pub const mov_x2_x0 = Stencil{
    .name = "mov_x2_x0",
    .code = &inst_bytes(add_imm(X2, X0, 0)),
    .holes = &[_]Hole{},
};

/// Move x2 <- x1
pub const mov_x2_x1 = Stencil{
    .name = "mov_x2_x1",
    .code = &inst_bytes(add_imm(X2, X1, 0)),
    .holes = &[_]Hole{},
};

/// Move x3 <- x1
pub const mov_x3_x1 = Stencil{
    .name = "mov_x3_x1",
    .code = &inst_bytes(add_imm(X3, X1, 0)),
    .holes = &[_]Hole{},
};

/// Move x0 <- x22 (ctx)
pub const mov_x0_x22 = Stencil{
    .name = "mov_x0_x22",
    .code = &inst_bytes(add_imm(X0, X22, 0)),
    .holes = &[_]Hole{},
};

/// Move x1 <- x22 (ctx)
pub const mov_x1_x22 = Stencil{
    .name = "mov_x1_x22",
    .code = &inst_bytes(add_imm(X1, X22, 0)),
    .holes = &[_]Hole{},
};

/// Store ctx.sp <- x19
pub const store_ctx_sp = Stencil{
    .name = "store_ctx_sp",
    .code = &inst_bytes(0xF90002D3), // STR x19, [x22, #0]
    .holes = &[_]Hole{},
};

/// Load x19 <- ctx.sp, and refresh x20 <- ctx.const_pool (may move after GC)
pub const load_ctx_sp = Stencil{
    .name = "load_ctx_sp",
    .code = &(
        // LDR x19, [x22, #0]
        inst_bytes(0xF94002D3) ++
            // LDR x20, [x22, #8]
            inst_bytes(0xF94006D4)),
    .holes = &[_]Hole{},
};

/// Move x0 <- x21 (ret_buf)
pub const mov_x0_x21 = Stencil{
    .name = "mov_x0_x21",
    .code = &inst_bytes(add_imm(X0, X21, 0)),
    .holes = &[_]Hole{},
};

/// Load error return trace pointer into x0
pub const load_err_trace = Stencil{
    .name = "load_err_trace",
    .code = &inst_bytes(0xF94022C0), // LDR x0, [x22, #64]
    .holes = &[_]Hole{},
};

/// Move x8 <- x21 (ret_buf)
pub const mov_x8_x21 = Stencil{
    .name = "mov_x8_x21",
    .code = &inst_bytes(add_imm(X8, X21, 0)),
    .holes = &[_]Hole{},
};

/// Clear ret_buf.err (write zero)
pub const clear_retbuf_err = Stencil{
    .name = "clear_retbuf_err",
    .code = &(
        // STRH wzr, [x21, #8]
        inst_bytes(0x790012BF)),
    .holes = &[_]Hole{},
};

/// Check runtime call error tag in ret_buf and load value
/// Loads err tag into w9 and branches to err handler if non-zero.
pub const runtime_check = Stencil{
    .name = "runtime_check",
    .code = &(
        // LDRH w9, [x21, #8]
        inst_bytes(0x794012A9) ++
            // CBNZ w9, <err>
            inst_bytes(cbnz_w_placeholder(X9)) ++
            // LDR x0, [x21]
            inst_bytes(0xF94002A0)),
    .holes = &[_]Hole{},
};
pub const runtime_check_branch_offset: usize = 4;

/// C-ABI runtime check: only check retbuf.err, don't overwrite x0.
/// Used after callconv(.c) calls where result is already in x0.
pub const runtime_check_c = Stencil{
    .name = "runtime_check_c",
    .code = &(
        // LDRH w9, [x21, #8]
        inst_bytes(0x794012A9) ++
            // CBNZ w9, <err>
            inst_bytes(cbnz_w_placeholder(X9))),
    .holes = &[_]Hole{},
};
pub const runtime_check_c_branch_offset: usize = 4;

/// Store error tag to ctx.err
pub const store_err = Stencil{
    .name = "store_err",
    .code = &(
        // STRH w9, [x22, #48]
        inst_bytes(0x790062C9)),
    .holes = &[_]Hole{},
};

/// Guard: branch if x0 is not fixnum (bit0 == 0)
pub const guard_fixnum_x0 = Stencil{
    .name = "guard_fixnum_x0",
    .code = &(
        // TST x0, #1
        inst_bytes(0xF240001F) ++
            // B.EQ <target>
            inst_bytes(0x54000000)),
    .holes = &[_]Hole{},
};

/// Guard: branch if x1 is not fixnum (bit0 == 0)
pub const guard_fixnum_x1 = Stencil{
    .name = "guard_fixnum_x1",
    .code = &(
        // TST x1, #1
        inst_bytes(0xF240003F) ++
            // B.EQ <target>
            inst_bytes(0x54000000)),
    .holes = &[_]Hole{},
};

/// Guard: branch if x0 is not a cons (tag bits set)
pub const guard_cons_x0 = Stencil{
    .name = "guard_cons_x0",
    .code = &(
        // MOVZ x10, #0xF
        inst_bytes(movz(X10, 0xF, 0)) ++
            // AND x10, x0, x10
            inst_bytes(and_reg(X10, X0, X10)) ++
            // MOVZ x9, #err
            inst_bytes(movz(X9, ERR_TYPE_MISMATCH, 0)) ++
            // CBNZ x10, <err>
            inst_bytes(cbnz_placeholder(X10))),
    .holes = &[_]Hole{},
};
pub const guard_cons_x0_branch_offset: usize = 12;

/// Peek top of stack into x0 without popping
pub const peek_tos = Stencil{
    .name = "peek_tos",
    .code = &(
        // LDUR x0, [x19, #-8]  — load TOS without moving sp
        // x19 = stack pointer; TOS is at [x19 - 8]
        inst_bytes(0xF85F8260)),
    .holes = &[_]Hole{},
};

/// Guard: branch if x0 has non-zero low 4 bits (not a cons/nil pointer)
/// For check_cons: cons tag is 0, so low 4 bits must be 0 AND not nil
pub const guard_check_cons = Stencil{
    .name = "guard_check_cons",
    .code = &(
        // AND x10, x0, #0xF (extract low 4 bits)
        inst_bytes(and_low4(X10, X0)) ++
            // CBNZ x10, <err> (branch if tag != 0)
            inst_bytes(cbnz_placeholder(X10))),
    .holes = &[_]Hole{},
};
pub const guard_check_cons_branch_offset: usize = 4;

/// Guard: branch if x0 low bits != 4 (vector tag)
pub const guard_check_vector = Stencil{
    .name = "guard_check_vector",
    .code = &(
        // AND x10, x0, #0xF
        inst_bytes(and_low4(X10, X0)) ++
            // CMP x10, #4
            inst_bytes(cmp_imm(X10, 4)) ++
            // B.NE <err>
            inst_bytes(bne_placeholder())),
    .holes = &[_]Hole{},
};
pub const guard_check_vector_branch_offset: usize = 8;

/// Guard: branch if x0 low bits != 2 (symbol tag), also reject nil (raw==0)
pub const guard_check_symbol = Stencil{
    .name = "guard_check_symbol",
    .code = &(
        // AND x10, x0, #0xF
        inst_bytes(and_low4(X10, X0)) ++
            // CMP x10, #2
            inst_bytes(cmp_imm(X10, 2)) ++
            // B.NE <err>
            inst_bytes(bne_placeholder())),
    .holes = &[_]Hole{},
};
pub const guard_check_symbol_branch_offset: usize = 8;

/// Guard: branch if x0 low bits != 6 (string tag)
pub const guard_check_string = Stencil{
    .name = "guard_check_string",
    .code = &(
        // AND x10, x0, #0xF
        inst_bytes(and_low4(X10, X0)) ++
            // CMP x10, #6
            inst_bytes(cmp_imm(X10, 6)) ++
            // B.NE <err>
            inst_bytes(bne_placeholder())),
    .holes = &[_]Hole{},
};
pub const guard_check_string_branch_offset: usize = 8;

/// Guard: branch if x0 low bits != 8 (closure tag)
pub const guard_check_closure = Stencil{
    .name = "guard_check_closure",
    .code = &(
        // AND x10, x0, #0xF
        inst_bytes(and_low4(X10, X0)) ++
            // CMP x10, #8
            inst_bytes(cmp_imm(X10, 8)) ++
            // B.NE <err>
            inst_bytes(bne_placeholder())),
    .holes = &[_]Hole{},
};
pub const guard_check_closure_branch_offset: usize = 8;

/// Guard: branch if x0 is nil (raw == 0)
pub const guard_check_non_nil = Stencil{
    .name = "guard_check_non_nil",
    .code = &(
        // CBZ x0, <err>
        inst_bytes(cbz_placeholder(X0))),
    .holes = &[_]Hole{},
};
pub const guard_check_non_nil_branch_offset: usize = 0;

/// Guard: branch if x0 is not a list (not nil and not cons)
/// List = nil (raw==0) OR cons (tag==0, not nil)
pub const guard_check_list = Stencil{
    .name = "guard_check_list",
    .code = &(
        // CBZ x0, +12 (nil is a list, skip check)
        inst_bytes(cbz_placeholder(X0) | (3 << 5)) ++
            // AND x10, x0, #0xF
            inst_bytes(and_low4(X10, X0)) ++
            // CBNZ x10, <err> (tag != 0 means not cons)
            inst_bytes(cbnz_placeholder(X10))),
    .holes = &[_]Hole{},
};
pub const guard_check_list_branch_offset: usize = 8;

/// Dup: push x0 without popping
pub const dup_stencil = Stencil{
    .name = "dup",
    .code = &(
        // STR x0, [x19], #8
        inst_bytes(0xF8008660)),
    .holes = &[_]Hole{},
};

/// Swap top two values
pub const swap_stencil = Stencil{
    .name = "swap",
    .code = &(
        // LDR x1, [x19, #-8]! (pop top)
        inst_bytes(0xF85F8E61) ++
            // LDR x0, [x19, #-8]! (pop next)
            inst_bytes(0xF85F8E60) ++
            // STR x1, [x19], #8 (push former top)
            inst_bytes(0xF8008661) ++
            // STR x0, [x19], #8 (push former next)
            inst_bytes(0xF8008660)),
    .holes = &[_]Hole{},
};

/// Load local variable (using x23 as frame pointer)
/// Hole: imm32 for offset
pub const load_local = Stencil{
    .name = "load_local",
    .code = &(
        // LDR x0, [x23, #offset] - offset patched
        inst_bytes(0xF94002E0)), // Base LDR with x23
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .imm32, .name = "offset" },
    },
};

/// Load constant from const_pool (x20)
pub const load_const = Stencil{
    .name = "load_const",
    .code = &(
        // LDR x0, [x20, #offset] - offset patched
        inst_bytes(0xF9400280)), // Base LDR with x20
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .imm32, .name = "offset" },
    },
};

/// Store to local variable
pub const store_local = Stencil{
    .name = "store_local",
    .code = &(
        // STR x0, [x23, #offset]
        inst_bytes(0xF90002E0)),
    .holes = &[_]Hole{
        .{ .offset = 0, .hole_type = .imm32, .name = "offset" },
    },
};

/// Logical not: x0 = (x0 == nil) ? t : nil
pub const not_stencil = Stencil{
    .name = "not",
    .code = &(
        // CBZ x0, is_nil  (if nil, result is t)
        // For now simplified: CMP x0, #0; CSET x0, EQ; convert to tagged
        inst_bytes(0xF100001F) ++ // CMP x0, #0
            inst_bytes(0x9A9F17E0) ++ // CSET x0, EQ
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Type check: is fixnum? x0 = (x0 & 1) ? t : nil
pub const fixnump_stencil = Stencil{
    .name = "fixnump",
    .code = &(
        // TST x0, #1
        inst_bytes(0xF240001F) ++
            // CSET x0, NE (non-zero = has bit0 set = fixnum)
            inst_bytes(0x9A9F07E0) ++
            // Convert to tagged
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Type check: is nil? x0 = (x0 == 0) ? t : nil
pub const nilp_stencil = Stencil{
    .name = "nilp",
    .code = &(
        // CMP x0, #0
        inst_bytes(0xF100001F) ++
            // CSET x0, EQ
            inst_bytes(0x9A9F17E0) ++
            // Convert to tagged
            inst_bytes(lsl_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

// ============================================================================
// Specialized (type-proven) stencils — NO runtime type checks
// ============================================================================

/// Specialized fixnum add: x0 = a.raw +% b.raw -% 1
/// Both operands proven fixnum by type system. No guards, no slow path.
/// Input: x0 = a, x1 = b (tagged fixnums)
/// Output: x0 = tagged fixnum result
pub const spec_fixnum_add = Stencil{
    .name = "spec_fixnum_add",
    .code = &(
        // ADD x0, x0, x1
        inst_bytes(add_reg(X0, X0, X1)) ++
            // SUB x0, x0, #1
            inst_bytes(sub_imm(X0, X0, 1))),
    .holes = &[_]Hole{},
};

/// Specialized fixnum sub: x0 = (a.raw -% b.raw) | 1
/// Input: x0 = a, x1 = b (tagged fixnums)
/// Output: x0 = tagged fixnum result
pub const spec_fixnum_sub = Stencil{
    .name = "spec_fixnum_sub",
    .code = &(
        // SUB x0, x0, x1
        inst_bytes(sub_reg(X0, X0, X1)) ++
            // ORR x0, x0, #1
            inst_bytes(orr_imm_bit0(X0, X0))),
    .holes = &[_]Hole{},
};

/// Specialized fixnum mul: x0 = ((a>>1) *% (b>>1)) << 1 | 1
/// Input: x0 = a, x1 = b (tagged fixnums)
/// Output: x0 = tagged fixnum result
pub const spec_fixnum_mul = Stencil{
    .name = "spec_fixnum_mul",
    .code = &(
        // ASR x9, x0, #1   (unbox a)
        inst_bytes(asr_imm(X9, X0, 1)) ++
            // ASR x10, x1, #1   (unbox b)
            inst_bytes(asr_imm(X10, X1, 1)) ++
            // MUL x0, x9, x10
            inst_bytes(mul_reg(X0, X9, X10)) ++
            // LSL x0, x0, #1
            inst_bytes(lsl_imm(X0, X0, 1)) ++
            // ORR x0, x0, #1
            inst_bytes(orr_imm_bit0(X0, X0))),
    .holes = &[_]Hole{},
};

/// Specialized unsafe car: x0 = cons.car (no nil check, no type check)
/// Input: x0 = tagged cons pointer
/// Output: x0 = car value
pub const spec_unsafe_car = Stencil{
    .name = "spec_unsafe_car",
    .code = &(
        // AND x0, x0, PTR_MASK (~0xF) — clear low 4 tag bits
        // BIC x0, x0, #0xF = AND x0, x0, #~0xF
        // N=1, immr=0, imms=59 encodes 0xFFFFFFFFFFFFFFF0
        inst_bytes(0x927CEC00) ++
            // LDR x0, [x0]     (car at offset 0)
            inst_bytes(0xF9400000)),
    .holes = &[_]Hole{},
};

/// Specialized unsafe cdr: x0 = cons.cdr (no nil check, no type check)
/// Input: x0 = tagged cons pointer
/// Output: x0 = cdr value
pub const spec_unsafe_cdr = Stencil{
    .name = "spec_unsafe_cdr",
    .code = &(
        // AND x0, x0, #~0xF — clear low 4 tag bits
        inst_bytes(0x927CEC00) ++
            // LDR x0, [x0, #8]  (cdr at offset 8)
            inst_bytes(0xF9400400)),
    .holes = &[_]Hole{},
};

/// Specialized direct aref: x0 = vec.data[idx]
/// Input: x0 = tagged vector pointer, x1 = tagged fixnum index
/// Output: x0 = element value
/// Vector layout: length(0), capacity(8), data ptr(16), fill_pointer(24)
pub const spec_direct_aref = Stencil{
    .name = "spec_direct_aref",
    .code = &(
        // AND x0, x0, #~0xF — clear tag bits from vector
        inst_bytes(0x927CEC00) ++
            // LDR x0, [x0, #16]  — load data pointer (offset 16)
            inst_bytes(0xF9400800) ++
            // ASR x1, x1, #1     — unbox fixnum index
            inst_bytes(asr_imm(X1, X1, 1)) ++
            // LDR x0, [x0, x1, LSL #3]  — load data[idx] (each Value is 8 bytes)
            inst_bytes(0xF8617800)),
    .holes = &[_]Hole{},
};

// ============================================================================
// Tests
// ============================================================================

test "stencil sizes" {
    const testing = std.testing;

    // load_imm64 should be 16 bytes (4 instructions)
    try testing.expectEqual(@as(usize, 16), load_imm64.code.len);

    // Arithmetic stencils (5 instructions each)
    try testing.expectEqual(@as(usize, 12), add_fixnum.code.len);
    try testing.expectEqual(@as(usize, 12), sub_fixnum.code.len);
    try testing.expectEqual(@as(usize, 16), mul_fixnum.code.len);
    try testing.expectEqual(@as(usize, 8), neg_fixnum.code.len);

    // Simple stencils (1 instruction)
    try testing.expectEqual(@as(usize, 4), ret_stencil.code.len);
    try testing.expectEqual(@as(usize, 4), push_nil_stencil.code.len);
    try testing.expectEqual(@as(usize, 4), push_t_stencil.code.len);
    try testing.expectEqual(@as(usize, 16), stack_push.code.len);
    try testing.expectEqual(@as(usize, 16), stack_push_x1.code.len);
    try testing.expectEqual(@as(usize, 16), stack_pop.code.len);
    try testing.expectEqual(@as(usize, 16), swap_stencil.code.len);
    try testing.expectEqual(@as(usize, 48), prologue_stencil.code.len);
    try testing.expectEqual(@as(usize, 20), epilogue_stencil.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x1_x0.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x2_x0.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x2_x1.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x3_x1.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x0_x22.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x1_x22.code.len);
    try testing.expectEqual(@as(usize, 4), store_ctx_sp.code.len);
    try testing.expectEqual(@as(usize, 8), load_ctx_sp.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x0_x21.code.len);
    try testing.expectEqual(@as(usize, 4), load_err_trace.code.len);
    try testing.expectEqual(@as(usize, 4), mov_x8_x21.code.len);
    try testing.expectEqual(@as(usize, 4), clear_retbuf_err.code.len);
    try testing.expectEqual(@as(usize, 8), guard_fixnum_x0.code.len);
    try testing.expectEqual(@as(usize, 8), guard_fixnum_x1.code.len);
    try testing.expectEqual(@as(usize, 16), guard_cons_x0.code.len);
    try testing.expectEqual(@as(usize, 4), load_local.code.len);
    try testing.expectEqual(@as(usize, 4), load_const.code.len);

    // Comparison stencils (3 instructions each)
    try testing.expectEqual(@as(usize, 12), eq_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), lt_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), gt_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), le_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), ge_stencil.code.len);

    // Type check stencils (3 instructions each)
    try testing.expectEqual(@as(usize, 12), not_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), nilp_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), fixnump_stencil.code.len);
    try testing.expectEqual(@as(usize, 12), runtime_check.code.len);
    try testing.expectEqual(@as(usize, 8), runtime_check_c.code.len);
    try testing.expectEqual(@as(usize, 4), store_err.code.len);
    try testing.expectEqual(@as(usize, 12), mul_overflow_check.code.len);
    try testing.expectEqual(@as(usize, 36), fixnum_range_check.code.len);

    // Call stencils
    try testing.expectEqual(@as(usize, 20), call_abs.code.len);

    // Specialized stencils (type-proven, no guards)
    try testing.expectEqual(@as(usize, 8), spec_fixnum_add.code.len);
    try testing.expectEqual(@as(usize, 8), spec_fixnum_sub.code.len);
    try testing.expectEqual(@as(usize, 20), spec_fixnum_mul.code.len);
    try testing.expectEqual(@as(usize, 8), spec_unsafe_car.code.len);
    try testing.expectEqual(@as(usize, 8), spec_unsafe_cdr.code.len);
    try testing.expectEqual(@as(usize, 16), spec_direct_aref.code.len);
}

test "instruction encoding" {
    const testing = std.testing;

    // MOVZ X0, #0 should encode to 0xD2800000
    try testing.expectEqual(@as(u32, 0xD2800000), movz(0, 0, 0));

    // push_t should encode MOV X0, #2
    try testing.expectEqualSlices(u8, &inst_bytes(0xD2800040), push_t_stencil.code);

    // RET should encode to 0xD65F03C0
    try testing.expectEqual(@as(u32, 0xD65F03C0), ret());

    // ADD X0, X1, X2 should encode to 0x8B020020
    try testing.expectEqual(@as(u32, 0x8B020020), add_reg(0, 1, 2));
}
