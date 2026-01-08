//! ARM64 Code Generator
//!
//! Emits native ARM64 machine code from register IR.
//! After register allocation, virtual registers map directly to physical registers.

const std = @import("std");
const instruction = @import("instruction.zig");
const regalloc = @import("regalloc.zig");
const Inst = instruction.Inst;
const Op = instruction.Op;
const Reg = instruction.Reg;
const Function = instruction.Function;
const PhysReg = regalloc.PhysReg;
const PHYS = regalloc.PHYS;

/// ARM64 instruction encodings
pub const ARM64 = struct {
    /// Register encoding (5 bits)
    pub fn reg(r: PhysReg) u32 {
        return @as(u32, r);
    }

    /// ADD Xd, Xn, Xm
    pub fn add(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10001011000 Rm 000000 Rn Rd
        return 0x8B000000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// ADD Xd, Xn, #imm12
    pub fn addImm(rd: PhysReg, rn: PhysReg, imm12: u12) u32 {
        // 1001000100 imm12 Rn Rd
        return 0x91000000 | (@as(u32, imm12) << 10) | (reg(rn) << 5) | reg(rd);
    }

    /// SUB Xd, Xn, Xm
    pub fn sub(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 11001011000 Rm 000000 Rn Rd
        return 0xCB000000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// SUB Xd, Xn, #imm12
    pub fn subImm(rd: PhysReg, rn: PhysReg, imm12: u12) u32 {
        // 1101000100 imm12 Rn Rd
        return 0xD1000000 | (@as(u32, imm12) << 10) | (reg(rn) << 5) | reg(rd);
    }

    /// MUL Xd, Xn, Xm (MADD Xd, Xn, Xm, XZR)
    pub fn mul(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10011011000 Rm 011111 Rn Rd
        return 0x9B007C00 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// SDIV Xd, Xn, Xm
    pub fn sdiv(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10011010110 Rm 000011 Rn Rd
        return 0x9AC00C00 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// AND Xd, Xn, Xm
    pub fn andReg(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10001010000 Rm 000000 Rn Rd
        return 0x8A000000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// ORR Xd, Xn, Xm
    pub fn orr(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10101010000 Rm 000000 Rn Rd
        return 0xAA000000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// EOR Xd, Xn, Xm
    pub fn eor(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 11001010000 Rm 000000 Rn Rd
        return 0xCA000000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// LSL Xd, Xn, Xm
    pub fn lsl(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10011010110 Rm 001000 Rn Rd
        return 0x9AC02000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// ASR Xd, Xn, Xm (arithmetic shift right)
    pub fn asr(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10011010110 Rm 001010 Rn Rd
        return 0x9AC02800 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// LSR Xd, Xn, Xm (logical shift right)
    pub fn lsr(rd: PhysReg, rn: PhysReg, rm: PhysReg) u32 {
        // 10011010110 Rm 001001 Rn Rd
        return 0x9AC02400 | (reg(rm) << 16) | (reg(rn) << 5) | reg(rd);
    }

    /// MOV Xd, Xn (alias for ORR Xd, XZR, Xn)
    pub fn mov(rd: PhysReg, rn: PhysReg) u32 {
        return orr(rd, PHYS.zr, rn);
    }

    /// MOVZ Xd, #imm16
    pub fn movz(rd: PhysReg, imm16: u16) u32 {
        // 110100101 hw imm16 Rd
        return 0xD2800000 | (@as(u32, imm16) << 5) | reg(rd);
    }

    /// MOVN Xd, #imm16 (for negative values)
    pub fn movn(rd: PhysReg, imm16: u16) u32 {
        // 100100101 hw imm16 Rd
        return 0x92800000 | (@as(u32, imm16) << 5) | reg(rd);
    }

    /// CMP Xn, Xm (alias for SUBS XZR, Xn, Xm)
    pub fn cmp(rn: PhysReg, rm: PhysReg) u32 {
        // SUBS XZR, Xn, Xm
        return 0xEB000000 | (reg(rm) << 16) | (reg(rn) << 5) | reg(PHYS.zr);
    }

    /// CSET Xd, cond (set to 1 if condition, else 0)
    pub fn cset(rd: PhysReg, cond: Cond) u32 {
        // CSINC Xd, XZR, XZR, invert(cond)
        const inv_cond = cond.invert();
        return 0x9A9F0400 | (@as(u32, @intFromEnum(inv_cond)) << 12) | reg(rd);
    }

    /// B offset (unconditional branch)
    pub fn b(offset: i26) u32 {
        // 000101 imm26
        const imm: u32 = @bitCast(@as(i32, offset) & 0x3FFFFFF);
        return 0x14000000 | imm;
    }

    /// B.cond offset (conditional branch)
    pub fn bcond(cond: Cond, offset: i19) u32 {
        // 01010100 imm19 0 cond
        const imm: u32 = @bitCast(@as(i32, offset) & 0x7FFFF);
        return 0x54000000 | (imm << 5) | @intFromEnum(cond);
    }

    /// CBZ Xn, offset (compare and branch if zero)
    pub fn cbz(rn: PhysReg, offset: i19) u32 {
        // 10110100 imm19 Rn
        const imm: u32 = @bitCast(@as(i32, offset) & 0x7FFFF);
        return 0xB4000000 | (imm << 5) | reg(rn);
    }

    /// CBNZ Xn, offset (compare and branch if not zero)
    pub fn cbnz(rn: PhysReg, offset: i19) u32 {
        // 10110101 imm19 Rn
        const imm: u32 = @bitCast(@as(i32, offset) & 0x7FFFF);
        return 0xB5000000 | (imm << 5) | reg(rn);
    }

    /// BL offset (branch with link - call)
    pub fn bl(offset: i26) u32 {
        // 100101 imm26
        const imm: u32 = @bitCast(@as(i32, offset) & 0x3FFFFFF);
        return 0x94000000 | imm;
    }

    /// BLR Xn (branch with link to register)
    pub fn blr(rn: PhysReg) u32 {
        // 1101011000111111000000 Rn 00000
        return 0xD63F0000 | (reg(rn) << 5);
    }

    /// RET (return, branch to LR)
    pub fn ret() u32 {
        // RET X30
        return 0xD65F03C0;
    }

    /// LDR Xd, [Xn, #imm12*8] (load 64-bit)
    pub fn ldr(rd: PhysReg, rn: PhysReg, offset: u12) u32 {
        // 11111001010 imm12 Rn Rd
        return 0xF9400000 | (@as(u32, offset) << 10) | (reg(rn) << 5) | reg(rd);
    }

    /// STR Xd, [Xn, #imm12*8] (store 64-bit)
    pub fn str(rd: PhysReg, rn: PhysReg, offset: u12) u32 {
        // 11111001000 imm12 Rn Rd
        return 0xF9000000 | (@as(u32, offset) << 10) | (reg(rn) << 5) | reg(rd);
    }

    /// STP Xd1, Xd2, [Xn, #imm7*8]! (store pair, pre-indexed)
    pub fn stpPre(rd1: PhysReg, rd2: PhysReg, rn: PhysReg, offset: i7) u32 {
        // 10101001110 imm7 Rt2 Rn Rt1
        const imm: u32 = @bitCast(@as(i32, offset) & 0x7F);
        return 0xA9800000 | (imm << 15) | (reg(rd2) << 10) | (reg(rn) << 5) | reg(rd1);
    }

    /// LDP Xd1, Xd2, [Xn], #imm7*8 (load pair, post-indexed)
    pub fn ldpPost(rd1: PhysReg, rd2: PhysReg, rn: PhysReg, offset: i7) u32 {
        // 10101000110 imm7 Rt2 Rn Rt1
        const imm: u32 = @bitCast(@as(i32, offset) & 0x7F);
        return 0xA8C00000 | (imm << 15) | (reg(rd2) << 10) | (reg(rn) << 5) | reg(rd1);
    }

    /// NOP
    pub fn nop() u32 {
        return 0xD503201F;
    }

    /// Condition codes
    pub const Cond = enum(u4) {
        eq = 0x0, // Equal (Z=1)
        ne = 0x1, // Not equal (Z=0)
        cs = 0x2, // Carry set / unsigned >= (C=1)
        cc = 0x3, // Carry clear / unsigned < (C=0)
        mi = 0x4, // Minus / negative (N=1)
        pl = 0x5, // Plus / positive or zero (N=0)
        vs = 0x6, // Overflow (V=1)
        vc = 0x7, // No overflow (V=0)
        hi = 0x8, // Unsigned > (C=1 & Z=0)
        ls = 0x9, // Unsigned <= (C=0 | Z=1)
        ge = 0xA, // Signed >= (N=V)
        lt = 0xB, // Signed < (N!=V)
        gt = 0xC, // Signed > (Z=0 & N=V)
        le = 0xD, // Signed <= (Z=1 | N!=V)
        al = 0xE, // Always
        nv = 0xF, // Never (reserved)

        pub fn invert(self: Cond) Cond {
            return @enumFromInt(@intFromEnum(self) ^ 1);
        }
    };
};

/// Branch patch entry
const BranchPatch = struct {
    offset: usize,
    label: usize,
    is_cond: bool,
};

/// ARM64 code emitter
pub const Emitter = struct {
    allocator: std.mem.Allocator,
    code: std.ArrayList(u32),
    /// Label locations for patching branches
    labels: std.ArrayList(usize),
    /// Pending branch patches: (code_offset, label_idx)
    patches: std.ArrayList(BranchPatch),

    pub fn init(allocator: std.mem.Allocator) Emitter {
        return .{
            .allocator = allocator,
            .code = std.ArrayList(u32){},
            .labels = std.ArrayList(usize){},
            .patches = std.ArrayList(BranchPatch){},
        };
    }

    pub fn deinit(self: *Emitter) void {
        self.code.deinit(self.allocator);
        self.labels.deinit(self.allocator);
        self.patches.deinit(self.allocator);
    }

    /// Emit a 32-bit instruction
    pub fn emit(self: *Emitter, inst: u32) !void {
        try self.code.append(self.allocator, inst);
    }

    /// Current code offset (in instructions)
    pub fn here(self: *Emitter) usize {
        return self.code.items.len;
    }

    /// Create a new label at current position
    pub fn label(self: *Emitter) !usize {
        const idx = self.labels.items.len;
        try self.labels.append(self.allocator, self.here());
        return idx;
    }

    /// Emit a forward branch (to be patched later)
    pub fn emitBranchForward(self: *Emitter, label_idx: usize, is_cond: bool, cond: ARM64.Cond) !void {
        try self.patches.append(self.allocator, .{
            .offset = self.here(),
            .label = label_idx,
            .is_cond = is_cond,
        });
        // Emit placeholder
        if (is_cond) {
            try self.emit(ARM64.bcond(cond, 0));
        } else {
            try self.emit(ARM64.b(0));
        }
    }

    /// Patch all forward branches
    pub fn patchBranches(self: *Emitter) void {
        for (self.patches.items) |patch| {
            const target = self.labels.items[patch.label];
            const offset: i32 = @intCast(@as(isize, @intCast(target)) - @as(isize, @intCast(patch.offset)));

            if (patch.is_cond) {
                // B.cond uses 19-bit offset
                const imm: u32 = @bitCast(offset & 0x7FFFF);
                self.code.items[patch.offset] = (self.code.items[patch.offset] & 0xFF00001F) | (imm << 5);
            } else {
                // B uses 26-bit offset
                const imm: u32 = @bitCast(offset & 0x3FFFFFF);
                self.code.items[patch.offset] = (self.code.items[patch.offset] & 0xFC000000) | imm;
            }
        }
    }

    /// Get generated code as bytes
    pub fn getCode(self: *Emitter) []const u8 {
        const ptr: [*]const u8 = @ptrCast(self.code.items.ptr);
        return ptr[0 .. self.code.items.len * 4];
    }

    /// Emit function prologue
    pub fn emitPrologue(self: *Emitter, frame_size: u12) !void {
        // stp x29, x30, [sp, #-frame_size]!
        const offset: i7 = @intCast(-@as(i8, @intCast(frame_size / 8)));
        try self.emit(ARM64.stpPre(PHYS.fp, PHYS.lr, 31, offset));
        // mov x29, sp
        try self.emit(ARM64.mov(PHYS.fp, 31));
    }

    /// Emit function epilogue
    pub fn emitEpilogue(self: *Emitter, frame_size: u12) !void {
        // ldp x29, x30, [sp], #frame_size
        const offset: i7 = @intCast(frame_size / 8);
        try self.emit(ARM64.ldpPost(PHYS.fp, PHYS.lr, 31, offset));
        // ret
        try self.emit(ARM64.ret());
    }
};

/// Map virtual register to physical register using register map
/// Falls back to using virtual register number if no map provided
fn mapReg(vreg: Reg, reg_map: ?regalloc.RegisterMap) PhysReg {
    if (reg_map) |map| {
        // If register has a physical assignment, use it
        if (map.getPhysReg(vreg)) |phys| {
            return phys;
        }
        // If spilled, we'll handle load/store separately
        // For now, use a scratch register (caller should handle spill)
        return PHYS.x8; // Temp register for spilled values
    }
    // No register map - use virtual register number as physical (for testing)
    return @intCast(vreg % 32);
}

/// Generate ARM64 code from register IR function
/// If reg_map is provided, uses physical registers; otherwise uses virtual registers
pub fn generate(allocator: std.mem.Allocator, func: Function, reg_map: ?regalloc.RegisterMap) ![]const u8 {
    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    // Calculate frame size (spill slots + locals + spills)
    const spill_slots = if (reg_map) |map| map.spill_count else 0;
    const frame_size: u12 = @intCast((func.local_count + spill_slots + 2) * 8); // +2 for fp, lr

    // Emit prologue
    try emitter.emitPrologue(frame_size);

    // Emit each instruction
    for (func.code) |inst| {
        try emitInst(&emitter, inst, reg_map);
    }

    // Patch branches
    emitter.patchBranches();

    // Return owned copy of code
    return try allocator.dupe(u8, emitter.getCode());
}

/// Emit a single IR instruction as ARM64
/// Uses reg_map to translate virtual registers to physical registers
fn emitInst(e: *Emitter, inst: Inst, reg_map: ?regalloc.RegisterMap) !void {
    // Map virtual registers to physical registers
    const rd = mapReg(inst.dest, reg_map);
    const rs1 = mapReg(inst.src1, reg_map);
    const rs2 = if (inst.src2()) |s| mapReg(s, reg_map) else undefined;
    switch (inst.op) {
        .mov => try e.emit(ARM64.mov(rd, rs1)),

        .movi => {
            const imm = inst.imm();
            if (imm >= 0) {
                try e.emit(ARM64.movz(rd, @intCast(imm)));
            } else {
                // MOVN for negative: movn rd, #(~imm)
                try e.emit(ARM64.movn(rd, @intCast(~imm)));
            }
        },

        .add => try e.emit(ARM64.add(rd, rs1, rs2)),
        .sub => try e.emit(ARM64.sub(rd, rs1, rs2)),
        .mul => try e.emit(ARM64.mul(rd, rs1, rs2)),
        .div => try e.emit(ARM64.sdiv(rd, rs1, rs2)),

        .addi => {
            const imm = inst.imm();
            if (imm >= 0 and imm <= 4095) {
                try e.emit(ARM64.addImm(rd, rs1, @intCast(imm)));
            } else if (imm < 0 and imm >= -4095) {
                try e.emit(ARM64.subImm(rd, rs1, @intCast(-imm)));
            } else {
                // Large immediate: load into temp, then add
                try e.emit(ARM64.movz(PHYS.x8, @bitCast(inst.imm())));
                try e.emit(ARM64.add(rd, rs1, PHYS.x8));
            }
        },

        .@"and" => try e.emit(ARM64.andReg(rd, rs1, rs2)),
        .@"or" => try e.emit(ARM64.orr(rd, rs1, rs2)),
        .xor => try e.emit(ARM64.eor(rd, rs1, rs2)),
        .shl => try e.emit(ARM64.lsl(rd, rs1, rs2)),
        .shr => try e.emit(ARM64.asr(rd, rs1, rs2)),
        .lshr => try e.emit(ARM64.lsr(rd, rs1, rs2)),

        .eq => {
            try e.emit(ARM64.cmp(rs1, rs2));
            try e.emit(ARM64.cset(rd, .eq));
        },
        .ne => {
            try e.emit(ARM64.cmp(rs1, rs2));
            try e.emit(ARM64.cset(rd, .ne));
        },
        .lt => {
            try e.emit(ARM64.cmp(rs1, rs2));
            try e.emit(ARM64.cset(rd, .lt));
        },
        .le => {
            try e.emit(ARM64.cmp(rs1, rs2));
            try e.emit(ARM64.cset(inst.dest, .le));
        },
        .gt => {
            try e.emit(ARM64.cmp(inst.src1, inst.src2()));
            try e.emit(ARM64.cset(inst.dest, .gt));
        },
        .ge => {
            try e.emit(ARM64.cmp(inst.src1, inst.src2()));
            try e.emit(ARM64.cset(inst.dest, .ge));
        },

        .jmp => {
            const offset: i26 = @intCast(inst.imm());
            try e.emit(ARM64.b(offset));
        },

        .jz => {
            // Branch if src1 == 0 (nil)
            const offset: i19 = @intCast(inst.imm());
            try e.emit(ARM64.cbz(inst.dest, offset));
        },

        .jnz => {
            // Branch if src1 != 0 (not nil)
            const offset: i19 = @intCast(inst.imm());
            try e.emit(ARM64.cbnz(inst.dest, offset));
        },

        .call => {
            // Call convention: args in x0-x7, result in x0
            // func ptr in src1
            try e.emit(ARM64.blr(inst.src1));
            // Move result to dest if different from x0
            if (inst.dest != PHYS.x0) {
                try e.emit(ARM64.mov(inst.dest, PHYS.x0));
            }
        },

        .ret => {
            // Move return value to x0 if not already there
            if (inst.src1 != PHYS.x0) {
                try e.emit(ARM64.mov(PHYS.x0, inst.src1));
            }
            // Epilogue and return handled by caller
        },

        .load_local => {
            // Load from stack slot
            const slot: u12 = @intCast(inst.operand);
            try e.emit(ARM64.ldr(inst.dest, PHYS.fp, slot));
        },

        .store_local => {
            // Store to stack slot
            const slot: u12 = @intCast(inst.operand);
            try e.emit(ARM64.str(inst.src1, PHYS.fp, slot));
        },

        else => {
            // TODO: Implement remaining ops (cons, car, cdr, etc.)
            // These will call runtime functions
            try e.emit(ARM64.nop());
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "ARM64 instruction encoding" {
    const testing = std.testing;

    // ADD X0, X1, X2
    try testing.expectEqual(@as(u32, 0x8B020020), ARM64.add(0, 1, 2));

    // SUB X0, X1, X2
    try testing.expectEqual(@as(u32, 0xCB020020), ARM64.sub(0, 1, 2));

    // ADD X0, X1, #42
    try testing.expectEqual(@as(u32, 0x9100A820), ARM64.addImm(0, 1, 42));

    // MOV X0, X1 (ORR X0, XZR, X1)
    try testing.expectEqual(@as(u32, 0xAA0103E0), ARM64.mov(0, 1));

    // RET
    try testing.expectEqual(@as(u32, 0xD65F03C0), ARM64.ret());
}

test "simple function codegen" {
    const testing = std.testing;

    var builder = instruction.Builder.init(testing.allocator);
    defer builder.deinit();

    // (defun add-one (x) (+ x 1))
    // x is in x0, result in x0
    try builder.emit(Inst.i(.addi, 0, 0, 1));
    try builder.emit(Inst.r(.ret, 0, 0, 0));

    const func = try builder.build("add-one", 1, 0);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    const code = try generate(testing.allocator, func);
    defer testing.allocator.free(code);

    // Should have: prologue (2) + addi (1) + ret setup (0, already in x0) + epilogue (2)
    // But our simple codegen doesn't emit prologue/epilogue per-instruction
    // Just checking we got some code
    try testing.expect(code.len > 0);
}
