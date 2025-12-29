//! Register-based IR for Habu
//!
//! ARM64-inspired design with fixed 32-bit instructions:
//! - 32 virtual registers (v0-v31)
//! - ~50 orthogonal opcodes
//! - Direct mapping to ARM64 x0-x30
//!
//! Instruction format (32 bits):
//! +--------+-------+-------+------------------+
//! | opcode | dest  | src1  | src2/immediate   |
//! | 6 bits | 5 bits| 5 bits| 16 bits          |
//! +--------+-------+-------+------------------+

const std = @import("std");

/// Virtual register (0-31)
pub const Reg = u5;

/// Special registers
pub const REG = struct {
    /// Zero register - reads as 0, writes discarded (like ARM64 xzr)
    pub const zero: Reg = 31;
    /// Frame pointer
    pub const fp: Reg = 29;
    /// Link register (return address)
    pub const lr: Reg = 30;
    /// Argument registers
    pub const a0: Reg = 0;
    pub const a1: Reg = 1;
    pub const a2: Reg = 2;
    pub const a3: Reg = 3;
    pub const a4: Reg = 4;
    pub const a5: Reg = 5;
    pub const a6: Reg = 6;
    pub const a7: Reg = 7;
    /// Return value register
    pub const ret: Reg = 0;
    /// Callee-saved registers
    pub const s0: Reg = 19;
    pub const s1: Reg = 20;
    pub const s2: Reg = 21;
    pub const s3: Reg = 22;
    pub const s4: Reg = 23;
    pub const s5: Reg = 24;
    pub const s6: Reg = 25;
    pub const s7: Reg = 26;
    /// Temporary registers (caller-saved)
    pub const t0: Reg = 8;
    pub const t1: Reg = 9;
    pub const t2: Reg = 10;
    pub const t3: Reg = 11;
    pub const t4: Reg = 12;
    pub const t5: Reg = 13;
    pub const t6: Reg = 14;
    pub const t7: Reg = 15;
};

/// Opcode (6 bits = 64 possible)
pub const Op = enum(u6) {
    // ========================================================================
    // Data Movement (0x00-0x07)
    // ========================================================================

    /// dest = src1
    mov = 0x00,
    /// dest = immediate (16-bit signed, extended to 64-bit)
    movi = 0x01,
    /// dest = immediate << 16 (for building large constants)
    movk = 0x02,
    /// dest = locals[imm]
    load_local = 0x03,
    /// locals[imm] = src1
    store_local = 0x04,
    /// dest = upvalues[imm]
    load_upval = 0x05,
    /// upvalues[imm] = src1
    store_upval = 0x06,
    /// dest = globals[imm] (constant pool index)
    load_global = 0x07,

    // ========================================================================
    // Arithmetic (0x08-0x0F) - polymorphic: fixnum or float
    // ========================================================================

    /// dest = src1 + src2
    add = 0x08,
    /// dest = src1 - src2
    sub = 0x09,
    /// dest = src1 * src2
    mul = 0x0A,
    /// dest = src1 / src2
    div = 0x0B,
    /// dest = src1 % src2
    mod = 0x0C,
    /// dest = -src1
    neg = 0x0D,
    /// dest = src1 + imm16
    addi = 0x0E,

    // ========================================================================
    // Comparison (0x10-0x17) - result is t or nil
    // ========================================================================

    /// dest = (src1 == src2) ? t : nil  (identity/eq)
    eq = 0x10,
    /// dest = (src1 != src2) ? t : nil
    ne = 0x11,
    /// dest = (src1 < src2) ? t : nil
    lt = 0x12,
    /// dest = (src1 <= src2) ? t : nil
    le = 0x13,
    /// dest = (src1 > src2) ? t : nil
    gt = 0x14,
    /// dest = (src1 >= src2) ? t : nil
    ge = 0x15,
    /// dest = (eql src1 src2) ? t : nil
    eql = 0x16,
    /// dest = (equal src1 src2) ? t : nil
    equal = 0x17,

    // ========================================================================
    // Logical / Bitwise (0x18-0x1F)
    // ========================================================================

    /// dest = src1 & src2
    @"and" = 0x18,
    /// dest = src1 | src2
    @"or" = 0x19,
    /// dest = src1 ^ src2
    xor = 0x1A,
    /// dest = ~src1
    not = 0x1B,
    /// dest = src1 << src2
    shl = 0x1C,
    /// dest = src1 >> src2 (arithmetic)
    shr = 0x1D,
    /// dest = src1 >> src2 (logical)
    lshr = 0x1E,

    // ========================================================================
    // Cons Operations (0x20-0x27)
    // ========================================================================

    /// dest = cons(src1, src2)
    cons = 0x20,
    /// dest = car(src1)
    car = 0x21,
    /// dest = cdr(src1)
    cdr = 0x22,
    /// (rplaca src1 src2), dest = src1
    rplaca = 0x23,
    /// (rplacd src1 src2), dest = src1
    rplacd = 0x24,
    /// dest = (consp src1) ? t : nil
    consp = 0x25,
    /// dest = (null src1) ? t : nil (nil check)
    nullp = 0x26,
    /// dest = (atom src1) ? t : nil
    atomp = 0x27,

    // ========================================================================
    // Vector/String Operations (0x28-0x2F)
    // ========================================================================

    /// dest = (make-vector src1 src2)  ; length, init
    mkvec = 0x28,
    /// dest = (svref src1 src2)
    vref = 0x29,
    /// (svset src1 src2 dest)  ; vec, idx, val -> val
    vset = 0x2A,
    /// dest = (length src1)
    len = 0x2B,
    /// dest = (make-string src1 src2)  ; length, char
    mkstr = 0x2C,
    /// dest = (sref src1 src2)
    sref = 0x2D,
    /// (sset src1 src2 dest)  ; str, idx, char
    sset = 0x2E,

    // ========================================================================
    // Control Flow (0x30-0x37)
    // ========================================================================

    /// Unconditional jump: PC += offset (offset in imm16, signed)
    jmp = 0x30,
    /// Jump if src1 is nil: PC += offset
    jz = 0x31,
    /// Jump if src1 is not nil: PC += offset
    jnz = 0x32,
    /// Call: dest = src1(args...), imm8 = argc
    call = 0x33,
    /// Tail call: goto src1(args...)
    tailcall = 0x34,
    /// Return src1
    ret = 0x35,
    /// Call native/FFI: dest = native[imm](args...), src1 = argc
    callz = 0x36,

    // ========================================================================
    // Type Operations (0x38-0x3B)
    // ========================================================================

    /// dest = type-of(src1) as symbol
    typeof = 0x38,
    /// Assert src1 is type imm, else error
    checktype = 0x39,
    /// dest = (symbolp src1) ? t : nil
    symbolp = 0x3A,
    /// dest = (numberp src1) ? t : nil
    numberp = 0x3B,

    // ========================================================================
    // Closure/Lambda (0x3C-0x3F)
    // ========================================================================

    /// dest = make-closure(code[imm], captures...)
    closure = 0x3C,
    /// dest = funcall(src1, args...), inline apply
    funcall = 0x3D,
    /// dest = apply(src1, src2)  ; func, arglist
    apply = 0x3E,
};

/// Instruction (32 bits, packed)
pub const Inst = packed struct(u32) {
    /// Operand 2 or 16-bit immediate
    operand: u16,
    /// Source register 1
    src1: Reg,
    /// Destination register
    dest: Reg,
    /// Operation
    op: Op,

    /// Create R-type instruction (register-register)
    pub fn r(op_: Op, dest_: Reg, src1_: Reg, src2_: Reg) Inst {
        return .{
            .op = op_,
            .dest = dest_,
            .src1 = src1_,
            .operand = @as(u16, src2_),
        };
    }

    /// Create I-type instruction (register-immediate)
    pub fn i(op_: Op, dest_: Reg, src1_: Reg, imm_: i16) Inst {
        return .{
            .op = op_,
            .dest = dest_,
            .src1 = src1_,
            .operand = @bitCast(imm_),
        };
    }

    /// Create I-type instruction with unsigned immediate
    pub fn iu(op_: Op, dest_: Reg, src1_: Reg, imm_: u16) Inst {
        return .{
            .op = op_,
            .dest = dest_,
            .src1 = src1_,
            .operand = imm_,
        };
    }

    /// Create branch instruction
    pub fn br(op_: Op, cond: Reg, offset: i16) Inst {
        return .{
            .op = op_,
            .dest = cond,
            .src1 = 0,
            .operand = @bitCast(offset),
        };
    }

    /// Get src2 as register (low 5 bits of operand)
    pub fn src2(self: Inst) Reg {
        return @truncate(self.operand);
    }

    /// Get operand as signed immediate
    pub fn imm(self: Inst) i16 {
        return @bitCast(self.operand);
    }

    /// Format instruction for debugging
    pub fn format(self: Inst, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        const op_name = @tagName(self.op);
        switch (self.op) {
            // R-type: dest = src1 op src2
            .add, .sub, .mul, .div, .mod, .eq, .ne, .lt, .le, .gt, .ge, .eql, .equal, .@"and", .@"or", .xor, .shl, .shr, .lshr, .cons, .rplaca, .rplacd => {
                try writer.print("{s} v{}, v{}, v{}", .{ op_name, self.dest, self.src1, self.src2() });
            },
            // Unary: dest = op src1
            .mov, .neg, .not, .car, .cdr, .consp, .nullp, .atomp, .len, .typeof, .symbolp, .numberp => {
                try writer.print("{s} v{}, v{}", .{ op_name, self.dest, self.src1 });
            },
            // Immediate: dest = src1 op imm
            .addi => {
                try writer.print("{s} v{}, v{}, #{}", .{ op_name, self.dest, self.src1, self.imm() });
            },
            // Load immediate
            .movi, .movk => {
                try writer.print("{s} v{}, #{}", .{ op_name, self.dest, self.imm() });
            },
            // Memory: dest = mem[imm] or mem[imm] = src1
            .load_local, .load_upval, .load_global => {
                try writer.print("{s} v{}, [{}]", .{ op_name, self.dest, self.operand });
            },
            .store_local, .store_upval => {
                try writer.print("{s} [{}], v{}", .{ op_name, self.operand, self.src1 });
            },
            // Branch
            .jmp => {
                try writer.print("{s} {}", .{ op_name, self.imm() });
            },
            .jz, .jnz => {
                try writer.print("{s} v{}, {}", .{ op_name, self.dest, self.imm() });
            },
            // Call
            .call => {
                try writer.print("{s} v{}, v{}, argc={}", .{ op_name, self.dest, self.src1, self.operand & 0xFF });
            },
            .ret => {
                try writer.print("{s} v{}", .{ op_name, self.src1 });
            },
            else => {
                try writer.print("{s} v{}, v{}, {}", .{ op_name, self.dest, self.src1, self.operand });
            },
        }
    }
};

/// Function in register IR
pub const Function = struct {
    /// Function name (for debugging)
    name: []const u8,
    /// Number of parameters
    param_count: u8,
    /// Number of local variables (including params)
    local_count: u8,
    /// Maximum registers used (for allocation)
    max_regs: u8,
    /// Instructions
    code: []const Inst,
    /// Constant pool (for large immediates, symbols, strings)
    constants: []const u64,
    /// Source locations for debugging (parallel to code)
    locations: ?[]const u32,

    /// Disassemble function
    pub fn disassemble(self: Function, writer: anytype) !void {
        try writer.print("function {s}(params={}, locals={}, regs={}):\n", .{
            self.name,
            self.param_count,
            self.local_count,
            self.max_regs,
        });
        for (self.code, 0..) |inst, idx| {
            try writer.print("  {d:4}: {}\n", .{ idx, inst });
        }
    }
};

/// Program in register IR (collection of functions)
pub const Program = struct {
    functions: []const Function,
    /// Entry point function index
    entry: u32,
    /// Global constant pool
    globals: []const u64,
};

// ============================================================================
// Builder for constructing IR
// ============================================================================

pub const Builder = struct {
    allocator: std.mem.Allocator,
    code: std.ArrayList(Inst),
    constants: std.ArrayList(u64),
    next_reg: Reg,

    pub fn init(allocator: std.mem.Allocator) Builder {
        return .{
            .allocator = allocator,
            .code = std.ArrayList(Inst){},
            .constants = std.ArrayList(u64){},
            .next_reg = 0,
        };
    }

    pub fn deinit(self: *Builder) void {
        self.code.deinit(self.allocator);
        self.constants.deinit(self.allocator);
    }

    /// Allocate a fresh virtual register
    pub fn freshReg(self: *Builder) Reg {
        const r = self.next_reg;
        if (self.next_reg < 31) {
            self.next_reg += 1;
        }
        return r;
    }

    /// Emit an instruction
    pub fn emit(self: *Builder, inst: Inst) !void {
        try self.code.append(self.allocator, inst);
    }

    /// Emit: dest = src
    pub fn emitMov(self: *Builder, dest: Reg, src: Reg) !void {
        try self.emit(Inst.r(.mov, dest, src, 0));
    }

    /// Emit: dest = immediate
    pub fn emitImm(self: *Builder, dest: Reg, value: i64) !void {
        if (value >= -32768 and value <= 32767) {
            // Fits in 16 bits
            try self.emit(Inst.i(.movi, dest, 0, @truncate(value)));
        } else {
            // Need multiple instructions for large constants
            // Low 16 bits
            try self.emit(Inst.i(.movi, dest, 0, @truncate(value)));
            // High bits via movk
            if ((value >> 16) != 0 and (value >> 16) != -1) {
                try self.emit(Inst.i(.movk, dest, 0, @truncate(value >> 16)));
            }
        }
    }

    /// Emit: dest = src1 + src2
    pub fn emitAdd(self: *Builder, dest: Reg, src1: Reg, src2: Reg) !void {
        try self.emit(Inst.r(.add, dest, src1, src2));
    }

    /// Emit: dest = src1 + imm
    pub fn emitAddI(self: *Builder, dest: Reg, src1: Reg, imm: i16) !void {
        try self.emit(Inst.i(.addi, dest, src1, imm));
    }

    /// Emit: dest = cons(car, cdr)
    pub fn emitCons(self: *Builder, dest: Reg, car: Reg, cdr: Reg) !void {
        try self.emit(Inst.r(.cons, dest, car, cdr));
    }

    /// Emit: dest = car(src)
    pub fn emitCar(self: *Builder, dest: Reg, src: Reg) !void {
        try self.emit(Inst.r(.car, dest, src, 0));
    }

    /// Emit: dest = cdr(src)
    pub fn emitCdr(self: *Builder, dest: Reg, src: Reg) !void {
        try self.emit(Inst.r(.cdr, dest, src, 0));
    }

    /// Emit: jump offset
    pub fn emitJmp(self: *Builder, offset: i16) !void {
        try self.emit(Inst.br(.jmp, 0, offset));
    }

    /// Emit: jump if zero (nil)
    pub fn emitJz(self: *Builder, cond: Reg, offset: i16) !void {
        try self.emit(Inst.br(.jz, cond, offset));
    }

    /// Emit: jump if not zero (not nil)
    pub fn emitJnz(self: *Builder, cond: Reg, offset: i16) !void {
        try self.emit(Inst.br(.jnz, cond, offset));
    }

    /// Emit: dest = call(func, argc)
    pub fn emitCall(self: *Builder, dest: Reg, func: Reg, argc: u8) !void {
        try self.emit(Inst.iu(.call, dest, func, argc));
    }

    /// Emit: return src
    pub fn emitRet(self: *Builder, src: Reg) !void {
        try self.emit(Inst.r(.ret, 0, src, 0));
    }

    /// Get current instruction index (for patching branches)
    pub fn here(self: *Builder) usize {
        return self.code.items.len;
    }

    /// Patch branch at index to jump to current position
    pub fn patchBranch(self: *Builder, idx: usize) void {
        const offset: i16 = @intCast(@as(isize, @intCast(self.code.items.len)) - @as(isize, @intCast(idx)) - 1);
        self.code.items[idx].operand = @bitCast(offset);
    }

    /// Add constant to pool, return index
    pub fn addConstant(self: *Builder, value: u64) !u16 {
        const idx = self.constants.items.len;
        try self.constants.append(self.allocator, value);
        return @intCast(idx);
    }

    /// Build function from accumulated instructions
    pub fn build(self: *Builder, name: []const u8, param_count: u8, local_count: u8) !Function {
        return .{
            .name = name,
            .param_count = param_count,
            .local_count = local_count,
            .max_regs = self.next_reg,
            .code = try self.code.toOwnedSlice(self.allocator),
            .constants = try self.constants.toOwnedSlice(self.allocator),
            .locations = null,
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "instruction encoding" {
    const testing = std.testing;

    // R-type: add v0, v1, v2
    const add = Inst.r(.add, 0, 1, 2);
    try testing.expectEqual(@as(u6, @intFromEnum(Op.add)), @intFromEnum(add.op));
    try testing.expectEqual(@as(Reg, 0), add.dest);
    try testing.expectEqual(@as(Reg, 1), add.src1);
    try testing.expectEqual(@as(Reg, 2), add.src2());

    // I-type: addi v0, v1, #42
    const addi = Inst.i(.addi, 0, 1, 42);
    try testing.expectEqual(@as(i16, 42), addi.imm());

    // I-type with negative: addi v0, v1, #-10
    const addi_neg = Inst.i(.addi, 0, 1, -10);
    try testing.expectEqual(@as(i16, -10), addi_neg.imm());

    // Instruction size is 32 bits
    try testing.expectEqual(@as(usize, 4), @sizeOf(Inst));
}

test "builder basic" {
    const testing = std.testing;

    var b = Builder.init(testing.allocator);
    defer b.deinit();

    // (defun add-one (x) (+ x 1))
    const v0 = b.freshReg(); // x (param)
    const v1 = b.freshReg(); // result

    try b.emitAddI(v1, v0, 1);
    try b.emitRet(v1);

    const func = try b.build("add-one", 1, 1);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    try testing.expectEqual(@as(usize, 2), func.code.len);
    try testing.expectEqual(Op.addi, func.code[0].op);
    try testing.expectEqual(Op.ret, func.code[1].op);
}

test "builder with branch" {
    const testing = std.testing;

    var b = Builder.init(testing.allocator);
    defer b.deinit();

    // (defun abs (x) (if (< x 0) (- x) x))
    const v0 = b.freshReg(); // x
    const v1 = b.freshReg(); // zero
    const v2 = b.freshReg(); // comparison result
    const v3 = b.freshReg(); // result

    try b.emitImm(v1, 0); // v1 = 0
    try b.emit(Inst.r(.lt, v2, v0, v1)); // v2 = x < 0

    const branch_idx = b.here();
    try b.emitJz(v2, 0); // placeholder offset

    // Then branch: negate
    try b.emit(Inst.r(.neg, v3, v0, 0)); // v3 = -x
    try b.emitRet(v3);

    // Else branch: return x
    b.patchBranch(branch_idx);
    try b.emitMov(v3, v0);
    try b.emitRet(v3);

    const func = try b.build("abs", 1, 1);
    defer testing.allocator.free(func.code);
    defer testing.allocator.free(func.constants);

    try testing.expectEqual(@as(usize, 7), func.code.len);
}

test "instruction format" {
    var buf: [256]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const writer = fbs.writer();

    const add = Inst.r(.add, 0, 1, 2);
    try add.format("", .{}, writer);
    const result = fbs.getWritten();
    try std.testing.expectEqualStrings("add v0, v1, v2", result);
}
