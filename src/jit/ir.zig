//! SSA IR for the JIT backend.
//!
//! This is a small, allocation-friendly core suitable for later optimization
//! passes (CFG, SSA values, block parameters) and lowering/codegen.

const std = @import("std");

pub const BlockId = enum(u32) { _ };
pub const InstId = enum(u32) { _ };
pub const ValueId = enum(u32) { _ };

pub const Ty = enum(u8) {
    void,
    i64,
    u64,
    f64,
    /// Tagged runtime Value (u64).
    val,
    /// Raw pointer (usize).
    ptr,
};

pub const UnOp = enum(u8) {
    neg,
    not,
    bit_not,
};

pub const BinOp = enum(u8) {
    add,
    sub,
    mul,
    sdiv,
    udiv,
    and_,
    or_,
    xor,
    shl,
    shr,
    eq,
    lt,
    gt,
};

pub const Inst = union(enum) {
    iconst: struct { ty: Ty, imm: i64 },
    uconst: struct { ty: Ty, imm: u64 },
    fconst: f64,
    un: struct { op: UnOp, ty: Ty, x: ValueId },
    bin: struct { op: BinOp, ty: Ty, lhs: ValueId, rhs: ValueId },
    call: struct { ty: Ty, callee: ValueId, arg_off: u32, arg_len: u16 },
    br: struct { target: BlockId, arg_off: u32, arg_len: u16 },
    br_if: struct {
        cond: ValueId,
        then_blk: BlockId,
        else_blk: BlockId,
        then_arg_off: u32,
        then_arg_len: u16,
        else_arg_off: u32,
        else_arg_len: u16,
    },
    ret: struct { val: ?ValueId },
};

pub const Value = struct {
    ty: Ty,
    def: Def,

    pub const Def = union(enum) {
        param: struct { blk: BlockId, idx: u16 },
        inst: InstId,
    };
};

pub const Block = struct {
    params: std.ArrayList(ValueId),
    first_inst: ?InstId,
    last_inst: ?InstId,
    term: ?InstId,

    fn init() Block {
        return .{
            .params = std.ArrayList(ValueId){},
            .first_inst = null,
            .last_inst = null,
            .term = null,
        };
    }

    fn deinit(self: *Block, allocator: std.mem.Allocator) void {
        self.params.deinit(allocator);
    }
};

pub const Succs = struct {
    n: u8 = 0,
    b0: BlockId = @enumFromInt(0),
    b1: BlockId = @enumFromInt(0),
};

pub const Func = struct {
    allocator: std.mem.Allocator,

    blocks: std.ArrayList(Block),
    insts: std.ArrayList(Inst),
    inst_next: std.ArrayList(?InstId),
    inst_prev: std.ArrayList(?InstId),
    inst_res: std.ArrayList(?ValueId),

    values: std.ArrayList(Value),
    args: std.ArrayList(ValueId),

    pub fn init(allocator: std.mem.Allocator) Func {
        return .{
            .allocator = allocator,
            .blocks = std.ArrayList(Block){},
            .insts = std.ArrayList(Inst){},
            .inst_next = std.ArrayList(?InstId){},
            .inst_prev = std.ArrayList(?InstId){},
            .inst_res = std.ArrayList(?ValueId){},
            .values = std.ArrayList(Value){},
            .args = std.ArrayList(ValueId){},
        };
    }

    pub fn deinit(self: *Func) void {
        for (self.blocks.items) |*b| b.deinit(self.allocator);
        self.blocks.deinit(self.allocator);
        self.insts.deinit(self.allocator);
        self.inst_next.deinit(self.allocator);
        self.inst_prev.deinit(self.allocator);
        self.inst_res.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.args.deinit(self.allocator);
    }

    pub fn addBlock(self: *Func) !BlockId {
        const id: BlockId = @enumFromInt(@as(u32, @intCast(self.blocks.items.len)));
        try self.blocks.append(self.allocator, Block.init());
        return id;
    }

    pub fn addParam(self: *Func, blk: BlockId, ty: Ty) !ValueId {
        const b = self.blockMut(blk);
        const idx: u16 = @intCast(b.params.items.len);

        const val = try self.addValue(.{
            .ty = ty,
            .def = .{ .param = .{ .blk = blk, .idx = idx } },
        });
        try b.params.append(self.allocator, val);
        return val;
    }

    pub fn block(self: *const Func, blk: BlockId) *const Block {
        const i: usize = @intCast(@intFromEnum(blk));
        return &self.blocks.items[i];
    }

    pub fn blockMut(self: *Func, blk: BlockId) *Block {
        const i: usize = @intCast(@intFromEnum(blk));
        return &self.blocks.items[i];
    }

    pub fn value(self: *const Func, v: ValueId) *const Value {
        const i: usize = @intCast(@intFromEnum(v));
        return &self.values.items[i];
    }

    pub fn inst(self: *const Func, inst_id: InstId) *const Inst {
        const i: usize = @intCast(@intFromEnum(inst_id));
        return &self.insts.items[i];
    }

    pub fn instResult(self: *const Func, inst_id: InstId) ?ValueId {
        const i: usize = @intCast(@intFromEnum(inst_id));
        return self.inst_res.items[i];
    }

    pub fn blockSuccs(self: *const Func, blk: BlockId) Succs {
        const b = self.block(blk);
        const term_id = b.term orelse return .{};
        return switch (self.inst(term_id).*) {
            .br => |br| .{ .n = 1, .b0 = br.target },
            .br_if => |br_if| .{ .n = 2, .b0 = br_if.then_blk, .b1 = br_if.else_blk },
            .ret => .{},
            else => .{},
        };
    }

    fn addValue(self: *Func, v: Value) !ValueId {
        const id: ValueId = @enumFromInt(@as(u32, @intCast(self.values.items.len)));
        try self.values.append(self.allocator, v);
        return id;
    }

    fn pushArgs(self: *Func, args: []const ValueId) !struct { off: u32, len: u16 } {
        const off: u32 = @intCast(self.args.items.len);
        const len: u16 = @intCast(args.len);
        try self.args.appendSlice(self.allocator, args);
        return .{ .off = off, .len = len };
    }

    fn appendInstToBlock(self: *Func, blk: BlockId, id: InstId) void {
        const b = self.blockMut(blk);
        const prev = b.last_inst;

        if (prev) |p| {
            const p_i: usize = @intCast(@intFromEnum(p));
            self.inst_next.items[p_i] = id;
        } else {
            b.first_inst = id;
        }

        const i: usize = @intCast(@intFromEnum(id));
        self.inst_prev.items[i] = prev;
        self.inst_next.items[i] = null;

        b.last_inst = id;
    }

    fn addInst(self: *Func, blk: BlockId, ins: Inst, res_ty: Ty) !?ValueId {
        const inst_id: InstId = @enumFromInt(@as(u32, @intCast(self.insts.items.len)));
        try self.insts.append(self.allocator, ins);
        try self.inst_next.append(self.allocator, null);
        try self.inst_prev.append(self.allocator, null);
        try self.inst_res.append(self.allocator, null);

        self.appendInstToBlock(blk, inst_id);

        if (res_ty == .void) return null;
        const v = try self.addValue(.{ .ty = res_ty, .def = .{ .inst = inst_id } });
        self.inst_res.items[@intCast(@intFromEnum(inst_id))] = v;
        return v;
    }

    pub fn setTerm(self: *Func, blk: BlockId, inst_id: InstId) void {
        self.blockMut(blk).term = inst_id;
    }

    pub const Builder = struct {
        f: *Func,
        blk: BlockId,

        pub fn init(f: *Func, entry: BlockId) Builder {
            return .{ .f = f, .blk = entry };
        }

        pub fn setBlock(self: *Builder, blk: BlockId) void {
            self.blk = blk;
        }

        pub fn addBlock(self: *Builder) !BlockId {
            return try self.f.addBlock();
        }

        pub fn param(self: *Builder, ty: Ty) !ValueId {
            return try self.f.addParam(self.blk, ty);
        }

        pub fn iconst(self: *Builder, ty: Ty, imm: i64) !ValueId {
            return (try self.f.addInst(self.blk, .{ .iconst = .{ .ty = ty, .imm = imm } }, ty)).?;
        }

        pub fn uconst(self: *Builder, ty: Ty, imm: u64) !ValueId {
            return (try self.f.addInst(self.blk, .{ .uconst = .{ .ty = ty, .imm = imm } }, ty)).?;
        }

        pub fn fconst(self: *Builder, imm: f64) !ValueId {
            return (try self.f.addInst(self.blk, .{ .fconst = imm }, .f64)).?;
        }

        pub fn un(self: *Builder, op: UnOp, ty: Ty, x: ValueId) !ValueId {
            return (try self.f.addInst(self.blk, .{ .un = .{ .op = op, .ty = ty, .x = x } }, ty)).?;
        }

        pub fn bin(self: *Builder, op: BinOp, ty: Ty, lhs: ValueId, rhs: ValueId) !ValueId {
            return (try self.f.addInst(self.blk, .{ .bin = .{ .op = op, .ty = ty, .lhs = lhs, .rhs = rhs } }, ty)).?;
        }

        pub fn call(self: *Builder, ty: Ty, callee: ValueId, args: []const ValueId) !ValueId {
            const a = try self.f.pushArgs(args);
            return (try self.f.addInst(self.blk, .{ .call = .{
                .ty = ty,
                .callee = callee,
                .arg_off = a.off,
                .arg_len = a.len,
            } }, ty)).?;
        }

        pub fn br(self: *Builder, target: BlockId, args: []const ValueId) !void {
            const a = try self.f.pushArgs(args);
            const inst_id: InstId = @enumFromInt(@as(u32, @intCast(self.f.insts.items.len)));
            _ = try self.f.addInst(self.blk, .{ .br = .{
                .target = target,
                .arg_off = a.off,
                .arg_len = a.len,
            } }, .void);
            self.f.setTerm(self.blk, inst_id);
        }

        pub fn brIf(
            self: *Builder,
            cond: ValueId,
            then_blk: BlockId,
            then_args: []const ValueId,
            else_blk: BlockId,
            else_args: []const ValueId,
        ) !void {
            const ta = try self.f.pushArgs(then_args);
            const ea = try self.f.pushArgs(else_args);
            const inst_id: InstId = @enumFromInt(@as(u32, @intCast(self.f.insts.items.len)));
            _ = try self.f.addInst(self.blk, .{ .br_if = .{
                .cond = cond,
                .then_blk = then_blk,
                .else_blk = else_blk,
                .then_arg_off = ta.off,
                .then_arg_len = ta.len,
                .else_arg_off = ea.off,
                .else_arg_len = ea.len,
            } }, .void);
            self.f.setTerm(self.blk, inst_id);
        }

        pub fn ret(self: *Builder, v: ?ValueId) !void {
            const inst_id: InstId = @enumFromInt(@as(u32, @intCast(self.f.insts.items.len)));
            _ = try self.f.addInst(self.blk, .{ .ret = .{ .val = v } }, .void);
            self.f.setTerm(self.blk, inst_id);
        }
    };
};

test "jit ir builds blocks and edges" {
    const testing = std.testing;

    var f = Func.init(testing.allocator);
    defer f.deinit();

    const entry = try f.addBlock();
    var b = Func.Builder.init(&f, entry);

    const then_blk = try b.addBlock();
    const else_blk = try b.addBlock();

    b.setBlock(entry);
    const p0 = try b.param(.i64);
    const one = try b.iconst(.i64, 1);
    const cond = try b.bin(.eq, .i64, p0, one);
    try b.brIf(cond, then_blk, &.{}, else_blk, &.{});

    b.setBlock(then_blk);
    try b.ret(p0);

    b.setBlock(else_blk);
    const two = try b.iconst(.i64, 2);
    try b.ret(two);

    const s0 = f.blockSuccs(entry);
    try testing.expectEqual(@as(u8, 2), s0.n);
    try testing.expectEqual(@as(u32, @intFromEnum(then_blk)), @as(u32, @intFromEnum(s0.b0)));
    try testing.expectEqual(@as(u32, @intFromEnum(else_blk)), @as(u32, @intFromEnum(s0.b1)));
}
