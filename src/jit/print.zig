//! IR printing helpers.

const std = @import("std");
const ir = @import("ir.zig");

pub fn dump(writer: anytype, f: *const ir.Func) !void {
    for (f.blocks.items, 0..) |*b, b_idx| {
        try writer.print("b{}(", .{b_idx});
        for (b.params.items, 0..) |v, i| {
            if (i != 0) try writer.writeAll(", ");
            try writer.print("v{}", .{@intFromEnum(v)});
        }
        try writer.writeAll(")\n");
        var it = b.first_inst;
        while (it) |inst_id| : (it = f.inst_next.items[@intCast(@intFromEnum(inst_id))]) {
            const res = f.instResult(inst_id);
            if (res) |rid| {
                try writer.print("  v{} = ", .{@intFromEnum(rid)});
            } else {
                try writer.writeAll("  ");
            }
            try dumpInst(writer, f, inst_id);
            try writer.writeAll("\n");
        }
    }
}

fn dumpArgs(writer: anytype, f: *const ir.Func, off: u32, len: u16) !void {
    try writer.writeAll("(");
    const start: usize = @intCast(off);
    const end = start + @as(usize, len);
    for (f.args.items[start..end], 0..) |v, i| {
        if (i != 0) try writer.writeAll(", ");
        try writer.print("v{}", .{@intFromEnum(v)});
    }
    try writer.writeAll(")");
}

fn dumpInst(writer: anytype, f: *const ir.Func, inst_id: ir.InstId) !void {
    const inst = f.inst(inst_id);
    switch (inst.*) {
        .iconst => |c| try writer.print("iconst {s} {}", .{ @tagName(c.ty), c.imm }),
        .uconst => |c| try writer.print("uconst {s} {}", .{ @tagName(c.ty), c.imm }),
        .fconst => |x| try writer.print("fconst {}", .{x}),
        .un => |u| try writer.print("{s} {s} v{}", .{ @tagName(u.op), @tagName(u.ty), @intFromEnum(u.x) }),
        .bin => |b| try writer.print("{s} {s} v{}, v{}", .{ @tagName(b.op), @tagName(b.ty), @intFromEnum(b.lhs), @intFromEnum(b.rhs) }),
        .safepoint => |sp| {
            try writer.writeAll("safepoint ");
            try dumpArgs(writer, f, sp.arg_off, sp.arg_len);
        },
        .call => |c| {
            try writer.print("call {s} v{} ", .{ @tagName(c.ty), @intFromEnum(c.callee) });
            try dumpArgs(writer, f, c.arg_off, c.arg_len);
        },
        .br => |br| {
            try writer.print("br b{} ", .{@intFromEnum(br.target)});
            try dumpArgs(writer, f, br.arg_off, br.arg_len);
        },
        .br_if => |br_if| {
            try writer.print("br_if v{} b{} ", .{ @intFromEnum(br_if.cond), @intFromEnum(br_if.then_blk) });
            try dumpArgs(writer, f, br_if.then_arg_off, br_if.then_arg_len);
            try writer.print(" b{} ", .{@intFromEnum(br_if.else_blk)});
            try dumpArgs(writer, f, br_if.else_arg_off, br_if.else_arg_len);
        },
        .ret => |r| if (r.val) |v| {
            try writer.print("ret v{}", .{@intFromEnum(v)});
        } else {
            try writer.writeAll("ret");
        },
    }
}

test "jit ir dump smoke" {
    const testing = std.testing;

    var f = ir.Func.init(testing.allocator);
    defer f.deinit();

    const entry = try f.addBlock();
    var b = ir.Func.Builder.init(&f, entry);
    const x = try b.iconst(.i64, 1);
    try b.ret(x);

    var buf: [1024]u8 = undefined;
    var out = std.io.fixedBufferStream(&buf);
    try dump(out.writer(), &f);
    try testing.expect(out.pos > 0);
}
