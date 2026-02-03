//! IR verifier.

const std = @import("std");
const ir = @import("ir.zig");

pub const VerifyError = error{
    MissingTerm,
    BadArgRange,
    BadEdgeArgs,
};

pub fn verify(f: *const ir.Func) VerifyError!void {
    const args_len: usize = f.args.items.len;

    for (f.blocks.items, 0..) |*b, b_idx| {
        _ = b_idx;
        const term_id = b.term orelse return error.MissingTerm;

        const inst = f.inst(term_id);
        switch (inst.*) {
            .call => |c| {
                const end = @as(usize, c.arg_off) + @as(usize, c.arg_len);
                if (end > args_len) return error.BadArgRange;
            },
            .br => |br| {
                const end = @as(usize, br.arg_off) + @as(usize, br.arg_len);
                if (end > args_len) return error.BadArgRange;

                const t = f.block(br.target);
                if (br.arg_len != @as(u16, @intCast(t.params.items.len))) return error.BadEdgeArgs;
            },
            .br_if => |br_if| {
                {
                    const end = @as(usize, br_if.then_arg_off) + @as(usize, br_if.then_arg_len);
                    if (end > args_len) return error.BadArgRange;
                    const t = f.block(br_if.then_blk);
                    if (br_if.then_arg_len != @as(u16, @intCast(t.params.items.len))) return error.BadEdgeArgs;
                }
                {
                    const end = @as(usize, br_if.else_arg_off) + @as(usize, br_if.else_arg_len);
                    if (end > args_len) return error.BadArgRange;
                    const t = f.block(br_if.else_blk);
                    if (br_if.else_arg_len != @as(u16, @intCast(t.params.items.len))) return error.BadEdgeArgs;
                }
            },
            .ret => {},
            else => {},
        }
    }
}

test "jit ir verify ok" {
    const testing = std.testing;

    var f = ir.Func.init(testing.allocator);
    defer f.deinit();

    const entry = try f.addBlock();
    var b = ir.Func.Builder.init(&f, entry);

    const p0 = try b.param(.i64);
    try b.ret(p0);

    try verify(&f);
}

