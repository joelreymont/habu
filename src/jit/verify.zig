//! IR verifier.

const std = @import("std");
const ir = @import("ir.zig");

pub const VerifyError = error{
    MissingTerm,
    BadTerm,
    BadArgRange,
    BadEdgeArgs,
    BadSafepointTy,
};

pub fn verify(f: *const ir.Func) VerifyError!void {
    const args_len: usize = f.args.items.len;

    for (f.blocks.items, 0..) |*b, b_idx| {
        _ = b_idx;
        const term_id = b.term orelse return error.MissingTerm;

        if (b.last_inst == null) return error.BadTerm;
        if (b.last_inst.? != term_id) return error.BadTerm;

        // Ensure terminator is a terminator instruction.
        switch (f.inst(term_id).*) {
            .br, .br_if, .ret => {},
            else => return error.BadTerm,
        }

        var it = b.first_inst;
        while (it) |inst_id| : (it = f.inst_next.items[@intCast(@intFromEnum(inst_id))]) {
            const inst = f.inst(inst_id);
            switch (inst.*) {
                .call => |c| {
                    const end = @as(usize, c.arg_off) + @as(usize, c.arg_len);
                    if (end > args_len) return error.BadArgRange;
                },
                .safepoint => |sp| {
                    const start: usize = @intCast(sp.arg_off);
                    const end = start + @as(usize, sp.arg_len);
                    if (end > args_len) return error.BadArgRange;
                    for (f.args.items[start..end]) |v| {
                        if (f.value(v).ty != .val) return error.BadSafepointTy;
                    }
                },
                .br => |br| {
                    if (inst_id != term_id) return error.BadTerm;
                    const end = @as(usize, br.arg_off) + @as(usize, br.arg_len);
                    if (end > args_len) return error.BadArgRange;

                    const t = f.block(br.target);
                    if (br.arg_len != @as(u16, @intCast(t.params.items.len))) return error.BadEdgeArgs;
                },
                .br_if => |br_if| {
                    if (inst_id != term_id) return error.BadTerm;
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
                .ret => {
                    if (inst_id != term_id) return error.BadTerm;
                },
                else => {},
            }
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

test "jit ir verify safepoint types" {
    const testing = std.testing;

    // Bad: non-.val live value.
    {
        var f = ir.Func.init(testing.allocator);
        defer f.deinit();

        const entry = try f.addBlock();
        var b = ir.Func.Builder.init(&f, entry);

        const p0 = try b.param(.i64);
        try b.safepoint(&.{p0});
        try b.ret(p0);

        try testing.expectError(error.BadSafepointTy, verify(&f));
    }

    // Good: .val live value.
    {
        var f = ir.Func.init(testing.allocator);
        defer f.deinit();

        const entry = try f.addBlock();
        var b = ir.Func.Builder.init(&f, entry);

        const p0 = try b.param(.val);
        try b.safepoint(&.{p0});
        try b.ret(p0);

        try verify(&f);
    }
}
