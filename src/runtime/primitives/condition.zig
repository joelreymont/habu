//! Condition system primitives

const std = @import("std");
const runtime = @import("../runtime.zig");
const io = @import("io.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;

/// make-condition: Create condition instance
/// (make-condition type &rest initargs)
pub fn makeCondition(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;

    const type_sym = args[0];
    if (!type_sym.isSymbol()) return error.TypeError;

    // Parse initargs for :format-control and :format-arguments
    const kw_format_control = try heap.internKeyword("format-control");
    const kw_format_args = try heap.internKeyword("format-arguments");
    var format_control = Value.nil;
    var format_args = Value.nil;

    var i: usize = 1;
    while (i + 1 < args.len) : (i += 2) {
        const key = args[i];
        const val = args[i + 1];

        if (!key.isKeyword()) continue;

        if (key.eq(kw_format_control)) {
            format_control = val;
        } else if (key.eq(kw_format_args)) {
            format_args = val;
        }
    }

    return try heap.allocCondition(type_sym, format_control, format_args);
}

/// warn: Signal warning condition
/// (warn datum &rest arguments)
pub fn warn(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;

    const datum = args[0];
    const rest = if (args.len > 1) args[1..] else &[_]Value{};

    // Create warning condition
    const condition = switch (datum.typeKind()) {
        .string => try heap.allocCondition(
            try heap.intern("simple-warning"),
            datum,
            try heap.listFromSlice(rest),
        ),
        .symbol => try makeCondition(heap, args),
        else => datum,
    };

    if (heap.warn_handler) |handler| {
        try handler(condition, heap.warn_ctx);
        return Value.nil;
    }

    // Signal the warning (default handler prints it)
    // TODO: implement proper handler dispatch
    var out_buf: [4096]u8 = undefined;
    var file_writer = std.fs.File.stderr().writer(&out_buf);
    const writer = &file_writer.interface;
    try writer.print("WARNING: ", .{});
    try io.writeValueToBuffer(condition, writer);
    try writer.print("\n", .{});
    try writer.flush();

    return Value.nil;
}

/// simple-condition-format-control: Get format-control slot from condition
/// (simple-condition-format-control condition)
pub fn simpleConditionFormatControl(_: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;
    const cond = args[0];

    if (!cond.isCondition()) return error.TypeError;

    const condition = cond.toPtr(runtime.Condition);
    return condition.format_control;
}

/// simple-condition-format-arguments: Get format-arguments slot from condition
/// (simple-condition-format-arguments condition)
pub fn simpleConditionFormatArguments(_: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;
    const cond = args[0];

    if (!cond.isCondition()) return error.TypeError;

    const condition = cond.toPtr(runtime.Condition);
    return condition.format_args;
}

/// invoke-restart: Invoke named restart with arguments
/// (invoke-restart restart-name &rest arguments)
pub fn invokeRestart(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;
    // TODO: implement restart invocation
    _ = heap;
    return error.NotImplemented;
}

/// invoke-restart-interactively: Invoke restart, prompting for arguments
/// (invoke-restart-interactively restart-name)
pub fn invokeRestartInteractively(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;
    // TODO: implement interactive restart invocation
    _ = heap;
    return error.NotImplemented;
}

/// find-restart: Lookup restart by name
/// (find-restart name &optional condition)
pub fn findRestart(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;
    // TODO: implement restart lookup
    _ = heap;
    return Value.nil;
}

/// compute-restarts: Return list of active restarts
/// (compute-restarts &optional condition)
pub fn computeRestarts(heap: *Heap, args: []const Value) !Value {
    _ = args;
    // TODO: implement restart enumeration
    _ = heap;
    return Value.nil;
}

/// restart-name: Get restart name
/// (restart-name restart)
pub fn restartName(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;
    // TODO: implement restart name accessor
    _ = heap;
    return Value.nil;
}

/// break: Enter debugger with continue restart
/// (break &optional format-control &rest format-arguments)
pub fn @"break"(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    var out_buf: [4096]u8 = undefined;
    var file_writer = std.fs.File.stderr().writer(&out_buf);
    const writer = &file_writer.interface;

    if (args.len > 0) {
        const format_control = args[0];
        if (format_control.isString()) {
            try writer.print("BREAK: ", .{});
            try io.writeValueToBuffer(format_control, writer);
            try writer.print("\n", .{});
        }
    } else {
        try writer.print("BREAK\n", .{});
    }

    // TODO: implement proper debugger with continue restart
    try writer.print("Continue restart available (not yet implemented)\n", .{});
    try writer.flush();

    return Value.nil;
}

// ============================================================================
// Tests
// ============================================================================

test "warn accepts string and symbol datum" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var count: u8 = 0;
    const handler = struct {
        fn handle(cond: Value, ctx: ?*anyopaque) !void {
            if (!cond.isCondition()) return error.TypeError;
            const count_ptr: *u8 = @ptrCast(@alignCast(ctx.?));
            count_ptr.* += 1;
        }
    }.handle;
    heap.setWarnHandler(handler, &count);
    defer heap.setWarnHandler(null, null);

    const msg = try heap.allocBaseString("oops");
    const args1 = [_]Value{msg};
    _ = try warn(&heap, &args1);

    const sym = try heap.intern("oops");
    const args2 = [_]Value{sym};
    _ = try warn(&heap, &args2);

    try testing.expectEqual(@as(u8, 2), count);
}
