//! Condition system primitives

const std = @import("std");
const runtime = @import("../runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;

/// make-condition: Create condition instance
/// (make-condition type &rest initargs)
pub fn makeCondition(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidSyntax;

    const type_sym = args[0];
    if (!type_sym.isSymbol()) return error.TypeError;

    // Parse initargs for :format-control and :format-arguments
    var format_control = Value.nil;
    var format_args = Value.nil;

    var i: usize = 1;
    while (i + 1 < args.len) : (i += 2) {
        const key = args[i];
        const val = args[i + 1];

        if (!key.isKeyword()) continue;

        const kw = key.toPtr(runtime.Keyword);
        const kw_name = kw.getName();

        if (std.mem.eql(u8, kw_name, "format-control")) {
            format_control = val;
        } else if (std.mem.eql(u8, kw_name, "format-arguments")) {
            format_args = val;
        }
    }

    return try heap.allocCondition(type_sym, format_control, format_args);
}
