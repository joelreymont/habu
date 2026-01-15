//! Symbol primitives
//!
//! gensym, symbol-name, symbol-package, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;

/// Generate a unique symbol (gensym)
/// Returns a new uninterned symbol with name prefix + counter
pub fn gensym(heap: *Heap, prefix: ?Value) error{OutOfMemory}!Value {
    const counter = &heap.gensym_counter;
    const count = counter.*;
    counter.* = count + 1;

    const prefix_str = if (prefix) |p| blk: {
        if (p.isString()) {
            const s = p.toPtr(@import("../objects.zig").String);
            break :blk s.bytes();
        } else if (p.isSymbol()) {
            const sym = p.toPtr(@import("../objects.zig").Symbol);
            break :blk sym.getName();
        } else {
            break :blk "G";
        }
    } else "G";

    // Format: PREFIX-N
    var buf: [256]u8 = undefined;
    const name = std.fmt.bufPrint(&buf, "{s}{d}", .{ prefix_str, count }) catch "GENSYM";

    return try heap.allocSymbol(name);
}
