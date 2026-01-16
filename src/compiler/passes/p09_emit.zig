//! Emit Pass
//!
//! Transforms IR to bytecode.
//! This is the final compilation step before execution.
//!
//! Input: Ir
//! Output: Chunk (bytecode)

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const Ir = @import("../ir.zig").Ir;
const bytecode = @import("../../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;
const Chunk = bytecode.Chunk;
const Value = @import("../../runtime/value.zig").Value;
const Heap = @import("../../runtime/heap.zig").Heap;

/// Emit pass - transforms IR to bytecode Value
/// Note: This requires a heap for GC-managed chunk allocation.
pub fn emit(allocator: std.mem.Allocator, heap: *Heap, input: *const Ir) !PassResult(Value) {
    var emitter = Emitter.initWithHeap(allocator, heap);
    defer emitter.deinit();

    try emitter.emit(input);
    const chunk_val = try emitter.finalize();

    return PassResult(Value).changed(chunk_val);
}

/// Create the emit pass
pub const pass = pass_mod.makePass(*const Ir, Value, "emit", emit);

// ============================================================================
// Tests
// ============================================================================

test "emit pass - literal" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const result = try emit(testing.allocator, &heap, &lit);

    try testing.expect(result.modified);
    const chunk = result.output.toPtr(Chunk);
    try testing.expect(chunk.getCode().len > 0);
}

test "emit pass - addition" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const add = Ir{ .add = .{ .left = &lit1, .right = &lit2 } };

    const result = try emit(testing.allocator, &heap, &add);

    try testing.expect(result.modified);
    const chunk = result.output.toPtr(Chunk);
    try testing.expect(chunk.getCode().len > 0);
}
