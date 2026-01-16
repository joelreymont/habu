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
const Heap = @import("../../runtime/heap.zig").Heap;

/// Emit pass - transforms IR to bytecode
/// Note: This is a simplified pass that doesn't handle heap/symbol interning.
/// For full functionality, use Emitter.initWithHeap() directly.
pub fn emit(allocator: std.mem.Allocator, input: *const Ir) !PassResult(Chunk) {
    var emitter = Emitter.init(allocator);
    defer emitter.deinit();

    try emitter.emit(input);
    const chunk = try emitter.finalize();

    return PassResult(Chunk).changed(chunk);
}

/// Emit with heap for symbol interning (full functionality)
pub fn emitWithHeap(allocator: std.mem.Allocator, heap: *Heap, input: *const Ir) !PassResult(Chunk) {
    var emitter = Emitter.initWithHeap(allocator, heap);
    defer emitter.deinit();

    try emitter.emit(input);
    const chunk = try emitter.finalize();

    return PassResult(Chunk).changed(chunk);
}

/// Create the emit pass
pub const pass = pass_mod.makePass(*const Ir, Chunk, "emit", emit);

// ============================================================================
// Tests
// ============================================================================

test "emit pass - literal" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const result = try emit(testing.allocator, &lit);

    try testing.expect(result.modified);
    try testing.expect(result.output.code.len > 0);

    // Clean up
    testing.allocator.free(result.output.code);
    testing.allocator.free(result.output.constants);
}

test "emit pass - addition" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const add = Ir{ .add = .{ .left = &lit1, .right = &lit2 } };

    const result = try emit(testing.allocator, &add);

    try testing.expect(result.modified);
    try testing.expect(result.output.code.len > 0);

    // Clean up
    testing.allocator.free(result.output.code);
    testing.allocator.free(result.output.constants);
}
