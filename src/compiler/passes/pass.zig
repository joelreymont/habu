//! Nanopass Infrastructure
//!
//! Each pass takes one IR type and produces another (possibly different) IR.
//! Passes are small, focused transformations.

const std = @import("std");
const Ir = @import("../ir.zig").Ir;
const ir_types = @import("ir_types.zig");
const TypedIr = ir_types.TypedIr;
const Value = @import("../../runtime/value.zig").Value;

/// Error type for passes
pub const PassError = error{
    OutOfMemory,
    PassFailed,
    TypeError,
    InvalidIr,
    SyntaxError,
};

/// Diagnostic from a pass
pub const Diagnostic = struct {
    level: Level,
    message: []const u8,

    pub const Level = enum { @"error", warning, note };
};

/// Result of running a pass
pub fn PassResult(comptime Out: type) type {
    return struct {
        /// Output of the pass
        output: Out,
        /// Whether the output differs from input
        modified: bool,
        /// Diagnostics collected during pass
        diagnostics: []const Diagnostic,

        pub fn unchanged(input: Out) @This() {
            return .{
                .output = input,
                .modified = false,
                .diagnostics = &.{},
            };
        }

        pub fn changed(output: Out) @This() {
            return .{
                .output = output,
                .modified = true,
                .diagnostics = &.{},
            };
        }

        pub fn withDiagnostics(output: Out, modified: bool, diags: []const Diagnostic) @This() {
            return .{
                .output = output,
                .modified = modified,
                .diagnostics = diags,
            };
        }
    };
}

/// A pass transforms In to Out
pub fn Pass(comptime In: type, comptime Out: type) type {
    return struct {
        const Self = @This();

        name: []const u8,
        runFn: *const fn (allocator: std.mem.Allocator, input: In) PassError!PassResult(Out),

        pub fn run(self: Self, allocator: std.mem.Allocator, input: In) PassError!PassResult(Out) {
            return self.runFn(allocator, input);
        }
    };
}

/// Create a pass from a function
pub fn makePass(
    comptime In: type,
    comptime Out: type,
    comptime name: []const u8,
    comptime runFn: fn (std.mem.Allocator, In) PassError!PassResult(Out),
) Pass(In, Out) {
    return .{
        .name = name,
        .runFn = runFn,
    };
}

// ============================================================================
// Standard Pass Types
// ============================================================================

/// Ir → Ir transformation
pub const IrPass = Pass(*const Ir, *const Ir);

/// Ir → TypedIr transformation (adds type annotations)
pub const AnnotatePass = Pass(*const Ir, *const TypedIr);

/// TypedIr → TypedIr transformation
pub const TypedPass = Pass(*const TypedIr, *const TypedIr);

/// TypedIr → Ir transformation (removes type annotations, e.g., erasure)
pub const ErasePass = Pass(*const TypedIr, *const Ir);

/// Value (S-expr) → Value transformation
pub const SExprPass = Pass(Value, Value);

/// Value → Ir transformation (lifting)
pub const LiftPass = Pass(Value, *const Ir);

// ============================================================================
// Built-in Passes
// ============================================================================

/// Identity pass - returns input unchanged (for testing)
pub fn identityPass(comptime T: type) Pass(T, T) {
    const Impl = struct {
        fn run(_: std.mem.Allocator, input: T) PassError!PassResult(T) {
            return PassResult(T).unchanged(input);
        }
    };
    return makePass(T, T, "identity", Impl.run);
}

// ============================================================================
// Tests
// ============================================================================

test "identity pass" {
    const testing = std.testing;

    const pass = identityPass(*const Ir);
    const lit = Ir{ .lit = Value.makeFixnum(42) };

    const result = try pass.run(testing.allocator, &lit);
    try testing.expectEqual(&lit, result.output);
    try testing.expect(!result.modified);
}

test "makePass" {
    const testing = std.testing;

    const double = struct {
        fn run(alloc: std.mem.Allocator, input: *const Ir) PassError!PassResult(*const Ir) {
            _ = alloc;
            // Just return unchanged for this test
            return PassResult(*const Ir).unchanged(input);
        }
    };

    const pass = makePass(*const Ir, *const Ir, "double", double.run);
    try testing.expectEqualStrings("double", pass.name);
}
