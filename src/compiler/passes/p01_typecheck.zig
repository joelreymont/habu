//! Type Checking Pass
//!
//! Runs bidirectional type checking on the IR.
//! Uses BiChecker to verify types and enforce QTT linearity.
//!
//! This pass:
//! - Infers types for untyped expressions
//! - Checks typed expressions against expected types
//! - Verifies linear/affine usage of 0/1-quantity variables
//! - Collects type errors without modifying IR

const std = @import("std");
const pass_mod = @import("pass.zig");
const Pass = pass_mod.Pass;
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const PassDiagnostic = pass_mod.PassDiagnostic;
const makePass = pass_mod.makePass;
const Ir = @import("../ir.zig").Ir;
const types = @import("../../types/types.zig");
const BiChecker = types.BiChecker;
const TypingCtx = types.TypingCtx;

/// Type checking pass configuration
pub const TypeCheckConfig = struct {
    /// Whether to fail on type errors (true) or just collect them (false)
    strict: bool = false,
};

/// Type checking pass - runs BiChecker on IR
pub const TypeCheckPass = struct {
    allocator: std.mem.Allocator,
    bi_checker: BiChecker,
    config: TypeCheckConfig,
    /// Accumulated errors
    errors: std.ArrayListUnmanaged(PassDiagnostic),

    pub const pass_name = "typecheck";

    pub fn init(allocator: std.mem.Allocator, config: TypeCheckConfig) TypeCheckPass {
        return .{
            .allocator = allocator,
            .bi_checker = BiChecker.init(allocator),
            .config = config,
            .errors = .{},
        };
    }

    pub fn deinit(self: *TypeCheckPass) void {
        self.bi_checker.deinit();
        self.errors.deinit(self.allocator);
    }

    pub fn run(self: *TypeCheckPass, allocator: std.mem.Allocator, ir: *const Ir) PassError!PassResult {
        _ = allocator;

        // Create a fresh typing context
        var ctx = TypingCtx.init(self.allocator);
        defer ctx.deinit();

        // Clear previous errors
        self.errors.clearRetainingCapacity();

        // Infer the type of the expression
        _ = self.bi_checker.infer(ir, &ctx) catch {
            // Type checking failed - add diagnostic
            const diag = PassDiagnostic{
                .level = .@"error",
                .message = "type error",
                .loc = null,
            };
            self.errors.append(self.allocator, diag) catch return PassError.OutOfMemory;

            if (self.config.strict) {
                return PassError.TypeError;
            }
        };

        // Check linearity violations
        if (ctx.checkLinearity()) |var_name| {
            const diag = PassDiagnostic{
                .level = .@"error",
                .message = var_name, // Variable name with linearity issue
                .loc = null,
            };
            self.errors.append(self.allocator, diag) catch return PassError.OutOfMemory;
        }

        // Type checking doesn't modify the IR
        return .{
            .ir = ir,
            .modified = false,
            .errors = self.errors.items,
        };
    }

    /// Get the BiChecker for accessing type information
    pub fn getBiChecker(self: *TypeCheckPass) *BiChecker {
        return &self.bi_checker;
    }

    /// Check if there were any type errors
    pub fn hasErrors(self: *const TypeCheckPass) bool {
        for (self.errors.items) |e| {
            if (e.level == .@"error") return true;
        }
        return false;
    }
};

/// Create a typecheck pass
pub fn create(allocator: std.mem.Allocator, config: TypeCheckConfig) TypeCheckPass {
    return TypeCheckPass.init(allocator, config);
}

// ============================================================================
// Tests
// ============================================================================

test "typecheck pass - literal" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var pass = TypeCheckPass.init(testing.allocator, .{});
    defer pass.deinit();

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const result = try pass.run(testing.allocator, &lit);

    try testing.expect(!pass.hasErrors());
    try testing.expect(!result.modified);
    try testing.expectEqual(&lit, result.ir);
}

test "typecheck pass - variable reference" {
    const testing = std.testing;

    var pass = TypeCheckPass.init(testing.allocator, .{});
    defer pass.deinit();

    // Reference to unbound variable
    const var_ref = Ir{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };
    const result = try pass.run(testing.allocator, &var_ref);

    // Should have an unbound variable error
    try testing.expect(pass.hasErrors());
    try testing.expect(!result.modified);
}

test "typecheck pass - addition" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var pass = TypeCheckPass.init(testing.allocator, .{});
    defer pass.deinit();

    // (+ 1 2)
    const lit1 = Ir{ .lit = Value.makeFixnum(1) };
    const lit2 = Ir{ .lit = Value.makeFixnum(2) };
    const add = Ir{ .add = .{ .left = &lit1, .right = &lit2 } };

    const result = try pass.run(testing.allocator, &add);

    try testing.expect(!pass.hasErrors());
    try testing.expect(!result.modified);
}
