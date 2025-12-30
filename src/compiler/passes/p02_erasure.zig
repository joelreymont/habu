//! Erasure Pass
//!
//! Removes 0-quantity (erased) terms from IR.
//! In QTT, 0-quantity variables are type-level only and
//! must not appear at runtime. This pass replaces them with nil.
//!
//! This pass:
//! - Replaces references to erased variables with nil
//! - Preserves IR structure otherwise
//! - Runs after type checking to ensure erasure is safe

const std = @import("std");
const pass_mod = @import("pass.zig");
const Pass = pass_mod.Pass;
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const PassDiagnostic = pass_mod.PassDiagnostic;
const makePass = pass_mod.makePass;
const Ir = @import("../ir.zig").Ir;
const types = @import("../../types/types.zig");
const erasure = types.erasure;
const Eraser = erasure.Eraser;
const ErasureCtx = erasure.ErasureCtx;

/// Erasure pass configuration
pub const ErasureConfig = struct {
    /// Names of variables to erase (from type checking pass)
    erased_vars: []const []const u8 = &.{},
};

/// Erasure pass - removes 0-quantity terms from IR
pub const ErasurePass = struct {
    allocator: std.mem.Allocator,
    eraser: Eraser,
    config: ErasureConfig,

    pub const pass_name = "erasure";

    pub fn init(allocator: std.mem.Allocator, config: ErasureConfig) ErasurePass {
        return .{
            .allocator = allocator,
            .eraser = Eraser.init(allocator),
            .config = config,
        };
    }

    pub fn deinit(self: *ErasurePass) void {
        self.eraser.deinit();
    }

    pub fn run(self: *ErasurePass, allocator: std.mem.Allocator, ir: *const Ir) PassError!PassResult {
        _ = allocator;

        // Build erasure context from config
        var ctx = ErasureCtx.init(self.allocator);
        defer ctx.deinit();

        for (self.config.erased_vars) |name| {
            ctx.markErased(name) catch return PassError.OutOfMemory;
        }

        // If no erased variables, return unchanged
        if (self.config.erased_vars.len == 0) {
            return .{
                .ir = ir,
                .modified = false,
                .errors = &[_]PassDiagnostic{},
            };
        }

        // Run erasure
        const erased_ir = self.eraser.erase(ir, &ctx) catch |err| {
            return switch (err) {
                error.OutOfMemory => PassError.OutOfMemory,
            };
        };

        // Check if IR was modified
        const modified = erased_ir != ir;

        return .{
            .ir = erased_ir,
            .modified = modified,
            .errors = &[_]PassDiagnostic{},
        };
    }

    /// Set the list of erased variables (called by typecheck pass)
    pub fn setErasedVars(self: *ErasurePass, vars: []const []const u8) void {
        self.config.erased_vars = vars;
    }
};

/// Create an erasure pass
pub fn create(allocator: std.mem.Allocator, config: ErasureConfig) ErasurePass {
    return ErasurePass.init(allocator, config);
}

// ============================================================================
// Tests
// ============================================================================

test "erasure pass - no erased vars" {
    const testing = std.testing;
    const runtime = @import("../../runtime/runtime.zig");

    var pass = ErasurePass.init(testing.allocator, .{});
    defer pass.deinit();

    const lit = Ir{ .lit = runtime.Value.makeFixnum(42) };
    const result = try pass.run(testing.allocator, &lit);

    try testing.expect(!result.modified);
    try testing.expectEqual(&lit, result.ir);
}

test "erasure pass - with erased var" {
    const testing = std.testing;

    var pass = ErasurePass.init(testing.allocator, .{
        .erased_vars = &.{"x"},
    });
    defer pass.deinit();

    // Reference to erased variable x
    const var_ref = Ir{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };
    const result = try pass.run(testing.allocator, &var_ref);

    // Should be modified (replaced with nil)
    try testing.expect(result.modified);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.ir.*));
    try testing.expect(result.ir.lit.isNil());

    // Clean up allocated nil node
    testing.allocator.destroy(@constCast(result.ir));
}

test "erasure pass - preserve non-erased var" {
    const testing = std.testing;

    var pass = ErasurePass.init(testing.allocator, .{
        .erased_vars = &.{"x"},
    });
    defer pass.deinit();

    // Reference to non-erased variable y
    const var_ref = Ir{ .@"var" = .{ .name = "y", .depth = 0, .index = 0 } };
    const result = try pass.run(testing.allocator, &var_ref);

    // Should not be modified
    try testing.expect(!result.modified);
    try testing.expectEqual(&var_ref, result.ir);
}
