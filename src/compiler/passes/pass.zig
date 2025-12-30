//! Pass Infrastructure
//!
//! Defines the interface for nanopass transformations.
//! Each pass takes IR and produces modified IR.

const std = @import("std");
const Ir = @import("../ir.zig").Ir;
const IrBuilder = @import("../ir.zig").IrBuilder;

/// Error type for passes
pub const PassError = error{
    OutOfMemory,
    PassFailed,
    TypeError,
    InvalidIr,
};

/// Result of running a pass
pub const PassResult = struct {
    /// The transformed IR (or original if unchanged)
    ir: *const Ir,
    /// Whether the pass modified the IR
    modified: bool,
    /// Any errors encountered (pass can continue with errors)
    errors: []const PassDiagnostic,
};

/// Diagnostic message from a pass
pub const PassDiagnostic = struct {
    /// Error or warning level
    level: Level,
    /// Message describing the issue
    message: []const u8,
    /// Optional source location
    loc: ?Loc,

    pub const Level = enum {
        @"error",
        warning,
        note,
    };

    pub const Loc = struct {
        file: []const u8,
        line: u32,
        column: u32,
    };
};

/// Pass interface - implemented by each nanopass
pub const Pass = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        /// Name of the pass for diagnostics/timing
        name: []const u8,
        /// Run the pass on the given IR
        run: *const fn (ptr: *anyopaque, allocator: std.mem.Allocator, ir: *const Ir) PassError!PassResult,
        /// Clean up any pass-specific state
        deinit: *const fn (ptr: *anyopaque) void,
    };

    /// Get the pass name
    pub fn name(self: Pass) []const u8 {
        return self.vtable.name;
    }

    /// Run the pass
    pub fn run(self: Pass, allocator: std.mem.Allocator, ir: *const Ir) PassError!PassResult {
        return self.vtable.run(self.ptr, allocator, ir);
    }

    /// Deinitialize the pass
    pub fn deinit(self: Pass) void {
        self.vtable.deinit(self.ptr);
    }
};

/// Create a Pass from a concrete implementation
pub fn makePass(comptime T: type, impl: *T) Pass {
    const gen = struct {
        fn run(ptr: *anyopaque, allocator: std.mem.Allocator, ir: *const Ir) PassError!PassResult {
            const self: *T = @ptrCast(@alignCast(ptr));
            return self.run(allocator, ir);
        }

        fn deinit(ptr: *anyopaque) void {
            const self: *T = @ptrCast(@alignCast(ptr));
            self.deinit();
        }

        const vtable = Pass.VTable{
            .name = T.pass_name,
            .run = run,
            .deinit = deinit,
        };
    };

    return .{
        .ptr = impl,
        .vtable = &gen.vtable,
    };
}

/// Identity pass - returns IR unchanged (for testing)
pub const IdentityPass = struct {
    pub const pass_name = "identity";

    pub fn run(_: *IdentityPass, _: std.mem.Allocator, ir: *const Ir) PassError!PassResult {
        return .{
            .ir = ir,
            .modified = false,
            .errors = &[_]PassDiagnostic{},
        };
    }

    pub fn deinit(_: *IdentityPass) void {}
};

// ============================================================================
// Tests
// ============================================================================

test "identity pass" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var identity = IdentityPass{};
    const pass = makePass(IdentityPass, &identity);

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    const result = try pass.run(testing.allocator, &lit);

    try testing.expectEqual(&lit, result.ir);
    try testing.expect(!result.modified);
}

test "pass name" {
    var identity = IdentityPass{};
    const pass = makePass(IdentityPass, &identity);

    try std.testing.expectEqualStrings("identity", pass.name());
}
