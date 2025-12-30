//! Compilation Pipeline
//!
//! Orchestrates the sequence of nanopass transformations:
//!   Lisp S-expr → IR → TypeCheck → Erase → Bytecode

const std = @import("std");
const pass_mod = @import("pass.zig");
const Pass = pass_mod.Pass;
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const PassDiagnostic = pass_mod.PassDiagnostic;
const Ir = @import("../ir.zig").Ir;

/// Pipeline configuration
pub const PipelineConfig = struct {
    /// Enable timing output
    timing: bool = false,
    /// Enable debug output
    debug: bool = false,
    /// Stop on first error
    fail_fast: bool = true,
};

/// Pipeline execution statistics
pub const PipelineStats = struct {
    /// Total time in nanoseconds
    total_ns: u64,
    /// Per-pass timing
    pass_times: []const PassTime,

    pub const PassTime = struct {
        name: []const u8,
        time_ns: u64,
    };
};

/// Pipeline manages a sequence of passes
pub const Pipeline = struct {
    allocator: std.mem.Allocator,
    passes: std.ArrayListUnmanaged(Pass),
    config: PipelineConfig,

    pub fn init(allocator: std.mem.Allocator, config: PipelineConfig) Pipeline {
        return .{
            .allocator = allocator,
            .passes = .{},
            .config = config,
        };
    }

    pub fn deinit(self: *Pipeline) void {
        for (self.passes.items) |pass| {
            pass.deinit();
        }
        self.passes.deinit(self.allocator);
    }

    /// Add a pass to the pipeline
    pub fn addPass(self: *Pipeline, pass: Pass) !void {
        try self.passes.append(self.allocator, pass);
    }

    /// Run all passes on the given IR
    pub fn run(self: *Pipeline, ir: *const Ir) !PipelineResult {
        var timer = std.time.Timer.start() catch return error.OutOfMemory;
        var pass_times = std.ArrayListUnmanaged(PipelineStats.PassTime){};
        var all_diagnostics = std.ArrayListUnmanaged(PassDiagnostic){};
        var current_ir = ir;
        var has_errors = false;

        for (self.passes.items) |pass| {
            const pass_start = timer.lap();

            const result = pass.run(self.allocator, current_ir) catch |err| {
                if (self.config.debug) {
                    std.debug.print("[pipeline] {s} failed: {any}\n", .{ pass.name(), err });
                }
                return error.PassFailed;
            };

            const pass_time = timer.lap();

            try pass_times.append(self.allocator, .{
                .name = pass.name(),
                .time_ns = pass_time - pass_start,
            });

            // Collect diagnostics
            for (result.errors) |diag| {
                try all_diagnostics.append(self.allocator, diag);
                if (diag.level == .@"error") {
                    has_errors = true;
                }
            }

            if (self.config.debug and result.modified) {
                std.debug.print("[pipeline] {s}: IR modified\n", .{pass.name()});
            }

            current_ir = result.ir;

            if (has_errors and self.config.fail_fast) {
                break;
            }
        }

        const total_time = timer.read();

        if (self.config.timing) {
            self.printTiming(pass_times.items, total_time);
        }

        return .{
            .ir = current_ir,
            .diagnostics = try all_diagnostics.toOwnedSlice(self.allocator),
            .stats = .{
                .total_ns = total_time,
                .pass_times = try pass_times.toOwnedSlice(self.allocator),
            },
            .success = !has_errors,
        };
    }

    fn printTiming(self: *Pipeline, pass_times: []const PipelineStats.PassTime, total_ns: u64) void {
        _ = self;
        std.debug.print("[pipeline] timing:\n", .{});
        for (pass_times) |pt| {
            const ms = @as(f64, @floatFromInt(pt.time_ns)) / 1_000_000.0;
            std.debug.print("  {s}: {d:.2}ms\n", .{ pt.name, ms });
        }
        const total_ms = @as(f64, @floatFromInt(total_ns)) / 1_000_000.0;
        std.debug.print("  total: {d:.2}ms\n", .{total_ms});
    }
};

/// Result of running the pipeline
pub const PipelineResult = struct {
    /// Final transformed IR
    ir: *const Ir,
    /// All diagnostics from all passes
    diagnostics: []const PassDiagnostic,
    /// Execution statistics
    stats: PipelineStats,
    /// Whether compilation succeeded (no errors)
    success: bool,

    pub fn deinit(self: *PipelineResult, allocator: std.mem.Allocator) void {
        allocator.free(self.diagnostics);
        allocator.free(self.stats.pass_times);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "empty pipeline" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var pipeline = Pipeline.init(testing.allocator, .{});
    defer pipeline.deinit();

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var result = try pipeline.run(&lit);
    defer result.deinit(testing.allocator);

    try testing.expectEqual(&lit, result.ir);
    try testing.expect(result.success);
}

test "pipeline with identity pass" {
    const testing = std.testing;
    const Value = @import("../../runtime/value.zig").Value;

    var pipeline = Pipeline.init(testing.allocator, .{});
    defer pipeline.deinit();

    var identity = pass_mod.IdentityPass{};
    try pipeline.addPass(pass_mod.makePass(pass_mod.IdentityPass, &identity));

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var result = try pipeline.run(&lit);
    defer result.deinit(testing.allocator);

    try testing.expectEqual(&lit, result.ir);
    try testing.expect(result.success);
    try testing.expectEqual(@as(usize, 1), result.stats.pass_times.len);
}
