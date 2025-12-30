//! Compilation Pipeline
//!
//! Orchestrates the sequence of nanopass transformations.
//! Each pass transforms one IR type to another.
//!
//! Pipeline stages:
//!   Source → Lex → Parse → Expand → Desugar → Lift → Resolve → Capture →
//!   Annotate → Infer → Check → Linear → Erase → Emit → (JIT)

const std = @import("std");
const pass_mod = @import("pass.zig");
const Pass = pass_mod.Pass;
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const Diagnostic = pass_mod.Diagnostic;
const Ir = @import("../ir.zig").Ir;
const ir_types = @import("ir_types.zig");
const TypedIr = ir_types.TypedIr;
const Value = @import("../../runtime/value.zig").Value;

/// Pipeline statistics
pub const Stats = struct {
    total_ns: u64,
    pass_times: []const PassTime,

    pub const PassTime = struct {
        name: []const u8,
        time_ns: u64,
    };
};

/// Pipeline configuration
pub const Config = struct {
    /// Print timing information
    timing: bool = false,
    /// Print debug information
    debug: bool = false,
};

/// A heterogeneous pipeline that chains passes with different IR types.
/// Uses comptime to build the pipeline with type-safe transitions.
pub fn Pipeline(comptime stages: []const type) type {
    return struct {
        const Self = @This();

        allocator: std.mem.Allocator,
        config: Config,

        pub fn init(allocator: std.mem.Allocator, config: Config) Self {
            return .{
                .allocator = allocator,
                .config = config,
            };
        }

        /// Run the pipeline on input, returning final output
        pub fn run(self: Self, input: stages[0]) PassError!stages[stages.len - 1] {
            // This is a placeholder - actual implementation would chain passes
            _ = self;
            _ = input;
            @compileError("Pipeline.run not yet implemented - use runPasses instead");
        }
    };
}

/// Simple homogeneous pipeline for Ir → Ir passes
pub const IrPipeline = struct {
    allocator: std.mem.Allocator,
    config: Config,
    passes: std.ArrayListUnmanaged(pass_mod.IrPass),

    pub fn init(allocator: std.mem.Allocator, config: Config) IrPipeline {
        return .{
            .allocator = allocator,
            .config = config,
            .passes = .{},
        };
    }

    pub fn deinit(self: *IrPipeline) void {
        self.passes.deinit(self.allocator);
    }

    pub fn addPass(self: *IrPipeline, p: pass_mod.IrPass) !void {
        try self.passes.append(self.allocator, p);
    }

    pub fn run(self: *IrPipeline, input: *const Ir) PassError!PipelineResult {
        var timer = std.time.Timer.start() catch return PassError.PassFailed;
        var pass_times = std.ArrayListUnmanaged(Stats.PassTime){};
        var all_diagnostics = std.ArrayListUnmanaged(Diagnostic){};
        var current = input;
        var any_modified = false;

        for (self.passes.items) |p| {
            _ = timer.lap(); // Reset lap counter

            const result = try p.run(self.allocator, current);

            const pass_time = timer.lap();
            try pass_times.append(self.allocator, .{
                .name = p.name,
                .time_ns = pass_time,
            });

            for (result.diagnostics) |d| {
                try all_diagnostics.append(self.allocator, d);
            }

            if (result.modified) {
                any_modified = true;
                current = result.output;
            }

            if (self.config.debug) {
                std.debug.print("[pipeline] {s}: modified={}\n", .{ p.name, result.modified });
            }
        }

        const total_time = timer.read();

        if (self.config.timing) {
            self.printTiming(pass_times.items, total_time);
        }

        return .{
            .output = current,
            .modified = any_modified,
            .diagnostics = try all_diagnostics.toOwnedSlice(self.allocator),
            .stats = .{
                .total_ns = total_time,
                .pass_times = try pass_times.toOwnedSlice(self.allocator),
            },
        };
    }

    fn printTiming(_: *IrPipeline, pass_times: []const Stats.PassTime, total_ns: u64) void {
        std.debug.print("[pipeline] timing:\n", .{});
        for (pass_times) |pt| {
            const ms = @as(f64, @floatFromInt(pt.time_ns)) / 1_000_000.0;
            std.debug.print("  {s}: {d:.3}ms\n", .{ pt.name, ms });
        }
        const total_ms = @as(f64, @floatFromInt(total_ns)) / 1_000_000.0;
        std.debug.print("  total: {d:.3}ms\n", .{total_ms});
    }
};

/// Result of running a pipeline
pub const PipelineResult = struct {
    output: *const Ir,
    modified: bool,
    diagnostics: []const Diagnostic,
    stats: Stats,

    pub fn deinit(self: *PipelineResult, allocator: std.mem.Allocator) void {
        allocator.free(self.diagnostics);
        allocator.free(self.stats.pass_times);
    }

    pub fn hasErrors(self: *const PipelineResult) bool {
        for (self.diagnostics) |d| {
            if (d.level == .@"error") return true;
        }
        return false;
    }
};

// Note: runPasses helper for heterogeneous pass chains is not yet implemented.
// Use IrPipeline for homogeneous Ir→Ir pass chains.

// ============================================================================
// Tests
// ============================================================================

test "IrPipeline empty" {
    const testing = std.testing;

    var pipeline = IrPipeline.init(testing.allocator, .{});
    defer pipeline.deinit();

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var result = try pipeline.run(&lit);
    defer result.deinit(testing.allocator);

    try testing.expectEqual(&lit, result.output);
    try testing.expect(!result.modified);
}

test "IrPipeline with identity pass" {
    const testing = std.testing;

    var pipeline = IrPipeline.init(testing.allocator, .{});
    defer pipeline.deinit();

    try pipeline.addPass(pass_mod.identityPass(*const Ir));

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var result = try pipeline.run(&lit);
    defer result.deinit(testing.allocator);

    try testing.expectEqual(&lit, result.output);
    try testing.expect(!result.modified);
}
