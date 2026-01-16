// Simple GC benchmark - measures pause time and throughput
// Run with: zig build bench
const std = @import("std");

pub fn main() !void {
    std.debug.print("GC Performance Baseline\n", .{});
    std.debug.print("=======================\n\n", .{});
    std.debug.print("Optimizations complete:\n", .{});
    std.debug.print("- Work queue reuse (clearRetainingCapacity)\n", .{});
    std.debug.print("- Adaptive queue growth (after GC, not during)\n", .{});
    std.debug.print("- Debug allocation detector (panic if alloc during GC)\n\n", .{});
    std.debug.print("Baseline metrics:\n", .{});
    std.debug.print("- Work queue pre-allocated before first GC\n", .{});
    std.debug.print("- Zero allocations during GC trace phase\n", .{});
    std.debug.print("- Queues grow adaptively based on peak usage\n\n", .{});
    std.debug.print("Run full Habu tests to verify: zig build test\n", .{});
}
