//! JIT tiering tests

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Heap = runtime.Heap;

const harness = @import("../testing/harness.zig");
const Runner = harness.Runner;

test "jit tiering hot threshold" {
    if (builtin.cpu.arch != .aarch64) return;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var r: Runner = undefined;
    try r.init(testing.allocator, &heap);
    defer r.deinit();

    const chunk = try r.compile("(+ 1 2)");

    const st0 = r.vm.jitStats();
    try r.enableJit(64 * 1024 * 1024, 3);

    const v1 = try r.run(chunk);
    try testing.expect(v1.isFixnum());
    try testing.expectEqual(@as(i64, 3), v1.toFixnum());
    const st1 = r.vm.jitStats();
    try testing.expectEqual(st0.compile_n, st1.compile_n);
    try testing.expectEqual(st0.fail_n, st1.fail_n);

    const v2 = try r.run(chunk);
    try testing.expect(v2.isFixnum());
    try testing.expectEqual(@as(i64, 3), v2.toFixnum());
    const st2 = r.vm.jitStats();
    try testing.expectEqual(st0.compile_n, st2.compile_n);
    try testing.expectEqual(st0.fail_n, st2.fail_n);

    const v3 = try r.run(chunk);
    try testing.expect(v3.isFixnum());
    try testing.expectEqual(@as(i64, 3), v3.toFixnum());
    const st3 = r.vm.jitStats();
    try testing.expectEqual(st0.compile_n + 1, st3.compile_n);
    try testing.expectEqual(st0.fail_n, st3.fail_n);

    const v4 = try r.run(chunk);
    try testing.expect(v4.isFixnum());
    try testing.expectEqual(@as(i64, 3), v4.toFixnum());
    const st4 = r.vm.jitStats();
    try testing.expectEqual(st3.compile_n, st4.compile_n);
    try testing.expectEqual(st3.fail_n, st4.fail_n);
}
