//! VM vs JIT parity tests

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Heap = runtime.Heap;

const harness = @import("../testing/harness.zig");
const Runner = harness.Runner;

fn runVmAndJit(allocator: std.mem.Allocator, source: []const u8) !struct { vm: []u8, jit: []u8 } {
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var r = try Runner.init(allocator, &heap);
    defer r.deinit();

    const chunk = try r.compile(source);

    const vm_val = try r.run(chunk);
    const vm_s = try harness.valueToString(allocator, vm_val);
    errdefer allocator.free(vm_s);

    const st0 = r.vm.jitStats();
    try r.enableJit(64 * 1024 * 1024, 1);
    const jit_val = try r.run(chunk);
    const st1 = r.vm.jitStats();

    try testing.expectEqual(st0.fail_n, st1.fail_n);
    try testing.expectEqual(st0.compile_n + 1, st1.compile_n);

    const jit_s = try harness.valueToString(allocator, jit_val);
    errdefer allocator.free(jit_s);

    return .{ .vm = vm_s, .jit = jit_s };
}

test "parity: vm vs jit (hand-picked)" {
    if (builtin.cpu.arch != .aarch64) return;

    const cases = [_][]const u8{
        "(+ 1 2)",
        "(- 10 3)",
        "(* 6 7)",
        "(mod 10 3)",
        "(= 1 1)",
        "(= 1 2)",
        "(not nil)",
        "(not t)",
        "(null nil)",
        "(null (list 1))",
        "(numberp 42)",
        "(numberp 'x)",
        "(consp (cons 1 2))",
        "(symbolp 'x)",
        "(stringp \"a\")",
        "(vectorp (make-vector 1 0))",
        "(keywordp :a)",
        "(closurep (lambda (x) x))",
        "(characterp #\\a)",
        "(floatp 1.5)",
        "(listp nil)",
        "(listp (cons 1 2))",
        "(atom 1)",
        "(atom (cons 1 2))",
        "(length (list 1 2 3 4))",
        "(length (make-vector 3 7))",
        "(length \"abc\")",
        "(vector-length (make-vector 4 7))",
        "(svref (vector 10 20 30) 1)",
        "(let ((v (make-vector 3 0))) (setf (svref v 1) 9) (svref v 1))",
        "(reverse (list 1 2 3))",
        "(append (list 1 2) (list 3 4))",
        "(nth 1 (list 7 8 9))",
        "(nthcdr 2 (list 1 2 3 4))",
        "(let ((x (cons 1 2))) (rplaca x 9) (car x))",
        "(let ((x (cons 1 2))) (rplacd x (list 3 4)) (cdr x))",
        "(if (numberp 1) 42 0)",
        "(let ((x 10) (y 20)) (+ x y))",
        "(cons 1 2)",
        "(car (cons 1 2))",
        "(cdr (cons 1 2))",
        "(car (list 1 2))",
        "(cdr (list 1 2))",
        "(list 1 2 3)",
        "(let ((xs (list 1 2 3))) (car xs))",
        "(let ((xs (list 1 2 3))) (cdr xs))",
        "(let ((i 0) (acc 0)) (while (< i 2000) (setq acc (+ acc i)) (setq i (+ i 1))) acc)",
        "(let ((x 1)) (setq x (+ x 2)) x)",
        "(let ((x 1) (y 2)) (if (< x y) (+ x y) (- x y)))",
        "(make-vector 3 7)",
    };

    for (cases) |src| {
        const res = try runVmAndJit(testing.allocator, src);
        defer testing.allocator.free(res.vm);
        defer testing.allocator.free(res.jit);
        try testing.expectEqualStrings(res.vm, res.jit);
    }
}

test "parity: vm vs jit (random arith)" {
    if (builtin.cpu.arch != .aarch64) return;

    var rng = std.Random.DefaultPrng.init(0x2c0f7d11);
    const random = rng.random();

    for (0..200) |_| {
        const a = random.intRangeAtMost(i32, -10_000, 10_000);
        const b = random.intRangeAtMost(i32, -10_000, 10_000);
        const c = random.intRangeAtMost(i32, -10_000, 10_000);

        const ops = [_][]const u8{ "+", "-", "*" };
        const op1 = ops[random.uintLessThan(usize, ops.len)];
        const op2 = ops[random.uintLessThan(usize, ops.len)];

        var buf: [256]u8 = undefined;
        const src = try std.fmt.bufPrint(
            &buf,
            "(let ((x {d}) (y {d}) (z {d})) ({s} ({s} x y) z))",
            .{ a, b, c, op2, op1 },
        );

        const res = try runVmAndJit(testing.allocator, src);
        defer testing.allocator.free(res.vm);
        defer testing.allocator.free(res.jit);
        try testing.expectEqualStrings(res.vm, res.jit);
    }
}
