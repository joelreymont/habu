//! VM vs JIT parity tests

const std = @import("std");
const builtin = @import("builtin");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Heap = runtime.Heap;
const Value = runtime.Value;
const Cons = runtime.Cons;

const harness = @import("../testing/harness.zig");
const Runner = harness.Runner;

fn buildListSource(allocator: std.mem.Allocator, cases: []const []const u8) ![]u8 {
    var buf = std.ArrayList(u8){};
    errdefer buf.deinit(allocator);

    try buf.appendSlice(allocator, "(list");
    for (cases) |src| {
        try buf.append(allocator, ' ');
        try buf.appendSlice(allocator, src);
    }
    try buf.append(allocator, ')');
    return try buf.toOwnedSlice(allocator);
}

fn runExprParity(p: *Parity, source: []const u8) !void {
    const chunk = try p.r.compile(source);
    var roots = [_]Value{Value.makeChunk(chunk)};
    p.r.vm.setExtRoots(roots[0..]);
    defer p.r.vm.clearExtRoots();

    const saved_jit_on = p.r.vm.jit_on;
    p.r.vm.jit_on = false;
    defer p.r.vm.jit_on = saved_jit_on;
    const vm_val = try p.r.run(chunk);

    const vm_s = try harness.valueToString(p.allocator, vm_val);
    defer p.allocator.free(vm_s);

    const st0 = p.r.vm.jitStats();
    p.r.vm.jit_on = true;
    const jit_chunk = roots[0].toPtr(runtime.Chunk);
    const jit_val = try p.r.run(jit_chunk);
    const st1 = p.r.vm.jitStats();

    try testing.expectEqual(st0.fail_n, st1.fail_n);
    try testing.expectEqual(st0.compile_n + 1, st1.compile_n);

    const jit_s = try harness.valueToString(p.allocator, jit_val);
    defer p.allocator.free(jit_s);

    try testing.expectEqualStrings(vm_s, jit_s);
}

const Parity = struct {
    allocator: std.mem.Allocator,

    heap: Heap,
    r: Runner,

    pub fn init(allocator: std.mem.Allocator) !Parity {
        var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
        errdefer heap.deinit();

        var r = try Runner.init(allocator, &heap);
        errdefer r.deinit();

        try r.enableJit(64 * 1024 * 1024, 1);

        return .{
            .allocator = allocator,
            .heap = heap,
            .r = r,
        };
    }

    pub fn deinit(self: *Parity) void {
        self.r.deinit();
        self.heap.deinit();
    }

    pub fn runCases(self: *Parity, cases: []const []const u8, source: []const u8) !void {
        const chunk = try self.r.compile(source);
        var roots = [_]Value{Value.makeChunk(chunk)};
        self.r.vm.setExtRoots(roots[0..]);
        defer self.r.vm.clearExtRoots();

        const saved_jit_on = self.r.vm.jit_on;
        self.r.vm.jit_on = false;
        defer self.r.vm.jit_on = saved_jit_on;

        const vm_val = try self.r.run(chunk);

        var vm_strs = std.ArrayList([]u8){};
        errdefer {
            for (vm_strs.items) |s| self.allocator.free(s);
            vm_strs.deinit(self.allocator);
        }
        try vm_strs.ensureTotalCapacity(self.allocator, cases.len);

        var vm_list = vm_val;
        for (cases, 0..) |src, i| {
            _ = src;
            if (!vm_list.isCons()) {
                const got = try harness.valueToString(self.allocator, vm_list);
                defer self.allocator.free(got);
                std.debug.panic("parity: vm result not list at idx {d}: {s}", .{ i, got });
            }
            const cons = vm_list.toPtr(Cons);
            const s = try harness.valueToString(self.allocator, cons.car);
            vm_strs.appendAssumeCapacity(s);
            vm_list = cons.cdr;
        }
        if (!vm_list.isNil()) {
            const got = try harness.valueToString(self.allocator, vm_list);
            defer self.allocator.free(got);
            std.debug.panic("parity: vm result has extra tail: {s}", .{got});
        }
        defer {
            for (vm_strs.items) |s| self.allocator.free(s);
            vm_strs.deinit(self.allocator);
        }

        const st0 = self.r.vm.jitStats();
        self.r.vm.jit_on = true;
        const jit_chunk = roots[0].toPtr(runtime.Chunk);
        const jit_val = try self.r.run(jit_chunk);
        const st1 = self.r.vm.jitStats();

        try testing.expectEqual(st0.fail_n, st1.fail_n);
        try testing.expectEqual(st0.compile_n + 1, st1.compile_n);

        var jit_list = jit_val;
        for (cases, 0..) |src, i| {
            if (!jit_list.isCons()) {
                const got = try harness.valueToString(self.allocator, jit_list);
                defer self.allocator.free(got);
                std.debug.panic("parity: jit result not list at idx {d}: {s}", .{ i, got });
            }
            const cons = jit_list.toPtr(Cons);
            const jit_s = try harness.valueToString(self.allocator, cons.car);
            defer self.allocator.free(jit_s);

            const vm_s = vm_strs.items[i];
            if (!std.mem.eql(u8, vm_s, jit_s)) {
                std.debug.panic("parity mismatch at {d}: {s}\nvm: {s}\njit: {s}", .{ i, src, vm_s, jit_s });
            }
            jit_list = cons.cdr;
        }
        if (!jit_list.isNil()) {
            const got = try harness.valueToString(self.allocator, jit_list);
            defer self.allocator.free(got);
            std.debug.panic("parity: jit result has extra tail: {s}", .{got});
        }
    }
};

test "parity: vm vs jit (single add)" {
    if (builtin.cpu.arch != .aarch64) return;

    var p = try Parity.init(testing.allocator);
    defer p.deinit();

    try runExprParity(&p, "(+ 1 2)");
}

test "parity: vm vs jit (single list)" {
    if (builtin.cpu.arch != .aarch64) return;

    var p = try Parity.init(testing.allocator);
    defer p.deinit();

    try runExprParity(&p, "(list (+ 1 2) 4)");
}

test "parity: vm vs jit (make_list 64)" {
    if (builtin.cpu.arch != .aarch64) return;

    var p = try Parity.init(testing.allocator);
    defer p.deinit();

    var buf = std.ArrayList(u8){};
    errdefer buf.deinit(testing.allocator);
    try buf.appendSlice(testing.allocator, "(list");
    for (0..64) |_| {
        try buf.appendSlice(testing.allocator, " (+ 1 2)");
    }
    try buf.append(testing.allocator, ')');

    const src = try buf.toOwnedSlice(testing.allocator);
    defer testing.allocator.free(src);
    try runExprParity(&p, src);
}

test "parity: vm vs jit (hand-picked)" {
    if (builtin.cpu.arch != .aarch64) return;

    var p = try Parity.init(testing.allocator);
    defer p.deinit();

    const cases = [_][]const u8{
        "(+ 1 2)",
        "(- 10 3)",
        "(* 6 7)",
        "(mod 10 3)",
        "(= 1 1)",
        "(= 1 2)",
        "(eql 0.0 -0.0)",
        "(equal (list 1 2) (list 1 2))",
        "(not nil)",
        "(not t)",
        "(null nil)",
        "(null (list 1))",
        "(numberp 42)",
        "(numberp 'x)",
        "(integerp 42)",
        "(integerp 1.5)",
        "(realp 42)",
        "(realp 1.5)",
        "(consp (cons 1 2))",
        "(symbolp 'x)",
        "(symbolp nil)",
        "(symbolp t)",
        "(stringp \"a\")",
        "(vectorp (make-vector 1 0))",
        "(keywordp :a)",
        "(closurep (lambda (x) x))",
        "(characterp #\\a)",
        "(char-code #\\a)",
        "(code-char 97)",
        "(char= #\\a #\\a)",
        "(char< #\\a #\\b)",
        "(char> #\\b #\\a)",
        "(char-upcase #\\a)",
        "(char-downcase #\\A)",
        "(digit-char-p #\\7)",
        "(alpha-char-p #\\Z)",
        "(string-upcase \"aBc\")",
        "(string-downcase \"AbC\")",
        "(string-length \"abc\")",
        "(char \"abc\" 1)",
        "(string-concat \"ab\" \"cd\")",
        "(let ((s (string-concat \"a\" \"b\"))) (%sset s 1 #\\x) s)",
        "(string= \"a\" \"a\")",
        "(string< \"a\" \"b\")",
        "(string> \"b\" \"a\")",
        "(string<= \"a\" \"a\")",
        "(string>= \"b\" \"a\")",
        "(write-to-string (list 1 2))",
        "(progn (random-seed 123) (random 10))",
        "(floatp 1.5)",
        "(listp nil)",
        "(listp (cons 1 2))",
        "(atom 1)",
        "(atom (cons 1 2))",
        "(length (list 1 2 3 4))",
        "(length (make-vector 3 7))",
        "(length \"abc\")",
        "(vector-length (make-vector 4 7))",
        "(let ((v (make-vector 3 0))) (%fill-pointer v))",
        "(let ((v (make-vector 3 0))) (%set-fill-pointer v 0) (%vector-push v 9))",
        "(let ((v (make-vector 0 0))) (%set-adjustable v t) (%vector-push-extend v 9 0) (%vector-pop v))",
        "(let ((v (make-vector 2 1))) (%adjust-array v 3 9) (vector-length v))",
        "(svref (vector 10 20 30) 1)",
        "(let ((v (make-vector 3 0))) (setf (svref v 1) 9) (svref v 1))",
        "(member 2 (list 1 2 3))",
        "(member 1.0 (list 0.0 1.0) :test 'eql)",
        "(member \"a\" (list \"b\" \"a\") :test 'equal)",
        "(assoc 'b '((a . 1) (b . 2)))",
        "(assoc 1.0 '((0.0 . 1) (1.0 . 2)) :test 'eql)",
        "(assoc \"b\" '((\"a\" . 1) (\"b\" . 2)) :test 'equal)",
        "(find 2 (list 1 2 3))",
        "(find 1.0 (list 0.0 1.0) :test 'eql)",
        "(find \"a\" (list \"b\" \"a\") :test 'equal)",
        "(position 2 (list 1 2 3))",
        "(count 2 (list 1 2 2 3))",
        "(count \"a\" (list \"a\" \"b\" \"a\") :test 'equal)",
        "(remove 2 (list 1 2 3 2))",
        "(remove \"a\" (list \"a\" \"b\" \"a\") :test 'equal)",
        "(last (list 1 2 3))",
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

    const src = try buildListSource(testing.allocator, cases[0..]);
    defer testing.allocator.free(src);
    try p.runCases(cases[0..], src);
}

test "parity: vm vs jit (random arith)" {
    if (builtin.cpu.arch != .aarch64) return;

    var p = try Parity.init(testing.allocator);
    defer p.deinit();

    var rng = std.Random.DefaultPrng.init(0x2c0f7d11);
    const random = rng.random();

    var exprs = std.ArrayList([]const u8){};
    errdefer {
        for (exprs.items) |s| testing.allocator.free(s);
        exprs.deinit(testing.allocator);
    }
    try exprs.ensureTotalCapacity(testing.allocator, 200);

    for (0..200) |_| {
        const a = random.intRangeAtMost(i32, -10_000, 10_000);
        const b = random.intRangeAtMost(i32, -10_000, 10_000);
        const c = random.intRangeAtMost(i32, -10_000, 10_000);

        const ops = [_][]const u8{ "+", "-", "*" };
        const op1 = ops[random.uintLessThan(usize, ops.len)];
        const op2 = ops[random.uintLessThan(usize, ops.len)];

        var buf: [256]u8 = undefined;
        const src_tmp = try std.fmt.bufPrint(
            &buf,
            "(let ((x {d}) (y {d}) (z {d})) ({s} ({s} x y) z))",
            .{ a, b, c, op2, op1 },
        );

        const src = try testing.allocator.dupe(u8, src_tmp);
        exprs.appendAssumeCapacity(src);
    }
    defer {
        for (exprs.items) |s| testing.allocator.free(s);
        exprs.deinit(testing.allocator);
    }

    const src = try buildListSource(testing.allocator, exprs.items);
    defer testing.allocator.free(src);
    try p.runCases(exprs.items, src);
}
