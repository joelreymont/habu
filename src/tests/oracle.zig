//! Oracle tests - verify Habu against Zig reference implementations
//!
//! Computes expected results in Zig and verifies Habu produces same output.
//! All tests use single self-contained expressions (no cross-call state).

const std = @import("std");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Heap = runtime.Heap;
const harness = @import("../testing/harness.zig");
const snap = @import("../testing/snapshot.zig");

// ============================================================================
// Oracle Tests - Self-Contained Expressions
// ============================================================================

test "oracle: nested let scoping" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test proper lexical scoping with nested lets
    const expr = "(let ((x 10)) (let ((y 20)) (let ((x 5)) (+ x y))))";

    const result = try harness.eval(allocator, &heap, expr);
    // Inner x shadows outer x, so x=5, y=20, result=25
    try testing.expectEqual(@as(i64, 25), result.toFixnum());
}

test "oracle: arithmetic expressions" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test complex arithmetic expressions
    const test_cases = [_]struct { expr: []const u8, expected: i64 }{
        .{ .expr = "(+ (* 3 4) (* 5 6))", .expected = 42 },
        .{ .expr = "(- (* 10 10) (* 6 10))", .expected = 40 },
        .{ .expr = "(mod (+ 100 23) 10)", .expected = 3 },
        .{ .expr = "(+ 1 (+ 2 (+ 3 (+ 4 5))))", .expected = 15 },
        .{ .expr = "(* 2 (+ 3 (* 4 5)))", .expected = 46 },
    };

    for (test_cases) |tc| {
        const result = try harness.eval(allocator, &heap, tc.expr);
        try testing.expectEqual(tc.expected, result.toFixnum());
    }

    // Test division: 150/3 = 50 (fixnum, evenly divisible)
    {
        const result = try harness.eval(allocator, &heap, "(/ (+ 100 50) 3)");
        try testing.expect(result.isFixnum());
        try testing.expectEqual(@as(i64, 50), result.toFixnum());
    }
}

test "oracle: list length after operations" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const test_cases = [_]struct { expr: []const u8, expected: i64 }{
        .{ .expr = "(length (append (list 1 2 3) (list 4 5)))", .expected = 5 },
        .{ .expr = "(length (reverse (list 1 2 3 4 5 6 7)))", .expected = 7 },
        .{ .expr = "(length (cons 1 (list 2 3 4)))", .expected = 4 },
        .{ .expr = "(length (cdr (list 1 2 3 4 5)))", .expected = 4 },
    };

    for (test_cases) |tc| {
        const result = try harness.eval(allocator, &heap, tc.expr);
        try testing.expectEqual(tc.expected, result.toFixnum());
    }
}

test "oracle: comparison operators" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test comparisons return correct boolean-like values
    const truthy_cases = [_][]const u8{
        "(< 1 2)",
        "(<= 2 2)",
        "(> 5 3)",
        "(>= 5 5)",
        "(= 42 42)",
    };

    for (truthy_cases) |expr| {
        const result = try harness.eval(allocator, &heap, expr);
        try testing.expect(!result.isNil());
    }

    const falsy_cases = [_][]const u8{
        "(< 2 1)",
        "(<= 3 2)",
        "(> 3 5)",
        "(>= 3 5)",
        "(= 1 2)",
    };

    for (falsy_cases) |expr| {
        const result = try harness.eval(allocator, &heap, expr);
        try testing.expect(result.isNil());
    }
}

test "oracle: conditional evaluation" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const test_cases = [_]struct { expr: []const u8, expected: i64 }{
        // Basic if
        .{ .expr = "(if t 1 2)", .expected = 1 },
        .{ .expr = "(if nil 1 2)", .expected = 2 },
        // Nested if
        .{ .expr = "(if (< 1 2) (if (> 5 3) 100 200) 300)", .expected = 100 },
        // if with computed condition
        .{ .expr = "(if (= (* 2 3) 6) 42 0)", .expected = 42 },
    };

    for (test_cases) |tc| {
        const result = try harness.eval(allocator, &heap, tc.expr);
        try testing.expectEqual(tc.expected, result.toFixnum());
    }
}

test "oracle: defclass CPL includes transitive supers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const expr =
        \\(progn
        \\  (defclass A () ())
        \\  (defclass B (A) ())
        \\  (defclass C (B) ())
        \\  (list
        \\    (class-name (car (class-precedence-list (find-class 'C))))
        \\    (class-name (car (cdr (class-precedence-list (find-class 'C)))))
        \\    (class-name (car (cdr (cdr (class-precedence-list (find-class 'C))))))))
    ;

    try snap.expectHarnessEval(@src(), allocator, &heap, expr, "(C B A)");
}

test "oracle: class-slots includes inherited slots" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const expr =
        \\(progn
        \\  (defclass A () (X))
        \\  (defclass B (A) (Y))
        \\  (list
        \\    (slot-definition-name (car (class-slots (find-class 'B))))
        \\    (slot-definition-name (car (cdr (class-slots (find-class 'B)))))))
    ;

    try snap.expectHarnessEval(@src(), allocator, &heap, expr, "(Y X)");
}

test "oracle: list construction" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const test_cases = [_]struct { expr: []const u8, expected: i64 }{
        // car/cdr access
        .{ .expr = "(car (list 1 2 3))", .expected = 1 },
        .{ .expr = "(car (cdr (list 1 2 3)))", .expected = 2 },
        .{ .expr = "(car (cdr (cdr (list 1 2 3))))", .expected = 3 },
        // cons builds
        .{ .expr = "(car (cons 99 nil))", .expected = 99 },
        .{ .expr = "(car (cons 1 (cons 2 nil)))", .expected = 1 },
    };

    for (test_cases) |tc| {
        const result = try harness.eval(allocator, &heap, tc.expr);
        try testing.expectEqual(tc.expected, result.toFixnum());
    }
}

test "oracle: let bindings" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const test_cases = [_]struct { expr: []const u8, expected: i64 }{
        .{ .expr = "(let ((x 5)) x)", .expected = 5 },
        .{ .expr = "(let ((x 3) (y 4)) (+ x y))", .expected = 7 },
        .{ .expr = "(let ((x 10)) (let ((y 20)) (+ x y)))", .expected = 30 },
    };

    for (test_cases) |tc| {
        const result = try harness.eval(allocator, &heap, tc.expr);
        try testing.expectEqual(tc.expected, result.toFixnum());
    }
}

test "oracle: null and equality" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // null checks
    {
        const result = try harness.eval(allocator, &heap, "(null nil)");
        try testing.expect(!result.isNil()); // null nil => t
    }
    {
        const result = try harness.eval(allocator, &heap, "(null (list 1))");
        try testing.expect(result.isNil()); // null (list 1) => nil
    }

    // eq checks
    {
        const result = try harness.eval(allocator, &heap, "(eq nil nil)");
        try testing.expect(!result.isNil());
    }
    {
        const result = try harness.eval(allocator, &heap, "(eq t t)");
        try testing.expect(!result.isNil());
    }
}
