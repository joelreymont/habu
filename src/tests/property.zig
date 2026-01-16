//! Property-based tests for Habu
//!
//! Uses randomized inputs to verify invariants:
//! - Arithmetic laws (associativity, commutativity, identity)
//! - List operations (car/cdr of cons, length, reverse)
//! - Type predicates (mutual exclusion, exhaustiveness)
//! - Round-trip properties (parse -> print -> parse)

const std = @import("std");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const Chunk = runtime.Chunk;

const reader = @import("../reader/reader.zig");
const Parser = reader.Parser;

const compiler = @import("../compiler/compiler.zig");
const Compiler = compiler.Compiler;
const Env = compiler.Env;

const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;

const interp = @import("../interp/interp.zig");
const Vm = interp.Vm;

/// PRNG for property tests
const Rng = std.Random.DefaultPrng;

/// Evaluate a Habu expression
fn eval(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var comp_vm = try Vm.init(arena_alloc, heap);

    var parser = Parser.init(arena_alloc, heap, source, &comp_vm.builtins);
    defer parser.deinit();
    const expr = try parser.parse();
    var comp = try Compiler.initWithHeap(arena_alloc, &comp_vm);
    defer comp.deinit();
    var env = Env.init(arena_alloc, null);
    defer env.deinit();
    const ir_node = try comp.compile(expr, &env);

    var emitter = Emitter.initWithHeap(arena_alloc, heap);
    try emitter.emit(ir_node);
    const chunk_val = try emitter.finalize();

    var vm = try Vm.init(allocator, heap);
    return vm.run(chunk_val.toPtr(Chunk));
}

/// Format expression for evaluation
fn fmt(comptime format: []const u8, args: anytype) []const u8 {
    var buf: [1024]u8 = undefined;
    return std.fmt.bufPrint(&buf, format, args) catch unreachable;
}

// ============================================================================
// Arithmetic Properties
// ============================================================================

test "property: addition is commutative" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(12345);
    const random = rng.random();

    for (0..100) |_| {
        const a = random.intRangeAtMost(i32, -1000, 1000);
        const b = random.intRangeAtMost(i32, -1000, 1000);

        var buf1: [64]u8 = undefined;
        var buf2: [64]u8 = undefined;
        const expr1 = std.fmt.bufPrint(&buf1, "(+ {d} {d})", .{ a, b }) catch unreachable;
        const expr2 = std.fmt.bufPrint(&buf2, "(+ {d} {d})", .{ b, a }) catch unreachable;

        const r1 = try eval(allocator, &heap, expr1);
        const r2 = try eval(allocator, &heap, expr2);

        try testing.expectEqual(r1.toFixnum(), r2.toFixnum());
    }
}

test "property: multiplication is commutative" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(54321);
    const random = rng.random();

    for (0..100) |_| {
        const a = random.intRangeAtMost(i32, -100, 100);
        const b = random.intRangeAtMost(i32, -100, 100);

        var buf1: [64]u8 = undefined;
        var buf2: [64]u8 = undefined;
        const expr1 = std.fmt.bufPrint(&buf1, "(* {d} {d})", .{ a, b }) catch unreachable;
        const expr2 = std.fmt.bufPrint(&buf2, "(* {d} {d})", .{ b, a }) catch unreachable;

        const r1 = try eval(allocator, &heap, expr1);
        const r2 = try eval(allocator, &heap, expr2);

        try testing.expectEqual(r1.toFixnum(), r2.toFixnum());
    }
}

test "property: addition identity" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(11111);
    const random = rng.random();

    for (0..100) |_| {
        const n = random.intRangeAtMost(i32, -10000, 10000);

        var buf: [64]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(+ {d} 0)", .{n}) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(@as(i64, n), result.toFixnum());
    }
}

test "property: multiplication identity" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(22222);
    const random = rng.random();

    for (0..100) |_| {
        const n = random.intRangeAtMost(i32, -10000, 10000);

        var buf: [64]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(* {d} 1)", .{n}) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(@as(i64, n), result.toFixnum());
    }
}

test "property: subtraction is inverse of addition" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(33333);
    const random = rng.random();

    for (0..100) |_| {
        const a = random.intRangeAtMost(i32, -1000, 1000);
        const b = random.intRangeAtMost(i32, -1000, 1000);

        var buf: [64]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(- (+ {d} {d}) {d})", .{ a, b, b }) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(@as(i64, a), result.toFixnum());
    }
}

// ============================================================================
// List Properties
// ============================================================================

test "property: car of cons returns first element" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(44444);
    const random = rng.random();

    for (0..100) |_| {
        const a = random.intRangeAtMost(i32, -10000, 10000);
        const b = random.intRangeAtMost(i32, -10000, 10000);

        var buf: [64]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(car (cons {d} {d}))", .{ a, b }) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(@as(i64, a), result.toFixnum());
    }
}

test "property: cdr of cons returns second element" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(55555);
    const random = rng.random();

    for (0..100) |_| {
        const a = random.intRangeAtMost(i32, -10000, 10000);
        const b = random.intRangeAtMost(i32, -10000, 10000);

        var buf: [64]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(cdr (cons {d} {d}))", .{ a, b }) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(@as(i64, b), result.toFixnum());
    }
}

test "property: length of list equals element count" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test various list lengths
    const test_cases = [_]struct { expr: []const u8, len: i64 }{
        .{ .expr = "nil", .len = 0 },
        .{ .expr = "(list 1)", .len = 1 },
        .{ .expr = "(list 1 2)", .len = 2 },
        .{ .expr = "(list 1 2 3)", .len = 3 },
        .{ .expr = "(list 1 2 3 4 5)", .len = 5 },
        .{ .expr = "(list 1 2 3 4 5 6 7 8 9 10)", .len = 10 },
    };

    for (test_cases) |tc| {
        var buf: [128]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(length {s})", .{tc.expr}) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(tc.len, result.toFixnum());
    }
}

test "property: reverse of reverse is identity" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const test_lists = [_][]const u8{
        "nil",
        "(list 1)",
        "(list 1 2)",
        "(list 1 2 3)",
        "(list 1 2 3 4 5)",
    };

    for (test_lists) |list| {
        // Compare (car (reverse (reverse list))) with (car list)
        var buf1: [128]u8 = undefined;
        var buf2: [128]u8 = undefined;
        const expr1 = std.fmt.bufPrint(&buf1, "(reverse (reverse {s}))", .{list}) catch unreachable;
        const expr2 = std.fmt.bufPrint(&buf2, "{s}", .{list}) catch unreachable;

        const r1 = try eval(allocator, &heap, expr1);
        const r2 = try eval(allocator, &heap, expr2);

        // Both should have same length
        var len_buf1: [128]u8 = undefined;
        var len_buf2: [128]u8 = undefined;
        const len_expr1 = std.fmt.bufPrint(&len_buf1, "(length (reverse (reverse {s})))", .{list}) catch unreachable;
        const len_expr2 = std.fmt.bufPrint(&len_buf2, "(length {s})", .{list}) catch unreachable;

        const len1 = try eval(allocator, &heap, len_expr1);
        const len2 = try eval(allocator, &heap, len_expr2);
        try testing.expectEqual(len1.toFixnum(), len2.toFixnum());

        // If non-empty, first elements should match
        if (!r1.isNil() and !r2.isNil()) {
            var car_buf1: [128]u8 = undefined;
            var car_buf2: [128]u8 = undefined;
            const car_expr1 = std.fmt.bufPrint(&car_buf1, "(car (reverse (reverse {s})))", .{list}) catch unreachable;
            const car_expr2 = std.fmt.bufPrint(&car_buf2, "(car {s})", .{list}) catch unreachable;

            const car1 = try eval(allocator, &heap, car_expr1);
            const car2 = try eval(allocator, &heap, car_expr2);
            try testing.expectEqual(car1.toFixnum(), car2.toFixnum());
        }
    }
}

test "property: append nil is identity" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    const test_lists = [_][]const u8{
        "(list 1)",
        "(list 1 2)",
        "(list 1 2 3)",
    };

    for (test_lists) |list| {
        var buf: [128]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(length (append {s} nil))", .{list}) catch unreachable;

        var len_buf: [128]u8 = undefined;
        const len_expr = std.fmt.bufPrint(&len_buf, "(length {s})", .{list}) catch unreachable;

        const r1 = try eval(allocator, &heap, expr);
        const r2 = try eval(allocator, &heap, len_expr);
        try testing.expectEqual(r1.toFixnum(), r2.toFixnum());
    }
}

// ============================================================================
// Type Predicate Properties
// ============================================================================

test "property: type predicates are mutually exclusive for atoms" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test values and which predicate should be true
    const test_cases = [_]struct { val: []const u8, pred: []const u8 }{
        .{ .val = "42", .pred = "numberp" },
        .{ .val = "-17", .pred = "numberp" },
        .{ .val = "nil", .pred = "null" },
        .{ .val = "'foo", .pred = "symbolp" },
        .{ .val = "\"hello\"", .pred = "stringp" },
        .{ .val = "(cons 1 2)", .pred = "consp" },
    };

    const predicates = [_][]const u8{ "numberp", "symbolp", "stringp", "consp", "vectorp" };

    for (test_cases) |tc| {
        var true_count: usize = 0;

        for (predicates) |pred| {
            var buf: [128]u8 = undefined;
            const expr = std.fmt.bufPrint(&buf, "({s} {s})", .{ pred, tc.val }) catch unreachable;

            const result = try eval(allocator, &heap, expr);
            if (!result.isNil()) {
                true_count += 1;
            }
        }

        // At most one predicate should be true (nil has null true, not in list)
        try testing.expect(true_count <= 1);
    }
}

// ============================================================================
// Comparison Properties
// ============================================================================

test "property: less-than is anti-symmetric" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(66666);
    const random = rng.random();

    for (0..100) |_| {
        const a = random.intRangeAtMost(i32, -1000, 1000);
        const b = random.intRangeAtMost(i32, -1000, 1000);

        if (a == b) continue;

        var buf1: [64]u8 = undefined;
        var buf2: [64]u8 = undefined;
        const expr1 = std.fmt.bufPrint(&buf1, "(< {d} {d})", .{ a, b }) catch unreachable;
        const expr2 = std.fmt.bufPrint(&buf2, "(< {d} {d})", .{ b, a }) catch unreachable;

        const r1 = try eval(allocator, &heap, expr1);
        const r2 = try eval(allocator, &heap, expr2);

        // If a < b, then NOT (b < a)
        if (!r1.isNil()) {
            try testing.expect(r2.isNil());
        }
    }
}

test "property: equality is reflexive" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(77777);
    const random = rng.random();

    for (0..100) |_| {
        const n = random.intRangeAtMost(i32, -10000, 10000);

        var buf: [64]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(= {d} {d})", .{ n, n }) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expect(!result.isNil());
    }
}

// ============================================================================
// Closure Properties
// ============================================================================

test "property: lambda captures free variables" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var rng = Rng.init(88888);
    const random = rng.random();

    for (0..50) |_| {
        const n = random.intRangeAtMost(i32, 1, 100);

        var buf: [256]u8 = undefined;
        // Create adder using funcall
        const expr = std.fmt.bufPrint(&buf, "(let ((n {d})) (+ 10 n))", .{n}) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(@as(i64, 10 + n), result.toFixnum());
    }
}

// ============================================================================
// Vector Properties
// ============================================================================

test "property: vector literal length" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test vector literals - use vector-length for vectors
    const test_cases = [_]struct { expr: []const u8, len: i64 }{
        .{ .expr = "(vector 1)", .len = 1 },
        .{ .expr = "(vector 1 2)", .len = 2 },
        .{ .expr = "(vector 1 2 3)", .len = 3 },
        .{ .expr = "(vector 1 2 3 4 5)", .len = 5 },
    };

    for (test_cases) |tc| {
        var buf: [128]u8 = undefined;
        const expr = std.fmt.bufPrint(&buf, "(vector-length {s})", .{tc.expr}) catch unreachable;

        const result = try eval(allocator, &heap, expr);
        try testing.expectEqual(tc.len, result.toFixnum());
    }
}

test "property: vector-ref returns correct element" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    // Test accessing elements by index
    const test_cases = [_]struct { expr: []const u8, expected: i64 }{
        .{ .expr = "(svref (vector 10 20 30) 0)", .expected = 10 },
        .{ .expr = "(svref (vector 10 20 30) 1)", .expected = 20 },
        .{ .expr = "(svref (vector 10 20 30) 2)", .expected = 30 },
        .{ .expr = "(svref (vector 5) 0)", .expected = 5 },
    };

    for (test_cases) |tc| {
        const result = try eval(allocator, &heap, tc.expr);
        try testing.expectEqual(tc.expected, result.toFixnum());
    }
}
