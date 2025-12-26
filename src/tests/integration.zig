//! Integration tests for the full Habu pipeline
//!
//! Tests: read -> compile -> emit -> run
//! Covers: arithmetic, conditionals, functions, closures, recursion

const std = @import("std");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;

const reader = @import("../reader/reader.zig");
const Parser = reader.Parser;

const compiler = @import("../compiler/compiler.zig");
const Compiler = compiler.Compiler;
const Env = compiler.Env;

const bytecode = @import("../bytecode/bytecode.zig");
const Emitter = bytecode.Emitter;

const interp = @import("../interp/interp.zig");
const Vm = interp.Vm;

/// Test helper: parse, compile, emit, run and return result
fn evalExpr(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    // Use arena for IR allocations (freed all at once)
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    // Parse
    var parser = Parser.init(arena_alloc, heap, source);
    defer parser.deinit();

    const expr = try parser.parse();

    // Compile (with heap for symbol interning)
    var comp = Compiler.initWithHeap(arena_alloc, heap);
    defer comp.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const ir_node = try comp.compile(expr, &env);

    // Emit bytecode
    var emitter = Emitter.initWithHeap(arena_alloc, heap);
    try emitter.emit(ir_node);
    const chunk = try emitter.finalize();
    // Arena handles cleanup

    // Run - use main allocator for VM stack
    var vm = Vm.init(allocator, heap);
    return vm.run(&chunk);
}

// ============================================================================
// Arithmetic Tests
// ============================================================================

test "eval integer literal" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "42");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval nil" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "nil");
    try testing.expect(result.isNil());
}

test "eval addition" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(+ 1 2)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "eval subtraction" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(- 10 3)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 7), result.toFixnum());
}

test "eval multiplication" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(* 6 7)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval division" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(/ 20 4)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 5), result.toFixnum());
}

test "eval nested arithmetic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (+ (* 3 4) (- 10 5)) = 12 + 5 = 17
    const result = try evalExpr(allocator, &heap, "(+ (* 3 4) (- 10 5))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 17), result.toFixnum());
}

// ============================================================================
// Conditional Tests
// ============================================================================

test "eval if true branch" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (if t 1 2) = 1
    const result = try evalExpr(allocator, &heap, "(if 1 42 0)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval if false branch" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (if nil 1 2) = 2
    const result = try evalExpr(allocator, &heap, "(if nil 1 99)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "eval comparison less than" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (< 1 2) = t
    const result = try evalExpr(allocator, &heap, "(< 1 2)");
    try testing.expect(!result.isNil()); // t is non-nil
}

test "eval comparison greater than false" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (> 1 2) = nil
    const result = try evalExpr(allocator, &heap, "(> 1 2)");
    try testing.expect(result.isNil());
}

// ============================================================================
// List Tests
// ============================================================================

test "eval cons" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(cons 1 2)");
    try testing.expect(result.isCons());

    const cons = result.toPtr(runtime.Cons);
    try testing.expectEqual(@as(i64, 1), cons.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), cons.cdr.toFixnum());
}

test "eval car" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(car (cons 42 99))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval cdr" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(cdr (cons 42 99))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "eval consp true" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(consp (cons 1 2))");
    try testing.expect(!result.isNil());
}

test "eval consp false" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(consp 42)");
    try testing.expect(result.isNil());
}

// ============================================================================
// Let Tests
// ============================================================================

test "eval let simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(let ((x 10)) x)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10), result.toFixnum());
}

test "eval let with arithmetic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(let ((x 3) (y 4)) (+ x y))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 7), result.toFixnum());
}

// TODO: Nested let with outer variable reference - needs upvalue support
// test "eval nested let" {
//     const allocator = testing.allocator;
//
//     var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
//     defer heap.deinit();
//
//     const result = try evalExpr(allocator, &heap,
//         \\(let ((x 10))
//         \\  (let ((y 20))
//         \\    (+ x y)))
//     );
//     try testing.expect(result.isFixnum());
//     try testing.expectEqual(@as(i64, 30), result.toFixnum());
// }

// ============================================================================
// Progn Tests
// ============================================================================

test "eval progn returns last" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn 1 2 3)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

// ============================================================================
// Global Variable Tests (using REPL for persistent state)
// ============================================================================

const repl_mod = @import("../interp/repl.zig");
const Repl = repl_mod.Repl;

test "eval define simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define a variable
    const def_result = try repl.eval("(define x 42)");
    try testing.expect(def_result.isFixnum());
    try testing.expectEqual(@as(i64, 42), def_result.toFixnum());

    // Use the variable
    const use_result = try repl.eval("x");
    try testing.expect(use_result.isFixnum());
    try testing.expectEqual(@as(i64, 42), use_result.toFixnum());
}

test "eval define with expression" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define with computed value
    _ = try repl.eval("(define y (+ 10 20))");

    // Use in expression
    const result = try repl.eval("(* y 2)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 60), result.toFixnum());
}

test "eval multiple defines" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    _ = try repl.eval("(define a 10)");
    _ = try repl.eval("(define b 20)");

    const result = try repl.eval("(+ a b)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "eval defun simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    _ = try repl.eval("(defun double (x) (* x 2))");
    const result = try repl.eval("(double 21)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval defun two params" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    _ = try repl.eval("(defun add (a b) (+ a b))");
    const result = try repl.eval("(add 10 20)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "eval defun recursive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    _ = try repl.eval("(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))");
    const result = try repl.eval("(fact 5)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 120), result.toFixnum());
}

test "eval letrec simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    const result = try repl.eval("(letrec ((x 5)) x)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 5), result.toFixnum());
}

test "eval letrec recursive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    const result = try repl.eval("(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 120), result.toFixnum());
}

// ============================================================================
// Macro Tests
// ============================================================================

test "eval defmacro simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // First test: identity macro (just returns its argument)
    const def_result = try repl.eval("(defmacro identity-macro (x) x)");
    try testing.expect(def_result.isSymbol()); // Should return the macro name

    // Use the identity macro - should just return 42
    const result = try repl.eval("(identity-macro 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval defmacro with cons" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define macro that builds (+ x 1) using cons
    _ = try repl.eval("(defmacro inc (x) (cons '+ (cons x (cons 1 nil))))");

    // Use the macro
    const result = try repl.eval("(inc 41)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval defmacro with quasiquote" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define when macro using quasiquote
    _ = try repl.eval("(defmacro when (test body) `(if ,test ,body nil))");

    // Use the macro
    const result = try repl.eval("(when (< 1 2) 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());

    // Test false branch
    const result2 = try repl.eval("(when (> 1 2) 42)");
    try testing.expect(result2.isNil());
}

test "eval defmacro unless" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define unless macro
    _ = try repl.eval("(defmacro unless (test body) `(if ,test nil ,body))");

    // Use the macro
    const result = try repl.eval("(unless (> 1 2) 99)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

// ============================================================================
// Type assertions: (the type expr)
// ============================================================================

test "the fixnum success" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the fixnum 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "the fixnum failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // String is not a fixnum
    const err = evalExpr(allocator, &heap, "(the fixnum \"hello\")");
    try testing.expectError(error.TypeMismatch, err);
}

test "the cons success" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the cons (cons 1 2))");
    try testing.expect(result.isCons());
}

test "the cons failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // nil is not a cons
    const err = evalExpr(allocator, &heap, "(the cons nil)");
    try testing.expectError(error.TypeMismatch, err);
}

test "the symbol success" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the symbol 'foo)");
    try testing.expect(result.isSymbol());
}

test "the string success" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the string \"hello\")");
    try testing.expect(result.isString());
}

test "the non-nil success" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the non-nil 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "the non-nil failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const err = evalExpr(allocator, &heap, "(the non-nil nil)");
    try testing.expectError(error.TypeMismatch, err);
}

test "the list success with cons" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the list (cons 1 2))");
    try testing.expect(result.isCons());
}

test "the list success with nil" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(the list nil)");
    try testing.expect(result.isNil());
}

test "the list failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // fixnum is not a list
    const err = evalExpr(allocator, &heap, "(the list 42)");
    try testing.expectError(error.TypeMismatch, err);
}

test "the any" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // any type accepts anything
    const result1 = try evalExpr(allocator, &heap, "(the any 42)");
    try testing.expect(result1.isFixnum());

    const result2 = try evalExpr(allocator, &heap, "(the any nil)");
    try testing.expect(result2.isNil());

    const result3 = try evalExpr(allocator, &heap, "(the any (cons 1 2))");
    try testing.expect(result3.isCons());
}

test "the or cons nil equals list" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (or cons nil) is equivalent to list type
    const result1 = try evalExpr(allocator, &heap, "(the (or cons nil) (cons 1 2))");
    try testing.expect(result1.isCons());

    const result2 = try evalExpr(allocator, &heap, "(the (or nil cons) nil)");
    try testing.expect(result2.isNil());
}

test "the or cons nil failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // fixnum is not (or cons nil)
    const err = evalExpr(allocator, &heap, "(the (or cons nil) 42)");
    try testing.expectError(error.TypeMismatch, err);
}

test "the in function" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define a function that asserts its argument is a fixnum
    _ = try repl.eval("(defun double (x) (* 2 (the fixnum x)))");

    // Valid call
    const result = try repl.eval("(double 21)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "the in function failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define a function that asserts its argument is a fixnum
    _ = try repl.eval("(defun double (x) (* 2 (the fixnum x)))");

    // Invalid call - string is not fixnum (REPL wraps as RuntimeError)
    const err = repl.eval("(double \"hello\")");
    try testing.expectError(error.RuntimeError, err);
}

test "occurrence typing skips redundant check" {
    // When we have (if (consp x) (the cons x) ...), the (the cons x) check
    // is skipped because the predicate already verified x is a cons.
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // safe-car uses occurrence typing: (the cons x) is redundant after (consp x)
    _ = try repl.eval("(defun safe-car (x) (if (consp x) (car (the cons x)) nil))");

    // Works on cons
    const result = try repl.eval("(safe-car (cons 42 nil))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());

    // Returns nil for non-cons (no error because we use if)
    const result2 = try repl.eval("(safe-car nil)");
    try testing.expect(result2.isNil());
}

test "occurrence typing with numberp" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // After (numberp x), (the fixnum x) should be skipped
    _ = try repl.eval("(defun safe-double (x) (if (numberp x) (* 2 (the fixnum x)) 0))");

    const result = try repl.eval("(safe-double 21)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());

    const result2 = try repl.eval("(safe-double \"not a number\")");
    try testing.expect(result2.isFixnum());
    try testing.expectEqual(@as(i64, 0), result2.toFixnum());
}

test "else-branch occurrence typing with null?" {
    // After (null? x) in if condition:
    // - then-branch: x is nil
    // - else-branch: x is non-nil (check skipped)
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // safe-first: if null, return 0; else car the non-nil value
    // The (the non-nil x) should be skipped in else-branch because we know x is not nil
    _ = try repl.eval("(defun safe-first (x) (if (null? x) 0 (car (the non-nil x))))");

    // Works on cons - x is non-nil, assertion skipped
    const result = try repl.eval("(safe-first (cons 42 nil))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());

    // Works on nil - returns 0 without error
    const result2 = try repl.eval("(safe-first nil)");
    try testing.expect(result2.isFixnum());
    try testing.expectEqual(@as(i64, 0), result2.toFixnum());
}

// ============================================================================
// Type introspection: type-of
// ============================================================================

test "type-of returns correct type symbols" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Check that type-of returns the expected symbol for each type
    const r1 = try repl.eval("(eq (type-of 42) 'fixnum)");
    try testing.expect(r1.raw == Value.t.raw);

    // nil returns symbol 'nil - compare via symbol-name since 'nil may differ from interned nil
    const r2 = try repl.eval("(string= (symbol-name (type-of nil)) \"nil\")");
    try testing.expect(r2.raw == Value.t.raw);

    const r3 = try repl.eval("(eq (type-of (cons 1 2)) 'cons)");
    try testing.expect(r3.raw == Value.t.raw);

    const r4 = try repl.eval("(eq (type-of 'foo) 'symbol)");
    try testing.expect(r4.raw == Value.t.raw);

    const r5 = try repl.eval("(eq (type-of \"hello\") 'string)");
    try testing.expect(r5.raw == Value.t.raw);

    // Note: No vector constructor available yet, skip vector test
    const r7 = try repl.eval("(eq (type-of (lambda (x) x)) 'closure)");
    try testing.expect(r7.raw == Value.t.raw);
}

// ============================================================================
// Typed function parameters: (defun name ((x type) ...) body)
// ============================================================================

test "typed defun parameter" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define function with typed parameter
    _ = try repl.eval("(defun inc ((x fixnum)) (+ x 1))");

    // Valid call
    const result = try repl.eval("(inc 41)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "typed defun parameter failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define function with typed parameter
    _ = try repl.eval("(defun inc ((x fixnum)) (+ x 1))");

    // Invalid call - string is not fixnum
    const err = repl.eval("(inc \"hello\")");
    try testing.expectError(error.RuntimeError, err);
}

test "typed defun multiple params" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define function with multiple typed parameters
    _ = try repl.eval("(defun add ((a fixnum) (b fixnum)) (+ a b))");

    const result = try repl.eval("(add 20 22)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "typed defun mixed params" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Mix of typed and untyped parameters
    _ = try repl.eval("(defun add-to ((x fixnum) y) (+ x y))");

    const result = try repl.eval("(add-to 40 2)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "typed lambda" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Lambda with typed parameter
    const result = try repl.eval("((lambda ((x fixnum)) (+ x 1)) 41)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "closure captures value" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // make-adder captures n and returns a closure that adds n
    _ = try repl.eval("(defun make-adder (n) (lambda (x) (+ x n)))");

    // Create an adder that adds 10
    _ = try repl.eval("(define add10 (make-adder 10))");

    // Use the closure
    const result = try repl.eval("(add10 32)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "closure captures multiple values" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Create a linear function y = ax + b
    _ = try repl.eval("(defun make-linear (a b) (lambda (x) (+ (* a x) b)))");

    // Create y = 2x + 5
    _ = try repl.eval("(define f (make-linear 2 5))");

    // f(10) = 2*10 + 5 = 25
    const result = try repl.eval("(f 10)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 25), result.toFixnum());
}

// ============================================================================
// Return type declarations: (defun (name -> type) ...)
// ============================================================================

test "defun with return type" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Function with return type
    _ = try repl.eval("(defun (always-42 -> fixnum) () 42)");

    const result = try repl.eval("(always-42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "defun with return type and params" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Fully typed function
    _ = try repl.eval("(defun (add -> fixnum) ((a fixnum) (b fixnum)) (+ a b))");

    const result = try repl.eval("(add 20 22)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "defun return type failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Function claims to return fixnum but returns string
    _ = try repl.eval("(defun (bad -> fixnum) () \"not a number\")");

    // Calling it should fail type check
    const err = repl.eval("(bad)");
    try testing.expectError(error.RuntimeError, err);
}

test "defun return type cons" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Function returns a cons
    _ = try repl.eval("(defun (make-pair -> cons) (a b) (cons a b))");

    const result = try repl.eval("(make-pair 1 2)");
    try testing.expect(result.isCons());
}

// ============================================================================
// flet and labels tests
// ============================================================================

test "flet basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // flet binds local functions
    const result = try repl.eval(
        \\(flet ((double (x) (* x 2))
        \\       (triple (x) (* x 3)))
        \\  (+ (double 5) (triple 4)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 22), result.toFixnum()); // 10 + 12
}

test "flet shadowing" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // Define a global function
    _ = try repl.eval("(defun square (x) (* x x))");

    // flet shadows the global
    const result = try repl.eval(
        \\(flet ((square (x) (+ x x)))
        \\  (square 5))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10), result.toFixnum()); // 5 + 5, not 25

    // Global is still available outside
    const global_result = try repl.eval("(square 5)");
    try testing.expectEqual(@as(i64, 25), global_result.toFixnum());
}

test "labels recursive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // labels allows recursion
    const result = try repl.eval(
        \\(labels ((fact (n)
        \\          (if (= n 0) 1 (* n (fact (- n 1))))))
        \\  (fact 5))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 120), result.toFixnum());
}

test "labels mutual recursion" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = Repl.init(allocator, &heap, .{});
    defer repl.deinit();

    // labels with mutual recursion (even?/odd?)
    const result = try repl.eval(
        \\(labels ((is-even (n) (if (= n 0) t (is-odd (- n 1))))
        \\         (is-odd (n) (if (= n 0) nil (is-even (- n 1)))))
        \\  (cons (is-even 4) (is-odd 5)))
    );
    try testing.expect(result.isCons());
    const cons = result.toPtr(@import("../runtime/objects.zig").Cons);
    try testing.expect(!cons.car.isNil()); // is-even 4 = t
    try testing.expect(!cons.cdr.isNil()); // is-odd 5 = t
}
