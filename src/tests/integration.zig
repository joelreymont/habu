//! Integration tests for the full Habu pipeline
//!
//! Tests: read -> compile -> emit -> run
//! Covers: arithmetic, conditionals, functions, closures, recursion

const std = @import("std");
const testing = std.testing;

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Chunk = runtime.Chunk;
const Cons = runtime.Cons;

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

    // Compile (with heap for symbol interning)
    var comp_vm = try Vm.init(arena_alloc, heap);

    // Parse
    var parser = Parser.init(arena_alloc, heap, source, &comp_vm.builtins);
    defer parser.deinit();

    const expr = try parser.parse();
    var comp = try Compiler.initWithHeap(arena_alloc, &comp_vm);
    defer comp.deinit();

    var env = Env.init(arena_alloc, null);
    defer env.deinit();

    const ir_node = try comp.compile(expr, &env);

    // Emit bytecode
    var emitter = Emitter.initWithHeap(arena_alloc, heap);
    try emitter.emit(ir_node);
    const chunk_val = try emitter.finalize();
    // Arena handles cleanup

    // Run - use main allocator for VM stack
    var vm = try Vm.init(allocator, heap);
    return vm.run(chunk_val.toPtr(Chunk));
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
    // Division now returns rational
    try testing.expect(result.typeKind() == .rational);
    const rat = result.toPtr(runtime.objects.Rational);
    try testing.expectEqual(@as(i64, 5), rat.numerator);
    try testing.expectEqual(@as(i64, 1), rat.denominator);
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

test "eval nested let" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap,
        \\(let ((x 10))
        \\  (let ((y 20))
        \\    (+ x y)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    const result = try repl.eval("(letrec ((x 5)) x)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 5), result.toFixnum());
}

test "eval letrec recursive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

test "the union cons nil equals list" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (union cons nil) is equivalent to list type
    const result1 = try evalExpr(allocator, &heap, "(the (union cons nil) (cons 1 2))");
    try testing.expect(result1.isCons());

    const result2 = try evalExpr(allocator, &heap, "(the (union nil cons) nil)");
    try testing.expect(result2.isNil());
}

test "the union cons nil failure" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // fixnum is not (union cons nil)
    const err = evalExpr(allocator, &heap, "(the (union cons nil) 42)");
    try testing.expectError(error.TypeMismatch, err);
}

test "the in function" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

test "else-branch occurrence typing with null" {
    // After (null x) in if condition:
    // - then-branch: x is nil
    // - else-branch: x is non-nil (check skipped)
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // safe-first: if null, return 0; else car the non-nil value
    // The (the non-nil x) should be skipped in else-branch because we know x is not nil
    _ = try repl.eval("(defun safe-first (x) (if (null x) 0 (car (the non-nil x))))");

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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
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

// ============================================================================
// block/return-from tests
// ============================================================================

test "block basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // block without return-from returns body value
    const result = try repl.eval("(block done (+ 1 2))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "return-from early exit" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // return-from exits early with value
    const result = try repl.eval(
        \\(block found
        \\  (return-from found 42)
        \\  999)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "return-from in conditional" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // return-from in conditional branch
    const result = try repl.eval(
        \\(block search
        \\  (if t
        \\      (return-from search 'found)
        \\      (return-from search 'not-found))
        \\  'unreachable)
    );
    try testing.expect(result.isSymbol());
}

test "cond with multiple body expressions" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // cond body should evaluate all expressions, return last
    const result = try repl.eval("(cond (t 1 2))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "nested blocks" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // return-from targets outer block from inner block
    const result = try repl.eval(
        \\(block outer
        \\  (block inner
        \\    (return-from outer 100)
        \\    999)
        \\  888)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 100), result.toFixnum());
}

// ============================================================================
// unwind-protect tests
// ============================================================================

test "unwind-protect normal exit" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // unwind-protect returns the protected value (cleanup result discarded)
    const result = try repl.eval(
        \\(unwind-protect
        \\    42
        \\  (+ 1 2))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "unwind-protect with return-from" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // return-from exits block but unwind-protect still returns correct value
    const result = try repl.eval(
        \\(block done
        \\  (unwind-protect
        \\      (return-from done 99)
        \\    (+ 100 200))
        \\  888)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "nested unwind-protect" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // Nested unwind-protects with return-from
    const result = try repl.eval(
        \\(block done
        \\  (unwind-protect
        \\      (unwind-protect
        \\          (return-from done 100)
        \\        (+ 1 1))
        \\    (+ 2 2))
        \\  999)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 100), result.toFixnum());
}

// NOTE: Error handling in unwind-protect is not yet fully implemented
// The VM needs to be enhanced to run cleanup forms when errors occur
// See: src/interp/vm.zig - doError function needs to check unwind stack
//
// test "unwind-protect with error - SKIP until VM error handling is implemented" {
//     const allocator = testing.allocator;
//
//     var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
//     defer heap.deinit();
//
//     var repl = try Repl.init(allocator, &heap, .{});
//     repl.wireGlobalEnv();
//     defer repl.deinit();
//
//     // Load stdlib for setq
//     const stdlib = @embedFile("../../lib/stdlib.habu");
//     const null_writer = std.io.null_writer;
//     try repl.evalFileContent(stdlib, null_writer);
//
//     // Cleanup should run even when protected form errors
//     // Currently this test would fail - cleanup doesn't run on errors
//     const result = try repl.eval(
//         \\(let ((cleanup-ran nil))
//         \\  (unwind-protect
//         \\      (/ 1 0)
//         \\    (setq cleanup-ran t))
//         \\  cleanup-ran)
//     );
//     // Should return t after cleanup runs
//     try testing.expect(result.eq(Value.t));
// }

// ============================================================================
// catch/throw tests
// ============================================================================

test "catch basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // catch without throw returns body value
    const result = try repl.eval("(catch 'done 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "throw to catch" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // throw exits to matching catch with value
    const result = try repl.eval(
        \\(catch 'exit
        \\  (progn
        \\    (throw 'exit 99)
        \\    888))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "nested catch" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // throw targets inner catch, not outer
    const result = try repl.eval(
        \\(catch 'outer
        \\  (catch 'inner
        \\    (throw 'inner 100)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 100), result.toFixnum());
}

test "throw across function call" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // Define a function that throws
    _ = try repl.eval("(defun thrower () (throw 'bail 42))");

    // throw from inside function to outer catch
    const result = try repl.eval(
        \\(catch 'bail
        \\  (progn
        \\    (thrower)
        \\    999))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

// ============================================================================
// tagbody/go tests
// ============================================================================

test "tagbody basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // tagbody without go returns nil
    const result = try repl.eval("(tagbody (+ 1 2))");
    try testing.expect(result.isNil());
}

test "tagbody with go forward" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // go skips to end tag
    _ = try repl.eval("(define x 0)");
    _ = try repl.eval(
        \\(tagbody
        \\  (go end)
        \\  (define x 100)
        \\  end)
    );
    const result = try repl.eval("x");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 0), result.toFixnum());
}

test "tagbody loop" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // Simple loop using tagbody/go
    _ = try repl.eval("(define counter 0)");
    _ = try repl.eval(
        \\(tagbody
        \\  loop
        \\  (define counter (+ counter 1))
        \\  (if (< counter 5)
        \\      (go loop)))
    );
    const result = try repl.eval("counter");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 5), result.toFixnum());
}

// ============================================================================
// multiple values tests
// ============================================================================

test "values single" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // (values 42) returns 42
    const result = try repl.eval("(values 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "values empty" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // (values) returns nil
    const result = try repl.eval("(values)");
    try testing.expect(result.isNil());
}

test "multiple-value-bind basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // Bind multiple values and use them
    const result = try repl.eval(
        \\(multiple-value-bind (a b c)
        \\    (values 1 2 3)
        \\  (+ a (* b c)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 7), result.toFixnum()); // 1 + 2*3 = 7
}

test "multiple-value-bind fewer values" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // When fewer values than variables, extras are nil
    const result = try repl.eval(
        \\(multiple-value-bind (a b c)
        \\    (values 10)
        \\  (if c 999 a))
    );
    // c is nil (falsy), so (if c 999 a) returns a which is 10
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10), result.toFixnum());
}

test "multiple-value-bind single var" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // Single var gets primary value
    const result = try repl.eval(
        \\(multiple-value-bind (x)
        \\    (values 42 99 100)
        \\  x)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "multiple-value-call basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // (multiple-value-call (lambda (&rest args) args) (values 1 2) (values 3 4)) => (1 2 3 4)
    const result = try repl.eval(
        \\(multiple-value-call (lambda (&rest args) args)
        \\    (values 1 2)
        \\    (values 3 4))
    );
    try testing.expect(result.isCons());
    // Should be list (1 2 3 4)
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
}

test "values-list returns elements as values" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // (values-list '(1 2 3)) should return 1 as primary, 2 and 3 as secondaries
    // Capture with multiple-value-bind to verify
    const result = try repl.eval(
        \\(multiple-value-bind (a b c) (values-list '(1 2 3))
        \\  a)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

// ============================================================================
// format tests
// ============================================================================

test "format nil returns string" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    const result = try repl.eval(
        \\(format nil "Hello ~A" "World")
    );
    try testing.expect(result.isString());
}

test "format ~D decimal" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    const result = try repl.eval(
        \\(format nil "Value is ~D" 42)
    );
    try testing.expect(result.isString());
}

test "format ~% newline" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    const result = try repl.eval(
        \\(format nil "Line1~%Line2")
    );
    try testing.expect(result.isString());
}

test "format ~S standard" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // ~S should quote strings
    const result = try repl.eval(
        \\(format nil "Got ~S" "test")
    );
    try testing.expect(result.isString());
}

// ============================================================================
// stdlib tests
// ============================================================================

test "stdlib compiles" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    repl.wireGlobalEnv();
    defer repl.deinit();

    // Read stdlib file
    const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    defer file.close();
    const stdlib = try file.readToEndAlloc(allocator, 256 * 1024);
    defer allocator.free(stdlib);

    // Use evalFileContent to evaluate the whole file (handles multiple expressions)
    const null_writer = std.io.null_writer;
    try repl.evalFileContent(stdlib, null_writer);

    // Test a few stdlib functions
    const length_result = try repl.eval("(length (list3 1 2 3))");
    try testing.expectEqual(@as(i64, 3), length_result.toFixnum());

    const reverse_result = try repl.eval("(reverse (list3 1 2 3))");
    try testing.expect(reverse_result.isCons());

    const map_result = try repl.eval("(map (lambda (x) (* x 2)) (list3 1 2 3))");
    try testing.expect(map_result.isCons());
}

test "next-method-p compiles" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    // Simple test: just check that next-method-p compiles and returns nil outside method
    const result = try repl.eval("(next-method-p)");
    // Outside of a method, %next-method% is nil, so next-method-p returns nil
    try testing.expect(result.isNil());
}

test "method dispatch - specificity ordering" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    // Define generic with two methods of different specificity
    _ = try repl.eval("(defgeneric test-fn (x))");
    _ = try repl.eval("(defmethod test-fn (x) 'general)");
    _ = try repl.eval("(defmethod test-fn ((x fixnum)) 'fixnum-specific)");

    // Specific method should win for fixnum
    const fix_result = try repl.eval("(test-fn 42)");
    const fix_spec = try heap.intern("fixnum-specific");
    try testing.expect(fix_result.eq(fix_spec));

    // General method should win for non-fixnum
    const gen_result = try repl.eval("(test-fn 'sym)");
    const general = try heap.intern("general");
    try testing.expect(gen_result.eq(general));
}

test "method dispatch - all qualifiers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defclass animal () ())");

    // Define generic with all qualifier types
    _ = try repl.eval("(defgeneric make-sound (x))");
    _ = try repl.eval("(defmethod make-sound :before ((x animal)) (print 'setup) (terpri))");
    _ = try repl.eval("(defmethod make-sound ((x animal)) (print 'sound) (terpri) 'primary-result)");
    _ = try repl.eval("(defmethod make-sound :after ((x animal)) (print 'cleanup) (terpri))");
    _ = try repl.eval("(defmethod make-sound :around ((x animal)) (print 'outer) (terpri) (call-next-method))");

    _ = try repl.eval("(setf my-animal (make-instance 'animal))");

    // Call should execute: around -> before -> primary -> after
    const result = try repl.eval("(make-sound my-animal)");

    // Verify primary result is returned
    const primary = try heap.intern("primary-result");
    try testing.expect(result.eq(primary));
}

test "call-next-method - with explicit args" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defclass base () ())");
    _ = try repl.eval("(defclass derived (base) ())");

    _ = try repl.eval("(defgeneric process (x y))");
    _ = try repl.eval("(defmethod process ((x base) y) (list 'base y))");
    _ = try repl.eval("(defmethod process :around ((x derived) y) (call-next-method x (* y 2)))");

    _ = try repl.eval("(setf obj (make-instance 'derived))");

    // Around method passes modified args to next method
    const result = try repl.eval("(process obj 5)");

    // Should return (base 10) since around doubles the second arg
    const base_sym = try heap.intern("base");
    const result_cons = result.toPtr(runtime.Cons);
    try testing.expect(result_cons.car.eq(base_sym));
    try testing.expect(result_cons.cdr.toPtr(runtime.Cons).car.eq(Value.makeFixnum(10)));
}

test "next-method-p - returns t when next exists" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defclass base () ())");
    _ = try repl.eval("(defclass derived (base) ())");

    _ = try repl.eval("(defgeneric check-next (x))");
    _ = try repl.eval("(defmethod check-next ((x base)) 'base-method)");
    _ = try repl.eval("(defmethod check-next ((x derived)) (next-method-p))");

    _ = try repl.eval("(setf obj (make-instance 'derived))");

    const result = try repl.eval("(check-next obj)");
    try testing.expect(result.eq(Value.t));
}

test "next-method-p - returns nil when no next" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defclass thing () ())");
    _ = try repl.eval("(defgeneric check-next (x))");
    _ = try repl.eval("(defmethod check-next ((x thing)) (next-method-p))");

    _ = try repl.eval("(setf obj (make-instance 'thing))");

    const result = try repl.eval("(check-next obj)");
    try testing.expect(result.isNil());
}

test "no-applicable-method - no methods defined" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric test-gf (x))");

    // Should error: no applicable method
    const err = repl.eval("(test-gf 42)");
    try testing.expectError(error.UserError, err);
}

test "no-applicable-method - no matching specializers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defclass foo () ())");
    _ = try repl.eval("(defgeneric process (x))");
    _ = try repl.eval("(defmethod process ((x foo)) 'foo-result)");

    // Should error: no applicable method for fixnum
    const err = repl.eval("(process 42)");
    try testing.expectError(error.UserError, err);
}

test "call-next-method - no next method error" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl = try Repl.init(allocator, &heap, .{});
    defer repl.deinit();
    repl.wireGlobalEnv();

    _ = try repl.eval("(defclass thing () ())");
    _ = try repl.eval("(defgeneric do-thing (x))");
    _ = try repl.eval("(defmethod do-thing ((x thing)) (call-next-method))");

    _ = try repl.eval("(setf obj (make-instance 'thing))");

    // Should error: no next method available
    const err = repl.eval("(do-thing obj)");
    try testing.expectError(error.UserError, err);
}
