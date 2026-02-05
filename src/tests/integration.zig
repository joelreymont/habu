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

const compiler = @import("../compiler/compiler.zig");
const Compiler = compiler.Compiler;

const interp = @import("../interp/interp.zig");
const Vm = interp.Vm;

const compile_chunk = @import("../testing/compile_chunk.zig");
const OhSnap = @import("ohsnap");

/// Test helper: parse, compile, emit, run and return result
fn evalExpr(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var vm = try Vm.init(allocator, heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();

    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(*Chunk){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPool(chunk_pool.items);

    const chunk = try compile_chunk.compileChunk(allocator, heap, &vm, &comp, &chunk_pool, source);
    return vm.run(chunk);
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
// Equality Tests
// ============================================================================

test "eval equalp case-insensitive string" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(equalp \"a\" \"A\")");
    try testing.expect(!result.isNil());
}

test "eval equalp numeric coercion" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(equalp 1 1.0)");
    try testing.expect(!result.isNil());
}

test "eval hash-table :test equalp folds string case" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        "(let ((h (make-hash-table :test 'equalp)))\n" ++
        "  (puthash \"a\" 42 h)\n" ++
        "  (gethash \"A\" h))";
    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
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

fn loadStdlib(repl: *Repl) !void {
    const null_writer = std.io.null_writer;
    const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    defer file.close();
    const content = try file.readToEndAlloc(repl.allocator, 16 * 1024 * 1024);
    defer repl.allocator.free(content);
    try repl.evalFileContent(content, null_writer);
}

fn asString(val: Value) ![]const u8 {
    switch (val.typeKind()) {
        .string => return val.toPtr(runtime.String).bytes(),
        else => return error.TypeMismatch,
    }
}

test "eval define simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Define a variable
    const def_result = try repl.eval("(define x 42)");
    try testing.expect(def_result.isFixnum());
    try testing.expectEqual(@as(i64, 42), def_result.toFixnum());

    // Use the variable
    const use_result = try repl.eval("x");
    try testing.expect(use_result.isFixnum());
    try testing.expectEqual(@as(i64, 42), use_result.toFixnum());
}

test "stdlib fdefinition basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(progn
        \\  (defun foo (x) x)
        \\  (eq (fdefinition 'foo) (symbol-function 'foo)))
    );
    try testing.expect(result.eq(Value.t));
}

test "stdlib setf fdefinition (symbol)" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(progn
        \\  (setf (fdefinition 'foo) (lambda (x) (+ x 1)))
        \\  (funcall (fdefinition 'foo) 41))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "stdlib setf fdefinition ((setf name))" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(progn
        \\  (setf (fdefinition '(setf bar)) (lambda (x) (+ x 1)))
        \\  (funcall (fdefinition '(setf bar)) 41))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "stdlib setf expander registry" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(hash-table-p *setf-expanders*)");
    try testing.expect(result.eq(Value.t));
}

test "stdlib define-setf-expander registers" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(define-setf-expander foo (place) (list place))");
    const result = try repl.eval("(gethash 'foo *setf-expanders*)");
    try testing.expect(!result.isNil());
}

test "stdlib get-setf-expansion snapshots" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        \\(write-to-string
        \\  (list
        \\    (let ((e (multiple-value-list (get-setf-expansion '(car x)))))
        \\      (list (length (first e)) (length (second e)) (length (third e))
        \\            (car (fourth e)) (car (fifth e))))
        \\    (let ((e (multiple-value-list (get-setf-expansion '(aref a i)))))
        \\      (list (length (first e)) (length (second e)) (length (third e))
        \\            (car (fourth e)) (car (fifth e))))
        \\    (let ((e (multiple-value-list (get-setf-expansion '(gethash k h)))))
        \\      (list (length (first e)) (length (second e)) (length (third e))
        \\            (car (fourth e)) (car (fifth e))))))
    );
    const got_str = try asString(got);
    const oh = OhSnap{};
    try oh.snap(@src(),
        \\((1 1 1 PROGN CAR) (2 2 1 PROGN AREF) (2 2 1 PROGN GETHASH))
    ).diff(got_str, true);
}

test "stdlib setf custom expander integration" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        "(progn\n" ++
            "  (defun cell-val (x) (car x))\n" ++
            "  (define-setf-expander cell-val (x)\n" ++
            "    (let ((g (gensym \"CELL\"))\n" ++
            "          (s (gensym \"STORE\")))\n" ++
            "      (values (list g) (list x) (list s)\n" ++
            "              `(progn (rplaca ,g ,s) ,s)\n" ++
            "              `(car ,g)))))",
    );

    const got = try repl.eval(
        "(let ((a (cons 1 nil))\n" ++
            "      (b (cons 2 nil)))\n" ++
            "  (setf (cell-val a) 9 (car b) 8)\n" ++
            "  (list (car a) (car b)))",
    );
    const cons = got.toPtr(Cons);
    try testing.expectEqual(@as(i64, 9), cons.car.toFixnum());
    const tail = cons.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 8), tail.car.toFixnum());
}

test "eval define with expression" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun double (x) (* x 2))");
    const result = try repl.eval("(double 21)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval defun two params" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun add (a b) (+ a b))");
    const result = try repl.eval("(add 10 20)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "keyword arg validation" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun f (&key a) a)");
    const ok = try repl.eval("(f :a 1)");
    try testing.expect(ok.isFixnum());
    try testing.expectEqual(@as(i64, 1), ok.toFixnum());

    const err = repl.eval("(f :b 2)");
    try testing.expectError(error.TypeMismatch, err);

    const ok2 = try repl.eval("(f :b 2 :allow-other-keys t)");
    try testing.expect(ok2.isNil());
}

test "eval defun recursive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))");
    const result = try repl.eval("(fact 5)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 120), result.toFixnum());
}

test "eval letrec simple" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval("(letrec ((x 5)) x)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 5), result.toFixnum());
}

test "eval letrec recursive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // First test: identity macro (just returns its argument)
    const def_result = try repl.eval("(defmacro identity-macro (x) x)");
    try testing.expect(def_result.isSymbol()); // Should return the macro name

    // Use the identity macro - should just return 42
    const result = try repl.eval("(identity-macro 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval defmacro &whole &environment" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defmacro m (&whole w &environment e x) `(list ',w ',e ',x))");
    const result = try repl.eval("(m 42)");
    try testing.expect(result.isCons());

    const r0 = result.toPtr(Cons);
    const w_val = r0.car;
    const r1 = r0.cdr.toPtr(Cons);
    const e_val = r1.car;
    const r2 = r1.cdr.toPtr(Cons);
    const x_val = r2.car;

    try testing.expect(w_val.isCons());
    const w_cons = w_val.toPtr(Cons);
    const m_sym = try heap.intern("m");
    try testing.expectEqual(m_sym.raw, w_cons.car.raw);
    try testing.expect(e_val.isBoxed());
    try testing.expectEqual(runtime.TypeKind.macro_env, e_val.typeKind());
    try testing.expectEqual(@as(i64, 42), x_val.toFixnum());
}

test "eval defmacro with cons" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Define unless macro
    _ = try repl.eval("(defmacro unless (test body) `(if ,test nil ,body))");

    // Use the macro
    const result = try repl.eval("(unless (> 1 2) 99)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
}

test "declare optimize safety controls type assertions" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun unsafe-id (x) (declare (optimize (safety 0))) (the fixnum x))");
    const unchecked = try repl.eval("(unsafe-id \"hi\")");
    try testing.expect(unchecked.isString());

    _ = try repl.eval("(defun safe-id (x) (declare (optimize (safety 3))) (the fixnum x))");
    const checked = repl.eval("(safe-id \"hi\")");
    try testing.expectError(error.TypeMismatch, checked);
}

test "declaim/proclaim optimize sets default safety" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(declaim (optimize (safety 0)))");
    _ = try repl.eval("(defun globally-unsafe (x) (the fixnum x))");
    const unchecked = try repl.eval("(globally-unsafe \"hi\")");
    try testing.expect(unchecked.isString());

    _ = try repl.eval("(proclaim '(optimize (safety 2)))");
    _ = try repl.eval("(defun globally-safe (x) (the fixnum x))");
    const checked = repl.eval("(globally-safe \"hi\")");
    try testing.expectError(error.TypeMismatch, checked);
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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Define a function that asserts its argument is a fixnum
    _ = try repl.eval("(defun double (x) (* 2 (the fixnum x)))");

    // Invalid call - string is not fixnum
    const err = repl.eval("(double \"hello\")");
    try testing.expectError(error.TypeMismatch, err);
}

test "occurrence typing skips redundant check" {
    // When we have (if (consp x) (the cons x) ...), the (the cons x) check
    // is skipped because the predicate already verified x is a cons.
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Define function with typed parameter
    _ = try repl.eval("(defun inc ((x fixnum)) (+ x 1))");

    // Invalid call - string is not fixnum
    const err = repl.eval("(inc \"hello\")");
    try testing.expectError(error.TypeMismatch, err);
}

test "typed defun multiple params" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Lambda with typed parameter
    const result = try repl.eval("((lambda ((x fixnum)) (+ x 1)) 41)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "closure captures value" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Function claims to return fixnum but returns string
    _ = try repl.eval("(defun (bad -> fixnum) () \"not a number\")");

    // Calling it should fail type check
    const err = repl.eval("(bad)");
    try testing.expectError(error.TypeMismatch, err);
}

test "defun return type cons" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // block without return-from returns body value
    const result = try repl.eval("(block done (+ 1 2))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "return-from early exit" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // cond body should evaluate all expressions, return last
    const result = try repl.eval("(cond (t 1 2))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "nested blocks" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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
//     try repl.wireGlobalEnv();
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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // catch without throw returns body value
    const result = try repl.eval("(catch 'done 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "throw to catch" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // tagbody without go returns nil
    const result = try repl.eval("(tagbody (+ 1 2))");
    try testing.expect(result.isNil());
}

test "tagbody with go forward" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // (values 42) returns 42
    const result = try repl.eval("(values 42)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "values empty" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // (values) returns nil
    const result = try repl.eval("(values)");
    try testing.expect(result.isNil());
}

test "multiple-value-bind basic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(format nil "Hello ~A" "World")
    );
    try testing.expect(result.isString());
}

test "format ~D decimal" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(format nil "Value is ~D" 42)
    );
    try testing.expect(result.isString());
}

test "format ~% newline" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(format nil "Line1~%Line2")
    );
    try testing.expect(result.isString());
}

test "format ~S standard" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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
    // TEMP: Disabled - hangs during test run but works in main
    return error.SkipZigTest;
    // const allocator = testing.allocator;

    // var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    // defer heap.deinit();

    // var repl = try Repl.init(allocator, &heap, .{});
    // try repl.wireGlobalEnv();
    // defer repl.deinit();

    // // Read stdlib file
    // const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    // defer file.close();
    // const stdlib = try file.readToEndAlloc(allocator, 256 * 1024);
    // defer allocator.free(stdlib);

    // // Use evalFileContent to evaluate the whole file (handles multiple expressions)
    // const null_writer = std.io.null_writer;
    // try repl.evalFileContent(stdlib, null_writer);

    // // Test a few stdlib functions
    // const length_result = try repl.eval("(length (list3 1 2 3))");
    // try testing.expectEqual(@as(i64, 3), length_result.toFixnum());

    // const reverse_result = try repl.eval("(reverse (list3 1 2 3))");
    // try testing.expect(reverse_result.isCons());

    // const map_result = try repl.eval("(map (lambda (x) (* x 2)) (list3 1 2 3))");
    // try testing.expect(map_result.isCons());
}

test "next-method-p compiles" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Simple test: just check that next-method-p compiles and returns nil outside method
    const result = try repl.eval("(next-method-p)");
    // Outside of a method, %next-method% is nil, so next-method-p returns nil
    try testing.expect(result.isNil());
}

test "method dispatch - specificity ordering" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass animal () ())");

    // Define generic with all qualifier types
    _ = try repl.eval("(defgeneric make-sound (x))");
    _ = try repl.eval("(defmethod make-sound :before ((x animal)) nil)");
    _ = try repl.eval("(defmethod make-sound ((x animal)) 'primary-result)");
    _ = try repl.eval("(defmethod make-sound :after ((x animal)) nil)");
    _ = try repl.eval("(defmethod make-sound :around ((x animal)) (call-next-method))");

    _ = try repl.eval("(defvar my-animal (make-instance 'animal))");

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

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass animal () ())");
    _ = try repl.eval("(defclass dog (animal) ())");

    _ = try repl.eval("(defgeneric my-describe (x))");
    _ = try repl.eval("(defmethod my-describe :around ((x dog)) (call-next-method (make-instance 'animal)))");
    _ = try repl.eval("(defmethod my-describe ((x animal)) 'animal-described)");

    // When called on a dog, the :around method passes an animal to call-next-method
    const result = try repl.eval("(my-describe (make-instance 'dog))");
    const expected = try heap.intern("animal-described");
    try testing.expect(result.eq(expected));
}

test "next-method-p - returns t when next exists" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass base () ())");
    _ = try repl.eval("(defclass derived (base) ())");

    _ = try repl.eval("(defgeneric check-next (x))");
    _ = try repl.eval("(defmethod check-next ((x base)) 'base-method)");
    _ = try repl.eval("(defmethod check-next ((x derived)) (next-method-p))");

    _ = try repl.eval("(defvar obj (make-instance 'derived))");

    const result = try repl.eval("(check-next obj)");
    try testing.expect(result.eq(Value.t));
}

test "next-method-p - returns nil when no next" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass base () ())");
    _ = try repl.eval("(defgeneric check-next (x))");
    _ = try repl.eval("(defmethod check-next ((x base)) (next-method-p))");

    _ = try repl.eval("(defvar obj (make-instance 'base))");

    // Only one method - no next method exists
    const result = try repl.eval("(check-next obj)");
    try testing.expect(result.isNil());
}

test "no-applicable-method - no methods defined" {
    // Skip: requires no-applicable-method from stdlib to signal proper error
    return error.SkipZigTest;
}

test "no-applicable-method - no matching specializers" {
    // Skip: requires no-applicable-method from stdlib to signal proper error
    return error.SkipZigTest;
}

test "call-next-method - no next method error" {
    // Skip: requires no-next-method from stdlib to signal proper error
    return error.SkipZigTest;
}

test "slot-boundp - returns t for bound slot" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass point () (x y))");
    _ = try repl.eval("(defvar p (make-instance 'point))");
    _ = try repl.eval("(%set-slot-value p 'x 10)");

    const result = try repl.eval("(slot-boundp p 'x)");
    try testing.expect(result.eq(Value.t));
}

test "slot-boundp - returns nil for unbound slot" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass point () (x y))");
    _ = try repl.eval("(defvar p (make-instance 'point))");

    const result = try repl.eval("(slot-boundp p 'x)");
    try testing.expect(result.isNil());
}

test "slot-makunbound - marks slot as unbound" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass point () (x y))");
    _ = try repl.eval("(defvar p (make-instance 'point))");
    _ = try repl.eval("(%set-slot-value p 'x 10)");

    const bound_before = try repl.eval("(slot-boundp p 'x)");
    try testing.expect(bound_before.eq(Value.t));

    _ = try repl.eval("(slot-makunbound p 'x)");

    const bound_after = try repl.eval("(slot-boundp p 'x)");
    try testing.expect(bound_after.isNil());
}

test "class-name - returns name of class" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass point () (x y))");
    _ = try repl.eval("(defvar p (make-instance 'point))");

    const result = try repl.eval("(class-name (class-of p))");
    const expected = try repl.eval("'point");
    try testing.expect(result.eq(expected));
}

test "class-direct-superclasses - returns direct supers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass shape () ())");
    _ = try repl.eval("(defclass colored () ())");
    _ = try repl.eval("(defclass square (shape colored) ())");

    const result = try repl.eval("(class-direct-superclasses (find-class 'square))");
    const shape = try repl.eval("(find-class 'shape)");
    const colored = try repl.eval("(find-class 'colored)");

    try testing.expect(result.isCons());
    const cons1 = result.toPtr(Cons);
    try testing.expect(cons1.car.eq(shape));

    const rest = cons1.cdr;
    try testing.expect(rest.isCons());
    const cons2 = rest.toPtr(Cons);
    try testing.expect(cons2.car.eq(colored));
}

test "class-precedence-list - returns CPL" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass a () ())");
    _ = try repl.eval("(defclass b () ())");
    _ = try repl.eval("(defclass c (a b) ())");

    const result = try repl.eval("(class-precedence-list (find-class 'c))");
    try testing.expect(result.isCons());

    const c_class = try repl.eval("(find-class 'c)");
    const cons1 = result.toPtr(Cons);
    try testing.expect(cons1.car.eq(c_class));
}

test "class-slots - returns all slot definitions" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass shape () (color))");
    _ = try repl.eval("(defclass square (shape) (side))");

    const result = try repl.eval("(class-slots (find-class 'square))");
    try testing.expect(result.isCons());
}

test "slot-definition-name" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () (my-slot))");
    const name = try repl.eval("(slot-definition-name (car (class-slots (find-class 'test-class))))");
    try testing.expect(name.isSymbol());
    const sym = name.toPtr(runtime.Symbol);
    const name_str = sym.getName();
    try testing.expect(std.mem.eql(u8, name_str, "MY-SLOT"));
}

test "slot-definition-initform" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () ((my-slot :initform 42)))");
    const initform = try repl.eval("(slot-definition-initform (car (class-slots (find-class 'test-class))))");
    try testing.expect(initform.isFixnum());
    try testing.expectEqual(@as(i64, 42), initform.toFixnum());
}

test "slot-definition-initargs" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () ((my-slot :initarg :my-slot)))");
    const initargs = try repl.eval("(slot-definition-initargs (car (class-slots (find-class 'test-class))))");
    try testing.expect(initargs.isCons());
    const cons = initargs.toPtr(Cons);
    try testing.expect(cons.car.isKeyword());
}

test "make-instance initarg matches keyword" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () (my-slot))");
    _ = try repl.eval("(defvar obj (make-instance 'test-class :my-slot 7))");
    const result = try repl.eval("(slot-value obj 'my-slot)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 7), result.toFixnum());
}

test "slot-definition-readers and writers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () ((my-slot :reader get-my-slot :writer set-my-slot)))");
    const readers = try repl.eval("(slot-definition-readers (car (class-slots (find-class 'test-class))))");
    try testing.expect(readers.isCons());
    const writers = try repl.eval("(slot-definition-writers (car (class-slots (find-class 'test-class))))");
    try testing.expect(writers.isCons());
}

test "slot-definition-allocation" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () ((my-slot :allocation :instance)))");
    const allocation = try repl.eval("(slot-definition-allocation (car (class-slots (find-class 'test-class))))");
    try testing.expect(allocation.isKeyword());
}

test "slot-definition-type" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass test-class () ((my-slot :type fixnum)))");
    const slot_type = try repl.eval("(slot-definition-type (car (class-slots (find-class 'test-class))))");
    try testing.expect(slot_type.isSymbol());
}

test "generic-function-name" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric my-gf (x))");
    const gf_name = try repl.eval("(generic-function-name (symbol-function 'my-gf))");
    try testing.expect(gf_name.isSymbol());
}

test "generic-function-methods" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric my-gf (x))");
    _ = try repl.eval("(defmethod my-gf ((x fixnum)) 1)");
    _ = try repl.eval("(defmethod my-gf ((x cons)) 2)");
    const methods = try repl.eval("(generic-function-methods (symbol-function 'my-gf))");
    try testing.expect(methods.isCons());
    const len = try repl.eval("(length (generic-function-methods (symbol-function 'my-gf)))");
    try testing.expectEqual(@as(i64, 2), len.toFixnum());
}

test "generic-function-lambda-list" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric my-gf (x y &optional z))");
    const lambda_list = try repl.eval("(generic-function-lambda-list (symbol-function 'my-gf))");
    try testing.expect(lambda_list.isCons());
}

test "function-lambda-expression" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const simple_ok = try repl.eval(
        \\(multiple-value-bind (lambda-expr closure-p name)
        \\    (function-lambda-expression (lambda (x) (+ x 1)))
        \\  (and (consp lambda-expr)
        \\       (eq (car lambda-expr) 'lambda)
        \\       (null closure-p)
        \\       (null name)))
    );
    try testing.expect(!simple_ok.isNil());

    _ = try repl.eval("(defun fle-make-adder (n) (lambda (x) (+ x n)))");
    const closure_ok = try repl.eval(
        \\(multiple-value-bind (lambda-expr closure-p name)
        \\    (function-lambda-expression (fle-make-adder 10))
        \\  (and (consp lambda-expr)
        \\       (eq (car lambda-expr) 'lambda)
        \\       closure-p
        \\       (null name)))
    );
    try testing.expect(!closure_ok.isNil());

    _ = try repl.eval("(defun fle-foo (x) (+ x 1))");
    const named_ok = try repl.eval(
        \\(multiple-value-bind (lambda-expr closure-p name)
        \\    (function-lambda-expression (symbol-function 'fle-foo))
        \\  (and (consp lambda-expr)
        \\       (eq (car lambda-expr) 'lambda)
        \\       (null closure-p)
        \\       (eq name 'fle-foo)))
    );
    try testing.expect(!named_ok.isNil());
}

test "method-qualifiers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric foo (x))");
    _ = try repl.eval("(defmethod foo :before ((x fixnum)) 1)");
    _ = try repl.eval("(defmethod foo ((x fixnum)) 2)");

    _ = try repl.eval("(generic-function-methods (symbol-function 'foo))");
    // Methods are prepended, so primary (added last) is first, :before is second
    const m1_quals = try repl.eval("(method-qualifiers (car (generic-function-methods (symbol-function 'foo))))");
    try testing.expect(m1_quals.isNil()); // primary method has nil qualifiers
    const m2_quals = try repl.eval("(method-qualifiers (cadr (generic-function-methods (symbol-function 'foo))))");
    try testing.expect(m2_quals.isCons()); // :before method has (:before) qualifiers
}

test "method-specializers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric bar (x y))");
    _ = try repl.eval("(defmethod bar ((x fixnum) (y cons)) 1)");

    const specs = try repl.eval("(method-specializers (car (generic-function-methods (symbol-function 'bar))))");
    try testing.expect(specs.isCons());
    const len = try repl.eval("(length (method-specializers (car (generic-function-methods (symbol-function 'bar)))))");
    try testing.expectEqual(@as(i64, 2), len.toFixnum());
}

test "method-function" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defgeneric baz (x))");
    _ = try repl.eval("(defmethod baz ((x fixnum)) (* x 2))");

    const func = try repl.eval("(method-function (car (generic-function-methods (symbol-function 'baz))))");
    try testing.expect(func.isClosure());
    const result = try repl.eval("(funcall (method-function (car (generic-function-methods (symbol-function 'baz)))) 5)");
    try testing.expectEqual(@as(i64, 10), result.toFixnum());
}

test "method-combination helper macros" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const call_simple = try repl.eval(
        "(call-method (make-method 41))",
    );
    try testing.expectEqual(@as(i64, 41), call_simple.toFixnum());

    const make_method = try repl.eval(
        "(funcall (make-method (+ 1 2)))",
    );
    try testing.expectEqual(@as(i64, 3), make_method.toFixnum());

    const standard_name = try repl.eval("(eq standard 'standard)");
    try testing.expect(!standard_name.isNil());
}

test "method-combination error helpers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const helpers_bound = try repl.eval(
        "(and (fboundp 'invalid-method-error)\n" ++
            "     (fboundp 'method-combination-error)\n" ++
            "     (eq standard 'standard))",
    );
    try testing.expect(!helpers_bound.isNil());
}

test "defmethod multi-arity dispatch" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Define methods with different arities
    _ = try repl.eval("(defmethod test-multi () :zero-args)");
    _ = try repl.eval("(defmethod test-multi ((x fixnum)) :one-fixnum-arg)");
    _ = try repl.eval("(defmethod test-multi ((x string)) :one-string-arg)");

    // Test zero-arg method
    const result0 = try repl.eval("(test-multi)");
    try testing.expect(result0.isKeyword());
    const kw0 = result0.toPtr(runtime.Keyword);
    try testing.expectEqualStrings("ZERO-ARGS", kw0.getName());

    // Test one-arg fixnum method
    const result1 = try repl.eval("(test-multi 42)");
    try testing.expect(result1.isKeyword());
    const kw1 = result1.toPtr(runtime.Keyword);
    try testing.expectEqualStrings("ONE-FIXNUM-ARG", kw1.getName());

    // Test one-arg string method
    const result2 = try repl.eval("(test-multi \"hello\")");
    try testing.expect(result2.isKeyword());
    const kw2 = result2.toPtr(runtime.Keyword);
    try testing.expectEqualStrings("ONE-STRING-ARG", kw2.getName());
}

test "metaclass: standard-class is its own class" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // standard-class should be its own metaclass
    try testing.expect(!heap.standard_class.isNil());
    const std_class = heap.standard_class.toPtr(runtime.Class);
    try testing.expect(std_class.metaclass.eq(heap.standard_class));
}

test "metaclass: built-in classes have built-in-class as metaclass" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // fixnum class
    const fixnum_sym = try heap.intern("fixnum");
    const fixnum_class = heap.findLispClass(fixnum_sym);
    try testing.expect(fixnum_class != null);
    const fixnum_cls = fixnum_class.?.toPtr(runtime.Class);
    try testing.expect(fixnum_cls.metaclass.eq(heap.built_in_class));

    // cons class
    const cons_sym = try heap.intern("cons");
    const cons_class = heap.findLispClass(cons_sym);
    try testing.expect(cons_class != null);
    const cons_cls = cons_class.?.toPtr(runtime.Class);
    try testing.expect(cons_cls.metaclass.eq(heap.built_in_class));
}

test "metaclass: class-of returns metaclass for Class objects" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // class-of on standard-class should return standard-class
    const sc_sym = try heap.intern("standard-class");
    const sc_class = heap.findLispClass(sc_sym);
    try testing.expect(sc_class != null);

    // Build args for class-of: (standard-class)
    const args = try heap.allocCons(sc_class.?, Value.nil);
    const result = try runtime.clos.classOf(&heap, args);
    try testing.expect(result.eq(heap.standard_class));
}

test "read-char and peek-char from stream" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap,
        "(let ((s (make-string-input-stream \"ab\"))) (list (%peek-char-from-stream s) (%read-char-from-stream s) (%read-char-from-stream s) (%read-char-from-stream s)))");

    try testing.expect(result.isCons());
    const cons1 = result.toPtr(Cons);
    try testing.expect(cons1.car.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), cons1.car.toFixnum());

    try testing.expect(cons1.cdr.isCons());
    const cons2 = cons1.cdr.toPtr(Cons);
    try testing.expect(cons2.car.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), cons2.car.toFixnum());

    try testing.expect(cons2.cdr.isCons());
    const cons3 = cons2.cdr.toPtr(Cons);
    try testing.expect(cons3.car.isFixnum());
    try testing.expectEqual(@as(i64, 'b'), cons3.car.toFixnum());

    try testing.expect(cons3.cdr.isCons());
    const cons4 = cons3.cdr.toPtr(Cons);
    try testing.expect(cons4.car.isNil());
    try testing.expect(cons4.cdr.isNil());
}

test "read-char-no-hang from stream" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap,
        "(let ((s (make-string-input-stream \"a\"))) (list (read-char-no-hang s) (read-char-no-hang s)))");

    try testing.expect(result.isCons());
    const cons1 = result.toPtr(Cons);
    try testing.expect(cons1.car.isCharacter());
    try testing.expectEqual(@as(u21, 'a'), cons1.car.toCharacter());

    try testing.expect(cons1.cdr.isCons());
    const cons2 = cons1.cdr.toPtr(Cons);
    try testing.expect(cons2.car.isNil());
    try testing.expect(cons2.cdr.isNil());
}

test "listen reports input availability on stream" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap,
        "(let ((s (make-string-input-stream \"a\"))) (list (listen s) (%read-char-from-stream s) (listen s)))");

    try testing.expect(result.isCons());
    const cons1 = result.toPtr(Cons);
    try testing.expect(cons1.car.eq(Value.t));

    try testing.expect(cons1.cdr.isCons());
    const cons2 = cons1.cdr.toPtr(Cons);
    try testing.expect(cons2.car.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), cons2.car.toFixnum());

    try testing.expect(cons2.cdr.isCons());
    const cons3 = cons2.cdr.toPtr(Cons);
    try testing.expect(cons3.car.isNil());
    try testing.expect(cons3.cdr.isNil());
}

test "copy-structure copies defstruct instance" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap,
        "(progn (defstruct foo (bar fixnum) (baz fixnum))\n" ++
        "  (let* ((x (make-foo 1 2))\n" ++
        "         (y (copy-structure x))\n" ++
        "         (z (copy-foo x)))\n" ++
        "    (list\n" ++
        "      (foo-p y)\n" ++
        "      (foo-p z)\n" ++
        "      (not (eq x y))\n" ++
        "      (not (eq x z))\n" ++
        "      (eql (foo-bar x) (foo-bar y))\n" ++
        "      (eql (foo-baz x) (foo-baz y))\n" ++
        "      (eql (foo-bar x) (foo-bar z))\n" ++
        "      (eql (foo-baz x) (foo-baz z))))))");

    var cur = result;
    try testing.expect(cur.isCons());
    const c0 = cur.toPtr(Cons);
    try testing.expect(c0.car.eq(Value.t));
    cur = c0.cdr;

    try testing.expect(cur.isCons());
    const c1 = cur.toPtr(Cons);
    try testing.expect(c1.car.eq(Value.t));
    cur = c1.cdr;

    try testing.expect(cur.isCons());
    const c2 = cur.toPtr(Cons);
    try testing.expect(c2.car.eq(Value.t));
    cur = c2.cdr;

    try testing.expect(cur.isCons());
    const c3 = cur.toPtr(Cons);
    try testing.expect(c3.car.eq(Value.t));
    cur = c3.cdr;

    try testing.expect(cur.isCons());
    const c4 = cur.toPtr(Cons);
    try testing.expect(c4.car.eq(Value.t));
    cur = c4.cdr;

    try testing.expect(cur.isCons());
    const c5 = cur.toPtr(Cons);
    try testing.expect(c5.car.eq(Value.t));
    cur = c5.cdr;

    try testing.expect(cur.isCons());
    const c6 = cur.toPtr(Cons);
    try testing.expect(c6.car.eq(Value.t));
    cur = c6.cdr;

    try testing.expect(cur.isCons());
    const c7 = cur.toPtr(Cons);
    try testing.expect(c7.car.eq(Value.t));
    cur = c7.cdr;

    try testing.expect(cur.isNil());
}
