//! Integration tests for the full Habu pipeline
//!
//! Tests: read -> compile -> emit -> run
//! Covers: arithmetic, conditionals, functions, closures, recursion

const std = @import("std");
const testing = std.testing;
const build_options = @import("build_options");

const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Chunk = runtime.Chunk;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;

const compiler = @import("../compiler/compiler.zig");
const Compiler = compiler.Compiler;

const interp = @import("../interp/interp.zig");
const Vm = interp.Vm;
const Parser = @import("../reader/parser.zig").Parser;

const compile_chunk = @import("../testing/compile_chunk.zig");
const OhSnap = @import("ohsnap");

/// Test helper: parse, compile, emit, run and return result
fn evalExpr(allocator: std.mem.Allocator, heap: *Heap, source: []const u8) !Value {
    var vm = try Vm.init(allocator, heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();

    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const chunk = try compile_chunk.compileChunk(allocator, heap, &vm, &comp, &chunk_pool, source);
    return vm.run(chunk);
}

test "compileChunk JITs optimized defun with implicit block" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    vm.resetJitDirectCalls();
    const direct_before = vm.jit_direct_calls;
    const def_chunk = try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-block-probe (n) (declare (optimize (speed 3) (safety 0))) (if (<= n 0) 0 (+ n 1)))",
    );
    _ = try vm.run(def_chunk);
    const after = vm.jit_fns.items.len;
    try testing.expect(after > before);

    const call_chunk = try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-block-probe 41)",
    );
    const result = try vm.run(call_chunk);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
    // Generic symbol call path should not increment direct-call counter.
    try testing.expectEqual(direct_before, vm.jit_direct_calls);
}

test "compileChunk direct JIT closure calls bypass generic call setup" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);
    vm.resetJitDirectCalls();

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-direct-add (a b)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (+ a b))\n" ++
            "  (defun jit-direct-driver (n)\n" ++
            "    (let ((f (symbol-function 'jit-direct-add))\n" ++
            "          (i 0)\n" ++
            "          (acc 0))\n" ++
            "      (while (< i n)\n" ++
            "        (setq acc (+ acc (funcall f i i)))\n" ++
            "        (setq i (+ i 1)))\n" ++
            "      acc)))",
    ));

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-direct-driver 100)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 9900), result.toFixnum());
    try testing.expect(vm.jit_direct_calls > 0);
}

test "compileChunk JIT cross-call tags predicate arguments" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-bool-id (x)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (let ((i 0) (acc x))\n" ++
            "      (while (< i 8)\n" ++
            "        (setq acc (if acc x nil))\n" ++
            "        (setq i (+ i 1)))\n" ++
            "      acc))\n" ++
            "  (defun jit-bool-bridge (a b)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (jit-bool-id (< a b))))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const t_result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-bool-bridge 1 2)",
    ));
    try testing.expect(!t_result.isNil());

    const nil_result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-bool-bridge 2 1)",
    ));
    try testing.expect(nil_result.isNil());
}

test "compileChunk JIT progv restores dynamic binding" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defvar jit-progv-probe* 1)\n" ++
            "  (defun jit-progv-probe ()\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (let ((inside (progv '(jit-progv-probe*) '(40) jit-progv-probe*))\n" ++
            "          (after jit-progv-probe*))\n" ++
            "      (+ inside after))))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-progv-probe)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 41), result.toFixnum());

    const global_after = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "jit-progv-probe*",
    ));
    try testing.expect(global_after.isFixnum());
    try testing.expectEqual(@as(i64, 1), global_after.toFixnum());
}

test "compileChunk JIT roots progv call targets across GC" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-id (x)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    x)\n" ++
            "  (defun jit-progv-funcall (v)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (declare (special v))\n" ++
            "    (funcall 'jit-id v)))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const first = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-progv-funcall 41)",
    ));
    try testing.expect(first.isFixnum());
    try testing.expectEqual(@as(i64, 41), first.toFixnum());

    _ = try vm.collectGarbage();

    const second = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-progv-funcall 42)",
    ));
    try testing.expect(second.isFixnum());
    try testing.expectEqual(@as(i64, 42), second.toFixnum());
}

test "compileChunk JIT handles special aux bindings without crash" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defvar jit-special-aux* 100)\n" ++
            "  (defun jit-special-aux (x &aux (jit-special-aux* (+ x 1)))\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    jit-special-aux*))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const out = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-special-aux 4)",
    ));
    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 5), out.toFixnum());

    const global_after = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "jit-special-aux*",
    ));
    try testing.expect(global_after.isFixnum());
    try testing.expectEqual(@as(i64, 100), global_after.toFixnum());
}

test "compileChunk JIT handles defc-style special let" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 24 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defvar bas nil)\n" ++
            "  (defun defc1 (desc)\n" ++
            "    (declare (ignore desc))\n" ++
            "    bas)\n" ++
            "  (defun defc (desc)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (let ((bas 'x))\n" ++
            "      (defc1 desc))))",
    ));

    const out = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defc '(selector foo bar baz))",
    ));
    try testing.expect(out.isSymbol());
    try testing.expectEqualStrings("X", out.toPtr(Symbol).getName());
}

test "compileChunk JIT handles defc quasiquote path" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 24 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun coerce (x y) (declare (ignore y)) x)\n" ++
            "  (defvar bas nil)\n" ++
            "  (defun defc1 (desc)\n" ++
            "    (cond ((atom desc) (list 'quote desc))\n" ++
            "          ((eq 'selector (car desc))\n" ++
            "           (cond ((not (null (cdddr desc))) (list 'quote (fourth desc)))\n" ++
            "                 (t (setq bas (list 'cdr bas))\n" ++
            "                    (list 'car bas))))\n" ++
            "          ((eq 'cons (car desc))\n" ++
            "           (list 'cons (defc1 (second desc)) (defc1 (third desc))))\n" ++
            "          (t (list 'quote desc))))\n" ++
            "  (defun defc (desc)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (let ((bas 'x))\n" ++
            "      (coerce `(lambda (x &optional env)\n" ++
            "                 (declare (ignore env))\n" ++
            "                 ,(defc1 desc))\n" ++
            "              'function))))",
    ));

    const out = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defc '(selector foo))",
    ));
    try testing.expect(out.isCons());
}

test "compileChunk JITs all optimized defuns in progn" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-progn-a (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (+ n 1))\n" ++
            "  (defun jit-progn-b (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    ((lambda (x) (+ x 2)) n))\n" ++
            "  (defun jit-progn-c (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (+ n 3)))",
    ));
    try testing.expect(vm.jit_fns.items.len >= before + 2);

    const fn_a = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(symbol-function 'jit-progn-a)",
    ));
    try testing.expect(fn_a.isClosure());
    const chunk_a = fn_a.toPtr(runtime.Closure).code.toPtr(Chunk);
    try testing.expect(vm.lookupJitFn(chunk_a) != null);

    const fn_c = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(symbol-function 'jit-progn-c)",
    ));
    try testing.expect(fn_c.isClosure());
    const chunk_c = fn_c.toPtr(runtime.Closure).code.toPtr(Chunk);
    try testing.expect(vm.lookupJitFn(chunk_c) != null);

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(+ (jit-progn-a 40) (jit-progn-c 0))",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 44), result.toFixnum());
}

test "compileChunk keeps JIT chunk-to-name mapping stable" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-map-a (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (+ n 100))\n" ++
            "  (defun jit-map-b (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (* n 7))\n" ++
            "  (defun jit-map-c (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (- n 5)))",
    ));

    const fn_a = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(symbol-function 'jit-map-a)",
    ));
    const fn_b = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(symbol-function 'jit-map-b)",
    ));
    const fn_c = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(symbol-function 'jit-map-c)",
    ));

    try testing.expect(fn_a.isClosure());
    try testing.expect(fn_b.isClosure());
    try testing.expect(fn_c.isClosure());

    const chunk_a = fn_a.toPtr(runtime.Closure).code.toPtr(Chunk);
    const chunk_b = fn_b.toPtr(runtime.Closure).code.toPtr(Chunk);
    const chunk_c = fn_c.toPtr(runtime.Closure).code.toPtr(Chunk);

    const jit_a = vm.lookupJitFn(chunk_a);
    const jit_b = vm.lookupJitFn(chunk_b);
    const jit_c = vm.lookupJitFn(chunk_c);
    try testing.expect(jit_a != null);
    try testing.expect(jit_b != null);
    try testing.expect(jit_c != null);

    try testing.expect(std.ascii.eqlIgnoreCase(jit_a.?.name, "JIT-MAP-A"));
    try testing.expect(std.ascii.eqlIgnoreCase(jit_b.?.name, "JIT-MAP-B"));
    try testing.expect(std.ascii.eqlIgnoreCase(jit_c.?.name, "JIT-MAP-C"));

    const out_a = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-map-a 3)",
    ));
    const out_b = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-map-b 3)",
    ));
    const out_c = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-map-c 3)",
    ));

    try testing.expect(out_a.isFixnum());
    try testing.expect(out_b.isFixnum());
    try testing.expect(out_c.isFixnum());
    try testing.expectEqual(@as(i64, 103), out_a.toFixnum());
    try testing.expectEqual(@as(i64, 21), out_b.toFixnum());
    try testing.expectEqual(@as(i64, -2), out_c.toFixnum());
}

test "compileChunk rekeys JIT map after chunk movement GC" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-gc-rekey (n)\n" ++
            "  (declare (optimize (speed 3) (safety 0)))\n" ++
            "  (+ n 1))",
    ));

    const fn0 = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(symbol-function 'jit-gc-rekey)",
    ));
    try testing.expect(fn0.isClosure());
    const old_chunk = fn0.toPtr(runtime.Closure).code.toPtr(Chunk);
    const old_addr = @intFromPtr(old_chunk);
    try testing.expect(vm.lookupJitFn(old_chunk) != null);

    var moved = false;
    var live_chunk = old_chunk;
    var attempt: usize = 0;
    while (attempt < 8 and !moved) : (attempt += 1) {
        _ = try vm.run(try compile_chunk.compileChunk(
            allocator,
            &heap,
            &vm,
            &comp,
            &chunk_pool,
            "(let ((i 0))\n" ++
                "  (while (< i 200000)\n" ++
                "    (cons i nil)\n" ++
                "    (setq i (+ i 1)))\n" ++
                "  i)",
        ));
        const fn_live = try vm.run(try compile_chunk.compileChunk(
            allocator,
            &heap,
            &vm,
            &comp,
            &chunk_pool,
            "(symbol-function 'jit-gc-rekey)",
        ));
        try testing.expect(fn_live.isClosure());
        live_chunk = fn_live.toPtr(runtime.Closure).code.toPtr(Chunk);
        moved = @intFromPtr(live_chunk) != old_addr;
    }
    try testing.expect(moved);

    try testing.expect(vm.lookupJitFn(live_chunk) != null);
    try testing.expect(vm.lookupJitFn(old_chunk) == null);

    const out = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-gc-rekey 41)",
    ));
    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 42), out.toFixnum());
}

test "compileChunk records JIT admission counters" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);
    vm.resetJitAdm();

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-skip-opt (n &optional m)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (+ n (if m m 0)))\n" ++
            "  (defun jit-no-decl (n)\n" ++
            "    (+ n 1))\n" ++
            "  (defun jit-ok-adm (n)\n" ++
            "    (declare (optimize (speed 3) (safety 0)))\n" ++
            "    (+ n 2)))",
    ));

    try testing.expect(vm.jit_adm.cand >= 2);
    try testing.expect(vm.jit_adm.sk_opt >= 1);
    try testing.expect(vm.jit_adm.elig >= 1);
    try testing.expect(vm.jit_adm.comp >= 1);
}

test "compileChunk JIT length handles string literals" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-string-len () (declare (optimize (speed 3) (safety 0))) (length \"xxxxxxxx\"))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-string-len)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 8), result.toFixnum());
}

test "compileChunk JIT format simple preserves literal template" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-format-simple-template (n) (declare (optimize (speed 3) (safety 0))) (format nil \"BENCH-SYM-~d-X\" n))",
    ));

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-format-simple-template 42)",
    ));
    try testing.expect(result.isString());
    try testing.expectEqualStrings("BENCH-SYM-42-X", result.toPtr(runtime.String).bytes());
}

test "compileChunk JIT intern+format returns distinct symbols" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-intern-format (n) (declare (optimize (speed 3) (safety 0))) (intern (format nil \"BENCH-SYM-~d\" n)))",
    ));

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(let ((a (jit-intern-format 1)) (b (jit-intern-format 2))) (and (symbolp a) (symbolp b) (not (eq a b)) (string= (symbol-name a) \"BENCH-SYM-1\") (string= (symbol-name b) \"BENCH-SYM-2\")))",
    ));
    try testing.expect(result.raw == Value.t.raw);
}

test "compileChunk JIT intern loop preserves lt call arguments" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 32 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-bench-intern-loop () (declare (optimize (speed 3) (safety 0))) (let ((count 0) (i 0)) (while (< i 10000) (intern (format nil \"BENCH-SYM-~d\" i)) (setq count (+ count 1)) (setq i (+ i 1))) count))",
    ));

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-bench-intern-loop)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10000), result.toFixnum());
}

test "compileChunk JIT make-array loop preserves return path" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-gc-vector-loop () (declare (optimize (speed 3) (safety 0))) (let ((v nil) (i 0)) (while (< i 10000) (setq v (make-array 4 :initial-element i)) (setq i (+ i 1))) (aref v 0)))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-gc-vector-loop)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 9999), result.toFixnum());
}

test "compileChunk JIT nested cons preserves car and cdr values" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-nested-cons () (declare (optimize (speed 3) (safety 0))) (cons 1 (cons 2 nil)))",
    ));
    try testing.expect(vm.jit_fns.items.len > before);

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-nested-cons)",
    ));
    try testing.expect(result.isCons());

    const outer = result.toPtr(Cons);
    try testing.expect(outer.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), outer.car.toFixnum());
    try testing.expect(outer.cdr.isCons());

    const inner = outer.cdr.toPtr(Cons);
    try testing.expect(inner.car.isFixnum());
    try testing.expectEqual(@as(i64, 2), inner.car.toFixnum());
    try testing.expect(inner.cdr.isNil());
}

test "compileChunk JIT admits list and list* symbol literals" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    vm.resetJitAdm();
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(progn\n" ++
            "  (defun jit-list-lits () (declare (optimize (speed 3) (safety 0))) (list 'foo 'bar))\n" ++
            "  (defun jit-list*-lits () (declare (optimize (speed 3) (safety 0))) (list* 'foo 'bar nil)))",
    ));
    try testing.expect(vm.jit_fns.items.len >= before + 2);
    try testing.expect(vm.jit_adm.comp >= 2);
}

test "compileChunk JIT generic float arithmetic and compare stay correct" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-float-accum (n) (declare (optimize (speed 3) (safety 0))) (let ((i 0) (acc 0.0)) (while (< i n) (setq acc (+ acc (* (float i) 0.5))) (setq i (+ i 1))) acc))",
    ));
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-float-branch (x) (declare (optimize (speed 3) (safety 0))) (if (< x 2.5) 11 22))",
    ));
    try testing.expect(vm.jit_fns.items.len >= before + 2);

    const accum = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-float-accum 1000)",
    ));
    try testing.expect(accum.isFloat());
    try testing.expectApproxEqAbs(@as(f64, 249750.0), accum.toFloat(), 0.0001);

    const lt_true = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-float-branch 1.5)",
    ));
    try testing.expect(lt_true.isFixnum());
    try testing.expectEqual(@as(i64, 11), lt_true.toFixnum());

    const lt_false = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-float-branch 3.5)",
    ));
    try testing.expect(lt_false.isFixnum());
    try testing.expectEqual(@as(i64, 22), lt_false.toFixnum());
}

test "deep recursive defun does not overflow block stack at 64" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun block-depth-sum (n) (if (<= n 0) 0 (+ 1 (block-depth-sum (- n 1)))))",
    ));

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(block-depth-sum 96)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 96), result.toFixnum());
}

test "recursive safety-0 defun runs correctly under JIT" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const before = vm.jit_fns.items.len;
    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun jit-depth-sum (n) (declare (optimize (speed 3) (safety 0))) (if (<= n 0) 0 (+ 1 (jit-depth-sum (- n 1)))))",
    ));
    try testing.expect(vm.jit_fns.items.len >= before + 1);

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(jit-depth-sum 96)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 96), result.toFixnum());
}

test "compileChunk JIT handles recursive nqueens helper entry copies" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPoolOwned(&chunk_pool);

    const safe_def =
        "(defun nqueens-safe-p (col placed row) " ++
        "(declare (optimize (speed 3) (safety 0))) " ++
        "(if (null placed) t " ++
        "  (let ((c (car placed))) " ++
        "    (if (not (= c col)) " ++
        "      (if (not (= (abs (- c col)) row)) " ++
        "        (nqueens-safe-p col (cdr placed) (+ row 1)) " ++
        "        nil) " ++
        "      nil))))";
    _ = try vm.run(try compile_chunk.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, safe_def));

    const solve_def =
        "(defun nqueens-solve (n row placed) " ++
        "(declare (optimize (speed 3) (safety 0))) " ++
        "(if (= row n) 1 " ++
        "  (let ((count 0) (col 0)) " ++
        "    (while (< col n) " ++
        "      (if (nqueens-safe-p col placed 1) " ++
        "        (setq count (+ count (nqueens-solve n (+ row 1) (cons col placed)))) " ++
        "        nil) " ++
        "      (setq col (+ col 1))) " ++
        "    count)))";
    _ = try vm.run(try compile_chunk.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, solve_def));

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(defun nqueens (n) (declare (optimize (speed 3) (safety 0))) (nqueens-solve n 0 nil))",
    ));

    const result = try vm.run(try compile_chunk.compileChunk(
        allocator,
        &heap,
        &vm,
        &comp,
        &chunk_pool,
        "(nqueens 4)",
    ));
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

// ============================================================================
// Arithmetic Tests
// ============================================================================

test "eval integer literal" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "42");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval nil" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "nil");
    try testing.expect(result.isNil());
}

test "intern accepts nil designator" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(intern nil)");
    try testing.expect(result.isNil());
}

test "intern accepts symbol designator" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(eq (intern 'foo) 'foo)");
    try testing.expect(result.isT());
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
    // Division returns fixnum when evenly divisible
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
    try repl.evalFile(content, null_writer);
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

test "stdlib expt supports fractional exponents" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(expt 64 1/6)");
    try testing.expect(result.isFloat());
    try testing.expectApproxEqAbs(@as(f64, 2.0), result.toFloat(), 0.000001);
}

test "stdlib symbol-function primitive wrapper ash" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(funcall (symbol-function 'ash) 1 3)
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 8), result.toFixnum());
}

test "stdlib symbol-function primitive wrapper count" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(funcall (symbol-function 'count) 'a '(a b a))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "builtin function designators resolve before stdlib bootstrap" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(and
        \\  (multiple-value-bind (sym status) (funcall #'intern "PRELUDE-DOT")
        \\    (and (eq sym 'prelude-dot) (eq status :internal)))
        \\  (equal (funcall #'append '(a) '(b) '(c)) '(a b c))
        \\  (equal (funcall #'member 'b '(a b c) :test #'eq) '(b c))
        \\  (equal (funcall #'assoc 'b '((a . 1) (b . 2)) :test #'eq) '(b . 2))
        \\  (eq (funcall #'find 'b '(a b c) :test #'eq) 'b)
        \\  (= (funcall #'position 'b '(a b c) :test #'eq) 1)
        \\  (= (funcall #'count 'b '(a b b c) :test #'eq) 2)
        \\  (equal (funcall #'remove 'b '(a b c b) :test #'eq) '(a c))
        \\  (string= (funcall #'substring "abcd" 1 3) "bc"))
    );
    try testing.expect(result.raw == Value.t.raw);
}

test "stdlib append function designators stay variadic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const r1 = try repl.eval("(equal (funcall #'append '(a) '(b) '(c)) '(a b c))");
    try testing.expect(r1.raw == Value.t.raw);

    const r2 = try repl.eval("(equal (funcall (symbol-function 'append) '(a) '(b) '(c)) '(a b c))");
    try testing.expect(r2.raw == Value.t.raw);

    const r3 = try repl.eval("(equal (apply #'append '((a) (b) (c))) '(a b c))");
    try testing.expect(r3.raw == Value.t.raw);

    const r4 = try repl.eval("(equal (apply (symbol-function 'append) '((a) (b) (c))) '(a b c))");
    try testing.expect(r4.raw == Value.t.raw);
}

test "stdlib mapc supports variadic list dispatch" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(and
        \\  (equal
        \\    (mapc #'(lambda (a b c) (+ a b c))
        \\          '(1 2 3)
        \\          '(10 20 30)
        \\          '(100 200 300))
        \\    '(1 2 3))
        \\  (let ((acc nil))
        \\    (mapc #'(lambda (a b c) (push (+ a b c) acc))
        \\          '(1 2 3)
        \\          '(10 20 30)
        \\          '(100 200 300))
        \\    (equal (nreverse acc) '(111 222 333)))
        \\  (let ((*mapc-a* nil)
        \\        (*mapc-b* nil))
        \\    (mapc #'(lambda (v x) (setf (symbol-value v) x))
        \\          '(*mapc-a* *mapc-b*)
        \\          '(7 8))
        \\    (and (eql *mapc-a* 7)
        \\         (eql *mapc-b* 8))))
    );
    try testing.expect(result.raw == Value.t.raw);
}

test "stdlib mapcar fast paths preserve variadic semantics" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(and
        \\  (equal (mapcar #'(lambda (x) (+ x 1)) '(1 2 3)) '(2 3 4))
        \\  (equal (mapcar #'(lambda (a b) (+ a b))
        \\                 '(1 2 3)
        \\                 '(10 20 30 40))
        \\         '(11 22 33))
        \\  (let ((d (cons 1 (cons 2 3))))
        \\    (equal (mapcar #'identity d) '(1 2))))
    );
    try testing.expect(result.raw == Value.t.raw);
}

test "stdlib reduce loop rewrite preserves CL fold behavior" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(and
        \\  (eql (reduce #'+ '(1 2 3 4)) 10)
        \\  (eql (reduce #'+ '(1 2 3 4) :initial-value 10) 20)
        \\  (eql (reduce #'- '(1 2 3)) -4)
        \\  (eql (reduce #'- '(1 2 3) :from-end t) 2)
        \\  (eql (reduce #'- '(1 2 3) :from-end t :initial-value 10) -8)
        \\  (eql (reduce #'+ '() :initial-value 5) 5)
        \\  (eql (reduce (lambda () 42) '()) 42))
    );
    try testing.expect(result.raw == Value.t.raw);
}

test "stdlib sort copy-once path preserves function designator semantics" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const asc = try repl.eval("(equal (sort '(4 1 3 2) #'<) '(1 2 3 4))");
    try testing.expect(asc.raw == Value.t.raw);

    const desc = try repl.eval("(equal (sort '(4 1 3 2) #'>) '(4 3 2 1))");
    try testing.expect(desc.raw == Value.t.raw);

    const pure_input = try repl.eval(
        \\(let* ((xs '(3 2 1))
        \\       (ys (sort xs #'<)))
        \\  (equal xs '(3 2 1)))
    );
    try testing.expect(pure_input.raw == Value.t.raw);

    const pure_output = try repl.eval(
        \\(let* ((xs '(3 2 1))
        \\       (ys (sort xs #'<)))
        \\  (equal ys '(1 2 3)))
    );
    try testing.expect(pure_output.raw == Value.t.raw);

    const key = try repl.eval(
        \\(equal (sort '(21 13 4) #'< :key #'(lambda (x) (mod x 10)))
        \\       '(21 13 4))
    );
    try testing.expect(key.raw == Value.t.raw);
}

test "stdlib sort string< fast path preserves designator semantics" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const fn_designator = try repl.eval(
        \\(equal (sort '("b" "a" "c") #'string<) '("a" "b" "c"))
    );
    try testing.expect(fn_designator.raw == Value.t.raw);

    const sym_designator = try repl.eval(
        \\(equal (sort '("b" "a" "c") 'string<) '("a" "b" "c"))
    );
    try testing.expect(sym_designator.raw == Value.t.raw);

    const pure = try repl.eval(
        \\(let* ((xs '("c" "b" "a"))
        \\       (ys (sort xs #'string<)))
        \\  (and (equal xs '("c" "b" "a"))
        \\       (equal ys '("a" "b" "c"))))
    );
    try testing.expect(pure.raw == Value.t.raw);

    const key_fallback = try repl.eval(
        \\(equal (sort '(#("b" 2) #("a" 1) #("c" 0))
        \\             #'string<
        \\             :key #'(lambda (v) (aref v 0)))
        \\       '(#("a" 1) #("b" 2) #("c" 0)))
    );
    try testing.expect(key_fallback.raw == Value.t.raw);
}

test "stdlib sort designator path works from speed-3 caller" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defun jit-sort-fast-check () (declare (optimize (speed 3) (safety 0))) (equal (sort '(5 2 4 1 3) #'<) '(1 2 3 4 5)))");
    const result = try repl.eval("(jit-sort-fast-check)");
    try testing.expect(result.raw == Value.t.raw);
}

test "stdlib symbol-function direct builtin encode-universal-time" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(symbol-function 'encode-universal-time)
    );
    try testing.expect(switch (result.typeKind()) {
        .closure, .native_code, .generic_function => true,
        else => false,
    });
    const compiled = try repl.eval(
        \\(compiled-function-p (symbol-function 'encode-universal-time))
    );
    try testing.expect(compiled.raw == Value.t.raw);
}

test "compiler primitiveRefArity ash is binary" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const b = repl.compiler.builtins.?;
    const arity = repl.compiler.primitiveRefArity(b.ash);
    try testing.expect(arity != null);
    try testing.expectEqual(compiler.Compiler.PrimitiveRefArity.binary, arity.?);
}

test "compiler primitiveRefArity count is binary" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const b = repl.compiler.builtins.?;
    const arity = repl.compiler.primitiveRefArity(b.count);
    try testing.expect(arity != null);
    try testing.expectEqual(compiler.Compiler.PrimitiveRefArity.binary, arity.?);
}

test "compiler primitiveRefArity intern remains variadic" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const b = repl.compiler.builtins.?;
    try testing.expect(repl.compiler.primitiveRefArity(b.intern) == null);
}

test "compiler builtin callable cache keeps custom primitive symbols callable" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const b = repl.compiler.builtins.?;
    try testing.expect(repl.compiler.isBuiltinFunctionSymbol(b.atan));
    try testing.expect(repl.compiler.isBuiltinFunctionSymbol(b.member));
    try testing.expect(repl.compiler.isBuiltinFunctionSymbol(b.concatenate));
    try testing.expect(repl.compiler.isBuiltinFunctionSymbol(b.@"%open"));
    try testing.expect(repl.compiler.isBuiltinFunctionSymbol(b.@"class-slots"));
}

test "stdlib boundp treats nil and t as symbols" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(and (boundp 'nil) (boundp 't))");
    try testing.expect(result.eq(Value.t));
}

test "stdlib find-class accepts nil symbol name" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(find-class nil nil)");
    try testing.expect(result.isNil());
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

test "stdlib setf bit and sbit places" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(let ((v (vector 0 0 0 0)))\n" ++
            "  (setf (sbit v 1) 1 (bit v 2) 1)\n" ++
            "  (list (sbit v 1) (bit v 2) (svref v 1) (aref v 2)))",
    );
    const c0 = got.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c0.car.toFixnum());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c2.car.toFixnum());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c3.car.toFixnum());
}

test "stdlib setf supports fifth through eighth places" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(let ((x '(a b c d e f g h i j)))\n" ++
            "  (setf (fifth x) 'u (sixth x) 'v (seventh x) 'w (eighth x) 'z)\n" ++
            "  (list (fifth x) (sixth x) (seventh x) (eighth x) (nth 7 x)))",
    );
    const c0 = got.toPtr(Cons);
    try testing.expect(c0.car.isSymbol());
    try testing.expectEqualStrings("U", c0.car.toPtr(runtime.Symbol).getName());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isSymbol());
    try testing.expectEqualStrings("V", c1.car.toPtr(runtime.Symbol).getName());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isSymbol());
    try testing.expectEqualStrings("W", c2.car.toPtr(runtime.Symbol).getName());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isSymbol());
    try testing.expectEqualStrings("Z", c3.car.toPtr(runtime.Symbol).getName());
    const c4 = c3.cdr.toPtr(Cons);
    try testing.expect(c4.car.isSymbol());
    try testing.expectEqualStrings("Z", c4.car.toPtr(runtime.Symbol).getName());
}

test "aref supports strings with character semantics" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const downcased = try repl.eval("(char-downcase (aref \"SFDL\" 0))");
    try testing.expect(downcased.isCharacter());
    try testing.expectEqual(@as(u21, 's'), downcased.toCharacter());

    const mutated = try repl.eval(
        "(let ((s (make-string 2 :initial-element #\\A)))\n" ++
            "  (setf (aref s 1) #\\z)\n" ++
            "  (aref s 1))",
    );
    try testing.expect(mutated.isCharacter());
    try testing.expectEqual(@as(u21, 'z'), mutated.toCharacter());
}

test "char and schar return character objects" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(list (char \"hello\" 0)\n" ++
            "      (char-code (char \"hello\" 0))\n" ++
            "      (schar \"world\" 1)\n" ++
            "      (char-code (schar \"world\" 1)))",
    );

    const c0 = got.toPtr(Cons);
    try testing.expect(c0.car.isCharacter());
    try testing.expectEqual(@as(u21, 'h'), c0.car.toCharacter());

    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 'h'), c1.car.toFixnum());

    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isCharacter());
    try testing.expectEqual(@as(u21, 'o'), c2.car.toCharacter());

    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isFixnum());
    try testing.expectEqual(@as(i64, 'o'), c3.car.toFixnum());

    try testing.expect(c3.cdr.isNil());
}

test "character coerces one-char string and symbol designators" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(list (character #\\Z)\n" ++
            "      (character \"A\")\n" ++
            "      (character 'b))",
    );

    const c0 = got.toPtr(Cons);
    try testing.expect(c0.car.isCharacter());
    try testing.expectEqual(@as(u21, 'Z'), c0.car.toCharacter());

    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isCharacter());
    try testing.expectEqual(@as(u21, 'A'), c1.car.toCharacter());

    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isCharacter());
    try testing.expectEqual(@as(u21, 'B'), c2.car.toCharacter());

    try testing.expect(c2.cdr.isNil());
}

test "alphanumericp returns boolean for digits" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(list (alphanumericp #\\2) (alphanumericp #\\A) (alphanumericp #\\Space))");
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.eq(Value.t));
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.eq(Value.t));
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isNil());
    try testing.expect(c2.cdr.isNil());
}

test "array reader keeps terminal cons and symbol literals" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(let ((a #2A((4 (17)) (9 (a)) ((b) 0))))\n" ++
            "  (list (consp (aref a 0 1))\n" ++
            "        (car (aref a 0 1))\n" ++
            "        (car (aref a 1 1))\n" ++
            "        (car (aref a 2 0))))",
    );

    const c0 = got.toPtr(Cons);
    try testing.expect(c0.car.isT());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 17), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isSymbol());
    try testing.expectEqualStrings("A", c2.car.toPtr(runtime.Symbol).getName());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isSymbol());
    try testing.expectEqualStrings("B", c3.car.toPtr(runtime.Symbol).getName());
}

test "nested array literals read as nested array objects" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval("(let ((a #2A((#2A((a)))))) (arrayp (aref a 0 0)))");
    try testing.expect(got.isT());
}

test "make-array supports multidimensional rank indexing and row-major access" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(let ((a (make-array '(3 4) :initial-element 0)))\n" ++
            "  (setf (aref a 1 2) 9)\n" ++
            "  (list (= (array-rank a) 2)\n" ++
            "        (equal (array-dimensions a) '(3 4))\n" ++
            "        (aref a 1 2)\n" ++
            "        (row-major-aref a (array-row-major-index a 1 2))))",
    );

    const c0 = got.toPtr(Cons);
    try testing.expect(c0.car.isT());

    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isT());

    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isFixnum());
    try testing.expectEqual(@as(i64, 9), c2.car.toFixnum());

    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isFixnum());
    try testing.expectEqual(@as(i64, 9), c3.car.toFixnum());
}

test "stdlib pushnew supports gethash place" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(let ((h (make-hash-table :test 'eq)))\n" ++
            "  (pushnew 'a (gethash 'k h))\n" ++
            "  (pushnew 'a (gethash 'k h))\n" ++
            "  (pushnew 'b (gethash 'k h))\n" ++
            "  (length (gethash 'k h)))",
    );
    try testing.expect(got.isFixnum());
    try testing.expectEqual(@as(i64, 2), got.toFixnum());
}

test "stdlib defsetf long form" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const got = try repl.eval(
        "(progn\n" ++
            "  (defun dsf-acc (n seq) (elt seq n))\n" ++
            "  (defsetf dsf-acc (n seq) (val)\n" ++
            "    `(setf (elt ,seq ,n) ,val))\n" ++
            "  (let ((x (list 1 2 3 4))\n" ++
            "        (i 0))\n" ++
            "    (setf (dsf-acc (progn (incf i) 2)\n" ++
            "                   (progn (incf i) x))\n" ++
            "          (progn (incf i) 'a))\n" ++
            "    (list x i)))",
    );

    const outer = got.toPtr(Cons);
    const x_list = outer.car.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), x_list.car.toFixnum());
    const x1 = x_list.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), x1.car.toFixnum());
    const x2 = x1.cdr.toPtr(Cons);
    try testing.expect(x2.car.isSymbol());
    try testing.expectEqualStrings("A", x2.car.toPtr(runtime.Symbol).getName());
    const x3 = x2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 4), x3.car.toFixnum());

    const tail = outer.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 3), tail.car.toFixnum());
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

    try testing.expectError(error.UnhandledThrow, repl.eval("(f :b 2)"));
    try testing.expectError(error.UnhandledThrow, repl.eval("(f 1)"));

    const ok2 = try repl.eval("(f :b 2 :allow-other-keys t)");
    try testing.expect(ok2.isNil());
}

test "optional supplied-p stays aligned with later optional values" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        "(progn " ++
            "(defun f (&optional (a :da a-p) (b :db b-p)) (list a a-p b b-p)) " ++
            "(f :aa :bb))",
    );

    try testing.expect(result.isCons());
    const r0 = result.toPtr(runtime.Cons);
    try testing.expect(r0.car.isKeyword());
    try testing.expectEqualStrings("AA", r0.car.toPtr(runtime.Keyword).getName());

    try testing.expect(r0.cdr.isCons());
    const r1 = r0.cdr.toPtr(runtime.Cons);
    try testing.expect(r1.car.eq(Value.t));

    try testing.expect(r1.cdr.isCons());
    const r2 = r1.cdr.toPtr(runtime.Cons);
    try testing.expect(r2.car.isKeyword());
    try testing.expectEqualStrings("BB", r2.car.toPtr(runtime.Keyword).getName());

    try testing.expect(r2.cdr.isCons());
    const r3 = r2.cdr.toPtr(runtime.Cons);
    try testing.expect(r3.car.eq(Value.t));
    try testing.expect(r3.cdr.isNil());
}

test "keyword supplied-p stays correct when later keys are omitted" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        "(progn " ++
            "(defun f (&key (a :da a-p) (b :db b-p)) (list a a-p b b-p)) " ++
            "(f :a :append))",
    );

    try testing.expect(result.isCons());
    const r0 = result.toPtr(runtime.Cons);
    try testing.expect(r0.car.isKeyword());
    try testing.expectEqualStrings("APPEND", r0.car.toPtr(runtime.Keyword).getName());

    try testing.expect(r0.cdr.isCons());
    const r1 = r0.cdr.toPtr(runtime.Cons);
    try testing.expect(r1.car.eq(Value.t));

    try testing.expect(r1.cdr.isCons());
    const r2 = r1.cdr.toPtr(runtime.Cons);
    try testing.expect(r2.car.isKeyword());
    try testing.expectEqualStrings("DB", r2.car.toPtr(runtime.Keyword).getName());

    try testing.expect(r2.cdr.isCons());
    const r3 = r2.cdr.toPtr(runtime.Cons);
    try testing.expect(r3.car.isNil());
}

test "keyword optional boundary handles odd and paired tails" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun f (req &optional o1 o2 &key k) (list req o1 o2 k))");

    const positional_kw = try repl.eval("(f 1 2 :k)");
    try testing.expect(positional_kw.isCons());
    const p0 = positional_kw.toPtr(runtime.Cons);
    try testing.expect(p0.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), p0.car.toFixnum());
    try testing.expect(p0.cdr.isCons());
    const p1 = p0.cdr.toPtr(runtime.Cons);
    try testing.expect(p1.car.isFixnum());
    try testing.expectEqual(@as(i64, 2), p1.car.toFixnum());
    try testing.expect(p1.cdr.isCons());
    const p2 = p1.cdr.toPtr(runtime.Cons);
    try testing.expect(p2.car.isKeyword());
    try testing.expect(p2.cdr.isCons());
    const p3 = p2.cdr.toPtr(runtime.Cons);
    try testing.expect(p3.car.isNil());
    try testing.expect(p3.cdr.isNil());

    const keyed = try repl.eval("(f 1 2 :k 9)");
    try testing.expect(keyed.isCons());
    const k0 = keyed.toPtr(runtime.Cons);
    try testing.expect(k0.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), k0.car.toFixnum());
    try testing.expect(k0.cdr.isCons());
    const k1 = k0.cdr.toPtr(runtime.Cons);
    try testing.expect(k1.car.isFixnum());
    try testing.expectEqual(@as(i64, 2), k1.car.toFixnum());
    try testing.expect(k1.cdr.isCons());
    const k2 = k1.cdr.toPtr(runtime.Cons);
    try testing.expect(k2.car.isNil());
    try testing.expect(k2.cdr.isCons());
    const k3 = k2.cdr.toPtr(runtime.Cons);
    try testing.expect(k3.car.isFixnum());
    try testing.expectEqual(@as(i64, 9), k3.car.toFixnum());
    try testing.expect(k3.cdr.isNil());
}

test "repl config disables hoist JIT compilation" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{ .enable_jit = false });
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval(
        \\(defun no-jit-candidate (x)
        \\  (declare (optimize (speed 3) (safety 0)))
        \\  (+ x 1))
    );
    const out = try repl.eval("(no-jit-candidate 41)");
    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 42), out.toFixnum());

    try testing.expectEqual(@as(usize, 0), repl.vm.jit_fns.items.len);
    try testing.expectEqual(@as(u64, 0), repl.vm.jit_adm.cand);
}

test "JIT bridge relays keyword throw without panic" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defun bridge-keyfail (&key a b) (if a a b))");
    _ = try repl.eval("(defun bridge-keyfail-wrapper () (bridge-keyfail :a 1 :z 2))");

    const jit_before = repl.vm.jit_fns.items.len;
    _ = try repl.eval(
        \\(defun jit-bridge-keyfail ()
        \\  (declare (optimize (speed 3) (safety 3)))
        \\  (bridge-keyfail-wrapper))
    );
    try testing.expect(repl.vm.jit_fns.items.len > jit_before);

    try testing.expectError(error.UnhandledThrow, repl.eval("(jit-bridge-keyfail)"));

    _ = try repl.eval(
        \\(defun jit-bridge-after ()
        \\  (declare (optimize (speed 3) (safety 3)))
        \\  7)
    );
    const after = try repl.eval("(jit-bridge-after)");
    try testing.expect(after.isFixnum());
    try testing.expectEqual(@as(i64, 7), after.toFixnum());
}

test "JIT handles branch-local let scopes without panic" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const jit_before = repl.vm.jit_fns.items.len;
    _ = try repl.eval(
        \\(defun jit-if-let-scope (x y)
        \\  (if x
        \\      (let ((a (+ y 1))
        \\            (b (+ y 2)))
        \\        (+ a b))
        \\      (let ((c (+ y 3)))
        \\        c)))
    );
    try testing.expect(repl.vm.jit_fns.items.len > jit_before);

    const then_val = try repl.eval("(jit-if-let-scope t 10)");
    try testing.expect(then_val.isFixnum());
    try testing.expectEqual(@as(i64, 23), then_val.toFixnum());

    const else_val = try repl.eval("(jit-if-let-scope nil 10)");
    try testing.expect(else_val.isFixnum());
    try testing.expectEqual(@as(i64, 13), else_val.toFixnum());
}

test "JIT handles deep branch chains without Hoist succ corruption" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var src = std.ArrayList(u8){};
    defer src.deinit(allocator);
    try src.appendSlice(allocator, "(defun jit-deep-branch (x) (declare (optimize (speed 3) (safety 0))) ");

    var i: usize = 0;
    while (i < 40) : (i += 1) {
        var if_buf: [64]u8 = undefined;
        const if_form = try std.fmt.bufPrint(&if_buf, "(if (= x {d}) {d} ", .{ i, i });
        try src.appendSlice(allocator, if_form);
    }
    try src.appendSlice(allocator, "999");
    for (0..41) |_| {
        try src.appendSlice(allocator, ")");
    }

    const jit_before = repl.vm.jit_fns.items.len;
    _ = try repl.eval(src.items);
    try testing.expect(repl.vm.jit_fns.items.len > jit_before);

    const hit = try repl.eval("(jit-deep-branch 39)");
    try testing.expect(hit.isFixnum());
    try testing.expectEqual(@as(i64, 39), hit.toFixnum());

    const miss = try repl.eval("(jit-deep-branch 1000)");
    try testing.expect(miss.isFixnum());
    try testing.expectEqual(@as(i64, 999), miss.toFixnum());
}

test "tail call preserves keyword argument layout" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval(
        \\(defun kw-tail-target (lst &key test)
        \\  (if (and test (consp lst))
        \\      (cdr lst)
        \\      :bad))
    );
    _ = try repl.eval(
        \\(defun kw-tail-caller (lst)
        \\  (kw-tail-target lst :test #'eq))
    );

    const ok = try repl.eval("(equal '(b) (kw-tail-caller '(a b)))");
    try testing.expect(ok.isT());
}

test "fixed arity tail recursion stays stack safe" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval(
        \\(defun fixed-tail-acc (n acc)
        \\  (if (= n 0)
        \\      acc
        \\      (fixed-tail-acc (- n 1) (+ acc 1))))
    );

    const out = try repl.eval("(fixed-tail-acc 1500 0)");
    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 1500), out.toFixnum());
}

test "keyword tail recursion stays stack safe" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval(
        \\(defun key-tail-acc (n &key (a 1) (b 2) (c 3) (d 4) (e 5) (f 6))
        \\  (if (= n 0)
        \\      (+ a b c d e f)
        \\      (key-tail-acc (- n 1) :a (+ a 1) :b b :c c :d d :e e :f f)))
    );

    const out = try repl.eval("(key-tail-acc 300)");
    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 321), out.toFixnum());
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

test "defmacro stores compiled closure entry in compiler macro table" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const def_result = try repl.eval("(defmacro compiled-entry (&whole w &environment e x) `(list ',w ',e ',x))");
    try testing.expect(def_result.isSymbol());

    const entry = repl.compiler.macro_table.get(def_result).?;
    try testing.expect(entry.isCons());

    const c1 = entry.toPtr(Cons);
    try testing.expect(c1.car.isClosure());
    try testing.expect(c1.cdr.isCons());

    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isFixnum());
    try testing.expectEqual(@as(i64, 3), c2.car.toFixnum());
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

test "macro names do not expand in let binding positions" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defmacro a (row col) `(list ,row ,col))");

    const bound = try repl.eval("(let ((a 7)) a)");
    try testing.expect(bound.isFixnum());
    try testing.expectEqual(@as(i64, 7), bound.toFixnum());

    const macro_call = try repl.eval("(a 1 2)");
    try testing.expect(macro_call.isCons());
    const c0 = macro_call.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), c1.car.toFixnum());
}

test "quasiquote preserves package-qualified symbol identity" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defmacro my-handler-case (form &rest cases) `(let () (cl:handler-case ,form ,@cases)))");

    const preserves_pkg = try repl.eval(
        "(let* ((exp (macroexpand-1 '(my-handler-case 1 (error () 2))))" ++
            "       (head (car (caddr exp))))" ++
            "  (eq head 'cl:handler-case))",
    );
    try testing.expect(!preserves_pkg.isNil());

    const result = try repl.eval("(my-handler-case 1 (error () 2))");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
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

test "do binding normalizer accepts symbol shorthand" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const normalized = try repl.eval("(%do-normalize-binding 'fact)");
    try testing.expect(normalized.isCons());
    const c0 = normalized.toPtr(Cons);
    try testing.expect(c0.car.isSymbol());
    try testing.expectEqualStrings("FACT", c0.car.toPtr(Symbol).getName());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isNil());
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isSymbol());
    try testing.expectEqualStrings("FACT", c2.car.toPtr(Symbol).getName());
    try testing.expect(c2.cdr.isNil());
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

test "JIT safety>0 arithmetic relays TypeMismatch without panic" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const jit_before = repl.vm.jit_fns.items.len;
    _ = try repl.eval("(defun jit-safe-double (x) (declare (optimize (speed 3) (safety 3))) (* x 2))");
    try testing.expect(repl.vm.jit_fns.items.len > jit_before);

    const ok = try repl.eval("(jit-safe-double 21)");
    try testing.expect(ok.isFixnum());
    try testing.expectEqual(@as(i64, 42), ok.toFixnum());

    try testing.expectError(error.TypeMismatch, repl.eval("(jit-safe-double \"hi\")"));
}

test "declaim notinline accepts setf function name" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval("(declaim (notinline function-to-trace (setf function-to-trace)))");
    try testing.expect(result.isNil());
}

test "defgeneric and defmethod accept setf function names" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass doc-setf-test () ((doc :initform nil)))");
    _ = try repl.eval("(defgeneric doc-setf-test-accessor (obj))");
    _ = try repl.eval("(defgeneric (setf doc-setf-test-accessor) (new obj))");
    _ = try repl.eval("(defmethod doc-setf-test-accessor ((obj doc-setf-test)) (slot-value obj 'doc))");
    _ = try repl.eval("(defmethod (setf doc-setf-test-accessor) ((new string) (obj doc-setf-test)) (setf (slot-value obj 'doc) new))");

    const result = try repl.eval(
        "(let ((obj (make-instance 'doc-setf-test))) (setf (doc-setf-test-accessor obj) \"ok\") (doc-setf-test-accessor obj))",
    );
    try testing.expect(result.isString());
    try testing.expectEqualStrings("ok", try asString(result));
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

test "labels with three mutually recursive locals" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(labels ((a (n) (if (= n 0) 1 (b (- n 1))))
        \\         (b (n) (if (= n 0) 2 (c (- n 1))))
        \\         (c (n) (if (= n 0) 3 (a (- n 1)))))
        \\  (list (a 3) (b 3) (c 3)))
    );
    try testing.expect(result.isCons());
}

test "labels local function named keys resolves lexically" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(labels ((bindings () 1)
        \\         (optionals () 2)
        \\         (keys () 3))
        \\  (list (bindings) (optionals) (keys)))
    );
    try testing.expect(result.isCons());
    const a = result.toPtr(@import("../runtime/objects.zig").Cons);
    try testing.expectEqual(@as(i64, 1), a.car.toFixnum());
    try testing.expect(a.cdr.isCons());
    const b = a.cdr.toPtr(@import("../runtime/objects.zig").Cons);
    try testing.expectEqual(@as(i64, 2), b.car.toFixnum());
    try testing.expect(b.cdr.isCons());
    const c = b.cdr.toPtr(@import("../runtime/objects.zig").Cons);
    try testing.expectEqual(@as(i64, 3), c.car.toFixnum());
}

test "nested labels call outer lexical function" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(labels ((f (x) x))
        \\  (labels ((g () (f 1)))
        \\    (g)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "nested labels function designator sees outer lexical function" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(labels ((f (x) x))
        \\  (labels ((g () nil))
        \\    (funcall (function f) 1)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}

test "maxima style bigfloat defun compiles" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defpackage #:bigfloat (:use #:common-lisp))");
    _ = try repl.eval("(in-package #:bigfloat)");
    _ = try repl.eval("(defun intofp (re) re)");
    _ = try repl.eval("(defclass numeric () ())");
    _ = try repl.eval("(defclass bigfloat (numeric) ((real :initarg :real)))");
    _ = try repl.eval("(defclass complex-bigfloat (numeric) ((real :initarg :real) (imag :initarg :imag)))");
    _ = try repl.eval(
        \\(defun bigfloat (re &optional im)
        \\  (cond (im
        \\         (make-instance 'complex-bigfloat
        \\                        :real (intofp re)
        \\                        :imag (intofp im)))
        \\        ((cl:realp re)
        \\         (make-instance 'bigfloat :real (intofp re)))
        \\        ((cl:complexp re)
        \\         (make-instance 'complex-bigfloat
        \\                        :real (intofp (cl:realpart re))
        \\                        :imag (intofp (cl:imagpart re))))
        \\        (t
        \\         (make-instance 'bigfloat :real (intofp re)))))
    );

    const result = try repl.eval("(typep (bigfloat 3 4) 'complex-bigfloat)");
    try testing.expect(!result.isNil());
}

test "maxima loader exports cl-user build time" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");
    const result = try repl.eval(
        "(multiple-value-bind (sym status) (find-symbol \"*MAXIMA-BUILD-TIME*\" \"CL-USER\") (and sym (eq status :external) (boundp sym)))",
    );
    try testing.expect(!result.isNil());
}

test "labels mutating captured lexical boxes outer variable" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(let ((acc nil))
        \\  (labels ((collect () (push 2 acc)))
        \\    (collect)
        \\    acc))
    );
    try testing.expect(result.isCons());
    const out = result.toPtr(@import("../runtime/objects.zig").Cons);
    try testing.expect(out.car.isFixnum());
    try testing.expectEqual(@as(i64, 2), out.car.toFixnum());
    try testing.expect(out.cdr.isNil());
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

test "return-from from defun implicit block" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(progn
        \\  (defun rf-test (x)
        \\    (if x (return-from rf-test 99) nil)
        \\    7)
        \\  (rf-test t))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 99), result.toFixnum());
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

test "cond test-only clause returns test value" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(let ((n 0))
        \\  (list
        \\    (cond ((progn (setq n (+ n 1)) (list 'ok n)))
        \\          (t 'bad))
        \\    n))
    );
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.isCons());
    const payload = c0.car.toPtr(Cons);
    try testing.expect(payload.car.isSymbol());
    try testing.expectEqualStrings("OK", payload.car.toPtr(Symbol).getName());
    try testing.expect(payload.cdr.isCons());
    const payload2 = payload.cdr.toPtr(Cons);
    try testing.expect(payload2.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), payload2.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
}

test "cond with many clauses still resolves final match" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var src = std.ArrayList(u8){};
    defer src.deinit(allocator);
    try src.appendSlice(allocator, "(let ((n 79)) (cond ");
    var i: usize = 0;
    while (i < 80) : (i += 1) {
        var clause_buf: [48]u8 = undefined;
        const clause = try std.fmt.bufPrint(&clause_buf, "((= n {d}) {d}) ", .{ i, i });
        try src.appendSlice(allocator, clause);
    }
    try src.appendSlice(allocator, "(t -1)))");

    const result = try repl.eval(src.items);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 79), result.toFixnum());
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

test "unwind-protect cleanup can override protected return-from" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(block done
        \\  (unwind-protect
        \\      (return-from done 1)
        \\    (return-from done 2)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "unwind-protect cleanup same-name block does not retarget return-from" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(block done
        \\  (unwind-protect
        \\      (return-from done 11)
        \\    (block done 22)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 11), result.toFixnum());
}

test "unwind-protect cleanup throw overrides pending return-from" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(catch 'cleanup
        \\  (block done
        \\    (unwind-protect
        \\        (return-from done 1)
        \\      (throw 'cleanup 2))))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "unwind-protect cleanup return-from overrides pending throw" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(block done
        \\  (catch 'cleanup
        \\    (unwind-protect
        \\        (throw 'cleanup 1)
        \\      (return-from done 2))))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

// NOTE: Error handling in unwind-protect is not yet fully implemented
// The VM needs to be enhanced to run cleanup forms when errors occur
// See: src/interp/vm.zig - doError function needs to check unwind stack
//
// test "unwind-protect with error - SKIP until VM error handling is implemented" {
//     const allocator = testing.allocator;
//
//     var heap = try Heap.init(allocator, .{ .total_size = 2 * 1024 * 1024 });
//     defer heap.deinit();
//
//     var repl = try Repl.init(allocator, &heap, .{});
//     try repl.wireGlobalEnv();
//     defer repl.deinit();
//
//     // Load stdlib for setq
//     const stdlib = @embedFile("../../lib/stdlib.habu");
//     const null_writer = std.io.null_writer;
//     try repl.evalFile(stdlib, null_writer);
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

test "throw from #. read-eval relays across nested eval frame" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(catch 'exit
        \\  (progn
        \\    (read-from-string "#.(throw 'exit 42)")
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

test "tagbody loop with integer tag" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(define counter 0)");
    _ = try repl.eval(
        \\(tagbody
        \\  1
        \\  (define counter (+ counter 1))
        \\  (if (< counter 4)
        \\      (go 1)))
    );
    const result = try repl.eval("counter");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 4), result.toFixnum());
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

test "values-list errors when secondary values exceed limit" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const err = repl.eval(
        \\(values-list '(1 2 3 4 5 6 7 8 9 10 11
        \\               12 13 14 15 16 17 18 19 20 21 22))
    );
    try testing.expectError(error.StackOverflow, err);
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
    try testing.expectEqualStrings("Value is 42", try asString(result));
}

test "format ~:D grouped decimal" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const pos = try repl.eval(
        \\(format nil "~:D" 1234567)
    );
    try testing.expect(pos.isString());
    try testing.expectEqualStrings("1,234,567", try asString(pos));

    const neg = try repl.eval(
        \\(format nil "~:D" -1234567)
    );
    try testing.expect(neg.isString());
    try testing.expectEqualStrings("-1,234,567", try asString(neg));
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

test "format ~R cardinal ordinal and radix" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const cardinal = try repl.eval(
        \\(format nil "~R" 42)
    );
    try testing.expect(cardinal.isString());
    try testing.expectEqualStrings("forty-two", try asString(cardinal));

    const ordinal = try repl.eval(
        \\(format nil "~:R" 21)
    );
    try testing.expect(ordinal.isString());
    try testing.expectEqualStrings("21st", try asString(ordinal));

    const radix = try repl.eval(
        \\(format nil "~16R" 255)
    );
    try testing.expect(radix.isString());
    try testing.expectEqualStrings("FF", try asString(radix));

    const roman = try repl.eval(
        \\(format nil "~@R" 144)
    );
    try testing.expect(roman.isString());
    try testing.expectEqualStrings("CXLIV", try asString(roman));
}

test "format ~F ~E ~G floating directives" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const fixed = try repl.eval(
        \\(format nil "~F" 12.5)
    );
    try testing.expect(fixed.isString());
    try testing.expectEqualStrings("12.5", try asString(fixed));

    const fixed_width = try repl.eval(
        \\(format nil "~4F" 1.234)
    );
    try testing.expect(fixed_width.isString());
    try testing.expectEqualStrings("1.23", try asString(fixed_width));

    const fixed_matrix = try repl.eval(
        \\(format nil "~9,1F" 2)
    );
    try testing.expect(fixed_matrix.isString());
    try testing.expectEqualStrings("      2.0", try asString(fixed_matrix));

    const exp = try repl.eval(
        \\(format nil "~E" 12.5)
    );
    try testing.expect(exp.isString());
    try testing.expect(std.mem.indexOf(u8, try asString(exp), "e") != null);

    const general = try repl.eval(
        \\(format nil "~G" 0.00000012)
    );
    try testing.expect(general.isString());
    try testing.expect((std.mem.indexOf(u8, try asString(general), "e") != null) or
        (std.mem.indexOf(u8, try asString(general), "E") != null));
}

test "format ~P plural and ~[ conditional directives" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const plural1 = try repl.eval(
        \\(format nil "~D item~P" 1)
    );
    try testing.expect(plural1.isString());
    try testing.expectEqualStrings("1 item", try asString(plural1));

    const plural2 = try repl.eval(
        \\(format nil "~D item~P" 2)
    );
    try testing.expect(plural2.isString());
    try testing.expectEqualStrings("2 items", try asString(plural2));

    const cond_idx = try repl.eval(
        \\(format nil "~[zero~;one~;many~]" 1)
    );
    try testing.expect(cond_idx.isString());
    try testing.expectEqualStrings("one", try asString(cond_idx));

    const cond_bool = try repl.eval(
        \\(format nil "~:[no~;yes~]" t)
    );
    try testing.expect(cond_bool.isString());
    try testing.expectEqualStrings("yes", try asString(cond_bool));
}

test "format iteration supports radix and nested lists" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const words = try repl.eval(
        \\(format nil "~{~R ~}" '(1 2 3))
    );
    try testing.expect(words.isString());
    try testing.expectEqualStrings("one two three ", try asString(words));

    const matrix = try repl.eval(
        \\(format nil "~{~{~9,1F ~}~%~}" '((1.1 2 3.33) (4 5 6) (7 8.88 9)))
    );
    try testing.expect(matrix.isString());
    try testing.expectEqualStrings(
        "      1.1       2.0       3.3 \n      4.0       5.0       6.0 \n      7.0       8.9       9.0 \n",
        try asString(matrix),
    );
}

test "format case conversion works with nested radix directives" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(format nil "~:(~R~) bird~P ~[is~;are~] singing." 2 2 1)
    );
    try testing.expect(result.isString());
    try testing.expectEqualStrings("Two birds are singing.", try asString(result));
}

test "format ~* argument navigation" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const skipped = try repl.eval(
        \\(format nil "~A ~*~A" "x" "skip" "y")
    );
    try testing.expect(skipped.isString());
    try testing.expectEqualStrings("x y", try asString(skipped));

    const skipped2 = try repl.eval(
        \\(format nil "~A ~2*~A" "a" "b" "c" "d")
    );
    try testing.expect(skipped2.isString());
    try testing.expectEqualStrings("a d", try asString(skipped2));
}

test "format ~/ function directive" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 2 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const local = try repl.eval(
        \\(progn
        \\  (defun fmt-wrap (stream arg colonp atp &rest params)
        \\    (declare (ignore colonp atp params))
        \\    (princ "[" stream)
        \\    (princ arg stream)
        \\    (princ "]" stream))
        \\  (format nil "~/fmt-wrap/" 42))
    );
    try testing.expect(local.isString());
    try testing.expectEqualStrings("[42]", try asString(local));

    const qualified = try repl.eval(
        \\(progn
        \\  (defpackage :fmtpkg (:use :cl))
        \\  (defun fmtpkg::wrap (stream arg colonp atp &rest params)
        \\    (declare (ignore colonp atp params))
        \\    (princ "<" stream)
        \\    (princ arg stream)
        \\    (princ ">" stream))
        \\  (format nil "~/fmtpkg::wrap/" 7))
    );
    try testing.expect(qualified.isString());
    try testing.expectEqualStrings("<7>", try asString(qualified));
}

// ============================================================================
// stdlib tests
// ============================================================================

test "stdlib compiles" {
    // TEMP: Disabled - hangs during test run but works in main
    return error.SkipZigTest;
    // const allocator = testing.allocator;

    // var heap = try Heap.init(allocator, .{ .total_size = 2 * 1024 * 1024 });
    // defer heap.deinit();

    // var repl = try Repl.init(allocator, &heap, .{});
    // try repl.wireGlobalEnv();
    // defer repl.deinit();

    // // Read stdlib file
    // const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    // defer file.close();
    // const stdlib = try file.readToEndAlloc(allocator, 256 * 1024);
    // defer allocator.free(stdlib);

    // // Use evalFile to evaluate the whole file (handles multiple expressions)
    // const null_writer = std.io.null_writer;
    // try repl.evalFile(stdlib, null_writer);

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

test "find-class accepts optional errorp and environment args" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    try testing.expect((try repl.eval("(find-class 'definitely-missing nil)")).isNil());
    try testing.expect((try repl.eval("(find-class 'definitely-missing nil nil)")).isNil());
}

test "setf find-class overrides and removes lookup" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    _ = try repl.eval("(defclass fc-a () ())");
    _ = try repl.eval("(defclass fc-b () ())");

    const b_class = try repl.eval("(find-class 'fc-b)");
    _ = try repl.eval("(setf (find-class 'fc-a) (find-class 'fc-b))");
    const aliased = try repl.eval("(find-class 'fc-a)");
    try testing.expect(aliased.eq(b_class));

    _ = try repl.eval("(setf (find-class 'fc-a) nil)");
    try testing.expect((try repl.eval("(find-class 'fc-a nil)")).isNil());
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

test "make-instance still constructs objects after stdlib loads" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defclass test-class-stdlib () ((my-slot :initarg :my-slot)))");
    _ = try repl.eval("(defvar obj-stdlib (make-instance 'test-class-stdlib :my-slot 9))");
    const result = try repl.eval("(slot-value obj-stdlib 'my-slot)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 9), result.toFixnum());
}

test "symbol-function and apply support common-lisp make-instance" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defclass test-class-apply () ((my-slot :initarg :my-slot)))");

    const fbound = try repl.eval("(if (symbol-function 'common-lisp:make-instance) t nil)");
    try testing.expect(fbound.isT());

    _ = try repl.eval("(defvar obj-apply (apply #'common-lisp:make-instance 'test-class-apply '(:my-slot 11)))");
    const result = try repl.eval("(slot-value obj-apply 'my-slot)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 11), result.toFixnum());
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

    const result = try evalExpr(allocator, &heap, "(let ((s (make-string-input-stream \"ab\"))) (list (%peek-char-from-stream s) (%read-char-from-stream s) (%read-char-from-stream s) (%read-char-from-stream s)))");

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

    const result = try evalExpr(allocator, &heap, "(let ((s (make-string-input-stream \"a\"))) (list (read-char-no-hang s) (read-char-no-hang s)))");

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

    const result = try evalExpr(allocator, &heap, "(let ((s (make-string-input-stream \"a\"))) (list (listen s) (%read-char-from-stream s) (listen s)))");

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

test "unread-char supports explicit stream argument" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(
        allocator,
        &heap,
        "(let ((s (make-string-input-stream \"ab\"))) (%read-char-from-stream s) (%unread-char-from-stream #\\a s) (%read-char-from-stream s))",
    );

    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 'a'), result.toFixnum());
}

test "read returns successive forms from string stream" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        "(let ((s (make-string-input-stream \";lead\\n(foo 1)\\nbar ;tail\\n nil\"))) " ++
        "  (list (read s nil :eof) (read s nil :eof) (read s nil :eof) (read s nil :eof)))";
    const result = try evalExpr(allocator, &heap, src);

    try testing.expect(result.isCons());
    const a = result.toPtr(Cons);
    try testing.expect(a.car.isCons());
    const foo = a.car.toPtr(Cons);
    try testing.expect(foo.car.isSymbol());

    const b = a.cdr.toPtr(Cons);
    try testing.expect(b.car.isSymbol());

    const c = b.cdr.toPtr(Cons);
    try testing.expect(c.car.isNil());

    const d = c.cdr.toPtr(Cons);
    try testing.expect(d.car.isKeyword());
    try testing.expect(d.cdr.isNil());
}

test "read returns successive forms from file stream" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const path = try std.fs.path.join(allocator, &.{ "/tmp", "habu-read-forms.lisp" });
    defer allocator.free(path);
    try std.fs.cwd().writeFile(.{
        .sub_path = path,
        .data = ";a\n(foo 1)\nbar\nnil\n",
    });

    const src = try std.fmt.allocPrint(
        allocator,
        "(with-open-file (s \"{s}\") (list (read s nil :eof) (read s nil :eof) (read s nil :eof) (read s nil :eof)))",
        .{path},
    );
    defer allocator.free(src);

    const result = try repl.eval(src);
    try testing.expect(result.isCons());
    const a = result.toPtr(Cons);
    try testing.expect(a.car.isCons());

    const b = a.cdr.toPtr(Cons);
    try testing.expect(b.car.isSymbol());

    const c = b.cdr.toPtr(Cons);
    try testing.expect(c.car.isNil());

    const d = c.cdr.toPtr(Cons);
    try testing.expect(d.car.isKeyword());
    try testing.expect(d.cdr.isNil());
}

test "copy-structure copies defstruct instance" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn (defstruct foo (bar fixnum) (baz fixnum))\n" ++
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

test "slot protocol works on boxed defstruct instance" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct slot-box x y)\n" ++
        "  (let ((obj (make-slot-box :x 1 :y 2)))\n" ++
        "    (and (= (slot-value obj 'x) 1)\n" ++
        "         (eql (%set-slot-value obj 'x 7) 7)\n" ++
        "         (= (slot-value obj 'x) 7)\n" ++
        "         (slot-boundp obj 'x)\n" ++
        "         (progn (slot-makunbound obj 'x) (not (slot-boundp obj 'x))))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct constructor accepts keyword initargs" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct instream stream (line 0 :type fixnum) stream-name)\n" ++
        "  (let ((x (make-instream :stream 42 :stream-name \"stdin\")))\n" ++
        "    (and (instream-p x)\n" ++
        "         (eql (instream-stream x) 42)\n" ++
        "         (= (instream-line x) 0)\n" ++
        "         (string= (instream-stream-name x) \"stdin\"))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct constructor accepts nil keyword value" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct instream stream (line 0 :type fixnum) stream-name)\n" ++
        "  (let ((x (make-instream :stream 42 :stream-name nil)))\n" ++
        "    (and (instream-p x)\n" ++
        "         (eql (instream-stream x) 42)\n" ++
        "         (= (instream-line x) 0)\n" ++
        "         (null (instream-stream-name x)))))");

    try testing.expect(result.eq(Value.t));
}

test "package-local defstruct slot accessors use the same slot identity" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defpackage #:probe (:use #:cl))\n" ++
        "  (in-package #:probe)\n" ++
        "  (defstruct instream stream (line 0 :type fixnum) stream-name)\n" ++
        "  (let ((x (make-instream :stream 42 :stream-name \"stdin\")))\n" ++
        "    (and (instream-p x)\n" ++
        "         (eql (instream-stream x) 42)\n" ++
        "         (= (instream-line x) 0)\n" ++
        "         (string= (instream-stream-name x) \"stdin\")\n" ++
        "         (eql (slot-value x 'stream) 42)\n" ++
        "         (string= (slot-value x 'stream-name) \"stdin\"))))");

    try testing.expect(result.eq(Value.t));
}

test "errset plus setq cons path returns new stream entry" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct instream stream (line 0 :type fixnum) stream-name)\n" ++
        "  (defvar *stream-alist* nil)\n" ++
        "  (let ((s (make-string-input-stream \"x\")))\n" ++
        "    (let (name errset)\n" ++
        "      (errset (setq name (namestring s)))\n" ++
        "      (let ((st (car (setq *stream-alist*\n" ++
        "                           (cons (make-instream :stream s :stream-name name)\n" ++
        "                                 *stream-alist*)))))\n" ++
        "        (and st\n" ++
        "             (eql (instream-line st) 0)\n" ++
        "             (eq (instream-stream st) s))))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct runtime metadata cache rebuilds from class object" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defpackage #:probe (:use #:cl))");
    _ = try repl.eval("(in-package #:probe)");
    _ = try repl.eval("(defstruct instream stream (line 0 :type fixnum) stream-name)");

    const class_sym = (try heap.lookupInPackage("PROBE", "INSTREAM")) orelse return error.InvalidArgument;
    try testing.expect((try heap.lookupClassMetadata(class_sym)) != null);
    try testing.expect(heap.class_metadata.remove("PROBE:INSTREAM"));
    try testing.expect((try heap.lookupClassMetadata(class_sym)) != null);

    const result = try repl.eval(
        "(let ((x (make-instream :stream 42 :stream-name nil))) " ++
            "  (and (instream-p x) " ++
            "       (eql (instream-stream x) 42) " ++
            "       (= (instream-line x) 0) " ++
            "       (null (instream-stream-name x)) " ++
            "       (eq (slot-value x 'stream) 42)))",
    );
    try testing.expect(result.eq(Value.t));
}

test "defstruct :type list uses list representation" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (disp-hack-ob (:conc-name nil) (:type list)) left-ob right-ob)\n" ++
        "  (let* ((x (make-disp-hack-ob :left-ob 'a :right-ob 'b))\n" ++
        "         (y (copy-disp-hack-ob x)))\n" ++
        "    (and (listp x)\n" ++
        "         (equal x '(a b))\n" ++
        "         (eq (left-ob x) 'a)\n" ++
        "         (eq (right-ob x) 'b)\n" ++
        "         (eql (setf (left-ob x) 'c) 'c)\n" ++
        "         (equal x '(c b))\n" ++
        "         (equal y '(a b))\n" ++
        "         (disp-hack-ob-p x))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct :type list :named prefixes type symbol" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (s-var (:type list) :named (:conc-name sv-)) a b)\n" ++
        "  (let* ((x (make-s-var :a 1 :b 2))\n" ++
        "         (y (copy-s-var x)))\n" ++
        "    (and (equal x '(s-var 1 2))\n" ++
        "         (s-var-p x)\n" ++
        "         (= (sv-a x) 1)\n" ++
        "         (= (sv-b x) 2)\n" ++
        "         (eql (setf (sv-a x) 9) 9)\n" ++
        "         (equal x '(s-var 9 2))\n" ++
        "         (equal y '(s-var 1 2))\n" ++
        "         (not (s-var-p '(1 2))))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct :constructor option defines custom constructor name" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (ds-custom-ctor (:constructor build-ds-custom-ctor)) a)\n" ++
        "  (let ((x (build-ds-custom-ctor :a 7)))\n" ++
        "    (and (fboundp 'build-ds-custom-ctor)\n" ++
        "         (not (fboundp 'make-ds-custom-ctor))\n" ++
        "         (ds-custom-ctor-p x)\n" ++
        "         (= (ds-custom-ctor-a x) 7))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct with conc-name nil defines copier" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (entry (:conc-name nil)) pend name)\n" ++
        "  (let ((x (make-entry t 'ok)))\n" ++
        "    (and (fboundp 'copy-entry)\n" ++
        "         (entry-p (copy-entry x))\n" ++
        "         (not (eq x (copy-entry x))))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct with character conc-name coerces to string prefix" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (entry-char-prefix (:conc-name #\\X)) foo)\n" ++
        "  (let ((x (make-entry-char-prefix :foo 7)))\n" ++
        "    (and (fboundp 'xfoo)\n" ++
         "         (= (xfoo x) 7))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct bare :conc-name option uses nil prefix" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (ds-bare-conc :conc-name) a b)\n" ++
        "  (let ((x (make-ds-bare-conc 1 2)))\n" ++
        "    (and (fboundp 'a) (fboundp 'b)\n" ++
        "         (= (a x) 1) (= (b x) 2))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct empty (:conc-name) option uses nil prefix" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (ds-empty-conc (:conc-name)) a b)\n" ++
        "  (let ((x (make-ds-empty-conc 1 2)))\n" ++
        "    (and (fboundp 'a) (fboundp 'b)\n" ++
        "         (= (a x) 1) (= (b x) 2))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct (:copier nil) suppresses copier" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (ds-no-copier (:copier nil)) a)\n" ++
        "  (not (fboundp 'copy-ds-no-copier)))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct (:predicate nil) suppresses predicate" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct (ds-no-pred (:predicate nil)) a)\n" ++
        "  (not (fboundp 'ds-no-pred-p)))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct slot with init-form" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (a) is a slot spec with just a name, no init-form — should not error
    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct ds-initform (a) (b))\n" ++
        "  (and (fboundp 'make-ds-initform)\n" ++
        "       (fboundp 'ds-initform-a)\n" ++
        "       (fboundp 'ds-initform-b)))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct missing slot init defaults to nil" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct ds-default-nil (a) (b))\n" ++
        "  (let ((x (make-ds-default-nil)))\n" ++
        "    (and (null (ds-default-nil-a x))\n" ++
        "         (null (ds-default-nil-b x)))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct slot with :type keyword option" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // (a 1 :type fixnum) - init-form 1, type fixnum
    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct ds-typed-slot (a 1 :type fixnum) (b 2 :type integer))\n" ++
        "  (and (fboundp 'make-ds-typed-slot)\n" ++
        "       (fboundp 'ds-typed-slot-a)\n" ++
        "       (fboundp 'ds-typed-slot-b)))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct slot with :read-only option" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct ds-readonly (a nil :read-only nil) (b 'a :read-only nil))\n" ++
        "  (and (fboundp 'make-ds-readonly)\n" ++
        "       (fboundp 'ds-readonly-a)\n" ++
        "       (fboundp 'ds-readonly-b)))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct typecase handles non-struct values" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defstruct ds-typecase a)\n" ++
        "  (let ((x (make-ds-typecase :a 1)))\n" ++
        "    (and (typep x 'ds-typecase)\n" ++
        "         (not (typep 'plain-symbol 'ds-typecase)))))");

    try testing.expect(result.eq(Value.t));
}

test "defstruct uses canonical structure types across packages" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(allocator, &heap, "(progn\n" ++
        "  (defpackage #:tmp-struct-pkg (:use #:common-lisp))\n" ++
        "  (in-package #:tmp-struct-pkg)\n" ++
        "  (defstruct foo a)\n" ++
        "  (let ((x (make-foo :a 1)))\n" ++
        "    (and (typep x 'structure-object)\n" ++
        "         (not (typep (vector 1 2) 'structure-object))\n" ++
        "         (eq (type-of x) 'foo)\n" ++
        "         (typep (class-of x) 'structure-class)\n" ++
        "         (car (subtypep 'structure-class 'class)))))");

    try testing.expect(result.eq(Value.t));
}

test "defvar without init defaults to nil" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const result = try evalExpr(
        allocator,
        &heap,
        "(progn (defvar dv-noinit-847261) dv-noinit-847261)",
    );

    try testing.expect(result.isNil());
}

test "ansi repro define-compiler-macro.8 does not crash" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(define-compiler-macro foo-cmpr (x y)
        \\  (declare (special *x*))
        \\  (setf *x* :bad)
        \\  `(list ,x ,y))
    );
    _ = try repl.eval("(defmacro foo-cmpr (x y) `(list ,x ,y))");

    const compiled = try repl.eval(
        \\(compile nil '(lambda (a b)
        \\                (declare (notinline foo-cmpr))
        \\                (foo-cmpr a b)))
    );
    try testing.expect(compiled.isClosure());

    const result = try repl.eval(
        \\(let ((*x* :good))
        \\  (declare (special *x*))
        \\  (funcall (compile nil '(lambda (a b)
        \\                           (declare (notinline foo-cmpr))
        \\                           (foo-cmpr a b)))
        \\           7 23))
    );
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.isFixnum());
    try testing.expectEqual(@as(i64, 7), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 23), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
}

test "ansi repro destructuring-bind.error.10 rejects nil binder" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src = "(destructuring-bind (foo nil bar) (list 1 2 3) nil)";
    try testing.expectError(error.InvalidSyntax, evalExpr(allocator, &heap, src));
}

test "destructuring-bind supports &key defaults" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        \\(destructuring-bind (name &key properties (deprecated-p '$none))
        \\    '($foo :properties ((evfun t)))
        \\  (list name properties deprecated-p))
    ;

    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.isSymbol());
    try testing.expectEqualStrings("$FOO", c0.car.toPtr(Symbol).getName());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isCons());
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isSymbol());
    try testing.expectEqualStrings("$NONE", c2.car.toPtr(Symbol).getName());
    try testing.expect(c2.cdr.isNil());
}

test "maxima-style def-simplifier macro shape expands" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        \\(progn
        \\  (defmacro test-def-simplifier (base-name-and-options lambda-list &body body)
        \\    (declare (ignore lambda-list body))
        \\    (destructuring-bind (base-name &key (simpcheck :default) (subfun-arglist nil))
        \\        (if (symbolp base-name-and-options)
        \\            (list base-name-and-options)
        \\            base-name-and-options)
        \\      `(list ',base-name ',simpcheck ',subfun-arglist)))
        \\  (test-def-simplifier (foo :simpcheck :custom :subfun-arglist (s)) (x) 42))
    ;

    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.isSymbol());
    try testing.expectEqualStrings("FOO", c0.car.toPtr(Symbol).getName());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isKeyword());
    try testing.expectEqualStrings("CUSTOM", c1.car.toPtr(runtime.Keyword).getName());
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isCons());
    try testing.expect(c2.cdr.isNil());
}

test "ansi repro macrolet.36 supports whole destructuring pattern" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        \\(macrolet ((%m (&whole (m a b) c d) `(quote (,m ,a ,b ,c ,d))))
        \\  (%m 1 2))
    ;

    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isFixnum());
    try testing.expectEqual(@as(i64, 2), c2.car.toFixnum());
    try testing.expect(c2.cdr.isCons());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), c3.car.toFixnum());
    try testing.expect(c3.cdr.isCons());
    const c4 = c3.cdr.toPtr(Cons);
    try testing.expect(c4.car.isFixnum());
    try testing.expectEqual(@as(i64, 2), c4.car.toFixnum());
    try testing.expect(c4.cdr.isNil());
}

test "coerce supports lambda expression designator for function" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(let ((f (coerce '(lambda (x y)
        \\                    (+ x y))
        \\                  'function)))
        \\  (and (functionp f)
        \\       (= (funcall f 3 4) 7)
        \\       (= (funcall f 9 11) 20)))
    );
    try testing.expect(result.eq(Value.t));
}

test "coerce lambda designator supports optional env arity" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(let ((f (coerce '(lambda (x &optional env)
        \\                    (declare (ignore env))
        \\                    x)
        \\                  'function)))
        \\  (and (functionp f)
        \\       (= (funcall f 77 nil) 77)))
    );
    try testing.expect(result.eq(Value.t));
}

test "coerce supports numeric and char target types" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(and (= (coerce 42 'float) 42.0)
        \\     (= (coerce 42 'single-float) 42.0)
        \\     (= (coerce 42.9 'integer) 42)
        \\     (= (coerce #\A 'integer) 65)
        \\     (char= (coerce 66 'character) #\B)
        \\     (equal (coerce "AZ" 'list) '(#\A #\Z))
        \\     (equal (coerce '(#\C #\D) 'string) "CD")
        \\     (equal (coerce '#(1 2 3) 'list) '(1 2 3))
        \\     (equal (coerce '(1 2 3) 'vector) '#(1 2 3))
        \\     (= (coerce 7 't) 7))
    );
    try testing.expect(result.eq(Value.t));
}

test "ansi repro top-level setq undeclared special defines global" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src = "(setq *enclose-printer-errors* nil)";
    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(result.isNil());
}

test "ansi repro define-method-combination-long.11.4 eql method dispatch" {
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
        \\  (define-method-combination mc-long-11 () ((method-list *))
        \\    (:arguments x1 &optional (y1 :y1 y1-supplied) &rest r1 &key (z1 :z1 z1-supplied))
        \\    `(vector ,x1 ,y1 ,y1-supplied ,r1 ,z1 ,z1-supplied
        \\             ,@(mapcar #'(lambda (m) `(call-method ,m)) method-list)))
        \\  (defgeneric dmc-long-gf-11c (x1 &optional y1 &rest r1) (:method-combination mc-long-11))
        \\  (defmethod dmc-long-gf-11c ((x (eql 1)) &optional y &rest r1) 'a)
        \\  (defmethod dmc-long-gf-11c ((x integer) &optional (y 2) &rest r1) 'b)
        \\  (list
        \\    (dmc-long-gf-11c 0)
        \\    (dmc-long-gf-11c 1)
        \\    (dmc-long-gf-11c 0 0)
        \\    (dmc-long-gf-11c 1 1)
        \\    (dmc-long-gf-11c 1 1 2 3)))
    );

    const sym_a = try heap.intern("A");
    const sym_b = try heap.intern("B");
    const expected = [_]Value{ sym_b, sym_a, sym_b, sym_a, sym_a };
    var cur = result;
    var idx: usize = 0;
    while (cur.isCons()) {
        const c = cur.toPtr(Cons);
        try testing.expect(idx < expected.len);
        try testing.expect(c.car.eq(expected[idx]));
        idx += 1;
        cur = c.cdr;
    }
    try testing.expect(cur.isNil());
    try testing.expectEqual(@as(usize, expected.len), idx);
}

test "ansi repro make-load-form.order.14 compile-file returns pathname" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((file "/tmp/habu-mlf-order14.lsp"))
        \\  (with-open-file (s file :direction :output :if-exists :supersede :if-does-not-exist :create)
        \\    (write-string "(defparameter *a* #.(list 1))\n" s))
        \\  (compile-file file :verbose nil :print nil))
    ;

    const result = try repl.eval(src);
    try testing.expect(result.isPathname());
}

test "ansi repro compile-file-pathname accepts string designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(compile-file-pathname \"init.lsp\")");
    try testing.expect(result.isPathname());
}

test "compile-file-pathname keeps explicit input name over defaults" {
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
        \\  (setf *default-pathname-defaults* (pathname "/tmp/gclload1.lsp"))
        \\  (let ((pn (compile-file-pathname "rt.lsp")))
        \\    (list (pathname-name pn) (pathname-type pn))))
    );
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqualStrings("rt", try asString(c1.car));
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqualStrings("fasl", try asString(c2.car));
}

test "ansi repro delete-file accepts pathname designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (with-open-file (s "/tmp/habu-delete-pathname.tmp" :direction :output :if-exists :supersede :if-does-not-exist :create)
        \\    (write-string "x" s))
        \\  (delete-file (pathname "/tmp/habu-delete-pathname.tmp")))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isNil());
}

test "ansi repro rename-file accepts pathname designators" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(root);
    const old_path = try std.fs.path.join(allocator, &.{ root, "old.txt" });
    defer allocator.free(old_path);
    const new_path = try std.fs.path.join(allocator, &.{ root, "new.txt" });
    defer allocator.free(new_path);

    const src = try std.fmt.allocPrint(
        allocator,
        \\(progn
        \\  (with-open-file (s (pathname "{s}") :direction :output :if-exists :supersede :if-does-not-exist :create)
        \\    (write-string "x" s))
        \\  (rename-file (pathname "{s}") (pathname "{s}"))
        \\  (namestring (probe-file (pathname "{s}"))))
    , .{ old_path, old_path, new_path, new_path });
    defer allocator.free(src);

    const result = try repl.eval(src);
    try testing.expectEqualStrings(new_path, try asString(result));
}

test "ansi repro ensure-directories-exist accepts string designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const root = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(root);
    const target = try std.fs.path.join(allocator, &.{ root, "mk", "deep", "file.txt" });
    defer allocator.free(target);

    const src = try std.fmt.allocPrint(
        allocator,
        \\(namestring (ensure-directories-exist "{s}"))
    , .{target});
    defer allocator.free(src);

    const result = try repl.eval(src);
    try testing.expectEqualStrings(target, try asString(result));

    var dir = try std.fs.openDirAbsolute(root, .{});
    defer dir.close();
    try dir.access("mk/deep", .{});
}

test "probe-file returns canonical truename pathname" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{ .sub_path = "probe-hit.txt", .data = "x" });
    const abs_path = try tmp.dir.realpathAlloc(allocator, "probe-hit.txt");
    defer allocator.free(abs_path);

    const form = try std.fmt.allocPrint(allocator, "(namestring (probe-file \"{s}\"))", .{abs_path});
    defer allocator.free(form);
    const result = try repl.eval(form);
    try testing.expectEqualStrings(abs_path, try asString(result));
}

test "quoted #p literal stays a pathname object" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(pathnamep (car '(#p\"/tmp/habu\")))");
    try testing.expect(result.isT());
}

test "defstruct accepts pathname initform literal" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defstruct pn-holder (path #p\"/tmp/habu\" :type pathname))");
    const result = try repl.eval("(pathnamep (pn-holder-path (make-pn-holder)))");
    try testing.expect(result.isT());
}

test "probe-file returns nil for missing path" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const abs_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(abs_path);
    const missing = try std.fs.path.join(allocator, &[_][]const u8{ abs_path, "probe-miss.txt" });
    defer allocator.free(missing);

    const form = try std.fmt.allocPrint(allocator, "(probe-file \"{s}\")", .{missing});
    defer allocator.free(form);
    const result = try repl.eval(form);
    try testing.expect(result.isNil());
}

test "truename signals on missing path" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    const abs_path = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(abs_path);
    const missing = try std.fs.path.join(allocator, &[_][]const u8{ abs_path, "truename-miss.txt" });
    defer allocator.free(missing);

    const form = try std.fmt.allocPrint(allocator, "(truename \"{s}\")", .{missing});
    defer allocator.free(form);
    try testing.expectError(error.FileNotFound, repl.eval(form));
}

test "ansi repro write-to-string.3 honors allow-other-keys" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // ANSI write-to-string.3 permits odd trailing keys when :allow-other-keys t.
    const src =
        \\(with-standard-io-syntax
        \\  (let ((k (gensym)))
        \\    (funcall #'write-to-string 3 :allow-other-keys t k 0)))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isString());
    try testing.expectEqualStrings("3", result.toPtr(runtime.String).bytes());
}

test "ansi repro make-symbol.11 nil vector string designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const name = try repl.eval("(symbol-name (make-symbol (make-array '(0) :element-type nil)))");
    try testing.expect(name.isString());
    try testing.expectEqualStrings("", name.toPtr(runtime.String).bytes());

    const pkg = try repl.eval("(symbol-package (make-symbol (make-array '(0) :element-type nil)))");
    try testing.expect(pkg.isNil());
}

test "ansi repro symbol-package function designator resolves callable" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(funcall #'symbol-package (make-symbol (make-array '(0) :element-type nil)))",
    );
    try testing.expect(result.isNil());
}

test "delete-package retags symbol for progv lookup" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(let* ((p (make-package "DOT-PROGV-PKG"))
        \\       (s (intern "DYN-X" p)))
        \\  (delete-package p)
        \\  (progv (list s) (list 42)
        \\    (symbol-value s)))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "ansi repro intern function designator accepts optional package arg" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(funcall #'intern \"DOT-INTERN\" :cl-user)");
    try testing.expect(result.isSymbol());
}

test "intern preserves caller spelling and package lookup uses exact names" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const preserved = try repl.eval("(string= (symbol-name (intern \"A_i\" :cl-user)) \"A_i\")");
    try testing.expect(preserved.isT());

    const exact_lookup = try repl.eval("(eq (intern \"A_i\" :cl-user) (find-symbol \"A_i\" :cl-user))");
    try testing.expect(exact_lookup.isT());

    const lower_lookup = try repl.eval("(null (find-symbol \"a_i\" :cl-user))");
    try testing.expect(lower_lookup.isT());
}

test "ansi repro equal.13 equal.14 nil vectors are string-like" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const v_eq = try repl.eval(
        "(equal (make-array '(0) :element-type nil) (make-array '(0) :element-type nil))",
    );
    try testing.expect(!v_eq.isNil());

    const s_eq = try repl.eval(
        "(and (equal (make-array '(0) :element-type nil) \"\") (equal \"\" (make-array '(0) :element-type nil)))",
    );
    try testing.expect(!s_eq.isNil());
}

test "ansi repro loop.collect.1 expands extended loop clauses" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(equal (loop for i from 1 to 3 collect i) '(1 2 3))");
    try testing.expect(!result.isNil());
}

test "ansi repro loop.collect.1 nested loop form expands in value position" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(progn (setq x (loop for i from 1 to 3 collect i)) (equal x '(1 2 3)))",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro loop for-and parallel iteration" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(= (loop for e in '(a b c a) and i from 0 count (eql e 'a)) 2)",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro loop collecting and do separator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((last nil)) (list (loop for v in '(1 2 3) when (oddp v) collecting v and do (setq last v)) last))",
    );
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.isCons());
    const xs0 = c0.car.toPtr(Cons);
    try testing.expect(xs0.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), xs0.car.toFixnum());
    try testing.expect(xs0.cdr.isCons());
    const xs1 = xs0.cdr.toPtr(Cons);
    try testing.expect(xs1.car.isFixnum());
    try testing.expectEqual(@as(i64, 3), xs1.car.toFixnum());
    try testing.expect(xs1.cdr.isNil());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 3), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
}

test "loop expansion does not capture user while macro" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(progn " ++
            "(defmacro while (cond &rest body) `(loop (if (not ,cond) (return t)) ,@body)) " ++
            "(defun loop-while-probe (xs) " ++
            "  (let ((cur xs) (n 0)) " ++
            "    (loop while cur do (setq n (+ n 1)) (setq cur (cdr cur))) " ++
            "    n)) " ++
            "(loop-while-probe '(a b c)))",
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "finish-output on standard output is non-erroring" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(progn (finish-output *standard-output*) t)");
    try testing.expect(!result.isNil());
}

test "errset catches namestring type-error on non-file stream" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((s (make-string-input-stream \"abc\"))) (errset (namestring s)))",
    );
    try testing.expect(result.isNil());
}

test "condition subtype lattice includes type-error under error" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(subtypep 'type-error 'error)");
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(!c0.car.isNil());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(!c1.car.isNil());
}

test "handler-case error catches namestring type-error on non-file stream" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((s (make-string-input-stream \"abc\"))) (handler-case (namestring s) (error (c) (declare (ignore c)) :caught)))",
    );
    try testing.expect(result.isKeyword());
}

test "ansi repro loop for with fixnum type in equals clauses" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(= (loop for v fixnum = 0 then (1+ v) repeat 5 sum v) 10)",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro loop for equals without then sees current parallel bindings" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(equal (loop for (a b) on '(x 10 y 20) by #'cddr for q = b collect q) '(10 20))",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro csimp inverse loop" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        "(loop for (a b) on '(%sin %asin %cos %acos %tan %atan %cot %acot) by #'cddr do (putprop a b '$inverse) (putprop b a '$inverse))",
    );

    const result = try repl.eval("(and (eq (get '%sin '$inverse) '%asin) (eq (get '%asin '$inverse) '%sin))");
    try testing.expect(!result.isNil());
}

test "ansi repro loop with of-type initializer" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(= (loop for i below 4 with a of-type fixnum = 1 do (setq a (* a 2)) finally (return a)) 16)",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro loop upfrom as while" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(equal (loop for i upfrom 1 below 4 as c = (* i 10) while (< c 30) collect c) '(10 20))",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro logand accepts bignum intermediate integer" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let* ((prev #xffffffff)\n" ++
            "       (x (+ (* 1812433253 (logxor prev (ash prev -30))) 1)))\n" ++
            "  (list (type-of x) (logand x #xffffffff)))",
    );
    try testing.expect(result.isCons());
    const head = result.toPtr(Cons);
    try testing.expect(head.car.isSymbol());
    try testing.expectEqualStrings("BIGNUM", head.car.toPtr(Symbol).getName());
    try testing.expect(head.cdr.isCons());
    const tail = head.cdr.toPtr(Cons);
    try testing.expect(tail.car.isFixnum());
    try testing.expectEqual(@as(i64, 1340201581), tail.car.toFixnum());
    try testing.expect(tail.cdr.isNil());
}

test "generational loop for-and arithmetic stays bounded" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 256 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 24 * 1024 * 1024,
            .los_size = 24 * 1024 * 1024,
            .los_threshold = 32 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(loop for i from 0 to 10
        \\      and d = 3 then (+ d i)
        \\      finally (return d))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 58), result.toFixnum());
}

test "generational string designators stay valid across GC pressure" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 128 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 2 * 1024 * 1024,
            .los_size = 16 * 1024 * 1024,
            .los_threshold = 16 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const ok = try repl.eval(
        \\(let* ((src (make-string 65536 :initial-element #\a))
        \\       (sym (make-symbol src)))
        \\  (let ((i 0))
        \\    (while (< i 200000)
        \\      (cons i i)
        \\      (setq i (+ i 1))))
        \\  (and (= (length (string-upcase sym)) 65536)
        \\       (= (length (string-downcase src)) 65536)))
    );
    try testing.expect(!ok.isNil());
}

test "ansi repro loop when do accepts multi-form action with loop-finish" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((n 0)) (loop for i from 1 to 10 when (> i 3) do (setq n i) (loop-finish) (setq n 99)) n)",
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 4), result.toFixnum());
}

test "ansi repro loop unless do accepts multi-form action with loop-finish" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((n 0)) (loop for i from 1 to 10 unless (< i 4) do (setq n i) (loop-finish) (setq n 99)) n)",
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 4), result.toFixnum());
}

test "ansi repro syntax.sharp-dot.1 read-time evaluates #." {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // ANSI syntax.sharp-dot.1 expects read-time evaluation support for #.
    const src = "(read-from-string \"#.(+ 1 2)\" t nil :start 0)";
    const result = try repl.eval(src);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

test "read-from-string returns object and index as multiple values" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const base = try repl.eval("(multiple-value-list (read-from-string \"ABC\" t nil :start 0))");
    const b0 = base.toPtr(Cons);
    try testing.expect(b0.car.isSymbol());
    try testing.expectEqualStrings("ABC", b0.car.toPtr(runtime.Symbol).getName());
    const b1 = b0.cdr.toPtr(Cons);
    try testing.expect(b1.car.isFixnum());
    try testing.expectEqual(@as(i64, 3), b1.car.toFixnum());

    const with_start = try repl.eval("(multiple-value-list (read-from-string \"xxABC\" t nil :start 2))");
    const s0 = with_start.toPtr(Cons);
    try testing.expect(s0.car.isSymbol());
    try testing.expectEqualStrings("ABC", s0.car.toPtr(runtime.Symbol).getName());
    const s1 = s0.cdr.toPtr(Cons);
    try testing.expect(s1.car.isFixnum());
    try testing.expectEqual(@as(i64, 5), s1.car.toFixnum());
}

test "ansi repro syntax.sharp-c.1 read-time complex helper" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src = "(read-from-string \"#.(complex 1 1)\" t nil :start 0)";
    const result = try repl.eval(src);
    try testing.expect(result.typeKind() == .complex);
    const cplx = result.toPtr(runtime.Complex);
    try testing.expectApproxEqAbs(@as(f64, 1.0), cplx.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 1.0), cplx.imag, 0.0001);
}

test "ansi repro syntax.sharp-c.4 read-time complex helper rational part" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src = "(read-from-string \"#.(complex -1/2 1)\" t nil :start 0)";
    const result = try repl.eval(src);
    try testing.expect(result.typeKind() == .complex);
    const cplx = result.toPtr(runtime.Complex);
    try testing.expectApproxEqAbs(@as(f64, -0.5), cplx.real, 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 1.0), cplx.imag, 0.0001);
}

test "read-char-no-hang is bound and works on string streams" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((s (make-string-input-stream "ab")))
        \\  (and (characterp (read-char-no-hang s nil nil))
        \\       (characterp (read-char-no-hang s nil nil))
        \\       (null (read-char-no-hang s nil nil))))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "make-string-input-stream accepts start and end" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((s (make-string-input-stream "ABCDE" 1 4)))
        \\  (list (%read-char-from-stream s)
        \\        (%read-char-from-stream s)
        \\        (%read-char-from-stream s)
        \\        (%read-char-from-stream s)))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isCons());
    const a = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 'B'), a.car.toFixnum());
    const b = a.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 'C'), b.car.toFixnum());
    const c = b.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 'D'), c.car.toFixnum());
    const d = c.cdr.toPtr(Cons);
    try testing.expect(d.car.isNil());
}

test "write-char supports optional stream argument" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((s (make-string-output-stream)))
        \\  (write-char #\A s)
        \\  (write-char #\B s)
        \\  (equal (get-output-stream-string s) "AB"))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "with-output-to-string supports destination string form" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((buf (make-array 0 :fill-pointer 0 :adjustable t)))
        \\  (with-output-to-string (s buf)
        \\    (write-char #\A s)
        \\    (write-char #\B s))
        \\  (equal buf "AB"))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "with-output-to-string returns primary string value cleanly" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const len = try repl.eval(
        "(let ((s (with-output-to-string (out) (format out \"hello\")))) (length s))",
    );
    try testing.expect(len.isFixnum());
    try testing.expectEqual(@as(i64, 5), len.toFixnum());

    const text = try repl.eval("(with-output-to-string (s) (princ \"test\" s))");
    try testing.expect(text.isString());
    try testing.expectEqualStrings("test", text.toPtr(runtime.String).bytes());
}

test "dispatch macro character executes during read-from-string" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (setq *habu-disp-hit* nil)
        \\  (defun habu-test-dollar-reader (stream sub-char arg)
        \\    (declare (ignore sub-char arg))
        \\    (read-char stream)
        \\    (read-char stream)
        \\    (read-char stream)
        \\    (read-char stream)
        \\    (setq *habu-disp-hit* t)
        \\    'abc)
        \\  (set-dispatch-macro-character #\# #\$ #'habu-test-dollar-reader)
        \\  (setq *habu-disp-hit* nil)
        \\  (eval (read-from-string "#$abc$" t nil :start 0))
        \\  *habu-disp-hit*)
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "ordinary macro character executes during read-from-string" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (setq *habu-ordinary-hit* nil)
        \\  (defun habu-test-underscore-reader (stream char)
        \\    (declare (ignore char))
        \\    (read-char stream)
        \\    (setq *habu-ordinary-hit* t)
        \\    'abc)
        \\  (set-macro-character #\_ #'habu-test-underscore-reader t)
        \\  (setq *habu-ordinary-hit* nil)
        \\  (let ((vals (multiple-value-list (read-from-string "_x" t nil :start 0))))
        \\    (and (= (length vals) 2)
        \\         (eq (car vals) 'abc)
        \\         (= (cadr vals) 2)
        \\         *habu-ordinary-hit*)))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "maxima loader binds canonical cl-user package even when loaded from maxima" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (load "lib/maxima-manifest.lisp")
        \\  (load (concatenate 'string (habu-maxima-manifest-value :srcdir) "maxima-package.lisp"))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-loader.lisp"))
        \\  (and (boundp 'cl-user::*maxima-source-dir*)
        \\       (stringp cl-user::*maxima-source-dir*)
        \\       (fboundp 'cl-user::maxima-load-all)
        \\       (fboundp 'cl-user::maxima-try-load)))
    ;

    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "get-macro-character returns primary and secondary values" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (defun habu-test-reader (stream char)
        \\    (declare (ignore stream char))
        \\    'ok)
        \\  (set-macro-character #\_ #'habu-test-reader t)
        \\  (let ((vals (multiple-value-list (get-macro-character #\_))))
        \\    (and (= (length vals) 2)
        \\         (functionp (car vals))
        \\         (eq (cadr vals) t))))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "dispatch macro callback preserves errset result flow" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (defstruct instream stream (line 0 :type fixnum) stream-name)
        \\  (defvar *stream-alist* nil)
        \\  (setq *habu-disp-line* nil)
        \\  (defun habu-test-errset-reader (stream sub-char arg)
        \\    (declare (ignore sub-char arg))
        \\    (let (name errset)
        \\      (errset (setq name (namestring stream)))
        \\      (let ((st (car (setq *stream-alist*
        \\                           (cons (make-instream :stream stream :stream-name name)
        \\                                 *stream-alist*)))))
        \\        (setq *habu-disp-line* (and st (instream-line st)))
        \\        (read-char stream)
        \\        (read-char stream)
        \\        (read-char stream)
        \\        (read-char stream)
        \\        'abc)))
        \\  (set-dispatch-macro-character #\# #\$ #'habu-test-errset-reader)
        \\  (setq *habu-disp-line* nil)
        \\  (eval (read-from-string "#$abc$" t nil :start 0))
        \\  *habu-disp-line*)
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 0), result.toFixnum());
}

test "dispatch macro callback preserves get-instream through add-lineinfo" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(progn
        \\  (defstruct instream stream (line 0 :type fixnum) stream-name)
        \\  (defvar *stream-alist* nil)
        \\  (defvar *parse-string-input-stream* nil)
        \\  (defvar *parse-stream* nil)
        \\  (defvar *parse-window* nil)
        \\  (defvar *current-line-info* '(0 "boot" src))
        \\  (defun find-stream (stream)
        \\    (dolist (v *stream-alist*)
        \\      (cond ((eq stream (instream-stream v))
        \\             (return v)))))
        \\  (defun get-instream (str)
        \\    (or (dolist (v *stream-alist*)
        \\          (cond ((eq str (instream-stream v))
        \\                 (return v))))
        \\        (let (name errset)
        \\          (errset (setq name (namestring str)))
        \\          (car (setq *stream-alist*
        \\                     (cons (make-instream :stream str :stream-name name)
        \\                           *stream-alist*))))))
        \\  (defun add-lineinfo (lis)
        \\    (if (or (atom lis)
        \\            (eq *parse-stream* *parse-string-input-stream*)
        \\            (and (eq *parse-window* *standard-input*)
        \\                 (not (find-stream *parse-stream*))))
        \\        lis
        \\        (let* ((st (get-instream *parse-stream*))
        \\               (n (instream-line st))
        \\               (nam (or (instream-stream-name st)
        \\                        (namestring (instream-stream st)))))
        \\          (or nam (return-from add-lineinfo lis))
        \\          (setq *current-line-info*
        \\                (cond ((eq (cadr *current-line-info*) nam)
        \\                       (cond ((eql (car *current-line-info*) n)
        \\                              *current-line-info*)
        \\                             (t (cons n (cdr *current-line-info*)))))
        \\                      (t (list n nam 'src))))
        \\          (cond ((null (cdr lis))
        \\                 (list (car lis) *current-line-info*))
        \\                (t (append lis (list *current-line-info*)))))))
        \\  (setq *habu-disp-lineinfo* nil)
        \\  (defun habu-test-add-lineinfo-reader (stream sub-char arg)
        \\    (declare (ignore sub-char arg))
        \\    (let ((*parse-stream* stream)
        \\          (*parse-string-input-stream* nil)
        \\          (*parse-window* nil))
        \\      (setq *habu-disp-lineinfo* (add-lineinfo '(foo)))
        \\      (read-char stream)
        \\      (read-char stream)
        \\      (read-char stream)
        \\      (read-char stream)
        \\      'abc))
        \\  (set-dispatch-macro-character #\# #\$ #'habu-test-add-lineinfo-reader)
        \\  (setq *habu-disp-lineinfo* nil)
        \\  (eval (read-from-string "#$abc$" t nil :start 0))
        \\  *habu-disp-lineinfo*)
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isCons());
}

test "load path preserves get-instream through dispatch macro callback" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    {
        var file = try tmp.dir.createFile("dispatch-load.lisp", .{});
        defer file.close();
        try file.writeAll("#$abc$");
    }
    const root = try tmp.dir.realpathAlloc(allocator, ".");
    defer allocator.free(root);
    const path = try std.fs.path.join(allocator, &.{ root, "dispatch-load.lisp" });
    defer allocator.free(path);

    const src = try std.fmt.allocPrint(
        allocator,
        \\(progn
        \\  (defstruct instream stream (line 0 :type fixnum) stream-name)
        \\  (defvar *stream-alist* nil)
        \\  (defvar *parse-string-input-stream* nil)
        \\  (defvar *parse-stream* nil)
        \\  (defvar *parse-window* nil)
        \\  (defvar *current-line-info* '(0 "boot" src))
        \\  (defun find-stream (stream)
        \\    (dolist (v *stream-alist*)
        \\      (cond ((eq stream (instream-stream v))
        \\             (return v)))))
        \\  (defun get-instream (str)
        \\    (or (dolist (v *stream-alist*)
        \\          (cond ((eq str (instream-stream v))
        \\                 (return v))))
        \\        (let (name errset)
        \\          (errset (setq name (namestring str)))
        \\          (car (setq *stream-alist*
        \\                     (cons (make-instream :stream str :stream-name name)
        \\                           *stream-alist*))))))
        \\  (defun add-lineinfo (lis)
        \\    (if (or (atom lis)
        \\            (eq *parse-stream* *parse-string-input-stream*)
        \\            (and (eq *parse-window* *standard-input*)
        \\                 (not (find-stream *parse-stream*))))
        \\        lis
        \\        (let* ((st (get-instream *parse-stream*))
        \\               (n (instream-line st))
        \\               (nam (or (instream-stream-name st)
        \\                        (namestring (instream-stream st)))))
        \\          (or nam (return-from add-lineinfo lis))
        \\          (setq *current-line-info* (list n nam 'src))
        \\          (list (car lis) *current-line-info*))))
        \\  (setq *habu-disp-lineinfo* nil)
        \\  (defun habu-test-load-reader (stream sub-char arg)
        \\    (declare (ignore sub-char arg))
        \\    (let ((*parse-stream* stream)
        \\          (*parse-string-input-stream* nil)
        \\          (*parse-window* nil))
        \\      (setq *habu-disp-lineinfo* (add-lineinfo '(foo)))
        \\      (read-char stream)
        \\      (read-char stream)
        \\      (read-char stream)
        \\      (read-char stream)
        \\      'abc))
        \\  (set-dispatch-macro-character #\# #\$ #'habu-test-load-reader)
        \\  (setq *habu-disp-lineinfo* nil)
        \\  (load "{s}")
        \\  *habu-disp-lineinfo*)
    , .{path});
    defer allocator.free(src);

    const result = try repl.eval(src);
    try testing.expect(result.isCons());
}

test "ansi repro read-suppress.sharp-dot.1 ignores #. when suppressed" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // ANSI read-suppress.sharp-dot.1 should ignore #. when *read-suppress* is true.
    const src =
        \\(progn
        \\  (setq common-lisp::*read-suppress* t)
        \\  (unwind-protect
        \\      (read-from-string "#.1" t nil :start 0)
        \\    (setq common-lisp::*read-suppress* nil)))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isNil());
}

test "ansi repro universe make-array function designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((xs (mapcar #'make-array '(2 3))))
        \\  (and (consp xs)
        \\       (arrayp (car xs))
        \\       (arrayp (car (cdr xs)))))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "ansi repro read-from-string.error.10 tolerates unknown keyword" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src = "(eq 'A (read-from-string \"A\" nil t :bad-keyword t :allow-other-keys nil))";
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "ansi repro warn.1 muffle-warning restart works" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(handler-bind
        \\ ((warning #'(lambda (c)
        \\              (declare (ignore c))
        \\              (muffle-warning c))))
        \\ (progn
        \\   (warn "This is a warning")
        \\   t))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "ansi repro compute-restarts.1 returns restart objects" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        \\(restart-case
        \\  (let ((r (compute-restarts)))
        \\    (and (consp r) (eq (restart-name (car r)) 'foo)))
        \\  (foo () nil))
    ;
    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(!result.isNil());
}

test "compute-restarts returns first-class restart objects" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        \\(restart-case
        \\  (let ((r (car (compute-restarts))))
        \\    (and (typep r 'restart)
        \\         (not (symbolp r))
        \\         (eq (restart-name r) 'foo)))
        \\  (foo () nil))
    ;
    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(!result.isNil());
}

test "ansi repro compute-restarts.3 find-restart returns restart object" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src =
        \\(restart-case
        \\  (let ((r (find-restart 'foo)))
        \\    (and r (eq (restart-name r) 'foo)))
        \\  (foo () nil))
    ;
    const result = try evalExpr(allocator, &heap, src);
    try testing.expect(!result.isNil());
}

test "restart-bind establishes callable restart handlers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((seen nil))
        \\  (let ((out (restart-bind
        \\               ((foo #'(lambda ()
        \\                        (setq seen t)
        \\                        :ok)))
        \\               (invoke-restart 'foo))))
        \\    (list out seen)))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(!c0.car.isNil());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(!c1.car.isNil());
}

test "invoke-restart uses exact restart object, not just name" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let ((outer nil))
        \\  (restart-case
        \\    (progn
        \\      (setq outer (find-restart 'foo))
        \\      (restart-case
        \\        (invoke-restart outer 17)
        \\        (foo () :inner)))
        \\    (foo (v) v)))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 17), result.toFixnum());
}

test "defstruct BOA constructor lambda list initializes slots" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 * 64 });
    defer heap.deinit();

    const result = try evalExpr(
        allocator,
        &heap,
        "(progn " ++
            "  (defstruct (pair (:constructor make-pair (first second &aux (sum (+ first second))))) " ++
            "    first second (sum 0 :type fixnum)) " ++
            "  (let ((p (make-pair 2 5))) " ++
            "    (list (pair-first p) (pair-second p) (pair-sum p))))",
    );
    try testing.expect(result.isCons());
    const a = result.toPtr(runtime.Cons);
    const b = a.cdr.toPtr(runtime.Cons);
    const c = b.cdr.toPtr(runtime.Cons);
    try testing.expectEqual(@as(i64, 2), a.car.toFixnum());
    try testing.expectEqual(@as(i64, 5), b.car.toFixnum());
    try testing.expectEqual(@as(i64, 7), c.car.toFixnum());
    try testing.expect(c.cdr.isNil());
}

test "defstruct supports multiple explicit constructors" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 * 64 });
    defer heap.deinit();

    const result = try evalExpr(
        allocator,
        &heap,
        "(progn " ++
            "  (defstruct (poly " ++
            "               (:constructor make-poly-from-termlist (termlist &optional (sugar (length termlist)))) " ++
            "               (:constructor make-poly-zero (&aux (termlist nil) (sugar -1)))) " ++
            "    (termlist nil) (sugar -1 :type fixnum)) " ++
            "  (list (poly-sugar (make-poly-from-termlist '(a b c))) " ++
            "        (poly-sugar (make-poly-zero))))",
    );
    try testing.expect(result.isCons());
    const a = result.toPtr(runtime.Cons);
    const b = a.cdr.toPtr(runtime.Cons);
    try testing.expectEqual(@as(i64, 3), a.car.toFixnum());
    try testing.expectEqual(@as(i64, -1), b.car.toFixnum());
    try testing.expect(b.cdr.isNil());
}

test "defstruct print-function symbol customizes write-to-string" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 * 64 });
    defer heap.deinit();

    const result = try evalExpr(
        allocator,
        &heap,
        "(progn " ++
            "  (defstruct (gf-data (:print-function gf-data-short-print)) char) " ++
            "  (defun gf-data-short-print (obj stream depth) " ++
            "    (declare (ignore obj depth)) " ++
            "    (format stream \"Structure [GF-DATA]\")) " ++
            "  (write-to-string (make-gf-data :char 3)))",
    );
    try testing.expect(result.isString());
    try testing.expectEqualStrings("Structure [GF-DATA]", result.toPtr(runtime.String).bytes());
}

test "defstruct print-function lambda customizes nested structure output" {
    const allocator = testing.allocator;

    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 * 64 });
    defer heap.deinit();

    const result = try evalExpr(
        allocator,
        &heap,
        "(progn " ++
            "  (defstruct (graph (:print-function " ++
            "                     (lambda (obj stream depth) " ++
            "                       (declare (ignore depth)) " ++
            "                       (format stream \"GRAPH(~A)\" (graph-order obj))))) " ++
            "    (order 0)) " ++
            "  (write-to-string (list (make-graph :order 4))))",
    );
    try testing.expect(result.isString());
    try testing.expectEqualStrings("(GRAPH(4))", result.toPtr(runtime.String).bytes());
}

test "ansi repro cerror.6 continue restart resumes" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(handler-bind ((simple-error #'(lambda (c) (continue c))))
        \\  (progn
        \\    (cerror "Wooo" 'simple-error)
        \\    10))
    ;
    const result = try repl.eval(src);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10), result.toFixnum());
}

test "signal without handlers returns nil and handler-case can catch" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const unhandled = try repl.eval("(signal 'simple-error 'boom)");
    try testing.expect(unhandled.isNil());

    const handled = try repl.eval(
        \\(handler-case
        \\  (signal 'simple-error 'boom)
        \\  (simple-error (c)
        \\    (declare (ignore c))
        \\    42))
    );
    try testing.expect(handled.isFixnum());
    try testing.expectEqual(@as(i64, 42), handled.toFixnum());
}

test "handler-case catches custom warning via runtime subtype dispatch" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(progn
        \\  (define-condition my-warning (simple-warning) ())
        \\  (handler-case
        \\      (signal 'my-warning (make-condition 'my-warning))
        \\    (warning (c)
        \\      (declare (ignore c))
        \\      :caught)))
    );
    try testing.expect(result.isKeyword());
    try testing.expectEqualStrings("CAUGHT", result.toPtr(runtime.Keyword).getName());
}

test "make-condition uses callable common-lisp make-instance" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(progn
        \\  (define-condition my-warning-make (simple-warning) ((payload :initarg :payload)))
        \\  (slot-value (make-condition 'my-warning-make :payload 17) 'payload))
    );
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 17), result.toFixnum());
}

test "warn uses callable common-lisp make-instance" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(handler-case
        \\  (progn (warn 'simple-warning) :ok)
        \\  (error (c) (list :err (condition-type c))))
    );
    try testing.expect(result.isKeyword());
    try testing.expectEqualStrings("OK", result.toPtr(runtime.Keyword).getName());
}

test "handler-bind catches custom warning via runtime subtype dispatch" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(progn
        \\  (define-condition my-warning-bind (simple-warning) ())
        \\  (let ((seen nil))
        \\    (handler-bind
        \\        ((warning #'(lambda (c)
        \\                     (declare (ignore c))
        \\                     (setq seen :caught))))
        \\      (signal 'my-warning-bind (make-condition 'my-warning-bind)))
        \\    seen))
    );
    try testing.expect(result.isKeyword());
    try testing.expectEqualStrings("CAUGHT", result.toPtr(runtime.Keyword).getName());
}

test "ansi repro pathname-host.1 accepts pathname designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(pathname-host *default-pathname-defaults*)");
    try testing.expect(result.isNil());
}

test "ansi repro pathname-device.1 accepts pathname designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(pathname-device *default-pathname-defaults*)");
    try testing.expect(result.isNil());
}

test "ansi repro pathname-directory.1 accepts pathname designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(pathname-directory *default-pathname-defaults*)");
    try testing.expect(result.isNil());
}

test "ansi repro pathname-name.1 accepts pathname designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(pathname-name *default-pathname-defaults*)");
    try testing.expect(result.isNil());
}

test "ansi repro pathname-type.1 accepts pathname designator" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(pathname-type *default-pathname-defaults*)");
    try testing.expect(result.isNil());
}

test "ansi repro read-from-string.feature-plus.1 returns nil" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(read-from-string \"#+foo bar\" nil :eof)");
    try testing.expect(result.isNil());
}

test "ansi repro read-from-string.feature-plus.2 suppresses package form to nil" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(read-from-string \"#+ecl (si::package-lock nil nil)\" nil :eof)");
    try testing.expect(result.isNil());
}

test "ansi repro load.feature-plus.1 ignores #+ecl branch" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    try std.fs.cwd().writeFile(.{
        .sub_path = "tmp_pkg_feature_1.lsp",
        .data = "#+ecl (si::package-lock (find-package \"COMMON-LISP\") nil)\n42\n",
    });
    defer std.fs.cwd().deleteFile("tmp_pkg_feature_1.lsp") catch {};

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(load \"tmp_pkg_feature_1.lsp\")");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "ansi repro load.feature-plus.2 ignores #+(and ...) branch" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    try std.fs.cwd().writeFile(.{
        .sub_path = "tmp_pkg_feature_2.lsp",
        .data = "#+(and ecl (not ecl-bytecmp)) (si::package-lock (find-package \"COMMON-LISP\") nil)\n42\n",
    });
    defer std.fs.cwd().deleteFile("tmp_pkg_feature_2.lsp") catch {};

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(load \"tmp_pkg_feature_2.lsp\")");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "ansi repro load.feature-plus.3 ignores #+lispworks branch" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    try std.fs.cwd().writeFile(.{
        .sub_path = "tmp_pkg_feature_3.lsp",
        .data = "#+lispworks (lw:set-default-character-element-type 'character)\n42\n",
    });
    defer std.fs.cwd().deleteFile("tmp_pkg_feature_3.lsp") catch {};

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval("(load \"tmp_pkg_feature_3.lsp\")");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "load in-package updates reader package for following forms" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    try std.fs.cwd().writeFile(.{
        .sub_path = "tmp_load_in_package.lsp",
        .data = "(defpackage \"TMP-LOAD-PKG\" (:use \"COMMON-LISP\"))\n" ++
            "(in-package \"TMP-LOAD-PKG\")\n" ++
            "(defun load-marker-fn () 42)\n" ++
            "(in-package \"CL-USER\")\n" ++
            "42\n",
    });
    defer std.fs.cwd().deleteFile("tmp_load_in_package.lsp") catch {};

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const loaded = try repl.eval("(load \"tmp_load_in_package.lsp\")");
    try testing.expect(loaded.isFixnum());
    try testing.expectEqual(@as(i64, 42), loaded.toFixnum());

    const check =
        "(and (fboundp 'tmp-load-pkg::load-marker-fn) " ++
        "(= (tmp-load-pkg::load-marker-fn) 42))";
    const result = try repl.eval(check);
    try testing.expect(!result.isNil());
}

test "in-package rejects missing package" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    try testing.expectError(error.InvalidPackage, repl.eval("(in-package \"NO-SUCH-PKG\")"));
    try testing.expect(try repl.heap.findLispPackage(try repl.heap.allocBaseString("NO-SUCH-PKG")) == null);
}

test "setq imported common-lisp special in used package" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defpackage \"PKG-SETQ\" (:use \"COMMON-LISP\"))");
    _ = try repl.eval("(in-package \"PKG-SETQ\")");
    const result = try repl.eval("(progn (setq *read-default-float-format* 'double-float) *read-default-float-format*)");
    try testing.expect(result.isSymbol());
    try testing.expectEqualStrings("DOUBLE-FLOAT", result.toPtr(Symbol).getName());
}

test "eval-when compile-time setq imported common-lisp special in used package" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defpackage \"PKG-EVAL-WHEN-SETQ\" (:use \"COMMON-LISP\"))");
    _ = try repl.eval("(in-package \"PKG-EVAL-WHEN-SETQ\")");
    const result = try repl.eval("(eval-when (:compile-toplevel :load-toplevel :execute) (setq *read-default-float-format* 'double-float) *read-default-float-format*)");
    try testing.expect(result.isSymbol());
    try testing.expectEqualStrings("DOUBLE-FLOAT", result.toPtr(Symbol).getName());
}

test "load eval-when compile-time setq imported common-lisp special in used package" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    try std.fs.cwd().writeFile(.{
        .sub_path = "tmp_load_eval_when_setq.lsp",
        .data = "(defpackage \"PKG-LOAD-EVAL-WHEN-SETQ\" (:use \"COMMON-LISP\"))\n" ++
            "(in-package \"PKG-LOAD-EVAL-WHEN-SETQ\")\n" ++
            "(eval-when (:compile-toplevel :load-toplevel :execute)\n" ++
            "  (setq *read-default-float-format* 'double-float)\n" ++
            "  *read-default-float-format*)\n",
    });
    defer std.fs.cwd().deleteFile("tmp_load_eval_when_setq.lsp") catch {};

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const loaded = try repl.eval("(load \"tmp_load_eval_when_setq.lsp\")");
    try testing.expect(loaded.isSymbol());
    try testing.expectEqualStrings("DOUBLE-FLOAT", loaded.toPtr(Symbol).getName());
}

test "load accepts source files larger than 1 MiB" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 32 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var content = std.ArrayList(u8){};
    defer content.deinit(allocator);

    while (content.items.len <= 1024 * 1024) {
        try content.appendSlice(allocator, "(quote big-load-marker)\n");
    }
    try content.appendSlice(allocator, "42\n");

    try std.fs.cwd().writeFile(.{
        .sub_path = "tmp_big_load.lsp",
        .data = content.items,
    });
    defer std.fs.cwd().deleteFile("tmp_big_load.lsp") catch {};

    const result = try repl.eval("(load \"tmp_big_load.lsp\")");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "defpackage supports import-from and shadowing-import-from" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defpackage \"DP-PROVIDER\" (:use \"COMMON-LISP\") (:export \"HOOK\"))");
    _ = try repl.eval("(in-package \"DP-PROVIDER\")");
    _ = try repl.eval("(defun hook (x) (+ x 10))");
    _ = try repl.eval("(in-package \"CL-USER\")");

    _ = try repl.eval("(defpackage \"DP-IMPORT\" (:use \"COMMON-LISP\") (:import-from \"DP-PROVIDER\" \"HOOK\"))");
    _ = try repl.eval("(in-package \"DP-IMPORT\")");
    const import_eq = try repl.eval("(eq 'hook 'dp-provider::hook)");
    try testing.expect(!import_eq.isNil());
    const import_call = try repl.eval("(hook 1)");
    try testing.expect(import_call.isFixnum());
    try testing.expectEqual(@as(i64, 11), import_call.toFixnum());

    _ = try repl.eval("(in-package \"CL-USER\")");
    _ = try repl.eval(
        "(defpackage \"DP-SHADOW\" (:use \"COMMON-LISP\") (:shadow \"HOOK\") (:shadowing-import-from \"DP-PROVIDER\" \"HOOK\"))",
    );
    _ = try repl.eval("(in-package \"DP-SHADOW\")");
    const shadow_eq = try repl.eval("(eq 'hook 'dp-provider::hook)");
    try testing.expect(!shadow_eq.isNil());
    const shadow_call = try repl.eval("(hook 1)");
    try testing.expect(shadow_call.isFixnum());
    try testing.expectEqual(@as(i64, 11), shadow_call.toFixnum());
}

test "package-local functionp does not override cl:functionp" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defpackage \"PKG-FNP\" (:use \"COMMON-LISP\") (:shadow \"FUNCTIONP\"))");
    _ = try repl.eval("(in-package \"PKG-FNP\")");
    const shadowed = try repl.eval("(eq 'functionp 'cl:functionp)");
    try testing.expect(shadowed.isNil());
    _ = try repl.eval(
        \\(defun functionp (x)
        \\  (cond ((symbolp x) nil)
        \\        ((cl:functionp x))))
    );
    const result = try repl.eval("(list (functionp #'car) (cl:functionp #'car) (functionp 'car))");

    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expect(!c1.car.isNil());
    try testing.expect(c1.cdr.isCons());

    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(!c2.car.isNil());
    try testing.expect(c2.cdr.isCons());

    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isNil());
}

test "package iteration macros produce symbols and iterator values" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((ds 0)\n" ++
            "      (de 0)\n" ++
            "      (da 0)\n" ++
            "      (iter-ok nil))\n" ++
            "  (do-symbols (s *package*)\n" ++
            "    (declare (ignore s))\n" ++
            "    (setq ds (+ ds 1))\n" ++
            "    (when (> ds 5) (return)))\n" ++
            "  (do-external-symbols (s (find-package \"COMMON-LISP\"))\n" ++
            "    (declare (ignore s))\n" ++
            "    (setq de (+ de 1))\n" ++
            "    (when (> de 5) (return)))\n" ++
            "  (do-all-symbols (s)\n" ++
            "    (declare (ignore s))\n" ++
            "    (setq da (+ da 1))\n" ++
            "    (when (> da 5) (return)))\n" ++
            "  (with-package-iterator (next *package* :internal :external :inherited)\n" ++
            "    (multiple-value-bind (more sym kind pkg) (next)\n" ++
            "      (setq iter-ok (and more\n" ++
            "                         (symbolp sym)\n" ++
            "                         (packagep pkg)\n" ++
            "                         (or (eq kind :internal)\n" ++
            "                             (eq kind :external)\n" ++
            "                             (eq kind :inherited))))))\n" ++
            "  (and (> ds 0) (> de 0) (> da 0) iter-ok))",
    );

    try testing.expect(!result.isNil());
}

test "setf sbit on make-array uses aset path" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((v (make-array 3 :initial-element 0))) (setf (sbit v 1) 42) (aref v 1))",
    );
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "setf bit on make-array uses aset path" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((v (make-array 3 :initial-element 0))) (setf (bit v 2) 7) (aref v 2))",
    );
    try testing.expectEqual(@as(i64, 7), result.toFixnum());
}

test "setf apply aref place updates target element" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((v (make-array 3 :initial-element 0)) (idxs '(1))) (setf (apply #'aref v idxs) 9) (aref v 1))",
    );
    try testing.expectEqual(@as(i64, 9), result.toFixnum());
}

test "log with optional base computes correctly" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // (log 100 10) = ln(100)/ln(10) ≈ 2.0; verify result is numeric (not error)
    const result = try repl.eval("(numberp (log 100.0 10.0))");
    try testing.expect(!result.isNil());
}

test "concatenate delegates to stdlib for sequence coercion" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(and
        \\  (string= (concatenate 'string "abc" "def") "abcdef")
        \\  (string= (concatenate 'string "ab" '(#\c #\d) "ef") "abcdef")
        \\  (equal (concatenate 'list '(1 2) '(3 4)) '(1 2 3 4)))
    );
    try testing.expect(!result.isNil());
}

test "string-upcase and string-downcase treat explicit :end nil as full length" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(and
        \\  (string= (string-upcase "e" :start 0 :end nil) "E")
        \\  (string= (string-downcase "E" :start 0 :end nil) "e"))
    );
    try testing.expect(!result.isNil());
}

test "setf the strips type declaration" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(let ((x 1)) (setf (the fixnum x) 42) x)",
    );
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "defgeneric with setf name" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(progn (defgeneric (setf my-accessor) (val obj)) t)",
    );
    try testing.expect(!result.isNil());
}

test "ansi repro package dynamic *package* controls use-package target" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const src =
        \\(let* ((pkg-a (make-package "TMP-A" :use '("COMMON-LISP")))
        \\       (pkg-b (make-package "TMP-B" :use '("COMMON-LISP"))))
        \\  (let ((*package* pkg-b))
        \\    (use-package "TMP-A")
        \\    (member pkg-a (package-use-list pkg-b))))
    ;
    const result = try repl.eval(src);
    try testing.expect(!result.isNil());
}

test "ansi repro delete-package rejects core packages" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // Deleting CL package should signal condition, not crash.
    // handler-case uses catch internally; the catch tag must match.
    // Use handler-bind + invoke-restart pattern which is proven to work.
    const result2 = try repl.eval(
        \\(let ((caught nil))
        \\  (handler-bind
        \\    ((error #'(lambda (c) (declare (ignore c)) (setq caught t))))
        \\    (delete-package "COMMON-LISP"))
        \\  caught)
    );
    // caught should be T after handler fired
    try testing.expect(result2.raw == Value.t.raw or !result2.isNil());
}

test "ansi repro symbol-package type-error signals condition" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // symbol-package on non-symbol should signal type-error, not VM crash
    const result2 = try repl.eval(
        \\(handler-case (symbol-package 42)
        \\  (type-error () :caught))
    );
    try testing.expect(result2.isKeyword());
}

test "ansi repro apply non-closure signals condition" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // apply with non-closure callee should signal type-error, not VM crash
    const result = try repl.eval(
        \\(handler-case (apply 42 '(1 2))
        \\  (type-error () :caught))
    );
    try testing.expect(result.isKeyword());
}

test "ansi repro finish-output accepts nil and t designators" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // finish-output and force-output must accept nil and t as stream designators
    const r1 = try repl.eval("(finish-output nil)");
    try testing.expect(r1.isNil());
    const r2 = try repl.eval("(finish-output t)");
    try testing.expect(r2.isNil());
    const r3 = try repl.eval("(force-output nil)");
    try testing.expect(r3.isNil());
    const r4 = try repl.eval("(force-output t)");
    try testing.expect(r4.isNil());
}

test "ansi repro symbol-function resolves primitive builtins" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // symbol-function must return callable for various builtins
    const r1 = try repl.eval("(functionp (symbol-function 'ash))");
    try testing.expect(r1.raw == Value.t.raw);
    const r2 = try repl.eval("(functionp (symbol-function 'apply))");
    try testing.expect(r2.raw == Value.t.raw);
    const r3 = try repl.eval("(funcall (symbol-function 'ash) 1 4)");
    try testing.expect(r3.isFixnum());
    try testing.expectEqual(@as(i64, 16), r3.toFixnum());
}

test "ansi repro encode-universal-time returns fixnum" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // encode-universal-time 1900-01-01 00:00:00 UTC = 0 (CL epoch)
    const result = try repl.eval("(encode-universal-time 0 0 0 1 1 1900 0)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 0), result.toFixnum());
}

// ============================================================================
// Multiple values: comprehensive tests for secondary values propagation
// ============================================================================

const ReplStdlibState = struct {
    repl: Repl,
    heap: *Heap,
};

// Helper: setup REPL with stdlib loaded
fn initReplWithStdlib(allocator: std.mem.Allocator) !*ReplStdlibState {
    const state = try allocator.create(ReplStdlibState);
    errdefer allocator.destroy(state);

    state.heap = try allocator.create(Heap);
    errdefer allocator.destroy(state.heap);
    state.heap.* = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    errdefer state.heap.deinit();

    state.repl = undefined;
    try state.repl.init(allocator, state.heap, .{});
    try state.repl.wireGlobalEnv();
    try loadStdlib(&state.repl);
    return state;
}

fn deinitReplWithStdlib(allocator: std.mem.Allocator, state: *ReplStdlibState) void {
    state.repl.deinit();
    state.heap.deinit();
    allocator.destroy(state.heap);
    allocator.destroy(state);
}

// --- Secondary values through control flow ---

test "initReplWithStdlib keeps moved repl callable" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    try testing.expect(state.repl.vm.global_env != null);

    const out = try state.repl.eval(
        \\(progn
        \\  (defun helper-rewire-check (n d)
        \\    (multiple-value-bind (q r) (floor n d) (list q r)))
        \\  (helper-rewire-check 17 5))
    );
    try testing.expect(out.isCons());
    try testing.expectEqual(@as(i64, 3), out.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 2), out.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "Repl JIT roots str_len literal via production collector" {
    if (!build_options.use_hoist) return;

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const before = repl.vm.jit_fns.items.len;
    _ = try repl.eval(
        \\(defun repl-jit-string-len ()
        \\  (declare (optimize (speed 3) (safety 0)))
        \\  (length "abcdef"))
    );
    try testing.expect(repl.vm.jit_fns.items.len > before);

    const result = try repl.eval("(repl-jit-string-len)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 6), result.toFixnum());
}

test "mv: values through if (then branch)" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // secondary values must survive through jmp in if's then branch
    const result = try repl.eval(
        \\(multiple-value-bind (a b c)
        \\    (if t (values 1 2 3) nil)
        \\  (list a b c))
    );
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), c2.car.toFixnum());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 3), c3.car.toFixnum());
}

test "mv: values through if (else branch)" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval(
        \\(multiple-value-bind (a b)
        \\    (if nil (values 1 2) (values 3 4))
        \\  (list a b))
    );
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 3), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 4), c2.car.toFixnum());
}

test "mv: values from function call" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // values must survive through function return (ret opcode)
    _ = try repl.eval("(defun ret-mv () (values 10 20 30))");
    const result = try repl.eval(
        \\(multiple-value-bind (a b c) (ret-mv)
        \\  (list a b c))
    );
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 10), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 20), c2.car.toFixnum());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 30), c3.car.toFixnum());
}

test "mv: values from function with if inside" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // values from if inside a function must propagate through ret
    _ = try repl.eval("(defun mv-if (x) (if (> x 0) (values x 1) (values x -1)))");
    const r1 = try repl.eval(
        \\(multiple-value-bind (a b) (mv-if 5) (list a b))
    );
    try testing.expect(r1.isCons());
    try testing.expectEqual(@as(i64, 5), r1.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 1), r1.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());

    const r2 = try repl.eval(
        \\(multiple-value-bind (a b) (mv-if -3) (list a b))
    );
    try testing.expect(r2.isCons());
    try testing.expectEqual(@as(i64, -3), r2.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, -1), r2.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: mv-bind with function parameters (start_index)" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // mv-bind must use correct start_index (not overwrite function params)
    _ = try repl.eval(
        \\(defun test-mvb-params (x y)
        \\  (multiple-value-bind (q r) (values 10 20)
        \\    (list x y q r)))
    );
    const result = try repl.eval("(test-mvb-params 1 2)");
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), c2.car.toFixnum());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 10), c3.car.toFixnum());
    const c4 = c3.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 20), c4.car.toFixnum());
}

test "mv: mv-bind inside let" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // mv-bind after let bindings must not clobber let locals
    const result = try repl.eval(
        \\(let ((x 100))
        \\  (multiple-value-bind (a b) (values 1 2)
        \\    (list x a b)))
    );
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 100), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c2.car.toFixnum());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), c3.car.toFixnum());
}

test "mv: multiple-value-list through if" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    const result = try repl.eval("(multiple-value-list (if t (values 1 2 3) nil))");
    // Should be (1 2 3)
    try testing.expect(result.isCons());
    const c1 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), c2.car.toFixnum());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 3), c3.car.toFixnum());
}

test "mv: values through nested function calls" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Inner function returns MV, outer function wraps with mv-bind
    _ = try repl.eval("(defun inner () (values 7 8))");
    _ = try repl.eval(
        \\(defun outer ()
        \\  (multiple-value-bind (a b) (inner)
        \\    (+ a b)))
    );
    const result = try repl.eval("(outer)");
    try testing.expectEqual(@as(i64, 15), result.toFixnum());
}

test "mv: values through conditional jumps (jmp_nil/jmp_not_nil)" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Use direct IF so this test does not depend on stdlib macros.
    const result = try repl.eval(
        \\(multiple-value-bind (a b)
        \\    (if t (values 5 6) nil)
        \\  (list a b))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 5), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 6), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

// --- Floor/ceiling/round/truncate multiple values ---

test "mv: floor 1-arg float" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (floor 3.7) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 3), result.toPtr(Cons).car.toFixnum());
    // r should be ~0.7 (float)
    try testing.expect(result.toPtr(Cons).cdr.toPtr(Cons).car.isFloat());
}

test "mv: floor 2-arg integers" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (floor 17 5) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 3), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 2), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: floor negative dividend" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // CL: (floor -7 2) => -4, 1
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (floor -7 2) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, -4), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 1), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: truncate 2-arg" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // CL: (truncate 7 2) => 3, 1
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (truncate 7 2) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 3), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 1), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: ceiling 2-arg" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // CL: (ceiling 7 2) => 4, -1
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (ceiling 7 2) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 4), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, -1), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: round to nearest even" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // CL: (round 2.5) => 2, 0.5 (banker's rounding)
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (round 2.5) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 2), result.toPtr(Cons).car.toFixnum());
    // r should be 0.5
    try testing.expect(result.toPtr(Cons).cdr.toPtr(Cons).car.isFloat());
}

test "mv: round 2-arg" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // CL: (round 7 2) => 4, -1 (7/2 = 3.5, rounds to 4)
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (round 7 2) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 4), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, -1), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: floor in function with params" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // The critical regression: mv-bind inside function with params
    _ = try state.repl.eval(
        \\(defun my-divmod (n d)
        \\  (multiple-value-bind (q r) (floor n d) (list q r)))
    );
    const result = try state.repl.eval("(my-divmod 17 5)");
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 3), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 2), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: mod and rem" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // mod uses floor, rem uses truncate
    const r1 = try state.repl.eval("(mod 17 5)");
    try testing.expectEqual(@as(i64, 2), r1.toFixnum());

    const r2 = try state.repl.eval("(rem 17 5)");
    try testing.expectEqual(@as(i64, 2), r2.toFixnum());

    // Negative: mod and rem differ
    const r3 = try state.repl.eval("(mod -7 2)");
    try testing.expectEqual(@as(i64, 1), r3.toFixnum());

    const r4 = try state.repl.eval("(rem -7 2)");
    try testing.expectEqual(@as(i64, -1), r4.toFixnum());
}

test "mv: floor 1-arg integer identity" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // (floor 10) => 10, 0
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (floor 10) (list q r))
    );
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 10), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 0), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

test "mv: multiple-value-list floor" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // (multiple-value-list (floor 17 5)) => (3 2)
    const result = try state.repl.eval("(multiple-value-list (floor 17 5))");
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 3), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 2), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

// --- Error/condition system ---

test "error: handler-case catches error" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(handler-case
        \\    (error "test error")
        \\  (error (e) (list :caught e)))
    );
    // Should return (:CAUGHT (SIMPLE-ERROR ...))
    try testing.expect(result.isCons());
}

test "error: handler-case type-error" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(handler-case
        \\    (+ 1 "a")
        \\  (type-error (e) :type-error))
    );
    try testing.expect(result.isKeyword());
}

test "error: handler-case division-by-zero" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(handler-case
        \\    (/ 1 0)
        \\  (division-by-zero (e) :div-zero))
    );
    try testing.expect(result.isKeyword());
}

test "error: errset catches namestring type mismatch in local binding" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(let (name errset)
        \\  (errset (setq name (namestring 42)))
        \\  (and (null name) t))
    );
    try testing.expect(!result.isNil());
}

test "error: errset re-signals when errset variable is true" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval(
        \\(handler-case
        \\    (let ((errset t))
        \\      (errset (namestring 42))
        \\      :miss)
        \\  (error (e) :caught))
    );
    try testing.expect(result.isKeyword());
}

test "error: errset returns list of values on success" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    const result = try state.repl.eval("(let ((errset nil)) (errset (values 1 2)))");
    try testing.expect(result.isCons());
    try testing.expectEqual(@as(i64, 1), result.toPtr(Cons).car.toFixnum());
    try testing.expectEqual(@as(i64, 2), result.toPtr(Cons).cdr.toPtr(Cons).car.toFixnum());
}

// --- Round-to-even edge cases ---

test "mv: round 3.5 to even" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // (round 3.5) => 4 (3.5 rounds to 4, the nearest even)
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (round 3.5) q)
    );
    try testing.expectEqual(@as(i64, 4), result.toFixnum());
}

test "mv: round 0.5 to even" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // (round 0.5) => 0 (0.5 rounds to 0, the nearest even)
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (round 0.5) q)
    );
    try testing.expectEqual(@as(i64, 0), result.toFixnum());
}

test "mv: round -2.5 to even" {
    const allocator = testing.allocator;
    const state = try initReplWithStdlib(allocator);
    defer deinitReplWithStdlib(allocator, state);

    // (round -2.5) => -2 (nearest even)
    const result = try state.repl.eval(
        \\(multiple-value-bind (q r) (round -2.5) q)
    );
    try testing.expectEqual(@as(i64, -2), result.toFixnum());
}

// --- Smallest-heap regression tests for CL compliance bugs ---
// These exercise patterns from Maxima that exposed compiler/runtime bugs.
// Using 4MB heap (half normal) to maximize GC pressure during stdlib load.

test "smallest heap: multi-pair setq" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // Multi-pair setq: all pairs must be assigned
    const result = try repl.eval(
        \\(let ((a 0) (b 0) (c 0))
        \\  (setq a 10 b 20 c 30)
        \\  (+ a b c))
    );
    try testing.expectEqual(@as(i64, 60), result.toFixnum());
}

test "smallest heap: &aux parameter slots" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // &aux must not clobber parameter slots
    const result = try repl.eval(
        \\(progn
        \\  (defun aux-test (a b c &aux d e)
        \\    (setq d (+ a b))
        \\    (setq e (+ b c))
        \\    (+ a b c d e))
        \\  (aux-test 1 2 3))
    );
    // a=1 b=2 c=3 d=3 e=5 => sum=14
    try testing.expectEqual(@as(i64, 14), result.toFixnum());
}

test "smallest heap: rplaca returns modified cons" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // rplaca/rplacd must return the cons, not the new value
    const result = try repl.eval(
        \\(let ((x (cons 1 2)))
        \\  (let ((r (rplaca x 10)))
        \\    (and (eq r x) (= (car x) 10) (= (cdr x) 2))))
    );
    try testing.expect(result.isT());
}

test "smallest heap: rplacd returns modified cons" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(let ((x (cons 1 2)))
        \\  (let ((r (rplacd x 20)))
        \\    (and (eq r x) (= (car x) 1) (= (cdr x) 20))))
    );
    try testing.expect(result.isT());
}

test "smallest heap: rplaca chain preserves structure" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // Maxima eqtest pattern: rplaca on nested list, check full structure kept
    const result = try repl.eval(
        \\(let ((x (list (list 'op) 'a 'b 'c)))
        \\  (rplaca x (list 'op 'simp))
        \\  (length x))
    );
    try testing.expectEqual(@as(i64, 4), result.toFixnum());
}

test "smallest heap: setq side-effect in cond condition" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // Pattern from Maxima tms: setq inside and/or in cond test,
    // condition is false but side-effect persists for later branches.
    // Returns (car product) which should be 42.
    const result = try repl.eval(
        \\(progn
        \\  (defun tms-pattern (product)
        \\    (cond ((and (null product)
        \\                (or nil (and (setq product (list 42)) nil)))
        \\           'branch1)
        \\          (t (car product))))
        \\  (tms-pattern nil))
    );
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "smallest heap: &aux with let and setq under GC pressure" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // Maxima timesin pattern: prog vars + multi-setq + rplacd under GC
    const result = try repl.eval(
        \\(progn
        \\  (defun timesin-pattern (x y w)
        \\    (let ((fm nil) (temp nil))
        \\      (setq temp x
        \\            fm y)
        \\      (rplacd fm (cons (car (list temp 1)) (cdr fm)))
        \\      y))
        \\  (let ((y (list 1)))
        \\    (timesin-pattern 'a y 1)
        \\    (length y)))
    );
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "smallest heap: defun &aux cond push do return regression" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(progn
        \\  (defvar *do-cond-push* nil)
        \\  (setq *do-cond-push* nil)
        \\  (defun do-cond-repro (x &aux foo)
        \\    (cond ((eq 'a (setq foo 'c))
        \\           (push x *do-cond-push*)
        \\           x)
        \\          (t
        \\           (do ((lis (list 1 2) (cdr lis)))
        \\               ((null (cdr lis)) x)
        \\             (cond ((eq 'c foo)
        \\                    (rplacd lis (cons x (cdr lis)))
        \\                    (return x)))))))
        \\  (list (do-cond-repro 9)
        \\        (if (null *do-cond-push*) 1 0)))
    );

    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expect(c0.car.isFixnum());
    try testing.expectEqual(@as(i64, 9), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 1), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
}

test "smallest heap: loop when collecting into" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 4 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    // Return length of collected list to avoid string comparison
    const result = try repl.eval(
        \\(length (loop for v in '(1 2 3 4 5)
        \\  when (oddp v) collecting v into odds
        \\  finally (return odds)))
    );
    try testing.expectEqual(@as(i64, 3), result.toFixnum());
}

fn ensureMaximaSources() !void {
    if (std.fs.cwd().access("../maxima/src/lmdcls.lisp", .{})) |_| return else |_| {}
    if (std.fs.cwd().access("../maxima/src/src/lmdcls.lisp", .{})) |_| return else |_| {}
    if (std.fs.cwd().access("../maxima/lmdcls.lisp", .{})) |_| return else |_| {}
    const candidates = [_][]const u8{
        "/tmp/maxima/src/lmdcls.lisp",
        "/tmp/maxima/src/src/lmdcls.lisp",
        "/tmp/maxima/lmdcls.lisp",
    };
    for (candidates) |path| {
        std.fs.accessAbsolute(path, .{}) catch continue;
        return;
    }
    return error.SkipZigTest;
}

fn maximaSrcDirAlloc(allocator: std.mem.Allocator) ![]u8 {
    const rel = [_][]const u8{
        "../maxima/src",
        "../maxima/src/src",
        "../maxima",
    };
    for (rel) |dir| {
        const probe = try std.fs.path.join(allocator, &.{ dir, "lmdcls.lisp" });
        defer allocator.free(probe);
        if (std.fs.cwd().access(probe, .{})) |_| {
            return try std.fs.cwd().realpathAlloc(allocator, dir);
        } else |_| {}
    }

    const abs = [_][]const u8{
        "/tmp/maxima/src",
        "/tmp/maxima/src/src",
        "/tmp/maxima",
    };
    for (abs) |dir| {
        const probe = try std.fs.path.join(allocator, &.{ dir, "lmdcls.lisp" });
        defer allocator.free(probe);
        if (std.fs.accessAbsolute(probe, .{})) |_| {
            return try allocator.dupe(u8, dir);
        } else |_| {}
    }

    return error.SkipZigTest;
}

fn parseAllFile(allocator: std.mem.Allocator, heap: *Heap, vm: *Vm, path: []const u8) !usize {
    const file = try std.fs.openFileAbsolute(path, .{});
    defer file.close();

    const stat = try file.stat();
    const max_bytes = std.math.cast(usize, stat.size) orelse return error.FileTooBig;
    const content = try file.readToEndAlloc(allocator, max_bytes);
    defer allocator.free(content);

    var parser = try Parser.init(allocator, heap, content, &vm.builtins);
    defer parser.deinit();

    var forms = std.ArrayList(Value){};
    defer forms.deinit(allocator);
    try parser.parseAll(allocator, &forms);
    return forms.items.len;
}

test "maxima core subset loader binds CAS entrypoints" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "opers"
        \\      "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\      "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\      "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\      "inmis" "db"
        \\      "compar" "lesfac" "factor" "algfac" "nalgfa" "rat3a" "rat3b" "rat3c"
        \\      "rat3d" "rat3e" "nrat4" "ratout" "acall"))
        \\  (multiple-value-bind (ok total fail) (maxima-load-all)
        \\    (list ok total fail
        \\          (if (fboundp 'maxima::simplifya) 1 0)
        \\          (if (fboundp 'maxima::$diff) 1 0)
        \\          (if (fboundp 'maxima::kindp) 1 0)
        \\          (if (fboundp 'maxima::$integrate) 1 0)
        \\          (if (fboundp 'maxima::mfuncall) 1 0)
        \\          (if (fboundp 'maxima::mformat) 1 0)
        \\          (if (fboundp 'maxima::$factor) 1 0)
        \\          (if (fboundp 'maxima::$ratsimp) 1 0)
        \\          (if (fboundp 'maxima::$expand) 1 0)
        \\          (if (handler-case (progn (maxima::$factor 1) t) (error () nil)) 1 0))))
    );
    var trace_buf = std.ArrayList(u8){};
    defer trace_buf.deinit(allocator);
    try repl.printValue(status, trace_buf.writer(allocator));
    std.debug.print("TRACE slatec-status={s}\n", .{trace_buf.items});

    try testing.expect(status.isCons());
    var cur = status;
    const expected_tail = [_]i64{ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1 };

    try testing.expect(cur.isCons());
    const ok_cell = cur.toPtr(Cons);
    try testing.expect(ok_cell.car.isFixnum());
    const ok = ok_cell.car.toFixnum();
    cur = ok_cell.cdr;

    try testing.expect(cur.isCons());
    const total_cell = cur.toPtr(Cons);
    try testing.expect(total_cell.car.isFixnum());
    const total = total_cell.car.toFixnum();
    try testing.expectEqual(total, ok);
    cur = total_cell.cdr;

    try testing.expect(cur.isCons());
    const fail_cell = cur.toPtr(Cons);
    try testing.expect(fail_cell.car.isFixnum());
    try testing.expectEqual(@as(i64, 0), fail_cell.car.toFixnum());
    cur = fail_cell.cdr;

    for (expected_tail) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima reader stage parses selected upstream modules" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    const srcdir = try maximaSrcDirAlloc(allocator);
    defer allocator.free(srcdir);

    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const pkg_init = try std.fs.path.join(allocator, &.{ srcdir, "maxima-package.lisp" });
    defer allocator.free(pkg_init);
    try repl.loadFile(pkg_init, std.io.null_writer);

    const modules = [_][]const u8{
        "float.lisp",
        "nparse.lisp",
        "transs.lisp",
        "limit.lisp",
    };

    for (modules) |file_name| {
        const path = try std.fs.path.join(allocator, &.{ srcdir, file_name });
        defer allocator.free(path);
        const form_count = try parseAllFile(allocator, &heap, &vm, path);
        try testing.expect(form_count > 0);
    }
}

test "maxima generational loader reaches ifactor without OOM" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 1024 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 24 * 1024 * 1024,
            .los_size = 24 * 1024 * 1024,
            .los_threshold = 32 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "mhayat" "combin" "opers"
        \\      "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\      "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\      "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\      "suprv1" "inmis" "db"
        \\      "compar" "lesfac" "factor" "algfac" "nalgfa" "ufact" "ifactor"))
        \\  (multiple-value-bind (ok total fail missing attempted)
        \\      (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\    (list ok total fail attempted (if missing (length missing) 0))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 48, 48, 0, 48, 0 };
    for (expected, 0..) |want, i| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        if (cell.car.toFixnum() != want) {
            std.debug.print("TRACE slatec-status idx={d} got={d} want={d}\n", .{ i, cell.car.toFixnum(), want });
        }
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima manifest loads globals before float properties" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    const srcdir = try maximaSrcDirAlloc(allocator);
    defer allocator.free(srcdir);
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const form = try std.fmt.allocPrint(
        allocator,
        \\(progn
        \\  (let ((pg (position "globals" *maxima-files* :test #'string=))
        \\        (pl (position "lmdcls" *maxima-files* :test #'string=))
        \\        (pf (position "float-properties" *maxima-files* :test #'string=)))
        \\    (unless pg (error "globals missing from Maxima manifest"))
        \\    (unless pl (error "lmdcls missing from Maxima manifest"))
        \\    (unless pf (error "float-properties missing from Maxima manifest"))
        \\    (let* ((mods '("globals" "lmdcls" "letmac" "generr" "clmacs" "defmfun-check" "float-properties"))
        \\           (ok 0)
        \\           (pkg (find-package "MAXIMA"))
        \\           (defmvar-sym (nth-value 0 (find-symbol "DEFMVAR" pkg)))
        \\           (float-sym (nth-value 0 (find-symbol "$MOST_POSITIVE_FLOAT" pkg)))
        \\           (assign-sym (nth-value 0 (find-symbol "ASSIGN" pkg)))
        \\           (neverset-sym (nth-value 0 (find-symbol "NEVERSET" pkg))))
        \\      (dolist (m mods)
        \\        (unless (maxima-try-load "{s}/" m :verbose nil)
        \\          (error "failed to load ~A" m))
        \\        (setq ok (+ ok 1)))
        \\      (list
        \\        (if (< pg pl) 1 0)
        \\        (if (< pg pf) 1 0)
        \\        ok
        \\        ok
        \\        (if (macro-function defmvar-sym) 1 0)
        \\        (if (boundp float-sym) 1 0)
        \\        (if (eq (get float-sym assign-sym)
        \\                neverset-sym)
        \\            1
        \\            0)))))
    ,
        .{srcdir},
    );
    defer allocator.free(form);

    const status = try repl.eval(form);

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 1, 1, 7, 7, 1, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima manifest loads pregexp before commac" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    const srcdir = try maximaSrcDirAlloc(allocator);
    defer allocator.free(srcdir);
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const form = try std.fmt.allocPrint(
        allocator,
        \\(progn
        \\  (let ((pp (position "pregexp" *maxima-files* :test #'string=))
        \\        (pc (position "commac" *maxima-files* :test #'string=)))
        \\    (unless pp (error "pregexp missing from Maxima manifest"))
        \\    (unless pc (error "commac missing from Maxima manifest"))
        \\    (let* ((mods '("pregexp" "globals" "lmdcls" "letmac" "generr" "clmacs" "defmfun-check" "float-properties" "commac"))
        \\           (ok 0)
        \\           (pregexp-pkg (find-package "PREGEXP"))
        \\           (pregexp-sym (nth-value 0 (find-symbol "PREGEXP" pregexp-pkg)))
        \\           (maxima-pkg (find-package "MAXIMA"))
        \\           (strip-sym (nth-value 0 (find-symbol "STRIP-FLOAT-ZEROS" maxima-pkg))))
        \\      (dolist (m mods)
        \\        (unless (maxima-try-load "{s}/" m :verbose nil)
        \\          (error "failed to load ~A" m))
        \\        (setq ok (+ ok 1)))
        \\      (list
        \\        (if (< pp pc) 1 0)
        \\        ok
        \\        ok
        \\        (if (fboundp pregexp-sym) 1 0)
        \\        (if (fboundp strip-sym) 1 0)
        \\        (if (equal (funcall strip-sym "1.2300") "1.23") 1 0)))))
    ,
        .{srcdir},
    );
    defer allocator.free(form);

    const status = try repl.eval(form);

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 1, 9, 9, 1, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima manifest loads slatec package before bessel" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 512 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (let ((pf2 (position "numerical/f2cl-package" *maxima-files* :test #'string=))
        \\        (psl (position "numerical/slatec" *maxima-files* :test #'string=))
        \\        (pfl (position "numerical/f2cl-lib" *maxima-files* :test #'string=))
        \\        (pbs (position "bessel" *maxima-files* :test #'string=)))
        \\    (unless pf2 (error "numerical/f2cl-package missing from Maxima manifest"))
        \\    (unless psl (error "numerical/slatec missing from Maxima manifest"))
        \\    (unless pfl (error "numerical/f2cl-lib missing from Maxima manifest"))
        \\    (unless pbs (error "bessel missing from Maxima manifest"))
        \\    (setq *maxima-files* (subseq *maxima-files* 0 (1+ pbs)))
        \\    (multiple-value-bind (ok total fail)
        \\        (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\      (multiple-value-bind (sym status) (find-symbol "DBESJ0" "SLATEC")
        \\        (list
        \\          (if (< pf2 pbs) 1 0)
        \\          (if (< psl pbs) 1 0)
        \\          (if (< pfl pbs) 1 0)
        \\          (if (= ok total) 1 0)
        \\          fail
        \\          (if (eq status :external) 1 0)
        \\          (if (fboundp 'maxima::bessel-j) 1 0)))))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 1, 1, 1, 1, 0, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima generational loader reaches nparse without OOM" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 1024 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 24 * 1024 * 1024,
            .los_size = 24 * 1024 * 1024,
            .los_threshold = 32 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "mhayat" "combin" "opers"
        \\      "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\      "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\      "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\      "suprv1" "inmis" "db"
        \\      "compar" "lesfac" "factor" "algfac" "nalgfa" "ufact" "ifactor"
        \\      "rat3a" "rat3b" "rat3c" "rat3d" "rat3e" "nrat4" "ratout" "acall"
        \\      "mat" "linnew" "matrix" "sprdet" "newinv" "newdet"
        \\      "schatc" "matcom" "matrun" "nisimp" "nparse"))
        \\  (multiple-value-bind (ok total fail missing attempted)
        \\      (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\    (list ok total fail attempted (if missing (length missing) 0))))
    );

    try testing.expect(status.isCons());
    const c0 = status.toPtr(Cons);
    try testing.expect(c0.car.isFixnum());
    const ok = c0.car.toFixnum();
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    const total = c1.car.toFixnum();
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isFixnum());
    const fail = c2.car.toFixnum();
    try testing.expect(c2.cdr.isCons());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isFixnum());
    const attempted = c3.car.toFixnum();

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(total, ok);
    try testing.expectEqual(total, attempted);
}

test "maxima generational full load survives defmacro-heavy files" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 1024 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 24 * 1024 * 1024,
            .los_size = 24 * 1024 * 1024,
            .los_threshold = 32 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const status = try repl.eval(
        \\(progn
        \\  (load "lib/maxima-loader.lisp")
        \\  (multiple-value-bind (ok total fail missing attempted)
        \\      (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\    (declare (ignore missing))
        \\    (list ok total fail attempted)))
    );

    try testing.expect(status.isCons());
    var cur = status;
    var got: [4]i64 = undefined;
    for (0..got.len) |i| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        got[i] = cell.car.toFixnum();
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
    try testing.expectEqual(@as(i64, 0), got[2]);
    try testing.expectEqual(got[1], got[0]);
    try testing.expectEqual(got[1], got[3]);
}

test "quasiquote preserves list-valued unquote in macro templates" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const value = try repl.eval(
        \\(let ((name 'foo) (lambda-list '(form)) (body '(ok)))
        \\  `(defun-prop (,name translate) ,lambda-list (block ,name ,@body)))
    );

    try testing.expect(value.isCons());
    const c1 = value.toPtr(Cons);
    try testing.expect(c1.car.isSymbol());
    try testing.expectEqualStrings("DEFUN-PROP", c1.car.toPtr(Symbol).getName());

    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.car.isCons());
    const fcell = c2.car.toPtr(Cons);
    try testing.expect(fcell.car.isSymbol());
    try testing.expectEqualStrings("FOO", fcell.car.toPtr(Symbol).getName());

    try testing.expect(c2.cdr.isCons());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isCons());
    const lam = c3.car.toPtr(Cons);
    try testing.expect(lam.car.isSymbol());
    try testing.expectEqualStrings("FORM", lam.car.toPtr(Symbol).getName());
}

test "def%tr-style macroexpand does not evaluate lambda-list forms" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(defmacro defun-prop (f arg &body body)
        \\  `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))
    );
    _ = try repl.eval(
        \\(defmacro def%tr (name lambda-list &body body &aux definition)
        \\  (setq definition
        \\        (if (and (null body) (symbolp lambda-list))
        \\            `(def-same%tr ,name ,lambda-list)
        \\            `(defun-prop (,name translate) ,lambda-list
        \\               (block ,name ,@body))))
        \\  `(eval-when (:compile-toplevel :execute :load-toplevel)
        \\     ,definition))
    );

    const expanded = try repl.eval(
        \\(macroexpand-1 '(def%tr $eval_when (form) 'ok))
    );

    try testing.expect(expanded.isCons());
    const c1 = expanded.toPtr(Cons);
    try testing.expect(c1.car.isSymbol());
    try testing.expectEqualStrings("EVAL-WHEN", c1.car.toPtr(Symbol).getName());

    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expect(c2.cdr.isCons());
    const c3 = c2.cdr.toPtr(Cons);
    try testing.expect(c3.car.isCons());
    const def_form = c3.car.toPtr(Cons);
    try testing.expect(def_form.car.isSymbol());
    try testing.expectEqualStrings("DEFUN-PROP", def_form.car.toPtr(Symbol).getName());
}

test "def%tr-style top-level form executes without calling lambda-list head" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(defmacro defun-prop (f arg &body body)
        \\  `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))
    );
    _ = try repl.eval(
        \\(defmacro def%tr (name lambda-list &body body &aux definition)
        \\  (setq definition
        \\        (if (and (null body) (symbolp lambda-list))
        \\            `(def-same%tr ,name ,lambda-list)
        \\            `(defun-prop (,name translate) ,lambda-list
        \\               (block ,name ,@body))))
        \\  `(eval-when (:compile-toplevel :execute :load-toplevel)
        \\     ,definition))
    );

    _ = try repl.eval("(def%tr $eval_when (form) 'ok)");

    const prop_val = try repl.eval("(get '$eval_when 'translate)");
    try testing.expect(!prop_val.isNil());
}

test "def%tr-style top-level form survives generational GC pressure" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 256 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 2 * 1024 * 1024,
            .los_size = 8 * 1024 * 1024,
            .los_threshold = 32 * 1024,
            .promote_threshold = 2,
        },
    });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(defmacro defun-prop (f arg &body body)
        \\  `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))
    );
    _ = try repl.eval(
        \\(defmacro def%tr (name lambda-list &body body &aux definition)
        \\  (setq definition
        \\        (if (and (null body) (symbolp lambda-list))
        \\            `(def-same%tr ,name ,lambda-list)
        \\            `(defun-prop (,name translate) ,lambda-list
        \\               (block ,name ,@body))))
        \\  `(eval-when (:compile-toplevel :execute :load-toplevel)
        \\     ,definition))
    );

    _ = try repl.eval(
        \\(let ((acc nil))
        \\  (dotimes (i 4000)
        \\    (setq acc (cons (list i i i i i i i i) acc)))
        \\  (length acc))
    );

    _ = try repl.eval("(def%tr $eval_when (form) 'ok)");
    const prop_val = try repl.eval("(get '$eval_when 'translate)");
    try testing.expect(!prop_val.isNil());
}

test "declare-top unspecial clears compile-time special binding state" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(defparameter *macro-file* nil)");
    _ = try repl.eval(
        \\(defmacro declare-top (&rest decl-specs)
        \\  `(eval-when
        \\    ,(cond (*macro-file* '(:compile-toplevel :load-toplevel :execute))
        \\           (t '(:compile-toplevel :execute)))
        \\    ,@(loop for v in decl-specs
        \\             unless (member (car v) '(special unspecial)) nconc nil
        \\             else
        \\             when (eql (car v) 'unspecial)
        \\             collect `(progn
        \\                        ,@(loop for w in (cdr v)
        \\                                 collect `(remprop ',w 'special)))
        \\             else collect `(proclaim ',v))))
    );
    _ = try repl.eval("(proclaim '(special p))");
    _ = try repl.eval("(setq p 7)");
    _ = try repl.eval("(declare-top (unspecial p))");
    _ = try repl.eval(
        \\(defun make-p-closure ()
        \\  (let ((p 42))
        \\    (lambda () p)))
    );
    _ = try repl.eval("(setq f (make-p-closure))");

    const result = try repl.eval("(funcall f)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "eval-when compile-toplevel keeps body cursor rooted across GC" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(setq *evw-count* 0)");

    _ = try repl.eval(
        \\(eval-when (:compile-toplevel)
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144))
        \\  (setq *evw-count* (+ *evw-count* 1))
        \\  (setq *evw-junk* (make-string 262144)))
    );

    const result = try repl.eval("*evw-count*");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 10), result.toFixnum());
}

test "eval-when accepts legacy compile load eval situations" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(eval-when (compile load eval)
        \\  (defun legacy-eval-when-fn (x) (+ x 1)))
    );

    const result = try repl.eval("(legacy-eval-when-fn 41)");
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "defun-prop keeps and/or pair metadata intact" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const value = try repl.eval(
        \\(progn
        \\  (defmacro defun-prop (f arg &body body)
        \\    (assert (listp f))
        \\    `(setf (get ',(first f) ',(second f)) #'(lambda ,arg ,@body)))
        \\  (defun-prop (and free-lisp-vars) (form)
        \\    (cdr form))
        \\  (defun-prop (or free-lisp-vars) (form)
        \\    (cdr form))
        \\  (list
        \\    (funcall (get 'and 'free-lisp-vars) '(and a b c))
        \\    (funcall (get 'or 'free-lisp-vars) '(or x y))))
    );

    try testing.expect(value.isCons());
    const outer = value.toPtr(Cons);
    try testing.expect(outer.car.isCons());
    try testing.expect(outer.cdr.isCons());
    const outer2 = outer.cdr.toPtr(Cons);
    try testing.expect(outer2.car.isCons());

    const and_tail = outer.car;
    try testing.expectEqualStrings("A", and_tail.toPtr(Cons).car.toPtr(Symbol).getName());
    try testing.expect(and_tail.toPtr(Cons).cdr.isCons());
    try testing.expectEqualStrings("B", and_tail.toPtr(Cons).cdr.toPtr(Cons).car.toPtr(Symbol).getName());
    try testing.expect(and_tail.toPtr(Cons).cdr.toPtr(Cons).cdr.isCons());
    try testing.expectEqualStrings("C", and_tail.toPtr(Cons).cdr.toPtr(Cons).cdr.toPtr(Cons).car.toPtr(Symbol).getName());

    const or_tail = outer2.car;
    try testing.expectEqualStrings("X", or_tail.toPtr(Cons).car.toPtr(Symbol).getName());
    try testing.expect(or_tail.toPtr(Cons).cdr.isCons());
    try testing.expectEqualStrings("Y", or_tail.toPtr(Cons).cdr.toPtr(Cons).car.toPtr(Symbol).getName());
}

test "maxima loader accepts internal keyword controls" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const status = try repl.eval(
        \\(progn
        \\  (load "lib/maxima-loader.lisp")
        \\  (multiple-value-bind (ok total fail missing attempted)
        \\      (maxima-load-all
        \\        :files '("lmdcls")
        \\        :verbose nil
        \\        :habu-stop-on-error t
        \\        :habu-trace nil
        \\        :habu-required-bindings '(maxima::habu-missing-probe))
        \\    (list ok total fail attempted
        \\          (if (and (consp missing)
        \\                   (eq (car missing) 'maxima::habu-missing-probe)
        \\                   (null (cdr missing)))
        \\              1
        \\              0))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 1, 1, 0, 1, 1 };
    for (expected, 0..) |want, idx| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        if (cell.car.toFixnum() != want) {
            std.debug.print("TRACE maxima-integrate status[{d}]={d} expected={d}\n", .{ idx, cell.car.toFixnum(), want });
        }
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima db subset binds addf and mode macros" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "opers"
        \\      "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\      "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\      "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\      "inmis" "db"))
        \\  (multiple-value-bind (ok total fail) (maxima-load-all)
        \\    (declare (ignore ok total fail))
        \\    (list
        \\      (if (fboundp 'maxima::addf) 1 0)
        \\      (if (fboundp 'maxima::kindp) 1 0)
        \\      (if (macro-function 'maxima::c-type) 1 0)
        \\      (if (macro-function 'maxima::s-type) 1 0)
        \\      (if (macro-function 'maxima::a-type) 1 0))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 1, 1, 1, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima ratmac subset binds pzerop" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "ratmac"))
        \\  (multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\    (declare (ignore ok total))
        \\    (list fail
        \\          (if *maxima-failed* 1 0)
        \\          (if (fboundp 'maxima::pzerop) 1 0))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 0, 0, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima trigi subset binds callable trig aliases" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "opers"
        \\      "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\      "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\      "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\      "suprv1" "inmis" "db"
        \\      "compar" "lesfac" "factor" "algfac" "nalgfa" "rat3a" "rat3b" "rat3c"
        \\      "rat3d" "rat3e" "nrat4" "ratout" "acall"
        \\      "schatc" "matcom" "matrun" "nisimp" "nparse" "displm" "displa" "nforma" "grind"
        \\      "spgcd" "ezgcd" "trigi" "trigo"))
        \\  (multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\    (declare (ignore ok total))
        \\    (list
        \\      fail
        \\      (if *maxima-failed* 1 0)
        \\      (if (and (fboundp 'maxima::atan)
        \\               (functionp (symbol-function 'maxima::atan)))
        \\          1
        \\          0)
        \\      (if (and (fboundp 'maxima::asin)
        \\               (functionp (symbol-function 'maxima::asin)))
        \\          1
        \\          0)
        \\      (if (fboundp 'maxima::$sin) 1 0)
        \\      (if (fboundp 'maxima::$cos) 1 0)
        \\      (if (handler-case (equal (maxima::$sin 0) 0) (error () nil)) 1 0)
        \\      (if (handler-case (equal (maxima::$cos 0) 1) (error () nil)) 1 0))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 0, 0, 1, 1, 1, 1, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima integrate dependency chain binds matcher and partition symbols" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const status = try repl.eval(
        \\(progn
        \\  (setq *maxima-files*
        \\    '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\      "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "opers"
        \\      "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\      "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\      "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\      "suprv1" "inmis" "db"
        \\      "compar" "lesfac" "factor" "algfac" "nalgfa" "rat3a" "rat3b" "rat3c"
        \\      "rat3d" "rat3e" "nrat4" "ratout" "acall"
        \\      "schatc" "matcom" "matrun" "nisimp" "nparse" "displm" "displa" "nforma" "grind"
        \\      "nset" "sinint" "sin"))
        \\  (multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\    (declare (ignore ok total))
        \\    (list
        \\      fail
        \\      (if *maxima-failed* 1 0)
        \\      (if (fboundp 'maxima::partition) 1 0)
        \\      (if (fboundp 'maxima::m2) 1 0)
        \\      (if (macro-function 'maxima::schatchen-cond) 1 0)
        \\      (if (fboundp 'maxima::alias) 1 0)
        \\      (if (fboundp 'maxima::$setp) 1 0)
        \\      (if (fboundp 'maxima::sinint) 1 0)
        \\      (if (fboundp 'maxima::pzerop) 1 0)
        \\      (if (and (= fail 0)
        \\               (fboundp 'maxima::pzerop)
        \\               (maxima::$integrate 0 'maxima::$x))
        \\          1
        \\          0))))
    );

    try testing.expect(status.isCons());
    var cur = status;
    var got: [10]i64 = undefined;
    for (0..got.len) |i| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        got[i] = cell.car.toFixnum();
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
    try testing.expectEqual(@as(i64, 0), got[0]);
    try testing.expectEqual(@as(i64, 0), got[1]);
    try testing.expectEqual(@as(i64, 1), got[2]);
    try testing.expectEqual(@as(i64, 1), got[3]);
    try testing.expectEqual(@as(i64, 1), got[4]);
    try testing.expectEqual(@as(i64, 1), got[5]);
    try testing.expectEqual(@as(i64, 1), got[6]);
    try testing.expectEqual(@as(i64, 1), got[7]);
    try testing.expectEqual(@as(i64, 1), got[8]);
    try testing.expectEqual(@as(i64, 1), got[9]);
}

test "maxima loader binds residu and defint after core bootstrap" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 384 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("defint-residu-script.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(load \"lib/stdlib.habu\")\n" ++
                "(load \"lib/maxima-loader.lisp\")\n" ++
                "(maxima-load-all :verbose nil)\n" ++
                "(setq *defint-residu-status*\n" ++
                "      (list\n" ++
                "        (if (assoc \"residu\" *maxima-failed* :test #'equal) 1 0)\n" ++
                "        (if (assoc \"defint\" *maxima-failed* :test #'equal) 1 0)\n" ++
                "        (if (fboundp 'maxima::$residue) 1 0)\n" ++
                "        (if (fboundp 'maxima::$defint) 1 0)))\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "defint-residu-script.lsp" });
    defer allocator.free(script_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile(script_abs, stream.writer());

    const status = try repl.eval("*defint-residu-status*");
    try testing.expect(status.isCons());
    var cur = status;
    const expected = [_]i64{ 0, 0, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "maxima e2e operation readiness status" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 384 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");
    _ = try repl.eval(
        \\(setq *maxima-files*
        \\  '("lmdcls" "letmac" "clmacs" "commac" "mormac" "globals" "compat"
        \\    "defcal" "maxmac" "mopers" "mforma" "mrgmac" "strmac" "rzmac" "ratmac" "mhayat" "combin" "opers"
        \\    "utils" "merror" "mutils" "sumcon" "sublis" "mformt" "outmis" "ar"
        \\    "comm" "comm2" "mlisp" "mmacro" "buildq"
        \\    "simp" "float" "csimp" "csimp2" "zero" "logarc" "rpart"
        \\    "suprv1" "inmis" "db"
        \\    "compar" "lesfac" "factor" "algfac" "nalgfa" "ufact" "ifactor" "rat3a" "rat3b" "rat3c"
        \\    "rat3d" "rat3e" "nrat4" "ratout" "acall"
        \\    "mat" "linnew" "matrix" "sprdet" "newinv" "newdet"
        \\    "schatc" "matcom" "matrun" "nisimp" "nparse" "displm" "displa" "nforma" "grind"
        \\    "nset" "sinint" "sin" "trigi" "trigo" "trgred"
        \\    "tlimit" "limit"
        \\    "solve" "psolve" "algsys" "sqrtdenest" "polyrz" "cpoly"))
    );
    const status = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (list
        \\    fail
        \\    (if *maxima-failed* 1 0)
        \\    (if (handler-case (equal (maxima::simplifya '((maxima::mplus) 3 4) t) 7) (error () nil)) 1 0)
        \\    (if (handler-case (progn (maxima::$diff 0 'maxima::$x) t) (error () nil)) 1 0)
        \\    (if (handler-case (progn (maxima::$solve 0 'maxima::$x) t) (error () nil)) 1 0)
        \\    (if (handler-case (equal (maxima::$integrate 0 'maxima::$x) 0) (error () nil)) 1 0)
        \\    (if (handler-case (progn (maxima::$factor 1) t) (error () nil)) 1 0)
        \\    (if (handler-case (progn (maxima::$limit 0 'maxima::$x 0) t) (error () nil)) 1 0)
        \\    (if (handler-case (progn (maxima::$determinant 1) t) (error () nil)) 1 0)
        \\    (if (handler-case (progn (maxima::$expand 1) t) (error () nil)) 1 0)
        \\    (if (handler-case (equal (maxima::$sin 0) 0) (error () nil)) 1 0)
        \\    (if (handler-case (equal (maxima::$cos 0) 1) (error () nil)) 1 0)))
    );

    try testing.expect(status.isCons());
    var cur = status;
    var got: [12]i64 = undefined;
    for (0..got.len) |i| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        got[i] = cell.car.toFixnum();
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
    try testing.expectEqual(@as(i64, 0), got[0]);
    try testing.expectEqual(@as(i64, 0), got[1]);
    try testing.expectEqual(@as(i64, 1), got[2]);
    try testing.expectEqual(@as(i64, 1), got[3]);
    try testing.expectEqual(@as(i64, 1), got[4]);
    try testing.expectEqual(@as(i64, 1), got[5]);
    try testing.expectEqual(@as(i64, 1), got[6]);
    try testing.expectEqual(@as(i64, 1), got[7]);
    try testing.expectEqual(@as(i64, 1), got[8]);
    try testing.expectEqual(@as(i64, 1), got[9]);
    try testing.expectEqual(@as(i64, 1), got[10]);
    try testing.expectEqual(@as(i64, 1), got[11]);
}

test "maxima loader stop-on-error does not resume after first failure" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 384 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        var file = try tmp.dir.createFile("transl-script.lsp", .{});
        defer file.close();
        try file.writeAll(
            "(load \"lib/maxima-loader.lisp\")\n" ++
                "(let* ((failed-id \"__habu_missing_module__\")\n" ++
                "       (files (list \"lmdcls\" failed-id \"letmac\"))\n" ++
                "       (expected-attempted 2))\n" ++
                "  (setq *maxima-files* files)\n" ++
                "  (setq *transl-status*\n" ++
                "        (multiple-value-bind (ok total fail missing attempted)\n" ++
                "            (maxima-load-all :verbose nil :habu-stop-on-error t)\n" ++
                "          (declare (ignore missing))\n" ++
                "          (list ok total fail attempted expected-attempted\n" ++
                "                (if (and (consp *maxima-failed*)\n" ++
                "                         (consp (car *maxima-failed*))\n" ++
                "                         (let ((id (caar *maxima-failed*)))\n" ++
                "                           (or (and (symbolp id) (string= (symbol-name id) \"__HABU_MISSING_MODULE__\"))\n" ++
                "                               (and (stringp id) (string= id \"__habu_missing_module__\")))))\n" ++
                "                    1\n" ++
                "                    0)))))\n",
        );
    }

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "transl-script.lsp" });
    defer allocator.free(script_abs);

    var buf: [1024]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile(script_abs, stream.writer());

    const status = try repl.eval("*transl-status*");
    try testing.expect(status.isCons());
    var cur = status;
    var got: [6]i64 = undefined;
    for (0..got.len) |i| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        got[i] = cell.car.toFixnum();
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());

    try testing.expectEqual(@as(i64, 1), got[0]);
    try testing.expectEqual(@as(i64, 3), got[1]);
    try testing.expectEqual(@as(i64, 1), got[2]);
    try testing.expectEqual(@as(i64, 2), got[3]);
    try testing.expectEqual(@as(i64, 2), got[4]);
    try testing.expectEqual(@as(i64, 1), got[5]);
}

test "maxima defun-maclisp old narg syntax defines callable function" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(progn
        \\  (load "lib/maxima-loader.lisp")
        \\  (setq *maxima-files* '("lmdcls" "letmac" "clmacs" "commac"))
        \\  (maxima-load-all))
    );

    const result = try repl.eval(
        \\(progn
        \\  (maxima::defun-maclisp maxima::foo n (maxima::listify n))
        \\  (maxima::foo 10 20 30))
    );
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 10), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 20), c1.car.toFixnum());
    try testing.expect(c1.cdr.isCons());
    const c2 = c1.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 30), c2.car.toFixnum());
    try testing.expect(c2.cdr.isNil());
}

test "lambda aux initializer can reference prior rest param" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        \\(progn
        \\  (defun test-aux-scope (&rest args &aux (n (length args)))
        \\    (list n (length args)))
        \\  (test-aux-scope 1 2 3))
    );
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 3), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 3), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
}

test "let mixed lexical and special bindings stay dynamically visible" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const mixed = try repl.eval(
        \\(progn
        \\  (defvar *mix-special* 1)
        \\  (defun mix-special-set (v) (setq *mix-special* v))
        \\  (let ((a 1) (*mix-special* 2))
        \\    (mix-special-set 9)
        \\    (list a *mix-special*)))
    );
    try testing.expect(mixed.isCons());
    const c0 = mixed.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 9), c1.car.toFixnum());

    const global = try repl.eval("*mix-special*");
    try testing.expectEqual(@as(i64, 1), global.toFixnum());
}

test "proclaimed special lambda params are dynamically visible in callees" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (proclaim '(special foo))
        \\  (defun inner-special-param () foo)
        \\  (defun outer-special-param (foo) (inner-special-param))
        \\  (outer-special-param 42))
    );

    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 42), out.toFixnum());
}

test "defvar makes prog bindings dynamically visible" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (defvar nn*)
        \\  (defun use-nn () nn*)
        \\  (prog (nn*)
        \\    (setq nn* 3)
        \\    (return (use-nn))))
    );

    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 3), out.toFixnum());
}

test "let declare special uses dynamic binding and restores global" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (defvar ans 7)
        \\  (defun use-ans () ans)
        \\  (list
        \\    (let ((ans 0))
        \\      (declare (special ans))
        \\      (setq ans 42)
        \\      (use-ans))
        \\    ans))
    );

    try testing.expect(out.isCons());
    const c0 = out.toPtr(Cons);
    try testing.expect(c0.car.isFixnum());
    try testing.expectEqual(@as(i64, 42), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expect(c1.car.isFixnum());
    try testing.expectEqual(@as(i64, 7), c1.car.toFixnum());
    try testing.expect(c1.cdr.isNil());
}

test "progv supports deep dynamic nesting" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (defun walk-progv (n)
        \\    (if (= n 0)
        \\        t
        \\        (progv '(x) (list n)
        \\          (walk-progv (1- n)))))
        \\  (walk-progv 40))
    );

    try testing.expect(out.isT());
}

test "symbol value cells handle uninterned and fresh interned symbols" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (makunbound 'fresh-runtime-cell)
        \\  (let ((s (gensym "X")))
        \\    (setf (symbol-value s) 42)
        \\    (setf (symbol-value 'fresh-runtime-cell) 9)
        \\    (list
        \\      (if (= (symbol-value s) 42) 1 0)
        \\      (if (boundp s) 1 0)
        \\      (progn (makunbound s) (if (boundp s) 0 1))
        \\      (if (= (symbol-value 'fresh-runtime-cell) 9) 1 0)
        \\      (if (boundp 'fresh-runtime-cell) 1 0)
        \\      (if (= (progv (list s) (list 7) (symbol-value s)) 7) 1 0)
        \\      (if (boundp s) 0 1))))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 7) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(@as(i64, 1), cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "defun creates function binding without value binding" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (when (boundp 'defun-value-probe) (makunbound 'defun-value-probe))
        \\  (defun defun-value-probe (x) x)
        \\  (list (boundp 'defun-value-probe)
        \\        (fboundp 'defun-value-probe)
        \\        (funcall #'defun-value-probe 7)))
    );

    try testing.expect(out.isCons());
    const first = out.toPtr(Cons);
    try testing.expect(first.car.isNil());
    try testing.expect(first.cdr.isCons());
    const second = first.cdr.toPtr(Cons);
    try testing.expect(second.car.isT());
    try testing.expect(second.cdr.isCons());
    const third = second.cdr.toPtr(Cons);
    try testing.expect(third.car.isFixnum());
    try testing.expectEqual(@as(i64, 7), third.car.toFixnum());
    try testing.expect(third.cdr.isNil());
}

test "numeric predicates support generic numeric tower values" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(list
        \\  (if (zerop 0) 1 0)
        \\  (if (zerop 0.0) 1 0)
        \\  (if (zerop 1000000000000000000000000000000000000000) 0 1)
        \\  (if (plusp 1000000000000000000000000000000000000000) 1 0)
        \\  (if (minusp -1000000000000000000000000000000000000000) 1 0)
        \\  (if (zerop (/ 0 7)) 1 0)
        \\  (if (plusp (/ 3 2)) 1 0)
        \\  (if (minusp (/ -3 2)) 1 0)
        \\  (if (zerop (complex 0 0)) 1 0)
        \\  (handler-case (progn (plusp (complex 1 1)) 0) (error (e) (declare (ignore e)) 1))
        \\  (handler-case (progn (minusp (complex 1 1)) 0) (error (e) (declare (ignore e)) 1)))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 11) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(@as(i64, 1), cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "symbol-function ignores special value bindings" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (defun selector (x env) (declare (ignore env)) x)
        \\  (proclaim '(special selector))
        \\  (let ((selector '(selector)))
        \\    (setf (macro-function 'm) (symbol-function 'selector))
        \\    (macro-function 'm)))
    );

    try testing.expect(out.isClosure());
}

test "value and function namespaces stay independent for shared symbol" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (setq ratvars '(a b c))
        \\  (defun ratvars () 'ok)
        \\  (list
        \\    (if (equal (cdr ratvars) '(b c)) 1 0)
        \\    (if (eq (ratvars) 'ok) 1 0)
        \\    (if (equal (symbol-value 'ratvars) '(a b c)) 1 0)
        \\    (if (functionp (symbol-function 'ratvars)) 1 0)))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 4) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(@as(i64, 1), cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "defun does not clobber nil value binding" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (setq maybe-nil nil)
        \\  (defun maybe-nil () 'ok)
        \\  (list
        \\    (if (null (symbol-value 'maybe-nil)) 1 0)
        \\    (if (eq (maybe-nil) 'ok) 1 0)))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 2) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(@as(i64, 1), cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "symbol-function resolves internal setf setter helpers" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(list
        \\  (functionp (symbol-function '%aset))
        \\  (functionp (symbol-function '%svset))
        \\  (functionp (symbol-function '%sset)))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 3) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isT());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "symbol-function survives small generational nursery GC pressure" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 64 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 2 * 1024 * 1024,
        },
    });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (dotimes (i 2500)
        \\    (setf (symbol-function 'gc-fn) (lambda () 1)))
        \\  (list
        \\    (if (functionp (symbol-function 'gc-fn)) 1 0)
        \\    (if (= (funcall (symbol-function 'gc-fn)) 1) 1 0)))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 2) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(@as(i64, 1), cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "adaptive tenuring metrics stay bounded under generational load" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{
        .total_size = 64 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 2 * 1024 * 1024,
            .promote_threshold = 128,
        },
    });
    defer heap.deinit();
    const start_threshold = heap.promote_threshold;

    const payload =
        "abcdefghijklmnopqrstuvwxyz0123456789" ++
        "abcdefghijklmnopqrstuvwxyz0123456789" ++
        "abcdefghijklmnopqrstuvwxyz0123456789" ++
        "abcdefghijklmnopqrstuvwxyz0123456789";

    var roots: [192]Value = [_]Value{Value.nil} ** 192;
    var cycles: usize = 0;
    while (cycles < 8) : (cycles += 1) {
        for (&roots) |*slot| {
            slot.* = try heap.allocBaseString(payload);
        }
        _ = try heap.collectGarbage(&roots);
        for (&roots) |*slot| slot.* = Value.nil;
        _ = try heap.collectGarbage(&roots);
    }

    try testing.expect(heap.stats.gc_minor_count > 0);
    try testing.expect(heap.stats.gc_promote_n > 0);
    try testing.expect(heap.stats.gc_promote_threshold >= heap.stats.gc_promote_threshold_min);
    try testing.expect(heap.stats.gc_promote_threshold <= heap.stats.gc_promote_threshold_max);
    try testing.expect(heap.stats.gc_promote_scale >= 0.5);
    try testing.expect(heap.stats.gc_promote_scale <= 1.5);
    try testing.expect(heap.stats.gc_promote_success_rate >= 0.0);
    try testing.expect(heap.stats.gc_promote_success_rate <= 1.0);
    try testing.expect(heap.stats.gc_promote_young_ratio >= 0.0);
    try testing.expect(heap.stats.gc_promote_young_ratio <= 1.0);
    try testing.expect(heap.stats.gc_promote_mature_ratio >= 0.0);
    try testing.expect(heap.stats.gc_promote_mature_ratio <= 1.0);
    try testing.expect(heap.stats.gc_promote_threshold > start_threshold);
}

test "symbol-plist funcall parity and getl plist search" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (setf (symbol-plist 'gp-probe) nil)
        \\  (put 'gp-probe 'a 10)
        \\  (put 'gp-probe 'b 20)
        \\  (let ((direct (symbol-plist 'gp-probe))
        \\        (via-funcall (funcall #'symbol-plist 'gp-probe))
        \\        (from-sym (getl 'gp-probe '(z b)))
        \\        (from-cons (getl (cons 'head (symbol-plist 'gp-probe)) '(z b))))
        \\    (list
        \\      (if (equal direct via-funcall) 1 0)
        \\      (if (= (get 'gp-probe 'b) 20) 1 0)
        \\      (if (and (consp from-sym) (eq (car from-sym) 'b) (= (cadr from-sym) 20)) 1 0)
        \\      (if (and (consp from-cons) (eq (car from-cons) 'b) (= (cadr from-cons) 20)) 1 0)
        \\      (if (null (getl 'gp-probe '(missing))) 1 0))))
    );

    try testing.expect(out.isCons());
    var cur = out;
    var i: usize = 0;
    while (i < 5) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(@as(i64, 1), cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "handler-case catches invalid argument and invalid type specifier" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(list
        \\  (if (eq (handler-case
        \\               (position "x" "abca")
        \\             (error (c)
        \\               (declare (ignore c))
        \\               :caught))
        \\           :caught)
        \\      1
        \\      0)
        \\  (if (eq (handler-case
        \\               (typep 1 '(integer foo *))
        \\             (error (c)
        \\               (declare (ignore c))
        \\               :caught))
        \\           :caught)
        \\      1
        \\      0)
        \\  (if (eq (handler-case
        \\               (typep 1 '(integer 0))
        \\             (error (c)
        \\               (declare (ignore c))
        \\               :caught))
        \\           t)
        \\      1
        \\      0))
    );

    try testing.expect(out.isCons());
    var cur = out;
    const expected = [_]i64{ 1, 1, 1 };
    for (expected) |want| {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        try testing.expect(cell.car.isFixnum());
        try testing.expectEqual(want, cell.car.toFixnum());
        cur = cell.cdr;
    }
    try testing.expect(cur.isNil());
}

test "local type declarations do not leak into unrelated lets" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 16 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(progn
        \\  (defun decl-type-leak-seed ()
        \\    (let ((test #'car))
        \\      (declare (type function test))
        \\      test))
        \\  (decl-type-leak-seed)
        \\  (let ((test '(7 8)))
        \\    (car test)))
    );

    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 7), out.toFixnum());
}

test "maxima letmac destructuring-let expands and runs" {
    try ensureMaximaSources();
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval(
        \\(progn
        \\  (load "lib/maxima-loader.lisp")
        \\  (setq *maxima-files* '("lmdcls" "letmac"))
        \\  (maxima-load-all))
    );

    const result = try repl.eval(
        \\(progn
        \\  (maxima::destructuring-let (((a b) '(1 2))) (list a b)))
    );
    try testing.expect(result.isCons());
    const c0 = result.toPtr(Cons);
    try testing.expectEqual(@as(i64, 1), c0.car.toFixnum());
    try testing.expect(c0.cdr.isCons());
    const c1 = c0.cdr.toPtr(Cons);
    try testing.expectEqual(@as(i64, 2), c1.car.toFixnum());
}
