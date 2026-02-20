const std = @import("std");
const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const Heap = runtime.Heap;
const Repl = interp.Repl;
const Value = runtime.Value;

const Bench = struct {
    name: []const u8,
    ns: u64,
    err_name: ?[]const u8 = null,
    category: []const u8,
};

const Opts = struct {
    heap_mb: usize = 512,
    iters: usize = 3,
    json: bool = false,
    mode: enum { jit, interp } = .jit,
};

fn parseArgs() Opts {
    var opts = Opts{};
    var it = std.process.args();
    _ = it.next();
    while (it.next()) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            opts.json = true;
        } else if (std.mem.eql(u8, arg, "--interp")) {
            opts.mode = .interp;
        } else if (std.mem.startsWith(u8, arg, "--heap-mb=")) {
            opts.heap_mb = std.fmt.parseInt(usize, arg["--heap-mb=".len..], 10) catch 512;
        } else if (std.mem.startsWith(u8, arg, "--iters=")) {
            opts.iters = std.fmt.parseInt(usize, arg["--iters=".len..], 10) catch 3;
        }
    }
    return opts;
}

const BenchDef = struct {
    name: []const u8,
    category: []const u8,
    /// Setup for JIT mode (defun with speed 3 safety 0 declarations)
    setup_jit: ?[]const u8 = null,
    /// Additional JIT setup (evaluated separately so JIT fires per-defun)
    setup_jit2: ?[]const u8 = null,
    /// Setup for interp mode (defun without optimization declarations)
    setup_interp: ?[]const u8 = null,
    /// Expression to time.
    expr: []const u8,
};

const decl_jit = "(declare (optimize (speed 3) (safety 0)))";

const bench_defs = [_]BenchDef{
    // ── arith ──
    .{
        .name = "fixnum_loop", .category = "arith",
        .setup_jit = "(defun bench-fixnum-loop () " ++ decl_jit ++
            " (let ((i 0) (acc 0)) (while (< i 1000000) (setq acc (+ acc i)) (setq i (+ i 1))) acc))",
        .setup_interp = "(defun bench-fixnum-loop () (let ((i 0) (acc 0)) (while (< i 1000000) (setq acc (+ acc i)) (setq i (+ i 1))) acc))",
        .expr = "(bench-fixnum-loop)",
    },
    .{
        .name = "fixnum_mul", .category = "arith",
        .setup_jit = "(defun bench-fixnum-mul () " ++ decl_jit ++
            " (let ((i 0) (acc 1)) (while (< i 1000000) (setq acc (logand (+ acc (* (+ i 1) 3)) #xffffff)) (setq i (+ i 1))) acc))",
        .setup_interp = "(defun bench-fixnum-mul () (let ((i 0) (acc 1)) (while (< i 1000000) (setq acc (logand (+ acc (* (+ i 1) 3)) #xffffff)) (setq i (+ i 1))) acc))",
        .expr = "(bench-fixnum-mul)",
    },
    .{
        .name = "gcd", .category = "arith",
        .setup_jit = "(defun bench-gcd () " ++ decl_jit ++
            " (let ((i 0) (sum 0)) (while (< i 100000) (setq sum (+ sum (gcd (+ i 17) (+ i 31)))) (setq i (+ i 1))) sum))",
        .setup_interp = "(defun bench-gcd () (let ((i 0) (sum 0)) (while (< i 100000) (setq sum (+ sum (gcd (+ i 17) (+ i 31)))) (setq i (+ i 1))) sum))",
        .expr = "(bench-gcd)",
    },

    // ── float ──
    .{
        .name = "float_sum", .category = "float",
        .setup_jit = "(defun bench-float-sum () " ++ decl_jit ++
            " (let ((i 0) (acc 0.0)) (while (< i 100000) (setq acc (+ acc (* (float i) 0.001))) (setq i (+ i 1))) (round acc)))",
        .setup_interp = "(defun bench-float-sum () (let ((i 0) (acc 0.0)) (while (< i 100000) (setq acc (+ acc (* (float i) 0.001))) (setq i (+ i 1))) (round acc)))",
        .expr = "(bench-float-sum)",
    },
    .{
        .name = "float_sqrt", .category = "float",
        .setup_jit = "(defun bench-float-sqrt () " ++ decl_jit ++
            " (let ((i 0) (acc 0.0)) (while (< i 100000) (setq acc (+ acc (sqrt (+ 1.0 (float i))))) (setq i (+ i 1))) (round acc)))",
        .setup_interp = "(defun bench-float-sqrt () (let ((i 0) (acc 0.0)) (while (< i 100000) (setq acc (+ acc (sqrt (+ 1.0 (float i))))) (setq i (+ i 1))) (round acc)))",
        .expr = "(bench-float-sqrt)",
    },

    // ── recurse ──
    .{
        .name = "fib35", .category = "recurse",
        .setup_jit = "(defun fib (n) (declare (type fixnum n) (optimize (speed 3) (safety 0))) (if (<= n 1) n (the fixnum (+ (fib (the fixnum (- n 1))) (fib (the fixnum (- n 2)))))))",
        .setup_interp = "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
        .expr = "(fib 35)",
    },
    .{
        .name = "tak", .category = "recurse",
        .setup_jit =
        \\(defun tak (x y z) (declare (type fixnum x y z) (optimize (speed 3) (safety 0)))
        \\    (if (<= x y) z (tak (tak (the fixnum (- x 1)) y z) (tak (the fixnum (- y 1)) z x) (tak (the fixnum (- z 1)) x y))))
        ,
        .setup_interp =
        \\(defun tak (x y z) (if (<= x y) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))
        ,
        .expr = "(let ((i 0)) (while (< i 1000) (tak 18 12 6) (setq i (+ i 1))))",
    },
    .{
        .name = "ack", .category = "recurse",
        .setup_jit = "(defun ack (m n) (declare (optimize (speed 3) (safety 0))) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))",
        .setup_interp = "(defun ack (m n) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))",
        .expr = "(ack 3 10)",  // ~180ms — eval overhead negligible
    },
    .{
        .name = "nqueens10", .category = "recurse",
        .setup_jit =
        \\(defun nqueens-safe-p (col placed row)
        \\    (declare (optimize (speed 3) (safety 0)))
        \\    (if (null placed) t
        \\        (let ((c (car placed)))
        \\          (if (not (= c col))
        \\              (if (not (= (abs (- c col)) row))
        \\                  (nqueens-safe-p col (cdr placed) (+ row 1))
        \\                  nil)
        \\              nil))))
        ,
        .setup_jit2 =
        \\(progn
        \\  (defun nqueens-solve (n row placed)
        \\    (declare (optimize (speed 3) (safety 0)))
        \\    (if (= row n) 1
        \\        (let ((count 0) (col 0))
        \\          (while (< col n)
        \\            (when (nqueens-safe-p col placed 1)
        \\              (setq count (+ count (nqueens-solve n (+ row 1) (cons col placed)))))
        \\            (setq col (+ col 1)))
        \\          count)))
        \\  (defun nqueens (n) (nqueens-solve n 0 nil)))
        ,
        .setup_interp =
        \\(progn
        \\  (defun nqueens-safe-p (col placed row)
        \\    (if (null placed) t
        \\        (let ((c (car placed)))
        \\          (if (not (= c col))
        \\              (if (not (= (abs (- c col)) row))
        \\                  (nqueens-safe-p col (cdr placed) (+ row 1))
        \\                  nil)
        \\              nil))))
        \\  (defun nqueens-solve (n row placed)
        \\    (if (= row n) 1
        \\        (let ((count 0) (col 0))
        \\          (while (< col n)
        \\            (when (nqueens-safe-p col placed 1)
        \\              (setq count (+ count (nqueens-solve n (+ row 1) (cons col placed)))))
        \\            (setq col (+ col 1)))
        \\          count)))
        \\  (defun nqueens (n) (nqueens-solve n 0 nil)))
        ,
        .expr = "(nqueens 10)",
    },

    // ── list ──
    .{
        .name = "list_build", .category = "list",
        .setup_jit = "(defun bench-list-build () " ++ decl_jit ++
            " (let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length xs)))",
        .setup_interp = "(defun bench-list-build () (let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length xs)))",
        .expr = "(bench-list-build)",
    },
    .{
        .name = "list_reverse", .category = "list",
        .setup_jit = "(defun bench-list-reverse () " ++ decl_jit ++
            " (let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length (nreverse xs))))",
        .setup_interp = "(defun bench-list-reverse () (let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length (nreverse xs))))",
        .expr = "(bench-list-reverse)",
    },
    .{
        .name = "list_append", .category = "list",
        .setup_jit = "(defun bench-list-append () " ++ decl_jit ++
            " (let ((base (let ((xs nil) (i 0)) (while (< i 100) (setq xs (cons i xs)) (setq i (+ i 1))) xs)) (result nil) (i 0)) (while (< i 1000) (setq result (append base result)) (setq i (+ i 1))) (length result)))",
        .setup_interp = "(defun bench-list-append () (let ((base (let ((xs nil) (i 0)) (while (< i 100) (setq xs (cons i xs)) (setq i (+ i 1))) xs)) (result nil) (i 0)) (while (< i 1000) (setq result (append base result)) (setq i (+ i 1))) (length result)))",
        .expr = "(bench-list-append)",
    },
    .{
        .name = "assoc", .category = "list",
        .setup_jit = "(defun bench-assoc () " ++ decl_jit ++
            " (let ((al (let ((xs nil) (i 0)) (while (< i 100) (setq xs (cons (cons i (* i i)) xs)) (setq i (+ i 1))) xs)) (sum 0) (i 0)) (while (< i 50000) (let ((pair (assoc (mod i 100) al))) (when pair (setq sum (+ sum (cdr pair))))) (setq i (+ i 1))) sum))",
        .setup_interp = "(defun bench-assoc () (let ((al (let ((xs nil) (i 0)) (while (< i 100) (setq xs (cons (cons i (* i i)) xs)) (setq i (+ i 1))) xs)) (sum 0) (i 0)) (while (< i 50000) (let ((pair (assoc (mod i 100) al))) (when pair (setq sum (+ sum (cdr pair))))) (setq i (+ i 1))) sum))",
        .expr = "(bench-assoc)",
    },

    // ── hof ──
    .{
        .name = "mapcar", .category = "hof",
        .setup_jit = "(defun bench-mapcar () " ++ decl_jit ++
            " (let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r))) (length (mapcar (lambda (x) (+ x 1)) xs))))",
        .setup_interp = "(defun bench-mapcar () (let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r))) (length (mapcar (lambda (x) (+ x 1)) xs))))",
        .expr = "(bench-mapcar)",
    },
    .{
        .name = "reduce", .category = "hof",
        .setup_jit = "(defun bench-reduce () " ++ decl_jit ++
            " (let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r))) (reduce #'+ xs)))",
        .setup_interp = "(defun bench-reduce () (let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r))) (reduce #'+ xs)))",
        .expr = "(bench-reduce)",
    },
    .{
        .name = "remove_if", .category = "hof",
        .setup_jit = "(defun bench-remove-if () " ++ decl_jit ++
            " (let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r)) (result nil) (rest nil)) (setq rest xs) (while rest (when (not (oddp (car rest))) (setq result (cons (car rest) result))) (setq rest (cdr rest))) (length result)))",
        .setup_interp = "(defun bench-remove-if () (let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r)) (result nil) (rest nil)) (setq rest xs) (while rest (when (not (oddp (car rest))) (setq result (cons (car rest) result))) (setq rest (cdr rest))) (length result)))",
        .expr = "(bench-remove-if)",
    },

    // ── hash ──
    .{
        .name = "hash_insert", .category = "hash",
        .setup_jit = "(defun bench-hash-insert () " ++ decl_jit ++
            " (let ((h (make-hash-table :size 256)) (i 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (hash-table-count h)))",
        .setup_interp = "(defun bench-hash-insert () (let ((h (make-hash-table :size 256)) (i 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (hash-table-count h)))",
        .expr = "(bench-hash-insert)",
    },
    .{
        .name = "hash_lookup", .category = "hash",
        .setup_jit = "(defun bench-hash-lookup () " ++ decl_jit ++
            " (let ((h (make-hash-table :size 256)) (i 0) (sum 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (setq i 0) (while (< i 50000) (let ((v (gethash (mod i 20000) h))) (when v (setq sum (+ sum v)))) (setq i (+ i 1))) sum))",
        .setup_interp = "(defun bench-hash-lookup () (let ((h (make-hash-table :size 256)) (i 0) (sum 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (setq i 0) (while (< i 50000) (let ((v (gethash (mod i 20000) h))) (when v (setq sum (+ sum v)))) (setq i (+ i 1))) sum))",
        .expr = "(bench-hash-lookup)",
    },

    // ── string ──
    .{
        .name = "string_concat", .category = "string",
        .setup_jit = "(defun bench-string-concat () " ++ decl_jit ++
            \\  (let ((result "") (i 0)) (while (< i 1000) (setq result (concatenate 'string result "x")) (setq i (+ i 1))) (length result)))
        ,
        .setup_interp = "(defun bench-string-concat ()" ++
            \\  (let ((result "") (i 0)) (while (< i 1000) (setq result (concatenate 'string result "x")) (setq i (+ i 1))) (length result)))
        ,
        .expr = "(bench-string-concat)",
    },
    .{
        .name = "string_search", .category = "string",
        .setup_jit = "(defun bench-string-search () " ++ decl_jit ++
            \\ (let ((haystack (make-string 10000 :initial-element #\a)) (count 0) (i 0)) (setf (char haystack 9999) #\b) (while (< i 1000) (when (position #\b haystack) (setq count (+ count 1))) (setq i (+ i 1))) count))
        ,
        .setup_interp = "(defun bench-string-search ()" ++
            \\ (let ((haystack (make-string 10000 :initial-element #\a)) (count 0) (i 0)) (setf (char haystack 9999) #\b) (while (< i 1000) (when (position #\b haystack) (setq count (+ count 1))) (setq i (+ i 1))) count))
        ,
        .expr = "(bench-string-search)",
    },

    // ── sort ──
    .{
        .name = "sort_fixnum", .category = "sort",
        .setup_jit = "(defun bench-sort-fixnum () " ++ decl_jit ++
            " (let ((xs (let ((r nil) (i 100)) (while (> i 0) (setq r (cons i r)) (setq i (- i 1))) r))) (length (sort xs #'<))))",
        .setup_interp = "(defun bench-sort-fixnum () (let ((xs (let ((r nil) (i 100)) (while (> i 0) (setq r (cons i r)) (setq i (- i 1))) r))) (length (sort xs #'<))))",
        .expr = "(bench-sort-fixnum)",
    },
    .{
        .name = "sort_string", .category = "sort",
        .setup_jit = "(defun bench-sort-string () " ++ decl_jit ++
            \\  (let ((xs (let ((r nil) (i 0)) (while (< i 100) (setq r (cons (format nil "~6,'0d" (- 100 i)) r)) (setq i (+ i 1))) r))) (length (sort xs #'string<))))
        ,
        .setup_interp = "(defun bench-sort-string ()" ++
            \\  (let ((xs (let ((r nil) (i 0)) (while (< i 100) (setq r (cons (format nil "~6,'0d" (- 100 i)) r)) (setq i (+ i 1))) r))) (length (sort xs #'string<))))
        ,
        .expr = "(bench-sort-string)",
    },

    // ── gc ──
    .{
        .name = "gc_cons", .category = "gc",
        .setup_jit = "(defun bench-gc-cons () " ++ decl_jit ++
            " (let ((i 0) (last nil)) (while (< i 100000) (setq last (cons i nil)) (setq i (+ i 1))) last))",
        .setup_interp = "(defun bench-gc-cons () (let ((i 0) (last nil)) (while (< i 100000) (setq last (cons i nil)) (setq i (+ i 1))) last))",
        .expr = "(bench-gc-cons)",
    },
    .{
        .name = "gc_vector", .category = "gc",
        .setup_jit = "(defun bench-gc-vector () " ++ decl_jit ++
            " (let ((v nil) (i 0)) (while (< i 10000) (setq v (make-array 4 :initial-element i)) (setq i (+ i 1))) (aref v 0)))",
        .setup_interp = "(defun bench-gc-vector () (let ((v nil) (i 0)) (while (< i 10000) (setq v (make-array 4 :initial-element i)) (setq i (+ i 1))) (aref v 0)))",
        .expr = "(bench-gc-vector)",
    },

    // ── symbol ──
    .{
        .name = "intern", .category = "symbol",
        .setup_jit = "(defun bench-intern () " ++ decl_jit ++
            \\  (let ((count 0) (i 0)) (while (< i 10000) (intern (format nil "BENCH-SYM-~d" i)) (setq count (+ count 1)) (setq i (+ i 1))) count))
        ,
        .setup_interp = "(defun bench-intern ()" ++
            \\  (let ((count 0) (i 0)) (while (< i 10000) (intern (format nil "BENCH-SYM-~d" i)) (setq count (+ count 1)) (setq i (+ i 1))) count))
        ,
        .expr = "(bench-intern)",
    },
};

pub fn main() !void {
    const opts = parseArgs();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var heap = try Heap.init(allocator, .{ .total_size = opts.heap_mb * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Load stdlib
    {
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        repl.loadFile("lib/stdlib.habu", &stderr_writer.interface) catch |err| {
            stderr_writer.interface.print("Could not load stdlib: {s}\n", .{@errorName(err)}) catch {};
            stderr_writer.interface.flush() catch {};
            return err;
        };
    }

    var timer = try std.time.Timer.start();
    const mode_str: []const u8 = if (opts.mode == .jit) "jit" else "interp";

    // Run benchmarks
    var benches: [bench_defs.len]Bench = undefined;
    for (bench_defs, 0..) |def, i| {
        {
            var buf: [4096]u8 = undefined;
            var w = std.fs.File.stderr().writer(&buf);
            w.interface.print("  {s:<24}", .{def.name}) catch {};
            w.interface.flush() catch {};
        }

        // Choose setup based on mode — evaluate each defun separately so JIT fires per form
        const setups: [3]?[]const u8 = .{
            if (opts.mode == .jit) def.setup_jit else def.setup_interp,
            if (opts.mode == .jit) def.setup_jit2 else null,
            null,
        };
        var setup_failed = false;
        for (setups) |maybe_s| {
            const s = maybe_s orelse continue;
            _ = repl.eval(s) catch |err| {
                benches[i] = .{ .name = def.name, .ns = 0, .err_name = @errorName(err), .category = def.category };
                {
                    var buf: [4096]u8 = undefined;
                    var w = std.fs.File.stderr().writer(&buf);
                    w.interface.print(" ERR(setup): {s}\n", .{@errorName(err)}) catch {};
                    w.interface.flush() catch {};
                }
                setup_failed = true;
                break;
            };
        }
        if (setup_failed) continue;

        // Warmup
        _ = repl.eval(def.expr) catch |err| {
            benches[i] = .{ .name = def.name, .ns = 0, .err_name = @errorName(err), .category = def.category };
            {
                var buf: [4096]u8 = undefined;
                var w = std.fs.File.stderr().writer(&buf);
                w.interface.print(" ERR(warmup): {s}\n", .{@errorName(err)}) catch {};
                w.interface.flush() catch {};
            }
            continue;
        };

        // Timed runs — take best of N
        var best_ns: u64 = std.math.maxInt(u64);
        var had_error: ?[]const u8 = null;
        for (0..opts.iters) |_| {
            const t0 = timer.read();
            _ = repl.eval(def.expr) catch |err| {
                had_error = @errorName(err);
                break;
            };
            const t1 = timer.read();
            const elapsed = t1 - t0;
            if (elapsed < best_ns) best_ns = elapsed;
        }

        if (had_error) |err| {
            benches[i] = .{ .name = def.name, .ns = 0, .err_name = err, .category = def.category };
            {
                var buf: [4096]u8 = undefined;
                var w = std.fs.File.stderr().writer(&buf);
                w.interface.print(" ERR: {s}\n", .{err}) catch {};
                w.interface.flush() catch {};
            }
        } else {
            benches[i] = .{ .name = def.name, .ns = best_ns, .category = def.category };
            {
                var buf: [4096]u8 = undefined;
                var w = std.fs.File.stderr().writer(&buf);
                const ms = @as(f64, @floatFromInt(best_ns)) / 1e6;
                w.interface.print(" {d:.3} ms\n", .{ms}) catch {};
                w.interface.flush() catch {};
            }
        }
    }

    // Output JSON
    var out_buf: [16384]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.print("{{\"engine\":\"habu\",\"mode\":\"{s}\",\"benches\":[", .{mode_str});
        for (benches, 0..) |b, i| {
            if (i != 0) try w.writeByte(',');
            if (b.err_name) |err| {
                try w.print("{{\"name\":\"{s}\",\"category\":\"{s}\",\"ns\":0,\"error\":\"{s}\"}}", .{ b.name, b.category, err });
            } else {
                try w.print("{{\"name\":\"{s}\",\"category\":\"{s}\",\"ns\":{d}}}", .{ b.name, b.category, b.ns });
            }
        }
        try w.writeAll("]}\n");
        try w.flush();
        return;
    }

    try w.print("\nComprehensive CL Benchmark — Habu ({s})\n", .{mode_str});
    try w.print("  heap: {d} MiB, iters: {d}\n\n", .{ opts.heap_mb, opts.iters });
    try w.print("{s:<24} {s:>12}\n", .{ "Benchmark", "Time" });
    try w.print("{s:<24} {s:>12}\n", .{ "-" ** 24, "-" ** 12 });
    for (benches) |b| {
        if (b.err_name) |err| {
            try w.print("{s:<24} {s:>12}\n", .{ b.name, err });
        } else {
            const ms = @as(f64, @floatFromInt(b.ns)) / 1e6;
            try w.print("{s:<24} {d:>10.3} ms\n", .{ b.name, ms });
        }
    }
    try w.flush();
}
