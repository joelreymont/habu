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
};

const Opts = struct {
    heap_mb: usize = 512,
    iters: usize = 3,
    json: bool = false,
};

fn parseArgs() Opts {
    var opts = Opts{};
    var it = std.process.args();
    _ = it.next();
    while (it.next()) |arg| {
        if (std.mem.eql(u8, arg, "--json")) {
            opts.json = true;
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
    setup: ?[]const u8 = null,
    expr: []const u8,
};

const bench_defs = [_]BenchDef{
    // ── arith ──
    .{ .name = "fixnum_loop", .expr = "(let ((i 0) (acc 0)) (while (< i 1000000) (setq acc (+ acc i)) (setq i (+ i 1))) acc)" },
    .{ .name = "fixnum_mul", .expr = "(let ((i 0) (acc 1)) (while (< i 1000000) (setq acc (logand (+ acc (* (+ i 1) 3)) #xffffff)) (setq i (+ i 1))) acc)" },
    .{ .name = "gcd", .expr = "(let ((i 0) (sum 0)) (while (< i 100000) (setq sum (+ sum (gcd (+ i 17) (+ i 31)))) (setq i (+ i 1))) sum)" },

    // ── float ──
    .{ .name = "float_sum", .expr = "(let ((i 0) (acc 0.0)) (while (< i 100000) (setq acc (+ acc (* (float i) 0.001))) (setq i (+ i 1))) (round acc))" },
    .{ .name = "float_sqrt", .expr = "(let ((i 0) (acc 0.0)) (while (< i 100000) (setq acc (+ acc (sqrt (+ 1.0 (float i))))) (setq i (+ i 1))) (round acc))" },

    // ── recurse ──
    .{
        .name = "fib30",
        .setup = "(defun fib (n) (if (<= n 1) n (+ (fib (- n 1)) (fib (- n 2)))))",
        .expr = "(fib 30)",
    },
    .{
        .name = "tak",
        .setup = "(defun tak (x y z) (if (<= x y) z (tak (tak (- x 1) y z) (tak (- y 1) z x) (tak (- z 1) x y))))",
        .expr = "(tak 18 12 6)",
    },
    .{
        .name = "ack",
        .setup = "(defun ack (m n) (cond ((= m 0) (+ n 1)) ((= n 0) (ack (- m 1) 1)) (t (ack (- m 1) (ack m (- n 1))))))",
        .expr = "(ack 3 5)",
    },
    .{
        .name = "nqueens10",
        .setup =
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
    .{ .name = "list_build", .expr = "(let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length xs))" },
    .{ .name = "list_reverse", .expr = "(let ((xs nil) (i 0)) (while (< i 100000) (setq xs (cons i xs)) (setq i (+ i 1))) (length (nreverse xs)))" },
    .{ .name = "list_append", .expr = "(let ((base (let ((xs nil) (i 0)) (while (< i 100) (setq xs (cons i xs)) (setq i (+ i 1))) xs)) (result nil) (i 0)) (while (< i 1000) (setq result (append base result)) (setq i (+ i 1))) (length result))" },
    .{ .name = "assoc", .expr = "(let ((al (let ((xs nil) (i 0)) (while (< i 100) (setq xs (cons (cons i (* i i)) xs)) (setq i (+ i 1))) xs)) (sum 0) (i 0)) (while (< i 50000) (let ((pair (assoc (mod i 100) al))) (when pair (setq sum (+ sum (cdr pair))))) (setq i (+ i 1))) sum)" },

    // ── hof ──
    .{ .name = "mapcar", .expr = "(let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r))) (length (mapcar (lambda (x) (+ x 1)) xs)))" },
    .{ .name = "reduce", .expr = "(let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r))) (reduce #'+ xs))" },
    .{ .name = "remove_if", .expr = "(let ((xs (let ((r nil) (i 0)) (while (< i 10000) (setq r (cons i r)) (setq i (+ i 1))) r)) (result nil) (rest nil)) (setq rest xs) (while rest (when (not (oddp (car rest))) (setq result (cons (car rest) result))) (setq rest (cdr rest))) (length result))" },

    // ── hash ──
    .{ .name = "hash_insert", .expr = "(let ((h (make-hash-table :size 256)) (i 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (hash-table-count h))" },
    .{ .name = "hash_lookup", .expr = "(let ((h (make-hash-table :size 256)) (i 0) (sum 0)) (while (< i 20000) (setf (gethash i h) i) (setq i (+ i 1))) (setq i 0) (while (< i 50000) (let ((v (gethash (mod i 20000) h))) (when v (setq sum (+ sum v)))) (setq i (+ i 1))) sum)" },

    // ── string ──
    .{ .name = "string_concat", .expr =
    \\(let ((result "") (i 0)) (while (< i 1000) (setq result (concatenate 'string result "x")) (setq i (+ i 1))) (length result))
    },
    .{ .name = "string_search", .expr =
    \\(let ((haystack (make-string 10000 :initial-element #\a)) (count 0) (i 0)) (setf (char haystack 9999) #\b) (while (< i 1000) (when (position #\b haystack) (setq count (+ count 1))) (setq i (+ i 1))) count)
    },

    // ── sort ──
    .{ .name = "sort_fixnum", .expr = "(let ((xs (let ((r nil) (i 100)) (while (> i 0) (setq r (cons i r)) (setq i (- i 1))) r))) (length (sort xs #'<)))" },
    .{ .name = "sort_string", .expr =
    \\(let ((xs (let ((r nil) (i 0)) (while (< i 100) (setq r (cons (format nil "~6,'0d" (- 100 i)) r)) (setq i (+ i 1))) r))) (length (sort xs #'string<)))
    },

    // ── gc ──
    .{ .name = "gc_cons", .expr = "(let ((i 0) (last nil)) (while (< i 100000) (setq last (cons i nil)) (setq i (+ i 1))) last)" },
    .{ .name = "gc_vector", .expr = "(let ((v nil) (i 0)) (while (< i 10000) (setq v (make-array 4 :initial-element i)) (setq i (+ i 1))) (aref v 0))" },

    // ── symbol ──
    .{ .name = "intern", .expr =
    \\(let ((count 0) (i 0)) (while (< i 10000) (intern (format nil "BENCH-SYM-~d" i)) (setq count (+ count 1)) (setq i (+ i 1))) count)
    },
};

pub fn main() !void {
    const opts = parseArgs();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var heap = try Heap.init(allocator, .{ .total_size = opts.heap_mb * 1024 * 1024 });
    defer heap.deinit();

    // Use Repl to get full CL environment with stdlib
    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    // Load stdlib
    {
        var stderr_buf: [4096]u8 = undefined;
        var stderr_writer = std.fs.File.stderr().writer(&stderr_buf);
        repl.loadFilePublic("lib/stdlib.habu", &stderr_writer.interface) catch |err| {
            stderr_writer.interface.print("Could not load stdlib: {s}\n", .{@errorName(err)}) catch {};
            stderr_writer.interface.flush() catch {};
            return err;
        };
        stderr_writer.interface.print("Stdlib loaded\n", .{}) catch {};
        stderr_writer.interface.flush() catch {};
    }

    // NOTE: JIT only applies to functions compiled with special annotations.
    // The interpreter runs all benchmarks; JIT is not enabled here because
    // the Repl-based eval path compiles-and-runs in one shot.

    var timer = try std.time.Timer.start();

    // Run benchmarks
    var benches: [bench_defs.len]Bench = undefined;
    for (bench_defs, 0..) |def, i| {
        {
            var buf: [4096]u8 = undefined;
            var w = std.fs.File.stderr().writer(&buf);
            w.interface.print("Running {s}...\n", .{def.name}) catch {};
            w.interface.flush() catch {};
        }

        // Run setup if needed
        if (def.setup) |setup| {
            _ = repl.eval(setup) catch |err| {
                benches[i] = .{ .name = def.name, .ns = 0, .err_name = @errorName(err) };
                continue;
            };
        }

        // Warmup
        _ = repl.eval(def.expr) catch |err| {
            benches[i] = .{ .name = def.name, .ns = 0, .err_name = @errorName(err) };
            continue;
        };

        // Timed runs
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
            benches[i] = .{ .name = def.name, .ns = 0, .err_name = err };
        } else {
            benches[i] = .{ .name = def.name, .ns = best_ns };
        }
    }

    // Output
    var out_buf: [8192]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.writeAll("{\"engine\":\"habu\",\"mode\":\"interp\",\"benches\":[");
        for (benches, 0..) |b, i| {
            if (i != 0) try w.writeByte(',');
            if (b.err_name) |err| {
                try w.print("{{\"name\":\"{s}\",\"ns\":0,\"error\":\"{s}\"}}", .{ b.name, err });
            } else {
                try w.print("{{\"name\":\"{s}\",\"ns\":{d}}}", .{ b.name, b.ns });
            }
        }
        try w.writeAll("]}\n");
        try w.flush();
        return;
    }

    try w.print("Comprehensive CL Benchmark (Habu, interp)\n", .{});
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
