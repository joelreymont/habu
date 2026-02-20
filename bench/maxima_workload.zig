const std = @import("std");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;

const Heap = runtime.Heap;
const Repl = interp.Repl;
const Value = runtime.Value;
const Cons = runtime.Cons;

const Opts = struct {
    heap_mb: usize = 1024,
    scale: usize = 1,
    json: bool = false,
};

const LoaderStats = struct {
    ok: i64 = 0,
    total: i64 = 0,
    fail: i64 = 0,
    attempted: i64 = 0,
    missing: i64 = 0,
    ns: u64 = 0,
};

const BenchDef = struct {
    name: []const u8,
    category: []const u8,
    setup: []const u8,
    call_name: []const u8,
    iters: usize,
};

const Bench = struct {
    name: []const u8,
    category: []const u8,
    iters: usize,
    ns: u64,
    err_name: ?[]const u8 = null,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\Maxima workload benchmark (Habu)
        \\
        \\Usage:
        \\  zig build -Duse-hoist=true bench-maxima -- [--heap-mb N] [--scale N] [--json]
        \\
    );
}

fn parseArgs() !Opts {
    var opts = Opts{};
    var it = std.process.args();
    _ = it.next();
    while (it.next()) |arg| {
        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            var buf: [4096]u8 = undefined;
            var out = std.fs.File.stdout().writer(&buf);
            try usage(&out.interface);
            try out.interface.flush();
            return error.InvalidArgs;
        }
        if (std.mem.eql(u8, arg, "--json")) {
            opts.json = true;
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--heap-mb=")) {
            opts.heap_mb = try std.fmt.parseInt(usize, arg["--heap-mb=".len..], 10);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--scale=")) {
            opts.scale = try std.fmt.parseInt(usize, arg["--scale=".len..], 10);
            continue;
        }
        return error.InvalidArgs;
    }

    if (opts.heap_mb == 0) return error.InvalidArgs;
    if (opts.scale == 0) return error.InvalidArgs;
    return opts;
}

fn listNthFixnum(list: Value, idx: usize) !i64 {
    var cur = list;
    var i: usize = 0;
    while (i < idx) : (i += 1) {
        if (!cur.isCons()) return error.TypeMismatch;
        cur = cur.toPtr(Cons).cdr;
    }

    if (!cur.isCons()) return error.TypeMismatch;
    const cell = cur.toPtr(Cons);
    if (!cell.car.isFixnum()) return error.TypeMismatch;
    return cell.car.toFixnum();
}

const maxima_files_expr =
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
;

fn loadMaxima(timer: *std.time.Timer, repl: *Repl) !LoaderStats {
    var stats = LoaderStats{};

    try repl.loadFilePublic("lib/stdlib.habu", std.io.null_writer);
    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");
    _ = try repl.eval(maxima_files_expr);

    const t0 = timer.read();
    const loaded = try repl.eval(
        \\(multiple-value-bind (ok total fail missing attempted)
        \\    (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (list ok total fail attempted (if missing (length missing) 0)))
    );
    const t1 = timer.read();

    stats.ok = try listNthFixnum(loaded, 0);
    stats.total = try listNthFixnum(loaded, 1);
    stats.fail = try listNthFixnum(loaded, 2);
    stats.attempted = try listNthFixnum(loaded, 3);
    stats.missing = try listNthFixnum(loaded, 4);
    stats.ns = t1 - t0;
    return stats;
}

fn runBench(allocator: std.mem.Allocator, timer: *std.time.Timer, repl: *Repl, def: BenchDef, scale: usize) Bench {
    _ = repl.eval(def.setup) catch |err| {
        return .{
            .name = def.name,
            .category = def.category,
            .iters = def.iters * scale,
            .ns = 0,
            .err_name = @errorName(err),
        };
    };

    const n = def.iters * scale;
    const call_src = std.fmt.allocPrint(allocator, "({s} {d})", .{ def.call_name, n }) catch |err| {
        return .{
            .name = def.name,
            .category = def.category,
            .iters = n,
            .ns = 0,
            .err_name = @errorName(err),
        };
    };
    defer allocator.free(call_src);

    _ = repl.eval(call_src) catch |err| {
        return .{
            .name = def.name,
            .category = def.category,
            .iters = n,
            .ns = 0,
            .err_name = @errorName(err),
        };
    };

    const t0 = timer.read();
    _ = repl.eval(call_src) catch |err| {
        return .{
            .name = def.name,
            .category = def.category,
            .iters = n,
            .ns = 0,
            .err_name = @errorName(err),
        };
    };
    const t1 = timer.read();

    return .{
        .name = def.name,
        .category = def.category,
        .iters = n,
        .ns = t1 - t0,
        .err_name = null,
    };
}

const bench_defs = [_]BenchDef{
    .{
        .name = "simplifya",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-simplifya (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::simplifya '((maxima::mplus) 3 4) t))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-simplifya",
        .iters = 200,
    },
    .{
        .name = "diff",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-diff (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$diff 0 'maxima::$x))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-diff",
        .iters = 200,
    },
    .{
        .name = "integrate",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-integrate (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$integrate 0 'maxima::$x))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-integrate",
        .iters = 200,
    },
    .{
        .name = "factor",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-factor (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$factor 1))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-factor",
        .iters = 200,
    },
    .{
        .name = "ratsimp",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-ratsimp (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$ratsimp 1))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-ratsimp",
        .iters = 200,
    },
    .{
        .name = "limit",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-limit (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$limit 0 'maxima::$x 0))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-limit",
        .iters = 20,
    },
    .{
        .name = "solve",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-solve (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$solve 0 'maxima::$x))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-solve",
        .iters = 20,
    },
    .{
        .name = "determinant",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-determinant (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$determinant 1))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-determinant",
        .iters = 20,
    },
    .{
        .name = "expand",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-expand (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$expand 1))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-expand",
        .iters = 200,
    },
    .{
        .name = "sin",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-sin (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$sin 0))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-sin",
        .iters = 200,
    },
    .{
        .name = "cos",
        .category = "maxima",
        .setup =
        \\(defun bench-maxima-cos (n)
        \\  (let ((i 0) (out nil))
        \\    (while (< i n)
        \\      (setq out (maxima::$cos 0))
        \\      (setq i (+ i 1)))
        \\    out))
        ,
        .call_name = "bench-maxima-cos",
        .iters = 200,
    },
};

pub fn main() !void {
    const opts = parseArgs() catch |err| switch (err) {
        error.InvalidArgs => return,
        else => return err,
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    var heap = try Heap.init(allocator, .{ .total_size = opts.heap_mb * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var timer = try std.time.Timer.start();

    const loader = loadMaxima(&timer, &repl) catch |err| {
        var out_buf_err: [2048]u8 = undefined;
        var out_err = std.fs.File.stdout().writer(&out_buf_err);
        const w_err = &out_err.interface;

        if (opts.json) {
            try w_err.print(
                "{{\"engine\":\"habu\",\"workload\":\"maxima\",\"error\":\"{s}\"}}\n",
                .{@errorName(err)},
            );
        } else {
            try w_err.print("maxima workload error: {s}\n", .{@errorName(err)});
        }
        try w_err.flush();
        return;
    };

    var benches = std.ArrayList(Bench){};
    defer benches.deinit(allocator);

    for (bench_defs) |def| {
        try benches.append(allocator, runBench(allocator, &timer, &repl, def, opts.scale));
    }

    var out_buf: [16384]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.print(
            "{{\"engine\":\"habu\",\"workload\":\"maxima\",\"heap_mb\":{d},\"scale\":{d},\"loader\":{{\"ok\":{d},\"total\":{d},\"fail\":{d},\"attempted\":{d},\"missing\":{d},\"ns\":{d}}},\"benches\":[",
            .{ opts.heap_mb, opts.scale, loader.ok, loader.total, loader.fail, loader.attempted, loader.missing, loader.ns },
        );
        for (benches.items, 0..) |b, i| {
            if (i != 0) try w.writeByte(',');
            if (b.err_name) |err_name| {
                try w.print(
                    "{{\"name\":\"{s}\",\"category\":\"{s}\",\"iters\":{d},\"ns\":0,\"error\":\"{s}\"}}",
                    .{ b.name, b.category, b.iters, err_name },
                );
            } else {
                try w.print(
                    "{{\"name\":\"{s}\",\"category\":\"{s}\",\"iters\":{d},\"ns\":{d}}}",
                    .{ b.name, b.category, b.iters, b.ns },
                );
            }
        }
        try w.writeAll("]}\n");
        try w.flush();
        return;
    }

    try w.print("Maxima workload benchmark (Habu)\n", .{});
    try w.print("  heap: {d} MiB, scale: {d}\n", .{ opts.heap_mb, opts.scale });
    try w.print(
        "  loader: ok={d}/{d}, fail={d}, attempted={d}, missing={d}, {d:.3} ms\n",
        .{ loader.ok, loader.total, loader.fail, loader.attempted, loader.missing, @as(f64, @floatFromInt(loader.ns)) / 1e6 },
    );
    for (benches.items) |b| {
        if (b.err_name) |err_name| {
            try w.print("  {s}: ERR({s})\n", .{ b.name, err_name });
        } else {
            try w.print(
                "  {s}: {d:.3} ms total ({d} iters)\n",
                .{ b.name, @as(f64, @floatFromInt(b.ns)) / 1e6, b.iters },
            );
        }
    }
    try w.flush();
}
