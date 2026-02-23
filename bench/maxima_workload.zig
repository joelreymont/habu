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
    nursery_mb: usize = 32,
    scale: usize = 1,
    json: bool = false,
    workloads_csv: ?[]const u8 = null,
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

const JsonBench = struct {
    name: []const u8,
    category: []const u8,
    iters: usize,
    ns: u64,
    @"error": ?[]const u8 = null,
};

const GcSnap = struct {
    gc_count: usize,
    gc_minor_count: usize,
    gc_major_count: usize,
    bytes_copied: usize,
    promoted_bytes: usize,
    gc_minor_ns: u64,
    gc_major_ns: u64,
    gc_nursery_target: usize,
    gc_nursery_scale: f64,
    gc_nursery_survival: f64,
    gc_nursery_pause_error: f64,
    gc_los_threshold: usize,
    gc_los_threshold_min: usize,
    gc_los_threshold_max: usize,
    gc_los_scale: f64,
    gc_los_large_ratio: f64,
    gc_los_occupancy: f64,
    gc_los_pause_error: f64,
    los_bytes: usize,
    los_live: usize,
    wb_calls: usize,
    wb_ns: u64,
    wb_jit_calls: usize,
    wb_jit_ns: u64,
    safepoint_vm_calls: usize,
    safepoint_vm_ns: u64,
    safepoint_jit_calls: usize,
    safepoint_jit_ns: u64,
    gc_debt_bytes: usize,
    gc_debt_threshold: usize,
    gc_debt_alloc_bytes: usize,
    gc_debt_paydown_bytes: usize,
    gc_debt_trigger_n: usize,
    gc_debt_skip_n: usize,
    gc_debt_score: f64,
    gc_debt_ratio: f64,
    gc_debt_occupancy: f64,
    gc_debt_survival: f64,
    gc_debt_pause_error: f64,
};

const GcDelta = struct {
    gc_count: usize,
    gc_minor_count: usize,
    gc_major_count: usize,
    bytes_copied: usize,
    promoted_bytes: usize,
    avg_minor_ns: u64,
    avg_major_ns: u64,
    gc_nursery_target: usize,
    gc_nursery_scale: f64,
    gc_nursery_survival: f64,
    gc_nursery_pause_error: f64,
    gc_los_threshold: usize,
    gc_los_threshold_min: usize,
    gc_los_threshold_max: usize,
    gc_los_scale: f64,
    gc_los_large_ratio: f64,
    gc_los_occupancy: f64,
    gc_los_pause_error: f64,
    los_bytes: usize,
    los_live: usize,
    wb_calls: usize,
    wb_ns: u64,
    wb_jit_calls: usize,
    wb_jit_ns: u64,
    safepoint_vm_calls: usize,
    safepoint_vm_ns: u64,
    safepoint_jit_calls: usize,
    safepoint_jit_ns: u64,
    gc_debt_bytes: usize,
    gc_debt_threshold: usize,
    gc_debt_alloc_bytes: usize,
    gc_debt_paydown_bytes: usize,
    gc_debt_trigger_n: usize,
    gc_debt_skip_n: usize,
    gc_debt_score: f64,
    gc_debt_ratio: f64,
    gc_debt_occupancy: f64,
    gc_debt_survival: f64,
    gc_debt_pause_error: f64,
};

fn counterDelta(comptime T: type, after: T, before: T) T {
    return after -% before;
}

fn gcSnap(heap: *const Heap) GcSnap {
    return .{
        .gc_count = heap.stats.gc_count,
        .gc_minor_count = heap.stats.gc_minor_count,
        .gc_major_count = heap.stats.gc_major_count,
        .bytes_copied = heap.stats.bytes_copied,
        .promoted_bytes = heap.stats.gc_promoted_bytes,
        .gc_minor_ns = heap.stats.gc_minor_ns,
        .gc_major_ns = heap.stats.gc_major_ns,
        .gc_nursery_target = heap.stats.gc_nursery_target,
        .gc_nursery_scale = heap.stats.gc_nursery_scale,
        .gc_nursery_survival = heap.stats.gc_nursery_survival,
        .gc_nursery_pause_error = heap.stats.gc_nursery_pause_error,
        .gc_los_threshold = heap.stats.gc_los_threshold,
        .gc_los_threshold_min = heap.stats.gc_los_threshold_min,
        .gc_los_threshold_max = heap.stats.gc_los_threshold_max,
        .gc_los_scale = heap.stats.gc_los_scale,
        .gc_los_large_ratio = heap.stats.gc_los_large_ratio,
        .gc_los_occupancy = heap.stats.gc_los_occupancy,
        .gc_los_pause_error = heap.stats.gc_los_pause_error,
        .los_bytes = heap.losBytesUsed(),
        .los_live = heap.los_objs.items.len,
        .wb_calls = heap.stats.wb_calls,
        .wb_ns = heap.stats.wb_ns,
        .wb_jit_calls = heap.stats.wb_jit_calls,
        .wb_jit_ns = heap.stats.wb_jit_ns,
        .safepoint_vm_calls = heap.stats.safepoint_vm_calls,
        .safepoint_vm_ns = heap.stats.safepoint_vm_ns,
        .safepoint_jit_calls = heap.stats.safepoint_jit_calls,
        .safepoint_jit_ns = heap.stats.safepoint_jit_ns,
        .gc_debt_bytes = heap.stats.gc_debt_bytes,
        .gc_debt_threshold = heap.stats.gc_debt_threshold,
        .gc_debt_alloc_bytes = heap.stats.gc_debt_alloc_bytes,
        .gc_debt_paydown_bytes = heap.stats.gc_debt_paydown_bytes,
        .gc_debt_trigger_n = heap.stats.gc_debt_trigger_n,
        .gc_debt_skip_n = heap.stats.gc_debt_skip_n,
        .gc_debt_score = heap.stats.gc_debt_score,
        .gc_debt_ratio = heap.stats.gc_debt_ratio,
        .gc_debt_occupancy = heap.stats.gc_debt_occupancy,
        .gc_debt_survival = heap.stats.gc_debt_survival,
        .gc_debt_pause_error = heap.stats.gc_debt_pause_error,
    };
}

fn gcDelta(before: GcSnap, after: GcSnap) GcDelta {
    const gc_n = counterDelta(usize, after.gc_count, before.gc_count);
    const minor_n = counterDelta(usize, after.gc_minor_count, before.gc_minor_count);
    const major_n = counterDelta(usize, after.gc_major_count, before.gc_major_count);
    const minor_ns = counterDelta(u64, after.gc_minor_ns, before.gc_minor_ns);
    const major_ns = counterDelta(u64, after.gc_major_ns, before.gc_major_ns);
    const minor_n_u64: u64 = @intCast(minor_n);
    const major_n_u64: u64 = @intCast(major_n);
    return .{
        .gc_count = gc_n,
        .gc_minor_count = minor_n,
        .gc_major_count = major_n,
        .bytes_copied = counterDelta(usize, after.bytes_copied, before.bytes_copied),
        .promoted_bytes = counterDelta(usize, after.promoted_bytes, before.promoted_bytes),
        .avg_minor_ns = if (minor_n_u64 == 0) 0 else minor_ns / minor_n_u64,
        .avg_major_ns = if (major_n_u64 == 0) 0 else major_ns / major_n_u64,
        .gc_nursery_target = after.gc_nursery_target,
        .gc_nursery_scale = after.gc_nursery_scale,
        .gc_nursery_survival = after.gc_nursery_survival,
        .gc_nursery_pause_error = after.gc_nursery_pause_error,
        .gc_los_threshold = after.gc_los_threshold,
        .gc_los_threshold_min = after.gc_los_threshold_min,
        .gc_los_threshold_max = after.gc_los_threshold_max,
        .gc_los_scale = after.gc_los_scale,
        .gc_los_large_ratio = after.gc_los_large_ratio,
        .gc_los_occupancy = after.gc_los_occupancy,
        .gc_los_pause_error = after.gc_los_pause_error,
        .los_bytes = after.los_bytes,
        .los_live = after.los_live,
        .wb_calls = counterDelta(usize, after.wb_calls, before.wb_calls),
        .wb_ns = counterDelta(u64, after.wb_ns, before.wb_ns),
        .wb_jit_calls = counterDelta(usize, after.wb_jit_calls, before.wb_jit_calls),
        .wb_jit_ns = counterDelta(u64, after.wb_jit_ns, before.wb_jit_ns),
        .safepoint_vm_calls = counterDelta(usize, after.safepoint_vm_calls, before.safepoint_vm_calls),
        .safepoint_vm_ns = counterDelta(u64, after.safepoint_vm_ns, before.safepoint_vm_ns),
        .safepoint_jit_calls = counterDelta(usize, after.safepoint_jit_calls, before.safepoint_jit_calls),
        .safepoint_jit_ns = counterDelta(u64, after.safepoint_jit_ns, before.safepoint_jit_ns),
        .gc_debt_bytes = after.gc_debt_bytes,
        .gc_debt_threshold = after.gc_debt_threshold,
        .gc_debt_alloc_bytes = counterDelta(usize, after.gc_debt_alloc_bytes, before.gc_debt_alloc_bytes),
        .gc_debt_paydown_bytes = counterDelta(usize, after.gc_debt_paydown_bytes, before.gc_debt_paydown_bytes),
        .gc_debt_trigger_n = counterDelta(usize, after.gc_debt_trigger_n, before.gc_debt_trigger_n),
        .gc_debt_skip_n = counterDelta(usize, after.gc_debt_skip_n, before.gc_debt_skip_n),
        .gc_debt_score = after.gc_debt_score,
        .gc_debt_ratio = after.gc_debt_ratio,
        .gc_debt_occupancy = after.gc_debt_occupancy,
        .gc_debt_survival = after.gc_debt_survival,
        .gc_debt_pause_error = after.gc_debt_pause_error,
    };
}

fn usage(w: anytype) !void {
    try w.writeAll(
        \\Maxima workload benchmark (Habu)
        \\
        \\Usage:
        \\  zig build -Duse-hoist=true bench-maxima -- [--heap-mb N] [--nursery-mb N] [--scale N] [--workloads a,b,c] [--json]
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
        if (std.mem.startsWith(u8, arg, "--nursery-mb=")) {
            opts.nursery_mb = try std.fmt.parseInt(usize, arg["--nursery-mb=".len..], 10);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--scale=")) {
            opts.scale = try std.fmt.parseInt(usize, arg["--scale=".len..], 10);
            continue;
        }
        if (std.mem.startsWith(u8, arg, "--workloads=")) {
            opts.workloads_csv = arg["--workloads=".len..];
            continue;
        }
        return error.InvalidArgs;
    }

    if (opts.heap_mb == 0) return error.InvalidArgs;
    if (opts.nursery_mb == 0) return error.InvalidArgs;
    if (opts.scale == 0) return error.InvalidArgs;
    return opts;
}

fn workloadSelected(workloads_csv: ?[]const u8, name: []const u8) bool {
    const csv = workloads_csv orelse return true;
    var it = std.mem.splitScalar(u8, csv, ',');
    while (it.next()) |raw_item| {
        const item = std.mem.trim(u8, raw_item, &std.ascii.whitespace);
        if (item.len == 0) continue;
        if (std.mem.eql(u8, item, name)) return true;
    }
    return false;
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

    try repl.loadFile("lib/stdlib.habu", std.io.null_writer);
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
    const trace_workload = std.posix.getenv("HABU_TRACE_MAXIMA_WORKLOAD") != null;
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

    // Stabilize per-workload timing by draining pending nursery debt after
    // warmup so cross-workload GC carryover does not land inside timed runs.
    _ = repl.vm.collectGarbage() catch |err| {
        return .{
            .name = def.name,
            .category = def.category,
            .iters = n,
            .ns = 0,
            .err_name = @errorName(err),
        };
    };

    if (trace_workload) {
        std.debug.print("MAXIMA_BENCH start {s} n={d}\n", .{ def.name, n });
    }
    const gc_before = repl.vm.heap.stats.gc_count;
    const gc_minor_before = repl.vm.heap.stats.gc_minor_count;
    const gc_major_before = repl.vm.heap.stats.gc_major_count;
    const t0 = timer.read();
    _ = repl.eval(call_src) catch |err| {
        if (trace_workload) {
            const gc_after_fail = repl.vm.heap.stats.gc_count;
            const gc_minor_after_fail = repl.vm.heap.stats.gc_minor_count;
            const gc_major_after_fail = repl.vm.heap.stats.gc_major_count;
            std.debug.print(
                "MAXIMA_BENCH fail {s} err={s} gc={d} minor={d} major={d}\n",
                .{
                    def.name,
                    @errorName(err),
                    gc_after_fail -% gc_before,
                    gc_minor_after_fail -% gc_minor_before,
                    gc_major_after_fail -% gc_major_before,
                },
            );
        }
        return .{
            .name = def.name,
            .category = def.category,
            .iters = n,
            .ns = 0,
            .err_name = @errorName(err),
        };
    };
    const t1 = timer.read();
    if (trace_workload) {
        const gc_after = repl.vm.heap.stats.gc_count;
        const gc_minor_after = repl.vm.heap.stats.gc_minor_count;
        const gc_major_after = repl.vm.heap.stats.gc_major_count;
        std.debug.print(
            "MAXIMA_BENCH end {s} ns={d} gc={d} minor={d} major={d}\n",
            .{
                def.name,
                t1 - t0,
                gc_after -% gc_before,
                gc_minor_after -% gc_minor_before,
                gc_major_after -% gc_major_before,
            },
        );
    }

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

    const heap_bytes = opts.heap_mb * 1024 * 1024;
    const nursery_bytes = opts.nursery_mb * 1024 * 1024;
    if (nursery_bytes > heap_bytes / 3) return error.InvalidArgs;
    var heap = try Heap.init(allocator, .{
        .total_size = heap_bytes,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = nursery_bytes,
            .los_size = nursery_bytes,
            .los_threshold = 32 * 1024,
            .promote_threshold = 1024,
        },
    });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    repl.vm.resetJitAdm();

    var timer = try std.time.Timer.start();

    const gc_start = gcSnap(&heap);
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
    const gc_after_load = gcSnap(&heap);

    var benches = std.ArrayList(Bench){};
    defer benches.deinit(allocator);

    for (bench_defs) |def| {
        if (!workloadSelected(opts.workloads_csv, def.name)) continue;
        try benches.append(allocator, runBench(allocator, &timer, &repl, def, opts.scale));
    }
    const jit_compiled = repl.vm.jit_fns.count();
    const jit_adm = repl.vm.jit_adm;
    const gc_after_run = gcSnap(&heap);
    const gc_load = gcDelta(gc_start, gc_after_load);
    const gc_run = gcDelta(gc_after_load, gc_after_run);

    var out_buf: [16384]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        var json_benches = std.ArrayList(JsonBench){};
        defer json_benches.deinit(allocator);
        try json_benches.ensureTotalCapacity(allocator, benches.items.len);
        for (benches.items) |b| {
            json_benches.appendAssumeCapacity(.{
                .name = b.name,
                .category = b.category,
                .iters = b.iters,
                .ns = b.ns,
                .@"error" = b.err_name,
            });
        }
        const payload = .{
            .engine = "habu",
            .workload = "maxima",
            .heap_mb = opts.heap_mb,
            .nursery_mb = opts.nursery_mb,
            .scale = opts.scale,
            .jit_compiled = jit_compiled,
            .jit_adm = jit_adm,
            .loader = .{
                .ok = loader.ok,
                .total = loader.total,
                .fail = loader.fail,
                .attempted = loader.attempted,
                .missing = loader.missing,
                .ns = loader.ns,
            },
            .gc = .{
                .load = gc_load,
                .run = gc_run,
            },
            .benches = json_benches.items,
        };
        try std.json.Stringify.value(payload, .{}, w);
        try w.writeByte('\n');
        try w.flush();
        return;
    }

    try w.print("Maxima workload benchmark (Habu)\n", .{});
    try w.print("  heap: {d} MiB, nursery: {d} MiB, scale: {d}\n", .{ opts.heap_mb, opts.nursery_mb, opts.scale });
    try w.print("  jit_compiled: {d}\n", .{jit_compiled});
    try w.print(
        "  jit_adm: cand={d} elig={d} comp={d} sk(speed={d},safety={d},assert={d},caps={d},opt={d},key={d},rest={d},chunk={d}) fail(unsupported={d},other={d})\n",
        .{
            jit_adm.cand,
            jit_adm.elig,
            jit_adm.comp,
            jit_adm.sk_speed,
            jit_adm.sk_safety,
            jit_adm.sk_assert,
            jit_adm.sk_caps,
            jit_adm.sk_opt,
            jit_adm.sk_key,
            jit_adm.sk_rest,
            jit_adm.sk_chunk,
            jit_adm.fail_unsupported,
            jit_adm.fail_other,
        },
    );
    try w.print(
        "  loader: ok={d}/{d}, fail={d}, attempted={d}, missing={d}, {d:.3} ms\n",
        .{ loader.ok, loader.total, loader.fail, loader.attempted, loader.missing, @as(f64, @floatFromInt(loader.ns)) / 1e6 },
    );
    try w.print(
        "  gc(load): n={d} minor={d} major={d} copied={d} promoted={d} avg_minor={d:.3}ms avg_major={d:.3}ms\n",
        .{
            gc_load.gc_count,
            gc_load.gc_minor_count,
            gc_load.gc_major_count,
            gc_load.bytes_copied,
            gc_load.promoted_bytes,
            @as(f64, @floatFromInt(gc_load.avg_minor_ns)) / 1e6,
            @as(f64, @floatFromInt(gc_load.avg_major_ns)) / 1e6,
        },
    );
    try w.print(
        "  gc(run): n={d} minor={d} major={d} copied={d} promoted={d} avg_minor={d:.3}ms avg_major={d:.3}ms nursery={d} scale={d:.3} surv={d:.3} pause_err={d:.3}\n",
        .{
            gc_run.gc_count,
            gc_run.gc_minor_count,
            gc_run.gc_major_count,
            gc_run.bytes_copied,
            gc_run.promoted_bytes,
            @as(f64, @floatFromInt(gc_run.avg_minor_ns)) / 1e6,
            @as(f64, @floatFromInt(gc_run.avg_major_ns)) / 1e6,
            gc_run.gc_nursery_target,
            gc_run.gc_nursery_scale,
            gc_run.gc_nursery_survival,
            gc_run.gc_nursery_pause_error,
        },
    );
    try w.print(
        "  gc los(load): bytes={d} live={d} threshold={d} [{d},{d}] scale={d:.3} large={d:.3} occ={d:.3} pause_err={d:.3}\n",
        .{
            gc_load.los_bytes,
            gc_load.los_live,
            gc_load.gc_los_threshold,
            gc_load.gc_los_threshold_min,
            gc_load.gc_los_threshold_max,
            gc_load.gc_los_scale,
            gc_load.gc_los_large_ratio,
            gc_load.gc_los_occupancy,
            gc_load.gc_los_pause_error,
        },
    );
    try w.print(
        "  gc los(run): bytes={d} live={d} threshold={d} [{d},{d}] scale={d:.3} large={d:.3} occ={d:.3} pause_err={d:.3}\n",
        .{
            gc_run.los_bytes,
            gc_run.los_live,
            gc_run.gc_los_threshold,
            gc_run.gc_los_threshold_min,
            gc_run.gc_los_threshold_max,
            gc_run.gc_los_scale,
            gc_run.gc_los_large_ratio,
            gc_run.gc_los_occupancy,
            gc_run.gc_los_pause_error,
        },
    );
    try w.print(
        "  gc mutator(load): wb={d} ({d:.3}ms) jit_wb={d} ({d:.3}ms) sp_vm={d} ({d:.3}ms) sp_jit={d} ({d:.3}ms)\n",
        .{
            gc_load.wb_calls,
            @as(f64, @floatFromInt(gc_load.wb_ns)) / 1e6,
            gc_load.wb_jit_calls,
            @as(f64, @floatFromInt(gc_load.wb_jit_ns)) / 1e6,
            gc_load.safepoint_vm_calls,
            @as(f64, @floatFromInt(gc_load.safepoint_vm_ns)) / 1e6,
            gc_load.safepoint_jit_calls,
            @as(f64, @floatFromInt(gc_load.safepoint_jit_ns)) / 1e6,
        },
    );
    try w.print(
        "  gc mutator(run): wb={d} ({d:.3}ms) jit_wb={d} ({d:.3}ms) sp_vm={d} ({d:.3}ms) sp_jit={d} ({d:.3}ms)\n",
        .{
            gc_run.wb_calls,
            @as(f64, @floatFromInt(gc_run.wb_ns)) / 1e6,
            gc_run.wb_jit_calls,
            @as(f64, @floatFromInt(gc_run.wb_jit_ns)) / 1e6,
            gc_run.safepoint_vm_calls,
            @as(f64, @floatFromInt(gc_run.safepoint_vm_ns)) / 1e6,
            gc_run.safepoint_jit_calls,
            @as(f64, @floatFromInt(gc_run.safepoint_jit_ns)) / 1e6,
        },
    );
    try w.print(
        "  gc debt(load): bytes={d}/{d} alloc={d} paydown={d} triggers={d} skips={d} score={d:.3} ratio={d:.3} occ={d:.3} surv={d:.3} pause_err={d:.3}\n",
        .{
            gc_load.gc_debt_bytes,
            gc_load.gc_debt_threshold,
            gc_load.gc_debt_alloc_bytes,
            gc_load.gc_debt_paydown_bytes,
            gc_load.gc_debt_trigger_n,
            gc_load.gc_debt_skip_n,
            gc_load.gc_debt_score,
            gc_load.gc_debt_ratio,
            gc_load.gc_debt_occupancy,
            gc_load.gc_debt_survival,
            gc_load.gc_debt_pause_error,
        },
    );
    try w.print(
        "  gc debt(run): bytes={d}/{d} alloc={d} paydown={d} triggers={d} skips={d} score={d:.3} ratio={d:.3} occ={d:.3} surv={d:.3} pause_err={d:.3}\n",
        .{
            gc_run.gc_debt_bytes,
            gc_run.gc_debt_threshold,
            gc_run.gc_debt_alloc_bytes,
            gc_run.gc_debt_paydown_bytes,
            gc_run.gc_debt_trigger_n,
            gc_run.gc_debt_skip_n,
            gc_run.gc_debt_score,
            gc_run.gc_debt_ratio,
            gc_run.gc_debt_occupancy,
            gc_run.gc_debt_survival,
            gc_run.gc_debt_pause_error,
        },
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
