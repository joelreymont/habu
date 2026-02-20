const std = @import("std");
const build_options = @import("build_options");

const Allocator = std.mem.Allocator;

const GcJson = struct {
    iters: u64,
    heap_bytes: u64,
    live_bytes: u64,
    avg_pause_ns: u64,
    p50_pause_ns: u64 = 0,
    p95_pause_ns: u64,
    p99_pause_ns: u64 = 0,
    gc_count: u64,
    gc_minor_count: u64 = 0,
    gc_major_count: u64 = 0,
    bytes_copied: u64,
    avg_minor_ns: u64 = 0,
    avg_major_ns: u64 = 0,
    avg_build_ns: u64 = 0,
    avg_root_ns: u64 = 0,
    avg_copy_ns: u64 = 0,
    avg_finalize_ns: u64 = 0,
    root_vals: u64 = 0,
    gc_remembered_scanned: u64 = 0,
    gc_remembered_runs: u64 = 0,
    gc_remembered_marked_cards: u64 = 0,
    promoted_bytes: u64 = 0,
    wb_marks: u64 = 0,
    tenured_live: u64 = 0,
    los_live: u64 = 0,
    tenured_bytes: u64 = 0,
    los_bytes: u64 = 0,
    alloc_sample_n: u64 = 0,
    alloc_sample_bytes: u64 = 0,
    alloc_sample_cons: u64 = 0,
    alloc_sample_symbol: u64 = 0,
    alloc_sample_keyword: u64 = 0,
    alloc_sample_vector: u64 = 0,
    alloc_sample_array: u64 = 0,
    alloc_sample_string: u64 = 0,
    alloc_sample_closure: u64 = 0,
    alloc_sample_stream: u64 = 0,
    alloc_sample_hash_table: u64 = 0,
    alloc_sample_chunk: u64 = 0,
    alloc_sample_other: u64 = 0,
    alloc_sample_size: [8]u64 = [_]u64{0} ** 8,
    gc_survive_n: u64 = 0,
    gc_survive_bytes: u64 = 0,
    gc_survive_class: []u64 = &[_]u64{},
    gc_survive_size: [8]u64 = [_]u64{0} ** 8,
    gc_survive_age: [8]u64 = [_]u64{0} ** 8,
    gc_promote_n: u64 = 0,
    gc_promote_bytes: u64 = 0,
    gc_promote_class: []u64 = &[_]u64{},
    gc_promote_size: [8]u64 = [_]u64{0} ** 8,
    gc_promote_age: [8]u64 = [_]u64{0} ** 8,
    gc_promote_success_n: u64 = 0,
    gc_promote_success_bytes: u64 = 0,
    gc_promote_success_class: []u64 = &[_]u64{},
    gc_promote_success_age: [8]u64 = [_]u64{0} ** 8,
    gc_promote_threshold: u64 = 0,
    gc_promote_threshold_min: u64 = 0,
    gc_promote_threshold_max: u64 = 0,
    gc_promote_scale: f64 = 1.0,
    gc_promote_success_rate: f64 = 0.0,
    gc_promote_young_ratio: f64 = 0.0,
    gc_promote_mature_ratio: f64 = 0.0,
    gc_nursery_target: u64 = 0,
    gc_nursery_scale: f64 = 1.0,
    gc_nursery_survival: f64 = 0.0,
    gc_nursery_pause_error: f64 = 0.0,
    gc_debt_bytes: u64 = 0,
    gc_debt_threshold: u64 = 0,
    gc_debt_alloc_bytes: u64 = 0,
    gc_debt_paydown_bytes: u64 = 0,
    gc_debt_trigger_n: u64 = 0,
};

const VmBench = struct {
    name: []const u8,
    ops: u64,
    ns: u64,
    ops_per_sec: f64,
    allocs: u64,
    bytes_alloc: u64,
    gc_count: u64,
};

const VmJson = struct {
    benches: []VmBench,
};

const JitBench = struct {
    ns: u64,
    ops_per_sec: f64,
};

const JitVm = struct {
    ns: u64,
    ops_per_sec: f64,
    allocs: u64,
    bytes_alloc: u64,
    gc_count: u64,
};

const JitJson = struct {
    ops: u64,
    vm: JitVm,
    jit: struct {
        cold: JitBench,
        steady: JitBench,
        compile_ns: u64,
        compile_n: u64,
        code_bytes: u64,
        fail_n: u64,
    },
    speedup: f64,
};

const Opts = struct {
    // VM bench thresholds
    min_fix_mops: f64 = 1.0,
    // JIT bench thresholds
    min_jit_speedup: f64 = 2.0,
    max_jit_compile_ms: f64 = 200.0,
    max_jit_code_bytes: u64 = 1_000_000,
    // GC bench thresholds
    max_gc_p95_ms: f64 = 200.0,
    json: bool = false,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\Bench regression checks
        \\
        \\Usage:
        \\  zig build bench-check -- [--min-fix-mops N] [--min-jit-speedup N] [--max-jit-compile-ms N] [--max-jit-code-bytes N] [--max-gc-p95-ms N] [--json]
        \\
    );
}

fn parseF64(arg: []const u8) !f64 {
    return try std.fmt.parseFloat(f64, arg);
}

fn parseU64(arg: []const u8) !u64 {
    return try std.fmt.parseInt(u64, arg, 10);
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
        } else if (std.mem.eql(u8, arg, "--json")) {
            opts.json = true;
        } else if (std.mem.startsWith(u8, arg, "--min-fix-mops=")) {
            opts.min_fix_mops = try parseF64(arg["--min-fix-mops=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--min-jit-speedup=")) {
            opts.min_jit_speedup = try parseF64(arg["--min-jit-speedup=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--max-jit-compile-ms=")) {
            opts.max_jit_compile_ms = try parseF64(arg["--max-jit-compile-ms=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--max-jit-code-bytes=")) {
            opts.max_jit_code_bytes = try parseU64(arg["--max-jit-code-bytes=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--max-gc-p95-ms=")) {
            opts.max_gc_p95_ms = try parseF64(arg["--max-gc-p95-ms=".len..]);
        } else {
            return error.InvalidArgs;
        }
    }

    if (opts.min_fix_mops <= 0.0) return error.InvalidArgs;
    if (opts.min_jit_speedup <= 0.0) return error.InvalidArgs;
    if (opts.max_jit_compile_ms <= 0.0) return error.InvalidArgs;
    if (opts.max_jit_code_bytes == 0) return error.InvalidArgs;
    if (opts.max_gc_p95_ms <= 0.0) return error.InvalidArgs;
    return opts;
}

fn runJson(allocator: Allocator, argv: []const []const u8) ![]u8 {
    const res = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = argv,
        .max_output_bytes = 1024 * 1024,
    });
    defer allocator.free(res.stderr);

    switch (res.term) {
        .Exited => |code| if (code != 0) {
            allocator.free(res.stdout);
            return error.BenchFailed;
        },
        else => {
            allocator.free(res.stdout);
            return error.BenchFailed;
        },
    }

    return res.stdout;
}

fn parseJson(comptime T: type, allocator: Allocator, bytes: []const u8) !std.json.Parsed(T) {
    return try std.json.parseFromSlice(T, allocator, bytes, .{
        .ignore_unknown_fields = true,
        .allocate = .alloc_always,
    });
}

fn getVmBench(vm: *const VmJson, name: []const u8) ?VmBench {
    for (vm.benches) |b| {
        if (std.mem.eql(u8, b.name, name)) return b;
    }
    return null;
}

fn fail(comptime fmt: []const u8, args: anytype) !void {
    std.debug.print(fmt ++ "\n", args);
    return error.BenchRegression;
}

pub fn main() !void {
    const opts = parseArgs() catch |err| switch (err) {
        error.InvalidArgs => {
            var buf: [4096]u8 = undefined;
            var out = std.fs.File.stderr().writer(&buf);
            usage(&out.interface) catch {};
            out.interface.flush() catch {};
            return error.InvalidArgs;
        },
        else => return err,
    };
    if (!build_options.use_hoist) return error.JitDisabled;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const gc_stdout = try runJson(allocator, &.{ "zig-out/bin/gc_bench", "--json" });
    defer allocator.free(gc_stdout);
    var gc_parsed = try parseJson(GcJson, allocator, gc_stdout);
    defer gc_parsed.deinit();

    const vm_stdout = try runJson(allocator, &.{ "zig-out/bin/vm_bench", "--json" });
    defer allocator.free(vm_stdout);
    var vm_parsed = try parseJson(VmJson, allocator, vm_stdout);
    defer vm_parsed.deinit();

    const jit_stdout = try runJson(allocator, &.{ "zig-out/bin/jit_bench", "--json" });
    defer allocator.free(jit_stdout);
    var jit_parsed = try parseJson(JitJson, allocator, jit_stdout);
    defer jit_parsed.deinit();

    const gc = gc_parsed.value;
    const vm = vm_parsed.value;
    const jit = jit_parsed.value;

    if (gc.gc_count != gc.iters) try fail("gc_count {d} != iters {d}", .{ gc.gc_count, gc.iters });
    if (gc.gc_minor_count + gc.gc_major_count != gc.gc_count) {
        try fail(
            "gc mode counts {d}+{d} != gc_count {d}",
            .{ gc.gc_minor_count, gc.gc_major_count, gc.gc_count },
        );
    }
    if (gc.bytes_copied == 0) try fail("gc bytes_copied is 0", .{});
    if (gc.live_bytes == 0) try fail("gc live_bytes is 0", .{});
    if (gc.avg_build_ns == 0) try fail("gc avg_build_ns is 0", .{});
    if (gc.avg_root_ns == 0) try fail("gc avg_root_ns is 0", .{});
    if (gc.avg_copy_ns == 0) try fail("gc avg_copy_ns is 0", .{});
    if (gc.root_vals == 0) try fail("gc root_vals is 0", .{});
    if (gc.avg_copy_ns < gc.avg_root_ns) {
        try fail("gc avg_copy_ns {d} < avg_root_ns {d}", .{ gc.avg_copy_ns, gc.avg_root_ns });
    }
    if (gc.gc_minor_count > 0 and gc.avg_minor_ns == 0) try fail("gc avg_minor_ns is 0", .{});
    if (gc.gc_major_count > 0 and gc.avg_major_ns == 0) try fail("gc avg_major_ns is 0", .{});
    if (gc.alloc_sample_n == 0) try fail("gc alloc_sample_n is 0", .{});
    if (gc.alloc_sample_bytes == 0) try fail("gc alloc_sample_bytes is 0", .{});
    var class_sum: u64 = 0;
    class_sum += gc.alloc_sample_cons;
    class_sum += gc.alloc_sample_symbol;
    class_sum += gc.alloc_sample_keyword;
    class_sum += gc.alloc_sample_vector;
    class_sum += gc.alloc_sample_array;
    class_sum += gc.alloc_sample_string;
    class_sum += gc.alloc_sample_closure;
    class_sum += gc.alloc_sample_stream;
    class_sum += gc.alloc_sample_hash_table;
    class_sum += gc.alloc_sample_chunk;
    class_sum += gc.alloc_sample_other;
    if (class_sum != gc.alloc_sample_n) {
        try fail("gc alloc class sum {d} != alloc_sample_n {d}", .{ class_sum, gc.alloc_sample_n });
    }
    var size_sum: u64 = 0;
    for (gc.alloc_sample_size) |n| size_sum += n;
    if (size_sum != gc.alloc_sample_n) {
        try fail("gc alloc size sum {d} != alloc_sample_n {d}", .{ size_sum, gc.alloc_sample_n });
    }
    if (gc.gc_survive_n == 0) try fail("gc gc_survive_n is 0", .{});
    if (gc.gc_survive_bytes == 0) try fail("gc gc_survive_bytes is 0", .{});
    const expected_survive_bytes = gc.bytes_copied + gc.promoted_bytes;
    if (gc.gc_survive_bytes != expected_survive_bytes) {
        try fail("gc survive_bytes {d} != copied+promoted {d}", .{ gc.gc_survive_bytes, expected_survive_bytes });
    }
    var survive_class_sum: u64 = 0;
    for (gc.gc_survive_class) |n| survive_class_sum += n;
    if (survive_class_sum != gc.gc_survive_n) {
        try fail("gc survive class sum {d} != gc_survive_n {d}", .{ survive_class_sum, gc.gc_survive_n });
    }
    var survive_size_sum: u64 = 0;
    for (gc.gc_survive_size) |n| survive_size_sum += n;
    if (survive_size_sum != gc.gc_survive_n) {
        try fail("gc survive size sum {d} != gc_survive_n {d}", .{ survive_size_sum, gc.gc_survive_n });
    }
    var survive_age_sum: u64 = 0;
    for (gc.gc_survive_age) |n| survive_age_sum += n;
    if (survive_age_sum != gc.gc_survive_n) {
        try fail("gc survive age sum {d} != gc_survive_n {d}", .{ survive_age_sum, gc.gc_survive_n });
    }
    if (gc.gc_promote_n == 0) try fail("gc gc_promote_n is 0", .{});
    if (gc.gc_promote_bytes == 0) try fail("gc gc_promote_bytes is 0", .{});
    if (gc.gc_promote_bytes != gc.promoted_bytes) {
        try fail("gc promote_bytes {d} != promoted_bytes {d}", .{ gc.gc_promote_bytes, gc.promoted_bytes });
    }
    var promote_class_sum: u64 = 0;
    for (gc.gc_promote_class) |n| promote_class_sum += n;
    if (promote_class_sum != gc.gc_promote_n) {
        try fail("gc promote class sum {d} != gc_promote_n {d}", .{ promote_class_sum, gc.gc_promote_n });
    }
    var promote_size_sum: u64 = 0;
    for (gc.gc_promote_size) |n| promote_size_sum += n;
    if (promote_size_sum != gc.gc_promote_n) {
        try fail("gc promote size sum {d} != gc_promote_n {d}", .{ promote_size_sum, gc.gc_promote_n });
    }
    var promote_age_sum: u64 = 0;
    for (gc.gc_promote_age) |n| promote_age_sum += n;
    if (promote_age_sum != gc.gc_promote_n) {
        try fail("gc promote age sum {d} != gc_promote_n {d}", .{ promote_age_sum, gc.gc_promote_n });
    }
    if (gc.gc_promote_success_n > gc.gc_promote_n) {
        try fail("gc promote success n {d} > promote n {d}", .{ gc.gc_promote_success_n, gc.gc_promote_n });
    }
    if (gc.gc_promote_success_bytes > gc.gc_promote_bytes) {
        try fail("gc promote success bytes {d} > promote bytes {d}", .{ gc.gc_promote_success_bytes, gc.gc_promote_bytes });
    }
    var promote_success_class_sum: u64 = 0;
    for (gc.gc_promote_success_class) |n| promote_success_class_sum += n;
    if (promote_success_class_sum != gc.gc_promote_success_n) {
        try fail("gc promote success class sum {d} != gc_promote_success_n {d}", .{ promote_success_class_sum, gc.gc_promote_success_n });
    }
    var promote_success_age_sum: u64 = 0;
    for (gc.gc_promote_success_age) |n| promote_success_age_sum += n;
    if (promote_success_age_sum != gc.gc_promote_success_n) {
        try fail("gc promote success age sum {d} != gc_promote_success_n {d}", .{ promote_success_age_sum, gc.gc_promote_success_n });
    }
    if (gc.gc_promote_threshold == 0) try fail("gc gc_promote_threshold is 0", .{});
    if (gc.gc_promote_threshold_min == 0) try fail("gc gc_promote_threshold_min is 0", .{});
    if (gc.gc_promote_threshold_max == 0) try fail("gc gc_promote_threshold_max is 0", .{});
    if (gc.gc_promote_threshold_min > gc.gc_promote_threshold_max) {
        try fail(
            "gc promote threshold min {d} > max {d}",
            .{ gc.gc_promote_threshold_min, gc.gc_promote_threshold_max },
        );
    }
    if (gc.gc_promote_threshold < gc.gc_promote_threshold_min or gc.gc_promote_threshold > gc.gc_promote_threshold_max) {
        try fail(
            "gc promote threshold {d} outside [{d},{d}]",
            .{ gc.gc_promote_threshold, gc.gc_promote_threshold_min, gc.gc_promote_threshold_max },
        );
    }
    if (gc.gc_promote_scale < 0.50 or gc.gc_promote_scale > 1.50) {
        try fail("gc promote scale {d:.4} outside [0.50,1.50]", .{gc.gc_promote_scale});
    }
    if (gc.gc_promote_success_rate < 0.0 or gc.gc_promote_success_rate > 1.0) {
        try fail("gc promote success rate {d:.4} outside [0,1]", .{gc.gc_promote_success_rate});
    }
    if (gc.gc_promote_young_ratio < 0.0 or gc.gc_promote_young_ratio > 1.0) {
        try fail("gc promote young ratio {d:.4} outside [0,1]", .{gc.gc_promote_young_ratio});
    }
    if (gc.gc_promote_mature_ratio < 0.0 or gc.gc_promote_mature_ratio > 1.0) {
        try fail("gc promote mature ratio {d:.4} outside [0,1]", .{gc.gc_promote_mature_ratio});
    }
    if (gc.gc_remembered_marked_cards == 0) try fail("gc remembered marked cards is 0", .{});
    if (gc.gc_remembered_runs == 0) try fail("gc remembered runs is 0", .{});
    if (gc.gc_remembered_runs > gc.gc_remembered_marked_cards) {
        try fail(
            "gc remembered runs {d} > marked cards {d}",
            .{ gc.gc_remembered_runs, gc.gc_remembered_marked_cards },
        );
    }
    if (gc.gc_remembered_scanned == 0) try fail("gc remembered scanned is 0", .{});
    if (gc.gc_remembered_scanned > gc.root_vals) {
        try fail(
            "gc remembered scanned {d} > root vals {d}",
            .{ gc.gc_remembered_scanned, gc.root_vals },
        );
    }
    if (gc.gc_debt_threshold == 0) try fail("gc debt threshold is 0", .{});
    if (gc.gc_debt_alloc_bytes == 0) try fail("gc debt alloc bytes is 0", .{});
    if (gc.gc_debt_paydown_bytes == 0) try fail("gc debt paydown bytes is 0", .{});
    if (gc.gc_debt_bytes > gc.gc_debt_threshold) {
        try fail("gc debt bytes {d} > threshold {d}", .{ gc.gc_debt_bytes, gc.gc_debt_threshold });
    }
    if (gc.gc_debt_paydown_bytes > gc.gc_debt_alloc_bytes + gc.gc_debt_threshold) {
        try fail(
            "gc debt paydown {d} > alloc+threshold {d}",
            .{ gc.gc_debt_paydown_bytes, gc.gc_debt_alloc_bytes + gc.gc_debt_threshold },
        );
    }
    if (gc.gc_nursery_target == 0) try fail("gc gc_nursery_target is 0", .{});
    if (gc.gc_nursery_target > gc.heap_bytes) {
        try fail("gc nursery_target {d} > heap_bytes {d}", .{ gc.gc_nursery_target, gc.heap_bytes });
    }
    if (gc.gc_nursery_target < gc.live_bytes) {
        try fail("gc nursery_target {d} < live_bytes {d}", .{ gc.gc_nursery_target, gc.live_bytes });
    }
    if (gc.gc_nursery_scale < 0.50 or gc.gc_nursery_scale > 1.50) {
        try fail("gc nursery_scale {d:.4} outside [0.50,1.50]", .{gc.gc_nursery_scale});
    }
    if (gc.gc_nursery_survival < 0.0) try fail("gc nursery_survival < 0", .{});
    if (gc.promoted_bytes == 0) try fail("gc promoted_bytes is 0", .{});
    if (gc.wb_marks == 0) try fail("gc wb_marks is 0", .{});
    if (gc.los_live == 0) try fail("gc los_live is 0", .{});
    if (gc.los_bytes == 0) try fail("gc los_bytes is 0", .{});
    if (gc.tenured_bytes + gc.los_bytes > gc.heap_bytes) {
        try fail("gc old-space bytes {d} > heap_bytes {d}", .{ gc.tenured_bytes + gc.los_bytes, gc.heap_bytes });
    }
    const gc_phase_sum = gc.avg_build_ns + gc.avg_root_ns + gc.avg_copy_ns + gc.avg_finalize_ns;
    if (gc.avg_pause_ns > 0 and gc_phase_sum > gc.avg_pause_ns * 4) {
        try fail("gc phase sum {d} > 4x avg_pause_ns {d}", .{ gc_phase_sum, gc.avg_pause_ns });
    }
    if (gc.p50_pause_ns == 0) try fail("gc p50_pause_ns is 0", .{});
    if (gc.p99_pause_ns == 0) try fail("gc p99_pause_ns is 0", .{});
    if (gc.p50_pause_ns > gc.p95_pause_ns) {
        try fail("gc p50 {d} > p95 {d}", .{ gc.p50_pause_ns, gc.p95_pause_ns });
    }
    if (gc.p95_pause_ns > gc.p99_pause_ns) {
        try fail("gc p95 {d} > p99 {d}", .{ gc.p95_pause_ns, gc.p99_pause_ns });
    }
    const p95_ms = @as(f64, @floatFromInt(gc.p95_pause_ns)) / 1e6;
    if (p95_ms > opts.max_gc_p95_ms) try fail("gc p95 {d:.3}ms > {d:.3}ms", .{ p95_ms, opts.max_gc_p95_ms });

    const fix_opt = getVmBench(&vm, "fixnum");
    if (fix_opt == null) try fail("missing vm bench: fixnum", .{});
    const fix = fix_opt.?;
    if (fix.allocs != 0) try fail("vm fixnum allocs {d} != 0", .{fix.allocs});
    if (fix.gc_count != 0) try fail("vm fixnum gc_count {d} != 0", .{fix.gc_count});
    const fix_mops = fix.ops_per_sec / 1e6;
    if (fix_mops < opts.min_fix_mops) try fail("vm fixnum {d:.3} Mops/s < {d:.3}", .{ fix_mops, opts.min_fix_mops });

    if (jit.jit.fail_n != 0) try fail("jit fail_n {d} != 0", .{jit.jit.fail_n});
    if (jit.jit.compile_n != 1) try fail("jit compile_n {d} != 1", .{jit.jit.compile_n});
    if (jit.jit.compile_n == 0) try fail("jit compile_n is 0", .{});
    const compile_ms = (@as(f64, @floatFromInt(jit.jit.compile_ns)) /
        @as(f64, @floatFromInt(jit.jit.compile_n))) / 1e6;
    if (compile_ms > opts.max_jit_compile_ms) {
        try fail("jit compile {d:.3}ms > {d:.3}ms", .{ compile_ms, opts.max_jit_compile_ms });
    }
    if (jit.jit.code_bytes > opts.max_jit_code_bytes) {
        try fail("jit code_bytes {d} > {d}", .{ jit.jit.code_bytes, opts.max_jit_code_bytes });
    }
    if (jit.speedup < opts.min_jit_speedup) try fail("jit speedup {d:.3}x < {d:.3}x", .{ jit.speedup, opts.min_jit_speedup });

    if (!opts.json) return;

    const gc_json = std.mem.trimRight(u8, gc_stdout, " \r\n\t");
    const vm_json = std.mem.trimRight(u8, vm_stdout, " \r\n\t");
    const jit_json = std.mem.trimRight(u8, jit_stdout, " \r\n\t");

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;
    try w.print(
        "{{\"gc\":{s},\"vm\":{s},\"jit\":{s}}}\n",
        .{ gc_json, vm_json, jit_json },
    );
    try w.flush();
}
