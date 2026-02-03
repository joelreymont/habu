const std = @import("std");

const Allocator = std.mem.Allocator;

const GcJson = struct {
    iters: u64,
    heap_bytes: u64,
    live_bytes: u64,
    avg_pause_ns: u64,
    p95_pause_ns: u64,
    gc_count: u64,
    bytes_copied: u64,
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
        fail_n: u64,
    },
    speedup: f64,
};

const Opts = struct {
    // VM bench thresholds
    min_fix_mops: f64 = 1.0,
    // JIT bench thresholds
    min_jit_speedup: f64 = 2.0,
    // GC bench thresholds
    max_gc_p95_ms: f64 = 200.0,
    json: bool = false,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\Bench regression checks
        \\
        \\Usage:
        \\  zig build bench-check -- [--min-fix-mops N] [--min-jit-speedup N] [--max-gc-p95-ms N] [--json]
        \\
    );
}

fn parseF64(arg: []const u8) !f64 {
    return try std.fmt.parseFloat(f64, arg);
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
        } else if (std.mem.startsWith(u8, arg, "--max-gc-p95-ms=")) {
            opts.max_gc_p95_ms = try parseF64(arg["--max-gc-p95-ms=".len..]);
        } else {
            return error.InvalidArgs;
        }
    }

    if (opts.min_fix_mops <= 0.0) return error.InvalidArgs;
    if (opts.min_jit_speedup <= 0.0) return error.InvalidArgs;
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
        error.InvalidArgs => return,
        else => return err,
    };

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
    if (gc.bytes_copied == 0) try fail("gc bytes_copied is 0", .{});
    if (gc.live_bytes == 0) try fail("gc live_bytes is 0", .{});
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
