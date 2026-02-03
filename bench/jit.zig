const std = @import("std");

const common = @import("common.zig");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const compiler_mod = habu.compiler;

const Heap = runtime.Heap;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;

const Bench = struct {
    ops: u64,
    ns: u64,
    allocs: usize,
    bytes_alloc: usize,
    gc_count: usize,
};

const Opts = struct {
    heap_mb: usize = 256,
    code_mb: usize = 64,
    hot: u32 = 1,
    fix_n: u64 = 50_000_000,
    json: bool = false,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\JIT microbench (VM vs JIT)
        \\
        \\Usage:
        \\  zig build bench-jit -- [--heap-mb N] [--code-mb N] [--hot N] [--fix-n N] [--json]
        \\
    );
}

fn parseU64(arg: []const u8) !u64 {
    return try std.fmt.parseInt(u64, arg, 10);
}

fn parseU32(arg: []const u8) !u32 {
    return try std.fmt.parseInt(u32, arg, 10);
}

fn parseUsize(arg: []const u8) !usize {
    return try std.fmt.parseInt(usize, arg, 10);
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
        } else if (std.mem.startsWith(u8, arg, "--heap-mb=")) {
            opts.heap_mb = try parseUsize(arg["--heap-mb=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--code-mb=")) {
            opts.code_mb = try parseUsize(arg["--code-mb=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--hot=")) {
            opts.hot = try parseU32(arg["--hot=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--fix-n=")) {
            opts.fix_n = try parseU64(arg["--fix-n=".len..]);
        } else {
            return error.InvalidArgs;
        }
    }

    if (opts.heap_mb == 0 or opts.code_mb == 0) return error.InvalidArgs;
    return opts;
}

fn run(timer: *std.time.Timer, heap: *Heap, vm: *Vm, chunk: *runtime.objects.Chunk, ops: u64) !Bench {
    const a0 = heap.stats.allocations;
    const b0 = heap.stats.bytes_allocated;
    const g0 = heap.stats.gc_count;
    const t0 = timer.read();

    _ = try vm.run(chunk);

    const t1 = timer.read();
    const a1 = heap.stats.allocations;
    const b1 = heap.stats.bytes_allocated;
    const g1 = heap.stats.gc_count;

    return .{
        .ops = ops,
        .ns = t1 - t0,
        .allocs = a1 - a0,
        .bytes_alloc = b1 - b0,
        .gc_count = g1 - g0,
    };
}

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

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    var comp = try Compiler.initWithHeap(allocator, &vm);
    defer comp.deinit();
    vm.setGlobalEnv(&comp.globals);

    var chunk_pool = std.ArrayList(*runtime.objects.Chunk){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPool(chunk_pool.items);

    var timer = try std.time.Timer.start();

    var src_buf: [512]u8 = undefined;
    const fix_src = try std.fmt.bufPrint(
        &src_buf,
        "(let ((i 0) (acc 0)) (while (< i {d}) (setq acc (+ acc i)) (setq i (+ i 1))) acc)",
        .{opts.fix_n},
    );
    const chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, fix_src);

    // Baseline VM (interp)
    _ = try vm.run(chunk);
    const vm_bench = try run(&timer, &heap, &vm, chunk, opts.fix_n);

    // JIT cold (includes compilation)
    try vm.enableJit(opts.code_mb * 1024 * 1024, opts.hot);
    const jit_cold = try run(&timer, &heap, &vm, chunk, opts.fix_n);
    const st = vm.jitStats();

    // JIT steady-state (cached code)
    const jit_steady = try run(&timer, &heap, &vm, chunk, opts.fix_n);

    const speedup = if (jit_steady.ns == 0) 0.0 else @as(f64, @floatFromInt(vm_bench.ns)) / @as(f64, @floatFromInt(jit_steady.ns));

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.print(
            "{{\"ops\":{d},\"vm\":{{\"ns\":{d},\"ops_per_sec\":{d:.3},\"allocs\":{d},\"bytes_alloc\":{d},\"gc_count\":{d}}},\"jit\":{{\"cold\":{{\"ns\":{d},\"ops_per_sec\":{d:.3}}},\"steady\":{{\"ns\":{d},\"ops_per_sec\":{d:.3}}},\"compile_ns\":{d},\"compile_n\":{d},\"code_bytes\":{d},\"fail_n\":{d}}},\"speedup\":{d:.3}}}\n",
            .{
                opts.fix_n,
                vm_bench.ns,
                common.opsPerSec(vm_bench.ops, vm_bench.ns),
                vm_bench.allocs,
                vm_bench.bytes_alloc,
                vm_bench.gc_count,
                jit_cold.ns,
                common.opsPerSec(jit_cold.ops, jit_cold.ns),
                jit_steady.ns,
                common.opsPerSec(jit_steady.ops, jit_steady.ns),
                st.compile_ns,
                st.compile_n,
                st.code_bytes,
                st.fail_n,
                speedup,
            },
        );
        try w.flush();
        return;
    }

    try w.print("JIT microbench\n", .{});
    try w.print("  heap: {d} MiB\n", .{opts.heap_mb});
    try w.print("  code: {d} MiB\n", .{opts.code_mb});
    try w.print("  hot: {d}\n", .{opts.hot});
    try w.print("  vm:  {d:.3} Mops/s\n", .{common.opsPerSec(vm_bench.ops, vm_bench.ns) / 1e6});
    try w.print("  jit: {d:.3} Mops/s (steady)\n", .{common.opsPerSec(jit_steady.ops, jit_steady.ns) / 1e6});
    try w.print("  compile: {d} ns (n={d}, fail={d})\n", .{ st.compile_ns, st.compile_n, st.fail_n });
    try w.print("  code: {d} bytes\n", .{st.code_bytes});
    try w.print("  speedup: {d:.3}x\n", .{speedup});
    try w.flush();
}
