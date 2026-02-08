const std = @import("std");

const common = @import("common.zig");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const compiler_mod = habu.compiler;

const Heap = runtime.Heap;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Chunk = runtime.Chunk;

const Bench = struct {
    name: []const u8,
    ops: u64,
    ns: u64,
    allocs: usize,
    bytes_alloc: usize,
    gc_count: usize,
};

const Opts = struct {
    heap_mb: usize = 256,
    iters: usize = 3,
    json: bool = false,
    jit: bool = true,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\CL comparison bench (Habu side)
        \\
        \\Usage:
        \\  zig build bench-cl -- [--heap-mb N] [--iters N] [--json]
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
        } else if (std.mem.eql(u8, arg, "--json")) {
            opts.json = true;
        } else if (std.mem.startsWith(u8, arg, "--heap-mb=")) {
            opts.heap_mb = try std.fmt.parseInt(usize, arg["--heap-mb=".len..], 10);
        } else if (std.mem.startsWith(u8, arg, "--iters=")) {
            opts.iters = try std.fmt.parseInt(usize, arg["--iters=".len..], 10);
        } else if (std.mem.eql(u8, arg, "--no-jit")) {
            opts.jit = false;
        } else {
            return error.InvalidArgs;
        }
    }
    if (opts.heap_mb == 0 or opts.iters == 0) return error.InvalidArgs;
    return opts;
}

fn tryBench(timer: *std.time.Timer, heap: *Heap, vm: *Vm, chunk: *Chunk, name: []const u8, iters: usize) Bench {
    {
        var buf: [4096]u8 = undefined;
        var w = std.fs.File.stderr().writer(&buf);
        w.interface.print("START {s}\n", .{name}) catch {};
        w.interface.flush() catch {};
    }
    return runBench(timer, heap, vm, chunk, name, iters) catch |err| {
        var buf: [4096]u8 = undefined;
        var w = std.fs.File.stderr().writer(&buf);
        w.interface.print("SKIP {s}: {}\n", .{ name, err }) catch {};
        w.interface.flush() catch {};
        return .{ .name = name, .ops = 0, .ns = 0, .allocs = 0, .bytes_alloc = 0, .gc_count = 0 };
    };
}

fn runBench(timer: *std.time.Timer, heap: *Heap, vm: *Vm, chunk: *Chunk, name: []const u8, iters: usize) !Bench {
    // Warmup
    _ = try vm.run(chunk);

    const a0 = heap.stats.allocations;
    const b0 = heap.stats.bytes_allocated;
    const g0 = heap.stats.gc_count;

    const t0 = timer.read();
    for (0..iters) |_| {
        _ = try vm.run(chunk);
    }
    const t1 = timer.read();

    return .{
        .name = name,
        .ops = iters,
        .ns = (t1 - t0) / iters,
        .allocs = (heap.stats.allocations - a0) / iters,
        .bytes_alloc = (heap.stats.bytes_allocated - b0) / iters,
        .gc_count = (heap.stats.gc_count - g0),
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

    var chunk_pool = std.ArrayList(*Chunk){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPool(chunk_pool.items);

    // Enable JIT: hot=1 means warmup call triggers compilation
    if (opts.jit) {
        vm.enableJit(16 * 1024 * 1024, 1) catch {};
    }

    var timer = try std.time.Timer.start();

    // 1. Fixnum loop: sum 0 to 10_000_000 (with type declarations for specialization)
    const fix_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool,
        "(let ((i 0) (acc 0)) (declare (type fixnum i acc)) (while (< i 10000000) (setq acc (the fixnum (+ (the fixnum acc) (the fixnum i)))) (setq i (the fixnum (+ (the fixnum i) 1)))) acc)");

    // 2. Fibonacci: fib(35) recursive (typed for specialization)
    const fib_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool,
        "(progn (defun fib (n) (declare (type fixnum n) (optimize (speed 3) (safety 0))) (if (<= n 1) n (the fixnum (+ (fib (the fixnum (- n 1))) (fib (the fixnum (- n 2))))))) (fib 35))");

    // 3. Tak: tak(18, 12, 6) (typed for specialization)
    const tak_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool,
        "(progn (defun tak (x y z) (declare (type fixnum x y z) (optimize (speed 3) (safety 0))) (if (<= x y) z (tak (tak (the fixnum (- x 1)) y z) (tak (the fixnum (- y 1)) z x) (tak (the fixnum (- z 1)) x y)))) (tak 18 12 6))");

    // 4. List length: length of 1M-element list
    const list_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool,
        "(let ((xs nil) (i 0)) (while (< i 1000000) (setq xs (cons i xs)) (setq i (+ i 1))) (length xs))");

    // 5. Cons allocation: create 1M cons cells
    const cons_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool,
        "(let ((i 0) (xs nil)) (while (< i 1000000) (setq xs (cons i xs)) (setq i (+ i 1))) xs)");

    // Update chunk pool pointer (ArrayList may have grown during compilation)
    vm.setChunkPool(chunk_pool.items);

    const benches = [_]Bench{
        tryBench(&timer, &heap, &vm, fix_chunk, "fixnum_loop", opts.iters),
        tryBench(&timer, &heap, &vm, list_chunk, "list_length", opts.iters),
        tryBench(&timer, &heap, &vm, cons_chunk, "cons_alloc", opts.iters),
        tryBench(&timer, &heap, &vm, fib_chunk, "fib35", opts.iters),
        tryBench(&timer, &heap, &vm, tak_chunk, "tak", opts.iters),
    };

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    const mode: []const u8 = if (vm.jit_on) "jit" else "interp";

    if (opts.json) {
        try w.print("{{\"engine\":\"habu\",\"mode\":\"{s}\",\"benches\":[", .{mode});
        for (benches, 0..) |b, i| {
            if (i != 0) try w.writeByte(',');
            try w.print(
                "{{\"name\":\"{s}\",\"ns\":{d},\"allocs\":{d},\"gc_count\":{d}}}",
                .{ b.name, b.ns, b.allocs, b.gc_count },
            );
        }
        try w.writeAll("]}\n");
        try w.flush();
        return;
    }

    try w.print("CL comparison bench (Habu, {s})\n", .{mode});
    try w.print("  heap: {d} MiB, iters: {d}\n", .{ opts.heap_mb, opts.iters });
    for (benches) |b| {
        const ms = @as(f64, @floatFromInt(b.ns)) / 1e6;
        try w.print("  {s}: {d:.3} ms (allocs {d}, gc {d})\n", .{ b.name, ms, b.allocs, b.gc_count });
    }
    try w.flush();
}
