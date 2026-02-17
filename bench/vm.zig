const std = @import("std");

const common = @import("common.zig");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const compiler_mod = habu.compiler;

const Heap = runtime.Heap;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Value = runtime.Value;

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
    fix_n: u64 = 50_000_000,
    cons_n: u64 = 1_000_000,
    hash_n: u64 = 500_000,
    str_n: u64 = 200_000,
    json: bool = false,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\VM microbench
        \\
        \\Usage:
        \\  zig build bench-vm -- [--heap-mb N] [--fix-n N] [--cons-n N] [--hash-n N] [--str-n N] [--json]
        \\
    );
}

fn parseU64(arg: []const u8) !u64 {
    return try std.fmt.parseInt(u64, arg, 10);
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
        } else if (std.mem.startsWith(u8, arg, "--fix-n=")) {
            opts.fix_n = try parseU64(arg["--fix-n=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--cons-n=")) {
            opts.cons_n = try parseU64(arg["--cons-n=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--hash-n=")) {
            opts.hash_n = try parseU64(arg["--hash-n=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--str-n=")) {
            opts.str_n = try parseU64(arg["--str-n=".len..]);
        } else {
            return error.InvalidArgs;
        }
    }
    if (opts.heap_mb == 0) return error.InvalidArgs;
    return opts;
}

fn runBench(timer: *std.time.Timer, heap: *Heap, vm: *Vm, chunk: *runtime.objects.Chunk, name: []const u8, ops: u64) !Bench {
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
        .name = name,
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

    var chunk_pool = std.ArrayList(Value){};
    defer chunk_pool.deinit(allocator);
    vm.setChunkPool(chunk_pool.items);

    var timer = try std.time.Timer.start();

    var src_buf: [512]u8 = undefined;

    const fix_src = try std.fmt.bufPrint(
        &src_buf,
        "(let ((i 0) (acc 0)) (while (< i {d}) (setq acc (+ acc i)) (setq i (+ i 1))) acc)",
        .{opts.fix_n},
    );
    const fix_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, fix_src);

    const cons_src = try std.fmt.bufPrint(
        &src_buf,
        "(let ((i 0) (xs nil)) (while (< i {d}) (setq xs (cons i xs)) (setq i (+ i 1))) xs)",
        .{opts.cons_n},
    );
    const cons_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, cons_src);

    const hash_src = try std.fmt.bufPrint(
        &src_buf,
        "(let ((ht (make-hash-table)) (i 0) (acc 0)) (while (< i {d}) (puthash i i ht) (setq acc (+ acc (gethash i ht 0))) (setq i (+ i 1))) acc)",
        .{opts.hash_n},
    );
    const hash_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, hash_src);

    const str_src = try std.fmt.bufPrint(
        &src_buf,
        "(let ((i 0) (acc 0)) (while (< i {d}) (let ((s (make-string 8 :initial-element #\\a))) (setq acc (+ acc (length s)))) (setq i (+ i 1))) acc)",
        .{opts.str_n},
    );
    const str_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, str_src);

    // Warmup (bytecode decode caches, etc.)
    _ = try vm.run(fix_chunk);

    const benches = [_]Bench{
        try runBench(&timer, &heap, &vm, fix_chunk, "fixnum", opts.fix_n),
        try runBench(&timer, &heap, &vm, cons_chunk, "cons", opts.cons_n),
        // puthash + gethash
        try runBench(&timer, &heap, &vm, hash_chunk, "hash", opts.hash_n * 2),
        try runBench(&timer, &heap, &vm, str_chunk, "string", opts.str_n),
    };

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.writeAll("{\"benches\":[");
        for (benches, 0..) |b, i| {
            if (i != 0) try w.writeByte(',');
            try w.print(
                "{{\"name\":\"{s}\",\"ops\":{d},\"ns\":{d},\"ops_per_sec\":{d:.3},\"allocs\":{d},\"bytes_alloc\":{d},\"gc_count\":{d}}}",
                .{ b.name, b.ops, b.ns, common.opsPerSec(b.ops, b.ns), b.allocs, b.bytes_alloc, b.gc_count },
            );
        }
        try w.writeAll("]}\n");
        try w.flush();
        return;
    }

    try w.print("VM microbench\n", .{});
    try w.print("  heap: {d} MiB\n", .{opts.heap_mb});
    for (benches) |b| {
        try w.print(
            "  {s}: {d:.3} Mops/s (allocs {d}, gc {d})\n",
            .{ b.name, common.opsPerSec(b.ops, b.ns) / 1e6, b.allocs, b.gc_count },
        );
    }
    try w.flush();
}
