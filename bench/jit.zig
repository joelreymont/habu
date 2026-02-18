const std = @import("std");

const common = @import("common.zig");

const habu = @import("habu");
const runtime = habu.runtime;
const interp = habu.interp;
const compiler_mod = habu.compiler;
const bytecode = habu.bytecode;

const Heap = runtime.Heap;
const Vm = interp.Vm;
const Compiler = compiler_mod.Compiler;
const Value = runtime.Value;
const Op = bytecode.Op;

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

fn run(timer: *std.time.Timer, heap: *Heap, vm: *Vm, chunk: *runtime.Chunk, ops: u64) !Bench {
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

fn countBytecodeOps(chunk: *runtime.Chunk) u64 {
    const code = chunk.getCode();
    var i: usize = 0;
    var n: u64 = 0;
    while (i + 1 < code.len) {
        const low: u16 = code[i];
        const high: u16 = code[i + 1];
        const op_raw: u16 = low | (high << 8);
        const op: Op = @enumFromInt(op_raw);
        i += 2 + op.operandSize();
        n += 1;
    }
    return n;
}

fn sumJitCodeBytes(vm: *const Vm) u64 {
    var sum: u64 = 0;
    var it = vm.jit_fns.iterator();
    while (it.next()) |entry| {
        sum +%= entry.value_ptr.*.mem.used;
    }
    return sum;
}

fn snapshotJitKeys(allocator: std.mem.Allocator, vm: *const Vm) !std.AutoHashMap(usize, void) {
    var keys = std.AutoHashMap(usize, void).init(allocator);
    var it = vm.jit_fns.iterator();
    while (it.next()) |entry| {
        try keys.put(entry.key_ptr.*, {});
    }
    return keys;
}

fn findNewJitChunk(vm: *const Vm, before: *const std.AutoHashMap(usize, void)) ?*runtime.Chunk {
    var it = vm.jit_fns.iterator();
    while (it.next()) |entry| {
        const key = entry.key_ptr.*;
        if (before.contains(key)) continue;
        const chunk: *runtime.Chunk = @ptrFromInt(key);
        return chunk;
    }
    return null;
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
    _ = opts.code_mb;
    _ = opts.hot;

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

    var src_buf_vm: [768]u8 = undefined;
    const setup_vm = try std.fmt.bufPrint(
        &src_buf_vm,
        "(defun bench-fixnum-vm () (let ((i 0) (acc 0)) (while (< i {d}) (setq acc (+ acc i)) (setq i (+ i 1))) acc))",
        .{opts.fix_n},
    );
    const setup_vm_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, setup_vm);
    _ = try vm.run(setup_vm_chunk);

    const call_vm_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, "(bench-fixnum-vm)");
    _ = try vm.run(call_vm_chunk);
    const vm_bench = try run(&timer, &heap, &vm, call_vm_chunk, opts.fix_n);

    const code_bytes_before = sumJitCodeBytes(&vm);
    var keys_before = try snapshotJitKeys(allocator, &vm);
    defer keys_before.deinit();
    const jit_count_before = vm.jit_fns.count();

    var src_buf_jit: [896]u8 = undefined;
    const setup_jit = try std.fmt.bufPrint(
        &src_buf_jit,
        "(defun bench-fixnum-jit () (declare (optimize (speed 3) (safety 0))) (let ((i 0) (acc 0)) (while (< i {d}) (setq acc (+ acc i)) (setq i (+ i 1))) acc))",
        .{opts.fix_n},
    );

    const t_compile0 = timer.read();
    const setup_jit_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, setup_jit);
    const t_compile1 = timer.read();
    const compile_ns = t_compile1 - t_compile0;
    _ = try vm.run(setup_jit_chunk);

    const jit_count_after = vm.jit_fns.count();
    const compile_n: u64 = if (jit_count_after >= jit_count_before) jit_count_after - jit_count_before else 0;
    const fail_n: u64 = if (compile_n == 0) 1 else 0;
    const code_bytes_after = sumJitCodeBytes(&vm);
    const code_bytes_delta: u64 = if (code_bytes_after >= code_bytes_before) code_bytes_after - code_bytes_before else 0;

    var bc_ops: u64 = 0;
    if (findNewJitChunk(&vm, &keys_before)) |jit_chunk| {
        bc_ops = countBytecodeOps(jit_chunk);
    }

    const call_jit_chunk = try common.compileChunk(allocator, &heap, &vm, &comp, &chunk_pool, "(bench-fixnum-jit)");
    const jit_cold = try run(&timer, &heap, &vm, call_jit_chunk, opts.fix_n);
    const jit_steady = try run(&timer, &heap, &vm, call_jit_chunk, opts.fix_n);

    const speedup = if (jit_steady.ns == 0) 0.0 else
        @as(f64, @floatFromInt(vm_bench.ns)) / @as(f64, @floatFromInt(jit_steady.ns));
    const bytes_per_op = if (bc_ops == 0) 0.0 else
        @as(f64, @floatFromInt(code_bytes_delta)) / @as(f64, @floatFromInt(bc_ops));

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.print(
            "{{\"ops\":{d},\"vm\":{{\"ns\":{d},\"ops_per_sec\":{d:.3},\"allocs\":{d},\"bytes_alloc\":{d},\"gc_count\":{d}}},\"jit\":{{\"cold\":{{\"ns\":{d},\"ops_per_sec\":{d:.3}}},\"steady\":{{\"ns\":{d},\"ops_per_sec\":{d:.3}}},\"compile_ns\":{d},\"compile_n\":{d},\"code_bytes\":{d},\"bytecode_ops\":{d},\"code_bytes_per_op\":{d:.3},\"fail_n\":{d}}},\"speedup\":{d:.3}}}\n",
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
                compile_ns,
                compile_n,
                code_bytes_delta,
                bc_ops,
                bytes_per_op,
                fail_n,
                speedup,
            },
        );
        try w.flush();
        return;
    }

    try w.print("JIT microbench\n", .{});
    try w.print("  heap: {d} MiB\n", .{opts.heap_mb});
    try w.print("  vm:  {d:.3} Mops/s\n", .{common.opsPerSec(vm_bench.ops, vm_bench.ns) / 1e6});
    try w.print("  jit: {d:.3} Mops/s (steady)\n", .{common.opsPerSec(jit_steady.ops, jit_steady.ns) / 1e6});
    try w.print("  compile: {d} ns (n={d}, fail={d})\n", .{ compile_ns, compile_n, fail_n });
    try w.print("  code: {d} bytes ({d:.3} bytes/op, ops={d})\n", .{ code_bytes_delta, bytes_per_op, bc_ops });
    try w.print("  speedup: {d:.3}x\n", .{speedup});
    try w.flush();
}
