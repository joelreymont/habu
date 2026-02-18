const std = @import("std");

const runtime = @import("runtime");
const heap_mod = runtime.heap;
const objects = runtime.objects;
const Value = runtime.Value;

const Opts = struct {
    iters: usize = 100,
    heap_mb: usize = 64,
    live_mb: usize = 8,
    json: bool = false,
};

fn usage(w: anytype) !void {
    try w.writeAll(
        \\GC benchmark
        \\
        \\Usage:
        \\  zig build bench -- [--iters N] [--heap-mb N] [--live-mb N] [--json]
        \\
        \\Defaults:
        \\  --iters   100
        \\  --heap-mb 64   (total heap, both semispaces)
        \\  --live-mb 8    (target live set; must fit in one semispace)
        \\
    );
}

fn parseUsize(arg: []const u8) !usize {
    return try std.fmt.parseInt(usize, arg, 10);
}

fn lessU64(_: void, a: u64, b: u64) bool {
    return a < b;
}

fn divRoundUp(n: usize, d: usize) usize {
    std.debug.assert(d != 0);
    return (n + d - 1) / d;
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
        } else if (std.mem.startsWith(u8, arg, "--iters=")) {
            opts.iters = try parseUsize(arg["--iters=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--heap-mb=")) {
            opts.heap_mb = try parseUsize(arg["--heap-mb=".len..]);
        } else if (std.mem.startsWith(u8, arg, "--live-mb=")) {
            opts.live_mb = try parseUsize(arg["--live-mb=".len..]);
        } else {
            return error.InvalidArgs;
        }
    }
    if (opts.iters == 0) return error.InvalidArgs;
    if (opts.heap_mb == 0) return error.InvalidArgs;
    if (opts.live_mb == 0) return error.InvalidArgs;
    return opts;
}

pub fn main() !void {
    const opts = parseArgs() catch |err| switch (err) {
        error.InvalidArgs => return,
        else => return err,
    };

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer std.debug.assert(gpa.deinit() == .ok);
    const allocator = gpa.allocator();

    const heap_bytes = opts.heap_mb * 1024 * 1024;
    var heap = try heap_mod.Heap.init(allocator, .{
        .total_size = heap_bytes,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = heap_bytes / 4,
            .los_size = heap_bytes / 8,
            .los_threshold = 4 * 1024,
            .promote_threshold = 512,
        },
    });
    defer heap.deinit();

    const semispace = heap.space_size;
    const target_live = opts.live_mb * 1024 * 1024;
    if (target_live >= semispace) return error.InvalidArgs;

    // Build a rooted list of cons cells whose size approximates target_live.
    const cons_size = @sizeOf(objects.Cons);
    const cons_count = divRoundUp(target_live, cons_size);
    if (cons_count * cons_size >= semispace) return error.InvalidArgs;

    var roots = [_]Value{ Value.nil, Value.nil, Value.nil };
    var list = Value.nil;
    for (0..cons_count) |_| {
        list = try heap.allocCons(Value.nil, list);
    }
    roots[0] = list;

    // Rooted LOS object to exercise LOS mark/scan updates.
    roots[2] = try heap.allocVector(1, 1024);
    roots[2].toPtr(objects.Vector).set(0, roots[0]);

    // Rooted promotable payload (rotated each iteration to exercise tenured sweep/reuse).
    var promote_buf: [2048]u8 = undefined;
    @memset(promote_buf[0..], 'x');
    roots[1] = try heap.allocBaseString(promote_buf[0..]);

    // Warmup GC once to stabilize queue sizing and cache effects.
    _ = try heap.collectGarbage(roots[0..]);

    const pauses = try allocator.alloc(u64, opts.iters);
    defer allocator.free(pauses);

    var timer = try std.time.Timer.start();

    const bytes_copied0 = heap.stats.bytes_copied;
    const gc0 = heap.stats.gc_count;
    const build_ns0 = heap.stats.gc_build_ns;
    const root_ns0 = heap.stats.gc_root_ns;
    const copy_ns0 = heap.stats.gc_copy_ns;
    const finalize_ns0 = heap.stats.gc_finalize_ns;
    const root_vals0 = heap.stats.gc_root_vals;
    const promoted0 = heap.stats.gc_promoted_bytes;
    const wb0 = heap.stats.wb_marks;
    for (pauses, 0..) |*ns, i| {
        _ = i;
        roots[1] = try heap.allocBaseString(promote_buf[0..]);
        roots[2].toPtr(objects.Vector).set(0, roots[0]);
        heap.writeBarrier(roots[2], roots[0]);
        const t0 = timer.read();
        _ = try heap.collectGarbage(roots[0..]);
        const t1 = timer.read();
        ns.* = t1 - t0;
    }
    const bytes_copied1 = heap.stats.bytes_copied;
    const gc1 = heap.stats.gc_count;
    const build_ns1 = heap.stats.gc_build_ns;
    const root_ns1 = heap.stats.gc_root_ns;
    const copy_ns1 = heap.stats.gc_copy_ns;
    const finalize_ns1 = heap.stats.gc_finalize_ns;
    const root_vals1 = heap.stats.gc_root_vals;
    const promoted1 = heap.stats.gc_promoted_bytes;
    const wb1 = heap.stats.wb_marks;

    var sum: u128 = 0;
    for (pauses) |ns| sum += ns;
    const avg_ns: u64 = @intCast(sum / pauses.len);

    std.sort.heap(u64, pauses, {}, lessU64);
    const p95_idx = (pauses.len - 1) * 95 / 100;
    const p95_ns = pauses[p95_idx];

    const copied_delta = bytes_copied1 - bytes_copied0;
    const gc_delta = gc1 - gc0;
    const build_delta = build_ns1 - build_ns0;
    const root_delta = root_ns1 - root_ns0;
    const copy_delta = copy_ns1 - copy_ns0;
    const finalize_delta = finalize_ns1 - finalize_ns0;
    const root_vals_delta = root_vals1 - root_vals0;
    const promoted_delta = promoted1 - promoted0;
    const wb_delta = wb1 - wb0;
    const tenured_live = heap.tenured_objs.items.len;
    const los_live = heap.los_objs.items.len;
    const tenured_bytes = heap.tenuredBytesUsed();
    const los_bytes = heap.losBytesUsed();
    const live_bytes = heap.bytesUsed();
    const gc_delta_u64: u64 = @intCast(gc_delta);
    const avg_build_ns = if (gc_delta_u64 == 0) 0 else build_delta / gc_delta_u64;
    const avg_root_ns = if (gc_delta_u64 == 0) 0 else root_delta / gc_delta_u64;
    const avg_copy_ns = if (gc_delta_u64 == 0) 0 else copy_delta / gc_delta_u64;
    const avg_finalize_ns = if (gc_delta_u64 == 0) 0 else finalize_delta / gc_delta_u64;

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        try w.print(
            "{{\"iters\":{d},\"heap_bytes\":{d},\"live_bytes\":{d},\"avg_pause_ns\":{d},\"p95_pause_ns\":{d},\"gc_count\":{d},\"bytes_copied\":{d},\"avg_build_ns\":{d},\"avg_root_ns\":{d},\"avg_copy_ns\":{d},\"avg_finalize_ns\":{d},\"root_vals\":{d},\"promoted_bytes\":{d},\"wb_marks\":{d},\"tenured_live\":{d},\"los_live\":{d},\"tenured_bytes\":{d},\"los_bytes\":{d}}}\n",
            .{ opts.iters, heap_bytes, live_bytes, avg_ns, p95_ns, gc_delta, copied_delta, avg_build_ns, avg_root_ns, avg_copy_ns, avg_finalize_ns, root_vals_delta, promoted_delta, wb_delta, tenured_live, los_live, tenured_bytes, los_bytes },
        );
        try w.flush();
        return;
    }

    const avg_ms = @as(f64, @floatFromInt(avg_ns)) / 1e6;
    const p95_ms = @as(f64, @floatFromInt(p95_ns)) / 1e6;
    const live_mb = @as(f64, @floatFromInt(live_bytes)) / (1024.0 * 1024.0);
    const copied_mb = @as(f64, @floatFromInt(copied_delta)) / (1024.0 * 1024.0);
    const gc_delta_f: f64 = @floatFromInt(gc_delta);

    try w.print("GC benchmark\n", .{});
    try w.print("  heap: {d} MiB (semispace {d} MiB)\n", .{ opts.heap_mb, semispace / (1024 * 1024) });
    try w.print("  live: {d:.2} MiB\n", .{live_mb});
    try w.print("  iters: {d}\n", .{opts.iters});
    try w.print("  pause: avg {d:.3} ms, p95 {d:.3} ms\n", .{ avg_ms, p95_ms });
    try w.print("  copied: {d:.2} MiB total ({d:.2} MiB/GC)\n", .{ copied_mb, copied_mb / gc_delta_f });
    try w.print(
        "  phase avg (us): build {d:.2}, root {d:.2}, copy {d:.2}, finalize {d:.2}\n",
        .{
            @as(f64, @floatFromInt(avg_build_ns)) / 1000.0,
            @as(f64, @floatFromInt(avg_root_ns)) / 1000.0,
            @as(f64, @floatFromInt(avg_copy_ns)) / 1000.0,
            @as(f64, @floatFromInt(avg_finalize_ns)) / 1000.0,
        },
    );
    try w.print("  root vals visited: {d}\n", .{root_vals_delta});
    try w.print("  promoted bytes: {d}\n", .{promoted_delta});
    try w.print("  write-barrier marks: {d}\n", .{wb_delta});
    try w.print("  tenured live/bytes: {d} / {d}\n", .{ tenured_live, tenured_bytes });
    try w.print("  los live/bytes: {d} / {d}\n", .{ los_live, los_bytes });
    try w.print("  gc_count: {d}\n", .{gc_delta});
    try w.flush();
}
