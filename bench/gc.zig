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
    const gc_minor0 = heap.stats.gc_minor_count;
    const gc_major0 = heap.stats.gc_major_count;
    const minor_ns0 = heap.stats.gc_minor_ns;
    const major_ns0 = heap.stats.gc_major_ns;
    const build_ns0 = heap.stats.gc_build_ns;
    const root_ns0 = heap.stats.gc_root_ns;
    const copy_ns0 = heap.stats.gc_copy_ns;
    const finalize_ns0 = heap.stats.gc_finalize_ns;
    const root_vals0 = heap.stats.gc_root_vals;
    const remembered_scanned0 = heap.stats.gc_remembered_scanned;
    const remembered_runs0 = heap.stats.gc_remembered_runs;
    const remembered_cards0 = heap.stats.gc_remembered_marked_cards;
    const promoted0 = heap.stats.gc_promoted_bytes;
    const wb0 = heap.stats.wb_marks;
    const sample_n0 = heap.stats.alloc_sample_n;
    const sample_bytes0 = heap.stats.alloc_sample_bytes;
    const sample_class0 = heap.stats.alloc_sample_class;
    const sample_size0 = heap.stats.alloc_sample_size;
    const survive_n0 = heap.stats.gc_survive_n;
    const survive_bytes0 = heap.stats.gc_survive_bytes;
    const survive_class0 = heap.stats.gc_survive_class;
    const survive_size0 = heap.stats.gc_survive_size;
    const survive_age0 = heap.stats.gc_survive_age;
    const promote_n0 = heap.stats.gc_promote_n;
    const promote_bytes0 = heap.stats.gc_promote_bytes;
    const promote_class0 = heap.stats.gc_promote_class;
    const promote_size0 = heap.stats.gc_promote_size;
    const promote_age0 = heap.stats.gc_promote_age;
    const promote_success_n0 = heap.stats.gc_promote_success_n;
    const promote_success_bytes0 = heap.stats.gc_promote_success_bytes;
    const promote_success_class0 = heap.stats.gc_promote_success_class;
    const promote_success_age0 = heap.stats.gc_promote_success_age;
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
    const gc_minor1 = heap.stats.gc_minor_count;
    const gc_major1 = heap.stats.gc_major_count;
    const minor_ns1 = heap.stats.gc_minor_ns;
    const major_ns1 = heap.stats.gc_major_ns;
    const build_ns1 = heap.stats.gc_build_ns;
    const root_ns1 = heap.stats.gc_root_ns;
    const copy_ns1 = heap.stats.gc_copy_ns;
    const finalize_ns1 = heap.stats.gc_finalize_ns;
    const root_vals1 = heap.stats.gc_root_vals;
    const remembered_scanned1 = heap.stats.gc_remembered_scanned;
    const remembered_runs1 = heap.stats.gc_remembered_runs;
    const remembered_cards1 = heap.stats.gc_remembered_marked_cards;
    const promoted1 = heap.stats.gc_promoted_bytes;
    const wb1 = heap.stats.wb_marks;
    const sample_n1 = heap.stats.alloc_sample_n;
    const sample_bytes1 = heap.stats.alloc_sample_bytes;
    const sample_class1 = heap.stats.alloc_sample_class;
    const sample_size1 = heap.stats.alloc_sample_size;
    const survive_n1 = heap.stats.gc_survive_n;
    const survive_bytes1 = heap.stats.gc_survive_bytes;
    const survive_class1 = heap.stats.gc_survive_class;
    const survive_size1 = heap.stats.gc_survive_size;
    const survive_age1 = heap.stats.gc_survive_age;
    const promote_n1 = heap.stats.gc_promote_n;
    const promote_bytes1 = heap.stats.gc_promote_bytes;
    const promote_class1 = heap.stats.gc_promote_class;
    const promote_size1 = heap.stats.gc_promote_size;
    const promote_age1 = heap.stats.gc_promote_age;
    const promote_success_n1 = heap.stats.gc_promote_success_n;
    const promote_success_bytes1 = heap.stats.gc_promote_success_bytes;
    const promote_success_class1 = heap.stats.gc_promote_success_class;
    const promote_success_age1 = heap.stats.gc_promote_success_age;

    var sum: u128 = 0;
    for (pauses) |ns| sum += ns;
    const avg_ns: u64 = @intCast(sum / pauses.len);

    std.sort.heap(u64, pauses, {}, lessU64);
    const p50_idx = (pauses.len - 1) * 50 / 100;
    const p95_idx = (pauses.len - 1) * 95 / 100;
    const p99_idx = (pauses.len - 1) * 99 / 100;
    const p50_ns = pauses[p50_idx];
    const p95_ns = pauses[p95_idx];
    const p99_ns = pauses[p99_idx];

    const copied_delta = bytes_copied1 - bytes_copied0;
    const gc_delta = gc1 - gc0;
    const gc_minor_delta = gc_minor1 - gc_minor0;
    const gc_major_delta = gc_major1 - gc_major0;
    const minor_ns_delta = minor_ns1 - minor_ns0;
    const major_ns_delta = major_ns1 - major_ns0;
    const build_delta = build_ns1 - build_ns0;
    const root_delta = root_ns1 - root_ns0;
    const copy_delta = copy_ns1 - copy_ns0;
    const finalize_delta = finalize_ns1 - finalize_ns0;
    const root_vals_delta = root_vals1 - root_vals0;
    const remembered_scanned_delta = remembered_scanned1 - remembered_scanned0;
    const remembered_runs_delta = remembered_runs1 - remembered_runs0;
    const remembered_cards_delta = remembered_cards1 - remembered_cards0;
    const promoted_delta = promoted1 - promoted0;
    const wb_delta = wb1 - wb0;
    const sample_n_delta = sample_n1 - sample_n0;
    const sample_bytes_delta = sample_bytes1 - sample_bytes0;
    var sample_class_delta: @TypeOf(heap.stats.alloc_sample_class) = undefined;
    for (&sample_class_delta, 0..) |*dst, i| {
        dst.* = sample_class1[i] - sample_class0[i];
    }
    var sample_size_delta: @TypeOf(heap.stats.alloc_sample_size) = undefined;
    for (&sample_size_delta, 0..) |*dst, i| {
        dst.* = sample_size1[i] - sample_size0[i];
    }
    const survive_n_delta = survive_n1 - survive_n0;
    const survive_bytes_delta = survive_bytes1 - survive_bytes0;
    var survive_class_delta: @TypeOf(heap.stats.gc_survive_class) = undefined;
    for (&survive_class_delta, 0..) |*dst, i| {
        dst.* = survive_class1[i] - survive_class0[i];
    }
    var survive_size_delta: @TypeOf(heap.stats.gc_survive_size) = undefined;
    for (&survive_size_delta, 0..) |*dst, i| {
        dst.* = survive_size1[i] - survive_size0[i];
    }
    var survive_age_delta: @TypeOf(heap.stats.gc_survive_age) = undefined;
    for (&survive_age_delta, 0..) |*dst, i| {
        dst.* = survive_age1[i] - survive_age0[i];
    }
    const promote_n_delta = promote_n1 - promote_n0;
    const promote_bytes_delta = promote_bytes1 - promote_bytes0;
    var promote_class_delta: @TypeOf(heap.stats.gc_promote_class) = undefined;
    for (&promote_class_delta, 0..) |*dst, i| {
        dst.* = promote_class1[i] - promote_class0[i];
    }
    var promote_size_delta: @TypeOf(heap.stats.gc_promote_size) = undefined;
    for (&promote_size_delta, 0..) |*dst, i| {
        dst.* = promote_size1[i] - promote_size0[i];
    }
    var promote_age_delta: @TypeOf(heap.stats.gc_promote_age) = undefined;
    for (&promote_age_delta, 0..) |*dst, i| {
        dst.* = promote_age1[i] - promote_age0[i];
    }
    const promote_success_n_delta = promote_success_n1 - promote_success_n0;
    const promote_success_bytes_delta = promote_success_bytes1 - promote_success_bytes0;
    var promote_success_class_delta: @TypeOf(heap.stats.gc_promote_success_class) = undefined;
    for (&promote_success_class_delta, 0..) |*dst, i| {
        dst.* = promote_success_class1[i] - promote_success_class0[i];
    }
    var promote_success_age_delta: @TypeOf(heap.stats.gc_promote_success_age) = undefined;
    for (&promote_success_age_delta, 0..) |*dst, i| {
        dst.* = promote_success_age1[i] - promote_success_age0[i];
    }
    const tenured_live = heap.tenured_objs.items.len;
    const los_live = heap.los_objs.items.len;
    const tenured_bytes = heap.tenuredBytesUsed();
    const los_bytes = heap.losBytesUsed();
    const live_bytes = heap.bytesUsed();
    const gc_delta_u64: u64 = @intCast(gc_delta);
    const gc_minor_delta_u64: u64 = @intCast(gc_minor_delta);
    const gc_major_delta_u64: u64 = @intCast(gc_major_delta);
    const avg_minor_ns = if (gc_minor_delta_u64 == 0) 0 else minor_ns_delta / gc_minor_delta_u64;
    const avg_major_ns = if (gc_major_delta_u64 == 0) 0 else major_ns_delta / gc_major_delta_u64;
    const avg_build_ns = if (gc_delta_u64 == 0) 0 else build_delta / gc_delta_u64;
    const avg_root_ns = if (gc_delta_u64 == 0) 0 else root_delta / gc_delta_u64;
    const avg_copy_ns = if (gc_delta_u64 == 0) 0 else copy_delta / gc_delta_u64;
    const avg_finalize_ns = if (gc_delta_u64 == 0) 0 else finalize_delta / gc_delta_u64;
    const nursery_target = heap.stats.gc_nursery_target;
    const nursery_scale = heap.stats.gc_nursery_scale;
    const nursery_survival = heap.stats.gc_nursery_survival;
    const nursery_pause_error = heap.stats.gc_nursery_pause_error;
    const promote_threshold = heap.stats.gc_promote_threshold;
    const promote_threshold_min = heap.stats.gc_promote_threshold_min;
    const promote_threshold_max = heap.stats.gc_promote_threshold_max;
    const promote_scale = heap.stats.gc_promote_scale;
    const promote_success_rate = heap.stats.gc_promote_success_rate;
    const promote_young_ratio = heap.stats.gc_promote_young_ratio;
    const promote_mature_ratio = heap.stats.gc_promote_mature_ratio;
    const sample_cons = sample_class_delta[@intFromEnum(heap_mod.AllocClass.cons)];
    const sample_symbol = sample_class_delta[@intFromEnum(heap_mod.AllocClass.symbol)];
    const sample_keyword = sample_class_delta[@intFromEnum(heap_mod.AllocClass.keyword)];
    const sample_vector = sample_class_delta[@intFromEnum(heap_mod.AllocClass.vector)];
    const sample_array = sample_class_delta[@intFromEnum(heap_mod.AllocClass.array)];
    const sample_string = sample_class_delta[@intFromEnum(heap_mod.AllocClass.string)];
    const sample_closure = sample_class_delta[@intFromEnum(heap_mod.AllocClass.closure)];
    const sample_stream = sample_class_delta[@intFromEnum(heap_mod.AllocClass.stream)];
    const sample_hash = sample_class_delta[@intFromEnum(heap_mod.AllocClass.hash_table)];
    const sample_chunk = sample_class_delta[@intFromEnum(heap_mod.AllocClass.chunk)];
    const sample_other = sample_class_delta[@intFromEnum(heap_mod.AllocClass.other)];

    var out_buf: [4096]u8 = undefined;
    var out = std.fs.File.stdout().writer(&out_buf);
    const w = &out.interface;

    if (opts.json) {
        const payload = .{
            .iters = opts.iters,
            .heap_bytes = heap_bytes,
            .live_bytes = live_bytes,
            .avg_pause_ns = avg_ns,
            .p50_pause_ns = p50_ns,
            .p95_pause_ns = p95_ns,
            .p99_pause_ns = p99_ns,
            .gc_count = gc_delta,
            .gc_minor_count = gc_minor_delta,
            .gc_major_count = gc_major_delta,
            .bytes_copied = copied_delta,
            .avg_minor_ns = avg_minor_ns,
            .avg_major_ns = avg_major_ns,
            .avg_build_ns = avg_build_ns,
            .avg_root_ns = avg_root_ns,
            .avg_copy_ns = avg_copy_ns,
            .avg_finalize_ns = avg_finalize_ns,
            .root_vals = root_vals_delta,
            .gc_remembered_scanned = remembered_scanned_delta,
            .gc_remembered_runs = remembered_runs_delta,
            .gc_remembered_marked_cards = remembered_cards_delta,
            .promoted_bytes = promoted_delta,
            .wb_marks = wb_delta,
            .tenured_live = tenured_live,
            .los_live = los_live,
            .tenured_bytes = tenured_bytes,
            .los_bytes = los_bytes,
            .alloc_sample_n = sample_n_delta,
            .alloc_sample_bytes = sample_bytes_delta,
            .alloc_sample_cons = sample_cons,
            .alloc_sample_symbol = sample_symbol,
            .alloc_sample_keyword = sample_keyword,
            .alloc_sample_vector = sample_vector,
            .alloc_sample_array = sample_array,
            .alloc_sample_string = sample_string,
            .alloc_sample_closure = sample_closure,
            .alloc_sample_stream = sample_stream,
            .alloc_sample_hash_table = sample_hash,
            .alloc_sample_chunk = sample_chunk,
            .alloc_sample_other = sample_other,
            .alloc_sample_size = sample_size_delta,
            .gc_survive_n = survive_n_delta,
            .gc_survive_bytes = survive_bytes_delta,
            .gc_survive_class = survive_class_delta,
            .gc_survive_size = survive_size_delta,
            .gc_survive_age = survive_age_delta,
            .gc_promote_n = promote_n_delta,
            .gc_promote_bytes = promote_bytes_delta,
            .gc_promote_class = promote_class_delta,
            .gc_promote_size = promote_size_delta,
            .gc_promote_age = promote_age_delta,
            .gc_promote_success_n = promote_success_n_delta,
            .gc_promote_success_bytes = promote_success_bytes_delta,
            .gc_promote_success_class = promote_success_class_delta,
            .gc_promote_success_age = promote_success_age_delta,
            .gc_promote_threshold = promote_threshold,
            .gc_promote_threshold_min = promote_threshold_min,
            .gc_promote_threshold_max = promote_threshold_max,
            .gc_promote_scale = promote_scale,
            .gc_promote_success_rate = promote_success_rate,
            .gc_promote_young_ratio = promote_young_ratio,
            .gc_promote_mature_ratio = promote_mature_ratio,
            .gc_nursery_target = nursery_target,
            .gc_nursery_scale = nursery_scale,
            .gc_nursery_survival = nursery_survival,
            .gc_nursery_pause_error = nursery_pause_error,
        };
        try std.json.Stringify.value(payload, .{}, w);
        try w.writeByte('\n');
        try w.flush();
        return;
    }

    const avg_ms = @as(f64, @floatFromInt(avg_ns)) / 1e6;
    const p50_ms = @as(f64, @floatFromInt(p50_ns)) / 1e6;
    const p95_ms = @as(f64, @floatFromInt(p95_ns)) / 1e6;
    const p99_ms = @as(f64, @floatFromInt(p99_ns)) / 1e6;
    const live_mb = @as(f64, @floatFromInt(live_bytes)) / (1024.0 * 1024.0);
    const copied_mb = @as(f64, @floatFromInt(copied_delta)) / (1024.0 * 1024.0);
    const gc_delta_f: f64 = @floatFromInt(gc_delta);

    try w.print("GC benchmark\n", .{});
    try w.print("  heap: {d} MiB (semispace {d} MiB)\n", .{ opts.heap_mb, semispace / (1024 * 1024) });
    try w.print("  live: {d:.2} MiB\n", .{live_mb});
    try w.print("  iters: {d}\n", .{opts.iters});
    try w.print("  pause: avg {d:.3} ms, p50 {d:.3} ms, p95 {d:.3} ms, p99 {d:.3} ms\n", .{ avg_ms, p50_ms, p95_ms, p99_ms });
    try w.print("  copied: {d:.2} MiB total ({d:.2} MiB/GC)\n", .{ copied_mb, copied_mb / gc_delta_f });
    try w.print("  mode avg (us): minor {d:.2}, major {d:.2}\n", .{
        @as(f64, @floatFromInt(avg_minor_ns)) / 1000.0,
        @as(f64, @floatFromInt(avg_major_ns)) / 1000.0,
    });
    try w.print("  alloc samples: {d} ({d} bytes sampled)\n", .{ sample_n_delta, sample_bytes_delta });
    try w.print(
        "  alloc hot classes: cons {d}, vec {d}, str {d}, sym {d}, hash {d}, other {d}\n",
        .{ sample_cons, sample_vector, sample_string, sample_symbol, sample_hash, sample_other },
    );
    try w.print(
        "  survival: n {d}, bytes {d}, promoted n {d}, bytes {d}\n",
        .{ survive_n_delta, survive_bytes_delta, promote_n_delta, promote_bytes_delta },
    );
    try w.print(
        "  promotion success: n {d}, bytes {d}\n",
        .{ promote_success_n_delta, promote_success_bytes_delta },
    );
    try w.print(
        "  promote policy: threshold {d} [{d},{d}] scale {d:.4} success {d:.4} young {d:.4} mature {d:.4}\n",
        .{
            promote_threshold,
            promote_threshold_min,
            promote_threshold_max,
            promote_scale,
            promote_success_rate,
            promote_young_ratio,
            promote_mature_ratio,
        },
    );
    try w.print(
        "  nursery policy: target {d} bytes, scale {d:.4}, survival {d:.4}, pause_err {d:.4}\n",
        .{ nursery_target, nursery_scale, nursery_survival, nursery_pause_error },
    );
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
    try w.print(
        "  remembered scan: scanned {d}, runs {d}, marked_cards {d}\n",
        .{ remembered_scanned_delta, remembered_runs_delta, remembered_cards_delta },
    );
    try w.print("  promoted bytes: {d}\n", .{promoted_delta});
    try w.print("  write-barrier marks: {d}\n", .{wb_delta});
    try w.print("  tenured live/bytes: {d} / {d}\n", .{ tenured_live, tenured_bytes });
    try w.print("  los live/bytes: {d} / {d}\n", .{ los_live, los_bytes });
    try w.print("  gc_count: {d}\n", .{gc_delta});
    try w.flush();
}
