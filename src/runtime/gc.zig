//! Cheney Copying Garbage Collector
//!
//! Algorithm:
//! 1. Copy roots to to-space, building work list
//! 2. Process work list, copying referenced objects
//! 3. Replace old pointers with forwarding pointers
//! 4. Swap spaces
//!
//! Forwarding pointers use tag 14 to mark already-copied objects.
//!
//! Work-list approach: Instead of sequential scanning (which requires knowing
//! object types), we maintain a list of (address, tag) pairs to process.

const std = @import("std");
const Value = @import("value.zig").Value;
const Tag = @import("value.zig").Tag;
const objects = @import("objects.zig");
const heap_mod = @import("heap.zig");
const roots_mod = @import("roots.zig");

const OriginKind = enum(u8) {
    none,
    range,
    slot,
};

/// Work item: object to scan
const WorkItem = struct {
    addr: usize,
    tag: Tag,
    parent_addr: usize,
    parent_tag: Tag,
    grand_addr: usize,
    grand_tag: Tag,
    origin_kind: OriginKind,
    origin_a: usize,
    origin_b: usize,
};

const builtin = @import("builtin");

fn elapsedNsSince(start_ns: i128) u64 {
    const now_ns = std.time.nanoTimestamp();
    if (now_ns <= start_ns) return 0;
    return @intCast(now_ns - start_ns);
}

pub const NurseryPolicyInput = struct {
    current_bytes: usize,
    min_bytes: usize,
    max_bytes: usize,
    survive_bytes: usize,
    copied_bytes: usize,
    p95_pause_ns: u64,
    target_pause_ns: u64,
};

pub const NurseryPolicyOutput = struct {
    target_bytes: usize,
    survival_ratio: f64,
    pause_error: f64,
    scale: f64,
};

pub const TenuringPolicyInput = struct {
    current_bytes: usize,
    min_bytes: usize,
    max_bytes: usize,
    promote_n: usize,
    promote_success_n: usize,
    promote_age: [heap_mod.GC_AGE_N]usize,
    survive_n: usize,
    survive_age: [heap_mod.GC_AGE_N]usize,
};

pub const TenuringPolicyOutput = struct {
    target_bytes: usize,
    scale: f64,
    success_rate: f64,
    young_promote_ratio: f64,
    mature_survive_ratio: f64,
};

pub const LosPolicyInput = struct {
    current_bytes: usize,
    min_bytes: usize,
    max_bytes: usize,
    alloc_size: [heap_mod.ALLOC_SIZE_N]usize,
    los_live_bytes: usize,
    los_capacity_bytes: usize,
    p95_pause_ns: u64,
    target_pause_ns: u64,
};

pub const LosPolicyOutput = struct {
    target_bytes: usize,
    scale: f64,
    large_alloc_ratio: f64,
    occupancy_ratio: f64,
    pause_error: f64,
};

pub const DebtTriggerInput = struct {
    debt_bytes: usize,
    debt_threshold: usize,
    nursery_used_bytes: usize,
    nursery_target_bytes: usize,
    survival_ratio: f64,
    pause_error: f64,
};

pub const DebtTriggerOutput = struct {
    should_collect: bool,
    score: f64,
    debt_ratio: f64,
    occupancy_ratio: f64,
    survival_ratio: f64,
    pause_error: f64,
};

const DEBT_SCORE_DEBT_W = 0.70;
const DEBT_SCORE_OCC_W = 0.20;
const DEBT_SCORE_SURV_W = 0.10;
const DEBT_SCORE_PAUSE_PENALTY = 0.20;
const DEBT_TRIGGER_HARD_RATIO = 1.25;
const DEBT_TRIGGER_MAIN_RATIO = 1.00;
const DEBT_TRIGGER_MAIN_SCORE = 0.65;
const DEBT_TRIGGER_SOFT_RATIO = 0.85;
const DEBT_TRIGGER_SOFT_SCORE = 0.95;
pub const MAJOR_MARK_BUDGET_OBJS: usize = 512;
pub const MAJOR_SWEEP_BUDGET_OBJS: usize = 1024;
pub const PROMOTE_AGE_THRESHOLD: u8 = 2;

const MajorPhase = enum {
    idle,
    mark,
    sweep_tenured,
    sweep_los,
};

const MajorState = struct {
    phase: MajorPhase = .idle,
    tenured_cursor: usize = 0,
    los_cursor: usize = 0,
};

fn clampf(v: f64, lo: f64, hi: f64) f64 {
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
}

fn counterDelta(comptime T: type, after: T, before: T) T {
    return after -% before;
}

/// Derive next nursery target from measured survival and pause behavior.
/// Control law goals:
/// - shrink when survival is high or pause exceeds target,
/// - grow when survival is low and pause is comfortably below target,
/// - avoid oscillation via deadband and bounded scale.
pub fn deriveNurseryPolicy(in: NurseryPolicyInput) NurseryPolicyOutput {
    const copied = if (in.copied_bytes == 0) @as(usize, 1) else in.copied_bytes;
    const pause_target = if (in.target_pause_ns == 0) @as(u64, 1) else in.target_pause_ns;

    const survive_f = @as(f64, @floatFromInt(in.survive_bytes));
    const copied_f = @as(f64, @floatFromInt(copied));
    const survival_ratio = clampf(survive_f / copied_f, 0.0, 1.5);

    const pause_f = @as(f64, @floatFromInt(in.p95_pause_ns));
    const pause_target_f = @as(f64, @floatFromInt(pause_target));
    const pause_error = (pause_f - pause_target_f) / pause_target_f;

    const shrink_scale = 1.0 - 0.60 * survival_ratio - 0.35 * @max(0.0, pause_error);
    const grow_scale = 1.0 + 0.30 * @max(0.0, 0.25 - survival_ratio) + 0.15 * @max(0.0, -pause_error);
    var scale = if (survival_ratio > 0.25 or pause_error > 0.0) shrink_scale else grow_scale;
    scale = clampf(scale, 0.50, 1.50);

    var target = @as(usize, @intFromFloat(@as(f64, @floatFromInt(in.current_bytes)) * scale));
    if (in.current_bytes > 0) {
        const delta = if (target >= in.current_bytes) target - in.current_bytes else in.current_bytes - target;
        if (delta * 100 <= in.current_bytes * 5) target = in.current_bytes;
    }

    if (target < in.min_bytes) target = in.min_bytes;
    if (target > in.max_bytes) target = in.max_bytes;
    target = std.mem.alignForward(usize, target, heap_mod.ALIGNMENT);

    return .{
        .target_bytes = target,
        .survival_ratio = survival_ratio,
        .pause_error = pause_error,
        .scale = scale,
    };
}

/// Derive next promotion threshold from promotion quality and survivor age mix.
/// Control law goals:
/// - raise threshold when many young promotions fail quickly,
/// - lower threshold when mature survivors keep recopying in nursery,
/// - keep threshold stable via deadband and bounded scale.
pub fn deriveTenuringPolicy(in: TenuringPolicyInput) TenuringPolicyOutput {
    const promote_n_f = @as(f64, @floatFromInt(@max(in.promote_n, @as(usize, 1))));
    const success_f = @as(f64, @floatFromInt(in.promote_success_n));
    const success_rate = clampf(success_f / promote_n_f, 0.0, 1.0);

    const young_promoted = in.promote_age[0] + in.promote_age[1] + in.promote_age[2] + in.promote_age[3];
    const young_ratio = if (in.promote_n == 0)
        0.0
    else
        clampf(@as(f64, @floatFromInt(young_promoted)) / @as(f64, @floatFromInt(in.promote_n)), 0.0, 1.0);

    const mature_survive = in.survive_age[4] + in.survive_age[5] + in.survive_age[6] + in.survive_age[7];
    const mature_ratio = if (in.survive_n == 0)
        0.0
    else
        clampf(@as(f64, @floatFromInt(mature_survive)) / @as(f64, @floatFromInt(in.survive_n)), 0.0, 1.0);

    var scale: f64 = 1.0;
    if (in.promote_n > 0) {
        if (success_rate < 0.25 or young_ratio > 0.60) {
            scale = 1.20;
        } else if (success_rate > 0.70 and mature_ratio > 0.20) {
            scale = 0.85;
        }
    } else if (mature_ratio > 0.30 and in.survive_n > 0) {
        scale = 0.90;
    }
    scale = clampf(scale, 0.50, 1.50);

    var target = @as(usize, @intFromFloat(@as(f64, @floatFromInt(in.current_bytes)) * scale));
    if (in.current_bytes > 0) {
        const delta = if (target >= in.current_bytes) target - in.current_bytes else in.current_bytes - target;
        if (delta * 100 <= in.current_bytes * 6) target = in.current_bytes;
    }

    if (target < in.min_bytes) target = in.min_bytes;
    if (target > in.max_bytes) target = in.max_bytes;
    target = std.mem.alignForward(usize, target, heap_mod.ALIGNMENT);

    return .{
        .target_bytes = target,
        .scale = scale,
        .success_rate = success_rate,
        .young_promote_ratio = young_ratio,
        .mature_survive_ratio = mature_ratio,
    };
}

/// Derive next LOS threshold from sampled allocation sizes and LOS pressure.
/// Control law goals:
/// - lower threshold when large-object share rises and LOS has headroom,
/// - raise threshold when LOS occupancy or pauses are high,
/// - bound movement to avoid oscillation.
pub fn deriveLosPolicy(in: LosPolicyInput) LosPolicyOutput {
    const sample_total = blk: {
        var n: usize = 0;
        for (in.alloc_size) |v| n +%= v;
        break :blk n;
    };
    const sample_total_f = @as(f64, @floatFromInt(@max(sample_total, @as(usize, 1))));
    const large_samples = in.alloc_size[6] + in.alloc_size[7];
    const large_ratio = clampf(@as(f64, @floatFromInt(large_samples)) / sample_total_f, 0.0, 1.0);

    const los_cap = @max(in.los_capacity_bytes, @as(usize, 1));
    const occupancy = clampf(
        @as(f64, @floatFromInt(in.los_live_bytes)) / @as(f64, @floatFromInt(los_cap)),
        0.0,
        2.0,
    );

    const pause_target = @max(in.target_pause_ns, @as(u64, 1));
    const pause_error = clampf(
        (@as(f64, @floatFromInt(in.p95_pause_ns)) - @as(f64, @floatFromInt(pause_target))) /
            @as(f64, @floatFromInt(pause_target)),
        -1.0,
        2.0,
    );

    var scale: f64 = 1.0;
    if (occupancy > 0.90 or pause_error > 0.35) {
        scale = 1.20;
    } else if (large_ratio > 0.35 and occupancy < 0.70 and pause_error <= 0.15) {
        scale = 0.75;
    } else if (large_ratio > 0.20 and occupancy < 0.85 and pause_error <= 0.20) {
        scale = 0.85;
    } else if (large_ratio < 0.05 and occupancy > 0.60) {
        scale = 1.10;
    }
    scale = clampf(scale, 0.50, 1.50);

    var target = @as(usize, @intFromFloat(@as(f64, @floatFromInt(in.current_bytes)) * scale));
    if (in.current_bytes > 0) {
        const delta = if (target >= in.current_bytes) target - in.current_bytes else in.current_bytes - target;
        if (delta * 100 <= in.current_bytes * 5) target = in.current_bytes;
    }
    if (target < in.min_bytes) target = in.min_bytes;
    if (target > in.max_bytes) target = in.max_bytes;
    target = std.mem.alignForward(usize, target, heap_mod.ALIGNMENT);

    return .{
        .target_bytes = target,
        .scale = scale,
        .large_alloc_ratio = large_ratio,
        .occupancy_ratio = occupancy,
        .pause_error = pause_error,
    };
}

/// Derive whether allocation debt should trigger a pre-collection.
/// Control law goals:
/// - trigger early when debt exceeds budget and occupancy is rising,
/// - back off when recent pauses are already over budget,
/// - hard-trigger for extreme debt regardless of pause state.
pub fn deriveDebtTrigger(in: DebtTriggerInput) DebtTriggerOutput {
    const threshold = @max(in.debt_threshold, @as(usize, 1));
    const target = @max(in.nursery_target_bytes, @as(usize, 1));
    const debt_ratio = clampf(
        @as(f64, @floatFromInt(in.debt_bytes)) / @as(f64, @floatFromInt(threshold)),
        0.0,
        4.0,
    );
    const occupancy_ratio = clampf(
        @as(f64, @floatFromInt(in.nursery_used_bytes)) / @as(f64, @floatFromInt(target)),
        0.0,
        2.0,
    );
    const survival_ratio = clampf(in.survival_ratio, 0.0, 1.5);
    const pause_error = clampf(in.pause_error, -1.0, 2.0);

    var score = DEBT_SCORE_DEBT_W * debt_ratio +
        DEBT_SCORE_OCC_W * occupancy_ratio +
        DEBT_SCORE_SURV_W * survival_ratio;
    if (pause_error > 0.0) score -= DEBT_SCORE_PAUSE_PENALTY * @min(pause_error, 1.0);
    score = clampf(score, 0.0, 4.0);

    const should_collect = debt_ratio >= DEBT_TRIGGER_HARD_RATIO or
        (debt_ratio >= DEBT_TRIGGER_MAIN_RATIO and score >= DEBT_TRIGGER_MAIN_SCORE) or
        (debt_ratio >= DEBT_TRIGGER_SOFT_RATIO and score >= DEBT_TRIGGER_SOFT_SCORE);
    return .{
        .should_collect = should_collect,
        .score = score,
        .debt_ratio = debt_ratio,
        .occupancy_ratio = occupancy_ratio,
        .survival_ratio = survival_ratio,
        .pause_error = pause_error,
    };
}

/// Garbage collector state
pub const GC = struct {
    /// Allocator for work list
    allocator: std.mem.Allocator,
    /// Work list of objects to scan (preallocated, reused across collections)
    work_list: std.ArrayList(WorkItem),
    /// Peak work queue size seen during the most recent collection.
    work_peak: usize,
    /// Survivor age updates collected during copy (preallocated, reused).
    age_updates: std.ArrayList(heap_mod.Heap.SurvivorAgeEntry),
    age_peak: usize,
    /// Coalesced remembered-card runs for minor-GC fast-path scans.
    remembered_runs: std.ArrayList(heap_mod.CardRun),
    runs_peak: usize,
    /// Persistent major-cycle work queue for incremental old-space marking.
    major_work: std.ArrayList(WorkItem),
    major_peak: usize,
    major: MajorState,
    prev_alloc_sample_size: [heap_mod.ALLOC_SIZE_N]usize,
    debug_scan_addr: usize,
    debug_scan_tag: Tag,
    debug_parent_addr: usize,
    debug_parent_tag: Tag,
    debug_grand_addr: usize,
    debug_grand_tag: Tag,
    debug_origin_kind: OriginKind,
    debug_origin_a: usize,
    debug_origin_b: usize,
    debug_roots_ranges: []const roots_mod.RootRange,
    debug_roots_slots: []const *Value,
    trace_bad_root: bool,
    trap_bad_root: bool,
    trace_stale_resolve: bool,
    trap_stale_resolve_reject: bool,
    trace_bad_keyword: bool,
    trap_bad_keyword: bool,
    trace_bad_symbol: bool,
    trace_gc_oom: bool,
    /// Debug: flag set during GC trace/copy phase
    gc_in_progress: if (builtin.mode == .Debug) bool else void,

    /// Initialize GC state
    pub fn init(allocator: std.mem.Allocator) GC {
        return .{
            .allocator = allocator,
            .work_list = std.ArrayList(WorkItem){},
            .work_peak = 0,
            .age_updates = std.ArrayList(heap_mod.Heap.SurvivorAgeEntry){},
            .age_peak = 0,
            .remembered_runs = std.ArrayList(heap_mod.CardRun){},
            .runs_peak = 0,
            .major_work = std.ArrayList(WorkItem){},
            .major_peak = 0,
            .major = .{},
            .prev_alloc_sample_size = [_]usize{0} ** heap_mod.ALLOC_SIZE_N,
            .debug_scan_addr = 0,
            .debug_scan_tag = .cons,
            .debug_parent_addr = 0,
            .debug_parent_tag = .cons,
            .debug_grand_addr = 0,
            .debug_grand_tag = .cons,
            .debug_origin_kind = .none,
            .debug_origin_a = 0,
            .debug_origin_b = 0,
            .debug_roots_ranges = &[_]roots_mod.RootRange{},
            .debug_roots_slots = &[_]*Value{},
            .trace_bad_root = std.posix.getenv("HABU_TRACE_BAD_ROOT") != null,
            .trap_bad_root = std.posix.getenv("HABU_TRAP_BAD_ROOT") != null,
            .trace_stale_resolve = std.posix.getenv("HABU_TRACE_STALE_RESOLVE") != null,
            .trap_stale_resolve_reject = std.posix.getenv("HABU_TRAP_STALE_RESOLVE_REJECT") != null,
            .trace_bad_keyword = std.posix.getenv("HABU_TRACE_GC_BAD_KEYWORD") != null,
            .trap_bad_keyword = std.posix.getenv("HABU_TRAP_BAD_KEYWORD") != null,
            .trace_bad_symbol = std.posix.getenv("HABU_TRACE_GC_BAD_SYMBOL") != null,
            .trace_gc_oom = std.posix.getenv("HABU_TRACE_GC_OOM") != null,
            .gc_in_progress = if (builtin.mode == .Debug) false else {},
        };
    }

    pub fn deinit(self: *GC) void {
        self.work_list.deinit(self.allocator);
        self.age_updates.deinit(self.allocator);
        self.remembered_runs.deinit(self.allocator);
        self.major_work.deinit(self.allocator);
    }

    /// Calculate initial capacity for work queues based on heap size
    /// Sizing: space_size / 64 as a heuristic (1.5% of semispace)
    fn calculateInitialCapacity(_: *const GC, heap: *const heap_mod.Heap) usize {
        const min_cap = 256;
        const cap = heap.space_size / 64;
        return @max(min_cap, cap);
    }

    /// Run a garbage collection cycle
    /// Returns the number of bytes copied, or error on OOM during work list allocation
    pub fn collect(self: *GC, heap: *heap_mod.Heap, roots: []Value) !usize {
        var ranges = [_]roots_mod.RootRange{.{ .ptr = roots.ptr, .len = roots.len }};
        return try self.collectRootSet(heap, .{
            .ranges = ranges[0..],
            .slots = &[_]*Value{},
        });
    }

    /// Run a garbage collection cycle with a precise root set (slot/range addresses).
    /// Returns the number of bytes copied, or error on OOM during work list allocation.
    pub fn collectRootSet(self: *GC, heap: *heap_mod.Heap, roots: roots_mod.RootSet) !usize {
        const pause_start = std.time.nanoTimestamp();
        const mode = heap.gcLayoutMode();
        const survive_before = heap.stats.gc_survive_bytes;
        const survive_n_before = heap.stats.gc_survive_n;
        const survive_age_before = heap.stats.gc_survive_age;
        const copied_before = heap.stats.bytes_copied;
        const promoted_before = heap.stats.gc_promoted_bytes;
        const promote_n_before = heap.stats.gc_promote_n;
        const promote_success_before = heap.stats.gc_promote_success_n;
        const promote_age_before = heap.stats.gc_promote_age;
        const copied = switch (mode) {
            .semispace => blk: {
                heap.setMajorCycleActive(false);
                break :blk try self.collectSemispaceRootSet(heap, roots);
            },
            .generational => try self.collectMinorRootSet(heap, roots),
        };
        const pause_ns = elapsedNsSince(pause_start);
        switch (mode) {
            .semispace => {
                heap.stats.gc_major_count +%= 1;
                heap.stats.gc_major_ns +%= pause_ns;
            },
            .generational => {
                heap.stats.gc_minor_count +%= 1;
                heap.stats.gc_minor_ns +%= pause_ns;
                const survive_cycle = counterDelta(usize, heap.stats.gc_survive_bytes, survive_before);
                const survive_n_cycle = counterDelta(usize, heap.stats.gc_survive_n, survive_n_before);
                const copied_cycle = counterDelta(usize, heap.stats.bytes_copied, copied_before);
                const promoted_cycle = counterDelta(usize, heap.stats.gc_promoted_bytes, promoted_before);
                const promote_n_cycle = counterDelta(usize, heap.stats.gc_promote_n, promote_n_before);
                const promote_success_cycle = counterDelta(usize, heap.stats.gc_promote_success_n, promote_success_before);
                var survive_age_cycle: [heap_mod.GC_AGE_N]usize = undefined;
                for (&survive_age_cycle, 0..) |*dst, i| {
                    dst.* = counterDelta(usize, heap.stats.gc_survive_age[i], survive_age_before[i]);
                }
                var promote_age_cycle: [heap_mod.GC_AGE_N]usize = undefined;
                for (&promote_age_cycle, 0..) |*dst, i| {
                    dst.* = counterDelta(usize, heap.stats.gc_promote_age[i], promote_age_before[i]);
                }
                var alloc_size_cycle: [heap_mod.ALLOC_SIZE_N]usize = undefined;
                for (&alloc_size_cycle, 0..) |*dst, i| {
                    dst.* = counterDelta(usize, heap.stats.alloc_sample_size[i], self.prev_alloc_sample_size[i]);
                    self.prev_alloc_sample_size[i] = heap.stats.alloc_sample_size[i];
                }
                const policy = deriveNurseryPolicy(.{
                    .current_bytes = heap.nursery_target_bytes,
                    .min_bytes = heap.nursery_min_bytes,
                    .max_bytes = heap.nursery_max_bytes,
                    .survive_bytes = survive_cycle,
                    .copied_bytes = copied_cycle +% promoted_cycle,
                    .p95_pause_ns = pause_ns,
                    .target_pause_ns = heap.nursery_target_pause_ns,
                });
                heap.setNurseryTarget(policy.target_bytes, policy.survival_ratio, policy.pause_error, policy.scale);
                const tenuring = deriveTenuringPolicy(.{
                    .current_bytes = heap.promote_threshold,
                    .min_bytes = heap.promote_threshold_min,
                    .max_bytes = heap.promote_threshold_max,
                    .promote_n = promote_n_cycle,
                    .promote_success_n = promote_success_cycle,
                    .promote_age = promote_age_cycle,
                    .survive_n = survive_n_cycle,
                    .survive_age = survive_age_cycle,
                });
                heap.setPromoteThreshold(
                    tenuring.target_bytes,
                    tenuring.scale,
                    tenuring.success_rate,
                    tenuring.young_promote_ratio,
                    tenuring.mature_survive_ratio,
                );
                const los_policy = deriveLosPolicy(.{
                    .current_bytes = heap.los_threshold,
                    .min_bytes = heap.los_threshold_min,
                    .max_bytes = heap.los_threshold_max,
                    .alloc_size = alloc_size_cycle,
                    .los_live_bytes = heap.losBytesUsed(),
                    .los_capacity_bytes = if (heap.losRegion()) |r| r.len() else 0,
                    .p95_pause_ns = pause_ns,
                    .target_pause_ns = heap.los_target_pause_ns,
                });
                heap.setLosThreshold(
                    los_policy.target_bytes,
                    los_policy.scale,
                    los_policy.large_alloc_ratio,
                    los_policy.occupancy_ratio,
                    los_policy.pause_error,
                );
            },
        }
        return copied;
    }

    fn collectSemispaceRootSet(self: *GC, heap: *heap_mod.Heap, roots: roots_mod.RootSet) !usize {
        const phase_start = std.time.nanoTimestamp();

        // Preallocate work queue if first collection
        if (self.work_list.capacity == 0) {
            const init_cap = self.calculateInitialCapacity(heap);
            try self.work_list.ensureTotalCapacity(self.allocator, init_cap);
            try self.age_updates.ensureTotalCapacity(self.allocator, init_cap * 4);
            try self.remembered_runs.ensureTotalCapacity(self.allocator, @max(init_cap / 8, @as(usize, 64)));
            try self.major_work.ensureTotalCapacity(self.allocator, init_cap);
        }

        // Set GC in-progress flag (debug only)
        if (builtin.mode == .Debug) self.gc_in_progress = true;
        defer {
            if (builtin.mode == .Debug) self.gc_in_progress = false;
        }

        // Clear work list, retaining capacity from previous collections
        self.work_list.clearRetainingCapacity();
        self.work_peak = 0;
        self.age_updates.clearRetainingCapacity();
        self.age_peak = 0;
        self.debug_parent_addr = 0;
        self.debug_parent_tag = .cons;
        self.debug_grand_addr = 0;
        self.debug_grand_tag = .cons;
        self.debug_origin_kind = .none;
        self.debug_origin_a = 0;
        self.debug_origin_b = 0;
        self.debug_roots_ranges = roots.ranges;
        self.debug_roots_slots = roots.slots;
        var alloc_ptr = heap.to_start;
        heap.clearTenuredMarks();
        var root_vals: usize = roots.slots.len;
        for (roots.ranges) |r| root_vals +%= r.len;

        // Phase 1: Copy roots
        for (roots.ranges, 0..) |r, range_idx| {
            for (r.ptr[0..r.len], 0..) |*root, elem_idx| {
                self.debug_origin_kind = .range;
                self.debug_origin_a = range_idx;
                self.debug_origin_b = elem_idx;
                root.* = try self.copyValue(heap, root.*, &alloc_ptr);
            }
        }
        for (roots.slots, 0..) |slot, slot_idx| {
            self.debug_origin_kind = .slot;
            self.debug_origin_a = slot_idx;
            self.debug_origin_b = 0;
            slot.* = try self.copyValue(heap, slot.*, &alloc_ptr);
        }
        const root_ns = elapsedNsSince(phase_start);

        // Phase 2: Process work list, scanning objects and copying references
        while (self.work_list.items.len > 0) {
            const item = self.work_list.items[self.work_list.items.len - 1];
            self.work_list.items.len -= 1;
            self.debug_parent_addr = item.parent_addr;
            self.debug_parent_tag = item.parent_tag;
            self.debug_grand_addr = item.grand_addr;
            self.debug_grand_tag = item.grand_tag;
            self.debug_origin_kind = item.origin_kind;
            self.debug_origin_a = item.origin_a;
            self.debug_origin_b = item.origin_b;
            try self.scanObject(heap, item.addr, item.tag, &alloc_ptr);
        }
        const copy_end_ns = elapsedNsSince(phase_start);
        const copy_ns = copy_end_ns - root_ns;

        // Calculate bytes copied
        const bytes_copied = @intFromPtr(alloc_ptr) - @intFromPtr(heap.to_start);

        // Save old alloc_ptr before swap for finalization
        const old_alloc_ptr = heap.alloc_ptr;

        // Phase 3: Swap spaces
        heap.swapSpaces();
        heap.resetAllocPtr(@ptrCast(@alignCast(heap.from_start + bytes_copied)));
        try heap.rebuildSurvivorAges(self.age_updates.items);

        // Phase 4: Finalize unreachable objects with resources (uses old space)
        const finalize_start = std.time.nanoTimestamp();
        self.finalizeUnreachable(heap, old_alloc_ptr);
        try heap.sweepTenured();
        const finalize_ns = elapsedNsSince(finalize_start);

        // Update stats
        heap.stats.gc_count += 1;
        heap.stats.bytes_copied += bytes_copied;
        heap.stats.gc_root_ns +%= root_ns;
        heap.stats.gc_copy_ns +%= copy_ns;
        heap.stats.gc_finalize_ns +%= finalize_ns;
        heap.stats.gc_root_vals +%= root_vals;

        // Phase 5: Grow queues AFTER collection completes if needed
        try self.maybeGrowQueues();

        return bytes_copied;
    }

    fn collectMinorRootSet(self: *GC, heap: *heap_mod.Heap, roots: roots_mod.RootSet) !usize {
        const phase_start = std.time.nanoTimestamp();

        if (self.work_list.capacity == 0) {
            const init_cap = self.calculateInitialCapacity(heap);
            try self.work_list.ensureTotalCapacity(self.allocator, init_cap);
            try self.age_updates.ensureTotalCapacity(self.allocator, init_cap * 4);
            try self.major_work.ensureTotalCapacity(self.allocator, init_cap);
        }

        if (builtin.mode == .Debug) self.gc_in_progress = true;
        defer {
            if (builtin.mode == .Debug) self.gc_in_progress = false;
        }

        self.beginMajorCycleIfNeeded(heap);

        self.work_list.clearRetainingCapacity();
        self.work_peak = 0;
        self.age_updates.clearRetainingCapacity();
        self.age_peak = 0;
        self.remembered_runs.clearRetainingCapacity();
        self.runs_peak = 0;
        self.debug_parent_addr = 0;
        self.debug_parent_tag = .cons;
        self.debug_grand_addr = 0;
        self.debug_grand_tag = .cons;
        self.debug_origin_kind = .none;
        self.debug_origin_a = 0;
        self.debug_origin_b = 0;
        self.debug_roots_ranges = roots.ranges;
        self.debug_roots_slots = roots.slots;
        var alloc_ptr = heap.to_start;
        var root_vals: usize = roots.slots.len;
        for (roots.ranges) |r| root_vals +%= r.len;
        const from_start_addr = @intFromPtr(heap.from_start);
        const from_end_addr = @intFromPtr(heap.from_end);
        const trace_root_raw: ?u64 = blk: {
            const raw_c = std.posix.getenv("HABU_TRACE_ROOT_RAW") orelse break :blk null;
            const raw = std.mem.trim(u8, std.mem.sliceTo(raw_c, 0), " \t\r\n");
            if (raw.len == 0) break :blk null;
            const hex = if (raw.len > 2 and (raw[0] == '0') and (raw[1] == 'x' or raw[1] == 'X')) raw[2..] else raw;
            break :blk std.fmt.parseUnsigned(u64, hex, 16) catch null;
        };
        const trace_low_off: ?usize = blk: {
            const raw_c = std.posix.getenv("HABU_TRACE_ROOT_LOW_OFFSET") orelse break :blk null;
            const raw = std.mem.trim(u8, std.mem.sliceTo(raw_c, 0), " \t\r\n");
            if (raw.len == 0) break :blk null;
            const hex = if (raw.len > 2 and (raw[0] == '0') and (raw[1] == 'x' or raw[1] == 'X')) raw[2..] else raw;
            break :blk std.fmt.parseUnsigned(usize, hex, 16) catch null;
        };
        const trace_bad_root = self.trace_bad_root;
        const trap_bad_root = self.trap_bad_root;

        var range_idx: usize = 0;
        for (roots.ranges) |r| {
            var elem_idx: usize = 0;
            for (r.ptr[0..r.len]) |*root| {
                self.debug_origin_kind = .range;
                self.debug_origin_a = range_idx;
                self.debug_origin_b = elem_idx;
                if (trace_root_raw) |target_raw| {
                    if (root.*.raw == target_raw) {
                        std.debug.print(
                            "TRACE root-hit range={d} idx={d} root_ptr=0x{x} val=0x{x} kind={s}\n",
                            .{ range_idx, elem_idx, @intFromPtr(root), root.*.raw, @tagName(root.*.typeKind()) },
                        );
                    }
                }
                if (trace_low_off) |limit| {
                    const root_val = root.*;
                    if (root_val.isPointer()) {
                        const addr = root_val.toPtrAddr();
                        if (addr >= from_start_addr and addr < from_end_addr and (addr - from_start_addr) <= limit) {
                            std.debug.print(
                                "TRACE root-low range={d} idx={d} root_ptr=0x{x} val=0x{x} off=0x{x} kind={s}\n",
                                .{ range_idx, elem_idx, @intFromPtr(root), root_val.raw, addr - from_start_addr, @tagName(root_val.typeKind()) },
                            );
                        }
                    }
                }
                if (trace_bad_root) {
                    const root_val = root.*;
                    if (root_val.isPointer() and root_val.getTag() == .boxed) {
                        const addr = root_val.toPtrAddr();
                        if (heap.containsAddrForDebug(addr)) {
                            const first_word: *const Value = @ptrFromInt(addr);
                            if (first_word.isForwarding()) {
                                // Another root already copied this object in the same cycle.
                                // The old header no longer contains BoxedKind.
                                continue;
                            }
                            const kind_raw = @as(*const u64, @ptrFromInt(addr)).*;
                            const kind_n = @typeInfo(objects.BoxedKind).@"enum".fields.len;
                            if (kind_raw >= kind_n) {
                                std.debug.print(
                                    "TRACE bad-root range={d} idx={d} root_ptr=0x{x} val=0x{x} boxed-kind-raw=0x{x}\n",
                                    .{ range_idx, elem_idx, @intFromPtr(root), root_val.raw, kind_raw },
                                );
                                if (trap_bad_root) {
                                    @panic("bad boxed root");
                                }
                            }
                        }
                    } else if (root_val.isPointer() and root_val.getTag() == .symbol) {
                        const addr = root_val.toPtrAddr();
                        if (heap.containsAddrForDebug(addr)) {
                            const first_word: *const Value = @ptrFromInt(addr);
                            if (first_word.isForwarding()) continue;
                            const sym: *const objects.Symbol = @ptrFromInt(addr);
                            if (sym.name_len > heap.space_size) {
                                std.debug.print(
                                    "TRACE bad-root range={d} idx={d} root_ptr=0x{x} val=0x{x} symbol-name-len={d}\n",
                                    .{ range_idx, elem_idx, @intFromPtr(root), root_val.raw, sym.name_len },
                                );
                                if (trap_bad_root) {
                                    @panic("bad symbol root");
                                }
                            }
                        }
                    } else if (root_val.isPointer() and root_val.getTag() == .closure) {
                        const addr = root_val.toPtrAddr();
                        if (heap.containsAddrForDebug(addr)) {
                            const first_word: *const Value = @ptrFromInt(addr);
                            if (first_word.isForwarding()) continue;
                            const cls: *const objects.Closure = @ptrFromInt(addr);
                            const max_caps = heap.space_size / @sizeOf(Value);
                            const cap_start = addr + @sizeOf(objects.Closure);
                            const bad_caps = cls.num_captures > max_caps;
                            const bad_ptr = @intFromPtr(cls.captures) != cap_start;
                            if (bad_caps or bad_ptr) {
                                std.debug.print(
                                    "TRACE bad-root range={d} idx={d} root_ptr=0x{x} val=0x{x} closure-captures={d} max={d} captures_ptr=0x{x} expected=0x{x}\n",
                                    .{
                                        range_idx,
                                        elem_idx,
                                        @intFromPtr(root),
                                        root_val.raw,
                                        cls.num_captures,
                                        max_caps,
                                        @intFromPtr(cls.captures),
                                        cap_start,
                                    },
                                );
                                if (trap_bad_root) {
                                    @panic("bad closure root");
                                }
                            }
                        }
                    }
                }
                root.* = try self.copyValue(heap, root.*, &alloc_ptr);
                elem_idx += 1;
            }
            range_idx += 1;
        }
        var slot_idx: usize = 0;
        for (roots.slots) |slot| {
            self.debug_origin_kind = .slot;
            self.debug_origin_a = slot_idx;
            self.debug_origin_b = 0;
            if (trace_root_raw) |target_raw| {
                if (slot.*.raw == target_raw) {
                    std.debug.print(
                        "TRACE root-hit slot={d} slot_ptr=0x{x} val=0x{x} kind={s}\n",
                        .{ slot_idx, @intFromPtr(slot), slot.*.raw, @tagName(slot.*.typeKind()) },
                    );
                }
            }
            if (trace_low_off) |limit| {
                const slot_val = slot.*;
                if (slot_val.isPointer()) {
                    const addr = slot_val.toPtrAddr();
                    if (addr >= from_start_addr and addr < from_end_addr and (addr - from_start_addr) <= limit) {
                        std.debug.print(
                            "TRACE root-low slot={d} slot_ptr=0x{x} val=0x{x} off=0x{x} kind={s}\n",
                            .{ slot_idx, @intFromPtr(slot), slot_val.raw, addr - from_start_addr, @tagName(slot_val.typeKind()) },
                        );
                    }
                }
            }
            if (trace_bad_root) {
                const slot_val = slot.*;
                if (slot_val.isPointer() and slot_val.getTag() == .boxed) {
                    const addr = slot_val.toPtrAddr();
                    if (heap.containsAddrForDebug(addr)) {
                        const first_word: *const Value = @ptrFromInt(addr);
                        if (first_word.isForwarding()) continue;
                        const kind_raw = @as(*const u64, @ptrFromInt(addr)).*;
                        const kind_n = @typeInfo(objects.BoxedKind).@"enum".fields.len;
                        if (kind_raw >= kind_n) {
                            std.debug.print(
                                "TRACE bad-root slot={d} slot_ptr=0x{x} val=0x{x} boxed-kind-raw=0x{x}\n",
                                .{ slot_idx, @intFromPtr(slot), slot_val.raw, kind_raw },
                            );
                            if (trap_bad_root) {
                                @panic("bad boxed slot root");
                            }
                        }
                    }
                } else if (slot_val.isPointer() and slot_val.getTag() == .symbol) {
                    const addr = slot_val.toPtrAddr();
                    if (heap.containsAddrForDebug(addr)) {
                        const first_word: *const Value = @ptrFromInt(addr);
                        if (first_word.isForwarding()) continue;
                        const sym: *const objects.Symbol = @ptrFromInt(addr);
                        if (sym.name_len > heap.space_size) {
                            std.debug.print(
                                "TRACE bad-root slot={d} slot_ptr=0x{x} val=0x{x} symbol-name-len={d}\n",
                                .{ slot_idx, @intFromPtr(slot), slot_val.raw, sym.name_len },
                            );
                            if (trap_bad_root) {
                                @panic("bad symbol slot root");
                            }
                        }
                    }
                } else if (slot_val.isPointer() and slot_val.getTag() == .closure) {
                    const addr = slot_val.toPtrAddr();
                    if (heap.containsAddrForDebug(addr)) {
                        const first_word: *const Value = @ptrFromInt(addr);
                        if (first_word.isForwarding()) continue;
                        const cls: *const objects.Closure = @ptrFromInt(addr);
                        const max_caps = heap.space_size / @sizeOf(Value);
                        const cap_start = addr + @sizeOf(objects.Closure);
                        const bad_caps = cls.num_captures > max_caps;
                        const bad_ptr = @intFromPtr(cls.captures) != cap_start;
                        if (bad_caps or bad_ptr) {
                            std.debug.print(
                                "TRACE bad-root slot={d} slot_ptr=0x{x} val=0x{x} closure-captures={d} max={d} captures_ptr=0x{x} expected=0x{x}\n",
                                .{
                                    slot_idx,
                                    @intFromPtr(slot),
                                    slot_val.raw,
                                    cls.num_captures,
                                    max_caps,
                                    @intFromPtr(cls.captures),
                                    cap_start,
                                },
                            );
                            if (trap_bad_root) {
                                @panic("bad closure slot root");
                            }
                        }
                    }
                }
            }
            slot.* = try self.copyValue(heap, slot.*, &alloc_ptr);
            slot_idx += 1;
        }
        const root_ns = elapsedNsSince(phase_start);

        // Scan tenured remembered objects on marked cards.
        var rem_scanned: usize = 0;
        const marked_cards = heap.markedCardCount();
        if (marked_cards > 0) {
            try heap.appendMarkedCardRuns(self.allocator, &self.remembered_runs);
            self.runs_peak = self.remembered_runs.items.len;
            rem_scanned +%= try self.scanRememberedObjects(heap, &heap.tenured_objs, &alloc_ptr);
            rem_scanned +%= try self.scanRememberedObjects(heap, &heap.los_objs, &alloc_ptr);
        }

        try self.processMinorWork(heap, &alloc_ptr);
        try self.advanceMajorCycle(heap, &alloc_ptr);
        const copy_end_ns = elapsedNsSince(phase_start);
        const copy_ns = copy_end_ns - root_ns;

        const bytes_copied = @intFromPtr(alloc_ptr) - @intFromPtr(heap.to_start);
        const old_alloc_ptr = heap.alloc_ptr;

        heap.swapSpaces();
        heap.resetAllocPtr(@ptrCast(@alignCast(heap.from_start + bytes_copied)));
        try heap.rebuildSurvivorAges(self.age_updates.items);

        const finalize_start = std.time.nanoTimestamp();
        self.finalizeUnreachable(heap, old_alloc_ptr);
        const finalize_ns = elapsedNsSince(finalize_start);

        heap.stats.gc_count +%= 1;
        heap.stats.bytes_copied +%= bytes_copied;
        heap.stats.gc_root_ns +%= root_ns;
        heap.stats.gc_copy_ns +%= copy_ns;
        heap.stats.gc_finalize_ns +%= finalize_ns;
        heap.stats.gc_root_vals +%= root_vals + rem_scanned;
        heap.stats.gc_remembered_scanned +%= rem_scanned;
        heap.stats.gc_remembered_runs +%= self.remembered_runs.items.len;
        heap.stats.gc_remembered_marked_cards +%= marked_cards;

        try self.maybeGrowQueues();
        return bytes_copied;
    }

    fn scanRememberedObjects(
        self: *GC,
        heap: *heap_mod.Heap,
        objs: *const std.ArrayList(heap_mod.Heap.TenuredObj),
        alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8,
    ) !usize {
        if (self.remembered_runs.items.len == 0) return 0;
        var scanned: usize = 0;
        const initial_len = objs.items.len;
        var i: usize = 0;
        while (i < initial_len) : (i += 1) {
            const obj = objs.items[i];
            const end_addr = std.math.add(usize, obj.addr, obj.size) catch @panic("corrupt remembered object range");
            if (!heap.hasMarkedCardInAddrRangeRuns(obj.addr, end_addr, self.remembered_runs.items)) continue;
            self.debug_parent_addr = 0;
            self.debug_parent_tag = .cons;
            self.debug_grand_addr = 0;
            self.debug_grand_tag = .cons;
            self.debug_origin_kind = .none;
            self.debug_origin_a = 0;
            self.debug_origin_b = 0;
            try self.scanObject(heap, obj.addr, obj.tag, alloc_ptr);
            scanned +%= 1;
        }
        return scanned;
    }

    /// Grow work queues after GC if they exceeded 75% capacity
    /// Growth happens AFTER GC completes to avoid allocations during trace
    fn maybeGrowQueues(self: *GC) !void {
        const work_cap = self.work_list.capacity;
        const work_peak = self.work_peak;
        const age_cap = self.age_updates.capacity;
        const age_peak = self.age_peak;
        const run_cap = self.remembered_runs.capacity;
        const run_peak = self.runs_peak;
        const major_cap = self.major_work.capacity;
        const major_peak = self.major_peak;

        // If we used >75% capacity, grow for next cycle
        if (work_peak * 4 > work_cap * 3) {
            const new_cap = work_cap * 2;
            try self.work_list.ensureTotalCapacity(self.allocator, new_cap);
        }
        if (age_cap == 0 or age_peak * 4 > age_cap * 3) {
            const new_cap = if (age_cap == 0) @as(usize, 256) else age_cap * 2;
            try self.age_updates.ensureTotalCapacity(self.allocator, new_cap);
        }
        if (run_cap == 0 or run_peak * 4 > run_cap * 3) {
            const new_cap = if (run_cap == 0) @as(usize, 64) else run_cap * 2;
            try self.remembered_runs.ensureTotalCapacity(self.allocator, new_cap);
        }
        if (major_cap == 0 or major_peak * 4 > major_cap * 3) {
            const new_cap = if (major_cap == 0) @as(usize, 256) else major_cap * 2;
            try self.major_work.ensureTotalCapacity(self.allocator, new_cap);
        }
    }

    fn shouldRunMajorCycle(heap: *const heap_mod.Heap) bool {
        return heap.gcLayoutMode() == .generational and
            (heap.tenured_objs.items.len > 0 or heap.los_objs.items.len > 0);
    }

    fn beginMajorCycleIfNeeded(self: *GC, heap: *heap_mod.Heap) void {
        if (heap.gcLayoutMode() != .generational) {
            self.major.phase = .idle;
            heap.setMajorCycleActive(false);
            return;
        }
        if (self.major.phase != .idle) {
            heap.setMajorCycleActive(true);
            return;
        }
        if (!shouldRunMajorCycle(heap)) {
            heap.setMajorCycleActive(false);
            return;
        }

        self.major.phase = .mark;
        self.major.tenured_cursor = 0;
        self.major.los_cursor = 0;
        self.major_work.clearRetainingCapacity();
        self.major_peak = 0;
        heap.clearTenuredMarks();
        heap.clearLosMarks();
        heap.setMajorCycleActive(true);
    }

    fn pushMajorWork(self: *GC, addr: usize, tag: Tag) !void {
        const parent = WorkItem{
            .addr = addr,
            .tag = tag,
            .parent_addr = self.debug_scan_addr,
            .parent_tag = self.debug_scan_tag,
            .grand_addr = self.debug_parent_addr,
            .grand_tag = self.debug_parent_tag,
            .origin_kind = self.debug_origin_kind,
            .origin_a = self.debug_origin_a,
            .origin_b = self.debug_origin_b,
        };
        if (builtin.mode == .Debug and self.gc_in_progress) {
            const old_cap = self.major_work.capacity;
            try self.major_work.append(self.allocator, parent);
            const new_cap = self.major_work.capacity;
            if (new_cap > old_cap) {
                std.debug.print("ERROR: major_work allocated during GC (cap: {} -> {})\n", .{ old_cap, new_cap });
                @panic("Allocation during GC detected");
            }
        } else {
            try self.major_work.append(self.allocator, parent);
        }
        if (self.major_work.items.len > self.major_peak) self.major_peak = self.major_work.items.len;
    }

    fn processMinorWork(self: *GC, heap: *heap_mod.Heap, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8) !void {
        while (self.work_list.items.len > 0) {
            const item = self.work_list.items[self.work_list.items.len - 1];
            self.work_list.items.len -= 1;
            self.debug_parent_addr = item.parent_addr;
            self.debug_parent_tag = item.parent_tag;
            self.debug_grand_addr = item.grand_addr;
            self.debug_grand_tag = item.grand_tag;
            self.debug_origin_kind = item.origin_kind;
            self.debug_origin_a = item.origin_a;
            self.debug_origin_b = item.origin_b;
            try self.scanObject(heap, item.addr, item.tag, alloc_ptr);
        }
    }

    fn processMajorWork(self: *GC, heap: *heap_mod.Heap, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8, budget: usize) !usize {
        var scanned: usize = 0;
        var left = budget;
        while (left > 0 and self.major_work.items.len > 0) : (left -= 1) {
            scanned +%= 1;
            const item = self.major_work.items[self.major_work.items.len - 1];
            self.major_work.items.len -= 1;
            self.debug_parent_addr = item.parent_addr;
            self.debug_parent_tag = item.parent_tag;
            self.debug_grand_addr = item.grand_addr;
            self.debug_grand_tag = item.grand_tag;
            self.debug_origin_kind = item.origin_kind;
            self.debug_origin_a = item.origin_a;
            self.debug_origin_b = item.origin_b;
            try self.scanObject(heap, item.addr, item.tag, alloc_ptr);
            try self.processMinorWork(heap, alloc_ptr);
        }
        return scanned;
    }

    fn advanceMajorCycle(self: *GC, heap: *heap_mod.Heap, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8) !void {
        if (self.major.phase == .idle) return;

        const marked = try self.processMajorWork(heap, alloc_ptr, MAJOR_MARK_BUDGET_OBJS);
        heap.stats.gc_major_mark_steps +%= marked;

        while (true) {
            switch (self.major.phase) {
                .idle => return,
                .mark => {
                    if (self.major_work.items.len != 0) return;
                    self.major.phase = .sweep_tenured;
                    continue;
                },
                .sweep_tenured => {
                    if (self.major_work.items.len != 0) return;
                    const slice = try heap.sweepTenuredSlice(&self.major.tenured_cursor, MAJOR_SWEEP_BUDGET_OBJS);
                    if (slice.scanned > 0) {
                        heap.stats.gc_major_sweep_tenured_steps +%= 1;
                        heap.stats.gc_major_swept_tenured +%= slice.scanned;
                        if (slice.scanned > heap.stats.gc_major_max_tenured_slice) {
                            heap.stats.gc_major_max_tenured_slice = slice.scanned;
                        }
                    }
                    if (!slice.done) return;
                    self.major.phase = .sweep_los;
                    continue;
                },
                .sweep_los => {
                    if (self.major_work.items.len != 0) return;
                    const slice = try heap.sweepLosSlice(&self.major.los_cursor, MAJOR_SWEEP_BUDGET_OBJS);
                    if (slice.scanned > 0) {
                        heap.stats.gc_major_sweep_los_steps +%= 1;
                        heap.stats.gc_major_swept_los +%= slice.scanned;
                        if (slice.scanned > heap.stats.gc_major_max_los_slice) {
                            heap.stats.gc_major_max_los_slice = slice.scanned;
                        }
                    }
                    if (!slice.done) return;
                    self.major.phase = .idle;
                    self.major.tenured_cursor = 0;
                    self.major.los_cursor = 0;
                    self.major_work.clearRetainingCapacity();
                    heap.setMajorCycleActive(false);
                    heap.stats.gc_major_cycle_n +%= 1;
                    return;
                },
            }
        }
    }

    fn pushWork(self: *GC, addr: usize, tag: Tag) !void {
        const parent = WorkItem{
            .addr = addr,
            .tag = tag,
            .parent_addr = self.debug_scan_addr,
            .parent_tag = self.debug_scan_tag,
            .grand_addr = self.debug_parent_addr,
            .grand_tag = self.debug_parent_tag,
            .origin_kind = self.debug_origin_kind,
            .origin_a = self.debug_origin_a,
            .origin_b = self.debug_origin_b,
        };
        // Debug check: detect allocations during GC trace/copy.
        if (builtin.mode == .Debug and self.gc_in_progress) {
            const old_cap = self.work_list.capacity;
            try self.work_list.append(self.allocator, parent);
            const new_cap = self.work_list.capacity;
            if (new_cap > old_cap) {
                std.debug.print("ERROR: work_list allocated during GC (cap: {} -> {})\n", .{ old_cap, new_cap });
                @panic("Allocation during GC detected");
            }
        } else {
            try self.work_list.append(self.allocator, parent);
        }

        if (self.work_list.items.len > self.work_peak) self.work_peak = self.work_list.items.len;
    }

    fn pushAgeUpdate(self: *GC, addr: usize, age: u8) !void {
        if (builtin.mode == .Debug and self.gc_in_progress) {
            const old_cap = self.age_updates.capacity;
            try self.age_updates.append(self.allocator, .{ .addr = addr, .age = age });
            const new_cap = self.age_updates.capacity;
            if (new_cap > old_cap) {
                std.debug.print("ERROR: age_updates allocated during GC (cap: {} -> {})\n", .{ old_cap, new_cap });
                @panic("Allocation during GC detected");
            }
        } else {
            try self.age_updates.append(self.allocator, .{ .addr = addr, .age = age });
        }
        if (self.age_updates.items.len > self.age_peak) self.age_peak = self.age_updates.items.len;
    }

    /// Finalize unreachable objects that hold resources (e.g., file handles)
    /// This walks the from-space and closes any open streams that weren't copied
    /// old_alloc_ptr: the alloc_ptr value BEFORE swapSpaces was called
    fn finalizeUnreachable(_: *GC, heap: *heap_mod.Heap, old_alloc_ptr: [*]align(heap_mod.ALIGNMENT) u8) void {
        const old_start = @intFromPtr(heap.to_start);
        const old_end = @intFromPtr(old_alloc_ptr);

        var i: usize = 0;
        while (i < heap.stream_list.items.len) {
            const stream = heap.stream_list.items[i];
            const addr = @intFromPtr(stream);

            if (addr >= old_start and addr < old_end) {
                const first_word: *Value = @ptrFromInt(addr);
                if (first_word.isForwarding()) {
                    const new_addr = first_word.toPtrAddr();
                    const nursery_start = @intFromPtr(heap.from_start);
                    const nursery_end = @intFromPtr(heap.from_end);
                    var live = new_addr >= nursery_start and new_addr < nursery_end;
                    if (!live) {
                        if (heap.tenuredRegion()) |tenured| {
                            const ten_start = @intFromPtr(tenured.start);
                            const ten_used_end = if (heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                            live = new_addr >= ten_start and new_addr < ten_used_end;
                        }
                    }
                    if (live) {
                        heap.stream_list.items[i] = @ptrFromInt(new_addr);
                        i += 1;
                        continue;
                    }
                }

                stream.finalize();
                _ = heap.stream_list.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    fn boxedHasRefs(addr: usize) bool {
        const kind_ptr: *const objects.BoxedKind = @ptrFromInt(addr);
        return switch (kind_ptr.*) {
            .hashtable,
            .array,
            .stream,
            .pathname,
            .package,
            .condition,
            .class,
            .slotdef,
            .generic_function,
            .method,
            .macro_env,
            .chunk,
            => true,
            .rational,
            .complex,
            .bignum,
            .string32,
            .native_code,
            => false,
        };
    }

    fn objectHasRefsAtAddr(tag: Tag, addr: usize) bool {
        return switch (tag) {
            .cons, .symbol, .vector, .closure => true,
            .boxed => boxedHasRefs(addr),
            .string, .keyword, .forwarding => false,
        };
    }

    fn shouldPromote(heap: *const heap_mod.Heap, tag: Tag, size: usize, survivor_age: u8) bool {
        if (heap.gcLayoutMode() != .generational) return false;
        if (tag == .forwarding) return false;
        if (size >= heap.promote_threshold) return true;
        return survivor_age >= PROMOTE_AGE_THRESHOLD;
    }

    fn resolveStaleForwardedValue(self: *GC, heap: *const heap_mod.Heap, val: Value, obj_addr: usize) ?Value {
        const stale_start = @intFromPtr(heap.to_start);
        const stale_end = stale_start + heap.space_size;
        if (obj_addr < stale_start or obj_addr >= stale_end) return null;

        const first_word: *const Value = @ptrFromInt(obj_addr);
        if (!first_word.isForwarding()) return null;

        const new_addr = first_word.toPtrAddr();
        // Current-cycle to-space objects can have header words that alias the
        // forwarding tag bits (for example symbol name_len=14). If the
        // "forwarding target" is not even inside heap memory, treat this as a
        // regular object header, not stale forwarding metadata.
        if (!heap.containsAddrForDebug(new_addr)) return null;
        const forwarded_size_ptr: *const usize = @ptrFromInt(obj_addr + @sizeOf(Value));
        const forwarded_size = forwarded_size_ptr.*;
        const forwarded_size_ok = forwarded_size > 0 and
            forwarded_size <= heap.space_size and
            std.mem.isAligned(forwarded_size, heap_mod.ALIGNMENT);

        const from_start = @intFromPtr(heap.from_start);
        const from_end = @intFromPtr(heap.from_end);
        const in_from_addr = new_addr >= from_start and new_addr < from_end;

        var in_tenured_addr = false;
        if (heap.gcLayoutMode() == .generational) {
            if (heap.tenuredRegion()) |tenured| {
                const ten_start = @intFromPtr(tenured.start);
                const ten_used_end = if (heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                in_tenured_addr = new_addr >= ten_start and new_addr < ten_used_end;
            }
        }
        if (!forwarded_size_ok) {
            if (self.trace_stale_resolve) {
                std.debug.print(
                    "TRACE stale-resolve reject-size val=0x{x} obj=0x{x} fw=0x{x} sz={d} stale=[0x{x},0x{x}) scan=0x{x}:{s} parent=0x{x}:{s} grand=0x{x}:{s} origin={s}:{d}:{d}\n",
                    .{
                        val.raw,
                        obj_addr,
                        first_word.raw,
                        forwarded_size,
                        stale_start,
                        stale_end,
                        self.debug_scan_addr,
                        @tagName(self.debug_scan_tag),
                        self.debug_parent_addr,
                        @tagName(self.debug_parent_tag),
                        self.debug_grand_addr,
                        @tagName(self.debug_grand_tag),
                        @tagName(self.debug_origin_kind),
                        self.debug_origin_a,
                        self.debug_origin_b,
                    },
                );
                if (val.getTag() == .symbol) {
                    const sym: *const objects.Symbol = @ptrFromInt(obj_addr);
                    const name_ptr = @intFromPtr(sym.name_ptr);
                    const sym_name = blk: {
                        if (sym.name_len == 0 or sym.name_len > 128) break :blk "<bad-len>";
                        const n: usize = @intCast(sym.name_len);
                        if (name_ptr != obj_addr + @sizeOf(objects.Symbol)) break :blk "<bad-ptr>";
                        break :blk sym.name_ptr[0..n];
                    };
                    std.debug.print(
                        "TRACE stale-resolve reject-size symbol obj=0x{x} len={d} name_ptr=0x{x} name={s} plist=0x{x}\n",
                        .{ obj_addr, sym.name_len, name_ptr, sym_name, sym.plist.raw },
                    );
                    if (!std.mem.eql(u8, sym_name, "<bad-len>") and !std.mem.eql(u8, sym_name, "<bad-ptr>")) {
                        if (heap.symbols.get(sym_name)) |global_sym| {
                            std.debug.print(
                                "TRACE stale-resolve reject-size symbol global-hit name={s} raw=0x{x}\n",
                                .{ sym_name, global_sym.raw },
                            );
                        }
                        var pkg_it = heap.packages.valueIterator();
                        while (pkg_it.next()) |pkg| {
                            if (pkg.*.symbols.get(sym_name)) |pkg_sym| {
                                std.debug.print(
                                    "TRACE stale-resolve reject-size symbol pkg-hit pkg={s} name={s} raw=0x{x}\n",
                                    .{ pkg.*.name, sym_name, pkg_sym.raw },
                                );
                            }
                        }
                    }
                }
            }
            if (self.trap_stale_resolve_reject) @panic("stale resolve reject-size");
            return null;
        }

        const in_from = in_from_addr and forwarded_size <= from_end - new_addr;
        var in_tenured = false;
        if (in_tenured_addr) {
            if (heap.tenuredRegion()) |tenured| {
                const ten_start = @intFromPtr(tenured.start);
                const ten_used_end = if (heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                in_tenured = forwarded_size <= ten_used_end - new_addr;
            }
        }
        if (!(in_from or in_tenured)) {
            if (self.trace_stale_resolve) {
                std.debug.print(
                    "TRACE stale-resolve reject-range val=0x{x} obj=0x{x} fw=0x{x} new=0x{x} sz={d} from=[0x{x},0x{x}) scan=0x{x}:{s} parent=0x{x}:{s} grand=0x{x}:{s} origin={s}:{d}:{d}\n",
                    .{
                        val.raw,
                        obj_addr,
                        first_word.raw,
                        new_addr,
                        forwarded_size,
                        from_start,
                        from_end,
                        self.debug_scan_addr,
                        @tagName(self.debug_scan_tag),
                        self.debug_parent_addr,
                        @tagName(self.debug_parent_tag),
                        self.debug_grand_addr,
                        @tagName(self.debug_grand_tag),
                        @tagName(self.debug_origin_kind),
                        self.debug_origin_a,
                        self.debug_origin_b,
                    },
                );
            }
            if (self.trap_stale_resolve_reject) @panic("stale resolve reject-range");
            return null;
        }
        if (!objects.forwardingTargetLooksValid(val.getTag(), new_addr, forwarded_size)) {
            if (self.trace_stale_resolve) {
                std.debug.print(
                    "TRACE stale-resolve reject-layout val=0x{x} obj=0x{x} fw=0x{x} new=0x{x} sz={d} tag={s} scan=0x{x}:{s} parent=0x{x}:{s} grand=0x{x}:{s} origin={s}:{d}:{d}\n",
                    .{
                        val.raw,
                        obj_addr,
                        first_word.raw,
                        new_addr,
                        forwarded_size,
                        @tagName(val.getTag()),
                        self.debug_scan_addr,
                        @tagName(self.debug_scan_tag),
                        self.debug_parent_addr,
                        @tagName(self.debug_parent_tag),
                        self.debug_grand_addr,
                        @tagName(self.debug_grand_tag),
                        @tagName(self.debug_origin_kind),
                        self.debug_origin_a,
                        self.debug_origin_b,
                    },
                );
            }
            if (self.trap_stale_resolve_reject) @panic("stale resolve reject-layout");
            return null;
        }
        if (self.trace_stale_resolve) {
            std.debug.print(
                "TRACE stale-resolve ok val=0x{x} obj=0x{x} fw=0x{x} sz={d} from=[0x{x},0x{x})\n",
                .{ val.raw, obj_addr, first_word.raw, forwarded_size, from_start, from_end },
            );
        }

        return .{ .raw = new_addr | @as(u64, @intFromEnum(val.getTag())) };
    }

    fn rememberScanStore(self: *GC, heap: *heap_mod.Heap, out: Value) Value {
        if (heap.gcLayoutMode() != .generational) return out;
        if (self.debug_scan_addr == 0) return out;
        if (heap.isInNurseryAddr(self.debug_scan_addr)) return out;
        if (!out.isPointer()) return out;
        if (!heap.isInNurseryAddr(out.toPtrAddr())) return out;
        heap.markCardForOwnerAddr(self.debug_scan_addr);
        return out;
    }

    /// Copy a value to to-space if needed
    fn copyValue(self: *GC, heap: *heap_mod.Heap, val: Value, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8) !Value {
        // Immediates don't need copying: nil, fixnums, floats, characters
        if (val.isNil() or val.isFixnum() or val.isFloat() or val.isCharacter()) {
            return self.rememberScanStore(heap, val);
        }

        // Only process actual heap pointers
        if (!val.isPointer()) {
            return self.rememberScanStore(heap, val);
        }

        // Resolve stale forwarded roots first, then continue normal copy flow.
        // Returning early here skips scan/copy for container objects and leaves
        // nested references stale across cycles.
        var live = val;
        var obj_addr = live.toPtrAddr();
        if (self.resolveStaleForwardedValue(heap, live, obj_addr)) |resolved| {
            live = resolved;
            obj_addr = live.toPtrAddr();
        }
        const from_start = @intFromPtr(heap.from_start);
        const from_end = @intFromPtr(heap.from_end);

        if (obj_addr < from_start or obj_addr >= from_end) {
            if (heap.isMajorCycleActive()) {
                const old_tag = live.getTag();
                const has_refs = objectHasRefsAtAddr(old_tag, obj_addr);

                switch (heap.markTenuredObject(obj_addr)) {
                    .newly => {
                        if (has_refs) {
                            try self.pushMajorWork(obj_addr, old_tag);
                        }
                    },
                    .already, .none => {},
                }
                switch (heap.markLosObject(obj_addr)) {
                    .newly => {
                        if (has_refs) {
                            try self.pushMajorWork(obj_addr, old_tag);
                        }
                    },
                    .already, .none => {},
                }
            }
            // Object is not in from-space (might be static), don't copy
            return self.rememberScanStore(heap, live);
        }

        // Check if already has forwarding pointer
        const first_word: *Value = @ptrFromInt(obj_addr);
        if (first_word.isForwarding()) {
            // Already copied, return new address with original tag.
            //
            // NOTE: Many Habu objects do not have a Value header word (e.g. strings start with
            // length). Those header words can coincidentally look like a forwarding Value, so we
            // validate both forwarding target and stored object size.
            const new_addr = first_word.toPtrAddr();
            const to_start = @intFromPtr(heap.to_start);
            const to_end = to_start + heap.space_size;
            const forwarded_size_ptr: *const usize = @ptrFromInt(obj_addr + @sizeOf(Value));
            const forwarded_size = forwarded_size_ptr.*;
            const forwarded_size_ok = forwarded_size > 0 and
                forwarded_size <= heap.space_size and
                std.mem.isAligned(forwarded_size, heap_mod.ALIGNMENT);
            const from_start_cur = @intFromPtr(heap.from_start);
            const from_end_cur = @intFromPtr(heap.from_end);
            const in_from_space = new_addr >= from_start_cur and new_addr < from_end_cur and forwarded_size <= from_end_cur - new_addr;
            const in_to_space = new_addr >= to_start and new_addr <= to_end and forwarded_size <= to_end - new_addr;
            var in_tenured = false;
            if (heap.gcLayoutMode() == .generational) {
                if (heap.tenuredRegion()) |tenured| {
                    const ten_start = @intFromPtr(tenured.start);
                    const ten_used_end = if (heap.tenured_alloc_ptr) |p| @intFromPtr(p) else ten_start;
                    in_tenured = new_addr >= ten_start and new_addr <= ten_used_end and
                        forwarded_size <= ten_used_end - new_addr;
                }
            }
            const forwarded_range_ok = in_from_space or in_to_space or in_tenured;
            if (forwarded_size_ok and forwarded_range_ok and
                objects.forwardingTargetLooksValid(live.getTag(), new_addr, forwarded_size))
            {
                return self.rememberScanStore(heap, .{ .raw = new_addr | @as(u64, @intFromEnum(live.getTag())) });
            }
        }

        // Copy object to to-space
        const tag = live.getTag();
        if (tag == .keyword and self.trace_bad_keyword) {
            const kw: *const objects.Keyword = @ptrFromInt(obj_addr);
            const name_addr = @intFromPtr(kw.name_ptr);
            const expected_name_addr = obj_addr + @sizeOf(objects.Keyword);
            const bad_len = kw.name_len > heap.space_size;
            const bad_ptr = name_addr != expected_name_addr;
            if (bad_len or bad_ptr) {
                std.debug.print(
                    "TRACE bad-keyword-copy val=0x{x} obj=0x{x} len={d} name_ptr=0x{x} expected=0x{x} parent=0x{x}:{s} scan=0x{x}:{s}\n",
                    .{
                        live.raw,
                        obj_addr,
                        kw.name_len,
                        name_addr,
                        expected_name_addr,
                        self.debug_parent_addr,
                        @tagName(self.debug_parent_tag),
                        self.debug_scan_addr,
                        @tagName(self.debug_scan_tag),
                    },
                );
                if (self.trap_bad_keyword) {
                    @panic("bad keyword object");
                }
            }
        }
        if (tag == .symbol and self.trace_bad_symbol) {
            const sym: *const objects.Symbol = @ptrFromInt(obj_addr);
            const name_addr = @intFromPtr(sym.name_ptr);
            const expected_name_addr = obj_addr + @sizeOf(objects.Symbol);
            const bad_len = sym.name_len > heap.space_size;
            const bad_ptr = name_addr != expected_name_addr;
            if (bad_len or bad_ptr) {
                const fw: *const Value = @ptrFromInt(obj_addr);
                const w1: *const usize = @ptrFromInt(obj_addr + @sizeOf(Value));
                const from_s = @intFromPtr(heap.from_start);
                const from_e = @intFromPtr(heap.from_end);
                const to_s = @intFromPtr(heap.to_start);
                const to_e = to_s + heap.space_size;
                std.debug.print(
                    "TRACE bad-symbol-copy val=0x{x} obj=0x{x} len={d} name_ptr=0x{x} expected=0x{x} fw=0x{x} is_fwd={} w1=0x{x} from=[0x{x},0x{x}) to=[0x{x},0x{x}) scan=0x{x}:{s} parent=0x{x}:{s} grand=0x{x}:{s} origin={s}:{d}:{d}\n",
                    .{
                        live.raw,
                        obj_addr,
                        sym.name_len,
                        name_addr,
                        expected_name_addr,
                        fw.raw,
                        fw.isForwarding(),
                        w1.*,
                        from_s,
                        from_e,
                        to_s,
                        to_e,
                        self.debug_scan_addr,
                        @tagName(self.debug_scan_tag),
                        self.debug_parent_addr,
                        @tagName(self.debug_parent_tag),
                        self.debug_grand_addr,
                        @tagName(self.debug_grand_tag),
                        @tagName(self.debug_origin_kind),
                        self.debug_origin_a,
                        self.debug_origin_b,
                    },
                );
                if (self.debug_scan_tag == .cons and self.debug_scan_addr != 0) {
                    const car_ptr: *const Value = @ptrFromInt(self.debug_scan_addr);
                    const cdr_ptr: *const Value = @ptrFromInt(self.debug_scan_addr + @sizeOf(Value));
                    std.debug.print(
                        "TRACE bad-symbol-cons car=0x{x}({s}) cdr=0x{x}({s})\n",
                        .{
                            car_ptr.raw,
                            @tagName(car_ptr.typeKind()),
                            cdr_ptr.raw,
                            @tagName(cdr_ptr.typeKind()),
                        },
                    );
                }
                if (self.debug_parent_tag == .cons and self.debug_parent_addr != 0) {
                    const car_ptr: *const Value = @ptrFromInt(self.debug_parent_addr);
                    const cdr_ptr: *const Value = @ptrFromInt(self.debug_parent_addr + @sizeOf(Value));
                    std.debug.print(
                        "TRACE bad-symbol-parent-cons car=0x{x}({s}) cdr=0x{x}({s})\n",
                        .{
                            car_ptr.raw,
                            @tagName(car_ptr.typeKind()),
                            cdr_ptr.raw,
                            @tagName(cdr_ptr.typeKind()),
                        },
                    );
                }
                if (self.debug_grand_tag == .cons and self.debug_grand_addr != 0) {
                    const car_ptr: *const Value = @ptrFromInt(self.debug_grand_addr);
                    const cdr_ptr: *const Value = @ptrFromInt(self.debug_grand_addr + @sizeOf(Value));
                    std.debug.print(
                        "TRACE bad-symbol-grand-cons car=0x{x}({s}) cdr=0x{x}({s})\n",
                        .{
                            car_ptr.raw,
                            @tagName(car_ptr.typeKind()),
                            cdr_ptr.raw,
                            @tagName(cdr_ptr.typeKind()),
                        },
                    );
                }
                switch (self.debug_origin_kind) {
                    .range => {
                        if (self.debug_origin_a < self.debug_roots_ranges.len) {
                            const rr = self.debug_roots_ranges[self.debug_origin_a];
                            if (self.debug_origin_b < rr.len) {
                                const root_val = rr.ptr[self.debug_origin_b];
                                std.debug.print(
                                    "TRACE bad-symbol-origin-root range={d} idx={d} raw=0x{x} kind={s}\n",
                                    .{
                                        self.debug_origin_a,
                                        self.debug_origin_b,
                                        root_val.raw,
                                        @tagName(root_val.typeKind()),
                                    },
                                );
                                if (root_val.isChunk()) {
                                    const ch = root_val.toPtr(objects.Chunk);
                                    const ch_name = if (ch.name.typeKind() == .symbol)
                                        ch.name.toPtr(objects.Symbol).getName()
                                    else if (ch.name.typeKind() == .string)
                                        ch.name.toPtr(objects.String).bytes()
                                    else
                                        "<non-name>";
                                    std.debug.print(
                                        "TRACE bad-symbol-origin-chunk name={s} consts={d} code_len={d} allowed=0x{x}({s})\n",
                                        .{
                                            ch_name,
                                            ch.const_count,
                                            ch.code_len,
                                            ch.allowed_keywords.raw,
                                            @tagName(ch.allowed_keywords.typeKind()),
                                        },
                                    );
                                    std.debug.print(
                                        "TRACE bad-symbol-origin-chunk-lambda raw=0x{x} kind={s}\n",
                                        .{
                                            ch.lambda_expr.raw,
                                            @tagName(ch.lambda_expr.typeKind()),
                                        },
                                    );
                                    if (ch.lambda_expr.isCons()) {
                                        var lam_list = ch.lambda_expr;
                                        var li: usize = 0;
                                        while (lam_list.isCons() and li < 24) : (li += 1) {
                                            const lc = lam_list.toPtr(objects.Cons);
                                            std.debug.print(
                                                "TRACE bad-symbol-origin-chunk-lambda-cons idx={d} car=0x{x}({s}) cdr=0x{x}({s})\n",
                                                .{
                                                    li,
                                                    lc.car.raw,
                                                    @tagName(lc.car.typeKind()),
                                                    lc.cdr.raw,
                                                    @tagName(lc.cdr.typeKind()),
                                                },
                                            );
                                            lam_list = lc.cdr;
                                        }
                                    }
                                    if (ch.allowed_keywords.isCons()) {
                                        var kw_list = ch.allowed_keywords;
                                        var kwi: usize = 0;
                                        while (kw_list.isCons() and kwi < 16) : (kwi += 1) {
                                            const kwc = kw_list.toPtr(objects.Cons);
                                            std.debug.print(
                                                "TRACE bad-symbol-origin-chunk-allowed idx={d} car=0x{x}({s}) cdr=0x{x}({s})\n",
                                                .{
                                                    kwi,
                                                    kwc.car.raw,
                                                    @tagName(kwc.car.typeKind()),
                                                    kwc.cdr.raw,
                                                    @tagName(kwc.cdr.typeKind()),
                                                },
                                            );
                                            kw_list = kwc.cdr;
                                        }
                                    }
                                    const consts = ch.getConstants();
                                    var ci: usize = 0;
                                    while (ci < consts.len and ci < 24) : (ci += 1) {
                                        const cv = consts[ci];
                                        std.debug.print(
                                            "TRACE bad-symbol-origin-const idx={d} raw=0x{x} kind={s}\n",
                                            .{ ci, cv.raw, @tagName(cv.typeKind()) },
                                        );
                                        if (cv.isCons()) {
                                            const cc = cv.toPtr(objects.Cons);
                                            std.debug.print(
                                                "TRACE bad-symbol-origin-const-cons idx={d} car=0x{x}({s}) cdr=0x{x}({s})\n",
                                                .{
                                                    ci,
                                                    cc.car.raw,
                                                    @tagName(cc.car.typeKind()),
                                                    cc.cdr.raw,
                                                    @tagName(cc.cdr.typeKind()),
                                                },
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    },
                    .slot => {
                        if (self.debug_origin_a < self.debug_roots_slots.len) {
                            const slot_ptr = self.debug_roots_slots[self.debug_origin_a];
                            std.debug.print(
                                "TRACE bad-symbol-origin-slot idx={d} slot_ptr=0x{x} raw=0x{x} kind={s}\n",
                                .{
                                    self.debug_origin_a,
                                    @intFromPtr(slot_ptr),
                                    slot_ptr.*.raw,
                                    @tagName(slot_ptr.*.typeKind()),
                                },
                            );
                        }
                    },
                    .none => {},
                }
                if (self.debug_parent_tag == .boxed and self.debug_parent_addr != 0) {
                    const kind_raw = @as(*const u64, @ptrFromInt(self.debug_parent_addr)).*;
                    if (kind_raw <= @intFromEnum(objects.BoxedKind.macro_env)) {
                        const kind: objects.BoxedKind = @enumFromInt(kind_raw);
                        std.debug.print("TRACE bad-symbol-parent boxed-kind={s}\n", .{@tagName(kind)});
                        if (kind == .chunk) {
                            const chunk: *const objects.Chunk = @ptrFromInt(self.debug_parent_addr);
                            const chunk_name = if (chunk.name.typeKind() == .symbol)
                                chunk.name.toPtr(objects.Symbol).getName()
                            else if (chunk.name.typeKind() == .string)
                                chunk.name.toPtr(objects.String).bytes()
                            else
                                "<non-name>";
                            std.debug.print(
                                "TRACE bad-symbol-parent chunk name=0x{x} name-str={s} allowed=0x{x} consts={d}\n",
                                .{ chunk.name.raw, chunk_name, chunk.allowed_keywords.raw, chunk.const_count },
                            );
                        }
                    } else {
                        std.debug.print("TRACE bad-symbol-parent boxed-kind-raw=0x{x}\n", .{kind_raw});
                    }
                }
            }
        }
        const size = objects.objectSize(live);
        const aligned_size = std.mem.alignForward(usize, size, heap_mod.ALIGNMENT);
        const survivor_age: u8 = if (heap.gcLayoutMode() == .generational)
            heap.nextSurvivorAge(obj_addr)
        else
            0;

        var promote = shouldPromote(heap, tag, aligned_size, survivor_age);
        const dest: [*]u8 = if (promote)
            @ptrCast(heap.allocTenuredRaw(aligned_size) catch |err| {
                if (err == error.OutOfMemory and self.trace_gc_oom) {
                    std.debug.print(
                        "TRACE gc-copy-oom promote={any} tag={s} size={d} aligned={d} val=0x{x} obj=0x{x} phase={s}\n",
                        .{ true, @tagName(tag), size, aligned_size, live.raw, obj_addr, @tagName(self.major.phase) },
                    );
                }
                return err;
            })
        else blk: {
            const to_cur = @intFromPtr(alloc_ptr.*);
            const to_end = @intFromPtr(heap.to_start) + heap.space_size;
            if (to_cur <= to_end and aligned_size <= to_end - to_cur) {
                const out: [*]u8 = @ptrCast(alloc_ptr.*);
                alloc_ptr.* = @ptrFromInt(to_cur + aligned_size);
                break :blk out;
            }
            if (heap.gcLayoutMode() == .generational) {
                // Minor evacuation overflow: promote survivor into tenured space.
                // This keeps collection progressing instead of failing when nursery live
                // bytes temporarily exceed to-space capacity.
                promote = true;
                break :blk @ptrCast(heap.allocTenuredRaw(aligned_size) catch |err| {
                    if (err == error.OutOfMemory and self.trace_gc_oom) {
                        std.debug.print(
                            "TRACE gc-copy-oom promote={any} tag={s} size={d} aligned={d} val=0x{x} obj=0x{x} phase={s}\n",
                            .{ true, @tagName(tag), size, aligned_size, live.raw, obj_addr, @tagName(self.major.phase) },
                        );
                    }
                    return err;
                });
            }
            return error.OutOfMemory;
        };

        // Copy bytes
        const src: [*]const u8 = @ptrFromInt(obj_addr);
        @memcpy(dest[0..size], src[0..size]);

        const new_addr = @intFromPtr(dest);
        if (promote) {
            try heap.recordTenuredObject(new_addr, tag, aligned_size, survivor_age);
            _ = heap.markTenuredObject(new_addr);
            heap.stats.gc_promoted_bytes +%= aligned_size;
        }
        heap.noteSurvival(tag, new_addr, aligned_size, promote, survivor_age);
        if (heap.gcLayoutMode() == .generational) {
            try self.pushAgeUpdate(new_addr, survivor_age);
        }

        // Repair interior pointers that point to inline data
        // These pointers are relative to the object start and need adjustment
        const addr_delta: isize = @as(isize, @intCast(new_addr)) - @as(isize, @intCast(obj_addr));
        self.repairInteriorPointers(new_addr, tag, addr_delta);

        // Install forwarding pointer in old location
        // Store the forwarding pointer in first word and size in second word
        // This allows finalizeUnreachable to skip past forwarded objects
        first_word.* = Value.makeForwarding(@as(*u8, @ptrFromInt(new_addr)));
        const size_ptr: *usize = @ptrFromInt(obj_addr + @sizeOf(Value));
        size_ptr.* = aligned_size;

        // Add to work list for scanning if object may contain Value refs.
        if (objectHasRefsAtAddr(tag, new_addr)) {
            try self.pushWork(new_addr, tag);
            if (promote) {
                // Keep promoted containers in the remembered set until tenured GC exists.
                heap.markCardForOwnerAddr(new_addr);
            }
        }

        // Return new tagged pointer
        return self.rememberScanStore(heap, .{ .raw = new_addr | @as(u64, @intFromEnum(tag)) });
    }

    /// Repair interior pointers after copying an object
    /// Interior pointers point to inline data that follows the object header
    fn repairInteriorPointers(_: *GC, new_addr: usize, tag: Tag, addr_delta: isize) void {
        switch (tag) {
            .symbol => {
                // Symbol.name_ptr points to inline name data after header
                const sym: *objects.Symbol = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(sym.name_ptr);
                sym.name_ptr = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .keyword => {
                // Keyword.name_ptr points to inline name data after header
                const kw: *objects.Keyword = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(kw.name_ptr);
                kw.name_ptr = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .vector => {
                // Vector.data points to inline element array after header
                const vec: *objects.Vector = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(vec.data);
                vec.data = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .string => {
                // String.data points to inline byte data after header
                const str: *objects.String = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(str.data);
                str.data = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .closure => {
                // Closure.captures points to inline captures array after header
                const cls: *objects.Closure = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(cls.captures);
                cls.captures = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .boxed => {
                // Check discriminator to determine actual type
                const kind_ptr: *const objects.BoxedKind = @ptrFromInt(new_addr);
                switch (kind_ptr.*) {
                    .array => {
                        // Array.data_ptr points to data array
                        const arr: *objects.Array = @ptrFromInt(new_addr);
                        const old_ptr: usize = arr.data_ptr;
                        arr.data_ptr = @intCast(@as(isize, @intCast(old_ptr)) + addr_delta);
                    },
                    .chunk => {
                        // Chunk has two interior pointers:
                        // const_pool points to inline array after header
                        // code points to inline array after constants
                        const chunk: *objects.Chunk = @ptrFromInt(new_addr);
                        const old_const_pool = @intFromPtr(chunk.const_pool);
                        const old_code = @intFromPtr(chunk.code);
                        chunk.const_pool = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_const_pool)) + addr_delta)));
                        chunk.code = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_code)) + addr_delta)));
                    },
                    .string32 => {
                        // String32.data points to inline codepoint data after header
                        const s32: *objects.String32 = @ptrFromInt(new_addr);
                        const old_ptr = @intFromPtr(s32.data);
                        s32.data = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
                    },
                    .hashtable, .rational, .complex, .stream, .bignum, .pathname, .package, .condition, .class, .slotdef, .generic_function, .method, .native_code, .macro_env => {
                        // No interior pointers to repair
                    },
                }
            },
            .cons, .forwarding => {
                // No interior pointers to repair
            },
        }
    }

    /// Scan an object and copy its referenced values
    fn scanObject(self: *GC, heap: *heap_mod.Heap, addr: usize, tag: Tag, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8) !void {
        const saved_scan_addr = self.debug_scan_addr;
        const saved_scan_tag = self.debug_scan_tag;
        self.debug_scan_addr = addr;
        self.debug_scan_tag = tag;
        defer {
            self.debug_scan_addr = saved_scan_addr;
            self.debug_scan_tag = saved_scan_tag;
        }
        switch (tag) {
            .cons => {
                // Scan car and cdr
                const car_ptr: *Value = @ptrFromInt(addr);
                const cdr_ptr: *Value = @ptrFromInt(addr + @sizeOf(Value));

                if (car_ptr.isPointer() and !car_ptr.isNil()) {
                    car_ptr.* = try self.copyValue(heap, car_ptr.*, alloc_ptr);
                }
                if (cdr_ptr.isPointer() and !cdr_ptr.isNil()) {
                    cdr_ptr.* = try self.copyValue(heap, cdr_ptr.*, alloc_ptr);
                }
            },
            .symbol => {
                const sym: *objects.Symbol = @ptrFromInt(addr);
                if (sym.plist.isPointer() and !sym.plist.isNil()) {
                    sym.plist = try self.copyValue(heap, sym.plist, alloc_ptr);
                }
            },
            .vector => {
                // Scan all elements
                const vec: *objects.Vector = @ptrFromInt(addr);
                const scan_len = @min(vec.length, vec.capacity);
                for (vec.data[0..scan_len]) |*item| {
                    if (item.isPointer() and !item.isNil()) {
                        item.* = try self.copyValue(heap, item.*, alloc_ptr);
                    }
                }
            },
            .closure => {
                // Scan code Value and captured values
                const cls: *objects.Closure = @ptrFromInt(addr);
                if (cls.code.isPointer() and !cls.code.isNil()) {
                    cls.code = try self.copyValue(heap, cls.code, alloc_ptr);
                }
                for (cls.getCapturedValues()) |*cap| {
                    if (cap.isPointer() and !cap.isNil()) {
                        cap.* = try self.copyValue(heap, cap.*, alloc_ptr);
                    }
                }
            },
            .boxed => {
                // Check discriminator to determine actual type
                const kind_ptr: *const objects.BoxedKind = @ptrFromInt(addr);
                switch (kind_ptr.*) {
                    .hashtable => {
                        // Scan entries vector (which scans keys/values transitively)
                        const ht: *objects.HashTable = @ptrFromInt(addr);
                        if (ht.entries_vec.isPointer() and !ht.entries_vec.isNil()) {
                            ht.entries_vec = try self.copyValue(heap, ht.entries_vec, alloc_ptr);
                        }
                    },
                    .array => {
                        // Scan all array elements
                        const arr: *objects.Array = @ptrFromInt(addr);
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        for (0..arr.total_size) |i| {
                            if (data[i].isPointer() and !data[i].isNil()) {
                                data[i] = try self.copyValue(heap, data[i], alloc_ptr);
                            }
                        }
                    },
                    .pathname => {
                        // Scan all pathname component values
                        const pn: *objects.Pathname = @ptrFromInt(addr);
                        if (pn.host.isPointer() and !pn.host.isNil()) {
                            pn.host = try self.copyValue(heap, pn.host, alloc_ptr);
                        }
                        if (pn.device.isPointer() and !pn.device.isNil()) {
                            pn.device = try self.copyValue(heap, pn.device, alloc_ptr);
                        }
                        if (pn.directory.isPointer() and !pn.directory.isNil()) {
                            pn.directory = try self.copyValue(heap, pn.directory, alloc_ptr);
                        }
                        if (pn.name.isPointer() and !pn.name.isNil()) {
                            pn.name = try self.copyValue(heap, pn.name, alloc_ptr);
                        }
                        if (pn.type.isPointer() and !pn.type.isNil()) {
                            pn.type = try self.copyValue(heap, pn.type, alloc_ptr);
                        }
                        if (pn.version.isPointer() and !pn.version.isNil()) {
                            pn.version = try self.copyValue(heap, pn.version, alloc_ptr);
                        }
                    },
                    .package => {
                        // Scan all package fields
                        const pkg: *objects.Package = @ptrFromInt(addr);
                        if (pkg.name.isPointer() and !pkg.name.isNil()) {
                            pkg.name = try self.copyValue(heap, pkg.name, alloc_ptr);
                        }
                        if (pkg.nicknames.isPointer() and !pkg.nicknames.isNil()) {
                            pkg.nicknames = try self.copyValue(heap, pkg.nicknames, alloc_ptr);
                        }
                        if (pkg.use_list.isPointer() and !pkg.use_list.isNil()) {
                            pkg.use_list = try self.copyValue(heap, pkg.use_list, alloc_ptr);
                        }
                        if (pkg.exports.isPointer() and !pkg.exports.isNil()) {
                            pkg.exports = try self.copyValue(heap, pkg.exports, alloc_ptr);
                        }
                        if (pkg.symbols.isPointer() and !pkg.symbols.isNil()) {
                            pkg.symbols = try self.copyValue(heap, pkg.symbols, alloc_ptr);
                        }
                        if (pkg.shadowing.isPointer() and !pkg.shadowing.isNil()) {
                            pkg.shadowing = try self.copyValue(heap, pkg.shadowing, alloc_ptr);
                        }
                    },
                    .macro_env => {
                        const env: *objects.MacroEnv = @ptrFromInt(addr);
                        if (env.macros.isPointer() and !env.macros.isNil()) {
                            env.macros = try self.copyValue(heap, env.macros, alloc_ptr);
                        }
                        if (env.symbol_macros.isPointer() and !env.symbol_macros.isNil()) {
                            env.symbol_macros = try self.copyValue(heap, env.symbol_macros, alloc_ptr);
                        }
                    },
                    .chunk => {
                        // Scan chunk metadata + all constants in the constant pool
                        const chunk: *objects.Chunk = @ptrFromInt(addr);
                        const chunk_in_nursery = heap.isInNurseryAddr(addr);
                        const chunk_size = objects.objectSize(Value.makeChunk(chunk));
                        const chunk_marked = heap.hasMarkedCardInAddrRange(addr, addr + chunk_size);
                        const from_start_dbg = @intFromPtr(heap.from_start);
                        const from_end_dbg = @intFromPtr(heap.from_end);
                        const chunk_in_from = addr >= from_start_dbg and addr < from_end_dbg;
                        var chunk_in_tenured = false;
                        for (heap.tenured_objs.items) |obj| {
                            if (obj.addr == addr) {
                                chunk_in_tenured = true;
                                break;
                            }
                        }
                        var chunk_in_los = false;
                        for (heap.los_objs.items) |obj| {
                            if (obj.addr == addr) {
                                chunk_in_los = true;
                                break;
                            }
                        }
                        const stale_start = @intFromPtr(heap.to_start);
                        const stale_end = stale_start + heap.space_size;
                        const trace_chunk_stale = self.trace_stale_resolve;
                        if (chunk.lambda_expr.isPointer() and !chunk.lambda_expr.isNil()) {
                            if (trace_chunk_stale) {
                                const ptr = chunk.lambda_expr.toPtrAddr();
                                if (ptr >= stale_start and ptr < stale_end) {
                                    std.debug.print(
                                        "TRACE chunk-stale field=lambda-expr chunk=0x{x} in-from={any} from=[0x{x},0x{x}) nursery={any} marked={any} tenured={any} los={any} name=0x{x}({s}) val=0x{x}\n",
                                        .{
                                            addr,
                                            chunk_in_from,
                                            from_start_dbg,
                                            from_end_dbg,
                                            chunk_in_nursery,
                                            chunk_marked,
                                            chunk_in_tenured,
                                            chunk_in_los,
                                            chunk.name.raw,
                                            @tagName(chunk.name.typeKind()),
                                            chunk.lambda_expr.raw,
                                        },
                                    );
                                }
                            }
                            chunk.lambda_expr = try self.copyValue(heap, chunk.lambda_expr, alloc_ptr);
                        }
                        if (chunk.name.isPointer() and !chunk.name.isNil()) {
                            if (trace_chunk_stale) {
                                const ptr = chunk.name.toPtrAddr();
                                if (ptr >= stale_start and ptr < stale_end) {
                                    std.debug.print(
                                        "TRACE chunk-stale field=name chunk=0x{x} nursery={any} name=0x{x}({s}) val=0x{x}\n",
                                        .{
                                            addr,
                                            chunk_in_nursery,
                                            chunk.name.raw,
                                            @tagName(chunk.name.typeKind()),
                                            chunk.name.raw,
                                        },
                                    );
                                }
                            }
                            chunk.name = try self.copyValue(heap, chunk.name, alloc_ptr);
                        }
                        if (chunk.allowed_keywords.isPointer() and !chunk.allowed_keywords.isNil()) {
                            if (trace_chunk_stale) {
                                const ptr = chunk.allowed_keywords.toPtrAddr();
                                if (ptr >= stale_start and ptr < stale_end) {
                                    std.debug.print(
                                        "TRACE chunk-stale field=allowed-keywords chunk=0x{x} nursery={any} name=0x{x}({s}) val=0x{x}\n",
                                        .{
                                            addr,
                                            chunk_in_nursery,
                                            chunk.name.raw,
                                            @tagName(chunk.name.typeKind()),
                                            chunk.allowed_keywords.raw,
                                        },
                                    );
                                }
                            }
                            chunk.allowed_keywords = try self.copyValue(heap, chunk.allowed_keywords, alloc_ptr);
                        }
                        for (chunk.getConstants(), 0..) |*const_val, ci| {
                            if (const_val.isPointer() and !const_val.isNil()) {
                                if (trace_chunk_stale) {
                                    const ptr = const_val.toPtrAddr();
                                    if (ptr >= stale_start and ptr < stale_end) {
                                        std.debug.print(
                                            "TRACE chunk-stale field=const idx={d} chunk=0x{x} nursery={any} name=0x{x}({s}) val=0x{x} kind={s}\n",
                                            .{
                                                ci,
                                                addr,
                                                chunk_in_nursery,
                                                chunk.name.raw,
                                                @tagName(chunk.name.typeKind()),
                                                const_val.raw,
                                                @tagName(const_val.typeKind()),
                                            },
                                        );
                                    }
                                }
                                const_val.* = try self.copyValue(heap, const_val.*, alloc_ptr);
                            }
                        }
                    },
                    .rational, .complex, .bignum, .native_code => {
                        // No Value references to scan
                    },
                    .condition => {
                        // Scan condition Value references
                        const cond: *objects.Condition = @ptrFromInt(addr);
                        if (cond.type_sym.isPointer() and !cond.type_sym.isNil()) {
                            cond.type_sym = try self.copyValue(heap, cond.type_sym, alloc_ptr);
                        }
                        if (cond.format_control.isPointer() and !cond.format_control.isNil()) {
                            cond.format_control = try self.copyValue(heap, cond.format_control, alloc_ptr);
                        }
                        if (cond.format_args.isPointer() and !cond.format_args.isNil()) {
                            cond.format_args = try self.copyValue(heap, cond.format_args, alloc_ptr);
                        }
                    },
                    .class => {
                        // Scan class Value references
                        const cls: *objects.Class = @ptrFromInt(addr);
                        if (cls.name.isPointer() and !cls.name.isNil()) {
                            cls.name = try self.copyValue(heap, cls.name, alloc_ptr);
                        }
                        if (cls.direct_supers.isPointer() and !cls.direct_supers.isNil()) {
                            cls.direct_supers = try self.copyValue(heap, cls.direct_supers, alloc_ptr);
                        }
                        if (cls.cpl.isPointer() and !cls.cpl.isNil()) {
                            cls.cpl = try self.copyValue(heap, cls.cpl, alloc_ptr);
                        }
                        if (cls.direct_slots.isPointer() and !cls.direct_slots.isNil()) {
                            cls.direct_slots = try self.copyValue(heap, cls.direct_slots, alloc_ptr);
                        }
                        if (cls.slots.isPointer() and !cls.slots.isNil()) {
                            cls.slots = try self.copyValue(heap, cls.slots, alloc_ptr);
                        }
                        if (cls.metaclass.isPointer() and !cls.metaclass.isNil()) {
                            cls.metaclass = try self.copyValue(heap, cls.metaclass, alloc_ptr);
                        }
                        for (cls.shared_slots[0..cls.num_shared]) |*slot_val| {
                            if (slot_val.isPointer() and !slot_val.isNil()) {
                                slot_val.* = try self.copyValue(heap, slot_val.*, alloc_ptr);
                            }
                        }
                    },
                    .stream => {
                        // Scan source_value if present
                        const stream: *objects.Stream = @ptrFromInt(addr);
                        if (!stream.source_value.isNil() and stream.source_value.isPointer()) {
                            stream.source_value = try self.copyValue(heap, stream.source_value, alloc_ptr);
                            // Recompute data_ptr from relocated string
                            if (stream.source_value.typeKind() == .string) {
                                const str = stream.source_value.toPtr(objects.String);
                                stream.data_ptr = @intFromPtr(str.data);
                            }
                        }
                    },
                    .slotdef => {
                        // Scan slot definition Value references
                        const slotdef: *objects.SlotDefinition = @ptrFromInt(addr);
                        if (slotdef.name.isPointer() and !slotdef.name.isNil()) {
                            slotdef.name = try self.copyValue(heap, slotdef.name, alloc_ptr);
                        }
                        if (slotdef.initform.isPointer() and !slotdef.initform.isNil()) {
                            slotdef.initform = try self.copyValue(heap, slotdef.initform, alloc_ptr);
                        }
                        if (slotdef.initargs.isPointer() and !slotdef.initargs.isNil()) {
                            slotdef.initargs = try self.copyValue(heap, slotdef.initargs, alloc_ptr);
                        }
                        if (slotdef.readers.isPointer() and !slotdef.readers.isNil()) {
                            slotdef.readers = try self.copyValue(heap, slotdef.readers, alloc_ptr);
                        }
                        if (slotdef.writers.isPointer() and !slotdef.writers.isNil()) {
                            slotdef.writers = try self.copyValue(heap, slotdef.writers, alloc_ptr);
                        }
                        if (slotdef.allocation.isPointer() and !slotdef.allocation.isNil()) {
                            slotdef.allocation = try self.copyValue(heap, slotdef.allocation, alloc_ptr);
                        }
                        if (slotdef.slot_type.isPointer() and !slotdef.slot_type.isNil()) {
                            slotdef.slot_type = try self.copyValue(heap, slotdef.slot_type, alloc_ptr);
                        }
                    },
                    .string32 => {
                        // No Value references to scan
                    },
                    .generic_function => {
                        // Scan generic function Value references
                        const gf: *objects.GenericFunction = @ptrFromInt(addr);
                        if (gf.name.isPointer() and !gf.name.isNil()) {
                            gf.name = try self.copyValue(heap, gf.name, alloc_ptr);
                        }
                        if (gf.lambda_list.isPointer() and !gf.lambda_list.isNil()) {
                            gf.lambda_list = try self.copyValue(heap, gf.lambda_list, alloc_ptr);
                        }
                        if (gf.methods.isPointer() and !gf.methods.isNil()) {
                            gf.methods = try self.copyValue(heap, gf.methods, alloc_ptr);
                        }
                        if (gf.dispatcher.isPointer() and !gf.dispatcher.isNil()) {
                            gf.dispatcher = try self.copyValue(heap, gf.dispatcher, alloc_ptr);
                        }
                    },
                    .method => {
                        // Scan method Value references
                        const method: *objects.Method = @ptrFromInt(addr);
                        if (method.qualifiers.isPointer() and !method.qualifiers.isNil()) {
                            method.qualifiers = try self.copyValue(heap, method.qualifiers, alloc_ptr);
                        }
                        if (method.specializers.isPointer() and !method.specializers.isNil()) {
                            method.specializers = try self.copyValue(heap, method.specializers, alloc_ptr);
                        }
                        if (method.lambda_list.isPointer() and !method.lambda_list.isNil()) {
                            method.lambda_list = try self.copyValue(heap, method.lambda_list, alloc_ptr);
                        }
                        if (method.function.isPointer() and !method.function.isNil()) {
                            method.function = try self.copyValue(heap, method.function, alloc_ptr);
                        }
                    },
                }
            },
            .string, .keyword => {
                // No Value references to scan
            },
            .forwarding => {
                // Should not happen - forwarding pointers aren't added to work list
            },
        }
    }
};

// ============================================================================
// Tests
// ============================================================================

test "gc init" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var gc_inst = GC.init(testing.allocator);
    defer gc_inst.deinit();
}

test "gc collect empty" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Use heap.collectGarbage which handles internal roots (lisp_packages)
    var roots = [_]Value{};
    _ = try heap.collectGarbage(&roots);

    // After GC, only lisp_packages hash table should remain
    try testing.expect(heap.bytesUsed() > 0);
}

test "gc collect with cons" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Allocate a cons cell
    var root = try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2));

    // Verify it's valid
    try testing.expect(root.isCons());

    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    // Collect with root
    var roots = [_]Value{root};
    const bytes = try gc.collect(&heap, &roots);

    // Should have copied the cons cell
    try testing.expect(bytes >= @sizeOf(objects.Cons));

    // Root should be updated to new location
    root = roots[0];
    try testing.expect(root.isCons());

    // Values should be preserved
    const cons = root.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), cons.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), cons.cdr.toFixnum());
}

test "gc collectRootSet updates slots" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var root = try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2));
    const raw_before = root.raw;

    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    var slots = [_]*Value{&root};
    const bytes = try gc.collectRootSet(&heap, .{ .ranges = &[_]roots_mod.RootRange{}, .slots = slots[0..] });

    try testing.expect(bytes >= @sizeOf(objects.Cons));
    try testing.expect(root.isCons());
    try testing.expect(root.raw != raw_before);

    const cons = root.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), cons.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), cons.cdr.toFixnum());
}

test "gc collect with nested cons" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Build (1 . (2 . (3 . nil)))
    const c3 = try heap.allocCons(Value.makeFixnum(3), Value.nil);
    const c2 = try heap.allocCons(Value.makeFixnum(2), c3);
    var root = try heap.allocCons(Value.makeFixnum(1), c2);

    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    var roots = [_]Value{root};
    _ = try gc.collect(&heap, &roots);

    // Verify structure is preserved
    root = roots[0];
    try testing.expect(root.isCons());
    const cons1 = root.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), cons1.car.toFixnum());

    try testing.expect(cons1.cdr.isCons());
    const cons2 = cons1.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 2), cons2.car.toFixnum());

    try testing.expect(cons2.cdr.isCons());
    const cons3 = cons2.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 3), cons3.car.toFixnum());
    try testing.expect(cons3.cdr.isNil());
}

test "gc grows work_list after peak use" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    try gc.work_list.ensureTotalCapacity(testing.allocator, 32);
    const cap0 = gc.work_list.capacity;
    const n: usize = (cap0 * 3) / 4 + 1;

    const vec_val = try heap.allocVector(n, n);
    const vec = vec_val.toPtr(objects.Vector);
    for (0..n) |i| {
        const cons = try heap.allocCons(Value.makeFixnum(@as(i64, @intCast(i))), Value.makeFixnum(0));
        vec.set(i, cons);
    }

    var roots = [_]Value{vec_val};
    _ = try gc.collect(&heap, &roots);

    try testing.expectEqual(n, gc.work_peak);
    try testing.expect(gc.work_list.capacity >= cap0 * 2);
}

test "gc finalizes unreachable file streams" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const tmp_path = "/tmp/habu_gc_test_stream.txt";

    // Create and write to a file
    {
        const file = try std.fs.createFileAbsolute(tmp_path, .{});
        defer file.close();
        try file.writeAll("test data\n");
    }

    // Open file stream
    const file = try std.fs.openFileAbsolute(tmp_path, .{});
    const fd = file.handle;
    const stream = try heap.allocStream(.input, .file, fd);

    // Create a root that references the stream
    var root = stream;
    try testing.expectEqual(@as(usize, 1), heap.stream_list.items.len);

    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    // First GC - stream is reachable, should not be finalized
    var roots = [_]Value{root};
    _ = try gc.collect(&heap, &roots);
    root = roots[0];
    try testing.expectEqual(@as(usize, 1), heap.stream_list.items.len);

    // Verify stream is still valid
    try testing.expect(root.isBoxed());

    // Second GC - stream becomes unreachable (empty roots)
    var empty_roots = [_]Value{};
    _ = try gc.collect(&heap, &empty_roots);
    try testing.expectEqual(@as(usize, 0), heap.stream_list.items.len);

    // File descriptor should be closed by finalizer
    // We can't directly verify the FD is closed, but we tested the finalization path
    try std.fs.deleteFileAbsolute(tmp_path);
}

test "gc finalizer path coverage" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Test that finalizeUnreachable visits stream objects
    // We don't test actual resource cleanup to avoid allocator mismatch issues
    const stream = try heap.allocStream(.input, .file, -1);
    const stream_ptr = stream.toPtr(objects.Stream);
    stream_ptr.closed = true; // Mark as closed so finalizer doesn't try to close

    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    // Stream becomes unreachable
    var empty_roots = [_]Value{};
    _ = try gc.collect(&heap, &empty_roots);

    try testing.expectEqual(@as(usize, 0), heap.stream_list.items.len);
}

test "package gc correctness" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Create package
    const pkg_name = try heap.intern("TEST-PKG");
    const pkg = try heap.allocPackage(pkg_name, Value.nil, Value.nil, false);

    var root = pkg;
    var gc = GC.init(testing.allocator);
    defer gc.deinit();

    // GC with package rooted
    var roots = [_]Value{root};
    _ = try gc.collect(&heap, &roots);
    root = roots[0];

    // Verify package structure intact after GC
    try testing.expect(root.isBoxed());
    const pkg_after = root.toPtr(objects.Package);
    // Note: pkg_name symbol may have been moved by GC, need to compare through symbol string
    try testing.expect(pkg_after.name.isSymbol());
    const name_after = pkg_after.name.toPtr(objects.Symbol);
    try testing.expect(std.mem.eql(u8, name_after.getName(), "TEST-PKG"));
}

test "gc scans class metaclass" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const name = try heap.intern("TEST-CLASS");
    const cls = try heap.alloc(objects.Class);
    cls.* = .{
        .kind = .class,
        .name = name,
        .direct_supers = Value.nil,
        .cpl = Value.nil,
        .direct_slots = Value.nil,
        .slots = Value.nil,
        .metaclass = heap.standard_class,
        .num_shared = 0,
        // num_shared=0 => must still be non-null per Zig pointer rules.
        .shared_slots = @ptrFromInt(@as(usize, @alignOf(Value))),
    };

    var roots = [_]Value{Value.makeClass(cls)};
    _ = try heap.collectGarbage(&roots);
    const cls_after = roots[0].toPtr(objects.Class);
    try testing.expect(cls_after.metaclass.eq(heap.standard_class));

    // Second collection should still preserve/update metaclass field.
    _ = try heap.collectGarbage(&roots);
    const cls_after2 = roots[0].toPtr(objects.Class);
    try testing.expect(cls_after2.metaclass.eq(heap.standard_class));
}

test "gc scans generic function dispatcher" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const name = try heap.intern("TEST-GF");
    const dispatcher = try heap.allocClosure(Value.nil, 0, &[_]Value{});

    const gf = try heap.alloc(objects.GenericFunction);
    gf.* = .{
        .kind = .generic_function,
        .name = name,
        .lambda_list = Value.nil,
        .methods = Value.nil,
        .dispatcher = dispatcher,
    };

    var roots = [_]Value{Value.makeGenericFunction(gf)};
    _ = try heap.collectGarbage(&roots);

    const gf_after = roots[0].toPtr(objects.GenericFunction);
    try testing.expect(gf_after.dispatcher.isClosure());

    // If dispatcher isn't scanned, it will still point into the old semispace.
    const disp_addr = gf_after.dispatcher.toPtrAddr();
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(disp_addr >= start and disp_addr < end);
}

test "gc records minor and major pause stats by layout mode" {
    const testing = std.testing;

    {
        var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
        defer heap.deinit();

        var roots = [_]Value{try heap.allocCons(Value.makeFixnum(1), Value.nil)};
        _ = try heap.collectGarbage(&roots);
        try testing.expect(heap.stats.gc_major_count > 0);
        try testing.expect(heap.stats.gc_major_ns > 0);
        try testing.expectEqual(@as(usize, 0), heap.stats.gc_minor_count);
        try testing.expectEqual(@as(u64, 0), heap.stats.gc_minor_ns);
    }

    {
        var heap = try heap_mod.Heap.init(testing.allocator, .{
            .total_size = 8 * 1024 * 1024,
            .gc_layout = .generational,
            .generational = .{
                .nursery_each = 512 * 1024,
                .los_size = 512 * 1024,
            },
        });
        defer heap.deinit();

        var roots = [_]Value{try heap.allocCons(Value.makeFixnum(2), Value.nil)};
        _ = try heap.collectGarbage(&roots);
        try testing.expect(heap.stats.gc_minor_count > 0);
        try testing.expect(heap.stats.gc_minor_ns > 0);
        try testing.expectEqual(@as(usize, 0), heap.stats.gc_major_count);
    }
}

test "nursery policy shrinks when survival and pause are high" {
    const testing = std.testing;
    const current = 1024 * 1024;
    const out = deriveNurseryPolicy(.{
        .current_bytes = current,
        .min_bytes = 256 * 1024,
        .max_bytes = 4 * 1024 * 1024,
        .survive_bytes = 900 * 1024,
        .copied_bytes = 1024 * 1024,
        .p95_pause_ns = 20_000_000,
        .target_pause_ns = 10_000_000,
    });
    try testing.expect(out.target_bytes < current);
    try testing.expect(out.survival_ratio > 0.8);
}

test "nursery policy grows when survival and pause are low" {
    const testing = std.testing;
    const current = 1024 * 1024;
    const out = deriveNurseryPolicy(.{
        .current_bytes = current,
        .min_bytes = 256 * 1024,
        .max_bytes = 4 * 1024 * 1024,
        .survive_bytes = 64 * 1024,
        .copied_bytes = 1024 * 1024,
        .p95_pause_ns = 4_000_000,
        .target_pause_ns = 10_000_000,
    });
    try testing.expect(out.target_bytes > current);
    try testing.expect(out.pause_error < 0.0);
}

test "nursery policy keeps current size inside deadband" {
    const testing = std.testing;
    const current = 1024 * 1024;
    const out = deriveNurseryPolicy(.{
        .current_bytes = current,
        .min_bytes = 256 * 1024,
        .max_bytes = 4 * 1024 * 1024,
        .survive_bytes = 256 * 1024,
        .copied_bytes = 1024 * 1024,
        .p95_pause_ns = 10_000_000,
        .target_pause_ns = 10_000_000,
    });
    try testing.expectEqual(current, out.target_bytes);
}

test "nursery policy clamps to min and max bounds" {
    const testing = std.testing;

    const low = deriveNurseryPolicy(.{
        .current_bytes = 512 * 1024,
        .min_bytes = 384 * 1024,
        .max_bytes = 2 * 1024 * 1024,
        .survive_bytes = 512 * 1024,
        .copied_bytes = 512 * 1024,
        .p95_pause_ns = 50_000_000,
        .target_pause_ns = 5_000_000,
    });
    try testing.expectEqual(@as(usize, 384 * 1024), low.target_bytes);

    const high = deriveNurseryPolicy(.{
        .current_bytes = 1400 * 1024,
        .min_bytes = 256 * 1024,
        .max_bytes = 1536 * 1024,
        .survive_bytes = 0,
        .copied_bytes = 1024 * 1024,
        .p95_pause_ns = 1_000_000,
        .target_pause_ns = 50_000_000,
    });
    try testing.expectEqual(@as(usize, 1536 * 1024), high.target_bytes);
}

test "debt trigger collects when debt reaches threshold" {
    const testing = std.testing;

    const out = deriveDebtTrigger(.{
        .debt_bytes = 1024 * 1024,
        .debt_threshold = 1024 * 1024,
        .nursery_used_bytes = 256 * 1024,
        .nursery_target_bytes = 1024 * 1024,
        .survival_ratio = 0.25,
        .pause_error = 0.0,
    });
    try testing.expect(out.should_collect);
    try testing.expect(out.debt_ratio >= 1.0);
}

test "debt trigger backs off under high pause pressure near threshold" {
    const testing = std.testing;

    const out = deriveDebtTrigger(.{
        .debt_bytes = 1024 * 1024,
        .debt_threshold = 1024 * 1024,
        .nursery_used_bytes = 64 * 1024,
        .nursery_target_bytes = 1024 * 1024,
        .survival_ratio = 0.90,
        .pause_error = 1.0,
    });
    try testing.expect(!out.should_collect);
    try testing.expect(out.score < 0.65);
}

test "debt trigger hard-collects on extreme debt" {
    const testing = std.testing;

    const out = deriveDebtTrigger(.{
        .debt_bytes = 2 * 1024 * 1024,
        .debt_threshold = 1024 * 1024,
        .nursery_used_bytes = 32 * 1024,
        .nursery_target_bytes = 1024 * 1024,
        .survival_ratio = 1.0,
        .pause_error = 1.0,
    });
    try testing.expect(out.should_collect);
    try testing.expect(out.debt_ratio >= 1.25);
}

test "tenuring policy raises threshold for young low-success promotions" {
    const testing = std.testing;

    const out = deriveTenuringPolicy(.{
        .current_bytes = 1024,
        .min_bytes = 256,
        .max_bytes = 4096,
        .promote_n = 100,
        .promote_success_n = 5,
        .promote_age = .{ 0, 70, 20, 10, 0, 0, 0, 0 },
        .survive_n = 1000,
        .survive_age = .{ 0, 500, 200, 100, 100, 50, 30, 20 },
    });
    try testing.expect(out.target_bytes > 1024);
    try testing.expect(out.scale > 1.0);
    try testing.expect(out.success_rate < 0.25);
}

test "tenuring policy lowers threshold on mature survivor pressure" {
    const testing = std.testing;

    const out = deriveTenuringPolicy(.{
        .current_bytes = 2048,
        .min_bytes = 256,
        .max_bytes = 4096,
        .promote_n = 20,
        .promote_success_n = 18,
        .promote_age = .{ 0, 1, 2, 1, 4, 4, 4, 4 },
        .survive_n = 1000,
        .survive_age = .{ 0, 50, 60, 90, 200, 200, 200, 200 },
    });
    try testing.expect(out.target_bytes < 2048);
    try testing.expect(out.scale < 1.0);
    try testing.expect(out.mature_survive_ratio > 0.20);
}

test "tenuring policy keeps threshold inside deadband" {
    const testing = std.testing;

    const out = deriveTenuringPolicy(.{
        .current_bytes = 2048,
        .min_bytes = 256,
        .max_bytes = 4096,
        .promote_n = 0,
        .promote_success_n = 0,
        .promote_age = .{0} ** heap_mod.GC_AGE_N,
        .survive_n = 1000,
        .survive_age = .{ 0, 800, 100, 50, 20, 15, 10, 5 },
    });
    try testing.expectEqual(@as(usize, 2048), out.target_bytes);
    try testing.expectEqual(@as(f64, 1.0), out.scale);
}

test "los policy lowers threshold when large-object share is high" {
    const testing = std.testing;

    const out = deriveLosPolicy(.{
        .current_bytes = 4096,
        .min_bytes = 512,
        .max_bytes = 16384,
        .alloc_size = .{ 0, 0, 0, 0, 10, 20, 40, 30 },
        .los_live_bytes = 128 * 1024,
        .los_capacity_bytes = 1024 * 1024,
        .p95_pause_ns = 5_000_000,
        .target_pause_ns = 10_000_000,
    });
    try testing.expect(out.target_bytes < 4096);
    try testing.expect(out.scale < 1.0);
    try testing.expect(out.large_alloc_ratio > 0.35);
}

test "los policy raises threshold when occupancy or pauses are high" {
    const testing = std.testing;

    const out = deriveLosPolicy(.{
        .current_bytes = 4096,
        .min_bytes = 512,
        .max_bytes = 16384,
        .alloc_size = .{ 20, 20, 20, 20, 10, 5, 3, 2 },
        .los_live_bytes = 980 * 1024,
        .los_capacity_bytes = 1024 * 1024,
        .p95_pause_ns = 16_000_000,
        .target_pause_ns = 10_000_000,
    });
    try testing.expect(out.target_bytes > 4096);
    try testing.expect(out.scale > 1.0);
    try testing.expect(out.occupancy_ratio > 0.90);
    try testing.expect(out.pause_error > 0.35);
}

test "los policy stays stable in deadband" {
    const testing = std.testing;

    const out = deriveLosPolicy(.{
        .current_bytes = 4096,
        .min_bytes = 512,
        .max_bytes = 16384,
        .alloc_size = .{ 10, 10, 10, 10, 10, 10, 5, 5 },
        .los_live_bytes = 384 * 1024,
        .los_capacity_bytes = 1024 * 1024,
        .p95_pause_ns = 10_000_000,
        .target_pause_ns = 10_000_000,
    });
    try testing.expectEqual(@as(usize, 4096), out.target_bytes);
    try testing.expectEqual(@as(f64, 1.0), out.scale);
}

test "minor gc applies adaptive promote threshold at runtime" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const initial = heap.promote_threshold;
    var roots = [_]Value{try heap.allocBaseString("abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789")};
    _ = try heap.collectGarbage(&roots);

    try testing.expect(heap.promote_threshold >= initial);
    try testing.expectEqual(heap.promote_threshold, heap.stats.gc_promote_threshold);
    try testing.expect(heap.stats.gc_promote_scale >= 1.0);
}

test "minor gc applies adaptive nursery target at runtime" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const initial_target = heap.nursery_target_bytes;
    var roots = [_]Value{try heap.allocBaseString("abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789")};
    _ = try heap.collectGarbage(&roots);

    try testing.expect(heap.nursery_target_bytes > 0);
    try testing.expectEqual(heap.nursery_target_bytes, heap.gc_threshold);
    try testing.expectEqual(heap.nursery_target_bytes, heap.stats.gc_nursery_target);
    try testing.expect(heap.nursery_target_bytes >= heap.bytesUsed());
    try testing.expect(heap.nursery_target_bytes <= initial_target);
}

test "minor gc applies adaptive los threshold at runtime" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 4096,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    for (0..128) |_| {
        _ = try heap.allocBaseString(payload);
    }

    var roots = [_]Value{};
    _ = try heap.collectGarbage(&roots);

    try testing.expect(heap.los_threshold >= heap.los_threshold_min);
    try testing.expect(heap.los_threshold <= heap.los_threshold_max);
    try testing.expectEqual(heap.los_threshold, heap.stats.gc_los_threshold);
    try testing.expectEqual(heap.los_threshold_min, heap.stats.gc_los_threshold_min);
    try testing.expectEqual(heap.los_threshold_max, heap.stats.gc_los_threshold_max);
    try testing.expect(heap.stats.gc_los_large_ratio > 0.0);
    try testing.expect(heap.stats.gc_los_occupancy >= 0.0);
    try testing.expect(heap.stats.gc_los_occupancy <= 2.0);
}

test "setNurseryTarget keeps threshold above live nursery bytes" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 4096,
        },
    });
    defer heap.deinit();

    var list = Value.nil;
    for (0..8192) |_| {
        list = try heap.allocCons(Value.makeFixnum(1), list);
    }
    var roots = [_]Value{list};
    _ = try heap.collectGarbage(&roots);

    const live = heap.bytesUsed();
    heap.setNurseryTarget(heap.nursery_min_bytes, 1.0, 1.0, 0.5);
    try testing.expect(heap.gc_threshold >= live);
    try testing.expectEqual(heap.gc_threshold, heap.nursery_target_bytes);
}

test "minor gc promotes large survivors to tenured" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    var root = try heap.allocBaseString("abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789");
    try testing.expect(heap.isInNurseryAddr(root.toPtrAddr()));

    var roots = [_]Value{root};
    _ = try heap.collectGarbage(&roots);
    root = roots[0];

    const tenured = heap.tenuredRegion().?;
    const addr = root.toPtrAddr();
    try testing.expect(addr >= @intFromPtr(tenured.start) and addr < @intFromPtr(tenured.end));
    try testing.expect(heap.stats.gc_promoted_bytes > 0);
    try testing.expect(heap.stats.gc_survive_n > 0);
    try testing.expect(heap.stats.gc_survive_bytes >= heap.stats.gc_promoted_bytes);
    try testing.expect(heap.stats.gc_promote_n > 0);
    try testing.expectEqual(heap.stats.gc_promote_bytes, heap.stats.gc_promoted_bytes);
    var survive_age_sum: usize = 0;
    for (heap.stats.gc_survive_age) |n| survive_age_sum +%= n;
    try testing.expectEqual(heap.stats.gc_survive_n, survive_age_sum);
    var promote_age_sum: usize = 0;
    for (heap.stats.gc_promote_age) |n| promote_age_sum +%= n;
    try testing.expectEqual(heap.stats.gc_promote_n, promote_age_sum);
    try testing.expect(heap.stats.gc_promote_age[1] > 0);
    try testing.expect(heap.tenuredBytesUsed() > 0);
}

test "minor gc promotes aged small survivors to tenured" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            // Keep size-gate effectively disabled for cons cells so this test
            // exercises age-gated promotion.
            .promote_threshold = 1024 * 1024,
        },
    });
    defer heap.deinit();

    var root = try heap.allocCons(Value.makeFixnum(7), Value.nil);
    var roots = [_]Value{root};

    _ = try heap.collectGarbage(&roots);
    root = roots[0];
    try testing.expect(heap.isInNurseryAddr(root.toPtrAddr()));

    _ = try heap.collectGarbage(&roots);
    root = roots[0];

    const tenured = heap.tenuredRegion().?;
    const addr = root.toPtrAddr();
    try testing.expect(addr >= @intFromPtr(tenured.start) and addr < @intFromPtr(tenured.end));
    try testing.expect(heap.stats.gc_promote_n > 0);
    try testing.expect(heap.stats.gc_promote_age[PROMOTE_AGE_THRESHOLD] > 0);
}

test "promotion success telemetry records surviving promoted objects" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    var s = try heap.allocBaseString(payload);
    var roots = [_]Value{s};

    _ = try heap.collectGarbage(&roots);
    s = roots[0];
    try testing.expect(heap.stats.gc_promote_n > 0);
    const promote_n = heap.stats.gc_promote_n;
    const promote_bytes = heap.stats.gc_promote_bytes;

    _ = try heap.collectGarbage(&roots);
    s = roots[0];

    try testing.expect(heap.stats.gc_promote_success_n > 0);
    try testing.expect(heap.stats.gc_promote_success_n <= promote_n);
    try testing.expect(heap.stats.gc_promote_success_bytes > 0);
    try testing.expect(heap.stats.gc_promote_success_bytes <= promote_bytes);

    var success_age_sum: usize = 0;
    for (heap.stats.gc_promote_success_age) |n| success_age_sum +%= n;
    try testing.expectEqual(heap.stats.gc_promote_success_n, success_age_sum);
}

fn tenuredContainsAddr(heap: *const heap_mod.Heap, addr: usize) bool {
    for (heap.tenured_objs.items) |obj| {
        if (obj.addr == addr) return true;
    }
    return false;
}

test "minor gc preserves ref container edges across promotions" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 128,
        },
    });
    defer heap.deinit();

    const child = try heap.allocCons(Value.makeFixnum(9), Value.nil);
    const owner_val = try heap.allocVector(1, 64); // Large, but has refs.
    const owner = owner_val.toPtr(objects.Vector);
    owner.set(0, child);

    const tenured_before = heap.tenured_objs.items.len;
    var roots = [_]Value{owner_val};
    _ = try heap.collectGarbage(&roots);
    _ = try heap.collectGarbage(&roots);

    const owner_after = roots[0].toPtr(objects.Vector);
    const child_after = owner_after.get(0);
    try testing.expect(child_after.isCons());
    try testing.expect(child_after.toPtr(objects.Cons).car.isFixnum());
    if (!heap.isInNurseryAddr(@intFromPtr(owner_after))) {
        try testing.expect(heap.tenured_objs.items.len >= tenured_before + 1);
    }
}

test "tenured sweep reclaims unreachable promoted objects" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    const tenured_base = heap.tenured_objs.items.len;

    var s1 = try heap.allocBaseString(payload);
    var s2 = try heap.allocBaseString(payload);
    var roots12 = [_]Value{ s1, s2 };
    _ = try heap.collectGarbage(&roots12);
    s1 = roots12[0];
    s2 = roots12[1];
    const s2_addr = s2.toPtrAddr();
    try testing.expect(heap.tenured_objs.items.len >= tenured_base + 2);
    try testing.expect(tenuredContainsAddr(&heap, s1.toPtrAddr()));
    try testing.expect(tenuredContainsAddr(&heap, s2_addr));

    var roots1 = [_]Value{s1};
    _ = try heap.collectGarbage(&roots1);
    s1 = roots1[0];
    try testing.expect(tenuredContainsAddr(&heap, s1.toPtrAddr()));
    try testing.expect(!tenuredContainsAddr(&heap, s2_addr));

    var s3 = try heap.allocBaseString(payload);
    var roots13 = [_]Value{ s1, s3 };
    _ = try heap.collectGarbage(&roots13);
    s3 = roots13[1];
    try testing.expectEqual(s2_addr, s3.toPtrAddr());
}

test "incremental major sweep slices large tenured sets" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 16 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    var none: [0]Value = .{};
    _ = try heap.collectGarbage(none[0..]);
    const tenured_base = heap.tenured_objs.items.len;
    const n = MAJOR_SWEEP_BUDGET_OBJS + 257;
    const baseline_tenured = heap.tenured_objs.items.len;
    var roots = try testing.allocator.alloc(Value, n);
    defer testing.allocator.free(roots);

    for (roots) |*r| {
        r.* = try heap.allocBaseString(payload);
    }
    _ = try heap.collectGarbage(roots);
    const tenured_after_promote = heap.tenured_objs.items.len;
    try testing.expect(tenured_after_promote > baseline_tenured);
    try testing.expect(tenured_after_promote - baseline_tenured >= n);

    // The promotion collection may have started an incremental major cycle while
    // all items were still rooted. Drain that cycle before dropping roots so the
    // next cycle reflects the reduced root set.
    const settle_cycles = tenured_after_promote / MAJOR_SWEEP_BUDGET_OBJS + 16;
    var settle_guard: usize = 0;
    while (heap.isMajorCycleActive() and settle_guard < settle_cycles) : (settle_guard += 1) {
        _ = try heap.collectGarbage(roots);
    }
    try testing.expect(!heap.isMajorCycleActive());

    const keep = roots[0];
    const keep_addr = keep.toPtrAddr();
    roots[0] = keep;
    for (roots[1..]) |*r| r.* = Value.nil;

    _ = try heap.collectGarbage(roots);
    try testing.expect(heap.tenured_objs.items.len >= tenured_base + 1);

    const max_cycles = tenured_after_promote / MAJOR_SWEEP_BUDGET_OBJS + 16;
    var guard: usize = 0;
    while (heap.isMajorCycleActive() and guard < max_cycles) : (guard += 1) {
        _ = try heap.collectGarbage(roots);
    }

    try testing.expect(!heap.isMajorCycleActive());
    const tenured_after_sweep = heap.tenured_objs.items.len;
    try testing.expect(tenured_after_sweep < tenured_after_promote);
    try testing.expect(tenured_after_sweep >= tenured_base + 1);
    try testing.expect(roots[0].isString());
    try testing.expectEqualStrings(payload, roots[0].toPtr(objects.String).bytes());

    try testing.expect(tenuredContainsAddr(&heap, keep_addr));
}

test "major slice telemetry stays within configured budgets" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 16 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    const n = MAJOR_SWEEP_BUDGET_OBJS + 257;
    const roots = try testing.allocator.alloc(Value, n);
    defer testing.allocator.free(roots);

    for (roots) |*r| {
        r.* = try heap.allocBaseString(payload);
    }
    _ = try heap.collectGarbage(roots);

    const keep = roots[0];
    roots[0] = keep;
    for (roots[1..]) |*r| r.* = Value.nil;

    var guard: usize = 0;
    while (guard < 32 and (heap.isMajorCycleActive() or guard == 0)) : (guard += 1) {
        _ = try heap.collectGarbage(roots);
    }

    try testing.expect(heap.stats.gc_major_cycle_n > 0);
    try testing.expect(heap.stats.gc_major_sweep_tenured_steps > 0);
    try testing.expect(heap.stats.gc_major_swept_tenured > 0);
    try testing.expect(heap.stats.gc_major_max_tenured_slice <= MAJOR_SWEEP_BUDGET_OBJS);
    try testing.expect(heap.stats.gc_major_max_los_slice <= MAJOR_SWEEP_BUDGET_OBJS);
}

test "major barrier rescues newly linked old object before sweep" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 64 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 4 * 1024 * 1024,
            .los_size = 4 * 1024 * 1024,
            .los_threshold = 256,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    const n = MAJOR_SWEEP_BUDGET_OBJS * 4 + 300;

    const bulk = try testing.allocator.alloc(Value, n);
    defer testing.allocator.free(bulk);
    for (bulk) |*v| v.* = try heap.allocBaseString(payload);
    var victim = try heap.allocBaseString(payload);
    const keeper_val = try heap.allocVector(1, 128); // LOS object with refs.
    keeper_val.toPtr(objects.Vector).set(0, Value.nil);

    const roots_all = try testing.allocator.alloc(Value, n + 2);
    defer testing.allocator.free(roots_all);
    @memcpy(roots_all[0..n], bulk);
    roots_all[n] = victim;
    roots_all[n + 1] = keeper_val;

    _ = try heap.collectGarbage(roots_all);
    victim = roots_all[n];
    const victim_addr = victim.toPtrAddr();
    try testing.expect(heap.tenured_objs.items.len >= n + 1);

    var roots_keep = [_]Value{keeper_val};
    _ = try heap.collectGarbage(&roots_keep);
    try testing.expect(heap.isMajorCycleActive());

    const keeper_vec = roots_keep[0].toPtr(objects.Vector);
    keeper_vec.set(0, victim);
    heap.writeBarrier(roots_keep[0], victim);

    var guard: usize = 0;
    while (heap.isMajorCycleActive() and guard < 32) : (guard += 1) {
        _ = try heap.collectGarbage(&roots_keep);
    }
    try testing.expect(!heap.isMajorCycleActive());

    var found = false;
    for (heap.tenured_objs.items) |obj| {
        if (obj.addr == victim_addr) {
            found = true;
            break;
        }
    }
    try testing.expect(found);
    try testing.expectEqual(victim_addr, keeper_vec.get(0).toPtrAddr());
}

test "tenured marking follows nursery references" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .promote_threshold = 64,
        },
    });
    defer heap.deinit();

    const payload = "abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789";
    var none: [0]Value = .{};
    _ = try heap.collectGarbage(none[0..]);
    const tenured_base = heap.tenured_objs.items.len;

    var s = try heap.allocBaseString(payload);
    var roots_s = [_]Value{s};
    _ = try heap.collectGarbage(&roots_s);
    s = roots_s[0];
    const s_addr = s.toPtrAddr();
    const tenured_after_s = heap.tenured_objs.items.len;
    try testing.expect(tenured_after_s >= tenured_base + 1);
    try testing.expect(tenuredContainsAddr(&heap, s_addr));

    var c = try heap.allocCons(s, Value.nil);
    var roots_c = [_]Value{c};
    _ = try heap.collectGarbage(&roots_c);
    c = roots_c[0];

    try testing.expect(heap.tenured_objs.items.len >= tenured_after_s);
    try testing.expect(tenuredContainsAddr(&heap, s_addr));
    const cons = c.toPtr(objects.Cons);
    try testing.expect(cons.car.isString());
    try testing.expectEqual(s_addr, cons.car.toPtrAddr());
}

test "los object scan updates nursery references" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 256,
            .promote_threshold = 1024 * 1024,
        },
    });
    defer heap.deinit();

    const owner = try heap.allocVector(1, 64);
    const owner_ptr = owner.toPtr(objects.Vector);
    owner_ptr.set(0, try heap.allocCons(Value.makeFixnum(1), Value.nil));

    var roots = [_]Value{owner};
    _ = try heap.collectGarbage(&roots);
    const owner1 = roots[0].toPtr(objects.Vector);
    const child1 = owner1.get(0);
    try testing.expect(child1.isCons());
    const child1_raw = child1.raw;

    _ = try heap.collectGarbage(&roots);
    const owner2 = roots[0].toPtr(objects.Vector);
    const child2 = owner2.get(0);
    try testing.expect(child2.isCons());
    try testing.expect(child2.raw != child1_raw);
    const child2_addr = child2.toPtrAddr();
    if (!heap.isInNurseryAddr(child2_addr)) {
        try testing.expect(tenuredContainsAddr(&heap, child2_addr));
    }
}

test "minor gc records remembered-set telemetry" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 256,
            .promote_threshold = 1024 * 1024,
        },
    });
    defer heap.deinit();

    const owner = try heap.allocVector(1, 64);
    const child = try heap.allocCons(Value.makeFixnum(3), Value.nil);
    owner.toPtr(objects.Vector).set(0, child);
    heap.writeBarrier(owner, child);

    var roots = [_]Value{owner};
    _ = try heap.collectGarbage(&roots);

    try testing.expect(heap.stats.gc_remembered_marked_cards > 0);
    try testing.expect(heap.stats.gc_remembered_runs > 0);
    try testing.expect(heap.stats.gc_remembered_scanned > 0);
    try testing.expect(heap.stats.gc_remembered_runs <= heap.stats.gc_remembered_marked_cards);
}

test "remembered scan survives tenured list growth" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 1024 * 1024,
            .promote_threshold = 16,
            .promote_threshold_min = 16,
            .promote_threshold_max = 16,
        },
    });
    defer heap.deinit();

    var owner_a = try heap.allocVector(64, 64);
    var owner_b = try heap.allocVector(1, 1);
    var owner_a_vec = owner_a.toPtr(objects.Vector);
    for (0..64) |i| owner_a_vec.set(i, Value.nil);
    owner_b.toPtr(objects.Vector).set(0, Value.nil);

    var roots = [_]Value{ owner_a, owner_b };
    _ = try heap.collectGarbage(&roots);
    owner_a = roots[0];
    owner_b = roots[1];
    owner_a_vec = owner_a.toPtr(objects.Vector);
    const tenured_before = heap.tenured_objs.items.len;

    var i: usize = 0;
    while (i < 64) : (i += 1) {
        const child = try heap.allocCons(Value.makeFixnum(@intCast(i)), Value.nil);
        owner_a_vec.set(i, child);
        heap.writeBarrier(owner_a, child);
    }

    _ = try heap.collectGarbage(&roots);
    owner_a = roots[0];
    owner_b = roots[1];
    owner_a_vec = owner_a.toPtr(objects.Vector);

    try testing.expect(owner_b.isVector());
    try testing.expect(heap.tenured_objs.items.len > tenured_before);
    for (0..64) |idx| {
        try testing.expect(owner_a_vec.get(idx).isCons());
    }
}

test "los sweep reclaims unreachable large objects" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{
        .total_size = 8 * 1024 * 1024,
        .gc_layout = .generational,
        .generational = .{
            .nursery_each = 512 * 1024,
            .los_size = 512 * 1024,
            .los_threshold = 256,
            .promote_threshold = 1024 * 1024,
        },
    });
    defer heap.deinit();

    const los0 = heap.los_objs.items.len;
    var v1 = try heap.allocVector(1, 64);
    var v2 = try heap.allocVector(1, 64);
    const v2_addr = v2.toPtrAddr();

    var roots12 = [_]Value{ v1, v2 };
    _ = try heap.collectGarbage(&roots12);
    v1 = roots12[0];

    var roots1 = [_]Value{v1};
    _ = try heap.collectGarbage(&roots1);
    try testing.expectEqual(los0 + 1, heap.los_objs.items.len);

    const v3 = try heap.allocVector(1, 64);
    try testing.expectEqual(v2_addr, v3.toPtrAddr());
}
