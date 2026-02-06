//! Rational number primitives

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const Rational = @import("../objects.zig").Rational;

pub const Error = error{ TypeMismatch, OutOfMemory };

/// Create a rational number (normalized)
pub fn makeRational(heap: *Heap, num: i64, den: i64) Error!Value {
    return heap.allocRational(num, den);
}

/// Get numerator of a rational
pub fn numerator(val: Value) Error!i64 {
    if (val.typeKind() != .rational) return error.TypeMismatch;
    const rat = val.toPtr(Rational);
    return rat.numerator;
}

/// Get denominator of a rational
pub fn denominator(val: Value) Error!i64 {
    if (val.typeKind() != .rational) return error.TypeMismatch;
    const rat = val.toPtr(Rational);
    return rat.denominator;
}

/// Check if value is rational
pub fn isRational(val: Value) bool {
    return val.typeKind() == .rational;
}

fn gcd(a: i64, b: i64) i64 {
    var x = if (a < 0) -a else a;
    var y = if (b < 0) -b else b;
    while (y != 0) {
        const temp = y;
        y = @mod(x, y);
        x = temp;
    }
    return x;
}

/// Convert float to rational
pub fn floatToRational(heap: *Heap, f: f64) Error!Value {
    // Handle special cases - return as float value
    if (std.math.isNan(f) or std.math.isInf(f)) {
        return Value.makeFloat(f);
    }

    const max_i64_f: f64 = @floatFromInt(std.math.maxInt(i64));
    const min_i64_f: f64 = @floatFromInt(std.math.minInt(i64));

    // Current Rational stores i64 numerator/denominator; values outside this
    // range must remain floats to avoid overflow/panics.
    if (f > max_i64_f or f < min_i64_f) {
        return Value.makeFloat(f);
    }

    // If already integer, return as rational n/1
    if (f == @floor(f)) {
        const n: i64 = @intFromFloat(@trunc(f));
        return makeRational(heap, n, 1);
    }

    // Use continued fraction approximation
    // For simplicity, use denominator limit of 1e9
    const max_den: i64 = 1_000_000_000;
    var n0: i64 = 0;
    var d0: i64 = 1;
    var n1: i64 = 1;
    var d1: i64 = 0;
    var x = f;

    while (true) {
        if (x > max_i64_f or x < min_i64_f) {
            return Value.makeFloat(f);
        }

        const a: i64 = @intFromFloat(@floor(x));

        const mul_n = @mulWithOverflow(a, n1);
        if (mul_n[1] != 0) return Value.makeFloat(f);
        const n2_of = @addWithOverflow(n0, mul_n[0]);
        if (n2_of[1] != 0) return Value.makeFloat(f);
        const n2 = n2_of[0];

        const mul_d = @mulWithOverflow(a, d1);
        if (mul_d[1] != 0) return Value.makeFloat(f);
        const d2_of = @addWithOverflow(d0, mul_d[0]);
        if (d2_of[1] != 0) return Value.makeFloat(f);
        const d2 = d2_of[0];

        if (d2 > max_den) break;

        n0 = n1;
        d0 = d1;
        n1 = n2;
        d1 = d2;

        const frac = x - @floor(x);
        if (frac < 1e-10) break;
        x = 1.0 / frac;
    }

    if (d1 == 0) return Value.makeFloat(f);
    return makeRational(heap, n1, d1);
}

test "rational creation" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    // 6/8 normalizes to 3/4
    const r = try makeRational(&heap, 6, 8);
    try testing.expect(isRational(r));
    try testing.expectEqual(@as(i64, 3), try numerator(r));
    try testing.expectEqual(@as(i64, 4), try denominator(r));
}

test "rational negative denominator" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    // 6/-8 normalizes to -3/4
    const r = try makeRational(&heap, 6, -8);
    try testing.expectEqual(@as(i64, -3), try numerator(r));
    try testing.expectEqual(@as(i64, 4), try denominator(r));
}

test "floatToRational keeps huge finite as float" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const v = try floatToRational(&heap, 1.0e40);
    try testing.expect(v.typeKind() == .float);
    try testing.expect(std.math.isFinite(v.toFloat()));
}
