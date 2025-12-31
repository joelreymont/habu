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
