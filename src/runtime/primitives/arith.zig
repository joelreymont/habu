//! Arithmetic primitives
//!
//! +, -, *, /, mod, logand, logior, logxor, lognot, ash

const std = @import("std");
const Value = @import("../value.zig").Value;

/// Add two fixnums
/// Returns nil on type error
pub fn add(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    return Value.makeFixnum(a.toFixnum() + b.toFixnum());
}

/// Subtract two fixnums
pub fn sub(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    return Value.makeFixnum(a.toFixnum() - b.toFixnum());
}

/// Multiply two fixnums
pub fn mul(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    return Value.makeFixnum(a.toFixnum() * b.toFixnum());
}

/// Divide two fixnums (integer division)
/// Returns nil on division by zero or type error
pub fn div(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    const divisor = b.toFixnum();
    if (divisor == 0) return Value.nil;
    return Value.makeFixnum(@divTrunc(a.toFixnum(), divisor));
}

/// Modulo operation
pub fn mod(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    const divisor = b.toFixnum();
    if (divisor == 0) return Value.nil;
    return Value.makeFixnum(@mod(a.toFixnum(), divisor));
}

/// Remainder operation (can be negative)
pub fn rem(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    const divisor = b.toFixnum();
    if (divisor == 0) return Value.nil;
    return Value.makeFixnum(@rem(a.toFixnum(), divisor));
}

/// Negate a fixnum
pub fn negate(a: Value) Value {
    if (!a.isFixnum()) return Value.nil;
    return Value.makeFixnum(-a.toFixnum());
}

/// Bitwise AND
pub fn logand(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua & ub));
}

/// Bitwise OR
pub fn logior(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua | ub));
}

/// Bitwise XOR
pub fn logxor(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua ^ ub));
}

/// Bitwise NOT
pub fn lognot(a: Value) Value {
    if (!a.isFixnum()) return Value.nil;
    const ua: u64 = @bitCast(a.toFixnum());
    return Value.makeFixnum(@bitCast(~ua));
}

/// Arithmetic shift (positive = left, negative = right)
pub fn ash(val: Value, count: Value) Value {
    if (!val.isFixnum() or !count.isFixnum()) return Value.nil;

    const v = val.toFixnum();
    const c = count.toFixnum();

    if (c >= 0) {
        // Left shift
        if (c >= 64) return Value.makeFixnum(0);
        const uc: u6 = @intCast(@as(u64, @intCast(c)));
        return Value.makeFixnum(v << uc);
    } else {
        // Right shift (arithmetic)
        const neg_c: u64 = @intCast(-c);
        if (neg_c >= 64) {
            return Value.makeFixnum(if (v < 0) -1 else 0);
        }
        const uc: u6 = @intCast(neg_c);
        return Value.makeFixnum(v >> uc);
    }
}

/// Check if value is zero
pub fn zerop(a: Value) bool {
    if (!a.isFixnum()) return false;
    return a.toFixnum() == 0;
}

/// Check if value is positive
pub fn plusp(a: Value) bool {
    if (!a.isFixnum()) return false;
    return a.toFixnum() > 0;
}

/// Check if value is negative
pub fn minusp(a: Value) bool {
    if (!a.isFixnum()) return false;
    return a.toFixnum() < 0;
}

/// Check if value is even
pub fn evenp(a: Value) bool {
    if (!a.isFixnum()) return false;
    return @rem(a.toFixnum(), 2) == 0;
}

/// Check if value is odd
pub fn oddp(a: Value) bool {
    if (!a.isFixnum()) return false;
    return @rem(a.toFixnum(), 2) != 0;
}

/// Absolute value
pub fn abs_val(a: Value) Value {
    if (!a.isFixnum()) return Value.nil;
    const n = a.toFixnum();
    return Value.makeFixnum(if (n < 0) -n else n);
}

/// Maximum of two values
pub fn max_val(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    return if (a.toFixnum() > b.toFixnum()) a else b;
}

/// Minimum of two values
pub fn min_val(a: Value, b: Value) Value {
    if (!a.isFixnum() or !b.isFixnum()) return Value.nil;
    return if (a.toFixnum() < b.toFixnum()) a else b;
}

// ============================================================================
// Comparison operations
// ============================================================================

/// Numeric equality
pub fn numEq(a: Value, b: Value) bool {
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() == b.toFixnum();
}

/// Less than
pub fn lt(a: Value, b: Value) bool {
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() < b.toFixnum();
}

/// Greater than
pub fn gt(a: Value, b: Value) bool {
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() > b.toFixnum();
}

/// Less than or equal
pub fn le(a: Value, b: Value) bool {
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() <= b.toFixnum();
}

/// Greater than or equal
pub fn ge(a: Value, b: Value) bool {
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() >= b.toFixnum();
}

// ============================================================================
// Random number generation
// ============================================================================

var prng: std.Random.DefaultPrng = std.Random.DefaultPrng.init(0);
var prng_seeded: bool = false;

/// Seed the random number generator
pub fn randomSeed(seed: Value) Value {
    if (!seed.isFixnum()) return Value.nil;
    const s: u64 = @bitCast(seed.toFixnum());
    prng = std.Random.DefaultPrng.init(s);
    prng_seeded = true;
    return seed;
}

/// Generate random integer in [0, n)
pub fn random(n: Value) Value {
    if (!n.isFixnum()) return Value.nil;
    const max = n.toFixnum();
    if (max <= 0) return Value.nil;

    // Auto-seed on first use
    if (!prng_seeded) {
        const ts = std.time.nanoTimestamp();
        const seed: u64 = @truncate(@as(u128, @bitCast(ts)));
        prng = std.Random.DefaultPrng.init(seed);
        prng_seeded = true;
    }

    const rand = prng.random();
    const result = rand.intRangeLessThan(i64, 0, max);
    return Value.makeFixnum(result);
}

// ============================================================================
// Tests
// ============================================================================

test "basic arithmetic" {
    const testing = std.testing;

    const a = Value.makeFixnum(10);
    const b = Value.makeFixnum(3);

    try testing.expectEqual(@as(i64, 13), add(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 7), sub(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 30), mul(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 3), div(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 1), mod(a, b).toFixnum());
}

test "division by zero" {
    const testing = std.testing;

    const a = Value.makeFixnum(10);
    const zero = Value.makeFixnum(0);

    try testing.expect(div(a, zero).isNil());
    try testing.expect(mod(a, zero).isNil());
}

test "negate" {
    const testing = std.testing;

    try testing.expectEqual(@as(i64, -5), negate(Value.makeFixnum(5)).toFixnum());
    try testing.expectEqual(@as(i64, 5), negate(Value.makeFixnum(-5)).toFixnum());
}

test "bitwise operations" {
    const testing = std.testing;

    const a = Value.makeFixnum(0b1100);
    const b = Value.makeFixnum(0b1010);

    try testing.expectEqual(@as(i64, 0b1000), logand(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 0b1110), logior(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 0b0110), logxor(a, b).toFixnum());
}

test "shift" {
    const testing = std.testing;

    const a = Value.makeFixnum(4);

    try testing.expectEqual(@as(i64, 16), ash(a, Value.makeFixnum(2)).toFixnum());
    try testing.expectEqual(@as(i64, 1), ash(a, Value.makeFixnum(-2)).toFixnum());
}

test "predicates" {
    const testing = std.testing;

    try testing.expect(zerop(Value.makeFixnum(0)));
    try testing.expect(!zerop(Value.makeFixnum(1)));

    try testing.expect(plusp(Value.makeFixnum(5)));
    try testing.expect(!plusp(Value.makeFixnum(-5)));

    try testing.expect(minusp(Value.makeFixnum(-5)));
    try testing.expect(!minusp(Value.makeFixnum(5)));

    try testing.expect(evenp(Value.makeFixnum(4)));
    try testing.expect(!evenp(Value.makeFixnum(5)));

    try testing.expect(oddp(Value.makeFixnum(5)));
    try testing.expect(!oddp(Value.makeFixnum(4)));
}

test "abs min max" {
    const testing = std.testing;

    try testing.expectEqual(@as(i64, 5), abs_val(Value.makeFixnum(-5)).toFixnum());
    try testing.expectEqual(@as(i64, 5), abs_val(Value.makeFixnum(5)).toFixnum());

    const a = Value.makeFixnum(3);
    const b = Value.makeFixnum(7);

    try testing.expectEqual(@as(i64, 7), max_val(a, b).toFixnum());
    try testing.expectEqual(@as(i64, 3), min_val(a, b).toFixnum());
}

test "comparisons" {
    const testing = std.testing;

    const a = Value.makeFixnum(5);
    const b = Value.makeFixnum(10);
    const c = Value.makeFixnum(5);

    try testing.expect(lt(a, b));
    try testing.expect(!lt(b, a));
    try testing.expect(!lt(a, c));

    try testing.expect(gt(b, a));
    try testing.expect(le(a, b));
    try testing.expect(le(a, c));
    try testing.expect(ge(b, a));
    try testing.expect(ge(a, c));

    try testing.expect(numEq(a, c));
    try testing.expect(!numEq(a, b));
}
