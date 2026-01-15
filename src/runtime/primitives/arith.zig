//! Arithmetic primitives
//!
//! +, -, *, /, mod, logand, logior, logxor, lognot, ash

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");

pub const Error = error{ TypeMismatch, DivisionByZero, OutOfMemory };

/// Add two numbers (fixnum, bignum, float, rational, or complex)
pub fn add(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion: if either operand is complex, use complex arithmetic
    if (a.typeKind() == .complex or b.typeKind() == .complex) return addComplex(heap, a, b);

    // Float contagion: if either operand is float, use float arithmetic
    if (a.isFloat() or b.isFloat()) return addFloat(a, b);

    // Rational arithmetic
    if (a.typeKind() == .rational or b.typeKind() == .rational) return addRational(heap, a, b);

    // Bignum arithmetic
    if (a.isBignum() or b.isBignum()) return addBignum(heap, a, b);

    // Fixnum arithmetic with overflow check
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const result = @addWithOverflow(a.toFixnum(), b.toFixnum());

    // Check for i64 overflow OR fixnum range overflow (62-bit signed)
    const max_fixnum: i64 = (1 << 62) - 1;
    const min_fixnum: i64 = -(1 << 62);
    if (result[1] != 0 or result[0] > max_fixnum or result[0] < min_fixnum) {
        // Overflow - promote to bignum
        return addBignum(heap, a, b);
    }
    return Value.makeFixnum(result[0]);
}

/// Subtract two numbers (fixnum, float, rational, or complex)
pub fn sub(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion
    if (a.typeKind() == .complex or b.typeKind() == .complex) return subComplex(heap, a, b);

    // Float contagion
    if (a.isFloat() or b.isFloat()) return subFloat(a, b);

    // Rational arithmetic
    if (a.typeKind() == .rational or b.typeKind() == .rational) return subRational(heap, a, b);

    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const result = @subWithOverflow(a.toFixnum(), b.toFixnum());
    if (result[1] != 0) return error.TypeMismatch;
    return Value.makeFixnum(result[0]);
}

/// Multiply two numbers (fixnum, bignum, float, rational, or complex)
pub fn mul(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion
    if (a.typeKind() == .complex or b.typeKind() == .complex) return mulComplex(heap, a, b);

    // Float contagion
    if (a.isFloat() or b.isFloat()) return mulFloat(a, b);

    // Rational arithmetic
    if (a.typeKind() == .rational or b.typeKind() == .rational) return mulRational(heap, a, b);

    // Bignum arithmetic
    if (a.isBignum() or b.isBignum()) return mulBignum(heap, a, b);

    // Fixnum arithmetic with overflow check
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const result = @mulWithOverflow(a.toFixnum(), b.toFixnum());

    // Check for i64 overflow OR fixnum range overflow (62-bit signed)
    const max_fixnum: i64 = (1 << 62) - 1;
    const min_fixnum: i64 = -(1 << 62);
    if (result[1] != 0 or result[0] > max_fixnum or result[0] < min_fixnum) {
        // Overflow - promote to bignum
        return mulBignum(heap, a, b);
    }
    return Value.makeFixnum(result[0]);
}

/// Divide two numbers (returns rational for exact division, complex/float for complex/float args)
pub fn div(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion
    if (a.typeKind() == .complex or b.typeKind() == .complex) return divComplex(heap, a, b);

    // Float contagion
    if (a.isFloat() or b.isFloat()) return divFloat(a, b);

    // Rational arithmetic - division of integers returns rational
    if (a.typeKind() == .rational or b.typeKind() == .rational or a.isFixnum() and b.isFixnum()) return divRational(heap, a, b);

    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const dividend = a.toFixnum();
    const divisor = b.toFixnum();
    if (divisor == 0) return error.DivisionByZero;
    // minInt / -1 overflows
    if (dividend == std.math.minInt(i64) and divisor == -1) return error.TypeMismatch;
    return Value.makeFixnum(@divTrunc(dividend, divisor));
}

/// Modulo operation
pub fn mod(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const divisor = b.toFixnum();
    if (divisor == 0) return error.DivisionByZero;
    return Value.makeFixnum(@mod(a.toFixnum(), divisor));
}

/// Remainder operation (can be negative)
pub fn rem(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const divisor = b.toFixnum();
    if (divisor == 0) return error.DivisionByZero;
    return Value.makeFixnum(@rem(a.toFixnum(), divisor));
}

/// Negate a fixnum
pub fn negate(a: Value) Error!Value {
    if (!a.isFixnum()) return error.TypeMismatch;
    const n = a.toFixnum();
    // -minInt overflows
    if (n == std.math.minInt(i64)) return error.TypeMismatch;
    return Value.makeFixnum(-n);
}

/// Bitwise AND
pub fn logand(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua & ub));
}

/// Bitwise OR
pub fn logior(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua | ub));
}

/// Bitwise XOR
pub fn logxor(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua ^ ub));
}

/// Bitwise NOT
pub fn lognot(a: Value) Error!Value {
    if (!a.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    return Value.makeFixnum(@bitCast(~ua));
}

/// Bitwise NAND
pub fn lognand(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(~(ua & ub)));
}

/// Bitwise NOR
pub fn lognor(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(~(ua | ub)));
}

/// Bitwise AND with NOT of first arg
pub fn logandc1(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast((~ua) & ub));
}

/// Bitwise AND with NOT of second arg
pub fn logandc2(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua & (~ub)));
}

/// Bitwise OR with NOT of first arg
pub fn logorc1(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast((~ua) | ub));
}

/// Bitwise OR with NOT of second arg
pub fn logorc2(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(ua | (~ub)));
}

/// Bitwise equivalence (NOT XOR)
pub fn logeqv(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return Value.makeFixnum(@bitCast(~(ua ^ ub)));
}

/// Test if any bits set in both args
pub fn logtest(a: Value, b: Value) bool {
    if (!a.isFixnum() or !b.isFixnum()) return false;
    const ua: u64 = @bitCast(a.toFixnum());
    const ub: u64 = @bitCast(b.toFixnum());
    return (ua & ub) != 0;
}

/// Test if bit at index is set
pub fn logbitp(index: Value, n: Value) bool {
    if (!index.isFixnum() or !n.isFixnum()) return false;
    const idx = index.toFixnum();
    if (idx < 0 or idx >= 64) return false;
    const un: u64 = @bitCast(n.toFixnum());
    const bit: u6 = @intCast(idx);
    return ((un >> bit) & 1) == 1;
}

/// Count number of 1 bits
pub fn logcount(n: Value) Error!Value {
    if (!n.isFixnum()) return error.TypeMismatch;
    const val = n.toFixnum();
    const un: u64 = @bitCast(if (val < 0) ~val else val);
    return Value.makeFixnum(@popCount(un));
}

/// Number of bits needed to represent integer
pub fn integer_length(n: Value) Error!Value {
    if (!n.isFixnum()) return error.TypeMismatch;
    const val = n.toFixnum();
    if (val == 0) return Value.makeFixnum(0);
    const un: u64 = @bitCast(if (val < 0) ~val else val);
    const bits = 64 - @clz(un);
    return Value.makeFixnum(@intCast(bits));
}

/// Arithmetic shift (positive = left, negative = right)
pub fn ash(val: Value, count: Value) Error!Value {
    if (!val.isFixnum() or !count.isFixnum()) return error.TypeMismatch;

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
pub fn abs_val(a: Value) Error!Value {
    if (!a.isFixnum()) return error.TypeMismatch;
    const n = a.toFixnum();
    return Value.makeFixnum(if (n < 0) -n else n);
}

/// Maximum of two values
pub fn max_val(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    return if (a.toFixnum() > b.toFixnum()) a else b;
}

/// Minimum of two values
pub fn min_val(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    return if (a.toFixnum() < b.toFixnum()) a else b;
}

// ============================================================================
// Comparison operations
// ============================================================================

/// Numeric equality
pub fn numEq(a: Value, b: Value) bool {
    // Handle bignum comparisons
    if (a.isBignum() or b.isBignum()) {
        if (!a.isFixnum() and !a.isBignum()) return false;
        if (!b.isFixnum() and !b.isBignum()) return false;
        return compareBignum(a, b) == 0;
    }

    // Fixnum comparison
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() == b.toFixnum();
}

/// Less than
pub fn lt(a: Value, b: Value) bool {
    // Float comparison
    if (a.isFloat() or b.isFloat()) {
        const af = toNumber(a) catch return false;
        const bf = toNumber(b) catch return false;
        return af < bf;
    }

    // Handle bignum comparisons
    if (a.isBignum() or b.isBignum()) {
        if (!a.isFixnum() and !a.isBignum()) return false;
        if (!b.isFixnum() and !b.isBignum()) return false;
        return compareBignum(a, b) < 0;
    }

    // Fixnum comparison
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() < b.toFixnum();
}

/// Greater than
pub fn gt(a: Value, b: Value) bool {
    // Float comparison
    if (a.isFloat() or b.isFloat()) {
        const af = toNumber(a) catch return false;
        const bf = toNumber(b) catch return false;
        return af > bf;
    }

    // Float comparison
    if (a.isFloat() or b.isFloat()) {
        const af = toNumber(a) catch return false;
        const bf = toNumber(b) catch return false;
        return af > bf;
    }

    // Handle bignum comparisons
    if (a.isBignum() or b.isBignum()) {
        if (!a.isFixnum() and !a.isBignum()) return false;
        if (!b.isFixnum() and !b.isBignum()) return false;
        return compareBignum(a, b) > 0;
    }

    // Fixnum comparison
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() > b.toFixnum();
}

/// Less than or equal
pub fn le(a: Value, b: Value) bool {
    // Float comparison
    if (a.isFloat() or b.isFloat()) {
        const af = toNumber(a) catch return false;
        const bf = toNumber(b) catch return false;
        return af <= bf;
    }

    // Handle bignum comparisons
    if (a.isBignum() or b.isBignum()) {
        if (!a.isFixnum() and !a.isBignum()) return false;
        if (!b.isFixnum() and !b.isBignum()) return false;
        return compareBignum(a, b) <= 0;
    }

    // Fixnum comparison
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() <= b.toFixnum();
}

/// Greater than or equal
pub fn ge(a: Value, b: Value) bool {
    // Float comparison
    if (a.isFloat() or b.isFloat()) {
        const af = toNumber(a) catch return false;
        const bf = toNumber(b) catch return false;
        return af >= bf;
    }

    // Handle bignum comparisons
    if (a.isBignum() or b.isBignum()) {
        if (!a.isFixnum() and !a.isBignum()) return false;
        if (!b.isFixnum() and !b.isBignum()) return false;
        return compareBignum(a, b) >= 0;
    }

    // Fixnum comparison
    if (!a.isFixnum() or !b.isFixnum()) return false;
    return a.toFixnum() >= b.toFixnum();
}

// ============================================================================
// Random number generation
// ============================================================================

var prng: std.Random.DefaultPrng = std.Random.DefaultPrng.init(0);
var prng_seeded: bool = false;

pub const RangeError = error{InvalidRange};

/// Seed the random number generator
pub fn randomSeed(seed: Value) Error!Value {
    if (!seed.isFixnum()) return error.TypeMismatch;
    const s: u64 = @bitCast(seed.toFixnum());
    prng = std.Random.DefaultPrng.init(s);
    prng_seeded = true;
    return seed;
}

/// Generate random integer in [0, n)
pub fn random(n: Value) (Error || RangeError)!Value {
    if (!n.isFixnum()) return error.TypeMismatch;
    const max = n.toFixnum();
    if (max <= 0) return error.InvalidRange;

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

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const a = Value.makeFixnum(10);
    const b = Value.makeFixnum(3);

    try testing.expectEqual(@as(i64, 13), (try add(&heap, a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 7), (try sub(&heap, a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 30), (try mul(&heap, a, b)).toFixnum());
    // div now returns rational, so this test needs updating
    const result = try div(&heap, a, b);
    try testing.expect(result.typeKind() == .rational);
    try testing.expectEqual(@as(i64, 1), (try mod(a, b)).toFixnum());
}

test "division by zero" {
    const testing = std.testing;
    var heap = try Heap.init(std.testing.allocator, .{});
    defer heap.deinit();

    const a = Value.makeFixnum(10);
    const zero = Value.makeFixnum(0);

    try testing.expectError(error.DivisionByZero, div(&heap, a, zero));
    try testing.expectError(error.DivisionByZero, mod(a, zero));
}

test "negate" {
    const testing = std.testing;

    try testing.expectEqual(@as(i64, -5), (try negate(Value.makeFixnum(5))).toFixnum());
    try testing.expectEqual(@as(i64, 5), (try negate(Value.makeFixnum(-5))).toFixnum());
}

test "bitwise operations" {
    const testing = std.testing;

    const a = Value.makeFixnum(0b1100);
    const b = Value.makeFixnum(0b1010);

    try testing.expectEqual(@as(i64, 0b1000), (try logand(a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 0b1110), (try logior(a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 0b0110), (try logxor(a, b)).toFixnum());
}

test "shift" {
    const testing = std.testing;

    const a = Value.makeFixnum(4);

    try testing.expectEqual(@as(i64, 16), (try ash(a, Value.makeFixnum(2))).toFixnum());
    try testing.expectEqual(@as(i64, 1), (try ash(a, Value.makeFixnum(-2))).toFixnum());
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

    try testing.expectEqual(@as(i64, 5), (try abs_val(Value.makeFixnum(-5))).toFixnum());
    try testing.expectEqual(@as(i64, 5), (try abs_val(Value.makeFixnum(5))).toFixnum());

    const a = Value.makeFixnum(3);
    const b = Value.makeFixnum(7);

    try testing.expectEqual(@as(i64, 7), (try max_val(a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 3), (try min_val(a, b)).toFixnum());
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

// ============================================================================
// Float arithmetic (Common Lisp numeric tower with contagion)
// ============================================================================

/// Helper: convert Value to f64
fn toNumber(v: Value) Error!f64 {
    if (v.isFloat()) return v.toFloat();
    if (v.isFixnum()) return @floatFromInt(v.toFixnum());
    return error.TypeMismatch;
}

/// Float addition (supports fixnum→float contagion)
pub fn addFloat(a: Value, b: Value) Error!Value {
    const af = try toNumber(a);
    const bf = try toNumber(b);
    return Value.makeFloat(af + bf);
}

/// Float subtraction
pub fn subFloat(a: Value, b: Value) Error!Value {
    const af = try toNumber(a);
    const bf = try toNumber(b);
    return Value.makeFloat(af - bf);
}

/// Float multiplication
pub fn mulFloat(a: Value, b: Value) Error!Value {
    const af = try toNumber(a);
    const bf = try toNumber(b);
    return Value.makeFloat(af * bf);
}

/// Float division
pub fn divFloat(a: Value, b: Value) Error!Value {
    const af = try toNumber(a);
    const bf = try toNumber(b);
    if (bf == 0.0) return error.DivisionByZero;
    return Value.makeFloat(af / bf);
}

/// Square root
pub fn sqrt_val(a: Value) Error!Value {
    const af = try toNumber(a);
    if (af < 0) {
        const complex_mod = @import("complex.zig");
        const heap = @import("../heap.zig").threadLocalHeap() orelse return error.OutOfMemory;
        return complex_mod.makeComplex(heap, 0.0, @sqrt(-af));
    }
    return Value.makeFloat(@sqrt(af));
}

/// Sine
pub fn sin_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(@sin(af));
}

/// Cosine
pub fn cos_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(@cos(af));
}

/// Tangent
pub fn tan_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(@tan(af));
}

/// Arcsine
pub fn asin_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.asin(af));
}

/// Arccosine
pub fn acos_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.acos(af));
}

/// Arctangent (1 arg)
pub fn atan_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.atan(af));
}

/// Arctangent (2 arg)
pub fn atan2_val(y: Value, x: Value) Error!Value {
    const yf = try toNumber(y);
    const xf = try toNumber(x);
    return Value.makeFloat(std.math.atan2(yf, xf));
}

/// Hyperbolic sine
pub fn sinh_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.sinh(af));
}

/// Hyperbolic cosine
pub fn cosh_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.cosh(af));
}

/// Hyperbolic tangent
pub fn tanh_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.tanh(af));
}

/// Inverse hyperbolic sine
pub fn asinh_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.asinh(af));
}

/// Inverse hyperbolic cosine
pub fn acosh_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.acosh(af));
}

/// Inverse hyperbolic tangent
pub fn atanh_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(std.math.atanh(af));
}

/// Natural logarithm
pub fn log_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(@log(af));
}

/// Exponential (e^x)
pub fn exp_val(a: Value) Error!Value {
    const af = try toNumber(a);
    return Value.makeFloat(@exp(af));
}

/// Power (x^y)
pub fn pow_val(x: Value, y: Value) Error!Value {
    const xf = try toNumber(x);
    const yf = try toNumber(y);
    return Value.makeFloat(std.math.pow(f64, xf, yf));
}

/// Floor: largest integer not greater than x/y
pub fn floor_val(_: *Heap, x: Value, y: Value) Error!Value {
    if (x.isFloat() or y.isFloat()) {
        const xf = try toNumber(x);
        const yf = try toNumber(y);
        const q = @floor(xf / yf);
        return Value.makeFloat(q);
    }

    if (!x.isFixnum() or !y.isFixnum()) return error.TypeMismatch;
    const dividend = x.toFixnum();
    const divisor = y.toFixnum();
    if (divisor == 0) return error.DivisionByZero;

    const q = @divFloor(dividend, divisor);
    return Value.makeFixnum(q);
}

/// Ceiling: smallest integer not less than x/y
pub fn ceil_val(_: *Heap, x: Value, y: Value) Error!Value {
    if (x.isFloat() or y.isFloat()) {
        const xf = try toNumber(x);
        const yf = try toNumber(y);
        const q = @ceil(xf / yf);
        return Value.makeFloat(q);
    }

    if (!x.isFixnum() or !y.isFixnum()) return error.TypeMismatch;
    const dividend = x.toFixnum();
    const divisor = y.toFixnum();
    if (divisor == 0) return error.DivisionByZero;

    // divCeil: smallest integer not less than dividend/divisor
    const q = std.math.divCeil(i64, dividend, divisor) catch return error.TypeMismatch;
    return Value.makeFixnum(q);
}

/// Truncate: integer part of x/y toward zero
pub fn trunc_val(_: *Heap, x: Value, y: Value) Error!Value {
    if (x.isFloat() or y.isFloat()) {
        const xf = try toNumber(x);
        const yf = try toNumber(y);
        const q = @trunc(xf / yf);
        return Value.makeFloat(q);
    }

    if (!x.isFixnum() or !y.isFixnum()) return error.TypeMismatch;
    const dividend = x.toFixnum();
    const divisor = y.toFixnum();
    if (divisor == 0) return error.DivisionByZero;

    const q = @divTrunc(dividend, divisor);
    return Value.makeFixnum(q);
}

/// Round: nearest integer to x/y, ties to even
pub fn round_val(_: *Heap, x: Value, y: Value) Error!Value {
    if (x.isFloat() or y.isFloat()) {
        const xf = try toNumber(x);
        const yf = try toNumber(y);
        const q = @round(xf / yf);
        return Value.makeFloat(q);
    }

    if (!x.isFixnum() or !y.isFixnum()) return error.TypeMismatch;
    const dividend = x.toFixnum();
    const divisor = y.toFixnum();
    if (divisor == 0) return error.DivisionByZero;

    // Round to nearest integer, ties to even
    const q = @divFloor(dividend, divisor);
    const r = @mod(dividend, divisor);
    const half = @divFloor(@abs(divisor), 2);

    if (@abs(r) > half) {
        // Strictly greater than halfway: round away from zero
        return Value.makeFixnum(if (dividend * divisor >= 0) q + 1 else q - 1);
    } else if (@abs(r) < half) {
        // Strictly less than halfway: round toward zero (truncate)
        return Value.makeFixnum(q);
    } else {
        // Exactly halfway: round to even
        if (@mod(q, 2) == 0) {
            return Value.makeFixnum(q);
        } else {
            return Value.makeFixnum(if (dividend * divisor >= 0) q + 1 else q - 1);
        }
    }
}

/// GCD: greatest common divisor
pub fn gcd_val(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;

    var x = @abs(a.toFixnum());
    var y = @abs(b.toFixnum());

    // Euclidean algorithm
    while (y != 0) {
        const tmp = @mod(x, y);
        x = y;
        y = tmp;
    }

    return Value.makeFixnum(x);
}

/// LCM: least common multiple
pub fn lcm_val(a: Value, b: Value) Error!Value {
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;

    const x = @abs(a.toFixnum());
    const y = @abs(b.toFixnum());

    if (x == 0 or y == 0) return Value.makeFixnum(0);

    // LCM(a,b) = |a*b| / GCD(a,b)
    const g = try gcd_val(a, b);
    const gcd_result = g.toFixnum();

    // Compute (x / gcd) * y to avoid overflow
    const result = @divExact(x, gcd_result) * y;
    return Value.makeFixnum(result);
}

// ============================================================================
// Rational arithmetic
// ============================================================================

/// Helper: convert Value to rational (numerator, denominator)
fn toRational(v: Value) Error!struct { num: i64, den: i64 } {
    switch (v.typeKind()) {
        .rational => {
            const rat = v.toPtr(objects.Rational);
            return .{ .num = rat.numerator, .den = rat.denominator };
        },
        .fixnum => return .{ .num = v.toFixnum(), .den = 1 },
        else => return error.TypeMismatch,
    }
}

/// Rational addition: a/b + c/d = (ad + bc) / (bd)
fn addRational(heap: *Heap, a: Value, b: Value) Error!Value {
    const ra = try toRational(a);
    const rb = try toRational(b);

    const num = ra.num * rb.den + rb.num * ra.den;
    const den = ra.den * rb.den;

    const rat = objects.Rational.make(num, den);
    const ptr = try heap.alloc(objects.Rational);
    ptr.* = rat;
    return Value.makeRational(ptr);
}

/// Rational subtraction: a/b - c/d = (ad - bc) / (bd)
fn subRational(heap: *Heap, a: Value, b: Value) Error!Value {
    const ra = try toRational(a);
    const rb = try toRational(b);

    const num = ra.num * rb.den - rb.num * ra.den;
    const den = ra.den * rb.den;

    const rat = objects.Rational.make(num, den);
    const ptr = try heap.alloc(objects.Rational);
    ptr.* = rat;
    return Value.makeRational(ptr);
}

/// Rational multiplication: a/b * c/d = (ac) / (bd)
fn mulRational(heap: *Heap, a: Value, b: Value) Error!Value {
    const ra = try toRational(a);
    const rb = try toRational(b);

    const num = ra.num * rb.num;
    const den = ra.den * rb.den;

    const rat = objects.Rational.make(num, den);
    const ptr = try heap.alloc(objects.Rational);
    ptr.* = rat;
    return Value.makeRational(ptr);
}

/// Rational division: (a/b) / (c/d) = (ad) / (bc)
fn divRational(heap: *Heap, a: Value, b: Value) Error!Value {
    const ra = try toRational(a);
    const rb = try toRational(b);

    if (rb.num == 0) return error.DivisionByZero;

    const num = ra.num * rb.den;
    const den = ra.den * rb.num;

    const rat = objects.Rational.make(num, den);
    const ptr = try heap.alloc(objects.Rational);
    ptr.* = rat;
    return Value.makeRational(ptr);
}

// ============================================================================
// Complex arithmetic
// ============================================================================

/// Helper: convert Value to complex (real, imag)
fn toComplex(v: Value) Error!struct { real: f64, imag: f64 } {
    switch (v.typeKind()) {
        .complex => {
            const cplx = v.toPtr(objects.Complex);
            return .{ .real = cplx.real, .imag = cplx.imag };
        },
        .float => return .{ .real = v.toFloat(), .imag = 0.0 },
        .fixnum => return .{ .real = @floatFromInt(v.toFixnum()), .imag = 0.0 },
        .rational => {
            const rat = v.toPtr(objects.Rational);
            const real: f64 = @as(f64, @floatFromInt(rat.numerator)) / @as(f64, @floatFromInt(rat.denominator));
            return .{ .real = real, .imag = 0.0 };
        },
        else => return error.TypeMismatch,
    }
}

/// Complex addition: supports mixed types
fn addComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = try toComplex(a);
    const cb = try toComplex(b);
    return heap.allocComplex(ca.real + cb.real, ca.imag + cb.imag);
}

/// Complex subtraction: supports mixed types
fn subComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = try toComplex(a);
    const cb = try toComplex(b);
    return heap.allocComplex(ca.real - cb.real, ca.imag - cb.imag);
}

/// Complex multiplication: supports mixed types
fn mulComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = try toComplex(a);
    const cb = try toComplex(b);
    // (a + bi)(c + di) = (ac - bd) + (ad + bc)i
    const real = ca.real * cb.real - ca.imag * cb.imag;
    const imag = ca.real * cb.imag + ca.imag * cb.real;
    return heap.allocComplex(real, imag);
}

/// Complex division: supports mixed types
fn divComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = try toComplex(a);
    const cb = try toComplex(b);
    // (a + bi)/(c + di) = [(ac + bd) + (bc - ad)i] / (c² + d²)
    const denom = cb.real * cb.real + cb.imag * cb.imag;
    if (denom == 0.0) return error.DivisionByZero;
    const real = (ca.real * cb.real + ca.imag * cb.imag) / denom;
    const imag = (ca.imag * cb.real - ca.real * cb.imag) / denom;
    return heap.allocComplex(real, imag);
}

// ============================================================================
// Bignum arithmetic
// ============================================================================

/// Add two bignums (or mixed bignum/fixnum)
fn addBignum(heap: *Heap, a: Value, b: Value) Error!Value {
    // Convert fixnums to temporary bignums
    var a_tmp: objects.Bignum = undefined;
    var b_tmp: objects.Bignum = undefined;

    const a_bn = if (a.isBignum()) a.toPtr(objects.Bignum) else blk: {
        a_tmp = objects.Bignum.make(a.toFixnum());
        break :blk &a_tmp;
    };
    const b_bn = if (b.isBignum()) b.toPtr(objects.Bignum) else blk: {
        b_tmp = objects.Bignum.make(b.toFixnum());
        break :blk &b_tmp;
    };

    const a_neg = a_bn.isNegative();
    const b_neg = b_bn.isNegative();

    // Get absolute values
    const a_size: usize = @intCast(@abs(a_bn.size));
    const b_size: usize = @intCast(@abs(b_bn.size));

    // If signs are the same, add magnitudes
    if (a_neg == b_neg) {
        var result_limbs: [8]u64 = [_]u64{0} ** 8;
        var carry: u64 = 0;
        const max_size = @max(a_size, b_size);

        for (0..max_size) |i| {
            const a_limb = if (i < a_size) a_bn.limbs[i] else 0;
            const b_limb = if (i < b_size) b_bn.limbs[i] else 0;

            const sum = @addWithOverflow(a_limb, b_limb);
            const sum_with_carry = @addWithOverflow(sum[0], carry);

            result_limbs[i] = sum_with_carry[0];
            carry = sum[1] + sum_with_carry[1];
        }

        // Handle final carry
        if (carry > 0 and max_size < 8) {
            result_limbs[max_size] = carry;
        }

        return heap.allocBignumFromLimbs(&result_limbs, a_neg);
    } else {
        // Signs differ - subtract magnitudes
        // For now, return type mismatch (implement subtraction separately)
        return error.TypeMismatch;
    }
}

/// Multiply two bignums (or mixed bignum/fixnum)
fn mulBignum(heap: *Heap, a: Value, b: Value) Error!Value {
    // Convert fixnums to temporary bignums
    var a_tmp: objects.Bignum = undefined;
    var b_tmp: objects.Bignum = undefined;

    const a_bn = if (a.isBignum()) a.toPtr(objects.Bignum) else blk: {
        a_tmp = objects.Bignum.make(a.toFixnum());
        break :blk &a_tmp;
    };
    const b_bn = if (b.isBignum()) b.toPtr(objects.Bignum) else blk: {
        b_tmp = objects.Bignum.make(b.toFixnum());
        break :blk &b_tmp;
    };

    const a_neg = a_bn.isNegative();
    const b_neg = b_bn.isNegative();

    // Get absolute values
    const a_size: usize = @intCast(@abs(a_bn.size));
    const b_size: usize = @intCast(@abs(b_bn.size));

    // Result is negative if signs differ
    const result_neg = a_neg != b_neg;

    // Schoolbook multiplication: result size is at most size1 + size2
    var result_limbs: [8]u64 = [_]u64{0} ** 8;

    for (0..a_size) |i| {
        var carry: u64 = 0;
        for (0..b_size) |j| {
            if (i + j >= 8) break; // Result overflow (too large for our 8-limb representation)

            // Multiply limbs and add to existing result
            const prod = @as(u128, a_bn.limbs[i]) * @as(u128, b_bn.limbs[j]);
            const low: u64 = @truncate(prod);
            const high: u64 = @truncate(prod >> 64);

            // Add low part + carry to current position
            const sum1 = @addWithOverflow(result_limbs[i + j], low);
            const sum2 = @addWithOverflow(sum1[0], carry);
            result_limbs[i + j] = sum2[0];

            // New carry is high part + overflow bits
            carry = high + sum1[1] + sum2[1];
        }

        // Propagate final carry
        if (carry > 0) {
            var k = b_size;
            while (carry > 0 and i + k < 8) {
                const sum = @addWithOverflow(result_limbs[i + k], carry);
                result_limbs[i + k] = sum[0];
                carry = sum[1];
                k += 1;
            }
        }
    }

    return heap.allocBignumFromLimbs(&result_limbs, result_neg);
}

/// Compare two bignums: returns -1 if a < b, 0 if a == b, 1 if a > b
fn compareBignum(a: Value, b: Value) i8 {
    // Convert fixnums to temporary bignums
    var a_tmp: objects.Bignum = undefined;
    var b_tmp: objects.Bignum = undefined;

    const a_bn = if (a.isBignum()) a.toPtr(objects.Bignum) else blk: {
        a_tmp = objects.Bignum.make(a.toFixnum());
        break :blk &a_tmp;
    };
    const b_bn = if (b.isBignum()) b.toPtr(objects.Bignum) else blk: {
        b_tmp = objects.Bignum.make(b.toFixnum());
        break :blk &b_tmp;
    };

    const a_neg = a_bn.isNegative();
    const b_neg = b_bn.isNegative();

    // Compare signs first
    if (a_neg and !b_neg) return -1; // negative < positive
    if (!a_neg and b_neg) return 1; // positive > negative

    // Same sign - compare magnitudes
    const a_size: usize = @intCast(@abs(a_bn.size));
    const b_size: usize = @intCast(@abs(b_bn.size));

    // Compare sizes
    if (a_size != b_size) {
        const size_cmp: i8 = if (a_size < b_size) -1 else 1;
        // If negative, invert the comparison (larger magnitude = smaller number)
        return if (a_neg) -size_cmp else size_cmp;
    }

    // Same size - compare limbs from most significant to least
    var i: usize = a_size;
    while (i > 0) {
        i -= 1;
        if (a_bn.limbs[i] != b_bn.limbs[i]) {
            const limb_cmp: i8 = if (a_bn.limbs[i] < b_bn.limbs[i]) -1 else 1;
            // If negative, invert the comparison
            return if (a_neg) -limb_cmp else limb_cmp;
        }
    }

    // All limbs equal
    return 0;
}
