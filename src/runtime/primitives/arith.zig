//! Arithmetic primitives
//!
//! +, -, *, /, mod, logand, logior, logxor, lognot, ash

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");
const list_prim = @import("list.zig");

pub const Error = error{ TypeMismatch, DivisionByZero, OutOfMemory, InvalidArgument };
const max_fixnum_i64: i64 = (1 << 62) - 1;
const min_fixnum_i64: i64 = -(1 << 62);
const max_fixnum_u64: u64 = (@as(u64, 1) << 62) - 1;
const max_neg_fixnum_mag_u64: u64 = (@as(u64, 1) << 62);
const max_bignum_limbs: usize = 8;

inline fn isFloatKind(v: Value) bool {
    return v.typeKind() == .float;
}

/// Add two numbers (fixnum, bignum, float, rational, or complex)
pub fn add(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion: if either operand is complex, use complex arithmetic
    if (a.typeKind() == .complex or b.typeKind() == .complex) return addComplex(heap, a, b);

    // Float contagion: if either operand is float, use float arithmetic
    if (isFloatKind(a) or isFloatKind(b)) return addFloat(a, b);

    // Rational arithmetic
    if (a.typeKind() == .rational or b.typeKind() == .rational) return addRational(heap, a, b);

    // Bignum arithmetic
    if (a.isBignum() or b.isBignum()) return addBignum(heap, a, b);

    // Fixnum arithmetic with overflow check
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const result = @addWithOverflow(a.toFixnum(), b.toFixnum());

    // Check for i64 overflow OR fixnum range overflow (62-bit signed)
    if (result[1] != 0 or result[0] > max_fixnum_i64 or result[0] < min_fixnum_i64) {
        // Overflow - promote to bignum
        return addBignum(heap, a, b);
    }
    return Value.makeFixnum(result[0]);
}

/// Subtract two numbers (fixnum, bignum, float, rational, or complex)
pub fn sub(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion
    if (a.typeKind() == .complex or b.typeKind() == .complex) return subComplex(heap, a, b);

    // Float contagion
    if (isFloatKind(a) or isFloatKind(b)) return subFloat(a, b);

    // Rational arithmetic
    if (a.typeKind() == .rational or b.typeKind() == .rational) return subRational(heap, a, b);

    // Bignum arithmetic
    if (a.isBignum() or b.isBignum()) return subBignum(heap, a, b);

    // Fixnum arithmetic with overflow check
    if (!a.isFixnum() or !b.isFixnum()) {
        if (std.posix.getenv("HABU_TRACE_SUB_MISMATCH") != null) {
            std.debug.print("TRACE sub mismatch: a={s} b={s}\n", .{ @tagName(a.typeKind()), @tagName(b.typeKind()) });
            if (a.isSymbol()) std.debug.print("  a.sym={s}\n", .{a.toPtr(objects.Symbol).getName()});
            if (b.isSymbol()) std.debug.print("  b.sym={s}\n", .{b.toPtr(objects.Symbol).getName()});
        }
        return error.TypeMismatch;
    }
    const result = @subWithOverflow(a.toFixnum(), b.toFixnum());

    // Check for i64 overflow OR fixnum range overflow (62-bit signed)
    if (result[1] != 0 or result[0] > max_fixnum_i64 or result[0] < min_fixnum_i64) {
        // Overflow - promote to bignum
        return subBignum(heap, a, b);
    }
    return Value.makeFixnum(result[0]);
}

/// Multiply two numbers (fixnum, bignum, float, rational, or complex)
pub fn mul(heap: *Heap, a: Value, b: Value) Error!Value {
    // Complex contagion
    if (a.typeKind() == .complex or b.typeKind() == .complex) return mulComplex(heap, a, b);

    // Float contagion
    if (isFloatKind(a) or isFloatKind(b)) return mulFloat(a, b);

    // Rational arithmetic
    if (a.typeKind() == .rational or b.typeKind() == .rational) return mulRational(heap, a, b);

    // Bignum arithmetic
    if (a.isBignum() or b.isBignum()) return mulBignum(heap, a, b);

    // Fixnum arithmetic with overflow check
    if (!a.isFixnum() or !b.isFixnum()) return error.TypeMismatch;
    const result = @mulWithOverflow(a.toFixnum(), b.toFixnum());

    // Check for i64 overflow OR fixnum range overflow (62-bit signed)
    if (result[1] != 0 or result[0] > max_fixnum_i64 or result[0] < min_fixnum_i64) {
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
    if (isFloatKind(a) or isFloatKind(b)) return divFloat(a, b);

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
    if (a.isFixnum() and b.isFixnum()) {
        const divisor = b.toFixnum();
        if (divisor == 0) return error.DivisionByZero;
        return Value.makeFixnum(@mod(a.toFixnum(), divisor));
    }

    const dividend = try toNumber(a);
    const divisor = try toNumber(b);
    if (divisor == 0.0) return error.DivisionByZero;
    const q = @floor(dividend / divisor);
    return Value.makeFloat(dividend - q * divisor);
}

/// Remainder operation (can be negative)
pub fn rem(a: Value, b: Value) Error!Value {
    if (a.isFixnum() and b.isFixnum()) {
        const divisor = b.toFixnum();
        if (divisor == 0) return error.DivisionByZero;
        return Value.makeFixnum(@rem(a.toFixnum(), divisor));
    }

    const dividend = try toNumber(a);
    const divisor = try toNumber(b);
    if (divisor == 0.0) return error.DivisionByZero;
    const q = @trunc(dividend / divisor);
    return Value.makeFloat(dividend - q * divisor);
}

/// Negate a fixnum
pub fn negate(a: Value) Error!Value {
    if (!a.isFixnum()) return error.TypeMismatch;
    const n = a.toFixnum();
    // -minInt overflows
    if (n == std.math.minInt(i64)) return error.TypeMismatch;
    return Value.makeFixnum(-n);
}

fn integerTwosWidth(v: Value) Error!usize {
    if (v.isFixnum()) return 1;
    if (!v.isBignum()) return error.TypeMismatch;

    const bn = v.toPtr(objects.Bignum);
    var width: usize = @intCast(@abs(bn.size));
    if (width == 0) return 1;

    // Positive sign-magnitude values with top-bit set need one extra sign limb.
    if (!bn.isNegative() and width < max_bignum_limbs and (bn.limbs[width - 1] & (@as(u64, 1) << 63)) != 0) {
        width += 1;
    }
    return width;
}

fn integerToTwos(v: Value, width: usize, out: *[max_bignum_limbs]u64) Error!void {
    if (width == 0 or width > max_bignum_limbs) return error.TypeMismatch;

    for (out, 0..) |*limb, i| {
        _ = i;
        limb.* = 0;
    }

    if (v.isFixnum()) {
        const n = v.toFixnum();
        const sign_fill: u64 = if (n < 0) std.math.maxInt(u64) else 0;
        out[0] = @bitCast(n);
        var i: usize = 1;
        while (i < width) : (i += 1) out[i] = sign_fill;
        return;
    }
    if (!v.isBignum()) return error.TypeMismatch;

    const bn = v.toPtr(objects.Bignum);
    const size: usize = @intCast(@abs(bn.size));
    if (!bn.isNegative()) {
        const copy_n = @min(size, width);
        for (0..copy_n) |i| out[i] = bn.limbs[i];
        return;
    }

    var carry: u64 = 1;
    var i: usize = 0;
    while (i < width) : (i += 1) {
        const mag = if (i < size) bn.limbs[i] else 0;
        const sum = @addWithOverflow(~mag, carry);
        out[i] = sum[0];
        carry = sum[1];
    }
}

fn twosToInteger(heap: *Heap, twos: *const [max_bignum_limbs]u64, width: usize) Error!Value {
    if (width == 0 or width > max_bignum_limbs) return error.TypeMismatch;

    const negative = (twos[width - 1] & (@as(u64, 1) << 63)) != 0;
    if (!negative) {
        var used = width;
        while (used > 0 and twos[used - 1] == 0) : (used -= 1) {}
        if (used == 0) return Value.makeFixnum(0);
        if (used == 1 and twos[0] <= max_fixnum_u64) return Value.makeFixnum(@intCast(twos[0]));
        return heap.allocBignumFromLimbs(twos[0..used], false);
    }

    var mag: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    var carry: u64 = 1;
    var i: usize = 0;
    while (i < width) : (i += 1) {
        const sum = @addWithOverflow(~twos[i], carry);
        mag[i] = sum[0];
        carry = sum[1];
    }

    var used = width;
    while (used > 0 and mag[used - 1] == 0) : (used -= 1) {}
    if (used == 0) return Value.makeFixnum(0);

    if (used == 1 and mag[0] <= max_neg_fixnum_mag_u64) {
        if (mag[0] == max_neg_fixnum_mag_u64) return Value.makeFixnum(min_fixnum_i64);
        const m: i64 = @intCast(mag[0]);
        return Value.makeFixnum(-m);
    }

    return heap.allocBignumFromLimbs(mag[0..used], true);
}

const BitBinaryOp = enum { and_, or_, xor_ };

fn bitBinary(heap: *Heap, a: Value, b: Value, op: BitBinaryOp) Error!Value {
    const width = @max(try integerTwosWidth(a), try integerTwosWidth(b));

    var at: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    var bt: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    var rt: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    try integerToTwos(a, width, &at);
    try integerToTwos(b, width, &bt);

    for (0..width) |i| {
        rt[i] = switch (op) {
            .and_ => at[i] & bt[i],
            .or_ => at[i] | bt[i],
            .xor_ => at[i] ^ bt[i],
        };
    }
    return twosToInteger(heap, &rt, width);
}

fn bitNot(heap: *Heap, a: Value) Error!Value {
    const width = try integerTwosWidth(a);
    var at: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    try integerToTwos(a, width, &at);
    for (0..width) |i| at[i] = ~at[i];
    return twosToInteger(heap, &at, width);
}

/// Bitwise AND
pub fn logand(heap: *Heap, a: Value, b: Value) Error!Value {
    return bitBinary(heap, a, b, .and_);
}

/// Bitwise OR
pub fn logior(heap: *Heap, a: Value, b: Value) Error!Value {
    return bitBinary(heap, a, b, .or_);
}

/// Bitwise XOR
pub fn logxor(heap: *Heap, a: Value, b: Value) Error!Value {
    return bitBinary(heap, a, b, .xor_);
}

/// Bitwise NOT
pub fn lognot(heap: *Heap, a: Value) Error!Value {
    return bitNot(heap, a);
}

/// Bitwise NAND
pub fn lognand(heap: *Heap, a: Value, b: Value) Error!Value {
    return bitNot(heap, try logand(heap, a, b));
}

/// Bitwise NOR
pub fn lognor(heap: *Heap, a: Value, b: Value) Error!Value {
    return bitNot(heap, try logior(heap, a, b));
}

/// Bitwise AND with NOT of first arg
pub fn logandc1(heap: *Heap, a: Value, b: Value) Error!Value {
    return logand(heap, try lognot(heap, a), b);
}

/// Bitwise AND with NOT of second arg
pub fn logandc2(heap: *Heap, a: Value, b: Value) Error!Value {
    return logand(heap, a, try lognot(heap, b));
}

/// Bitwise OR with NOT of first arg
pub fn logorc1(heap: *Heap, a: Value, b: Value) Error!Value {
    return logior(heap, try lognot(heap, a), b);
}

/// Bitwise OR with NOT of second arg
pub fn logorc2(heap: *Heap, a: Value, b: Value) Error!Value {
    return logior(heap, a, try lognot(heap, b));
}

/// Bitwise equivalence (NOT XOR)
pub fn logeqv(heap: *Heap, a: Value, b: Value) Error!Value {
    return bitNot(heap, try logxor(heap, a, b));
}

/// Test if bit at index is set
pub fn logbitp(index: Value, n: Value) Error!bool {
    if (!index.isFixnum() or !n.isFixnum()) return error.TypeMismatch;
    const idx = index.toFixnum();
    if (idx < 0) return error.TypeMismatch;
    const val = n.toFixnum();
    if (idx >= 63) return val < 0;
    const bit: u6 = @intCast(idx);
    return ((val >> bit) & 1) == 1;
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
    if (n.isFixnum()) {
        const val = n.toFixnum();
        if (val == 0) return Value.makeFixnum(0);
        const un: u64 = @bitCast(if (val < 0) ~val else val);
        const bits = 64 - @clz(un);
        return Value.makeFixnum(@intCast(bits));
    }
    if (!n.isBignum()) return error.TypeMismatch;

    const bn = n.toPtr(objects.Bignum);
    const size: usize = @intCast(@abs(bn.size));
    if (size == 0) return Value.makeFixnum(0);

    if (!bn.isNegative()) {
        const top = bn.limbs[size - 1];
        const bits = (size - 1) * 64 + (64 - @clz(top));
        return Value.makeFixnum(@intCast(bits));
    }

    const width = try integerTwosWidth(n);
    var twos: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    try integerToTwos(n, width, &twos);
    for (0..width) |i| twos[i] = ~twos[i];

    var used = width;
    while (used > 0 and twos[used - 1] == 0) : (used -= 1) {}
    if (used == 0) return Value.makeFixnum(0);

    const bits = (used - 1) * 64 + (64 - @clz(twos[used - 1]));
    return Value.makeFixnum(@intCast(bits));
}

/// Arithmetic shift (positive = left, negative = right)
pub fn ash(val: Value, count: Value) Error!Value {
    if ((!val.isFixnum() and !val.isBignum()) or !count.isFixnum()) return error.TypeMismatch;

    const c = count.toFixnum();
    if (val.isFixnum()) {
        const v = val.toFixnum();
        if (c >= 0 and c < 62) {
            const uc: u6 = @intCast(@as(u64, @intCast(c)));
            const shifted = v << uc;
            if (shifted <= max_fixnum_i64 and shifted >= min_fixnum_i64) {
                return Value.makeFixnum(shifted);
            }
        } else if (c < 0) {
            const neg_c: u64 = @intCast(-c);
            if (neg_c >= 64) {
                return Value.makeFixnum(if (v < 0) -1 else 0);
            }
            const uc: u6 = @intCast(neg_c);
            return Value.makeFixnum(v >> uc);
        }
    }

    const heap = if (@import("../runtime.zig").heapContext()) |h| h else return error.OutOfMemory;
    const base_width = try integerTwosWidth(val);
    const shift_mag: usize = @intCast(if (c < 0) -c else c);
    const limb_shift = shift_mag / 64;
    const bit_shift: u6 = @intCast(shift_mag % 64);
    const width = if (c >= 0)
        @min(max_bignum_limbs, base_width + limb_shift + 1)
    else
        base_width;

    var in_twos: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    var out_twos: [max_bignum_limbs]u64 = [_]u64{0} ** max_bignum_limbs;
    try integerToTwos(val, width, &in_twos);

    const sign_fill: u64 = if ((in_twos[width - 1] & (@as(u64, 1) << 63)) != 0) std.math.maxInt(u64) else 0;

    if (c >= 0) {
        for (0..width) |i| out_twos[i] = 0;
        for (0..base_width) |i| {
            const dst = i + limb_shift;
            if (dst >= width) break;
            out_twos[dst] |= in_twos[i] << bit_shift;
            if (bit_shift != 0 and dst + 1 < width) {
                const carry_shift: u6 = @intCast(@as(u7, 64) - @as(u7, bit_shift));
                out_twos[dst + 1] |= in_twos[i] >> carry_shift;
            }
        }
    } else {
        for (0..width) |i| out_twos[i] = sign_fill;
        for (0..width) |i| {
            const src_idx = i + limb_shift;
            const low = if (src_idx < width) in_twos[src_idx] else sign_fill;
            if (bit_shift == 0) {
                out_twos[i] = low;
            } else {
                const high = if (src_idx + 1 < width) in_twos[src_idx + 1] else sign_fill;
                const carry_shift: u6 = @intCast(@as(u7, 64) - @as(u7, bit_shift));
                out_twos[i] = (low >> bit_shift) | (high << carry_shift);
            }
        }
    }

    return twosToInteger(heap, &out_twos, width);
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
    if (a.isFixnum()) {
        const n = a.toFixnum();
        return Value.makeFixnum(if (n < 0) -n else n);
    }
    if (!a.isBignum()) return error.TypeMismatch;

    const bn = a.toPtr(objects.Bignum);
    if (!bn.isNegative()) return a;

    const heap = if (@import("../runtime.zig").heapContext()) |h| h else return error.OutOfMemory;
    const size: usize = @intCast(@abs(bn.size));
    return heap.allocBignumFromLimbs(bn.limbs[0..size], false);
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
    // Complex comparison
    if (a.typeKind() == .complex or b.typeKind() == .complex) {
        const ca = complexParts(a) orelse return false;
        const cb = complexParts(b) orelse return false;
        return numEq(ca.real, cb.real) and numEq(ca.imag, cb.imag);
    }

    // Float comparison (contagion)
    if (isFloatKind(a) or isFloatKind(b)) {
        const af = toNumber(a) catch return false;
        const bf = toNumber(b) catch return false;
        return af == bf;
    }

    // Rational comparison
    if (a.typeKind() == .rational or b.typeKind() == .rational) {
        const ra = toRational(a) catch return false;
        const rb = toRational(b) catch return false;
        return ra.num == rb.num and ra.den == rb.den;
    }

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
pub fn lt(a: Value, b: Value) Error!bool {
    // Float comparison
    if (isFloatKind(a) or isFloatKind(b)) {
        const af = try toNumber(a);
        const bf = try toNumber(b);
        return af < bf;
    }

    // Rational comparison
    if (a.typeKind() == .rational or b.typeKind() == .rational) {
        const ra = try toRational(a);
        const rb = try toRational(b);
        // a/b < c/d iff a*d < c*b (when b,d > 0, which normalize ensures)
        return ra.num * rb.den < rb.num * ra.den;
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
pub fn gt(a: Value, b: Value) Error!bool {
    // Float comparison
    if (isFloatKind(a) or isFloatKind(b)) {
        const af = try toNumber(a);
        const bf = try toNumber(b);
        return af > bf;
    }

    // Rational comparison
    if (a.typeKind() == .rational or b.typeKind() == .rational) {
        const ra = try toRational(a);
        const rb = try toRational(b);
        return ra.num * rb.den > rb.num * ra.den;
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
pub fn le(a: Value, b: Value) Error!bool {
    // Float comparison
    if (isFloatKind(a) or isFloatKind(b)) {
        const af = try toNumber(a);
        const bf = try toNumber(b);
        return af <= bf;
    }

    // Rational comparison
    if (a.typeKind() == .rational or b.typeKind() == .rational) {
        const ra = try toRational(a);
        const rb = try toRational(b);
        return ra.num * rb.den <= rb.num * ra.den;
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
pub fn ge(a: Value, b: Value) Error!bool {
    // Float comparison
    if (isFloatKind(a) or isFloatKind(b)) {
        const af = try toNumber(a);
        const bf = try toNumber(b);
        return af >= bf;
    }

    // Rational comparison
    if (a.typeKind() == .rational or b.typeKind() == .rational) {
        const ra = try toRational(a);
        const rb = try toRational(b);
        return ra.num * rb.den >= rb.num * ra.den;
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

pub const RangeError = error{InvalidRange};

/// Seed the random number generator
pub fn randomSeed(prng: *std.Random.DefaultPrng, seeded: *bool, seed: Value) Error!Value {
    if (!seed.isFixnum()) return error.TypeMismatch;
    const s: u64 = @bitCast(seed.toFixnum());
    prng.* = std.Random.DefaultPrng.init(s);
    seeded.* = true;
    return seed;
}

/// Generate random number in [0, n)
/// - Integer/bignum bounds return non-negative integers less than n.
/// - Float bounds return floats in [0.0, n).
pub fn random(heap: *Heap, prng: *std.Random.DefaultPrng, seeded: *bool, n: Value) (Error || RangeError)!Value {
    // Auto-seed on first use
    if (!seeded.*) {
        const ts = std.time.nanoTimestamp();
        const seed: u64 = @truncate(@as(u128, @bitCast(ts)));
        prng.* = std.Random.DefaultPrng.init(seed);
        seeded.* = true;
    }

    const rand = prng.random();

    if (n.isFixnum()) {
        const max = n.toFixnum();
        if (max <= 0) return error.InvalidRange;
        const result = rand.intRangeLessThan(i64, 0, max);
        return Value.makeFixnum(result);
    }

    if (isFloatKind(n)) {
        const max = n.toFloat();
        if (!(max > 0.0) or !std.math.isFinite(max)) {
            if (std.posix.getenv("HABU_TRACE_RANDOM_RANGE") != null) {
                std.debug.print(
                    "TRACE random invalid float bound={d} raw=0x{x} tag={d} kind={s}\n",
                    .{ max, n.raw, @intFromEnum(n.getTag()), @tagName(n.typeKind()) },
                );
            }
            return error.InvalidRange;
        }
        return Value.makeFloat(rand.float(f64) * max);
    }

    if (n.isBignum()) {
        return randomBignumLessThan(heap, rand, n.toPtr(objects.Bignum));
    }

    if (std.posix.getenv("HABU_TRACE_RANDOM_MISMATCH") != null) {
        std.debug.print("TRACE random mismatch type={s}\n", .{@tagName(n.typeKind())});
    }
    return error.TypeMismatch;
}

fn randomBignumLessThan(heap: *Heap, rand: std.Random, limit: *const objects.Bignum) (Error || RangeError)!Value {
    if (limit.size <= 0) return error.InvalidRange;
    const size: usize = @intCast(limit.size);
    if (size == 0) return error.InvalidRange;
    if (std.posix.getenv("HABU_TRACE_RANDOM_BIGNUM") != null) {
        const top_idx = size - 1;
        std.debug.print("TRACE random-bignum size={d} top=0x{x}\n", .{ size, limit.limbs[top_idx] });
    }

    if (size == 1) {
        const max = limit.limbs[0];
        if (max == 0) return error.InvalidRange;
        const sample = rand.intRangeLessThan(u64, 0, max);
        if (sample <= max_fixnum_u64) return Value.makeFixnum(@intCast(sample));
        var limbs: [8]u64 = [_]u64{0} ** 8;
        limbs[0] = sample;
        return heap.allocBignumFromLimbs(limbs[0..1], false);
    }

    const top = limit.limbs[size - 1];
    if (top == 0) return error.InvalidRange;
    const top_bits: u7 = @intCast(64 - @clz(top));
    const top_mask: u64 = if (top_bits == 64) std.math.maxInt(u64) else (@as(u64, 1) << @intCast(top_bits)) - 1;

    var stack_candidate: [8]u64 = [_]u64{0} ** 8;
    var candidate = stack_candidate[0..@min(size, stack_candidate.len)];
    if (size > stack_candidate.len) {
        candidate = try heap.backing_allocator.alloc(u64, size);
        defer heap.backing_allocator.free(candidate);
    }

    while (true) {
        for (0..size) |i| candidate[i] = rand.int(u64);
        candidate[size - 1] &= top_mask;

        if (limbsLessThanLimit(candidate, limit, size)) {
            return valueFromUnsignedLimbs(heap, candidate[0..size]);
        }
    }
}

fn limbsLessThanLimit(candidate: []const u64, limit: *const objects.Bignum, size: usize) bool {
    var i = size;
    while (i > 0) {
        i -= 1;
        const a = candidate[i];
        const b = limit.limbs[i];
        if (a < b) return true;
        if (a > b) return false;
    }
    return false;
}

fn valueFromUnsignedLimbs(heap: *Heap, limbs: []const u64) Error!Value {
    var used = limbs.len;
    while (used > 0 and limbs[used - 1] == 0) : (used -= 1) {}
    if (used == 0) return Value.makeFixnum(0);
    if (used == 1 and limbs[0] <= max_fixnum_u64) return Value.makeFixnum(@intCast(limbs[0]));
    return heap.allocBignumFromLimbs(limbs[0..used], false);
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
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const a = Value.makeFixnum(0b1100);
    const b = Value.makeFixnum(0b1010);

    try testing.expectEqual(@as(i64, 0b1000), (try logand(&heap, a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 0b1110), (try logior(&heap, a, b)).toFixnum());
    try testing.expectEqual(@as(i64, 0b0110), (try logxor(&heap, a, b)).toFixnum());
}

test "bitwise operations support bignum intermediates" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const big = try mul(&heap, Value.makeFixnum(1812433253), Value.makeFixnum(4294967295));
    try testing.expect(big.isBignum());

    const masked = try logand(&heap, big, Value.makeFixnum(4294967295));
    try testing.expect(masked.isFixnum());
    try testing.expectEqual(@as(i64, 2482534043), masked.toFixnum());

    const not_big = try lognot(&heap, big);
    try testing.expect(not_big.isBignum() or not_big.isFixnum());
    const not_masked = try logand(&heap, not_big, Value.makeFixnum(4294967295));
    try testing.expectEqual(@as(i64, 1812433252), not_masked.toFixnum());

    const neg_big = try sub(&heap, Value.makeFixnum(0), big);
    const neg_masked = try logand(&heap, neg_big, Value.makeFixnum(4294967295));
    try testing.expectEqual(@as(i64, 1812433253), neg_masked.toFixnum());

    const eqv_self = try logeqv(&heap, big, big);
    try testing.expect(eqv_self.isFixnum());
    try testing.expectEqual(@as(i64, -1), eqv_self.toFixnum());
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

    try testing.expect(try lt(a, b));
    try testing.expect(!try lt(b, a));
    try testing.expect(!try lt(a, c));

    try testing.expect(try gt(b, a));
    try testing.expect(try le(a, b));
    try testing.expect(try le(a, c));
    try testing.expect(try ge(b, a));
    try testing.expect(try ge(a, c));

    try testing.expect(numEq(a, c));
    try testing.expect(!numEq(a, b));
}

// ============================================================================
// Float arithmetic (Common Lisp numeric tower with contagion)
// ============================================================================

/// Helper: convert Value to f64
pub fn toNumber(v: Value) Error!f64 {
    return switch (v.typeKind()) {
        .float => v.toFloat(),
        .fixnum => @floatFromInt(v.toFixnum()),
        .rational => blk: {
            const rat = v.toPtr(objects.Rational);
            const num: f64 = @floatFromInt(rat.numerator);
            const den: f64 = @floatFromInt(rat.denominator);
            break :blk num / den;
        },
        .bignum => blk: {
            const bn = v.toPtr(objects.Bignum);
            const size: usize = @intCast(@abs(bn.size));
            var out: f64 = 0.0;
            var i = size;
            while (i > 0) {
                i -= 1;
                out = out * 18446744073709551616.0 + @as(f64, @floatFromInt(bn.limbs[i]));
            }
            break :blk if (bn.isNegative()) -out else out;
        },
        else => error.TypeMismatch,
    };
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
        const heap = if (@import("../runtime.zig").heapContext()) |val| val else return error.OutOfMemory;
        return complex_mod.makeComplex(heap, Value.makeFixnum(0), Value.makeFloat(@sqrt(-af)));
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

/// Decode-float: returns (significand, exponent, sign) as raw values
/// Such that (= f (* sign significand (expt 2 exponent)))
/// significand is between 0.5 (inclusive) and 1.0 (exclusive) for non-zero
pub fn decodeFloat(a: Value) Error![3]Value {
    const f = try toNumber(a);

    // Handle special cases
    if (f == 0.0) {
        return .{
            Value.makeFloat(0.0),
            Value.makeFixnum(0),
            Value.makeFloat(1.0),
        };
    }

    const sign: f64 = if (f < 0) -1.0 else 1.0;
    const abs_f = @abs(f);

    // Get the binary exponent using frexp-like logic
    // frexp returns (significand, exponent) where 0.5 <= |sig| < 1.0
    const frexp_result = std.math.frexp(abs_f);

    return .{
        Value.makeFloat(frexp_result.significand),
        Value.makeFixnum(frexp_result.exponent),
        Value.makeFloat(sign),
    };
}

/// Integer-decode-float: returns (significand, exponent, sign) as integers
/// Such that (= f (* sign significand (expt 2 exponent)))
/// significand is an integer representing the mantissa bits
pub fn integerDecodeFloat(a: Value) Error![3]Value {
    const f = try toNumber(a);

    // Handle special cases
    if (f == 0.0) {
        return .{
            Value.makeFixnum(0),
            Value.makeFixnum(0),
            Value.makeFixnum(1),
        };
    }

    // Extract bits from the IEEE 754 representation
    const bits: u64 = @bitCast(f);

    // IEEE 754 double: 1 sign + 11 exponent + 52 mantissa
    const sign_bit = (bits >> 63) & 1;
    const exp_bits = (bits >> 52) & 0x7FF;
    var mantissa = bits & 0xFFFFFFFFFFFFF;

    const sign: i64 = if (sign_bit == 1) -1 else 1;

    // Handle denormalized numbers (exp_bits == 0)
    var exponent: i64 = undefined;
    if (exp_bits == 0) {
        // Denormalized: no implicit leading 1
        exponent = 1 - 1023 - 52; // -1074
    } else {
        // Normalized: implicit leading 1
        mantissa |= (1 << 52);
        exponent = @as(i64, @intCast(exp_bits)) - 1023 - 52;
    }

    // Normalize: remove trailing zeros from mantissa, adjust exponent
    while (mantissa != 0 and (mantissa & 1) == 0) {
        mantissa >>= 1;
        exponent += 1;
    }

    return .{
        Value.makeFixnum(@intCast(mantissa)),
        Value.makeFixnum(exponent),
        Value.makeFixnum(sign),
    };
}

/// Float-radix: returns the radix of floating-point representation (always 2)
pub fn floatRadix(_: Value) Error!Value {
    return Value.makeFixnum(2);
}

/// Float-digits: returns the number of digits in floating-point representation
pub fn floatDigits(_: Value) Error!Value {
    // IEEE 754 double has 53 bits of precision (52 mantissa + implicit leading 1)
    return Value.makeFixnum(53);
}

/// Float-precision: returns the precision of the float (same as float-digits for normalized)
pub fn floatPrecision(a: Value) Error!Value {
    const f = try toNumber(a);
    if (f == 0.0) return Value.makeFixnum(0);

    // For denormalized numbers, precision may be less
    const bits: u64 = @bitCast(f);
    const exp_bits = (bits >> 52) & 0x7FF;

    if (exp_bits == 0) {
        // Denormalized: count significant bits in mantissa
        var mantissa = bits & 0xFFFFFFFFFFFFF;
        var precision: i64 = 0;
        while (mantissa != 0) {
            precision += 1;
            mantissa >>= 1;
        }
        return Value.makeFixnum(precision);
    }

    // Normalized: full precision
    return Value.makeFixnum(53);
}

/// Floor: largest integer not greater than x/y
pub fn floor_val(_: *Heap, x: Value, y: Value) Error!Value {
    if (isFloatKind(x) or isFloatKind(y)) {
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
    if (isFloatKind(x) or isFloatKind(y)) {
        const xf = try toNumber(x);
        const yf = try toNumber(y);
        const q = @ceil(xf / yf);
        return Value.makeFloat(q);
    }

    if (!x.isFixnum() or !y.isFixnum()) return error.TypeMismatch;
    const dividend = x.toFixnum();
    const divisor = y.toFixnum();
    if (divisor == 0) return error.DivisionByZero;

    // minInt / -1 overflows
    if (dividend == std.math.minInt(i64) and divisor == -1) return error.TypeMismatch;

    const q = @divTrunc(dividend, divisor);
    const r = @rem(dividend, divisor);
    const needs_inc = r != 0 and ((r > 0) == (divisor > 0));
    return Value.makeFixnum(if (needs_inc) q + 1 else q);
}

/// Truncate: integer part of x/y toward zero
pub fn trunc_val(_: *Heap, x: Value, y: Value) Error!Value {
    if (isFloatKind(x) or isFloatKind(y)) {
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
    if (isFloatKind(x) or isFloatKind(y)) {
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
    // If denominator is 1 after normalization, return fixnum
    if (rat.denominator == 1) return Value.makeFixnum(rat.numerator);
    const ptr = try heap.alloc(objects.Rational);
    ptr.* = rat;
    return Value.makeRational(ptr);
}

// ============================================================================
// Complex arithmetic
// ============================================================================

/// Helper: convert Value to complex (real, imag)
fn complexParts(v: Value) ?struct { real: Value, imag: Value } {
    switch (v.typeKind()) {
        .complex => {
            const cplx = v.toPtr(objects.Complex);
            return .{ .real = cplx.real, .imag = cplx.imag };
        },
        .float, .fixnum, .rational, .bignum => return .{ .real = v, .imag = Value.makeFixnum(0) },
        else => return null,
    }
}

/// Complex addition: supports mixed types
fn addComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = complexParts(a) orelse return error.TypeMismatch;
    const cb = complexParts(b) orelse return error.TypeMismatch;
    return heap.allocComplex(try add(heap, ca.real, cb.real), try add(heap, ca.imag, cb.imag));
}

/// Complex subtraction: supports mixed types
fn subComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = complexParts(a) orelse return error.TypeMismatch;
    const cb = complexParts(b) orelse return error.TypeMismatch;
    return heap.allocComplex(try sub(heap, ca.real, cb.real), try sub(heap, ca.imag, cb.imag));
}

/// Complex multiplication: supports mixed types
fn mulComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = complexParts(a) orelse return error.TypeMismatch;
    const cb = complexParts(b) orelse return error.TypeMismatch;
    const ac = try mul(heap, ca.real, cb.real);
    const bd = try mul(heap, ca.imag, cb.imag);
    const ad = try mul(heap, ca.real, cb.imag);
    const bc = try mul(heap, ca.imag, cb.real);
    return heap.allocComplex(try sub(heap, ac, bd), try add(heap, ad, bc));
}

/// Complex division: supports mixed types
fn divComplex(heap: *Heap, a: Value, b: Value) Error!Value {
    const ca = complexParts(a) orelse return error.TypeMismatch;
    const cb = complexParts(b) orelse return error.TypeMismatch;
    const cc = try mul(heap, cb.real, cb.real);
    const dd = try mul(heap, cb.imag, cb.imag);
    const denom = try add(heap, cc, dd);
    if (numEq(denom, Value.makeFixnum(0))) return error.DivisionByZero;
    const ac = try mul(heap, ca.real, cb.real);
    const bd = try mul(heap, ca.imag, cb.imag);
    const bc = try mul(heap, ca.imag, cb.real);
    const ad = try mul(heap, ca.real, cb.imag);
    return heap.allocComplex(
        try div(heap, try add(heap, ac, bd), denom),
        try div(heap, try sub(heap, bc, ad), denom),
    );
}

// ============================================================================
// Bignum arithmetic
// ============================================================================

/// Subtract bignum magnitudes when signs differ
fn subBignumMagnitudes(heap: *Heap, a_bn: *const objects.Bignum, b_bn: *const objects.Bignum, a_neg: bool) Error!Value {
    const a_size: usize = @intCast(@abs(a_bn.size));
    const b_size: usize = @intCast(@abs(b_bn.size));

    // Determine which magnitude is larger
    const a_larger = blk: {
        if (a_size != b_size) break :blk a_size > b_size;
        var i = a_size;
        while (i > 0) {
            i -= 1;
            if (a_bn.limbs[i] != b_bn.limbs[i]) {
                break :blk a_bn.limbs[i] > b_bn.limbs[i];
            }
        }
        break :blk true; // Equal magnitudes
    };

    // larger - smaller
    const larger = if (a_larger) a_bn else b_bn;
    const smaller = if (a_larger) b_bn else a_bn;
    const larger_size = if (a_larger) a_size else b_size;
    const smaller_size = if (a_larger) b_size else a_size;

    var result_limbs: [8]u64 = [_]u64{0} ** 8;
    var borrow: u64 = 0;

    for (0..larger_size) |i| {
        const l_limb = larger.limbs[i];
        const s_limb = if (i < smaller_size) smaller.limbs[i] else 0;

        const sub_res = @subWithOverflow(l_limb, s_limb);
        const sub_with_borrow = @subWithOverflow(sub_res[0], borrow);

        result_limbs[i] = sub_with_borrow[0];
        borrow = sub_res[1] + sub_with_borrow[1];
    }

    // Result sign: if a was larger, use a's sign; else flip
    const result_neg = if (a_larger) a_neg else !a_neg;

    return heap.allocBignumFromLimbs(&result_limbs, result_neg);
}

/// Subtract two bignums (or mixed bignum/fixnum)
fn subBignum(heap: *Heap, a: Value, b: Value) Error!Value {
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

    // a - b = a + (-b), so flip b's sign and add
    return addBignumImpl(heap, a_bn, b_bn, a_neg, !b_neg);
}

/// Add two bignum structs with explicit signs
fn addBignumImpl(heap: *Heap, a_bn: *const objects.Bignum, b_bn: *const objects.Bignum, a_neg: bool, b_neg: bool) Error!Value {
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
        return subBignumMagnitudes(heap, a_bn, b_bn, a_neg);
    }
}

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

    return addBignumImpl(heap, a_bn, b_bn, a_neg, b_neg);
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

test "logbitp fixnum boundaries" {
    const testing = std.testing;

    // Positive fixnum
    try testing.expect(try logbitp(Value.makeFixnum(0), Value.makeFixnum(0b1011)) == true); // bit 0
    try testing.expect(try logbitp(Value.makeFixnum(1), Value.makeFixnum(0b1011)) == true); // bit 1
    try testing.expect(try logbitp(Value.makeFixnum(2), Value.makeFixnum(0b1011)) == false); // bit 2
    try testing.expect(try logbitp(Value.makeFixnum(3), Value.makeFixnum(0b1011)) == true); // bit 3

    // Negative fixnum (-1 has all bits set)
    try testing.expect(try logbitp(Value.makeFixnum(0), Value.makeFixnum(-1)) == true);
    try testing.expect(try logbitp(Value.makeFixnum(31), Value.makeFixnum(-1)) == true);
    try testing.expect(try logbitp(Value.makeFixnum(62), Value.makeFixnum(-1)) == true);
    try testing.expect(try logbitp(Value.makeFixnum(63), Value.makeFixnum(-1)) == true);
    try testing.expect(try logbitp(Value.makeFixnum(127), Value.makeFixnum(-1)) == true);
    try testing.expect(try logbitp(Value.makeFixnum(1000), Value.makeFixnum(-1)) == true);

    // Boundary at 63 (sign bit)
    try testing.expect(try logbitp(Value.makeFixnum(62), Value.makeFixnum(1)) == false);
    try testing.expect(try logbitp(Value.makeFixnum(63), Value.makeFixnum(1)) == false);
    try testing.expect(try logbitp(Value.makeFixnum(62), Value.makeFixnum(-2)) == true);
    try testing.expect(try logbitp(Value.makeFixnum(63), Value.makeFixnum(-2)) == true);

    // Zero
    try testing.expect(try logbitp(Value.makeFixnum(0), Value.makeFixnum(0)) == false);
    try testing.expect(try logbitp(Value.makeFixnum(100), Value.makeFixnum(0)) == false);
}

test "logbitp negative index" {
    const testing = std.testing;

    // Negative index is an error
    try testing.expectError(error.TypeMismatch, logbitp(Value.makeFixnum(-1), Value.makeFixnum(42)));
}

test "logbitp type errors" {
    const testing = std.testing;

    // Non-integer index
    try testing.expectError(error.TypeMismatch, logbitp(Value.nil, Value.makeFixnum(42)));

    // Non-integer value
    try testing.expectError(error.TypeMismatch, logbitp(Value.makeFixnum(0), Value.nil));
}

test "random supports float bounds" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var prng = std.Random.DefaultPrng.init(1234567);
    var seeded = true;
    const result = try random(&heap, &prng, &seeded, Value.makeFloat(10.0));
    try testing.expect(result.isFloat());
    try testing.expect(result.toFloat() >= 0.0 and result.toFloat() < 10.0);
}

test "random supports bignum bounds" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const limit = try heap.allocBignumFromLimbs(&[_]u64{ 0, 1 }, false); // 2^64
    var prng = std.Random.DefaultPrng.init(7654321);
    var seeded = true;

    const result = try random(&heap, &prng, &seeded, limit);
    try testing.expect(result.isFixnum() or result.isBignum());
    if (result.isFixnum()) {
        try testing.expect(result.toFixnum() >= 0);
    } else {
        try testing.expect(!result.toPtr(objects.Bignum).isNegative());
    }
}
