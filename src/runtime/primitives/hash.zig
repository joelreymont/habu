//! Hash table primitives

const std = @import("std");
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const runtime = @import("../runtime.zig");
const Heap = @import("../heap.zig").Heap;
const HashTable = objects.HashTable;
const HashTest = objects.HashTest;

/// (make-hash-table &key test size) - create hash table
/// test: eq (default), eql, equal, equalp
/// size: initial capacity (default 16)
pub fn primMakeHashTable(heap: *Heap, args: []const Value) !Value {
    // Intern keywords and symbols once
    const kw_test = try heap.internKeyword("test");
    const kw_size = try heap.internKeyword("size");
    const sym_eq = try heap.intern("eq");
    const sym_eql = try heap.intern("eql");
    const sym_equal = try heap.intern("equal");
    const sym_equalp = try heap.intern("equalp");

    var test_type = HashTest.eq;
    var size: usize = 16;

    // Parse keyword arguments
    var i: usize = 0;
    while (i < args.len) : (i += 2) {
        if (i + 1 >= args.len) return error.TypeMismatch;

        const key = args[i];
        const val = args[i + 1];

        if (!key.isKeyword()) return error.TypeMismatch;

        if (key.eq(kw_test)) {
            if (!val.isSymbol()) return error.TypeMismatch;

            if (val.eq(sym_eq)) {
                test_type = .eq;
            } else if (val.eq(sym_eql)) {
                test_type = .eql;
            } else if (val.eq(sym_equal)) {
                test_type = .equal;
            } else if (val.eq(sym_equalp)) {
                test_type = .equalp;
            } else {
                return error.TypeMismatch;
            }
        } else if (key.eq(kw_size)) {
            if (!val.isFixnum()) return error.TypeMismatch;
            const n = val.toFixnum();
            if (n < 0) return error.TypeMismatch;
            size = @intCast(n);
        }
    }

    return heap.allocHashTable(size, test_type);
}

/// (gethash key hash-table &optional default) - get value from hash table
pub fn primGethash(_: *Heap, args: []const Value) !Value {
    if (args.len < 2 or args.len > 3) return error.TypeMismatch;

    const key = args[0];
    const ht_val = args[1];
    const default = if (args.len == 3) args[2] else Value.nil;

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    if (ht.get(key)) |val| {
        return val;
    }
    return default;
}

/// (puthash key value hash-table) - set value in hash table
/// Returns value
pub fn primPuthash(heap: *Heap, args: []const Value) !Value {
    if (args.len != 3) return error.TypeMismatch;

    const key = args[0];
    const value = args[1];
    const ht_val = args[2];

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    try ht.put(key, value);
    heap.writeBarrier(ht_val, key);
    heap.writeBarrier(ht_val, value);
    return value;
}

/// (remhash key hash-table) - remove key from hash table
/// Returns t if key was present, nil otherwise
pub fn primRemhash(_: *Heap, args: []const Value) !Value {
    if (args.len != 2) return error.TypeMismatch;

    const key = args[0];
    const ht_val = args[1];

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    const removed = ht.remove(key);
    return if (removed) Value.t else Value.nil;
}

/// (clrhash hash-table) - clear all entries
/// Returns the hash table
pub fn primClrhash(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    ht.clear();
    return ht_val;
}

/// (hash-table-count hash-table) - get number of entries
pub fn primHashTableCount(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    return Value.makeFixnum(@intCast(ht.count));
}

/// (hash-table-capacity hash-table) - get allocated capacity
pub fn primHashTableCapacity(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    return Value.makeFixnum(@intCast(ht.capacity));
}

/// (hash-table-test hash-table) - get test function
pub fn primHashTableTest(heap: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;

    const ht_val = args[0];
    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);
    const test_name = switch (ht.test_type) {
        .eq => "eq",
        .eql => "eql",
        .equal => "equal",
        .equalp => "equalp",
    };
    return heap.intern(test_name);
}

/// (maphash function hash-table) - call function on each key-value pair
/// Returns nil
pub fn primMaphash(heap: *Heap, args: []const Value) !Value {
    if (args.len != 2) return error.TypeMismatch;

    const func = args[0];
    const ht_val = args[1];

    if (ht_val.typeKind() != .hashtable) return error.TypeMismatch;

    const ht = ht_val.toPtr(HashTable);

    // Iterate over entries
    for (0..@intCast(ht.capacity)) |i| {
        const entry_key = ht.getKey(i);
        if (!HashTable.isAvailableKey(entry_key)) {
            const entry_val = ht.getValue(i);
            // Call function with (key value)
            const call_args = [_]Value{ entry_key, entry_val };

            // Need to call the function - for now just return nil
            // Full implementation would need VM callback
            _ = func;
            _ = call_args;
            _ = heap;
        }
    }

    return Value.nil;
}

/// (sxhash object) - compute hash code for object
/// Returns non-negative fixnum
pub fn primSxhash(_: *Heap, args: []const Value) !Value {
    if (args.len != 1) return error.TypeMismatch;
    const obj = args[0];
    const h = hashValue(obj);
    const fixnum_h = @as(i63, @intCast(h & 0x3FFFFFFFFFFFFFFF));
    return Value.makeFixnum(fixnum_h);
}

// ============================================================================
// Equality + hashing helpers (hash table + equalp)
// ============================================================================

/// Float equality per CL semantics: NaN != NaN, 0.0 == -0.0
pub fn floatEql(fa: f64, fb: f64) bool {
    if (std.math.isNan(fa) or std.math.isNan(fb)) return false;
    return fa == fb;
}

fn normalizeFloatForHash(f: f64) u64 {
    if (std.math.isNan(f)) return 0x7FF8_0000_0000_0000; // canonical NaN
    if (f == 0.0) return 0; // normalize -0.0 to 0.0
    return @bitCast(f);
}

fn upperAsciiCp(cp: u32) u32 {
    return if (cp >= 'a' and cp <= 'z') cp - 32 else cp;
}

fn isCharacterVector(val: Value) bool {
    return val.isVector() and val.toPtr(objects.Vector).isCharacterVector();
}

fn isStringLike(val: Value) bool {
    return val.isString() or val.isString32() or isCharacterVector(val);
}

fn stringLikeLen(val: Value) ?usize {
    return switch (val.typeKind()) {
        .string => val.toPtr(objects.String).length,
        .string32 => val.toPtr(objects.String32).length,
        .vector => blk: {
            const vec = val.toPtr(objects.Vector);
            if (!vec.isCharacterVector()) break :blk null;
            break :blk @intCast(vec.getFillPointer() orelse vec.length);
        },
        else => null,
    };
}

fn stringLikeCodepointAt(val: Value, idx: usize) ?u32 {
    return switch (val.typeKind()) {
        .string => blk: {
            const s = val.toPtr(objects.String);
            if (idx >= s.length) break :blk null;
            break :blk s.data[idx];
        },
        .string32 => blk: {
            const s = val.toPtr(objects.String32);
            if (idx >= s.length) break :blk null;
            break :blk s.codepoints()[idx];
        },
        .vector => blk: {
            const vec = val.toPtr(objects.Vector);
            if (!vec.isCharacterVector()) break :blk null;
            const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
            if (idx >= len) break :blk null;
            const ch = vec.data[idx];
            break :blk switch (ch.typeKind()) {
                .char => ch.toCharacter(),
                .fixnum => blk2: {
                    const cp = ch.toFixnum();
                    if (cp < 0 or cp > std.math.maxInt(u21)) break :blk2 null;
                    break :blk2 @intCast(cp);
                },
                else => null,
            };
        },
        else => null,
    };
}

fn stringLikeEqual(a: Value, b: Value) bool {
    const len_a = stringLikeLen(a) orelse return false;
    const len_b = stringLikeLen(b) orelse return false;
    if (len_a != len_b) return false;
    for (0..len_a) |i| {
        const ca = stringLikeCodepointAt(a, i) orelse return false;
        const cb = stringLikeCodepointAt(b, i) orelse return false;
        if (ca != cb) return false;
    }
    return true;
}

fn stringLikeEqualp(a: Value, b: Value) bool {
    const len_a = stringLikeLen(a) orelse return false;
    const len_b = stringLikeLen(b) orelse return false;
    if (len_a != len_b) return false;
    for (0..len_a) |i| {
        const ca = stringLikeCodepointAt(a, i) orelse return false;
        const cb = stringLikeCodepointAt(b, i) orelse return false;
        if (upperAsciiCp(ca) != upperAsciiCp(cb)) return false;
    }
    return true;
}

fn hashStringLike(val: Value) u64 {
    const len = stringLikeLen(val) orelse return 0;
    var h: u64 = 0;
    for (0..len) |i| {
        const cp = stringLikeCodepointAt(val, i) orelse return 0;
        h = h *% 31 +% cp;
    }
    return h;
}

fn hashStringLikeEqualp(val: Value) u64 {
    const len = stringLikeLen(val) orelse return 0;
    var h: u64 = 0;
    for (0..len) |i| {
        const cp = stringLikeCodepointAt(val, i) orelse return 0;
        h = hashMix(h, upperAsciiCp(cp));
    }
    return h;
}

fn bignumEq(a: *const objects.Bignum, b: *const objects.Bignum) bool {
    if (a.size != b.size) return false;
    const n: usize = @intCast(@abs(a.size));
    var i: usize = 0;
    while (i < n) : (i += 1) {
        if (a.limbs[i] != b.limbs[i]) return false;
    }
    return true;
}

fn bignumFitsI64(bn: *const objects.Bignum) ?i64 {
    if (bn.size == 0) return 0;
    const n: usize = @intCast(@abs(bn.size));
    if (n != 1) return null;
    const limb = bn.limbs[0];
    if (limb > @as(u64, @intCast(std.math.maxInt(i64)))) return null;
    const mag: i64 = @intCast(limb);
    return if (bn.isNegative()) -mag else mag;
}

fn floatNormMantExp(f: f64) struct { kind: enum { nan, inf, zero, finite }, sign: bool, mant: u64, exp: i32 } {
    const bits: u64 = @bitCast(f);
    const sign = (bits >> 63) == 1;
    const exp_bits: u16 = @intCast((bits >> 52) & 0x7FF);
    const frac: u64 = bits & 0x000F_FFFF_FFFF_FFFF;

    if (exp_bits == 0x7FF) {
        return if (frac != 0)
            .{ .kind = .nan, .sign = sign, .mant = 0, .exp = 0 }
        else
            .{ .kind = .inf, .sign = sign, .mant = 0, .exp = 0 };
    }

    if (exp_bits == 0 and frac == 0) {
        return .{ .kind = .zero, .sign = false, .mant = 0, .exp = 0 };
    }

    // Value = (-1)^sign * mant * 2^exp, where mant is an integer.
    // For normals: mant = 2^52 + frac, exp = (exp_bits - bias) - 52.
    // For subnormals: mant = frac, exp = (1 - bias) - 52.
    const bias: i32 = 1023;
    var mant: u64 = undefined;
    var exp: i32 = undefined;
    if (exp_bits == 0) {
        mant = frac;
        exp = (1 - bias) - 52;
    } else {
        mant = (1 << 52) | frac;
        exp = (@as(i32, exp_bits) - bias) - 52;
    }

    // Normalize so mant is odd (canonical rational form).
    if (mant != 0) {
        const tz: u6 = @intCast(@ctz(mant));
        mant >>= tz;
        exp += @intCast(tz);
    }

    return .{ .kind = .finite, .sign = sign, .mant = mant, .exp = exp };
}

fn floatEqualsI64(f: f64, n: i64) bool {
    const k = floatNormMantExp(f);
    if (k.kind == .nan or k.kind == .inf) return false;
    if (k.kind == .zero) return n == 0;
    if (k.exp < 0) return false;

    const bits_needed = 64 - @clz(k.mant) + @as(u32, @intCast(k.exp));
    if (bits_needed > 63) return false;

    const mag_u64: u64 = k.mant << @intCast(k.exp);
    const mag_i64: i64 = @intCast(mag_u64);
    const signed = if (k.sign) -mag_i64 else mag_i64;
    return signed == n;
}

fn floatEqualsBignum(f: f64, bn: *const objects.Bignum) bool {
    const k = floatNormMantExp(f);
    if (k.kind == .nan or k.kind == .inf) return false;
    if (k.kind == .zero) return bn.isZero();
    if (k.exp < 0) return false;

    const bn_neg = bn.isNegative();
    if (k.sign != bn_neg) return false;

    // Build limbs for (mant << exp) and compare.
    const limb_shift: usize = @intCast(@divTrunc(k.exp, 64));
    const bit_shift: u6 = @intCast(@rem(k.exp, 64));

    var limbs: [8]u64 = [_]u64{0} ** 8;
    if (limb_shift >= limbs.len) return false;

    if (bit_shift == 0) {
        limbs[limb_shift] = k.mant;
    } else {
        const low: u64 = k.mant << bit_shift;
        const inv_shift: u6 = @intCast(@as(u7, 64) - @as(u7, bit_shift));
        const hi: u64 = k.mant >> inv_shift;
        limbs[limb_shift] = low;
        if (hi != 0) {
            if (limb_shift + 1 >= limbs.len) return false;
            limbs[limb_shift + 1] = hi;
        }
    }

    // Compute effective size (trim trailing zeros).
    var size: usize = limbs.len;
    while (size > 0 and limbs[size - 1] == 0) : (size -= 1) {}

    if (size == 0) return bn.isZero();
    if (size != @as(usize, @intCast(@abs(bn.size)))) return false;
    var i: usize = 0;
    while (i < size) : (i += 1) {
        if (bn.limbs[i] != limbs[i]) return false;
    }
    return true;
}

fn floatEqualsRational(f: f64, rat: *const objects.Rational) bool {
    const k = floatNormMantExp(f);
    if (k.kind == .nan or k.kind == .inf) return false;
    if (k.kind == .zero) return rat.numerator == 0;

    // Compare mant * 2^exp against num/den using i128 arithmetic.
    const num = rat.numerator;
    const den = rat.denominator;
    if (den == 0) return false;

    const sign_match = k.sign == (num < 0);
    const num_abs: i128 = @intCast(@abs(num));
    const den_abs: i128 = @intCast(den);
    const mant_abs: i128 = @intCast(k.mant);

    if (k.exp >= 0) {
        // (mant << exp) * den == num
        const shift: u7 = @intCast(k.exp);
        if (shift >= 127) return false; // num is i64, can't reach
        const lhs = (mant_abs << shift) * den_abs;
        if (lhs < 0) return false;
        const rhs = num_abs;
        return sign_match and lhs == rhs;
    }

    const k_abs: u7 = @intCast(-k.exp);
    if (k_abs >= 127) return false;
    const two_k: i128 = @as(i128, 1) << k_abs;

    // mant * den == num * 2^k
    const lhs = mant_abs * den_abs;
    const rhs = num_abs * two_k;
    return sign_match and lhs == rhs;
}

fn numberEqualp(a: Value, b: Value) bool {
    // Complex handling: compare as complex; complex with 0 imag equals real.
    if (a.typeKind() == .complex or b.typeKind() == .complex) {
        if (a.typeKind() == .complex and b.typeKind() == .complex) {
            const ca = a.toPtr(objects.Complex);
            const cb = b.toPtr(objects.Complex);
            return floatEql(ca.real, cb.real) and floatEql(ca.imag, cb.imag);
        }
        const c = if (a.typeKind() == .complex) a.toPtr(objects.Complex) else b.toPtr(objects.Complex);
        const r = if (a.typeKind() == .complex) b else a;
        if (!floatEql(c.imag, 0.0)) return false;
        // Compare real part (float) to r.
        return switch (r.typeKind()) {
            .float => floatEql(c.real, r.toFloat()),
            .fixnum => floatEqualsI64(c.real, r.toFixnum()),
            .bignum => floatEqualsBignum(c.real, r.toPtr(objects.Bignum)),
            .rational => floatEqualsRational(c.real, r.toPtr(objects.Rational)),
            else => false,
        };
    }

    // Fast path: same raw value.
    if (a.raw == b.raw) return true;

    // Float comparisons (exact, cross-type).
    if (a.isFloat() or b.isFloat()) {
        const fa: f64 = if (a.isFloat())
            a.toFloat()
        else if (a.isFixnum())
            @as(f64, @floatFromInt(a.toFixnum()))
        else
            0.0;
        const fb: f64 = if (b.isFloat())
            b.toFloat()
        else if (b.isFixnum())
            @as(f64, @floatFromInt(b.toFixnum()))
        else
            0.0;

        // float vs float
        if (a.isFloat() and b.isFloat()) return floatEql(fa, fb);

        // float vs integer/rational
        if (a.isFloat()) {
            return switch (b.typeKind()) {
                .fixnum => floatEqualsI64(fa, b.toFixnum()),
                .bignum => floatEqualsBignum(fa, b.toPtr(objects.Bignum)),
                .rational => floatEqualsRational(fa, b.toPtr(objects.Rational)),
                else => false,
            };
        }
        // b is float
        return switch (a.typeKind()) {
            .fixnum => floatEqualsI64(fb, a.toFixnum()),
            .bignum => floatEqualsBignum(fb, a.toPtr(objects.Bignum)),
            .rational => floatEqualsRational(fb, a.toPtr(objects.Rational)),
            else => false,
        };
    }

    // Rational comparisons.
    if (a.typeKind() == .rational or b.typeKind() == .rational) {
        const ra = if (a.typeKind() == .rational) a.toPtr(objects.Rational) else null;
        const rb = if (b.typeKind() == .rational) b.toPtr(objects.Rational) else null;

        if (ra != null and rb != null) {
            return ra.?.numerator == rb.?.numerator and ra.?.denominator == rb.?.denominator;
        }

        const r = ra orelse rb.?;
        const other = if (ra != null) b else a;

        if (r.denominator != 1) return false;
        return switch (other.typeKind()) {
            .fixnum => other.toFixnum() == r.numerator,
            .bignum => if (bignumFitsI64(other.toPtr(objects.Bignum))) |n| n == r.numerator else false,
            else => false,
        };
    }

    // Integer comparisons (fixnum/bignum).
    if ((a.typeKind() == .bignum or a.isFixnum()) and (b.typeKind() == .bignum or b.isFixnum())) {
        // Reuse existing bignum-aware compare in arith.
        const arith = @import("arith.zig");
        return arith.numEq(a, b);
    }

    return false;
}

const MAX_EQUAL_DEPTH = 1000;

fn valueEql(a: Value, b: Value) bool {
    if (a.raw == b.raw) return true;
    if (a.typeKind() != b.typeKind()) return false;

    return switch (a.typeKind()) {
        .float => floatEql(a.toFloat(), b.toFloat()),
        .char => a.toCharacter() == b.toCharacter(),
        .bignum => bignumEq(a.toPtr(objects.Bignum), b.toPtr(objects.Bignum)),
        .rational => blk: {
            const ra = a.toPtr(objects.Rational);
            const rb = b.toPtr(objects.Rational);
            break :blk ra.numerator == rb.numerator and ra.denominator == rb.denominator;
        },
        .complex => blk: {
            const ca = a.toPtr(objects.Complex);
            const cb = b.toPtr(objects.Complex);
            break :blk floatEql(ca.real, cb.real) and floatEql(ca.imag, cb.imag);
        },
        else => false,
    };
}

/// Structural equality (equal in Lisp)
pub fn valueEqual(a: Value, b: Value) bool {
    return valueEqualWithDepth(a, b, 0);
}

fn valueEqualWithDepth(a: Value, b: Value, depth: usize) bool {
    if (depth > MAX_EQUAL_DEPTH) return false;

    // Fast path: identical values.
    if (a.raw == b.raw) return true;

    // Numbers and characters use eql semantics.
    if (a.isNumber() or b.isNumber() or a.isCharacter() or b.isCharacter()) {
        return valueEql(a, b);
    }

    const ta = a.typeKind();
    const tb = b.typeKind();

    if (isStringLike(a) and isStringLike(b)) {
        return stringLikeEqual(a, b);
    }

    // ANSI compatibility: a zero-length rank-1 array compares equal to an empty string.
    if ((ta == .array and isStringLike(b)) or (isStringLike(a) and tb == .array)) {
        const arr = if (ta == .array) a.toPtr(objects.Array) else b.toPtr(objects.Array);
        const str = if (ta == .array) b else a;
        return arr.rank == 1 and arr.total_size == 0 and stringLikeLen(str) == 0;
    }

    if (ta != tb) return false;

    return switch (ta) {
        .cons => blk: {
            const ca = a.toPtr(objects.Cons);
            const cb = b.toPtr(objects.Cons);
            break :blk valueEqualWithDepth(ca.car, cb.car, depth + 1) and valueEqualWithDepth(ca.cdr, cb.cdr, depth + 1);
        },
        .string, .string32 => stringLikeEqual(a, b),
        .vector => blk: {
            const va = a.toPtr(objects.Vector);
            const vb = b.toPtr(objects.Vector);
            if (va.length != vb.length) break :blk false;
            for (va.items(), vb.items()) |ea, eb| {
                if (!valueEqualWithDepth(ea, eb, depth + 1)) break :blk false;
            }
            break :blk true;
        },
        .array => blk: {
            const aa = a.toPtr(objects.Array);
            const ab = b.toPtr(objects.Array);
            if (aa.rank != ab.rank) break :blk false;
            if (aa.total_size != ab.total_size) break :blk false;

            const rank: usize = @intCast(aa.rank);
            for (0..rank) |i| {
                if (aa.dimensions[i] != ab.dimensions[i]) break :blk false;
            }

            const size: usize = @intCast(aa.total_size);
            const data_a: [*]Value = @ptrFromInt(aa.data_ptr);
            const data_b: [*]Value = @ptrFromInt(ab.data_ptr);
            for (0..size) |i| {
                if (!valueEqualWithDepth(data_a[i], data_b[i], depth + 1)) break :blk false;
            }
            break :blk true;
        },
        else => false,
    };
}

const MAX_EQUALP_DEPTH = 1000;

/// Extended equality (equalp in Lisp)
pub fn valueEqualp(a: Value, b: Value) bool {
    return valueEqualpWithDepth(a, b, 0);
}

fn valueEqualpWithDepth(a: Value, b: Value, depth: usize) bool {
    if (depth > MAX_EQUALP_DEPTH) return false;
    if (a.raw == b.raw) return true;

    // Numeric coercions.
    if (a.isNumber() and b.isNumber()) return numberEqualp(a, b);

    // Characters: case-insensitive.
    if (a.isCharacter() and b.isCharacter()) {
        const ca: u32 = @intCast(a.toCharacter());
        const cb: u32 = @intCast(b.toCharacter());
        return upperAsciiCp(ca) == upperAsciiCp(cb);
    }

    const ta = a.typeKind();
    const tb = b.typeKind();
    if (isStringLike(a) and isStringLike(b)) {
        return stringLikeEqualp(a, b);
    }

    if (ta != tb) return false;

    return switch (ta) {
        .cons => blk: {
            const ca = a.toPtr(objects.Cons);
            const cb = b.toPtr(objects.Cons);
            break :blk valueEqualpWithDepth(ca.car, cb.car, depth + 1) and valueEqualpWithDepth(ca.cdr, cb.cdr, depth + 1);
        },
        .vector => blk: {
            const va = a.toPtr(objects.Vector);
            const vb = b.toPtr(objects.Vector);
            if (va.length != vb.length) break :blk false;
            for (va.items(), vb.items()) |ea, eb| {
                if (!valueEqualpWithDepth(ea, eb, depth + 1)) break :blk false;
            }
            break :blk true;
        },
        .array => blk: {
            const aa = a.toPtr(objects.Array);
            const ab = b.toPtr(objects.Array);
            if (aa.rank != ab.rank) break :blk false;
            if (aa.total_size != ab.total_size) break :blk false;
            var i: usize = 0;
            while (i < aa.rank) : (i += 1) {
                if (aa.dimensions[i] != ab.dimensions[i]) break :blk false;
            }
            const data_a: [*]Value = @ptrFromInt(aa.data_ptr);
            const data_b: [*]Value = @ptrFromInt(ab.data_ptr);
            const n: usize = @intCast(aa.total_size);
            for (0..n) |j| {
                if (!valueEqualpWithDepth(data_a[j], data_b[j], depth + 1)) break :blk false;
            }
            break :blk true;
        },
        else => false,
    };
}

fn hashMix(h: u64, x: u64) u64 {
    return h *% 31 +% x;
}

fn hashIntCanonical(neg: bool, limbs: []const u64) u64 {
    var h: u64 = 0x6E6F_6E69_746E_6921; // "nonint!"-ish tag
    h = hashMix(h, if (neg) 1 else 0);
    h = hashMix(h, @intCast(limbs.len));
    for (limbs) |limb| h = hashMix(h, limb);
    return h;
}

fn hashI64Canonical(n: i64) u64 {
    if (n == 0) return hashIntCanonical(false, &.{});
    const neg = n < 0;
    const mag: u64 = @intCast(@abs(n));
    return hashIntCanonical(neg, &.{mag});
}

fn hashBignumCanonical(bn: *const objects.Bignum) u64 {
    const neg = bn.isNegative();
    const n: usize = @intCast(@abs(bn.size));
    return if (n == 0) hashIntCanonical(false, &.{}) else hashIntCanonical(neg, bn.limbs[0..n]);
}

fn hashRatCanonical(num: i64, den: u64) u64 {
    // den is always positive.
    if (den == 1) return hashI64Canonical(num);
    var h: u64 = 0x7261_7421_0000_0000; // "rat!"
    h = hashMix(h, @bitCast(@as(i64, num)));
    h = hashMix(h, den);
    return h;
}

fn hashFloatCanonical(f: f64) u64 {
    const k = floatNormMantExp(f);
    switch (k.kind) {
        .nan => return 0x7FF8_0000_0000_0000,
        .inf => return if (k.sign) 0xFFF0_0000_0000_0000 else 0x7FF0_0000_0000_0000,
        .zero => return hashI64Canonical(0),
        .finite => {},
    }

    if (k.exp >= 0) {
        const bits_needed = 64 - @clz(k.mant) + @as(u32, @intCast(k.exp));
        if (bits_needed <= 63) {
            const mag: i64 = @intCast(k.mant << @intCast(k.exp));
            return hashI64Canonical(if (k.sign) -mag else mag);
        }
        if (bits_needed <= 512) {
            // Build limbs for mant << exp and hash them.
            const limb_shift: usize = @intCast(@divTrunc(k.exp, 64));
            const bit_shift: u6 = @intCast(@rem(k.exp, 64));

            var limbs: [8]u64 = [_]u64{0} ** 8;
            if (limb_shift >= limbs.len) return normalizeFloatForHash(f);
            if (bit_shift == 0) {
                limbs[limb_shift] = k.mant;
            } else {
                limbs[limb_shift] = k.mant << bit_shift;
                const inv_shift: u6 = @intCast(@as(u7, 64) - @as(u7, bit_shift));
                const hi: u64 = k.mant >> inv_shift;
                if (hi != 0) {
                    if (limb_shift + 1 >= limbs.len) return normalizeFloatForHash(f);
                    limbs[limb_shift + 1] = hi;
                }
            }
            var size: usize = limbs.len;
            while (size > 0 and limbs[size - 1] == 0) : (size -= 1) {}
            return hashIntCanonical(k.sign, limbs[0..size]);
        }
        return normalizeFloatForHash(f);
    }

    const k_abs: u7 = @intCast(-k.exp);
    if (k_abs <= 62) {
        const den: u64 = @as(u64, 1) << @as(u6, @intCast(k_abs));
        const num: i64 = if (k.sign) -@as(i64, @intCast(k.mant)) else @as(i64, @intCast(k.mant));
        return hashRatCanonical(num, den);
    }

    return normalizeFloatForHash(f);
}

fn hashNumberEqualp(val: Value) u64 {
    return switch (val.typeKind()) {
        .fixnum => hashI64Canonical(val.toFixnum()),
        .bignum => hashBignumCanonical(val.toPtr(objects.Bignum)),
        .rational => blk: {
            const r = val.toPtr(objects.Rational);
            break :blk hashRatCanonical(r.numerator, @intCast(r.denominator));
        },
        .float => hashFloatCanonical(val.toFloat()),
        .complex => blk: {
            const c = val.toPtr(objects.Complex);
            if (floatEql(c.imag, 0.0)) break :blk hashFloatCanonical(c.real);
            var h: u64 = 0x6370_6C78_0000_0000; // "cplx"
            h = hashMix(h, hashFloatCanonical(c.real));
            h = hashMix(h, hashFloatCanonical(c.imag));
            break :blk h;
        },
        else => hashValue(val),
    };
}

fn hashValueEqualp(val: Value, depth: usize) u64 {
    if (depth > MAX_EQUALP_DEPTH) return 0;

    return switch (val.typeKind()) {
        .nil => 0,
        .t => 1,
        .unbound => 2,

        .fixnum, .float, .rational, .complex, .bignum => hashNumberEqualp(val),

        .char => blk: {
            const cp: u32 = @intCast(val.toCharacter());
            break :blk upperAsciiCp(cp);
        },

        .symbol => blk: {
            const s = val.toPtr(objects.Symbol);
            var h: u64 = 0;
            for (s.getName()) |b| h = hashMix(h, b);
            break :blk h;
        },
        .keyword => blk: {
            // Keywords are interned; hash by name for GC stability.
            const k = val.toPtr(objects.Keyword);
            var h: u64 = 0;
            for (k.getName()) |b| h = hashMix(h, b);
            break :blk h;
        },

        .string, .string32 => hashStringLikeEqualp(val),

        .cons => blk: {
            const c = val.toPtr(objects.Cons);
            const h1 = hashValueEqualp(c.car, depth + 1);
            const h2 = hashValueEqualp(c.cdr, depth + 1);
            break :blk hashMix(h1, h2);
        },
        .vector => blk: {
            const v = val.toPtr(objects.Vector);
            if (v.isCharacterVector()) break :blk hashStringLikeEqualp(val);
            var h: u64 = 0;
            for (v.items()) |item| h = hashMix(h, hashValueEqualp(item, depth + 1));
            break :blk h;
        },
        .array => blk: {
            const a = val.toPtr(objects.Array);
            var h: u64 = 0x6172_7279_0000_0000; // "arry"
            h = hashMix(h, a.rank);
            var i: usize = 0;
            while (i < a.rank) : (i += 1) h = hashMix(h, a.dimensions[i]);
            const data: [*]Value = @ptrFromInt(a.data_ptr);
            const n: usize = @intCast(a.total_size);
            for (0..n) |j| h = hashMix(h, hashValueEqualp(data[j], depth + 1));
            break :blk h;
        },
        .structure => val.raw,

        else => hashValue(val),
    };
}

pub fn hashValueWithTest(val: Value, test_type: HashTest) u64 {
    return switch (test_type) {
        .equalp => hashValueEqualp(val, 0),
        .eq, .eql, .equal => hashValue(val),
    };
}

pub fn keyEqualWithTest(a: Value, b: Value, test_type: HashTest) bool {
    return switch (test_type) {
        .eq => a.raw == b.raw,
        .eql => valueEql(a, b),
        .equal => valueEqual(a, b),
        .equalp => valueEqualp(a, b),
    };
}

pub fn hashValue(val: Value) u64 {
    return switch (val.typeKind()) {
        .nil => 0,
        .t => 1,
        .unbound => 2,
        .fixnum => @bitCast(@as(i64, val.toFixnum())),
        .float => normalizeFloatForHash(val.toFloat()),
        .char => @intCast(val.toCharacter()),
        .cons => blk: {
            const c = val.toPtr(objects.Cons);
            const h1 = hashValue(c.car);
            const h2 = hashValue(c.cdr);
            break :blk h1 *% 31 +% h2;
        },
        .symbol => blk: {
            const s = val.toPtr(objects.Symbol);
            var h: u64 = 0;
            for (s.getName()) |b| {
                h = h *% 31 +% b;
            }
            break :blk h;
        },
        .keyword => val.toPtr(objects.Keyword).hash,
        .string, .string32 => hashStringLike(val),
        .vector => blk: {
            const v = val.toPtr(objects.Vector);
            if (v.isCharacterVector()) break :blk hashStringLike(val);
            var h: u64 = 0;
            for (v.items()) |item| {
                h = h *% 31 +% hashValue(item);
            }
            break :blk h;
        },
        .rational => blk: {
            const r = val.toPtr(objects.Rational);
            var h: u64 = 0;
            h = h *% 31 +% @as(u64, @bitCast(r.numerator));
            h = h *% 31 +% @as(u64, @bitCast(r.denominator));
            break :blk h;
        },
        .complex => blk: {
            const c = val.toPtr(objects.Complex);
            var h: u64 = 0;
            h = h *% 31 +% normalizeFloatForHash(c.real);
            h = h *% 31 +% normalizeFloatForHash(c.imag);
            break :blk h;
        },
        .bignum => blk: {
            const bn = val.toPtr(objects.Bignum);
            var h: u64 = 0;
            h = h *% 31 +% @as(u64, @bitCast(bn.size));
            const n: usize = @intCast(@abs(bn.size));
            for (bn.limbs[0..n]) |limb| {
                h = h *% 31 +% limb;
            }
            break :blk h;
        },
        .closure, .hashtable, .stream, .array, .pathname, .package, .readtable, .chunk, .condition, .class, .slotdef, .generic_function, .method, .native_code, .structure, .macro_env => val.raw,
    };
}

test "equal treats base strings and character vectors identically" {
    const heap_mod = @import("../heap.zig");
    var heap = try heap_mod.Heap.init(std.testing.allocator);
    defer heap.deinit();

    const s = try heap.allocBaseString("infixie");
    const v = try heap.allocVector(7);
    const vec = v.toPtr(objects.Vector);
    for ("infixie", 0..) |ch, i| {
        vec.data[i] = Value.makeCharacter(ch);
    }

    try std.testing.expect(valueEqual(s, v));
    try std.testing.expect(valueEqual(v, s));
    try std.testing.expectEqual(hashValue(s), hashValue(v));
}

test "equal hash tables match base strings against character vectors" {
    const heap_mod = @import("../heap.zig");
    var heap = try heap_mod.Heap.init(std.testing.allocator);
    defer heap.deinit();

    const ht_val = try heap.allocHashTable(8, .equal);
    const ht = ht_val.toPtr(objects.HashTable);
    const s = try heap.allocBaseString("infixie");
    const v = try heap.allocVector(7);
    const vec = v.toPtr(objects.Vector);
    for ("infixie", 0..) |ch, i| {
        vec.data[i] = Value.makeCharacter(ch);
    }

    try ht.put(v, Value.t);
    try std.testing.expectEqual(Value.t.raw, (ht.get(s) orelse Value.nil).raw);
}
