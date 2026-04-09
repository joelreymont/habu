//! Complex number primitives

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const arith = @import("arith.zig");
const Complex = @import("../objects.zig").Complex;

pub const Error = arith.Error;

fn isRealNumber(v: Value) bool {
    return switch (v.typeKind()) {
        .fixnum, .float, .rational, .bignum => true,
        else => false,
    };
}

/// Create a complex number
pub fn makeComplex(heap: *Heap, real: Value, imag: Value) error{OutOfMemory, TypeMismatch}!Value {
    if (!isRealNumber(real) or !isRealNumber(imag)) return error.TypeMismatch;
    return heap.allocComplex(real, imag);
}

/// Get real part of a complex number
pub fn realpart(val: Value) Error!Value {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    return cplx.real;
}

/// Get imaginary part of a complex number
pub fn imagpart(val: Value) Error!Value {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    return cplx.imag;
}

/// Check if value is complex
pub fn isComplex(val: Value) bool {
    return val.typeKind() == .complex;
}

/// Complex addition
pub fn complexAdd(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    return heap.allocComplex(
        try arith.add(heap, ca.real, cb.real),
        try arith.add(heap, ca.imag, cb.imag),
    );
}

/// Complex subtraction
pub fn complexSub(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    return heap.allocComplex(
        try arith.sub(heap, ca.real, cb.real),
        try arith.sub(heap, ca.imag, cb.imag),
    );
}

/// Complex multiplication
pub fn complexMul(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    const ac = try arith.mul(heap, ca.real, cb.real);
    const bd = try arith.mul(heap, ca.imag, cb.imag);
    const ad = try arith.mul(heap, ca.real, cb.imag);
    const bc = try arith.mul(heap, ca.imag, cb.real);
    return heap.allocComplex(
        try arith.sub(heap, ac, bd),
        try arith.add(heap, ad, bc),
    );
}

/// Complex division
pub fn complexDiv(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    const cc = try arith.mul(heap, cb.real, cb.real);
    const dd = try arith.mul(heap, cb.imag, cb.imag);
    const denom = try arith.add(heap, cc, dd);
    if (arith.numEq(denom, Value.makeFixnum(0))) return error.TypeMismatch;
    const ac = try arith.mul(heap, ca.real, cb.real);
    const bd = try arith.mul(heap, ca.imag, cb.imag);
    const bc = try arith.mul(heap, ca.imag, cb.real);
    const ad = try arith.mul(heap, ca.real, cb.imag);
    return heap.allocComplex(
        try arith.div(heap, try arith.add(heap, ac, bd), denom),
        try arith.div(heap, try arith.sub(heap, bc, ad), denom),
    );
}

/// Complex absolute value (magnitude)
pub fn complexAbs(val: Value) Error!f64 {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    const r = try arith.toNumber(cplx.real);
    const i = try arith.toNumber(cplx.imag);
    return @sqrt(r * r + i * i);
}

/// Complex conjugate
pub fn conjugate(heap: *Heap, val: Value) Error!Value {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    return heap.allocComplex(cplx.real, try arith.sub(heap, Value.makeFixnum(0), cplx.imag));
}

test "complex creation" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const c = try makeComplex(&heap, Value.makeFixnum(3), Value.makeFixnum(4));
    try testing.expect(isComplex(c));
    try testing.expectEqual(@as(i64, 3), (try realpart(c)).toFixnum());
    try testing.expectEqual(@as(i64, 4), (try imagpart(c)).toFixnum());
}

test "complex magnitude" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    // 3 + 4i has magnitude 5
    const c = try makeComplex(&heap, Value.makeFixnum(3), Value.makeFixnum(4));
    try testing.expectApproxEqAbs(@as(f64, 5.0), try complexAbs(c), 0.0001);
}
