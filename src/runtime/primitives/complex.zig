//! Complex number primitives

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const Complex = @import("../objects.zig").Complex;

pub const Error = error{ TypeMismatch, OutOfMemory };

/// Create a complex number
pub fn makeComplex(heap: *Heap, real: f64, imag: f64) Error!Value {
    return heap.allocComplex(real, imag);
}

/// Get real part of a complex number
pub fn realpart(val: Value) Error!f64 {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    return cplx.real;
}

/// Get imaginary part of a complex number
pub fn imagpart(val: Value) Error!f64 {
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
    const result = Complex.add(ca.*, cb.*);
    return heap.allocComplex(result.real, result.imag);
}

/// Complex subtraction
pub fn complexSub(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    const result = Complex.sub(ca.*, cb.*);
    return heap.allocComplex(result.real, result.imag);
}

/// Complex multiplication
pub fn complexMul(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    const result = Complex.mul(ca.*, cb.*);
    return heap.allocComplex(result.real, result.imag);
}

/// Complex division
pub fn complexDiv(heap: *Heap, a: Value, b: Value) Error!Value {
    if (a.typeKind() != .complex or b.typeKind() != .complex) return error.TypeMismatch;
    const ca = a.toPtr(Complex);
    const cb = b.toPtr(Complex);
    const result = Complex.div(ca.*, cb.*);
    return heap.allocComplex(result.real, result.imag);
}

/// Complex absolute value (magnitude)
pub fn complexAbs(val: Value) Error!f64 {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    return cplx.abs();
}

/// Complex conjugate
pub fn conjugate(heap: *Heap, val: Value) Error!Value {
    if (val.typeKind() != .complex) return error.TypeMismatch;
    const cplx = val.toPtr(Complex);
    const result = cplx.conjugate();
    return heap.allocComplex(result.real, result.imag);
}

test "complex creation" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const c = try makeComplex(&heap, 3.0, 4.0);
    try testing.expect(isComplex(c));
    try testing.expectApproxEqAbs(@as(f64, 3.0), try realpart(c), 0.0001);
    try testing.expectApproxEqAbs(@as(f64, 4.0), try imagpart(c), 0.0001);
}

test "complex magnitude" {
    const testing = std.testing;
    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    // 3 + 4i has magnitude 5
    const c = try makeComplex(&heap, 3.0, 4.0);
    try testing.expectApproxEqAbs(@as(f64, 5.0), try complexAbs(c), 0.0001);
}
