//! Vector primitives
//!
//! make-vector, vector-ref, vector-set, vector-length, vector-push, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Tag = @import("../value.zig").Tag;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

/// Create a new vector
pub fn makeVector(heap: *Heap, length: usize) error{OutOfMemory}!Value {
    return try heap.allocVector(length, length);
}

/// Create a vector with specified capacity
pub fn makeVectorWithCapacity(heap: *Heap, length: usize, capacity: usize) error{OutOfMemory}!Value {
    return try heap.allocVector(length, capacity);
}

/// Create a vector initialized with a fill value
pub fn makeVectorFill(heap: *Heap, length: usize, fill: Value) error{OutOfMemory}!Value {
    const vec = try makeVector(heap, length);
    const vec_obj = vec.toPtr(objects.Vector);

    for (0..length) |i| {
        vec_obj.data[i] = fill;
    }

    return vec;
}

/// Create a multi-dimensional array
/// For 1D: makeArray(heap, &[_]u64{10})
/// For 2D: makeArray(heap, &[_]u64{3, 4})
pub fn makeArray(heap: *Heap, dimensions: []const u64) error{OutOfMemory}!Value {
    return try heap.allocArray(dimensions);
}

/// Create an array with initial fill value
pub fn makeArrayFill(heap: *Heap, dimensions: []const u64, fill: Value) error{OutOfMemory}!Value {
    const arr = try makeArray(heap, dimensions);
    const arr_obj = arr.toPtr(objects.Array);
    const data: [*]Value = @ptrFromInt(arr_obj.data_ptr);

    for (0..arr_obj.total_size) |i| {
        data[i] = fill;
    }

    return arr;
}

/// Get array rank (number of dimensions)
pub fn arrayRank(val: Value) i64 {
    if (!val.isArray()) return -1;
    const arr = val.toPtr(objects.Array);
    return arr.rank;
}

/// Get array dimensions as a list of fixnums
pub fn arrayDimensions(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    if (!val.isArray()) return Value.nil;
    const arr = val.toPtr(objects.Array);

    // Build list in reverse
    var result = Value.nil;
    var i: usize = arr.rank;
    while (i > 0) {
        i -= 1;
        const dim = Value.makeFixnum(@intCast(arr.dimensions[i]));
        result = try heap.allocCons(dim, result);
    }

    return result;
}

/// Get total array size
pub fn arrayTotalSize(val: Value) i64 {
    if (!val.isArray()) return -1;
    const arr = val.toPtr(objects.Array);
    return @intCast(arr.total_size);
}

/// Calculate row-major index from subscripts
/// For a 3x4 array: subscripts [i,j] -> index i*4 + j
fn calculateRowMajorIndex(arr: *const objects.Array, subscripts: []const u64) ?usize {
    if (subscripts.len != arr.rank) return null;

    var index: usize = 0;
    var multiplier: usize = 1;

    // Process dimensions from right to left (row-major order)
    var i: usize = arr.rank;
    while (i > 0) {
        i -= 1;
        const sub = subscripts[i];
        const dim = arr.dimensions[i];

        // Bounds check
        if (sub >= dim) return null;

        const offset = std.math.mul(usize, @as(usize, @intCast(sub)), multiplier) catch return null;
        index = std.math.add(usize, index, offset) catch return null;
        multiplier = std.math.mul(usize, multiplier, @intCast(dim)) catch return null;
    }

    return index;
}

/// Get array element using row-major indexing
/// subscripts must match array rank
pub fn arrayRef(val: Value, subscripts: []const u64) ?Value {
    if (!val.isArray()) return null;
    const arr = val.toPtr(objects.Array);

    const index = calculateRowMajorIndex(arr, subscripts) orelse return null;
    const data: [*]Value = @ptrFromInt(arr.data_ptr);

    return data[index];
}

/// Set array element using row-major indexing
pub fn arraySet(val: Value, subscripts: []const u64, new_val: Value) bool {
    if (!val.isArray()) return false;
    const arr = val.toPtr(objects.Array);

    const index = calculateRowMajorIndex(arr, subscripts) orelse return false;
    const data: [*]Value = @ptrFromInt(arr.data_ptr);

    data[index] = new_val;
    return true;
}

/// Check if value is an array
pub fn arrayp(val: Value) bool {
    return val.isArray();
}

/// Get vector length
pub fn vectorLength(val: Value) i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);
    return @intCast(vec.length);
}

/// Get element at index
pub fn vectorRef(val: Value, index: usize) Value {
    if (!val.isVector()) return Value.nil;
    const vec = val.toPtr(objects.Vector);
    if (index >= vec.length) return Value.nil;
    return vec.data[index];
}

/// Set element at index
pub fn vectorSet(val: Value, index: usize, new_val: Value) bool {
    if (!val.isVector()) return false;
    const vec = val.toPtr(objects.Vector);
    if (index >= vec.length) return false;
    vec.data[index] = new_val;
    return true;
}

/// Check if value is a vector
pub fn vectorp(val: Value) bool {
    return val.isVector();
}

/// Get vector as slice
pub fn vectorSlice(val: Value) ?[]Value {
    if (!val.isVector()) return null;
    const vec = val.toPtr(objects.Vector);
    return vec.items();
}

/// Push element to vector (if capacity allows)
/// Returns new length, or -1 if failed
pub fn vectorPush(val: Value, element: Value) i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);
    if (vec.length >= vec.capacity) return -1;

    vec.data[vec.length] = element;
    vec.length += 1;
    return @intCast(vec.length);
}

/// Pop element from vector
/// Returns the popped element, or nil if empty
pub fn vectorPop(val: Value) Value {
    if (!val.isVector()) return Value.nil;
    const vec = val.toPtr(objects.Vector);
    if (vec.length == 0) return Value.nil;

    vec.length -= 1;
    return vec.data[vec.length];
}

/// Fill vector with a value
pub fn vectorFill(val: Value, fill: Value) bool {
    if (!val.isVector()) return false;
    const vec = val.toPtr(objects.Vector);

    for (0..vec.length) |i| {
        vec.data[i] = fill;
    }

    return true;
}

/// Copy vector
pub fn vectorCopy(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    if (!val.isVector()) return Value.nil;
    const src = val.toPtr(objects.Vector);

    const new_vec = try makeVector(heap, src.length);
    const dst = new_vec.toPtr(objects.Vector);

    @memcpy(dst.data[0..src.length], src.data[0..src.length]);

    return new_vec;
}

/// Create vector from list
pub fn listToVector(heap: *Heap, list_val: Value) error{OutOfMemory}!Value {
    const list_prim = @import("list.zig");

    // Count elements
    const len = list_prim.length(list_val);
    if (len < 0) return Value.nil;

    const vec = try makeVector(heap, @intCast(len));
    const vec_obj = vec.toPtr(objects.Vector);

    // Copy elements
    var current = list_val;
    var i: usize = 0;
    while (!current.isNil() and current.isCons()) {
        vec_obj.data[i] = list_prim.car(current);
        current = list_prim.cdr(current);
        i += 1;
    }

    return vec;
}

/// Convert vector to list
pub fn vectorToList(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    if (!val.isVector()) return Value.nil;
    const vec = val.toPtr(objects.Vector);

    const list_prim = @import("list.zig");
    var result = Value.nil;

    var i = vec.length;
    while (i > 0) {
        i -= 1;
        result = try list_prim.cons(heap, vec.data[i], result);
    }

    return result;
}

/// Vector equality (element-wise)
pub fn vectorEqual(a: Value, b: Value) bool {
    if (!a.isVector() or !b.isVector()) return false;

    const vec_a = a.toPtr(objects.Vector);
    const vec_b = b.toPtr(objects.Vector);

    if (vec_a.length != vec_b.length) return false;

    for (0..vec_a.length) |i| {
        // Simple eq comparison (identity)
        if (vec_a.data[i].raw != vec_b.data[i].raw) return false;
    }

    return true;
}

/// Find element in vector
/// Returns index or -1 if not found
pub fn vectorFind(val: Value, element: Value) i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);

    for (0..vec.length) |i| {
        if (vec.data[i].raw == element.raw) {
            return @intCast(i);
        }
    }

    return -1;
}

/// Reverse vector in place
pub fn vectorReverse(val: Value) bool {
    if (!val.isVector()) return false;
    const vec = val.toPtr(objects.Vector);

    var i: usize = 0;
    var j: usize = vec.length;
    while (i < j) {
        j -= 1;
        const tmp = vec.data[i];
        vec.data[i] = vec.data[j];
        vec.data[j] = tmp;
        i += 1;
    }

    return true;
}

// ============================================================================
// Tests
// ============================================================================

test "make vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 5);

    try testing.expect(vectorp(vec));
    try testing.expectEqual(@as(i64, 5), vectorLength(vec));

    // All elements should be nil
    for (0..5) |i| {
        try testing.expect(vectorRef(vec, i).isNil());
    }
}

test "vector ref and set" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);

    try testing.expect(vectorSet(vec, 0, Value.makeFixnum(10)));
    try testing.expect(vectorSet(vec, 1, Value.makeFixnum(20)));
    try testing.expect(vectorSet(vec, 2, Value.makeFixnum(30)));

    try testing.expectEqual(@as(i64, 10), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(vec, 2).toFixnum());

    // Out of bounds
    try testing.expect(vectorRef(vec, 3).isNil());
    try testing.expect(!vectorSet(vec, 3, Value.makeFixnum(40)));
}

test "vector with fill" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVectorFill(&heap, 5, Value.makeFixnum(42));

    for (0..5) |i| {
        try testing.expectEqual(@as(i64, 42), vectorRef(vec, i).toFixnum());
    }
}

test "vector push and pop" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVectorWithCapacity(&heap, 0, 5);

    try testing.expectEqual(@as(i64, 1), vectorPush(vec, Value.makeFixnum(10)));
    try testing.expectEqual(@as(i64, 2), vectorPush(vec, Value.makeFixnum(20)));
    try testing.expectEqual(@as(i64, 3), vectorPush(vec, Value.makeFixnum(30)));

    try testing.expectEqual(@as(i64, 3), vectorLength(vec));

    try testing.expectEqual(@as(i64, 30), vectorPop(vec).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorPop(vec).toFixnum());
    try testing.expectEqual(@as(i64, 1), vectorLength(vec)); // 3 - 2 = 1
}

test "vector copy" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));

    const copy = try vectorCopy(&heap, vec);

    // Values should be equal
    try testing.expect(vectorEqual(vec, copy));

    // But different memory
    _ = vectorSet(copy, 0, Value.makeFixnum(100));
    try testing.expect(!vectorEqual(vec, copy));
}

test "list to vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const list_prim = @import("list.zig");
    const lst = try list_prim.list(&heap, &[_]Value{
        Value.makeFixnum(10),
        Value.makeFixnum(20),
        Value.makeFixnum(30),
    });

    const vec = try listToVector(&heap, lst);

    try testing.expectEqual(@as(i64, 3), vectorLength(vec));
    try testing.expectEqual(@as(i64, 10), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(vec, 2).toFixnum());
}

test "vector to list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));

    const lst = try vectorToList(&heap, vec);

    const list_prim = @import("list.zig");
    try testing.expectEqual(@as(i64, 3), list_prim.length(lst));
    try testing.expectEqual(@as(i64, 1), list_prim.nth(lst, 0).toFixnum());
    try testing.expectEqual(@as(i64, 2), list_prim.nth(lst, 1).toFixnum());
    try testing.expectEqual(@as(i64, 3), list_prim.nth(lst, 2).toFixnum());
}

test "vector reverse" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 4);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));
    _ = vectorSet(vec, 3, Value.makeFixnum(4));

    try testing.expect(vectorReverse(vec));

    try testing.expectEqual(@as(i64, 4), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 3), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 2), vectorRef(vec, 2).toFixnum());
    try testing.expectEqual(@as(i64, 1), vectorRef(vec, 3).toFixnum());
}

test "vector find" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);
    _ = vectorSet(vec, 0, Value.makeFixnum(10));
    _ = vectorSet(vec, 1, Value.makeFixnum(20));
    _ = vectorSet(vec, 2, Value.makeFixnum(30));

    try testing.expectEqual(@as(i64, 0), vectorFind(vec, Value.makeFixnum(10)));
    try testing.expectEqual(@as(i64, 1), vectorFind(vec, Value.makeFixnum(20)));
    try testing.expectEqual(@as(i64, 2), vectorFind(vec, Value.makeFixnum(30)));
    try testing.expectEqual(@as(i64, -1), vectorFind(vec, Value.makeFixnum(40)));
}
