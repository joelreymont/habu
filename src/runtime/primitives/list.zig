//! List primitives
//!
//! cons, car, cdr, null, consp, length, list operations

const std = @import("std");
const Value = @import("../value.zig").Value;
const Tag = @import("../value.zig").Tag;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

/// Create a cons cell
pub fn cons(heap: *Heap, car_val: Value, cdr_val: Value) error{OutOfMemory}!Value {
    return try heap.allocCons(car_val, cdr_val);
}

/// Get the car of a cons cell
/// Returns nil if not a cons
pub fn car(val: Value) Value {
    if (!val.isCons()) return Value.nil;
    const c = val.toPtr(objects.Cons);
    return c.car;
}

/// Get the cdr of a cons cell
/// Returns nil if not a cons
pub fn cdr(val: Value) Value {
    if (!val.isCons()) return Value.nil;
    const c = val.toPtr(objects.Cons);
    return c.cdr;
}

/// Check if value is nil
pub fn isNull(val: Value) bool {
    return val.isNil();
}

/// Check if value is a cons cell
pub fn consp(val: Value) bool {
    return val.isCons();
}

/// Get list length
/// Returns -1 for non-list (improper list or not a list)
pub fn length(val: Value) i64 {
    var current = val;
    var len: i64 = 0;

    while (!current.isNil()) {
        if (!current.isCons()) return -1;
        const c = current.toPtr(objects.Cons);
        current = c.cdr;
        len += 1;
    }

    return len;
}

/// Set the car of a cons cell
pub fn setCar(val: Value, new_car: Value) void {
    if (!val.isCons()) return;
    const c = val.toPtr(objects.Cons);
    c.car = new_car;
}

/// Set the cdr of a cons cell
pub fn setCdr(val: Value, new_cdr: Value) void {
    if (!val.isCons()) return;
    const c = val.toPtr(objects.Cons);
    c.cdr = new_cdr;
}

/// Get nth element of a list (0-indexed)
pub fn nth(val: Value, n: usize) Value {
    var current = val;
    var i: usize = 0;

    while (!current.isNil()) {
        if (!current.isCons()) return Value.nil;
        if (i == n) {
            return car(current);
        }
        current = cdr(current);
        i += 1;
    }

    return Value.nil;
}

/// Get nth cdr of a list (0-indexed)
pub fn nthcdr(val: Value, n: usize) Value {
    var current = val;
    var i: usize = 0;

    while (i < n) {
        if (current.isNil() or !current.isCons()) return Value.nil;
        current = cdr(current);
        i += 1;
    }

    return current;
}

/// Create a list from a slice of values
pub fn list(heap: *Heap, values: []const Value) error{OutOfMemory}!Value {
    var result = Value.nil;
    var i = values.len;

    while (i > 0) {
        i -= 1;
        result = try cons(heap, values[i], result);
    }

    return result;
}

/// Append two lists (iterative, O(n) time/space, no stack overflow)
pub fn append(heap: *Heap, list1: Value, list2: Value) error{OutOfMemory}!Value {
    if (list1.isNil()) return list2;
    if (!list1.isCons()) return list2;

    // Build reversed copy of list1
    var reversed = Value.nil;
    var curr = list1;
    while (curr.isCons()) {
        const c = curr.toPtr(objects.Cons);
        reversed = try cons(heap, c.car, reversed);
        curr = c.cdr;
    }

    // Cons reversed elements onto list2
    var result = list2;
    while (reversed.isCons()) {
        const c = reversed.toPtr(objects.Cons);
        result = try cons(heap, c.car, result);
        reversed = c.cdr;
    }
    return result;
}

/// Reverse a list
pub fn reverse(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    var current = val;
    var result = Value.nil;

    while (!current.isNil()) {
        if (!current.isCons()) break;
        const c = current.toPtr(objects.Cons);
        result = try cons(heap, c.car, result);
        current = c.cdr;
    }

    return result;
}

/// Get last cons cell of a list
pub fn last(val: Value) Value {
    if (val.isNil()) return Value.nil;
    if (!val.isCons()) return Value.nil;

    var current = val;
    while (true) {
        const c = current.toPtr(objects.Cons);
        if (c.cdr.isNil() or !c.cdr.isCons()) return current;
        current = c.cdr;
    }
}

// ============================================================================
// Tests
// ============================================================================

test "cons car cdr" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const cell = try cons(&heap, Value.makeFixnum(1), Value.makeFixnum(2));
    try testing.expect(consp(cell));
    try testing.expectEqual(@as(i64, 1), car(cell).toFixnum());
    try testing.expectEqual(@as(i64, 2), cdr(cell).toFixnum());
}

test "null check" {
    const testing = std.testing;

    try testing.expect(isNull(Value.nil));
    try testing.expect(!isNull(Value.makeFixnum(1)));
}

test "list length" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Empty list
    try testing.expectEqual(@as(i64, 0), length(Value.nil));

    // Single element
    const single = try cons(&heap, Value.makeFixnum(1), Value.nil);
    try testing.expectEqual(@as(i64, 1), length(single));

    // Multiple elements
    const three = try list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
    });
    try testing.expectEqual(@as(i64, 3), length(three));
}

test "nth and nthcdr" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const lst = try list(&heap, &[_]Value{
        Value.makeFixnum(10),
        Value.makeFixnum(20),
        Value.makeFixnum(30),
    });

    try testing.expectEqual(@as(i64, 10), nth(lst, 0).toFixnum());
    try testing.expectEqual(@as(i64, 20), nth(lst, 1).toFixnum());
    try testing.expectEqual(@as(i64, 30), nth(lst, 2).toFixnum());
    try testing.expect(nth(lst, 3).isNil());

    try testing.expectEqual(@as(i64, 20), car(nthcdr(lst, 1)).toFixnum());
    try testing.expectEqual(@as(i64, 30), car(nthcdr(lst, 2)).toFixnum());
}

test "setcar setcdr" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const cell = try cons(&heap, Value.makeFixnum(1), Value.makeFixnum(2));

    setCar(cell, Value.makeFixnum(100));
    setCdr(cell, Value.makeFixnum(200));

    try testing.expectEqual(@as(i64, 100), car(cell).toFixnum());
    try testing.expectEqual(@as(i64, 200), cdr(cell).toFixnum());
}

test "reverse" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const lst = try list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
    });

    const rev = try reverse(&heap, lst);

    try testing.expectEqual(@as(i64, 3), nth(rev, 0).toFixnum());
    try testing.expectEqual(@as(i64, 2), nth(rev, 1).toFixnum());
    try testing.expectEqual(@as(i64, 1), nth(rev, 2).toFixnum());
}

test "append" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const lst1 = try list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
    });

    const lst2 = try list(&heap, &[_]Value{
        Value.makeFixnum(3),
        Value.makeFixnum(4),
    });

    const combined = try append(&heap, lst1, lst2);

    try testing.expectEqual(@as(i64, 4), length(combined));
    try testing.expectEqual(@as(i64, 1), nth(combined, 0).toFixnum());
    try testing.expectEqual(@as(i64, 2), nth(combined, 1).toFixnum());
    try testing.expectEqual(@as(i64, 3), nth(combined, 2).toFixnum());
    try testing.expectEqual(@as(i64, 4), nth(combined, 3).toFixnum());
}

test "last" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const lst = try list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
    });

    const lst_last = last(lst);
    try testing.expectEqual(@as(i64, 3), car(lst_last).toFixnum());
    try testing.expect(cdr(lst_last).isNil());
}
