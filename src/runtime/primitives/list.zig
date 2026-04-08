//! List primitives
//!
//! cons, car, cdr, null, consp, length, list operations

const std = @import("std");
const Value = @import("../value.zig").Value;
const Tag = @import("../value.zig").Tag;
const objects = @import("../objects.zig");
const Symbol = objects.Symbol;
const Cons = objects.Cons;
const Heap = @import("../heap.zig").Heap;
const symbol_prims = @import("symbol.zig");

fn allocConsRooted(heap: *Heap, car_val: Value, cdr_val: Value, roots: []Value) error{OutOfMemory}!Value {
    const cell = try heap.allocWithGC(Cons, roots);
    cell.* = Cons.init(car_val, cdr_val);
    return Value.makeCons(cell);
}

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
pub fn setCar(heap: *Heap, val: Value, new_car: Value) void {
    if (!val.isCons()) return;
    const c = val.toPtr(objects.Cons);
    c.car = new_car;
    heap.writeBarrier(val, new_car);
}

/// Set the cdr of a cons cell
pub fn setCdr(heap: *Heap, val: Value, new_cdr: Value) void {
    if (!val.isCons()) return;
    const c = val.toPtr(objects.Cons);
    c.cdr = new_cdr;
    heap.writeBarrier(val, new_cdr);
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

    // Copy list1 once, preserving order, then splice list2 at tail.
    const first_src = list1.toPtr(objects.Cons);
    const head = try cons(heap, first_src.car, Value.nil);
    var tail = head;
    var curr = first_src.cdr;
    while (curr.isCons()) {
        const src_cell = curr.toPtr(objects.Cons);
        const copied = try cons(heap, src_cell.car, Value.nil);
        setCdr(heap, tail, copied);
        tail = copied;
        curr = src_cell.cdr;
    }
    setCdr(heap, tail, list2);
    return head;
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

    setCar(&heap, cell, Value.makeFixnum(100));
    setCdr(&heap, cell, Value.makeFixnum(200));

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

test "append allocates one cons per left element" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const lst1 = try list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
    });
    const lst2 = try list(&heap, &[_]Value{
        Value.makeFixnum(4),
        Value.makeFixnum(5),
    });

    const before = heap.bytesUsed();
    const combined = try append(&heap, lst1, lst2);
    const after = heap.bytesUsed();

    try testing.expectEqual(@as(usize, @sizeOf(Cons) * 3), after - before);
    try testing.expectEqual(@as(i64, 5), length(combined));
    try testing.expectEqual(@as(i64, 1), nth(combined, 0).toFixnum());
    try testing.expectEqual(@as(i64, 2), nth(combined, 1).toFixnum());
    try testing.expectEqual(@as(i64, 3), nth(combined, 2).toFixnum());
    try testing.expectEqual(@as(i64, 4), nth(combined, 3).toFixnum());
    try testing.expectEqual(@as(i64, 5), nth(combined, 4).toFixnum());
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

// ============================================================================
// Property list operations
// ============================================================================

/// Get property from symbol's property list
/// (get symbol indicator) -> value or nil
fn symbolPlistPtr(heap: *Heap, sym: Value) ?*Value {
    if (sym.isNil()) return &heap.nil_symbol_plist;
    if (sym.isT()) return &heap.t_symbol_plist;
    if (sym.isSymbol()) return &sym.toPtr(Symbol).plist;
    return null;
}

fn loadPlist(heap: *Heap, sym: Value) !Value {
    if (sym.isKeyword()) return heap.getKeywordPlist(sym);
    const plist_ptr = symbolPlistPtr(heap, sym) orelse return error.TypeMismatch;
    return plist_ptr.*;
}

fn loadFlatSymbolPlist(heap: *Heap, sym: Value) !Value {
    return try symbol_prims.symbolPlist(heap, sym);
}

fn flatGet(plist: Value, indicator: Value) Value {
    var tail = plist;
    while (tail.isCons()) {
        const ind_cell = tail.toPtr(Cons);
        const rest = ind_cell.cdr;
        if (!rest.isCons()) break;
        const value_cell = rest.toPtr(Cons);
        if (ind_cell.car.eq(indicator)) return value_cell.car;
        tail = value_cell.cdr;
    }
    return Value.nil;
}

fn flatPut(heap: *Heap, plist: Value, indicator: Value, value: Value) !Value {
    var tail = plist;
    while (tail.isCons()) {
        const ind_cell_val = tail;
        const ind_cell = ind_cell_val.toPtr(Cons);
        const rest = ind_cell.cdr;
        if (!rest.isCons()) break;
        const value_cell_val = rest;
        const value_cell = value_cell_val.toPtr(Cons);
        if (ind_cell.car.eq(indicator)) {
            value_cell.car = value;
            heap.writeBarrier(value_cell_val, value);
            return plist;
        }
        tail = value_cell.cdr;
    }

    var roots = [_]Value{ plist, indicator, value };
    const value_cell = try allocConsRooted(heap, roots[2], roots[0], roots[0..]);
    roots[0] = value_cell;
    return try allocConsRooted(heap, roots[1], roots[0], roots[0..]);
}

fn flatRemprop(heap: *Heap, plist: Value, indicator: Value) !struct { plist: Value, removed: bool } {
    if (!plist.isCons()) return .{ .plist = plist, .removed = false };

    const first = plist.toPtr(Cons);
    if (first.cdr.isCons()) {
        const value_cell = first.cdr.toPtr(Cons);
        if (first.car.eq(indicator)) {
            return .{ .plist = value_cell.cdr, .removed = true };
        }
    }

    var prev = first.cdr;
    while (prev.isCons()) {
        const prev_cons = prev.toPtr(Cons);
        if (!prev_cons.cdr.isCons()) break;
        const ind_cell_val = prev_cons.cdr;
        const ind_cell = ind_cell_val.toPtr(Cons);
        if (!ind_cell.cdr.isCons()) break;
        const value_cell_val = ind_cell.cdr;
        const value_cell = value_cell_val.toPtr(Cons);
        if (ind_cell.car.eq(indicator)) {
            prev_cons.cdr = value_cell.cdr;
            heap.writeBarrier(prev, value_cell.cdr);
            return .{ .plist = plist, .removed = true };
        }
        prev = value_cell_val;
    }

    return .{ .plist = plist, .removed = false };
}

fn storePlist(heap: *Heap, sym: Value, plist: Value) !void {
    if (sym.isKeyword()) {
        try heap.setKeywordPlist(sym, plist);
        return;
    }
    const plist_ptr = symbolPlistPtr(heap, sym) orelse return error.TypeMismatch;
    plist_ptr.* = plist;
    if (sym.isSymbol()) heap.writeBarrier(sym, plist);
}

fn getFlatHeadPlist(target: Value, indicator: Value) Value {
    var tail = target.toPtr(Cons).cdr;
    while (tail.isCons()) {
        const ind_cell = tail.toPtr(Cons);
        const rest = ind_cell.cdr;
        if (!rest.isCons()) break;
        const value_cell = rest.toPtr(Cons);
        if (ind_cell.car.eq(indicator)) return value_cell.car;
        tail = value_cell.cdr;
    }
    return Value.nil;
}

fn putFlatHeadPlist(heap: *Heap, target: Value, indicator: Value, value: Value) !Value {
    var head = target.toPtr(Cons);
    var tail = head.cdr;
    while (tail.isCons()) {
        const ind_cell = tail.toPtr(Cons);
        const rest = ind_cell.cdr;
        if (!rest.isCons()) break;
        const value_cell_val = rest;
        const value_cell = value_cell_val.toPtr(Cons);
        if (ind_cell.car.eq(indicator)) {
            value_cell.car = value;
            heap.writeBarrier(value_cell_val, value);
            return value;
        }
        tail = value_cell.cdr;
    }

    var roots = [_]Value{ target, indicator, value, head.cdr };
    const new_value_cell = try allocConsRooted(heap, roots[2], roots[3], roots[0..]);
    const target_live = roots[0];
    const indicator_live = roots[1];
    head = target_live.toPtr(Cons);
    roots[2] = new_value_cell;
    roots[3] = head.cdr;
    const new_ind_cell = try allocConsRooted(heap, indicator_live, new_value_cell, roots[0..]);
    head = roots[0].toPtr(Cons);
    head.cdr = new_ind_cell;
    heap.writeBarrier(roots[0], new_ind_cell);
    return value;
}

fn rempropFlatHeadPlist(heap: *Heap, target: Value, indicator: Value) !Value {
    var prev = target;
    var tail = target.toPtr(Cons).cdr;
    while (tail.isCons()) {
        const ind_cell = tail.toPtr(Cons);
        const rest = ind_cell.cdr;
        if (!rest.isCons()) break;
        const value_cell_val = rest;
        const value_cell = value_cell_val.toPtr(Cons);
        if (ind_cell.car.eq(indicator)) {
            const prev_cons = prev.toPtr(Cons);
            prev_cons.cdr = value_cell.cdr;
            heap.writeBarrier(prev, value_cell.cdr);
            return Value.t;
        }
        prev = value_cell_val;
        tail = value_cell.cdr;
    }
    return Value.nil;
}

pub fn get(heap: *Heap, sym: Value, indicator: Value) !Value {
    if (sym.isCons()) return getFlatHeadPlist(sym, indicator);
    if (!sym.isSymbolLike()) return error.TypeMismatch;
    const plist = try loadFlatSymbolPlist(heap, sym);
    return flatGet(plist, indicator);
}

/// Set property in symbol's property list
/// (put symbol indicator value) -> value
pub fn put(heap: *Heap, sym: Value, indicator: Value, value: Value) !Value {
    if (sym.isCons()) return putFlatHeadPlist(heap, sym, indicator, value);
    if (!sym.isSymbolLike()) return error.TypeMismatch;
    const plist = try loadFlatSymbolPlist(heap, sym);
    const new_plist = try flatPut(heap, plist, indicator, value);
    try symbol_prims.setSymbolPlist(heap, sym, new_plist);
    return value;
}

/// Remove property from symbol's property list
/// (remprop symbol indicator) -> t if removed, nil otherwise
pub fn remprop(heap: *Heap, sym: Value, indicator: Value) !Value {
    if (sym.isCons()) return rempropFlatHeadPlist(heap, sym, indicator);
    if (!sym.isSymbolLike()) return error.TypeMismatch;
    const plist = try loadFlatSymbolPlist(heap, sym);
    const result = try flatRemprop(heap, plist, indicator);
    if (!result.removed) return Value.nil;
    try symbol_prims.setSymbolPlist(heap, sym, result.plist);
    return Value.t;
}

test "put preserves existing plist entries" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const sym = try heap.intern("STRING");
    const fn_key = try heap.intern("%FUNCTION-CELL");
    const meta_key = try heap.intern("MACRO-FUNCTION");

    _ = try put(&heap, sym, fn_key, Value.makeFixnum(7));
    _ = try put(&heap, sym, meta_key, Value.makeFixnum(9));

    try testing.expectEqual(@as(i64, 7), (try get(&heap, sym, fn_key)).toFixnum());
    try testing.expectEqual(@as(i64, 9), (try get(&heap, sym, meta_key)).toFixnum());
}

test "put/get/remprop support Maxima-style head cons plists" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const head = try heap.allocCons(Value.nil, Value.nil);
    const k1 = try heap.intern("MFEXPR*");
    const k2 = try heap.intern("OPERATORS");

    _ = try put(&heap, head, k1, Value.makeFixnum(7));
    _ = try put(&heap, head, k2, Value.makeFixnum(9));
    try testing.expectEqual(@as(i64, 7), (try get(&heap, head, k1)).toFixnum());
    try testing.expectEqual(@as(i64, 9), (try get(&heap, head, k2)).toFixnum());

    _ = try put(&heap, head, k1, Value.makeFixnum(11));
    try testing.expectEqual(@as(i64, 11), (try get(&heap, head, k1)).toFixnum());
    try testing.expect((try remprop(&heap, head, k2)).isT());
    try testing.expect((try get(&heap, head, k2)).isNil());
    try testing.expect((try remprop(&heap, head, k2)).isNil());
}

test "get/put/remprop handle mixed symbol plists" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const sym = try heap.intern("MIXED-PLIST");
    const ka = try heap.intern("A");
    const kb = try heap.intern("B");
    const kc = try heap.intern("C");

    var roots_a = [_]Value{ sym, ka };
    const pair_a = try allocConsRooted(&heap, ka, Value.makeFixnum(1), roots_a[0..]);
    var roots_b_val = [_]Value{ sym, kb };
    const flat_b_val = try allocConsRooted(&heap, Value.makeFixnum(2), Value.nil, roots_b_val[0..]);
    var roots_b = [_]Value{ sym, kb, flat_b_val };
    const flat_b = try allocConsRooted(&heap, kb, flat_b_val, roots_b[0..]);
    var roots_mixed = [_]Value{ sym, pair_a, flat_b };
    const mixed = try allocConsRooted(&heap, pair_a, flat_b, roots_mixed[0..]);
    try symbol_prims.setSymbolPlist(&heap, sym, mixed);

    try testing.expectEqual(@as(i64, 1), (try get(&heap, sym, ka)).toFixnum());
    try testing.expectEqual(@as(i64, 2), (try get(&heap, sym, kb)).toFixnum());

    _ = try put(&heap, sym, kc, Value.makeFixnum(3));
    try testing.expectEqual(@as(i64, 3), (try get(&heap, sym, kc)).toFixnum());

    try testing.expect((try remprop(&heap, sym, kb)).isT());
    try testing.expect((try get(&heap, sym, kb)).isNil());

    const flat = try symbol_prims.symbolPlist(&heap, sym);
    try testing.expect(flat.isCons());
    try testing.expectEqual(@as(i64, 1), (try get(&heap, sym, ka)).toFixnum());
    try testing.expectEqual(@as(i64, 3), (try get(&heap, sym, kc)).toFixnum());
}

test "remprop removes non-head key from flat plist" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const sym = try heap.intern("FLAT-REM-SYM");
    const ka = try heap.intern("A");
    const kb = try heap.intern("B");

    _ = try put(&heap, sym, ka, Value.makeFixnum(1));
    _ = try put(&heap, sym, kb, Value.makeFixnum(2));
    _ = try put(&heap, sym, ka, Value.makeFixnum(3));

    try testing.expectEqual(@as(i64, 3), (try get(&heap, sym, ka)).toFixnum());
    try testing.expectEqual(@as(i64, 2), (try get(&heap, sym, kb)).toFixnum());

    try testing.expect((try remprop(&heap, sym, ka)).isT());
    try testing.expect((try get(&heap, sym, ka)).isNil());
    try testing.expectEqual(@as(i64, 2), (try get(&heap, sym, kb)).toFixnum());
}
