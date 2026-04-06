//! String primitives
//!
//! string-length, string-ref, symbol-name, string comparison

const std = @import("std");
const Value = @import("../value.zig").Value;
const Tag = @import("../value.zig").Tag;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

fn isCharacterVector(val: Value) bool {
    return val.isVector() and val.toPtr(objects.Vector).isCharacterVector();
}

fn stringLen(val: Value) ?usize {
    if (val.isString()) return val.toPtr(objects.String).length;
    if (isCharacterVector(val)) {
        const vec = val.toPtr(objects.Vector);
        return @intCast(vec.getFillPointer() orelse vec.length);
    }
    return null;
}

fn stringCharAt(val: Value, index: usize) ?u8 {
    if (val.isString()) {
        const str = val.toPtr(objects.String);
        if (index >= str.length) return null;
        return str.data[index];
    }
    if (isCharacterVector(val)) {
        const vec = val.toPtr(objects.Vector);
        const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
        if (index >= len) return null;
        const ch = vec.data[index];
        if (!ch.isCharacter()) return null;
        const cp = ch.toCharacter();
        if (cp > 0xFF) return null;
        return @intCast(cp);
    }
    return null;
}

/// Get string length
pub fn stringLength(val: Value) i64 {
    return if (stringLen(val)) |len| @intCast(len) else -1;
}

/// Get character at index (0-indexed)
/// Returns -1 on error (not a string, out of bounds)
pub fn stringRef(val: Value, index: usize) i64 {
    return if (stringCharAt(val, index)) |b| @intCast(b) else -1;
}

/// Set character at index
/// Returns false on error
pub fn stringSet(val: Value, index: usize, char: u8) bool {
    if (val.isString()) {
        const str = val.toPtr(objects.String);
        if (index >= str.length) return false;
        str.data[index] = char;
        return true;
    }
    if (isCharacterVector(val)) {
        const vec = val.toPtr(objects.Vector);
        const len: usize = @intCast(vec.getFillPointer() orelse vec.length);
        if (index >= len) return false;
        vec.data[index] = Value.makeCharacter(char);
        return true;
    }
    return false;
}

/// Get bytes as a slice
pub fn stringBytes(val: Value) ?[]const u8 {
    if (val.isString()) return val.toPtr(objects.String).bytes();
    return null;
}

/// Get symbol name as string value
pub fn symbolName(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    // Handle special symbols nil and t
    if (val.isNil()) {
        return try heap.allocBaseString("nil");
    }
    if (val.isT()) {
        return try heap.allocBaseString("t");
    }
    if (val.isKeyword()) {
        const kw = val.toPtr(objects.Keyword);
        return try heap.allocBaseString(kw.getName());
    }
    if (!val.isSymbol()) return Value.nil;
    const sym = val.toPtr(objects.Symbol);
    const name_bytes = sym.getName();
    return try heap.allocBaseString(name_bytes);
}

/// Get symbol name bytes directly
pub fn symbolNameBytes(val: Value) ?[]const u8 {
    // Handle special symbols nil and t
    if (val.isNil()) return "nil";
    if (val.isT()) return "t";
    if (val.isKeyword()) return val.toPtr(objects.Keyword).getName();
    if (!val.isSymbol()) return null;
    const sym = val.toPtr(objects.Symbol);
    return sym.getName();
}

/// Check if value is a string
pub fn stringp(val: Value) bool {
    return val.isString() or isCharacterVector(val);
}

/// Check if value is a symbol (includes nil and t as per CL)
pub fn symbolp(val: Value) bool {
    return val.isSymbolLike();
}

/// String equality
pub fn stringEqual(a: Value, b: Value) bool {
    const len_a = stringLen(a) orelse return false;
    const len_b = stringLen(b) orelse return false;
    if (len_a != len_b) return false;
    var i: usize = 0;
    while (i < len_a) : (i += 1) {
        if (stringCharAt(a, i).? != stringCharAt(b, i).?) return false;
    }
    return true;
}

/// String less than (lexicographic)
pub fn stringLt(a: Value, b: Value) bool {
    const len_a = stringLen(a) orelse return false;
    const len_b = stringLen(b) orelse return false;
    const min_len = @min(len_a, len_b);
    var i: usize = 0;
    while (i < min_len) : (i += 1) {
        const ca = stringCharAt(a, i).?;
        const cb = stringCharAt(b, i).?;
        if (ca < cb) return true;
        if (ca > cb) return false;
    }
    return len_a < len_b;
}

/// String greater than (lexicographic)
pub fn stringGt(a: Value, b: Value) bool {
    const len_a = stringLen(a) orelse return false;
    const len_b = stringLen(b) orelse return false;
    const min_len = @min(len_a, len_b);
    var i: usize = 0;
    while (i < min_len) : (i += 1) {
        const ca = stringCharAt(a, i).?;
        const cb = stringCharAt(b, i).?;
        if (ca > cb) return true;
        if (ca < cb) return false;
    }
    return len_a > len_b;
}

/// String less than or equal (lexicographic)
pub fn stringLe(a: Value, b: Value) bool {
    return stringLt(a, b) or stringEqual(a, b);
}

/// String greater than or equal (lexicographic)
pub fn stringGe(a: Value, b: Value) bool {
    return stringGt(a, b) or stringEqual(a, b);
}

/// Concatenate two strings
pub fn stringConcat(heap: *Heap, a: Value, b: Value) error{OutOfMemory, Overflow}!Value {
    const len_a = stringLen(a) orelse return Value.nil;
    const len_b = stringLen(b) orelse return Value.nil;
    const new_len = try std.math.add(usize, len_a, len_b);
    const out = try heap.allocStringUninitialized(new_len);
    const str = out.toPtr(objects.String);
    var i: usize = 0;
    while (i < len_a) : (i += 1) str.data[i] = stringCharAt(a, i).?;
    var j: usize = 0;
    while (j < len_b) : (j += 1) str.data[len_a + j] = stringCharAt(b, j).?;
    return out;
}

/// Create substring
pub fn substring(heap: *Heap, val: Value, start: usize, end: usize) error{OutOfMemory, Overflow}!Value {
    const len = stringLen(val) orelse return Value.nil;
    if (start > end or end > len) return Value.nil;
    const out = try heap.allocStringUninitialized(end - start);
    const str = out.toPtr(objects.String);
    var i: usize = 0;
    while (i < end - start) : (i += 1) str.data[i] = stringCharAt(val, start + i).?;
    return out;
}

/// Convert string to uppercase
pub fn stringUpcase(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    const len = stringLen(val) orelse return Value.nil;
    const new_str = try heap.allocStringUninitialized(len);

    const new_str_obj = new_str.toPtr(objects.String);
    for (0..new_str_obj.length) |i| {
        new_str_obj.data[i] = stringCharAt(val, i).?;
        const c = &new_str_obj.data[i];
        if (c.* >= 'a' and c.* <= 'z') {
            c.* -= 32;
        }
    }

    return new_str;
}

/// Convert string to lowercase
pub fn stringDowncase(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    const len = stringLen(val) orelse return Value.nil;
    const new_str = try heap.allocStringUninitialized(len);

    const new_str_obj = new_str.toPtr(objects.String);
    for (0..new_str_obj.length) |i| {
        new_str_obj.data[i] = stringCharAt(val, i).?;
        const c = &new_str_obj.data[i];
        if (c.* >= 'A' and c.* <= 'Z') {
            c.* += 32;
        }
    }

    return new_str;
}

/// Get keyword name as string
pub fn keywordName(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    if (!val.isKeyword()) return Value.nil;
    const kw = val.toPtr(objects.Keyword);
    const name_bytes = kw.getName();
    return try heap.allocBaseString(name_bytes);
}

/// Get keyword name bytes
pub fn keywordNameBytes(val: Value) ?[]const u8 {
    if (!val.isKeyword()) return null;
    const kw = val.toPtr(objects.Keyword);
    return kw.getName();
}

/// Check if value is a keyword
pub fn keywordp(val: Value) bool {
    return val.isKeyword();
}

/// Create string of given size filled with character
pub fn makeString(heap: *Heap, size: usize, char: u8) error{OutOfMemory, Overflow}!Value {
    const str_val = try heap.allocStringUninitialized(size);
    const str = str_val.toPtr(objects.String);
    @memset(str.data[0..size], char);
    return str_val;
}

// ============================================================================
// Tests
// ============================================================================

test "string length and ref" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("hello");

    try testing.expectEqual(@as(i64, 5), stringLength(str));
    try testing.expectEqual(@as(i64, 'h'), stringRef(str, 0));
    try testing.expectEqual(@as(i64, 'e'), stringRef(str, 1));
    try testing.expectEqual(@as(i64, 'o'), stringRef(str, 4));
    try testing.expectEqual(@as(i64, -1), stringRef(str, 5)); // out of bounds
}

test "string set" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("hello");

    try testing.expect(stringSet(str, 0, 'H'));
    try testing.expectEqual(@as(i64, 'H'), stringRef(str, 0));
    try testing.expect(!stringSet(str, 10, 'X')); // out of bounds
}

test "string equality" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str1 = try heap.allocBaseString("hello");
    const str2 = try heap.allocBaseString("hello");
    const str3 = try heap.allocBaseString("world");

    try testing.expect(stringEqual(str1, str2));
    try testing.expect(!stringEqual(str1, str3));
}

test "string concat" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str1 = try heap.allocBaseString("hello");
    const str2 = try heap.allocBaseString(" world");

    const combined = try stringConcat(&heap, str1, str2);
    const bytes = stringBytes(combined);

    try testing.expect(bytes != null);
    try testing.expectEqualStrings("hello world", bytes.?);
}

test "string concat non-string" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("hello");
    const result = try stringConcat(&heap, str, Value.makeFixnum(1));

    try testing.expect(result.isNil());
}

test "substring" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("hello world");

    const sub = try substring(&heap, str, 0, 5);
    const bytes = stringBytes(sub);

    try testing.expect(bytes != null);
    try testing.expectEqualStrings("hello", bytes.?);
}

test "string case conversion" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("Hello World");

    const upper = try stringUpcase(&heap, str);
    try testing.expectEqualStrings("HELLO WORLD", stringBytes(upper).?);

    const lower = try stringDowncase(&heap, str);
    try testing.expectEqualStrings("hello world", stringBytes(lower).?);
}

test "string predicates" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocBaseString("test");

    try testing.expect(stringp(str));
    try testing.expect(!stringp(Value.makeFixnum(42)));
    try testing.expect(!stringp(Value.nil));
}
