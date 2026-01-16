//! String primitives
//!
//! string-length, string-ref, symbol-name, string comparison

const std = @import("std");
const Value = @import("../value.zig").Value;
const Tag = @import("../value.zig").Tag;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

/// Get string length
pub fn stringLength(val: Value) i64 {
    if (!val.isString()) return -1;
    const str = val.toPtr(objects.String);
    return @intCast(str.length);
}

/// Get character at index (0-indexed)
/// Returns -1 on error (not a string, out of bounds)
pub fn stringRef(val: Value, index: usize) i64 {
    if (!val.isString()) return -1;
    const str = val.toPtr(objects.String);
    if (index >= str.length) return -1;
    return @intCast(str.data[index]);
}

/// Set character at index
/// Returns false on error
pub fn stringSet(val: Value, index: usize, char: u8) bool {
    if (!val.isString()) return false;
    const str = val.toPtr(objects.String);
    if (index >= str.length) return false;
    str.data[index] = char;
    return true;
}

/// Get bytes as a slice
pub fn stringBytes(val: Value) ?[]const u8 {
    if (!val.isString()) return null;
    const str = val.toPtr(objects.String);
    return str.bytes();
}

/// Get symbol name as string value
pub fn symbolName(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    // Handle special symbols nil and t
    if (val.isNil()) {
        return try heap.allocString("nil");
    }
    if (val.isT()) {
        return try heap.allocString("t");
    }
    if (!val.isSymbol()) return Value.nil;
    const sym = val.toPtr(objects.Symbol);
    const name_bytes = sym.getName();
    return try heap.allocString(name_bytes);
}

/// Get symbol name bytes directly
pub fn symbolNameBytes(val: Value) ?[]const u8 {
    // Handle special symbols nil and t
    if (val.isNil()) return "nil";
    if (val.isT()) return "t";
    if (!val.isSymbol()) return null;
    const sym = val.toPtr(objects.Symbol);
    return sym.getName();
}

/// Check if value is a string
pub fn stringp(val: Value) bool {
    return val.isString();
}

/// Check if value is a symbol (includes nil and t as per CL)
pub fn symbolp(val: Value) bool {
    return val.isSymbolLike();
}

/// String equality
pub fn stringEqual(a: Value, b: Value) bool {
    if (!a.isString() or !b.isString()) return false;

    const str_a = a.toPtr(objects.String);
    const str_b = b.toPtr(objects.String);

    if (str_a.length != str_b.length) return false;
    // String comparison needed: comparing actual string content (not symbols)
    return std.mem.eql(u8, str_a.bytes(), str_b.bytes());
}

/// String less than (lexicographic)
pub fn stringLt(a: Value, b: Value) bool {
    if (!a.isString() or !b.isString()) return false;

    const str_a = a.toPtr(objects.String);
    const str_b = b.toPtr(objects.String);

    return std.mem.order(u8, str_a.bytes(), str_b.bytes()) == .lt;
}

/// String greater than (lexicographic)
pub fn stringGt(a: Value, b: Value) bool {
    if (!a.isString() or !b.isString()) return false;

    const str_a = a.toPtr(objects.String);
    const str_b = b.toPtr(objects.String);

    return std.mem.order(u8, str_a.bytes(), str_b.bytes()) == .gt;
}

/// String less than or equal (lexicographic)
pub fn stringLe(a: Value, b: Value) bool {
    if (!a.isString() or !b.isString()) return false;

    const str_a = a.toPtr(objects.String);
    const str_b = b.toPtr(objects.String);

    const ord = std.mem.order(u8, str_a.bytes(), str_b.bytes());
    return ord == .lt or ord == .eq;
}

/// String greater than or equal (lexicographic)
pub fn stringGe(a: Value, b: Value) bool {
    if (!a.isString() or !b.isString()) return false;

    const str_a = a.toPtr(objects.String);
    const str_b = b.toPtr(objects.String);

    const ord = std.mem.order(u8, str_a.bytes(), str_b.bytes());
    return ord == .gt or ord == .eq;
}

/// Concatenate two strings
pub fn stringConcat(heap: *Heap, a: Value, b: Value) error{OutOfMemory}!Value {
    if (!a.isString() or !b.isString()) return Value.nil;

    const str_a = a.toPtr(objects.String);
    const str_b = b.toPtr(objects.String);

    // Use checked addition to prevent overflow
    const new_len = std.math.add(usize, str_a.length, str_b.length) catch return error.OutOfMemory;
    const aligned_len = std.mem.alignForward(usize, new_len, 8);
    const total_size = @sizeOf(objects.String) + aligned_len;

    const ptr = try heap.allocRaw(total_size);
    const str: *objects.String = @ptrCast(@alignCast(ptr));
    const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

    // Copy bytes from both strings
    @memcpy(data_ptr[0..str_a.length], str_a.bytes());
    @memcpy(data_ptr[str_a.length..new_len], str_b.bytes());

    str.* = .{
        .length = new_len,
        .data = data_ptr,
    };

    return Value.makeString(str);
}

/// Create substring
pub fn substring(heap: *Heap, val: Value, start: usize, end: usize) error{OutOfMemory}!Value {
    if (!val.isString()) return Value.nil;

    const str = val.toPtr(objects.String);
    if (start > end or end > str.length) return Value.nil;

    const slice = str.bytes()[start..end];
    return try heap.allocString(slice);
}

/// Convert string to uppercase
pub fn stringUpcase(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    if (!val.isString()) return Value.nil;

    const str = val.toPtr(objects.String);
    const new_str = try heap.allocString(str.bytes());

    const new_str_obj = new_str.toPtr(objects.String);
    for (new_str_obj.data[0..new_str_obj.length]) |*c| {
        if (c.* >= 'a' and c.* <= 'z') {
            c.* -= 32;
        }
    }

    return new_str;
}

/// Convert string to lowercase
pub fn stringDowncase(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    if (!val.isString()) return Value.nil;

    const str = val.toPtr(objects.String);
    const new_str = try heap.allocString(str.bytes());

    const new_str_obj = new_str.toPtr(objects.String);
    for (new_str_obj.data[0..new_str_obj.length]) |*c| {
        if (c.* >= 'A' and c.* <= 'Z') {
            c.* += 32;
        }
    }

    return new_str;
}

/// Get keyword name as string
pub fn keywordName(heap: *Heap, val: Value) error{OutOfMemory}!Value {
    if (!val.isKeyword()) return Value.nil;
    const kw = val.toPtr(objects.Keyword);
    const name_bytes = kw.getName();
    return try heap.allocString(name_bytes);
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
pub fn makeString(heap: *Heap, size: usize, char: u8) error{OutOfMemory}!Value {
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

    const str = try heap.allocString("hello");

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

    const str = try heap.allocString("hello");

    try testing.expect(stringSet(str, 0, 'H'));
    try testing.expectEqual(@as(i64, 'H'), stringRef(str, 0));
    try testing.expect(!stringSet(str, 10, 'X')); // out of bounds
}

test "string equality" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str1 = try heap.allocString("hello");
    const str2 = try heap.allocString("hello");
    const str3 = try heap.allocString("world");

    try testing.expect(stringEqual(str1, str2));
    try testing.expect(!stringEqual(str1, str3));
}

test "string concat" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str1 = try heap.allocString("hello");
    const str2 = try heap.allocString(" world");

    const combined = try stringConcat(&heap, str1, str2);
    const bytes = stringBytes(combined);

    try testing.expect(bytes != null);
    try testing.expectEqualStrings("hello world", bytes.?);
}

test "substring" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocString("hello world");

    const sub = try substring(&heap, str, 0, 5);
    const bytes = stringBytes(sub);

    try testing.expect(bytes != null);
    try testing.expectEqualStrings("hello", bytes.?);
}

test "string case conversion" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocString("Hello World");

    const upper = try stringUpcase(&heap, str);
    try testing.expectEqualStrings("HELLO WORLD", stringBytes(upper).?);

    const lower = try stringDowncase(&heap, str);
    try testing.expectEqualStrings("hello world", stringBytes(lower).?);
}

test "string predicates" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const str = try heap.allocString("test");

    try testing.expect(stringp(str));
    try testing.expect(!stringp(Value.makeFixnum(42)));
    try testing.expect(!stringp(Value.nil));
}
