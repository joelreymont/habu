//! Character primitives
//!
//! char-code, code-char, char-upcase, char-downcase, char=, char<, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const objects = @import("../objects.zig");

pub const Error = error{ TypeMismatch, OutOfMemory, OutOfRange, Overflow };

/// Get character code (ASCII/Unicode value) from character
pub fn charCode(val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const code = val.toCharacter();
    return Value.makeFixnum(@intCast(code));
}

/// Create character from code point
pub fn codeChar(code: Value) Error!Value {
    if (!code.isFixnum()) return error.TypeMismatch;
    const c = code.toFixnum();
    if (c < 0 or c > 0x10FFFF) return error.OutOfRange;
    return Value.makeCharacter(@intCast(c));
}

/// Convert character to uppercase
pub fn charUpcase(val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const c = val.toCharacter();
    if (c >= 'a' and c <= 'z') {
        return Value.makeCharacter(c - 32);
    }
    return val;
}

/// Convert character to lowercase
pub fn charDowncase(val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const c = val.toCharacter();
    if (c >= 'A' and c <= 'Z') {
        return Value.makeCharacter(c + 32);
    }
    return val;
}

/// Test if character is equal
pub fn charEq(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    return if (a.raw == b.raw) Value.t else Value.nil;
}

/// Test if character is less than
pub fn charLt(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    return if (a.toCharacter() < b.toCharacter()) Value.t else Value.nil;
}

/// Test if character is less than or equal
pub fn charLe(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    return if (a.toCharacter() <= b.toCharacter()) Value.t else Value.nil;
}

/// Test if character is greater than
pub fn charGt(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    return if (a.toCharacter() > b.toCharacter()) Value.t else Value.nil;
}

/// Test if character is greater than or equal
pub fn charGe(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    return if (a.toCharacter() >= b.toCharacter()) Value.t else Value.nil;
}

/// Test if character is not equal
pub fn charNe(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    return if (a.raw != b.raw) Value.t else Value.nil;
}

/// Test if character is equal (case-insensitive)
pub fn charEqualp(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    const a_up = try charUpcase(a);
    const b_up = try charUpcase(b);
    return if (a_up.raw == b_up.raw) Value.t else Value.nil;
}

/// Test if character is less than (case-insensitive)
pub fn charLessp(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    const a_up = try charUpcase(a);
    const b_up = try charUpcase(b);
    return if (a_up.toCharacter() < b_up.toCharacter()) Value.t else Value.nil;
}

/// Test if character is greater than (case-insensitive)
pub fn charGreaterp(a: Value, b: Value) Error!Value {
    if (!a.isCharacter() or !b.isCharacter()) return error.TypeMismatch;
    const a_up = try charUpcase(a);
    const b_up = try charUpcase(b);
    return if (a_up.toCharacter() > b_up.toCharacter()) Value.t else Value.nil;
}

/// Test if character is alphanumeric
pub fn alphanumericp(val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const c = val.toCharacter();
    const is_alpha = (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
    const is_digit = c >= '0' and c <= '9';
    return if (is_alpha or is_digit) Value.t else Value.nil;
}

/// Test if character is alphabetic
pub fn alphap(val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const c = val.toCharacter();
    const is_alpha = (c >= 'A' and c <= 'Z') or (c >= 'a' and c <= 'z');
    return if (is_alpha) Value.t else Value.nil;
}

/// Test if character is digit
pub fn digitp(val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const c = val.toCharacter();
    const is_digit = c >= '0' and c <= '9';
    return if (is_digit) Value.t else Value.nil;
}

/// Convert character to integer (same as char-code)
pub fn charInt(val: Value) Error!Value {
    return charCode(val);
}

/// Convert integer to character (same as code-char)
pub fn intChar(code: Value) Error!Value {
    return codeChar(code);
}

/// Convert digit character to integer with optional radix
pub fn digitChar(weight: Value, radix: Value) Error!Value {
    if (!weight.isFixnum() or !radix.isFixnum()) return error.TypeMismatch;
    const w = weight.toFixnum();
    const r = radix.toFixnum();
    if (r < 2 or r > 36) return error.OutOfRange;
    if (w < 0 or w >= r) return Value.nil;

    if (w < 10) {
        return Value.makeCharacter(@intCast('0' + w));
    } else {
        return Value.makeCharacter(@intCast('A' + (w - 10)));
    }
}

/// Get character name (e.g., #\Space -> "Space")
pub fn charName(heap: *Heap, val: Value) Error!Value {
    if (!val.isCharacter()) return error.TypeMismatch;
    const c = val.toCharacter();

    const name: []const u8 = switch (c) {
        ' ' => "Space",
        '\n' => "Newline",
        '\t' => "Tab",
        '\r' => "Return",
        0 => "Null",
        else => return Value.nil,
    };

    return try heap.allocBaseString(name);
}

/// Get character from name (e.g., "Space" -> #\Space)
pub fn nameChar(_: *Heap, name: Value) Error!Value {
    if (name.typeKind() != .string) return error.TypeMismatch;
    const str = name.toPtr(objects.String);
    const bytes = str.bytes();

    if (std.ascii.eqlIgnoreCase(bytes, "Space")) return Value.makeCharacter(' ');
    if (std.ascii.eqlIgnoreCase(bytes, "Newline")) return Value.makeCharacter('\n');
    if (std.ascii.eqlIgnoreCase(bytes, "Tab")) return Value.makeCharacter('\t');
    if (std.ascii.eqlIgnoreCase(bytes, "Return")) return Value.makeCharacter('\r');
    if (std.ascii.eqlIgnoreCase(bytes, "Null")) return Value.makeCharacter(0);

    return Value.nil;
}

test "char-code round-trip" {
    const testing = std.testing;

    const ch_a = Value.makeCharacter('A');
    const code = try charCode(ch_a);
    try testing.expectEqual(@as(i64, 'A'), code.toFixnum());

    const ch_back = try codeChar(code);
    try testing.expectEqual(ch_a.raw, ch_back.raw);
}

test "char case conversion" {
    const testing = std.testing;

    const lower_a = Value.makeCharacter('a');
    const upper_a = try charUpcase(lower_a);
    try testing.expectEqual(@as(i64, 'A'), upper_a.toCharacter());

    const upper_z = Value.makeCharacter('Z');
    const lower_z = try charDowncase(upper_z);
    try testing.expectEqual(@as(i64, 'z'), lower_z.toCharacter());

    const digit = Value.makeCharacter('5');
    const same = try charUpcase(digit);
    try testing.expectEqual(digit.raw, same.raw);
}

test "char comparisons" {
    const testing = std.testing;

    const a = Value.makeCharacter('a');
    const b = Value.makeCharacter('b');
    const a2 = Value.makeCharacter('a');

    try testing.expect((try charEq(a, a2)) == Value.t);
    try testing.expect((try charEq(a, b)) == Value.nil);

    try testing.expect((try charLt(a, b)) == Value.t);
    try testing.expect((try charLt(b, a)) == Value.nil);

    try testing.expect((try charGt(b, a)) == Value.t);
    try testing.expect((try charGt(a, b)) == Value.nil);

    try testing.expect((try charLe(a, a2)) == Value.t);
    try testing.expect((try charGe(a, a2)) == Value.t);
}

test "char case-insensitive comparison" {
    const testing = std.testing;

    const lower_a = Value.makeCharacter('a');
    const upper_a = Value.makeCharacter('A');

    try testing.expect((try charEqualp(lower_a, upper_a)) == Value.t);
    try testing.expect((try charEqualp(upper_a, lower_a)) == Value.t);

    const lower_b = Value.makeCharacter('b');
    try testing.expect((try charLessp(lower_a, lower_b)) == Value.t);
    try testing.expect((try charGreaterp(lower_b, upper_a)) == Value.t);
}

test "char type predicates" {
    const testing = std.testing;

    const ch_a = Value.makeCharacter('A');
    const ch_5 = Value.makeCharacter('5');
    const ch_space = Value.makeCharacter(' ');

    try testing.expect((try alphap(ch_a)) == Value.t);
    try testing.expect((try alphap(ch_5)) == Value.nil);
    try testing.expect((try alphap(ch_space)) == Value.nil);

    try testing.expect((try digitp(ch_5)) == Value.t);
    try testing.expect((try digitp(ch_a)) == Value.nil);

    try testing.expect((try alphanumericp(ch_a)) == Value.t);
    try testing.expect((try alphanumericp(ch_5)) == Value.t);
    try testing.expect((try alphanumericp(ch_space)) == Value.nil);
}

test "char-int conversion" {
    const testing = std.testing;

    const ch = Value.makeCharacter('X');
    const code = try charInt(ch);
    try testing.expectEqual(@as(i64, 'X'), code.toFixnum());

    const ch_back = try intChar(code);
    try testing.expectEqual(ch.raw, ch_back.raw);
}

test "digit-char radix" {
    const testing = std.testing;

    const radix10 = Value.makeFixnum(10);
    const radix16 = Value.makeFixnum(16);

    const d5 = try digitChar(Value.makeFixnum(5), radix10);
    try testing.expectEqual(@as(i64, '5'), d5.toCharacter());

    const d15 = try digitChar(Value.makeFixnum(15), radix16);
    try testing.expectEqual(@as(i64, 'F'), d15.toCharacter());

    const d10 = try digitChar(Value.makeFixnum(10), radix16);
    try testing.expectEqual(@as(i64, 'A'), d10.toCharacter());

    const invalid = try digitChar(Value.makeFixnum(10), radix10);
    try testing.expect(invalid == Value.nil);
}

test "char names" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const space = Value.makeCharacter(' ');
    const name = try charName(&heap, space);
    try testing.expect(name.typeKind() == .string);

    const str = name.toPtr(objects.String);
    try testing.expect(std.mem.eql(u8, str.bytes(), "Space"));

    const ch_back = try nameChar(&heap, name);
    try testing.expectEqual(space.raw, ch_back.raw);

    const lower_space = try heap.allocBaseString("space");
    const lower_ch = try nameChar(&heap, lower_space);
    try testing.expectEqual(space.raw, lower_ch.raw);

    const newline = Value.makeCharacter('\n');
    const nl_name = try charName(&heap, newline);
    const nl_str = nl_name.toPtr(objects.String);
    try testing.expect(std.mem.eql(u8, nl_str.bytes(), "Newline"));

    const upper_nl = try heap.allocBaseString("NEWLINE");
    const upper_ch = try nameChar(&heap, upper_nl);
    try testing.expectEqual(newline.raw, upper_ch.raw);
}
