//! I/O primitives
//!
//! sys-read, sys-write, sys-exit, file operations

const std = @import("std");
const fs = std.fs;
const Value = @import("../value.zig").Value;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

/// Pushback buffer for unread-char (single character)
var pushback_char: ?u8 = null;

/// Write a string to stdout
pub fn sysWrite(val: Value) !void {
    if (!val.isString()) return;

    const str = val.toPtr(objects.String);
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeAll(str.bytes());
    try w.flush();
}

/// Write bytes directly to stdout
pub fn sysWriteBytes(bytes: []const u8) !void {
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeAll(bytes);
    try w.flush();
}

/// Write a character to stdout
pub fn sysWriteChar(char: u8) !void {
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.writeByte(char);
    try w.flush();
}

/// Write a fixnum to stdout
pub fn sysWriteFixnum(val: Value) !void {
    if (!val.isFixnum()) return;

    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try w.print("{d}", .{val.toFixnum()});
    try w.flush();
}

/// Write newline to stdout
pub fn sysNewline() !void {
    try sysWriteChar('\n');
}

/// Read a line from stdin (allocates in heap)
pub fn sysReadLine(heap: *Heap) !Value {
    const stdin_file = fs.File.stdin();
    var read_buf: [4096]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    var line_buf: [1024]u8 = undefined;
    var line_len: usize = 0;

    while (line_len < line_buf.len) {
        const byte = reader.takeByte() catch break;

        if (byte == '\n') break;
        line_buf[line_len] = byte;
        line_len += 1;
    }

    if (line_len == 0) return Value.nil;

    return heap.allocString(line_buf[0..line_len]) orelse error.OutOfMemory;
}

/// Read a single character from stdin
pub fn sysReadChar() !i64 {
    // Check pushback buffer first
    if (pushback_char) |ch| {
        pushback_char = null;
        return @intCast(ch);
    }

    const stdin_file = fs.File.stdin();
    var read_buf: [4096]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    const byte = reader.takeByte() catch return -1;

    return @intCast(byte);
}

/// Peek at next character without consuming it
pub fn sysPeekChar() !i64 {
    // If already have pushback, return it
    if (pushback_char) |ch| {
        return @intCast(ch);
    }

    // Read and push back
    const stdin_file = fs.File.stdin();
    var read_buf: [4096]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    const byte = reader.takeByte() catch return -1;

    pushback_char = byte;
    return @intCast(byte);
}

/// Push a character back to be read again
pub fn sysUnreadChar(ch: u8) void {
    pushback_char = ch;
}

/// Print a Habu value to stdout (Lisp-style)
pub fn printValue(val: Value) !void {
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try printValueTo(val, w);
    try w.flush();
}

fn printValueTo(val: Value, w: anytype) !void {
    if (val.isNil()) {
        try w.writeAll("nil");
    } else if (val.isFixnum()) {
        try w.print("{d}", .{val.toFixnum()});
    } else if (val.isCharacter()) {
        const cp = val.toCharacter();
        if (cp == ' ') {
            try w.writeAll("#\\space");
        } else if (cp == '\n') {
            try w.writeAll("#\\newline");
        } else if (cp == '\t') {
            try w.writeAll("#\\tab");
        } else if (cp == '\r') {
            try w.writeAll("#\\return");
        } else if (cp >= 32 and cp < 127) {
            try w.print("#\\{c}", .{@as(u8, @intCast(cp))});
        } else {
            try w.print("#\\U+{X:0>4}", .{cp});
        }
    } else if (val.isCons()) {
        // Print as list
        try w.writeByte('(');
        var current = val;
        var first = true;
        while (current.isCons()) {
            if (!first) try w.writeByte(' ');
            first = false;
            const cons = current.toPtr(objects.Cons);
            try printValueTo(cons.car, w);
            current = cons.cdr;
        }
        // Handle dotted list
        if (!current.isNil()) {
            try w.writeAll(" . ");
            try printValueTo(current, w);
        }
        try w.writeByte(')');
    } else if (val.isSymbol()) {
        const sym = val.toPtr(objects.Symbol);
        try w.writeAll(sym.getName());
    } else if (val.isString()) {
        const str = val.toPtr(objects.String);
        try w.writeByte('"');
        try w.writeAll(str.bytes());
        try w.writeByte('"');
    } else if (val.isClosure()) {
        try w.writeAll("#<closure>");
    } else if (val.isKeyword()) {
        const kw = val.toPtr(objects.Keyword);
        try w.writeByte(':');
        try w.writeAll(kw.getName());
    } else if (val.isVector()) {
        const vec = val.toPtr(objects.Vector);
        try w.writeAll("#(");
        for (vec.items(), 0..) |item, i| {
            if (i > 0) try w.writeByte(' ');
            try printValueTo(item, w);
        }
        try w.writeByte(')');
    } else if (val.isHashTable()) {
        const ht = val.toPtr(objects.HashTable);
        try w.print("#<hash-table count={d}>", .{ht.count});
    } else {
        try w.writeAll("#<unknown>");
    }
}

/// Exit the process
pub fn sysExit(code: i64) noreturn {
    const exit_code: u8 = @truncate(@as(u64, @bitCast(code)));
    std.process.exit(exit_code);
}

/// Read entire file contents (allocates in heap)
pub fn readFile(heap: *Heap, path: []const u8) !Value {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();

    const stat = try file.stat();
    const size = stat.size;

    if (size > 10 * 1024 * 1024) {
        return error.FileTooLarge; // Limit to 10MB
    }

    // Allocate string in heap
    const aligned_len = std.mem.alignForward(usize, size, 8);
    const total_size = @sizeOf(objects.String) + aligned_len;

    const ptr = heap.allocRaw(total_size) orelse return error.OutOfMemory;
    const str: *objects.String = @ptrCast(@alignCast(ptr));
    const data_ptr: [*]u8 = @ptrCast(ptr + @sizeOf(objects.String));

    // Read file contents
    const bytes_read = try file.readAll(data_ptr[0..size]);
    _ = bytes_read;

    str.* = .{
        .length = size,
        .data = data_ptr,
    };

    return Value.makeString(str);
}

/// Write string to file
pub fn writeFile(path: []const u8, content: Value) !void {
    if (!content.isString()) return error.InvalidArgument;

    const str = content.toPtr(objects.String);

    const file = try fs.createFileAbsolute(path, .{});
    defer file.close();

    try file.writeAll(str.bytes());
}

/// Check if file exists
pub fn fileExists(path: []const u8) bool {
    fs.accessAbsolute(path, .{}) catch return false;
    return true;
}

/// Get file size
pub fn fileSize(path: []const u8) !i64 {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();

    const stat = try file.stat();
    return @intCast(stat.size);
}

/// Get current time in milliseconds
pub fn currentTimeMillis() i64 {
    return std.time.milliTimestamp();
}

/// Sleep for milliseconds
pub fn sleep(ms: i64) void {
    if (ms <= 0) return;
    std.Thread.sleep(@as(u64, @intCast(ms)) * std.time.ns_per_ms);
}

// Custom error types
const IoError = struct {
    const FileTooLarge = std.fs.File.OpenError || std.fs.File.StatError;
    const InvalidArgument = std.fs.File.OpenError;
};

// ============================================================================
// Tests
// ============================================================================

test "write bytes" {
    // This test just verifies the function compiles
    // Actual I/O testing would require mocking
    _ = sysWriteBytes;
}

test "time functions" {
    const testing = std.testing;

    const before = currentTimeMillis();
    sleep(10); // 10ms
    const after = currentTimeMillis();

    // Should have elapsed at least 10ms
    try testing.expect(after >= before + 10);
}
