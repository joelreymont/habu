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

    return try heap.allocString(line_buf[0..line_len]);
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

/// Read a complete S-expression from stdin into buffer
/// Returns the number of bytes read, or error
pub fn sysReadSexp(buffer: []u8) !usize {
    const stdin_file = fs.File.stdin();
    var read_buf: [4096]u8 = undefined;
    var file_reader = stdin_file.reader(&read_buf);
    const reader = &file_reader.interface;

    var len: usize = 0;
    var paren_depth: i32 = 0;
    var in_string = false;
    var in_escape = false;
    var started = false;

    while (len < buffer.len) {
        // Check pushback buffer first
        const byte: u8 = if (pushback_char) |ch| blk: {
            pushback_char = null;
            break :blk ch;
        } else reader.takeByte() catch break;

        buffer[len] = byte;
        len += 1;

        if (in_escape) {
            in_escape = false;
            continue;
        }

        if (byte == '\\' and in_string) {
            in_escape = true;
            continue;
        }

        if (byte == '"') {
            in_string = !in_string;
            started = true;
            continue;
        }

        if (in_string) continue;

        // Skip leading whitespace
        if (!started and (byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r')) {
            len -= 1;
            continue;
        }

        started = true;

        if (byte == '(') {
            paren_depth += 1;
        } else if (byte == ')') {
            paren_depth -= 1;
            if (paren_depth == 0) break; // Complete expression
        } else if (paren_depth == 0 and (byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r')) {
            // Atom terminated by whitespace
            len -= 1; // Don't include the whitespace
            break;
        }
    }

    return if (len > 0) len else error.EndOfStream;
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

/// Print a Habu value to stdout without escaping (princ style)
pub fn princValue(val: Value) !void {
    const stdout_file = fs.File.stdout();
    var buf: [4096]u8 = undefined;
    var file_writer = stdout_file.writer(&buf);
    const w = &file_writer.interface;

    try princValueTo(val, w);
    try w.flush();
}

fn princValueTo(val: Value, w: anytype) !void {
    switch (val.typeKind()) {
        .nil => try w.writeAll("nil"),
        .t => try w.writeAll("t"),
        .fixnum => try w.print("{d}", .{val.toFixnum()}),
        .float => try w.print("{d}", .{val.toFloat()}),
        .char => {
            const cp = val.toCharacter();
            if (cp < 128) {
                try w.writeByte(@as(u8, @intCast(cp)));
            } else {
                var utf8_buf: [4]u8 = undefined;
                const len = std.unicode.utf8Encode(@intCast(cp), &utf8_buf) catch 0;
                try w.writeAll(utf8_buf[0..len]);
            }
        },
        .cons => {
            try w.writeByte('(');
            var current = val;
            var first = true;
            while (current.isCons()) {
                if (!first) try w.writeByte(' ');
                first = false;
                const cons = current.toPtr(objects.Cons);
                try princValueTo(cons.car, w);
                current = cons.cdr;
            }
            if (!current.isNil()) {
                try w.writeAll(" . ");
                try princValueTo(current, w);
            }
            try w.writeByte(')');
        },
        .symbol => try w.writeAll(val.toPtr(objects.Symbol).getName()),
        .string => try w.writeAll(val.toPtr(objects.String).bytes()),
        .closure => try w.writeAll("#<closure>"),
        .keyword => {
            try w.writeByte(':');
            try w.writeAll(val.toPtr(objects.Keyword).getName());
        },
        .vector => {
            const vec = val.toPtr(objects.Vector);
            try w.writeAll("#(");
            for (vec.items(), 0..) |item, i| {
                if (i > 0) try w.writeByte(' ');
                try princValueTo(item, w);
            }
            try w.writeByte(')');
        },
        .hashtable => try w.print("#<hash-table count={d}>", .{val.toPtr(objects.HashTable).count}),
        .rational => {
            const rat = val.toPtr(objects.Rational);
            try w.print("{d}/{d}", .{ rat.numerator, rat.denominator });
        },
        .complex => {
            const cplx = val.toPtr(objects.Complex);
            try w.print("#C({d} {d})", .{ cplx.real, cplx.imag });
        },
        .stream => {
            const stream = val.toPtr(objects.Stream);
            const dir = if (stream.direction == .input) "input" else "output";
            const kind = switch (stream.stream_type) {
                .string => "string",
                .file => "file",
                .stdin => "stdin",
                .stdout => "stdout",
                .stderr => "stderr",
            };
            try w.print("#<{s}-{s}-stream>", .{ kind, dir });
        },
    }
}

/// Write value to any writer (for write-to-string)
pub fn writeValueToBuffer(val: Value, w: anytype) !void {
    try printValueTo(val, w);
}

fn printValueTo(val: Value, w: anytype) !void {
    switch (val.typeKind()) {
        .nil => try w.writeAll("nil"),
        .t => try w.writeAll("t"),
        .fixnum => try w.print("{d}", .{val.toFixnum()}),
        .float => try w.print("{d}", .{val.toFloat()}),
        .char => {
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
        },
        .cons => {
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
            if (!current.isNil()) {
                try w.writeAll(" . ");
                try printValueTo(current, w);
            }
            try w.writeByte(')');
        },
        .symbol => try w.writeAll(val.toPtr(objects.Symbol).getName()),
        .string => {
            try w.writeByte('"');
            try w.writeAll(val.toPtr(objects.String).bytes());
            try w.writeByte('"');
        },
        .closure => try w.writeAll("#<closure>"),
        .keyword => {
            try w.writeByte(':');
            try w.writeAll(val.toPtr(objects.Keyword).getName());
        },
        .vector => {
            const vec = val.toPtr(objects.Vector);
            try w.writeAll("#(");
            for (vec.items(), 0..) |item, i| {
                if (i > 0) try w.writeByte(' ');
                try printValueTo(item, w);
            }
            try w.writeByte(')');
        },
        .hashtable => try w.print("#<hash-table count={d}>", .{val.toPtr(objects.HashTable).count}),
        .rational => {
            const rat = val.toPtr(objects.Rational);
            try w.print("{d}/{d}", .{ rat.numerator, rat.denominator });
        },
        .complex => {
            const cplx = val.toPtr(objects.Complex);
            try w.print("#C({d} {d})", .{ cplx.real, cplx.imag });
        },
        .stream => {
            const stream = val.toPtr(objects.Stream);
            const dir = if (stream.direction == .input) "input" else "output";
            const kind = switch (stream.stream_type) {
                .string => "string",
                .file => "file",
                .stdin => "stdin",
                .stdout => "stdout",
                .stderr => "stderr",
            };
            try w.print("#<{s}-{s}-stream>", .{ kind, dir });
        },
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

    const ptr = try heap.allocRaw(total_size);
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
