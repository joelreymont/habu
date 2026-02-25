//! File Stream Operations
const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const io = @import("io.zig");
const Vector = @import("../objects.zig").Vector;
const String = @import("../objects.zig").String;
const String32 = @import("../objects.zig").String32;
const Stream = @import("../objects.zig").Stream;
const StreamDirection = @import("../objects.zig").StreamDirection;
const StreamType = @import("../objects.zig").StreamType;
const Symbol = @import("../objects.zig").Symbol;
const BuiltinSymbols = @import("../builtins.zig").BuiltinSymbols;

pub fn primOpen(heap: *Heap, args: []const Value, builtins: *const BuiltinSymbols) !Value {
    if (args.len < 2) return error.InvalidArgument;

    const path_val = args[0];
    const mode_val = args[1];

    if (!path_val.isString()) return error.InvalidArgument;
    if (!mode_val.isKeyword()) return error.InvalidArgument;

    const path_str = path_val.toPtr(String);
    const path = path_str.data[0..path_str.length];

    // Parse mode keyword using identity: :read, :write, :append
    const is_read = mode_val.raw == builtins.kw_read.raw;
    const is_write = mode_val.raw == builtins.kw_write.raw;
    const is_io = mode_val.raw == builtins.kw_io.raw;
    const is_append = mode_val.raw == builtins.kw_append.raw;

    if (!is_read and !is_write and !is_io and !is_append) return error.InvalidArgument;

    const direction: StreamDirection = if (is_read) .input else if (is_io) .io else .output;

    // Open the file with appropriate flags
    const file = if (is_read)
        try std.fs.cwd().openFile(path, .{ .mode = .read_only })
    else if (is_append)
        try std.fs.cwd().createFile(path, .{ .truncate = false })
    else if (is_io)
        try std.fs.cwd().createFile(path, .{ .truncate = false, .read = true })
    else
        try std.fs.cwd().createFile(path, .{ .truncate = true });

    const fd = file.handle;

    return try heap.allocStream(direction, .file, fd);
}

pub fn primClose(_: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;
    try io.closeStream(stream_val, null);
    return Value.t;
}

pub fn primReadLine(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isInput()) return error.NotInputStreamError;

    // Handle string input streams
    if (stream.stream_type == .string) {
        if (stream.data_ptr == 0) return error.InvalidArgument;
        const data: [*]const u8 = @ptrFromInt(stream.data_ptr);
        const len = stream.length;
        const pos = stream.position;

        if (pos >= len) {
            return try heap.allocBaseString("");
        }

        var line_end = pos;
        while (line_end < len and data[line_end] != '\n') : (line_end += 1) {}

        const line = data[pos..line_end];
        stream.position = if (line_end < len) line_end + 1 else line_end;

        return try heap.allocBaseString(line);
    }

    if (stream.stream_type != .file and stream.stream_type != .stdin)
        return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    // Simple line reading: read until newline
    var buffer: [4096]u8 = undefined;
    var index: usize = 0;
    while (index < buffer.len) {
        const byte_opt: ?u8 = if (stream.pushback_char != 0xFF) blk: {
            const ch = stream.pushback_char;
            stream.pushback_char = 0xFF;
            break :blk ch;
        } else blk: {
            var byte: [1]u8 = undefined;
            const bytes_read = try file.read(&byte);
            if (bytes_read == 0) break :blk null;
            break :blk byte[0];
        };

        if (byte_opt == null) {
            if (index == 0) return Value.nil;
            break;
        }

        const byte = byte_opt.?;
        if (byte == '\n') break;
        buffer[index] = byte;
        index += 1;
    }

    return try heap.allocBaseString(buffer[0..index]);
}

test "primReadLine returns nil on EOF" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        const file = try tmp.dir.createFile("empty.txt", .{});
        defer file.close();
    }

    const file = try tmp.dir.openFile("empty.txt", .{});
    defer file.close();
    const dup_fd = try std.posix.dup(file.handle);
    const stream = try heap.allocFileInputStream(@intCast(dup_fd));
    const res = try primReadLine(&heap, &.{stream});
    try testing.expect(res.isNil());
}

test "primReadLine honors unread-char for file streams" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        const file = try tmp.dir.createFile("pushback.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("ab\nc");
    }

    const file = try tmp.dir.openFile("pushback.txt", .{});
    defer file.close();
    const dup_fd = try std.posix.dup(file.handle);
    const stream = try heap.allocFileInputStream(@intCast(dup_fd));

    const first = try io.readChar(stream, null, null);
    try io.unreadChar(Value.makeCharacter(@intCast(first.toFixnum())), stream);

    const line1 = try primReadLine(&heap, &.{stream});
    try testing.expect(line1.isString());
    try testing.expect(std.mem.eql(u8, line1.toPtr(String).bytes(), "ab"));

    const line2 = try primReadLine(&heap, &.{stream});
    try testing.expect(line2.isString());
    try testing.expect(std.mem.eql(u8, line2.toPtr(String).bytes(), "c"));
}

test "primOpen supports :io" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var builtins = try BuiltinSymbols.init(&heap);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    {
        const file = try tmp.dir.createFile("io-open.txt", .{ .read = true, .truncate = true });
        defer file.close();
        try file.writeAll("a");
    }

    const path = try tmp.dir.realpathAlloc(testing.allocator, "io-open.txt");
    defer testing.allocator.free(path);
    const path_val = try heap.allocBaseString(path);

    const stream_val = try primOpen(&heap, &.{ path_val, builtins.kw_io }, &builtins);
    try testing.expect(stream_val.isStream());

    try io.writeChar(Value.makeFixnum('Q'), stream_val);
    _ = try io.filePosition(&heap, stream_val, Value.makeFixnum(0));
    const ch = try io.readChar(stream_val, null, null);
    try testing.expectEqual(@as(i64, 'Q'), ch.toFixnum());

    try io.closeStream(stream_val, null);
}

pub fn primWriteLine(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 2) return error.InvalidArgument;

    const stream_val = args[0];
    const line_val = args[1];

    if (!stream_val.isStream()) return error.InvalidArgument;
    if (!line_val.isString()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);
    const line_str = line_val.toPtr(String);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isOutput()) return error.NotOutputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    try file.writeAll(line_str.data[0..line_str.length]);
    const newline = [_]u8{'\n'};
    try file.writeAll(&newline);

    return Value.t;
}

pub fn primReadChar(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isInput()) return error.NotInputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    var byte: [1]u8 = undefined;
    const bytes_read = try file.read(&byte);
    if (bytes_read == 0) return Value.nil;

    return Value.makeCharacter(byte[0]);
}

pub fn primWriteChar(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 2) return error.InvalidArgument;

    const stream_val = args[0];
    const char_val = args[1];

    if (!stream_val.isStream()) return error.InvalidArgument;
    if (!char_val.isChar()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isOutput()) return error.NotOutputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    const ch = char_val.toChar();
    const byte = [_]u8{@intCast(ch)};
    try file.writeAll(&byte);

    return Value.t;
}

pub fn primReadByte(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isInput()) return error.NotInputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    var byte: [1]u8 = undefined;
    const bytes_read = try file.read(&byte);
    if (bytes_read == 0) return Value.nil;

    return Value.makeFixnum(byte[0]);
}

pub fn primWriteByte(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 2) return error.InvalidArgument;

    const stream_val = args[0];
    const byte_val = args[1];

    if (!stream_val.isStream()) return error.InvalidArgument;
    if (!byte_val.isFixnum()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isOutput()) return error.NotOutputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    const byte = byte_val.toFixnum();
    if (byte < 0 or byte > 255) return error.InvalidArgument;

    const byte_arr = [_]u8{@intCast(byte)};
    try file.writeAll(&byte_arr);

    return Value.t;
}

pub fn primFilePosition(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };
    const pos = try file.getPos();

    return Value.makeFixnum(@intCast(pos));
}

pub fn primFileLength(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };
    const stat = try file.stat();

    return Value.makeFixnum(@intCast(stat.size));
}

pub fn primFinishOutput(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    // ANSI stream designators: NIL and T are both accepted.
    if (stream_val.isNil() or stream_val.isT()) return Value.nil;
    if (!stream_val.isStream()) return error.TypeError;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isOutput()) return error.NotOutputStreamError;

    // Streams are already unbuffered at this layer, so finish-output is a no-op.

    return Value.nil;
}

pub fn primForceOutput(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    // ANSI stream designators: NIL and T are both accepted.
    if (stream_val.isNil() or stream_val.isT()) return Value.nil;
    if (!stream_val.isStream()) return error.TypeError;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isOutput()) return error.NotOutputStreamError;

    return Value.nil;
}

/// Write a string to an output stream (file or string output stream)
pub fn primWriteString(_: *Heap, args: []const Value) !Value {
    if (args.len < 2) return error.InvalidArgument;

    const str_val = args[0];
    const stream_val = args[1];

    if (!str_val.isString() and !str_val.isString32()) return error.InvalidArgument;
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (!stream.isOutput()) return error.NotOutputStreamError;

    // Convert to UTF-8 bytes for output, then route through shared stream writer.
    // This keeps stream-type support centralized (stdout/stderr/broadcast/etc).
    if (str_val.isString()) {
        const str = str_val.toPtr(String);
        const data = str.data[0..str.length];
        try io.writeBytesToStream(stream_val, data);
    } else {
        // String32 - encode to UTF-8
        const s32 = str_val.toPtr(String32);
        var utf8_buf: [4096]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&utf8_buf);
        const w = fbs.writer();

        for (s32.codepoints()) |cp| {
            var cp_buf: [4]u8 = undefined;
            const len = try std.unicode.utf8Encode(@intCast(cp), &cp_buf);
            try w.writeAll(cp_buf[0..len]);
        }

        try io.writeBytesToStream(stream_val, fbs.getWritten());
    }

    return str_val;
}

/// Write bytes to a string output stream, growing buffer if needed
fn streamWriteBytes(_: *Heap, stream: *Stream, data: []const u8) !void {
    const new_len = stream.length + data.len;

    // Check if we need to grow the buffer
    if (new_len > stream.position) {
        // Grow buffer (position stores capacity for string output streams)
        const new_capacity = @max(new_len * 2, 256);
        const old_buf: ?[*]u8 = if (stream.data_ptr != 0)
            @ptrFromInt(stream.data_ptr)
        else
            null;

        const new_buf = try std.heap.page_allocator.alloc(u8, new_capacity);

        // Copy old data if any
        if (old_buf) |old| {
            @memcpy(new_buf[0..stream.length], old[0..stream.length]);
            std.heap.page_allocator.free(old[0..stream.position]);
        }

        stream.data_ptr = @intFromPtr(new_buf.ptr);
        stream.position = new_capacity;
    }

    // Append new data
    const buf: [*]u8 = @ptrFromInt(stream.data_ptr);
    @memcpy(buf[stream.length..][0..data.len], data);
    stream.length += data.len;
}

/// Get the accumulated string from a string output stream
pub fn primGetOutputStreamString(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.stream_type != .string or !stream.isOutput()) {
        return error.InvalidArgument;
    }

    if (stream.data_ptr == 0 or stream.length == 0) {
        return try heap.allocBaseString("");
    }

    const buf: [*]u8 = @ptrFromInt(stream.data_ptr);
    return try heap.allocBaseString(buf[0..stream.length]);
}
