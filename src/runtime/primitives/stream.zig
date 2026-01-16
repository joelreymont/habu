//! File Stream Operations
const std = @import("std");
const Value = @import("../value.zig").Value;
const Heap = @import("../heap.zig").Heap;
const Vector = @import("../objects.zig").Vector;
const String = @import("../objects.zig").String;
const Stream = @import("../objects.zig").Stream;
const StreamDirection = @import("../objects.zig").StreamDirection;
const StreamType = @import("../objects.zig").StreamType;
const Symbol = @import("../objects.zig").Symbol;

pub fn primOpen(heap: *Heap, args: []const Value) !Value {
    if (args.len < 2) return error.InvalidArgument;

    const path_val = args[0];
    const mode_val = args[1];

    if (!path_val.isString()) return error.InvalidArgument;
    if (!mode_val.isSymbol()) return error.InvalidArgument;

    const path_str = path_val.toPtr(String);
    const path = path_str.data[0..path_str.length];

    const mode_sym = mode_val.toPtr(Symbol);
    const mode_name = mode_sym.getName();

    // Parse mode symbol: :read, :write, :append
    const direction: StreamDirection = blk: {
        if (std.mem.eql(u8, mode_name, "read")) break :blk .input;
        if (std.mem.eql(u8, mode_name, "write")) break :blk .output;
        if (std.mem.eql(u8, mode_name, "append")) break :blk .output;
        return error.InvalidArgument;
    };

    // Open the file with appropriate flags
    const flags: std.fs.File.OpenFlags = if (direction == .input)
        .{ .mode = .read_only }
    else if (std.mem.eql(u8, mode_name, "append"))
        .{ .mode = .write_only }
    else
        .{ .mode = .write_only };

    const file = if (direction == .input)
        try std.fs.cwd().openFile(path, flags)
    else if (std.mem.eql(u8, mode_name, "append"))
        try std.fs.cwd().createFile(path, .{ .truncate = false })
    else
        try std.fs.cwd().createFile(path, .{ .truncate = true });

    const fd = file.handle;

    return try heap.allocStream(direction, .file, fd);
}

pub fn primClose(_: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    // Don't double-close
    if (stream.closed) return Value.t;

    // Close file if it's a file stream
    if (stream.stream_type == .file and stream.file_fd >= 0) {
        const file = std.fs.File{ .handle = stream.file_fd };
        file.close();
    }

    // Mark as closed
    stream.closed = true;

    return Value.t;
}

pub fn primReadLine(heap: *Heap, args: []const Value) !Value {
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (stream.direction != .input) return error.NotInputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };

    // Simple line reading: read until newline
    var buffer: [4096]u8 = undefined;
    var index: usize = 0;
    while (index < buffer.len) {
        var byte: [1]u8 = undefined;
        const bytes_read = file.read(&byte) catch |err| switch (err) {
            else => return err,
        };
        if (bytes_read == 0) {
            // EOF
            if (index == 0) return Value.nil;
            break;
        }
        if (byte[0] == '\n') break;
        buffer[index] = byte[0];
        index += 1;
    }

    return try heap.allocString(buffer[0..index]);
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
    if (stream.direction != .output) return error.NotOutputStreamError;
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
    if (stream.direction != .input) return error.NotInputStreamError;
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
    if (stream.direction != .output) return error.NotOutputStreamError;
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
    if (stream.direction != .input) return error.NotInputStreamError;
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
    if (stream.direction != .output) return error.NotOutputStreamError;
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
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (stream.direction != .output) return error.NotOutputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };
    try file.sync();

    return Value.nil;
}

pub fn primForceOutput(heap: *Heap, args: []const Value) !Value {
    _ = heap;
    if (args.len < 1) return error.InvalidArgument;

    const stream_val = args[0];
    if (!stream_val.isStream()) return error.InvalidArgument;

    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (stream.direction != .output) return error.NotOutputStreamError;
    if (stream.stream_type != .file) return error.InvalidArgument;

    const file = std.fs.File{ .handle = stream.file_fd };
    try file.sync();

    return Value.nil;
}

/// Write a string to an output stream (file or string output stream)
pub fn primWriteString(heap: *Heap, args: []const Value) !Value {
    if (args.len < 2) return error.InvalidArgument;

    const str_val = args[0];
    const stream_val = args[1];

    if (!str_val.isString()) return error.InvalidArgument;
    if (!stream_val.isStream()) return error.InvalidArgument;

    const str = str_val.toPtr(String);
    const stream = stream_val.toPtr(Stream);

    if (stream.closed) return error.StreamClosed;
    if (stream.direction != .output) return error.NotOutputStreamError;

    const data = str.data[0..str.length];

    if (stream.stream_type == .file) {
        // Write to file
        const file = std.fs.File{ .handle = stream.file_fd };
        try file.writeAll(data);
    } else if (stream.stream_type == .string) {
        // Write to string output stream buffer
        try streamWriteBytes(heap, stream, data);
    } else {
        return error.InvalidArgument;
    }

    return str_val;
}

/// Write bytes to a string output stream, growing buffer if needed
fn streamWriteBytes(heap: *Heap, stream: *Stream, data: []const u8) !void {
    const new_len = stream.length + data.len;

    // Check if we need to grow the buffer
    if (new_len > stream.position) {
        // Grow buffer (position stores capacity for string output streams)
        const new_capacity = @max(new_len * 2, 256);
        const old_buf: ?[*]u8 = if (stream.data_ptr != 0)
            @ptrFromInt(stream.data_ptr)
        else
            null;

        const new_buf = try heap.backing_allocator.alloc(u8, new_capacity);

        // Copy old data if any
        if (old_buf) |old| {
            @memcpy(new_buf[0..stream.length], old[0..stream.length]);
            heap.backing_allocator.free(old[0..stream.position]);
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

    if (stream.stream_type != .string or stream.direction != .output) {
        return error.InvalidArgument;
    }

    if (stream.data_ptr == 0 or stream.length == 0) {
        return try heap.allocString("");
    }

    const buf: [*]u8 = @ptrFromInt(stream.data_ptr);
    return try heap.allocString(buf[0..stream.length]);
}
