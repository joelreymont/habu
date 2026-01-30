//! FASL (Fast Load) Format
//!
//! Binary format for compiled Habu bytecode:
//!   Magic: "HABU" (4 bytes)
//!   Version: u32 (4 bytes)
//!   Chunk count: u32 (4 bytes)
//!   For each chunk:
//!     Code length: u32
//!     Code bytes: [code_len]u8
//!     Constants count: u32
//!     Constants: [const_count]u64
//!     Arity: u8
//!     Optional count: u8
//!     Key count: u8
//!     Has rest: u8 (0 or 1)
//!     Num locals: u16
//!     Name length: u32
//!     Name: [name_len]u8

const std = @import("std");
const bytecode = @import("../bytecode/bytecode.zig");
const Chunk = bytecode.Chunk;

const MAGIC = "HABU";
const VERSION: u32 = 1;

pub const Error = error{
    InvalidMagic,
    InvalidVersion,
    InvalidFormat,
};

/// Write a chunk to a FASL file
pub fn writeChunk(allocator: std.mem.Allocator, chunk: *const Chunk, path: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    var buf_writer = std.io.bufferedWriter(file.writer());
    const writer = buf_writer.writer();

    // Write magic
    try writer.writeAll(MAGIC);

    // Write version
    try writer.writeInt(u32, VERSION, .little);

    // Write chunk count (1 for single chunk)
    try writer.writeInt(u32, 1, .little);

    // Write chunk
    try writeChunkData(writer, chunk, allocator);

    try buf_writer.flush();
}

/// Write multiple chunks to a FASL file
pub fn writeChunks(allocator: std.mem.Allocator, chunks: []const Chunk, path: []const u8) !void {
    const file = try std.fs.cwd().createFile(path, .{});
    defer file.close();

    var buf_writer = std.io.bufferedWriter(file.writer());
    const writer = buf_writer.writer();

    // Write magic
    try writer.writeAll(MAGIC);

    // Write version
    try writer.writeInt(u32, VERSION, .little);

    // Write chunk count
    try writer.writeInt(u32, @intCast(chunks.len), .little);

    // Write each chunk
    for (chunks) |*chunk| {
        try writeChunkData(writer, chunk, allocator);
    }

    try buf_writer.flush();
}

fn writeChunkData(writer: anytype, chunk: *const Chunk, allocator: std.mem.Allocator) !void {
    _ = allocator;

    // Write code length and bytes
    try writer.writeInt(u32, @intCast(chunk.code.len), .little);
    try writer.writeAll(chunk.code);

    // Write constants count and data
    try writer.writeInt(u32, @intCast(chunk.constants.len), .little);
    for (chunk.constants) |constant| {
        try writer.writeInt(u64, constant, .little);
    }

    // Write metadata
    try writer.writeInt(u8, chunk.arity, .little);
    try writer.writeInt(u8, chunk.optional_count, .little);
    try writer.writeInt(u8, chunk.key_count, .little);
    try writer.writeInt(u8, if (chunk.has_rest) 1 else 0, .little);
    try writer.writeInt(u16, chunk.num_locals, .little);

    // Write name
    try writer.writeInt(u32, @intCast(chunk.name.len), .little);
    try writer.writeAll(chunk.name);
}

/// Read chunks from a FASL file
pub fn readChunks(allocator: std.mem.Allocator, path: []const u8) ![]Chunk {
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();

    var buf_reader = std.io.bufferedReader(file.reader());
    const reader = buf_reader.reader();

    // Read and verify magic
    var magic_buf: [4]u8 = undefined;
    try reader.readNoEof(&magic_buf);
    if (!std.mem.eql(u8, &magic_buf, MAGIC)) {
        return Error.InvalidMagic;
    }

    // Read and verify version
    const version = try reader.readInt(u32, .little);
    if (version != VERSION) {
        return Error.InvalidVersion;
    }

    // Read chunk count
    const chunk_count = try reader.readInt(u32, .little);

    // Allocate chunks array
    const chunks = try allocator.alloc(Chunk, chunk_count);
    errdefer allocator.free(chunks);

    var loaded: usize = 0;
    errdefer {
        for (chunks[0..loaded]) |*prev_chunk| {
            allocator.free(prev_chunk.code);
            allocator.free(prev_chunk.constants);
            allocator.free(prev_chunk.name);
        }
        allocator.free(chunks);
    }

    // Read each chunk
    for (chunks) |*chunk| {
        chunk.* = try readChunkData(reader, allocator);
        loaded += 1;
    }

    return chunks;
}

fn readChunkData(reader: anytype, allocator: std.mem.Allocator) !Chunk {
    // Read code
    const code_len = try reader.readInt(u32, .little);
    const code = try allocator.alloc(u8, code_len);
    errdefer allocator.free(code);
    try reader.readNoEof(code);

    // Read constants
    const const_count = try reader.readInt(u32, .little);
    const constants = try allocator.alloc(u64, const_count);
    errdefer allocator.free(constants);
    for (constants) |*constant| {
        constant.* = try reader.readInt(u64, .little);
    }

    // Read metadata
    const arity = try reader.readInt(u8, .little);
    const optional_count = try reader.readInt(u8, .little);
    const key_count = try reader.readInt(u8, .little);
    const has_rest_byte = try reader.readInt(u8, .little);
    const has_rest = has_rest_byte != 0;
    const num_locals = try reader.readInt(u16, .little);

    // Read name
    const name_len = try reader.readInt(u32, .little);
    const name = try allocator.alloc(u8, name_len);
    errdefer allocator.free(name);
    try reader.readNoEof(name);

    return Chunk{
        .code = code,
        .constants = constants,
        .arity = arity,
        .optional_count = optional_count,
        .key_count = key_count,
        .has_rest = has_rest,
        .num_locals = num_locals,
        .name = name,
    };
}

/// Free chunks allocated by readChunks
pub fn freeChunks(allocator: std.mem.Allocator, chunks: []Chunk) void {
    for (chunks) |*chunk| {
        allocator.free(chunk.code);
        allocator.free(chunk.constants);
        allocator.free(chunk.name);
    }
    allocator.free(chunks);
}

test "readChunks truncated file" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();

    const filename = "bad.fasl";
    {
        const file = try tmp.dir.createFile(filename, .{});
        defer file.close();

        var writer = file.writer();
        try writer.writeAll(MAGIC);
        try writer.writeInt(u32, VERSION, .little);
        try writer.writeInt(u32, 1, .little);
        try writer.writeInt(u32, 1, .little); // code_len, but no code bytes written
    }

    const path = try tmp.dir.realpathAlloc(allocator, filename);
    defer allocator.free(path);

    try testing.expectError(error.EndOfStream, readChunks(allocator, path));
}
