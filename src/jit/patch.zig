//! Stencil patching - fill holes with runtime values
//!
//! Takes a stencil template and patches the holes with actual values.
//! Handles ARM64 instruction encoding for immediates and branches.

const std = @import("std");
const builtin = @import("builtin");
const darwin = if (builtin.os.tag == .macos)
    @cImport({
        @cInclude("pthread.h");
        @cInclude("libkern/OSCacheControl.h");
    })
else
    struct {};
const stencils = @import("stencils.zig");
const Stencil = stencils.Stencil;
const Hole = stencils.Hole;
const HoleType = stencils.HoleType;

pub const PatchError = error{
    OutOfMemory,
    OffsetTooLarge,
    InvalidHoleType,
    InvalidImm,
    InsufficientPatchValues,
    AccessDenied,
    Unexpected,
};

/// Executable memory allocator for JIT code
pub const CodeBuffer = struct {
    /// Raw memory (mmap'd with execute permission on real systems)
    memory: []align(std.heap.page_size_min) u8,
    /// Current write position
    pos: usize,
    /// Whether the buffer is currently writable
    writable: bool,

    pub fn init(allocator: std.mem.Allocator, size: usize) !CodeBuffer {
        _ = allocator;
        const len = std.mem.alignForward(usize, size, std.heap.page_size_min);
        var flags: std.posix.MAP = .{ .TYPE = .PRIVATE, .ANONYMOUS = true };
        if (@hasField(@TypeOf(flags), "JIT")) {
            flags.JIT = true;
        }
        const prot: u32 = if (builtin.os.tag == .macos)
            std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC
        else
            std.posix.PROT.READ | std.posix.PROT.WRITE;
        const memory = try std.posix.mmap(null, len, prot, flags, -1, 0);
        var buffer = CodeBuffer{
            .memory = memory,
            .pos = 0,
            .writable = false,
        };
        try buffer.setWritable(true);
        return buffer;
    }

    pub fn deinit(self: *CodeBuffer) void {
        std.posix.munmap(self.memory);
    }

    /// Get current write position as function pointer
    pub fn currentAddr(self: *const CodeBuffer) usize {
        return @intFromPtr(self.memory.ptr) + self.pos;
    }

    /// Reserve space and return slice to write into
    pub fn reserve(self: *CodeBuffer, size: usize) ![]u8 {
        if (self.pos + size > self.memory.len) {
            return error.OutOfMemory;
        }
        const start = self.pos;
        self.pos += size;
        return self.memory[start..self.pos];
    }

    /// Write bytes directly
    pub fn write(self: *CodeBuffer, bytes: []const u8) !void {
        const dest = try self.reserve(bytes.len);
        try self.setWritable(true);
        @memcpy(dest, bytes);
    }

    pub fn setWritable(self: *CodeBuffer, enable: bool) PatchError!void {
        if (self.writable == enable) return;
        if (builtin.os.tag == .macos) {
            darwin.pthread_jit_write_protect_np(@intFromBool(!enable));
        } else {
            const prot: u32 = if (enable)
                std.posix.PROT.READ | std.posix.PROT.WRITE
            else
                std.posix.PROT.READ | std.posix.PROT.EXEC;
            try std.posix.mprotect(self.memory, prot);
        }
        self.writable = enable;
    }

    /// Get a function pointer to the code at offset
    pub fn getFnPtr(self: *const CodeBuffer, comptime T: type, offset: usize) T {
        const addr = @intFromPtr(self.memory.ptr) + offset;
        return @ptrFromInt(addr);
    }
};

/// Patch a stencil and write to code buffer
pub fn patchStencil(
    buffer: *CodeBuffer,
    stencil: Stencil,
    values: []const PatchValue,
) PatchError!usize {
    const start = buffer.pos;

    // Copy stencil code
    try buffer.setWritable(true);
    const dest = try buffer.reserve(stencil.code.len);
    @memcpy(dest, stencil.code);

    // Apply patches
    if (values.len < stencil.holes.len) return error.InsufficientPatchValues;
    for (stencil.holes, 0..) |hole, i| {
        const value = values[i];
        try applyPatch(dest, hole, value, start, buffer);
    }

    flushIcache(dest.ptr, dest.len);

    return start;
}

/// Value to patch into a hole
pub const PatchValue = union(enum) {
    /// 64-bit immediate
    imm64: u64,
    /// 32-bit immediate
    imm32: u32,
    /// Absolute address (will be converted to relative)
    addr: usize,
};

/// Apply a single patch to code
fn applyPatch(
    code: []u8,
    hole: Hole,
    value: PatchValue,
    code_start: usize,
    buffer: *const CodeBuffer,
) PatchError!void {
    switch (hole.hole_type) {
        .imm64 => {
            const imm = switch (value) {
                .imm64 => |v| v,
                .addr => |a| @as(u64, a),
                else => return error.InvalidHoleType,
            };
            // Patch MOVZ/MOVK sequence (4 instructions)
            patchMovzMovk(code[hole.offset..], imm);
        },
        .imm32 => {
            const imm = switch (value) {
                .imm32 => |v| v,
                .imm64 => |v| blk: {
                    if (v > std.math.maxInt(u32)) return error.InvalidImm;
                    break :blk @as(u32, @intCast(v));
                },
                else => return error.InvalidHoleType,
            };
            // Patch 32-bit immediate in instruction
            try patchImm32(code[hole.offset..], imm);
        },
        .rel26 => {
            const target = switch (value) {
                .addr => |a| a,
                else => return error.InvalidHoleType,
            };
            // Calculate relative offset from instruction
            const inst_addr = @intFromPtr(buffer.memory.ptr) + code_start + hole.offset;
            const offset = @as(i64, @intCast(target)) - @as(i64, @intCast(inst_addr));

            // Check range: 26-bit signed offset * 4 = ±128MB
            if (offset < -0x8000000 or offset > 0x7FFFFFC) {
                return error.OffsetTooLarge;
            }

            // Encode as 26-bit word offset
            const word_offset: i32 = @intCast(@divTrunc(offset, 4));
            patchRel26(code[hole.offset..], @bitCast(word_offset));
        },
        .rel19 => {
            const target = switch (value) {
                .addr => |a| a,
                else => return error.InvalidHoleType,
            };
            const inst_addr = @intFromPtr(buffer.memory.ptr) + code_start + hole.offset;
            const offset = @as(i64, @intCast(target)) - @as(i64, @intCast(inst_addr));

            // Check range: 19-bit signed offset * 4 = ±1MB
            if (offset < -0x100000 or offset > 0xFFFFC) {
                return error.OffsetTooLarge;
            }

            const word_offset: i32 = @intCast(@divTrunc(offset, 4));
            patchRel19(code[hole.offset..], @bitCast(word_offset));
        },
        .rel14 => {
            const target = switch (value) {
                .addr => |a| a,
                else => return error.InvalidHoleType,
            };
            const inst_addr = @intFromPtr(buffer.memory.ptr) + code_start + hole.offset;
            const offset = @as(i64, @intCast(target)) - @as(i64, @intCast(inst_addr));

            // Check range: 14-bit signed offset * 4 = ±32KB
            if (offset < -0x8000 or offset > 0x7FFC) {
                return error.OffsetTooLarge;
            }

            const word_offset: i32 = @intCast(@divTrunc(offset, 4));
            patchRel14(code[hole.offset..], @bitCast(word_offset));
        },
    }
}

/// Patch MOVZ/MOVK sequence for 64-bit immediate
fn patchMovzMovk(code: []u8, imm: u64) void {
    // MOVZ Xd, #imm16, LSL #0
    // MOVK Xd, #imm16, LSL #16
    // MOVK Xd, #imm16, LSL #32
    // MOVK Xd, #imm16, LSL #48

    const imm0: u16 = @truncate(imm);
    const imm16: u16 = @truncate(imm >> 16);
    const imm32: u16 = @truncate(imm >> 32);
    const imm48: u16 = @truncate(imm >> 48);

    // Read existing instructions to get register
    var inst0: u32 = std.mem.readInt(u32, code[0..4], .little);
    var inst1: u32 = std.mem.readInt(u32, code[4..8], .little);
    var inst2: u32 = std.mem.readInt(u32, code[8..12], .little);
    var inst3: u32 = std.mem.readInt(u32, code[12..16], .little);

    // Clear and set immediate fields (bits 5-20)
    inst0 = (inst0 & 0xFFE0001F) | (@as(u32, imm0) << 5);
    inst1 = (inst1 & 0xFFE0001F) | (@as(u32, imm16) << 5);
    inst2 = (inst2 & 0xFFE0001F) | (@as(u32, imm32) << 5);
    inst3 = (inst3 & 0xFFE0001F) | (@as(u32, imm48) << 5);

    std.mem.writeInt(u32, code[0..4], inst0, .little);
    std.mem.writeInt(u32, code[4..8], inst1, .little);
    std.mem.writeInt(u32, code[8..12], inst2, .little);
    std.mem.writeInt(u32, code[12..16], inst3, .little);
}

/// Patch 32-bit immediate
fn patchImm32(code: []u8, imm: u32) PatchError!void {
    if ((imm & 0x7) != 0) return error.InvalidImm;
    const imm12 = imm >> 3;
    if (imm12 > 0xFFF) return error.InvalidImm;

    var inst: u32 = std.mem.readInt(u32, code[0..4], .little);
    inst = (inst & 0xFFC003FF) | (@as(u32, imm12) << 10);
    std.mem.writeInt(u32, code[0..4], inst, .little);
}

/// Patch 26-bit relative branch offset (BL, B)
pub fn patchRel26(code: []u8, offset: u32) void {
    var inst: u32 = std.mem.readInt(u32, code[0..4], .little);
    // Clear and set offset field (bits 0-25)
    inst = (inst & 0xFC000000) | (offset & 0x03FFFFFF);
    std.mem.writeInt(u32, code[0..4], inst, .little);
}

/// Patch 19-bit relative branch offset (CBZ, CBNZ, B.cond)
pub fn patchRel19(code: []u8, offset: u32) void {
    var inst: u32 = std.mem.readInt(u32, code[0..4], .little);
    // Clear and set offset field (bits 5-23)
    inst = (inst & 0xFF00001F) | ((offset & 0x7FFFF) << 5);
    std.mem.writeInt(u32, code[0..4], inst, .little);
}

/// Patch 14-bit relative branch offset (TBZ, TBNZ)
fn patchRel14(code: []u8, offset: u32) void {
    var inst: u32 = std.mem.readInt(u32, code[0..4], .little);
    // Clear and set offset field (bits 5-18)
    inst = (inst & 0xFFF8001F) | ((offset & 0x3FFF) << 5);
    std.mem.writeInt(u32, code[0..4], inst, .little);
}

pub fn flushIcache(ptr: [*]u8, len: usize) void {
    if (len == 0) return;
    if (builtin.os.tag == .macos) {
        darwin.sys_icache_invalidate(ptr, len);
        return;
    }
    switch (builtin.cpu.arch) {
        .aarch64, .aarch64_be => {
            const line: usize = std.atomic.cacheLineForCpu(builtin.cpu);
            const start = @intFromPtr(ptr) & ~(@as(usize, line) - 1);
            const end = @intFromPtr(ptr) + len;
            var p = start;
            while (p < end) : (p += line) {
                asm volatile ("dc cvau, %[addr]"
                    :
                    : [addr] "{x0}" (p),
                    : .{ .memory = true });
            }
            asm volatile ("dsb ish");
            p = start;
            while (p < end) : (p += line) {
                asm volatile ("ic ivau, %[addr]"
                    :
                    : [addr] "{x0}" (p),
                    : .{ .memory = true });
            }
            asm volatile ("dsb ish");
            asm volatile ("isb");
        },
        else => std.atomic.compilerFence(.SeqCst),
    }
}

// ============================================================================
// Tests
// ============================================================================

test "code buffer" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var buffer = try CodeBuffer.init(allocator, 4096);
    defer buffer.deinit();

    try testing.expectEqual(@as(usize, 0), buffer.pos);

    try buffer.write(&[_]u8{ 0x01, 0x02, 0x03, 0x04 });
    try testing.expectEqual(@as(usize, 4), buffer.pos);
}

test "patch imm64" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var buffer = try CodeBuffer.init(allocator, 4096);
    defer buffer.deinit();

    _ = try patchStencil(&buffer, stencils.load_imm64, &[_]PatchValue{
        .{ .imm64 = 0x123456789ABCDEF0 },
    });

    // Verify the immediate was patched correctly
    // First instruction: MOVZ X0, #0xDEF0
    const inst0 = std.mem.readInt(u32, buffer.memory[0..4], .little);
    const imm0 = (inst0 >> 5) & 0xFFFF;
    try testing.expectEqual(@as(u32, 0xDEF0), imm0);
}

fn dummyCall(_: usize) callconv(.c) void {}

test "patch call abs" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var buffer = try CodeBuffer.init(allocator, 4096);
    defer buffer.deinit();

    const target = @intFromPtr(&dummyCall);
    _ = try patchStencil(&buffer, stencils.call_abs, &[_]PatchValue{
        .{ .imm64 = target },
    });

    const inst0 = std.mem.readInt(u32, buffer.memory[0..4], .little);
    const imm0 = (inst0 >> 5) & 0xFFFF;
    const imm0_expected: u16 = @truncate(target);
    try testing.expectEqual(@as(u32, imm0_expected), imm0);
}

test "patch imm32" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var buffer = try CodeBuffer.init(allocator, 4096);
    defer buffer.deinit();

    const offset_bytes: u32 = 16;
    _ = try patchStencil(&buffer, stencils.load_local, &[_]PatchValue{
        .{ .imm32 = offset_bytes },
    });

    const inst = std.mem.readInt(u32, buffer.memory[0..4], .little);
    const imm12 = (inst >> 10) & 0xFFF;
    try testing.expectEqual(@as(u32, offset_bytes >> 3), imm12);
}

test "patch imm32 rejects overflow" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var buffer = try CodeBuffer.init(allocator, 4096);
    defer buffer.deinit();

    const big: u64 = @as(u64, std.math.maxInt(u32)) + 1;
    try testing.expectError(error.InvalidImm, patchStencil(&buffer, stencils.load_local, &[_]PatchValue{
        .{ .imm64 = big },
    }));
}

test "patch stencil without holes" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var buffer = try CodeBuffer.init(allocator, 4096);
    defer buffer.deinit();

    const offset = try patchStencil(&buffer, stencils.ret_stencil, &[_]PatchValue{});

    try testing.expectEqual(@as(usize, 0), offset);
    try testing.expectEqual(@as(usize, 4), buffer.pos);

    // Verify RET instruction
    const inst = std.mem.readInt(u32, buffer.memory[0..4], .little);
    try testing.expectEqual(@as(u32, 0xD65F03C0), inst);
}
