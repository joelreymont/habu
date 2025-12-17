//! Cheney Copying Garbage Collector
//!
//! Algorithm:
//! 1. Copy roots to to-space
//! 2. Scan to-space, copying referenced objects
//! 3. Replace old pointers with forwarding pointers
//! 4. Swap spaces
//!
//! Forwarding pointers use tag 14 to mark already-copied objects.

const std = @import("std");
const Value = @import("value.zig").Value;
const Tag = @import("value.zig").Tag;
const objects = @import("objects.zig");
const Heap = @import("heap.zig").Heap;
const ALIGNMENT = @import("heap.zig").ALIGNMENT;

/// Garbage collector state
pub const GC = struct {
    heap: *Heap,
    /// Scan pointer (where we're reading in to-space)
    scan_ptr: [*]align(ALIGNMENT) u8,

    /// Initialize GC with heap
    pub fn init(heap: *Heap) GC {
        return .{
            .heap = heap,
            .scan_ptr = heap.to_start,
        };
    }

    /// Run a garbage collection cycle
    /// Returns the number of bytes copied
    pub fn collect(self: *GC, roots: []Value) usize {
        // Reset to-space allocation pointer
        self.scan_ptr = self.heap.to_start;
        var alloc_ptr = self.heap.to_start;

        // Phase 1: Copy roots
        for (roots) |*root| {
            root.* = self.copyValue(root.*, &alloc_ptr);
        }

        // Phase 2: Scan to-space and copy referenced objects
        while (@intFromPtr(self.scan_ptr) < @intFromPtr(alloc_ptr)) {
            const val = self.scanNextObject(&alloc_ptr);
            _ = val;
        }

        // Calculate bytes copied
        const bytes_copied = @intFromPtr(alloc_ptr) - @intFromPtr(self.heap.to_start);

        // Phase 3: Swap spaces
        self.heap.swapSpaces();
        self.heap.resetAllocPtr(@alignCast(@ptrCast(self.heap.from_start + bytes_copied)));

        // Update stats
        self.heap.stats.gc_count += 1;
        self.heap.stats.bytes_copied += bytes_copied;

        return bytes_copied;
    }

    /// Copy a value to to-space if needed
    fn copyValue(self: *GC, val: Value, alloc_ptr: *[*]align(ALIGNMENT) u8) Value {
        // Nil and fixnums don't need copying
        if (val.isNil() or val.isFixnum()) {
            return val;
        }

        // Check if already forwarded
        if (val.isForwarding()) {
            // Follow forwarding pointer
            const fwd_addr = val.toPtrAddr();
            return .{ .raw = fwd_addr | @as(u64, @intFromEnum(self.getOriginalTag(val))) };
        }

        // Check if object is in from-space
        const obj_addr = val.toPtrAddr();
        const from_start = @intFromPtr(self.heap.from_start);
        const from_end = @intFromPtr(self.heap.from_end);

        if (obj_addr < from_start or obj_addr >= from_end) {
            // Object is not in from-space (might be static), don't copy
            return val;
        }

        // Check if already has forwarding pointer
        const first_word: *Value = @ptrFromInt(obj_addr);
        if (first_word.isForwarding()) {
            // Already copied, return new address with original tag
            const new_addr = first_word.toPtrAddr();
            return .{ .raw = new_addr | @as(u64, @intFromEnum(val.getTag())) };
        }

        // Copy object to to-space
        const tag = val.getTag();
        const size = objects.objectSize(val);
        const aligned_size = std.mem.alignForward(usize, size, ALIGNMENT);

        // Copy bytes
        const dest: [*]u8 = @ptrCast(alloc_ptr.*);
        const src: [*]const u8 = @ptrFromInt(obj_addr);
        @memcpy(dest[0..size], src[0..size]);

        // Update alloc pointer
        alloc_ptr.* = @ptrFromInt(@intFromPtr(alloc_ptr.*) + aligned_size);

        // Install forwarding pointer in old location
        const new_addr = @intFromPtr(dest);
        first_word.* = Value.makeForwarding(@as(*u8, @ptrFromInt(new_addr)));

        // Return new tagged pointer
        return .{ .raw = new_addr | @as(u64, @intFromEnum(tag)) };
    }

    /// Get the original tag from a forwarding pointer
    /// (stored in bits 1-3 before forwarding)
    fn getOriginalTag(self: *GC, val: Value) Tag {
        _ = self;
        // The forwarding pointer stores the new address
        // We need to look at the destination to get the tag
        // For now, assume we stored it correctly
        return val.getTag();
    }

    /// Scan the next object in to-space and update its references
    /// For now, we scan cons cells only (16 bytes each)
    fn scanNextObject(self: *GC, alloc_ptr: *[*]align(ALIGNMENT) u8) void {
        const addr = @intFromPtr(self.scan_ptr);

        // Scan two words (car and cdr of cons cell)
        const car_ptr: *Value = @ptrFromInt(addr);
        const cdr_ptr: *Value = @ptrFromInt(addr + @sizeOf(Value));

        if (car_ptr.isPointer() and !car_ptr.isNil()) {
            car_ptr.* = self.copyValue(car_ptr.*, alloc_ptr);
        }
        if (cdr_ptr.isPointer() and !cdr_ptr.isNil()) {
            cdr_ptr.* = self.copyValue(cdr_ptr.*, alloc_ptr);
        }

        // Move scan pointer forward by cons cell size (16 bytes, maintains alignment)
        self.scan_ptr = @ptrFromInt(addr + @sizeOf(objects.Cons));
    }
};

// ============================================================================
// Root registration for conservative stack scanning
// ============================================================================

/// Root set for GC
pub const RootSet = struct {
    values: std.ArrayList(Value),

    pub fn init(allocator: std.mem.Allocator) RootSet {
        return .{
            .values = std.ArrayList(Value).init(allocator),
        };
    }

    pub fn deinit(self: *RootSet) void {
        self.values.deinit();
    }

    pub fn addRoot(self: *RootSet, val: Value) !void {
        try self.values.append(val);
    }

    pub fn clear(self: *RootSet) void {
        self.values.clearRetainingCapacity();
    }

    pub fn items(self: *RootSet) []Value {
        return self.values.items;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "gc init" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const gc_inst = GC.init(&heap);
    _ = gc_inst;
}

test "gc collect empty" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var gc = GC.init(&heap);

    var roots = [_]Value{};
    const bytes = gc.collect(&roots);

    try testing.expectEqual(@as(usize, 0), bytes);
}

test "gc collect with cons" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Allocate a cons cell
    var root = heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2)) orelse return error.OutOfMemory;

    // Verify it's valid
    try testing.expect(root.isCons());

    var gc = GC.init(&heap);

    // Collect with root
    var roots = [_]Value{root};
    const bytes = gc.collect(&roots);

    // Should have copied the cons cell
    try testing.expect(bytes >= @sizeOf(objects.Cons));

    // Root should be updated to new location
    root = roots[0];
    try testing.expect(root.isCons());

    // Values should be preserved
    const cons = root.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), cons.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), cons.cdr.toFixnum());
}
