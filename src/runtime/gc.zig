//! Cheney Copying Garbage Collector
//!
//! Algorithm:
//! 1. Copy roots to to-space, building work list
//! 2. Process work list, copying referenced objects
//! 3. Replace old pointers with forwarding pointers
//! 4. Swap spaces
//!
//! Forwarding pointers use tag 14 to mark already-copied objects.
//!
//! Work-list approach: Instead of sequential scanning (which requires knowing
//! object types), we maintain a list of (address, tag) pairs to process.

const std = @import("std");
const Value = @import("value.zig").Value;
const Tag = @import("value.zig").Tag;
const objects = @import("objects.zig");
const Heap = @import("heap.zig").Heap;
const ALIGNMENT = @import("heap.zig").ALIGNMENT;

/// Work item: object to scan
const WorkItem = struct {
    addr: usize,
    tag: Tag,
};

/// Garbage collector state
pub const GC = struct {
    heap: *Heap,
    /// Allocator for work list
    allocator: std.mem.Allocator,
    /// Work list of objects to scan
    work_list: std.ArrayList(WorkItem),

    /// Initialize GC with heap
    pub fn init(allocator: std.mem.Allocator, heap: *Heap) GC {
        return .{
            .heap = heap,
            .allocator = allocator,
            .work_list = std.ArrayList(WorkItem){},
        };
    }

    pub fn deinit(self: *GC) void {
        self.work_list.deinit(self.allocator);
    }

    /// Run a garbage collection cycle
    /// Returns the number of bytes copied, or error on OOM during work list allocation
    pub fn collect(self: *GC, roots: []Value) !usize {
        // Clear work list
        self.work_list.clearRetainingCapacity();
        var alloc_ptr = self.heap.to_start;

        // Phase 1: Copy roots
        for (roots) |*root| {
            root.* = try self.copyValue(root.*, &alloc_ptr);
        }

        // Phase 2: Process work list, scanning objects and copying references
        while (self.work_list.items.len > 0) {
            const item = self.work_list.items[self.work_list.items.len - 1];
            self.work_list.items.len -= 1;
            try self.scanObject(item.addr, item.tag, &alloc_ptr);
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
    fn copyValue(self: *GC, val: Value, alloc_ptr: *[*]align(ALIGNMENT) u8) !Value {
        // Immediates don't need copying: nil, fixnums, floats, characters
        if (val.isNil() or val.isFixnum() or val.isFloat() or val.isCharacter()) {
            return val;
        }

        // Only process actual heap pointers
        if (!val.isPointer()) {
            return val;
        }

        // Note: val should never have a forwarding tag - forwarding pointers are only
        // stored in from-space object locations, never passed as values. If this fires,
        // there's a bug elsewhere in the system.
        if (val.isForwarding()) {
            unreachable;
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

        const new_addr = @intFromPtr(dest);

        // Repair interior pointers that point to inline data
        // These pointers are relative to the object start and need adjustment
        const addr_delta: isize = @as(isize, @intCast(new_addr)) - @as(isize, @intCast(obj_addr));
        self.repairInteriorPointers(new_addr, tag, addr_delta);

        // Install forwarding pointer in old location
        first_word.* = Value.makeForwarding(@as(*u8, @ptrFromInt(new_addr)));

        // Add to work list for scanning (except strings/keywords which have no Value refs)
        if (tag != .string and tag != .keyword) {
            try self.work_list.append(self.allocator, .{
                .addr = new_addr,
                .tag = tag,
            });
        }

        // Return new tagged pointer
        return .{ .raw = new_addr | @as(u64, @intFromEnum(tag)) };
    }

    /// Repair interior pointers after copying an object
    /// Interior pointers point to inline data that follows the object header
    fn repairInteriorPointers(_: *GC, new_addr: usize, tag: Tag, addr_delta: isize) void {
        switch (tag) {
            .symbol => {
                // Symbol.name_ptr points to inline name data after header
                const sym: *objects.Symbol = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(sym.name_ptr);
                sym.name_ptr = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .keyword => {
                // Keyword.name_ptr points to inline name data after header
                const kw: *objects.Keyword = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(kw.name_ptr);
                kw.name_ptr = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .vector => {
                // Vector.data points to inline element array after header
                const vec: *objects.Vector = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(vec.data);
                vec.data = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .string => {
                // String.data points to inline byte data after header
                const str: *objects.String = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(str.data);
                str.data = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .closure => {
                // Closure.captures points to inline captures array after header
                const cls: *objects.Closure = @ptrFromInt(new_addr);
                const old_ptr = @intFromPtr(cls.captures);
                cls.captures = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
            },
            .boxed => {
                // Check discriminator to determine actual type
                const kind_ptr: *const objects.BoxedKind = @ptrFromInt(new_addr);
                switch (kind_ptr.*) {
                    .hashtable => {
                        // HashTable.entries points to inline entries array after header
                        const ht: *objects.HashTable = @ptrFromInt(new_addr);
                        const old_ptr = @intFromPtr(ht.entries);
                        ht.entries = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
                    },
                    .rational, .complex, .stream, .bignum => {
                        // No interior pointers to repair
                    },
                }
            },
            .cons, .forwarding => {
                // No interior pointers to repair
            },
        }
    }

    /// Scan an object and copy its referenced values
    fn scanObject(self: *GC, addr: usize, tag: Tag, alloc_ptr: *[*]align(ALIGNMENT) u8) !void {
        switch (tag) {
            .cons => {
                // Scan car and cdr
                const car_ptr: *Value = @ptrFromInt(addr);
                const cdr_ptr: *Value = @ptrFromInt(addr + @sizeOf(Value));

                if (car_ptr.isPointer() and !car_ptr.isNil()) {
                    car_ptr.* = try self.copyValue(car_ptr.*, alloc_ptr);
                }
                if (cdr_ptr.isPointer() and !cdr_ptr.isNil()) {
                    cdr_ptr.* = try self.copyValue(cdr_ptr.*, alloc_ptr);
                }
            },
            .symbol => {
                // Scan plist (offset 16: after name_len and name_ptr)
                const plist_ptr: *Value = @ptrFromInt(addr + 16);
                if (plist_ptr.isPointer() and !plist_ptr.isNil()) {
                    plist_ptr.* = try self.copyValue(plist_ptr.*, alloc_ptr);
                }
            },
            .vector => {
                // Scan all elements
                const vec: *objects.Vector = @ptrFromInt(addr);
                for (vec.items()) |*item| {
                    if (item.isPointer() and !item.isNil()) {
                        item.* = try self.copyValue(item.*, alloc_ptr);
                    }
                }
            },
            .closure => {
                // Scan captured values
                const cls: *objects.Closure = @ptrFromInt(addr);
                for (cls.getCapturedValues()) |*cap| {
                    if (cap.isPointer() and !cap.isNil()) {
                        cap.* = try self.copyValue(cap.*, alloc_ptr);
                    }
                }
            },
            .boxed => {
                // Check discriminator to determine actual type
                const kind_ptr: *const objects.BoxedKind = @ptrFromInt(addr);
                switch (kind_ptr.*) {
                    .hashtable => {
                        // Scan all key-value entries
                        const ht: *objects.HashTable = @ptrFromInt(addr);
                        for (ht.getEntries()) |*entry| {
                            if (!objects.HashTable.isAvailable(entry.*)) {
                                if (entry.key.isPointer() and !entry.key.isNil()) {
                                    entry.key = try self.copyValue(entry.key, alloc_ptr);
                                }
                                if (entry.value.isPointer() and !entry.value.isNil()) {
                                    entry.value = try self.copyValue(entry.value, alloc_ptr);
                                }
                            }
                        }
                    },
                    .rational, .complex, .stream, .bignum => {
                        // No Value references to scan
                    },
                }
            },
            .string, .keyword => {
                // No Value references to scan
            },
            .forwarding => {
                // Should not happen - forwarding pointers aren't added to work list
            },
        }
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

    var gc_inst = GC.init(testing.allocator, &heap);
    defer gc_inst.deinit();
}

test "gc collect empty" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    var roots = [_]Value{};
    const bytes = try gc.collect(&roots);

    try testing.expectEqual(@as(usize, 0), bytes);
}

test "gc collect with cons" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Allocate a cons cell
    var root = try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2));

    // Verify it's valid
    try testing.expect(root.isCons());

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    // Collect with root
    var roots = [_]Value{root};
    const bytes = try gc.collect(&roots);

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

test "gc collect with nested cons" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Build (1 . (2 . (3 . nil)))
    const c3 = try heap.allocCons(Value.makeFixnum(3), Value.nil);
    const c2 = try heap.allocCons(Value.makeFixnum(2), c3);
    var root = try heap.allocCons(Value.makeFixnum(1), c2);

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    var roots = [_]Value{root};
    _ = try gc.collect(&roots);

    // Verify structure is preserved
    root = roots[0];
    try testing.expect(root.isCons());
    const cons1 = root.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), cons1.car.toFixnum());

    try testing.expect(cons1.cdr.isCons());
    const cons2 = cons1.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 2), cons2.car.toFixnum());

    try testing.expect(cons2.cdr.isCons());
    const cons3 = cons2.cdr.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 3), cons3.car.toFixnum());
    try testing.expect(cons3.cdr.isNil());
}
