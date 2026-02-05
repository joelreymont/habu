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
const heap_mod = @import("heap.zig");
const roots_mod = @import("roots.zig");

/// Work item: object to scan
const WorkItem = struct {
    addr: usize,
    tag: Tag,
};

const builtin = @import("builtin");

/// Garbage collector state
pub const GC = struct {
    heap: *heap_mod.Heap,
    /// Allocator for work list
    allocator: std.mem.Allocator,
    /// Work list of objects to scan (preallocated, reused across collections)
    work_list: std.ArrayList(WorkItem),
    /// Peak work queue size seen during the most recent collection.
    work_peak: usize,
    /// Debug: flag set during GC trace/copy phase
    gc_in_progress: if (builtin.mode == .Debug) bool else void,

    /// Initialize GC with heap
    pub fn init(allocator: std.mem.Allocator, heap: *heap_mod.Heap) GC {
        return .{
            .heap = heap,
            .allocator = allocator,
            .work_list = std.ArrayList(WorkItem){},
            .work_peak = 0,
            .gc_in_progress = if (builtin.mode == .Debug) false else {},
        };
    }

    pub fn deinit(self: *GC) void {
        self.work_list.deinit(self.allocator);
    }

    /// Calculate initial capacity for work queues based on heap size
    /// Sizing: space_size / 64 as a heuristic (1.5% of semispace)
    fn calculateInitialCapacity(self: *const GC) usize {
        const min_cap = 256;
        const cap = self.heap.space_size / 64;
        return @max(min_cap, cap);
    }

    /// Run a garbage collection cycle
    /// Returns the number of bytes copied, or error on OOM during work list allocation
    pub fn collect(self: *GC, roots: []Value) !usize {
        var ranges = [_]roots_mod.RootRange{.{ .ptr = roots.ptr, .len = roots.len }};
        return try self.collectRootSet(.{
            .ranges = ranges[0..],
            .slots = &[_]*Value{},
        });
    }

    /// Run a garbage collection cycle with a precise root set (slot/range addresses).
    /// Returns the number of bytes copied, or error on OOM during work list allocation.
    pub fn collectRootSet(self: *GC, roots: roots_mod.RootSet) !usize {
        // Preallocate work queue if first collection
        if (self.work_list.capacity == 0) {
            const init_cap = self.calculateInitialCapacity();
            try self.work_list.ensureTotalCapacity(self.allocator, init_cap);
        }

        // Set GC in-progress flag (debug only)
        if (builtin.mode == .Debug) self.gc_in_progress = true;
        defer {
            if (builtin.mode == .Debug) self.gc_in_progress = false;
        }

        // Clear work list, retaining capacity from previous collections
        self.work_list.clearRetainingCapacity();
        self.work_peak = 0;
        var alloc_ptr = self.heap.to_start;

        // Phase 1: Copy roots
        for (roots.ranges) |r| {
            for (r.ptr[0..r.len]) |*root| {
                root.* = try self.copyValue(root.*, &alloc_ptr);
            }
        }
        for (roots.slots) |slot| {
            slot.* = try self.copyValue(slot.*, &alloc_ptr);
        }

        // Phase 2: Process work list, scanning objects and copying references
        while (self.work_list.items.len > 0) {
            const item = self.work_list.items[self.work_list.items.len - 1];
            self.work_list.items.len -= 1;
            try self.scanObject(item.addr, item.tag, &alloc_ptr);
        }

        // Calculate bytes copied
        const bytes_copied = @intFromPtr(alloc_ptr) - @intFromPtr(self.heap.to_start);

        // Save old alloc_ptr before swap for finalization
        const old_alloc_ptr = self.heap.alloc_ptr;

        // Phase 3: Swap spaces
        self.heap.swapSpaces();
        self.heap.resetAllocPtr(@ptrCast(@alignCast(self.heap.from_start + bytes_copied)));

        // Phase 4: Finalize unreachable objects with resources (uses old space)
        self.finalizeUnreachable(old_alloc_ptr);

        // Update stats
        self.heap.stats.gc_count += 1;
        self.heap.stats.bytes_copied += bytes_copied;

        // Phase 5: Grow queues AFTER collection completes if needed
        try self.maybeGrowQueues();

        return bytes_copied;
    }

    /// Grow work queues after GC if they exceeded 75% capacity
    /// Growth happens AFTER GC completes to avoid allocations during trace
    fn maybeGrowQueues(self: *GC) !void {
        const work_cap = self.work_list.capacity;
        const work_peak = self.work_peak;

        // If we used >75% capacity, grow for next cycle
        if (work_peak * 4 > work_cap * 3) {
            const new_cap = work_cap * 2;
            try self.work_list.ensureTotalCapacity(self.allocator, new_cap);
        }
    }

    fn pushWork(self: *GC, addr: usize, tag: Tag) !void {
        // Debug check: detect allocations during GC trace/copy.
        if (builtin.mode == .Debug and self.gc_in_progress) {
            const old_cap = self.work_list.capacity;
            try self.work_list.append(self.allocator, .{ .addr = addr, .tag = tag });
            const new_cap = self.work_list.capacity;
            if (new_cap > old_cap) {
                std.debug.print("ERROR: work_list allocated during GC (cap: {} -> {})\n", .{ old_cap, new_cap });
                @panic("Allocation during GC detected");
            }
        } else {
            try self.work_list.append(self.allocator, .{ .addr = addr, .tag = tag });
        }

        if (self.work_list.items.len > self.work_peak) self.work_peak = self.work_list.items.len;
    }

    /// Finalize unreachable objects that hold resources (e.g., file handles)
    /// This walks the from-space and closes any open streams that weren't copied
    /// old_alloc_ptr: the alloc_ptr value BEFORE swapSpaces was called
    fn finalizeUnreachable(self: *GC, old_alloc_ptr: [*]align(heap_mod.ALIGNMENT) u8) void {
        const old_start = @intFromPtr(self.heap.to_start);
        const old_end = @intFromPtr(old_alloc_ptr);

        var i: usize = 0;
        while (i < self.heap.stream_list.items.len) {
            const stream = self.heap.stream_list.items[i];
            const addr = @intFromPtr(stream);

            if (addr >= old_start and addr < old_end) {
                const first_word: *Value = @ptrFromInt(addr);
                if (first_word.isForwarding()) {
                    const new_addr = first_word.toPtrAddr();
                    const new_start = @intFromPtr(self.heap.from_start);
                    const new_end = @intFromPtr(self.heap.from_end);
                    // Forwarding pointers must point into the new from-space.
                    if (new_addr >= new_start and new_addr < new_end) {
                        self.heap.stream_list.items[i] = @ptrFromInt(new_addr);
                        i += 1;
                        continue;
                    }
                }

                stream.finalize();
                _ = self.heap.stream_list.swapRemove(i);
            } else {
                i += 1;
            }
        }
    }

    /// Copy a value to to-space if needed
    fn copyValue(self: *GC, val: Value, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8) !Value {
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
            // Already copied, return new address with original tag.
            //
            // NOTE: Many Habu objects do not have a Value header word (e.g. strings start with
            // length). Those header words can coincidentally look like a forwarding Value, so we
            // validate that the forwarding target lands in to-space.
            const new_addr = first_word.toPtrAddr();
            const to_start = @intFromPtr(self.heap.to_start);
            const to_end = to_start + self.heap.space_size;
            if (new_addr >= to_start and new_addr < to_end) {
                return .{ .raw = new_addr | @as(u64, @intFromEnum(val.getTag())) };
            }
        }

        // Copy object to to-space
        const tag = val.getTag();
        const size = objects.objectSize(val);
        const aligned_size = std.mem.alignForward(usize, size, heap_mod.ALIGNMENT);

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
        // Store the forwarding pointer in first word and size in second word
        // This allows finalizeUnreachable to skip past forwarded objects
        first_word.* = Value.makeForwarding(@as(*u8, @ptrFromInt(new_addr)));
        const size_ptr: *usize = @ptrFromInt(obj_addr + @sizeOf(Value));
        size_ptr.* = aligned_size;

        // Add to work list for scanning (except strings/keywords which have no Value refs)
        if (tag != .string and tag != .keyword) {
            try self.pushWork(new_addr, tag);
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
                    .array => {
                        // Array.data_ptr points to data array
                        const arr: *objects.Array = @ptrFromInt(new_addr);
                        const old_ptr: usize = arr.data_ptr;
                        arr.data_ptr = @intCast(@as(isize, @intCast(old_ptr)) + addr_delta);
                    },
                    .chunk => {
                        // Chunk has two interior pointers:
                        // const_pool points to inline array after header
                        // code points to inline array after constants
                        const chunk: *objects.Chunk = @ptrFromInt(new_addr);
                        const old_const_pool = @intFromPtr(chunk.const_pool);
                        const old_code = @intFromPtr(chunk.code);
                        chunk.const_pool = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_const_pool)) + addr_delta)));
                        chunk.code = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_code)) + addr_delta)));
                    },
                    .string32 => {
                        // String32.data points to inline codepoint data after header
                        const s32: *objects.String32 = @ptrFromInt(new_addr);
                        const old_ptr = @intFromPtr(s32.data);
                        s32.data = @ptrFromInt(@as(usize, @intCast(@as(isize, @intCast(old_ptr)) + addr_delta)));
                    },
                    .hashtable, .rational, .complex, .stream, .bignum, .pathname, .package, .condition, .class, .slotdef, .generic_function, .method, .native_code => {
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
    fn scanObject(self: *GC, addr: usize, tag: Tag, alloc_ptr: *[*]align(heap_mod.ALIGNMENT) u8) !void {
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
                // Scan code Value and captured values
                const cls: *objects.Closure = @ptrFromInt(addr);
                if (cls.code.isPointer() and !cls.code.isNil()) {
                    cls.code = try self.copyValue(cls.code, alloc_ptr);
                }
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
                        // Scan entries vector (which scans keys/values transitively)
                        const ht: *objects.HashTable = @ptrFromInt(addr);
                        if (ht.entries_vec.isPointer() and !ht.entries_vec.isNil()) {
                            ht.entries_vec = try self.copyValue(ht.entries_vec, alloc_ptr);
                        }
                    },
                    .array => {
                        // Scan all array elements
                        const arr: *objects.Array = @ptrFromInt(addr);
                        const data: [*]Value = @ptrFromInt(arr.data_ptr);
                        for (0..arr.total_size) |i| {
                            if (data[i].isPointer() and !data[i].isNil()) {
                                data[i] = try self.copyValue(data[i], alloc_ptr);
                            }
                        }
                    },
                    .pathname => {
                        // Scan all pathname component values
                        const pn: *objects.Pathname = @ptrFromInt(addr);
                        if (pn.host.isPointer() and !pn.host.isNil()) {
                            pn.host = try self.copyValue(pn.host, alloc_ptr);
                        }
                        if (pn.device.isPointer() and !pn.device.isNil()) {
                            pn.device = try self.copyValue(pn.device, alloc_ptr);
                        }
                        if (pn.directory.isPointer() and !pn.directory.isNil()) {
                            pn.directory = try self.copyValue(pn.directory, alloc_ptr);
                        }
                        if (pn.name.isPointer() and !pn.name.isNil()) {
                            pn.name = try self.copyValue(pn.name, alloc_ptr);
                        }
                        if (pn.type.isPointer() and !pn.type.isNil()) {
                            pn.type = try self.copyValue(pn.type, alloc_ptr);
                        }
                        if (pn.version.isPointer() and !pn.version.isNil()) {
                            pn.version = try self.copyValue(pn.version, alloc_ptr);
                        }
                    },
                    .package => {
                        // Scan all package fields
                        const pkg: *objects.Package = @ptrFromInt(addr);
                        if (pkg.name.isPointer() and !pkg.name.isNil()) {
                            pkg.name = try self.copyValue(pkg.name, alloc_ptr);
                        }
                        if (pkg.nicknames.isPointer() and !pkg.nicknames.isNil()) {
                            pkg.nicknames = try self.copyValue(pkg.nicknames, alloc_ptr);
                        }
                        if (pkg.use_list.isPointer() and !pkg.use_list.isNil()) {
                            pkg.use_list = try self.copyValue(pkg.use_list, alloc_ptr);
                        }
                        if (pkg.exports.isPointer() and !pkg.exports.isNil()) {
                            pkg.exports = try self.copyValue(pkg.exports, alloc_ptr);
                        }
                        if (pkg.symbols.isPointer() and !pkg.symbols.isNil()) {
                            pkg.symbols = try self.copyValue(pkg.symbols, alloc_ptr);
                        }
                        if (pkg.shadowing.isPointer() and !pkg.shadowing.isNil()) {
                            pkg.shadowing = try self.copyValue(pkg.shadowing, alloc_ptr);
                        }
                    },
                    .chunk => {
                        // Scan chunk metadata + all constants in the constant pool
                        const chunk: *objects.Chunk = @ptrFromInt(addr);
                        if (chunk.lambda_expr.isPointer() and !chunk.lambda_expr.isNil()) {
                            chunk.lambda_expr = try self.copyValue(chunk.lambda_expr, alloc_ptr);
                        }
                        if (chunk.name.isPointer() and !chunk.name.isNil()) {
                            chunk.name = try self.copyValue(chunk.name, alloc_ptr);
                        }
                        for (chunk.getConstants()) |*const_val| {
                            if (const_val.isPointer() and !const_val.isNil()) {
                                const_val.* = try self.copyValue(const_val.*, alloc_ptr);
                            }
                        }
                    },
                    .rational, .complex, .bignum, .native_code => {
                        // No Value references to scan
                    },
                    .condition => {
                        // Scan condition Value references
                        const cond: *objects.Condition = @ptrFromInt(addr);
                        if (cond.type_sym.isPointer() and !cond.type_sym.isNil()) {
                            cond.type_sym = try self.copyValue(cond.type_sym, alloc_ptr);
                        }
                        if (cond.format_control.isPointer() and !cond.format_control.isNil()) {
                            cond.format_control = try self.copyValue(cond.format_control, alloc_ptr);
                        }
                        if (cond.format_args.isPointer() and !cond.format_args.isNil()) {
                            cond.format_args = try self.copyValue(cond.format_args, alloc_ptr);
                        }
                    },
                    .class => {
                        // Scan class Value references
                        const cls: *objects.Class = @ptrFromInt(addr);
                        if (cls.name.isPointer() and !cls.name.isNil()) {
                            cls.name = try self.copyValue(cls.name, alloc_ptr);
                        }
                        if (cls.direct_supers.isPointer() and !cls.direct_supers.isNil()) {
                            cls.direct_supers = try self.copyValue(cls.direct_supers, alloc_ptr);
                        }
                        if (cls.cpl.isPointer() and !cls.cpl.isNil()) {
                            cls.cpl = try self.copyValue(cls.cpl, alloc_ptr);
                        }
                        if (cls.direct_slots.isPointer() and !cls.direct_slots.isNil()) {
                            cls.direct_slots = try self.copyValue(cls.direct_slots, alloc_ptr);
                        }
                        if (cls.slots.isPointer() and !cls.slots.isNil()) {
                            cls.slots = try self.copyValue(cls.slots, alloc_ptr);
                        }
                        if (cls.metaclass.isPointer() and !cls.metaclass.isNil()) {
                            cls.metaclass = try self.copyValue(cls.metaclass, alloc_ptr);
                        }
                        for (cls.shared_slots[0..cls.num_shared]) |*slot_val| {
                            if (slot_val.isPointer() and !slot_val.isNil()) {
                                slot_val.* = try self.copyValue(slot_val.*, alloc_ptr);
                            }
                        }
                    },
                    .stream => {
                        // Scan source_value if present
                        const stream: *objects.Stream = @ptrFromInt(addr);
                        if (!stream.source_value.isNil() and stream.source_value.isPointer()) {
                            stream.source_value = try self.copyValue(stream.source_value, alloc_ptr);
                            // Recompute data_ptr from relocated string
                            if (stream.source_value.typeKind() == .string) {
                                const str = stream.source_value.toPtr(objects.String);
                                stream.data_ptr = @intFromPtr(str.data);
                            }
                        }
                    },
                    .slotdef => {
                        // Scan slot definition Value references
                        const slotdef: *objects.SlotDefinition = @ptrFromInt(addr);
                        if (slotdef.name.isPointer() and !slotdef.name.isNil()) {
                            slotdef.name = try self.copyValue(slotdef.name, alloc_ptr);
                        }
                        if (slotdef.initform.isPointer() and !slotdef.initform.isNil()) {
                            slotdef.initform = try self.copyValue(slotdef.initform, alloc_ptr);
                        }
                        if (slotdef.initargs.isPointer() and !slotdef.initargs.isNil()) {
                            slotdef.initargs = try self.copyValue(slotdef.initargs, alloc_ptr);
                        }
                        if (slotdef.readers.isPointer() and !slotdef.readers.isNil()) {
                            slotdef.readers = try self.copyValue(slotdef.readers, alloc_ptr);
                        }
                        if (slotdef.writers.isPointer() and !slotdef.writers.isNil()) {
                            slotdef.writers = try self.copyValue(slotdef.writers, alloc_ptr);
                        }
                        if (slotdef.allocation.isPointer() and !slotdef.allocation.isNil()) {
                            slotdef.allocation = try self.copyValue(slotdef.allocation, alloc_ptr);
                        }
                        if (slotdef.slot_type.isPointer() and !slotdef.slot_type.isNil()) {
                            slotdef.slot_type = try self.copyValue(slotdef.slot_type, alloc_ptr);
                        }
                    },
                    .string32 => {
                        // No Value references to scan
                    },
                    .generic_function => {
                        // Scan generic function Value references
                        const gf: *objects.GenericFunction = @ptrFromInt(addr);
                        if (gf.name.isPointer() and !gf.name.isNil()) {
                            gf.name = try self.copyValue(gf.name, alloc_ptr);
                        }
                        if (gf.lambda_list.isPointer() and !gf.lambda_list.isNil()) {
                            gf.lambda_list = try self.copyValue(gf.lambda_list, alloc_ptr);
                        }
                        if (gf.methods.isPointer() and !gf.methods.isNil()) {
                            gf.methods = try self.copyValue(gf.methods, alloc_ptr);
                        }
                        if (gf.dispatcher.isPointer() and !gf.dispatcher.isNil()) {
                            gf.dispatcher = try self.copyValue(gf.dispatcher, alloc_ptr);
                        }
                    },
                    .method => {
                        // Scan method Value references
                        const method: *objects.Method = @ptrFromInt(addr);
                        if (method.qualifiers.isPointer() and !method.qualifiers.isNil()) {
                            method.qualifiers = try self.copyValue(method.qualifiers, alloc_ptr);
                        }
                        if (method.specializers.isPointer() and !method.specializers.isNil()) {
                            method.specializers = try self.copyValue(method.specializers, alloc_ptr);
                        }
                        if (method.lambda_list.isPointer() and !method.lambda_list.isNil()) {
                            method.lambda_list = try self.copyValue(method.lambda_list, alloc_ptr);
                        }
                        if (method.function.isPointer() and !method.function.isNil()) {
                            method.function = try self.copyValue(method.function, alloc_ptr);
                        }
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
// Tests
// ============================================================================

test "gc init" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var gc_inst = GC.init(testing.allocator, &heap);
    defer gc_inst.deinit();
}

test "gc collect empty" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Use heap.collectGarbage which handles internal roots (lisp_packages)
    var roots = [_]Value{};
    _ = try heap.collectGarbage(&roots);

    // After GC, only lisp_packages hash table should remain
    try testing.expect(heap.bytesUsed() > 0);
}

test "gc collect with cons" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
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

test "gc collectRootSet updates slots" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var root = try heap.allocCons(Value.makeFixnum(1), Value.makeFixnum(2));
    const raw_before = root.raw;

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    var slots = [_]*Value{&root};
    const bytes = try gc.collectRootSet(.{ .ranges = &[_]roots_mod.RootRange{}, .slots = slots[0..] });

    try testing.expect(bytes >= @sizeOf(objects.Cons));
    try testing.expect(root.isCons());
    try testing.expect(root.raw != raw_before);

    const cons = root.toPtr(objects.Cons);
    try testing.expectEqual(@as(i64, 1), cons.car.toFixnum());
    try testing.expectEqual(@as(i64, 2), cons.cdr.toFixnum());
}

test "gc collect with nested cons" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
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

test "gc grows work_list after peak use" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    try gc.work_list.ensureTotalCapacity(testing.allocator, 32);
    const cap0 = gc.work_list.capacity;
    const n: usize = (cap0 * 3) / 4 + 1;

    const vec_val = try heap.allocVector(n, n);
    const vec = vec_val.toPtr(objects.Vector);
    for (0..n) |i| {
        const cons = try heap.allocCons(Value.makeFixnum(@as(i64, @intCast(i))), Value.makeFixnum(0));
        vec.set(i, cons);
    }

    var roots = [_]Value{vec_val};
    _ = try gc.collect(&roots);

    try testing.expectEqual(n, gc.work_peak);
    try testing.expect(gc.work_list.capacity >= cap0 * 2);
}

test "gc finalizes unreachable file streams" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const tmp_path = "/tmp/habu_gc_test_stream.txt";

    // Create and write to a file
    {
        const file = try std.fs.createFileAbsolute(tmp_path, .{});
        defer file.close();
        try file.writeAll("test data\n");
    }

    // Open file stream
    const file = try std.fs.openFileAbsolute(tmp_path, .{});
    const fd = file.handle;
    const stream = try heap.allocStream(.input, .file, fd);

    // Create a root that references the stream
    var root = stream;
    try testing.expectEqual(@as(usize, 1), heap.stream_list.items.len);

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    // First GC - stream is reachable, should not be finalized
    var roots = [_]Value{root};
    _ = try gc.collect(&roots);
    root = roots[0];
    try testing.expectEqual(@as(usize, 1), heap.stream_list.items.len);

    // Verify stream is still valid
    try testing.expect(root.isBoxed());

    // Second GC - stream becomes unreachable (empty roots)
    var empty_roots = [_]Value{};
    _ = try gc.collect(&empty_roots);
    try testing.expectEqual(@as(usize, 0), heap.stream_list.items.len);

    // File descriptor should be closed by finalizer
    // We can't directly verify the FD is closed, but we tested the finalization path
    try std.fs.deleteFileAbsolute(tmp_path);
}

test "gc finalizer path coverage" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Test that finalizeUnreachable visits stream objects
    // We don't test actual resource cleanup to avoid allocator mismatch issues
    const stream = try heap.allocStream(.input, .file, -1);
    const stream_ptr = stream.toPtr(objects.Stream);
    stream_ptr.closed = true; // Mark as closed so finalizer doesn't try to close

    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    // Stream becomes unreachable
    var empty_roots = [_]Value{};
    _ = try gc.collect(&empty_roots);

    try testing.expectEqual(@as(usize, 0), heap.stream_list.items.len);
}

test "package gc correctness" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    // Create package
    const pkg_name = try heap.intern("TEST-PKG");
    const pkg = try heap.allocPackage(pkg_name, Value.nil, Value.nil, false);

    var root = pkg;
    var gc = GC.init(testing.allocator, &heap);
    defer gc.deinit();

    // GC with package rooted
    var roots = [_]Value{root};
    _ = try gc.collect(&roots);
    root = roots[0];

    // Verify package structure intact after GC
    try testing.expect(root.isBoxed());
    const pkg_after = root.toPtr(objects.Package);
    // Note: pkg_name symbol may have been moved by GC, need to compare through symbol string
    try testing.expect(pkg_after.name.isSymbol());
    const name_after = pkg_after.name.toPtr(objects.Symbol);
    try testing.expect(std.mem.eql(u8, name_after.getName(), "TEST-PKG"));
}

test "gc scans class metaclass" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const name = try heap.intern("TEST-CLASS");
    const cls = try heap.alloc(objects.Class);
    cls.* = .{
        .kind = .class,
        .name = name,
        .direct_supers = Value.nil,
        .cpl = Value.nil,
        .direct_slots = Value.nil,
        .slots = Value.nil,
        .metaclass = heap.standard_class,
        .num_shared = 0,
        // num_shared=0 => must still be non-null per Zig pointer rules.
        .shared_slots = @ptrFromInt(@as(usize, @alignOf(Value))),
    };

    var roots = [_]Value{Value.makeClass(cls)};
    _ = try heap.collectGarbage(&roots);
    const cls_after = roots[0].toPtr(objects.Class);
    try testing.expect(cls_after.metaclass.eq(heap.standard_class));

    // Second collection should still preserve/update metaclass field.
    _ = try heap.collectGarbage(&roots);
    const cls_after2 = roots[0].toPtr(objects.Class);
    try testing.expect(cls_after2.metaclass.eq(heap.standard_class));
}

test "gc scans generic function dispatcher" {
    const testing = std.testing;

    var heap = try heap_mod.Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const name = try heap.intern("TEST-GF");
    const dispatcher = try heap.allocClosure(Value.nil, 0, &[_]Value{});

    const gf = try heap.alloc(objects.GenericFunction);
    gf.* = .{
        .kind = .generic_function,
        .name = name,
        .lambda_list = Value.nil,
        .methods = Value.nil,
        .dispatcher = dispatcher,
    };

    var roots = [_]Value{Value.makeGenericFunction(gf)};
    _ = try heap.collectGarbage(&roots);

    const gf_after = roots[0].toPtr(objects.GenericFunction);
    try testing.expect(gf_after.dispatcher.isClosure());

    // If dispatcher isn't scanned, it will still point into the old semispace.
    const disp_addr = gf_after.dispatcher.toPtrAddr();
    const start = @intFromPtr(heap.from_start);
    const end = start + heap.space_size;
    try testing.expect(disp_addr >= start and disp_addr < end);
}
