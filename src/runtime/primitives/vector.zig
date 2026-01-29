//! Vector primitives
//!
//! make-vector, vector-ref, vector-set, vector-length, vector-push, etc.

const std = @import("std");
const Value = @import("../value.zig").Value;
const Tag = @import("../value.zig").Tag;
const objects = @import("../objects.zig");
const Heap = @import("../heap.zig").Heap;

/// Create a new vector
pub fn makeVector(heap: *Heap, length: usize) error{OutOfMemory, Overflow}!Value {
    return try heap.allocVector(length, length);
}

/// Create a vector with specified capacity
pub fn makeVectorWithCapacity(heap: *Heap, length: usize, capacity: usize) error{OutOfMemory, Overflow}!Value {
    return try heap.allocVector(length, capacity);
}

/// Create a vector initialized with a fill value
pub fn makeVectorFill(heap: *Heap, length: usize, fill_value: Value) error{OutOfMemory, Overflow}!Value {
    const vec = try makeVector(heap, length);
    const vec_obj = vec.toPtr(objects.Vector);

    for (0..length) |i| {
        vec_obj.data[i] = fill_value;
    }

    return vec;
}

/// Create a multi-dimensional array
/// For 1D: makeArray(heap, &[_]u64{10})
/// For 2D: makeArray(heap, &[_]u64{3, 4})
pub fn makeArray(heap: *Heap, dimensions: []const u64) error{OutOfMemory, Overflow}!Value {
    return try heap.allocArray(dimensions);
}

/// Create an array with initial fill value
pub fn makeArrayFill(heap: *Heap, dimensions: []const u64, fill_value: Value) error{OutOfMemory, Overflow}!Value {
    const arr = try makeArray(heap, dimensions);
    const arr_obj = arr.toPtr(objects.Array);
    const data: [*]Value = @ptrFromInt(arr_obj.data_ptr);

    for (0..arr_obj.total_size) |i| {
        data[i] = fill_value;
    }

    return arr;
}

/// Get array rank (number of dimensions)
pub fn arrayRank(val: Value) i64 {
    if (!val.isArray()) return -1;
    const arr = val.toPtr(objects.Array);
    return arr.rank;
}

/// Get array dimensions as a list of fixnums
pub fn arrayDimensions(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    if (!val.isArray()) return Value.nil;
    const arr = val.toPtr(objects.Array);

    // Build list in reverse
    var result = Value.nil;
    var i: usize = arr.rank;
    while (i > 0) {
        i -= 1;
        const dim = Value.makeFixnum(@intCast(arr.dimensions[i]));
        result = try heap.allocCons(dim, result);
    }

    return result;
}

/// Get dimension size for specific axis
pub fn arrayDimension(val: Value, axis: i64) ?i64 {
    if (axis < 0) return null;

    switch (val.typeKind()) {
        .vector => {
            if (axis != 0) return null;
            return @intCast(val.toPtr(objects.Vector).length);
        },
        .array => {
            const arr = val.toPtr(objects.Array);
            if (axis >= arr.rank) return null;
            return @intCast(arr.dimensions[@intCast(axis)]);
        },
        else => return null,
    }
}

/// Get total array size
pub fn arrayTotalSize(val: Value) i64 {
    if (!val.isArray()) return -1;
    const arr = val.toPtr(objects.Array);
    return @intCast(arr.total_size);
}

/// Convert subscripts to linear row-major index (public API)
pub fn arrayRowMajorIndex(val: Value, subscripts: []const u64) ?i64 {
    if (!val.isArray()) return null;
    const arr = val.toPtr(objects.Array);
    const idx = calculateRowMajorIndex(arr, subscripts) orelse return null;
    return @intCast(idx);
}

/// Calculate row-major index from subscripts
/// For a 3x4 array: subscripts [i,j] -> index i*4 + j
fn calculateRowMajorIndex(arr: *const objects.Array, subscripts: []const u64) ?usize {
    if (subscripts.len != arr.rank) return null;

    var index: usize = 0;
    var multiplier: usize = 1;

    // Process dimensions from right to left (row-major order)
    var i: usize = arr.rank;
    while (i > 0) {
        i -= 1;
        const sub = subscripts[i];
        const dim = arr.dimensions[i];

        // Bounds check
        if (sub >= dim) return null;

        const offset = std.math.mul(usize, @as(usize, @intCast(sub)), multiplier) catch return null;
        index = std.math.add(usize, index, offset) catch return null;
        multiplier = std.math.mul(usize, multiplier, @intCast(dim)) catch return null;
    }

    return index;
}

/// Get element by linear index (public API for row-major-aref)
pub fn rowMajorAref(val: Value, linear_idx: i64) ?Value {
    if (linear_idx < 0) return null;
    if (!val.isArray()) return null;
    const arr = val.toPtr(objects.Array);
    if (linear_idx >= arr.total_size) return null;
    const data: [*]Value = @ptrFromInt(arr.data_ptr);
    return data[@intCast(linear_idx)];
}

/// Get array element using row-major indexing
/// subscripts must match array rank
pub fn arrayRef(val: Value, subscripts: []const u64) ?Value {
    if (!val.isArray()) return null;
    const arr = val.toPtr(objects.Array);

    const index = calculateRowMajorIndex(arr, subscripts) orelse return null;
    const data: [*]Value = @ptrFromInt(arr.data_ptr);

    return data[index];
}

/// Set array element using row-major indexing
pub fn arraySet(val: Value, subscripts: []const u64, new_val: Value) bool {
    if (!val.isArray()) return false;
    const arr = val.toPtr(objects.Array);

    const index = calculateRowMajorIndex(arr, subscripts) orelse return false;
    const data: [*]Value = @ptrFromInt(arr.data_ptr);

    data[index] = new_val;
    return true;
}

/// Check if value is an array
pub fn arrayp(val: Value) bool {
    return val.isArray();
}

/// Check if array is adjustable (always false for now)
pub fn adjustableArrayP(val: Value) bool {
    return val.isArray() or val.isVector();
}

/// Get array displacement info (returns nil and 0 for simple arrays)
/// Returns a tuple (displaced-to, offset) as multiple values
/// For now, all arrays are simple (not displaced), so always (nil 0)
pub fn arrayDisplacement(val: Value) Value {
    if (val.isArray() or val.isVector()) {
        return Value.nil;
    }
    return Value.nil;
}

/// Adjust vector to new size
/// For vectors: resizes to new-size
/// Preserves fill-pointer and adjustable flag
/// Initializes new elements with fill_value
pub fn adjustArray(heap: *Heap, val: Value, new_size: u64, fill_value: Value) !Value {
    if (!val.isVector()) return val;

    const old_vec = val.toPtr(objects.Vector);
    const old_fp = old_vec.getFillPointer();
    const old_adj = old_vec.isAdjustable();

    // Create new vector with new size
    const new_vec = try heap.allocVector(new_size, new_size);
    const new_obj = new_vec.toPtr(objects.Vector);

    // Copy existing data
    const copy_len = @min(old_vec.length, new_size);
    @memcpy(new_obj.data[0..copy_len], old_vec.data[0..copy_len]);

    // Fill new slots with fill_value
    for (copy_len..new_size) |i| {
        new_obj.data[i] = fill_value;
    }

    // Preserve fill-pointer (clamped to new size)
    if (old_fp) |fp| {
        new_obj.setFillPointer(@min(fp, new_size));
    }

    // Preserve adjustable flag
    new_obj.setAdjustable(old_adj);

    // Update original vector to point to new storage
    old_vec.data = new_obj.data;
    old_vec.length = new_obj.length;
    old_vec.capacity = new_obj.capacity;
    old_vec.fill_pointer = new_obj.fill_pointer;

    return val;
}

/// Check if subscripts are in bounds
pub fn arrayInBoundsP(val: Value, subscripts: []const u64) bool {
    switch (val.typeKind()) {
        .vector => {
            if (subscripts.len != 1) return false;
            const vec = val.toPtr(objects.Vector);
            return subscripts[0] < vec.length;
        },
        .array => {
            const arr = val.toPtr(objects.Array);
            if (subscripts.len != arr.rank) return false;
            for (0..arr.rank) |i| {
                if (subscripts[i] >= arr.dimensions[i]) return false;
            }
            return true;
        },
        else => return false,
    }
}

/// Get array element type (always t for now - untyped arrays)
pub fn arrayElementType(val: Value) Value {
    if (val.isArray() or val.isVector()) {
        return Value.t;
    }
    return Value.nil;
}

/// Check if simple vector (non-displaced, non-adjustable, rank-1 array)
pub fn simpleVectorP(val: Value) bool {
    return val.isVector();
}

/// Check if vector (includes simple and general vectors)
pub fn vectorP(val: Value) bool {
    return val.isVector();
}

/// Check if simple bit vector (not implemented yet)
pub fn simpleBitVectorP(val: Value) bool {
    _ = val;
    return false;
}

/// Check if bit vector (not implemented yet)
pub fn bitVectorP(val: Value) bool {
    _ = val;
    return false;
}

/// Get vector length
pub fn vectorLength(val: Value) i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);
    return @intCast(vec.length);
}

/// Get element at index
pub fn vectorRef(val: Value, index: usize) Value {
    if (!val.isVector()) return Value.nil;
    const vec = val.toPtr(objects.Vector);
    if (index >= vec.length) return Value.nil;
    return vec.data[index];
}

/// Set element at index
pub fn vectorSet(val: Value, index: usize, new_val: Value) bool {
    if (!val.isVector()) return false;
    const vec = val.toPtr(objects.Vector);
    if (index >= vec.length) return false;
    vec.data[index] = new_val;
    return true;
}

/// Check if value is a vector
pub fn vectorp(val: Value) bool {
    return val.isVector();
}

/// Get vector as slice
pub fn vectorSlice(val: Value) ?[]Value {
    if (!val.isVector()) return null;
    const vec = val.toPtr(objects.Vector);
    return vec.items();
}

/// Get fill-pointer value
pub fn fillPointer(val: Value) ?i64 {
    if (!val.isVector()) return null;
    const vec = val.toPtr(objects.Vector);
    const fp = vec.getFillPointer() orelse return null;
    return @intCast(fp);
}

/// Set fill-pointer value. Returns true on success, false if val is not a vector
/// or if new_fp is out of range.
pub fn setFillPointer(val: Value, new_fp: i64) bool {
    if (!val.isVector()) return false;
    if (new_fp < 0) return false;
    const vec = val.toPtr(objects.Vector);
    const fp: u64 = @intCast(new_fp);
    if (fp > vec.capacity) return false;
    vec.setFillPointer(fp);
    return true;
}

/// Set adjustable flag on a vector. Returns true on success.
pub fn setAdjustable(val: Value, adjustable: bool) bool {
    if (!val.isVector()) return false;
    const vec = val.toPtr(objects.Vector);
    vec.setAdjustable(adjustable);
    return true;
}

/// Push element to vector (if capacity allows)
/// Returns new fill-pointer value, or -1 if failed
pub fn vectorPush(val: Value, element: Value) i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);

    const fp = vec.getFillPointer() orelse {
        // No fill-pointer: use length
        if (vec.length >= vec.capacity) return -1;
        vec.data[vec.length] = element;
        vec.length += 1;
        return @intCast(vec.length);
    };

    if (fp >= vec.capacity) return -1;
    vec.data[fp] = element;
    vec.setFillPointer(fp + 1);
    return @intCast(fp + 1);
}

/// Push element with auto-extend (adjustable vectors only)
/// Returns new fill-pointer value, or -1 if failed
/// If extension is 0, doubles capacity (or adds 1 if empty)
pub fn vectorPushExtend(heap: *Heap, val: Value, element: Value, extension: u64) !i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);

    const fp = vec.getFillPointer() orelse vec.length;

    if (fp < vec.capacity) {
        vec.data[fp] = element;
        vec.setFillPointer(fp + 1);
        return @intCast(fp);
    }

    // Need to extend - must be adjustable
    if (!vec.isAdjustable()) return -1;
    const ext = if (extension == 0) @max(vec.capacity, 1) else extension;
    const new_cap = vec.capacity + ext;

    // Allocate new vector
    const new_vec = try heap.allocVector(fp + 1, new_cap);
    const new_obj = new_vec.toPtr(objects.Vector);

    // Copy existing data
    @memcpy(new_obj.data[0..fp], vec.data[0..fp]);

    // Add new element
    new_obj.data[fp] = element;

    // Preserve fill-pointer and adjustable flag
    new_obj.setFillPointer(fp + 1);
    new_obj.setAdjustable(true);

    // Update original vector to point to new storage
    // CRITICAL: This modifies the Vector struct in place
    vec.data = new_obj.data;
    vec.length = new_obj.length;
    vec.capacity = new_obj.capacity;
    vec.setFillPointer(fp + 1);

    return @intCast(fp);
}

/// Pop element from vector
/// Returns the popped element, or nil if empty
pub fn vectorPop(val: Value) Value {
    if (!val.isVector()) return Value.nil;
    const vec = val.toPtr(objects.Vector);

    if (vec.getFillPointer()) |fp| {
        if (fp == 0) return Value.nil;
        vec.setFillPointer(fp - 1);
        return vec.data[fp - 1];
    } else {
        if (vec.length == 0) return Value.nil;
        vec.length -= 1;
        return vec.data[vec.length];
    }
}

/// Fill sequence with a value (destructive)
/// start and end are indices (0-based)
/// Returns true on success
pub fn fill(seq: Value, fill_value: Value, start: usize, end: ?usize) bool {
    switch (seq.typeKind()) {
        .vector => {
            const vec = seq.toPtr(objects.Vector);
            const len = vec.length;
            const e = end orelse len;
            if (start >= len or e > len or start > e) return false;

            for (start..e) |i| {
                vec.data[i] = fill_value;
            }
            return true;
        },
        .cons, .nil => {
            var current = seq;
            var idx: usize = 0;
            const e = end orelse std.math.maxInt(usize);

            while (!current.isNil()) {
                if (!current.isCons()) return false;

                if (idx >= start and idx < e) {
                    const cons_obj = current.toPtr(objects.Cons);
                    cons_obj.car = fill_value;
                }

                if (idx >= e) break;

                const cons_obj = current.toPtr(objects.Cons);
                current = cons_obj.cdr;
                idx += 1;
            }

            return idx >= start;
        },
        else => return false,
    }
}

/// Fill vector with a value (convenience wrapper)
pub fn vectorFill(val: Value, fill_value: Value) bool {
    return fill(val, fill_value, 0, null);
}

/// Copy vector
pub fn vectorCopy(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    if (!val.isVector()) return Value.nil;
    const src = val.toPtr(objects.Vector);

    const new_vec = try makeVector(heap, src.length);
    const dst = new_vec.toPtr(objects.Vector);

    @memcpy(dst.data[0..src.length], src.data[0..src.length]);

    return new_vec;
}

/// Create vector from list
pub fn listToVector(heap: *Heap, list_val: Value) error{OutOfMemory, Overflow}!Value {
    const list_prim = @import("list.zig");

    // Count elements
    const len = list_prim.length(list_val);
    if (len < 0) return Value.nil;

    const vec = try makeVector(heap, @intCast(len));
    const vec_obj = vec.toPtr(objects.Vector);

    // Copy elements
    var current = list_val;
    var i: usize = 0;
    while (!current.isNil() and current.isCons()) {
        vec_obj.data[i] = list_prim.car(current);
        current = list_prim.cdr(current);
        i += 1;
    }

    return vec;
}

/// Convert vector to list
pub fn vectorToList(heap: *Heap, val: Value) error{OutOfMemory, Overflow}!Value {
    if (!val.isVector()) return Value.nil;
    const vec = val.toPtr(objects.Vector);

    const list_prim = @import("list.zig");
    var result = Value.nil;

    var i = vec.length;
    while (i > 0) {
        i -= 1;
        result = try list_prim.cons(heap, vec.data[i], result);
    }

    return result;
}

/// Vector equality (element-wise)
pub fn vectorEqual(a: Value, b: Value) bool {
    if (!a.isVector() or !b.isVector()) return false;

    const vec_a = a.toPtr(objects.Vector);
    const vec_b = b.toPtr(objects.Vector);

    if (vec_a.length != vec_b.length) return false;

    for (0..vec_a.length) |i| {
        // Simple eq comparison (identity)
        if (vec_a.data[i].raw != vec_b.data[i].raw) return false;
    }

    return true;
}

/// Find element in vector
/// Returns index or -1 if not found
pub fn vectorFind(val: Value, element: Value) i64 {
    if (!val.isVector()) return -1;
    const vec = val.toPtr(objects.Vector);

    for (0..vec.length) |i| {
        if (vec.data[i].raw == element.raw) {
            return @intCast(i);
        }
    }

    return -1;
}

/// Reverse vector in place
pub fn vectorReverse(val: Value) bool {
    if (!val.isVector()) return false;
    const vec = val.toPtr(objects.Vector);

    var i: usize = 0;
    var j: usize = vec.length;
    while (i < j) {
        j -= 1;
        const tmp = vec.data[i];
        vec.data[i] = vec.data[j];
        vec.data[j] = tmp;
        i += 1;
    }

    return true;
}

/// Replace elements from src into dst
/// Copies from src[s2..e2] to dst[s1..s1+(e2-s2)]
/// Returns true on success
pub fn replace(dst: Value, src: Value, s1: usize, e1: ?usize, s2: usize, e2: ?usize) bool {
    // Determine lengths
    const dst_len = switch (dst.typeKind()) {
        .vector => dst.toPtr(objects.Vector).length,
        .cons, .nil => blk: {
            const list_prim = @import("list.zig");
            const len = list_prim.length(dst);
            break :blk if (len < 0) return false else @as(usize, @intCast(len));
        },
        else => return false,
    };

    const src_len = switch (src.typeKind()) {
        .vector => src.toPtr(objects.Vector).length,
        .cons, .nil => blk: {
            const list_prim = @import("list.zig");
            const len = list_prim.length(src);
            break :blk if (len < 0) return false else @as(usize, @intCast(len));
        },
        else => return false,
    };

    // Resolve end indices
    const end1 = e1 orelse dst_len;
    const end2 = e2 orelse src_len;

    // Validate ranges
    if (s1 > end1 or end1 > dst_len) return false;
    if (s2 > end2 or end2 > src_len) return false;

    // Compute copy count (min of available in src and space in dst)
    const src_count = end2 - s2;
    const dst_space = end1 - s1;
    const copy_count = @min(src_count, dst_space);

    // Fast path: vector to vector
    if (dst.isVector() and src.isVector()) {
        const dst_vec = dst.toPtr(objects.Vector);
        const src_vec = src.toPtr(objects.Vector);

        // Check for overlapping ranges on same vector
        if (dst.raw == src.raw) {
            // Same vector - handle overlap
            if (s1 < s2) {
                // Copy forward
                for (0..copy_count) |i| {
                    dst_vec.data[s1 + i] = src_vec.data[s2 + i];
                }
            } else if (s1 > s2) {
                // Copy backward
                var i = copy_count;
                while (i > 0) {
                    i -= 1;
                    dst_vec.data[s1 + i] = src_vec.data[s2 + i];
                }
            }
            // s1 == s2: no-op
        } else {
            // Different vectors - simple copy
            for (0..copy_count) |i| {
                dst_vec.data[s1 + i] = src_vec.data[s2 + i];
            }
        }
        return true;
    }

    // Mixed or list sequences
    const list_prim = @import("list.zig");

    // Collect source elements
    var src_items: [512]Value = undefined;
    var idx: usize = 0;
    switch (src.typeKind()) {
        .vector => {
            const src_vec = src.toPtr(objects.Vector);
            for (s2..s2 + copy_count) |i| {
                if (idx >= src_items.len) return false;
                src_items[idx] = src_vec.data[i];
                idx += 1;
            }
        },
        .cons, .nil => {
            var curr = src;
            var pos: usize = 0;
            while (pos < s2 and !curr.isNil()) {
                if (!curr.isCons()) return false;
                curr = list_prim.cdr(curr);
                pos += 1;
            }
            while (pos < s2 + copy_count and !curr.isNil()) {
                if (!curr.isCons()) return false;
                if (idx >= src_items.len) return false;
                src_items[idx] = list_prim.car(curr);
                curr = list_prim.cdr(curr);
                idx += 1;
                pos += 1;
            }
        },
        else => return false,
    }

    // Write to destination
    switch (dst.typeKind()) {
        .vector => {
            const dst_vec = dst.toPtr(objects.Vector);
            for (0..idx) |i| {
                dst_vec.data[s1 + i] = src_items[i];
            }
        },
        .cons, .nil => {
            var curr = dst;
            var pos: usize = 0;
            while (pos < s1 and !curr.isNil()) {
                if (!curr.isCons()) return false;
                curr = list_prim.cdr(curr);
                pos += 1;
            }
            for (0..idx) |i| {
                if (!curr.isCons()) return false;
                const cons = curr.toPtr(objects.Cons);
                cons.car = src_items[i];
                curr = cons.cdr;
                pos += 1;
            }
        },
        else => return false,
    }

    return true;
}

/// Search for subsequence in sequence
/// Returns starting index if found, or null
pub fn search(seq: Value, pat: Value, start: usize, end: ?usize, from_end: bool) ?usize {
    // Get lengths
    const seq_len = switch (seq.typeKind()) {
        .vector => seq.toPtr(objects.Vector).length,
        .cons, .nil => blk: {
            const list_prim = @import("list.zig");
            const len = list_prim.length(seq);
            break :blk if (len < 0) return null else @as(usize, @intCast(len));
        },
        else => return null,
    };

    const pat_len = switch (pat.typeKind()) {
        .vector => pat.toPtr(objects.Vector).length,
        .cons, .nil => blk: {
            const list_prim = @import("list.zig");
            const len = list_prim.length(pat);
            break :blk if (len < 0) return null else @as(usize, @intCast(len));
        },
        else => return null,
    };

    const e = end orelse seq_len;
    if (start > e or e > seq_len) return null;

    // Empty pattern always matches at start
    if (pat_len == 0) return if (from_end) e else start;

    // Pattern longer than search range
    if (pat_len > e - start) return null;

    const search_len = e - start - pat_len + 1;

    if (from_end) {
        var pos = search_len;
        while (pos > 0) {
            pos -= 1;
            if (matchesAt(seq, pat, start + pos, pat_len)) {
                return start + pos;
            }
        }
    } else {
        for (0..search_len) |i| {
            if (matchesAt(seq, pat, start + i, pat_len)) {
                return start + i;
            }
        }
    }

    return null;
}

fn matchesAt(seq: Value, pat: Value, seq_pos: usize, pat_len: usize) bool {
    const list_prim = @import("list.zig");

    // Fast path: both vectors
    if (seq.isVector() and pat.isVector()) {
        const seq_vec = seq.toPtr(objects.Vector);
        const pat_vec = pat.toPtr(objects.Vector);
        for (0..pat_len) |i| {
            if (seq_vec.data[seq_pos + i].raw != pat_vec.data[i].raw) return false;
        }
        return true;
    }

    // Mixed: collect elements and compare
    for (0..pat_len) |i| {
        const s_val = switch (seq.typeKind()) {
            .vector => seq.toPtr(objects.Vector).data[seq_pos + i],
            .cons, .nil => list_prim.nth(seq, @intCast(seq_pos + i)),
            else => return false,
        };

        const p_val = switch (pat.typeKind()) {
            .vector => pat.toPtr(objects.Vector).data[i],
            .cons, .nil => list_prim.nth(pat, @intCast(i)),
            else => return false,
        };

        if (s_val.raw != p_val.raw) return false;
    }

    return true;
}

/// Find first position where sequences differ
/// Returns index in seq1 where mismatch occurs, or null if equal
pub fn mismatch(s1: Value, s2: Value, s1_start: usize, s1_end: ?usize, s2_start: usize, s2_end: ?usize, from_end: bool) ?usize {
    // Get lengths
    const s1_len = switch (s1.typeKind()) {
        .vector => s1.toPtr(objects.Vector).length,
        .cons, .nil => blk: {
            const list_prim = @import("list.zig");
            const len = list_prim.length(s1);
            break :blk if (len < 0) return null else @as(usize, @intCast(len));
        },
        else => return null,
    };

    const s2_len = switch (s2.typeKind()) {
        .vector => s2.toPtr(objects.Vector).length,
        .cons, .nil => blk: {
            const list_prim = @import("list.zig");
            const len = list_prim.length(s2);
            break :blk if (len < 0) return null else @as(usize, @intCast(len));
        },
        else => return null,
    };

    const e1 = s1_end orelse s1_len;
    const e2 = s2_end orelse s2_len;

    if (s1_start > e1 or e1 > s1_len) return null;
    if (s2_start > e2 or e2 > s2_len) return null;

    const cmp_len = @min(e1 - s1_start, e2 - s2_start);

    if (from_end) {
        var i = cmp_len;
        while (i > 0) {
            i -= 1;
            if (!elemEq(s1, s1_start + i, s2, s2_start + i)) {
                return s1_start + i;
            }
        }
    } else {
        for (0..cmp_len) |i| {
            if (!elemEq(s1, s1_start + i, s2, s2_start + i)) {
                return s1_start + i;
            }
        }
    }

    // All compared elements equal - check if lengths differ
    if ((e1 - s1_start) != (e2 - s2_start)) {
        return if (from_end) s1_start else s1_start + cmp_len;
    }

    return null;
}

fn elemEq(s: Value, idx: usize, s2: Value, idx2: usize) bool {
    const list_prim = @import("list.zig");

    const v1 = switch (s.typeKind()) {
        .vector => s.toPtr(objects.Vector).data[idx],
        .cons, .nil => list_prim.nth(s, @intCast(idx)),
        else => return false,
    };

    const v2 = switch (s2.typeKind()) {
        .vector => s2.toPtr(objects.Vector).data[idx2],
        .cons, .nil => list_prim.nth(s2, @intCast(idx2)),
        else => return false,
    };

    return v1.raw == v2.raw;
}

// ============================================================================
// Tests
// ============================================================================

test "make vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 5);

    try testing.expect(vectorp(vec));
    try testing.expectEqual(@as(i64, 5), vectorLength(vec));

    // All elements should be nil
    for (0..5) |i| {
        try testing.expect(vectorRef(vec, i).isNil());
    }
}

test "make vector overflow" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const cap = std.math.maxInt(usize) / @sizeOf(Value) + 1;
    try testing.expectError(error.Overflow, makeVectorWithCapacity(&heap, 0, cap));
}

test "vector ref and set" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);

    try testing.expect(vectorSet(vec, 0, Value.makeFixnum(10)));
    try testing.expect(vectorSet(vec, 1, Value.makeFixnum(20)));
    try testing.expect(vectorSet(vec, 2, Value.makeFixnum(30)));

    try testing.expectEqual(@as(i64, 10), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(vec, 2).toFixnum());

    // Out of bounds
    try testing.expect(vectorRef(vec, 3).isNil());
    try testing.expect(!vectorSet(vec, 3, Value.makeFixnum(40)));
}

test "vector with fill" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVectorFill(&heap, 5, Value.makeFixnum(42));

    for (0..5) |i| {
        try testing.expectEqual(@as(i64, 42), vectorRef(vec, i).toFixnum());
    }
}

test "vector push and pop" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVectorWithCapacity(&heap, 0, 5);

    try testing.expectEqual(@as(i64, 1), vectorPush(vec, Value.makeFixnum(10)));
    try testing.expectEqual(@as(i64, 2), vectorPush(vec, Value.makeFixnum(20)));
    try testing.expectEqual(@as(i64, 3), vectorPush(vec, Value.makeFixnum(30)));

    try testing.expectEqual(@as(i64, 3), vectorLength(vec));

    try testing.expectEqual(@as(i64, 30), vectorPop(vec).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorPop(vec).toFixnum());
    try testing.expectEqual(@as(i64, 1), vectorLength(vec)); // 3 - 2 = 1
}

test "vector copy" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));

    const copy = try vectorCopy(&heap, vec);

    // Values should be equal
    try testing.expect(vectorEqual(vec, copy));

    // But different memory
    _ = vectorSet(copy, 0, Value.makeFixnum(100));
    try testing.expect(!vectorEqual(vec, copy));
}

test "list to vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const list_prim = @import("list.zig");
    const lst = try list_prim.list(&heap, &[_]Value{
        Value.makeFixnum(10),
        Value.makeFixnum(20),
        Value.makeFixnum(30),
    });

    const vec = try listToVector(&heap, lst);

    try testing.expectEqual(@as(i64, 3), vectorLength(vec));
    try testing.expectEqual(@as(i64, 10), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(vec, 2).toFixnum());
}

test "vector to list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));

    const lst = try vectorToList(&heap, vec);

    const list_prim = @import("list.zig");
    try testing.expectEqual(@as(i64, 3), list_prim.length(lst));
    try testing.expectEqual(@as(i64, 1), list_prim.nth(lst, 0).toFixnum());
    try testing.expectEqual(@as(i64, 2), list_prim.nth(lst, 1).toFixnum());
    try testing.expectEqual(@as(i64, 3), list_prim.nth(lst, 2).toFixnum());
}

test "vector reverse" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 4);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));
    _ = vectorSet(vec, 3, Value.makeFixnum(4));

    try testing.expect(vectorReverse(vec));

    try testing.expectEqual(@as(i64, 4), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 3), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 2), vectorRef(vec, 2).toFixnum());
    try testing.expectEqual(@as(i64, 1), vectorRef(vec, 3).toFixnum());
}

test "vector find" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 3);
    _ = vectorSet(vec, 0, Value.makeFixnum(10));
    _ = vectorSet(vec, 1, Value.makeFixnum(20));
    _ = vectorSet(vec, 2, Value.makeFixnum(30));

    try testing.expectEqual(@as(i64, 0), vectorFind(vec, Value.makeFixnum(10)));
    try testing.expectEqual(@as(i64, 1), vectorFind(vec, Value.makeFixnum(20)));
    try testing.expectEqual(@as(i64, 2), vectorFind(vec, Value.makeFixnum(30)));
    try testing.expectEqual(@as(i64, -1), vectorFind(vec, Value.makeFixnum(40)));
}

test "vector push extend" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVectorWithCapacity(&heap, 0, 2);
    const vec_obj = vec.toPtr(objects.Vector);
    vec_obj.setFillPointer(0);
    vec_obj.setAdjustable(true);

    try testing.expectEqual(@as(i64, 0), try vectorPushExtend(&heap, vec, Value.makeFixnum(10), 0));
    try testing.expectEqual(@as(i64, 1), try vectorPushExtend(&heap, vec, Value.makeFixnum(20), 0));

    // Should not extend yet
    try testing.expectEqual(@as(u64, 2), vec_obj.capacity);

    // This should trigger extension (doubles capacity: 2 -> 4)
    try testing.expectEqual(@as(i64, 2), try vectorPushExtend(&heap, vec, Value.makeFixnum(30), 0));
    try testing.expectEqual(@as(u64, 4), vec_obj.capacity);

    // Verify data
    try testing.expectEqual(@as(i64, 10), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(vec, 2).toFixnum());

    // Test explicit extension
    try testing.expectEqual(@as(i64, 3), try vectorPushExtend(&heap, vec, Value.makeFixnum(40), 10));
    try testing.expectEqual(@as(u64, 4), vec_obj.capacity); // Still 4 (didn't need to extend)

    try testing.expectEqual(@as(i64, 4), try vectorPushExtend(&heap, vec, Value.makeFixnum(50), 0));
    try testing.expectEqual(@as(u64, 8), vec_obj.capacity); // Extended again (4 -> 8)
}

test "vector push extend non-adjustable" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVectorWithCapacity(&heap, 0, 2);
    const vec_obj = vec.toPtr(objects.Vector);
    vec_obj.setFillPointer(0);
    // Not adjustable

    // Should succeed without extending
    try testing.expectEqual(@as(i64, 0), try vectorPushExtend(&heap, vec, Value.makeFixnum(10), 0));

    // Non-adjustable vector returns error when full
    _ = try vectorPushExtend(&heap, vec, Value.makeFixnum(20), 0);
    try testing.expectEqual(@as(i64, -1), try vectorPushExtend(&heap, vec, Value.makeFixnum(30), 0));
}

test "fill vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 5);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));
    _ = vectorSet(vec, 3, Value.makeFixnum(4));
    _ = vectorSet(vec, 4, Value.makeFixnum(5));

    // Fill entire vector
    try testing.expect(fill(vec, Value.makeFixnum(99), 0, null));
    for (0..5) |i| {
        try testing.expectEqual(@as(i64, 99), vectorRef(vec, i).toFixnum());
    }

    // Partial fill
    try testing.expect(fill(vec, Value.makeFixnum(42), 1, 4));
    try testing.expectEqual(@as(i64, 99), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 42), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 42), vectorRef(vec, 2).toFixnum());
    try testing.expectEqual(@as(i64, 42), vectorRef(vec, 3).toFixnum());
    try testing.expectEqual(@as(i64, 99), vectorRef(vec, 4).toFixnum());
}

test "fill list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const list_prim = @import("list.zig");
    const lst = try list_prim.list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
        Value.makeFixnum(4),
        Value.makeFixnum(5),
    });

    // Fill entire list
    try testing.expect(fill(lst, Value.makeFixnum(99), 0, null));
    for (0..5) |i| {
        try testing.expectEqual(@as(i64, 99), list_prim.nth(lst, @intCast(i)).toFixnum());
    }

    // Partial fill
    const lst2 = try list_prim.list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
        Value.makeFixnum(4),
        Value.makeFixnum(5),
    });
    try testing.expect(fill(lst2, Value.makeFixnum(42), 1, 4));
    try testing.expectEqual(@as(i64, 1), list_prim.nth(lst2, 0).toFixnum());
    try testing.expectEqual(@as(i64, 42), list_prim.nth(lst2, 1).toFixnum());
    try testing.expectEqual(@as(i64, 42), list_prim.nth(lst2, 2).toFixnum());
    try testing.expectEqual(@as(i64, 42), list_prim.nth(lst2, 3).toFixnum());
    try testing.expectEqual(@as(i64, 5), list_prim.nth(lst2, 4).toFixnum());
}

test "replace vector to vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const src = try makeVector(&heap, 4);
    _ = vectorSet(src, 0, Value.makeFixnum(10));
    _ = vectorSet(src, 1, Value.makeFixnum(20));
    _ = vectorSet(src, 2, Value.makeFixnum(30));
    _ = vectorSet(src, 3, Value.makeFixnum(40));

    const dst = try makeVector(&heap, 6);
    _ = vectorSet(dst, 0, Value.makeFixnum(1));
    _ = vectorSet(dst, 1, Value.makeFixnum(2));
    _ = vectorSet(dst, 2, Value.makeFixnum(3));
    _ = vectorSet(dst, 3, Value.makeFixnum(4));
    _ = vectorSet(dst, 4, Value.makeFixnum(5));
    _ = vectorSet(dst, 5, Value.makeFixnum(6));

    // Replace dst[1..4] with src[0..3]
    try testing.expect(replace(dst, src, 1, 4, 0, 3));
    try testing.expectEqual(@as(i64, 1), vectorRef(dst, 0).toFixnum());
    try testing.expectEqual(@as(i64, 10), vectorRef(dst, 1).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(dst, 2).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(dst, 3).toFixnum());
    try testing.expectEqual(@as(i64, 5), vectorRef(dst, 4).toFixnum());
    try testing.expectEqual(@as(i64, 6), vectorRef(dst, 5).toFixnum());
}

test "replace overlapping same vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const vec = try makeVector(&heap, 6);
    _ = vectorSet(vec, 0, Value.makeFixnum(1));
    _ = vectorSet(vec, 1, Value.makeFixnum(2));
    _ = vectorSet(vec, 2, Value.makeFixnum(3));
    _ = vectorSet(vec, 3, Value.makeFixnum(4));
    _ = vectorSet(vec, 4, Value.makeFixnum(5));
    _ = vectorSet(vec, 5, Value.makeFixnum(6));

    // Shift right: copy [0..3] to [2..5]
    try testing.expect(replace(vec, vec, 2, 5, 0, 3));
    try testing.expectEqual(@as(i64, 1), vectorRef(vec, 0).toFixnum());
    try testing.expectEqual(@as(i64, 2), vectorRef(vec, 1).toFixnum());
    try testing.expectEqual(@as(i64, 1), vectorRef(vec, 2).toFixnum());
    try testing.expectEqual(@as(i64, 2), vectorRef(vec, 3).toFixnum());
    try testing.expectEqual(@as(i64, 3), vectorRef(vec, 4).toFixnum());
    try testing.expectEqual(@as(i64, 6), vectorRef(vec, 5).toFixnum());
}

test "replace list to vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const list_prim = @import("list.zig");
    const src = try list_prim.list(&heap, &[_]Value{
        Value.makeFixnum(10),
        Value.makeFixnum(20),
        Value.makeFixnum(30),
    });

    const dst = try makeVector(&heap, 5);
    _ = vectorSet(dst, 0, Value.makeFixnum(1));
    _ = vectorSet(dst, 1, Value.makeFixnum(2));
    _ = vectorSet(dst, 2, Value.makeFixnum(3));
    _ = vectorSet(dst, 3, Value.makeFixnum(4));
    _ = vectorSet(dst, 4, Value.makeFixnum(5));

    try testing.expect(replace(dst, src, 1, 4, 0, 3));
    try testing.expectEqual(@as(i64, 1), vectorRef(dst, 0).toFixnum());
    try testing.expectEqual(@as(i64, 10), vectorRef(dst, 1).toFixnum());
    try testing.expectEqual(@as(i64, 20), vectorRef(dst, 2).toFixnum());
    try testing.expectEqual(@as(i64, 30), vectorRef(dst, 3).toFixnum());
    try testing.expectEqual(@as(i64, 5), vectorRef(dst, 4).toFixnum());
}

test "replace vector to list" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const list_prim = @import("list.zig");

    const src = try makeVector(&heap, 3);
    _ = vectorSet(src, 0, Value.makeFixnum(10));
    _ = vectorSet(src, 1, Value.makeFixnum(20));
    _ = vectorSet(src, 2, Value.makeFixnum(30));

    const dst = try list_prim.list(&heap, &[_]Value{
        Value.makeFixnum(1),
        Value.makeFixnum(2),
        Value.makeFixnum(3),
        Value.makeFixnum(4),
        Value.makeFixnum(5),
    });

    try testing.expect(replace(dst, src, 1, 4, 0, 3));
    try testing.expectEqual(@as(i64, 1), list_prim.nth(dst, 0).toFixnum());
    try testing.expectEqual(@as(i64, 10), list_prim.nth(dst, 1).toFixnum());
    try testing.expectEqual(@as(i64, 20), list_prim.nth(dst, 2).toFixnum());
    try testing.expectEqual(@as(i64, 30), list_prim.nth(dst, 3).toFixnum());
    try testing.expectEqual(@as(i64, 5), list_prim.nth(dst, 4).toFixnum());
}

test "search vector in vector" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const seq = try makeVector(&heap, 6);
    _ = vectorSet(seq, 0, Value.makeFixnum(1));
    _ = vectorSet(seq, 1, Value.makeFixnum(2));
    _ = vectorSet(seq, 2, Value.makeFixnum(3));
    _ = vectorSet(seq, 3, Value.makeFixnum(2));
    _ = vectorSet(seq, 4, Value.makeFixnum(3));
    _ = vectorSet(seq, 5, Value.makeFixnum(4));

    const pat = try makeVector(&heap, 2);
    _ = vectorSet(pat, 0, Value.makeFixnum(2));
    _ = vectorSet(pat, 1, Value.makeFixnum(3));

    // Find first occurrence
    try testing.expectEqual(@as(usize, 1), search(seq, pat, 0, null, false).?);

    // Find last occurrence
    try testing.expectEqual(@as(usize, 3), search(seq, pat, 0, null, true).?);

    // Not found
    const pat2 = try makeVector(&heap, 2);
    _ = vectorSet(pat2, 0, Value.makeFixnum(5));
    _ = vectorSet(pat2, 1, Value.makeFixnum(6));
    try testing.expect(search(seq, pat2, 0, null, false) == null);
}

test "search empty pattern" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const seq = try makeVector(&heap, 3);
    _ = vectorSet(seq, 0, Value.makeFixnum(1));
    _ = vectorSet(seq, 1, Value.makeFixnum(2));
    _ = vectorSet(seq, 2, Value.makeFixnum(3));

    const pat = try makeVector(&heap, 0);

    try testing.expectEqual(@as(usize, 0), search(seq, pat, 0, null, false).?);
    try testing.expectEqual(@as(usize, 3), search(seq, pat, 0, null, true).?);
}

test "search with range" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const seq = try makeVector(&heap, 6);
    _ = vectorSet(seq, 0, Value.makeFixnum(1));
    _ = vectorSet(seq, 1, Value.makeFixnum(2));
    _ = vectorSet(seq, 2, Value.makeFixnum(3));
    _ = vectorSet(seq, 3, Value.makeFixnum(2));
    _ = vectorSet(seq, 4, Value.makeFixnum(3));
    _ = vectorSet(seq, 5, Value.makeFixnum(4));

    const pat = try makeVector(&heap, 2);
    _ = vectorSet(pat, 0, Value.makeFixnum(2));
    _ = vectorSet(pat, 1, Value.makeFixnum(3));

    // Search in range [2..6]
    try testing.expectEqual(@as(usize, 3), search(seq, pat, 2, 6, false).?);

    // Search in range [0..3] - should only find first
    try testing.expectEqual(@as(usize, 1), search(seq, pat, 0, 3, false).?);
}

test "mismatch vectors" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const v1 = try makeVector(&heap, 5);
    _ = vectorSet(v1, 0, Value.makeFixnum(1));
    _ = vectorSet(v1, 1, Value.makeFixnum(2));
    _ = vectorSet(v1, 2, Value.makeFixnum(3));
    _ = vectorSet(v1, 3, Value.makeFixnum(4));
    _ = vectorSet(v1, 4, Value.makeFixnum(5));

    const v2 = try makeVector(&heap, 5);
    _ = vectorSet(v2, 0, Value.makeFixnum(1));
    _ = vectorSet(v2, 1, Value.makeFixnum(2));
    _ = vectorSet(v2, 2, Value.makeFixnum(9));
    _ = vectorSet(v2, 3, Value.makeFixnum(4));
    _ = vectorSet(v2, 4, Value.makeFixnum(5));

    // Mismatch at index 2
    try testing.expectEqual(@as(usize, 2), mismatch(v1, v2, 0, null, 0, null, false).?);

    // Identical sequences
    const v3 = try makeVector(&heap, 5);
    _ = vectorSet(v3, 0, Value.makeFixnum(1));
    _ = vectorSet(v3, 1, Value.makeFixnum(2));
    _ = vectorSet(v3, 2, Value.makeFixnum(3));
    _ = vectorSet(v3, 3, Value.makeFixnum(4));
    _ = vectorSet(v3, 4, Value.makeFixnum(5));

    try testing.expect(mismatch(v1, v3, 0, null, 0, null, false) == null);
}

test "mismatch different lengths" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const v1 = try makeVector(&heap, 5);
    _ = vectorSet(v1, 0, Value.makeFixnum(1));
    _ = vectorSet(v1, 1, Value.makeFixnum(2));
    _ = vectorSet(v1, 2, Value.makeFixnum(3));
    _ = vectorSet(v1, 3, Value.makeFixnum(4));
    _ = vectorSet(v1, 4, Value.makeFixnum(5));

    const v2 = try makeVector(&heap, 3);
    _ = vectorSet(v2, 0, Value.makeFixnum(1));
    _ = vectorSet(v2, 1, Value.makeFixnum(2));
    _ = vectorSet(v2, 2, Value.makeFixnum(3));

    // v1 longer than v2 - mismatch at index 3
    try testing.expectEqual(@as(usize, 3), mismatch(v1, v2, 0, null, 0, null, false).?);
}

test "mismatch from end" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const v1 = try makeVector(&heap, 5);
    _ = vectorSet(v1, 0, Value.makeFixnum(1));
    _ = vectorSet(v1, 1, Value.makeFixnum(2));
    _ = vectorSet(v1, 2, Value.makeFixnum(3));
    _ = vectorSet(v1, 3, Value.makeFixnum(4));
    _ = vectorSet(v1, 4, Value.makeFixnum(5));

    const v2 = try makeVector(&heap, 5);
    _ = vectorSet(v2, 0, Value.makeFixnum(9));
    _ = vectorSet(v2, 1, Value.makeFixnum(2));
    _ = vectorSet(v2, 2, Value.makeFixnum(3));
    _ = vectorSet(v2, 3, Value.makeFixnum(4));
    _ = vectorSet(v2, 4, Value.makeFixnum(5));

    // From end: mismatch at index 0
    try testing.expectEqual(@as(usize, 0), mismatch(v1, v2, 0, null, 0, null, true).?);
}
