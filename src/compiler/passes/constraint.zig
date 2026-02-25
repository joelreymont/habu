//! Constraint Data Structures for Flow-Sensitive Type Narrowing
//!
//! Constraints represent proven facts about variables at specific program points.
//! Used by p07b_constrain to eliminate redundant type checks.

const std = @import("std");
const type_adt = @import("../../types/type.zig");
const Type = type_adt.Type;
const Value = @import("../../runtime/value.zig").Value;

/// A constraint represents a proven fact about a variable at a program point.
pub const Constraint = union(enum) {
    /// Variable is known to be of type T (e.g., after consp succeeds)
    type_is: *const Type,

    /// Variable is known NOT to be of type T (e.g., in else-branch of consp)
    type_not: *const Type,

    /// Variable's value is within an integer range
    range: Range,

    /// Variable equals a specific constant
    eql: Value,

    /// Variable is not nil (e.g., after non-nil check)
    non_nil: void,

    pub const Range = struct {
        lo: ?i64,
        hi: ?i64,
    };

    /// Check if this constraint implies the given type assertion is redundant.
    /// Returns true if the constraint proves the value already satisfies the type.
    pub fn proves(self: Constraint, target: *const Type) bool {
        return switch (self) {
            .type_is => |ty| ty.eql(target.*),
            .type_not => false, // Negative info can't prove positive
            .range => switch (target.*) {
                .primitive => |p| p == .fixnum,
                .integer => true, // Range implies integer
                else => false,
            },
            .eql => |val| {
                // A known constant proves its type
                if (val.isFixnum()) {
                    return switch (target.*) {
                        .primitive => |p| p == .fixnum,
                        else => false,
                    };
                }
                if (val.isNil()) {
                    return switch (target.*) {
                        .primitive => |p| p == .nil,
                        else => false,
                    };
                }
                return false;
            },
            .non_nil => switch (target.*) {
                .non_nil => true,
                else => false,
            },
        };
    }

    /// Check if this constraint contradicts the given type (proves it false).
    pub fn contradicts(self: Constraint, target: *const Type) bool {
        return switch (self) {
            .type_not => |ty| ty.eql(target.*),
            .type_is => |ty| !ty.eql(target.*) and isDisjoint(ty, target),
            .non_nil => switch (target.*) {
                .primitive => |p| p == .nil,
                else => false,
            },
            .eql => |val| {
                if (val.isNil()) {
                    return switch (target.*) {
                        .non_nil => true,
                        else => false,
                    };
                }
                return false;
            },
            .range => false,
        };
    }
};

/// Check if two primitive types are disjoint (cannot overlap).
fn isDisjoint(a: *const Type, b: *const Type) bool {
    const pa = switch (a.*) {
        .primitive => |p| p,
        else => return false,
    };
    const pb = switch (b.*) {
        .primitive => |p| p,
        else => return false,
    };
    return pa != pb;
}

/// Set of constraints for variables in scope.
/// Maps variable name → list of known constraints.
pub const ConstraintSet = struct {
    map: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(Constraint)),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) ConstraintSet {
        return .{
            .map = .{},
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ConstraintSet) void {
        var it = self.map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.deinit(self.allocator);
        }
        self.map.deinit(self.allocator);
    }

    /// Add a constraint for a variable.
    pub fn add(self: *ConstraintSet, name: []const u8, c: Constraint) !void {
        const gop = try self.map.getOrPut(self.allocator, name);
        if (!gop.found_existing) {
            gop.value_ptr.* = .{};
        }
        try gop.value_ptr.append(self.allocator, c);
    }

    /// Check if any constraint for `name` proves `target` type is satisfied.
    pub fn provesType(self: *const ConstraintSet, name: []const u8, target: *const Type) bool {
        const list = self.map.get(name) orelse return false;
        for (list.items) |c| {
            if (c.proves(target)) return true;
        }
        return false;
    }

    /// Check if any constraint for `name` contradicts `target` type.
    pub fn contradictsType(self: *const ConstraintSet, name: []const u8, target: *const Type) bool {
        const list = self.map.get(name) orelse return false;
        for (list.items) |c| {
            if (c.contradicts(target)) return true;
        }
        return false;
    }

    /// Clone this constraint set (for branching).
    pub fn clone(self: *const ConstraintSet) !ConstraintSet {
        var new = ConstraintSet.init(self.allocator);
        var it = self.map.iterator();
        while (it.next()) |entry| {
            const new_list = try entry.value_ptr.clone(self.allocator);
            try new.map.put(self.allocator, entry.key_ptr.*, new_list);
        }
        return new;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "Constraint proves type_is" {
    const t = type_adt.t_cons;
    const c = Constraint{ .type_is = &t };
    try std.testing.expect(c.proves(&t));
    try std.testing.expect(!c.proves(&type_adt.t_fixnum));
}

test "Constraint proves non_nil" {
    const c = Constraint{ .non_nil = {} };
    try std.testing.expect(c.proves(&type_adt.t_non_nil));
    try std.testing.expect(!c.proves(&type_adt.t_cons));
}

test "Constraint range proves fixnum" {
    const c = Constraint{ .range = .{ .lo = 0, .hi = 100 } };
    try std.testing.expect(c.proves(&type_adt.t_fixnum));
    try std.testing.expect(!c.proves(&type_adt.t_cons));
}

test "Constraint contradicts" {
    const t = type_adt.t_cons;
    const c = Constraint{ .type_not = &t };
    try std.testing.expect(c.contradicts(&t));
    try std.testing.expect(!c.contradicts(&type_adt.t_fixnum));
}

test "Constraint type_is contradicts disjoint" {
    const c = Constraint{ .type_is = &type_adt.t_cons };
    try std.testing.expect(c.contradicts(&type_adt.t_fixnum));
    try std.testing.expect(!c.contradicts(&type_adt.t_cons));
}

test "ConstraintSet basic operations" {
    const testing = std.testing;
    var cs = ConstraintSet.init(testing.allocator);
    defer cs.deinit();

    try cs.add("x", .{ .type_is = &type_adt.t_cons });
    try testing.expect(cs.provesType("x", &type_adt.t_cons));
    try testing.expect(!cs.provesType("x", &type_adt.t_fixnum));
    try testing.expect(!cs.provesType("y", &type_adt.t_cons));
}

test "ConstraintSet clone" {
    const testing = std.testing;
    var cs = ConstraintSet.init(testing.allocator);
    defer cs.deinit();

    try cs.add("x", .{ .type_is = &type_adt.t_cons });

    var cs2 = try cs.clone();
    defer cs2.deinit();

    try testing.expect(cs2.provesType("x", &type_adt.t_cons));

    // Adding to clone doesn't affect original
    try cs2.add("y", .{ .non_nil = {} });
    try testing.expect(!cs.provesType("y", &type_adt.t_non_nil));
    try testing.expect(cs2.provesType("y", &type_adt.t_non_nil));
}
