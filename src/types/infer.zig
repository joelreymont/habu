//! Type inference for Habu's gradual type system
//!
//! Hindley-Milner style inference with extensions:
//! - Type variables for unknowns
//! - Equality constraints from expression usage
//! - Union-find based unification
//! - Occurs check for infinite type prevention

const std = @import("std");
const Type = @import("type.zig").Type;
const Primitive = @import("type.zig").Primitive;
const t_fixnum = @import("type.zig").t_fixnum;
const t_any = @import("type.zig").t_any;

/// A type variable representing an unknown type
pub const TypeVar = struct {
    id: u32,

    pub fn format(
        self: TypeVar,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("?T{d}", .{self.id});
    }
};

/// Extended type that includes type variables for inference
pub const InferType = union(enum) {
    /// Concrete type (from type.zig)
    concrete: *const Type,

    /// Type variable (unknown, to be solved)
    variable: TypeVar,

    /// Function type with possibly-variable components
    arrow: struct {
        domain: []const *const InferType,
        range: *const InferType,
    },

    /// List with element type
    list: *const InferType,

    pub fn isVariable(self: InferType) bool {
        return self == .variable;
    }

    pub fn format(
        self: InferType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .concrete => |t| try writer.print("{s}", .{t.name()}),
            .variable => |v| try writer.print("{}", .{v}),
            .arrow => |a| {
                try writer.writeAll("(-> (");
                for (a.domain, 0..) |d, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{}", .{d.*});
                }
                try writer.print(") {})", .{a.range.*});
            },
            .list => |elem| try writer.print("(list {})", .{elem.*}),
        }
    }
};

/// Constraint between two types that must be satisfied
pub const Constraint = union(enum) {
    /// Types must be equal: T1 = T2
    eq: struct {
        left: *const InferType,
        right: *const InferType,
    },

    /// Subtype relation: T1 <: T2 (T1 can be used where T2 expected)
    subtype: struct {
        sub: *const InferType,
        super: *const InferType,
    },
};

/// Inference context for type inference and constraint solving
pub const InferCtx = struct {
    allocator: std.mem.Allocator,

    /// Counter for fresh type variable generation
    next_var_id: u32,

    /// Substitution map: TypeVar id -> InferType
    /// Represents the current solution
    substitutions: std.AutoHashMap(u32, *const InferType),

    /// Collected constraints to solve
    constraints: std.ArrayList(Constraint),

    pub fn init(allocator: std.mem.Allocator) InferCtx {
        return .{
            .allocator = allocator,
            .next_var_id = 0,
            .substitutions = std.AutoHashMap(u32, *const InferType).init(allocator),
            .constraints = std.ArrayList(Constraint){},
        };
    }

    pub fn deinit(self: *InferCtx) void {
        self.substitutions.deinit();
        self.constraints.deinit(self.allocator);
    }

    /// Generate a fresh type variable
    pub fn freshVar(self: *InferCtx) !*InferType {
        const id = self.next_var_id;
        self.next_var_id += 1;

        const t = try self.allocator.create(InferType);
        t.* = .{ .variable = .{ .id = id } };
        return t;
    }

    /// Wrap a concrete type in InferType
    pub fn concrete(self: *InferCtx, t: *const Type) !*InferType {
        const it = try self.allocator.create(InferType);
        it.* = .{ .concrete = t };
        return it;
    }

    /// Add an equality constraint
    pub fn addEq(self: *InferCtx, left: *const InferType, right: *const InferType) !void {
        try self.constraints.append(self.allocator, .{
            .eq = .{ .left = left, .right = right },
        });
    }

    /// Add a subtype constraint
    pub fn addSubtype(self: *InferCtx, sub: *const InferType, super: *const InferType) !void {
        try self.constraints.append(self.allocator, .{
            .subtype = .{ .sub = sub, .super = super },
        });
    }

    /// Look up current substitution for a type variable
    pub fn lookup(self: *const InferCtx, id: u32) ?*const InferType {
        return self.substitutions.get(id);
    }

    /// Apply substitutions to get the current type
    pub fn resolve(self: *const InferCtx, t: *const InferType) *const InferType {
        switch (t.*) {
            .variable => |v| {
                if (self.lookup(v.id)) |resolved| {
                    return self.resolve(resolved);
                }
                return t;
            },
            else => return t,
        }
    }

    /// Unify two types, updating substitutions
    /// Returns error if types cannot be unified
    pub fn unify(self: *InferCtx, t1: *const InferType, t2: *const InferType) UnifyError!void {
        const r1 = self.resolve(t1);
        const r2 = self.resolve(t2);

        // Same type (pointer equality after resolution)
        if (r1 == r2) return;

        // Variable on left - bind it
        if (r1.* == .variable) {
            const v = r1.variable;
            if (self.occursIn(v, r2)) return error.InfiniteType;
            try self.substitutions.put(v.id, r2);
            return;
        }

        // Variable on right - bind it
        if (r2.* == .variable) {
            const v = r2.variable;
            if (self.occursIn(v, r1)) return error.InfiniteType;
            try self.substitutions.put(v.id, r1);
            return;
        }

        // Both concrete - check structural equality
        if (r1.* == .concrete and r2.* == .concrete) {
            if (!typeEquals(r1.concrete, r2.concrete)) {
                return error.TypeMismatch;
            }
            return;
        }

        // Arrow types - unify components
        if (r1.* == .arrow and r2.* == .arrow) {
            const a1 = r1.arrow;
            const a2 = r2.arrow;

            if (a1.domain.len != a2.domain.len) return error.ArityMismatch;

            for (a1.domain, a2.domain) |d1, d2| {
                try self.unify(d1, d2);
            }
            try self.unify(a1.range, a2.range);
            return;
        }

        // List types - unify element types
        if (r1.* == .list and r2.* == .list) {
            try self.unify(r1.list, r2.list);
            return;
        }

        return error.TypeMismatch;
    }

    /// Check if type variable occurs in a type (prevents infinite types)
    fn occursIn(self: *const InferCtx, v: TypeVar, t: *const InferType) bool {
        const resolved = self.resolve(t);
        switch (resolved.*) {
            .variable => |v2| return v.id == v2.id,
            .concrete => return false,
            .arrow => |a| {
                for (a.domain) |d| {
                    if (self.occursIn(v, d)) return true;
                }
                return self.occursIn(v, a.range);
            },
            .list => |elem| return self.occursIn(v, elem),
        }
    }

    /// Solve all collected constraints
    pub fn solve(self: *InferCtx) UnifyError!void {
        for (self.constraints.items) |c| {
            switch (c) {
                .eq => |eq| try self.unify(eq.left, eq.right),
                .subtype => |sub| {
                    // For now, treat subtype as equality
                    // TODO: proper subtyping with variance
                    try self.unify(sub.sub, sub.super);
                },
            }
        }
    }
};

pub const UnifyError = error{
    TypeMismatch,
    ArityMismatch,
    InfiniteType,
    OutOfMemory,
};

/// Check if two concrete types are equal
fn typeEquals(t1: *const Type, t2: *const Type) bool {
    if (t1 == t2) return true;

    // Check structural equality
    switch (t1.*) {
        .primitive => |p1| {
            if (t2.* == .primitive) {
                return p1 == t2.primitive;
            }
            return false;
        },
        .any => return t2.* == .any,
        else => {
            // TODO: structural equality for compound types
            return false;
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "fresh type variables" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const v1 = try ctx.freshVar();
    const v2 = try ctx.freshVar();

    try testing.expectEqual(@as(u32, 0), v1.variable.id);
    try testing.expectEqual(@as(u32, 1), v2.variable.id);
}

test "unify variables" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const v1 = try ctx.freshVar();
    const fixnum = try ctx.concrete(&t_fixnum);

    try ctx.unify(v1, fixnum);

    const resolved = ctx.resolve(v1);
    try testing.expect(resolved.* == .concrete);
    try testing.expectEqual(Primitive.fixnum, resolved.concrete.primitive);
}

test "unify same concrete types" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const f1 = try ctx.concrete(&t_fixnum);
    const f2 = try ctx.concrete(&t_fixnum);

    try ctx.unify(f1, f2); // Should succeed
}

test "unify different concrete types fails" {
    const testing = std.testing;
    const t_string = @import("type.zig").t_string;

    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const f = try ctx.concrete(&t_fixnum);
    const s = try ctx.concrete(&t_string);

    try testing.expectError(error.TypeMismatch, ctx.unify(f, s));
}

test "occurs check prevents infinite types" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const v = try ctx.freshVar();

    // Create (list v) where v is a type variable
    const list_v = try ctx.allocator.create(InferType);
    list_v.* = .{ .list = v };

    // Trying to unify v = (list v) should fail with infinite type
    try testing.expectError(error.InfiniteType, ctx.unify(v, list_v));
}
