//! Type checker skeleton for Habu's gradual type system
//!
//! Features:
//! - Type inference for untyped code
//! - Occurrence typing (type narrows after predicates)
//! - Contract insertion at typed/untyped boundaries

const std = @import("std");
const Type = @import("type.zig").Type;
const Primitive = @import("type.zig").Primitive;
const t = @import("type.zig");

/// Type environment: maps variable names to types
pub const TypeEnv = struct {
    parent: ?*const TypeEnv,
    bindings: std.StringHashMap(*const Type),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeEnv {
        return .{
            .parent = null,
            .bindings = std.StringHashMap(*const Type).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn initWithParent(allocator: std.mem.Allocator, parent: *const TypeEnv) TypeEnv {
        return .{
            .parent = parent,
            .bindings = std.StringHashMap(*const Type).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *TypeEnv) void {
        self.bindings.deinit();
    }

    pub fn lookup(self: TypeEnv, name: []const u8) ?*const Type {
        if (self.bindings.get(name)) |ty| {
            return ty;
        }
        if (self.parent) |p| {
            return p.lookup(name);
        }
        return null;
    }

    pub fn bind(self: *TypeEnv, name: []const u8, ty: *const Type) !void {
        try self.bindings.put(name, ty);
    }
};

/// Occurrence typing context
/// Tracks type refinements from conditionals
pub const OccurrenceCtx = struct {
    /// Narrowed types for variables (after predicates)
    narrowed: std.StringHashMap(*const Type),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) OccurrenceCtx {
        return .{
            .narrowed = std.StringHashMap(*const Type).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *OccurrenceCtx) void {
        self.narrowed.deinit();
    }

    /// After (consp x), narrow x to cons type
    pub fn narrowToCons(self: *OccurrenceCtx, var_name: []const u8) !void {
        try self.narrowed.put(var_name, &t.t_cons);
    }

    /// After (null x) or (not x), narrow x to nil type
    pub fn narrowToNil(self: *OccurrenceCtx, var_name: []const u8) !void {
        try self.narrowed.put(var_name, &t.t_nil);
    }

    /// After (symbolp x), narrow x to symbol type
    pub fn narrowToSymbol(self: *OccurrenceCtx, var_name: []const u8) !void {
        try self.narrowed.put(var_name, &t.t_symbol);
    }

    /// After (stringp x), narrow x to string type
    pub fn narrowToString(self: *OccurrenceCtx, var_name: []const u8) !void {
        try self.narrowed.put(var_name, &t.t_string);
    }

    /// After (numberp x), narrow x to fixnum type
    pub fn narrowToFixnum(self: *OccurrenceCtx, var_name: []const u8) !void {
        try self.narrowed.put(var_name, &t.t_fixnum);
    }

    /// Get narrowed type for a variable, or null if not narrowed
    pub fn getNarrowed(self: OccurrenceCtx, var_name: []const u8) ?*const Type {
        return self.narrowed.get(var_name);
    }

    /// Combine two occurrence contexts (for if-then-else join)
    /// Result type is union of both branches
    pub fn join(self: *OccurrenceCtx, other: *const OccurrenceCtx, builder: anytype) !void {
        var it = self.narrowed.iterator();
        while (it.next()) |entry| {
            if (other.narrowed.get(entry.key_ptr.*)) |other_ty| {
                // Both branches have a type for this var - union them
                const types = [_]*const Type{ entry.value_ptr.*, other_ty };
                const union_ty = try builder.makeOr(&types);
                try self.narrowed.put(entry.key_ptr.*, union_ty);
            }
            // If only in self, it stays narrowed only on that path
        }
    }
};

/// Type checking result
pub const CheckResult = union(enum) {
    /// Successfully inferred type
    ok: *const Type,
    /// Type error
    err: TypeError,
};

/// Type error information
pub const TypeError = struct {
    message: []const u8,
    expected: ?*const Type,
    got: ?*const Type,
    span_start: usize,
    span_end: usize,
};

/// Type checker state
pub const TypeChecker = struct {
    allocator: std.mem.Allocator,
    errors: std.ArrayList(TypeError),
    builder: @import("type.zig").TypeBuilder,

    pub fn init(allocator: std.mem.Allocator) TypeChecker {
        return .{
            .allocator = allocator,
            .errors = std.ArrayList(TypeError){},
            .builder = @import("type.zig").TypeBuilder.init(allocator),
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.errors.deinit(self.allocator);
    }

    pub fn hasErrors(self: TypeChecker) bool {
        return self.errors.items.len > 0;
    }

    /// Report a type error
    pub fn reportError(self: *TypeChecker, message: []const u8, span_start: usize, span_end: usize) !void {
        try self.errors.append(self.allocator, .{
            .message = message,
            .expected = null,
            .got = null,
            .span_start = span_start,
            .span_end = span_end,
        });
    }

    /// Report a type mismatch error
    pub fn reportMismatch(
        self: *TypeChecker,
        message: []const u8,
        expected: *const Type,
        got: *const Type,
        span_start: usize,
        span_end: usize,
    ) !void {
        try self.errors.append(self.allocator, .{
            .message = message,
            .expected = expected,
            .got = got,
            .span_start = span_start,
            .span_end = span_end,
        });
    }

    // ========================================================================
    // Type checking for expressions (skeleton - to be filled in)
    // ========================================================================

    /// Check/infer type of a literal
    pub fn checkLiteral(self: *TypeChecker, value: anytype) *const Type {
        _ = self;
        const T = @TypeOf(value);
        if (T == i64 or T == i32 or T == comptime_int) {
            return &t.t_fixnum;
        }
        return &t.t_any;
    }

    /// Check type of variable reference
    pub fn checkVar(self: *TypeChecker, name: []const u8, env: *const TypeEnv, occ: *const OccurrenceCtx) *const Type {
        _ = self;
        // First check occurrence typing (narrowed types)
        if (occ.getNarrowed(name)) |narrowed| {
            return narrowed;
        }
        // Then check environment
        if (env.lookup(name)) |ty| {
            return ty;
        }
        // Unknown variable - return any (will be caught elsewhere)
        return &t.t_any;
    }

    /// Check that a type is a subtype of another
    pub fn isSubtype(self: *TypeChecker, sub: *const Type, super: *const Type) bool {
        // Any is supertype of everything
        if (super.* == .any) return true;

        // Same type
        if (std.meta.eql(sub.*, super.*)) return true;

        // Nil is subtype of list (empty list)
        if (sub.* == .primitive and sub.primitive == .nil) {
            if (super.* == .list) return true;
        }

        // Or: sub must be subtype of at least one alternative
        if (super.* == .@"or") {
            for (super.@"or") |alt| {
                if (self.isSubtype(sub, alt)) return true;
            }
        }

        // Non-nil T is subtype of T
        if (sub.* == .non_nil) {
            return self.isSubtype(sub.non_nil, super);
        }

        return false;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "type environment" {
    const testing = std.testing;
    var env = TypeEnv.init(testing.allocator);
    defer env.deinit();

    try env.bind("x", &t.t_fixnum);
    try testing.expectEqual(&t.t_fixnum, env.lookup("x").?);
    try testing.expectEqual(@as(?*const Type, null), env.lookup("y"));
}

test "occurrence typing" {
    const testing = std.testing;
    var occ = OccurrenceCtx.init(testing.allocator);
    defer occ.deinit();

    try occ.narrowToCons("x");
    try testing.expectEqual(&t.t_cons, occ.getNarrowed("x").?);
}

test "subtype" {
    const testing = std.testing;
    var checker = TypeChecker.init(testing.allocator);
    defer checker.deinit();

    // Everything is subtype of any
    try testing.expect(checker.isSubtype(&t.t_fixnum, &t.t_any));
    try testing.expect(checker.isSubtype(&t.t_cons, &t.t_any));

    // Same type
    try testing.expect(checker.isSubtype(&t.t_fixnum, &t.t_fixnum));

    // Nil is subtype of list
    try testing.expect(checker.isSubtype(&t.t_nil, &t.t_list_any));
}
