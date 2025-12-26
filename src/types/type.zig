//! Type ADT for Habu's gradual type system
//!
//! Racket-style types:
//! - Primitive: fixnum, cons, symbol, string, vector, closure, keyword, nil
//! - Compound: (or T1 T2), (-> args ret), (list T), any
//!
//! Tag values (1+3 bit hybrid scheme):
//!   bit0=1: fixnum (63-bit signed, value >> 1)
//!   bit0=0: pointer | tag in bits 1-3
//!     0: cons, 2: symbol, 4: vector, 6: string
//!     8: closure, 10: keyword, 14: forwarding (GC)
//!   Special: 0 = nil

const std = @import("std");

/// Primitive types matching Habu's tagged value scheme
/// Note: enum values are NOT the runtime tags (see tag() method)
pub const Primitive = enum {
    fixnum, // bit0=1, value >> 1
    float, // bit63=0, bit62=1, f64>>2
    cons, // tag 0
    symbol, // tag 2
    vector, // tag 4
    string, // tag 6
    closure, // tag 8
    keyword, // tag 10
    nil, // value 0
    char, // bit63=1, bits 0-20 = codepoint

    /// Get the runtime tag value for pointer types
    /// Returns null for fixnum (bit0=1) and nil (special value 0)
    pub fn tag(self: Primitive) ?u4 {
        return switch (self) {
            .cons => 0,
            .symbol => 2,
            .vector => 4,
            .string => 6,
            .closure => 8,
            .keyword => 10,
            .fixnum, .float, .nil, .char => null, // Not pointer-tagged
        };
    }

    /// Check if this is a pointer type (vs fixnum/nil)
    pub fn isPointer(self: Primitive) bool {
        return switch (self) {
            .fixnum, .float, .nil, .char => false,
            else => true,
        };
    }

    pub fn name(self: Primitive) []const u8 {
        return @tagName(self);
    }
};

/// Type representation
pub const Type = union(enum) {
    /// Primitive type
    primitive: Primitive,

    /// Union type: (or T1 T2 ...)
    @"or": []const *const Type,

    /// Function type: (-> (T1 T2 ...) R)
    arrow: struct {
        domain: []const *const Type,
        range: *const Type,
    },

    /// Homogeneous list: (list T)
    /// A list is nil or (cons T (list T))
    list: *const Type,

    /// Homogeneous vector: (vector T)
    vec: *const Type,

    /// Non-nil constraint: (non-nil T)
    /// Used to exclude nil from a type
    non_nil: *const Type,

    /// Dynamic type (escape hatch)
    /// Matches any value, no contract checking
    any,

    /// Check if type matches any value (no checking needed)
    pub fn isAny(self: Type) bool {
        return self == .any;
    }

    /// Check if this type could be nil
    pub fn couldBeNil(self: Type) bool {
        return switch (self) {
            .primitive => |p| p == .nil,
            .@"or" => |types| {
                for (types) |t| {
                    if (t.couldBeNil()) return true;
                }
                return false;
            },
            .list => true, // Empty list is nil
            .non_nil => false,
            .any => true,
            .arrow, .vec => false,
        };
    }

    /// Check if type is definitely a cons (for occurrence typing)
    pub fn isCons(self: Type) bool {
        return switch (self) {
            .primitive => |p| p == .cons,
            else => false,
        };
    }

    /// Get human-readable type name
    pub fn name(self: Type) []const u8 {
        return switch (self) {
            .primitive => |p| p.name(),
            .@"or" => "(or ...)",
            .arrow => "(-> ...)",
            .list => "(list ...)",
            .vec => "(vector ...)",
            .non_nil => "(non-nil ...)",
            .any => "any",
        };
    }
};

// ============================================================================
// Common type constants (compile-time)
// ============================================================================

pub const t_fixnum = Type{ .primitive = .fixnum };
pub const t_float = Type{ .primitive = .float };
pub const t_cons = Type{ .primitive = .cons };
pub const t_symbol = Type{ .primitive = .symbol };
pub const t_string = Type{ .primitive = .string };
pub const t_vector = Type{ .primitive = .vector };
pub const t_closure = Type{ .primitive = .closure };
pub const t_keyword = Type{ .primitive = .keyword };
pub const t_nil = Type{ .primitive = .nil };
pub const t_char = Type{ .primitive = .char };
pub const t_any = Type{ .any = {} };

// Common compound types
pub const t_list_any = Type{ .list = &t_any };
/// Non-nil: any value that is not nil (used in else-branch of null? checks)
pub const t_non_nil = Type{ .non_nil = &t_any };

// ============================================================================
// Type constructors (for runtime type building)
// ============================================================================

pub const TypeBuilder = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypeBuilder {
        return .{ .allocator = allocator };
    }

    /// Create (or T1 T2)
    pub fn makeOr(self: TypeBuilder, types: []const *const Type) !*Type {
        const t = try self.allocator.create(Type);
        const copy = try self.allocator.dupe(*const Type, types);
        t.* = .{ .@"or" = copy };
        return t;
    }

    /// Create (-> (domain...) range)
    pub fn makeArrow(self: TypeBuilder, domain: []const *const Type, range: *const Type) !*Type {
        const t = try self.allocator.create(Type);
        const dom_copy = try self.allocator.dupe(*const Type, domain);
        t.* = .{ .arrow = .{ .domain = dom_copy, .range = range } };
        return t;
    }

    /// Create (list T)
    pub fn makeList(self: TypeBuilder, elem: *const Type) !*Type {
        const t = try self.allocator.create(Type);
        t.* = .{ .list = elem };
        return t;
    }

    /// Create (vector T)
    pub fn makeVec(self: TypeBuilder, elem: *const Type) !*Type {
        const t = try self.allocator.create(Type);
        t.* = .{ .vec = elem };
        return t;
    }

    /// Create (non-nil T)
    pub fn makeNonNil(self: TypeBuilder, inner: *const Type) !*Type {
        const t = try self.allocator.create(Type);
        t.* = .{ .non_nil = inner };
        return t;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "type name" {
    const testing = std.testing;

    try testing.expectEqualStrings("fixnum", t_fixnum.name());
    try testing.expectEqualStrings("any", t_any.name());
    try testing.expectEqualStrings("(list ...)", t_list_any.name());
}

test "primitive tags" {
    const testing = std.testing;

    try testing.expectEqual(@as(?u4, 0), Primitive.cons.tag());
    try testing.expectEqual(@as(?u4, 2), Primitive.symbol.tag());
    try testing.expectEqual(@as(?u4, 6), Primitive.string.tag());
    try testing.expectEqual(@as(?u4, null), Primitive.fixnum.tag());
    try testing.expectEqual(@as(?u4, null), Primitive.nil.tag());
}

test "type properties" {
    const testing = std.testing;

    try testing.expect(t_nil.couldBeNil());
    try testing.expect(t_any.couldBeNil());
    try testing.expect(t_list_any.couldBeNil());
    try testing.expect(!t_cons.couldBeNil());
    try testing.expect(!t_fixnum.couldBeNil());

    try testing.expect(t_cons.isCons());
    try testing.expect(!t_nil.isCons());
}
