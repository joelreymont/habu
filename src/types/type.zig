//! Type ADT for Habu's gradual type system
//!
//! Racket-style types:
//! - Primitive: fixnum, cons, symbol, string, vector, closure, keyword, nil
//! - Compound: (or T1 T2), (-> args ret), (list T), any

const std = @import("std");

/// Primitive types matching Habu's tagged value scheme
pub const Primitive = enum {
    fixnum,
    cons,
    symbol,
    string,
    vector,
    closure,
    keyword,
    nil,
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
    list: *const Type,

    /// Homogeneous vector: (vector T)
    vec: *const Type,

    /// Dynamic type (escape hatch)
    any,

    /// Format type for display
    pub fn format(
        self: Type,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .primitive => |p| try writer.print("{s}", .{@tagName(p)}),
            .@"or" => |types| {
                try writer.writeAll("(or");
                for (types) |t| {
                    try writer.writeAll(" ");
                    try writer.print("{f}", .{t.*});
                }
                try writer.writeAll(")");
            },
            .arrow => |a| {
                try writer.writeAll("(-> (");
                for (a.domain, 0..) |t, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{f}", .{t.*});
                }
                try writer.print(") {f})", .{a.range.*});
            },
            .list => |t| try writer.print("(list {f})", .{t.*}),
            .vec => |t| try writer.print("(vector {f})", .{t.*}),
            .any => try writer.writeAll("any"),
        }
    }
};

// Common type constants
pub const t_fixnum = Type{ .primitive = .fixnum };
pub const t_cons = Type{ .primitive = .cons };
pub const t_symbol = Type{ .primitive = .symbol };
pub const t_string = Type{ .primitive = .string };
pub const t_nil = Type{ .primitive = .nil };
pub const t_any = Type{ .any = {} };

test "type formatting" {
    const testing = std.testing;
    var buf: [256]u8 = undefined;

    const s1 = try std.fmt.bufPrint(&buf, "{f}", .{t_fixnum});
    try testing.expectEqualStrings("fixnum", s1);

    const s2 = try std.fmt.bufPrint(&buf, "{f}", .{t_any});
    try testing.expectEqualStrings("any", s2);
}
