//! IR Types for Nanopass Pipeline
//!
//! Different IR representations used at different pipeline stages:
//! - SExpr: Parsed S-expressions (Value tree)
//! - CoreExpr: Desugared core language
//! - Ir: Tree-based IR
//! - TypedIr: IR with type annotations
//! - Chunk: Bytecode

const std = @import("std");
const Ir = @import("../ir.zig").Ir;
const type_adt = @import("../../types/type.zig");
const Type = type_adt.Type;
pub const Quantity = type_adt.Quantity;
const Value = @import("../../runtime/value.zig").Value;

/// TypedIr wraps an IR node with type and quantity information.
/// Used between type annotation and erasure passes.
pub const TypedIr = struct {
    /// The underlying IR node
    ir: *const Ir,
    /// Inferred or annotated type (null if not yet inferred)
    ty: ?*const Type,
    /// QTT quantity: zero (erased), one (linear), many (unrestricted)
    quantity: Quantity,
    /// Child nodes (for tree traversal)
    children: []const *const TypedIr,

    pub fn init(ir: *const Ir) TypedIr {
        return .{
            .ir = ir,
            .ty = null,
            .quantity = .many, // Default to unrestricted
            .children = &.{},
        };
    }

    pub fn withType(self: TypedIr, ty: *const Type) TypedIr {
        var copy = self;
        copy.ty = ty;
        return copy;
    }

    pub fn withQuantity(self: TypedIr, q: Quantity) TypedIr {
        var copy = self;
        copy.quantity = q;
        return copy;
    }

    /// Check if this node should be erased (0-quantity)
    pub fn isErased(self: *const TypedIr) bool {
        return self.quantity == .zero;
    }
};

/// TypedIrBuilder constructs TypedIr trees
pub const TypedIrBuilder = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) TypedIrBuilder {
        return .{ .allocator = allocator };
    }

    /// Wrap an Ir node as TypedIr
    pub fn wrap(self: *TypedIrBuilder, ir: *const Ir) !*const TypedIr {
        const node = try self.allocator.create(TypedIr);
        node.* = TypedIr.init(ir);
        return node;
    }

    /// Wrap with type annotation
    pub fn wrapTyped(self: *TypedIrBuilder, ir: *const Ir, ty: *const Type, q: Quantity) !*const TypedIr {
        const node = try self.allocator.create(TypedIr);
        node.* = .{
            .ir = ir,
            .ty = ty,
            .quantity = q,
            .children = &.{},
        };
        return node;
    }

    /// Build TypedIr tree from Ir tree (recursive)
    pub fn buildTree(self: *TypedIrBuilder, ir: *const Ir) !*const TypedIr {
        const node = try self.allocator.create(TypedIr);
        var children = std.ArrayListUnmanaged(*const TypedIr){};

        // Recursively wrap children based on IR node type
        switch (ir.*) {
            .lit, .quote_sym, .global_ref, .go => {
                // Leaf nodes - no children
            },
            .quote => |q| {
                try children.append(self.allocator, try self.buildTree(q));
            },
            .@"var" => {
                // Leaf node
            },
            .set => |s| {
                try children.append(self.allocator, try self.buildTree(s.value));
            },
            .define => |d| {
                try children.append(self.allocator, try self.buildTree(d.value));
            },
            .let => |l| {
                for (l.bindings) |b| {
                    try children.append(self.allocator, try self.buildTree(b.value));
                }
                try children.append(self.allocator, try self.buildTree(l.body));
            },
            .lambda => |l| {
                try children.append(self.allocator, try self.buildTree(l.body));
            },
            .@"if" => |i| {
                try children.append(self.allocator, try self.buildTree(i.cond));
                try children.append(self.allocator, try self.buildTree(i.then_branch));
                try children.append(self.allocator, try self.buildTree(i.else_branch));
            },
            .progn => |exprs| {
                for (exprs) |e| {
                    try children.append(self.allocator, try self.buildTree(e));
                }
            },
            .loop => |l| {
                try children.append(self.allocator, try self.buildTree(l.cond));
                try children.append(self.allocator, try self.buildTree(l.body));
            },
            .block => |b| {
                try children.append(self.allocator, try self.buildTree(b.body));
            },
            .return_from => |r| {
                try children.append(self.allocator, try self.buildTree(r.value));
            },
            .call => |c| {
                try children.append(self.allocator, try self.buildTree(c.func));
                for (c.args) |a| {
                    try children.append(self.allocator, try self.buildTree(a));
                }
            },
            .add, .sub, .mul, .div, .mod => |binop| {
                try children.append(self.allocator, try self.buildTree(binop.left));
                try children.append(self.allocator, try self.buildTree(binop.right));
            },
            .eq, .lt, .gt, .le, .ge, .num_eq => |cmp| {
                try children.append(self.allocator, try self.buildTree(cmp.left));
                try children.append(self.allocator, try self.buildTree(cmp.right));
            },
            else => {
                // For other node types, we'd need to handle them specifically
                // This is a simplified version
            },
        }

        node.* = .{
            .ir = ir,
            .ty = null,
            .quantity = .many,
            .children = try children.toOwnedSlice(self.allocator),
        };
        return node;
    }
};

/// Pipeline stage markers for type safety
pub const Stage = enum {
    parsed,      // After parsing (SExpr)
    expanded,    // After macro expansion
    desugared,   // After desugaring
    lifted,      // After lifting to IR
    resolved,    // After name resolution
    captured,    // After capture analysis
    annotated,   // After type annotation
    inferred,    // After type inference
    checked,     // After type checking
    linear,      // After linearity check
    erased,      // After erasure
    emitted,     // After bytecode emission
};

// ============================================================================
// Tests
// ============================================================================

test "TypedIr wrap" {
    const testing = std.testing;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var builder = TypedIrBuilder.init(testing.allocator);
    const typed = try builder.wrap(&lit);
    defer testing.allocator.destroy(@constCast(typed));

    try testing.expectEqual(&lit, typed.ir);
    try testing.expect(typed.ty == null);
    try testing.expectEqual(Quantity.many, typed.quantity);
}

test "TypedIr with type" {
    const testing = std.testing;
    const t_fixnum = @import("../../types/type.zig").t_fixnum;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var builder = TypedIrBuilder.init(testing.allocator);
    const typed = try builder.wrapTyped(&lit, &t_fixnum, .one);
    defer testing.allocator.destroy(@constCast(typed));

    try testing.expectEqual(&t_fixnum, typed.ty.?);
    try testing.expectEqual(Quantity.one, typed.quantity);
}

test "TypedIr isErased" {
    const testing = std.testing;
    const t_fixnum = @import("../../types/type.zig").t_fixnum;

    const lit = Ir{ .lit = Value.makeFixnum(42) };
    var builder = TypedIrBuilder.init(testing.allocator);

    const typed_many = try builder.wrapTyped(&lit, &t_fixnum, .many);
    defer testing.allocator.destroy(@constCast(typed_many));
    try testing.expect(!typed_many.isErased());

    const typed_zero = try builder.wrapTyped(&lit, &t_fixnum, .zero);
    defer testing.allocator.destroy(@constCast(typed_zero));
    try testing.expect(typed_zero.isErased());
}
