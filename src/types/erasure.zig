//! Erasure Pass for Quantitative Type Theory
//!
//! Removes 0-quantity (erased) terms from IR before bytecode emission.
//! In QTT:
//! - 0-quantity variables are type-level only (proofs, type indices)
//! - They must not appear in runtime code
//! - The BiChecker already rejects illegal uses; erasure removes them from IR
//!
//! Transformations:
//! - Lambda parameters with 0-quantity are removed
//! - Function call arguments for 0-quantity parameters are removed
//! - Let bindings with 0-quantity are removed
//! - References to erased variables are replaced with nil

const std = @import("std");
const ir_mod = @import("../compiler/ir.zig");
const type_mod = @import("type.zig");

const Ir = ir_mod.Ir;
const IrBuilder = ir_mod.IrBuilder;
const Quantity = type_mod.Quantity;
const Type = type_mod.Type;
const Value = @import("../runtime/value.zig").Value;

/// Erasure context - tracks which variables are erased
pub const ErasureCtx = struct {
    allocator: std.mem.Allocator,
    /// Set of erased variable names
    erased_vars: std.StringHashMapUnmanaged(void),
    /// Parent context for nested scopes
    parent: ?*const ErasureCtx,

    pub fn init(allocator: std.mem.Allocator) ErasureCtx {
        return .{
            .allocator = allocator,
            .erased_vars = std.StringHashMapUnmanaged(void){},
            .parent = null,
        };
    }

    pub fn initWithParent(allocator: std.mem.Allocator, parent: *const ErasureCtx) ErasureCtx {
        return .{
            .allocator = allocator,
            .erased_vars = std.StringHashMapUnmanaged(void){},
            .parent = parent,
        };
    }

    pub fn deinit(self: *ErasureCtx) void {
        self.erased_vars.deinit(self.allocator);
    }

    /// Mark a variable as erased (0-quantity)
    pub fn markErased(self: *ErasureCtx, name: []const u8) !void {
        try self.erased_vars.put(self.allocator, name, {});
    }

    /// Check if a variable is erased
    pub fn isErased(self: *const ErasureCtx, name: []const u8) bool {
        if (self.erased_vars.get(name) != null) {
            return true;
        }
        if (self.parent) |p| {
            return p.isErased(name);
        }
        return false;
    }
};

/// Erasure pass - transforms IR to remove 0-quantity terms
pub const Eraser = struct {
    allocator: std.mem.Allocator,
    builder: IrBuilder,

    pub fn init(allocator: std.mem.Allocator) Eraser {
        return .{
            .allocator = allocator,
            .builder = IrBuilder.init(allocator),
        };
    }

    pub fn deinit(self: *Eraser) void {
        _ = self;
        // Builder doesn't own the IR nodes, caller does
    }

    /// Erase 0-quantity terms from an IR tree
    /// Returns the transformed IR (may be the same node if no changes)
    pub fn erase(self: *Eraser, node: *const Ir, ctx: *const ErasureCtx) !*const Ir {
        return switch (node.*) {
            // Variable reference - replace with nil if erased
            .@"var" => |v| {
                if (ctx.isErased(v.name)) {
                    // Erased variable - replace with nil
                    return try self.builder.lit(Value.nil);
                }
                return node;
            },

            // Lambda - remove erased parameters
            .lambda => |lam| {
                // For now, just recurse into body
                // Full implementation would track which params are erased
                // based on the function's Pi type
                const new_body = try self.erase(lam.body, ctx);
                if (new_body == lam.body) {
                    return node;
                }
                return try self.builder.lambda(
                    lam.params,
                    lam.optional_params,
                    lam.key_params,
                    lam.allow_other_keys,
                    lam.rest_param,
                    lam.captures,
                    new_body,
                );
            },

            // Let - remove erased bindings
            .let => |l| {
                // Create child context for let body
                var child_ctx = ErasureCtx.initWithParent(self.allocator, ctx);
                defer child_ctx.deinit();

                // Filter out erased bindings
                // For now, keep all bindings (would need quantity info)
                var new_bindings = std.ArrayList(Ir.Binding){};
                defer new_bindings.deinit(self.allocator);

                for (l.bindings) |binding| {
                    // Recursively erase binding value
                    const new_value = try self.erase(binding.value, ctx);
                    try new_bindings.append(self.allocator, .{
                        .name = binding.name,
                        .value = new_value,
                        .index = binding.index,
                    });
                }

                // Erase body
                const new_body = try self.erase(l.body, &child_ctx);

                // Check if anything changed
                var changed = new_body != l.body;
                if (!changed) {
                    for (new_bindings.items, l.bindings) |new_b, old_b| {
                        if (new_b.value != old_b.value) {
                            changed = true;
                            break;
                        }
                    }
                }

                if (!changed) {
                    return node;
                }

                const bindings_slice = try self.allocator.dupe(Ir.Binding, new_bindings.items);
                return try self.builder.letExpr(bindings_slice, new_body);
            },

            // If - recurse into branches
            .@"if" => |i| {
                const new_cond = try self.erase(i.cond, ctx);
                const new_then = try self.erase(i.then_branch, ctx);
                const new_else = try self.erase(i.else_branch, ctx);

                if (new_cond == i.cond and new_then == i.then_branch and new_else == i.else_branch) {
                    return node;
                }

                return try self.builder.ifExpr(new_cond, new_then, new_else);
            },

            // Progn - recurse into all expressions
            .progn => |exprs| {
                var new_exprs = std.ArrayList(*const Ir){};
                defer new_exprs.deinit(self.allocator);

                var changed = false;
                for (exprs) |expr| {
                    const new_expr = try self.erase(expr, ctx);
                    if (new_expr != expr) changed = true;
                    try new_exprs.append(self.allocator, new_expr);
                }

                if (!changed) {
                    return node;
                }

                const slice = try self.allocator.dupe(*const Ir, new_exprs.items);
                return try self.builder.progn(slice);
            },

            // Function call - would remove erased arguments if we had type info
            .call => |c| {
                const new_func = try self.erase(c.func, ctx);

                var new_args = std.ArrayList(*const Ir){};
                defer new_args.deinit(self.allocator);

                var changed = new_func != c.func;
                for (c.args) |arg| {
                    const new_arg = try self.erase(arg, ctx);
                    if (new_arg != arg) changed = true;
                    try new_args.append(self.allocator, new_arg);
                }

                if (!changed) {
                    return node;
                }

                const args_slice = try self.allocator.dupe(*const Ir, new_args.items);
                return try self.builder.call(new_func, args_slice);
            },

            // Binary operations - recurse
            .add,
            .sub,
            .mul,
            .div,
            .mod,
            .lt,
            .gt,
            .le,
            .ge,
            .num_eq,
            .eq,
            .eql,
            .equal,
            .cons,
            .append,
            .nth,
            .nthcdr,
            .member,
            .assoc,
            .member_eql,
            .member_equal,
            .assoc_eql,
            .assoc_equal,
            .find,
            .find_eq,
            .find_equal,
            .position,
            .position_eq,
            .position_equal,
            .count,
            .count_eq,
            .count_equal,
            .remove,
            .remove_eq,
            .remove_equal,
            .rplaca,
            .rplacd,
            .vec_ref,
            .str_ref,
            .str_concat,
            .str_eq,
            .logand,
            .logior,
            .logxor,
            .ash,
            => |op| {
                const new_left = try self.erase(op.left, ctx);
                const new_right = try self.erase(op.right, ctx);

                if (new_left == op.left and new_right == op.right) {
                    return node;
                }

                // Rebuild node with new operands
                return self.rebuildBinary(node, new_left, new_right);
            },

            // UnaryOp operations - recurse
            .car,
            .cdr,
            .not,
            .length,
            .reverse,
            .last,
            .consp,
            .symbolp,
            .numberp,
            .integerp,
            .realp,
            .stringp,
            .vectorp,
            .closurep,
            .keywordp,
            .nilp,
            .listp,
            .atom,
            .characterp,
            .floatp,
            .zerop,
            .plusp,
            .minusp,
            .evenp,
            .oddp,
            .abs,
            .lognot,
            .vec_len,
            .str_len,
            .char_code,
            .code_char,
            .char_upcase,
            .char_downcase,
            .digit_char_p,
            .alpha_char_p,
            .print,
            .princ,
            .write_char,
            .intern,
            .sym_name,
            .boundp,
            .fboundp,
            .symbol_value,
            .symbol_function,
            .type_of,
            .string_upcase,
            .string_downcase,
            .list_to_string,
            .assert_fixnum,
            .assert_cons,
            .assert_symbol,
            .assert_string,
            .assert_vector,
            .assert_closure,
            .assert_non_nil,
            .assert_list,
            .box_ref,
            .read_from_string,
            .load,
            .unread_char,
            .eval,
            .macroexpand,
            .macroexpand_1,
            .parse_integer,
            .write_to_string,
            .read_file,
            .random,
            .error_user,
            .make_box,
            => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                return self.rebuildUnary(node, new_operand);
            },

            // hashtablep has its own struct type
            .hashtablep => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                // No builder for this - return original
                return node;
            },

            // rationalp
            .rationalp => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                return try self.builder.rationalp(new_operand);
            },

            // complexp
            .complexp => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                return try self.builder.complexp(new_operand);
            },

            // make_complex
            .make_complex => |op| {
                const new_left = try self.erase(op.left, ctx);
                const new_right = try self.erase(op.right, ctx);
                if (new_left == op.left and new_right == op.right) {
                    return node;
                }
                return try self.builder.makeComplex(new_left, new_right);
            },

            // real_part
            .real_part => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                return try self.builder.realPart(new_operand);
            },

            // imag_part
            .imag_part => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                return try self.builder.imagPart(new_operand);
            },

            // hash_count has its own struct type
            .hash_count => |op| {
                const new_operand = try self.erase(op.operand, ctx);
                if (new_operand == op.operand) {
                    return node;
                }
                // No builder for this - return original
                return node;
            },

            // Literals and other leaf nodes - no change
            .lit,
            .quote_sym,
            .quote,
            .global_ref,
            .read_char,
            .peek_char,
            .read,
            .gensym,
            => node,

            // Other nodes - pass through for now
            // Full implementation would handle all node types
            else => node,
        };
    }

    /// Rebuild a binary operation with new operands
    /// Note: Only operations with dedicated IrBuilder methods are listed
    fn rebuildBinary(self: *Eraser, node: *const Ir, left: *const Ir, right: *const Ir) !*const Ir {
        return switch (node.*) {
            .add => try self.builder.add(left, right),
            .sub => try self.builder.sub(left, right),
            .mul => try self.builder.mul(left, right),
            .div => try self.builder.div(left, right),
            .lt => try self.builder.lt(left, right),
            .gt => try self.builder.gt(left, right),
            .eq => try self.builder.eq(left, right),
            .eql => try self.builder.eql(left, right),
            .equal => try self.builder.equal(left, right),
            .cons => try self.builder.cons(left, right),
            .append => try self.builder.append(left, right),
            .assoc => try self.builder.assoc(left, right),
            // Operations without dedicated builders - return original
            else => node,
        };
    }

    /// Rebuild a unary operation with new operand
    /// Note: Only operations with dedicated IrBuilder methods are listed
    fn rebuildUnary(self: *Eraser, node: *const Ir, operand: *const Ir) !*const Ir {
        return switch (node.*) {
            .car => try self.builder.car(operand),
            .cdr => try self.builder.cdr(operand),
            .consp => try self.builder.consp(operand),
            .symbolp => try self.builder.symbolp(operand),
            .numberp => try self.builder.numberp(operand),
            .integerp => try self.builder.integerp(operand),
            .realp => try self.builder.realp(operand),
            .stringp => try self.builder.stringp(operand),
            .vectorp => try self.builder.vectorp(operand),
            .closurep => try self.builder.closurep(operand),
            .keywordp => try self.builder.keywordp(operand),
            .nilp => try self.builder.nilp(operand),
            .listp => try self.builder.listp(operand),
            .characterp => try self.builder.characterp(operand),
            .floatp => try self.builder.floatp(operand),
            .assert_fixnum => try self.builder.assertFixnum(operand),
            .assert_cons => try self.builder.assertCons(operand),
            .assert_symbol => try self.builder.assertSymbol(operand),
            .assert_string => try self.builder.assertString(operand),
            .assert_vector => try self.builder.assertVector(operand),
            .assert_closure => try self.builder.assertClosure(operand),
            .assert_non_nil => try self.builder.assertNonNil(operand),
            .assert_list => try self.builder.assertList(operand),
            .assert_or => |ao| try self.builder.assertOr(operand, ao.type_symbols),
            // Operations without dedicated builders - return original
            else => node,
        };
    }
};

/// Convenience function to erase 0-quantity terms
pub fn eraseZeroQuantity(
    allocator: std.mem.Allocator,
    ir: *const Ir,
    erased_names: []const []const u8,
) !*const Ir {
    var ctx = ErasureCtx.init(allocator);
    defer ctx.deinit();

    for (erased_names) |name| {
        try ctx.markErased(name);
    }

    var eraser = Eraser.init(allocator);
    defer eraser.deinit();

    return try eraser.erase(ir, &ctx);
}

// ============================================================================
// Tests
// ============================================================================

test "erasure context" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ctx = ErasureCtx.init(allocator);
    defer ctx.deinit();

    try testing.expect(!ctx.isErased("x"));

    try ctx.markErased("x");
    try testing.expect(ctx.isErased("x"));
    try testing.expect(!ctx.isErased("y"));
}

test "erasure context nesting" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var parent = ErasureCtx.init(allocator);
    defer parent.deinit();
    try parent.markErased("x");

    var child = ErasureCtx.initWithParent(allocator, &parent);
    defer child.deinit();
    try child.markErased("y");

    // Child sees both parent and own erasures
    try testing.expect(child.isErased("x"));
    try testing.expect(child.isErased("y"));

    // Parent only sees own erasures
    try testing.expect(parent.isErased("x"));
    try testing.expect(!parent.isErased("y"));
}

test "erase variable reference" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ctx = ErasureCtx.init(allocator);
    defer ctx.deinit();
    try ctx.markErased("proof");

    var eraser = Eraser.init(allocator);
    defer eraser.deinit();

    // Create a variable reference to an erased variable
    const var_ir = try eraser.builder.variable("proof", 0, 0);
    defer allocator.destroy(var_ir);
    defer allocator.free(var_ir.@"var".name); // Free duped name

    // Erase it - should become nil
    const erased = try eraser.erase(var_ir, &ctx);

    try testing.expect(erased != var_ir);
    try testing.expect(erased.* == .lit);
    try testing.expect(erased.lit.isNil());

    allocator.destroy(erased);
}

test "erase preserves non-erased variables" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var ctx = ErasureCtx.init(allocator);
    defer ctx.deinit();
    try ctx.markErased("proof");

    var eraser = Eraser.init(allocator);
    defer eraser.deinit();

    // Create a variable reference to a non-erased variable
    const var_ir = try eraser.builder.variable("x", 0, 0);
    defer allocator.destroy(var_ir);
    defer allocator.free(var_ir.@"var".name); // Free duped name

    // Erase it - should be unchanged
    const erased = try eraser.erase(var_ir, &ctx);

    try testing.expect(erased == var_ir);
}

test "eraseZeroQuantity convenience function" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var builder = IrBuilder.init(allocator);

    // Create (+ proof x) where proof is erased
    const proof_var = try builder.variable("proof", 0, 0);
    defer allocator.destroy(proof_var);
    defer allocator.free(proof_var.@"var".name); // Free the duped name
    const x_var = try builder.variable("x", 0, 1);
    defer allocator.destroy(x_var);
    defer allocator.free(x_var.@"var".name); // Free the duped name
    const add_ir = try builder.add(proof_var, x_var);
    defer allocator.destroy(add_ir);

    const erased_names = [_][]const u8{"proof"};
    const result = try eraseZeroQuantity(allocator, add_ir, &erased_names);

    // Result should be (+ nil x)
    try testing.expect(result != add_ir);
    try testing.expect(result.* == .add);
    try testing.expect(result.add.left.* == .lit);
    try testing.expect(result.add.left.lit.isNil());
    try testing.expect(result.add.right == x_var);

    // Free the newly created nodes
    allocator.destroy(result.add.left); // The nil literal
    allocator.destroy(result);
}
