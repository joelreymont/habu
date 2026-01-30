//! Resolve Pass
//!
//! Resolves variable names to lexical bindings.
//! Fills in depth and index for all variable references.
//!
//! Input: Ir (with UNRESOLVED depth/index)
//! Output: Ir (with resolved depth/index)
//!
//! Note: This pass must run after p03_lift and before p05_capture.

const std = @import("std");
const ir_mod = @import("../ir.zig");
const Ir = ir_mod.Ir;
const IrBuilder = ir_mod.IrBuilder;
const UNRESOLVED = ir_mod.UNRESOLVED;

pub const Error = error{
    UnboundVariable,
    OutOfMemory,
};

/// Scope for tracking variable bindings
pub const Scope = struct {
    /// Variable bindings: name -> index
    bindings: std.StringHashMap(u16),
    /// Parent scope (for nested scopes)
    parent: ?*const Scope,
    /// Whether this is a new frame (lambda) or same frame (let)
    new_frame: bool,
    /// Base index for bindings
    base_index: u16,
    /// Allocator
    allocator: std.mem.Allocator,

    /// Create a new frame scope (for lambda)
    pub fn initFrame(allocator: std.mem.Allocator, parent: ?*const Scope) Scope {
        return .{
            .bindings = std.StringHashMap(u16).init(allocator),
            .parent = parent,
            .new_frame = true,
            .base_index = 0,
            .allocator = allocator,
        };
    }

    /// Create a same-frame scope (for let)
    pub fn initLet(allocator: std.mem.Allocator, parent: *const Scope) Scope {
        return .{
            .bindings = std.StringHashMap(u16).init(allocator),
            .parent = parent,
            .new_frame = false,
            .base_index = parent.localCount(),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Scope) void {
        self.bindings.deinit();
    }

    /// Get total local count in this frame
    pub fn localCount(self: *const Scope) u16 {
        const own_count: u16 = @intCast(self.bindings.count());
        if (!self.new_frame) {
            if (self.parent) |p| {
                return p.localCount() + own_count;
            }
        }
        return self.base_index + own_count;
    }

    /// Add a binding, returns the absolute index
    pub fn bind(self: *Scope, name: []const u8) !u16 {
        const local_index: u16 = @intCast(self.bindings.count());
        const abs_index = self.base_index + local_index;
        try self.bindings.put(name, abs_index);
        return abs_index;
    }

    /// Look up a variable, returns (depth, index) or null
    pub fn lookup(self: *const Scope, name: []const u8) ?struct { depth: u16, index: u16 } {
        if (self.bindings.get(name)) |index| {
            return .{ .depth = 0, .index = index };
        }
        if (self.parent) |parent| {
            if (parent.lookup(name)) |result| {
                if (self.new_frame) {
                    // Cross frame boundary - increment depth
                    return .{ .depth = result.depth + 1, .index = result.index };
                } else {
                    // Same frame (let) - keep same depth
                    return result;
                }
            }
        }
        return null;
    }
};

/// Global registry for top-level definitions
pub const GlobalRegistry = struct {
    names: std.StringHashMap(u16),
    next_index: u16,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) GlobalRegistry {
        return .{
            .names = std.StringHashMap(u16).init(allocator),
            .next_index = 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *GlobalRegistry) void {
        self.names.deinit();
    }

    /// Define a global, returns its index
    pub fn define(self: *GlobalRegistry, name: []const u8) !u16 {
        if (self.names.get(name)) |idx| {
            return idx;
        }
        const idx = self.next_index;
        try self.names.put(name, idx);
        self.next_index += 1;
        return idx;
    }

    /// Look up a global
    pub fn lookup(self: *const GlobalRegistry, name: []const u8) ?u16 {
        return self.names.get(name);
    }
};

/// Resolver - resolves variable names in IR
pub const Resolver = struct {
    allocator: std.mem.Allocator,
    builder: IrBuilder,
    globals: *GlobalRegistry,

    pub fn init(allocator: std.mem.Allocator, globals: *GlobalRegistry) Resolver {
        return .{
            .allocator = allocator,
            .builder = IrBuilder.init(allocator),
            .globals = globals,
        };
    }

    /// Resolve all variables in an IR tree
    pub fn resolve(self: *Resolver, node: *const Ir) Error!*Ir {
        var scope = Scope.initFrame(self.allocator, null);
        defer scope.deinit();
        return self.resolveInScope(node, &scope);
    }

    /// Resolve with a given scope
    fn resolveInScope(self: *Resolver, node: *const Ir, scope: *const Scope) Error!*Ir {
        return switch (node.*) {
            .lit => self.copyNode(node),
            .quote_sym => self.copyNode(node),
            .quote => self.copyNode(node),

            .@"var" => |v| {
                // Resolve the variable
                if (v.depth != UNRESOLVED) {
                    // Already resolved
                    return self.copyNode(node);
                }

                // Look up in scope
                if (scope.lookup(v.name)) |binding| {
                    return try self.builder.variable(v.name, binding.depth, binding.index);
                }

                // Check globals
                if (self.globals.lookup(v.name)) |idx| {
                    return try self.builder.globalRef(v.name, idx);
                }

                return error.UnboundVariable;
            },

            .set => |s| {
                const resolved_value = try self.resolveInScope(s.value, scope);

                if (s.depth != UNRESOLVED) {
                    // Already resolved
                    return try self.builder.set(s.name, s.depth, s.index, resolved_value);
                }

                // Look up in scope
                if (scope.lookup(s.name)) |binding| {
                    return try self.builder.set(s.name, binding.depth, binding.index, resolved_value);
                }

                // Check globals - use set with special depth marker
                if (self.globals.lookup(s.name)) |idx| {
                    return try self.builder.set(s.name, UNRESOLVED, idx, resolved_value);
                }

                return error.UnboundVariable;
            },

            .global_ref => self.copyNode(node),

            .define => |d| {
                // Register the global
                const idx = if (d.index == UNRESOLVED)
                    try self.globals.define(d.name)
                else
                    d.index;

                const resolved_value = try self.resolveInScope(d.value, scope);
                return try self.builder.define(d.name, idx, resolved_value);
            },

            .let => |l| {
                // Create let scope
                var let_scope = Scope.initLet(self.allocator, scope);
                defer let_scope.deinit();

                // Resolve bindings and add to scope
                var resolved_bindings = std.ArrayList(Ir.Binding){};
                defer resolved_bindings.deinit(self.allocator);

                for (l.bindings) |b| {
                    const resolved_val = try self.resolveInScope(b.value, scope);
                    const idx = try let_scope.bind(b.name);
                    try resolved_bindings.append(self.allocator, .{ .name = b.name, .value = resolved_val, .index = idx });
                }

                // Resolve body in let scope
                const resolved_body = try self.resolveInScope(l.body, &let_scope);

                const bindings_slice = try self.allocator.dupe(Ir.Binding, resolved_bindings.items);

                return try self.builder.letExpr(bindings_slice, resolved_body);
            },

            .lambda => |lam| {
                // Create lambda scope
                var lambda_scope = Scope.initFrame(self.allocator, scope);
                defer lambda_scope.deinit();

                // Bind parameters
                for (lam.params) |p| {
                    _ = try lambda_scope.bind(p);
                }

                // Bind optional params
                for (lam.optional_params) |op| {
                    _ = try lambda_scope.bind(op.name);
                }

                // Bind key params
                for (lam.key_params) |kp| {
                    _ = try lambda_scope.bind(kp.name);
                }

                // Bind rest param
                if (lam.rest_param) |rp| {
                    _ = try lambda_scope.bind(rp);
                }

                // Resolve body in lambda scope
                const resolved_body = try self.resolveInScope(lam.body, &lambda_scope);

                // Keep empty captures - will be filled by p05_capture
                return try self.builder.lambda(
                    lam.params,
                    lam.optional_params,
                    lam.key_params,
                    lam.rest_param,
                    lam.captures,
                    resolved_body,
                );
            },

            .@"if" => |i| {
                const cond = try self.resolveInScope(i.cond, scope);
                const then_br = try self.resolveInScope(i.then_branch, scope);
                const else_br = try self.resolveInScope(i.else_branch, scope);
                return try self.builder.ifExpr(cond, then_br, else_br);
            },

            .progn => |exprs| {
                var resolved = std.ArrayList(*const Ir){};
                defer resolved.deinit(self.allocator);
                for (exprs) |e| {
                    const r = try self.resolveInScope(e, scope);
                    try resolved.append(self.allocator, r);
                }
                const slice = try self.allocator.dupe(*const Ir, resolved.items);
                return try self.builder.progn(slice);
            },

            .loop => |w| {
                const cond = try self.resolveInScope(w.cond, scope);
                const body = try self.resolveInScope(w.body, scope);
                return try self.builder.loop(cond, body);
            },

            .call => |c| {
                const func = try self.resolveInScope(c.func, scope);
                var resolved_args = std.ArrayList(*const Ir){};
                defer resolved_args.deinit(self.allocator);
                for (c.args) |a| {
                    const r = try self.resolveInScope(a, scope);
                    try resolved_args.append(self.allocator, r);
                }
                const args_slice = try self.allocator.dupe(*const Ir, resolved_args.items);
                return try self.builder.call(func, args_slice);
            },

            // Binary operations
            .add, .sub, .mul, .div, .mod, .eq, .lt, .gt, .le, .ge, .num_eq, .cons => |b| {
                const left = try self.resolveInScope(b.left, scope);
                const right = try self.resolveInScope(b.right, scope);
                const result = try self.allocator.create(Ir);
                result.* = node.*;
                switch (result.*) {
                    inline .add, .sub, .mul, .div, .mod, .eq, .lt, .gt, .le, .ge, .num_eq, .cons => |*bin| {
                        bin.left = left;
                        bin.right = right;
                    },
                    else => unreachable,
                }
                return result;
            },

            // Unary operations
            .car, .cdr, .consp, .symbolp, .numberp, .stringp, .vectorp, .not, .nilp => |u| {
                const operand = try self.resolveInScope(u.operand, scope);
                const result = try self.allocator.create(Ir);
                result.* = node.*;
                switch (result.*) {
                    inline .car, .cdr, .consp, .symbolp, .numberp, .stringp, .vectorp, .not, .nilp => |*un| {
                        un.operand = operand;
                    },
                    else => unreachable,
                }
                return result;
            },

            else => self.copyNode(node),
        };
    }

    /// Copy a node (for nodes that don't need modification)
    fn copyNode(self: *Resolver, node: *const Ir) Error!*Ir {
        const copy = try self.allocator.create(Ir);
        copy.* = node.*;
        return copy;
    }
};

/// Pass wrapper for pipeline integration
pub fn resolve(allocator: std.mem.Allocator, globals: *GlobalRegistry, node: *const Ir) Error!*Ir {
    var resolver = Resolver.init(allocator, globals);
    return resolver.resolve(node);
}

// ============================================================================
// Tests
// ============================================================================

test "resolve - variable lookup" {
    const testing = std.testing;

    // Use arena for IR nodes (they're not individually freed)
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build (let ((x 1)) x)
    var builder = IrBuilder.init(alloc);
    const lit1 = try builder.lit(@import("../../runtime/value.zig").Value.makeFixnum(1));
    const var_x = try builder.variable("x", UNRESOLVED, UNRESOLVED);
    const bindings = try alloc.dupe(Ir.Binding, &.{
        .{ .name = "x", .value = lit1, .index = 0 },
    });
    const let_ir = try builder.letExpr(bindings, var_x);

    // Resolve
    var globals = GlobalRegistry.init(alloc);
    defer globals.deinit();

    const resolved = try resolve(alloc, &globals, let_ir);

    // The variable inside the let should be resolved
    try testing.expectEqual(Ir.let, std.meta.activeTag(resolved.*));
    try testing.expectEqual(Ir.@"var", std.meta.activeTag(resolved.let.body.*));
    try testing.expectEqual(@as(u16, 0), resolved.let.body.@"var".depth);
    try testing.expectEqual(@as(u16, 0), resolved.let.body.@"var".index);
}

test "resolve - global definition" {
    const testing = std.testing;

    // Use arena for IR nodes
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);
    const lit1 = try builder.lit(@import("../../runtime/value.zig").Value.makeFixnum(42));
    const def = try builder.define("foo", UNRESOLVED, lit1);

    var globals = GlobalRegistry.init(alloc);
    defer globals.deinit();

    const resolved = try resolve(alloc, &globals, def);

    try testing.expectEqual(Ir.define, std.meta.activeTag(resolved.*));
    try testing.expectEqual(@as(u16, 0), resolved.define.index);
}

test "resolve - call args" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);
    const lit1 = try builder.lit(@import("../../runtime/value.zig").Value.makeFixnum(1));
    const lit2 = try builder.lit(@import("../../runtime/value.zig").Value.makeFixnum(2));
    const var_f = try builder.variable("f", UNRESOLVED, UNRESOLVED);
    const var_x = try builder.variable("x", UNRESOLVED, UNRESOLVED);
    const args = try alloc.dupe(*const Ir, &.{ var_x, lit2 });
    const call_ir = try builder.call(var_f, args);
    const bindings = try alloc.dupe(Ir.Binding, &.{
        .{ .name = "x", .value = lit1, .index = 0 },
    });
    const let_ir = try builder.letExpr(bindings, call_ir);

    var globals = GlobalRegistry.init(alloc);
    defer globals.deinit();
    _ = try globals.define("f");

    const resolved = try resolve(alloc, &globals, let_ir);

    try testing.expectEqual(Ir.let, std.meta.activeTag(resolved.*));
    try testing.expectEqual(Ir.call, std.meta.activeTag(resolved.let.body.*));
    try testing.expectEqual(Ir.global_ref, std.meta.activeTag(resolved.let.body.call.func.*));
    try testing.expectEqual(Ir.@"var", std.meta.activeTag(resolved.let.body.call.args[0].*));
    try testing.expectEqual(@as(u16, 0), resolved.let.body.call.args[0].@"var".depth);
    try testing.expectEqual(@as(u16, 0), resolved.let.body.call.args[0].@"var".index);
}
