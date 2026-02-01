//! Capture Pass
//!
//! Analyzes closures to identify captured (free) variables.
//! A variable is captured if it's referenced from inside a lambda
//! but defined outside that lambda.
//!
//! Input: Ir (with resolved depth/index, empty captures)
//! Output: Ir (with filled captures)
//!
//! Note: This pass must run after p04_resolve.

const std = @import("std");
const ir_mod = @import("../ir.zig");
const Ir = ir_mod.Ir;
const IrBuilder = ir_mod.IrBuilder;
const UNRESOLVED = ir_mod.UNRESOLVED;

pub const Error = error{
    OutOfMemory,
};

/// CaptureSet - tracks captured variables for a lambda
const CaptureSet = struct {
    captures: std.ArrayList(Ir.Capture),
    seen: std.AutoHashMap(u32, void),
    allocator: std.mem.Allocator,

    fn init(allocator: std.mem.Allocator) CaptureSet {
        return .{
            .captures = std.ArrayList(Ir.Capture){},
            .seen = std.AutoHashMap(u32, void).init(allocator),
            .allocator = allocator,
        };
    }

    fn deinit(self: *CaptureSet) void {
        self.captures.deinit(self.allocator);
        self.seen.deinit();
    }

    /// Add a capture if not already present
    fn addCapture(self: *CaptureSet, name: []const u8, depth: u16, index: u16) !void {
        const key: u32 = (@as(u32, depth) << 16) | @as(u32, index);
        if (self.seen.contains(key)) return;
        try self.seen.put(key, {});
        try self.captures.append(self.allocator, .{
            .name = name,
            .depth = depth,
            .index = index,
        });
    }

    fn toSlice(self: *CaptureSet) ![]const Ir.Capture {
        return self.allocator.dupe(Ir.Capture, self.captures.items);
    }
};

/// CaptureAnalyzer - identifies captured variables in IR
pub const CaptureAnalyzer = struct {
    allocator: std.mem.Allocator,
    builder: IrBuilder,

    pub fn init(allocator: std.mem.Allocator) CaptureAnalyzer {
        return .{
            .allocator = allocator,
            .builder = IrBuilder.init(allocator),
        };
    }

    /// Analyze captures in an IR tree
    pub fn analyze(self: *CaptureAnalyzer, node: *const Ir) Error!*Ir {
        return self.analyzeNode(node);
    }

    /// Analyze a single node
    fn analyzeNode(self: *CaptureAnalyzer, node: *const Ir) Error!*Ir {
        return switch (node.*) {
            .lit, .quote_sym, .quote, .@"var", .set, .global_ref => self.copyNode(node),

            .define => |d| {
                const analyzed_value = try self.analyzeNode(d.value);
                return try self.builder.define(d.name, d.index, analyzed_value);
            },

            .let => |l| {
                // Analyze binding values
                var analyzed_bindings = std.ArrayList(Ir.Binding){};
                defer analyzed_bindings.deinit(self.allocator);

                for (l.bindings) |b| {
                    const analyzed_val = try self.analyzeNode(b.value);
                    try analyzed_bindings.append(self.allocator, .{ .name = b.name, .value = analyzed_val, .index = b.index });
                }

                // Analyze body
                const analyzed_body = try self.analyzeNode(l.body);

                const bindings_slice = try self.allocator.dupe(Ir.Binding, analyzed_bindings.items);

                return try self.builder.letExpr(bindings_slice, analyzed_body);
            },

            .lambda => |lam| {
                // Collect captures from lambda body
                var capture_set = CaptureSet.init(self.allocator);
                defer capture_set.deinit();

                // Collect all variable references with depth > 0
                try self.collectCaptures(lam.body, &capture_set);

                // Recursively analyze body (for nested lambdas)
                const analyzed_body = try self.analyzeNode(lam.body);

                // Get captures
                const captures = try capture_set.toSlice();

                return try self.builder.lambda(
                    lam.params,
                    lam.optional_params,
                    lam.key_params,
                    lam.rest_param,
                    captures,
                    analyzed_body,
                );
            },

            .@"if" => |i| {
                const cond = try self.analyzeNode(i.cond);
                const then_br = try self.analyzeNode(i.then_branch);
                const else_br = try self.analyzeNode(i.else_branch);
                return try self.builder.ifExpr(cond, then_br, else_br);
            },

            .progn => |exprs| {
                var analyzed = std.ArrayList(*const Ir){};
                defer analyzed.deinit(self.allocator);
                for (exprs) |e| {
                    const r = try self.analyzeNode(e);
                    try analyzed.append(self.allocator, r);
                }
                const slice = try self.allocator.dupe(*const Ir, analyzed.items);
                return try self.builder.progn(slice);
            },

            .loop => |w| {
                const cond = try self.analyzeNode(w.cond);
                const body = try self.analyzeNode(w.body);
                return try self.builder.loop(cond, body);
            },

            .call => |c| {
                const func = try self.analyzeNode(c.func);
                var analyzed_args = std.ArrayList(*const Ir){};
                defer analyzed_args.deinit(self.allocator);
                for (c.args) |a| {
                    const r = try self.analyzeNode(a);
                    try analyzed_args.append(self.allocator, r);
                }
                const args_slice = try self.allocator.dupe(*const Ir, analyzed_args.items);
                return try self.builder.call(func, args_slice);
            },

            // Binary operations
            .add, .sub, .mul, .div, .mod, .eq, .lt, .gt, .le, .ge, .num_eq, .cons => |b| {
                const left = try self.analyzeNode(b.left);
                const right = try self.analyzeNode(b.right);
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
                const operand = try self.analyzeNode(u.operand);
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

    /// Collect captured variables from an expression
    fn collectCaptures(self: *CaptureAnalyzer, node: *const Ir, captures: *CaptureSet) Error!void {
        switch (node.*) {
            .@"var" => |v| {
                // A variable with depth > 0 is captured from outer scope
                if (v.depth > 0 and v.depth != UNRESOLVED) {
                    try captures.addCapture(v.name, v.depth, v.index);
                }
            },

            .set => |s| {
                if (s.depth > 0 and s.depth != UNRESOLVED) {
                    try captures.addCapture(s.name, s.depth, s.index);
                }
                try self.collectCaptures(s.value, captures);
            },

            .define => |d| {
                try self.collectCaptures(d.value, captures);
            },

            .let => |l| {
                for (l.bindings) |b| {
                    try self.collectCaptures(b.value, captures);
                }
                try self.collectCaptures(l.body, captures);
            },

            .lambda => {
                // Don't descend into nested lambdas - they have their own captures
                // The nested lambda will be analyzed separately
            },

            .@"if" => |i| {
                try self.collectCaptures(i.cond, captures);
                try self.collectCaptures(i.then_branch, captures);
                try self.collectCaptures(i.else_branch, captures);
            },

            .progn => |exprs| {
                for (exprs) |e| {
                    try self.collectCaptures(e, captures);
                }
            },

            .loop => |w| {
                try self.collectCaptures(w.cond, captures);
                try self.collectCaptures(w.body, captures);
            },

            .call => |c| {
                try self.collectCaptures(c.func, captures);
                for (c.args) |a| {
                    try self.collectCaptures(a, captures);
                }
            },

            // Binary operations
            .add, .sub, .mul, .div, .mod, .eq, .lt, .gt, .le, .ge, .num_eq, .cons => |b| {
                try self.collectCaptures(b.left, captures);
                try self.collectCaptures(b.right, captures);
            },

            // Unary operations
            .car, .cdr, .consp, .symbolp, .numberp, .stringp, .vectorp, .not, .nilp => |u| {
                try self.collectCaptures(u.operand, captures);
            },

            else => {},
        }
    }

    /// Copy a node (for nodes that don't need modification)
    fn copyNode(self: *CaptureAnalyzer, node: *const Ir) Error!*Ir {
        const copy = try self.allocator.create(Ir);
        copy.* = node.*;
        return copy;
    }
};

/// Pass wrapper for pipeline integration
pub fn capture(allocator: std.mem.Allocator, node: *const Ir) Error!*Ir {
    var analyzer = CaptureAnalyzer.init(allocator);
    return analyzer.analyze(node);
}

// ============================================================================
// Tests
// ============================================================================

test "capture - no captures needed" {
    const testing = std.testing;

    // Use arena for IR nodes (they're not individually freed)
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build (lambda (x) x) - no captures
    var builder = IrBuilder.init(alloc);
    const var_x = try builder.variable("x", 0, 0);
    const params = try alloc.dupe([]const u8, &.{"x"});
    const lam = try builder.lambda(params, &.{}, &.{}, null, &.{}, var_x);

    const analyzed = try capture(alloc, lam);

    try testing.expectEqual(Ir.lambda, std.meta.activeTag(analyzed.*));
    try testing.expectEqual(@as(usize, 0), analyzed.lambda.captures.len);
}

test "capture - variable from outer scope" {
    const testing = std.testing;

    // Use arena for IR nodes (they're not individually freed)
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    // Build (lambda () y) where y is from outer scope (depth=1)
    var builder = IrBuilder.init(alloc);
    const var_y = try builder.variable("y", 1, 0); // depth=1 means from outer scope
    const params: []const []const u8 = &.{};
    const lam = try builder.lambda(params, &.{}, &.{}, null, &.{}, var_y);

    const analyzed = try capture(alloc, lam);

    try testing.expectEqual(Ir.lambda, std.meta.activeTag(analyzed.*));
    try testing.expectEqual(@as(usize, 1), analyzed.lambda.captures.len);
    try testing.expectEqualStrings("y", analyzed.lambda.captures[0].name);
    try testing.expectEqual(@as(u16, 1), analyzed.lambda.captures[0].depth);
    try testing.expectEqual(@as(u16, 0), analyzed.lambda.captures[0].index);
}

test "capture - call captures args" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);
    const var_f = try builder.variable("f", 0, 0);
    const var_y = try builder.variable("y", 1, 0);
    const args = try alloc.dupe(*const Ir, &.{ var_y });
    const call = try builder.call(var_f, args);
    const params: []const []const u8 = &.{};
    const lam = try builder.lambda(params, &.{}, &.{}, null, &.{}, call);

    const analyzed = try capture(alloc, lam);

    try testing.expectEqual(Ir.lambda, std.meta.activeTag(analyzed.*));
    try testing.expectEqual(Ir.call, std.meta.activeTag(analyzed.lambda.body.*));
    try testing.expectEqual(@as(usize, 1), analyzed.lambda.captures.len);
    try testing.expectEqualStrings("y", analyzed.lambda.captures[0].name);
}

test "capture - dedupe repeated refs" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var builder = IrBuilder.init(alloc);
    const var_y1 = try builder.variable("y", 1, 0);
    const var_y2 = try builder.variable("y", 1, 0);
    const exprs = try alloc.dupe(*const Ir, &.{ var_y1, var_y2 });
    const body = try builder.progn(exprs);
    const params: []const []const u8 = &.{};
    const lam = try builder.lambda(params, &.{}, &.{}, null, &.{}, body);

    const analyzed = try capture(alloc, lam);

    try testing.expectEqual(Ir.lambda, std.meta.activeTag(analyzed.*));
    try testing.expectEqual(@as(usize, 1), analyzed.lambda.captures.len);
    try testing.expectEqualStrings("y", analyzed.lambda.captures[0].name);
    try testing.expectEqual(@as(u16, 1), analyzed.lambda.captures[0].depth);
    try testing.expectEqual(@as(u16, 0), analyzed.lambda.captures[0].index);
}
