//! Term ADT for type-level computation
//!
//! Terms are values that can appear within types in a dependently-typed language.
//! In Habu's QTT system, types can contain terms (e.g., the length in `(vec a n)`).
//!
//! Terms are used for:
//! - Type-level computation (evaluating types that contain expressions)
//! - Refinement predicates (e.g., `(> x 0)` in `(refine int x (> x 0))`)
//! - Dependent function/pair bodies that reference bound variables
//!
//! The Term ADT mirrors runtime values but exists at the type level during
//! type checking. During normalization, terms are evaluated to values for
//! comparison in conversion checking.

const std = @import("std");
const types = @import("type.zig");

/// A term that can appear within a type
pub const Term = union(enum) {
    // ========================================================================
    // Variables and Literals
    // ========================================================================

    /// Variable reference - either by name or de Bruijn index
    /// In type-level code, variables refer to bound parameters
    var_ref: struct {
        /// Variable name for display/debugging
        name: []const u8,
        /// De Bruijn index (0 = innermost binding)
        /// Optional: -1 means use name lookup instead
        index: i32,
    },

    /// Literal value (fixnum, string, etc.)
    /// Stored as raw 64-bit tagged value
    literal: u64,

    // ========================================================================
    // Type-level Constructors
    // ========================================================================

    /// Lambda abstraction: (lambda (x) body)
    /// Creates a type-level function
    lambda: struct {
        /// Parameter name
        param: []const u8,
        /// Parameter type (optional, for type checking)
        param_type: ?*const types.Type,
        /// Function body
        body: *const Term,
    },

    /// Pair constructor: (pair fst snd)
    /// Creates a dependent pair value
    pair: struct {
        first: *const Term,
        second: *const Term,
    },

    // ========================================================================
    // Eliminators
    // ========================================================================

    /// Function application: (f arg)
    app: struct {
        func: *const Term,
        arg: *const Term,
    },

    /// First projection: (fst pair)
    fst: *const Term,

    /// Second projection: (snd pair)
    snd: *const Term,

    // ========================================================================
    // Arithmetic and Comparisons (for refinement predicates)
    // ========================================================================

    /// Binary operation: +, -, *, /, mod, and, or
    binop: struct {
        op: BinOp,
        left: *const Term,
        right: *const Term,
    },

    /// Unary operation: not, -
    unop: struct {
        op: UnOp,
        operand: *const Term,
    },

    /// Comparison: =, <, >, <=, >=
    cmp: struct {
        op: CmpOp,
        left: *const Term,
        right: *const Term,
    },

    // ========================================================================
    // Control Flow (for complex predicates)
    // ========================================================================

    /// Conditional: (if cond then else)
    @"if": struct {
        condition: *const Term,
        then_branch: *const Term,
        else_branch: *const Term,
    },

    /// Let binding: (let ((x val)) body)
    let: struct {
        name: []const u8,
        value: *const Term,
        body: *const Term,
    },

    // ========================================================================
    // Built-in Functions (for refinements)
    // ========================================================================

    /// Built-in function call (length, null, etc.)
    builtin: struct {
        func: Builtin,
        args: []const *const Term,
    },

    // ========================================================================
    // Type Annotations
    // ========================================================================

    /// Annotated term: (the type term)
    annotated: struct {
        term: *const Term,
        type: *const types.Type,
    },

    /// Check if this term is a value (fully evaluated)
    pub fn isValue(self: Term) bool {
        return switch (self) {
            .literal, .lambda, .pair => true,
            .var_ref => false, // Could be a value if closed
            .app, .fst, .snd, .binop, .unop, .cmp => false,
            .@"if", .let, .builtin, .annotated => false,
        };
    }

    /// Check if term is a neutral form (blocked on a variable)
    pub fn isNeutral(self: Term) bool {
        return switch (self) {
            .var_ref => true,
            .app => |a| a.func.isNeutral(),
            .fst => |t| t.isNeutral(),
            .snd => |t| t.isNeutral(),
            else => false,
        };
    }

    /// Get the type annotation if present
    pub fn getAnnotation(self: Term) ?*const types.Type {
        return switch (self) {
            .annotated => |a| a.type,
            else => null,
        };
    }
};

/// Binary operations
pub const BinOp = enum {
    add, // +
    sub, // -
    mul, // *
    div, // /
    mod, // mod
    @"and", // and
    @"or", // or

    pub fn name(self: BinOp) []const u8 {
        return switch (self) {
            .add => "+",
            .sub => "-",
            .mul => "*",
            .div => "/",
            .mod => "mod",
            .@"and" => "and",
            .@"or" => "or",
        };
    }
};

/// Unary operations
pub const UnOp = enum {
    neg, // -
    not, // not

    pub fn name(self: UnOp) []const u8 {
        return switch (self) {
            .neg => "-",
            .not => "not",
        };
    }
};

/// Comparison operations
pub const CmpOp = enum {
    eq, // =
    lt, // <
    gt, // >
    le, // <=
    ge, // >=
    ne, // /=

    pub fn name(self: CmpOp) []const u8 {
        return switch (self) {
            .eq => "=",
            .lt => "<",
            .gt => ">",
            .le => "<=",
            .ge => ">=",
            .ne => "/=",
        };
    }
};

/// Built-in functions for type-level computation
pub const Builtin = enum {
    length, // (length list) -> nat
    null, // (null list) -> bool
    consp, // (consp x) -> bool
    car, // (car cons) -> a
    cdr, // (cdr cons) -> (list a)
    vectorp, // (vectorp x) -> bool
    vector_length, // (vector-length vec) -> nat
    vector_ref, // (vector-ref vec idx) -> a

    pub fn name(self: Builtin) []const u8 {
        return switch (self) {
            .length => "length",
            .null => "null",
            .consp => "consp",
            .car => "car",
            .cdr => "cdr",
            .vectorp => "vectorp",
            .vector_length => "vector-length",
            .vector_ref => "vector-ref",
        };
    }

    /// Number of arguments expected
    pub fn arity(self: Builtin) u8 {
        return switch (self) {
            .length, .null, .consp, .car, .cdr, .vectorp, .vector_length => 1,
            .vector_ref => 2,
        };
    }
};

// ============================================================================
// Term Builder
// ============================================================================

pub const TermBuilder = struct {
    arena: std.heap.ArenaAllocator,

    pub fn init(backing_allocator: std.mem.Allocator) TermBuilder {
        return .{
            .arena = std.heap.ArenaAllocator.init(backing_allocator),
        };
    }

    pub fn deinit(self: *TermBuilder) void {
        self.arena.deinit();
    }

    fn allocator(self: *TermBuilder) std.mem.Allocator {
        return self.arena.allocator();
    }

    /// Create a variable reference
    pub fn varRef(self: *TermBuilder, term_name: []const u8, index: i32) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        const name_copy = try alloc.dupe(u8, term_name);
        t.* = .{ .var_ref = .{ .name = name_copy, .index = index } };
        return t;
    }

    /// Create a variable reference by name only (index = -1)
    pub fn varByName(self: *TermBuilder, term_name: []const u8) !*Term {
        return self.varRef(term_name, -1);
    }

    /// Create a literal from a raw tagged value
    pub fn literal(self: *TermBuilder, value: u64) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .literal = value };
        return t;
    }

    /// Create a fixnum literal
    pub fn fixnum(self: *TermBuilder, n: i63) !*Term {
        // Tag as fixnum: (n << 1) | 1
        const tagged: u64 = @bitCast((@as(i64, n) << 1) | 1);
        return self.literal(tagged);
    }

    /// Create a lambda
    pub fn lambda(self: *TermBuilder, param: []const u8, param_type: ?*const types.Type, body: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        const param_copy = try alloc.dupe(u8, param);
        t.* = .{ .lambda = .{ .param = param_copy, .param_type = param_type, .body = body } };
        return t;
    }

    /// Create a pair
    pub fn pair(self: *TermBuilder, first: *const Term, second: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .pair = .{ .first = first, .second = second } };
        return t;
    }

    /// Create an application
    pub fn app(self: *TermBuilder, func: *const Term, arg: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .app = .{ .func = func, .arg = arg } };
        return t;
    }

    /// Create first projection
    pub fn fst(self: *TermBuilder, term: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .fst = term };
        return t;
    }

    /// Create second projection
    pub fn snd(self: *TermBuilder, term: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .snd = term };
        return t;
    }

    /// Create a binary operation
    pub fn binop(self: *TermBuilder, op: BinOp, left: *const Term, right: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .binop = .{ .op = op, .left = left, .right = right } };
        return t;
    }

    /// Create a unary operation
    pub fn unop(self: *TermBuilder, op: UnOp, operand: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .unop = .{ .op = op, .operand = operand } };
        return t;
    }

    /// Create a comparison
    pub fn cmp(self: *TermBuilder, op: CmpOp, left: *const Term, right: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .cmp = .{ .op = op, .left = left, .right = right } };
        return t;
    }

    /// Create an if expression
    pub fn ifExpr(self: *TermBuilder, condition: *const Term, then_branch: *const Term, else_branch: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .@"if" = .{ .condition = condition, .then_branch = then_branch, .else_branch = else_branch } };
        return t;
    }

    /// Create a let binding
    pub fn letExpr(self: *TermBuilder, let_name: []const u8, value: *const Term, body: *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        const name_copy = try alloc.dupe(u8, let_name);
        t.* = .{ .let = .{ .name = name_copy, .value = value, .body = body } };
        return t;
    }

    /// Create a builtin call
    pub fn builtin(self: *TermBuilder, func: Builtin, args: []const *const Term) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        const args_copy = try alloc.dupe(*const Term, args);
        t.* = .{ .builtin = .{ .func = func, .args = args_copy } };
        return t;
    }

    /// Create a type annotation
    pub fn annotated(self: *TermBuilder, term: *const Term, ty: *const types.Type) !*Term {
        const alloc = self.allocator();
        const t = try alloc.create(Term);
        t.* = .{ .annotated = .{ .term = term, .type = ty } };
        return t;
    }

    /// Substitute a term for a variable in a term
    /// Implements: t[x := r] - replace x with r in t
    pub fn substitute(self: *TermBuilder, term: *const Term, var_name: []const u8, replacement: *const Term) !*const Term {
        return switch (term.*) {
            .literal => term,

            .var_ref => |v| {
                if (std.mem.eql(u8, v.name, var_name)) return replacement;
                return term;
            },

            .lambda => |lam| blk: {
                // Check for variable capture
                if (std.mem.eql(u8, lam.param, var_name)) break :blk term;

                const new_body = try self.substitute(lam.body, var_name, replacement);
                if (new_body == lam.body) break :blk term;
                break :blk try self.lambda(lam.param, lam.param_type, new_body);
            },

            .pair => |p| blk: {
                const new_first = try self.substitute(p.first, var_name, replacement);
                const new_second = try self.substitute(p.second, var_name, replacement);
                if (new_first == p.first and new_second == p.second) break :blk term;
                break :blk try self.pair(new_first, new_second);
            },

            .app => |a| blk: {
                const new_func = try self.substitute(a.func, var_name, replacement);
                const new_arg = try self.substitute(a.arg, var_name, replacement);
                if (new_func == a.func and new_arg == a.arg) break :blk term;
                break :blk try self.app(new_func, new_arg);
            },

            .fst => |t| blk: {
                const new_t = try self.substitute(t, var_name, replacement);
                if (new_t == t) break :blk term;
                break :blk try self.fst(new_t);
            },

            .snd => |t| blk: {
                const new_t = try self.substitute(t, var_name, replacement);
                if (new_t == t) break :blk term;
                break :blk try self.snd(new_t);
            },

            .binop => |b| blk: {
                const new_left = try self.substitute(b.left, var_name, replacement);
                const new_right = try self.substitute(b.right, var_name, replacement);
                if (new_left == b.left and new_right == b.right) break :blk term;
                break :blk try self.binop(b.op, new_left, new_right);
            },

            .unop => |u| blk: {
                const new_operand = try self.substitute(u.operand, var_name, replacement);
                if (new_operand == u.operand) break :blk term;
                break :blk try self.unop(u.op, new_operand);
            },

            .cmp => |c| blk: {
                const new_left = try self.substitute(c.left, var_name, replacement);
                const new_right = try self.substitute(c.right, var_name, replacement);
                if (new_left == c.left and new_right == c.right) break :blk term;
                break :blk try self.cmp(c.op, new_left, new_right);
            },

            .@"if" => |i| blk: {
                const new_cond = try self.substitute(i.condition, var_name, replacement);
                const new_then = try self.substitute(i.then_branch, var_name, replacement);
                const new_else = try self.substitute(i.else_branch, var_name, replacement);
                if (new_cond == i.condition and new_then == i.then_branch and new_else == i.else_branch) break :blk term;
                break :blk try self.ifExpr(new_cond, new_then, new_else);
            },

            .let => |l| blk: {
                // Substitute in value
                const new_value = try self.substitute(l.value, var_name, replacement);

                // Check for variable capture in body
                if (std.mem.eql(u8, l.name, var_name)) {
                    if (new_value == l.value) break :blk term;
                    break :blk try self.letExpr(l.name, new_value, l.body);
                }

                const new_body = try self.substitute(l.body, var_name, replacement);
                if (new_value == l.value and new_body == l.body) break :blk term;
                break :blk try self.letExpr(l.name, new_value, new_body);
            },

            .builtin => |b| blk: {
                var changed = false;
                const new_args = try self.allocator().alloc(*const Term, b.args.len);
                for (b.args, 0..) |arg, i| {
                    new_args[i] = try self.substitute(arg, var_name, replacement);
                    if (new_args[i] != arg) changed = true;
                }

                if (!changed) {
                    self.allocator().free(new_args);
                    break :blk term;
                }
                break :blk try self.builtin(b.func, new_args);
            },

            .annotated => |a| blk: {
                const new_term = try self.substitute(a.term, var_name, replacement);
                if (new_term == a.term) break :blk term;
                break :blk try self.annotated(new_term, a.type);
            },
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "term creation" {
    const testing = std.testing;

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create a simple predicate: (> x 0)
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const gt_x_0 = try builder.cmp(.gt, x, zero);

    try testing.expect(!gt_x_0.isValue());
    try testing.expect(x.isNeutral()); // Variable is neutral
    try testing.expect(!gt_x_0.isNeutral()); // Comparison is not neutral (not blocked on head variable)
}

test "lambda and application" {
    const testing = std.testing;

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create: (lambda (x) x)
    const x = try builder.varByName("x");
    const identity = try builder.lambda("x", null, x);

    try testing.expect(identity.isValue()); // Lambda is a value

    // Create application: ((lambda (x) x) 42)
    const arg = try builder.fixnum(42);
    const applied = try builder.app(identity, arg);

    try testing.expect(!applied.isValue()); // Application is not a value
}

test "builtin functions" {
    const testing = std.testing;

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create: (length xs)
    const xs = try builder.varByName("xs");
    const args = [_]*const Term{xs};
    const len_call = try builder.builtin(.length, &args);
    _ = len_call;

    try testing.expectEqual(@as(u8, 1), Builtin.length.arity());
    try testing.expectEqualStrings("length", Builtin.length.name());
}
