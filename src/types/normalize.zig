//! Normalizer for type-level computation
//!
//! The normalizer evaluates terms to their normal forms. This is essential for:
//! - Type equality checking (two types are equal if their normal forms are equal)
//! - Evaluating type applications (e.g., `(Vec A n)` where `n` is a term)
//! - Checking refinement predicates
//!
//! Two evaluation strategies:
//! - WHNF (Weak Head Normal Form): evaluate until head is a value or neutral
//! - Full normalization: recursively normalize all subterms
//!
//! The normalizer uses an environment that maps variable names to their values.

const std = @import("std");
const term_mod = @import("term.zig");
const type_mod = @import("type.zig");

const Term = term_mod.Term;
const TermBuilder = term_mod.TermBuilder;
const BinOp = term_mod.BinOp;
const UnOp = term_mod.UnOp;
const CmpOp = term_mod.CmpOp;
const Builtin = term_mod.Builtin;
const Type = type_mod.Type;

/// A value in the normalization environment
pub const EnvValue = union(enum) {
    /// A term value
    term: *const Term,
    /// A closure (for lambda evaluation)
    closure: struct {
        param: []const u8,
        body: *const Term,
        env: *const NormEnv,
    },
};

/// Normalization environment - maps variable names to values
pub const NormEnv = struct {
    allocator: std.mem.Allocator,
    bindings: std.StringHashMapUnmanaged(EnvValue),
    parent: ?*const NormEnv,

    pub fn init(allocator: std.mem.Allocator) NormEnv {
        return .{
            .allocator = allocator,
            .bindings = std.StringHashMapUnmanaged(EnvValue){},
            .parent = null,
        };
    }

    pub fn initWithParent(allocator: std.mem.Allocator, parent: *const NormEnv) NormEnv {
        return .{
            .allocator = allocator,
            .bindings = std.StringHashMapUnmanaged(EnvValue){},
            .parent = parent,
        };
    }

    pub fn deinit(self: *NormEnv) void {
        self.bindings.deinit(self.allocator);
    }

    /// Bind a variable to a term
    pub fn bind(self: *NormEnv, name: []const u8, value: *const Term) !void {
        try self.bindings.put(self.allocator, name, .{ .term = value });
    }

    /// Bind a variable to a closure
    pub fn bindClosure(self: *NormEnv, name: []const u8, param: []const u8, body: *const Term, env: *const NormEnv) !void {
        try self.bindings.put(self.allocator, name, .{ .closure = .{
            .param = param,
            .body = body,
            .env = env,
        } });
    }

    /// Look up a variable
    pub fn lookup(self: *const NormEnv, name: []const u8) ?EnvValue {
        if (self.bindings.get(name)) |v| {
            return v;
        }
        if (self.parent) |p| {
            return p.lookup(name);
        }
        return null;
    }
};

/// Normalizer for type-level terms
pub const Normalizer = struct {
    allocator: std.mem.Allocator,
    builder: TermBuilder,
    /// Fuel limit to prevent infinite loops
    fuel: usize,

    pub fn init(allocator: std.mem.Allocator) Normalizer {
        return .{
            .allocator = allocator,
            .builder = TermBuilder.init(allocator),
            .fuel = 10000, // Default fuel
        };
    }

    pub fn initWithFuel(allocator: std.mem.Allocator, fuel: usize) Normalizer {
        return .{
            .allocator = allocator,
            .builder = TermBuilder.init(allocator),
            .fuel = fuel,
        };
    }

    pub fn deinit(self: *Normalizer) void {
        self.builder.deinit();
    }

    /// Normalize a term to Weak Head Normal Form
    /// Returns the normalized term, or error if fuel exhausted
    pub fn whnf(self: *Normalizer, t: *const Term, env: *const NormEnv) NormError!*const Term {
        if (self.fuel == 0) return error.FuelExhausted;
        self.fuel -= 1;

        return switch (t.*) {
            // Values are already in WHNF
            .literal, .lambda, .pair => t,

            // Variable: look up in environment
            .var_ref => |v| {
                if (env.lookup(v.name)) |val| {
                    return switch (val) {
                        .term => |term| self.whnf(term, env),
                        .closure => t, // Return original if closure (needs application)
                    };
                }
                return t; // Free variable - neutral
            },

            // Application: evaluate function, apply if lambda
            .app => |a| {
                const func = try self.whnf(a.func, env);
                return switch (func.*) {
                    .lambda => |lam| {
                        // Beta reduction: ((λx.body) arg) → body[x := arg]
                        var new_env = NormEnv.initWithParent(self.allocator, env);
                        defer new_env.deinit();
                        try new_env.bind(lam.param, a.arg);
                        return self.whnf(lam.body, &new_env);
                    },
                    else => {
                        // Neutral: return application with normalized function
                        if (func == a.func) return t;
                        return try self.builder.app(func, a.arg);
                    },
                };
            },

            // Projections
            .fst => |inner| {
                const normalized = try self.whnf(inner, env);
                return switch (normalized.*) {
                    .pair => |p| self.whnf(p.first, env),
                    else => {
                        if (normalized == inner) return t;
                        return try self.builder.fst(normalized);
                    },
                };
            },

            .snd => |inner| {
                const normalized = try self.whnf(inner, env);
                return switch (normalized.*) {
                    .pair => |p| self.whnf(p.second, env),
                    else => {
                        if (normalized == inner) return t;
                        return try self.builder.snd(normalized);
                    },
                };
            },

            // Binary operations
            .binop => |b| {
                const left = try self.whnf(b.left, env);
                const right = try self.whnf(b.right, env);
                if (left.* == .literal and right.* == .literal) {
                    const result = try self.evalBinOp(b.op, left.literal, right.literal);
                    return try self.builder.literal(result);
                }
                if (left == b.left and right == b.right) return t;
                return try self.builder.binop(b.op, left, right);
            },

            // Unary operations
            .unop => |u| {
                const operand = try self.whnf(u.operand, env);
                if (operand.* == .literal) {
                    const result = try self.evalUnOp(u.op, operand.literal);
                    return try self.builder.literal(result);
                }
                if (operand == u.operand) return t;
                return try self.builder.unop(u.op, operand);
            },

            // Comparisons
            .cmp => |c| {
                const left = try self.whnf(c.left, env);
                const right = try self.whnf(c.right, env);
                if (left.* == .literal and right.* == .literal) {
                    const result = self.evalCmpOp(c.op, left.literal, right.literal);
                    // true = 1, false = nil (0)
                    return try self.builder.literal(if (result) @as(u64, 1) else @as(u64, 0));
                }
                if (left == c.left and right == c.right) return t;
                return try self.builder.cmp(c.op, left, right);
            },

            // Conditional
            .@"if" => |cond| {
                const test_val = try self.whnf(cond.condition, env);
                if (test_val.* == .literal) {
                    // nil (0) is false, anything else is true
                    if (test_val.literal == 0) {
                        return self.whnf(cond.else_branch, env);
                    } else {
                        return self.whnf(cond.then_branch, env);
                    }
                }
                return t; // Neutral - can't reduce
            },

            // Let binding
            .let => |l| {
                var new_env = NormEnv.initWithParent(self.allocator, env);
                defer new_env.deinit();
                try new_env.bind(l.name, l.value);
                return self.whnf(l.body, &new_env);
            },

            // Builtins - evaluate if possible
            .builtin => |b| {
                const reduced = try self.evalBuiltin(b.func, b.args, env);
                return if (reduced) |term| term else t;
            },

            // Annotations - normalize the inner term
            .annotated => |a| {
                return self.whnf(a.term, env);
            },
        };
    }

    /// Full normalization - recursively normalize all subterms
    pub fn normalize(self: *Normalizer, t: *const Term, env: *const NormEnv) NormError!*const Term {
        const whnf_result = try self.whnf(t, env);

        return switch (whnf_result.*) {
            .literal, .var_ref => whnf_result,

            .lambda => {
                // Normalize under the binder (need fresh variable)
                // For now, just return the lambda as-is
                // Full normalization would require alpha-renaming
                return whnf_result;
            },

            .pair => |p| {
                const first = try self.normalize(p.first, env);
                const second = try self.normalize(p.second, env);
                if (first == p.first and second == p.second) return whnf_result;
                return try self.builder.pair(first, second);
            },

            .app => |a| {
                const func = try self.normalize(a.func, env);
                const arg = try self.normalize(a.arg, env);
                if (func == a.func and arg == a.arg) return whnf_result;
                return try self.builder.app(func, arg);
            },

            .fst => |inner| {
                const normalized = try self.normalize(inner, env);
                if (normalized == inner) return whnf_result;
                return try self.builder.fst(normalized);
            },

            .snd => |inner| {
                const normalized = try self.normalize(inner, env);
                if (normalized == inner) return whnf_result;
                return try self.builder.snd(normalized);
            },

            .binop => |b| {
                const left = try self.normalize(b.left, env);
                const right = try self.normalize(b.right, env);
                if (left == b.left and right == b.right) return whnf_result;
                return try self.builder.binop(b.op, left, right);
            },

            .unop => |u| {
                const operand = try self.normalize(u.operand, env);
                if (operand == u.operand) return whnf_result;
                return try self.builder.unop(u.op, operand);
            },

            .cmp => |c| {
                const left = try self.normalize(c.left, env);
                const right = try self.normalize(c.right, env);
                if (left == c.left and right == c.right) return whnf_result;
                return try self.builder.cmp(c.op, left, right);
            },

            .@"if" => |cond| {
                const test_val = try self.normalize(cond.condition, env);
                const then_val = try self.normalize(cond.then_branch, env);
                const else_val = try self.normalize(cond.else_branch, env);
                if (test_val == cond.condition and then_val == cond.then_branch and else_val == cond.else_branch) {
                    return whnf_result;
                }
                return try self.builder.ifExpr(test_val, then_val, else_val);
            },

            .let => whnf_result, // Already reduced by whnf

            .builtin => |b| {
                var normalized_args = try self.allocator.alloc(*const Term, b.args.len);
                var changed = false;
                for (b.args, 0..) |arg, i| {
                    normalized_args[i] = try self.normalize(arg, env);
                    if (normalized_args[i] != arg) changed = true;
                }
                if (!changed) {
                    self.allocator.free(normalized_args);
                    return whnf_result;
                }
                return try self.builder.builtin(b.func, normalized_args);
            },

            .annotated => |a| {
                const inner = try self.normalize(a.term, env);
                if (inner == a.term) return whnf_result;
                return try self.builder.annotated(inner, a.type);
            },
        };
    }

    /// Check if two terms are definitionally equal (after normalization)
    pub fn convertible(self: *Normalizer, t1: *const Term, t2: *const Term, env: *const NormEnv) NormError!bool {
        const n1 = try self.normalize(t1, env);
        const n2 = try self.normalize(t2, env);
        return self.termEqual(n1, n2);
    }

    /// Structural equality of normalized terms
    fn termEqual(self: *Normalizer, t1: *const Term, t2: *const Term) bool {
        if (t1 == t2) return true;

        return switch (t1.*) {
            .literal => |l1| switch (t2.*) {
                .literal => |l2| l1 == l2,
                else => false,
            },
            .var_ref => |v1| switch (t2.*) {
                .var_ref => |v2| {
                    if (v1.index >= 0 and v2.index >= 0) {
                        return v1.index == v2.index;
                    }
                    return std.mem.eql(u8, v1.name, v2.name);
                },
                else => false,
            },
            .lambda => |l1| switch (t2.*) {
                .lambda => |l2| {
                    // Alpha equivalence - names can differ
                    // For full alpha-eq, would need substitution
                    return self.termEqual(l1.body, l2.body);
                },
                else => false,
            },
            .pair => |p1| switch (t2.*) {
                .pair => |p2| self.termEqual(p1.first, p2.first) and self.termEqual(p1.second, p2.second),
                else => false,
            },
            .app => |a1| switch (t2.*) {
                .app => |a2| self.termEqual(a1.func, a2.func) and self.termEqual(a1.arg, a2.arg),
                else => false,
            },
            .fst => |f1| switch (t2.*) {
                .fst => |f2| self.termEqual(f1, f2),
                else => false,
            },
            .snd => |s1| switch (t2.*) {
                .snd => |s2| self.termEqual(s1, s2),
                else => false,
            },
            .binop => |b1| switch (t2.*) {
                .binop => |b2| b1.op == b2.op and self.termEqual(b1.left, b2.left) and self.termEqual(b1.right, b2.right),
                else => false,
            },
            .unop => |un1| switch (t2.*) {
                .unop => |un2| un1.op == un2.op and self.termEqual(un1.operand, un2.operand),
                else => false,
            },
            .cmp => |c1| switch (t2.*) {
                .cmp => |c2| c1.op == c2.op and self.termEqual(c1.left, c2.left) and self.termEqual(c1.right, c2.right),
                else => false,
            },
            .@"if" => |if1| switch (t2.*) {
                .@"if" => |if2| {
                    return self.termEqual(if1.condition, if2.condition) and
                        self.termEqual(if1.then_branch, if2.then_branch) and
                        self.termEqual(if1.else_branch, if2.else_branch);
                },
                else => false,
            },
            .let => |l1| switch (t2.*) {
                .let => |l2| {
                    return std.mem.eql(u8, l1.name, l2.name) and
                        self.termEqual(l1.value, l2.value) and
                        self.termEqual(l1.body, l2.body);
                },
                else => false,
            },
            .builtin => |b1| switch (t2.*) {
                .builtin => |b2| {
                    if (b1.func != b2.func) return false;
                    if (b1.args.len != b2.args.len) return false;
                    for (b1.args, b2.args) |a1, a2| {
                        if (!self.termEqual(a1, a2)) return false;
                    }
                    return true;
                },
                else => false,
            },
            .annotated => |a1| switch (t2.*) {
                .annotated => |a2| self.termEqual(a1.term, a2.term),
                else => self.termEqual(a1.term, t2),
            },
        };
    }

    // ========================================================================
    // Primitive Operations
    // ========================================================================

    fn evalBinOp(self: *Normalizer, op: BinOp, left: u64, right: u64) !u64 {
        _ = self;
        // Decode fixnums (bit0=1, value >> 1)
        const l: i64 = @bitCast(left);
        const r: i64 = @bitCast(right);

        // Check if both are fixnums
        if ((left & 1) == 0 or (right & 1) == 0) {
            return error.TypeError;
        }

        const lv = l >> 1;
        const rv = r >> 1;

        const result: i64 = switch (op) {
            .add => lv + rv,
            .sub => lv - rv,
            .mul => lv * rv,
            .div => if (rv == 0) return error.DivisionByZero else @divTrunc(lv, rv),
            .mod => if (rv == 0) return error.DivisionByZero else @mod(lv, rv),
            .@"and" => if (lv != 0 and rv != 0) 1 else 0,
            .@"or" => if (lv != 0 or rv != 0) 1 else 0,
        };

        // Re-encode as fixnum
        return @bitCast((result << 1) | 1);
    }

    fn evalUnOp(self: *Normalizer, op: UnOp, operand: u64) !u64 {
        _ = self;
        const v: i64 = @bitCast(operand);

        if ((operand & 1) == 0) {
            return error.TypeError;
        }

        const val = v >> 1;

        const result: i64 = switch (op) {
            .neg => -val,
            .not => if (val == 0) 1 else 0,
        };

        return @bitCast((result << 1) | 1);
    }

    fn evalCmpOp(self: *Normalizer, op: CmpOp, left: u64, right: u64) bool {
        _ = self;
        const l: i64 = @bitCast(left);
        const r: i64 = @bitCast(right);

        // For fixnums, compare the values
        if ((left & 1) == 1 and (right & 1) == 1) {
            const lv = l >> 1;
            const rv = r >> 1;
            return switch (op) {
                .eq => lv == rv,
                .ne => lv != rv,
                .lt => lv < rv,
                .gt => lv > rv,
                .le => lv <= rv,
                .ge => lv >= rv,
            };
        }

        // For other values, only equality comparison makes sense
        return switch (op) {
            .eq => left == right,
            .ne => left != right,
            else => false,
        };
    }

    fn evalBuiltin(self: *Normalizer, func: Builtin, args: []const *const Term, env: *const NormEnv) !?*const Term {
        _ = env;
        _ = self;
        _ = args;

        // Builtin evaluation would require runtime value inspection
        // For now, return null to indicate it can't be reduced
        return switch (func) {
            .length, .null, .consp, .car, .cdr, .vectorp, .vector_length, .vector_ref => null,
        };
    }
};

pub const NormError = error{
    FuelExhausted,
    TypeError,
    DivisionByZero,
    OutOfMemory,
};

// ============================================================================
// Tests
// ============================================================================

test "normalize literal" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();
    const five = try builder.fixnum(5);

    const result = try normalizer.whnf(five, &env);
    try testing.expect(result == five); // Literals are already normalized
}

test "whnf builtin returns original when not reducible" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    const xs = try builder.varByName("xs");
    const args = [_]*const Term{xs};
    const len_call = try builder.builtin(.length, &args);

    const result = try normalizer.whnf(len_call, &env);
    try testing.expect(result == len_call);
}

test "normalize arithmetic" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create: (+ 3 4) = 7
    const three = try builder.fixnum(3);
    const four = try builder.fixnum(4);
    const add_expr = try builder.binop(.add, three, four);

    const result = try normalizer.whnf(add_expr, &env);
    try testing.expect(result.* == .literal);

    // Decode result: should be 7
    const val: i64 = @bitCast(result.literal);
    try testing.expectEqual(@as(i64, 7), val >> 1);
}

test "normalize beta reduction" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create: ((lambda (x) x) 42) = 42
    const x = try builder.varByName("x");
    const identity = try builder.lambda("x", null, x);
    const forty_two = try builder.fixnum(42);
    const applied = try builder.app(identity, forty_two);

    const result = try normalizer.whnf(applied, &env);

    // Result should be the literal 42
    try testing.expect(result.* == .literal);
    const val: i64 = @bitCast(result.literal);
    try testing.expectEqual(@as(i64, 42), val >> 1);
}

test "normalize comparison" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create: (> 5 3) = true (1)
    const five = try builder.fixnum(5);
    const three = try builder.fixnum(3);
    const gt_expr = try builder.cmp(.gt, five, three);

    const result = try normalizer.whnf(gt_expr, &env);
    try testing.expect(result.* == .literal);
    try testing.expectEqual(@as(u64, 1), result.literal); // true
}

test "normalize conditional" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create: (if true 1 2) = 1
    const true_lit = try builder.literal(1); // Non-nil = true
    const one = try builder.fixnum(1);
    const two = try builder.fixnum(2);
    const if_expr = try builder.ifExpr(true_lit, one, two);

    const result = try normalizer.whnf(if_expr, &env);
    try testing.expect(result.* == .literal);

    // Should be fixnum 1
    const val: i64 = @bitCast(result.literal);
    try testing.expectEqual(@as(i64, 1), val >> 1);
}

test "convertible terms" {
    const testing = std.testing;

    var normalizer = Normalizer.init(testing.allocator);
    defer normalizer.deinit();
    var env = NormEnv.init(testing.allocator);
    defer env.deinit();

    var builder = TermBuilder.init(testing.allocator);
    defer builder.deinit();

    // (+ 2 3) should be convertible with 5
    const two = try builder.fixnum(2);
    const three = try builder.fixnum(3);
    const add_expr = try builder.binop(.add, two, three);

    const five = try builder.fixnum(5);

    const result = try normalizer.convertible(add_expr, five, &env);
    try testing.expect(result);
}
