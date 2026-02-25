//! Z3 SMT Solver Integration for Refinement Types
//!
//! Provides Z3 bindings for checking refinement type predicates.
//! Used by the type checker to verify that refinement predicates are satisfiable
//! and that subtyping relationships hold.
//!
//! Based on dixie/src/testgen/z3.zig, extended with integer arithmetic for
//! Habu's refinement types.

const std = @import("std");

// Import Z3 C header
const c = @cImport({
    @cInclude("z3.h");
});

// Re-export opaque types
pub const Context = c.Z3_context;
pub const Solver = c.Z3_solver;
pub const Model = c.Z3_model;
pub const Ast = c.Z3_ast;
pub const Sort = c.Z3_sort;
pub const Symbol = c.Z3_symbol;
pub const Config = c.Z3_config;

/// Z3_lbool enum - satisfiability result
pub const Lbool = enum(c_int) {
    false_ = c.Z3_L_FALSE,
    undef = c.Z3_L_UNDEF,
    true_ = c.Z3_L_TRUE,
};

/// Refinement checking result
pub const RefineResult = enum {
    /// Predicate is always true (valid)
    valid,
    /// Predicate is sometimes false (invalid)
    invalid,
    /// Couldn't determine (timeout, unknown theory)
    unknown,
};

/// High-level wrapper for Z3 SMT solver
pub const SmtContext = struct {
    ctx: Context,
    solver: Solver,

    pub fn init() SmtContext {
        const cfg = c.Z3_mk_config();
        defer c.Z3_del_config(cfg);

        // Set a timeout to prevent infinite solving
        c.Z3_set_param_value(cfg, "timeout", "5000"); // 5 seconds

        const ctx = c.Z3_mk_context(cfg);
        const solver = c.Z3_mk_simple_solver(ctx);
        c.Z3_solver_inc_ref(ctx, solver);

        return .{ .ctx = ctx, .solver = solver };
    }

    pub fn deinit(self: *SmtContext) void {
        c.Z3_solver_dec_ref(self.ctx, self.solver);
        c.Z3_del_context(self.ctx);
    }

    // ========================================================================
    // Integer Operations (for refinement types)
    // ========================================================================

    /// Create an integer variable
    pub fn mkIntVar(self: *SmtContext, name: [*:0]const u8) Ast {
        const sym = c.Z3_mk_string_symbol(self.ctx, name);
        const sort = c.Z3_mk_int_sort(self.ctx);
        return c.Z3_mk_const(self.ctx, sym, sort);
    }

    /// Create an integer constant
    pub fn mkIntConst(self: *SmtContext, value: i64) Ast {
        const sort = c.Z3_mk_int_sort(self.ctx);
        return c.Z3_mk_int64(self.ctx, value, sort);
    }

    /// Create a boolean variable
    pub fn mkBoolVar(self: *SmtContext, name: [*:0]const u8) Ast {
        const sym = c.Z3_mk_string_symbol(self.ctx, name);
        const sort = c.Z3_mk_bool_sort(self.ctx);
        return c.Z3_mk_const(self.ctx, sym, sort);
    }

    /// Create boolean true
    pub fn mkTrue(self: *SmtContext) Ast {
        return c.Z3_mk_true(self.ctx);
    }

    /// Create boolean false
    pub fn mkFalse(self: *SmtContext) Ast {
        return c.Z3_mk_false(self.ctx);
    }

    // ========================================================================
    // Arithmetic Operations
    // ========================================================================

    /// Addition: a + b
    pub fn mkAdd(self: *SmtContext, a: Ast, b: Ast) Ast {
        const args = [_]Ast{ a, b };
        return c.Z3_mk_add(self.ctx, 2, &args);
    }

    /// Subtraction: a - b
    pub fn mkSub(self: *SmtContext, a: Ast, b: Ast) Ast {
        const args = [_]Ast{ a, b };
        return c.Z3_mk_sub(self.ctx, 2, &args);
    }

    /// Multiplication: a * b
    pub fn mkMul(self: *SmtContext, a: Ast, b: Ast) Ast {
        const args = [_]Ast{ a, b };
        return c.Z3_mk_mul(self.ctx, 2, &args);
    }

    /// Division: a / b
    pub fn mkDiv(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_div(self.ctx, a, b);
    }

    /// Modulo: a mod b
    pub fn mkMod(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_mod(self.ctx, a, b);
    }

    /// Unary minus: -a
    pub fn mkNeg(self: *SmtContext, a: Ast) Ast {
        const args = [_]Ast{a};
        return c.Z3_mk_unary_minus(self.ctx, args[0]);
    }

    // ========================================================================
    // Comparison Operations
    // ========================================================================

    /// Less than: a < b
    pub fn mkLt(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_lt(self.ctx, a, b);
    }

    /// Less than or equal: a <= b
    pub fn mkLe(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_le(self.ctx, a, b);
    }

    /// Greater than: a > b
    pub fn mkGt(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_gt(self.ctx, a, b);
    }

    /// Greater than or equal: a >= b
    pub fn mkGe(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_ge(self.ctx, a, b);
    }

    /// Equality: a = b
    pub fn mkEq(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_eq(self.ctx, a, b);
    }

    /// Not equal: a != b
    pub fn mkNe(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_not(self.ctx, c.Z3_mk_eq(self.ctx, a, b));
    }

    // ========================================================================
    // Boolean Operations
    // ========================================================================

    /// Logical not: !a
    pub fn mkNot(self: *SmtContext, a: Ast) Ast {
        return c.Z3_mk_not(self.ctx, a);
    }

    /// Logical and: a && b
    pub fn mkAnd(self: *SmtContext, a: Ast, b: Ast) Ast {
        const args = [_]Ast{ a, b };
        return c.Z3_mk_and(self.ctx, 2, &args);
    }

    /// Logical or: a || b
    pub fn mkOr(self: *SmtContext, a: Ast, b: Ast) Ast {
        const args = [_]Ast{ a, b };
        return c.Z3_mk_or(self.ctx, 2, &args);
    }

    /// Implication: a => b
    pub fn mkImplies(self: *SmtContext, a: Ast, b: Ast) Ast {
        return c.Z3_mk_implies(self.ctx, a, b);
    }

    /// If-then-else: if c then t else e
    pub fn mkIte(self: *SmtContext, cond: Ast, then_: Ast, else_: Ast) Ast {
        return c.Z3_mk_ite(self.ctx, cond, then_, else_);
    }

    // ========================================================================
    // Bitvector Operations (from original dixie code)
    // ========================================================================

    pub fn mkBvVar(self: *SmtContext, name: [*:0]const u8, bits: u32) Ast {
        const sym = c.Z3_mk_string_symbol(self.ctx, name);
        const sort = c.Z3_mk_bv_sort(self.ctx, bits);
        return c.Z3_mk_const(self.ctx, sym, sort);
    }

    pub fn mkBvConst(self: *SmtContext, value: u64, bits: u32) Ast {
        const sort = c.Z3_mk_bv_sort(self.ctx, bits);
        return c.Z3_mk_unsigned_int64(self.ctx, value, sort);
    }

    pub fn mkBvAnd(self: *SmtContext, t1: Ast, t2: Ast) Ast {
        return c.Z3_mk_bvand(self.ctx, t1, t2);
    }

    pub fn mkBvOr(self: *SmtContext, t1: Ast, t2: Ast) Ast {
        return c.Z3_mk_bvor(self.ctx, t1, t2);
    }

    pub fn mkBvLshr(self: *SmtContext, t1: Ast, t2: Ast) Ast {
        return c.Z3_mk_bvlshr(self.ctx, t1, t2);
    }

    pub fn mkBvShl(self: *SmtContext, t1: Ast, t2: Ast) Ast {
        return c.Z3_mk_bvshl(self.ctx, t1, t2);
    }

    // ========================================================================
    // Solver Operations
    // ========================================================================

    /// Assert a constraint
    pub fn assert_(self: *SmtContext, a: Ast) void {
        c.Z3_solver_assert(self.ctx, self.solver, a);
    }

    /// Check satisfiability
    pub fn check(self: *SmtContext) Lbool {
        return @enumFromInt(c.Z3_solver_check(self.ctx, self.solver));
    }

    /// Get model (if satisfiable)
    pub fn getModel(self: *SmtContext) ?Model {
        return c.Z3_solver_get_model(self.ctx, self.solver);
    }

    /// Evaluate an integer in a model
    pub fn evalInt(self: *SmtContext, model: Model, ast: Ast) ?i64 {
        var result: Ast = null;
        if (!c.Z3_model_eval(self.ctx, model, ast, true, &result)) {
            return null;
        }
        const r = if (result) |val| val else return null;
        var val: i64 = 0;
        if (c.Z3_get_numeral_int64(self.ctx, r, &val)) {
            return val;
        }
        return null;
    }

    /// Evaluate a bitvector in a model
    pub fn evalBvU64(self: *SmtContext, model: Model, ast: Ast) ?u64 {
        var result: Ast = null;
        if (!c.Z3_model_eval(self.ctx, model, ast, true, &result)) {
            return null;
        }
        const r = if (result) |val| val else return null;
        var val: u64 = 0;
        if (c.Z3_get_numeral_uint64(self.ctx, r, &val)) {
            return val;
        }
        return null;
    }

    /// Reset solver (clear all assertions)
    pub fn reset(self: *SmtContext) void {
        c.Z3_solver_reset(self.ctx, self.solver);
    }

    /// Push solver state
    pub fn push(self: *SmtContext) void {
        c.Z3_solver_push(self.ctx, self.solver);
    }

    /// Pop solver state
    pub fn pop(self: *SmtContext, levels: u32) void {
        c.Z3_solver_pop(self.ctx, self.solver, levels);
    }

    // ========================================================================
    // High-level Refinement Type Operations
    // ========================================================================

    /// Check if a refinement predicate is valid (always true)
    /// Returns valid if ¬P is unsatisfiable
    pub fn checkValid(self: *SmtContext, predicate: Ast) RefineResult {
        self.push();
        defer self.pop(1);

        // To check validity, assert negation and check for unsat
        self.assert_(self.mkNot(predicate));

        return switch (self.check()) {
            .false_ => .valid, // Negation is unsat, so predicate is valid
            .true_ => .invalid, // Found counterexample
            .undef => .unknown, // Timeout or unknown
        };
    }

    /// Check if predicate P implies predicate Q
    /// Used for refinement subtyping: {x : T | P} <: {x : T | Q}
    pub fn checkImplication(self: *SmtContext, p: Ast, q: Ast) RefineResult {
        return self.checkValid(self.mkImplies(p, q));
    }

    /// Check if two predicates are equivalent
    pub fn checkEquivalent(self: *SmtContext, p: Ast, q: Ast) RefineResult {
        self.push();
        defer self.pop(1);

        // P <=> Q is valid iff (P => Q) && (Q => P) is valid
        const equiv = self.mkAnd(
            self.mkImplies(p, q),
            self.mkImplies(q, p),
        );

        return self.checkValid(equiv);
    }
};

// ============================================================================
// Tests
// ============================================================================

test "smt integer satisfiability" {
    var smt = SmtContext.init();
    defer smt.deinit();

    // x > 0 is satisfiable
    const x = smt.mkIntVar("x");
    const zero = smt.mkIntConst(0);
    smt.assert_(smt.mkGt(x, zero));

    const result = smt.check();
    try std.testing.expect(result == .true_);

    if (smt.getModel()) |model| {
        const val = smt.evalInt(model, x);
        try std.testing.expect(val != null);
        try std.testing.expect(val.? > 0);
    }
}

test "smt integer unsatisfiability" {
    var smt = SmtContext.init();
    defer smt.deinit();

    // x > 0 && x < 0 is unsatisfiable
    const x = smt.mkIntVar("x");
    const zero = smt.mkIntConst(0);
    smt.assert_(smt.mkGt(x, zero));
    smt.assert_(smt.mkLt(x, zero));

    const result = smt.check();
    try std.testing.expect(result == .false_);
}

test "smt validity check" {
    var smt = SmtContext.init();
    defer smt.deinit();

    // x > 0 => x >= 0 is valid
    const x = smt.mkIntVar("x");
    const zero = smt.mkIntConst(0);

    const p = smt.mkGt(x, zero);
    const q = smt.mkGe(x, zero);

    const result = smt.checkImplication(p, q);
    try std.testing.expect(result == .valid);
}

test "smt implication failure" {
    var smt = SmtContext.init();
    defer smt.deinit();

    // x >= 0 => x > 0 is NOT valid (x = 0 is counterexample)
    const x = smt.mkIntVar("x");
    const zero = smt.mkIntConst(0);

    const p = smt.mkGe(x, zero);
    const q = smt.mkGt(x, zero);

    const result = smt.checkImplication(p, q);
    try std.testing.expect(result == .invalid);
}

test "smt arithmetic" {
    var smt = SmtContext.init();
    defer smt.deinit();

    // x + 1 > x is valid (always true for integers)
    const x = smt.mkIntVar("x");
    const one = smt.mkIntConst(1);
    const x_plus_1 = smt.mkAdd(x, one);

    const result = smt.checkValid(smt.mkGt(x_plus_1, x));
    try std.testing.expect(result == .valid);
}

test "smt push pop" {
    var smt = SmtContext.init();
    defer smt.deinit();

    const x = smt.mkIntVar("x");
    const zero = smt.mkIntConst(0);

    // Base constraint
    smt.assert_(smt.mkGt(x, zero));
    try std.testing.expect(smt.check() == .true_);

    // Push and add contradicting constraint
    smt.push();
    smt.assert_(smt.mkLt(x, zero));
    try std.testing.expect(smt.check() == .false_);

    // Pop and check original is still sat
    smt.pop(1);
    try std.testing.expect(smt.check() == .true_);
}
