//! Refinement Type Checker
//!
//! Translates Habu refinement predicates to Z3 formulas and checks them.
//! Used by the type checker to verify refinement subtyping.
//!
//! Example: {x : int | x > 0} <: {x : int | x >= 0}
//! Translates to: forall x. (x > 0) => (x >= 0)  -- valid!

const std = @import("std");
const smt = @import("smt.zig");
const term_mod = @import("term.zig");
const type_mod = @import("type.zig");

const SmtContext = smt.SmtContext;
const RefineResult = smt.RefineResult;
const Ast = smt.Ast;
const Term = term_mod.Term;
const BinOp = term_mod.BinOp;
const UnOp = term_mod.UnOp;
const CmpOp = term_mod.CmpOp;
const Type = type_mod.Type;

/// Refinement type checker using SMT
pub const RefinementChecker = struct {
    allocator: std.mem.Allocator,
    smt_ctx: SmtContext,

    /// Variables in scope (name -> Z3 Ast)
    var_map: std.StringHashMap(Ast),

    pub fn init(allocator: std.mem.Allocator) RefinementChecker {
        return .{
            .allocator = allocator,
            .smt_ctx = SmtContext.init(),
            .var_map = std.StringHashMap(Ast).init(allocator),
        };
    }

    pub fn deinit(self: *RefinementChecker) void {
        self.var_map.deinit();
        self.smt_ctx.deinit();
    }

    /// Check if refinement {x : T | P} is valid (P is satisfiable)
    pub fn checkSatisfiable(self: *RefinementChecker, predicate: *const Term, var_name: []const u8) RefineResult {
        self.smt_ctx.reset();
        self.var_map.clearRetainingCapacity();

        // Create null-terminated name for Z3 C API
        var name_buf: [256]u8 = undefined;
        if (var_name.len >= name_buf.len) return .unknown;
        @memcpy(name_buf[0..var_name.len], var_name);
        name_buf[var_name.len] = 0;

        // Create variable for refinement binder
        const z3_var = self.smt_ctx.mkIntVar(@ptrCast(&name_buf));
        self.var_map.put(var_name, z3_var) catch return .unknown;

        // Translate predicate to Z3
        const z3_pred = self.translateTerm(predicate) orelse return .unknown;

        // Check if P is satisfiable
        self.smt_ctx.assert_(z3_pred);
        return switch (self.smt_ctx.check()) {
            .true_ => .valid, // Satisfiable
            .false_ => .invalid, // Unsatisfiable (impossible type!)
            .undef => .unknown,
        };
    }

    /// Check refinement subtyping: {x : T | P} <: {x : T | Q}
    /// Valid iff P => Q (whenever P holds, Q also holds)
    pub fn checkSubtype(
        self: *RefinementChecker,
        sub_pred: *const Term,
        super_pred: *const Term,
        var_name: []const u8,
    ) RefineResult {
        self.smt_ctx.reset();
        self.var_map.clearRetainingCapacity();

        // Create null-terminated name for Z3 C API
        var name_buf: [256]u8 = undefined;
        if (var_name.len >= name_buf.len) return .unknown;
        @memcpy(name_buf[0..var_name.len], var_name);
        name_buf[var_name.len] = 0;

        const z3_var = self.smt_ctx.mkIntVar(@ptrCast(&name_buf));
        self.var_map.put(var_name, z3_var) catch return .unknown;

        // Translate both predicates
        const z3_sub = self.translateTerm(sub_pred) orelse return .unknown;
        const z3_super = self.translateTerm(super_pred) orelse return .unknown;

        // Check P => Q
        return self.smt_ctx.checkImplication(z3_sub, z3_super);
    }

    /// Check if two refinements are equivalent
    pub fn checkEquivalent(
        self: *RefinementChecker,
        pred1: *const Term,
        pred2: *const Term,
        var_name: []const u8,
    ) RefineResult {
        self.smt_ctx.reset();
        self.var_map.clearRetainingCapacity();

        var name_buf: [256]u8 = undefined;
        if (var_name.len >= name_buf.len) return .unknown;
        @memcpy(name_buf[0..var_name.len], var_name);
        name_buf[var_name.len] = 0;

        const z3_var = self.smt_ctx.mkIntVar(@ptrCast(&name_buf));
        self.var_map.put(var_name, z3_var) catch return .unknown;

        const z3_p1 = self.translateTerm(pred1) orelse return .unknown;
        const z3_p2 = self.translateTerm(pred2) orelse return .unknown;

        return self.smt_ctx.checkEquivalent(z3_p1, z3_p2);
    }

    /// Translate a Habu Term to a Z3 Ast
    fn translateTerm(self: *RefinementChecker, t: *const Term) ?Ast {
        return switch (t.*) {
            // Variables - look up in scope
            .var_ref => |v| self.var_map.get(v.name),

            // Literals - extract fixnum value
            .literal => |raw| {
                // Check if it's a fixnum (bit0 = 1)
                if (raw & 1 == 1) {
                    // Extract fixnum value
                    const val: i64 = @bitCast(raw);
                    return self.smt_ctx.mkIntConst(val >> 1);
                }
                // Could be nil (false) or t (true)
                if (raw == 0) return self.smt_ctx.mkFalse(); // nil
                if (raw == 2) return self.smt_ctx.mkTrue(); // t symbol
                return null;
            },

            // Binary operations
            .binop => |op| {
                const left = self.translateTerm(op.left) orelse return null;
                const right = self.translateTerm(op.right) orelse return null;

                return switch (op.op) {
                    .add => self.smt_ctx.mkAdd(left, right),
                    .sub => self.smt_ctx.mkSub(left, right),
                    .mul => self.smt_ctx.mkMul(left, right),
                    .div => self.smt_ctx.mkDiv(left, right),
                    .mod => self.smt_ctx.mkMod(left, right),
                    .@"and" => self.smt_ctx.mkAnd(left, right),
                    .@"or" => self.smt_ctx.mkOr(left, right),
                };
            },

            // Unary operations
            .unop => |op| {
                const operand = self.translateTerm(op.operand) orelse return null;

                return switch (op.op) {
                    .neg => self.smt_ctx.mkNeg(operand),
                    .not => self.smt_ctx.mkNot(operand),
                };
            },

            // Comparisons
            .cmp => |op| {
                const left = self.translateTerm(op.left) orelse return null;
                const right = self.translateTerm(op.right) orelse return null;

                return switch (op.op) {
                    .lt => self.smt_ctx.mkLt(left, right),
                    .le => self.smt_ctx.mkLe(left, right),
                    .gt => self.smt_ctx.mkGt(left, right),
                    .ge => self.smt_ctx.mkGe(left, right),
                    .eq => self.smt_ctx.mkEq(left, right),
                    .ne => self.smt_ctx.mkNe(left, right),
                };
            },

            // Lambda, application, pairs - not directly supported in SMT
            // These would need to be normalized/evaluated first
            .lambda, .app, .pair, .fst, .snd, .annotated, .@"if", .let => null,

            // Builtins - some can be translated
            .builtin => null, // TODO: handle length, null, etc.
        };
    }
};

// ============================================================================
// Tests
// ============================================================================

test "refinement satisfiability" {
    const allocator = std.testing.allocator;
    var checker = RefinementChecker.init(allocator);
    defer checker.deinit();

    var builder = term_mod.TermBuilder.init(allocator);
    defer builder.deinit();

    // {x : int | x > 0} - satisfiable
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const pred = try builder.cmp(.gt, x, zero);

    const result = checker.checkSatisfiable(pred, "x");
    try std.testing.expect(result == .valid);
}

test "refinement unsatisfiable" {
    const allocator = std.testing.allocator;
    var checker = RefinementChecker.init(allocator);
    defer checker.deinit();

    var builder = term_mod.TermBuilder.init(allocator);
    defer builder.deinit();

    // {x : int | x > 0 and x < 0} - unsatisfiable!
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const gt_zero = try builder.cmp(.gt, x, zero);
    const lt_zero = try builder.cmp(.lt, x, zero);
    const pred = try builder.binop(.@"and", gt_zero, lt_zero);

    const result = checker.checkSatisfiable(pred, "x");
    try std.testing.expect(result == .invalid);
}

test "refinement subtyping valid" {
    const allocator = std.testing.allocator;
    var checker = RefinementChecker.init(allocator);
    defer checker.deinit();

    var builder = term_mod.TermBuilder.init(allocator);
    defer builder.deinit();

    // {x | x > 0} <: {x | x >= 0}  -- valid!
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const gt_zero = try builder.cmp(.gt, x, zero);
    const ge_zero = try builder.cmp(.ge, x, zero);

    const result = checker.checkSubtype(gt_zero, ge_zero, "x");
    try std.testing.expect(result == .valid);
}

test "refinement subtyping invalid" {
    const allocator = std.testing.allocator;
    var checker = RefinementChecker.init(allocator);
    defer checker.deinit();

    var builder = term_mod.TermBuilder.init(allocator);
    defer builder.deinit();

    // {x | x >= 0} <: {x | x > 0}  -- invalid! (x=0 is counterexample)
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const gt_zero = try builder.cmp(.gt, x, zero);
    const ge_zero = try builder.cmp(.ge, x, zero);

    const result = checker.checkSubtype(ge_zero, gt_zero, "x");
    try std.testing.expect(result == .invalid);
}

test "refinement equivalence" {
    const allocator = std.testing.allocator;
    var checker = RefinementChecker.init(allocator);
    defer checker.deinit();

    var builder = term_mod.TermBuilder.init(allocator);
    defer builder.deinit();

    // {x | x > 0} == {x | x >= 1}  -- equivalent for integers!
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const one = try builder.fixnum(1);
    const gt_zero = try builder.cmp(.gt, x, zero);
    const ge_one = try builder.cmp(.ge, x, one);

    const result = checker.checkEquivalent(gt_zero, ge_one, "x");
    try std.testing.expect(result == .valid);
}

test "refinement with arithmetic" {
    const allocator = std.testing.allocator;
    var checker = RefinementChecker.init(allocator);
    defer checker.deinit();

    var builder = term_mod.TermBuilder.init(allocator);
    defer builder.deinit();

    // {x | x + 1 > 1} <: {x | x >= 0}  -- valid!
    const x = try builder.varByName("x");
    const zero = try builder.fixnum(0);
    const one = try builder.fixnum(1);
    const x_plus_1 = try builder.binop(.add, x, one);
    const pred1 = try builder.cmp(.gt, x_plus_1, one);
    const pred2 = try builder.cmp(.ge, x, zero);

    const result = checker.checkSubtype(pred1, pred2, "x");
    try std.testing.expect(result == .valid);
}
