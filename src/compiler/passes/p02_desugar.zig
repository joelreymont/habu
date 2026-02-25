//! Desugar Pass
//!
//! Transforms syntactic sugar in S-expressions to core forms.
//! This is a pure transformation pass with no side effects.
//!
//! Input: Value (S-expression)
//! Output: Value (S-expression with sugar removed)
//!
//! Transformations:
//!   (let* ((a 1) (b 2)) body) → (let ((a 1)) (let ((b 2)) body))
//!   (cond (c1 e1) (c2 e2) (t e3)) → (if c1 e1 (if c2 e2 e3))
//!   (and a b c) → (if a (if b c nil) nil)
//!   (or a b) → (let ((g a)) (if g g b))
//!   (defun f (args) body) → (defun f (args) body')  ; body recursively desugared
//!   (when test body...) → (if test (progn body...) nil)
//!   (unless test body...) → (if test nil (progn body...))

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const runtime = @import("../../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const Heap = runtime.Heap;
const builtins_mod = @import("../../runtime/builtins.zig");

/// Desugarer transforms syntactic sugar to core forms
pub const Desugarer = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    gensym_counter: u32,
    builtins: *const builtins_mod.BuiltinSymbols,

    /// Special form identifiers for dispatch
    const Form = enum {
        @"let*",
        cond,
        @"and",
        @"or",
        defun,
        when,
        unless,
    };

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, builtins: *const builtins_mod.BuiltinSymbols) Desugarer {
        return .{
            .allocator = allocator,
            .heap = heap,
            .gensym_counter = 0,
            .builtins = builtins,
        };
    }

    /// Generate a unique symbol name for temporary bindings
    fn gensym(self: *Desugarer) Error!Value {
        var buf: [32]u8 = undefined;
        const name = try std.fmt.bufPrint(&buf, "#g{d}", .{self.gensym_counter});
        self.gensym_counter += 1;
        return self.heap.intern(name);
    }

    pub const Error = error{OutOfMemory, NoSpaceLeft};

    /// Main entry point - desugar an expression recursively
    pub fn desugar(self: *Desugarer, expr: Value) Error!Value {
        // Atoms pass through unchanged
        if (expr.isNil()) return expr;
        if (!expr.isCons()) return expr;

        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check if head is a symbol
        if (head.isSymbol()) {
            // Skip quote forms - don't desugar inside quotes
            if (head.raw == self.builtins.sym_quote.raw or head.raw == self.builtins.sym_quasiquote.raw) {
                return expr;
            }

            // Check if head is a sugar form
            if (self.sugarForm(head)) |form| {
                return switch (form) {
                    .@"let*" => self.desugarLetStar(tail),
                    .cond => self.desugarCond(tail),
                    .@"and" => self.desugarAnd(tail),
                    .@"or" => self.desugarOr(tail),
                    .defun => self.desugarDefun(tail),
                    .when => self.desugarWhen(tail),
                    .unless => self.desugarUnless(tail),
                };
            }
        }

        // Not a sugar form - recurse on list elements
        return self.desugarList(expr);
    }

    /// Desugar all elements of a list
    fn desugarList(self: *Desugarer, list: Value) Error!Value {
        if (list.isNil()) return Value.nil;
        if (!list.isCons()) return list;

        const cons = list.toPtr(Cons);
        const desugared_car = try self.desugar(cons.car);
        const desugared_cdr = try self.desugarList(cons.cdr);

        // If nothing changed, return original
        if (desugared_car.raw == cons.car.raw and desugared_cdr.raw == cons.cdr.raw) {
            return list;
        }

        return try self.heap.allocCons(desugared_car, desugared_cdr);
    }

    fn sugarForm(self: *Desugarer, head: Value) ?Form {
        const b = self.builtins;
        if (head.raw == b.sym_let_star.raw) return .@"let*";
        if (head.raw == b.sym_cond.raw) return .cond;
        if (head.raw == b.sym_and.raw) return .@"and";
        if (head.raw == b.sym_or.raw) return .@"or";
        if (head.raw == b.sym_defun.raw) return .defun;
        if (head.raw == b.sym_when.raw) return .when;
        if (head.raw == b.sym_unless.raw) return .unless;
        return null;
    }

    /// (let* ((a 1) (b 2)) body) → (let ((a 1)) (let ((b 2)) body))
    fn desugarLetStar(self: *Desugarer, args: Value) Error!Value {
        if (!args.isCons()) return Value.nil;

        const args_cons = args.toPtr(Cons);
        const bindings = args_cons.car;
        const body = args_cons.cdr;

        // Empty bindings → (progn body...)
        if (bindings.isNil()) {
            return self.makeProgn(body);
        }

        // Get first binding and rest
        const bindings_cons = bindings.toPtr(Cons);
        const first_binding = bindings_cons.car;
        const rest_bindings = bindings_cons.cdr;

        // Desugar the binding value
        var desugared_binding = first_binding;
        if (first_binding.isCons()) {
            const binding_cons = first_binding.toPtr(Cons);
            if (binding_cons.cdr.isCons()) {
                const value_cons = binding_cons.cdr.toPtr(Cons);
                const desugared_value = try self.desugar(value_cons.car);
                if (desugared_value.raw != value_cons.car.raw) {
                    desugared_binding = try self.heap.allocCons(
                        binding_cons.car,
                        try self.heap.allocCons(desugared_value, Value.nil),
                    );
                }
            }
        }

        // If more bindings, create nested let*
        if (!rest_bindings.isNil()) {
            // (let* rest body) - recursive
            const inner_let_star = try self.heap.allocCons(
                try self.heap.intern("let*"),
                try self.heap.allocCons(
                    rest_bindings,
                    body,
                ),
            );

            // Build (let ((first)) inner)
            const let_sym = try self.heap.intern("let");
            const binding_list = try self.heap.allocCons(desugared_binding, Value.nil);
            const inner = try self.desugar(inner_let_star);

            return try self.heap.allocCons(
                let_sym,
                try self.heap.allocCons(
                    binding_list,
                    try self.heap.allocCons(inner, Value.nil),
                ),
            );
        }

        // Single binding → (let ((binding)) body...)
        const let_sym = try self.heap.intern("let");
        const binding_list = try self.heap.allocCons(desugared_binding, Value.nil);
        const desugared_body = try self.desugarList(body);

        return try self.heap.allocCons(
            let_sym,
            try self.heap.allocCons(
                binding_list,
                desugared_body,
            ),
        );
    }

    /// (cond (c1 e1) (c2 e2) (t e3)) → (if c1 e1 (if c2 e2 e3))
    fn desugarCond(self: *Desugarer, clauses: Value) Error!Value {
        if (clauses.isNil()) return Value.nil;
        if (!clauses.isCons()) return Value.nil;

        const clauses_cons = clauses.toPtr(Cons);
        const first_clause = clauses_cons.car;
        const rest_clauses = clauses_cons.cdr;

        if (!first_clause.isCons()) return Value.nil;

        const clause_cons = first_clause.toPtr(Cons);
        const test_expr = clause_cons.car;
        const then_exprs = clause_cons.cdr;

        // Check for (t ...) or (else ...) - default clause
        const is_default = blk: {
            if (test_expr.raw == Value.t.raw) break :blk true;
            if (test_expr.eq(self.builtins.sym_else)) break :blk true;
            if (test_expr.eq(self.builtins.sym_t)) break :blk true;
            break :blk false;
        };

        // ANSI CL: a cond clause with no body returns the test value itself.
        if (then_exprs.isNil()) {
            const desugared_test = try self.desugar(test_expr);
            if (is_default) return desugared_test;
            const temp_sym = try self.gensym();
            const desugared_else = try self.desugarCond(rest_clauses);
            const if_expr = try self.makeIf(temp_sym, temp_sym, desugared_else);
            return self.makeLet(temp_sym, desugared_test, if_expr);
        }

        // Get the body expression - wrap in progn if multiple
        const then_body = try self.makeProgn(then_exprs);

        if (is_default) {
            // Default clause - just return the body (desugared)
            return self.desugar(then_body);
        }

        // Non-default clause: (if test then_body (cond rest...))
        const desugared_test = try self.desugar(test_expr);
        const desugared_then = try self.desugar(then_body);
        const desugared_else = try self.desugarCond(rest_clauses);

        return self.makeIf(desugared_test, desugared_then, desugared_else);
    }

    /// (and a b c) → (if a (if b c nil) nil)
    fn desugarAnd(self: *Desugarer, args: Value) Error!Value {
        // (and) → t
        if (args.isNil()) return Value.t;

        if (!args.isCons()) return args;

        const args_cons = args.toPtr(Cons);
        const first = args_cons.car;
        const rest = args_cons.cdr;

        // (and x) → x
        if (rest.isNil()) {
            return self.desugar(first);
        }

        // (and a b c...) → (if a (and b c...) nil)
        const desugared_first = try self.desugar(first);
        const desugared_rest = try self.desugarAnd(rest);

        return self.makeIf(desugared_first, desugared_rest, Value.nil);
    }

    /// (or a b) → (let ((g a)) (if g g b))
    fn desugarOr(self: *Desugarer, args: Value) Error!Value {
        // (or) → nil
        if (args.isNil()) return Value.nil;

        if (!args.isCons()) return args;

        const args_cons = args.toPtr(Cons);
        const first = args_cons.car;
        const rest = args_cons.cdr;

        // (or x) → x
        if (rest.isNil()) {
            return self.desugar(first);
        }

        // (or a b...) → (let ((g a)) (if g g (or b...)))
        const temp_sym = try self.gensym();
        const desugared_first = try self.desugar(first);
        const desugared_rest = try self.desugarOr(rest);

        // Build (if g g (or rest))
        const if_expr = try self.makeIf(temp_sym, temp_sym, desugared_rest);

        // Build (let ((g first)) if_expr)
        return self.makeLet(temp_sym, desugared_first, if_expr);
    }

    /// Preserve DEFUN so compile-time DEFUN semantics remain intact.
    /// The body is recursively desugared, but DEFUN itself is not lowered to DEFINE.
    fn desugarDefun(self: *Desugarer, args: Value) Error!Value {
        if (!args.isCons()) return Value.nil;

        const args_cons = args.toPtr(Cons);
        const name_spec = args_cons.car;
        const rest = args_cons.cdr;

        if (!rest.isCons()) return Value.nil;

        const rest_cons = rest.toPtr(Cons);
        const params = rest_cons.car;
        const body = rest_cons.cdr;

        // Desugar body expressions
        const desugared_body = try self.desugarList(body);
        const defun_sym = try self.heap.intern("defun");
        return try self.heap.allocCons(
            defun_sym,
            try self.heap.allocCons(
                name_spec,
                try self.heap.allocCons(params, desugared_body),
            ),
        );
    }

    /// (when test body...) → (if test (progn body...) nil)
    fn desugarWhen(self: *Desugarer, args: Value) Error!Value {
        if (!args.isCons()) return Value.nil;

        const args_cons = args.toPtr(Cons);
        const test_expr = args_cons.car;
        const body = args_cons.cdr;

        const desugared_test = try self.desugar(test_expr);
        const desugared_body = try self.desugarList(body);
        const then_expr = try self.makeProgn(desugared_body);

        return self.makeIf(desugared_test, then_expr, Value.nil);
    }

    /// (unless test body...) → (if test nil (progn body...))
    fn desugarUnless(self: *Desugarer, args: Value) Error!Value {
        if (!args.isCons()) return Value.nil;

        const args_cons = args.toPtr(Cons);
        const test_expr = args_cons.car;
        const body = args_cons.cdr;

        const desugared_test = try self.desugar(test_expr);
        const desugared_body = try self.desugarList(body);
        const else_expr = try self.makeProgn(desugared_body);

        return self.makeIf(desugared_test, Value.nil, else_expr);
    }

    // ========================================================================
    // Helpers for building core forms
    // ========================================================================

    /// Build (if cond then else)
    fn makeIf(self: *Desugarer, cond_expr: Value, then_branch: Value, else_branch: Value) Error!Value {
        const if_sym = try self.heap.intern("if");
        const else_cons = try self.heap.allocCons(else_branch, Value.nil);
        const then_cons = try self.heap.allocCons(then_branch, else_cons);
        const cond_cons = try self.heap.allocCons(cond_expr, then_cons);
        return self.heap.allocCons(if_sym, cond_cons);
    }

    /// Build (let ((name value)) body)
    fn makeLet(self: *Desugarer, name: Value, value: Value, body: Value) Error!Value {
        const let_sym = try self.heap.intern("let");
        const value_list = try self.heap.allocCons(value, Value.nil);
        const binding = try self.heap.allocCons(name, value_list);
        const bindings = try self.heap.allocCons(binding, Value.nil);
        const body_list = try self.heap.allocCons(body, Value.nil);
        const let_tail = try self.heap.allocCons(bindings, body_list);
        return self.heap.allocCons(let_sym, let_tail);
    }

    /// Build (progn exprs...) or just return single expr
    fn makeProgn(self: *Desugarer, exprs: Value) Error!Value {
        // Empty → nil
        if (exprs.isNil()) return Value.nil;

        // Single expr → just return it
        if (exprs.isCons()) {
            const cons = exprs.toPtr(Cons);
            if (cons.cdr.isNil()) {
                return cons.car;
            }
        }

        // Multiple exprs → (progn ...)
        const progn_sym = try self.heap.intern("progn");
        return self.heap.allocCons(progn_sym, exprs);
    }
};

/// Pass wrapper for pipeline integration
pub fn desugar(allocator: std.mem.Allocator, heap: *Heap, builtins: *const builtins_mod.BuiltinSymbols, expr: Value) !Value {
    var desugarer = Desugarer.init(allocator, heap, builtins);
    return desugarer.desugar(expr);
}

// ============================================================================
// Tests
// ============================================================================

test "desugar - atom passthrough" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const Vm = @import("../../interp/vm.zig").Vm;
    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var desugarer = Desugarer.init(testing.allocator, &heap, &vm.builtins);

    // Nil passes through
    const result_nil = try desugarer.desugar(Value.nil);
    try testing.expect(result_nil.isNil());

    // t passes through
    const result_t = try desugarer.desugar(Value.t);
    try testing.expect(result_t.isMagicSymbol());

    // Fixnum passes through
    const num = Value.makeFixnum(42);
    const result_num = try desugarer.desugar(num);
    try testing.expectEqual(num.raw, result_num.raw);
}

test "desugar - and" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const Vm = @import("../../interp/vm.zig").Vm;
    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var desugarer = Desugarer.init(testing.allocator, &heap, &vm.builtins);

    // (and) → t
    const and_sym = try heap.intern("and");
    const empty_and = try heap.allocCons(and_sym, Value.nil);
    const result = try desugarer.desugar(empty_and);
    try testing.expect(result.isMagicSymbol());
    try testing.expect(result.eq(try heap.intern("t")));
}

test "desugar - or single" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const Vm = @import("../../interp/vm.zig").Vm;
    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var desugarer = Desugarer.init(testing.allocator, &heap, &vm.builtins);

    // (or x) → x
    const or_sym = try heap.intern("or");
    const x = Value.makeFixnum(42);
    const single_or = try heap.allocCons(or_sym, try heap.allocCons(x, Value.nil));
    const result = try desugarer.desugar(single_or);
    try testing.expectEqual(x.raw, result.raw);
}

test "desugar - or gensym" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const Vm = @import("../../interp/vm.zig").Vm;
    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var desugarer = Desugarer.init(testing.allocator, &heap, &vm.builtins);

    const or_sym = try heap.intern("or");
    const a = Value.makeFixnum(1);
    const b = Value.makeFixnum(2);
    const or_expr = try heap.allocCons(or_sym, try heap.allocCons(a, try heap.allocCons(b, Value.nil)));
    const result = try desugarer.desugar(or_expr);

    try testing.expect(result.isCons());
    const result_cons = result.toPtr(Cons);
    const let_sym = try heap.intern("let");
    try testing.expectEqual(let_sym.raw, result_cons.car.raw);

    const let_tail = result_cons.cdr.toPtr(Cons);
    const bindings = let_tail.car.toPtr(Cons);
    const binding = bindings.car.toPtr(Cons);
    const temp_sym = binding.car;
    try testing.expect(temp_sym.isSymbol());
    try testing.expectEqualStrings("#G0", temp_sym.toPtr(Symbol).getName());

    const body_list = let_tail.cdr.toPtr(Cons);
    const if_expr = body_list.car;
    const if_cons = if_expr.toPtr(Cons);
    const if_sym = try heap.intern("if");
    try testing.expectEqual(if_sym.raw, if_cons.car.raw);

    const cond_cons = if_cons.cdr.toPtr(Cons);
    const cond = cond_cons.car;
    const then_cons = cond_cons.cdr.toPtr(Cons);
    const then_val = then_cons.car;
    const else_cons = then_cons.cdr.toPtr(Cons);
    const else_val = else_cons.car;

    try testing.expectEqual(temp_sym.raw, cond.raw);
    try testing.expectEqual(temp_sym.raw, then_val.raw);
    try testing.expectEqual(b.raw, else_val.raw);
}

test "desugar - defun" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    const Vm = @import("../../interp/vm.zig").Vm;
    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();
    var desugarer = Desugarer.init(testing.allocator, &heap, &vm.builtins);

    // DEFUN should be preserved so compiler-level DEFUN semantics remain active.
    const defun_sym = try heap.intern("defun");
    const name = try heap.intern("square");
    const x_sym = try heap.intern("x");
    const mul_sym = try heap.intern("*");

    const params = try heap.allocCons(x_sym, Value.nil);
    const body = try heap.allocCons(mul_sym, try heap.allocCons(x_sym, try heap.allocCons(x_sym, Value.nil)));

    const defun_expr = try heap.allocCons(
        defun_sym,
        try heap.allocCons(name, try heap.allocCons(params, try heap.allocCons(body, Value.nil))),
    );

    const result = try desugarer.desugar(defun_expr);

    // Should still start with DEFUN.
    try testing.expect(result.isCons());
    const result_cons = result.toPtr(Cons);
    try testing.expectEqual(defun_sym.raw, result_cons.car.raw);
}
