//! Lift Pass
//!
//! Converts S-expressions (Value) to IR nodes.
//! Variables are created with UNRESOLVED depth/index.
//! Lambdas are created with empty captures.
//!
//! Input: Value (S-expression)
//! Output: Ir (with unresolved variables)
//!
//! Note: This pass does NOT perform name resolution or capture analysis.
//! Those are handled by p04_resolve and p05_capture respectively.

const std = @import("std");
const ir_mod = @import("../ir.zig");
const Ir = ir_mod.Ir;
const IrBuilder = ir_mod.IrBuilder;
const UNRESOLVED = ir_mod.UNRESOLVED;
const runtime = @import("../../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const Heap = runtime.Heap;

pub const Error = error{
    InvalidSyntax,
    InvalidLambda,
    InvalidLet,
    InvalidIf,
    OutOfMemory,
};

/// Pre-interned symbols for identity comparison
pub const LiftSymbols = struct {
    // Control flow
    @"if": Value,
    // Binding forms
    let: Value,
    lambda: Value,
    @"fn": Value,
    define: Value,
    setq: Value,
    // Sequencing
    progn: Value,
    begin: Value,
    @"while": Value,
    // Quoting
    quote: Value,
    // Primitives - Arithmetic
    @"+": Value,
    @"-": Value,
    @"*": Value,
    @"/": Value,
    mod: Value,
    // Primitives - Comparison
    eq: Value,
    @"<": Value,
    @">": Value,
    @"<=": Value,
    @">=": Value,
    @"=": Value,
    // Primitives - List operations
    cons: Value,
    car: Value,
    cdr: Value,
    list: Value,
    // Primitives - Type predicates
    consp: Value,
    symbolp: Value,
    numberp: Value,
    stringp: Value,
    vectorp: Value,
    null: Value,
    not: Value,

    pub fn init(heap: *Heap) !LiftSymbols {
        return .{
            .@"if" = try heap.intern("if"),
            .let = try heap.intern("let"),
            .lambda = try heap.intern("lambda"),
            .@"fn" = try heap.intern("fn"),
            .define = try heap.intern("define"),
            .setq = try heap.intern("setq"),
            .progn = try heap.intern("progn"),
            .begin = try heap.intern("begin"),
            .@"while" = try heap.intern("while"),
            .quote = try heap.intern("quote"),
            .@"+" = try heap.intern("+"),
            .@"-" = try heap.intern("-"),
            .@"*" = try heap.intern("*"),
            .@"/" = try heap.intern("/"),
            .mod = try heap.intern("mod"),
            .eq = try heap.intern("eq"),
            .@"<" = try heap.intern("<"),
            .@">" = try heap.intern(">"),
            .@"<=" = try heap.intern("<="),
            .@">=" = try heap.intern(">="),
            .@"=" = try heap.intern("="),
            .cons = try heap.intern("cons"),
            .car = try heap.intern("car"),
            .cdr = try heap.intern("cdr"),
            .list = try heap.intern("list"),
            .consp = try heap.intern("consp"),
            .symbolp = try heap.intern("symbolp"),
            .numberp = try heap.intern("numberp"),
            .stringp = try heap.intern("stringp"),
            .vectorp = try heap.intern("vectorp"),
            .null = try heap.intern("null"),
            .not = try heap.intern("not"),
        };
    }
};

/// Lifter - converts S-expressions to IR
pub const Lifter = struct {
    allocator: std.mem.Allocator,
    builder: IrBuilder,
    symbols: LiftSymbols,

    pub fn init(allocator: std.mem.Allocator, heap: *Heap) !Lifter {
        return .{
            .allocator = allocator,
            .builder = IrBuilder.init(allocator),
            .symbols = try LiftSymbols.init(heap),
        };
    }

    /// Lift an S-expression to IR
    pub fn lift(self: *Lifter, expr: Value) Error!*Ir {
        // Nil
        if (expr.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        // Fixnum
        if (expr.isFixnum()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Float
        if (expr.isFloat()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Character
        if (expr.isCharacter()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // T
        if (expr.eq(Value.t)) {
            return self.builder.lit(Value.t) catch return error.OutOfMemory;
        }

        // String
        if (expr.isString()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Keyword
        if (expr.isKeyword()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Symbol - create unresolved variable reference
        if (expr.isSymbol()) {
            const name = expr.toPtr(Symbol).getName();
            return self.builder.variable(name, UNRESOLVED, UNRESOLVED) catch
                return error.OutOfMemory;
        }

        // List - dispatch on head
        if (expr.isCons()) {
            return self.liftList(expr);
        }

        return error.InvalidSyntax;
    }

    /// Lift a list expression
    fn liftList(self: *Lifter, expr: Value) Error!*Ir {
        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check for special forms
        if (head.isSymbol()) {
            const s = &self.symbols;

            if (head.raw == s.@"if".raw) return self.liftIf(tail);
            if (head.raw == s.let.raw) return self.liftLet(tail);
            if (head.raw == s.lambda.raw or head.raw == s.@"fn".raw) return self.liftLambda(tail);
            if (head.raw == s.define.raw) return self.liftDefine(tail);
            if (head.raw == s.setq.raw) return self.liftSet(tail);
            if (head.raw == s.progn.raw or head.raw == s.begin.raw) return self.liftProgn(tail);
            if (head.raw == s.@"while".raw) return self.liftWhile(tail);
            if (head.raw == s.quote.raw) return self.liftQuote(tail);

            // Primitives
            if (head.raw == s.@"+".raw) return self.liftBinary(tail, .add);
            if (head.raw == s.@"-".raw) return self.liftBinary(tail, .sub);
            if (head.raw == s.@"*".raw) return self.liftBinary(tail, .mul);
            if (head.raw == s.@"/".raw) return self.liftBinary(tail, .div);
            if (head.raw == s.mod.raw) return self.liftBinary(tail, .mod);
            if (head.raw == s.eq.raw) return self.liftBinary(tail, .eq);
            if (head.raw == s.@"<".raw) return self.liftBinary(tail, .lt);
            if (head.raw == s.@">".raw) return self.liftBinary(tail, .gt);
            if (head.raw == s.@"<=".raw) return self.liftBinary(tail, .le);
            if (head.raw == s.@">=".raw) return self.liftBinary(tail, .ge);
            if (head.raw == s.@"=".raw) return self.liftBinary(tail, .num_eq);
            if (head.raw == s.cons.raw) return self.liftBinary(tail, .cons);
            if (head.raw == s.car.raw) return self.liftUnary(tail, .car);
            if (head.raw == s.cdr.raw) return self.liftUnary(tail, .cdr);
            if (head.raw == s.list.raw) return self.liftListForm(tail);
            if (head.raw == s.consp.raw) return self.liftUnary(tail, .consp);
            if (head.raw == s.symbolp.raw) return self.liftUnary(tail, .symbolp);
            if (head.raw == s.numberp.raw) return self.liftUnary(tail, .numberp);
            if (head.raw == s.stringp.raw) return self.liftUnary(tail, .stringp);
            if (head.raw == s.vectorp.raw) return self.liftUnary(tail, .vectorp);
            if (head.raw == s.null.raw or head.raw == s.not.raw) return self.liftUnary(tail, .not);
        }

        // Function call
        return self.liftCall(expr);
    }

    /// Lift (if cond then else)
    fn liftIf(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidIf;

        const cons1 = args.toPtr(Cons);
        const cond_expr = cons1.car;

        if (!cons1.cdr.isCons()) return error.InvalidIf;
        const cons2 = cons1.cdr.toPtr(Cons);
        const then_expr = cons2.car;

        const else_expr = if (cons2.cdr.isCons())
            cons2.cdr.toPtr(Cons).car
        else
            Value.nil;

        const cond_ir = try self.lift(cond_expr);
        const then_ir = try self.lift(then_expr);
        const else_ir = try self.lift(else_expr);

        return self.builder.ifExpr(cond_ir, then_ir, else_ir) catch
            return error.OutOfMemory;
    }

    /// Lift (let ((x 1) (y 2)) body...)
    fn liftLet(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidLet;

        const cons1 = args.toPtr(Cons);
        const bindings_expr = cons1.car;
        const body_exprs = cons1.cdr;

        // Parse bindings
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        var b = bindings_expr;
        var idx: u16 = 0;
        while (b.isCons()) {
            const b_cons = b.toPtr(Cons);
            const binding = b_cons.car;

            if (!binding.isCons()) return error.InvalidLet;
            const bind_cons = binding.toPtr(Cons);

            if (!bind_cons.car.isSymbol()) return error.InvalidLet;
            const name = bind_cons.car.toPtr(Symbol).getName();

            if (!bind_cons.cdr.isCons()) return error.InvalidLet;
            const val_cons = bind_cons.cdr.toPtr(Cons);
            const val_ir = try self.lift(val_cons.car);

            bindings.append(self.allocator, .{ .name = name, .value = val_ir, .index = idx }) catch
                return error.OutOfMemory;
            idx += 1;

            b = b_cons.cdr;
        }

        // Parse body
        const body_ir = try self.liftBody(body_exprs);

        const bindings_slice = self.allocator.dupe(Ir.Binding, bindings.items) catch
            return error.OutOfMemory;

        return self.builder.letExpr(bindings_slice, body_ir) catch
            return error.OutOfMemory;
    }

    /// Lift (lambda (params...) body...)
    fn liftLambda(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidLambda;

        const cons1 = args.toPtr(Cons);
        const params_expr = cons1.car;
        const body_exprs = cons1.cdr;

        // Parse parameters
        var params = std.ArrayList([]const u8){};
        defer params.deinit(self.allocator);

        var p = params_expr;
        while (p.isCons()) {
            const p_cons = p.toPtr(Cons);
            if (!p_cons.car.isSymbol()) return error.InvalidLambda;
            params.append(self.allocator, p_cons.car.toPtr(Symbol).getName()) catch
                return error.OutOfMemory;
            p = p_cons.cdr;
        }

        // Parse body
        const body_ir = try self.liftBody(body_exprs);

        const params_slice = self.allocator.dupe([]const u8, params.items) catch
            return error.OutOfMemory;

        // Empty captures - will be filled by p05_capture
        const empty_captures: []const Ir.Capture = &.{};
        const empty_opt: []const Ir.OptionalParam = &.{};
        const empty_key: []const Ir.KeyParam = &.{};

        return self.builder.lambda(params_slice, empty_opt, empty_key, null, empty_captures, body_ir) catch
            return error.OutOfMemory;
    }

    /// Lift (define name value)
    fn liftDefine(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;
        const name = cons1.car.toPtr(Symbol).getName();

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.lift(cons2.car);

        // Use UNRESOLVED index - will be filled by resolve pass or global registry
        return self.builder.define(name, UNRESOLVED, val_ir) catch
            return error.OutOfMemory;
    }

    /// Lift (setq name value)
    fn liftSet(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSyntax;
        const name = cons1.car.toPtr(Symbol).getName();

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.lift(cons2.car);

        return self.builder.set(name, UNRESOLVED, UNRESOLVED, val_ir) catch
            return error.OutOfMemory;
    }

    /// Lift (progn expr...)
    fn liftProgn(self: *Lifter, args: Value) Error!*Ir {
        return self.liftBody(args);
    }

    /// Lift body expressions into progn
    fn liftBody(self: *Lifter, exprs: Value) Error!*Ir {
        if (exprs.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        if (!exprs.isCons()) {
            return self.lift(exprs);
        }

        var body_list = std.ArrayList(*const Ir){};
        defer body_list.deinit(self.allocator);

        var e = exprs;
        while (e.isCons()) {
            const e_cons = e.toPtr(Cons);
            const ir = try self.lift(e_cons.car);
            body_list.append(self.allocator, ir) catch return error.OutOfMemory;
            e = e_cons.cdr;
        }

        if (body_list.items.len == 1) {
            return @constCast(body_list.items[0]);
        }

        const body_slice = self.allocator.dupe(*const Ir, body_list.items) catch
            return error.OutOfMemory;

        return self.builder.progn(body_slice) catch return error.OutOfMemory;
    }

    /// Lift (while cond body...)
    fn liftWhile(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const cond_ir = try self.lift(cons1.car);
        const body_ir = try self.liftBody(cons1.cdr);

        return self.builder.loop(cond_ir, body_ir) catch
            return error.OutOfMemory;
    }

    /// Lift (quote expr)
    fn liftQuote(self: *Lifter, args: Value) Error!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const quoted = cons1.car;

        if (quoted.isSymbol()) {
            const name = quoted.toPtr(Symbol).getName();
            return self.builder.quoteSym(name) catch return error.OutOfMemory;
        }

        // For complex quoted data, create a lit node
        return self.builder.lit(quoted) catch return error.OutOfMemory;
    }

    /// Lift binary operation
    fn liftBinary(self: *Lifter, args: Value, comptime op: anytype) Error!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const left_ir = try self.lift(cons1.car);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const right_ir = try self.lift(cons2.car);

        const result = self.allocator.create(Ir) catch return error.OutOfMemory;
        result.* = @unionInit(Ir, @tagName(op), .{ .left = left_ir, .right = right_ir });
        return result;
    }

    /// Lift unary operation
    fn liftUnary(self: *Lifter, args: Value, comptime op: anytype) Error!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;

        const cons1 = args.toPtr(Cons);
        const operand_ir = try self.lift(cons1.car);

        const result = self.allocator.create(Ir) catch return error.OutOfMemory;
        result.* = @unionInit(Ir, @tagName(op), .{ .operand = operand_ir });
        return result;
    }

    /// Lift (list a b c...) -> (cons a (cons b (cons c nil)))
    fn liftListForm(self: *Lifter, args: Value) Error!*Ir {
        if (args.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const head_ir = try self.lift(cons.car);
        const tail_ir = try self.liftListForm(cons.cdr);

        const result = self.allocator.create(Ir) catch return error.OutOfMemory;
        result.* = .{ .cons = .{ .left = head_ir, .right = tail_ir } };
        return result;
    }

    /// Lift function call
    fn liftCall(self: *Lifter, expr: Value) Error!*Ir {
        const cons = expr.toPtr(Cons);

        // Lift function expression
        const func_ir = try self.lift(cons.car);

        // Lift arguments
        var arg_list = std.ArrayList(*const Ir){};
        defer arg_list.deinit(self.allocator);

        var a = cons.cdr;
        while (a.isCons()) {
            const a_cons = a.toPtr(Cons);
            const arg_ir = try self.lift(a_cons.car);
            arg_list.append(self.allocator, arg_ir) catch return error.OutOfMemory;
            a = a_cons.cdr;
        }

        const args_slice = self.allocator.dupe(*const Ir, arg_list.items) catch
            return error.OutOfMemory;

        return self.builder.call(func_ir, args_slice) catch return error.OutOfMemory;
    }
};

/// Pass wrapper for pipeline integration
pub fn lift(allocator: std.mem.Allocator, heap: *Heap, expr: Value) !*Ir {
    var lifter = try Lifter.init(allocator, heap);
    return lifter.lift(expr);
}

// ============================================================================
// Tests
// ============================================================================

test "lift - literal" {
    const testing = std.testing;

    // Use arena for IR nodes (they're not individually freed)
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var heap = try Heap.init(alloc, .{});
    defer heap.deinit();

    const result = try lift(alloc, &heap, Value.makeFixnum(42));
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.*));
    try testing.expectEqual(@as(i64, 42), result.lit.toFixnum());
}

test "lift - variable" {
    const testing = std.testing;

    // Use arena for IR nodes
    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var heap = try Heap.init(alloc, .{});
    defer heap.deinit();

    const x_sym = try heap.intern("x");
    const result = try lift(alloc, &heap, x_sym);

    try testing.expectEqual(Ir.@"var", std.meta.activeTag(result.*));
    try testing.expectEqualStrings("X", result.@"var".name);
    try testing.expectEqual(UNRESOLVED, result.@"var".depth);
    try testing.expectEqual(UNRESOLVED, result.@"var".index);
}
