//! S-expression to IR compiler
//!
//! Compiles parsed Habu expressions (cons trees) to IR nodes.
//! Handles:
//! - Special forms: if, lambda, let, set!, quote, progn, while
//! - Primitive operations: +, -, *, /, cons, car, cdr, etc.
//! - Function calls
//! - Variable references with lexical scoping

const std = @import("std");
const ir = @import("ir.zig");
const Ir = ir.Ir;
const IrBuilder = ir.IrBuilder;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const Heap = runtime.Heap;

pub const CompileError = error{
    InvalidSyntax,
    UnboundVariable,
    InvalidLambda,
    InvalidLet,
    InvalidIf,
    InvalidSet,
    OutOfMemory,
};

/// Lexical environment for variable resolution
pub const Env = struct {
    /// Variable bindings at this level
    bindings: std.StringHashMap(u16),
    /// Parent environment (for closures)
    parent: ?*const Env,
    /// Depth from root (0 = top level)
    depth: u16,
    /// Allocator for bindings
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, parent: ?*const Env) Env {
        return .{
            .bindings = std.StringHashMap(u16).init(allocator),
            .parent = parent,
            .depth = if (parent) |p| p.depth + 1 else 0,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *Env) void {
        self.bindings.deinit();
    }

    /// Add a binding, returns the index
    pub fn bind(self: *Env, name: []const u8) !u16 {
        const index: u16 = @intCast(self.bindings.count());
        try self.bindings.put(name, index);
        return index;
    }

    /// Look up a variable, returns (depth, index) or null
    pub fn lookup(self: *const Env, name: []const u8) ?struct { depth: u16, index: u16 } {
        if (self.bindings.get(name)) |index| {
            return .{ .depth = 0, .index = index };
        }
        if (self.parent) |parent| {
            if (parent.lookup(name)) |result| {
                return .{ .depth = result.depth + 1, .index = result.index };
            }
        }
        return null;
    }
};

/// Compiler state
pub const Compiler = struct {
    builder: IrBuilder,
    allocator: std.mem.Allocator,
    /// Track free variables during lambda compilation
    captures: std.ArrayList(Ir.Capture),

    pub fn init(allocator: std.mem.Allocator) Compiler {
        return .{
            .builder = IrBuilder.init(allocator),
            .allocator = allocator,
            .captures = std.ArrayList(Ir.Capture){},
        };
    }

    pub fn deinit(self: *Compiler) void {
        self.captures.deinit(self.allocator);
    }

    /// Compile a single expression
    pub fn compile(self: *Compiler, expr: Value, env: *const Env) CompileError!*Ir {
        // Nil
        if (expr.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        // Fixnum
        if (expr.isFixnum()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // String
        if (expr.isString()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        // Symbol (variable reference)
        if (expr.isSymbol()) {
            const sym = expr.toPtr(Symbol);
            const name = sym.getName();

            if (env.lookup(name)) |binding| {
                return self.builder.variable(name, binding.depth, binding.index) catch
                    return error.OutOfMemory;
            }
            // Unbound variable - could be a global/primitive
            // For now, treat as error
            return error.UnboundVariable;
        }

        // List (special form or function call)
        if (expr.isCons()) {
            return self.compileList(expr, env);
        }

        // Keyword - just return as literal
        if (expr.isKeyword()) {
            return self.builder.lit(expr) catch return error.OutOfMemory;
        }

        return error.InvalidSyntax;
    }

    fn compileList(self: *Compiler, expr: Value, env: *const Env) CompileError!*Ir {
        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check for special forms
        if (head.isSymbol()) {
            const sym = head.toPtr(Symbol);
            const name = sym.getName();

            if (std.mem.eql(u8, name, "if")) {
                return self.compileIf(tail, env);
            }
            if (std.mem.eql(u8, name, "lambda")) {
                return self.compileLambda(tail, env);
            }
            if (std.mem.eql(u8, name, "let")) {
                return self.compileLet(tail, env);
            }
            if (std.mem.eql(u8, name, "set!")) {
                return self.compileSet(tail, env);
            }
            if (std.mem.eql(u8, name, "quote")) {
                return self.compileQuote(tail);
            }
            if (std.mem.eql(u8, name, "progn") or std.mem.eql(u8, name, "begin")) {
                return self.compileProgn(tail, env);
            }
            if (std.mem.eql(u8, name, "while")) {
                return self.compileWhile(tail, env);
            }

            // Check for primitives
            if (self.compilePrimitive(name, tail, env)) |prim| {
                return prim;
            } else |_| {
                // Fall through to function call
            }
        }

        // Function call
        return self.compileCall(head, tail, env);
    }

    fn compileIf(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (if test then else?)
        if (!args.isCons()) return error.InvalidIf;

        const cons1 = args.toPtr(Cons);
        const test_expr = cons1.car;
        const rest1 = cons1.cdr;

        if (!rest1.isCons()) return error.InvalidIf;
        const cons2 = rest1.toPtr(Cons);
        const then_expr = cons2.car;
        const rest2 = cons2.cdr;

        // else is optional, defaults to nil
        const else_expr = if (rest2.isCons())
            rest2.toPtr(Cons).car
        else
            Value.nil;

        const test_ir = try self.compile(test_expr, env);
        const then_ir = try self.compile(then_expr, env);
        const else_ir = try self.compile(else_expr, env);

        return self.builder.ifExpr(test_ir, then_ir, else_ir) catch return error.OutOfMemory;
    }

    fn compileLambda(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (lambda (params...) body)
        if (!args.isCons()) return error.InvalidLambda;

        const cons = args.toPtr(Cons);
        const params_expr = cons.car;
        const body_exprs = cons.cdr;

        // Parse parameters
        var params = std.ArrayList([]const u8){};
        defer params.deinit(self.allocator);

        var param_list = params_expr;
        while (param_list.isCons()) {
            const param_cons = param_list.toPtr(Cons);
            if (!param_cons.car.isSymbol()) return error.InvalidLambda;
            const param_sym = param_cons.car.toPtr(Symbol);
            params.append(self.allocator, param_sym.getName()) catch return error.OutOfMemory;
            param_list = param_cons.cdr;
        }

        // Create new environment with parameters
        var lambda_env = Env.init(self.allocator, env);
        defer lambda_env.deinit();

        for (params.items) |param| {
            _ = lambda_env.bind(param) catch return error.OutOfMemory;
        }

        // Compile body (implicit progn)
        const body_ir = try self.compileBody(body_exprs, &lambda_env);

        // TODO: Capture analysis for free variables
        const captures = &[_]Ir.Capture{};

        return self.builder.lambda(params.items, captures, body_ir) catch
            return error.OutOfMemory;
    }

    fn compileLet(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (let ((x 1) (y 2)) body)
        if (!args.isCons()) return error.InvalidLet;

        const cons = args.toPtr(Cons);
        const bindings_expr = cons.car;
        const body_exprs = cons.cdr;

        // Parse bindings
        var bindings = std.ArrayList(Ir.Binding){};
        defer bindings.deinit(self.allocator);

        var binding_list = bindings_expr;
        while (binding_list.isCons()) {
            const binding_cons = binding_list.toPtr(Cons);
            const binding = binding_cons.car;

            if (!binding.isCons()) return error.InvalidLet;
            const b = binding.toPtr(Cons);

            if (!b.car.isSymbol()) return error.InvalidLet;
            const name_sym = b.car.toPtr(Symbol);
            const name = name_sym.getName();

            // Get value expression
            if (!b.cdr.isCons()) return error.InvalidLet;
            const val_cons = b.cdr.toPtr(Cons);
            const val_ir = try self.compile(val_cons.car, env);

            bindings.append(self.allocator, .{ .name = name, .value = val_ir }) catch
                return error.OutOfMemory;

            binding_list = binding_cons.cdr;
        }

        // Create new environment with bindings
        var let_env = Env.init(self.allocator, env);
        defer let_env.deinit();

        for (bindings.items) |b| {
            _ = let_env.bind(b.name) catch return error.OutOfMemory;
        }

        // Compile body
        const body_ir = try self.compileBody(body_exprs, &let_env);

        return self.builder.letExpr(bindings.items, body_ir) catch return error.OutOfMemory;
    }

    fn compileSet(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (set! var value)
        if (!args.isCons()) return error.InvalidSet;

        const cons1 = args.toPtr(Cons);
        if (!cons1.car.isSymbol()) return error.InvalidSet;
        const var_sym = cons1.car.toPtr(Symbol);
        const name = var_sym.getName();

        if (!cons1.cdr.isCons()) return error.InvalidSet;
        const cons2 = cons1.cdr.toPtr(Cons);
        const val_ir = try self.compile(cons2.car, env);

        if (env.lookup(name)) |binding| {
            return self.builder.set(name, binding.depth, binding.index, val_ir) catch
                return error.OutOfMemory;
        }

        return error.UnboundVariable;
    }

    fn compileQuote(self: *Compiler, args: Value) CompileError!*Ir {
        // (quote expr)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const quoted = cons.car;

        // For symbols, use quote_sym
        if (quoted.isSymbol()) {
            const sym = quoted.toPtr(Symbol);
            return self.builder.quoteSym(sym.getName()) catch return error.OutOfMemory;
        }

        // For other values, return as literal
        return self.builder.lit(quoted) catch return error.OutOfMemory;
    }

    fn compileProgn(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        return self.compileBody(args, env);
    }

    fn compileWhile(self: *Compiler, args: Value, env: *const Env) CompileError!*Ir {
        // (while test body...)
        if (!args.isCons()) return error.InvalidSyntax;

        const cons = args.toPtr(Cons);
        const test_expr = cons.car;
        const body_exprs = cons.cdr;

        const test_ir = try self.compile(test_expr, env);
        const body_ir = try self.compileBody(body_exprs, env);

        return self.builder.loop(test_ir, body_ir) catch return error.OutOfMemory;
    }

    fn compileBody(self: *Compiler, exprs: Value, env: *const Env) CompileError!*Ir {
        if (exprs.isNil()) {
            return self.builder.lit(Value.nil) catch return error.OutOfMemory;
        }

        var expr_list = std.ArrayList(*Ir){};
        defer expr_list.deinit(self.allocator);

        var list = exprs;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const expr_ir = try self.compile(cons.car, env);
            expr_list.append(self.allocator, expr_ir) catch return error.OutOfMemory;
            list = cons.cdr;
        }

        if (expr_list.items.len == 1) {
            return expr_list.items[0];
        }

        // Convert to const slice for progn
        const items = self.allocator.dupe(*const Ir, expr_list.items) catch
            return error.OutOfMemory;
        return self.builder.progn(items) catch return error.OutOfMemory;
    }

    fn compilePrimitive(self: *Compiler, name: []const u8, args: Value, env: *const Env) CompileError!*Ir {
        // Binary arithmetic
        if (std.mem.eql(u8, name, "+")) {
            return self.compileBinaryPrim(args, env, .add);
        }
        if (std.mem.eql(u8, name, "-")) {
            return self.compileBinaryPrim(args, env, .sub);
        }
        if (std.mem.eql(u8, name, "*")) {
            return self.compileBinaryPrim(args, env, .mul);
        }
        if (std.mem.eql(u8, name, "/")) {
            return self.compileBinaryPrim(args, env, .div);
        }
        if (std.mem.eql(u8, name, "mod") or std.mem.eql(u8, name, "%")) {
            return self.compileBinaryPrim(args, env, .mod);
        }

        // Comparison
        if (std.mem.eql(u8, name, "eq")) {
            return self.compileBinaryPrim(args, env, .eq);
        }
        if (std.mem.eql(u8, name, "<")) {
            return self.compileBinaryPrim(args, env, .lt);
        }
        if (std.mem.eql(u8, name, ">")) {
            return self.compileBinaryPrim(args, env, .gt);
        }
        if (std.mem.eql(u8, name, "<=")) {
            return self.compileBinaryPrim(args, env, .le);
        }
        if (std.mem.eql(u8, name, ">=")) {
            return self.compileBinaryPrim(args, env, .ge);
        }
        if (std.mem.eql(u8, name, "=")) {
            return self.compileBinaryPrim(args, env, .num_eq);
        }

        // List operations
        if (std.mem.eql(u8, name, "cons")) {
            return self.compileBinaryPrim(args, env, .cons);
        }
        if (std.mem.eql(u8, name, "car")) {
            return self.compileUnaryPrim(args, env, .car);
        }
        if (std.mem.eql(u8, name, "cdr")) {
            return self.compileUnaryPrim(args, env, .cdr);
        }

        // Type predicates
        if (std.mem.eql(u8, name, "consp") or std.mem.eql(u8, name, "cons?")) {
            return self.compileUnaryPrim(args, env, .consp);
        }
        if (std.mem.eql(u8, name, "symbolp") or std.mem.eql(u8, name, "symbol?")) {
            return self.compileUnaryPrim(args, env, .symbolp);
        }
        if (std.mem.eql(u8, name, "numberp") or std.mem.eql(u8, name, "number?")) {
            return self.compileUnaryPrim(args, env, .numberp);
        }
        if (std.mem.eql(u8, name, "stringp") or std.mem.eql(u8, name, "string?")) {
            return self.compileUnaryPrim(args, env, .stringp);
        }
        if (std.mem.eql(u8, name, "vectorp") or std.mem.eql(u8, name, "vector?")) {
            return self.compileUnaryPrim(args, env, .vectorp);
        }
        if (std.mem.eql(u8, name, "null") or std.mem.eql(u8, name, "null?")) {
            return self.compileUnaryPrim(args, env, .nilp);
        }
        if (std.mem.eql(u8, name, "not")) {
            return self.compileUnaryPrim(args, env, .not);
        }

        // Vector operations
        if (std.mem.eql(u8, name, "vector-ref")) {
            return self.compileBinaryPrim(args, env, .vec_ref);
        }
        if (std.mem.eql(u8, name, "vector-length")) {
            return self.compileUnaryPrim(args, env, .vec_len);
        }

        // String operations
        if (std.mem.eql(u8, name, "string-ref")) {
            return self.compileBinaryPrim(args, env, .str_ref);
        }
        if (std.mem.eql(u8, name, "string-length")) {
            return self.compileUnaryPrim(args, env, .str_len);
        }

        // I/O
        if (std.mem.eql(u8, name, "print")) {
            return self.compileUnaryPrim(args, env, .print);
        }

        return error.InvalidSyntax; // Not a known primitive
    }

    const PrimTag = enum { add, sub, mul, div, mod, eq, lt, gt, le, ge, num_eq, cons, car, cdr, consp, symbolp, numberp, stringp, vectorp, nilp, not, vec_ref, vec_len, str_ref, str_len, print };

    fn compileBinaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) CompileError!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons1 = args.toPtr(Cons);
        const left = try self.compile(cons1.car, env);

        if (!cons1.cdr.isCons()) return error.InvalidSyntax;
        const cons2 = cons1.cdr.toPtr(Cons);
        const right = try self.compile(cons2.car, env);

        return switch (prim) {
            .add => self.builder.add(left, right),
            .sub => self.builder.sub(left, right),
            .mul => self.builder.mul(left, right),
            .div => self.builder.div(left, right),
            .mod => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .mod = .{ .left = left, .right = right } };
                break :blk node;
            },
            .eq => self.builder.eq(left, right),
            .lt => self.builder.lt(left, right),
            .gt => self.builder.gt(left, right),
            .le => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .le = .{ .left = left, .right = right } };
                break :blk node;
            },
            .ge => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .ge = .{ .left = left, .right = right } };
                break :blk node;
            },
            .num_eq => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .num_eq = .{ .left = left, .right = right } };
                break :blk node;
            },
            .cons => self.builder.cons(left, right),
            .vec_ref => self.builder.vecRef(left, right),
            .str_ref => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .str_ref = .{ .left = left, .right = right } };
                break :blk node;
            },
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
    }

    fn compileUnaryPrim(self: *Compiler, args: Value, env: *const Env, prim: PrimTag) CompileError!*Ir {
        if (!args.isCons()) return error.InvalidSyntax;
        const cons = args.toPtr(Cons);
        const operand = try self.compile(cons.car, env);

        return switch (prim) {
            .car => self.builder.car(operand),
            .cdr => self.builder.cdr(operand),
            .consp => self.builder.consp(operand),
            .symbolp => self.builder.symbolp(operand),
            .numberp => self.builder.numberp(operand),
            .nilp => self.builder.nilp(operand),
            .not => self.builder.not(operand),
            .vec_len => self.builder.vecLen(operand),
            .str_len => self.builder.strLen(operand),
            .print => self.builder.print(operand),
            .stringp => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .stringp = .{ .operand = operand } };
                break :blk node;
            },
            .vectorp => blk: {
                const node = self.allocator.create(Ir) catch return error.OutOfMemory;
                node.* = .{ .vectorp = .{ .operand = operand } };
                break :blk node;
            },
            else => error.InvalidSyntax,
        } catch return error.OutOfMemory;
    }

    fn compileCall(self: *Compiler, func_expr: Value, args_expr: Value, env: *const Env) CompileError!*Ir {
        const func_ir = try self.compile(func_expr, env);

        var args = std.ArrayList(*Ir){};
        defer args.deinit(self.allocator);

        var list = args_expr;
        while (list.isCons()) {
            const cons = list.toPtr(Cons);
            const arg_ir = try self.compile(cons.car, env);
            args.append(self.allocator, arg_ir) catch return error.OutOfMemory;
            list = cons.cdr;
        }

        // Convert to const slice
        const items = self.allocator.dupe(*const Ir, args.items) catch
            return error.OutOfMemory;
        return self.builder.call(func_ir, items) catch return error.OutOfMemory;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "compile fixnum" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const result = try compiler.compile(Value.makeFixnum(42), &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.*));
    try testing.expectEqual(@as(i64, 42), result.lit.toFixnum());

    allocator.destroy(result);
}

test "compile nil" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var compiler = Compiler.init(allocator);
    defer compiler.deinit();

    var env = Env.init(allocator, null);
    defer env.deinit();

    const result = try compiler.compile(Value.nil, &env);
    try testing.expectEqual(Ir.lit, std.meta.activeTag(result.*));
    try testing.expect(result.lit.isNil());

    allocator.destroy(result);
}

test "env lookup" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var outer = Env.init(allocator, null);
    defer outer.deinit();
    _ = try outer.bind("x");
    _ = try outer.bind("y");

    var inner = Env.init(allocator, &outer);
    defer inner.deinit();
    _ = try inner.bind("z");

    // z is at depth 0, index 0
    const z_lookup = inner.lookup("z");
    try testing.expect(z_lookup != null);
    try testing.expectEqual(@as(u16, 0), z_lookup.?.depth);
    try testing.expectEqual(@as(u16, 0), z_lookup.?.index);

    // x is at depth 1, index 0
    const x_lookup = inner.lookup("x");
    try testing.expect(x_lookup != null);
    try testing.expectEqual(@as(u16, 1), x_lookup.?.depth);
    try testing.expectEqual(@as(u16, 0), x_lookup.?.index);

    // w doesn't exist
    try testing.expect(inner.lookup("w") == null);
}
