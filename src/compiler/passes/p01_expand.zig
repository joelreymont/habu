//! Macro Expansion Pass
//!
//! Expands macro calls in S-expressions.
//! Macros are expanded at compile-time using the VM.
//!
//! Input: Value (S-expression with macro calls)
//! Output: Value (S-expression with macros expanded)
//!
//! Note: This pass requires VM access to evaluate macro expanders.

const std = @import("std");
const pass_mod = @import("pass.zig");
const PassResult = pass_mod.PassResult;
const PassError = pass_mod.PassError;
const runtime = @import("../../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const Heap = runtime.Heap;
const Chunk = runtime.Chunk;
const Vm = @import("../../interp/vm.zig").Vm;

/// MacroTable maps symbol names to macro definitions
pub const MacroTable = std.StringHashMap(Value);

/// Macro expander - expands macro calls in S-expressions
pub const Expander = struct {
    allocator: std.mem.Allocator,
    heap: *Heap,
    vm: ?*Vm,
    macros: *const MacroTable,

    pub const Error = anyerror;

    pub fn init(allocator: std.mem.Allocator, heap: *Heap, vm: ?*Vm, macros: *const MacroTable) Expander {
        return .{
            .allocator = allocator,
            .heap = heap,
            .vm = vm,
            .macros = macros,
        };
    }

    /// Expand all macros in an expression
    pub fn expand(self: *Expander, expr: Value) Error!Value {
        // Atoms pass through unchanged
        if (expr.isNil()) return expr;
        if (!expr.isCons()) return expr;

        const cons = expr.toPtr(Cons);
        const head = cons.car;
        const tail = cons.cdr;

        // Check if head is a macro
        if (head.isSymbol()) {
            const name = head.toPtr(Symbol).getName();

            if (self.macros.get(name)) |macro_def| {
                if (self.vm) |vm| {
                    // Expand the macro
                    const expanded = try self.expandMacro(macro_def, tail, vm);
                    // Recursively expand the result
                    return self.expand(expanded);
                }
                // No VM - can't expand, return unchanged
            }
        }

        // Not a macro - recurse on list elements
        return self.expandList(expr);
    }

    /// Expand all elements of a list
    fn expandList(self: *Expander, list: Value) Error!Value {
        if (list.isNil()) return Value.nil;
        if (!list.isCons()) return list;

        const cons = list.toPtr(Cons);
        const expanded_car = try self.expand(cons.car);
        const expanded_cdr = try self.expandList(cons.cdr);

        // If nothing changed, return original
        if (expanded_car.raw == cons.car.raw and expanded_cdr.raw == cons.cdr.raw) {
            return list;
        }

        return try self.heap.allocCons(expanded_car, expanded_cdr);
    }

    /// Expand a single macro call
    fn expandMacro(self: *Expander, macro_def: Value, args: Value, vm: *Vm) Error!Value {
        // macro_def is ((params...) body...)
        if (!macro_def.isCons()) return error.InvalidSyntax;

        const def_cons = macro_def.toPtr(Cons);
        const params = def_cons.car;
        const body_list = def_cons.cdr;

        if (!body_list.isCons()) return error.InvalidSyntax;

        const body_cons = body_list.toPtr(Cons);
        const body = body_cons.car;

        // Build lambda: (lambda (params) body)
        const sym_lambda = self.heap.intern("lambda") catch return error.OutOfMemory;
        const params_list = self.heap.allocCons(params, Value.nil) catch return error.OutOfMemory;
        const body_list_val = self.heap.allocCons(body, Value.nil) catch return error.OutOfMemory;
        const lambda_body = self.heap.allocCons(params_list, body_list_val) catch return error.OutOfMemory;
        const lambda_expr = self.heap.allocCons(sym_lambda, lambda_body) catch return error.OutOfMemory;

        // Compile lambda to bytecode
        const compiler_mod = @import("../../compiler/compiler.zig");
        var comp = try compiler_mod.Compiler.initWithHeap(self.allocator, vm);
        defer comp.deinit();

        var env = compiler_mod.Env.init(self.allocator, null);
        defer env.deinit();
        const ir = try comp.compile(lambda_expr, &env);

        const emit_mod = @import("../../bytecode/emit.zig");
        var emitter = emit_mod.Emitter.initWithHeap(self.allocator, self.heap);
        defer emitter.deinit();
        try emitter.emit(ir);
        const chunk_val = try emitter.finalize();

        // Execute to get closure
        const closure_val = try vm.run(chunk_val.toPtr(Chunk));

        // Push args onto stack
        var arg_iter = args;
        var nargs: u8 = 0;
        while (arg_iter.isCons()) {
            const arg_cons = arg_iter.toPtr(Cons);
            try vm.push(arg_cons.car);
            arg_iter = arg_cons.cdr;
            nargs += 1;
        }

        // Call closure with args
        return vm.callClosure(closure_val.toPtr(runtime.Closure), nargs);
    }

    /// Simple parameter substitution (for macros without complex patterns)
    fn substituteParams(self: *Expander, body: Value, params: Value, args: Value) Error!Value {
        if (body.isNil()) return Value.nil;
        if (!body.isCons()) {
            // If body is a symbol, check if it's a parameter
            if (body.isSymbol()) {
                const body_name = body.toPtr(Symbol).getName();

                // Find matching param
                var p = params;
                var a = args;
                while (p.isCons() and a.isCons()) {
                    const param_cons = p.toPtr(Cons);
                    const arg_cons = a.toPtr(Cons);

                    if (param_cons.car.isSymbol()) {
                        const param_name = param_cons.car.toPtr(Symbol).getName();
                        if (std.mem.eql(u8, body_name, param_name)) {
                            return arg_cons.car;
                        }
                    }

                    p = param_cons.cdr;
                    a = arg_cons.cdr;
                }
            }
            return body;
        }

        // Recurse on list
        const cons = body.toPtr(Cons);
        const subst_car = try self.substituteParams(cons.car, params, args);
        const subst_cdr = try self.substituteParams(cons.cdr, params, args);

        if (subst_car.raw == cons.car.raw and subst_cdr.raw == cons.cdr.raw) {
            return body;
        }

        return try self.heap.allocCons(subst_car, subst_cdr);
    }
};

/// Pass wrapper for pipeline integration
pub fn expand(allocator: std.mem.Allocator, heap: *Heap, vm: ?*Vm, macros: *const MacroTable, expr: Value) !Value {
    var expander = Expander.init(allocator, heap, vm, macros);
    return expander.expand(expr);
}

// ============================================================================
// Tests
// ============================================================================

test "expand - no macros" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var macros = MacroTable.init(testing.allocator);
    defer macros.deinit();

    var expander = Expander.init(testing.allocator, &heap, null, &macros);

    // Expression with no macros passes through
    const x = Value.makeFixnum(42);
    const result = try expander.expand(x);
    try testing.expectEqual(x.raw, result.raw);
}

test "expand - list without macros" {
    const testing = std.testing;

    var heap = try Heap.init(testing.allocator, .{});
    defer heap.deinit();

    var macros = MacroTable.init(testing.allocator);
    defer macros.deinit();

    var expander = Expander.init(testing.allocator, &heap, null, &macros);

    // (+ 1 2) with no + macro passes through
    const plus = try heap.intern("+");
    const one = Value.makeFixnum(1);
    const two = Value.makeFixnum(2);
    const inner1 = try heap.allocCons(two, Value.nil);
    const inner2 = try heap.allocCons(one, inner1);
    const expr = try heap.allocCons(plus, inner2);

    const result = try expander.expand(expr);
    try testing.expectEqual(expr.raw, result.raw);
}
