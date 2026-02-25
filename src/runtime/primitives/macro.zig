//! Macro introspection primitives

const std = @import("std");
const runtime = @import("../runtime.zig");
const Value = runtime.Value;
const Heap = runtime.Heap;
const Cons = runtime.Cons;
const Symbol = runtime.Symbol;
const BuiltinSymbols = @import("../builtins.zig").BuiltinSymbols;

pub const Error = error{
    InvalidArgument,
    OutOfMemory,
    NotAMacro,
};

/// macro-function: get macro expansion function
/// (macro-function symbol) -> function or nil
pub fn macroFunction(heap: *Heap, sym: Value) Error!Value {
    _ = heap;
    if (!sym.isSymbol()) return error.InvalidArgument;
    // Macro expansion lives above the runtime (compiler/REPL); runtime-only build returns NIL.
    return Value.nil;
}

/// macroexpand: expand macro call once
/// (macroexpand form) -> expanded-form, expanded-p
/// Returns two values: the expanded form and whether expansion occurred
pub fn macroexpand(heap: *Heap, form: Value) Error!Value {
    _ = heap;
    // TODO: Implement actual macro expansion
    // For now, return the form unchanged
    // This requires integration with the compiler's expandMacro
    return form;
}

/// macroexpand-1: expand macro call repeatedly until not a macro
/// (macroexpand-1 form) -> expanded-form, expanded-p
pub fn macroexpand1(heap: *Heap, form: Value) Error!Value {
    _ = heap;
    // TODO: Implement recursive macro expansion
    return form;
}

/// special-operator-p: test if symbol is a special form
/// (special-operator-p symbol) -> t or nil
pub fn specialOperatorP(builtins: *const BuiltinSymbols, sym: Value) Error!Value {
    if (!sym.isSymbol()) return Value.nil;

    // Check against known special forms - simplified list
    const special_forms = [_]Value{
        builtins.sym_if,
        builtins.sym_cond,
        builtins.sym_and,
        builtins.sym_or,
        builtins.sym_let,
        builtins.sym_lambda,
        builtins.sym_defun,
        builtins.sym_setq,
        builtins.sym_progn,
        builtins.sym_while,
        builtins.sym_quote,
        builtins.sym_defmacro,
        builtins.sym_return,
        builtins.sym_case,
        builtins.sym_defstruct,
        builtins.sym_defclass,
        builtins.sym_defgeneric,
        builtins.sym_defmethod,
    };

    for (special_forms) |special| {
        if (sym.eq(special)) return Value.t;
    }

    return Value.nil;
}

/// constantp: test if form is constant (evaluates to itself)
/// (constantp form) -> t or nil
pub fn constantp(builtins: *const BuiltinSymbols, form: Value) Error!Value {
    switch (form.typeKind()) {
        .nil, .t => return Value.t,
        .fixnum => return Value.t,
        .keyword => return Value.t,
        .string => return Value.t,
        .vector => return Value.t,
        .cons => {
            // Check if it's a quoted form: (quote x) is constant
            const cons = form.toPtr(Cons);
            const head = cons.car;
            if (head.eq(builtins.sym_quote)) {
                return Value.t;
            }
            return Value.nil;
        },
        .symbol => return Value.nil,
        else => return Value.nil,
    }
}

test "special-operator-p recognizes special forms" {
    const allocator = std.testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const builtins = try BuiltinSymbols.init(&heap);

    const result_if = try specialOperatorP(&builtins, builtins.sym_if);
    try std.testing.expect(result_if.eq(Value.t));

    const result_lambda = try specialOperatorP(&builtins, builtins.sym_lambda);
    try std.testing.expect(result_lambda.eq(Value.t));

    const not_special = try heap.intern("not-special");
    const result_not = try specialOperatorP(&builtins, not_special);
    try std.testing.expect(result_not.eq(Value.nil));
}

test "constantp recognizes constants" {
    const allocator = std.testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const builtins = try BuiltinSymbols.init(&heap);

    // Numbers are constant
    const num = Value.makeFixnum(42);
    const result_num = try constantp(&builtins, num);
    try std.testing.expect(result_num.eq(Value.t));

    // nil/t are constant
    const result_nil = try constantp(&builtins, Value.nil);
    try std.testing.expect(result_nil.eq(Value.t));

    const result_t = try constantp(&builtins, Value.t);
    try std.testing.expect(result_t.eq(Value.t));

    // Symbols are not constant (unless keywords)
    const sym = try heap.intern("x");
    const result_sym = try constantp(&builtins, sym);
    try std.testing.expect(result_sym.eq(Value.nil));
}
