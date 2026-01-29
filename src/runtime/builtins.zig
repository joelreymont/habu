const std = @import("std");
const Value = @import("value.zig").Value;
const Heap = @import("heap.zig").Heap;

/// Pre-interned builtin symbols for fast dispatch
pub const BuiltinSymbols = struct {
    // Control flow
    sym_if: Value,
    sym_let: Value,
    sym_let_star: Value,
    sym_lambda: Value,
    sym_quote: Value,
    sym_quasiquote: Value,
    sym_defun: Value,
    sym_defmacro: Value,
    sym_setq: Value,
    sym_progn: Value,
    sym_cond: Value,
    sym_case: Value,
    sym_when: Value,
    sym_unless: Value,
    sym_while: Value,
    sym_loop: Value,
    sym_return: Value,

    // Special forms
    sym_defclass: Value,
    sym_defmethod: Value,
    sym_defgeneric: Value,
    sym_defstruct: Value,
    sym_in_package: Value,
    sym_defpackage: Value,

    // Type names
    sym_fixnum: Value,
    sym_bignum: Value,
    sym_rational: Value,
    sym_float: Value,
    sym_complex: Value,
    sym_cons: Value,
    sym_symbol: Value,
    sym_string: Value,
    sym_vector: Value,
    sym_closure: Value,
    sym_keyword: Value,
    sym_hash_table: Value,
    sym_stream: Value,
    sym_package: Value,

    // Special markers
    sym_t: Value,
    sym_nil: Value,
    sym_rest: Value, // &rest
    sym_optional: Value, // &optional
    sym_key: Value, // &key
    sym_underscore: Value, // _
    sym_else: Value,

    // Common functions
    sym_car: Value,
    sym_cdr: Value,
    sym_append: Value,
    sym_setf: Value,
    sym_eq: Value,
    sym_equal: Value,
    sym_no_applicable_method: Value,
    sym_no_next_method: Value,

    // Feature evaluation
    sym_and: Value,
    sym_or: Value,
    sym_not: Value,

    // Keywords
    kw_absolute: Value,
    kw_relative: Value,
    kw_read: Value,
    kw_write: Value,
    kw_append: Value,
    kw_upcase: Value,
    kw_downcase: Value,
    kw_capitalize: Value,

    pub fn init(heap: *Heap) !BuiltinSymbols {
        return BuiltinSymbols{
            // Control flow
            .sym_if = try heap.intern("if"),
            .sym_let = try heap.intern("let"),
            .sym_let_star = try heap.intern("let*"),
            .sym_lambda = try heap.intern("lambda"),
            .sym_quote = try heap.intern("quote"),
            .sym_quasiquote = try heap.intern("quasiquote"),
            .sym_defun = try heap.intern("defun"),
            .sym_defmacro = try heap.intern("defmacro"),
            .sym_setq = try heap.intern("setq"),
            .sym_progn = try heap.intern("progn"),
            .sym_cond = try heap.intern("cond"),
            .sym_case = try heap.intern("case"),
            .sym_when = try heap.intern("when"),
            .sym_unless = try heap.intern("unless"),
            .sym_while = try heap.intern("while"),
            .sym_loop = try heap.intern("loop"),
            .sym_return = try heap.intern("return"),

            // Special forms
            .sym_defclass = try heap.intern("defclass"),
            .sym_defmethod = try heap.intern("defmethod"),
            .sym_defgeneric = try heap.intern("defgeneric"),
            .sym_defstruct = try heap.intern("defstruct"),
            .sym_in_package = try heap.intern("in-package"),
            .sym_defpackage = try heap.intern("defpackage"),

            // Type names
            .sym_fixnum = try heap.intern("fixnum"),
            .sym_bignum = try heap.intern("bignum"),
            .sym_rational = try heap.intern("rational"),
            .sym_float = try heap.intern("float"),
            .sym_complex = try heap.intern("complex"),
            .sym_cons = try heap.intern("cons"),
            .sym_symbol = try heap.intern("symbol"),
            .sym_string = try heap.intern("string"),
            .sym_vector = try heap.intern("vector"),
            .sym_closure = try heap.intern("closure"),
            .sym_keyword = try heap.intern("keyword"),
            .sym_hash_table = try heap.intern("hash-table"),
            .sym_stream = try heap.intern("stream"),
            .sym_package = try heap.intern("package"),

            // Special markers
            .sym_t = Value.t,
            .sym_nil = Value.nil,
            .sym_rest = try heap.intern("&rest"),
            .sym_optional = try heap.intern("&optional"),
            .sym_key = try heap.intern("&key"),
            .sym_underscore = try heap.intern("_"),
            .sym_else = try heap.intern("else"),

            // Common functions
            .sym_car = try heap.intern("car"),
            .sym_cdr = try heap.intern("cdr"),
            .sym_append = try heap.intern("append"),
            .sym_setf = try heap.intern("setf"),
            .sym_eq = try heap.intern("eq"),
            .sym_equal = try heap.intern("equal"),
            .sym_no_applicable_method = try heap.intern("no-applicable-method"),
            .sym_no_next_method = try heap.intern("no-next-method"),

            // Feature evaluation
            .sym_and = try heap.intern("and"),
            .sym_or = try heap.intern("or"),
            .sym_not = try heap.intern("not"),

            // Keywords
            .kw_absolute = try heap.internKeyword("absolute"),
            .kw_relative = try heap.internKeyword("relative"),
            .kw_read = try heap.internKeyword("read"),
            .kw_write = try heap.internKeyword("write"),
            .kw_append = try heap.internKeyword("append"),
            .kw_upcase = try heap.internKeyword("upcase"),
            .kw_downcase = try heap.internKeyword("downcase"),
            .kw_capitalize = try heap.internKeyword("capitalize"),
        };
    }
};
