//! Intermediate Representation for Habu compiler
//!
//! The IR is a tree structure representing Habu programs after parsing
//! but before bytecode emission. It's designed to be:
//! - Easy to generate from S-expressions
//! - Easy to type-check (occurrence typing)
//! - Easy to compile to bytecode or JIT
//!
//! IR nodes use tagged unions for exhaustive matching.

const std = @import("std");
const Value = @import("../runtime/value.zig").Value;

/// IR node - represents a Habu expression
pub const Ir = union(enum) {
    // ========================================================================
    // Literals
    // ========================================================================

    /// Literal value (fixnum, nil, t)
    lit: Value,

    /// Symbol literal (for quote)
    quote_sym: []const u8,

    /// Quoted expression (for complex quoted data)
    quote: *const Ir,

    // ========================================================================
    // Variables
    // ========================================================================

    /// Variable reference
    @"var": struct {
        name: []const u8,
        /// Lexical depth (0 = current frame, 1 = parent, etc.)
        depth: u16,
        /// Slot index within frame
        index: u16,
    },

    /// Variable assignment
    set: struct {
        name: []const u8,
        depth: u16,
        index: u16,
        value: *const Ir,
    },

    /// Global variable reference
    global_ref: struct {
        name: []const u8,
        index: u16,
    },

    /// Global variable definition
    define: struct {
        name: []const u8,
        index: u16,
        value: *const Ir,
    },

    // ========================================================================
    // Binding forms
    // ========================================================================

    /// Let binding: (let ((x 1) (y 2)) body)
    let: struct {
        bindings: []const Binding,
        body: *const Ir,
    },

    /// Lambda: (lambda (x y) body)
    lambda: struct {
        params: []const []const u8,
        /// Rest parameter name (for dotted param lists like (a b . rest))
        rest_param: ?[]const u8,
        /// Free variables captured from enclosing scope
        captures: []const Capture,
        body: *const Ir,
    },

    // ========================================================================
    // Control flow
    // ========================================================================

    /// Conditional: (if cond then else)
    @"if": struct {
        cond: *const Ir,
        then_branch: *const Ir,
        else_branch: *const Ir,
    },

    /// Sequence: (progn e1 e2 ... en)
    progn: []const *const Ir,

    /// While loop: (while cond body...)
    loop: struct {
        cond: *const Ir,
        body: *const Ir,
    },

    /// Named block: (block name body...)
    block: struct {
        name: []const u8,
        body: *const Ir,
    },

    /// Return from block: (return-from name value)
    return_from: struct {
        name: []const u8,
        value: *const Ir,
    },

    /// Unwind-protect: (unwind-protect protected cleanup...)
    unwind_protect: struct {
        protected: *const Ir,
        cleanup: *const Ir,
    },

    /// Catch: (catch tag body...)
    @"catch": struct {
        tag: *const Ir,
        body: *const Ir,
    },

    /// Throw: (throw tag value)
    throw: struct {
        tag: *const Ir,
        value: *const Ir,
    },

    /// Tagbody: (tagbody tag1 form1 tag2 form2 ...)
    /// Tags are symbols, forms are expressions
    tagbody: struct {
        /// Tag names (for go targets)
        tags: []const []const u8,
        /// Segments: code between tags (segments.len == tags.len + 1)
        /// segments[0] = code before first tag
        /// segments[i] = code after tags[i-1]
        segments: []const *const Ir,
    },

    /// Go: (go tag)
    go: struct {
        tag: []const u8,
    },

    /// Multiple values: (values v1 v2 ...)
    values: []const *const Ir,

    /// Multiple-value-bind: (multiple-value-bind (vars...) expr body...)
    mv_bind: struct {
        vars: []const []const u8,
        expr: *const Ir,
        body: *const Ir,
    },

    /// Multiple-value-list: (multiple-value-list expr)
    /// Evaluates expr and gathers all values into a list
    mv_list: struct {
        expr: *const Ir,
    },

    /// Format: (format dest control-string args...)
    format: struct {
        dest: *const Ir,
        control: *const Ir,
        args: []const *const Ir,
    },

    // ========================================================================
    // Hash table operations
    // ========================================================================

    /// Make hash table: (make-hash-table)
    make_hash: struct {
        capacity: u16,
    },

    /// Hash table get: (gethash key hashtable)
    hash_get: struct {
        table: *const Ir,
        key: *const Ir,
    },

    /// Hash table set: (sethash key hashtable value)
    hash_set: struct {
        table: *const Ir,
        key: *const Ir,
        value: *const Ir,
    },

    /// Hash table remove: (remhash key hashtable)
    hash_rem: struct {
        table: *const Ir,
        key: *const Ir,
    },

    /// Hash table count: (hash-table-count hashtable)
    hash_count: struct {
        operand: *const Ir,
    },

    /// Hash table predicate: (hash-table-p x)
    hashtablep: struct {
        operand: *const Ir,
    },

    // ========================================================================
    // Function calls
    // ========================================================================

    /// Function call: (f arg1 arg2 ...)
    call: struct {
        func: *const Ir,
        args: []const *const Ir,
    },

    /// Tail call (same as call but in tail position)
    tailcall: struct {
        func: *const Ir,
        args: []const *const Ir,
    },

    /// Apply function to list of args
    apply: struct {
        func: *const Ir,
        args: *const Ir,
    },

    /// Multiple-value-call: (multiple-value-call fn form1 form2 ...)
    /// Gathers all values from each form and passes them as args to fn
    mv_call: struct {
        func: *const Ir,
        forms: []const *const Ir,
    },

    // ========================================================================
    // Primitives - Arithmetic
    // ========================================================================

    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,
    mod: BinaryOp,

    // ========================================================================
    // Primitives - Comparison
    // ========================================================================

    eq: BinaryOp, // eq (pointer/fixnum equality)
    lt: BinaryOp,
    gt: BinaryOp,
    le: BinaryOp,
    ge: BinaryOp,
    num_eq: BinaryOp, // = (numeric equality)

    // ========================================================================
    // Primitives - Logic
    // ========================================================================

    not: UnaryOp,

    // ========================================================================
    // Primitives - List operations
    // ========================================================================

    cons: BinaryOp,
    car: UnaryOp,
    cdr: UnaryOp,
    list: []const *const Ir, // (list a b c)
    append: BinaryOp, // (append list1 list2)
    length: UnaryOp, // (length list)
    reverse: UnaryOp, // (reverse list)
    nth: BinaryOp, // (nth n list)
    nthcdr: BinaryOp, // (nthcdr n list)
    last: UnaryOp, // (last list)
    member: BinaryOp, // (member item list)

    // ========================================================================
    // Primitives - Type predicates
    // ========================================================================

    consp: UnaryOp,
    symbolp: UnaryOp,
    numberp: UnaryOp,
    stringp: UnaryOp,
    vectorp: UnaryOp,
    closurep: UnaryOp,
    keywordp: UnaryOp,
    nilp: UnaryOp,
    characterp: UnaryOp,
    floatp: UnaryOp,
    listp: UnaryOp, // nil or cons
    atom: UnaryOp, // not a cons

    // ========================================================================
    // Primitives - Character operations
    // ========================================================================

    char_code: UnaryOp,
    code_char: UnaryOp,
    char_eq: BinaryOp,
    char_lt: BinaryOp,
    char_gt: BinaryOp,
    read_char: void, // No operands - reads from stdin
    peek_char: void, // No operands - peeks at stdin
    read: void, // Read S-expression from stdin
    read_from_string: UnaryOp, // Parse string to value
    load: UnaryOp, // Load and evaluate a file
    unread_char: UnaryOp, // Push character back
    eval: UnaryOp, // Evaluate expression at runtime
    gensym: void, // Generate unique symbol
    macroexpand: UnaryOp, // Expand macros in expression
    princ: UnaryOp, // Print without escaping
    terpri: void, // Print newline
    write_char: UnaryOp, // Write character to stdout
    char_upcase: UnaryOp, // Convert char to uppercase
    char_downcase: UnaryOp, // Convert char to lowercase
    digit_char_p: UnaryOp, // Check if char is digit
    alpha_char_p: UnaryOp, // Check if char is alphabetic
    parse_integer: UnaryOp, // Parse string to integer
    write_to_string: UnaryOp, // Convert value to string
    logand: BinaryOp, // Bitwise AND
    logior: BinaryOp, // Bitwise OR
    logxor: BinaryOp, // Bitwise XOR
    lognot: UnaryOp, // Bitwise NOT
    ash: BinaryOp, // Arithmetic shift
    read_file: UnaryOp, // Read file to string
    write_file: BinaryOp, // Write string to file
    make_string: BinaryOp, // Create string (length, char)
    string_to_list: UnaryOp, // String to list of chars
    list_to_string: UnaryOp, // List of chars to string
    string_upcase: UnaryOp, // Convert string to uppercase
    string_downcase: UnaryOp, // Convert string to lowercase
    boundp: UnaryOp, // Check if symbol has global binding
    fboundp: UnaryOp, // Check if symbol has function binding
    symbol_value: UnaryOp, // Get symbol's global value
    symbol_function: UnaryOp, // Get symbol's function binding
    typep: BinaryOp, // Check if object is of given type

    // ========================================================================
    // Primitives - Numeric predicates
    // ========================================================================

    abs: UnaryOp,
    zerop: UnaryOp,
    plusp: UnaryOp,
    minusp: UnaryOp,
    evenp: UnaryOp,
    oddp: UnaryOp,

    // ========================================================================
    // Primitives - Vector operations
    // ========================================================================

    vec_new: struct {
        size: *const Ir,
        init: ?*const Ir,
    },
    vec: []const *const Ir, // (vector a b c)
    vec_ref: BinaryOp,
    vec_set: struct {
        vec: *const Ir,
        index: *const Ir,
        value: *const Ir,
    },
    vec_len: UnaryOp,

    // ========================================================================
    // Primitives - Box operations (mutable cells)
    // ========================================================================

    make_box: UnaryOp,
    box_ref: UnaryOp,
    box_set: BinaryOp,

    // ========================================================================
    // Primitives - String operations
    // ========================================================================

    str_ref: BinaryOp,
    str_len: UnaryOp,
    str_concat: BinaryOp,
    str_eq: BinaryOp,
    substring: struct {
        str: *const Ir,
        start: *const Ir,
        end: *const Ir,
    },

    // ========================================================================
    // Primitives - I/O and Symbol
    // ========================================================================

    print: UnaryOp,
    random: UnaryOp,
    intern: UnaryOp,
    sym_name: UnaryOp,
    type_of: UnaryOp, // Get type of value as symbol

    // ========================================================================
    // Type assertions (gradual typing)
    // ========================================================================

    /// Assert value is fixnum (error if not)
    /// ( x -- x ) but errors if not fixnum
    assert_fixnum: UnaryOp,

    /// Assert value is cons (error if not)
    assert_cons: UnaryOp,

    /// Assert value is symbol (error if not)
    assert_symbol: UnaryOp,

    /// Assert value is string (error if not)
    assert_string: UnaryOp,

    /// Assert value is vector (error if not)
    assert_vector: UnaryOp,

    /// Assert value is closure (error if not)
    assert_closure: UnaryOp,

    /// Assert value is non-nil (error if nil)
    assert_non_nil: UnaryOp,

    /// Assert value is list (nil or cons)
    assert_list: UnaryOp,

    // ========================================================================
    // Helper types
    // ========================================================================

    pub const BinaryOp = struct {
        left: *const Ir,
        right: *const Ir,
    };

    pub const UnaryOp = struct {
        operand: *const Ir,
    };

    pub const Binding = struct {
        name: []const u8,
        value: *const Ir,
    };

    pub const Capture = struct {
        name: []const u8,
        /// Depth in enclosing scope chain
        depth: u16,
        /// Index in that scope
        index: u16,
    };

    // ========================================================================
    // Predicates
    // ========================================================================

    /// Check if this node is in tail position (for TCO)
    pub fn isTailPosition(self: Ir) bool {
        return switch (self) {
            .tailcall => true,
            else => false,
        };
    }

    /// Check if this is a primitive operation
    pub fn isPrimitive(self: Ir) bool {
        return switch (self) {
            .add, .sub, .mul, .div, .mod,
            .eq, .lt, .gt, .le, .ge, .num_eq,
            .not,
            .cons, .car, .cdr, .list,
            .consp, .symbolp, .numberp, .stringp, .vectorp, .closurep, .keywordp, .nilp, .listp, .atom,
            .vec_new, .vec, .vec_ref, .vec_set, .vec_len, .make_box, .box_ref, .box_set,
            .str_ref, .str_len, .str_concat, .str_eq, .substring, .string_upcase, .string_downcase,
            .print, .random, .intern, .sym_name, .type_of,
            .assert_fixnum, .assert_cons, .assert_symbol, .assert_string,
            .assert_vector, .assert_closure, .assert_non_nil, .assert_list,
            => true,
            else => false,
        };
    }

    /// Get the active tag name for debugging
    pub fn tagName(self: Ir) []const u8 {
        return @tagName(self);
    }
};

// ============================================================================
// IR Builder - allocates IR nodes
// ============================================================================

pub const IrBuilder = struct {
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) IrBuilder {
        return .{ .allocator = allocator };
    }

    // Literals
    pub fn lit(self: IrBuilder, v: Value) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lit = v };
        return node;
    }

    pub fn quoteSym(self: IrBuilder, name: []const u8) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .quote_sym = name_copy };
        return node;
    }

    // Variables
    pub fn variable(self: IrBuilder, name: []const u8, depth: u16, index: u16) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .@"var" = .{ .name = name_copy, .depth = depth, .index = index } };
        return node;
    }

    pub fn set(self: IrBuilder, name: []const u8, depth: u16, index: u16, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .set = .{ .name = name_copy, .depth = depth, .index = index, .value = value } };
        return node;
    }

    pub fn globalRef(self: IrBuilder, name: []const u8, index: u16) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .global_ref = .{ .name = name_copy, .index = index } };
        return node;
    }

    pub fn define(self: IrBuilder, name: []const u8, index: u16, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const name_copy = try self.allocator.dupe(u8, name);
        node.* = .{ .define = .{ .name = name_copy, .index = index, .value = value } };
        return node;
    }

    // Binding forms
    pub fn letExpr(self: IrBuilder, bindings: []const Ir.Binding, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const bindings_copy = try self.allocator.dupe(Ir.Binding, bindings);
        node.* = .{ .let = .{ .bindings = bindings_copy, .body = body } };
        return node;
    }

    pub fn lambda(self: IrBuilder, params: []const []const u8, rest_param: ?[]const u8, captures: []const Ir.Capture, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        // Copy params
        var params_copy = try self.allocator.alloc([]const u8, params.len);
        for (params, 0..) |p, i| {
            params_copy[i] = try self.allocator.dupe(u8, p);
        }
        const captures_copy = try self.allocator.dupe(Ir.Capture, captures);
        const rest_copy = if (rest_param) |rp| try self.allocator.dupe(u8, rp) else null;
        node.* = .{ .lambda = .{ .params = params_copy, .rest_param = rest_copy, .captures = captures_copy, .body = body } };
        return node;
    }

    // Control flow
    pub fn ifExpr(self: IrBuilder, cond: *const Ir, then_branch: *const Ir, else_branch: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .@"if" = .{ .cond = cond, .then_branch = then_branch, .else_branch = else_branch } };
        return node;
    }

    pub fn progn(self: IrBuilder, exprs: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const exprs_copy = try self.allocator.dupe(*const Ir, exprs);
        node.* = .{ .progn = exprs_copy };
        return node;
    }

    pub fn loop(self: IrBuilder, cond: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .loop = .{ .cond = cond, .body = body } };
        return node;
    }

    pub fn block(self: IrBuilder, name: []const u8, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .block = .{ .name = name, .body = body } };
        return node;
    }

    pub fn returnFrom(self: IrBuilder, name: []const u8, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .return_from = .{ .name = name, .value = value } };
        return node;
    }

    pub fn unwindProtect(self: IrBuilder, protected: *const Ir, cleanup: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .unwind_protect = .{ .protected = protected, .cleanup = cleanup } };
        return node;
    }

    pub fn @"catch"(self: IrBuilder, tag: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .@"catch" = .{ .tag = tag, .body = body } };
        return node;
    }

    pub fn throw(self: IrBuilder, tag: *const Ir, value: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .throw = .{ .tag = tag, .value = value } };
        return node;
    }

    pub fn tagbody(self: IrBuilder, tags: []const []const u8, segments: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const tags_copy = try self.allocator.dupe([]const u8, tags);
        const segments_copy = try self.allocator.dupe(*const Ir, segments);
        node.* = .{ .tagbody = .{ .tags = tags_copy, .segments = segments_copy } };
        return node;
    }

    pub fn go(self: IrBuilder, tag: []const u8) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .go = .{ .tag = tag } };
        return node;
    }

    pub fn values(self: IrBuilder, vals: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const vals_copy = try self.allocator.dupe(*const Ir, vals);
        node.* = .{ .values = vals_copy };
        return node;
    }

    pub fn mvBind(self: IrBuilder, vars: []const []const u8, expr: *const Ir, body: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const vars_copy = try self.allocator.dupe([]const u8, vars);
        node.* = .{ .mv_bind = .{ .vars = vars_copy, .expr = expr, .body = body } };
        return node;
    }

    pub fn mvCall(self: IrBuilder, func: *const Ir, forms: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const forms_copy = try self.allocator.dupe(*const Ir, forms);
        node.* = .{ .mv_call = .{ .func = func, .forms = forms_copy } };
        return node;
    }

    pub fn mvList(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .mv_list = .{ .expr = expr } };
        return node;
    }

    pub fn format(self: IrBuilder, dest: *const Ir, control: *const Ir, args: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const args_copy = try self.allocator.dupe(*const Ir, args);
        node.* = .{ .format = .{ .dest = dest, .control = control, .args = args_copy } };
        return node;
    }

    // Function calls
    pub fn call(self: IrBuilder, func: *const Ir, args: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const args_copy = try self.allocator.dupe(*const Ir, args);
        node.* = .{ .call = .{ .func = func, .args = args_copy } };
        return node;
    }

    pub fn tailcall(self: IrBuilder, func: *const Ir, args: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const args_copy = try self.allocator.dupe(*const Ir, args);
        node.* = .{ .tailcall = .{ .func = func, .args = args_copy } };
        return node;
    }

    // Arithmetic
    pub fn add(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .add = .{ .left = left, .right = right } };
        return node;
    }

    pub fn sub(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .sub = .{ .left = left, .right = right } };
        return node;
    }

    pub fn mul(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .mul = .{ .left = left, .right = right } };
        return node;
    }

    pub fn div(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .div = .{ .left = left, .right = right } };
        return node;
    }

    // Comparison
    pub fn eq(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .eq = .{ .left = left, .right = right } };
        return node;
    }

    pub fn lt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn gt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .gt = .{ .left = left, .right = right } };
        return node;
    }

    // List operations
    pub fn cons(self: IrBuilder, car_val: *const Ir, cdr_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .cons = .{ .left = car_val, .right = cdr_val } };
        return node;
    }

    pub fn car(self: IrBuilder, pair_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .car = .{ .operand = pair_val } };
        return node;
    }

    pub fn cdr(self: IrBuilder, pair_val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .cdr = .{ .operand = pair_val } };
        return node;
    }

    pub fn list(self: IrBuilder, elements: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const elems_copy = try self.allocator.dupe(*const Ir, elements);
        node.* = .{ .list = elems_copy };
        return node;
    }

    pub fn append(self: IrBuilder, list1: *const Ir, list2: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .append = .{ .left = list1, .right = list2 } };
        return node;
    }

    // Type predicates
    pub fn consp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .consp = .{ .operand = operand } };
        return node;
    }

    pub fn symbolp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbolp = .{ .operand = operand } };
        return node;
    }

    pub fn numberp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .numberp = .{ .operand = operand } };
        return node;
    }

    pub fn nilp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .nilp = .{ .operand = operand } };
        return node;
    }

    pub fn characterp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .characterp = .{ .operand = operand } };
        return node;
    }

    pub fn floatp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .floatp = .{ .operand = operand } };
        return node;
    }

    pub fn listp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .listp = .{ .operand = operand } };
        return node;
    }

    pub fn atomp(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .atom = .{ .operand = operand } };
        return node;
    }

    pub fn not(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .not = .{ .operand = operand } };
        return node;
    }

    // Character operations
    pub fn charCode(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_code = .{ .operand = operand } };
        return node;
    }

    pub fn codeChar(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .code_char = .{ .operand = operand } };
        return node;
    }

    pub fn charEq(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_eq = .{ .left = left, .right = right } };
        return node;
    }

    pub fn charLt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_lt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn charGt(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_gt = .{ .left = left, .right = right } };
        return node;
    }

    pub fn readChar(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_char = {} };
        return node;
    }

    pub fn peekChar(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .peek_char = {} };
        return node;
    }

    pub fn readSexp(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read = {} };
        return node;
    }

    pub fn load(self: IrBuilder, filename: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .load = .{ .operand = filename } };
        return node;
    }

    pub fn readFromString(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_from_string = .{ .operand = str } };
        return node;
    }

    pub fn unreadChar(self: IrBuilder, char: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .unread_char = .{ .operand = char } };
        return node;
    }

    pub fn eval(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .eval = .{ .operand = expr } };
        return node;
    }

    pub fn gensym(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .gensym = {} };
        return node;
    }

    pub fn macroexpand(self: IrBuilder, expr: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .macroexpand = .{ .operand = expr } };
        return node;
    }

    pub fn princ(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .princ = .{ .operand = val } };
        return node;
    }

    pub fn terpri(self: IrBuilder) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .terpri = {} };
        return node;
    }

    pub fn writeChar(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_char = .{ .operand = val } };
        return node;
    }

    pub fn charUpcase(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_upcase = .{ .operand = val } };
        return node;
    }

    pub fn charDowncase(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .char_downcase = .{ .operand = val } };
        return node;
    }

    pub fn digitCharP(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .digit_char_p = .{ .operand = val } };
        return node;
    }

    pub fn alphaCharP(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .alpha_char_p = .{ .operand = val } };
        return node;
    }

    pub fn parseInteger(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .parse_integer = .{ .operand = val } };
        return node;
    }

    pub fn writeToString(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_to_string = .{ .operand = val } };
        return node;
    }

    pub fn logand(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logand = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logior(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logior = .{ .left = left, .right = right } };
        return node;
    }

    pub fn logxor(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .logxor = .{ .left = left, .right = right } };
        return node;
    }

    pub fn lognot(self: IrBuilder, val: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .lognot = .{ .operand = val } };
        return node;
    }

    pub fn ash(self: IrBuilder, n: *const Ir, count: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .ash = .{ .left = n, .right = count } };
        return node;
    }

    pub fn readFile(self: IrBuilder, path: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .read_file = .{ .operand = path } };
        return node;
    }

    pub fn writeFile(self: IrBuilder, path: *const Ir, content: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .write_file = .{ .left = path, .right = content } };
        return node;
    }

    pub fn makeString(self: IrBuilder, len: *const Ir, char: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .make_string = .{ .left = len, .right = char } };
        return node;
    }

    pub fn stringToList(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .string_to_list = .{ .operand = str } };
        return node;
    }

    pub fn listToString(self: IrBuilder, lst: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .list_to_string = .{ .operand = lst } };
        return node;
    }

    pub fn stringUpcase(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .string_upcase = .{ .operand = str } };
        return node;
    }

    pub fn stringDowncase(self: IrBuilder, str: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .string_downcase = .{ .operand = str } };
        return node;
    }

    pub fn boundp(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .boundp = .{ .operand = sym } };
        return node;
    }

    pub fn fboundp(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .fboundp = .{ .operand = sym } };
        return node;
    }

    pub fn symbolValue(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbol_value = .{ .operand = sym } };
        return node;
    }

    pub fn symbolFunction(self: IrBuilder, sym: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .symbol_function = .{ .operand = sym } };
        return node;
    }

    pub fn typep(self: IrBuilder, obj: *const Ir, type_spec: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .typep = .{ .left = obj, .right = type_spec } };
        return node;
    }

    // Numeric predicates
    pub fn abs(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .abs = .{ .operand = n } };
        return node;
    }

    pub fn zerop(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .zerop = .{ .operand = n } };
        return node;
    }

    pub fn plusp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .plusp = .{ .operand = n } };
        return node;
    }

    pub fn minusp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .minusp = .{ .operand = n } };
        return node;
    }

    pub fn evenp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .evenp = .{ .operand = n } };
        return node;
    }

    pub fn oddp(self: IrBuilder, n: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .oddp = .{ .operand = n } };
        return node;
    }

    // Vector operations
    pub fn vecNew(self: IrBuilder, size: *const Ir, init_val: ?*const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_new = .{ .size = size, .init = init_val } };
        return node;
    }

    pub fn vec(self: IrBuilder, elements: []const *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        const elems_copy = try self.allocator.dupe(*const Ir, elements);
        node.* = .{ .vec = elems_copy };
        return node;
    }

    pub fn vecRef(self: IrBuilder, v: *const Ir, index: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_ref = .{ .left = v, .right = index } };
        return node;
    }

    pub fn vecLen(self: IrBuilder, v: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .vec_len = .{ .operand = v } };
        return node;
    }

    // String operations
    pub fn strLen(self: IrBuilder, s: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .str_len = .{ .operand = s } };
        return node;
    }

    pub fn strConcat(self: IrBuilder, left: *const Ir, right: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .str_concat = .{ .left = left, .right = right } };
        return node;
    }

    // I/O
    pub fn print(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .print = .{ .operand = operand } };
        return node;
    }

    pub fn typeOf(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .type_of = .{ .operand = operand } };
        return node;
    }

    // Type assertions
    pub fn assertFixnum(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_fixnum = .{ .operand = operand } };
        return node;
    }

    pub fn assertCons(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_cons = .{ .operand = operand } };
        return node;
    }

    pub fn assertSymbol(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_symbol = .{ .operand = operand } };
        return node;
    }

    pub fn assertString(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_string = .{ .operand = operand } };
        return node;
    }

    pub fn assertVector(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_vector = .{ .operand = operand } };
        return node;
    }

    pub fn assertClosure(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_closure = .{ .operand = operand } };
        return node;
    }

    pub fn assertNonNil(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_non_nil = .{ .operand = operand } };
        return node;
    }

    pub fn assertList(self: IrBuilder, operand: *const Ir) !*Ir {
        const node = try self.allocator.create(Ir);
        node.* = .{ .assert_list = .{ .operand = operand } };
        return node;
    }
};

// ============================================================================
// Tests
// ============================================================================

test "ir literal" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const builder = IrBuilder.init(allocator);
    const node = try builder.lit(Value.makeFixnum(42));
    defer allocator.destroy(node);

    try testing.expectEqual(Ir.lit, std.meta.activeTag(node.*));
    try testing.expectEqual(@as(i64, 42), node.lit.toFixnum());
}

test "ir binary op" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const builder = IrBuilder.init(allocator);
    const left = try builder.lit(Value.makeFixnum(1));
    const right = try builder.lit(Value.makeFixnum(2));
    const sum = try builder.add(left, right);
    defer {
        allocator.destroy(left);
        allocator.destroy(right);
        allocator.destroy(sum);
    }

    try testing.expectEqual(Ir.add, std.meta.activeTag(sum.*));
    try testing.expect(sum.isPrimitive());
}

test "ir if expression" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const builder = IrBuilder.init(allocator);
    const test_expr = try builder.lit(Value.t);
    const then_expr = try builder.lit(Value.makeFixnum(1));
    const else_expr = try builder.lit(Value.makeFixnum(0));
    const if_node = try builder.ifExpr(test_expr, then_expr, else_expr);
    defer {
        allocator.destroy(test_expr);
        allocator.destroy(then_expr);
        allocator.destroy(else_expr);
        allocator.destroy(if_node);
    }

    try testing.expectEqual(Ir.@"if", std.meta.activeTag(if_node.*));
    try testing.expect(!if_node.isPrimitive());
}

test "ir lambda" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const builder = IrBuilder.init(allocator);
    const body = try builder.lit(Value.nil);
    const params = [_][]const u8{ "x", "y" };
    const captures = [_]Ir.Capture{};
    const lam = try builder.lambda(&params, null, &captures, body);
    defer {
        allocator.destroy(body);
        for (lam.lambda.params) |p| allocator.free(p);
        allocator.free(lam.lambda.params);
        allocator.free(lam.lambda.captures);
        allocator.destroy(lam);
    }

    try testing.expectEqual(Ir.lambda, std.meta.activeTag(lam.*));
    try testing.expectEqual(@as(usize, 2), lam.lambda.params.len);
    try testing.expectEqualStrings("x", lam.lambda.params[0]);
}

test "ir variable" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const builder = IrBuilder.init(allocator);
    const v = try builder.variable("foo", 0, 3);
    defer {
        allocator.free(v.@"var".name);
        allocator.destroy(v);
    }

    try testing.expectEqual(Ir.@"var", std.meta.activeTag(v.*));
    try testing.expectEqualStrings("foo", v.@"var".name);
    try testing.expectEqual(@as(u16, 0), v.@"var".depth);
    try testing.expectEqual(@as(u16, 3), v.@"var".index);
}

test "ir tag name" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const builder = IrBuilder.init(allocator);
    const node = try builder.lit(Value.nil);
    defer allocator.destroy(node);

    try testing.expectEqualStrings("lit", node.tagName());
}
