//! Bidirectional Type Checker for Habu's Dependent Type System
//!
//! Bidirectional type checking has two modes:
//! - **Synthesis (infer)**: Γ ⊢ e ⇒ A - infer the type of expression e
//! - **Checking**: Γ ⊢ e ⇐ A - verify that expression e has type A
//!
//! This approach is particularly suited for dependent types because:
//! - Type annotations propagate inward, reducing inference burden
//! - Lambda bodies can be checked against expected Pi types
//! - Application arguments can be checked against expected domain types
//!
//! Key typing rules implemented:
//! - Pi-Form: Γ ⊢ A ⇐ Type, Γ,x:A ⊢ B ⇐ Type ⟹ Γ ⊢ (Π x:A. B) ⇐ Type
//! - Lambda-Check: Γ,x:A ⊢ e ⇐ B ⟹ Γ ⊢ (λx.e) ⇐ (Π x:A. B)
//! - App-Infer: Γ ⊢ f ⇒ (Π x:A. B), Γ ⊢ a ⇐ A ⟹ Γ ⊢ (f a) ⇒ B[a/x]
//! - Sub: Γ ⊢ e ⇒ A, A <: B ⟹ Γ ⊢ e ⇐ B (subsumption)

const std = @import("std");
const type_mod = @import("type.zig");
const term_mod = @import("term.zig");
const normalize_mod = @import("normalize.zig");
const conversion_mod = @import("conversion.zig");
const ir_mod = @import("../compiler/ir.zig");
const builtins_mod = @import("../runtime/builtins.zig");
const Value = @import("../runtime/value.zig").Value;

const Type = type_mod.Type;
const TypeBuilder = type_mod.TypeBuilder;
const Quantity = type_mod.Quantity;
const Term = term_mod.Term;
const TermBuilder = term_mod.TermBuilder;
const Normalizer = normalize_mod.Normalizer;
const NormEnv = normalize_mod.NormEnv;
const TypeConverter = conversion_mod.TypeConverter;
const Ir = ir_mod.Ir;

/// Typing context - maps variable names to their types and quantities
pub const TypingCtx = struct {
    allocator: std.mem.Allocator,
    bindings: std.StringHashMapUnmanaged(Binding),
    parent: ?*const TypingCtx,

    pub const Binding = struct {
        ty: *const Type,
        quantity: Quantity,
        /// How many times this variable has been used
        uses: usize,
    };

    pub fn init(allocator: std.mem.Allocator) TypingCtx {
        return .{
            .allocator = allocator,
            .bindings = std.StringHashMapUnmanaged(Binding){},
            .parent = null,
        };
    }

    pub fn initWithParent(allocator: std.mem.Allocator, parent: *const TypingCtx) TypingCtx {
        return .{
            .allocator = allocator,
            .bindings = std.StringHashMapUnmanaged(Binding){},
            .parent = parent,
        };
    }

    pub fn deinit(self: *TypingCtx) void {
        self.bindings.deinit(self.allocator);
    }

    /// Add a binding to the context
    pub fn bind(self: *TypingCtx, name: []const u8, ty: *const Type, quantity: Quantity) !void {
        try self.bindings.put(self.allocator, name, .{
            .ty = ty,
            .quantity = quantity,
            .uses = 0,
        });
    }

    /// Look up a variable's type
    pub fn lookup(self: *const TypingCtx, name: []const u8) ?*const Type {
        if (self.bindings.get(name)) |b| {
            return b.ty;
        }
        if (self.parent) |p| {
            return p.lookup(name);
        }
        return null;
    }

    /// Look up a variable's binding (type + quantity)
    pub fn lookupBinding(self: *const TypingCtx, name: []const u8) ?Binding {
        if (self.bindings.get(name)) |b| {
            return b;
        }
        if (self.parent) |p| {
            return p.lookupBinding(name);
        }
        return null;
    }

    /// Record a use of a variable (for linearity checking)
    pub fn use(self: *TypingCtx, name: []const u8) !void {
        if (self.bindings.getPtr(name)) |b| {
            b.uses += 1;
        } else if (self.parent) |p| {
            // Can't modify parent - would need mutable parent
            _ = p;
        }
    }

    /// Check that all linear variables were used exactly once
    pub fn checkLinearity(self: *const TypingCtx) ?[]const u8 {
        var it = self.bindings.iterator();
        while (it.next()) |entry| {
            const binding = entry.value_ptr.*;
            if (binding.quantity == .one) {
                if (binding.uses != 1) {
                    return entry.key_ptr.*;
                }
            }
        }
        return null;
    }
};

/// Bidirectional type checker
pub const BiChecker = struct {
    allocator: std.mem.Allocator,
    type_builder: TypeBuilder,
    term_builder: TermBuilder,
    normalizer: Normalizer,
    converter: TypeConverter,
    errors: std.ArrayListUnmanaged(TypeError),
    builtins: *const builtins_mod.BuiltinSymbols,

    pub fn init(allocator: std.mem.Allocator, builtins: *const builtins_mod.BuiltinSymbols) BiChecker {
        return .{
            .allocator = allocator,
            .type_builder = TypeBuilder.init(allocator),
            .term_builder = TermBuilder.init(allocator),
            .normalizer = Normalizer.init(allocator),
            .converter = TypeConverter.init(allocator),
            .errors = std.ArrayListUnmanaged(TypeError){},
            .builtins = builtins,
        };
    }

    pub fn deinit(self: *BiChecker) void {
        self.errors.deinit(self.allocator);
        self.type_builder.deinit();
        self.term_builder.deinit();
        self.normalizer.deinit();
    }

    // ========================================================================
    // Synthesis Mode: Γ ⊢ e ⇒ A
    // ========================================================================

    /// Synthesize (infer) the type of an expression
    pub fn infer(self: *BiChecker, expr: *const Ir, ctx: *const TypingCtx) CheckError!*const Type {
        return switch (expr.*) {
            // Literals have known types
            .lit => |val| self.inferLiteral(val.raw),

            // Variables: look up in context
            .@"var" => |v| {
                if (ctx.lookupBinding(v.name)) |binding| {
                    // Check that zero-quantity variables aren't used at runtime
                    if (binding.quantity == .zero) {
                        try self.reportError("Cannot use erased (0-quantity) variable at runtime", v.name);
                        return error.TypeError;
                    }
                    return binding.ty;
                }
                try self.reportError("Unbound variable", v.name);
                return error.TypeError;
            },

            // Global variables
            .global_ref => |g| {
                if (ctx.lookup(g.name)) |ty| {
                    return ty;
                }
                // Globals are assumed to be any type if not in context
                return &type_mod.t_any;
            },

            // Arithmetic primitives: fixnum -> fixnum -> fixnum
            .add, .sub, .mul, .div, .mod => &type_mod.t_fixnum,

            // Comparison primitives: returns nil or t (any)
            .eq, .lt, .gt, .le, .ge, .num_eq, .equal, .eql => &type_mod.t_any,

            // List construction
            .cons => &type_mod.t_cons,

            // Projections - can use Sigma types for precision
            .car => |op| self.inferCar(op.operand, ctx),
            .cdr => |op| self.inferCdr(op.operand, ctx),
            .list => &type_mod.t_cons, // Returns a cons (or nil if empty)
            .append, .reverse, .nthcdr, .last => &type_mod.t_any,
            .nth, .length => &type_mod.t_any,
            .member, .member_eql, .member_equal => &type_mod.t_any,
            .assoc, .assoc_eql, .assoc_equal => &type_mod.t_any,
            .find, .find_eq, .find_equal => &type_mod.t_any,
            .position, .position_eq, .position_equal => &type_mod.t_any,
            .count, .count_eq, .count_equal => &type_mod.t_fixnum,
            .remove, .remove_eq, .remove_equal => &type_mod.t_any,
            .rplaca, .rplacd => &type_mod.t_cons,

            // Type predicates: any -> bool (nil or t)
            .consp, .symbolp, .stringp, .vectorp, .numberp, .integerp, .realp, .closurep, .keywordp, .nilp, .characterp, .floatp, .listp, .atom, .hashtablep, .rationalp, .complexp => &type_mod.t_any,

            // Complex number operations
            .make_complex => &type_mod.t_any, // returns complex
            .real_part, .imag_part => &type_mod.t_float,

            // Numeric predicates
            .zerop, .plusp, .minusp, .evenp, .oddp => &type_mod.t_any,
            .abs => &type_mod.t_fixnum,

            // Logic
            .not => &type_mod.t_any,

            // Function application: infer function type, check argument
            .call => |c| self.inferApplication(c.func, c.args, ctx),

            // If expressions: infer both branches, join types
            .@"if" => |i| self.inferIf(i.cond, i.then_branch, i.else_branch, ctx),

            // Let bindings
            .let => |l| self.inferLet(l.bindings, l.body, ctx),

            // Lambda without annotation - can't infer, need type annotation
            .lambda => {
                try self.reportError("Cannot infer type of unannotated lambda", "lambda");
                return error.TypeError;
            },

            // Type assertions: narrow the type
            .assert_fixnum => &type_mod.t_fixnum,
            .assert_cons => &type_mod.t_cons,
            .assert_symbol => &type_mod.t_symbol,
            .assert_string => &type_mod.t_string,
            .assert_vector => &type_mod.t_vector,
            .assert_closure => &type_mod.t_closure,
            .assert_non_nil => &type_mod.t_non_nil,
            .assert_list => &type_mod.t_any, // Could be nil or cons
            .assert_or => &type_mod.t_any, // Could be any of the union types

            // String operations
            .str_ref => &type_mod.t_char,
            .str_len => &type_mod.t_fixnum,
            .str_concat, .str_eq => &type_mod.t_any,
            .string_upcase, .string_downcase => &type_mod.t_string,
            .list_to_string => &type_mod.t_string,
            .make_string => &type_mod.t_string,

            // Vector operations
            .vec_ref => &type_mod.t_any,
            .vec_set => &type_mod.t_any,
            .vec_len => &type_mod.t_fixnum,
            .vec_new, .vec => &type_mod.t_vector,

            // Character operations
            .char_code => &type_mod.t_fixnum,
            .code_char => &type_mod.t_char,
            .char_eq, .char_lt, .char_gt => &type_mod.t_any,
            .char_upcase, .char_downcase => &type_mod.t_char,
            .digit_char_p, .alpha_char_p => &type_mod.t_any,

            // Bitwise operations
            .logand, .logior, .logxor, .ash => &type_mod.t_fixnum,
            .lognot => &type_mod.t_fixnum,

            // Symbol operations
            .intern => &type_mod.t_symbol,
            .sym_name => &type_mod.t_string,
            .boundp, .fboundp => &type_mod.t_any,
            .symbol_value, .symbol_function => &type_mod.t_any,

            // Hash table operations
            .make_hash => &type_mod.t_any, // Would be hashtable type
            .hash_get, .hash_set, .hash_rem => &type_mod.t_any,
            .hash_count => &type_mod.t_fixnum,

            // I/O
            .print, .princ, .write_char => &type_mod.t_any,
            .read_from_string => &type_mod.t_any,
            .write_to_string => &type_mod.t_string,
            .read_file => &type_mod.t_string,
            .write_file => &type_mod.t_any,
            .unread_char => &type_mod.t_any,

            // Other
            .type_of => &type_mod.t_symbol,
            .typep => &type_mod.t_any,
            .random => &type_mod.t_fixnum,
            .eval, .macroexpand, .macroexpand_1, .load => &type_mod.t_any,
            .error_user => &type_mod.t_any, // Never returns normally
            .parse_integer => &type_mod.t_fixnum,

            // Box operations
            .make_box => &type_mod.t_any,
            .box_ref => &type_mod.t_any,
            .box_set => &type_mod.t_any,

            // Other IR nodes - fall back to any
            else => &type_mod.t_any,
        };
    }

    /// Infer type of a literal value
    fn inferLiteral(self: *BiChecker, val: u64) *const Type {
        _ = self;
        // Check tag bits
        if ((val & 1) == 1) {
            return &type_mod.t_fixnum;
        }
        if (val == 0) {
            return &type_mod.t_nil;
        }
        // Check pointer tags
        const tag = (val >> 1) & 0x7;
        return switch (tag) {
            0 => &type_mod.t_cons,
            1 => &type_mod.t_symbol,
            2 => &type_mod.t_vector,
            3 => &type_mod.t_string,
            4 => &type_mod.t_closure,
            5 => &type_mod.t_keyword,
            else => &type_mod.t_any,
        };
    }

    /// Infer type of car (first projection)
    /// For Sigma types, returns the first type
    fn inferCar(self: *BiChecker, operand: *const Ir, ctx: *const TypingCtx) CheckError!*const Type {
        const pair_ty = try self.infer(operand, ctx);

        return switch (pair_ty.*) {
            // Sigma type: return first type
            .sigma => |s| s.first_type,

            // List type: return element type
            .list => |elem| elem,

            // Cons/any: return any
            .primitive => |p| if (p == .cons) &type_mod.t_any else blk: {
                try self.reportError("car requires cons or list type", pair_ty.name());
                break :blk error.TypeError;
            },

            .any => &type_mod.t_any,

            else => {
                try self.reportError("car requires cons or list type", pair_ty.name());
                return error.TypeError;
            },
        };
    }

    /// Infer type of cdr (second projection)
    /// For Sigma types, returns the second type (with substitution)
    fn inferCdr(self: *BiChecker, operand: *const Ir, ctx: *const TypingCtx) CheckError!*const Type {
        const pair_ty = try self.infer(operand, ctx);

        return switch (pair_ty.*) {
            // Sigma type: return second type
            // Full implementation would substitute car into second type
            .sigma => |s| s.second_type,

            // List type: return list type (cdr of list is list)
            .list => pair_ty,

            // Cons/any: return any
            .primitive => |p| if (p == .cons) &type_mod.t_any else blk: {
                try self.reportError("cdr requires cons or list type", pair_ty.name());
                break :blk error.TypeError;
            },

            .any => &type_mod.t_any,

            else => {
                try self.reportError("cdr requires cons or list type", pair_ty.name());
                return error.TypeError;
            },
        };
    }

    /// Infer type of function application
    fn inferApplication(
        self: *BiChecker,
        func: *const Ir,
        args: []const *const Ir,
        ctx: *const TypingCtx,
    ) CheckError!*const Type {
        const func_ty = try self.infer(func, ctx);

        return switch (func_ty.*) {
            // Non-dependent function type
            .arrow => |a| {
                // Check argument count
                if (args.len != a.domain.len) {
                    try self.reportError("Wrong number of arguments", "application");
                    return error.TypeError;
                }
                // Check each argument against domain type
                for (args, a.domain) |arg, dom_ty| {
                    try self.check(arg, dom_ty, ctx);
                }
                return a.range;
            },

            // Dependent function type (Pi)
            .pi => |p| {
                if (args.len < 1) {
                    try self.reportError("Pi type expects at least one argument", "application");
                    return error.TypeError;
                }
                // Check first argument against parameter type
                try self.check(args[0], p.param_type, ctx);

                // Substitute argument into return type: B[x := a]
                const arg_term = try self.irToTerm(args[0]);
                const result_ty = try self.substituteInType(p.body, p.param_name, arg_term);

                // Handle remaining arguments if this is a curried function
                if (args.len > 1) {
                    // Apply remaining args to result type
                    if (result_ty.isPi()) {
                        // Recursively apply - create a call IR node for remaining args
                        // For simplicity, just return the result type for now
                        return result_ty;
                    }
                }
                return result_ty;
            },

            // Any type - allow any application
            .any => &type_mod.t_any,

            // Not a function type
            else => {
                try self.reportError("Cannot apply non-function", func_ty.name());
                return error.TypeError;
            },
        };
    }

    /// Infer type of if expression
    fn inferIf(
        self: *BiChecker,
        cond: *const Ir,
        then_branch: *const Ir,
        else_branch: *const Ir,
        ctx: *const TypingCtx,
    ) CheckError!*const Type {
        // Condition can be any type (nil = false, else = true)
        _ = try self.infer(cond, ctx);

        // Infer both branches
        const then_ty = try self.infer(then_branch, ctx);
        const else_ty = try self.infer(else_branch, ctx);

        // If types are equal, return that type
        if (try self.converter.convertible(then_ty, else_ty)) return then_ty;

        // Otherwise, return union type
        const types_arr = [_]*const Type{ then_ty, else_ty };
        return try self.type_builder.makeOr(&types_arr);
    }

    /// Infer type of let binding
    fn inferLet(
        self: *BiChecker,
        bindings: []const Ir.Binding,
        body: *const Ir,
        ctx: *const TypingCtx,
    ) CheckError!*const Type {
        // Create extended context with bindings
        var new_ctx = TypingCtx.initWithParent(self.allocator, ctx);
        defer new_ctx.deinit();

        for (bindings) |binding| {
            const init_ty = try self.infer(binding.value, ctx);
            try new_ctx.bind(binding.name, init_ty, .many);
        }

        return self.infer(body, &new_ctx);
    }

    // ========================================================================
    // Checking Mode: Γ ⊢ e ⇐ A
    // ========================================================================

    /// Check that an expression has the expected type
    pub fn check(self: *BiChecker, expr: *const Ir, expected: *const Type, ctx: *const TypingCtx) CheckError!void {
        switch (expr.*) {
            // Lambda: check against Pi type
            .lambda => |lam| {
                switch (expected.*) {
                    .pi => |p| {
                        // Extend context with parameter
                        var new_ctx = TypingCtx.initWithParent(self.allocator, ctx);
                        defer new_ctx.deinit();

                        // Use the Pi's parameter name or lambda's if available
                        const param_name = if (lam.params.len > 0) lam.params[0] else p.param_name;
                        try new_ctx.bind(param_name, p.param_type, p.quantity);

                        // Check body against return type
                        try self.check(lam.body, p.body, &new_ctx);

                        // Check linearity if required
                        if (p.quantity == .one) {
                            if (new_ctx.checkLinearity()) |unused| {
                                try self.reportError("Linear variable not used exactly once", unused);
                                return error.TypeError;
                            }
                        }
                    },
                    .arrow => |a| {
                        // Non-dependent function type
                        if (lam.params.len != a.domain.len) {
                            try self.reportError("Lambda arity mismatch", "lambda");
                            return error.TypeError;
                        }

                        var new_ctx = TypingCtx.initWithParent(self.allocator, ctx);
                        defer new_ctx.deinit();

                        for (lam.params, a.domain) |param, dom_ty| {
                            try new_ctx.bind(param, dom_ty, .many);
                        }

                        try self.check(lam.body, a.range, &new_ctx);
                    },
                    .any => {
                        // Any accepts anything
                    },
                    else => {
                        try self.reportError("Lambda expected function type", expected.name());
                        return error.TypeError;
                    },
                }
            },

            // Pair: check against Sigma type
            .cons => |c| {
                switch (expected.*) {
                    .sigma => |s| {
                        // Check first element
                        try self.check(c.left, s.first_type, ctx);
                        // Check second element (would need substitution for full dependent types)
                        try self.check(c.right, s.second_type, ctx);
                    },
                    .primitive => |p| {
                        if (p == .cons) {
                            // Just a cons, no type checking needed
                        } else {
                            try self.reportError("Cons expected cons or sigma type", expected.name());
                            return error.TypeError;
                        }
                    },
                    .any => {},
                    else => {
                        try self.reportError("Cons expected cons or sigma type", expected.name());
                        return error.TypeError;
                    },
                }
            },

            // If: check both branches against expected type
            .@"if" => |i| {
                _ = try self.infer(i.cond, ctx);
                try self.check(i.then_branch, expected, ctx);
                try self.check(i.else_branch, expected, ctx);
            },

            // Let: check body against expected type
            .let => |l| {
                var new_ctx = TypingCtx.initWithParent(self.allocator, ctx);
                defer new_ctx.deinit();

                for (l.bindings) |binding| {
                    const init_ty = try self.infer(binding.value, ctx);
                    try new_ctx.bind(binding.name, init_ty, .many);
                }

                try self.check(l.body, expected, &new_ctx);
            },

            // Default: infer and check subtyping (subsumption rule)
            else => {
                const inferred = try self.infer(expr, ctx);

                // Check if inferred type is subtype of expected
                const is_subtype = try self.converter.isSubtype(inferred, expected);
                if (!is_subtype) {
                    try self.reportMismatch("Type mismatch", expected, inferred);
                    return error.TypeError;
                }
            },
        }
    }

    // ========================================================================
    // Type Substitution (for dependent types)
    // ========================================================================

    /// Substitute a term for a variable in a type
    /// Implements: B[x := t] - replace x with t in B
    pub fn substituteInType(
        self: *BiChecker,
        ty: *const Type,
        var_name: []const u8,
        replacement: *const Term,
    ) !*const Type {
        return switch (ty.*) {
            // Primitive types don't contain variables
            .primitive, .integer, .rational_range, .float_range, .any, .type_level, .satisfies, .member, .type_eql => ty,

            // Type variable - check if it matches
            .type_var => |name| {
                if (std.mem.eql(u8, name, var_name)) {
                    // This variable is being substituted
                    // For now, if the replacement is a type term, we'd need to eval it
                    // Could use normalizer to evaluate replacement and convert to type
                    // For now, check if replacement is a literal we can handle
                    switch (replacement.*) {
                        .literal => |val| {
                            // Infer type from literal value
                            if ((val & 1) == 1) return &type_mod.t_fixnum;
                            if (val == 0) return &type_mod.t_nil;
                            return &type_mod.t_any;
                        },
                        else => return &type_mod.t_any,
                    }
                }
                return ty;
            },

            // List type - substitute in element type
            .list => |elem| blk: {
                const new_elem = try self.substituteInType(elem, var_name, replacement);
                if (new_elem == elem) break :blk ty;
                break :blk try self.type_builder.makeList(new_elem);
            },

            // Vector type
            .vec => |v| blk: {
                const new_elem = try self.substituteInType(v.elem, var_name, replacement);
                if (new_elem == v.elem) break :blk ty;
                break :blk try self.type_builder.makeVec(new_elem, v.dim);
            },

            // Non-nil type
            .non_nil => |inner| blk: {
                const new_inner = try self.substituteInType(inner, var_name, replacement);
                if (new_inner == inner) break :blk ty;
                break :blk try self.type_builder.makeNonNil(new_inner);
            },

            // Arrow type - substitute in domain and range
            .arrow => |a| blk: {
                var changed = false;
                const new_domain = try self.allocator.alloc(*const Type, a.domain.len);
                for (a.domain, 0..) |dom, i| {
                    new_domain[i] = try self.substituteInType(dom, var_name, replacement);
                    if (new_domain[i] != dom) changed = true;
                }
                const new_range = try self.substituteInType(a.range, var_name, replacement);
                if (new_range != a.range) changed = true;

                if (!changed) {
                    self.allocator.free(new_domain);
                    break :blk ty;
                }
                break :blk try self.type_builder.makeArrow(new_domain, new_range);
            },

            // Pi type - substitute, but shadow if same var name
            .pi => |p| blk: {
                // Check for variable capture
                if (std.mem.eql(u8, p.param_name, var_name)) {
                    // The variable is shadowed, don't substitute in body
                    break :blk ty;
                }

                const new_param = try self.substituteInType(p.param_type, var_name, replacement);
                const new_body = try self.substituteInType(p.body, var_name, replacement);

                if (new_param == p.param_type and new_body == p.body) break :blk ty;
                break :blk try self.type_builder.makePi(p.param_name, new_param, new_body, p.quantity);
            },

            // Sigma type
            .sigma => |s| blk: {
                // Check for variable capture
                if (std.mem.eql(u8, s.first_name, var_name)) {
                    break :blk ty;
                }

                const new_first = try self.substituteInType(s.first_type, var_name, replacement);
                const new_second = try self.substituteInType(s.second_type, var_name, replacement);

                if (new_first == s.first_type and new_second == s.second_type) break :blk ty;
                break :blk try self.type_builder.makeSigma(s.first_name, new_first, new_second);
            },

            // Union types
            .@"or" => |types| blk: {
                var changed = false;
                const new_types = try self.allocator.alloc(*const Type, types.len);
                for (types, 0..) |t, i| {
                    new_types[i] = try self.substituteInType(t, var_name, replacement);
                    if (new_types[i] != t) changed = true;
                }

                if (!changed) {
                    self.allocator.free(new_types);
                    break :blk ty;
                }
                break :blk try self.type_builder.makeOr(new_types);
            },

            // Intersection types
            .@"and" => |types| blk: {
                var changed = false;
                const new_types = try self.allocator.alloc(*const Type, types.len);
                for (types, 0..) |t, i| {
                    new_types[i] = try self.substituteInType(t, var_name, replacement);
                    if (new_types[i] != t) changed = true;
                }

                if (!changed) {
                    self.allocator.free(new_types);
                    break :blk ty;
                }
                break :blk try self.type_builder.makeAnd(new_types);
            },

            // Negation type
            .not => |inner| blk: {
                const new_inner = try self.substituteInType(inner, var_name, replacement);
                if (new_inner == inner) break :blk ty;
                break :blk try self.type_builder.makeNot(new_inner);
            },

            // Refinement type
            .refinement => |r| blk: {
                // Check for variable capture
                if (std.mem.eql(u8, r.predicate_var, var_name)) {
                    break :blk ty;
                }

                const new_base = try self.substituteInType(r.base_type, var_name, replacement);
                const new_predicate = if (r.predicate) |pred| blk2: {
                    const old_term: *const Term = @ptrCast(@alignCast(pred));
                    const new_term = try self.term_builder.substitute(old_term, var_name, replacement);
                    break :blk2 @as(?*const anyopaque, @ptrCast(new_term));
                } else null;

                if (new_base == r.base_type and new_predicate == r.predicate) break :blk ty;
                break :blk try self.type_builder.makeRefinement(new_base, r.predicate_var, new_predicate);
            },

            // Type application - would need term substitution
            .type_app, .@"struct" => ty,
        };
    }

    /// Convert an IR expression to a Term for type-level computation
    /// Used when applying dependent functions - the argument becomes a type-level term
    fn irToTerm(self: *BiChecker, expr: *const Ir) !*const Term {
        return switch (expr.*) {
            .lit => |val| try self.term_builder.literal(val.raw),
            .@"var" => |v| try self.term_builder.varByName(v.name),
            else => {
                // For complex expressions, create a neutral term
                // Full implementation would recursively convert
                return try self.term_builder.varByName("_expr_");
            },
        };
    }

    // ========================================================================
    // Type-level expression evaluation
    // ========================================================================

    /// Evaluate a type expression to a Type
    fn evalTypeExpr(self: *BiChecker, type_ir: *const Ir, ctx: *const TypingCtx) CheckError!*const Type {
        _ = ctx;
        // For now, just handle basic type references
        // Full implementation would evaluate type-level terms
        switch (type_ir.*) {
            .@"var" => |v| {
                // Table-driven type lookup
                const TypeEntry = struct { name: []const u8, ty: *const Type };
                const type_table: []const TypeEntry = &.{
                    .{ .name = "fixnum", .ty = &type_mod.t_fixnum },
                    .{ .name = "string", .ty = &type_mod.t_string },
                    .{ .name = "symbol", .ty = &type_mod.t_symbol },
                    .{ .name = "cons", .ty = &type_mod.t_cons },
                    .{ .name = "nil", .ty = &type_mod.t_nil },
                    .{ .name = "any", .ty = &type_mod.t_any },
                    .{ .name = "Type", .ty = &type_mod.t_type },
                };
                for (type_table) |e| {
                    if (std.mem.eql(u8, v.name, e.name)) return e.ty;
                }

                try self.reportError("Unknown type", v.name);
                return error.TypeError;
            },
            else => {
                // For complex type expressions, would need full evaluation
                return &type_mod.t_any;
            },
        }
    }

    // ========================================================================
    // Error reporting
    // ========================================================================

    fn reportError(self: *BiChecker, message: []const u8, context: []const u8) !void {
        try self.errors.append(self.allocator, .{
            .message = message,
            .context = context,
            .expected = null,
            .got = null,
        });
    }

    fn reportMismatch(self: *BiChecker, message: []const u8, expected: *const Type, got: *const Type) !void {
        try self.errors.append(self.allocator, .{
            .message = message,
            .context = "",
            .expected = expected,
            .got = got,
        });
    }

    pub fn hasErrors(self: *const BiChecker) bool {
        return self.errors.items.len > 0;
    }
};

/// Type checking error
pub const TypeError = struct {
    message: []const u8,
    context: []const u8,
    expected: ?*const Type,
    got: ?*const Type,
};

pub const CheckError = error{
    TypeError,
    DivisionByZero,
    FuelExhausted,
    OutOfMemory,
};

// ============================================================================
// Tests
// ============================================================================

test "infer literal types" {
    const testing = std.testing;
    const test_alloc = testing.allocator;
    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(test_alloc, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(test_alloc, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(test_alloc, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(test_alloc);
    defer ctx.deinit();

    // Create a fixnum literal IR node
    const ir_alloc = test_alloc;
    const lit_node = try ir_alloc.create(Ir);
    defer ir_alloc.destroy(lit_node);
    lit_node.* = .{ .lit = Value.makeFixnum(21) }; // Creates tagged fixnum

    const ty = try checker.infer(lit_node, &ctx);
    try testing.expect(ty.* == .primitive);
    try testing.expectEqual(type_mod.Primitive.fixnum, ty.primitive);
}

test "context binding and lookup" {
    const testing = std.testing;
    const test_alloc = testing.allocator;

    var ctx = TypingCtx.init(test_alloc);
    defer ctx.deinit();

    try ctx.bind("x", &type_mod.t_fixnum, .many);
    try ctx.bind("y", &type_mod.t_string, .one);

    try testing.expect(ctx.lookup("x") != null);
    try testing.expectEqual(&type_mod.t_fixnum, ctx.lookup("x").?);
    try testing.expectEqual(&type_mod.t_string, ctx.lookup("y").?);
    try testing.expect(ctx.lookup("z") == null);
}

test "nested context" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var parent = TypingCtx.init(allocator);
    defer parent.deinit();
    try parent.bind("x", &type_mod.t_fixnum, .many);

    var child = TypingCtx.initWithParent(allocator, &parent);
    defer child.deinit();
    try child.bind("y", &type_mod.t_string, .many);

    // Child can see parent's bindings
    try testing.expectEqual(&type_mod.t_fixnum, child.lookup("x").?);
    try testing.expectEqual(&type_mod.t_string, child.lookup("y").?);

    // Parent can't see child's bindings
    try testing.expect(parent.lookup("y") == null);
}

test "infer variable" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();
    try ctx.bind("x", &type_mod.t_fixnum, .many);

    // Create var IR node
    const var_node = try allocator.create(Ir);
    defer allocator.destroy(var_node);
    var_node.* = .{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };

    const ty = try checker.infer(var_node, &ctx);
    try testing.expectEqual(&type_mod.t_fixnum, ty);
}

test "check lambda against pi type" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Create Pi type: (Π (x : fixnum) fixnum)
    const pi_ty = try checker.type_builder.makePi("x", &type_mod.t_fixnum, &type_mod.t_fixnum, .many);

    // Create lambda: (lambda (x) x)
    const body = try allocator.create(Ir);
    body.* = .{ .@"var" = .{ .name = "x", .depth = 0, .index = 0 } };

    const lambda_node = try allocator.create(Ir);
    const params = try allocator.alloc([]const u8, 1);
    params[0] = "x";
    lambda_node.* = .{
        .lambda = .{
            .params = params,
            .optional_params = &.{},
            .key_params = &.{},
            .rest_param = null,
            .captures = &.{},
            .body = body,
        },
    };

    // Check lambda against Pi type - should succeed
    try checker.check(lambda_node, pi_ty, &ctx);
    try testing.expect(!checker.hasErrors());
}

test "substitution in type" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    // Create a type variable
    const ty_var = try checker.type_builder.makeTypeVar("n");

    // Create a term to substitute (a literal)
    const term_val = try checker.term_builder.fixnum(42);

    // Substitute - should return a type based on the literal
    const result = try checker.substituteInType(ty_var, "n", term_val);

    // Since 42 is a fixnum, result should be fixnum
    try testing.expectEqual(&type_mod.t_fixnum, result);
}

test "substitution preserves unchanged types" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    // Substituting in a primitive type should return the same type
    const term_val = try checker.term_builder.fixnum(42);
    const result = try checker.substituteInType(&type_mod.t_fixnum, "x", term_val);
    try testing.expectEqual(&type_mod.t_fixnum, result);
}

test "substitution with shadowing" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    // Create: (Π (x : fixnum) x) where inner x shadows
    const inner_var = try checker.type_builder.makeTypeVar("x");
    const pi_ty = try checker.type_builder.makePi("x", &type_mod.t_fixnum, inner_var, .many);

    // Substitute x in pi type - should not substitute in body (shadowed)
    const term_val = try checker.term_builder.fixnum(42);
    const result = try checker.substituteInType(pi_ty, "x", term_val);

    // Result should be unchanged (shadowing prevents substitution)
    try testing.expectEqual(pi_ty, result);
}

test "check cons against sigma type" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Create Sigma type: (Σ (n : fixnum) fixnum)
    const sigma_ty = try checker.type_builder.makeSigma("n", &type_mod.t_fixnum, &type_mod.t_fixnum);

    // Create cons IR: (cons 42 100)
    const left = try allocator.create(Ir);
    left.* = .{ .lit = Value.makeFixnum(42) };

    const right = try allocator.create(Ir);
    right.* = .{ .lit = Value.makeFixnum(100) };

    const cons_node = try allocator.create(Ir);
    cons_node.* = .{ .cons = .{ .left = left, .right = right } };

    // Check cons against Sigma type - should succeed
    try checker.check(cons_node, sigma_ty, &ctx);
    try testing.expect(!checker.hasErrors());
}

test "infer car from sigma type" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Add a variable with Sigma type: (Σ (n : fixnum) string)
    const sigma_ty = try checker.type_builder.makeSigma("n", &type_mod.t_fixnum, &type_mod.t_string);
    try ctx.bind("pair", sigma_ty, .many);

    // Create: (car pair)
    const pair_var = try allocator.create(Ir);
    pair_var.* = .{ .@"var" = .{ .name = "pair", .depth = 0, .index = 0 } };

    const car_node = try allocator.create(Ir);
    car_node.* = .{ .car = .{ .operand = pair_var } };

    // Infer type of (car pair) - should be fixnum (first type of sigma)
    const ty = try checker.infer(car_node, &ctx);
    try testing.expectEqual(&type_mod.t_fixnum, ty);
}

test "infer cdr from sigma type" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Add a variable with Sigma type: (Σ (n : fixnum) string)
    const sigma_ty = try checker.type_builder.makeSigma("n", &type_mod.t_fixnum, &type_mod.t_string);
    try ctx.bind("pair", sigma_ty, .many);

    // Create: (cdr pair)
    const pair_var = try allocator.create(Ir);
    pair_var.* = .{ .@"var" = .{ .name = "pair", .depth = 0, .index = 0 } };

    const cdr_node = try allocator.create(Ir);
    cdr_node.* = .{ .cdr = .{ .operand = pair_var } };

    // Infer type of (cdr pair) - should be string (second type of sigma)
    const ty = try checker.infer(cdr_node, &ctx);
    try testing.expectEqual(&type_mod.t_string, ty);
}

test "infer car from list type" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Add a variable with list type: (list fixnum)
    const list_ty = try checker.type_builder.makeList(&type_mod.t_fixnum);
    try ctx.bind("xs", list_ty, .many);

    // Create: (car xs)
    const xs_var = try allocator.create(Ir);
    xs_var.* = .{ .@"var" = .{ .name = "xs", .depth = 0, .index = 0 } };

    const car_node = try allocator.create(Ir);
    car_node.* = .{ .car = .{ .operand = xs_var } };

    // Infer type of (car xs) - should be fixnum (element type)
    const ty = try checker.infer(car_node, &ctx);
    try testing.expectEqual(&type_mod.t_fixnum, ty);
}

test "zero-quantity variable cannot be used at runtime" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Bind a zero-quantity (erased) variable
    try ctx.bind("proof", &type_mod.t_fixnum, .zero);

    // Create var IR node that tries to use the erased variable
    const var_node = try allocator.create(Ir);
    var_node.* = .{ .@"var" = .{ .name = "proof", .depth = 0, .index = 0 } };

    // Inferring should fail because zero-quantity variables can't be used at runtime
    const result = checker.infer(var_node, &ctx);
    try testing.expectError(error.TypeError, result);
    try testing.expect(checker.hasErrors());
}

test "one-quantity variable in linear context" {
    const testing = std.testing;

    var arena = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;
    var heap = try Heap.init(allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();
    var checker = BiChecker.init(allocator, &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(allocator);
    defer ctx.deinit();

    // Bind a linear (one-quantity) variable
    try ctx.bind("handle", &type_mod.t_any, .one);

    // Accessing it once should succeed
    const var_node = try allocator.create(Ir);
    var_node.* = .{ .@"var" = .{ .name = "handle", .depth = 0, .index = 0 } };

    const ty = try checker.infer(var_node, &ctx);
    try testing.expectEqual(&type_mod.t_any, ty);
}

test "infer if propagates allocation failure" {
    const testing = std.testing;
    const test_alloc = testing.allocator;
    const Vm = @import("../interp/vm.zig").Vm;
    const Heap = @import("../runtime/heap.zig").Heap;

    var heap = try Heap.init(test_alloc, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();
    var vm = try Vm.init(test_alloc, &heap);
    defer vm.deinit();

    const FailAlloc = struct {
        const Self = @This();

        fn alloc(_: *anyopaque, _: usize, _: std.mem.Alignment, _: usize) ?[*]u8 {
            return null;
        }
        fn resize(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) bool {
            return false;
        }
        fn remap(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize, _: usize) ?[*]u8 {
            return null;
        }
        fn free(_: *anyopaque, _: []u8, _: std.mem.Alignment, _: usize) void {}

        const vtable = std.mem.Allocator.VTable{
            .alloc = alloc,
            .resize = resize,
            .remap = remap,
            .free = free,
        };

        fn allocator(self: *Self) std.mem.Allocator {
            return .{ .ptr = self, .vtable = &vtable };
        }
    };

    var failing = FailAlloc{};
    var checker = BiChecker.init(failing.allocator(), &vm.builtins);
    defer checker.deinit();

    var ctx = TypingCtx.init(test_alloc);
    defer ctx.deinit();

    const cond = try test_alloc.create(Ir);
    defer test_alloc.destroy(cond);
    cond.* = .{ .lit = Value.t };

    const then_node = try test_alloc.create(Ir);
    defer test_alloc.destroy(then_node);
    then_node.* = .{ .lit = Value.makeFixnum(1) };

    const str_val = try heap.allocBaseString("x");
    const else_node = try test_alloc.create(Ir);
    defer test_alloc.destroy(else_node);
    else_node.* = .{ .lit = str_val };

    const if_node = try test_alloc.create(Ir);
    defer test_alloc.destroy(if_node);
    if_node.* = .{ .@"if" = .{ .cond = cond, .then_branch = then_node, .else_branch = else_node } };

    try testing.expectError(error.OutOfMemory, checker.infer(if_node, &ctx));
}
