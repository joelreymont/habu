//! Type inference for Habu's gradual type system
//!
//! Hindley-Milner style inference with extensions:
//! - Type variables for unknowns
//! - Equality constraints from expression usage
//! - Union-find based unification
//! - Occurs check for infinite type prevention

const std = @import("std");
const types = @import("type.zig");
const ir_mod = @import("../compiler/ir.zig");
const value_mod = @import("../runtime/value.zig");

/// A type variable representing an unknown type
pub const TypeVar = struct {
    id: u32,

    pub fn format(
        self: TypeVar,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try writer.print("?T{d}", .{self.id});
    }
};

/// Extended type that includes type variables for inference
pub const InferType = union(enum) {
    /// Concrete type (from type.zig)
    concrete: *const types.Type,

    /// Type variable (unknown, to be solved)
    variable: TypeVar,

    /// Function type with possibly-variable components
    arrow: struct {
        domain: []const *const InferType,
        range: *const InferType,
    },

    /// List with element type
    list: *const InferType,

    pub fn isVariable(self: InferType) bool {
        return self == .variable;
    }

    pub fn format(
        self: InferType,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self) {
            .concrete => |t| try writer.print("{s}", .{t.name()}),
            .variable => |v| try writer.print("{}", .{v}),
            .arrow => |a| {
                try writer.writeAll("(-> (");
                for (a.domain, 0..) |d, i| {
                    if (i > 0) try writer.writeAll(" ");
                    try writer.print("{}", .{d.*});
                }
                try writer.print(") {})", .{a.range.*});
            },
            .list => |elem| try writer.print("(list {})", .{elem.*}),
        }
    }
};

/// Type scheme for let-polymorphism: ∀a1,a2,...an. T
/// Represents a polymorphic type where bound vars can be instantiated
pub const TypeScheme = struct {
    /// IDs of bound type variables (quantified)
    bound_vars: []const u32,
    /// The body type (may reference bound vars)
    body: *const InferType,

    pub fn format(
        self: TypeScheme,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        if (self.bound_vars.len > 0) {
            try writer.writeAll("∀");
            for (self.bound_vars, 0..) |v, i| {
                if (i > 0) try writer.writeAll(",");
                try writer.print("T{d}", .{v});
            }
            try writer.writeAll(". ");
        }
        try writer.print("{}", .{self.body.*});
    }

    /// Check if this is a monomorphic type (no bound variables)
    pub fn isMono(self: TypeScheme) bool {
        return self.bound_vars.len == 0;
    }
};

/// Constraint between two types that must be satisfied
pub const Constraint = union(enum) {
    /// Types must be equal: T1 = T2
    eq: struct {
        left: *const InferType,
        right: *const InferType,
    },

    /// Subtype relation: T1 <: T2 (T1 can be used where T2 expected)
    subtype: struct {
        sub: *const InferType,
        super: *const InferType,
    },
};

/// Inference context for type inference and constraint solving
pub const InferCtx = struct {
    allocator: std.mem.Allocator,

    /// Counter for fresh type variable generation
    next_var_id: u32,

    /// Substitution map: TypeVar id -> InferType
    /// Represents the current solution
    substitutions: std.AutoHashMap(u32, *const InferType),

    /// Collected constraints to solve
    constraints: std.ArrayList(Constraint),

    pub fn init(allocator: std.mem.Allocator) InferCtx {
        return .{
            .allocator = allocator,
            .next_var_id = 0,
            .substitutions = std.AutoHashMap(u32, *const InferType).init(allocator),
            .constraints = std.ArrayList(Constraint){},
        };
    }

    pub fn deinit(self: *InferCtx) void {
        self.substitutions.deinit();
        self.constraints.deinit(self.allocator);
    }

    /// Generate a fresh type variable
    pub fn freshVar(self: *InferCtx) !*InferType {
        const id = self.next_var_id;
        self.next_var_id += 1;

        const t = try self.allocator.create(InferType);
        t.* = .{ .variable = .{ .id = id } };
        return t;
    }

    /// Wrap a concrete type in InferType
    pub fn concrete(self: *InferCtx, t: *const types.Type) !*InferType {
        const it = try self.allocator.create(InferType);
        it.* = .{ .concrete = t };
        return it;
    }

    /// Add an equality constraint
    pub fn addEq(self: *InferCtx, left: *const InferType, right: *const InferType) !void {
        try self.constraints.append(self.allocator, .{
            .eq = .{ .left = left, .right = right },
        });
    }

    /// Add a subtype constraint
    pub fn addSubtype(self: *InferCtx, sub: *const InferType, super: *const InferType) !void {
        try self.constraints.append(self.allocator, .{
            .subtype = .{ .sub = sub, .super = super },
        });
    }

    /// Look up current substitution for a type variable
    pub fn lookup(self: *const InferCtx, id: u32) ?*const InferType {
        return self.substitutions.get(id);
    }

    /// Apply substitutions to get the current type
    pub fn resolve(self: *const InferCtx, t: *const InferType) *const InferType {
        switch (t.*) {
            .variable => |v| {
                if (self.lookup(v.id)) |resolved| {
                    return self.resolve(resolved);
                }
                return t;
            },
            else => return t,
        }
    }

    /// Unify two types, updating substitutions
    /// Returns error if types cannot be unified
    pub fn unify(self: *InferCtx, t1: *const InferType, t2: *const InferType) UnifyError!void {
        const r1 = self.resolve(t1);
        const r2 = self.resolve(t2);

        // Same type (pointer equality after resolution)
        if (r1 == r2) return;

        // Variable on left - bind it
        if (r1.* == .variable) {
            const v = r1.variable;
            if (self.occursIn(v, r2)) return error.InfiniteType;
            try self.substitutions.put(v.id, r2);
            return;
        }

        // Variable on right - bind it
        if (r2.* == .variable) {
            const v = r2.variable;
            if (self.occursIn(v, r1)) return error.InfiniteType;
            try self.substitutions.put(v.id, r1);
            return;
        }

        // Both concrete - check structural equality
        if (r1.* == .concrete and r2.* == .concrete) {
            if (!typeEquals(r1.concrete, r2.concrete)) {
                return error.TypeMismatch;
            }
            return;
        }

        // Arrow types - unify components
        if (r1.* == .arrow and r2.* == .arrow) {
            const a1 = r1.arrow;
            const a2 = r2.arrow;

            if (a1.domain.len != a2.domain.len) return error.ArityMismatch;

            for (a1.domain, a2.domain) |d1, d2| {
                try self.unify(d1, d2);
            }
            try self.unify(a1.range, a2.range);
            return;
        }

        // List types - unify element types
        if (r1.* == .list and r2.* == .list) {
            try self.unify(r1.list, r2.list);
            return;
        }

        return error.TypeMismatch;
    }

    /// Check if type variable occurs in a type (prevents infinite types)
    fn occursIn(self: *const InferCtx, v: TypeVar, t: *const InferType) bool {
        const resolved = self.resolve(t);
        switch (resolved.*) {
            .variable => |v2| return v.id == v2.id,
            .concrete => return false,
            .arrow => |a| {
                for (a.domain) |d| {
                    if (self.occursIn(v, d)) return true;
                }
                return self.occursIn(v, a.range);
            },
            .list => |elem| return self.occursIn(v, elem),
        }
    }

    /// Solve all collected constraints
    pub fn solve(self: *InferCtx) UnifyError!void {
        for (self.constraints.items) |c| {
            switch (c) {
                .eq => |eq| try self.unify(eq.left, eq.right),
                .subtype => |sub| {
                    // For now, treat subtype as equality
                    // TODO: proper subtyping with variance
                    try self.unify(sub.sub, sub.super);
                },
            }
        }
    }

    // ========================================================================
    // Let-polymorphism: Generalization and Instantiation
    // ========================================================================

    /// Collect all free type variables in a type
    fn collectFreeVars(self: *const InferCtx, t: *const InferType, free_vars: *std.ArrayList(u32)) !void {
        const resolved = self.resolve(t);
        switch (resolved.*) {
            .variable => |v| {
                // Add if not already present
                for (free_vars.items) |fv| {
                    if (fv == v.id) return;
                }
                try free_vars.append(self.allocator, v.id);
            },
            .concrete => {}, // No type variables in concrete types
            .arrow => |a| {
                for (a.domain) |d| {
                    try self.collectFreeVars(d, free_vars);
                }
                try self.collectFreeVars(a.range, free_vars);
            },
            .list => |elem| {
                try self.collectFreeVars(elem, free_vars);
            },
        }
    }

    /// Collect all free type variables in a type environment
    fn collectEnvFreeVars(self: *const InferCtx, env: *const TypeEnv, free_vars: *std.ArrayList(u32)) !void {
        var it = env.bindings.iterator();
        while (it.next()) |entry| {
            try self.collectFreeVars(entry.value_ptr.*, free_vars);
        }
        if (env.parent) |parent| {
            try self.collectEnvFreeVars(parent, free_vars);
        }
    }

    /// Generalize a type with respect to an environment
    /// Free variables in the type that are NOT free in the env become bound
    /// This implements the Gen rule: Gen(Γ, τ) = ∀(FV(τ) - FV(Γ)). τ
    pub fn generalize(self: *InferCtx, t: *const InferType, env: *const TypeEnv) !TypeScheme {
        // Collect free vars in the type
        var type_vars = std.ArrayList(u32){};
        defer type_vars.deinit(self.allocator);
        try self.collectFreeVars(t, &type_vars);

        // Collect free vars in the environment
        var env_vars = std.ArrayList(u32){};
        defer env_vars.deinit(self.allocator);
        try self.collectEnvFreeVars(env, &env_vars);

        // Find vars free in type but not in env (these get generalized)
        var bound_vars = std.ArrayList(u32){};
        for (type_vars.items) |tv| {
            var in_env = false;
            for (env_vars.items) |ev| {
                if (tv == ev) {
                    in_env = true;
                    break;
                }
            }
            if (!in_env) {
                try bound_vars.append(self.allocator, tv);
            }
        }

        return TypeScheme{
            .bound_vars = try bound_vars.toOwnedSlice(self.allocator),
            .body = t,
        };
    }

    /// Instantiate a type scheme with fresh type variables
    /// This implements the Inst rule: Inst(∀a1...an. τ) = τ[a1 := β1, ..., an := βn]
    pub fn instantiate(self: *InferCtx, scheme: TypeScheme) !*const InferType {
        if (scheme.bound_vars.len == 0) {
            // Monomorphic type - no instantiation needed
            return scheme.body;
        }

        // Create mapping from bound vars to fresh vars
        var var_map = std.AutoHashMap(u32, *const InferType).init(self.allocator);
        defer var_map.deinit();

        for (scheme.bound_vars) |bv| {
            const fresh = try self.freshVar();
            try var_map.put(bv, fresh);
        }

        // Substitute bound vars with fresh vars
        return try self.substituteVars(scheme.body, &var_map);
    }

    /// Substitute type variables according to a mapping
    fn substituteVars(self: *InferCtx, t: *const InferType, var_map: *const std.AutoHashMap(u32, *const InferType)) !*const InferType {
        switch (t.*) {
            .variable => |v| {
                if (var_map.get(v.id)) |replacement| {
                    return replacement;
                }
                return t;
            },
            .concrete => return t,
            .arrow => |a| {
                var new_domain = try self.allocator.alloc(*const InferType, a.domain.len);
                for (a.domain, 0..) |d, i| {
                    new_domain[i] = try self.substituteVars(d, var_map);
                }
                const new_range = try self.substituteVars(a.range, var_map);

                const new_t = try self.allocator.create(InferType);
                new_t.* = .{ .arrow = .{ .domain = new_domain, .range = new_range } };
                return new_t;
            },
            .list => |elem| {
                const new_elem = try self.substituteVars(elem, var_map);
                const new_t = try self.allocator.create(InferType);
                new_t.* = .{ .list = new_elem };
                return new_t;
            },
        }
    }

    // ========================================================================
    // Type Inference - walks IR and generates constraints
    // ========================================================================

    /// Helper to infer function call types
    fn inferCall(self: *InferCtx, func: *const ir_mod.Ir, args: []const *const ir_mod.Ir, env: *TypeEnv) InferError!*const InferType {
        const func_ty = try self.infer(func, env);

        // Infer argument types
        var arg_types = try self.allocator.alloc(*const InferType, args.len);
        for (args, 0..) |arg, i| {
            arg_types[i] = try self.infer(arg, env);
        }

        // Create expected arrow type with fresh result variable
        const result_ty = try self.freshVar();
        const expected_arrow = try self.allocator.create(InferType);
        expected_arrow.* = .{ .arrow = .{ .domain = arg_types, .range = result_ty } };

        // Constrain function type to match
        try self.addEq(func_ty, expected_arrow);

        return result_ty;
    }

    /// Type environment for tracking variable types during inference
    pub const TypeEnv = struct {
        parent: ?*const TypeEnv,
        bindings: std.StringHashMap(*const InferType),
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) TypeEnv {
            return .{
                .parent = null,
                .bindings = std.StringHashMap(*const InferType).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn initWithParent(allocator: std.mem.Allocator, parent: *const TypeEnv) TypeEnv {
            return .{
                .parent = parent,
                .bindings = std.StringHashMap(*const InferType).init(allocator),
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *TypeEnv) void {
            self.bindings.deinit();
        }

        pub fn lookup(self: TypeEnv, name: []const u8) ?*const InferType {
            if (self.bindings.get(name)) |ty| {
                return ty;
            }
            if (self.parent) |p| {
                return p.lookup(name);
            }
            return null;
        }

        pub fn bind(self: *TypeEnv, name: []const u8, ty: *const InferType) !void {
            try self.bindings.put(name, ty);
        }
    };

    /// Infer the type of an IR node, generating constraints as needed
    /// Returns the inferred type (may contain type variables)
    pub fn infer(self: *InferCtx, node: *const ir_mod.Ir, env: *TypeEnv) InferError!*const InferType {
        switch (node.*) {
            // Literals - return concrete types
            .lit => |v| {
                if (v.isFixnum()) return try self.concrete(&types.t_fixnum);
                if (v.isNil()) return try self.concrete(&types.t_nil);
                if (v.isCharacter()) return try self.concrete(&types.t_char);
                // t (true) is represented as fixnum 1
                return try self.concrete(&types.t_any);
            },
            .quote_sym => return try self.concrete(&types.t_symbol),
            .quote => return try self.concrete(&types.t_any),

            // Variables - look up in environment or return fresh var
            .@"var" => |v| {
                if (env.lookup(v.name)) |ty| {
                    return ty;
                }
                // Unknown variable - create fresh type variable
                return try self.freshVar();
            },
            .global_ref => {
                // Globals are dynamically typed - return any
                return try self.concrete(&types.t_any);
            },

            // Arithmetic - operands must be fixnum, result is fixnum
            .add, .sub, .mul, .div, .mod => |op| {
                const left_ty = try self.infer(op.left, env);
                const right_ty = try self.infer(op.right, env);
                const fixnum_ty = try self.concrete(&types.t_fixnum);

                try self.addEq(left_ty, fixnum_ty);
                try self.addEq(right_ty, fixnum_ty);

                return fixnum_ty;
            },

            // Numeric predicates - operand is fixnum, result is boolean (any)
            .zerop, .plusp, .minusp, .evenp, .oddp, .abs => |op| {
                const operand_ty = try self.infer(op.operand, env);
                const fixnum_ty = try self.concrete(&types.t_fixnum);
                try self.addEq(operand_ty, fixnum_ty);
                return try self.concrete(&types.t_any); // CL returns nil or t
            },

            // Comparisons - result is boolean (any in CL)
            .lt, .gt, .le, .ge, .num_eq => |op| {
                const left_ty = try self.infer(op.left, env);
                const right_ty = try self.infer(op.right, env);
                const fixnum_ty = try self.concrete(&types.t_fixnum);

                try self.addEq(left_ty, fixnum_ty);
                try self.addEq(right_ty, fixnum_ty);

                return try self.concrete(&types.t_any);
            },

            // Equality - any types, result is boolean
            .eq, .equal, .eql => |op| {
                _ = try self.infer(op.left, env);
                _ = try self.infer(op.right, env);
                return try self.concrete(&types.t_any);
            },

            // Logic
            .not => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_any);
            },

            // List operations
            .cons => |op| {
                _ = try self.infer(op.left, env);
                _ = try self.infer(op.right, env);
                return try self.concrete(&types.t_cons);
            },
            .car, .cdr => |op| {
                const operand_ty = try self.infer(op.operand, env);
                // Operand should be cons or list
                // For now, just return any (full list typing needs more work)
                _ = operand_ty;
                return try self.concrete(&types.t_any);
            },
            .list => |elems| {
                for (elems) |elem| {
                    _ = try self.infer(elem, env);
                }
                // Return list type (for now, list of any)
                const elem_ty = try self.concrete(&types.t_any);
                const list_ty = try self.allocator.create(InferType);
                list_ty.* = .{ .list = elem_ty };
                return list_ty;
            },
            .append, .nth, .nthcdr, .member, .assoc => |op| {
                _ = try self.infer(op.left, env);
                _ = try self.infer(op.right, env);
                return try self.concrete(&types.t_any);
            },
            .length, .reverse, .last => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_fixnum);
            },

            // Type predicates - return boolean
            .consp, .symbolp, .numberp, .stringp, .vectorp,
            .closurep, .keywordp, .nilp, .characterp, .floatp,
            .listp, .atom => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_any);
            },

            // If expression - both branches should have same type
            .@"if" => |if_expr| {
                _ = try self.infer(if_expr.cond, env);
                const then_ty = try self.infer(if_expr.then_branch, env);
                const else_ty = try self.infer(if_expr.else_branch, env);

                // Create fresh variable and constrain both branches to it
                const result_ty = try self.freshVar();
                try self.addEq(then_ty, result_ty);
                try self.addEq(else_ty, result_ty);

                return result_ty;
            },

            // Progn - type is type of last expression
            .progn => |exprs| {
                if (exprs.len == 0) {
                    return try self.concrete(&types.t_nil);
                }
                var last_ty: *const InferType = undefined;
                for (exprs) |expr| {
                    last_ty = try self.infer(expr, env);
                }
                return last_ty;
            },

            // Let binding
            .let => |let_expr| {
                // Create child environment
                var child_env = TypeEnv.initWithParent(self.allocator, env);
                defer child_env.deinit();

                // Infer types for each binding
                for (let_expr.bindings) |binding| {
                    const val_ty = try self.infer(binding.value, env);
                    try child_env.bind(binding.name, val_ty);
                }

                // Infer body type in extended environment
                return try self.infer(let_expr.body, &child_env);
            },

            // Lambda - create arrow type
            .lambda => |lam| {
                // Create child environment with parameters
                var child_env = TypeEnv.initWithParent(self.allocator, env);
                defer child_env.deinit();

                // Create fresh type variables for each parameter
                var param_types = try self.allocator.alloc(*const InferType, lam.params.len);
                for (lam.params, 0..) |param, i| {
                    param_types[i] = try self.freshVar();
                    try child_env.bind(param, param_types[i]);
                }

                // Handle rest parameter
                if (lam.rest_param) |rest| {
                    const rest_ty = try self.concrete(&types.t_any);
                    try child_env.bind(rest, rest_ty);
                }

                // Infer body type
                const body_ty = try self.infer(lam.body, &child_env);

                // Create arrow type
                const arrow_ty = try self.allocator.create(InferType);
                arrow_ty.* = .{ .arrow = .{ .domain = param_types, .range = body_ty } };

                return arrow_ty;
            },

            // Function call
            .call => |call_expr| {
                return try self.inferCall(call_expr.func, call_expr.args, env);
            },
            .tailcall => |tailcall_expr| {
                return try self.inferCall(tailcall_expr.func, tailcall_expr.args, env);
            },

            // Set - returns value type
            .set => |s| {
                return try self.infer(s.value, env);
            },
            .define => |d| {
                return try self.infer(d.value, env);
            },

            // Loop - returns nil
            .loop => |l| {
                _ = try self.infer(l.cond, env);
                _ = try self.infer(l.body, env);
                return try self.concrete(&types.t_nil);
            },

            // Vector operations
            .vec_new => |v| {
                _ = try self.infer(v.size, env);
                if (v.init) |init_val| {
                    _ = try self.infer(init_val, env);
                }
                return try self.concrete(&types.t_vector);
            },
            .vec => |elems| {
                for (elems) |elem| {
                    _ = try self.infer(elem, env);
                }
                return try self.concrete(&types.t_vector);
            },
            .vec_ref => |op| {
                _ = try self.infer(op.left, env);
                _ = try self.infer(op.right, env);
                return try self.concrete(&types.t_any);
            },
            .vec_set => |v| {
                _ = try self.infer(v.vec, env);
                _ = try self.infer(v.index, env);
                _ = try self.infer(v.value, env);
                return try self.concrete(&types.t_any);
            },
            .vec_len => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_fixnum);
            },

            // String operations
            .str_ref => |op| {
                _ = try self.infer(op.left, env);
                _ = try self.infer(op.right, env);
                return try self.concrete(&types.t_char);
            },
            .str_len => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_fixnum);
            },
            .str_concat, .str_eq => |op| {
                _ = try self.infer(op.left, env);
                _ = try self.infer(op.right, env);
                return try self.concrete(&types.t_string);
            },
            .substring => |s| {
                _ = try self.infer(s.str, env);
                _ = try self.infer(s.start, env);
                _ = try self.infer(s.end, env);
                return try self.concrete(&types.t_string);
            },

            // I/O - return any
            .print, .princ, .write_char => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_any);
            },
            .read_char, .peek_char, .read, .gensym, .terpri => {
                return try self.concrete(&types.t_any);
            },

            // Type assertions - return operand type
            .assert_fixnum => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_fixnum);
            },
            .assert_cons => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_cons);
            },
            .assert_symbol => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_symbol);
            },
            .assert_string => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_string);
            },
            .assert_vector => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_vector);
            },
            .assert_closure => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_closure);
            },
            .assert_non_nil, .assert_list => |op| {
                _ = try self.infer(op.operand, env);
                return try self.concrete(&types.t_any);
            },

            // Everything else returns any for now
            else => return try self.concrete(&types.t_any),
        }
    }
};

pub const UnifyError = error{
    TypeMismatch,
    ArityMismatch,
    InfiniteType,
    OutOfMemory,
};

pub const InferError = error{
    TypeMismatch,
    ArityMismatch,
    InfiniteType,
    OutOfMemory,
};

/// Check if two concrete types are equal
fn typeEquals(t1: *const types.Type, t2: *const types.Type) bool {
    if (t1 == t2) return true;

    // Check structural equality
    switch (t1.*) {
        .primitive => |p1| {
            if (t2.* == .primitive) {
                return p1 == t2.primitive;
            }
            return false;
        },
        .any => return t2.* == .any,
        else => {
            // TODO: structural equality for compound types
            return false;
        },
    }
}

// ============================================================================
// Tests
// ============================================================================

test "fresh type variables" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const v1 = try ctx.freshVar();
    const v2 = try ctx.freshVar();

    try testing.expectEqual(@as(u32, 0), v1.variable.id);
    try testing.expectEqual(@as(u32, 1), v2.variable.id);
}

test "unify variables" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const v1 = try ctx.freshVar();
    const fixnum = try ctx.concrete(&types.t_fixnum);

    try ctx.unify(v1, fixnum);

    const resolved = ctx.resolve(v1);
    try testing.expect(resolved.* == .concrete);
    try testing.expectEqual(types.Primitive.fixnum, resolved.concrete.primitive);
}

test "unify same concrete types" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const f1 = try ctx.concrete(&types.t_fixnum);
    const f2 = try ctx.concrete(&types.t_fixnum);

    try ctx.unify(f1, f2); // Should succeed
}

test "unify different concrete types fails" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const f = try ctx.concrete(&types.t_fixnum);
    const s = try ctx.concrete(&types.t_string);

    try testing.expectError(error.TypeMismatch, ctx.unify(f, s));
}

test "occurs check prevents infinite types" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const v = try ctx.freshVar();

    // Create (list v) where v is a type variable
    const list_v = try ctx.allocator.create(InferType);
    list_v.* = .{ .list = v };

    // Trying to unify v = (list v) should fail with infinite type
    try testing.expectError(error.InfiniteType, ctx.unify(v, list_v));
}

test "infer literal fixnum" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const builder = ir_mod.IrBuilder.init(testing.allocator);
    const lit_node = try builder.lit(value_mod.Value.makeFixnum(42));
    defer testing.allocator.destroy(lit_node);

    var env = InferCtx.TypeEnv.init(testing.allocator);
    defer env.deinit();

    const inferred = try ctx.infer(lit_node, &env);
    try testing.expect(inferred.* == .concrete);
    try testing.expectEqual(types.Primitive.fixnum, inferred.concrete.primitive);
}

test "infer arithmetic constrains to fixnum" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const builder = ir_mod.IrBuilder.init(testing.allocator);
    const left = try builder.lit(value_mod.Value.makeFixnum(1));
    const right = try builder.lit(value_mod.Value.makeFixnum(2));
    const add_node = try builder.add(left, right);
    defer {
        testing.allocator.destroy(left);
        testing.allocator.destroy(right);
        testing.allocator.destroy(add_node);
    }

    var env = InferCtx.TypeEnv.init(testing.allocator);
    defer env.deinit();

    const inferred = try ctx.infer(add_node, &env);

    // Result should be fixnum
    try testing.expect(inferred.* == .concrete);
    try testing.expectEqual(types.Primitive.fixnum, inferred.concrete.primitive);

    // Should have generated constraints for operands
    try testing.expectEqual(@as(usize, 2), ctx.constraints.items.len);
}

test "infer lambda creates arrow type" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const builder = ir_mod.IrBuilder.init(testing.allocator);
    const body = try builder.lit(value_mod.Value.makeFixnum(42));
    const params = [_][]const u8{"x"};
    const captures = [_]ir_mod.Ir.Capture{};
    const lam = try builder.lambda(&params, null, &captures, body);
    defer {
        testing.allocator.destroy(body);
        for (lam.lambda.params) |p| testing.allocator.free(p);
        testing.allocator.free(lam.lambda.params);
        testing.allocator.free(lam.lambda.captures);
        testing.allocator.destroy(lam);
    }

    var env = InferCtx.TypeEnv.init(testing.allocator);
    defer env.deinit();

    const inferred = try ctx.infer(lam, &env);

    // Should be arrow type
    try testing.expect(inferred.* == .arrow);
    try testing.expectEqual(@as(usize, 1), inferred.arrow.domain.len);

    // Parameter should be type variable
    try testing.expect(inferred.arrow.domain[0].* == .variable);

    // Return type should be fixnum
    try testing.expect(inferred.arrow.range.* == .concrete);
    try testing.expectEqual(types.Primitive.fixnum, inferred.arrow.range.concrete.primitive);
}

test "infer if unifies branches" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    const builder = ir_mod.IrBuilder.init(testing.allocator);
    const cond = try builder.lit(value_mod.Value.t);
    const then_expr = try builder.lit(value_mod.Value.makeFixnum(1));
    const else_expr = try builder.lit(value_mod.Value.makeFixnum(2));
    const if_node = try builder.ifExpr(cond, then_expr, else_expr);
    defer {
        testing.allocator.destroy(cond);
        testing.allocator.destroy(then_expr);
        testing.allocator.destroy(else_expr);
        testing.allocator.destroy(if_node);
    }

    var env = InferCtx.TypeEnv.init(testing.allocator);
    defer env.deinit();

    const inferred = try ctx.infer(if_node, &env);

    // Result should be type variable (before solving)
    try testing.expect(inferred.* == .variable);

    // Should have constraints equating both branches to result
    try testing.expectEqual(@as(usize, 2), ctx.constraints.items.len);

    // Solve constraints
    try ctx.solve();

    // After solving, should resolve to fixnum
    const resolved = ctx.resolve(inferred);
    try testing.expect(resolved.* == .concrete);
    try testing.expectEqual(types.Primitive.fixnum, resolved.concrete.primitive);
}

test "generalize type with free variables" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    // Create arrow type: ?T0 -> ?T0 (identity function)
    const param = try ctx.freshVar(); // ?T0
    var domain = [_]*const InferType{param};
    const arrow = try ctx.allocator.create(InferType);
    arrow.* = .{ .arrow = .{ .domain = &domain, .range = param } };

    // Empty environment
    var env = InferCtx.TypeEnv.init(testing.allocator);
    defer env.deinit();

    // Generalize: should bind ?T0
    const scheme = try ctx.generalize(arrow, &env);
    defer testing.allocator.free(scheme.bound_vars);

    try testing.expectEqual(@as(usize, 1), scheme.bound_vars.len);
    try testing.expectEqual(@as(u32, 0), scheme.bound_vars[0]);
}

test "generalize respects environment" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    // Create type variable that's also in environment
    const env_var = try ctx.freshVar(); // ?T0

    // Environment has binding for ?T0
    var env = InferCtx.TypeEnv.init(testing.allocator);
    defer env.deinit();
    try env.bind("x", env_var);

    // Type to generalize also uses ?T0
    const arrow = try ctx.allocator.create(InferType);
    var domain = [_]*const InferType{env_var};
    arrow.* = .{ .arrow = .{ .domain = &domain, .range = env_var } };

    // Generalize: ?T0 is in env, so shouldn't be bound
    const scheme = try ctx.generalize(arrow, &env);
    defer testing.allocator.free(scheme.bound_vars);

    try testing.expectEqual(@as(usize, 0), scheme.bound_vars.len);
    try testing.expect(scheme.isMono());
}

test "instantiate creates fresh variables" {
    const testing = std.testing;
    var ctx = InferCtx.init(testing.allocator);
    defer ctx.deinit();

    // Create arrow type: ?T0 -> ?T0
    const param = try ctx.freshVar(); // ?T0
    var domain = [_]*const InferType{param};
    const arrow = try ctx.allocator.create(InferType);
    arrow.* = .{ .arrow = .{ .domain = &domain, .range = param } };

    // Create scheme: ∀T0. T0 -> T0
    var bound = [_]u32{0};
    const scheme = TypeScheme{ .bound_vars = &bound, .body = arrow };

    // Instantiate twice - should get different fresh variables each time
    const inst1 = try ctx.instantiate(scheme);
    const inst2 = try ctx.instantiate(scheme);

    try testing.expect(inst1.* == .arrow);
    try testing.expect(inst2.* == .arrow);

    // Both should have variable domains, but different var ids
    try testing.expect(inst1.arrow.domain[0].* == .variable);
    try testing.expect(inst2.arrow.domain[0].* == .variable);
    try testing.expect(inst1.arrow.domain[0].variable.id != inst2.arrow.domain[0].variable.id);
}
