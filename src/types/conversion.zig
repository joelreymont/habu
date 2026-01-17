//! Type Conversion Checking
//!
//! In a dependent type system, type equality is more complex than simple structural
//! equality. Two types are "convertible" (definitionally equal) if they reduce to
//! the same normal form.
//!
//! Key concepts:
//! - Alpha equivalence: (Π (x : A) B) = (Π (y : A) B[y/x]) - names don't matter
//! - Beta equivalence: ((λx.B) A) reduces to B[x := A]
//! - Eta equivalence: f = λx. f x (function extensionality)
//!
//! This module provides `TypeConverter` which uses the term normalizer to check
//! type equality for dependent types.

const std = @import("std");
const type_mod = @import("type.zig");
const term_mod = @import("term.zig");
const normalize_mod = @import("normalize.zig");
const refinement_mod = @import("refinement.zig");

const Type = type_mod.Type;
const Quantity = type_mod.Quantity;
const Term = term_mod.Term;
const Normalizer = normalize_mod.Normalizer;
const NormEnv = normalize_mod.NormEnv;
const NormError = normalize_mod.NormError;
const RefinementChecker = refinement_mod.RefinementChecker;
const RefineResult = @import("smt.zig").RefineResult;

/// Type conversion checker
pub const TypeConverter = struct {
    allocator: std.mem.Allocator,
    normalizer: Normalizer,
    /// Optional refinement checker for SMT-based predicate checking
    ref_checker: ?RefinementChecker,

    pub fn init(allocator: std.mem.Allocator) TypeConverter {
        return .{
            .allocator = allocator,
            .normalizer = Normalizer.init(allocator),
            .ref_checker = null, // Lazy init on first use
        };
    }

    pub fn deinit(self: *TypeConverter) void {
        if (self.ref_checker) |*rc| {
            rc.deinit();
        }
    }

    /// Get or create refinement checker (lazy initialization)
    fn getRefChecker(self: *TypeConverter) *RefinementChecker {
        if (self.ref_checker == null) {
            self.ref_checker = RefinementChecker.init(self.allocator);
        }
        return &self.ref_checker.?;
    }

    /// Check if two types are convertible (definitionally equal)
    /// Returns true if t1 ≡ t2
    pub fn convertible(self: *TypeConverter, t1: *const Type, t2: *const Type) ConversionError!bool {
        // Same pointer - trivially equal
        if (t1 == t2) return true;

        // Check structural equality with alpha equivalence
        return self.typeEqual(t1, t2, &.{});
    }

    /// Check type equality with a context of bound variable names
    /// The context maps de Bruijn levels to whether the names match
    fn typeEqual(
        self: *TypeConverter,
        t1: *const Type,
        t2: *const Type,
        ctx: *const AlphaCtx,
    ) ConversionError!bool {
        return switch (t1.*) {
            // Primitive types - must be identical
            .primitive => |p1| switch (t2.*) {
                .primitive => |p2| p1 == p2,
                else => false,
            },

            // Any matches any
            .any => t2.* == .any,

            // Struct types - nominal equality
            .@"struct" => |s1| switch (t2.*) {
                .@"struct" => |s2| std.mem.eql(u8, s1.name, s2.name),
                else => false,
            },

            // Arrow types - compare domain and range
            .arrow => |a1| switch (t2.*) {
                .arrow => |a2| {
                    if (a1.domain.len != a2.domain.len) return false;
                    for (a1.domain, a2.domain) |d1, d2| {
                        if (!try self.typeEqual(d1, d2, ctx)) return false;
                    }
                    return self.typeEqual(a1.range, a2.range, ctx);
                },
                else => false,
            },

            // List types
            .list => |e1| switch (t2.*) {
                .list => |e2| self.typeEqual(e1, e2, ctx),
                else => false,
            },

            // Vector types
            .vec => |e1| switch (t2.*) {
                .vec => |e2| self.typeEqual(e1, e2, ctx),
                else => false,
            },

            // Non-nil types
            .non_nil => |n1| switch (t2.*) {
                .non_nil => |n2| self.typeEqual(n1, n2, ctx),
                else => false,
            },

            // Union types - must have same members in same order
            .@"or" => |types1| switch (t2.*) {
                .@"or" => |types2| {
                    if (types1.len != types2.len) return false;
                    for (types1, types2) |ty1, ty2| {
                        if (!try self.typeEqual(ty1, ty2, ctx)) return false;
                    }
                    return true;
                },
                else => false,
            },

            // Intersection types - must have same members in same order
            .@"and" => |types1| switch (t2.*) {
                .@"and" => |types2| {
                    if (types1.len != types2.len) return false;
                    for (types1, types2) |ty1, ty2| {
                        if (!try self.typeEqual(ty1, ty2, ctx)) return false;
                    }
                    return true;
                },
                else => false,
            },

            // Negation types
            .not => |n1| switch (t2.*) {
                .not => |n2| self.typeEqual(n1, n2, ctx),
                else => false,
            },

            // Pi types - alpha equivalence for binders
            .pi => |p1| switch (t2.*) {
                .pi => |p2| {
                    // Quantities must match
                    if (p1.quantity != p2.quantity) return false;

                    // Parameter types must be equal
                    if (!try self.typeEqual(p1.param_type, p2.param_type, ctx)) return false;

                    // Bodies must be equal under extended context
                    // Alpha equivalence: names can differ
                    const new_ctx = AlphaCtx{
                        .name1 = p1.param_name,
                        .name2 = p2.param_name,
                        .parent = ctx,
                    };
                    return self.typeEqual(p1.body, p2.body, &new_ctx);
                },
                else => false,
            },

            // Sigma types - alpha equivalence for binders
            .sigma => |s1| switch (t2.*) {
                .sigma => |s2| {
                    // First types must be equal
                    if (!try self.typeEqual(s1.first_type, s2.first_type, ctx)) return false;

                    // Second types must be equal under extended context
                    const new_ctx = AlphaCtx{
                        .name1 = s1.first_name,
                        .name2 = s2.first_name,
                        .parent = ctx,
                    };
                    return self.typeEqual(s1.second_type, s2.second_type, &new_ctx);
                },
                else => false,
            },

            // Refinement types
            .refinement => |r1| switch (t2.*) {
                .refinement => |r2| {
                    // Base types must be equal
                    if (!try self.typeEqual(r1.base_type, r2.base_type, ctx)) return false;

                    // Both predicates must be present
                    const p1 = r1.predicate orelse return false;
                    const p2 = r2.predicate orelse return false;

                    // Predicates must be convertible (term equality)
                    // Cast opaque pointers to Term pointers
                    const pred1: *const Term = @ptrCast(@alignCast(p1));
                    const pred2: *const Term = @ptrCast(@alignCast(p2));

                    var env = NormEnv.init(self.allocator);
                    defer env.deinit();

                    return self.normalizer.convertible(pred1, pred2, &env) catch |err| switch (err) {
                        error.FuelExhausted => return error.FuelExhausted,
                        error.TypeError, error.DivisionByZero, error.CannotReduce => false,
                        error.OutOfMemory => return error.OutOfMemory,
                    };
                },
                else => false,
            },

            // Type variables - compare by name (in same context)
            .type_var => |v1| switch (t2.*) {
                .type_var => |v2| {
                    // Check if names correspond under alpha context
                    return self.namesEquivalent(v1, v2, ctx);
                },
                else => false,
            },

            // Type applications - normalize and compare
            .type_app => |ta1| switch (t2.*) {
                .type_app => |ta2| {
                    // Compare function types
                    if (!try self.typeEqual(ta1.func, ta2.func, ctx)) return false;

                    // Compare arguments (terms)
                    const arg1: *const Term = @ptrCast(@alignCast(ta1.arg));
                    const arg2: *const Term = @ptrCast(@alignCast(ta2.arg));

                    var env = NormEnv.init(self.allocator);
                    defer env.deinit();

                    return self.normalizer.convertible(arg1, arg2, &env) catch |err| switch (err) {
                        error.FuelExhausted => return error.FuelExhausted,
                        error.TypeError, error.DivisionByZero, error.CannotReduce => false,
                        error.OutOfMemory => return error.OutOfMemory,
                    };
                },
                else => false,
            },

            // Universe levels - must be same level
            .type_level => |lvl1| switch (t2.*) {
                .type_level => |lvl2| lvl1 == lvl2,
                else => false,
            },
        };
    }

    /// Check if two variable names are equivalent under alpha context
    fn namesEquivalent(self: *TypeConverter, name1: []const u8, name2: []const u8, ctx: *const AlphaCtx) bool {
        _ = self;
        // Walk up the context looking for matching binders
        var current: ?*const AlphaCtx = ctx;
        while (current) |c| {
            if (c.name1.len > 0) { // Not empty sentinel
                // If name1 is bound at this level, check if name2 is too
                if (std.mem.eql(u8, name1, c.name1)) {
                    return std.mem.eql(u8, name2, c.name2);
                }
                // If name2 is bound but name1 isn't, not equivalent
                if (std.mem.eql(u8, name2, c.name2)) {
                    return false;
                }
            }
            current = c.parent;
        }
        // Neither name is bound - compare directly
        return std.mem.eql(u8, name1, name2);
    }

    /// Check if t1 is a subtype of t2
    /// Subtyping rules:
    /// - any <: any
    /// - T <: any (any is top)
    /// - (refine T x P) <: T (refinement is subtype of base)
    /// - (refine T x (P ∧ Q)) <: (refine T x P) (stronger refinement is subtype)
    pub fn isSubtype(self: *TypeConverter, t1: *const Type, t2: *const Type) ConversionError!bool {
        // Identical types are subtypes
        if (try self.convertible(t1, t2)) return true;

        // Any is the top type
        if (t2.* == .any) return true;

        // Refinement subtyping
        switch (t1.*) {
            .refinement => |r1| {
                // {x : T | P} <: T  (refinement is subtype of base)
                if (try self.isSubtype(r1.base_type, t2)) return true;

                // {x : T | P} <: {x : T | Q} if P => Q
                switch (t2.*) {
                    .refinement => |r2| {
                        // First check base types are compatible
                        if (!try self.isSubtype(r1.base_type, r2.base_type)) return false;

                        // Both predicates must be present for SMT checking
                        const p1 = r1.predicate orelse return false;
                        const p2 = r2.predicate orelse return false;

                        // Now check predicates with SMT: P => Q
                        const pred1: *const Term = @ptrCast(@alignCast(p1));
                        const pred2: *const Term = @ptrCast(@alignCast(p2));

                        // Use same variable name for both predicates
                        const var_name = r1.predicate_var;

                        const ref_checker = self.getRefChecker();
                        const result = ref_checker.checkSubtype(pred1, pred2, var_name);

                        return result == .valid;
                    },
                    else => {},
                }
            },
            else => {},
        }

        // Non-nil is subtype of inner type
        switch (t1.*) {
            .non_nil => |n1| return self.isSubtype(n1, t2),
            else => {},
        }

        // List covariance: (list A) <: (list B) if A <: B
        switch (t1.*) {
            .list => |e1| switch (t2.*) {
                .list => |e2| return self.isSubtype(e1, e2),
                else => {},
            },
            else => {},
        }

        // Vector covariance
        switch (t1.*) {
            .vec => |e1| switch (t2.*) {
                .vec => |e2| return self.isSubtype(e1, e2),
                else => {},
            },
            else => {},
        }

        // Pi contravariance in domain, covariance in range
        switch (t1.*) {
            .pi => |p1| switch (t2.*) {
                .pi => |p2| {
                    // Domain contravariant: A2 <: A1
                    if (!try self.isSubtype(p2.param_type, p1.param_type)) return false;
                    // Range covariant: B1 <: B2
                    return self.isSubtype(p1.body, p2.body);
                },
                else => {},
            },
            else => {},
        }

        return false;
    }
};

/// Alpha-equivalence context
/// Tracks bound variable name correspondences for alpha comparison
const AlphaCtx = struct {
    /// Variable name in first type
    name1: []const u8 = "",
    /// Corresponding variable name in second type
    name2: []const u8 = "",
    /// Parent context
    parent: ?*const AlphaCtx = null,
};

pub const ConversionError = error{
    FuelExhausted,
    OutOfMemory,
};

// ============================================================================
// Tests
// ============================================================================

test "primitive type conversion" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var converter = TypeConverter.init(allocator);

    // Same primitive types are convertible
    try testing.expect(try converter.convertible(&type_mod.t_fixnum, &type_mod.t_fixnum));
    try testing.expect(try converter.convertible(&type_mod.t_string, &type_mod.t_string));

    // Different primitives are not convertible
    try testing.expect(!try converter.convertible(&type_mod.t_fixnum, &type_mod.t_string));
}

test "any type conversion" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var converter = TypeConverter.init(allocator);

    try testing.expect(try converter.convertible(&type_mod.t_any, &type_mod.t_any));
    try testing.expect(!try converter.convertible(&type_mod.t_any, &type_mod.t_fixnum));
}

test "list type conversion" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var converter = TypeConverter.init(allocator);

    try testing.expect(try converter.convertible(&type_mod.t_list_any, &type_mod.t_list_any));
}

test "pi type alpha equivalence" {
    const testing = std.testing;

    var converter = TypeConverter.init(testing.allocator);
    defer converter.deinit();
    var builder = type_mod.TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // Create (pi (x : fixnum) fixnum) and (pi (y : fixnum) fixnum)
    // These should be alpha-equivalent
    const pi1 = try builder.makePi("x", &type_mod.t_fixnum, &type_mod.t_fixnum, .many);
    const pi2 = try builder.makePi("y", &type_mod.t_fixnum, &type_mod.t_fixnum, .many);

    try testing.expect(try converter.convertible(pi1, pi2));
}

test "pi type different param types" {
    const testing = std.testing;

    var converter = TypeConverter.init(testing.allocator);
    defer converter.deinit();
    var builder = type_mod.TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    // (pi (x : fixnum) fixnum) vs (pi (x : string) fixnum)
    const pi1 = try builder.makePi("x", &type_mod.t_fixnum, &type_mod.t_fixnum, .many);
    const pi2 = try builder.makePi("x", &type_mod.t_string, &type_mod.t_fixnum, .many);

    try testing.expect(!try converter.convertible(pi1, pi2));
}

test "type variable equivalence" {
    const testing = std.testing;

    var converter = TypeConverter.init(testing.allocator);
    defer converter.deinit();
    var builder = type_mod.TypeBuilder.init(testing.allocator);
    defer builder.deinit();

    const var_a = try builder.makeTypeVar("a");
    const var_a2 = try builder.makeTypeVar("a");
    const var_b = try builder.makeTypeVar("b");

    try testing.expect(try converter.convertible(var_a, var_a2));
    try testing.expect(!try converter.convertible(var_a, var_b));
}

test "subtyping" {
    const testing = std.testing;

    var converter = TypeConverter.init(testing.allocator);
    defer converter.deinit();

    // Any is top - everything is subtype of any
    try testing.expect(try converter.isSubtype(&type_mod.t_fixnum, &type_mod.t_any));
    try testing.expect(try converter.isSubtype(&type_mod.t_string, &type_mod.t_any));

    // Reflexivity
    try testing.expect(try converter.isSubtype(&type_mod.t_fixnum, &type_mod.t_fixnum));

    // Different types not related
    try testing.expect(!try converter.isSubtype(&type_mod.t_fixnum, &type_mod.t_string));
}

test "universe level conversion" {
    const testing = std.testing;

    var converter = TypeConverter.init(testing.allocator);
    defer converter.deinit();

    try testing.expect(try converter.convertible(&type_mod.t_type, &type_mod.t_type));
    try testing.expect(try converter.convertible(&type_mod.t_type1, &type_mod.t_type1));
    try testing.expect(!try converter.convertible(&type_mod.t_type, &type_mod.t_type1));
}

test "refinement subtyping with SMT" {
    const testing = std.testing;

    var converter = TypeConverter.init(testing.allocator);
    defer converter.deinit();

    var type_builder = type_mod.TypeBuilder.init(testing.allocator);
    defer type_builder.deinit();
    var term_builder = term_mod.TermBuilder.init(testing.allocator);
    defer term_builder.deinit();

    // Create {x : fixnum | x > 0}
    const x = try term_builder.varByName("x");
    const zero = try term_builder.fixnum(0);
    const one = try term_builder.fixnum(1);
    const gt_zero = try term_builder.cmp(.gt, x, zero);
    const ge_zero = try term_builder.cmp(.ge, x, zero);
    const ge_one = try term_builder.cmp(.ge, x, one);

    const refine_gt_zero = try type_builder.makeRefinement(&type_mod.t_fixnum, "x", gt_zero);
    const refine_ge_zero = try type_builder.makeRefinement(&type_mod.t_fixnum, "x", ge_zero);
    const refine_ge_one = try type_builder.makeRefinement(&type_mod.t_fixnum, "x", ge_one);

    // {x | x > 0} <: {x | x >= 0}  -- valid!
    try testing.expect(try converter.isSubtype(refine_gt_zero, refine_ge_zero));

    // {x | x >= 0} <: {x | x > 0}  -- invalid! (x=0 is counterexample)
    try testing.expect(!try converter.isSubtype(refine_ge_zero, refine_gt_zero));

    // {x | x > 0} <: fixnum  -- valid! (refinement is subtype of base)
    try testing.expect(try converter.isSubtype(refine_gt_zero, &type_mod.t_fixnum));

    // {x | x > 0} == {x | x >= 1} for integers - both valid as subtypes of each other
    try testing.expect(try converter.isSubtype(refine_gt_zero, refine_ge_one));
    try testing.expect(try converter.isSubtype(refine_ge_one, refine_gt_zero));
}
