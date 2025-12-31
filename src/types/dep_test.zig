//! Tests for dependent types (Pi, Sigma, Refinement) and QTT

const std = @import("std");
const type_adt = @import("type.zig");
const Type = type_adt.Type;
const TypeBuilder = type_adt.TypeBuilder;
const Quantity = type_adt.Quantity;

test "Pi type creation and properties" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (pi (n : fixnum) fixnum)
    const pi_simple = try builder.makePi("n", &type_adt.t_fixnum, &type_adt.t_fixnum, .many);

    try std.testing.expect(pi_simple.isPi());
    try std.testing.expect(pi_simple.isDependent());
    try std.testing.expect(!pi_simple.couldBeNil());
    try std.testing.expectEqualStrings("(pi ...)", pi_simple.name());

    // Extract Pi components
    const pi_data = pi_simple.pi;
    try std.testing.expectEqualStrings("n", pi_data.param_name);
    try std.testing.expectEqual(Quantity.many, pi_data.quantity);
    try std.testing.expect(pi_data.param_type.eql(type_adt.t_fixnum));
    try std.testing.expect(pi_data.body.eql(type_adt.t_fixnum));
}

test "Pi type with erased parameter (quantity=zero)" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (pi (0 n : fixnum) string) - erased proof parameter
    const pi_erased = try builder.makePi("n", &type_adt.t_fixnum, &type_adt.t_string, .zero);

    try std.testing.expect(pi_erased.isPi());
    try std.testing.expectEqual(Quantity.zero, pi_erased.pi.quantity);

    // Erased parameters shouldn't affect runtime behavior
    try std.testing.expect(pi_erased.pi.quantity.allowsUses(0));
    try std.testing.expect(!pi_erased.pi.quantity.allowsUses(1));
}

test "Pi type with linear parameter (quantity=one)" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (pi (1 fd : file-descriptor) unit) - linear resource
    const t_fd = &type_adt.t_symbol; // Mock file-descriptor type
    const t_unit = &type_adt.t_nil;

    const pi_linear = try builder.makePi("fd", t_fd, t_unit, .one);

    try std.testing.expect(pi_linear.isPi());
    try std.testing.expectEqual(Quantity.one, pi_linear.pi.quantity);
    try std.testing.expect(pi_linear.pi.quantity.allowsUses(1));
    try std.testing.expect(!pi_linear.pi.quantity.allowsUses(2));
}

test "Sigma type creation and properties" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (sigma (n : fixnum) string) - pair of number and string
    const sigma = try builder.makeSigma("n", &type_adt.t_fixnum, &type_adt.t_string);

    try std.testing.expect(sigma.isSigma());
    try std.testing.expect(sigma.isDependent());
    try std.testing.expect(!sigma.couldBeNil());
    try std.testing.expectEqualStrings("(sigma ...)", sigma.name());

    const sigma_data = sigma.sigma;
    try std.testing.expectEqualStrings("n", sigma_data.first_name);
    try std.testing.expect(sigma_data.first_type.eql(type_adt.t_fixnum));
    try std.testing.expect(sigma_data.second_type.eql(type_adt.t_string));
}

test "Refinement type creation and properties" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (refine fixnum x (> x 0)) - positive integers
    // Predicate is null for now (not yet implemented)
    const refine_pos = try builder.makeRefinement(&type_adt.t_fixnum, "x", null);

    try std.testing.expect(refine_pos.isRefinement());
    try std.testing.expect(refine_pos.isDependent());
    try std.testing.expectEqualStrings("(refine ...)", refine_pos.name());

    const refine_data = refine_pos.refinement;
    try std.testing.expectEqualStrings("x", refine_data.predicate_var);
    try std.testing.expect(refine_data.base_type.eql(type_adt.t_fixnum));
    try std.testing.expectEqual(@as(?*const anyopaque, null), refine_data.predicate);
}

test "Refinement type base type extraction" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (refine (refine fixnum x (> x 0)) y (< y 100))
    // Nested refinement: positive integers less than 100
    const inner = try builder.makeRefinement(&type_adt.t_fixnum, "x", null);
    const outer = try builder.makeRefinement(inner, "y", null);

    // getBaseType should unwrap to fixnum
    const base = outer.getBaseType();
    try std.testing.expect(base.eql(type_adt.t_fixnum));
}

test "Refinement inherits couldBeNil from base type" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (refine (list fixnum) xs (not (null xs))) - non-empty list
    const t_list_fixnum = try builder.makeList(&type_adt.t_fixnum);
    const refine_nonempty = try builder.makeRefinement(t_list_fixnum, "xs", null);

    // List could be nil, so refinement could be nil (before predicate check)
    try std.testing.expect(refine_nonempty.couldBeNil());

    // (refine fixnum n (> n 0)) - positive, fixnum can't be nil
    const refine_pos = try builder.makeRefinement(&type_adt.t_fixnum, "n", null);
    try std.testing.expect(!refine_pos.couldBeNil());
}

test "Type variable creation and equality" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    const type_var_a = try builder.makeTypeVar("a");
    const type_var_b = try builder.makeTypeVar("b");
    const type_var_a2 = try builder.makeTypeVar("a");

    try std.testing.expect(type_var_a.isDependent());
    try std.testing.expectEqualStrings("a", type_var_a.name());

    // Same name = equal
    try std.testing.expect(type_var_a.eql(type_var_a2.*));

    // Different names = not equal
    try std.testing.expect(!type_var_a.eql(type_var_b.*));
}

test "Universe levels" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    const type0 = try builder.makeTypeLevel(0);
    const type1 = try builder.makeTypeLevel(1);
    const type2 = try builder.makeTypeLevel(2);

    try std.testing.expect(type0.eql(type_adt.t_type));
    try std.testing.expect(type1.eql(type_adt.t_type1));
    try std.testing.expect(type2.eql(type_adt.t_type2));

    try std.testing.expectEqualStrings("Type", type0.name());
    try std.testing.expectEqualStrings("Type+", type1.name());
    try std.testing.expectEqualStrings("Type+", type2.name());

    // Universe types can't be nil
    try std.testing.expect(!type0.couldBeNil());
}

test "Type application creation" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (vec a n) where n is a term
    // For now, arg is just a mock pointer
    const mock_term: *const anyopaque = @ptrCast(&type_adt.t_fixnum);
    const type_var_a = try builder.makeTypeVar("a");
    const vec_a = try builder.makeVec(type_var_a);
    const vec_a_n = try builder.makeTypeApp(vec_a, mock_term);

    try std.testing.expect(vec_a_n.isDependent());
    try std.testing.expectEqualStrings("(type-app ...)", vec_a_n.name());

    const app_data = vec_a_n.type_app;
    try std.testing.expect(app_data.func.eql(vec_a.*));
    try std.testing.expectEqual(mock_term, app_data.arg);
}

test "Pi type equality - alpha equivalence" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (pi (x : fixnum) fixnum) and (pi (y : fixnum) fixnum)
    // Should be alpha-equivalent (names don't matter structurally)
    const pi1 = try builder.makePi("x", &type_adt.t_fixnum, &type_adt.t_fixnum, .many);
    const pi2 = try builder.makePi("y", &type_adt.t_fixnum, &type_adt.t_fixnum, .many);

    // Current implementation does structural equality (doesn't check names)
    try std.testing.expect(pi1.eql(pi2.*));

    // Different quantity = not equal
    const pi3 = try builder.makePi("x", &type_adt.t_fixnum, &type_adt.t_fixnum, .zero);
    try std.testing.expect(!pi1.eql(pi3.*));
}

test "Sigma type equality" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    const sigma1 = try builder.makeSigma("n", &type_adt.t_fixnum, &type_adt.t_string);
    const sigma2 = try builder.makeSigma("m", &type_adt.t_fixnum, &type_adt.t_string);
    const sigma3 = try builder.makeSigma("n", &type_adt.t_string, &type_adt.t_fixnum);

    // Same structure = equal (names don't matter)
    try std.testing.expect(sigma1.eql(sigma2.*));

    // Different types = not equal
    try std.testing.expect(!sigma1.eql(sigma3.*));
}

test "Quantity arithmetic for QTT" {
    // 0 * q = 0 (erasing multiplies to zero)
    try std.testing.expectEqual(Quantity.zero, Quantity.zero.mult(.zero));
    try std.testing.expectEqual(Quantity.zero, Quantity.zero.mult(.one));
    try std.testing.expectEqual(Quantity.zero, Quantity.zero.mult(.many));

    // 1 * q = q (linear preserves quantity)
    try std.testing.expectEqual(Quantity.zero, Quantity.one.mult(.zero));
    try std.testing.expectEqual(Quantity.one, Quantity.one.mult(.one));
    try std.testing.expectEqual(Quantity.many, Quantity.one.mult(.many));

    // ω * q (unrestricted becomes unrestricted unless erased)
    try std.testing.expectEqual(Quantity.zero, Quantity.many.mult(.zero));
    try std.testing.expectEqual(Quantity.many, Quantity.many.mult(.one));
    try std.testing.expectEqual(Quantity.many, Quantity.many.mult(.many));

    // 0 + q = q (no usage adds to usage)
    try std.testing.expectEqual(Quantity.zero, Quantity.zero.add(.zero));
    try std.testing.expectEqual(Quantity.one, Quantity.zero.add(.one));
    try std.testing.expectEqual(Quantity.many, Quantity.zero.add(.many));

    // 1 + 1 = ω (two linear uses = unrestricted)
    try std.testing.expectEqual(Quantity.many, Quantity.one.add(.one));

    // ω + q = ω (unrestricted stays unrestricted)
    try std.testing.expectEqual(Quantity.many, Quantity.many.add(.zero));
    try std.testing.expectEqual(Quantity.many, Quantity.many.add(.one));
    try std.testing.expectEqual(Quantity.many, Quantity.many.add(.many));
}

test "Quantity usage allowance" {
    // Zero allows only 0 uses
    try std.testing.expect(Quantity.zero.allowsUses(0));
    try std.testing.expect(!Quantity.zero.allowsUses(1));
    try std.testing.expect(!Quantity.zero.allowsUses(2));

    // One allows 0 or 1 use
    try std.testing.expect(Quantity.one.allowsUses(0));
    try std.testing.expect(Quantity.one.allowsUses(1));
    try std.testing.expect(!Quantity.one.allowsUses(2));

    // Many allows any number of uses
    try std.testing.expect(Quantity.many.allowsUses(0));
    try std.testing.expect(Quantity.many.allowsUses(1));
    try std.testing.expect(Quantity.many.allowsUses(100));
}

test "Complex dependent type: length-indexed vector" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (pi (n : fixnum) (vec fixnum n))
    // Function that takes a length and returns a vector of that length

    // First, create the return type: (vec fixnum)
    const vec_fixnum = try builder.makeVec(&type_adt.t_fixnum);

    // Now wrap in Pi: (pi (n : fixnum) (vec fixnum))
    // Note: Full dependent typing would have (vec fixnum n) referencing n
    const length_indexed = try builder.makePi("n", &type_adt.t_fixnum, vec_fixnum, .many);

    try std.testing.expect(length_indexed.isPi());
    try std.testing.expect(length_indexed.pi.body.eql(vec_fixnum.*));
}

test "Complex dependent type: sorted list refinement" {
    var builder = TypeBuilder.init(std.testing.allocator);
    defer builder.deinit();

    // (refine (list fixnum) xs (sorted? xs))
    const list_fixnum = try builder.makeList(&type_adt.t_fixnum);
    const sorted_list = try builder.makeRefinement(list_fixnum, "xs", null);

    try std.testing.expect(sorted_list.isRefinement());
    try std.testing.expect(sorted_list.refinement.base_type.eql(list_fixnum.*));

    // Base type extraction should unwrap to (list fixnum)
    const base = sorted_list.getBaseType();
    try std.testing.expect(base.* == .list);
}
