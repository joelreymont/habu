const std = @import("std");
const objects = @import("../runtime/objects.zig");
const Value = @import("../runtime/value.zig").Value;
const Heap = @import("../runtime/heap.zig").Heap;

test "C3 linearization - single parent" {
    var heap = try Heap.init(std.testing.allocator, .{});
    defer heap.deinit();

    const class_a = try heap.intern("A");
    const class_b = try heap.intern("B");

    const a_cpl = try heap.allocCons(class_a, Value.nil);
    const Ctx = struct {
        class_a: Value,
        a_cpl: Value,
        fn getCpl(ctx_ptr: *anyopaque, cls: Value) Value {
            const ctx: *@This() = @ptrCast(@alignCast(ctx_ptr));
            if (cls.eq(ctx.class_a)) return ctx.a_cpl;
            return Value.nil;
        }
    };
    var ctx = Ctx{ .class_a = class_a, .a_cpl = a_cpl };

    const direct_supers = [_]Value{class_a};
    const cpl = try objects.computeCpl(std.testing.allocator, class_b, &direct_supers, &ctx, &Ctx.getCpl);
    defer std.testing.allocator.free(cpl);

    try std.testing.expectEqual(@as(usize, 2), cpl.len);
    try std.testing.expect(cpl[0].eq(class_b));
    try std.testing.expect(cpl[1].eq(class_a));
}

test "C3 linearization - diamond" {
    var heap = try Heap.init(std.testing.allocator, .{});
    defer heap.deinit();

    const o = try heap.intern("O");
    const a = try heap.intern("A");
    const b = try heap.intern("B");
    const c = try heap.intern("C");

    const Ctx = struct {
        o: Value,
        a: Value,
        b: Value,
        o_cpl: Value,
        a_cpl: Value,
        b_cpl: Value,
        fn getCpl(ctx_ptr: *anyopaque, cls: Value) Value {
            const ctx: *@This() = @ptrCast(@alignCast(ctx_ptr));
            if (cls.eq(ctx.o)) return ctx.o_cpl;
            if (cls.eq(ctx.a)) return ctx.a_cpl;
            if (cls.eq(ctx.b)) return ctx.b_cpl;
            return Value.nil;
        }
    };

    const o_cpl = try heap.allocCons(o, Value.nil);
    const a_cpl = try heap.allocCons(a, o_cpl);
    const b_cpl = try heap.allocCons(b, o_cpl);
    var ctx = Ctx{
        .o = o,
        .a = a,
        .b = b,
        .o_cpl = o_cpl,
        .a_cpl = a_cpl,
        .b_cpl = b_cpl,
    };

    const direct_supers = [_]Value{ a, b };
    const cpl = try objects.computeCpl(std.testing.allocator, c, &direct_supers, &ctx, &Ctx.getCpl);
    defer std.testing.allocator.free(cpl);

    try std.testing.expectEqual(@as(usize, 4), cpl.len);
    try std.testing.expect(cpl[0].eq(c));
    try std.testing.expect(cpl[1].eq(a));
    try std.testing.expect(cpl[2].eq(b));
    try std.testing.expect(cpl[3].eq(o));
}

test "method specificity - sorts most specific first" {
    const compile = @import("../compiler/compile.zig");

    var heap = try Heap.init(std.testing.allocator, .{});
    defer heap.deinit();

    const dog = try heap.intern("dog");
    const animal = try heap.intern("animal");

    var methods = [_]compile.Compiler.MethodDef{
        .{ .specializers = &.{Value.t}, .function_name = "m1", .qualifier = .primary },
        .{ .specializers = &.{animal}, .function_name = "m2", .qualifier = .primary },
        .{ .specializers = &.{dog}, .function_name = "m3", .qualifier = .primary },
        .{ .specializers = &.{ dog, animal }, .function_name = "m4", .qualifier = .primary },
    };

    const Ctx = struct {
        fn sort(ms: []compile.Compiler.MethodDef) !void {
            std.mem.sort(compile.Compiler.MethodDef, ms, {}, struct {
                fn lessThan(_: void, a: compile.Compiler.MethodDef, b: compile.Compiler.MethodDef) bool {
                    // Count non-t specializers (more = more specific)
                    var a_count: usize = 0;
                    var b_count: usize = 0;
                    for (a.specializers) |s| {
                        if (!s.eq(Value.t)) a_count += 1;
                    }
                    for (b.specializers) |s| {
                        if (!s.eq(Value.t)) b_count += 1;
                    }
                    if (a_count != b_count) return a_count > b_count;

                    // Same count, compare positionally (earlier non-t = more specific)
                    const min_len = @min(a.specializers.len, b.specializers.len);
                    for (0..min_len) |i| {
                        const a_is_t = a.specializers[i].eq(Value.t);
                        const b_is_t = b.specializers[i].eq(Value.t);
                        if (!a_is_t and b_is_t) return true;
                        if (a_is_t and !b_is_t) return false;
                    }
                    return false;
                }
            }.lessThan);
        }
    };

    try Ctx.sort(&methods);

    // m4 (2 specialized params) should be first, m1 (0 specialized) should be last
    // m2 and m3 (both 1 specialized) have equal specificity, order is unspecified
    try std.testing.expect(std.mem.eql(u8, methods[0].function_name, "m4"));
    try std.testing.expect(std.mem.eql(u8, methods[3].function_name, "m1"));
    // Middle two should be m2 and m3 in some order
    const mid1 = std.mem.eql(u8, methods[1].function_name, "m2") or std.mem.eql(u8, methods[1].function_name, "m3");
    const mid2 = std.mem.eql(u8, methods[2].function_name, "m2") or std.mem.eql(u8, methods[2].function_name, "m3");
    try std.testing.expect(mid1 and mid2);
}
