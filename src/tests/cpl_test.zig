const std = @import("std");
const objects = @import("../runtime/objects.zig");
const Value = @import("../runtime/value.zig").Value;
const Heap = @import("../runtime/heap.zig").Heap;

test "C3 linearization - single parent" {
    var heap = try Heap.init(std.testing.allocator, .{});
    defer heap.deinit();

    const class_a = try heap.intern("A");
    const class_b = try heap.intern("B");

    const get_cpl = struct {
        fn f(c: Value) []const Value {
            const static = struct {
                var a_cpl: [1]Value = undefined;
                var initialized = false;
            };
            if (!static.initialized) {
                static.a_cpl[0] = c;
                static.initialized = true;
            }
            return &static.a_cpl;
        }
    }.f;

    const direct_supers = [_]Value{class_a};
    const cpl = try objects.computeCpl(std.testing.allocator, class_b, &direct_supers, &get_cpl);
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
        o_cpl: [1]Value,
        a_cpl: [2]Value,
        b_cpl: [2]Value,
        fn getCpl(self: *const @This(), cls: Value) []const Value {
            if (cls.eq(self.o_cpl[0])) return &self.o_cpl;
            if (cls.eq(self.a_cpl[0])) return &self.a_cpl;
            if (cls.eq(self.b_cpl[0])) return &self.b_cpl;
            unreachable;
        }
    };

    var ctx = Ctx{
        .o_cpl = .{o},
        .a_cpl = .{ a, o },
        .b_cpl = .{ b, o },
    };

    const static = struct {
        var ctx_ptr: *Ctx = undefined;
        fn getCpl(cls: Value) []const Value {
            return ctx_ptr.getCpl(cls);
        }
    };
    static.ctx_ptr = &ctx;

    const direct_supers = [_]Value{ a, b };
    const cpl = try objects.computeCpl(std.testing.allocator, c, &direct_supers, &static.getCpl);
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
