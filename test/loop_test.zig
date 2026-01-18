const std = @import("std");
const testing = std.testing;
const repl_mod = @import("../src/interp/repl.zig");
const Heap = @import("../src/runtime/heap.zig").Heap;
const Value = @import("../src/runtime/value.zig").Value;
const Cons = @import("../src/runtime/objects.zig").Cons;

test "loop simple iteration" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(define sum 0)
        \\(loop for i from 1 to 5
        \\  do (setq sum (+ sum i)))
        \\sum
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 15), result.toFixnum());
}

test "loop collect" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 5
        \\  collect (* i 2))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());

    const first = result.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 2), first.toFixnum());
}

test "loop append" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 3
        \\  append (list i i))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());
}

test "loop sum" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 10
        \\  sum i)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 55), result.toFixnum());
}

test "loop count" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 10
        \\  count (evenp i))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 5), result.toFixnum());
}

test "loop maximize" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i in '(3 7 2 9 4)
        \\  maximize i)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 9), result.toFixnum());
}

test "loop minimize" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i in '(3 7 2 9 4)
        \\  minimize i)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 2), result.toFixnum());
}

test "loop when conditional" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 10
        \\  when (evenp i)
        \\    collect i)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());

    const first = result.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 2), first.toFixnum());
}

test "loop unless conditional" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 5
        \\  unless (evenp i)
        \\    collect i)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());

    const first = result.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 1), first.toFixnum());
}

test "loop on list iteration" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for x on '(1 2 3)
        \\  collect (car x))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());
}

test "loop with multiple variables" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 3
        \\      for j from 10 to 12
        \\  collect (+ i j))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());

    const first = result.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 11), first.toFixnum());
}

test "loop with by modifier" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 0 to 10 by 2
        \\  collect i)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());

    const first = result.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 0), first.toFixnum());
}

test "loop always termination" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 5
        \\  always (< i 10))
    ;

    const result = try r.eval(code);
    try testing.expect(result.raw != Value.nil.raw);
}

test "loop never termination" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 5
        \\  never (> i 10))
    ;

    const result = try r.eval(code);
    try testing.expect(result.raw != Value.nil.raw);
}

test "loop thereis termination" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(loop for i from 1 to 10
        \\  thereis (and (> i 5) i))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 6), result.toFixnum());
}
