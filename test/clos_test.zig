const std = @import("std");
const testing = std.testing;
const repl_mod = @import("../src/interp/repl.zig");
const Heap = @import("../src/runtime/heap.zig").Heap;
const Value = @import("../src/runtime/value.zig").Value;

test "defclass creates constructor, predicate, and accessors" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass person () name age)
        \\(define p (make-person "Alice" 30))
        \\(list (person-p p) (person-name p) (person-age p))
    ;

    const result = try r.eval(code);

    // Final result should be (t "Alice" 30)
    try testing.expect(result.isCons());
}

test "inheritance includes parent slots" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass person () name age)
        \\(defclass employee (person) id department)
        \\(define e (make-employee "Bob" 25 "E001" "Eng"))
        \\(list (employee-name e) (employee-id e))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());
}

test "make-instance with keyword arguments" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass person () name age)
        \\(define p (make-instance 'person :age 30 :name "Charlie"))
        \\(person-age p)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 30), result.toFixnum());
}

test "defgeneric and defmethod dispatch" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defgeneric area (shape))
        \\(defclass circle () radius)
        \\(defmethod area ((c circle)) (* 3 (circle-radius c) (circle-radius c)))
        \\(define c (make-circle 5))
        \\(area c)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 75), result.toFixnum());
}

test "class instances satisfy their predicate" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass test-class () x y z)
        \\(define inst (make-test-class 1 2 3))
        \\(test-class-p inst)
    ;

    const result = try r.eval(code);
    try testing.expect(result.raw != Value.nil.raw);
}

test "accessor returns constructor argument" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass point () x y)
        \\(define p (make-point 42 99))
        \\(point-x p)
    ;

    const result = try r.eval(code);
    try testing.expectEqual(@as(i64, 42), result.toFixnum());
}

test "inheritance preserves slot order" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass base () a b)
        \\(defclass derived (base) c d)
        \\(define d (make-derived 1 2 3 4))
        \\(= (derived-a d) 1)
    ;

    const result = try r.eval(code);
    try testing.expect(result.raw != Value.nil.raw);
}

test "make-instance with partial keywords uses nil" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass person () name age)
        \\(define p (make-instance 'person :name "Alice"))
        \\(person-age p)
    ;

    const result = try r.eval(code);
    try testing.expect(result.isNil());
}

test "multiple inheritance merges slots" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defclass mixin1 () x)
        \\(defclass mixin2 () y)
        \\(defclass combined (mixin1 mixin2) z)
        \\(define c (make-combined 1 2 3))
        \\(combined-x c)
    ;

    const result = try r.eval(code);
    try testing.expectEqual(@as(i64, 1), result.toFixnum());
}
