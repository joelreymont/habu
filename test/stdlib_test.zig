
test "defstruct creates constructor, predicate, and accessors" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    const config = repl_mod.Config{};
    var r = try repl_mod.Repl.init(testing.allocator, &heap, config);
    defer r.deinit();
    r.wireGlobalEnv();

    const code =
        \\(defstruct point x y)
        \\(define p (make-point 42 99))
        \\(list (point-p p) (point-x p) (point-y p))
    ;

    const result = try r.eval(code);
    try testing.expect(result.isCons());
    
    // First element should be t (predicate returns true)
    const first = result.toPtr(Cons).car;
    try testing.expect(first.raw != Value.nil.raw);
    
    // Second element should be 42
    const rest1 = result.toPtr(Cons).cdr;
    try testing.expect(rest1.isCons());
    const second = rest1.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 42), second.toFixnum());
    
    // Third element should be 99
    const rest2 = rest1.toPtr(Cons).cdr;
    try testing.expect(rest2.isCons());
    const third = rest2.toPtr(Cons).car;
    try testing.expectEqual(@as(i64, 99), third.toFixnum());
}
