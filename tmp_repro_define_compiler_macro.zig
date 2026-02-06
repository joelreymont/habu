const std = @import("std");
const repl_mod = @import("src/interp/repl.zig");
const heap_mod = @import("src/runtime/heap.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var heap = try heap_mod.Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: repl_mod.Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try repl.loadStdlib();

    std.debug.print("step1\n", .{});
    _ = try repl.eval(
        \\(define-compiler-macro foo-cmpr (x y)
        \\  (declare (special *x*))
        \\  (setf *x* :bad)
        \\  `(list ,x ,y))
    );

    std.debug.print("step2\n", .{});
    _ = try repl.eval("(defmacro foo-cmpr (x y) `(list ,x ,y))");

    std.debug.print("step3\n", .{});
    const compiled = try repl.eval(
        \\(compile nil '(lambda (a b)
        \\                (declare (notinline foo-cmpr))
        \\                (foo-cmpr a b)))
    );
    std.debug.print("compiled kind={s}\n", .{@tagName(compiled.typeKind())});

    std.debug.print("step4\n", .{});
    const result = try repl.eval(
        \\(let ((*x* :good))
        \\  (declare (special *x*))
        \\  (funcall (compile nil '(lambda (a b)
        \\                           (declare (notinline foo-cmpr))
        \\                           (foo-cmpr a b)))
        \\           7 23))
    );
    std.debug.print("result kind={s}\n", .{@tagName(result.typeKind())});
}
