const std = @import("std");
const testing = std.testing;

const Heap = @import("../runtime/heap.zig").Heap;
const Repl = @import("../interp/repl.zig").Repl;
const runtime = @import("../runtime/runtime.zig");
const Value = runtime.Value;
const Cons = runtime.Cons;

fn loadStdlib(repl: *Repl) !void {
    const null_writer = std.io.null_writer;
    const file = try std.fs.cwd().openFile("lib/stdlib.habu", .{});
    defer file.close();
    const content = try file.readToEndAlloc(repl.allocator, 16 * 1024 * 1024);
    defer repl.allocator.free(content);
    try repl.evalFile(content, null_writer);
}

fn ensureMaximaSources() !void {
    if (std.fs.cwd().access("../maxima/src/lmdcls.lisp", .{})) |_| return else |_| {}
    if (std.fs.cwd().access("../maxima/src/src/lmdcls.lisp", .{})) |_| return else |_| {}
    if (std.fs.cwd().access("../maxima/lmdcls.lisp", .{})) |_| return else |_| {}
    const candidates = [_][]const u8{
        "/tmp/maxima/src/lmdcls.lisp",
        "/tmp/maxima/src/src/lmdcls.lisp",
        "/tmp/maxima/lmdcls.lisp",
    };
    for (candidates) |path| {
        std.fs.accessAbsolute(path, .{}) catch continue;
        return;
    }
    return error.SkipZigTest;
}

fn consFixnumAt(list: Value, idx: usize) !i64 {
    var cur = list;
    var i: usize = 0;
    while (true) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        if (i == idx) {
            try testing.expect(cell.car.isFixnum());
            return cell.car.toFixnum();
        }
        cur = cell.cdr;
    }
}

test "loop on destructuring updates later for clauses in source order" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 8 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const result = try repl.eval(
        "(equal (loop for (exp coef) on '(1 a 0 b) by #'cddr for part = coef collect part) '(a b))",
    );
    try testing.expect(!result.isNil());
}

test "rtest6 integrate matches for file and string streams" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const out = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let* ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let* ((root (namestring (truename (habu-maxima-manifest-value :root))))
        \\           (path (concatenate 'string root "/tests/rtest6.mac"))
        \\           (file-result nil)
        \\           (string-result nil)
        \\           (file-expected nil)
        \\           (string-expected nil)
        \\           (file-form-eq 0)
        \\           (file-ok 0)
        \\           (string-ok 0)
        \\           (same-result 0))
        \\      (with-open-file (strm path :direction :input)
        \\        (let ((*query-io* (make-two-way-stream strm (make-string-output-stream)))
        \\              (*standard-input* *standard-input*)
        \\              (*mread-prompt* "")
        \\              (*read-base* 10.)
        \\              ($ratprint nil)
        \\              ($batch_answers_from_file t))
        \\          (let* ((reset-form (maxima::mread strm 'eof))
        \\                 (reset-res (maxima::meval* (list (list 'maxima::$errcatch) (third reset-form)))))
        \\            (declare (ignore reset-res))
        \\            (maxima::mread strm 'eof)
        \\            (let* ((file-form (maxima::mread strm 'eof))
        \\                   (string-form (with-input-from-string (s "integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);")
        \\                                  (maxima::mread s 'maxima::$eof)))
        \\                   (file-res (maxima::meval* (list (list 'maxima::$errcatch) (third file-form))))
        \\                   (string-res (maxima::meval* (list (list 'maxima::$errcatch) (third string-form)))))
        \\              (setq file-result (if (maxima::$emptyp file-res) 'error-catch (second file-res)))
        \\              (setq string-result (if (maxima::$emptyp string-res) 'error-catch (second string-res)))
        \\              (setq file-expected (third (with-input-from-string (s "beta(9/4,1/4);")
        \\                                           (maxima::mread s 'maxima::$eof))))
        \\              (setq string-expected file-expected)
        \\              (setq file-form-eq (if (equal (third file-form) (third string-form)) 1 0))
        \\              (setq file-ok (if (maxima::batch-equal-check file-expected file-result) 1 0))
        \\              (setq string-ok (if (maxima::batch-equal-check string-expected string-result) 1 0))
        \\              (setq same-result (if (equal file-result string-result) 1 0))))))
        \\      (list fail file-form-eq file-ok string-ok same-result)))))
    );

    const fail = try consFixnumAt(out, 0);
    const file_form_eq = try consFixnumAt(out, 1);
    const file_ok = try consFixnumAt(out, 2);
    const string_ok = try consFixnumAt(out, 3);
    const same_result = try consFixnumAt(out, 4);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), file_form_eq);
    try testing.expectEqual(@as(i64, 1), file_ok);
    try testing.expectEqual(@as(i64, 1), string_ok);
    try testing.expectEqual(@as(i64, 1), same_result);
}

test "rtest6 integrate parse stays in maxima symbol space" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const out = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let* ((form (with-input-from-string (s "integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);")
        \\                   (maxima::mread s 'maxima::$eof)))
        \\           (expr (third form))
        \\           (op (caar expr))
        \\           (var (third expr))
        \\           (hi (fifth expr))
        \\           (res (maxima::meval* (list (list 'maxima::$errcatch) expr)))
        \\           (val (if (maxima::$emptyp res) 'error-catch (second res))))
        \\      (list fail
        \\            (if (eq op 'maxima::$integrate) 1 0)
        \\            (if (eq var 'maxima::$x) 1 0)
        \\            (if (eq hi 'maxima::$inf) 1 0)
        \\            (if (eq hi 'maxima::inf) 1 0)
        \\            (if (numberp val) 1 0)
        \\            (if (equal val '$%e) 1 0)))))
    );

    const fail = try consFixnumAt(out, 0);
    const op_ok = try consFixnumAt(out, 1);
    const var_ok = try consFixnumAt(out, 2);
    const hi_dollar_ok = try consFixnumAt(out, 3);
    const hi_plain_ok = try consFixnumAt(out, 4);
    const val_number = try consFixnumAt(out, 5);
    const val_e = try consFixnumAt(out, 6);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), op_ok);
    try testing.expectEqual(@as(i64, 1), var_ok);
    try testing.expectEqual(@as(i64, 1), hi_dollar_ok);
    try testing.expectEqual(@as(i64, 0), hi_plain_ok);
    try testing.expectEqual(@as(i64, 0), val_number);
    try testing.expectEqual(@as(i64, 0), val_e);
}

test "maxima sysconst globals self-evaluate" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const out = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (if (and (= fail 0)
        \\             (eq maxima::$inf 'maxima::$inf)
        \\             (eq maxima::$minf 'maxima::$minf)
        \\             (eq maxima::$und 'maxima::$und)
        \\             (eq maxima::$ind 'maxima::$ind)
        \\             (eq maxima::$infinity 'maxima::$infinity))
        \\        1
        \\        0))))
    );

    try testing.expect(out.isFixnum());
    try testing.expectEqual(@as(i64, 1), out.toFixnum());
}

test "maxima meval* clears temporary sign facts on error" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const out = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (maxima::clear)
        \\    (setq maxima::*local-signs* nil)
        \\    (maxima::tdzero 'maxima::*z*)
        \\    (let ((before-mark (if (null (symbol-plist 'maxima::*z*)) 0 1))
        \\          (before-local (if maxima::*local-signs* 1 0)))
        \\      (handler-case
        \\          (maxima::meval* '((mprogn) (($error) 1)))
        \\        (condition (c)
        \\          (declare (ignore c))
        \\          nil))
        \\      (list fail
        \\            before-mark
        \\            before-local
        \\            (if (null maxima::*local-signs*) 1 0)
        \\            (if (null (symbol-plist 'maxima::*z*)) 1 0)))))
    );

    const fail = try consFixnumAt(out, 0);
    const before_mark = try consFixnumAt(out, 1);
    const before_local = try consFixnumAt(out, 2);
    const local_cleared = try consFixnumAt(out, 3);
    const plist_cleared = try consFixnumAt(out, 4);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), before_mark);
    try testing.expectEqual(@as(i64, 1), before_local);
    try testing.expectEqual(@as(i64, 1), local_cleared);
    try testing.expectEqual(@as(i64, 1), plist_cleared);
}

test "rtest6 integrate does not leak *z* sign marks across errcatch" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    const out = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (maxima::meval* '((mprogn) (($reset)) (($kill) $all) 0))
        \\    (let* ((z 'maxima::*z*)
        \\           (before (if (null (symbol-plist z)) 1 0))
        \\           (form (with-input-from-string (s "integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);")
        \\                   (maxima::mread s 'maxima::$eof)))
        \\           (expr (third form))
        \\           (res (maxima::meval* (list (list 'maxima::$errcatch) expr)))
        \\           (after (symbol-plist z))
        \\           (after-nil (if (null after) 1 0))
        \\           (local-clear (if (null maxima::*local-signs*) 1 0))
        \\           (caught (if (maxima::$emptyp res) 1 0)))
        \\      (list fail before after-nil local-clear caught)))))
    );

    const fail = try consFixnumAt(out, 0);
    const before_nil = try consFixnumAt(out, 1);
    const after_nil = try consFixnumAt(out, 2);
    const local_clear = try consFixnumAt(out, 3);
    const caught = try consFixnumAt(out, 4);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), before_nil);
    try testing.expectEqual(@as(i64, 1), after_nil);
    try testing.expectEqual(@as(i64, 1), local_clear);
    try testing.expectEqual(@as(i64, 0), caught);
}
