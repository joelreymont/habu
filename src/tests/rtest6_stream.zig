const std = @import("std");
const testing = std.testing;

const Heap = @import("../runtime/heap.zig").Heap;
const Repl = @import("../interp/repl.zig").Repl;
const script_run = @import("../app/script_run.zig");
const runtime = @import("../runtime/runtime.zig");
const snap = @import("../testing/snapshot.zig");
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

fn consAt(list: Value, idx: usize) !Value {
    var cur = list;
    var i: usize = 0;
    while (true) : (i += 1) {
        try testing.expect(cur.isCons());
        const cell = cur.toPtr(Cons);
        if (i == idx) return cell.car;
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
        \\(multiple-value-bind (ok total fail) (maxima::maxima-load-all :verbose nil :habu-stop-on-error t)
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
        \\(multiple-value-bind (ok total fail) (maxima::maxima-load-all :verbose nil :habu-stop-on-error t)
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
        \\(multiple-value-bind (ok total fail) (maxima::maxima-load-all :verbose nil :habu-stop-on-error t)
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
        \\(multiple-value-bind (ok total fail) (maxima::maxima-load-all :verbose nil :habu-stop-on-error t)
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

test "rtest6 test-batch sink behavior stays inside handler-case" {
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
        \\  (declare (ignore ok total fail))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let* ((root (namestring (truename (habu-maxima-manifest-value :root))))
        \\           (path (concatenate 'string root "/tests/rtest6.mac"))
        \\           (sink-str (make-string-output-stream))
        \\           (nil-ok (handler-case (progn (let ((*collect-errors* nil)
        \\                                              (maxima::$batch_answers_from_file t))
        \\                                          (test-batch path nil))
        \\                                        1)
        \\                     (condition (c) (declare (ignore c)) 0)))
        \\           (str-ok (handler-case (progn (let ((*collect-errors* sink-str)
        \\                                              (maxima::$batch_answers_from_file t))
        \\                                          (test-batch path nil))
        \\                                        1)
        \\                     (condition (c) (declare (ignore c)) 0)))
        \\           (t-ok (handler-case (progn (let ((*collect-errors* t)
        \\                                            (maxima::$batch_answers_from_file t))
        \\                                        (test-batch path nil))
        \\                                      1)
        \\                   (condition (c) (declare (ignore c)) 0))))
        \\      (list fail nil-ok str-ok t-ok))))
    );

    const fail = try consFixnumAt(out, 0);
    const nil_ok = try consFixnumAt(out, 1);
    const str_ok = try consFixnumAt(out, 2);
    const t_ok = try consFixnumAt(out, 3);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), nil_ok);
    try testing.expectEqual(@as(i64, 1), str_ok);
    try testing.expectEqual(@as(i64, 1), t_ok);
}

test "rtest6 scientific float strings round-trip through maxima forms" {
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
        \\    (flet ((okp (src)
        \\             (let* ((form (with-input-from-string (s src)
        \\                            (maxima::mread s 'maxima::$eof)))
        \\                    (val (maxima::meval form)))
        \\               (if (eq val 'maxima::$true) 1 0))))
        \\      (list fail
        \\            (okp "(string(2e7), %% = \"2.0e+7\" or %% = \"2.0E+7\" or %% = \"2.0e7\" or %% = \"2.0E7\" or %%);")
        \\            (okp "(string(2e-7), %% = \"2.0e-7\" or %% = \"2.0E-7\" or %%);")
        \\            (okp "(string(12345000000.0), %% = \"1.2345e+10\" or %% = \"1.2345E+10\" or %% = \"1.2345e10\" or %% = \"1.2345E10\" or %%);")
        \\            (okp "(string(1/1024.0), %% = \"9.765625e-4\" or %% = \"9.765625E-4\" or %%);")
        \\            (okp "is(parse_string(string(most_positive_float)) = most_positive_float);")))))
    );

    const fail = try consFixnumAt(out, 0);
    const s1 = try consFixnumAt(out, 1);
    const s2 = try consFixnumAt(out, 2);
    const s3 = try consFixnumAt(out, 3);
    const s4 = try consFixnumAt(out, 4);
    const roundtrip = try consFixnumAt(out, 5);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), s1);
    try testing.expectEqual(@as(i64, 1), s2);
    try testing.expectEqual(@as(i64, 1), s3);
    try testing.expectEqual(@as(i64, 1), s4);
    try testing.expectEqual(@as(i64, 1), roundtrip);
}

test "rtest6 problem 47 isolated test-batch path stays clean" {
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

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p47.mac",
        .data =
            \\(reset (fpprintprec), 0);
            \\0;
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p47.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (maxima::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problem 2 isolated test-batch path stays clean" {
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

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p2.mac",
        .data =
            \\(reset(), kill(all),0);
            \\0;
            \\
            \\integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);
            \\beta(9/4,1/4);
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p2.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problem 2 isolated path after testsuite bootstrap stays clean" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-manifest.lisp\")");
    _ = try repl.eval("(load \"../maxima/src/maxima-package.lisp\")");
    _ = try repl.eval("(load \"lib/maxima-stubs.lisp\")");
    _ = try repl.eval("(load \"../maxima/src/testsuite.lisp\")");
    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p2-bootstrap.mac",
        .data =
            \\(reset(), kill(all),0);
            \\0;
            \\
            \\integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);
            \\beta(9/4,1/4);
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p2-bootstrap.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problem 20 isolated tellsimp path stays clean" {
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
        \\    (let* ((form
        \\             (with-input-from-string
        \\               (s "(kill (f), matchdeclare (xx, integerp), tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))), [f(1), f(1)(y)]);")
        \\               (maxima::mread s 'maxima::$eof)))
        \\           (res (maxima::meval* (third form)))
        \\           (expected
        \\             (third
        \\               (with-input-from-string
        \\                 (s "[lambda ([a], a - 1), y - 1];")
        \\                 (maxima::mread s 'maxima::$eof))))
        \\           (oldrules (maxima::mget 'maxima::$f 'maxima::oldrules))
        \\           (r (and oldrules (car oldrules)))
        \\           (lam1
        \\             (third
        \\               (with-input-from-string
        \\                 (s "lambda ([a], a - 1);")
        \\                 (maxima::mread s 'maxima::$eof))))
        \\           (rule-lambda
        \\             (and r
        \\                  (multiple-value-bind (lambda-expr closure-p name)
        \\                      (function-lambda-expression (symbol-function r))
        \\                    (declare (ignore closure-p name))
        \\                    lambda-expr)))
        \\           (raw-call (and r (funcall r '((maxima::$f) 1) 1 nil)))
        \\           (simp-call (and r (funcall r '((maxima::$f simp) 1) 1 nil)))
        \\           (simp-call-dosimp (and r (funcall r '((maxima::$f simp) 1) 1 t))))
        \\      (list fail
        \\            (if (get 'maxima::$f 'maxima::operators) 1 0)
        \\            (if (and r (eq (get 'maxima::$f 'maxima::operators) r)) 1 0)
        \\            (if oldrules 1 0)
        \\            (if (eq r 'maxima::$frule1) 1 0)
        \\            (if (and r (symbol-package r) (eq (symbol-package r) (find-package :maxima))) 1 0)
        \\            (if (and r (maxima::mget r 'maxima::ruleof)) 1 0)
        \\            (if (and r (maxima::mget r 'maxima::$rule)) 1 0)
        \\            (if (and r (fboundp r)) 1 0)
        \\            (if (eq t (integerp 1)) 1 0)
        \\            (if (eq t (maxima::definitely-so '((integerp) 1))) 1 0)
        \\            (if (= (let ((xx 1))
        \\                      (declare (special xx))
        \\                      (setf (symbol-value 'xx) 9)
        \\                      xx)
        \\                    9)
        \\                1
        \\                0)
        \\            (if (and r (equal raw-call lam1)) 1 0)
        \\            (if (and r (equal simp-call lam1)) 1 0)
        \\            (if (and r (equal simp-call-dosimp lam1)) 1 0)
        \\            (if (equal expected res) 1 0)
        \\            (if (maxima::batch-equal-check expected res) 1 0)
        \\            (symbol-name r)
        \\            (write-to-string rule-lambda)
        \\            (write-to-string raw-call)
        \\            (write-to-string simp-call)
        \\            (write-to-string simp-call-dosimp)
        \\            (write-to-string res)
        \\            (write-to-string expected)))))
    );

    const fail = try consFixnumAt(out, 0);
    const operators_ok = try consFixnumAt(out, 1);
    const operators_eq_rule_ok = try consFixnumAt(out, 2);
    const oldrules_ok = try consFixnumAt(out, 3);
    const rule_eq_ok = try consFixnumAt(out, 4);
    const rule_pkg_ok = try consFixnumAt(out, 5);
    const ruleof_ok = try consFixnumAt(out, 6);
    const rule_ok = try consFixnumAt(out, 7);
    const fboundp_ok = try consFixnumAt(out, 8);
    const int_ok = try consFixnumAt(out, 9);
    const maybe_ok = try consFixnumAt(out, 10);
    const symv_ok = try consFixnumAt(out, 11);
    const raw_ok = try consFixnumAt(out, 12);
    const simp_ok = try consFixnumAt(out, 13);
    const simp_dosimp_ok = try consFixnumAt(out, 14);
    const equal_ok = try consFixnumAt(out, 15);
    const result_ok = try consFixnumAt(out, 16);
    const rule_name = try consAt(out, 17);
    const lambda_str = try consAt(out, 18);
    const raw_str = try consAt(out, 19);
    const simp_str = try consAt(out, 20);
    const simp_dosimp_str = try consAt(out, 21);
    const res_str = try consAt(out, 22);
    const expected_str = try consAt(out, 23);

    std.debug.print(
        "p20 flags ops={d} ops_eq={d} oldrules={d} eq={d} pkg={d} ruleof={d} rule={d} fboundp={d} int={d} maybe={d} symv={d} raw={d} simp={d} simp_t={d} equal={d} result={d} name={s} lambda={s} raw-res={s} simp-res={s} simp-t-res={s} res={s} expected={s}\n",
        .{
            operators_ok,
            operators_eq_rule_ok,
            oldrules_ok,
            rule_eq_ok,
            rule_pkg_ok,
            ruleof_ok,
            rule_ok,
            fboundp_ok,
            int_ok,
            maybe_ok,
            symv_ok,
            raw_ok,
            simp_ok,
            simp_dosimp_ok,
            equal_ok,
            result_ok,
            rule_name.toPtr(runtime.String).bytes(),
            lambda_str.toPtr(runtime.String).bytes(),
            raw_str.toPtr(runtime.String).bytes(),
            simp_str.toPtr(runtime.String).bytes(),
            simp_dosimp_str.toPtr(runtime.String).bytes(),
            res_str.toPtr(runtime.String).bytes(),
            expected_str.toPtr(runtime.String).bytes(),
        },
    );

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), operators_ok);
    try testing.expectEqual(@as(i64, 1), operators_eq_rule_ok);
    try testing.expectEqual(@as(i64, 1), oldrules_ok);
    try testing.expectEqual(@as(i64, 1), rule_eq_ok);
    try testing.expectEqual(@as(i64, 1), rule_pkg_ok);
    try testing.expectEqual(@as(i64, 1), ruleof_ok);
    try testing.expectEqual(@as(i64, 1), rule_ok);
    try testing.expectEqual(@as(i64, 1), fboundp_ok);
    try testing.expectEqual(@as(i64, 1), int_ok);
    try testing.expectEqual(@as(i64, 1), maybe_ok);
    try testing.expectEqual(@as(i64, 1), symv_ok);
    try testing.expectEqual(@as(i64, 1), raw_ok);
    try testing.expectEqual(@as(i64, 1), simp_ok);
    try testing.expectEqual(@as(i64, 1), simp_dosimp_ok);
    try testing.expectEqual(@as(i64, 1), equal_ok);
    try testing.expectEqual(@as(i64, 1), result_ok);
}

test "rtest6 problem 22 isolated tellsimp mqapply path stays clean" {
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
        \\    (let* ((form (with-input-from-string
        \\                    (s "(matchdeclare ([xx, yy], integerp), tellsimp (f(xx)(yy), yy*xx), [f(2), f(2)(3)]);")
        \\                    (maxima::mread s 'maxima::$eof)))
        \\           (res (maxima::meval* (third form)))
        \\           (expected (third (with-input-from-string
        \\                              (s "[f(2), 6];")
        \\                              (maxima::mread s 'maxima::$eof)))))
        \\      (list fail
        \\            (if (maxima::mget 'maxima::$f 'maxima::operators) 1 0)
        \\            (if (maxima::mget 'maxima::$f 'maxima::oldrules) 1 0)
        \\            (if (maxima::mget 'maxima::$subvarrule1 'maxima::ruleof) 1 0)
        \\            (if (maxima::mget 'maxima::$subvarrule1 'maxima::$rule) 1 0)
        \\            (if (fboundp 'maxima::$subvarrule1) 1 0)
        \\            (if (maxima::batch-equal-check expected res) 1 0)))))
    );

    const fail = try consFixnumAt(out, 0);
    const operators_ok = try consFixnumAt(out, 1);
    const oldrules_ok = try consFixnumAt(out, 2);
    const ruleof_ok = try consFixnumAt(out, 3);
    const rule_ok = try consFixnumAt(out, 4);
    const fboundp_ok = try consFixnumAt(out, 5);
    const result_ok = try consFixnumAt(out, 6);

    try testing.expectEqual(@as(i64, 0), fail);
    try testing.expectEqual(@as(i64, 1), operators_ok);
    try testing.expectEqual(@as(i64, 1), oldrules_ok);
    try testing.expectEqual(@as(i64, 1), ruleof_ok);
    try testing.expectEqual(@as(i64, 1), rule_ok);
    try testing.expectEqual(@as(i64, 1), fboundp_ok);
    try testing.expectEqual(@as(i64, 1), result_ok);
}

test "maxima match catch basics" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const plain = try repl.eval("(catch 'maxima::match 42)");
    try testing.expect(plain.isFixnum());
    try testing.expectEqual(@as(i64, 42), plain.toFixnum());

    const thrown = try repl.eval("(catch 'maxima::match (throw 'maxima::match 7))");
    try testing.expect(thrown.isFixnum());
    try testing.expectEqual(@as(i64, 7), thrown.toFixnum());

    const matcherr = try repl.eval("(catch 'maxima::match (maxima::matcherr))");
    try testing.expect(matcherr.isNil());

    const out = try repl.eval(
        \\(write-to-string
        \\  (list
        \\  (write-to-string
        \\    (let ((a '(1)))
        \\      (maxima::kar a)))
        \\  (write-to-string
        \\    (prog (a)
        \\      (setq a '(1))
        \\      (return (maxima::kar a))))
        \\  (write-to-string
        \\    (prog (a)
        \\      (declare (special a))
        \\      (setq a '(1))
        \\      (return (maxima::kar a))))
        \\  (write-to-string
        \\    (prog (a)
        \\      (declare (special a))
        \\      (setq a '(1))
        \\      (catch 'maxima::match
        \\        (return (maxima::kar a)))))
        \\  (write-to-string
        \\    (prog (a)
        \\      (declare (special a))
        \\      (setq a '(1))
        \\      (return
        \\        (catch 'maxima::match
        \\          (prog (g)
        \\            (declare (special g))
        \\            (setq g (maxima::kar a))
        \\            (return g))))))))
    );
    try snap.expectValue(@src(), out, "(\"1\" \"1\" \"1\" \"1\" \"1\")");
}

test "maxima definitely-so sees gensym special bindings" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let* ((g (gensym "TR-G"))
        \\         (form `(prog (,g)
        \\                  (declare (special ,g))
        \\                  (setq ,g 1)
        \\                  (return
        \\                    (list
        \\                      ,g
        \\                      (boundp ',g)
        \\                      (symbol-value ',g)
        \\                      (progn (setf (symbol-value ',g) 9)
        \\                             (symbol-value ',g))
        \\                      ,g
        \\                      (maxima::definitely-so '((integerp) ,g))))))
        \\         (bind (car (second form)))
        \\         (setq-sym (second (fourth form)))
        \\         (list-form (second (fifth form)))
        \\         (direct-1 (second list-form))
        \\         (boundp-sym (second (second (third list-form))))
        \\         (symval-sym (second (second (fourth list-form))))
        \\         (direct-2 (sixth list-form))
        \\         (defsym-sym (second (second (second (seventh list-form))))))
        \\    (list
        \\      (eq bind setq-sym)
        \\      (eq bind direct-1)
        \\      (eq bind boundp-sym)
        \\      (eq bind symval-sym)
        \\      (eq bind direct-2)
        \\      (eq bind defsym-sym)
        \\      (eval form))))
    );
    try snap.expectValue(@src(), out, "(t t t t t t (1 t 1 9 9 t))");
}

test "maxima definitely-so sees gensym special bindings through catch" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((g (gensym "TR-G")))
        \\    (eval
        \\      `(catch 'maxima::match
        \\         (prog (,g)
        \\           (declare (special ,g))
        \\           (setq ,g 1)
        \\           (return
        \\             (list
        \\               ,g
        \\               (boundp ',g)
        \\               (symbol-value ',g)
        \\               (progn (setf (symbol-value ',g) 9)
        \\                      (symbol-value ',g))
        \\               ,g
        \\               (maxima::definitely-so '((integerp) ,g)))))))))
    );
    try snap.expectValue(@src(), out, "(1 t 1 9 9 t)");
}

test "maxima msetq sees gensym special bindings" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((g (gensym "TR-G")))
        \\    (eval
        \\      `(prog ($xx ,g)
        \\         (declare (special $xx ,g))
        \\         (setq ,g 1)
        \\         (cond ((maxima::definitely-so '((integerp) ,g))
        \\                (msetq $xx ,g))
        \\               (t 'miss))
        \\         (return (list $xx ,g)))))))
    );
    try snap.expectValue(@src(), out, "(1 1)");
}

test "maxima definitely-so sees gensym special bindings inside compiled lambda" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((g (gensym "TR-G")))
        \\    (funcall
        \\      (eval
        \\        `(lambda ()
        \\           (prog (,g)
        \\             (declare (special ,g))
        \\             (setq ,g 1)
        \\             (return
        \\               (list
        \\                 ,g
        \\                 (boundp ',g)
        \\                 (symbol-value ',g)
        \\                 (progn (setf (symbol-value ',g) 9)
        \\                        (symbol-value ',g))
        \\                 ,g
        \\                 (maxima::definitely-so '((integerp) ,g))))))))))
    );
    try snap.expectValue(@src(), out, "(1 t 1 9 9 t)");
}

test "maxima tellsimp inner prog fragment stays clean" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((g (gensym "TR-G")))
        \\    (multiple-value-list
        \\      (funcall
        \\        (eval
        \\          `(lambda ()
        \\             (catch 'match
        \\               (prog ($xx ,g)
        \\                 (declare (special $xx ,g))
        \\                 (setq ,g 1)
        \\                 (cond ((maxima::definitely-so '((integerp) ,g))
        \\                        (msetq $xx ,g))
        \\                       ((matcherr)))
        \\                 (cond (nil (matcherr)))
        \\                 (return (values $xx t))))))))))
    );
    try snap.expectValue(@src(), out, "(1 t)");
}

test "prog return preserves multiple values" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(write-to-string
        \\  (multiple-value-list
        \\    (prog ()
        \\      (return (values 1 t)))))
    );
    try snap.expectValue(@src(), out, "(1 t)");
}

test "catch preserves multiple values on normal exit" {
    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 64 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(write-to-string
        \\  (multiple-value-list
        \\    (catch 'tag
        \\      (values 1 t))))
    );
    try snap.expectValue(@src(), out, "(1 t)");
}

test "maxima tellsimp matcher fragment stays clean" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((g1 (gensym "TR-G"))
        \\        (g2 (gensym "TR-G")))
        \\    (funcall
        \\      (eval
        \\        `(lambda ()
        \\           (prog (ans $xx ,g1 rule-hit)
        \\             (declare (special ans $xx ,g1))
        \\             (setq ,g1 '(1))
        \\             (multiple-value-setq (ans rule-hit)
        \\               (prog (,g2)
        \\                 (declare (special ,g2))
        \\                 (setq ,g2 (maxima::kar ,g1))
        \\                 (return
        \\                   (values
        \\                     (list
        \\                       ,g2
        \\                       (maxima::definitely-so '((integerp) ,g2))
        \\                       (nthkdr ,g1 1)
        \\                       (progn (msetq $xx ,g2) $xx)
        \\                       $xx)
        \\                     t))))
        \\             (return (list ans rule-hit))))))))
    );
    try snap.expectValue(@src(), out, "((1 t nil 1 1) t)");
}

test "rtest6 problem 20 isolated test-batch path stays clean" {
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

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p20.mac",
        .data =
            \\(kill (f),
            \\ matchdeclare (xx, integerp),
            \\ tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))),
            \\ [f(1), f(1)(y)]);
            \\[lambda ([a], a - 1), y - 1];
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p20.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problem 22 isolated test-batch path stays clean" {
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

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p22.mac",
        .data =
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p22.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problem 20 script wrapper preserves tellsimp state" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p20-wrapper.lisp",
        .data =
            \\(load "lib/maxima-loader.lisp")
            \\(maxima-load-all :verbose nil :habu-stop-on-error t)
            \\(let ((*package* (find-package :maxima)))
            \\  (let* ((form (with-input-from-string
            \\                  (s "(kill (f), matchdeclare (xx, integerp), tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))), [f(1), f(1)(y)]);")
            \\                  (maxima::mread s 'maxima::$eof)))
            \\         (res (maxima::meval* (third form))))
            \\    (format t "RES ~S~%" res)
            \\    (format t "OPS ~S~%" (maxima::mget 'maxima::$f 'maxima::operators))
            \\    (format t "RULEOF ~S~%" (maxima::mget 'maxima::$frule1 'maxima::ruleof))
            \\    (format t "RULE ~S~%" (maxima::mget 'maxima::$frule1 'maxima::$rule))))
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script = try std.fs.path.join(allocator, &.{ base, "p20-wrapper.lisp" });
    defer allocator.free(script);

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "./zig-out/bin/habu", script },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "RES ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "OPS ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "RULEOF ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "RULE ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "OPS nil") == null);
    try testing.expect(std.mem.indexOf(u8, out, "RULEOF nil") == null);
    try testing.expect(std.mem.indexOf(u8, out, "RULE nil") == null);
}

test "rtest6 problem 20 script wrapper preserves tellsimp state on spawned thread" {
    const Ctx = struct {
        err: ?anyerror = null,

        fn run(self: *@This()) void {
            self.runInner() catch |err| {
                self.err = err;
            };
        }

        fn runInner(self: *@This()) !void {
            _ = self;
            try ensureMaximaSources();

            const allocator = testing.allocator;
            var tmp = testing.tmpDir(.{});
            defer tmp.cleanup();

            try tmp.dir.writeFile(.{
                .sub_path = "p20-wrapper.lisp",
                .data =
                    \\(load "lib/maxima-loader.lisp")
                    \\(maxima-load-all :verbose nil :habu-stop-on-error t)
                    \\(let ((*package* (find-package :maxima)))
                    \\  (let* ((form (with-input-from-string
                    \\                  (s "(kill (f), matchdeclare (xx, integerp), tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))), [f(1), f(1)(y)]);")
                    \\                  (maxima::mread s 'maxima::$eof)))
                    \\         (res (maxima::meval* (third form))))
                    \\    (format t "RES ~S~%" res)
                    \\    (format t "OPS ~S~%" (maxima::mget 'maxima::$f 'maxima::operators))
                    \\    (format t "RULEOF ~S~%" (maxima::mget 'maxima::$frule1 'maxima::ruleof))
                    \\    (format t "RULE ~S~%" (maxima::mget 'maxima::$frule1 'maxima::$rule))))
                    \\
                ,
            });

            const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
            defer allocator.free(base);
            const script = try std.fs.path.join(allocator, &.{ base, "p20-wrapper.lisp" });
            defer allocator.free(script);

            var buf: [4096]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            try script_run.run(
                allocator,
                256 * 1024 * 1024,
                &.{ "./zig-out/bin/habu", script },
                stream.writer(),
            );

            const out = stream.getWritten();
            try testing.expect(std.mem.indexOf(u8, out, "RES ") != null);
            try testing.expect(std.mem.indexOf(u8, out, "OPS ") != null);
            try testing.expect(std.mem.indexOf(u8, out, "RULEOF ") != null);
            try testing.expect(std.mem.indexOf(u8, out, "RULE ") != null);
            try testing.expect(std.mem.indexOf(u8, out, "OPS nil") == null);
            try testing.expect(std.mem.indexOf(u8, out, "RULEOF nil") == null);
            try testing.expect(std.mem.indexOf(u8, out, "RULE nil") == null);
        }
    };

    var ctx = Ctx{};
    const thread = try std.Thread.spawn(.{ .stack_size = 512 * 1024 * 1024 }, Ctx.run, .{&ctx});
    thread.join();
    if (ctx.err) |err| return err;
}

test "rtest6 problem 20 script wrapper preserves tellsimp state with gpa" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p20-wrapper.lisp",
        .data =
            \\(load "lib/maxima-loader.lisp")
            \\(maxima-load-all :verbose nil :habu-stop-on-error t)
            \\(let ((*package* (find-package :maxima)))
            \\  (let* ((form (with-input-from-string
            \\                  (s "(kill (f), matchdeclare (xx, integerp), tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))), [f(1), f(1)(y)]);")
            \\                  (maxima::mread s 'maxima::$eof)))
            \\         (res (maxima::meval* (third form))))
            \\    (format t "RES ~S~%" res)
            \\    (format t "OPS ~S~%" (maxima::mget 'maxima::$f 'maxima::operators))
            \\    (format t "RULEOF ~S~%" (maxima::mget 'maxima::$frule1 'maxima::ruleof))
            \\    (format t "RULE ~S~%" (maxima::mget 'maxima::$frule1 'maxima::$rule))))
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script = try std.fs.path.join(allocator, &.{ base, "p20-wrapper.lisp" });
    defer allocator.free(script);

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "./zig-out/bin/habu", script },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "RES ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "OPS ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "RULEOF ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "RULE ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "OPS nil") == null);
    try testing.expect(std.mem.indexOf(u8, out, "RULEOF nil") == null);
    try testing.expect(std.mem.indexOf(u8, out, "RULE nil") == null);
}

test "rtest6 problem 20 script wrapper preserves tellsimp state with file writer" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p20-wrapper.lisp",
        .data =
            \\(load "lib/maxima-loader.lisp")
            \\(maxima-load-all :verbose nil :habu-stop-on-error t)
            \\(let ((*package* (find-package :maxima)))
            \\  (let* ((form (with-input-from-string
            \\                  (s "(kill (f), matchdeclare (xx, integerp), tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))), [f(1), f(1)(y)]);")
            \\                  (maxima::mread s 'maxima::$eof)))
            \\         (res (maxima::meval* (third form))))
            \\    (format t "RES ~S~%" res)
            \\    (format t "OPS ~S~%" (maxima::mget 'maxima::$f 'maxima::operators))
            \\    (format t "RULEOF ~S~%" (maxima::mget 'maxima::$frule1 'maxima::ruleof))
            \\    (format t "RULE ~S~%" (maxima::mget 'maxima::$frule1 'maxima::$rule))))
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script = try std.fs.path.join(allocator, &.{ base, "p20-wrapper.lisp" });
    defer allocator.free(script);

    var out_file = try tmp.dir.createFile("out.txt", .{ .read = true, .truncate = true });
    defer out_file.close();
    var out_buf: [4096]u8 = undefined;
    var out_writer = out_file.writer(&out_buf);

    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "./zig-out/bin/habu", script },
        &out_writer.interface,
    );
    try out_writer.interface.flush();
    try out_file.seekTo(0);

    const out = try out_file.readToEndAlloc(allocator, 8192);
    defer allocator.free(out);

    try testing.expect(std.mem.indexOf(u8, out, "RES ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "OPS ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "RULEOF ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "RULE ") != null);
    try testing.expect(std.mem.indexOf(u8, out, "OPS nil") == null);
    try testing.expect(std.mem.indexOf(u8, out, "RULEOF nil") == null);
    try testing.expect(std.mem.indexOf(u8, out, "RULE nil") == null);
}

test "rtest6 problem 20 child process preserves tellsimp state" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p20-wrapper.lisp",
        .data =
            \\(load "lib/maxima-loader.lisp")
            \\(maxima-load-all :verbose nil :habu-stop-on-error t)
            \\(let ((*package* (find-package :maxima)))
            \\  (let* ((form (with-input-from-string
            \\                  (s "(kill (f), matchdeclare (xx, integerp), tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))), [f(1), f(1)(y)]);")
            \\                  (maxima::mread s 'maxima::$eof)))
            \\         (res (maxima::meval* (third form))))
            \\    (format t "RES ~S~%" res)
            \\    (format t "OPS ~S~%" (maxima::mget 'maxima::$f 'maxima::operators))
            \\    (format t "RULEOF ~S~%" (maxima::mget 'maxima::$frule1 'maxima::ruleof))
            \\    (format t "RULE ~S~%" (maxima::mget 'maxima::$frule1 'maxima::$rule))))
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script = try std.fs.path.join(allocator, &.{ base, "p20-wrapper.lisp" });
    defer allocator.free(script);

    const run = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "./zig-out/bin/habu", script },
        .cwd = ".",
        .max_output_bytes = 16 * 1024,
    });
    defer allocator.free(run.stdout);
    defer allocator.free(run.stderr);

    try testing.expectEqual(.Exited, std.meta.activeTag(run.term));
    try testing.expectEqual(@as(u8, 0), run.term.Exited);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "RES ") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "OPS ") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "RULEOF ") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "RULE ") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "OPS nil") == null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "RULEOF nil") == null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "RULE nil") == null);
}

test "rtest6 problems 20 through 39 exact runner child process probe" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p20-39.mac",
        .data =
            \\(kill (f),
            \\ matchdeclare (xx, integerp),
            \\ tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))),
            \\ [f(1), f(1)(y)]);
            \\[lambda ([a], a - 1), y - 1];
            \\
            \\(remrule (f, all), 0);
            \\0;
            \\
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
            \\kill (rules);
            \\done;
            \\
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p20-39.mac" });
    defer allocator.free(path);

    const script_src = try std.fmt.allocPrint(
        allocator,
        \\(load "lib/stdlib.habu")
        \\(load "lib/maxima-manifest.lisp")
        \\(load (concatenate 'string (habu-maxima-manifest-value :srcdir) "maxima-package.lisp"))
        \\(load "lib/maxima-stubs.lisp")
        \\(load (concatenate 'string (habu-maxima-manifest-value :srcdir) "testsuite.lisp"))
        \\(load "lib/maxima-loader.lisp")
        \\(multiple-value-bind (ok total fail missing attempted)
        \\    (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total fail missing attempted)))
        \\(load "lib/maxima-post-load.lisp")
        \\(let ((*package* (find-package :maxima))
        \\      (*collect-errors* nil)
        \\      (maxima::$batch_answers_from_file t))
        \\  (multiple-value-bind (filename diff unexpected-pass total)
        \\      (test-batch "{s}" nil)
        \\    (format t "FILE ~S~%" filename)
        \\    (format t "DIFF ~S~%" diff)
        \\    (format t "UPASS ~S~%" unexpected-pass)
        \\    (format t "TOTAL ~S~%" total)))
        \\
    ,
        .{path},
    );
    defer allocator.free(script_src);

    try tmp.dir.writeFile(.{
        .sub_path = "runner.lisp",
        .data = script_src,
    });

    const script = try std.fs.path.join(allocator, &.{ base, "runner.lisp" });
    defer allocator.free(script);

    const run = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &.{ "./zig-out/bin/habu", script },
        .cwd = ".",
        .max_output_bytes = 32 * 1024,
    });
    defer allocator.free(run.stdout);
    defer allocator.free(run.stderr);

    try testing.expectEqual(.Exited, std.meta.activeTag(run.term));
    try testing.expectEqual(@as(u8, 0), run.term.Exited);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "FILE ") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "DIFF (") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "UPASS NIL") != null);
    try testing.expect(std.mem.indexOf(u8, run.stdout, "TOTAL 3") != null);
}

test "rtest6 problems 20 through 39 exact runner in-process reproduces failure" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p20-39.mac",
        .data =
            \\(kill (f),
            \\ matchdeclare (xx, integerp),
            \\ tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))),
            \\ [f(1), f(1)(y)]);
            \\[lambda ([a], a - 1), y - 1];
            \\
            \\(remrule (f, all), 0);
            \\0;
            \\
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
            \\kill (rules);
            \\done;
            \\
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p20-39.mac" });
    defer allocator.free(path);

    _ = try repl.eval("(load \"lib/maxima-manifest.lisp\")");
    _ = try repl.eval("(load (concatenate 'string (habu-maxima-manifest-value :srcdir) \"maxima-package.lisp\"))");
    _ = try repl.eval("(load \"lib/maxima-stubs.lisp\")");
    _ = try repl.eval("(load (concatenate 'string (habu-maxima-manifest-value :srcdir) \"testsuite.lisp\"))");
    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail missing attempted)
        \\    (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total fail missing attempted))
        \\  t)
    );
    _ = try repl.eval("(load \"lib/maxima-post-load.lisp\")");

    const form = try std.fmt.allocPrint(
        allocator,
        \\(let ((*package* (find-package :maxima))
        \\      (*collect-errors* nil)
        \\      (maxima::$batch_answers_from_file t))
        \\  (handler-case
        \\      (multiple-value-bind (filename diff unexpected-pass total)
        \\          (test-batch "{s}" nil)
        \\        (declare (ignore filename unexpected-pass))
        \\        (list 'ok diff total))
        \\    (condition (c)
        \\      (list 'err (write-to-string c)))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    const tag = try consAt(out, 0);
    try testing.expect(tag.isSymbol());
    const name = tag.toPtr(runtime.Symbol).getName();
    if (std.mem.eql(u8, name, "OK")) {
        const diff = try consAt(out, 1);
        const total = try consFixnumAt(out, 2);
        try testing.expect(!diff.isNil());
        try testing.expectEqual(@as(i64, 3), total);
        return;
    }
    try testing.expectEqualStrings("ERR", name);
    const msg = try consAt(out, 1);
    try testing.expect(msg.isString());
}

test "rtest6 problem 39 isolated test-batch path stays clean" {
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

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p39.mac",
        .data =
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p39.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problem 54 isolated direct forms stay clean" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (list
        \\        (eval3 "(kill (all), SPEL([rest])::= buildq([rest], buildq(splice(rest))), game_action(command,subj,obj,place,[rest])::= SPEL([command,subj,obj,place,rest], block(infix(command), command(subject,object):= block(if location = place and subject = subj and object = obj and have(subj) then apply(sconcat,rest) else sconcat(\"you cannot \",command,\" like that. \") ))), game_action(\"weld\",chain,bucket,attic, if have(bucket) and not chain_welded then (chain_welded: true, \"the chain is now securely welded to the bucket. \" ) else \"you do not have a bucket. \"), 0);")
        \\        (write-to-string
        \\          (third (with-input-from-string (st "chain weld bucket;")
        \\                   (maxima::mread st 'maxima::$eof))))
        \\        (eval3 "chain weld bucket;")))))
    );
    try snap.expectValue(@src(), out, "(0 \"(($WELD) $CHAIN $BUCKET)\" \"you cannot weld like that. \")");
}

test "rtest6 parse-string checks poison direct problem 54 setup" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);")
        \\      (eval3 "is (parse_string (string (most_negative_float)) = most_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_float)) = least_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);")
        \\      (eval3 "is (parse_string (string (float_eps ())) = float_eps ());")
        \\      (list
        \\        (eval3 "(kill (all), SPEL([rest])::= buildq([rest], buildq(splice(rest))), game_action(command,subj,obj,place,[rest])::= SPEL([command,subj,obj,place,rest], block(infix(command), command(subject,object):= block(if location = place and subject = subj and object = obj and have(subj) then apply(sconcat,rest) else sconcat(\"you cannot \",command,\" like that. \") ))), game_action(\"weld\",chain,bucket,attic, if have(bucket) and not chain_welded then (chain_welded: true, \"the chain is now securely welded to the bucket. \" ) else \"you do not have a bucket. \"), 0);")
        \\        (if (maxima::mget 'maxima::$weld 'maxima::operators) 1 0)
        \\        (write-to-string
        \\          (third (with-input-from-string (st "chain weld bucket;")
        \\                   (maxima::mread st 'maxima::$eof))))
        \\        (eval3 "chain weld bucket;")))))
    );
    try snap.expectValue(@src(), out, "(0 0 \"(($WELD) $CHAIN $BUCKET)\" \"you cannot weld like that. \")");
}

test "rtest6 let-bound parse-string restores temp-file batch-shaped problem 54 path" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (load "../maxima/share/stringproc/eval_string.lisp")
        \\    fail))
    );

    _ = try repl.eval(
        \\(let ((*package* (find-package :maxima)))
        \\  (defun parse-string (s)
        \\    (declare (special *mread-prompt* *parse-string-input-stream*))
        \\    (let ((*parse-string-input-stream*
        \\            (make-string-input-stream (ensure-terminator s)))
        \\          (*mread-prompt* ""))
        \\      (third (mread *parse-string-input-stream*)))))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "plet-54.mac",
        .data =
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "plet-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 parse-string leaves fresh streams distinct" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);")
        \\      (eval3 "is (parse_string (string (most_negative_float)) = most_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_float)) = least_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);")
        \\      (eval3 "is (parse_string (string (float_eps ())) = float_eps ());")
        \\      (let ((s (make-string-input-stream "chain weld bucket;")))
        \\        (list
        \\          (if (eq s *parse-string-input-stream*) 1 0)
        \\          (open-stream-p s)
        \\          (open-stream-p *parse-string-input-stream*))))))
    );
    try snap.expectValue(@src(), out, "(0 t t)");
}

test "rtest6 resetting parse-string-input-stream restores temp-file batch-shaped problem 54 path" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "preset-54.mac",
        .data =
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(block nil
            \\  (let ((s (make-string-input-stream "")))
            \\    (setq *parse-string-input-stream* s)
            \\    0));
            \\0;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "preset-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 no fresh-stream collision after problem 54 setup" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);")
        \\      (eval3 "is (parse_string (string (most_negative_float)) = most_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_float)) = least_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);")
        \\      (eval3 "is (parse_string (string (float_eps ())) = float_eps ());")
        \\      (list
        \\        (eval3 "(kill (all), SPEL([rest])::= buildq([rest], buildq(splice(rest))), game_action(command,subj,obj,place,[rest])::= SPEL([command,subj,obj,place,rest], block(infix(command), command(subject,object):= block(if location = place and subject = subj and object = obj and have(subj) then apply(sconcat,rest) else sconcat(\"you cannot \",command,\" like that. \") ))), game_action(\"weld\",chain,bucket,attic, if have(bucket) and not chain_welded then (chain_welded: true, \"the chain is now securely welded to the bucket. \" ) else \"you do not have a bucket. \"), 0);")
        \\        (if (maxima::mget 'maxima::$weld 'maxima::operators) 1 0)
        \\        (let ((s (make-string-input-stream "chain weld bucket;")))
        \\          (list
        \\            (if (eq s *parse-string-input-stream*) 1 0)
        \\            (open-stream-p s)
        \\            (open-stream-p *parse-string-input-stream*)))))))
    );
    try snap.expectValue(@src(), out, "(0 0 (0 t t))");
}

test "rtest6 direct reset of parse-string stream does not restore problem 54 setup" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);")
        \\      (eval3 "is (parse_string (string (most_negative_float)) = most_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_float)) = least_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);")
        \\      (eval3 "is (parse_string (string (float_eps ())) = float_eps ());")
        \\      (let ((s (make-string-input-stream "")))
        \\        (setq *parse-string-input-stream* s))
        \\      (list
        \\        (eval3 "(kill (all), SPEL([rest])::= buildq([rest], buildq(splice(rest))), game_action(command,subj,obj,place,[rest])::= SPEL([command,subj,obj,place,rest], block(infix(command), command(subject,object):= block(if location = place and subject = subj and object = obj and have(subj) then apply(sconcat,rest) else sconcat(\"you cannot \",command,\" like that. \") ))), game_action(\"weld\",chain,bucket,attic, if have(bucket) and not chain_welded then (chain_welded: true, \"the chain is now securely welded to the bucket. \" ) else \"you do not have a bucket. \"), 0);")
        \\        (if (maxima::mget 'maxima::$weld 'maxima::operators) 1 0)))))
    );
    try snap.expectValue(@src(), out, "(0 0)");
}

test "rtest6 stock parse-string dirties parser stream state" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (setq *stream-alist* nil)
        \\      (list
        \\        (length *stream-alist*)
        \\        (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\        (length *stream-alist*)
        \\        (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\        (length *stream-alist*)
        \\        (if (find-stream *parse-string-input-stream*) 1 0)))))
    );
    try snap.expectValue(@src(), out, "(0 t 2 t 3 0)");
}

test "rtest6 let-bound parse-string dirties parser stream state the same way" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    _ = try repl.eval(
        \\(let ((*package* (find-package :maxima)))
        \\  (defun parse-string (s)
        \\    (declare (special *mread-prompt* *parse-string-input-stream*))
        \\    (let ((*parse-string-input-stream*
        \\            (make-string-input-stream (ensure-terminator s)))
        \\          (*mread-prompt* ""))
        \\      (third (mread *parse-string-input-stream*)))))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (setq *stream-alist* nil)
        \\      (list
        \\        (length *stream-alist*)
        \\        (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\        (length *stream-alist*)
        \\        (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\        (length *stream-alist*)
        \\        (if (find-stream *parse-string-input-stream*) 1 0)))))
    );
    try snap.expectValue(@src(), out, "(0 t 2 t 3 0)");
}

test "rtest6 let-bound parse-string also fails direct problem 54 setup" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    _ = try repl.eval(
        \\(let ((*package* (find-package :maxima)))
        \\  (defun parse-string (s)
        \\    (declare (special *mread-prompt* *parse-string-input-stream*))
        \\    (let ((*parse-string-input-stream*
        \\            (make-string-input-stream (ensure-terminator s)))
        \\          (*mread-prompt* ""))
        \\      (third (mread *parse-string-input-stream*)))))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (flet ((eval3 (s)
        \\             (let* ((expr (third (with-input-from-string (st s) (maxima::mread st 'maxima::$eof))))
        \\                    (res (maxima::meval* (list (list 'maxima::$errcatch) expr))))
        \\               (if (maxima::$emptyp res) 'error-catch (second res)))))
        \\      (eval3 "is (parse_string (string (most_positive_float)) = most_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_float)) = least_positive_float);")
        \\      (eval3 "is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);")
        \\      (eval3 "is (parse_string (string (most_negative_float)) = most_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_float)) = least_negative_float);")
        \\      (eval3 "is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);")
        \\      (eval3 "is (parse_string (string (float_eps ())) = float_eps ());")
        \\      (list
        \\        (eval3 "(kill (all), SPEL([rest])::= buildq([rest], buildq(splice(rest))), game_action(command,subj,obj,place,[rest])::= SPEL([command,subj,obj,place,rest], block(infix(command), command(subject,object):= block(if location = place and subject = subj and object = obj and have(subj) then apply(sconcat,rest) else sconcat(\"you cannot \",command,\" like that. \") ))), game_action(\"weld\",chain,bucket,attic, if have(bucket) and not chain_welded then (chain_welded: true, \"the chain is now securely welded to the bucket. \" ) else \"you do not have a bucket. \"), 0);")
        \\        (if (maxima::mget 'maxima::$weld 'maxima::operators) 1 0)))))
    );
    try snap.expectValue(@src(), out, "(0 0)");
}

test "rtest6 problem 39 internal trace" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    const out = try repl.eval(
        \\(write-to-string
        \\  (let* ((expr (third (with-input-from-string (st "sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2);")
        \\                        (maxima::mread st 'maxima::$eof))))
        \\         (rad (maxima::$radcan expr)))
        \\    (list
        \\      expr
        \\      rad
        \\      (maxima::intsc1 0 maxima::$%pi2 expr maxima::$t)
        \\      (maxima::intsc1 0 maxima::$%pi2 rad maxima::$t)
        \\      (maxima::scprod expr maxima::$t)
        \\      (maxima::scprod rad maxima::$t))))
    );
    try snap.expectValue(@src(), out, "");
}

test "rtest6 problems 22 through 39 slice stays clean" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p22-39.mac",
        .data =
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
            \\kill (rules);
            \\done;
            \\
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p22-39.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 problems 20 through 39 slice after testsuite bootstrap reproduces failure" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 192 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    _ = try repl.eval("(load \"lib/maxima-manifest.lisp\")");
    _ = try repl.eval("(load \"../maxima/src/maxima-package.lisp\")");
    _ = try repl.eval("(load \"lib/maxima-stubs.lisp\")");
    _ = try repl.eval("(load \"../maxima/src/testsuite.lisp\")");
    _ = try repl.eval("(load \"lib/maxima-loader.lisp\")");
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total fail))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    t))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p20-39-bootstrap.mac",
        .data =
            \\(kill (f),
            \\ matchdeclare (xx, integerp),
            \\ tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))),
            \\ [f(1), f(1)(y)]);
            \\[lambda ([a], a - 1), y - 1];
            \\
            \\(remrule (f, all), 0);
            \\0;
            \\
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
            \\kill (rules);
            \\done;
            \\
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p20-39-bootstrap.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(let ((*package* (find-package :maxima))
        \\      (*collect-errors* nil)
        \\      (maxima::$batch_answers_from_file t))
        \\  (handler-case
        \\      (multiple-value-bind (filename diff unexpected-pass total)
        \\          (test-batch "{s}" nil)
        \\        (declare (ignore filename unexpected-pass))
        \\        (list 'ok diff total))
        \\    (condition (c)
        \\      (list 'err (write-to-string c)))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    const tag = try consAt(out, 0);
    try testing.expect(tag.isSymbol());
    const name = tag.toPtr(runtime.Symbol).getName();
    if (std.mem.eql(u8, name, "OK")) {
        const diff = try consAt(out, 1);
        const total = try consFixnumAt(out, 2);
        try testing.expect(!diff.isNil());
        try testing.expectEqual(@as(i64, 3), total);
        return;
    }
    try testing.expectEqualStrings("ERR", name);
    const msg = try consAt(out, 1);
    try testing.expect(msg.isString());
}

test "rtest6 problems 20 through 39 slice after testsuite-only preload reproduces failure" {
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
    _ = try repl.eval("(load \"../maxima/src/testsuite.lisp\")");
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total fail))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    t))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p20-39-testsuite-only.mac",
        .data =
            \\(kill (f),
            \\ matchdeclare (xx, integerp),
            \\ tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))),
            \\ [f(1), f(1)(y)]);
            \\[lambda ([a], a - 1), y - 1];
            \\
            \\(remrule (f, all), 0);
            \\0;
            \\
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
            \\kill (rules);
            \\done;
            \\
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p20-39-testsuite-only.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(let ((*package* (find-package :maxima))
        \\      (*collect-errors* nil)
        \\      (maxima::$batch_answers_from_file t))
        \\  (handler-case
        \\      (multiple-value-bind (filename diff unexpected-pass total)
        \\          (test-batch "{s}" nil)
        \\        (declare (ignore filename unexpected-pass))
        \\        (list 'ok diff total))
        \\    (condition (c)
        \\      (list 'err (write-to-string c)))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    const tag = try consAt(out, 0);
    try testing.expect(tag.isSymbol());
    const name = tag.toPtr(runtime.Symbol).getName();
    if (std.mem.eql(u8, name, "OK")) {
        const diff = try consAt(out, 1);
        const total = try consFixnumAt(out, 2);
        try testing.expect(!diff.isNil());
        try testing.expectEqual(@as(i64, 3), total);
        return;
    }
    try testing.expectEqualStrings("ERR", name);
    const msg = try consAt(out, 1);
    try testing.expect(msg.isString());
}

test "rtest6 problems 20 through 39 slice after tiny testsuite assignment stays clean" {
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
    _ = try repl.eval(
        \\(let ((*package* (find-package :maxima)))
        \\  (setq $testsuite_files '((mlist simp) "rtest6"))
        \\  (setq $share_testsuite_files '((mlist simp)))
        \\  t)
    );
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total fail))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    t))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p20-39-tiny-testsuite.mac",
        .data =
            \\(kill (f),
            \\ matchdeclare (xx, integerp),
            \\ tellsimp (f(xx), subst ('xx = xx, lambda ([a], a - xx))),
            \\ [f(1), f(1)(y)]);
            \\[lambda ([a], a - 1), y - 1];
            \\
            \\(remrule (f, all), 0);
            \\0;
            \\
            \\(matchdeclare ([xx, yy], integerp),
            \\ tellsimp (f(xx)(yy), yy*xx),
            \\ [f(2), f(2)(3)]);
            \\[f(2), 6];
            \\
            \\kill (rules);
            \\done;
            \\
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p20-39-tiny-testsuite.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(let ((*package* (find-package :maxima))
        \\      (*collect-errors* nil)
        \\      (maxima::$batch_answers_from_file t))
        \\  (handler-case
        \\      (multiple-value-bind (filename diff unexpected-pass total)
        \\          (test-batch "{s}" nil)
        \\        (declare (ignore filename unexpected-pass))
        \\        (list 'ok diff total))
        \\    (condition (c)
        \\      (list 'err (write-to-string c)))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    const tag = try consAt(out, 0);
    try testing.expect(tag.isSymbol());
    try testing.expectEqualStrings("OK", tag.toPtr(runtime.Symbol).getName());
    const diff = try consAt(out, 1);
    const total = try consFixnumAt(out, 2);
    try testing.expect(diff.isNil());
    try testing.expectEqual(@as(i64, 3), total);
}

test "rtest6 problems 42 through 54 slice reproduces current failure" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p42-54.mac",
        .data =
            \\string (25.0);
            \\"25.0";
            \\
            \\string(1/16.0);
            \\"0.0625";
            \\
            \\(string(2e7), %% = "2.0e+7" or %% = "2.0E+7" or %% = "2.0e7" or %% = "2.0E7" or %%);
            \\true;
            \\
            \\(string(2e-7), %% = "2.0e-7" or %% = "2.0E-7" or %%);
            \\true;
            \\
            \\(string(12345000000.0), %% = "1.2345e+10" or %% = "1.2345E+10" or %% = "1.2345e10" or %% = "1.2345E10" or %%);
            \\true;
            \\
            \\(string(1/1024.0), %% = "9.765625e-4" or %% = "9.765625E-4" or %%);
            \\true;
            \\
            \\(reset (fpprintprec), 0);
            \\0;
            \\
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p42-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isFixnum());
    try testing.expectEqual(@as(i64, 54), cell.car.toFixnum());
}

test "rtest6 float-string prefix before 54 stays clean" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p42a-54.mac",
        .data =
            \\string (25.0);
            \\"25.0";
            \\
            \\string(1/16.0);
            \\"0.0625";
            \\
            \\(string(2e7), %% = "2.0e+7" or %% = "2.0E+7" or %% = "2.0e7" or %% = "2.0E7" or %%);
            \\true;
            \\
            \\(string(2e-7), %% = "2.0e-7" or %% = "2.0E-7" or %%);
            \\true;
            \\
            \\(string(12345000000.0), %% = "1.2345e+10" or %% = "1.2345E+10" or %% = "1.2345e10" or %% = "1.2345E10" or %%);
            \\true;
            \\
            \\(string(1/1024.0), %% = "9.765625e-4" or %% = "9.765625E-4" or %%);
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p42a-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 parse-string prefix before 54 reproduces current failure" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p42b-54.mac",
        .data =
            \\(reset (fpprintprec), 0);
            \\0;
            \\
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p42b-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isFixnum());
    try testing.expectEqual(@as(i64, 54), cell.car.toFixnum());
}

test "rtest6 reset-only prefix before 54 stays clean" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "preset-54.mac",
        .data =
            \\(reset (fpprintprec), 0);
            \\0;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "preset-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 parse-string checks before 54 reproduce current failure" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "pparse-54.mac",
        .data =
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "pparse-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isFixnum());
    try testing.expectEqual(@as(i64, 54), cell.car.toFixnum());
}

test "rtest6 problems 39 through 54 slice reproduces current failure" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p39-54.mac",
        .data =
            \\(kill(t, R), integrate(sqrt(sin(t)^2*R^2+(1-cos(t))^2*R^2),t,0,2*%pi));
            \\8*R;
            \\
            \\string (25.0);
            \\"25.0";
            \\
            \\string(1/16.0);
            \\"0.0625";
            \\
            \\(string(2e7), %% = "2.0e+7" or %% = "2.0E+7" or %% = "2.0e7" or %% = "2.0E7" or %%);
            \\true;
            \\
            \\(string(2e-7), %% = "2.0e-7" or %% = "2.0E-7" or %%);
            \\true;
            \\
            \\(string(12345000000.0), %% = "1.2345e+10" or %% = "1.2345E+10" or %% = "1.2345e10" or %% = "1.2345E10" or %%);
            \\true;
            \\
            \\(string(1/1024.0), %% = "9.765625e-4" or %% = "9.765625E-4" or %%);
            \\true;
            \\
            \\(reset (fpprintprec), 0);
            \\0;
            \\
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p39-54.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(multiple-value-bind (ok total fail) (cl-user::maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (progn
        \\            (test-batch "{s}" nil)
        \\            '(ok))
        \\        (condition (c)
        \\          (list fail (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isFixnum());
    try testing.expectEqual(@as(i64, 54), cell.car.toFixnum());
}

test "rtest6 problem 54 custom batch loop trace" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    fail))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p42-54-trace.mac",
        .data =
            \\string (25.0);
            \\"25.0";
            \\
            \\string(1/16.0);
            \\"0.0625";
            \\
            \\(string(2e7), %% = "2.0e+7" or %% = "2.0E+7" or %% = "2.0e7" or %% = "2.0E7" or %%);
            \\true;
            \\
            \\(string(2e-7), %% = "2.0e-7" or %% = "2.0E-7" or %%);
            \\true;
            \\
            \\(string(12345000000.0), %% = "1.2345e+10" or %% = "1.2345E+10" or %% = "1.2345e10" or %% = "1.2345E10" or %%);
            \\true;
            \\
            \\(string(1/1024.0), %% = "9.765625e-4" or %% = "9.765625E-4" or %%);
            \\true;
            \\
            \\(reset (fpprintprec), 0);
            \\0;
            \\
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p42-54-trace.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima)))
        \\    (let ((*collect-errors* nil)
        \\          (maxima::$batch_answers_from_file t))
        \\      (handler-case
        \\          (multiple-value-list (test-batch "{s}" nil))
        \\        (condition (c)
        \\          (list 'err (write-to-string c)))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try snap.expectValue(@src(), out, "X");
}

test "p54batchtuple" {
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
    _ = try repl.eval(
        \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
        \\  (declare (ignore ok total fail))
        \\  (let ((*package* (find-package :maxima)))
        \\    (load "lib/maxima-post-load.lisp")
        \\    t))
    );

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.writeFile(.{
        .sub_path = "p54-batch-tuple.mac",
        .data =
            \\string (25.0);
            \\"25.0";
            \\
            \\string(1/16.0);
            \\"0.0625";
            \\
            \\(string(2e7), %% = "2.0e+7" or %% = "2.0E+7" or %% = "2.0e7" or %% = "2.0E7" or %%);
            \\true;
            \\
            \\(string(2e-7), %% = "2.0e-7" or %% = "2.0E-7" or %%);
            \\true;
            \\
            \\(string(12345000000.0), %% = "1.2345e+10" or %% = "1.2345E+10" or %% = "1.2345e10" or %% = "1.2345E10" or %%);
            \\true;
            \\
            \\(string(1/1024.0), %% = "9.765625e-4" or %% = "9.765625E-4" or %%);
            \\true;
            \\
            \\(reset (fpprintprec), 0);
            \\0;
            \\
            \\is (parse_string (string (most_positive_float)) = most_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_float)) = least_positive_float);
            \\true;
            \\
            \\is (parse_string (string (least_positive_normalized_float)) = least_positive_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (most_negative_float)) = most_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_float)) = least_negative_float);
            \\true;
            \\
            \\is (parse_string (string (least_negative_normalized_float)) = least_negative_normalized_float);
            \\true;
            \\
            \\is (parse_string (string (float_eps ())) = float_eps ());
            \\true;
            \\
            \\(kill (all),
            \\SPEL([rest])::= buildq(
            \\  [rest],
            \\  buildq(splice(rest)) ),
            \\game_action(command,subj,obj,place,[rest])::= SPEL(
            \\  [command,subj,obj,place,rest],
            \\  block(
            \\     infix(command),
            \\     command(subject,object):= block(
            \\        if location = place
            \\           and subject = subj
            \\           and object = obj
            \\           and have(subj) then apply(sconcat,rest)
            \\        else sconcat("you cannot ",command," like that. ") ))),
            \\game_action("weld",chain,bucket,attic,
            \\  if have(bucket)
            \\  and not chain_welded then (
            \\     chain_welded: true,
            \\     "the chain is now securely welded to the bucket. " )
            \\  else "you do not have a bucket. "),
            \\0);
            \\0;
            \\
            \\chain weld bucket;
            \\"you cannot weld like that. ";
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const path = try std.fs.path.join(allocator, &.{ base, "p54-batch-tuple.mac" });
    defer allocator.free(path);

    const form = try std.fmt.allocPrint(
        allocator,
        \\(write-to-string
        \\  (let ((*package* (find-package :maxima))
        \\        (*collect-errors* nil)
        \\        (maxima::$batch_answers_from_file t))
        \\    (handler-case
        \\        (multiple-value-list (test-batch "{s}" nil))
        \\      (condition (c)
        \\        (list 'err (write-to-string c))))))
    ,
        .{path},
    );
    defer allocator.free(form);

    const out = try repl.eval(form);
    try snap.expectValue(@src(), out, "X");
}

test "rtest6 canonical runner harness repro" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();
    try loadStdlib(&repl);

    const out = try repl.eval(
        \\(let ((*command-line-args* '("rtest6")))
        \\  (handler-case
        \\      (progn
        \\        (load "tools/maxima-rtest.lisp")
        \\        '(ok))
        \\    (condition (c)
        \\      (list 'err (write-to-string c)))))
    );

    try testing.expect(out.isCons());
    const cell = out.toPtr(Cons);
    try testing.expect(cell.car.isSymbol());
    try testing.expectEqualStrings("OK", cell.car.toPtr(runtime.Symbol).getName());
}

test "rtest6 canonical runner direct loadFile repro" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile("lib/stdlib.habu", stream.writer());
    try repl.publishCommandLineArgs(
        &.{ "rtest6" },
        &.{ "habu", "tools/maxima-rtest.lisp", "rtest6" },
    );
    try repl.addTrustedLoadRootForFile("tools/maxima-rtest.lisp");
    try repl.loadFile("tools/maxima-rtest.lisp", stream.writer());
    const written = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, written, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, written, "canonical test-batch failed") == null);
    const name = try repl.eval("*habu-rtest-name*");
    try testing.expect(name.isString());
    try testing.expectEqualStrings("rtest6", name.toPtr(runtime.String).bytes());
}

test "rtest6 canonical runner direct loadFile repro on gpa" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile("lib/stdlib.habu", stream.writer());
    try repl.publishCommandLineArgs(
        &.{ "rtest6" },
        &.{ "habu", "tools/maxima-rtest.lisp", "rtest6" },
    );
    try repl.addTrustedLoadRootForFile("tools/maxima-rtest.lisp");
    try repl.loadFile("tools/maxima-rtest.lisp", stream.writer());
    const written = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, written, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, written, "canonical test-batch failed") == null);
    const name = try repl.eval("*habu-rtest-name*");
    try testing.expect(name.isString());
    try testing.expectEqualStrings("rtest6", name.toPtr(runtime.String).bytes());
}

test "rtest6 canonical runner wrapper loadFile repro" {
    try ensureMaximaSources();

    const allocator = testing.allocator;
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile("lib/stdlib.habu", stream.writer());
    try repl.publishCommandLineArgs(
        &.{ "rtest6" },
        &.{ "habu", "wrapper.lisp", "rtest6" },
    );

    const root = try std.process.getCwdAlloc(allocator);
    defer allocator.free(root);
    const target = try std.fs.path.join(allocator, &.{ root, "tools", "maxima-rtest.lisp" });
    defer allocator.free(target);

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();
    const wrapper_src = try std.fmt.allocPrint(allocator, "(load \"{s}\")\n", .{target});
    defer allocator.free(wrapper_src);
    try tmp.dir.writeFile(.{
        .sub_path = "wrapper.lisp",
        .data = wrapper_src,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const wrapper_abs = try std.fs.path.join(allocator, &.{ base, "wrapper.lisp" });
    defer allocator.free(wrapper_abs);

    try repl.addTrustedLoadRoot(".");
    try repl.addTrustedLoadRootForFile(wrapper_abs);
    try repl.loadFile(wrapper_abs, stream.writer());
    const name = try repl.eval("*habu-rtest-name*");
    try testing.expect(name.isString());
    try testing.expectEqualStrings("rtest6", name.toPtr(runtime.String).bytes());
}

test "rtest6 canonical runner wrapper loadFile repro on spawned thread" {
    const Ctx = struct {
        err: ?anyerror = null,

        fn run(self: *@This()) void {
            self.runInner() catch |err| {
                self.err = err;
            };
        }

        fn runInner(self: *@This()) !void {
            _ = self;
            try ensureMaximaSources();

            const allocator = testing.allocator;
            var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
            defer heap.deinit();

            var repl: Repl = undefined;
            try repl.init(allocator, &heap, .{});
            defer repl.deinit();
            try repl.wireGlobalEnv();

            var buf: [4096]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            try repl.loadFile("lib/stdlib.habu", stream.writer());
            try repl.publishCommandLineArgs(
                &.{ "rtest6" },
                &.{ "habu", "wrapper.lisp", "rtest6" },
            );

            const root = try std.process.getCwdAlloc(allocator);
            defer allocator.free(root);
            const target = try std.fs.path.join(allocator, &.{ root, "tools", "maxima-rtest.lisp" });
            defer allocator.free(target);

            var tmp = testing.tmpDir(.{});
            defer tmp.cleanup();
            const wrapper_src = try std.fmt.allocPrint(allocator, "(load \"{s}\")\n", .{target});
            defer allocator.free(wrapper_src);
            try tmp.dir.writeFile(.{
                .sub_path = "wrapper.lisp",
                .data = wrapper_src,
            });

            const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
            defer allocator.free(base);
            const wrapper_abs = try std.fs.path.join(allocator, &.{ base, "wrapper.lisp" });
            defer allocator.free(wrapper_abs);

            try repl.addTrustedLoadRoot(".");
            try repl.addTrustedLoadRootForFile(wrapper_abs);
            try repl.loadFile(wrapper_abs, stream.writer());
            const name = try repl.eval("*habu-rtest-name*");
            try testing.expect(name.isString());
            try testing.expectEqualStrings("rtest6", name.toPtr(runtime.String).bytes());
        }
    };

    var ctx = Ctx{};
    const thread = try std.Thread.spawn(.{ .stack_size = 512 * 1024 * 1024 }, Ctx.run, .{&ctx});
    thread.join();
    if (ctx.err) |err| return err;
}

test "rtest6 canonical runner absolute load repro" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer heap.deinit();

    var repl: Repl = undefined;
    try repl.init(allocator, &heap, .{});
    defer repl.deinit();
    try repl.wireGlobalEnv();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try repl.loadFile("lib/stdlib.habu", stream.writer());
    try repl.publishCommandLineArgs(
        &.{ "rtest6" },
        &.{ "habu", "tools/maxima-rtest.lisp", "rtest6" },
    );
    try repl.addTrustedLoadRootForFile("tools/maxima-rtest.lisp");

    const abs = try std.fs.path.resolve(allocator, &.{"tools/maxima-rtest.lisp"});
    defer allocator.free(abs);
    const form = try std.fmt.allocPrint(allocator, "(load \"{s}\")", .{abs});
    defer allocator.free(form);
    _ = try repl.eval(form);

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
}

test "rtest6 canonical runner exact main script path repro" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "habu", "tools/maxima-rtest.lisp", "rtest6" },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
}

test "rtest6 canonical runner exact main script path repro on spawned thread" {
    const Ctx = struct {
        err: ?anyerror = null,

        fn run(self: *@This()) void {
            self.runInner() catch |err| {
                self.err = err;
            };
        }

        fn runInner(self: *@This()) !void {
            _ = self;
            try ensureMaximaSources();

            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            const allocator = gpa.allocator();

            var buf: [4096]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            try script_run.run(
                allocator,
                256 * 1024 * 1024,
                &.{ "habu", "tools/maxima-rtest.lisp", "rtest6" },
                stream.writer(),
            );

            const out = stream.getWritten();
            try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
            try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
        }
    };

    var ctx = Ctx{};
    const thread = try std.Thread.spawn(.{ .stack_size = 512 * 1024 * 1024 }, Ctx.run, .{&ctx});
    thread.join();
    if (ctx.err) |err| return err;
}

test "rtest6 canonical runner full main script mode repro" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var outer_heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 * 1024 });
    defer outer_heap.deinit();

    var outer_repl: Repl = undefined;
    try outer_repl.init(allocator, &outer_heap, .{});
    defer outer_repl.deinit();
    try outer_repl.wireGlobalEnv();

    var outer_buf: [4096]u8 = undefined;
    var outer_stream = std.io.fixedBufferStream(&outer_buf);
    try outer_repl.loadFile("lib/stdlib.habu", outer_stream.writer());

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "habu", "tools/maxima-rtest.lisp", "rtest6" },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
}

test "rtest6 canonical runner exact main script path repro with external argv0" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "./zig-out/bin/habu", "tools/maxima-rtest.lisp", "rtest6" },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
}

test "rtest6 canonical runner exact main script path repro on tight heap" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        16 * 1024 * 1024,
        &.{ "./zig-out/bin/habu", "tools/maxima-rtest.lisp", "rtest6" },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
}

test "rtest6 canonical runner exact main script path repro with allocated argv" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const argv = try allocator.alloc([]const u8, 3);
    defer allocator.free(argv);
    argv[0] = try allocator.dupe(u8, "./zig-out/bin/habu");
    defer allocator.free(argv[0]);
    argv[1] = try allocator.dupe(u8, "tools/maxima-rtest.lisp");
    defer allocator.free(argv[1]);
    argv[2] = try allocator.dupe(u8, "rtest6");
    defer allocator.free(argv[2]);

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        argv,
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "[HABU-RTEST] file=") != null);
    try testing.expect(std.mem.indexOf(u8, out, "canonical test-batch failed") == null);
}

test "rtest6 problem 2 direct script_run path returns beta" {
    try ensureMaximaSources();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    try tmp.dir.writeFile(.{
        .sub_path = "p2-direct.lisp",
        .data =
            \\(load "lib/maxima-loader.lisp")
            \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
            \\  (declare (ignore ok total fail))
            \\  (let ((*package* (find-package :maxima)))
            \\    (load "lib/maxima-post-load.lisp")
            \\    (with-input-from-string (s "integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);")
            \\      (let* ((form (maxima::mread s 'maxima::$eof))
            \\             (res (maxima::meval* (list (list 'maxima::$errcatch) (third form))))
            \\             (val (if (maxima::$emptyp res) 'error-catch (second res))))
            \\        (format t "VAL ~S~%" val)))))
            \\
        ,
    });

    const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
    defer allocator.free(base);
    const script_abs = try std.fs.path.join(allocator, &.{ base, "p2-direct.lisp" });
    defer allocator.free(script_abs);

    var buf: [4096]u8 = undefined;
    var stream = std.io.fixedBufferStream(&buf);
    try script_run.run(
        allocator,
        256 * 1024 * 1024,
        &.{ "./zig-out/bin/habu", script_abs },
        stream.writer(),
    );

    const out = stream.getWritten();
    try testing.expect(std.mem.indexOf(u8, out, "ERROR-CATCH") == null);
    try testing.expect(std.mem.indexOf(u8, out, "BETA") != null);
}

test "rtest6 problem 2 direct script_run path returns beta on spawned thread" {
    const Ctx = struct {
        err: ?anyerror = null,

        fn run(self: *@This()) void {
            self.runInner() catch |err| {
                self.err = err;
            };
        }

        fn runInner(self: *@This()) !void {
            _ = self;
            try ensureMaximaSources();

            var gpa = std.heap.GeneralPurposeAllocator(.{}){};
            defer _ = gpa.deinit();
            const allocator = gpa.allocator();

            var tmp = testing.tmpDir(.{});
            defer tmp.cleanup();

            try tmp.dir.writeFile(.{
                .sub_path = "p2-direct-thread.lisp",
                .data =
                    \\(load "lib/maxima-loader.lisp")
                    \\(multiple-value-bind (ok total fail) (maxima-load-all :verbose nil :habu-stop-on-error t)
                    \\  (declare (ignore ok total fail))
                    \\  (let ((*package* (find-package :maxima)))
                    \\    (load "lib/maxima-post-load.lisp")
                    \\    (with-input-from-string (s "integrate(x^(5/4)/(x+1)^(5/2),x,0,inf);")
                    \\      (let* ((form (maxima::mread s 'maxima::$eof))
                    \\             (res (maxima::meval* (list (list 'maxima::$errcatch) (third form))))
                    \\             (val (if (maxima::$emptyp res) 'error-catch (second res))))
                    \\        (format t "VAL ~S~%" val)))))
                    \\
                ,
            });

            const base = try tmp.parent_dir.realpathAlloc(allocator, &tmp.sub_path);
            defer allocator.free(base);
            const script_abs = try std.fs.path.join(allocator, &.{ base, "p2-direct-thread.lisp" });
            defer allocator.free(script_abs);

            var buf: [4096]u8 = undefined;
            var stream = std.io.fixedBufferStream(&buf);
            try script_run.run(
                allocator,
                256 * 1024 * 1024,
                &.{ "./zig-out/bin/habu", script_abs },
                stream.writer(),
            );

            const out = stream.getWritten();
            try testing.expect(std.mem.indexOf(u8, out, "ERROR-CATCH") == null);
            try testing.expect(std.mem.indexOf(u8, out, "BETA") != null);
        }
    };

    var ctx = Ctx{};
    const thread = try std.Thread.spawn(.{ .stack_size = 512 * 1024 * 1024 }, Ctx.run, .{&ctx});
    thread.join();
    if (ctx.err) |err| return err;
}
