---
title: Verify canonical rtest runner
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-08T17:08:50.295392+01:00\\\"\""
closed-at: "2026-03-08T18:54:12.334652+01:00"
close-reason: "done: fixed src/main.zig script entrypoint semantics so first CLI arg is loaded as script and remaining args are exposed via *command-line-args*. Validation: /tmp/argv_probe.lisp now sees (/tmp/argv_probe.lisp alpha beta); /tmp/maxima_rtest_select_probe.lisp resolves rtest1 -> ../maxima/tests/rtest1.mac and rtest_stringproc -> ../maxima/share/stringproc/rtest_stringproc.mac; direct tools/maxima-rtest.lisp rtest_stringproc now runs share-backed canonical test-batch instead of treating the test name as a file."
---

Files: tools/maxima-rtest.lisp:1-83, ../maxima/src/mload.lisp:379-509, ../maxima/src/testsuite.lisp:1-329. What: verify current run-rtest path still matches canonical test-batch semantics (expected-failure metadata, answers-from-file behavior, canonical diff counts). Why: PLAN 3.1/4.1 treats this as already-landed infra that must be verified before deeper correctness work. Verification: direct ./zig-out/bin/habu tools/maxima-rtest.lisp rtest1 and one share-backed sample; confirm test-batch-style summary.
