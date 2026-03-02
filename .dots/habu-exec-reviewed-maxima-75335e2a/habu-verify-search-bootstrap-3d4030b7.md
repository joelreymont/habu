---
title: Verify search/bootstrap and share resolution
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-08T17:08:50.301002+01:00\""
closed-at: "2026-03-08T18:54:12.339493+01:00"
close-reason: "done: verified current search/bootstrap path reaches share-backed tests. /tmp/maxima_rtest_select_probe.lisp proved testsuite entry + file_search resolution for rtest_stringproc under maxima-load-all + maxima-post-load, and direct tools/maxima-rtest.lisp rtest_stringproc executed ../maxima/share/stringproc/rtest_stringproc.mac through canonical test-batch."
---

Files: lib/maxima-post-load.lisp:223-307, src/runtime/primitives/io.zig:1856-2093,2634-2708, lib/maxima-loader.lisp:27-88. What: probe current search lists, recursive share/** resolution, and composite-stream behavior used by test-batch/query I/O. Why: old plan items were stale; need closure evidence. Verification: direct probe scripts resolving nested share targets and exercising composite-stream read paths under habu.
