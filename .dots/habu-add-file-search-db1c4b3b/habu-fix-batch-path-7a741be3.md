---
title: Fix batch path after search-list bootstrap
status: closed
priority: 1
issue-type: task
created-at: "\"2026-03-07T09:35:52.720718+01:00\""
closed-at: "2026-03-07T19:34:17.889259+01:00"
close-reason: superseded by habu-delegate-composite-stream-9553a1f8 + habu-finish-composite-stream-638bd128 + habu-restore-real-dbm-d050da7f + habu-adopt-canonical-test-a8a0cbe4
blocks:
  - habu-bootstrap-maxima-src-af3aa99d
---

lib/maxima-stubs.lisp:307-330; lib/maxima-post-load.lisp:60-90; ../maxima/src/macsys.lisp:163-240; ../maxima/src/mload.lisp:194-210. After search lists exist,  still fails in continue/dbm-read and stream-name/get-stream-truename paths. Make batch(test-file) work for rtest1/testsuite.
