---
title: Implement open/pathname semantics for append/save/test logs
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.555928+01:00\\\"\""
closed-at: "2026-03-07T22:20:10.153854+01:00"
close-reason: done (implemented generic CL-level open/file-position/file-length behavior needed for append/supersede paths; validated with direct ./zig-out/bin/habu append probe showing with-open-file writes preserve existing content and append new lines)
blocks:
  - habu-implement-recursive-wildcard-32f96a70
---

lib/stdlib.habu:6189-6199; src/runtime/primitives/io.zig:2206-2227; ../maxima/src/dskfn.lisp; ../maxima/src/mload.lisp:379-509. Root cause: open ignores :if-exists/:if-does-not-exist and append-mode paths silently truncate. Fix: honor at least the append/supersede cases used by save/test-batch. Why: rtest2 save/load and testsuite logging need correct file semantics.
