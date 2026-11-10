---
title: Keep exported code across undefine in verify replay
status: closed
priority: 3
issue-type: task
created-at: "2026-10-25T02:12:52.167872+01:00"
closed-at: "2026-11-10T21:50:20.817995+02:00"
close-reason: merged into habu-check-src-through-1198f4ed
---

Problem: EXPORT of a word, then undefine of its source (with or without redefinition), then use of the alias: verify follows the alias to the current global word (LIBRARY-DEFINER? via SYM-SOURCE) and can certify a use the load refuses (the load binds the alias to the exported code). Documented as a known gap above LIBRARY-DEFINER? in src/habu/verify-source.f. Acceptance: the verify replay's export record keeps the exported code; a test/ffi-struct-verify.f row (EXPORT STRUCT-ARG, undefine, use alias in FUNCTION:) is refused by both check and load with the same code; the known-gap comment is removed. Verify: test/ffi-struct-verify.f via src/lib/ffi-test.f, tools/check-verify-test.f.

Census 2026-11-04: a disagreement between two implementations of one thing; it is closed by its parent removing the second implementation, not by patching that copy.
