---
title: Keep exported code across undefine in verify replay
status: open
priority: 3
issue-type: task
created-at: "2026-10-25T02:12:52.167872+01:00"
---

Problem: EXPORT of a word, then undefine of its source (with or without redefinition), then use of the alias: verify follows the alias to the current global word (LIBRARY-DEFINER? via SYM-SOURCE) and can certify a use the load refuses (the load binds the alias to the exported code). Documented as a known gap above LIBRARY-DEFINER? in src/habu/verify-source.f. Acceptance: the verify replay's export record keeps the exported code; a test/ffi-struct-verify.f row (EXPORT STRUCT-ARG, undefine, use alias in FUNCTION:) is refused by both check and load with the same code; the known-gap comment is removed. Verify: test/ffi-struct-verify.f via src/lib/ffi-test.f, tools/check-verify-test.f.
