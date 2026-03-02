---
title: Triage isolated rtest_stringproc failures
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-08T18:54:12.342644+01:00\\\"\""
closed-at: "2026-03-08T22:13:13.901758+01:00"
close-reason: done (canonical rtest_stringproc now passes 87/87; validation beyond stringproc pending)
---

Canonical share-backed runner now works via src/main.zig script-arg fix. Triage earliest failures in tools/maxima-rtest.lisp rtest_stringproc: problem 8 supcase start/end error, problem 11/14 printf/format iteration output, problems 17-26 ascii/character classification helpers, problem 30/31 parse_string numeric edge cases. Likely files: src/runtime/primitives/string.zig, src/runtime/primitives/io.zig, src/interp/vm.zig, lib/stdlib.habu, ../maxima/share/stringproc/*.lisp. Use focused probes that match the failing rtest forms before patching.
