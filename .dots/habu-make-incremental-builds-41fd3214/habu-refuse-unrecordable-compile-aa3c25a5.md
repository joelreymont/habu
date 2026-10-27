---
title: Refuse unrecordable compile-time reads
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T17:12:02.971279+03:00"
---

Problem: a file whose compile-time code reads earlier data through a raw address, a TRUSTED: body outside an owner interface, or an untracked foreign or system call cannot list its consumed facts. Acceptance: such a file stores no object and the build names the word (docs/architecture.md "Builds"); every engine and Dots file the frame census reports ordered=1 (native-file: frame lines from the frame dot, with reason and word) whose compile-time code reads an earlier file's state is converted to a typed read, a dictionary record or a declared input, so no engine file is refused. Rule: a checked word's contract is its interface, so the observer marks a file ordered only when a word its top level or an immediate executes directly is itself unchecked (TRUSTED: body, compiled with checking off) or foreign; checked loader and defining words calling trusted helpers stay recordable. Hook: NHOST:SELECT-REC (src/compiler/native/host.f), called from RUN-FOUND (src/habu/outer.f) for top-level words and DEF-RUN (src/habu/definers.f) for immediates; NHOST reasons today keep only the first reason and give constants/variables/defers UNKNOWN, so the observer needs the executed word's own tier, not its first closure reason. Verify: an engine build with HABU_NATIVE_CACHE_DIAG=1 shows zero ordered=1 engine files; native-file-frame e2e refusal case. Design: docs/architecture.md "Builds"; working notes ~/.cache/tmp/carl-file/design.md. Depends: frame dot. Claim: unassigned.

Progress 2026-10-27 (master bd24d8af): unchecked words with a checker contract are admitted at any depth, and a run that reaches one marks its file unchecked so its undeclared DATA is judged; a compiled `is` (REBIND) and the unset-defer sentinel are admitted. Warm engine census (memory.f comment edit, carl-bat3.zES7): reused 8, compiled 14121, 87 s; warm equals cold byte for byte. Ordered-run holders now: NULL-PTR-CELL run-host-cast 34, DECLARE run-host-unknown 12, seed write 10, ptr-cell-mark 9, TF-PK-P mapped-compile-time-read 8, ndict@ 4, DATA-CELL cast 4.
