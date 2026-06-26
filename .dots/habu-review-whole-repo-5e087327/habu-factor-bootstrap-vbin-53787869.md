---
title: Factor bootstrap VBIN prep
status: closed
priority: 1
issue-type: task
created-at: "\"2026-06-25T12:19:43.494798+02:00\""
closed-at: "2026-06-25T12:57:00.357306+02:00"
close-reason: "completed: factored mirrored VBIN prep helpers in src/habu/jit.f and bootstrap/cg/jit.fs in commit c3deb86c. Evidence: tools/bootstrap-codegen-test passed; trust-lint reported 236 TRUST site(s), 318 manifest row(s), 0 finding(s); full native gate passed with fixpoint, engine suite, checked hb, REPL, and hb-build; no-binary recovery launcher exited 69 before touching bin/hb because local gforth 0.7.3 fails the documented {: :} locals probe."
---

Finding F01. Evidence: docs/factorization-review.md:29; bootstrap/cg/jit.fs:240 duplicates bootstrap/cg/jit.fs:211. Root cause: EMIT-VBINIPREP copy-expands EMIT-VBINPREP logic for depth checks, VS tag/value loads, force paths, rm-bit free/pop, and fold loading. Fix: extract emitted helpers for VS tag/value access, forced top/deep handling, rm-bit free/pop, and fold loading; make EMIT-VBINPREP and EMIT-VBINIPREP small mode selectors. Why: removes manual stack/register reasoning in a high-risk bootstrap JIT path. Validate with bootstrap-codegen-test, native fixpoint, and full native gate.

Handoff state 2026-06-25: an uncommitted implementation has started in
src/habu/jit.f and bootstrap/cg/jit.fs, adding C-VBIN-FRAME, C-VBIN-TAGS,
C-VBIN-REG-PATH, C-VBIN-FOLD-PATH, C-VBIN-RET, and C-VBINI-IMM12-PATH. It has
not been validated. Before closing this dot, inspect the source diff, run
tools/bootstrap-codegen-test.f, run the native fixpoint/full gate through
test/run.f, and record no-binary recovery evidence or the local Gforth locals
probe blocker.
