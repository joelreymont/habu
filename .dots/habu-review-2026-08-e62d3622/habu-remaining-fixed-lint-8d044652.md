---
title: remaining fixed lint buffers and a nameless token-table death
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:47:07.142262+02:00"
---

Problem: tools/lint/token.f:4-5 TMAX 65,536 tokens with checker.f at 45,838 (30% margin), TOKEN-ENSURE dies without naming the file, and the three tables cost 1.5 MB static in 13 gate files; tools/repl-lint-core.f:5 $8000; namespace-lint-core.f:40 262,144; dot-dep-lint-core.f:10 262,144; stdin-closure-lint.f:25 262,144; schedule-lint.f:186 FILE-CAP 1 MiB (44% margin on checker.f) and :193 REACH-CAP 65,536 (41%); error-code-lint-core.f:76-77 2,048 claims / 1,024 reservations; tools/lint/shadow-lint.f:41-47 PNAMES 8192 / PMAX 512 written by ADD-PRIM with no bound check; hb-build-lib.f:37 HBB-CAPTURE-CAP 65,536 for child output; tools/bootstrap-mirror-lint.f SEED-BUF $8000 for tools/bootstrap.sh (~15 KiB, added 2026-08-22, dies named); tools/error-code-lint-core.f:76 MAX-CLAIMS 2048 at 1113 claims today (the same shape as the deleted INTERN-MAX: a number on a growing table, its comment says a full table dies rather than certify a partial ledger); tools/lint/intern.f INTERN-FOLD-CAP $100 fixed fold buffer (a token over 256 bytes throws E-LINT-INTERN-CAP; pinned by set-test since 2026-08-23). Acceptance: TOKENIZE's lint consumers (maki-dep, namespace, clobber, shadow prim pass, repl) move to LINT-LEX and token.f goes, or TOKEN-ENSURE carries the path; every fixed read buffer becomes LINT-SLAB or a FILE-SIZE allocation; ADD-PRIM bounded. Files: tools/lint/token.f and the listed lints. Verify: each lint's test; a probe past each old cap. Depends: none. Ownership: lints. Claim: unassigned.
