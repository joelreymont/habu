---
title: die instead of throw on user-reachable checker inputs
status: open
priority: 2
issue-type: task
created-at: "2026-08-22T22:38:25.867129+02:00"
---

Problem: checker.f:3057 CT-ADD-LINEAR '70 die' (reused DEFLINEAR name), :3540 CHECKER-DEFRECORD '70 die', :3529-3535 value-record field errors, :6664 CHECKER-PACKAGE-COPY '76 die' (package name >= 256), :8617 PTX-BARRIER! '76 die', type-family.f:3216 TFQ-FOLD-COPY '76 die', render.f:25 RSBUF-CAP and :61 RDIAG-APPEND '76 die'. docs/forth.md:936-939: interactive/REPL paths recover by throw. Each ends the REPL, an --all-errors run and tools/check.f with no rollback. Acceptance: each becomes a named throw the hook reports; a fixture per site under tools/check.f --all-errors shows a diagnostic and nonzero exit, not a dead process. Files: src/core/checker.f, type-family.f, render.f, lib/errors.f. Verify: the fixtures. Depends: none. Ownership: checker diagnostics. Claim: unassigned.
