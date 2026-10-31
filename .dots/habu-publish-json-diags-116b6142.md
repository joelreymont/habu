---
title: Give the checker its verdict and recovery words
status: active
priority: 2
issue-type: task
created-at: "2026-10-31T13:42:25.582700+02:00"
---

Problem: src/habu/verify-source.f CHECK-BODY and src/core/cell-effects.f MULTI-ERR-BEGIN stay TRUSTED: because, as :, the cold host refuses them: 'E-UNDEFINED habu: in check-body: undefined word JSON-DIAGS' (check-verify row top-cold-prim: cold scanner, expected 0 got 70) and 'in multi-err-begin: undefined word UEND' (lane tc5). Publishing JSON-DIAGS would not free CHECK-BODY: it also reads DIAG-QUIET, which src/core/checker.f deliberately leaves unpublished. CHECK-BODY repeats src/core/check-hook.f REPORT-UNCHECKABLE: render an uncheckable verdict unless CHECK already rendered it (JSON, multi-error load, quiet). MULTI-ERR-BEGIN has the same shape: it reads the checker's UEND only to hand it back to CHECKER-EFFECT-AUTHORITY:RECOVERY-START.

Root fix: the checker owns both decisions. REPORT-UNCHECKABLE moves from check-hook.f into checker.f after CHECK! with a PRIM: row; check-hook.f and verify-source.f call it, CHECK-BODY and verify-source.f's MULTI-ERR-MODE? copy are deleted. MULTI-ERR-BEGIN and MULTI-ERR-END move from cell-effects.f into checker.f after MULTI-ERR? with PRIM: rows, so BEGIN reads UEND inside its owner. JSON-DIAGS, DIAG-QUIET and UEND stay unpublished. src/habu/aot-data-cell.f keeps its JSON-DIAGS TRUST row: its data-cell JSON path reads the flag.

Acceptance: no TRUSTED: CHECK-BODY or MULTI-ERR-BEGIN; one copy of the verdict rule; check-verify (top-cold-prim) passes. Files: src/core/checker.f, src/core/check-hook.f, src/habu/verify-source.f, src/core/cell-effects.f. Verify: engine build; rows check-verify, check-verify-batch, cold-naming, program-diagnostics, checker-dup-record, sumtype-unchecked, aot-payload-graph, tool-boundary-check-repair; full suite at landing. Depends: none. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-vw
