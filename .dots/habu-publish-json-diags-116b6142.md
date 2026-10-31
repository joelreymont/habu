---
title: Give the checker the verdict word CHECK-BODY repeats
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T13:42:25.582700+02:00"
---

Problem: src/habu/verify-source.f CHECK-BODY and src/core/cell-effects.f MULTI-ERR-BEGIN stay TRUSTED: because, as :, the cold host refuses them: 'E-UNDEFINED habu: in check-body: undefined word JSON-DIAGS' (check-verify row top-cold-prim: cold scanner, expected 0 got 70) and 'in multi-err-begin: undefined word UEND' (lane tc5). Publishing JSON-DIAGS would not free CHECK-BODY: it also reads DIAG-QUIET, which src/core/checker.f deliberately leaves unpublished. CHECK-BODY repeats src/core/check-hook.f REPORT-UNCHECKABLE: render an uncheckable verdict unless CHECK already rendered it (JSON, multi-error load, quiet). MULTI-ERR-BEGIN has the same shape: it reads the checker's UEND only to hand it back to CHECKER-EFFECT-AUTHORITY:RECOVERY-START.

Root fix: the checker owns both decisions. One checker word, with a PRIM: row, renders an uncheckable verdict unless already rendered; REPORT-UNCHECKABLE and CHECK-BODY both call it. One checker word, with a PRIM: row, starts recovery at the current UEND; MULTI-ERR-BEGIN calls it. JSON-DIAGS, DIAG-QUIET and UEND stay unpublished. src/habu/aot-data-cell.f's `s" JSON-DIAGS" s" -- ptr a" TRUST` row goes if nothing else needs it.

Acceptance: CHECK-BODY and MULTI-ERR-BEGIN become : and pass check-verify (top-cold-prim); one copy of the verdict rule; the new words named by docs/forth-card.md §1. Files: src/core/checker.f, src/core/check-hook.f, src/habu/verify-source.f, src/core/cell-effects.f, src/habu/aot-data-cell.f. Verify: engine build; check-verify; the aot rows that load aot-data-cell.f; a multi-error load row. Depends: none. Claim: unassigned.
