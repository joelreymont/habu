---
title: Name the reason when ncomp refuses a write in checker.f
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:44:23.888794+03:00"
---

Problem: a word in src/core/checker.f whose body is a plain write sequence (the raw-ptr lane's census reporter, 2026-09-17: the same lines that compile in src/core/check-hook.f) is refused by the native compiler as 'ncomp: cannot compile RAW-CENSUS-REPORT' with an EMPTY reason symbol, so the refusal names nothing to fix; the difference between the two files (load position before the hook installs, package or trust state of write there) is not diagnosed. Acceptance: the reduction reproduced with a minimal word in checker.f, the responsible layer identified (the reason symbol left empty is a diagnostic defect whatever the cause), ncomp's refusal naming the reason by name, and a regression under test/compiler/. Files: src/compiler/native/ (ncomp's refusal), src/core/checker.f (reduction), test/compiler/. Verify: the regression; test/run.f. Depends: none. Ownership: native compiler diagnostics. Claim: unassigned.
