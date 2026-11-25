---
title: Agree verify and load on an open seeded payload
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:10:26.220806+02:00"
---

Problem: before the quotation fix in TYPE-VAR-WALK, tools/check.f --verify-only refused test/engine-suite.f:917's COK-QL-SEED-JOIN with E-CONSTRUCT-WIDE at QLBOX2:MAKE (CONSTRUCT-WIDE-STAGED-REJECT, src/core/type-family.f:4587) while the load's CHECK-CANDIDATE! certified the same text. The quotation case now agrees; whether a seeded constructor whose payload is open but not a quotation (a type variable, an open row) still gets two verdicts is unknown. Acceptance: the smallest such definition gets one verdict from both routes, or the code shows none can differ. Depends: none. Claim: unassigned.
