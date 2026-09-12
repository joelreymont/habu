---
title: Qualify the family in the canonical constructor body
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:24:28.459338+03:00"
---

Problem: src/core/sumtype.f TDGEN-CONSTRUCT-BODY renders 'construct <family> <variant>' with the family spelled bare while the output type is package-qualified (measured 2026-09-12: PV-ZPL:ONE ( n -- pv:zpl ) construct zpl one); construct's family resolve is owner-only (TFAM-CONSTRUCT-FAM), so the token certifies only because the checker context at generation is still the declaring package and resolves elsewhere when re-parsed at AOT intake, the seeded-signature scope defect the file's own comment records (habu-seeded-signature-loses-78c16109). Acceptance: the rendered body spells the family qualified (or construct resolves a qualified family), every generated constructor's text changes accordingly with the pins in test/type-ctor-suite.f re-derived, and an AOT intake case re-parses a generated constructor outside its package. Files: src/core/sumtype.f, src/core/checker.f (construct), test/type-ctor-suite.f, test/compiler/. Verify: the suites and test/run.f on a rebuilt engine. Depends: none. Ownership: hazel. Claim: unassigned.
