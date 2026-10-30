---
title: Drop the hb-stdin aliases in type-ctor-suite.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:57:24.477176+02:00"
---

Problem: test/type-ctor-suite.f:42-48 keeps TC-VIS-PRIVATE and E-TC-PROTECTED (aliases of CHECKER-PACKAGE-PRIVATE and E-CTOR-PROTECTED) with a reason that exists only for the hb-stdin cold-prefix run that E1-2 (xrwrmlsy, the unseal build setting) removed; forth-card row 158 says the refusal happens on a sealed from-source prefix boot only. Found by the E1-2 review 2026-10-30. Acceptance: lines 42-48 gone and the original names spelled at the uses (:149, :291-297, :488-489 and any rg finds). Files: test/type-ctor-suite.f. Verify: the type-ctor WHITEBOX-SUITE row on the whitebox engine green. Depends: E1-2 landing. Ownership: that file. Claim: unassigned.
