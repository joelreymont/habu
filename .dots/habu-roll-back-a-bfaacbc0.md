---
title: "Roll back a refused generated constructor's symbol"
status: open
priority: 2
issue-type: task
created-at: "2026-09-12T13:24:28.456424+03:00"
---

Problem: src/core/sumtype.f TDPLAN-CTOR-NAME+ applies SUMV-CTOR-SYM! while rendering, before the preflight, and TDPLAN-PREFLIGHT-CHECKER rewinds only CTOR-PEND and the candidate scope; after a refused generation (measured 2026-09-12 with a hostile payload provider in test/type-ctor-suite.f: SWAP-CODE throws 70) GENERATE-COMMITTED on the same family dies 'tfam: variant constructor symbol already set' (rc 76), so the family can never generate again in that process. Not reachable with the committed provider. Acceptance: a refused generation leaves the variant's constructor symbol unset, and the suite's case asserts recovery (generate again after the refusal). Files: src/core/sumtype.f, test/type-ctor-suite.f. Verify: the suite on a rebuilt engine. Depends: none. Ownership: hazel. Claim: unassigned.
