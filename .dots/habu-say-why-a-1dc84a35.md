---
title: Say why a returned reading quotation is refused
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T12:05:26.776326+02:00"
---

Problem: a quotation that reads a local, given to a constructor whose result the signature returns in a generic structure or sum type, is rightly refused, but the diagnostic carries no reason: habu: in w: at 'BOX2:MAKE'; suggestion: Inspect the token, signature, and raw stack evidence. Reproducers ~/.cache/tmp/quot-fix-b2/review/p/s01-declseed-read.f (BOX2:MAKE), s06-construct-declseed.f (construct jb2 some) and s07-ctor-declseed.f (JB2:SOME), each rc 70 on the quotation tip engine with only the generic line; the same on the engine before the constructor fix. The refusal is raised where the constructor's output meets the declared output, on a path that sets no E-LOCAL-ESCAPE reason. Acceptance: each of the three programs is refused with E-LOCAL-ESCAPE and the line a quotation that reads a local would outlive it at the constructor token; a plain quotation in the same position still certifies. Files: src/core/checker.f, src/core/type-family.f (the construct step seeded from a declared output). Verify: CBAD-QL cases in test/engine-suite.f for the three shapes, each checking the reason text. Depends: none. Ownership: unassigned. Claim: unassigned.
