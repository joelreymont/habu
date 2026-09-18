---
title: Avoid quadratic named-enum payload scans
status: open
priority: 2
issue-type: task
created-at: "2026-09-18T19:58:29.035596+03:00"
---

Problem: src/core/type-family.f:1595-1709 repeatedly scans every field and variant in a family inside SUMV-NAMED-PAYLOAD?, SUMV-PAY-N and SUMV-PAY-ROOT; SUM-IWIDTH invokes these readers for each variant/payload. With one named n field per variant, a width query performs quadratic family traversal. Measured on the installed aarch64 engine at shared-machine load around 7-8: tier-0 compilation of ten fresh empty identity words ( eN -- eN ) averaged 294, 813 and 2654 microseconds for predeclared ENUMs with 8, 16 and 32 variants. Reverse-order means were 290, 1953 and 2655 respectively (the 16-variant run was noisy). Source construction and ENUM declaration were outside the timed interval. Acceptance: resolve/validate the family payload representation once per traversal or provide indexed variant-owned slices so ordinary width/arity reads avoid nested whole-family scans; preserve named/legacy representation separation, bounds, ownership, rejection codes, rollback and capture behavior. Add a controlled scaling check over identical-width enums and attribute the measured cost before changing the representation. Files: src/core/type-family.f, owning registry code if necessary, and focused type-family/compiler tests. Verify: rejection/rollback tests, scaling at both tiers, rebuild and full native suite. Ownership: checker/type-family registry. Claim: unassigned.
