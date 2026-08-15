---
title: Intern compiler types
status: closed
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:54:58.832437+02:00\""
closed-at: "2026-08-15T14:07:28.851986+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): type interning (codec superseded CG-31; FRENDER survives with consumers). Production-consumed by the native chain; suites dual-registered, green through the real entry."
blocks:
  - habu-intern-compiler-symbols-ed025e24
---

Full context: design section 6.3 requires canonical scalar, pointer, function, and token types shared by dialects. Add exhaustive STRUCTURE/ENUM type records and structural interning; widths, address spaces, target legality, and recursive/reference form are explicit. Acceptance: identical types intern; malformed width/address space/target combinations reject; render and canonical encoding fixtures pin identity.

Claim: agent=ir-type workspace=.jj-ws/habu-intern-compiler-types-bf952f0f
