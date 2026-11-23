---
title: "Mark an evaluated STRUCTURE's family for the check"
status: open
priority: 2
issue-type: task
created-at: "2026-11-23T11:07:43.250089+01:00"
---

Problem: a STRUCTURE a top-level evaluate declares inside package pa, then PA-HELD:MAKE after ;package, loads (rc 0) but tools/check.f --verify-only rejects it E-UNDEFINED: CHECKER-VERIFY-RENDERS (src/core/checker.f :18181-18193) marks the statement's wordlist, never the family's namespace (TF-CTOR-PKG$, src/core/type-family.f :1247-1260). Found by the BUFFER pre-pass design (Fable, 2026-11-23, /tmp/plan-bufcount/v1.f). Acceptance: every program --load accepts that declares a family by evaluate passes --verify-only, with uses of the family deferred, not rejected. Depends: none. Claim: unassigned.
