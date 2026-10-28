---
title: Keep declared row variables parametric
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.121924+02:00"
---

Problem: a declared type variable is rigid (': F17E ( a -- b ) ;' is E-NONPARAMETRIC-EFFECT) but a row variable is not: ': F17F ( R -- S ) ;' certifies, and so does ': F17 ( R [ R -- S ] -- S ) catch drop ;', which claims S on the throw path (probes /tmp/tim-dots/f17f.f, f17.f). Callers see the unified effect (a caller passing [ -- n ] is E-MISMATCH: expected n [ n-- n]), so the declaration overstates its generality rather than letting a caller forge cells. The alias check (NPBAD-KIND 1, src/core/checker.f:5389-5393, :23316-23323) covers type quantifiers only. Acceptance: two declared row variables the body unifies are refused as E-NONPARAMETRIC-EFFECT naming both, with rows for both probes; tree-wide signatures that rely on it fixed. Files: src/core/checker.f, src/core/render.f, a checker suite. Verify: the probes; test/run.f. Depends: none. Ownership: type system. Claim: unassigned.
