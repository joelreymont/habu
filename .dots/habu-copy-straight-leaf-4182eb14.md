---
title: Copy straight leaf words into their callers
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T16:32:56.729373+01:00"
blocks:
  - habu-inline-the-views-e1cd3f5c
---

Problem: on a compile 38% of calls go to straight leaves of at most six instructions (TAG, RES-FALSE, PAY and more); a call and its return cost x86 0.6 ns, about 0.9 s of a 7.1 s load, and arm64 about none. Acceptance: T0 (src/habu/t0.f CALL-TOKEN) copies such a leaf's code where its call was, a changed leaf recompiles its callers (the words are the unit of dependency), breakpoints and profiles still name the leaf, and the load is faster on x86 and no slower on arm64. A design comes first: what invalidates a copied leaf in a cached object, and what the debugger shows. The code must not grow: a leaf longer than its call is copied only where measured to pay. Copying engine primitives the same way was measured on arm64 once and gained nothing there (habu-inline-primitive-bodies-fe421e1d), so x86 decides. Depends: habu-inline-the-views-e1cd3f5c. Claim: unassigned.
