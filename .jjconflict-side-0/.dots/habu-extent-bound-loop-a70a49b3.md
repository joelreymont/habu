---
title: Extent-bound loop induction
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T20:44:06.376307+02:00"
---

Checker capability: a loop form whose induction value is typed as the iterated extent's index (candidate name ?IDX-DO), so hash-K 0 ?do loops yield ix<extk> directly instead of a plain n the author re-injects by hand. Today the injector call is unchecked at the loop boundary: inside a K-loop, i >#M is accepted because every injector accepts any plain n, so the author-time flip protection promised by docs/golden-syntax.md line 68 has a silent hole exactly there (destruction-review finding M1 on habu-extent-typed-tensor-bde435dc). Runtime range checks in the injectors (directed as a merge fix on that lane; the originally generated injector body is an unchecked no-op cast) narrow the damage only when extents differ in size; equal-size extents stay indistinguishable until the loop counter itself carries the extent type. Scope: checker/loop-word capability in src/core (bind the counter of a counted loop to a family instance chosen by the limit expression or an explicit extent argument), positive and negative checked fixtures, then retire the hand-written injector crossings in maki/extent-tensor tests and the SPEC-generated loop bodies. This is Foundation-A-adjacent checker work; do not fold it into the maki tensor lane.
