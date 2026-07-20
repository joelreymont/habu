---
title: Eval train leg grades the public authoring surface
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T14:55:15.180806+02:00\""
---

Joel-ratified direction 2026-07-20, deciding the judgment call from the decoupling landing (4ffe4da6): the eval train leg stays LIBRARY-side (never showcases an example - the framework's evaluation must not couple to one application; each example locks its own end-to-end numbers). Upgrade path: replace the current hand-wired reference regressions in maki/eval/train.f with authoring-surface grading - author a small model exactly as a user would (MODEL: composition + SPEC: equations in the canonical Σ/· spelling + the opt-in trainer arming words for schedule/clip), train it deterministically, lock the result. This grades the framework's real promise (author this, it trains) through the same public path every example takes, and grows with the surface: when batched attention enters the grammar the eval model gains a batched line, not a bespoke harness. Keep the two current reference regressions until the authoring-surface version lands (they are the interim floor, labeled as such). Behavior bar: deterministic lock, run-twice bit-identical, suite budget respected. Territory: maki/eval/train.f + docs/maki/eval.md.

Claim: agent=evalauth workspace=.jj-ws/fable-evalauth machine=spark (owns maki/eval/train.f + docs/maki/eval.md)
