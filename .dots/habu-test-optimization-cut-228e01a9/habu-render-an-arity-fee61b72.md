---
title: Render an arity-0 parameter type without empty brackets
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T18:46:24.292795+02:00"
closed-at: "2026-10-15T01:26:45.318017+03:00"
close-reason: Completed or superseded on current master. 1b9b33ef0d19 renders zero-argument family names bare while preserving signature parsing; exact prose/JSON fields, rebuilt engine and generation checks passed. Read-only source/history evidence checked in backlog review; no new code or broader runtime claim.
---

Problem (lane 354 r4-needle, f01a657c): a DEFTYPE nominal type renders as 'frame-idx<>' in every diagnostic field (declared_effect 'frame-idx<> -- n', inferred_effect, actual): src/core/render.f:410-419, the T-PARAM branch, emits '<' ... '>' around zero arguments. A diagnostic should name the type as the source spells it, and a rendered effect recorded as a signature must read back. Acceptance: an arity-0 param type renders as its bare name in prose and JSON; check whether any reader (signature replay, cert text, render-then-parse round trips) depended on the brackets and keep it reading back; the engine-suite DEFTYPE check (now bound to "actual":"frame-idx) asserts the exact bare form; rebuild (baked), g1 == g2. Files: src/core/render.f, test/engine-suite.f.
