---
title: Type a variable a top-level does> definer patched
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T05:44:29.542015+02:00"
---

Problem: the checker types a variable patched by a top-level does> definer as its cell. `variable V : B ( -- ) does> ( -- n ) drop 9 ; B V .` is refused 'expected n actual ptr a' on master 9f905e31 while the runtime pushes 9 (probes ~/.cache/tmp/carl-oi.H6XY). Acceptance: the checker uses the patched does> effect for V after B runs, or refuses B's patch of a declared word; rejected-program and runtime e2e cases. Files: src/core/checker.f (owner to locate). Verify: the probe plus test/outer-interpret.f. Depends: none. Claim: unassigned.
