---
title: "Name set-check's refusal the same in load and checker"
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T11:39:18.058253+02:00"
---

Problem: the two front ends disagree on set-check. Top level: '0 set-check' is E-CHECKER-MUTATION under tools/check.f (rc 1) while the load accepts it (rc 0) and then compiles ': F13X ( -- ) 1 2 ;' unchecked (probe /tmp/tim-dots/f13c.f prints 2 1; docs/forth.md:34-36 records this as the one open case). In a body: ': F13W ( -- ) 0 set-check ;' is E-CHECKER-MUTATION (trusted_boundary_required) under the checker but only 'habu: in f13w: at set-check' / 'hook: non-certified definition' with no code in the load. Acceptance: the load refuses set-check where the checker does, with the same code, or the docs state the difference as a contract; a row for each shape. Files: src/core/checker.f, src/core/check-hook.f, src/habu/outer.f, docs/forth.md. Verify: /tmp/tim-dots/f13.f, f13b.f, f13c.f through both. Depends: none. Ownership: checked boundary. Claim: unassigned.
