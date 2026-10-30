---
title: "fix arena: no head accessor reaches an owner's head at T0"
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T22:41:49.766752+02:00"
---

Problem: test/arena.f (master's ARENA feature) at the stack candidate's T0: case 'no head accessor reaches an owner's head' (test/arena.f ~200-207) expected 2 got 0, then 'hb: capture: live arena ARENA-TEST:RECS'. Output /tmp/jerry-lead-tmp/rows3/arena.out. Acceptance: root cause found by comparing public hb, candidate T0 and candidate with the JIT selected; the wrong layer fixed; row arena green on the candidate. Files: as the root cause finds. Verify: arena row on a rebuilt candidate. Depends: none. Ownership: the fixed layer. Workspace: .jj-ws.noindex/jerry-arena, brief /tmp/jerry-big/fix-arena-brief.md. Claim: jerry.
