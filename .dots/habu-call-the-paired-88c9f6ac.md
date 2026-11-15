---
title: Call the paired checker without checker-owner defers
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T15:54:24.942341+01:00"
---

Problem: src/compiler/native/checker-owner.f:20-35 holds 16 SOURCE-* defers, bound once by BIND-SOURCE-CALLS (:76) from the checker's owner record through PAIRED-FIELD (:73). The comments that gave the reason (no checked call or tick of the checker's words could load there) were false: a checked call and a tick of CWIN-CELLS, REC-BINDING-WINDOW and CHECK-UNJUDGED! from that file load with rc 0. docs/design.md 'Machinery being replaced' replaces a hook with a direct call or data the owner records. Acceptance: each SOURCE-* word is a plain call of the checker word it binds, or a build shows why it cannot be and the comment above the defers states that reason; the engine converges (gen 2 equals gen 3). Depends: none. Claim: unassigned.
