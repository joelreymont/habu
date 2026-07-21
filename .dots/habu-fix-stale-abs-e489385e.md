---
title: Fix stale abs-chain comments after linker narrowing
status: open
priority: 3
issue-type: task
created-at: "2026-07-21T12:54:28.840107+02:00"
---

Comment-only cleanup from the AOT linker narrowing (d84ca7d1): src/habu/habu2.f:5001 references the deleted FINDADDR and describes LP2VEXEC's call as an absolute movz/movk chain (now a direct BL); test/gate-aot-positive-lib.f GAP-LAYOUT-STORE (~256) and GAP-LAYOUT-FETCH (~291) comments still narrate the collapsed-in-image mechanism. Retarget all three to the direct-BL reality. Comments emit no bytes - zero size/census movement expected, verify.
