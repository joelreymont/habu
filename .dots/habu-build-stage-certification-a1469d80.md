---
title: Build-stage certification remaining checker self-type gap
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T16:53:01.566418+02:00"
---

BF-CERTIFY nonblocking now runs VERIFY:SOURCE-BUF over generated stage2/stdin/snap
sources from tools/build-fixpoint.f. It reports the current self-typing miss
without failing the build; latest reduced first miss after VREC offset +
signature cleanup is src/core/checker.f PSTACK recursive renderer at RECURSE.
Blocking certification depends on checker-self-typing/single-pass work, not AOT.
