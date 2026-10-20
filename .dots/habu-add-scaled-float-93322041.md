---
title: Add scaled float vector helpers
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T14:59:16.926222+03:00"
---

Jorge Maki reports concrete generic float copies in src/trim.f trim-stretch: BIGGER(r r--r) chooses larger finite value, NORM3(r r r--r) scales by largest absolute component before squares to avoid avoidable overflow/underflow. Add public FMATH FMAX and HYPOT3 with precise zero, finite, infinity and NaN semantics consistent with the library. Inspect existing float runtime/E2E coverage first, preserve tiny/huge representable norms, document actual contracts and return qualified public source/release for Maki migration. Habu library owns generic math; no Maki edits. Queue behind current incremental core release.
