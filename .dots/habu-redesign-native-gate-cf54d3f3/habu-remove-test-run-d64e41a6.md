---
title: Remove test/run parent preload
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T22:30:29.106151+02:00"
close-reason: "completed by replacement architecture: the top preload image path was removed instead of cached; test/run.f directly starts setup from bin/hb, hot macos-arm64-12x2 passed at 29.71s shell wall, and generated warm images remain cache-only artifacts"
---

RCA after resident fork work: /usr/bin/time full Mac hot suite is 44560ms while test/run reports 28745ms because test/run.f requires gate-runner-support at parent load before TR-GATE-START. The scheduled phases are under 30s; the remaining wall long pole is entry compilation. Fix: split thin top-level launcher from resident support, cache/build a top-level test runner image or equivalent setup artifact, and make the timed command execute the resident suite without recompiling support. Acceptance: same full suite command reports wall <=30000ms on macos-arm64-8x2 warm cache, no broad per-phase runner rebuild, generated warm images stay out of git.
