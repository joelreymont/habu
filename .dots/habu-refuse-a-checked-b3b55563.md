---
title: "Refuse a checked generates: without a stack overflow"
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T13:02:03.732434+02:00"
---

Finding (census tc2, 2026-10-31): turning src/core/checker.f generates: (or MARK-REP) from TRUSTED: into : and building the engine (bin/hb --load tools/native-build.f -- <out>) refuses the definition, then the build dies 'hb: stack bounds exceeded (machine)', rc 102. A refusal must end the load as a refusal, never a machine stack fault. Evidence: /Users/joel/Work/habu/.jj-ws.noindex/jerry-tc2/.out/probe-*.log, /tmp/jerry-big/trusted-census-tc2.md. Acceptance: reduce to a reproducer, find the layer, the build ends rc 70 with the refusal named. Ownership: unassigned.
