---
title: Make the Gforth recovery engine build the native product
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T10:56:42.174617+02:00"
---

Problem: a Gforth-bootstrapped recovery engine on spark cannot build the native product from current source: with the preload tools/native-layout.f:34 fails 'unknown cell ... stale<n>'; without it, save dies E-UNDEFINED NATIVE-RUNTIME:CAPTURE-PREPARE (timer lane 1, 2026-10-30). Acceptance: the recovery route in docs/bootstrap.md builds bin/hb from the Gforth seed on Linux arm64. Files: tools/native-layout.f, tools/native-build-core.f, src/habu/native-runtime.f, docs/bootstrap.md. Verify: the recovery route on spark ends in a bin/hb that runs test/run.f. Depends: none. Ownership: the recovery route. Claim: unassigned.
