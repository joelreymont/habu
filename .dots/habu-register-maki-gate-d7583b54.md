---
title: Register maki gate as a suite
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:31:41.068348+02:00"
---

The maki gate is a manual README command blob (maki/README.md:45-79), permanently outside test/run.f (README:29-31 'not a native-gate dependency'; zero maki/ refs in test/*.f). Nothing automated proves it before a master merge even though AGENTS.md lists it as an owning gate. Fix: express it as a registered TEST:SUITE (own group, still fenced from the trust root) runnable via one entry point with recorded pass/fail + timing, wired into the pre-merge checklist; keep device-smoke SKIPPED off-device. Do not fold into trust root.
