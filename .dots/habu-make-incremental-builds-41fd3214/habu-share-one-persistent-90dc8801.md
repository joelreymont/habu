---
title: Share one persistent build cache across gates and lanes
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:17:10.212310+03:00"
---

Problem: gate scripts and worker briefs give every run a fresh HABU_BUILD_CACHE, so the engine build is always cold (92 s) and no gate exercises reuse. Acceptance: docs/gate.md and docs/bootstrap.md state one persistent cache per checkout/engine for gates and lanes; the suite's own build rows that are not testing cold builds reuse it; measure full-suite and two-generation wall time before and after. Files: docs/gate.md, docs/bootstrap.md, test/run.f / test/gate-stdlib-lib.f if they create caches. Depends: none. Claim: unassigned.
