---
title: Review and tidy the keyed native fixture writer
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T21:56:04.592335+02:00"
---

2dce15a3 (Cache the native fixture writer as a keyed image) landed without an independent review. Known loose ends: test/cold-engine.f WRITER-RUN/PUBLISH die without removing the work dir (it leaks into the build cache); stale comments in test/gate-stdlib-lib.f SUITE-SETUP and test/whitebox-engine.f; the build cache never evicts (16 MB per writer key, ~331 MB observed). Acceptance: independent review findings on 2dce15a3 fixed; failed writer runs leave no work dir; comments match the code; eviction either bounded or stated as out of scope with the reason. Depends: none.
