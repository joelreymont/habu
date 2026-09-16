---
title: Refuse two builds sharing one HB_TMP
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:51:15.415221+03:00"
---

Problem: two concurrent tools/native-build.f runs that share one HB_TMP both die with `duplicate definition: IMAGE-BASE` rc 78 (table-fill lane, 2026-09-16), the same message the corrupted host engine gave, so a shared temp directory masquerades as a source defect. Acceptance: the build takes a per-run subdirectory under HB_TMP (pid plus a random suffix, removed on success) or refuses by name when it finds another run live there (a lock file with the owning pid, stale locks reaped); a fixture starts two builds on one HB_TMP and shows both succeed or one is refused by name; docs/bootstrap.md states the rule. Files: tools/native-build.f, tools/native-build-core.f, src/habu/build-*.f if the temp naming lives there, docs/bootstrap.md, test/. Verify: the fixture; test/run.f. Depends: none. Ownership: build driver. Claim: unassigned.
