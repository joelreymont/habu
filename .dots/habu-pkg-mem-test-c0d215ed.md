---
title: Package memory-test fixtures
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T23:22:48.841607+02:00"
---

Companion to the vector packaging, same wall, file-local: lib/memory-test.f holds four MEMT-* boundary fixtures (lines 257-270) and STAT-MEM (line 477) OUTSIDE its existing package MEM block (309-473), so the MEM:RELEASE rename's edits to their bodies trip E-PACKAGE-OWNERSHIP. Move them inside the package (reopen or extend the existing block; white-box suite idiom - the file already owns package MEM's test surface; load-position rule respected). Callers measured file-local; no cross-file cascade expected - if the sweep finds an external caller, report it, do not export a bridge. Acceptance: package-diff-lint rc=0 on a representative body edit to each moved word; lib/memory-test.f rc=0; both diff lints on the artifact. Real pre-change failure: the rename artifact's six findings, five of them in this file.
