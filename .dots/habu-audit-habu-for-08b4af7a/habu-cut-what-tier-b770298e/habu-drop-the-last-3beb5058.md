---
title: Drop the last write-only record fields and dead files
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T18:08:07.668600+01:00"
---

Problem: the owner record's TRUST-DECL and LOCAL-WIDTH fields are filled and never read; TFL-VAR? has only a test caller; src/compiler/native/fetch-check.f is loaded and never called (the --checks dot restores checks from history); the primitive rows' R-BUILD reach and its build atom mark no row. Acceptance: each is deleted with its stores, rows and requires; the engine builds (through a stage if needed) and its link, engine and parity tests pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu2.

Acceptance (2026-11-13, from the lane's own finding): test/layout-valid-part-bad.f, test/layout-valid-tags-bad.f and test/fixtures/file-objects/part-main.f open a `package NFETCH-CHECK` that overrides BAD-TAG or TAGS; with no engine package of that name the override proves nothing. Each drops it, and a file whose remaining case another test already covers goes with its suite case.
