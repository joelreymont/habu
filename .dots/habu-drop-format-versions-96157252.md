---
title: Drop the build report's version field
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T17:57:09.215650+02:00"
---

Problem: the hb-build JSON report carries "version": 1 beside "schema": "hb-build-report", asserted by tools/hb-build-test-lib.f:475-479. Joel 2026-10-27: "why do you need to version any schemas? we have no legacy. hard cutover only!"; a format number is kept only where it makes an old file refused when opened (the native unit cache, 26 -> 27). A build report is output, not a stored file a later build must refuse, so its version field refuses nothing. (snapshot-writer's trailer version is such a refusal and stays.)
Acceptance: if no reader in Habu or Etch reads the report's version, the writer drops the field and the test drops its assert; if one does, report it instead.
Files: the report writer in tools/hb-build-lib.f, tools/hb-build-test-lib.f:475-479.
Verify: rg -n '"version"|s" version"' over Habu and Etch readers of hb-build-report; bin/hb --load test/run.f -- hb-build-aot hb-build-cli-errors.
Depends: none. Ownership: those files. Claim: unassigned.
