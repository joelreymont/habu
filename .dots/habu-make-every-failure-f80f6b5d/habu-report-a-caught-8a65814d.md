---
title: Report a caught error in full from tools
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T14:02:21.850421+02:00"
---

Problem: tools and harnesses that catch an error print only its number (tools/chain-run-build.f:16-18, tools/build-profile.f:101-102, tools/check-core.f:1394, test/gate-pool.f:415-418; src/lib/test/assert.f:138-141 prints neither code on a mismatch). Acceptance: public ERROR:REPORT ( n -- ) in src/lib/error.f (Joel named it 2026-11-12) prints the uncaught reporter's lines with `caught` for `uncaught` and returns; a tool that catches only to print loses its catch, and one that carries on prints its lead line and then calls ERROR:REPORT. Three lanes: ERROR:REPORT with assert.f; tools/; test/ with src/lib/pg-test.f. Depends: habu-uncaught-throws-name-1a43b055 (the reporter). Claim: unassigned.
