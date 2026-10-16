---
title: Make the T-REPORT placement rule agree
status: open
priority: 3
issue-type: task
created-at: "2026-10-16T08:34:57.379749+03:00"
---

Problem: docs/gate.md:227-229 says a suite file closes its package before T-REPORT, or a later suite in the same spawn dies exit 75 with a bare token. docs/forth.md:1300-1326's example, and 24+ suites, call T-REPORT inside the package and close it afterwards. T-REPORT (src/lib/test/assert.f:126) prints 'test: ok' and returns on success, so either way the package closes before the next file loads. Acceptance: measure a two-file SUITE row on the current engine (file 1 calls T-REPORT inside its package, then ;package; file 2 is another package-scoped suite). If it passes, docs/gate.md states the real rule with that measurement. If it dies exit 75, the forth.md example and every suite that reports inside its package are changed to close the package first. Files: docs/gate.md, or docs/forth.md plus those suites. Verify: the two-file probe. Depends: none. Ownership: docs/gate.md, docs/forth.md. Claim: unassigned.
