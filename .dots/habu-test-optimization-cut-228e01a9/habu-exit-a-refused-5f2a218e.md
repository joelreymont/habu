---
title: Exit a refused checker record as a refusal, not an uncaught throw
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T23:39:30.937012+02:00"
---

Problem (review 382 of ec8978cc): tools/check.f exits 67 for a source the run stage refuses with E-TRUST-UNRESOLVED or E-PKG-CONTEXT (the load's uncaught throw, src/habu/layout.f:409 UNCAUGHT-RC), and 67 is also check.f's own CHK-E-CAPACITY (tools/check-core.f:69 'source path exceeds capacity'); every other refusal check.f reports with a JSON record exits a refusal code (statement throw and bad declaration 70, storage 70, duplicate 78, engine-provided 64), so a repair loop keyed on the exit code cannot tell a refused record from an overlong path. Also docs/forth-card.md:109 says E-BAD-DECLARATION exits 67; through check.f --json-errors it measures 70. Acceptance: a run-stage child exit carrying a refused-record diagnostic maps to 70 'as for a refusal' (or the engine raises these refusals through a path check.f classifies), documented in docs/repair-diagnostics.md; the card's line corrected; tools/repair-packet-test.f TEST-RECORDS expects the new rc; seen failing first. Files: tools/check-core.f, docs/repair-diagnostics.md, docs/forth-card.md, tools/repair-packet-test.f.
