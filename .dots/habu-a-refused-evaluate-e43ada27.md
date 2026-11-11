---
title: A refused evaluate at top level writes no JSON under --all-errors --json-errors
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.644922+02:00"
---

Problem: a file whose top level runs s" NOSUCHWORD" evaluate, checked with tools/check.f -- --all-errors --json-errors, prints only the prose line E-UNDEFINED: NOSUCHWORD at <file> and no JSON packet, so diag-contract fails with no JSON diagnostics; diag-code.f states a valid line even when the input is rejected. Reproducer: ~/.cache/tmp/rv-arena-refix/probe/u1.f (found by the arena F2/F3 re-review, .out/review-arena-refix-report.md). Acceptance: the run writes one JSON record for the refusal and diag-contract accepts it; a case in test/diag-position-test.f. Files: the checker's evaluate refusal path, test/diag-position-test.f. Verify: the probe and the test. Depends: none. Ownership: carl. Claim: agent=carl workspace=none (cause found; fix design owed by the lead). Related: habu-all-errors-reports-09344102.

Related lane: habu-all-errors-reports-09344102 (the same evaluated-token refusal path under --all-errors) is parked and unclaimed since 2026-11-11, so no lane holds this path.

Cause (hunt 2026-11-11, probes ~/.cache/tmp/carl-json, reduced: run-u1.f `-1 JSON-DIAGS !` then `s" NOSUCHWORD" evaluate` under bin/hb --load): no JSON writer handles a refusal the engine raises while the run executes. The checker's replay stops at `evaluate` (TOP-RUNS src/core/checker.f:17990-17993 answers 2, CTL-PARSES) and reports nothing; the run child's interpreter raises it (UNDEFINED src/habu/interpreter.f:378-379) and THROW-AT :375-376 / AT-SOURCE :363 write prose to fd 2 without reading JSON-DIAGS, which only the checker's render.f reads; the parent (tools/check-core.f CHK-RUN-JSON-ONLY :1195) moves the prose to stdout. Not specific to --all-errors: plain `--json-errors` is the same (rc 70, no record); --verify-only writes only W-CHECK-DEFERRED. Not CA-RUN-COMPOSE-DEFS and not dot 09344102's last case; commit 9e0d3197 does not change it. A bare `NOSUCHWORD` gets its record only because the replay refuses it before the run.
Fix (lead, to design): a refusal raised by the run under JSON-DIAGS is written as a record, in the interpreter's THROW-AT (and the boot prefix's twin, habu2.f EM-COMPILE-UNDEF) or by check.f turning a prose-only failed run into one. Not traced at run time which UNDEFINED site fires.
