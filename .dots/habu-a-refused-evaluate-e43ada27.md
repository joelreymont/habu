---
title: A refused evaluate at top level writes no JSON under --all-errors --json-errors
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:27:27.644922+02:00"
---

Problem: a refusal the engine raises while a checked run executes is written as prose, never as a JSON record. A file whose top level runs s" NOSUCHWORD" evaluate, checked with tools/check.f -- --all-errors --json-errors or plain --json-errors, prints only 'E-UNDEFINED: NOSUCHWORD at <file>' and ends rc 70, so diag-contract fails with no JSON diagnostics; diag-code.f states a valid line even when the input is rejected. Reproducer: ~/.cache/tmp/rv-arena-refix/probe/u1.f. Cause: the checker's replay stops at evaluate (TOP-RUNS src/core/checker.f:17067 answers CTL-PARSES) and reports nothing; the run child's interpreter raises it (UNDEFINED src/habu/interpreter.f:342), and THROW-AT (:339) and AT-SOURCE (:330) write prose to fd 2 without reading JSON-DIAGS, which only the checker's render.f reads; tools/check-core.f CHK-RUN-JSON-ONLY (:1195) moves the prose to stdout. --verify-only writes only W-CHECK-DEFERRED. A bare NOSUCHWORD gets its record only because the replay refuses it before the run. Not traced: which UNDEFINED site fires at run time.

Fix (lead, to design): a refusal raised by the run under JSON-DIAGS is written as a record, in the interpreter's THROW-AT or by check.f turning a prose-only failed run into one.

Acceptance: the run writes one JSON record for the refusal and diag-contract accepts it; a case in test/diag-position-test.f. Depends: none. Claim: unassigned.
