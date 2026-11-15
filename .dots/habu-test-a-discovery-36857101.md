---
title: "Test a discovery stop's place through plain check.f"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T03:01:48.134121+01:00"
---

Problem: the source-discovery tests arm DISCOVER:BYTES$ themselves (tools/source-discovery-test.f SDT-ARMED-RUN copies tools/check-verify-core.f CHK-ARM); the only check.f case asserting a discovery stop's place, test/diag-position-test.f TEST-ALL-ORDER, runs --all-errors, so a regression in CHK-ARM passes every test. Acceptance: a plain check.f --json-errors case asserts a dynamic required's place. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
