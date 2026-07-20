---
title: Census-assert the STATUS.md certification counts
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-20T13:01:10.444485+02:00\""
---

Verification gap flagged at the BTC-7 landing (b192992e): STATUS.md declares 'Certified: 987 Uncheckable: 0 Rejected: 0' as the single source of truth, and stale-status-lint polices the DATE and count-shaped strings elsewhere - but NO tool measures the actual live counts and no gate assertion ties the declared number to the engine. The BTC-7 lane added 5 checked words to src/core/checker.f and could not determine whether the tally should now read 992 (left STATUS.md unedited rather than write an unverified number - correct call). Fix: make the build-fixpoint self-check (tools/build-fixpoint.f BF-CERT-* machinery already counts certifications for the audit path) EMIT the certified/uncheckable/rejected triple in its report, add a gate assertion comparing the emitted triple to the STATUS.md declaration (fail closed on drift, same shape as the CODELEN ratchet), and update STATUS.md to the measured value with the standard dated-history line. Red-first: a deliberately wrong STATUS.md count must red the gate. Territory: tools/build-fixpoint.f report surface, the gate slice that runs it, STATUS.md, stale-status-lint coordination.

Claim: agent=certcensus workspace=.jj-ws/fable-certcensus machine=spark (owns tools/build-fixpoint.f report surface + the gate assertion + STATUS.md count line + stale-status coordination)
