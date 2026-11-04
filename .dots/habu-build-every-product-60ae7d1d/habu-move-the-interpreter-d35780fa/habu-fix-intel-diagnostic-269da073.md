---
title: Fix Intel diagnostic positions
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T13:58:07.634973+00:00"
---

Owner: pulsar /root/diag_rows in .jj-ws.noindex/pulsar-intel-diag. Combined census reports seven existing diag-position failures, including E-UNDEFINED where E-MISMATCH is required. Reproduce with matching production engine/source, fix responsible code, preserve master assertions. Acceptance: unchanged test/diag-position-test.f exits 0 plus focused downstream checks and independent review. Shared changes need an OPEN Mac ARM check.

Census 2026-11-04: the claim named a workspace or agent whose work is gone or landed; reset to open, Claim: unassigned.

Census 2026-11-04: an acceptance case of its parent; fix it in the code that stays (named above), never in the machinery the parent replaces.
