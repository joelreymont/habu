---
title: Make JSON writer self-composition alias-safe
status: closed
priority: 1
issue-type: task
created-at: "2026-08-23T18:41:29.217918+02:00"
closed-at: "2026-09-19T23:23:59.806019+03:00"
close-reason: "resolved by caller-owned writer storage (5f9fe43f, 8ccd9802): no grow/free remains; JWT-TEST-SELF-RAW/SELF-STRING regressions present and green in the tail-pure row (alder's audit, verified in the tree)"
---

JSON-WRITE:$ is a legal public span but RAW/STRING can grow, release that span, then copy from it. Rebase aliases into the copied buffer and add public-path growth regressions; no ownership framework.
Claim: unassigned (stale claim cleared 2026-09-16).
