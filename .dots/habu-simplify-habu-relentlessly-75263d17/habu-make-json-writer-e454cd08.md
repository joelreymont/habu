---
title: Make JSON writer self-composition alias-safe
status: open
priority: 1
issue-type: task
created-at: "\"2026-08-23T18:41:29.217918+02:00\""
---

JSON-WRITE:$ is a legal public span but RAW/STRING can grow, release that span, then copy from it. Rebase aliases into the copied buffer and add public-path growth regressions; no ownership framework.
Claim: unassigned (stale claim cleared 2026-09-16).
