---
title: Delete the dead E-PATH-NUL arms
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.180317+01:00"
closed-at: "2026-11-16T04:07:36.347534+01:00"
---

Problem: src/habu/verify-source.f (~:3396) and tools/source-discovery.f (~:1112) keep E-PATH-NUL arms no throw reaches since the loader labels its path rejection. Acceptance: the arms are gone, or a reaching path is shown and pinned. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
