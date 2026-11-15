---
title: Delete unreachable stops
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:09:22.012133+01:00"
closed-at: "2026-11-16T04:07:36.298153+01:00"
---

Problem: the overlay-open stops, ADDRESS-CELLS:PERSIST and its stop, two dead clobber-lint entries, LISSUE's x30 save and sumtype.f's transaction install checks are unreachable from a program. Acceptance: deleted; build converges; named tests pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
