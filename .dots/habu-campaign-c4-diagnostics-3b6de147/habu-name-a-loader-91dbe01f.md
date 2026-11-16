---
title: "Name a loader's record as the program wrote it"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T06:52:14.007393+01:00"
---

Problem: the runner names the loader word in lower case whatever the source wrote; INCLUDE-PATH0's record names INCLUDE-PATH0, not the public word that called it; discovery's provided names itself and an escaped literal falls back to SRCLOC:TOKEN$; the sealed-package record in src/core/include.f stands at SRCLOC:TOKEN$. Acceptance: each record names the word as the source spelled it and stands at a token the reader holds, the same in the three readers. Depends: habu-reject-a-path-672d66ba. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w225.
