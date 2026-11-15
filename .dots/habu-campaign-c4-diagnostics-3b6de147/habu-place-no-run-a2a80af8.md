---
title: "Place no run-time path rejection at the reader's token"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T15:59:35.252814+01:00"
closed-at: "2026-11-16T04:07:34.975201+01:00"
---

Problem: src/core/include.f PATH-PASS (:230) places its record through SRCLOC:TOKEN$ (LOADER-PATH-RECORD :117, LOADER-PATH-AT :107), and SOURCE-ROOT:CANONICAL and CANON-OS reach it from run-time library code (src/lib/build-out.f, tree-copy.f); a built program has no reader token, so its place is whatever the cell holds. Acceptance: a path rejection takes its place from the loader that read the path, and run-time callers write it with no place. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (integration, lands with C4).
