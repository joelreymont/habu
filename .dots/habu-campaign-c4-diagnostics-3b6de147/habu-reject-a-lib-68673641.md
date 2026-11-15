---
title: Reject a library name past the path cap
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T07:46:08.900613+01:00"
closed-at: "2026-11-16T04:07:34.669577+01:00"
---

Problem: VERSIONED-LIBRARY with a base whose made name passes LIBRARY-PATH-CAP ends in a bare uncaught -4700 from src/lib/ffi-abi.f LIBRARY-NAME$ (:591), rc 67, no record. Acceptance: one record at the base token naming the cap, rc 70. Depends: habu-keep-a-declared-72547576. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
