---
title: Delete the checker handover and target-owner forwarding nothing uses
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T08:01:41.879878+02:00"
---

Problem: src/core/checker.f TRANSFER-CHECKED (:27457) has no caller; the target-owner forwarding (src/habu/interpreter.f :789-793, :957-1000, :2148-2167, :2251-2257, :2341-2382; src/habu/xref.f :488-493; checker.f :16883-16900) calls a second owner record no process is known to hold (inferred by .jj-ws.noindex/carl-layout/.out/design-two-checkers.md). Acceptance: TRANSFER-CHECKED and the words only it reached are deleted; the forwarding is deleted once a census shows no process holds two records, or kept with the line that sets a second one cited beside it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-xfer
