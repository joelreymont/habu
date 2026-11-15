---
title: Reject a bad callback declaration with a record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:54:27.421425+01:00"
closed-at: "2026-11-16T04:07:34.750490+01:00"
---

Problem: src/lib/ffi-callback.f's declaration parser throws bare codes while a file loads. Acceptance: each load-time site prints one placed record in the FFI record form and stops with 70 under bin/hb. Depends: habu-write-a-placed-a652e82a. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
