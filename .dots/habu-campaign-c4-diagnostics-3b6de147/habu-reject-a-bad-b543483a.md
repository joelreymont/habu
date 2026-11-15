---
title: Reject a bad FFI library with a record
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:54:27.408989+01:00"
closed-at: "2026-11-16T04:07:34.740444+01:00"
---

Problem: src/lib/ffi-abi.f's library, name, version and scope checks throw bare E-FFI-SYNTAX or E-FFI-LIBRARY while a file loads. Acceptance: each load-time site prints one placed record and stops with 70 under bin/hb; run-time sites are named and unchanged. Depends: habu-write-a-placed-a652e82a. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
