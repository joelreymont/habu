---
title: Built programs print run-time records
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T15:46:32.501834+01:00"
closed-at: "2026-11-16T04:07:36.656271+01:00"
---

Problem: C4's run-time records (src/core/dynamic-storage.f MAP-REFUSED, src/lib/fs-mutate.f TMPDIR-REJECT) cannot be built into a stripped program: any DYNAMIC-BUFFER or HEAP program is rejected (E-CANNOT-RUN-STRIPPED via SRCLOC:TOKEN$; on Wasm REJECTION:OPEN..SAVE reach here); test/wasm/hb-build.f PASS-CASE fails. Acceptance: those records carry no place, build native and Wasm, and print when a built program leaves the code uncaught; wasm-device passes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (integration, lands with C4).
