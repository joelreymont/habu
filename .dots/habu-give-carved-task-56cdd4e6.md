---
title: Give carved task-local rows a record type
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
---

Problem: modules carve a TASK:+USERS byte row into cells by hand-summed offsets and CELL-VIEW, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/process.f:143-160 (STORAGE, its -OFF chain, public PID, RC, OUT-R, OUT-W, ERR-R ( -- ^n ) as CELL-VIEWs), src/lib/crypto/evp.f:142-160 (EVP-STORAGE, CTX-OFF..MAC-LEN-OFF) and :328 (RS-STORAGE), src/lib/serial.f:57 (IO-STORAGE), src/lib/fd.f:133 (POLL-STORAGE). Acceptance: each such row is a STRUCTURE held through TASK:+USER (a byte row stays only where an OS call fills raw bytes, such as fs-statx.f's statx buffer, and says so); no offset sum or CELL-VIEW over a task-local row remains in src/lib; the modules' tests pass. Depends: none. Claim: unassigned.
