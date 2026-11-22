---
title: Give carved task-local rows a record type
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
---

Problem: modules carve a TASK:+USERS row into cells or slots by offsets, CELL-VIEW, ptr-field or index, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/process.f:111-175 (STORAGE: path, pollfds, probe byte and 15 cells; public PID, RC, OUT-R and the rest ( -- ^n ) as CELL-VIEWs; STORAGE and RAW-PID-OFF exported for PROC-RAW), src/lib/crypto/evp.f:140-160 (EVP-STORAGE) and :303-328 (RS-STORAGE, 22 cells, C out-parameters), src/lib/svg-pdf.f:71-75 (STATE: handle cells by ptr-field, then the viewport), src/lib/c2-memory.f:36-41 (OWNER-REGION by CELL-VIEW), src/lib/zip-state.f:30-32 (ZIP-SCRATCH: a stat cell and an EOF byte), src/lib/unicode.f:45-48 (RESULT-ROW by CELL-VIEW), src/lib/zlib.f:53-58 (SCRATCH: z_stream and a probe byte by ptr-field and offsets), src/lib/time-cpu.f:26-43 (CPU-SPEC read by CELL-VIEW), src/lib/task.f:1672-1678 (SLEEP-SPECS, two timespecs by CELL-VIEW), src/lib/genio-core.f:83-89 (IO-SCRATCH by CELL-VIEW), src/lib/font.f:104-112 (RENDER-STATE and HULL-STATE slots named by index), src/lib/process-command.f:43-46 (CMD-TASK slots by index), src/lib/net/tcp4.f:155-163 (IO-STORAGE carved into sockaddr, addrlen, option and pollfd). Acceptance: each such row is a STRUCTURE held through TASK:+USER, or one row per role (a byte row stays only where an OS call fills raw bytes, such as fs-statx.f's statx buffer or fd.f's pollfd, and says so); no offset sum, index or CELL-VIEW carves a task-local row in src/lib; the modules' tests pass. Depends: none. Claim: unassigned.
