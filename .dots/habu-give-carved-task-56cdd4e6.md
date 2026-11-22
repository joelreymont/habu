---
title: Give carved task-local rows a record type
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T12:55:57.007232+01:00"
---

Problem: modules carve a TASK:+USERS row into cells or slots by offsets, CELL-VIEW, ptr-field or index, though TASK:+USER answers a typed ( -- ^t ) accessor: src/lib/svg-pdf.f:71-75 (STATE: handle cells by ptr-field, then the viewport), src/lib/zip-state.f:30-32 (ZIP-SCRATCH: a stat block, patch and drain bytes, an EOF byte), src/lib/unicode.f:45-48 (RESULT-ROW by CELL-VIEW), src/lib/zlib.f:53-58 (SCRATCH: z_stream and a probe byte by ptr-field and offsets), src/lib/time-cpu.f:26-43 (CPU-SPEC read by CELL-VIEW), src/lib/task.f:1672-1678 (SLEEP-SPECS, two timespecs by CELL-VIEW), src/lib/font.f:104-112 (RENDER-STATE and HULL-STATE slots by index), src/lib/process-command.f:43-46 (CMD-TASK slots by index), src/lib/c2-memory.f:30-56 (OWNER-REGION: five header cells and FRAME-CAP frames by offsets and casts). A C struct's record reaches its foreign call through BYTE-VIEW, which refuses a record pointer until habu-let-a-record-8555bd9c lands. c2-memory's frames are also written by the c2-init-stow and c2-records-stow primitives at fixed offsets (src/habu/habu1.f:3300-3305, :3331-3334), whose rows take the frame as ^n (src/habu/prims.f:283-290), so a frame record needs those rows to name it. Acceptance: each such row is a STRUCTURE held through TASK:+USER(S), or one row per role (a byte row stays only where an OS call fills raw bytes and says so); no offset sum, index or CELL-VIEW carves a task-local row in src/lib; the modules' tests pass. Depends: habu-let-a-record-8555bd9c. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-trow, .jj-ws.noindex/jerry-tzip.
