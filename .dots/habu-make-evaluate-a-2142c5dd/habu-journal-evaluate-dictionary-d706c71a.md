---
title: Journal EVALUATE dictionary truncation
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-14T20:10:10.572696+02:00\""
---

Full context: src/habu/xref.f HIDE-DEFS-FROM and FORGET-DEFS-FROM lower NDICT/CP and destructively truncate checker USIG rows. A caught failed EVALUATE then restores only high-water pointers, leaving pre-entry dictionary records/code/checker rows overwritten. Instrument HIDE to journal the DREC suffix before authorized ndict! and checker terminator/row mutation; instrument FORGET to journal code [newCP, entryCP), DREC [newNDICT, entryNDICT), and checker truncation before authorized CP/NDICT rewind. Raw cp!/ndict!/patch32 must reject under EVALD; only the sealed scoped xref path may mutate. Nested inner commit must remain undoable by outer rollback. Files owned: src/habu/xref.f and focused xref transaction tests only; no engine/checker/layout edits. Depends on the engine journal/authority API in parent dot.
