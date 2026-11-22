---
title: "Give FS-LIST's mapped context a record"
status: active
priority: 2
issue-type: task
created-at: "2026-11-22T13:29:54.798050+01:00"
---

Problem: src/lib/fs-list.f:31-45 lays its mapped listing context out by hand: STORAGE-BYTES (2 cells + BLOCK-CAP), FD-CELL and BASE-CELL as CELL-VIEWs and BLOCK at an offset, though a STRUCTURE with two fields and a BUFFER block BLOCK-CAP says it. Acceptance: the context is a private STRUCTURE (the getdents bytes a BUFFER field the kernel fills); STORAGE-BYTES, FD-CELL, BASE-CELL and BLOCK's offset go; the fs-list tests pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tfsl.
