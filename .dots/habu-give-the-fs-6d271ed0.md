---
title: "Give the fs walker's handle and frames records"
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T13:34:07.835975+01:00"
---

Problem: src/lib/fs.f:192-232 lays the walker's handle (WALK-HANDLE-CELLS cells: the frames mapping through ptr-field, CAP, DEPTH) and each frame (six cells, then PATH and DIR at FRAME-PATH-OFF and FRAME-DIR-OFF, FRAME-BYTES apart) out by hand with index constants and offsets, though a record says it: a STRUCTURE frame with six fields and BUFFER path and dir, and a handle record holding the frames. Acceptance: the handle and frames are records whose fields name each cell; the *-CELL and *-OFF constants, FRAME-BYTES, WALK-HANDLE-BYTES and the ptr-field go; the frames sit in a VEC:vector of the frame record (vector.f and heap.f are baked); the fs walk tests pass. Depends: none. Claim: unassigned.
