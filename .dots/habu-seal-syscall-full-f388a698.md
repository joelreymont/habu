---
title: "Seal: syscall full-range overlap guards"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:30:09.220031+02:00"
---

2b-i deferral. PROT-GUARD at syscall sinks (BREAD x1, BREADLINK x2, BSTAT64/BLSTAT64 x1, BGETDIRENTRIES64 x1/x3, BPOLL x0, BIOCTL x2, BMMAP x0) checks only the buffer START address; a buffer starting below DATA+FRIEND-ARENA () that spans the arena (e.g. data-base 0 +  read, or MAP_FIXED mmap of DATA page 0) writes over the latch+jewels undetected. Fix: overlap check start<arena_end && start+len>arena_start where len is knowable (read/readlink/getdirentries take len regs; stat has fixed struct size). BIOCTL write extent is request-dependent — needs a request-size model or protected-pointer provenance; coordinate with 2b-iii cat 5 (habu-tfam-2b-iii-d8af2634). Mirror in bootstrap/cg/forth.fs. Negative fixtures: sub-arena-start read spanning latch must exit 83; positive: legit low-cell reads unaffected.
