---
title: Answer mapped questions for the current map
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T19:20:26.936487+02:00"
---

Problem: PROC-MAPS:MAPPED? (src/habu/proc-maps.f) reads the process map once; lane 11 D1 deleted ADMIT-FILE's per-file RELOAD, so memory mapped after the first read is called unmapped and dep H-POINTER hashes a cell pointing into it as an integer instead of refusing mapped-compile-time-read (+3238 file-level DATA rows over 638 keys in 95 objects, 11B p vs 11D1 p). A RELOAD at WALK-BEGIN is correct but measured too slow (native-build-support 8:22 CPU unfinished vs 109 s for the whole engine build; samples in mach_msg2_trap). Acceptance: a snapshot miss is confirmed by a one-address kernel query and a new area triggers one RELOAD; effects unchanged, no new public word; reproducer refuses before/after; P0 warm == cold with eng/cold CPU within noise; AOT link time before/after. Files: src/habu/proc-maps.f (+ comments in aot-data-cell.f, dep). Verify: brief carl-dots/.out/carl-file/brief-fix-map.md Proof 1-3. Depends: none (on lane 11 tip mlszotnk). Ownership: proc-maps.f. Claim: carl, worker in .jj-ws.noindex/carl-fmap (launch pending).

Implemented in lane 11 as unmrmrst bad9782d "Answer mapped questions for the current map", squashed into uyzslxrr; closes at the lane 11 landing.
