---
title: "Refuse a top-level call or tick of HEAP's runtime words before the run"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T15:04:35.431765+02:00"
---

Problem (arena 2b1 worker, carl-arena2 qvuplkqv): a file holding only "' DYNAMIC-STORAGE:RECORD-RELEASE drop", or only 'NULL-PTR 16 DYNAMIC-STORAGE:RECORD-RELEASE', passes tools/check.f --verify-only rc 0 with no packet, on the installed engine too (ARENA-RELEASE the same), while bin/hb --load refuses it rc 70 'arena call: ...' (checker.f TOP-TICK-STEP 32041 and the call path at 24710 reach ARENA-CALL-REFUSE?, 24501). A body tick (': R4 ( -- ) ['] DYNAMIC-STORAGE:RECORD-RELEASE drop ;') is refused E-ARENA-CALL under --verify-only. Cause not found: the pre-pass's top-level step does not reach ARENA-CALL-REFUSE?, or reaches it with LBUF-PEND-MATCH? true. Acceptance: both one-line files refused E-ARENA-CALL under --verify-only, plain check and --all-errors, as the load refuses them; test/arena.f VERIFY-CALLS' count of 4 (carl-arena2 qvuplkqv, which adds the top-level tick to VERIFY-SRC$) passes once the arena stack sits on the fix. Files: src/habu/verify-source.f or src/core/checker.f (the hunt names which). Verify: the two one-line files in the three modes. Depends: none. Ownership: the pre-pass's top-level call gate. Claim: unassigned.
