---
title: "FFI's forget path has no caller"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:47:49.029531+02:00"
---

Problem (by reading at the integration stack, 2026-11-10): once the capture hooks are gone, src/lib/ffi-abi.f FORGET-CACHE (:786, public) has no caller in src, tools, test, docs, ../etch or ../tender (Etch's kiapi/transport.f FORGET-SYMBOLS is its own word), and it is FORGET-SYMBOLS's (:764) only caller. FORGET-SYMBOLS is the only caller of CLOSE-BANK, CLOSE-SLOT, CLEAR-BANK, DRAIN-RETIRED and the three FORGET-STAGED calls, and the only user of the dlclose row (DLCLOSE-FN-CELL :501, :1014) and E-FFI-DLCLOSE (src/lib/error.f:175). DRAIN-RETIRED is the only word that empties the retired-table list (RETIRED-BASE, RETIRED-SIZE, 64 rows, :249-251; RETIRE :281-286 appends), so today a process that grows FFI's tables 65 times with no drain is refused by the list's bound: reproduce that first. FN-RESOLVE (:777) repeats FN-LOOKUP's (:511) nonzero check. Acceptance: a reproducer that shows what the 65th table growth does; then either the forget path is deleted with what only it uses and retired tables are released or kept by a stated rule, or FORGET-CACHE gets its caller; FN-RESOLVE's callers call FN-LOOKUP; comments that cite FORGET-SYMBOLS as a capture step (src/lib/os-cpu.f:4, src/lib/os-memory.f:5, src/lib/ffi-abi.f:728, :1013) say what holds. Files: src/lib/ffi-abi.f, src/lib/error.f, src/lib/os-cpu.f, src/lib/os-memory.f. Verify: engine built twice equal; suite ffi-abi. Depends: the image-lifecycle removal (habu-delete-image-lifecycle-96ec25ea) landing. Ownership: src/lib/ffi-abi.f. Claim: unassigned.
