---
title: Drop the unread FFI producer binds
status: open
priority: 3
issue-type: task
created-at: "2026-10-31T17:23:15.268699+02:00"
---

Problem: tools/native-unit-capture.f binds FFI:NEXT-FN, FFI:FN-ROW@, FFI:LIB-ROW@, FFI:MAX-ARGS, FFI:SYMBOL-CAP, FFI:SELECTION, FFI:OBSERVE, FFI:UNOBSERVE and FFI:MAKER-BANK with BIND-ONE, and no BOUND-XT, BOUND-PRODUCER-XT or OPTIONAL-PRODUCER-XT caller in tools/, src/ or test/ names them (lane 11 f7 worker; lane 11 already dropped FFI:MARK). Acceptance: each bind with no reader is removed, each one kept names its reader; test/ffi-callback-file-cache.f and test/native-file-dynamic-load-e2e.f pass. Depends: lane 11 landed. Ownership: carl. Claim: unassigned.
