---
title: Make the arena ceiling reachable
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T17:54:45.308678+02:00"
---

Full context: DEFECT found by the Rocq storage proof and CONFIRMED live. src/compiler/ir/arena.f:13-15 calls the abandoned-span discipline bounded by the committed ceiling. The real bound is the context's single 64K mapping: reaching capacity C costs 8+16+...+C cells of a 65408-byte scratch region, so the largest reachable capacity is 2048. Proven: an arena with ceiling 4096 dies with E-IR-CTX-SCRATCH (-6644), never E-IR-ARENA-FULL (-6652) — a different code from the one the caller was told to expect, and one that the file's own promise that exhaustion never kills the arena does not describe. Atomicity is unaffected. Fix: either reject at IR-ARENA:NEW a ceiling whose doubling chain cannot fit SCRATCH-CAP (a CEIL-OK extension, cheap and structural), or stop abandoning spans by reserving the ceiling once or freeing the old span. Correct the two comment blocks afterwards. Regression: IR-ARENA:NEW with ceiling 4096 must fail AT CREATION with E-IR-ARENA-CEIL, not mid-fill with a context error. Related: habu-size-compiler-ctx-640d85fb.
