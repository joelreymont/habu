---
title: Comments say a process starts on the JIT
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:17:29.201271+02:00"
---

Problem: found by the JIT reach census (.jj-ws.noindex/jerry-wasm/.out/jit-census-notes.md item 4): src/habu/habu2.f :9053 and src/habu/layout.f :860-876 say a process starts on the JIT; no engine entry reads tokens through habu2.f LMAIN and tier 0 is the compiler in COMP-CELL (T0). Acceptance: both comments say what runs, or go with the code the JIT deletion removes. Files: src/habu/habu2.f, src/habu/layout.f. Verify: read the two comments against the census. Depends: goes with habu-delete-the-jit-d9fdaf67's lanes. Ownership: those lanes. Claim: unassigned.
