---
title: Shared MEM narrowing helper for allocation callers
status: active
priority: 3
issue-type: task
created-at: "\"2026-07-15T10:47:30.377282+02:00\""
---

Follow-up from habu-migrate-lib-allocation-f8c6821f (landed cfbfdba0): the six caller files each inline the same CAD-NUM:BYTE-LEN -> MATCH -> AS-ALLOC-BYTE-LEN -> MATCH narrowing block (repeated because lib/memory.f was another lane's file and flat-file helpers become public stdlib words needing manifest rows). Fix: add public MEM:BYTES-ALLOC-LEN ( n -- CAD-NUM:alloc-byte-len ) in lib/memory.f (throwing E-MEM-SIZE on refusal; manifest row; memory-test coverage incl. zero/negative/overflow), then collapse the six inline blocks to it - byte-identical positive behavior pinned by the existing focused suites. Also consider MEM:CELLS-ALLOC-COUNT for the cell side before the vector wave uses it. Files: lib/memory.f(+test), lib/std.manifest, the six caller files, TRUSTED.md untouched. Verify: memory-test, six focused suites, lint-manifest, full run.f. Ownership: MEM surface. NOTE: coordinate with the remaining allocation waves (vector 360069d6, test 0e295089, tool 22c04eb4, maki 3335b802) - land the helper FIRST so those waves use it.

Claim: agent=memhelper workspace=.jj-ws/fable-cadnum
