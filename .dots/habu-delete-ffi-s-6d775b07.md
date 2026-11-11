---
title: "Delete FFI's span scan, which nothing calls"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T10:56:12.323080+02:00"
---

Problem (review of the observer deletion, 2026-11-11, by search only): nothing in Habu, Zen, Tender or Maki calls FFI:SCAN-SPANS (src/lib/ffi-abi.f) or the SCAN-SPANS in src/lib/ffi-callback.f, apart from their own CODEGEN:SCAN-SPANS calls; TABLE-SPANS, AOT-CONTROLS, MAKER-FRESH-CONTROLS and AOT-TABLES-AT are reached only from there, and the comment above AOT-CONTROLS says the linker gets the table's exact storage through the two callbacks, which no code does. Not traced: when the scan lost its caller, whether CODEGEN:SCAN-SPANS has other callers, and whether CONTEXT-CELLS or PARSER-SPANS lists share the cells. Acceptance: each word is traced (rg -i --no-ignore over src tools test and the three consumers, then an engine build without it) and deleted with its comments, or kept with its caller named here. Files: src/lib/ffi-abi.f, src/lib/ffi-callback.f. Verify: the engine built twice; ffi-test, zip-test, file-objects rc 0; Maki, Zen, Tender warm = cold at landing. Consumers: none meets it. Depends: the observer deletion landed (habu-delete-ffi-s-b7ab1316, same file). Ownership: carl. Claim: unassigned.
