---
title: Merge package FFI-DECL into FFI
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T13:08:28.771204+02:00"
---

Problem: Joel 2026-10-29: FFI-DECL (src/lib/ffi-abi.f:1245-2042) merges into FFI; REFUSE and CLAUSE-TARGET become private parser helpers; the SCAN-SPANS clash gets one name per meaning. Acceptance: one package FFI, no FFI-DECL reference left in the tree, FFI:SCAN-SPANS visits registry and parser spans with its owner callers registering it once. Brief: .jj-ws.noindex/carl-ffimerge/.out/brief-ffi-merge.md. Files: src/lib/ffi-abi.f, src/habu/verify-source.f, src/lib/f64-text.f, src/lib/ffi-test.f, test/ffi-struct-arg.f, test/native-file-frame-e2e.f, tools/native-unit-{capture,dependency,object}.f, src/habu/aot-data-cell.f. Verify: engine build, ffi-test, ffi-struct-arg, native-file-frame-e2e, hb-build-stripped-cells-test, full suite. Depends: none. Ownership: those files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-ffimerge
