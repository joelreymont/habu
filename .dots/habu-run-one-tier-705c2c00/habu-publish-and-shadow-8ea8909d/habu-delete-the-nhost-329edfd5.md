---
title: Delete the NHOST edge table and call columns
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T20:11:32.503977+02:00"
---

L3-C3. Joel 2026-10-28 ruling: delete the NHOST edge table (host.f EDGE+, EDGE-TO/SITE/KIND/LOC, IMPL-FIRST/IMPL-EDGES: written, never read) and the impl and loc call columns; NEMIT:CALL-SITE+ loses them and the file-object call row shrinks (no version; caches rebuild). Acceptance: host.f edge buffers and EDGE+ gone; host-publish.f TAKE-EDGES/EDGE-TARGET/IMPORT-EDGES/IMPORT-EDGE-TARGET gone; NEMIT CALL-IMPL/CALL-LOC buffers and readers gone, CALL-SITE+ ( site kind target -- ); every producer (x86-64 emit-x64.f X64EMIT:CALL-IMPL@/LOC@, arm64 passes.f, wasm encode.f/passes.f WENC readers, t0-emit.f CALL-LOC, t0-arm64.f PCALL-LOC) drops them; NART CALL-IMPL@/LOC@ and their cells gone; tools/native-unit-capture.f binds and native-unit-object.f DEF-CAPTURE-CALLS/DEF-CALL+ row (site kind ref) and its readers shrink; NHOST:ID-OF deleted if unused. Files: src/compiler/native/host.f, host-publish.f, emission.f, emit-x64.f, session/emission.f, src/arch/*/passes.f, src/arch/wasm/encode.f, src/habu/t0-emit.f, t0-arm64.f, tools/native-unit-capture.f, native-unit-object.f, test/wasm/encode.f. Verify: candidate build, focused rows, native-file-*, full suite. Depends: L3-C2 (ktmkppmt). Ownership: those files. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-l3 (yltmqoko on ktmkppmt).

Review fold-ins (Joel 2026-10-28 rulings): delete A64EMIT:MAP-SPAN@ and the source-span buffers M-ST-BUF/M-LN-BUF/M-SRC in src/compiler/native/emit.f (MAP! keeps only M-OFF; header says offset map); one exported call-row width/offset constant in tools/native-unit-file.f (NUNIT-FILE) used by native-unit-object.f DEF-CALL-BYTES and native-file-reference.f CALL-BYTES and the reference offset 16. Load-timeout rows to rerun at the stack gate: lsp-boundary (lsp-entry-test E-PROC-TIMEOUT), prefix-provided-unfilled (E-PROC-TIMEOUT at 600 s, load 125).

FINAL (yltmqoko). Fold-in re-review: code claims pass; its three LOW comment findings (stale source-map wording in emit.f, passes.f, emit-x64.f, errors.f; call-row numbers repeated in native-file-reference.f prose; NUNIT-FILE row comment) folded by the lead. The five load rows (lsp-boundary, prefix-provided-unfilled, check-drivers, check-verify, check-cli-boundary) pass on candidate jerry-l3/.out/c3b at load ~40-60: logs jerry-l3/.out/proofs3/fold/.
