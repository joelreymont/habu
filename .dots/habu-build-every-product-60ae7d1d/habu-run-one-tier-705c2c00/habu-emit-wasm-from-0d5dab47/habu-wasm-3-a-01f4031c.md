---
title: "Wasm 3: a Wasm-target build throws E-COMP-STATE at the first start-up file"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:42:07.992674+02:00"
---

Step 3 of the Wasm campaign, first finding. Problem: with tier 0's Wasm generator in hb and wasm as a target name (src/compiler/target/model.f PLATFORM), 'bin/hb --load tools/hb-build.f -- --target wasm m.f -o m.wasm' for a file ': MAIN ( -- ) 53 emit ;' exits 74 with 'hb-build: src/core/util.f threw -8570' (E-COMP-STATE) in 0.3 s; the cause is not known. Acceptance: the cause named as path:line with the definition of util.f it happens on and the state that differs from the generator check's working path; then its fix, a second brief. Files: none (a hunt). Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-hunt.md. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3h.

Cause (hunt, 2026-11-10): src/habu/linker.f EMISSION-CODE+ `size 0= if E-COMP-STATE throw then`, reached from LINKER:LOAD's `-1 PIECE-END` through PIECE+ and CODE+ at the end of src/core/util.f. A cross build laid the piece's object code section from its shadow, and EMISSION-CODE+ reads every address site as an arm64 chain (ADDRESS-CARRIER:CHAIN-SIZE); emission 0 (`variable IMK-NDICT0`) holds Wasm `i64.const` there. Fix (in the Wasm stack, "Lay no object code in a cross build"): CODE+ writes an emission count of 0 in a cross build, which writes no object (WRITE-OBJECT, LOAD); the per-machine site encoding joins the object when an object carries the target's and the host's copies (warm cross builds). Shown: the same build passes util.f and stops next at src/habu/layout.f:59, `HB-TARGET-LINUX-X86-64?` undefined (no target file loads for Wasm), which the census dot takes. Close when the Wasm stack lands.
