---
title: "Wasm 3: a Wasm-target build throws E-COMP-STATE at the first start-up file"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:42:07.992674+02:00"
---

Step 3 of the Wasm campaign, first finding. Problem: with tier 0's Wasm generator in hb and wasm as a target name (src/compiler/target/model.f PLATFORM), 'bin/hb --load tools/hb-build.f -- --target wasm m.f -o m.wasm' for a file ': MAIN ( -- ) 53 emit ;' exits 74 with 'hb-build: src/core/util.f threw -8570' (E-COMP-STATE) in 0.3 s; the cause is not known. Acceptance: the cause named as path:line with the definition of util.f it happens on and the state that differs from the generator check's working path; then its fix, a second brief. Files: none (a hunt). Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-hunt.md. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3h.
