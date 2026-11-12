---
title: "Wasm 6a: the shadow keeps the host's copy of a definition"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:54:45.388472+02:00"
closed-at: "2026-11-13T00:29:17.841843+02:00"
close-reason: "the shadow keeps a definition's host copy beside its target copy"
---

Problem: in a build for another machine T0 compiles each definition for the target and for arm64 (src/habu/t0.f SHADOWED, HOSTED :1891-1965), and the arm64 copy's unplaced form and rows are taken into the shadow only when it is native (:1962), so a file's object cannot carry it. Design: .jj-ws.noindex/jerry-wasm/.out/warm-design.md section 3, with the lead's cuts in step3-design-notes.md 'Twenty-fourth session'; brief .out/briefs/s1.md. Acceptance: in a non-native shadow a definition files its target emission and then its host emission (NSHADOW:TAKE-HOST, TAKE-HOST-DOES, HOST@, ARCH@; PUBLISH, ABANDON and PATCH-DOES cover the pair); a native shadow files one, as before; link-band.f REACH-ALL marks by record; a Wasm build's module and an arm64 build's program are byte-equal to the parent engine's. Files: src/compiler/native/shadow.f, src/habu/t0.f, src/habu/link-band.f, the tests that count a cross shadow's emissions. Verify: cmp of the viewer copy's Wasm module and arm64 program built by the parent's engine and the lane's; shadow-kinds, arm64-place, wasm/backend, file-objects, linker and wasm/hb-build tests. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s3w.
