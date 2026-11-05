---
title: Stamp the callback stub into a native shadow (step 5 S1b)
status: active
priority: 2
issue-type: task
created-at: "2026-11-05T15:53:40.821200+02:00"
---

Problem: under a native shadow (NSHADOW:OPEN-NATIVE, the arm64-to-arm64 build's mode) T0's HOSTED (src/habu/t0.f) seals the host walk unplaced for the shadow before STUB-ARM, so a CALLBACK:'s -ENTRY copy lacks the C entry stub the placed host routine carries (measured on the S1a candidate: KD-CB-ENTRY host 48 bytes, shadow 12; /tmp/jerry-s1b/native-check-base.log) and the host stub pairs with no shadow function, so a product cannot map its entry cell to its own stub. DECISIONS 2026-11-03 'T0 stamps the stub for both machines'. Acceptance: STUB-ARM before the unplaced seal; the seam stamps the stub once per emission (the first seal appends it, later seals reuse its offset); NSHADOW's SOURCE-FUNS keys the stub adjustment on whether the copy was taken while the stub was armed (tier 1 unchanged); test/compiler/shadow-kinds.f opens a native shadow and reads KD-CB-ENTRY with CHECK, CB-CHECK and SHADOW-KINDS:STUB gone. Also S2's proof: constant, create, defer, does> and CAST: load under the typed loop with a native shadow open. Files: src/habu/t0.f, src/habu/t0-emit.f, src/compiler/native/shadow.f, test/compiler/shadow-kinds.f, test/compiler/shadow-kinds-fixture.f, test/gate-arm-cases.f. Verify: candidate hb; rows compiler-shadow aot-shadow-capture ffi-callback. Depends: none. Ownership: those files. Brief /tmp/jerry-big/brief-s1b.md. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s1b.
