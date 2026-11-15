---
title: "Wasm 6b: an object keeps call and slot bytes as compiled"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T21:58:53.235174+02:00"
closed-at: "2026-11-13T00:29:17.861383+02:00"
close-reason: "an object keeps a call site's bytes as compiled"
---

Problem: src/habu/linker.f EMISSION-CODE+ (:1309-1310) writes 0 over each call site's 4 bytes and the does> slot's 4 bytes in an object, CALL-CK (:2538) and SLOT-CK (:2578) require the 0, and NEMIT-FILL (:3379, :3400) writes arm64's unplaced call and ret back: arm64's encoding, which cannot serve a Wasm emission (a 5-byte index after an opcode, a 6-byte slot). The compile's sealed bytes are already the unplaced form. Acceptance: the object keeps those bytes as sealed, the checks keep only their bounds tests, the fill restores nothing; address chains unchanged; hb built cold and warm by the lane's engine is equal before the signature, and the viewer copy's arm64 program equals the parent engine's. Files: src/habu/linker.f. Verify: the engine cold/warm compare, the viewer program compare, test/linker.f, test/file-objects.f. Depends: none. Ownership: jerry. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-s2b.
