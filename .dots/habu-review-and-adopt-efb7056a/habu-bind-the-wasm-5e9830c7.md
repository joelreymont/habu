---
title: Bind the Wasm module to HBR2 and release it
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.927947+03:00"
---

Problem: a Wasm module is not a browser release; HBR2 §24.5 fixes the wrapper (hbr_control, hbr_reserve_input, hbr_start, hbr_ingest, hbr_step, hbr_stop; imports submit and wake), §21.2 the release set, and PA-r2 §19-§20 the callback verifier and sidecar (P7, without P4). Acceptance: a generated wrapper and codecs from the pinned registry; CallbackAdmission records and runtime fuel over UI-ADMIT's pass (lib/ui/admit.f, habu-admit-callbacks-by-13caa95b; loop <= 64, cost 2,048, 256 nodes, 16 KiB); a PortabilityBuildDescriptor sidecar without a contribution-root field until P4; no core start function; W08-W19 and W27-W28 pass in at least one real browser profile; W29-W30 run against the HBR2 owners' tests; W04 (import with the right name and wrong signature rejected at admission), W07's browser-Idle behaviour and N04's HBR2 u32 Bool half, moved from P6. Files: lib/browser/ (codec generator, wrapper), src/arch/wasm/admit.f (new; emits CallbackAdmission evidence and lowers fuel over lib/ui/admit.f's pass), host/browser/ (HBR2 adapter), test/wasm/hbr2-*.f. Verify: a browser boots the release through the adapter and the STOP packet round-trips; a registry mismatch rejects before bootstrap (W27). Depends: habu-emit-a-wasm-05443776, habu-pin-hbr2-wire-0b340032, habu-build-hbr2-runtime-731d5ddd. Ownership: lib/browser/, host/browser/, src/arch/wasm/admit.f. Lane: tim. Claim: unassigned.
