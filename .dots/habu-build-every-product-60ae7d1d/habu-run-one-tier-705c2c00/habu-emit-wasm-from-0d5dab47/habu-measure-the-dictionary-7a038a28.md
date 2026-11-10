---
title: Measure the dictionary scan a Wasm reach makes per kernel-word site
status: closed
priority: 4
issue-type: task
created-at: "2026-11-10T19:18:46.650890+02:00"
closed-at: "2026-11-10T21:55:03.351295+02:00"
close-reason: "measured in the cold Wasm build's profile: the dictionary scan is negligible; no change"
---

Problem (feature review B, F8, unmeasured): src/habu/link-band.f TARGET-REC calls WASM:PROVIDER, which scans ndict@ by name (link-kernel.f NAME-AT), for every kernel-call site in LIVE-TARGET and PLACE-TARGET and again in CALLEE; KERNEL-FUNCTION caches by address only after the scan. Acceptance: the scan's share of the link of Maki's viewer is measured; if it shows, an address-to-provider cache per link. Files: src/habu/link-band.f, src/arch/wasm/link-kernel.f. Verify: the measurement, with the warm Wasm builds step. Depends: none. Ownership: none. Claim: unassigned.
