---
title: "Wasm 3: lay a module's data from one base, as non-zero runs"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T16:58:14.882033+02:00"
closed-at: "2026-11-10T20:15:32.380370+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Step 3 of the Wasm campaign. Problem: src/arch/wasm/profile.f puts the return stack and DO frames between the DATA region and IMAGE-BASE, and src/arch/wasm/link.f writes the image as one segment at IMAGE-BASE, so a build's space [BASE, DP) cannot sit offset for offset from WPROF:DATA-BASE, where tier 0's Wasm code addresses header cells; the space's first 2.9 MB is nearly all zero. test/wasm/link.f fails 6 cases on the stack since the profile change of 2c. Acceptance: the stacks lie below DATA-BASE, IMAGE-BASE is gone, WLINK writes the image from DATA-BASE as non-zero runs with cells stored before the module is written, memory covers the DATA region; test/wasm/link.f passes with run cases. Files: src/arch/wasm/profile.f, src/arch/wasm/link.f, test/wasm/link.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-layout.md (engine rebuild, test/wasm/link.f, check-w2d.f, backend.f, select.f). Depends: none. Ownership: the three files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3l.
