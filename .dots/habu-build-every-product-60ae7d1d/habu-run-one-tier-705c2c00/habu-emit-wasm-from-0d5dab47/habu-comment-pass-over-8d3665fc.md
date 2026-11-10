---
title: Comment pass over the Wasm stack from the feature review
status: closed
priority: 3
issue-type: task
created-at: "2026-11-10T19:22:00.101476+02:00"
closed-at: "2026-11-10T20:15:33.240624+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Problem (feature review A F7, B F9): false, plan-worded or over-long comments in src/arch/wasm/link-routines.f (:65-66, :135-136), src/habu/t0-wasm.f (:7-8 and the blocks over two lines), src/arch/wasm/link.f (header, the wrappers, the sections), src/habu/link-band.f (SITE-VALUE, OWN-FUNCTION, LIVE-TARGET, ROOT, EM-TRAILER), src/arch/wasm/link-module.f CALLEE, src/habu/link-copy.f's 15-line block, src/arch/wasm/profile.f :33-35 and :40-43. Acceptance: each is true and within the card's 9 Comments; design text sits in docs/xcomp.md. Files: those. Verify: engines converge (comments only). Depends: none. Ownership: the lead. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
