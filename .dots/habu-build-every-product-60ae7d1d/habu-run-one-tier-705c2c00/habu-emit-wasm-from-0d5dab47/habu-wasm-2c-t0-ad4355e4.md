---
title: "Wasm 2c: T0's Wasm generator, DO loops and the return stack"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T10:23:46.530737+02:00"
closed-at: "2026-11-10T20:15:32.676312+02:00"
close-reason: "landed in master e4ec011f; its gate: engines converge, 12 focused tests rc 0, suite 273 of 273, chain fixpoint at generation 2"
---

Step 2 of the Wasm campaign, third lane. Problem: a definition with do, leave, i, >r or r@ compiled for Wasm ends hb with 'hb: unset quotation' (exit 86): src/habu/t0-wasm.f leaves the return-stack and loop cells of the tier 0 seam unset, and a module gives the two stacks no memory. Acceptance: src/arch/wasm/profile.f places the return stack and the DO frames after the DATA region (RETURN-BASE, LOOP-BASE; the image follows); t0-wasm.f fills TOR, RFROM, RFETCH and the nine loop cells, keeping the depths in the DATA cells native code uses, with a bounds fault on each push since Wasm memory has no guard page; a DO level is a block around a loop. Not in this lane (they need catch and the run wrapper, so they go with the write step's routines): run zeroing the two depths, and catch and finally restoring them. Files: src/habu/t0-wasm.f, src/arch/wasm/profile.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w2c.md (the engine builds; the control-flow check extended with loop and return-stack words validates and runs to status 0; test/wasm/link.f and select.f pass). Depends: habu-wasm-2b-t0-67604f37 (merged in the integration workspace). Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w2c.
