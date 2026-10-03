---
title: Build HBR2 BROWSER and the host adapter
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T22:32:28.913239+03:00"
---

Problem: HBR2 §1.1 requires a closed native interaction adapter in host/browser/ and lib/browser/ transport, DOM, input, editing and capability packages; none exists; Maki writes no JavaScript (maki AGENTS.md), so this generic host is Habu's. Acceptance: lib/browser/ transport and DOM/input/form packages; a host/browser/ generic adapter and worker driving the six exports serially, reacquiring views after growth; HBR2 gate G2 (two-field Apply/Cancel through a durable receipt against a server fixture) in one real browser. Files: lib/browser/ (new), host/browser/ (new), test/browser/. Verify: the G2 fixture; W12-W17 pass. Depends: habu-build-hbr2-runtime-731d5ddd, habu-emit-a-wasm-05443776, habu-pin-hbr2-wire-0b340032. Ownership: lib/browser/, host/browser/. Lane: tim. Claim: unassigned.
