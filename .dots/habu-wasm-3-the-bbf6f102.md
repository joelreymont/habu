---
title: "Wasm 3: the linker's reach reads a Wasm shadow"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T17:17:51.056760+02:00"
---

Step 3 of the Wasm campaign, part of habu-wasm-3-linker-0a8d4f6b. Problem: the reach (src/habu/link-band.f CHAIN-AT, PLACE-TARGET, ROOT; link-data.f READ-EM) reads an address site as an arm64 chain, looks up a Wasm emission's own function offsets as host addresses, and keeps hb's arm64 kernel code for a primitive; a Wasm module's '.', 'type', 'cr' are Habu words of src/arch/wasm/kernel-words.f the reach must make live instead. Acceptance: SITE-VALUE by the shadow's machine, own functions skipped, a kernel target a WKWORDS word provides makes that word's record live (new src/arch/wasm/link-kernel.f, the map moved to src/arch/wasm/kernel-map.f), an arm64 engine and stripped programs byte-identical before the signature. Files: src/habu/link-band.f, src/habu/link-data.f, src/arch/wasm/link-kernel.f, kernel-map.f, kernel.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-reach.md. Depends: none. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3r.
