---
title: "Wasm 3: a Wasm program build runs no arm64 kernel build"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T17:12:16.052994+02:00"
---

Step 3 of the Wasm campaign, part of habu-wasm-3-linker-0a8d4f6b. Problem: tools/hb-build-core.f APP runs KERNEL-BUILD for every target, so a Wasm build writes and adopts an arm64 kernel object; no build loads src/arch/wasm/kernel-words.f, the Habu words (WKWORDS) a module's '.', 'type' and 'cr' resolve to. Acceptance: a Wasm APP build stores its target and paths without KERNEL-BUILD and loads kernel-words.f after the start-up files; the native app build is unchanged. Files: tools/hb-build-core.f. Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-driver.md (the Wasm build of '53 emit' stops in the link with no kernel object written; a native build runs and prints 5). Depends: none. Ownership: tools/hb-build-core.f. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3d.
