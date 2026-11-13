---
title: Build Wasm modules through the one linker
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.664135+02:00"
---

Campaign. Design settled 2026-11-09 (the Plan agent's full text, read-only and never run, in .jj-ws.noindex/jerry-wasm/.out/wasm-design.md). Joel, 2026-11-09: arm-to-arm first (pg included), then Wasm, then x86-64; so this comes after arm64, before x86-64 (habu-re-enable-the-959502b5 waits on this dot).

Problem: `tools/hb-build.f -- --target wasm source.f -o out.wasm` builds a module through T0's Wasm generator (src/habu/t0-wasm.f) and the linker; test/wasm/hb-build.f, the browser tests (test/browser/*.f) and Maki's viewer build (~/Work/maki/README.md:454, viewer-main.f) use it; tools/wasm-build.f and the capture tests are deleted. What remains is this dot's open children: the two waiting cases (habu-waiting-case-a-416aaa28, habu-waiting-case-a-9347ad62), what WKMAP and kernel-words.f still carry for tier 1's capture (habu-wkmap-and-kernel-7d73d82a), and WLINK's function typing only its tests use (habu-wlink-types-functions-97a4099d). Acceptance: those children close.

Files: per child. Verify: per child. Depends: none. Ownership: the children's. Claim: unassigned.
