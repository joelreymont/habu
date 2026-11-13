---
title: Build Wasm modules through the one linker
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T14:52:51.664135+02:00"
---

Campaign. Design settled 2026-11-09 (the Plan agent's full text, read-only and never run, in .jj-ws.noindex/jerry-wasm/.out/wasm-design.md). Goals step 2 ("Wasm and x86-64 after arm64"): after arm64, before x86-64 (habu-re-enable-the-959502b5 waits on this dot).

Problem: no command builds a Wasm module. tools/wasm-build.f builds by capturing the running hb's memory (machinery being replaced) and dies at load on the landing tree; tier 1 makes every Wasm body; T0 has an empty Wasm slot in its emitter seam (src/habu/t0-emit.f). Acceptance: `tools/hb-build.f -- --target wasm source.f -o out.wasm` links a module through LINKER:WITH/WRITE that src/lib/browser/turn.js loads (same exports, WPROF layout); Maki's viewer (~/Work/maki/viewer.f) builds and runs through it; the capture tests' subject is gone and the Wasm device check and browser tests run on the new command.

Steps, one dot each below, in order: 1 the linker takes the build's target; 2 T0's Wasm generator and the kernel routines it calls; 3 the linker's Wasm write step, the flag, the removals and the moved tests; 4 Maki's viewer (its repository's owner changes viewer.f and README.md); 5 the kernel census and cold-build time; 6 warm Wasm builds (an object carries the host copy too).

Not depending on habu-cross-compile-through-66516afb any more: its C3 (T0 files fixed, cast and does> bodies in a shadow) is step 2's for Wasm, and its C4 is x86-64's.

Acceptance cases (defects the replaced Wasm build shows on the arm64 landing tree, 2026-11-09):
- tools/wasm-build.f (Maki's viewer build, maki/viewer.f) dies at load: it includes src/habu/prefix-rewind.f, which the old builders' deletion removed. A Wasm build runs through the linker, with no rewind of the running hb.
- test/wasm/capture.f and link-capture.f die "tfam: the captured type registry does not fit the artifact's buffer": a capture copies hb's whole type registry into the fixed AOT-REG-CAP (src/habu/aot-decl.f), which the hb carrying the linker and the typed interpreter outgrew. A Wasm build does not fail by what hb itself carries. Both tests are off (`false [if]` in test/gate-stdlib-cases.f) until this move.

Files: per step. Verify: per step. Depends: none. Ownership: the steps'. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wasm.
