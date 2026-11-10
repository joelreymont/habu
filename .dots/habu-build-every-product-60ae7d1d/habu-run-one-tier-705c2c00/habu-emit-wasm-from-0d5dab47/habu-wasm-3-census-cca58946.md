---
title: "Wasm 3: census of where a Wasm-target build stops"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:51:55.674725+02:00"
---

Step 3 of the Wasm campaign (DECISIONS.md 'Wasm through the one linker'). Problem: 'hb --load tools/hb-build.f -- --target wasm m.f -o m.wasm' for ': MAIN ( -- ) 53 emit ;' stops at src/habu/layout.f:59 with HB-TARGET-LINUX-X86-64? undefined: tools/hb-build-core.f LOAD? loads only COMMON start-up rows for a Wasm target, so no target file defines the HB-TARGET words; what stops the build after that is not known, and the fixes (a Wasm target's start-up rows, generator gaps, kernel routines) cannot be designed before it is. Acceptance: a list of each stop from there to LINKER:WRITE, each with its message, path:line, the layer at fault and the probe edit that passed it; no commit. Files: none (a hunt; probes stay uncommitted). Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-census.md. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3c.
