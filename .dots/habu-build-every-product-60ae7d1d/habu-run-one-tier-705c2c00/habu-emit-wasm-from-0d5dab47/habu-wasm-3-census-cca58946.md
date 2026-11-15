---
title: "Wasm 3: census of where a Wasm-target build stops"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T16:51:55.674725+02:00"
closed-at: "2026-11-10T16:58:14.870172+02:00"
close-reason: "reported 2026-11-10: with macOS's start-up rows loaded as a probe, every start-up file, the source and app-end.f compile for Wasm with no generator refusal and the build reaches LINKER:WRITE, which dies in link-band.f LAYOUT ('MAIN is not whole 4-byte words'); KERNEL-BUILD runs for a Wasm target and writes an arm64 kernel object. Two stops, each with its own dot: the Wasm target's start-up rows, and the write step."
---

Step 3 of the Wasm campaign. Problem: 'hb --load tools/hb-build.f -- --target wasm m.f -o m.wasm' for ': MAIN ( -- ) 53 emit ;' stops at src/habu/layout.f:59 with HB-TARGET-LINUX-X86-64? undefined: tools/hb-build-core.f LOAD? loads only COMMON start-up rows for a Wasm target, so no target file defines the HB-TARGET words; what stops the build after that is not known, and the fixes (a Wasm target's start-up rows, generator gaps, kernel routines) cannot be designed before it is. Acceptance: a list of each stop from there to LINKER:WRITE, each with its message, path:line, the layer at fault and the probe edit that passed it; no commit. Files: none (a hunt; probes stay uncommitted). Verify: brief .jj-ws.noindex/jerry-wasm/.out/briefs/w3-census.md. Depends: none. Ownership: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w3c.
