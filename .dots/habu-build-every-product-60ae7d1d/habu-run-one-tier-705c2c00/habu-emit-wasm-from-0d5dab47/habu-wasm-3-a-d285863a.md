---
title: "Wasm 3: a Wasm target loads no start-up target rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:58:14.903716+02:00"
---

Step 3 of the Wasm campaign, census stop 1. Problem: tools/hb-build-core.f LOAD? loads a start-up row only when its kind is COMMON or the build's target, and src/habu/boot-files.f has target rows only for LINUX, MACOS and X64 (each OS's target.f with the HB-TARGET words, and its layout files with DATA-SIZE, DATA-VA and the like), so a Wasm build stops at src/habu/layout.f:59 with HB-TARGET-LINUX-X86-64? undefined. Loading macOS's rows as a probe passes every start-up file, but the module then says HB-TARGET-MACOS? is true and holds macOS layout constants. One source text sets both the module's copy and the host copy later files run during the build (carl's cross-OS probe died when host-run code got the target's dlsym constant). Acceptance: a Wasm build loads rows that tell the truth about the target, and code the build runs on the host still works; the design first (which start-up uses read a target word, and which of them run on the host during a build). Files: src/habu/boot-files.f, tools/hb-build-core.f, new target files. Verify: the build of ': MAIN ( -- ) 53 emit ;' passes its start-up files without the probe. Depends: none. Ownership: none. Claim: unassigned.
