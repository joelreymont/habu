---
title: "Run a cross-build's top level on host words only"
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T08:45:05.542034+02:00"
---

Problem: a cross-build compiles every definition twice, for arm64 and for the target (src/habu/t0.f HOSTED and SHADOWED; docs/xcomp.md 'Dual emission'), so that top level can run program words on the host, and src/habu/linker.f CROSS? writes and links no object for a cross-build: a Wasm build recompiles every file each time. Source: Joel, 2026-11-12: 'machine code is produced during compilation and object files ARE the place to store machine code. the only thing linking does is put the object files together into one executable'; 'the target must never require anything to run on the host'; of top-level code, after the SwiftX report (docs/swiftx.md): 'do it like swiftx does unless you can prove you shouldn't'. Acceptance: a cross-build compiles each definition once, for the target; top level runs on the host and calls only hb's own words and words a source defines in a host section; a target word called at top level in a cross-build is refused by name; host-section words are in no object and no image; a cross-build writes and links objects, so a Wasm build of Maki's viewer with nothing changed compiles no file and writes the cold build's module. Design first (lead): the host-section words, what a native build does with them, a word needed on both machines, and the count of program words Maki, Zen and Tender call at top level. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-layout (design).
