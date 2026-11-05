---
title: "S6 L5b1: a stripped product's header cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T22:23:04.581239+02:00"
---

Problem: design-s6-l5.md L5-3: a stripped product reads engine header cells (DP and its bounds, FD-PTR, the wids, IMAGE-BASE, UNCGH, SIGNAL-ABI:STUB-CELL, CODE-END, RBASE, APP-ENTRY:XT-CELL) that the build's space leaves zero or holds with build-time values; the old stripped entry (aot-link-arm.f EMIT-ENTRY :514-541, aot-owned-cells.f COLLECT :188-201) wrote them at start. Acceptance: link-data.f HEADER-CELLS writes each into the DATA image, code addresses listed once; EXIT-HOOK, OWNED-SOURCE and the owned cells zero; LINK-STRIPPED takes the program root; arm64-link-data HEADER-CASE. Files: src/habu/link-data.f, test/compiler/arm64-link-data.f, test/compiler/arm64-link-data-bad.f. Verify: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-l5b1.md: bin/hb --load test/run.f -- arm64-link-data arm64-place linker. Depends: habu-s6-l3b-a-58d3478e. Ownership: carl. Claim: unassigned.
