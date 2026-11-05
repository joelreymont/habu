---
title: "S6 L5b1: a stripped product's header cells"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T22:23:04.581239+02:00"
closed-at: "2026-11-06T00:46:25.722014+02:00"
close-reason: "landed as \"Write a stripped product's header cells\": linker 1140 ms, arm64-place 2288 ms, arm64-link-data 3405 ms (load 15); reviewed, folds: header values checked against what hb ran the build under, formula mirrors dropped, every listed cell checked to hold an address in the product's code, an unbound label refused, the refusal children's expected text built from UNCGH-CELL. Untested: STUB-CELL's value in a running product (no signal case) and CODE-END-CELL, which a stripped product never reads"
---

Problem: design-s6-l5.md L5-3, as Revision 2 revises it: a stripped product reads engine header cells (DP and its bounds, the heap start, FD-PTR, the wids, IMAGE-BASE, UNCGH, SIGNAL-ABI:STUB-CELL, CODE-END, RBASE) that the build's space leaves zero; the old stripped entry (aot-link-arm.f EMIT-ENTRY :514-541) wrote them at start. The entry is no longer the link's: the build stores it with `xt!` into APP-ENTRY:XT-CELL (app-image-core.f:54), ROUTE-HEADER sends that store to the space, and it is an ordinary xt row. Acceptance: link-data.f HEADER-CELLS writes each cell's link-time value into the DATA image and lists RBASE, UNCGH, SIGNAL-ABI:STUB-CELL and CODE-END once each; CODE-END-CELL = TEXT-VA + DATA-IMAGE-AT (the end of the product's code); the link refuses (74) a header cell it writes that the build stored nonzero; nothing is zeroed (EXIT-HOOK, OWNED-SOURCE and the owned cells hold what the build stored); LINK-STRIPPED takes no root; arm64-link-data HEADER-CASE checks the values, the list and the APP-ENTRY row; arm64-link-data-bad gains a `header` child case. Files: src/habu/link-data.f, test/compiler/arm64-link-data.f, test/compiler/arm64-link-data-bad.f. Verify: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-l5b1.md: bin/hb --load test/run.f -- arm64-link-data arm64-place linker. Depends: habu-s6-l3b-a-58d3478e (landed 2bd5e1aa). Ownership: carl. Claim: agent=carl, workspace carl-s6-l5b1.
