---
title: "S6 L5b2a: boot a stripped arm64 program at code offset 0"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T22:23:14.557226+02:00"
closed-at: "2026-11-06T01:13:05.615434+02:00"
close-reason: "landed as \"Boot a stripped arm64 program at offset 0\": linker 1209 ms, arm64-place 2402 ms, arm64-link-data 3515 ms at load 7.5 (installed hb, master 572c3b43's engine; master alone 1489/2918/4098 ms at load 11); reviewed (code ACCEPT, comment fold ACCEPT); folds: START, takes the linked address from LINKER's TEXT-VA so boot.f holds no OS layout constant, DATA-IMAGE-LEN is the space's length (BYTES, pads to a word), three comment fixes, src/arch/AGENTS.md names boot.f. Untested: the boot itself runs first in L5b2b's arm64-link-run."
---

Problem: design-s6-l5.md L5-4 with Revision 2: a product linked from a build's space has no start-up, so nothing can run it. Acceptance: src/arch/arm64/boot.f (package A64BOOT) START, ( label label label label -- ), straight-line at code offset 0 (refused elsewhere, die 76): entry args, signal stack, boot data stack x19, DATA map, DATA copy from the image, the load offset added to each listed cell, run-time cells, frame stacks, crash install, `APP-ENTRY:XT-CELL LDR, BLR`, exit hook, exit 0; ARM64:KERNEL splits into ARM64:OPEN and ARM64:KERNEL; LINK-STRIPPED runs ARM64:OPEN, four tail labels, START,, ARM64:KERNEL, then its link; DATA-IMAGE binds the labels and DATA-IMAGE-AT/-LEN/-LIST-AT/-LIST-N become words reading them; the tests' ARM64:KERNEL LINK-STRIPPED pairs become LINK-STRIPPED. Files: src/arch/arm64/boot.f, src/arch/arm64/link-kernel.f, src/habu/link-data.f, test/compiler/arm64-link-data.f, test/compiler/arm64-link-data-bad.f, test/compiler/arm64-place.f. Verify: brief .jj-ws.noindex/carl-dots/.out/carl-file/brief-s6-l5b2a.md: bin/hb --load test/run.f -- arm64-link-data arm64-place linker. Depends: habu-s6-l5b1-a-f44dacbe. Ownership: carl. Claim: agent=carl, workspace carl-s6-l5b2a.
