---
title: "Place a build's routines after the arm64 kernel (S6 L3a)"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T20:11:19.473962+02:00"
closed-at: "2026-11-05T20:39:41.223853+02:00"
close-reason: "Landed: src/habu/link-band.f places every native shadow emission after the arm64 kernel at TEXT-VA + offset; ARM64:PLACE takes a data resolver; arm64-kernel-head folded into arm64-place (PASS ~1.3-2.7 s wall, ~2.3 s CPU, against 510 + 4501 ms). The test installs INTERPRETER:INSTALL-CREATE: the machine-code create's shadow path throws E-COMP-STATE under a native shadow (acceptance case added to habu-move-the-interpreter-d35780fa)."
---

Problem: design r2 D1/D2/D4 (.jj-ws.noindex/carl-dots/.out/carl-file/design-s6-r2.md, L3) lay every product routine (native shadow emission) back to back after the kernel in icode's CODE and resolve each call and code address to a link address: a product routine's TEXT-VA + offset, else a kernel primitive's TEXT-VA + KERNEL-ENTRY, else refused by name. Acceptance: src/habu/link-band.f (new, package LINKER, private): BAND places every emission of the open shadow from A64ICODE:ASM-LEN, a host-entry table over every shadow record (HM), RESOLVE and a refusal naming the word; ARM64:PLACE takes a data resolver in place of a delta; test/compiler/arm64-place.f adds a band case (product call, kernel call, code address, does> companion, data addresses, a foreign word unresolved, a second band equal byte for byte). Files: src/arch/arm64/link.f, src/habu/link-band.f (new), test/compiler/arm64-place.f, test/compiler/arm64-place-fixture.f. Verify: rebuilt-free: bin/hb --load test/run.f -- arm64-place arm64-kernel-head. Depends: none (L1, L2 landed). The DATA image, header cells and DATA list are L3b, after S5b1 (jerry's LINKER:WITH open/close). Ownership: those files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l3a.
