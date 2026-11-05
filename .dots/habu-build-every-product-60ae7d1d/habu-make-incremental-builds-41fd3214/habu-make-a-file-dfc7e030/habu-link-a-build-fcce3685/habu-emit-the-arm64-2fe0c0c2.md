---
title: "Emit the arm64 kernel at a product's code head (S6 L1)"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-05T16:57:13.656870+02:00"
closed-at: "2026-11-05T17:30:05.759796+02:00"
close-reason: "Landed 7982e3ed: LINKER:ARM64 KERNEL, PRIM-OFFSET, KERNEL-ENTRY in src/arch/arm64/link-kernel.f; arm64-kernel-head and compiler-shadow PASS at the tip on the installed 3170dbca engine (270 primitives map, 269 first words equal, 1 pc-relative)."
---

Problem: design r2 D1/D8 (.jj-ws.noindex/carl-dots/.out/carl-file/design-s6-r2.md) start a product's code with the arm64 kernel and map each kernel address a T0 routine names (a host absolute address, shadow.f:36-41) to the product kernel's offset. Census: s6-census-l1a.md, s6-census-l1b.md beside it. Acceptance: src/arch/arm64/link-kernel.f, package LINKER private part ARM64: KERNEL (ENGINE-EMIT:FORTH, empty source, at icode CODE offset 0), PRIM-OFFSET, KERNEL-ENTRY (host kernel entry -> product offset or -1); a test maps every host kernel primitive entry to a product offset whose first instruction matches, an interior address to -1. Files: src/arch/arm64/link-kernel.f, test/compiler/arm64-kernel-head.f (new), test/gate-arm-cases.f. Verify: bin/hb --load test/run.f -- arm64-kernel-head. Depends: none. Ownership: those files. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-s6-l1.
