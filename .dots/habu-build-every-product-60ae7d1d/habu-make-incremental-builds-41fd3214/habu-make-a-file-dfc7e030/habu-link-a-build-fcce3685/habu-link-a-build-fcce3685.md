---
title: "Link a build's space for arm64 (step 5 S6)"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T15:11:55.348222+02:00"
---

Problem: step 5's arm64-to-arm64 cross-build needs the space LINKER:WITH opens (S5, jerry) written out as an arm64 Mach-O product; the only linker that writes a product as data is src/habu/link-x64.f, x86-64 and ELF. Plan: /tmp/jerry-big/step5-plan.md S6 (:62-70); design r2 3.6 and section 5 (/tmp/jerry-big/design-xbuild-r2.md:314-353, :454-489), approved by Joel 2026-11-04 (DECISIONS 'how Habu builds Habu'). Acceptance: one linker over link-x64.f's ISA-neutral parts (LAYOUT, INDEX, PRIM!, CELL-REGISTRY, DATA-BAND, LINK-SITES' walk) with an arm64 code patcher (bl/b imm26, adr/movz-movk chains), the Mach-O writer and signing, code where the OS loads it with the linker's list of stored addresses into the image, each adjusted by the load offset at start (DECISIONS 2026-11-05 "where a built program's code runs"), the stripped closure from MAIN, the arm64 kernel's primitive bodies placed from habu1.f's generator, and src/arch/arm64/boot.f (A64BOOT); S7's stripped app cross-built in-process runs. Several lanes, designed from the census child after S1-S5 land; Design r2 approved 2026-11-05 (.jj-ws.noindex/carl-dots/.out/carl-file/design-s6-r2.md); machine code goes under src/arch/arm64/ (DECISIONS "where machine-specific code lives"). No x86 builds, pulsar runs or x86 lanes (Joel 2026-11-05). Files: src/habu/link-x64.f, src/habu/aot-link-arm.f, src/habu/address-carrier.f, src/os/macos/macho.f, src/os/macos/sign2.f, src/habu/aot-closure.f, src/arch/arm64/boot.f (new, package A64BOOT). Verify: S7's test. Depends: S5 (LINKER:WITH open and close, jerry). Ownership: carl (handed over by jerry 2026-11-05). Claim: agent=carl.

Acceptance from the arm64 assembler (DECISIONS 2026-11-05): aot-link-arm.f's own reach checks and bl/b packing (:644-666, :774, :973-980) go with it; the one linker calls A64ASM's encoders and keeps no reach check of its own.

Acceptance from the capture's prelude band: a product file that requires a file the build itself has loaded (debug.f requires asm.f, which aot-decl.f's icode.f require puts in the build) gets that file in the product's own space. The in-process capture cannot: a window debugger calls A64ASM:ENC-B in the prelude band and ACAP-SITE-BAND dies with exit 74, so test/aot-wid-build.f leaves the debugger out of its window.

Status 2026-11-06 (carl): S6 L1-L5 and the kernel cut (a67cf6b8) are on master; L5's docs are done (compilation.md "Building an image" Now line, xcomp.md "Fixed segments"), and architecture.md "Images" waits for step 5a (dot habu-build-stripped-apps-805dee2b). L5's remaining proof is S7 on spark, blocked: `ssh spark` asks for a Tailscale browser sign-in on Joel's account. L6 is step 6.
