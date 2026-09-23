---
title: Cross-build the x86_64 engine and gate it on the peer
status: active
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.544466+03:00"
---

Claim: agent=alder workspace=.jj-ws/alder-x64-peer, based on 45608866.
The compiler emission/retirement rows are integrated at 06c5c9e9. The first
runtime proof will wrap the existing HIR/pass-chain fixture in the production
x86-64 ELF writer and execute it on the ThinkPad, checking arithmetic and the
internal stack convention. This is a step toward this dot, not a full engine:
the primitive/runtime bodies and cross-build entry remain to be implemented.
The peer's Tailscale SSH check currently requires Joel's authorization;
image construction and host-side validation can proceed independently.

The host fixture is test/x86-64-peer-image.f, registered as
`x86-64-peer-image`. It reuses x64-chain.f's HIR subtraction fixture through
the actual backend rows, the production x86-64 ELF writer and syscall seam.
Five arithmetic cases include signed wraparound; the emitted program checks
both stack positions, reserved registers and success/error carry polarity.
Its negative control deliberately expects the wrong first answer. Expected
peer statuses are 0 and 21. The image, seam, seam-emitter and chain-plan rows
pass on private engine 3da80b23. Sources, images, logs, LLVM disassembly and
readelf output are saved at ~/.cache/habu/x86-peer/source-45608866/.
Peer execution is pending the SSH authorization; no runtime pass is claimed.

Problem: docs/x86-64.md (campaign habu-campaign-c6-targets-86bb56bb): order is cross-build first, self-host second: the arm64 engine compiles Habu for the x86_64 contract and writes an x86_64 ELF, and the Intel machine runs it as a device peer the way the serial peers do. Acceptance: tools/build-fixpoint.f (or tools/native-build.f) takes the x86_64 contract and writes an x86_64 engine image from the arm64 host, with the x86_64 primitive bodies from the primitive table; a device-peer gate under test/ ships the image to the Intel machine, runs test/run.f there and reports the result here; docs/bootstrap.md records the recovery rule for x86_64 (cross-build from a working arm64 engine; no Gforth mirror). Obligations recorded 2026-09-18 from the seam emitter lane (habu-add-the-x86-56726659 worker 2): (1) the x86_64 seam's emitters (SYS, OS-OPEN-*, the process words) append through a word named ASM-SINK ( -- ptr a ), the byte buffer the current code stream appends into, which the seam references unrequired the way src/os/linux/sys.f references mnem.f's MOVZ, and SVC,; this dot's x86-64 code layer (the icode equivalent under src/arch/x86-64/) defines it. (2) Payload order: X64ASM requires lib/byte-buffer.f -> lib/memory.f -> the mmap primitives habu1.f defines, while BF-APPEND-TARGET-SYS places the seam's sys.f BEFORE habu1.f, so an x86_64 engine payload cannot load X64ASM where the seam sits; the engine build's assembler sink cannot be a lib buffer (the arm64 icode.f appends into the mapped code region with no lib), so this dot gives X64ASM an append seam the engine payload can satisfy (the sink parameter is already the last operand of every encoder) or reorders the payload. (3) SNAP-RELOC:MOVABS-IMM-OFF and X64ASM:MOV-RI64-IMM-OFF state one fact (2) in two files, pinned equal by test/x86-64-seam.f until backends are loadable modules. (4) The residual two-arm HB-TARGET-LINUX? forms in src/habu/habu1.f (25) and bootstrap/cg/*.fs are this dot's, with the x86_64 bodies. (5) From the emitter lane (691fec39): ASM-SINK's effect is ( -- ptr u8 ), not ptr a (a type variable in a declared return is refused over raw storage, E-NONPARAMETRIC-EFFECT, and ptr u8 unifies with each encoder's ptr a sink); the seam's G-POP/G-PUSH take x86_64 register numbers (7 rdi, 6 rsi, 2 rdx, 0 rax); OS-OPEN-FLAGS and OS-MMAP-FLAGS clobber rax, rcx and r11, with the caller-ordering constraint documented in the seam; SYS, leaves CF set on error (mov eax,NR / syscall / mov rcx,-4096 / cmp rcx,rax), so the x86_64 SYS-PUSH is a setc; the stencils are byte strings ( -- ptr u8 n ) consumed by jit.f C-EMIT-STENCIL, whose arm64 emitter requires whole 4-byte words, so the x86_64 emitter's stencil consumer appends bytes. HBB-KEY-LINUX-X86-64-SOURCES folds the two process files while the linux and macos keys do not fold theirs (pre-existing asymmetry, one line each). Files: tools/build-fixpoint.f, tools/native-build.f, src/habu/habu1.f (x86_64 bodies) or the table's backend files, src/arch/x86-64/ (the code layer defining ASM-SINK), test/ (peer gate), docs/bootstrap.md. Verify: the cross-built engine boots on the Intel machine and test/run.f runs there. Depends: the OS seam, the lowering. Ownership: Joel (x86_64 lane).
