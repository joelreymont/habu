---
title: Cross-build the x86-64 engine from spark
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:32:42.544466+03:00"
blocks:
  - habu-emit-a-second-8a260f5a
  - habu-read-recorded-sites-d4949953
  - habu-carry-the-shadow-dcb84138
  - habu-select-the-build-610b4492
  - habu-write-and-link-f6e6017f
  - habu-link-records-and-647852d3
  - habu-link-the-shadow-74e41be7
  - habu-resolve-x86-entry-cb671d4d
  - habu-run-a-captured-15728fcc
  - habu-run-bin-hb-6378f297
  - habu-fail-closed-on-f84f1197
---

Lane X: cross-build the x86-64 engine from spark. A cross-build must execute the prefix on the host while emitting for the target (`LOAD-TARGET`, `tools/native-build-core.f:186-225`, runs every declarer and immediate on the host), so the cross-build is dual emission in the host compiler (X1), recorded-site capture and a shadow section in the capture (X2a, X2b), window target selection (X3) and a linked image with fixed segments written at build time (X4a-d); its acceptance is M3 (X5: a captured Habu program runs on x86) and M4 (X6: x86 `bin/hb --load` works). X7 makes the `habu1.f` two-arm forms fail closed. Spark builds, the ThinkPad runs. No peer-gate script under `test/`: the lead runs the gate natively on the ThinkPad, and X6 puts the two-command recipe (scp, run) and the x86 recovery rule (cross-build from a working arm64 engine; no Gforth mirror) in `docs/bootstrap.md`.
Obligations recorded 2026-09-18 from the seam emitter lane, with their owners: (1) the x86-64 seam's emitters (SYS, OS-OPEN-*, the process words) append through `ASM-SINK`, which the x86-64 code layer defines -> habu-add-the-x86-aad02c7e (K1). (2) Payload order (X64ASM requires `lib/byte-buffer.f`, which the cold payload places after the seam's `sys.f`) -> discharged by construction: the x86 seam and kernel builder run on a host that has `lib/byte-buffer.f` (`tools/native-emit.f:13-16`, `test/x86-64-peer-image.f:14-17`); no bounded writer. (3) `SNAP-RELOC:MOVABS-IMM-OFF` and `X64ASM:MOV-RI64-IMM-OFF` state one fact in two files; the equality pin in `test/x86-64-seam.f` stays until the kind is defined once (habu-record-symbolic-x86-10037f07 (C6) records sites by `SNAP-RELOC:MOVABS`). (4) The 25 residual two-arm `HB-TARGET-LINUX?` forms in `src/habu/habu1.f` -> habu-fail-closed-on-f84f1197 (X7), fail closed; the forms in `bootstrap/cg/*.fs` stay ARM64 because the Gforth chain emits ARM64 and refuses x86 by name. (5) -> habu-add-the-x86-a8bf9973 (K2): `ASM-SINK`'s effect is `( -- ptr u8 )`; the seam's `G-POP`/`G-PUSH` take x86-64 register numbers (7 rdi, 6 rsi, 2 rdx, 0 rax); `OS-OPEN-FLAGS` and `OS-MMAP-FLAGS` clobber rax, rcx and r11 (caller-ordering constraint documented in the seam); `SYS,` leaves CF set on error, so the x86-64 `SYS-PUSH` is a `setc` (used by K6); the x86 stencil consumer appends bytes. Recorded, unowned: `HBB-KEY-LINUX-X86-64-SOURCES` folds the two process files while the linux and macos keys do not fold theirs (pre-existing asymmetry, one line each).
Executed peer result: at master afd626da, `test/x86-64-peer-image.f` (registered as `x86-64-peer-image`; x64-chain.f's HIR subtraction fixture through the real backend rows, the production x86-64 ELF writer and syscall seam; five arithmetic cases including signed wraparound; checks both stack positions, reserved registers and carry polarity; its negative control expects a wrong first answer) built on spark produced `hb-x64-peer` (exit 0) and `hb-x64-peer-negative` (exit 21), run natively on the ThinkPad on 2026-09-29.
Leaves: habu-emit-a-second-8a260f5a (X1), habu-read-recorded-sites-d4949953 (X2a), habu-carry-the-shadow-dcb84138 (X2b), habu-select-the-build-610b4492 (X3), habu-write-and-link-f6e6017f (X4a), habu-link-records-and-647852d3 (X4b), habu-link-the-shadow-74e41be7 (X4c), habu-resolve-x86-entry-cb671d4d (X4d), habu-run-a-captured-15728fcc (X5), habu-run-bin-hb-6378f297 (X6), habu-fail-closed-on-f84f1197 (X7).
Campaign lane only; do not dispatch. It lists its leaves under blocks: so it stays off dot ready until they close.
Ownership: krait (Intel lane).
Claim: unassigned.
