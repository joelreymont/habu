---
title: Size the x86 text window for the whole kernel
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T10:30:06.103914+03:00"
---

Problem: a booted x86 image's code must fit the text window, `4096 constant CODE-CAP-BYTES` (`src/arch/x86-64/icode.f:55`), enforced at `src/os/linux-x86-64/elf.f:228` (`elf: code exceeds text window`, rc 73). The scaffold's control image already ends at byte 1720. K6a lists 25 rows, K7 9 bodies and 5 refusals, K8 about 15, K9 about 40: about 90 rows, over the window at about 30 bytes each. Every leaf test emits the whole `KERNEL,`, so the last leaf to land would break all four kernel suites. Found by the scaffold's Fable review.
Acceptance: size the x86 text window for the whole kernel plus the engine it will hold at X6: one constant, with `ELF-MSIZE-CHECK` (`src/os/linux-x86-64/elf.f:78`) and `MSIZE` (`src/os/image-bytes.f:8`) deriving from it and their load-time checks kept. State the chosen size and its reason beside the constant. Pre-change failing check: a booted harness image whose kernel exceeds 4096 bytes refuses rc 73.
Files: `src/arch/x86-64/icode.f`, `src/os/linux-x86-64/elf.f`, `src/os/image-bytes.f` if it needs the derivation, `docs/x86-64.md`, a case in `test/x86-64-kernel-engine.f`.
Verify: ThinkPad: the four kernel suites and their native images; `test/x86-64-skel-image.f`, `test/x86-64-peer-image.f`.
Depends: habu-scaffold-the-x86-9af80979 (scaffold).
Route: direct if no file a macOS build loads changes; `src/os/image-bytes.f` is shared, so check.
Ownership: krait (Intel lane).
Claim: agent=krait (direct, default workspace).

Preflight corrections (Fable, 2026-09-30; these override the lines above where they differ):
- The window holds the hand-written kernel alone (`_start`, helpers, every K6a-K9 row, the K10-K13 handlers and trampolines). The captured engine is the region's own `PT_LOAD` at `VMBASE + REGION-OFF` (X4a); `src/habu/boot-x64.f:97-99` already maps it fixed there, so a window sized for the engine would be clobbered by that mapping. rel32 reaches the whole image; the bound is `REGION-OFF`, below which the text and its RW tail must end.
- One constant: `src/arch/x86-64/icode.f:55` becomes `$100000 constant CODE-CAP-BYTES`, the allowance ARM64 gives its engine code part (`ADR-HI`). `MPAGE` (`elf.f:69`), `ELF-MSIZE-CHECK` (`elf.f:77-80`), the rc-73 refusal (`elf.f:228`) and `MSIZE` (`image-bytes.f:8-10`) already derive from it and stay unchanged.
- Pre-change failing check: `test/x86-64-kernel-engine.f` gains `hb-x64-kernel-window` (`BOOT-OPEN,`, `PUSH-TEXT,` of a `PROT-PAGE-MAX` span, `2 EXPECT-DEPTH,`, the length popped, `BOOT-CLOSE,`); on master the suite dies rc 73 `elf: code exceeds text window`. No per-image negative twin: `hb-x64-kernel-engine-negative` proves the harness.
- Files: `src/arch/x86-64/icode.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`. Route: direct (x86-only files; `image-bytes.f` and `elf.f` untouched).
- Verify: ThinkPad, master's product `5f4d3321…`: `x86-64-seam`, `x86-64-emit`, `x86-64-peer-image`, `x86-64-skel-image`, `compiler/x64-emit`, `compiler/x64-chain` and the four kernel suites; every image natively with its documented status; `readelf -lW hb-x64-kernel-window` shows the RW LOAD past the text.
