---
title: Build x86-64 files through the linker
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T14:09:36.119795+02:00"
---

Problem: x86-64 files do not build through the one linker (src/habu/linker.f; arm64 and Wasm do). Joel 2026-11-03: "we don't have the loader anymore. you need to implement the linker and then switch x86 (then wasm?) over to it". x86-64 gets no observer: its files link through the linker with that machine's code patcher.

Facts the move needs:
- Nothing publishes REGION-ALIAS:DELTA-CELL on x86-64 (arm64 does at src/arch/arm64/boot.f:185), so linker.f RESTORE's store of the flags through it would fault there; the x86-64 boot must publish it, or the move must not need it.
- The kernel cut's walk in src/arch/arm64/link-kernel.f is machine-neutral except WORD@ and TERMINAL? (:488, :488), which read whole words at word positions to ask whether control passes a node's end. A byte-positioned code buffer (x86-64) must record where control does not pass on as it emits, as it records references.
- A negative `allot` that releases a facts text must drop it from the string table, or INTERN:TEXT answers the released place again and data laid over its length cell can hang the table's next growth. arm64 does it: habu1.f BALLOT (:3113) calls the checker word at CHECKER-OWNER-ABI:GRID-REVOKE-OFF, src/core/checker.f:15387 DP-CUT, which runs INTERN:KEEP-BELOW. x86-64 does not: kernel-x64.f ALLOT-BODY (:3122) raises DPHW-CELL and calls no checked word, so neither record grids nor texts are cut there. The fix is one rule on both machines from the shared layer, not an x86 patch.
- src/habu/snap-x64.f and snap-decode-x64.f are the old snapshot writer: no file requires them, and snap-x64.f:34 calls BEFORE-CLOSE, which nothing defines since snap-lib.f was deleted. The move deletes them once the linker writes x86-64 programs.

Acceptance: `tools/hb-build.f -- --target` with an x86-64 target builds a program through LINKER:WITH with that machine's code patcher, with the objects a Wasm build writes and links (src/habu/linker.f COPIES: a file's object carries the target's and the host's copy of a definition); the suites off in the two `\ Off: x86-64.` blocks of test/gate-stdlib-cases.f (:640, :658) are on again and pass on pulsar (habu-re-enable-the-959502b5); after `n allot` with n negative, INTERN:FIND of a text laid above the new DP answers 0 on x86-64, shown by a checked probe as test/address-cell-rollback.f's failed-load text case shows it on arm64.

Depends: the Wasm campaign (habu-emit-wasm-from-0d5dab47). Claim: unassigned, owner jerry.
