---
title: Keep the x86 code pointer on code slots
status: open
priority: 2
issue-type: task
created-at: "2026-09-30T14:35:05.373466+03:00"
blocks:
  - habu-model-the-code-c40c75d1
---

Problem: `X64EMIT:PLACE-AT` refuses a slot that is not a multiple of `X64IR:SP-ALIGN` (`emit-x64.f:1258-1264`), and the driver places every definition at `NPUB:NEXT-SLOT` = `cp@` (`compiler.f:444`, `publish.f:165-166`). x86 `code-publish` moves CP by the exact length (`kernel-x64.f:1917`) and `does-record` pads its name only to 4 (`:1624`, `:2012-2013`), so the definition after a 31-byte routine is refused `E-X64EMIT-PLACE`. `cp!` refuses only `& 3` (`:2268-2276`), although its comment (`:2265-2266`) and `docs/x86-64.md:1228-1231` say CP is always a slot.
Acceptance: `kernel-x64.f` declares `16 constant CODE-SLOT` (its comment ties it to `X64IR:SP-ALIGN`, the unit `PLACE-AT` accepts; the kernel loads no dialect). `code-publish` opens the window over [CP, the first slot at or past CP+len), copies, fills the gap with `int3` ($CC), drops the rows of [dst, dst+len) and sets CP to that slot. `does-record` zero-pads the name to `CODE-SLOT`; `NAME-ALIGN` goes. `cp!` exits 83 for a CP that is not a slot. Whichever of this leaf and K9b lands second makes the native provenance range [dst, new CP) (`snap-lib.f:452-455` requires native evidence for every byte below CP, padding included). `publish.f` does not change: its room checks count unpadded bytes and the 16 KiB `CODE-RESERVE` (`publish.f:16`) absorbs the padding; `docs/x86-64.md` says so. `prims.f`'s `code-publish`/`does-record` comments and `docs/x86-64.md` (publication table, CP bound) state the rule for every CP writer; boot's `DBASE+DICT-SIZE` is already page-aligned.
Files: `src/habu/kernel-x64.f`, `src/habu/prims.f` (comments only), `test/x86-64-kernel-atomics.f`, `test/x86-64-kernel-engine.f`, `docs/x86-64.md`.
Verify: the host product engine `--load`s both tests; ThinkPad natively: after a 17-byte publish CP = dst+32 and bytes 17-31 are $CC; after `does-record` CP is a slot; an armed image `cp!`s a 4-aligned non-slot and exits 83; each suite's `-negative` exits 21.
Route: direct (x86-only files).
Depends: habu-emit-x86-code-973a0074 (K8b), habu-emit-x86-engine-86b5f8e7 (K9a), habu-model-the-code-c40c75d1 (K9b: serialise on `PUBLISH,`).
Route: direct (x86-only files).
Ownership: krait (Intel lane).
Claim: unassigned.
