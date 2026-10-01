---
title: Pin a coalesced shift count without a holder
status: closed
priority: 2
issue-type: task
created-at: "2026-09-30T10:32:49.303353+03:00"
closed-at: "2026-10-01T13:06:11.235968+03:00"
close-reason: the sum-shift shape is accepted under LEAF and DLEAF (was -8460) with the count loaded into rcx (llvm-mc fixture); ARM64 gen1 b079f883 == gen2; x86 suites and 73 images pass
---

Problem: a valid program whose shift count is loaded or computed while a short-lived value holds rcx is refused. The selector copies the count immediately before `x64.shl`/`x64.shr`, but `MB-COALESCE1` (`src/compiler/native/regalloc.f:1308-1321`) merges that copy into a source that dies at it, so the pinned class opens at the source's definition; if another class holds rcx there, `MB-PIN` (`:1736-1742`) throws `E-A64RA-FIXED`. `( a b n -- x ) -rot + swap lshift` under the data-stack convention: arguments load a, b, n in order; b dies at the add and takes rcx; `{n, copy}` is pinned to rcx at n's load while b still holds it. Refused through `NBACK:DECLARE/SELECT/PRUNE/FIXPOINT` under `X64ABI:LEAF-FRAMED`; the register convention accepts it. Found by Cfix's Fable review (repros `repro-coalesce.f`, `repro-rows.f` in the reviewer's scratchpad). Pre-existing: master refuses it too (`E-A64RAV-FIXED`). Never a miscompile; the validator backstops.
Acceptance: the shape is accepted under LEAF and DLEAF, the count in rcx, with the fix at the responsible layer: either the pin's register is forbidden to every class overlapping the pinned class's whole hull (from its opening, not only across the form), or `MB-COALESCE1` does not merge a fixed copy into a source whose range would hoist the pin across a holder. Choose by what keeps ARM64 placement unchanged and the allocator simplest; the chain proves ARM64 bytes. `test/compiler/x64-regalloc.f` gains the shape as an accepted row (pre-change refused), `test/compiler/x64-emit-fixture.f` pins its bytes, `test/x86-64-peer-routines.f` runs it natively. The qualifying comments at `select-x64.f` point THREE and `docs/x86-64.md` (the shift paragraph) are rewritten to the new rule.
Files: `src/compiler/native/regalloc.f`, `src/compiler/native/select-x64.f`, `test/compiler/x64-regalloc.f`, `test/compiler/x64-emit-fixture.f`, `test/x86-64-peer-routines.f`, `docs/x86-64.md`.
Verify: ThinkPad (qemu): the x64 suites and `native-regalloc.f`; the native images. spark: rebuild, five-generation chain, gate.
Depends: habu-pin-schema-fixed-1983d191 (Cfix).
Route: lands on master after the Linux gate; Alder pools the Mac gate.
Ownership: krait (Intel lane).
Claim: unassigned.
