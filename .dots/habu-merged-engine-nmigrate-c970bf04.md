---
title: "Merged-engine NMIGRATE:DEFINE crashes"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T01:09:50.141217+02:00\""
---

Claim: agent=bake-chain-10 workspace=.jj-ws/habu-bake-chain

Found by bake-chain-9 (2026-08-16) past the wordlist fix: with the two sealed-WID gate refusals bypassed experimentally, the merged engine boots, runs ordinary programs (1 2 + . works), the chain's boot-run installer resolves and runs - but NMIGRATE:DEFINE SIGSEGVs (exit 134, pc 0x10052191c) where the same call on the source-loaded chain returns 0; without the chain's installer it throws 7134 E-PATH-RANGE. Needs a debugger session (docs/debugging.md; lldb: process launch --stop-at-entry THEN br set - the breakpoints-before-run lesson), not inference. Suspects to eliminate in order: a DATA literal the merge rebased into the wrong band; a code literal (csite) value; the DKEEP-HOOK defer state; a chain word whose body holds state the capture snapshotted mid-initialization. Blocks e98b03d4 items (3)-(6) and the boot milestone with 9d7d8e72's gate ruling. Build the tool, don't guess: extend the imgdump/record readers if the state is not visible.

PROGRESS 2026-08-16 (bake-chain-10). TWO INSTRUMENTS BUILT, which the
dot ordered: the breakpoint handler prints `habu-bp-lr:` + the
INTERRUPTED thread's x30 (habu2.f package BP-CALLER; the handler's
own x30 is gone by then, so it is read from the same mcontext the pc
comes from), and tools/code-owner.f + tools/code-owner-main.f turn a
JIT-region address into the record(s) that own it, reporting EVERY
owner and the offset into each, keyed on a REGION OFFSET so one run's
number resolves in another. Both are in docs/debugging.md. lldb is not
an option here and that is now recorded: neither software nor hardware
breakpoints fire on these images (a breakpoint on c!'s __text entry did
not fire while c! demonstrably executed; -H addresses bind to dyld).
LOCALISED, with the engine's own tools. The fault is `strb w9,[x10]`
with x10=8 inside the `c!` primitive - the FIRST c! of the process -
called from PATHZ+380, the `over d + c!` of PATHZ's copy loop, first
iteration, with the destination local d = 8. CONTROL ENTERS PATHZ AT
OFFSET 180 EXACTLY: a raw BRK planted with patch32 at +172 and at +176
never fires, at +180 it does, and `' PATHZ BP*` on the entry never
fires either. So a branch lands mid-body, and PATHZ's prologue never
runs - which is why d/u/a read as another frame's locals.
lr at that point is STAGE+88 (NMIGRATE:STAGE), but it is STALE, not the
branch: all twelve of STAGE's BL targets are byte-identical between the
merged engine and the source-loaded chain.
ELIMINATED, each with evidence: the PZB DATA literal (PATH0 works in
the merged engine and passes the same pointer as the source chain); the
DKEEP-HOOK defer vector (decoded its movz/movk cell chain in both
engines - both dispatch to DKEEP-HOOK-DEFAULT+12, the inline quotation
body); record-entry overlap (a full scan reports 0 records whose entry
lands inside another's span).
NEXT, and it is one question: WHAT BRANCHES TO PATHZ+180. It is not a
BL from STAGE and not a record entry, so the candidates left are an
indirect `blr` through a code literal (16 csites, the quotation-entry
class) and a RET taken with a corrupted saved x30 - the second now
looks likelier, because a RET explains all three observations at once
(no prologue, no entry breakpoint, and an lr left over from the last
real BL). Bisect with the same patch32 BRK across the words that RET
into that address.

ROOT CAUSE + RULINGS 2026-08-16 (bake-chain-11; proof by repair -
patching the three does-links to PTR-VARIABLE+68 removes the
SIGSEGV): DOESPATCH:EMIT plants a plain PC-relative B one word
past the record's span, targeting an INTERIOR address; ACAP-CALL?
scans only BL, so the branch is copied verbatim and relocation
strands it at PATHZ+36. Three words are the whole class (M-SRC,
M-DATA, TX-DIA - the PTR-VARIABLEs called rather than inlined).
RULING 1: option (2) - THE DOES-CLAUSE GETS ITS OWN DICTIONARY
RECORD, so the link's target is an entry and the existing
name-keyed site path carries it with no new field. Option (1)
(name+delta) is REFUSED: a captured branch must never depend on
a callee's interior byte layout. Two requirements ride the fix:
(a) the seed's patcher must PRESERVE THE BRANCH KIND - a B
patched as BL would corrupt x30; the site row or the patch site
itself must keep the original opcode; (b) the capture-side audit
lands with it - a B in the blob whose target is outside the
window (or resolves to no record) is refused BY NAME, and the
"word at start+len is ret" completeness audit (102 vs 98, the
class detector) is worth keeping as a capture check if it holds
over the real populations - probe it. Compiler change: record
counts move, the fixpoint churns - attribute it, that is what
the control-build discipline is for.
RULING 2: TAKE b8fec035 in the same lane - both defects block
the milestone transcript, you hold the harness and the evidence,
and the fix is bounded (align the merged window base up to 8,
pad the content, extend the merge suite's sums to prove the pad,
alignment assert over the merged engine's declared cells). But
SEPARATE commits, each fully gated - the does-record change is
seed-affecting compiler work; the alignment fix is merge-reader
work; neither review should have to untangle the other.
