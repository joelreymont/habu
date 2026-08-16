---
title: "Merged-engine NMIGRATE:DEFINE crashes"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T01:09:50.141217+02:00\""
---

Claim: agent=bake-chain-11 workspace=.jj-ws/habu-bake-chain

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

BRANCH CENSUS 2026-08-16 (bake-chain-11, ruling 1(b)'s probe). A
CONTIGUOUS scan of the merged engine's whole code region - not per
record, which is how every earlier scan missed the link - decodes
40819 B/BL words and classifies every one of them against the record
table:
  24874  BL -> a record entry            (ordinary calls)
   6990  BL -> no record                 (__text primitives)
   8912  B  -> inside its own record     (ordinary control flow)
     43  B  -> the INTERIOR of another record
and the 43 are the whole does>-link population: 41 to PTR-VARIABLE+68,
1 to BEGIN-STRUCTURE+136, and the 3 broken ones to PATHZ+36. There is
no other cross-record interior branch and no tail-call B to an entry,
so the class is closed. Every one of the 43 sites sits at a
`start+len` address that NO record span covers - the signature of the
link, and the reason a per-record walk cannot see it.
WHAT THIS SETTLES ABOUT THE AUDIT. "The word at start+len is ret" is
NOT the check to keep: it answers 102 in the merged engine and 98 in
bin/hb, and ~59 of those are records whose start+len simply lands on
inline string data, so it carries a permanent false-positive
population. The census gives the total classification instead, and it
is the same shape as the DATA/code span partition aot-capture.f
already uses: EVERY branch in the blob targets a record entry, an
address inside its own record, or __text - and after the does-clause
gets its record, nothing else exists. The capture-side audit is
therefore "a B whose target is outside the window's code span must
resolve to a record entry, else refuse by name", which is a total
rule with no tolerated exceptions rather than a heuristic with a
false-positive list.
EM-COMPILE-FLUSH-PEND (habu2.f) writes `len = CP - start - 4`, which
is where the start+len convention comes from and where a clause
record's own length has to be written too.

DESIGN LEAF 2026-08-16 (bake-chain-11). Ruling 1 is surveyed and the
implementation is fully specified below; every unknown that could have
changed the shape has been answered against the tree, so the next
worker starts at the edit, not at the reading.
WHAT THE SURVEY SETTLED, each with its file:line:
- EXT NAMES ARE CAPTURABLE, cap 255 bytes (aot-capture.f
  ACAP-REC-NAME ~400-409 and ACAP-POOL-ADD's `u 255 >` at ~339; the
  chain already ships 45 EXT-named records). So a synthesized name
  longer than the 16-byte inline field is fine and needs no new
  machinery.
- LEGAL NAME BYTES are every byte > $20 (habu1.f EMIT-TOK ~3113-3121),
  so `NAME;does` is a legal, findable name. AVOID `:` in it - a colon
  not at an edge makes the name package-qualified (habu2.f
  C-QUALIFY-SEAL-GUARD ~2721) - and avoid a space, which would make
  the record permanently unfindable.
- THE DUP WALL is same-name-same-wid and exits 78 (habu2.f
  C-REJECT-DUP-DEF ~2658, C-DUP-DEF-FAIL ~2649), so the synthesized
  name must be unique in the parent's wid. It is reached from
  C-QUALIFY-DEF; a raw publication (the shape EM-AOT-REGISTER-RECS
  uses) skips it.
- OVERLAPPING RECORD SPANS ARE LEGITIMATE and documented, with two
  ordinary producers named (EXPORT aliases and republication:
  tools/code-owner.f 13-18, xref.f CODE-RECLAIM 476-504, proven by
  test/code-reclaim.f). NOTHING requires disjointness. So the parent's
  record does NOT have to be trimmed - the clause record may simply
  overlap it, which removes the whole blast radius that trimming
  (inliner, snapshot, size accounting) would have carried. TRIM
  NOTHING.
- THERE IS NO SPARE RESERVED WID. The reserved band is exactly
  {0,1,2} with FIRST-DYNAMIC-WID=3 (layout.f 122-124) and all three
  are live, so the "park the clause in a wordlist ordinary lookup
  never searches, and let the site's scope name it" idea is DEAD
  without bumping FIRST-DYNAMIC-WID. The clause record therefore takes
  THE PARENT'S OWN WID, which is also the right answer for the sealed-
  WID gate: the clause is exactly as public or private as its parent.
- THE LIFECYCLE. `:` allocates the record at &dict[NDICT] WITHOUT
  bumping NDICT and latches it in PEND-CELL (habu2.f EM-INTERPRET-COLON
  ~5638-5643); `;` writes [8] = CP - start - 4 in EM-COMPILE-FLUSH-PEND
  (~7234-7238) and only then bumps NDICT + LHIDXADD in
  EM-COMPILE-PUBLISH (~7374), clearing PEND-CELL at ~7379. Two paths
  abandon instead: hook-rejected (~7315) and held-for-chain (~7358).
THE EDIT, in order:
(1) layout.f: one new DATA cell (DOESREC-CELL) to hold the clause
    record's address, 0 when there is no clause.
(2) habu2.f J-DOES (~2476): after `J-EXIT`, CP is exactly the clause
    entry D. Allocate the clause record at &dict[NDICT+1] - NOT
    &dict[NDICT], which is the parent's pending slot - write [0]=CP,
    [40]=the parent's wid, and the name = the parent's name plus a
    `;does` suffix; latch the record address in DOESREC-CELL. The name
    bytes come from the parent's record via PEND-CELL ([24..40) inline,
    or the code-region pointer when DNAME-EXT is set), so the copy has
    to handle both.
(3) habu2.f EM-COMPILE-FLUSH-PEND (~7234): when DOESREC-CELL is
    non-zero, write the clause record's [8] = CP - clause.start - 4 by
    the same rule, inside the same protection span.
(4) habu2.f EM-COMPILE-PUBLISH (~7374): bump NDICT by 2 and call
    LHIDXADD for both records when a clause exists; clear DOESREC-CELL
    there and on BOTH abandon paths (~7315, ~7358).
(5) bootstrap/cg/forth.fs J-DOES (~3051) and its publish path: the
    same change. The mirror is the recovery host and docs/debugging.md
    says a mirror-only divergence stops tools/bootstrap.sh while every
    child-engine test stays green - so change it in the same commit and
    run the launcher.
(6) aot-capture.f: ACAP-CALL? (~69) currently answers only
    `$FC000000 and $94000000` (BL). Add the B class as a SITE, but only
    when its target is OUTSIDE the window's code span
    [AOT-CODE-B0 @, AOT-CODE-B0 @ + AOT-BLOB-LEN @) - an in-window B is
    ordinary control flow and a rigid move keeps it correct, which is
    why the 8912 in-window Bs must not become sites. For an
    out-of-window B whose ACAP-TGT>REC answers -1, DIE BY NAME rather
    than counting it into AOT-UNRES-N the way an unresolved BL is
    tolerated. ACAP-TGT decodes imm26 and is already correct for both
    opcodes; ACAP-ZERO-IMM already preserves the opcode.
RIDER (a) IS ALREADY SATISFIED and should get a test rather than a
change: the seed's patcher keeps whatever opcode is at the site -
habu2.f EM-AOT-PATCH-SITES `5 $FC000000 LIT64, 14 14 5 AND, 14 14 10
ORR,` (~4468) masks the top six bits and ORs the imm26 in. Pin it with
a case that proves a B site is still a B after seeding, because a B
silently patched to BL would corrupt x30 and be this crash's mirror
image.
WHAT TO EXPECT FROM THE GATE: one extra record per does>-definer. The
prefix has four (pointer-storage.f PTR-VARIABLE, enums.f x2,
structures.f) plus BEGIN-STRUCTURE, and lib adds string.f, codegen.f
and task.f x2. Record counts and the byte-identical fixpoint move;
attribute with the control build.
