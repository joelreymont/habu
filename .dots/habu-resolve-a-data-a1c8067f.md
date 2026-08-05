---
title: "Resolve a data word's address in the native chain"
status: active
priority: 2
issue-type: task
created-at: "2026-08-01T11:58:25.944987+02:00"
---

The HIR source-word model now carries a 'fixed' meaning: a word that pushes one value, which is what a create-d data word does. The value is STATED by whoever builds the word model (test/compiler/native-source-fixture.f NSRC:MODEL-DATA, tools/codegen-compare-chain.f), because the chain cannot yet look a data word's address up in the engine's dictionary. Wanted: the elaborator resolves a name the word model does not declare by asking the engine what that word is - a data word answers its address - so tools/codegen-compare-corpus.f CELL-BUMP compiles from its own spelling with nothing told to the harness. Also the AOT half of it: a published routine holding a raw process address needs a relocation, which is why this is a capability and not a one-line lookup.

Scout update (2026-08-05): still real and THE one hard capability blocker for the cut — every create'd data word in the engine's own source hits it (migrate.f:288-290, hir-word.f:589-593 still state the parked-address seam). Path repair: the cited tools/codegen-compare-chain.f is now tools/codegen-compare-migrated.f (CELL-BUMP still hands BUMP-ADDR to DEFINE-DATA at :83-90). The AOT half (a published routine holding a raw process address needs a relocation record — the new publisher owns relocation, so the record kind lands there) is untouched.

Claim: agent=dataword workspace=.jj-ws/habu-resolve-a-data-a1c8067f

RELOCATION DECIDED (2026-08-05, dataword): NO NEW RELOCATION KIND IS NEEDED FOR
THE SNAPSHOT. A data-address literal is restore-invariant by construction, and
the invariant is enforced fail-closed at boot. Four independent pieces of
evidence, all read off the tree:

1. src/habu/layout.f:767-770 states it as the design, in the address-literal
   map's own prose: "The sibling C-DATA-ADDR literals are deliberately NOT
   recorded: they hold DATA addresses, and DATA is mapped at a fixed address in
   every run, so they are already the same in the writing and the restoring
   run."
2. The emitter agrees at the emit point. habu2.f C-CODE-ADDR (:213) calls
   SNAP-RELOC:MARK-SITE before pushing; C-DATA-ADDR (:207) is bare C-ADDR-PUSH
   with no mark. The engine already distinguishes the two kinds where the
   literal is BUILT, which is where layout.f says the kind must be decided.
3. The mapping is MAP_FIXED and VERIFIED, not assumed. habu2.f
   EM-MMAP-DATA-REGION (:3986) maps DATA-VA with MAP-ANON-PRIVATE-FIXED, then
   compares the result against DATA-VA and exits 78 ("hb: cannot map fixed data
   region") on mismatch. src/habu/aot-lib.f EMIT-DATA-REGION-MAP (:131) does the
   same for the AOT entry.
4. aot-lib.f:87-88 says it in words: "the entry maps DATA-VA and copies it back
   to the SAME absolute VA (DATA-VA is a fixed MAP_FIXED VA, so those addresses
   are load-stable)".

So the publisher needs no data-address record and publish.f's contract is
unchanged: RELOC-CALLS covers BL displacements because a call's distance depends
on where the kernel put the region, and a DATA address does not depend on that
at all.

BUT THE AOT SEED IS A DIFFERENT PATH AND IT HAS AN x9-SHAPED HOLE. habu2.f
EM-AOT-RELOC-DATA (:3840) is a THIRD relocation class that DOES rebase DATA
literals, by a single delta (seedDP - captureD0), because the seed replays a
captured REPL DATA span at a possibly different DP. It rewrites recorded sites,
and the sites come from aot-capture.f ACAP-SCAN-DATA (:250), which finds them
with ACAP-LIT9? - a scan for a movz/movk chain into x9 SPECIFICALLY. The engine
always materialises through x9 (C-ADDR-RAW ends `9 8 7 ORR`), so the scan is
complete for engine-emitted code. The native chain's register allocator picks
whatever register is free, so a chain-emitted data-address literal is NOT in x9
and ACAP-SCAN-DATA would not see it. That is not a bug today - it only matters
if chain-published routines ever enter the AOT seed capture - but it is a real
precondition on that work and it is recorded here rather than discovered later.

THE DERIVATION MECHANISM, AND WHY IT IS NOT A RECORD READ. There is no record
slot holding a created word's data address: xref.f's slots are START(0), LEN(1),
FLAGS(2), NAME(3), WORDLIST(5). The address lives only as the four-instruction
MOVZ/MOVK chain C-ADDR-RAW baked into the word's own code, and layout.f:771-773
forbids recovering it by decoding - "Nothing ever recognises a chain by looking
at region bytes or at the value a chain carries: a compiled word may hold inline
non-instruction data, and an ordinary integer may hold any value at all." So
"ask the engine what a created word is" has exactly one honest form: EXECUTE it.
A created word's published effect is `-- ptr a` (habu2.f
LASTC-TRUST:PUBLISH-PTR-A) and its body pushes its address and returns, so
running it IS the engine answering. migrate.f already resolves names through the
engine's own `search-wl` (:185-187), which is the idiom to extend. The corpus
seam confirms the shape: tools/codegen-compare-corpus.f:70 is
`TRUSTED: BUMP-ADDR ( -- n ) BUMP-CELL ;` - the caller is already executing the
data word and handing over the result, so the repair moves that execution from
the caller into the chain, it does not invent a new source of truth.

NOT BUILT. Nothing in src/ or test/ changed. The relocation question the dot
calls its heart is answered above with evidence; the derivation is specified but
unimplemented, and its open design point is where the execute-the-word boundary
lives and how it is typed (`execute` is arity-guarded), plus the DEFINE-DATA
parameter deletion rippling through migrate.f, hir-word.f, the corpus case files
and the fixtures.

DECIDED, READY TO BUILD (2026-08-06): (1) Relocation — NO new kind: a data-address literal is restore-invariant by enforced construction (layout.f:767-770 states it, C-DATA-ADDR emits unmarked while C-CODE-ADDR marks, EM-MMAP-DATA-REGION exits 78 on a misplaced MAP_FIXED, aot-lib.f verifies the AOT entry). The publisher's contract is unchanged. (2) The derivation — execute the created word: no record slot carries the address and recovering it from chain bytes is forbidden (layout.f:771-773); a created word's published effect is -- ptr a, and the corpus's TRUSTED BUMP-ADDR is already this query performed caller-side. BUILD: move that execution into the chain's migration entry — resolve the name via the existing search-wl idiom (migrate.f:185-187), execute under ONE named TRUSTED boundary at the engine-facing entry (typed capability for arity-guarded execute is the missing checker feature — reference the boundary to habu-make-retire-on-051d25aa's sibling pattern and file the capability dot at build time), delete DEFINE-DATA's stated-address parameter and DECLARE-FIXED's acceptance, thread the corpus. Mutation: FORGET + redefine must yield the new address. Byte-identity: deriving the same number emits the same code. (3) HAZARD GATE for the cut: before any chain routine enters the AOT seed capture, ACAP-SCAN-DATA's x9 assumption (ACAP-LIT9?) must be stated at the scan site or generalised — a chain-emitted data literal would be silently missed and fail as a wrong address at boot (recorded also for the cut lane).
