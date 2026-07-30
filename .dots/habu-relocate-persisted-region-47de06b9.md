---
title: Relocate persisted region address literals
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-30T09:48:48.527626+02:00\""
---

PRIORITY 1: this is the defect that keeps the gate phase owner-wid-internal red and blocks the 200-clean-boot campaign acceptance; it is NOT a DATA cell, so it is not dot habu-fix-persisted-dangling-a520f7b4's class. Proven under lldb on a snapshot image built by 'bin/hb --load tools/build-fixpoint-refresh.f -- snap' from the tree at change 5721f554: the image boots and prints for '1 . cr', then dies the moment it compiles a definition. Signal 11, program counter 0x10627b8b4, live region base (x26) 0x1053e0000 - 0x69b8b4 past the end of an 8 MiB region, so the target is not in this run's region at all. Disassembly of the caller settles what it is. Region code at offset 0x2eb8cc holds 'mov x9,#0xb8b4 / movk x9,#0x627,lsl 16 / movk x9,#1,lsl 32 / movk x9,#0,lsl 48 / str x9,[x19] / bl <catch>' - that is the four-instruction MOVZ/MOVK x9 address chain habu2.f C-ADDR-RAW emits, pushing the entry address of the quotation that starts eight instructions earlier at region offset 0x2eb8b4, and then calling catch. In other words '[: ... ;] catch'. The correct value for this run is 0x1012fb8b4; the image carries 0x10627b8b4, the address the WRITING run's region had. Root cause: nothing relocates address literals compiled into region code. LSNAPRBD (habu2.f EM-SNAPSHOT-REBASE-DICT) walks DICTIONARY RECORDS only - field [0], the code start, and field [24], the external name pointer. The comment in src/habu/layout.f that says region-internal pointers canonicalise to the RBASE-VA sentinel is true of record [0] and of nothing else. Every quotation entry address, every ['] and every postpone target compiled by C-CODE-ADDR is therefore persisted as the writing run's absolute address, and since the region no longer has a fixed base every one of them is wrong in every restored run. Independent corroboration: two snapshot images built from one hb-stdin and one hb-snap-src differ in about 3100 to 6200 REGION bytes, which is this class and not DATA. Fix, in the shape the campaign already uses for calls: declare the site where the kind is decided. C-CODE-ADDR (habu2.f:39) is the single emit point for a region address literal and C-DATA-ADDR (habu2.f:35) is the single emit point for a DATA one, so the emitter knows which is which without decoding anything; record each C-CODE-ADDR chain's region offset in a site map beside SNAP-RELOC:CALLMAP-OFF in src/habu/layout.f, and add a relocation pass modelled exactly on SNAP-RELOC:EMIT-CALLS that rewrites the chain's four immediates - writer: live region base to the RBASE-VA sentinel, loader: sentinel to the live base. Do NOT scan region bytes for anything that looks like an address chain: compiled words carry inline non-instruction data, which is the same reason the call map is recorded at emit time. Feed the map from EM-AOT-PATCH-SITES too if that pass can create or move such a literal. Bump SNAP-FORMAT-VERSION, since a version 5 engine would read the canonical form as a live address. Acceptance: a restored image compiles a definition and runs it; test/owner-wid-internal.f goes green; 200 consecutive bare runs exit 0.

Claim: agent=regliteral workspace=.jj-ws/habu-relocate-persisted-region-47de06b9

MEASURED 2026-07-30 (agent=regliteral, commit "Relocate persisted region
address literals"). The class this dot names is real, is now fixed, and is
proven fixed. It was NOT the whole reason a restored image cannot compile: two
further defects of a different class sit behind it, both now named with
evidence and both dotted.

What the change does. src/habu/layout.f gains a second one-bit-per-region-word
map, SNAP-RELOC:ADDRMAP-OFF, immediately after the call map and sized from
REGION the same way, so it cannot overflow and needs no capacity check; its
contents are keyed by region offset and are therefore identical in every run.
A separate map rather than a second bit in the call map: a call site and a
chain start are different instruction shapes at different addresses, the two
passes rewrite completely different fields, a two-bit call map would cost the
same bytes while forcing the proven call pass to decode a tag it does not need,
and two one-bit maps let both passes keep the identical bit-scan loop.
src/habu/habu2.f gains SNAP-RELOC:EMIT-ADDR-SITE, which records the site, and
SNAP-RELOC:EMIT-ADDRS, which rewrites it. C-CODE-ADDR calls the recorder with
CP still pointing at the chain's first word, so the recorded site can never be
off by an instruction, and C-DATA-ADDR / C-DATA-ADDR-RAW deliberately do not:
DATA is mapped at a fixed address in every run, so a persisted DATA literal is
already correct where it is. Nothing anywhere looks at region bytes or at the
value a chain carries.

One correction to this dot's prescription. `[']` and `postpone` can name a
PRIMITIVE, whose code is in the engine's loaded __text and not in the region,
so a single region-band rewrite would have left those chains raw. EMIT-ADDRS is
therefore parameterized exactly like the dictionary walk LSNAPRBD - x21 band
base, x22 band length, x25 the base the band is moving to - and is called ONCE
PER BAND, right behind the dictionary walk for that band, in both BSNAPREBASE
and EM-SNAPSHOT-RESTORE. The two bands are disjoint at both write and restore
time (the live region sits REGION-OFF above __text; canonical text offsets are
far below the RBASE-VA sentinel), so every chain is rewritten by exactly one of
the two calls. The band test is the extent of the object being moved, not a
guess about a value, which is the same structural test EMIT-CEMITBL already
uses.

A second producer, as the dot suspected but in a different word. It is not
EM-AOT-PATCH-SITES (that patches BL immediates and already records into the
call map); it is EM-AOT-RELOC-CODE, the AOT seed's code-literal rebase, which
rewrites captured MOVZ/MOVK x9 chains after copying the seed blob. It now
records each site in the same map, and it knows its literals are code addresses
without decoding anything, because they come off the captured CODE-site list
while the DATA-site list is walked by a different word.

Fail-closed. A recorded site that no longer holds the chain (all four words
masked against W-MOVZ0/W-MOVK1/W-MOVK2/W-MOVK3, which pins the destination
register and the shift as well as the opcode) exits ADDRMAP-RC = 97 with
"hb: snapshot address map mismatch", the exact analogue of CALLMAP-RC.
SNAP-FORMAT-VERSION is 5 -> 6: a version 5 image stores these chains as the
writing run's absolute addresses and a version 6 image stores them
canonicalized, so each engine must refuse the other's image rather than execute
its literals. bootstrap/cg/forth.fs keeps its own SNAP-FORMAT-VERSION (3) and
its own pre-BL wire format and is deliberately NOT touched, per this campaign's
round-3 rule that each build path relocates its own format.

MEASURED, all on this workspace with `bin/hb --load
tools/build-fixpoint-refresh.f -- install --force` rerun before every
measurement.

  Region drift, the direct measure of this class. Two snapshot images built
  from one unchanged bin/hb, compared byte by byte and bucketed with the
  trailer's own lengths (region file base = trailer base - data length - region
  length; region offsets below DICT-SIZE are dictionary records, above it are
  compiled code):
     before: 9,243 differing bytes total - 6,203 region (3,102 four-byte words:
             2,792 in the dictionary area, 310 in the CODE area), 138 DATA,
             2,902 in the trailing extra section.
     after:  6,472 differing bytes total - 5,603 region (2,802 words, ALL of
             them dictionary-area), 137 DATA, 735 extra.
     The CODE-area drift is 310 words -> 0. Every one of those 310 was the
     second instruction of a chain (only the bits-16-to-31 immediate moved,
     because the region base is 64 KiB aligned), and both builds put the same
     region OFFSET in it - e.g. one site read 0x1017195d0 in build A and
     0x1033395d0 in build B, both exactly base + 0x1895D0. That is what proved
     the class before the change and what proves it gone after.
  Determinism probe as requested: three consecutive snap builds from one
  bin/hb now differ by 6,475 / 6,471 / 6,454 bytes pairwise, with the CODE-area
  word count 0 in all three pairs. The residual is entirely the dictionary-area
  and extra-section classes, which this dot does not own (see the residual
  section below).
  Bare boots: 200 consecutive runs of the validated image with input "1 . cr",
  200 exit 0, 0 failures.
  test/snapshot-xt-cell-decl.f: green.
  package-diff-lint 0, typed-local-diff-lint 0, error-code-lint 0 finding(s),
  trust-lint 0 finding(s) (no TRUSTED row changed), dot-dep-lint 0 finding(s).
  Engine fixpoint rebuild green; self-check census 0 uncheckable, 0 rejected.

FALSIFICATION (measured, not argued). Delete the two loader arms
(SNAP-RELOC:LADDRS in EM-SNAPSHOT-RESTORE), leave everything else alone, and
rebuild. In two images built that way and the correct way, with the SAME live
region base 0x1010b0000 under lldb, the chain at region offset 0x32bf30 - the
`[: SD-GUARDED ;] catch` inside src/core/structure-decl.f SD-REPLAY - reads:
  correct build: mov x9,#0xbf18 / movk x9,#0x13d,lsl 16 / movk x9,#1,lsl 32
                 -> 0x1013dbf18 = live region base + 0x32BF18, the quotation body
  arms deleted:  mov x9,#0xbf18 / movk x9,#0x32,lsl 16 / movk x9,#3,lsl 32
                 -> 0x30032bf18 = the RBASE-VA sentinel, never rebased, unmapped
Loading library source through that build's restored image dies rc 134; the
correct build gets further. So the loader arm is load-bearing and its removal
is visible in the image's own instructions.

WHAT IS STILL RED, and why it is not this class. test/owner-wid-internal.f is
still red and tools/build-fixpoint-test.f still fails asserts 144, 151 and 152
with rc 134, all of them the same thing: a restored image still dies when it
compiles a definition. That crash is NO LONGER an address literal. Traced with
lldb: the executed token is read from a persisted DP-heap cell, the cell IS
present in the image file (so it is persisted, not computed), and the frame
that executes it is CHECKER-CERT-CALL. The cell is
src/core/checker.f CHECKER-CERT:PRODUCER-XT, a plain `variable` that an
`execute` dispatches through, so nothing ever declares it to the address-cell
table the way `defer`/`is` declare a dispatch cell. Behind it is exactly one
more of the same kind, src/core/lower-cert-base.f LOWER-CERT:FULL-XT.
PROVEN, not guessed: with both converted to declared defers ON TOP of this
change, a restored image runs ": FOO 1 . ;" then "FOO" and exits 0. That
experiment was reverted and is not in this commit; it is dotted as
habu-declare-persisted-producer-76fbce09, which is now the blocker for this
dot's acceptance items 1 and 2 and for the owner-wid-internal gate phase.

RESIDUAL, dotted, not silently accepted:
  - The 2,802 dictionary-area words that still drift are dead records above
    ndict plus uninitialised padding in inline name fields, plus eleven LIVE
    records (5608..5618: SNAP-TAIL-MARK, ARM64-W32I, MSK-MATCH?, ...) whose
    code start is at or above CP and therefore outside the persisted payload.
    Dotted as habu-stop-dictionary-records-3280a444.
  - Both site maps record a region word and neither is cleared when CP rewinds,
    so a truncate-and-recompile that leaves a stale bit over a word that is no
    longer a call or a chain would exit CALLMAP-RC / ADDRMAP-RC on a legitimate
    image. It is loud, never silent, and did not fire in any build, boot or test
    here, but it is a real hole shared with the landed call map. Dotted as
    habu-clear-relocation-site-b270c651.

BEST LONG-TERM FIX OR A PATCH? Long-term. The invariant is re-derived from the
code, not from any label: an address compiled into region code is only
meaningful relative to the base of the thing it names, and the emitter is the
only place that knows which thing that is - C-CODE-ADDR names code, C-DATA-ADDR
names DATA, and the AOT seed's two site lists name the same distinction again.
The fix records the kind exactly there and nowhere else, and the relocation
reads a table rather than the bytes. It rests on no magic value, no lucky
range, no timing: the only test at rewrite time is whether an address lies
inside the extent of the band being moved, which is a fact about the object,
and the site's shape is verified against the four exact instruction encodings
before a byte is written. The map is sized from REGION so it cannot overflow,
is keyed by offset so it never needs canonicalising itself, and the format
version makes the two meanings of the same bytes refuse each other.

NOTE FOR THE ORCHESTRATOR: the reloc-proof gate mentioned at dispatch
(test/compiler/reloc-proof.f, formal/Common/Reloc.v, theorem
code_address_chain_is_the_open_gap) does not exist in this workspace, in this
dot's base, or at master@origin as of the fetch on 2026-07-30. Nothing here
could update its closure rows or its classify function. When that lane lands,
C-CODE-ADDR moves from open gap to recorded-and-relocated: it is recorded at
the emit site into SNAP-RELOC:ADDRMAP-OFF by EMIT-ADDR-SITE and by
EM-AOT-RELOC-CODE, and relocated by EMIT-ADDRS once per band, all four
immediates rewritten together.

MEASURED 2026-07-30 (agent=relocsync, commit "Prove relocated address chains
in the parity gate"). The parity gate the note above said did not exist has
since landed, and it went red on this tree exactly as designed: its model still
described C-CODE-ADDR as the open gap and its closure rows still froze the old
caller sets. This change is the other half of the work: the model and the rows
now describe what the emitter actually does, and the gate is green again
because the new behaviour is proven, not because anything was weakened.

What the model now says. formal/Common/Reloc.v gains a third relocation class
beside the call displacements and the declared address cells: the
four-instruction MOVZ/MOVK chain. One 16-bit immediate of a MOVZ or MOVK is
modelled as `(w / 32) mod 65536` and everything else in the word as
`scaffold_of`; four of them spell out a 64-bit address. On top of that sit the
band move the shipped SNAP-RELOC:EMIT-ADDRS performs -- a chain whose address
falls inside the band being moved becomes `address - band base + target base`,
and one that does not is left alone -- and a walk over a whole image's worth of
recorded sites.

Fourteen new results, all closed under the global context, none admitted:
the shipped ADDR-OPC-MASK really is the complement of the modelled immediate
field; a chain built the way the compiler builds one passes the shipped check;
the address read back out of a rewritten chain is the one written in; writing
back the address a chain already carries leaves the four words untouched; a
relocated chain is still a chain, so running the pass twice is meaningful; a
chain outside the band is not touched; the canonical image carries the offset
within the band and not the writing run's base; the writer's pass composed with
the loader's is the identity at one chain and over a whole image; and for an
arbitrary pair of bases the restored chain names the same word at the base this
run got. The two negative results that mattered are kept and joined by two
more: an unrecorded site is never visited, a recorded site that is not a chain
is refused with ADDRMAP-RC (97) and keeps its bytes, a pass that wrote only
three of the four immediates loses the top sixteen bits of the address, and
dropping the chain guard rewrites a data word in silence.

The open-gap theorem is gone, replaced by the general form its own comment
promised: `snapshot_covers_every_producer`. That is not a restatement of a
definition. The recorder for the AOT seed's capture-time code-literal list is
deliberately kept in the model with "the restore does not replay this" attached
to it, so classifying any producer there makes the theorem fail. The emit
vocabulary also gains EM-AOT-RELOC-CODE, the second place that writes a code
address into region bytes, and EM-AOT-RELOC-DATA beside it; C-CODE-ADDR and
EM-AOT-RELOC-CODE are both classified as recorded in the address-literal map,
which a restore does replay.

What the rows now ask. test/compiler/reloc-schema.f gains eight more pinned
constants (ADDRMAP-RC from layout.f, ADDR-OPC-MASK / ADDR-IMM-MASK /
ADDR-CHAIN-BYTES from habu2.f, and the four scaffold words W-MOVZ0..W-MOVK3
from habu1.f, which is a source file this gate had not read before) and five
address-literal chain rows. Each row names the band the pass is moving and
every four-word slot of a small region. test/compiler/reloc-cases.f decodes the
shipped SNAP-RELOC:EMIT-ADDRS out of habu2.f and RUNS it over a real region
image and a real address-literal map band, once per leg, exactly the way the
call rows are already driven; test/compiler/reloc-obligations.f turns the very
same rows into `addr_walk` obligations about the model. The four words of a
slot are never written down on either side: both build them from the row's
address and the scaffold words read out of habu1.f.

test/compiler/reloc-vm.f needed three things it did not have: the register-to-
register AND, and the three UNSIGNED conditions C-CC, C-CS and C-HI that the
band test uses. Modelling those as signed would have let a band above 2^63 pass
a test the hardware fails, so the machine folds both operands by the sign bit
and compares.

Closure rows, enumerated from the shipped source rather than guessed. W-MOVZ0
now occurs in two definitions, C-ADDR-RAW and EMIT-ADDRS, because the pass has
to recognise the shape it rewrites; that is what turned the gate red in the
first place. Three rows are new: SNAP-RELOC:MARK-SITE is carried by C-CODE-ADDR
and nothing else, SNAP-RELOC:ADDRMAP-OFF by EM-AOT-RELOC-CODE and nothing else,
and SNAP-RELOC:LADDRS by BSNAPREBASE, EM-SNAPSHOT-RESTORE and CORE -- which is
the per-band call structure the model's band parameter depends on.

FALSIFICATION MATRIX (measured, every mutation applied and reverted).

  1. EMIT-ADDRS drops the write of the fourth immediate.
     First attempt: GATE STAYED GREEN. Every address in the first four chain
     rows is below 2^48, because real region bases and the RBASE-VA sentinel
     are, so the fourth immediate was zero on both sides and skipping it
     changed nothing. The rows were the problem, not the pass. A fifth row,
     chain_wide, was added: a band whose bases differ in every one of the four
     sixteen-bit fields. With it, the same mutation reds asserts 200 and 202,
     the chain image comparisons, and nothing else. This is the strongest
     single thing this change bought and it was found by trying to break the
     gate rather than by reading it.
  2. EMIT-ADDRS drops the band's lower-bound skip: reds asserts 194 and 196,
     the chain_other_band row, and nothing else.
  3. EMIT-ADDRS drops the fourth scaffold check: reds asserts 197 and 198, the
     chain_refuse row's exit status and image, and nothing else.
  4. The model classifies C-CODE-ADDR back to the seed-only site list: the
     model stops compiling, "Unable to unify true with snapshot_covers
     P_code_addr".
  5. The model's band move forgets to subtract the band base: the model stops
     compiling on canonical_chain_is_base_independent.
  6. W-MOVK3 renumbered in habu1.f: the pinned-constant row reds.
  7. One chain row's canonical address skewed by one: BOTH halves red -- the
     Habu image comparison and, separately compiled to be sure, the generated
     Rocq obligation for the same row. That is the evidence that the row really
     is one artifact with two readers.
  8. C-CODE-ADDR stops recording its site: the SNAP-RELOC:MARK-SITE closure row
     reds.

  Mutation 1 was additionally run through a real engine rebuild in both
  directions: skew habu2.f, `bin/hb --load tools/build-fixpoint-refresh.f --
  install --force` (green, census 0 uncheckable / 0 rejected), gate red on the
  chain rows only; restore habu2.f byte-identical, rebuild again (green), gate
  green. src/habu is byte-identical to the base change in the final tree.

GATES on the final tree: test/compiler/reloc-proof.f exit 0,
test/compiler/reloc-manifest.f exit 0, test/compiler/insn-proof.f exit 0,
`make -C formal` green then `make -C formal clean`, package-diff-lint and
typed-local-diff-lint exit 0 on the change's own `jj diff --git`,
error-code-lint 0 finding(s), suite-coverage-lint 0 finding(s), dot-dep-lint
0 finding(s).

BEST LONG-TERM FIX OR A PATCH? Long-term, and re-derived rather than taken on
trust. The question a gate like this has to answer is whether the thing it
proves is the thing that ships. The invariant is that an address baked into
region code is only meaningful relative to the base of the object it names, so
the model has to be parameterized by that object's extent -- and it is, with
the same triple the shipped pass takes, which is why the two-call design's
"each chain moves under exactly one band" is a stated theorem and not a comment.
Nothing rests on a magic value: the addresses in the rows are frozen literals
worked out from the geometry each row names, the four instruction words are
built from constants read out of the shipped source, and the one place a
constant is written twice (the model and the schema) is held equal by the
pinned-constant rows. The place this could have been a patch is the
classification: it would have been easy to delete the seed-only recorder along
with the open-gap theorem, which would have left `snapshot_covers_every_producer`
true by construction and worth nothing. It is kept, so the theorem still fails
the moment a producer is named only by a table the restore does not walk.

HONEST GAPS, not silently accepted.
  - The gate still binds the model to the shipped INSTRUCTION SEQUENCE, one
    step short of the shipped bytes, for the address pass exactly as for the
    call pass: the machine reads mnemonics and operands, and the encoders in
    src/arch/arm64 have their own tests.
  - The fixture sets the address-map bits itself, the same way the call rows
    set the call-map bits. So the bit index SNAP-RELOC:EMIT-ADDR-SITE writes is
    not checked against the bit index EMIT-ADDRS reads; an off-by-one shared
    between the recorder and the relocator would pass. Running the recorder's
    own instruction sequence would close this and needs two more mnemonics
    (LSLV, STRB) in the machine. Dotted as
    habu-run-the-addr-e9252c2a.
  - The model's slot list is one entry per four-word chain; the shipped pass
    walks one bit per region word and reads four words at a set bit. That
    stride, like the bitmap indexing, is exercised only on the Habu side. It is
    MODEL GAP 3 and is named there.
  - The model treats registers as unbounded integers. The band comparisons are
    proved in Z and the shipped ones are unsigned 64-bit; they agree while
    every address stays below 2^63, which is MODEL GAP 1 and is where the
    machine's own unsigned compares do the real work.
