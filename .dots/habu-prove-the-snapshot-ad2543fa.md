---
title: Prove the snapshot relocation round trip
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T09:22:08.248509+02:00"
---

Full context: PRIORITY 1 proof upgrade, directed 2026-07-30. The costliest defect class of this campaign - the deleted BL relocation pass, the four-value displacement lottery, stale persisted cells - all violated one unstated invariant: for every RECORDED site, writer-side canonicalization composed with loader-side rebase is the IDENTITY, for any writer and loader bases within BL reach. Nothing states or checks that today. Build the model: formal/Common/Reloc.v with the BL imm26 arithmetic (sign-preserving shift, 26-bit mask, the >>2 instruction units), the canonical forms (call displacement as if the region sat exactly REGION-OFF above text; declared DATA cells relative to the RBASE-VA sentinel - read src/habu/habu2.f SNAP-RELOC:EMIT-CALLS/EMIT-XT and src/habu/snap-lib.f SND-CANON-XT-CELLS for the shipped arithmetic), and theorems: round-trip identity for recorded sites under arbitrary base pairs within reach; region-internal displacements untouched; a non-call word at a recorded site is refused (the CALLMAP-RC arm), never rewritten. Binding to the shipped code is the hard half and must be honest: the pass is emitted assembly, so either (a) extract the displacement arithmetic into checked Habu words the emitter test drives with the SAME shared vector rows that generate the Rocq obligations (parity-gate style, one table), plus a mutation proof that skewing the emitted pass (shift constant off by one in EMIT-CALLS) reds a real write-restore boot test; or (b) if extraction is infeasible, the vector table drives a real snapshot write/restore on synthetic recorded sites and the dot records why. Never a model that only talks to itself - AGENTS.md Proof Integrity worth test applies: a plausible change to the SHIPPED arithmetic must falsify the gate.

Claim: agent=relocproof workspace=.jj-ws/habu-prove-the-snapshot-ad2543fa (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED (relocproof, 2026-07-30). What landed, and what every claim rests on.

Model: formal/Common/Reloc.v, 22 published results, no Admitted, no axioms;
every one reports closed under the global context. It states the BL imm26
arithmetic (the arithmetic shift by two, the 26-bit field, the sign extension),
the two canonical forms (a call displacement as if the region sat exactly
REGION-OFF above text; a declared DATA cell relative to the RBASE-VA sentinel),
and the three required theorems: image_round_trip (writer canonicalization then
loader rebase is the identity over a whole image), call_round_trip_rebases (the
same at one site for an arbitrary pair of bases within BL reach),
reloc_leaves_unrecorded (a site the map does not record is never touched) and
reloc_bad_site_not_rewritten (a recorded site holding a non-call keeps its bytes
and the walk reports CALLMAP-RC). It also carries three negative results, which
are what stop the file from only agreeing with itself: wraparound out of reach,
a misaligned region base breaking the round trip, and the same walk without the
call guard corrupting a data word.

Binding, option (a) as asked, done the strong way. The two passes are emitted
assembly, so no test can call them. test/compiler/reloc-vm.f decodes the token
stream of SNAP-RELOC:EMIT-CALLS and SNAP-RELOC:EMIT-XT out of src/habu/habu2.f
through the shared source lexer and RUNS that instruction sequence over a real
region image and a real call-map band. The arithmetic under test is therefore
the arithmetic in habu2.f, operand for operand - not a second copy of it. The
one shared vector table in test/compiler/reloc-schema.f drives both that run and
the generated Rocq obligations. The writer's address-cell half is checked Habu
inside builder-only src/habu/snap-lib.f, which no test can load; its three
bodies are pinned as exact token runs instead.

Completeness half (added on the coordinator's amendment). "Every recorded site
round-trips" is vacuous for an address class nobody records, which is how a
JIT-region address baked into region code as a MOVZ/MOVK chain came to crash a
restored image. Reloc.v now enumerates the emit vocabulary of habu2.f that can
put an address-bearing value into region bytes (eleven producers) and classifies
each with a total function over an inductive type, so a producer added without a
class is a Rocq error. On the Habu side the vocabulary is rebuilt FROM habu2.f:
nine closure rows each name a token and the exact ordered set of definitions
whose body carries it, and the gate recomputes every set. A new word calling the
shared MOVZ/MOVK carrier, or a second hand-built copy of that chain, therefore
fails the gate until it has been classified. The classification records the live
defect rather than hiding it: code_address_chain_is_the_open_gap states that
C-CODE-ADDR is region-dependent and NOT replayed by a snapshot restore, and
snapshot_covers_every_producer_but_code_addr states that everything else is. When
the fix lands, the first must be deleted and the second generalised.

FALSIFICATION MATRIX. Model side (schema, manifest and model edits, each
reverted):
  M1 one call row's canonical displacement -4192256 -> -4192255      RED
  M2 pinned REGION-OFF $1000000 -> $1000001                          RED
  M3 image_round_trip's manifest type row weakened to `True`         RED
  M4 C-CODE-ADDR reclassified Recorded R_aot_code_sites -> Fixed_mapping  RED
  M5 one `Print Assumptions` deleted from Reloc.v                    RED

Shipped side (src/habu/habu2.f edits, each restored byte-identically; the final
tree is byte-identical to cf55605a under src/habu):
  S1  `7 10 2 ASRI,` -> `3 ASRI`   (the delta's shift)               RED
  S2  `3 9 38 LSLI, 3 3 38 LSRI,` -> 39/39  (field width)            RED
  S3  the re-mask after the add, 38/38 -> 39/39                      RED
  S4  `3 BL-OP-HI CMPI,` -> `3 CALLMSG-LEN CMPI,`                    RED
  S5  `C-NE scbad BCOND,` -> `C-AL ...` (a condition never taught)   HARD REFUSAL
                                                     (E-CRL-DECODE, -6882)
  S6  the whole `CMPI/BCOND` call guard line deleted                 RED
  S7  `14 4 2 LSLI,` -> `3 LSLI` (site offset from word index)       RED
  S8  `9 9 26 LSRI, 9 9 26 LSLI, 9 9 3 ORR,` -> `9 0 3 ORR,`         RED
  S9  `3 9 38 LSLI, 3 3 38 LSRI,` -> 37/37                           GREEN
  S10 a new `: C-NEW-THING-ADDR ( -- ) C-ADDR-PUSH ;`                RED
  S11 a second hand-built chain (W-MOVZ0 used in C-DATA-ADDR)        RED

S9 is green and should be: BL-OP-HI is $25, whose low bit is set, so widening
the extracted field to twenty-seven bits and then OR-ing the opcode back
reproduces the identical word. It is a real equivalence in the shipped code, not
a hole in the gate.

RUNTIME MEASUREMENT, and why the shipped-assembly leg of the mutation proof is
BLOCKED. Engine rebuilt in this workspace with
`bin/hb --load tools/build-fixpoint-refresh.f -- install --force` (bin/hb copied
in first; ~14s), then `-- snap`.

  1. BASELINE IS ALREADY RED. With src/habu byte-identical to cf55605a, the
     image `$HB_TMP/hb-new` builds and reports "candidate validated", and then
     CRASHES on boot - SIGILL on one run, SIGSEGV on another, with the
     habu-crash register dump. There is therefore no green write-then-boot
     baseline for a mutation to break, and the green -> red -> green leg the dot
     asked for cannot be run until the code-address class is recorded and
     relocated (the regliteral lane).

  2. A SHARP, REPRODUCIBLE RED ON THE SHIPPED PATH, for that lane to use as its
     acceptance probe: with bin/hb UNCHANGED, three consecutive `-- snap` runs
     produce three DIFFERENT images. Sizes identical at 15,318,112 bytes; 9,613
     and 9,617 bytes differ between runs; the first difference is at offset
     384,935. A snapshot image is supposed to be byte-identical whatever base
     the writing run got (src/habu/layout.f, and
     canonical_form_is_base_independent in the model). It is not, and the size
     of the differing set is about what an unrelocated MOVZ/MOVK address class
     would account for. Acceptance probe for the fix: three consecutive `-- snap`
     runs must be byte-identical, and the image must boot.

  3. IMAGE BYTE COUNTS ARE NOT A USABLE PROXY, so no claim is made from them.
     Under S1 the engine still builds and the image differs from baseline in
     18,707 bytes, against 6,748 for the semantically-equivalent S9 - but the
     run-to-run noise floor measured in (2) is 9,589 to 9,617 bytes, which
     overlaps. The comparison is recorded and explicitly not relied on.

WHAT THE GATE DOES AND DOES NOT PROVE ABOUT THE EMITTED ASSEMBLY. It runs the
instruction SEQUENCE that habu2.f emits, decoded from that source: mnemonics and
operands, with the meaning each mnemonic has. It does not run the encoded
instruction words, so the encoders under src/arch/arm64 and the machine code a
CPU finally executes are outside it - they have their own tests. It also does
not see the region walk's interaction with the rest of a real boot. Within those
limits it is sensitive to every semantic change to the shipped arithmetic that
was tried, including the guard, the field width, the shift, the site indexing
and the opcode-preserving write.

FOLLOW-ON WORK (dots to mint, not done here):
  - the snapshot write-then-boot mutation leg, once the code-address class is
    recorded: re-run S1 against a green baseline and record it here;
  - a scheduled regression for image byte-identity across three `-- snap` runs;
  - the gate cannot see an emitter that assembles an address through the generic
    word emitter LCEMIT without the fixed chain (MODEL GAP 7 in Reloc.v); closing
    that needs the emitted bytes themselves classified.

NOTE FOR THE regliteral LANE (habu-relocate-persisted-region-47de06b9): the nine
closure rows in test/compiler/reloc-schema.f freeze, among others, the exact set
of definitions that carry C-CODE-ADDR ({J-SEMIQUOT, C-POSTPONE, C-BTICK}) and
SNAP-RELOC:MARK-CELL ({EM-STARTUP-RUNTIME-STATE}). Adding a recording call or a
new site will red test/compiler/reloc-proof.f until those rows and the model's
`classify` are updated together. That is the forcing function, not an accident.
