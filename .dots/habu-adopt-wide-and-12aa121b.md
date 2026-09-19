---
title: Adopt wide and parametric typed locals across the codebase
status: active
priority: 2
issue-type: task
created-at: "2026-09-19T18:23:03.521709+03:00"
---

Active migration: alder, .jj-ws/alder-binding-locals on 3dbac18c; binding.f
after the wide-local engine integrated. Keep the existing assertions unchanged.

Active migration: alder, .jj-ws/alder-ir-build-locals on 3dbac18c; the released
IR builder and schema transport sites, with existing assertions unchanged.

Problem: Joel (2026-09-19): the wide/multi-cell and parametric typed-local features exist to improve the codebase, so every site that destructures a layout value only because a typed local could not hold it must be found and converted. Today src/, lib/, tools/ and test/ carry the workaround shapes the handoff names: unpack-then-repack around a use (X:UNMAKE ... X:MAKE), locals exploded into scalar cells ({: a b c d :} over a two-field value), adapter words that exist only to name pieces, swap/rot juggling around MATCH payloads. Acceptance (alder, after each feature dot lands and its engine is integrated): (1) an audit listing every candidate site by file:line with the shape it exhibits, measured by reading, over src/ lib/ tools/ test/ and, read-only, the consumer trees (loom, maki, kiba, radar, Tender - reported to their owners, never edited here); (2) migrations in commit-sized groups per subsystem, each keeping behaviour (its suites unchanged in what they assert), each reviewed on the line before it chains, converted sites named in the commit body; sites in a live hazel lane (span band 1 files, the native compiler, checker.f) are handed to that lane's owner instead of edited; (3) the card (docs/forth-card.md) and docs/forth.md state the idiom: a domain value stays a named local, destructure only to compute. Depends: habu-bind-a-wide-bc67d207, habu-parse-local-annotations-50be4d43. Ownership: alder (audit and migrations), hazel (review, the lane-owned files). Claim: agent=alder workspace=.jj-ws/alder-review-fixes. Measure, do not count lines: the planner port that motivated these features (~/Downloads/habu-full-session-history.md sections 9-13) reported the effect as representation plumbing - reconstruction sites 23 -> 0, DISCARD-* calls 14 -> 0, destructuring sites 17 -> 13 (the rest compute on raw fields) - while substantive lines moved 159 -> 153; every migration commit states those three counts for its group before and after, and a site whose destructuring is the computation stays.

## Read audit, base 39d9a399

The first pass searched src/, lib/, tools/ and test/ for UNMAKE, long scalar
local groups, reconstruction and comments naming local restrictions, then read
the candidate bodies. These are migration candidates, not claims that a
replacement has compiled. Feature integration and behavior-preserving suites
remain prerequisites. Keep public representations and validation order.

### Alder: files released by Hazel after wide-local integration

| Site | Plumbing to remove |
| --- | --- |
| src/compiler/binding.f:86 BIND | Ten scalar fields stand in for two validated values; only contraction/features are inspected, then both values are rebuilt. |
| src/compiler/binding.f:107 POLICY@ | Five policy locals and a reconstruction solely to discard the target below it. |
| src/compiler/binding.f:117 SAME? | Ten locals retain one binding's components until the two existing component comparisons. |
| src/compiler/binding.f:134 ENCODE | Five locals preserve the policy while the target preimage is appended. |
| src/compiler/ir/build.f:944 INTERN-DIGEST-ATTR | Four digest words unpacked and reconstructed around ATTR-USE. |
| src/compiler/ir/build.f:1041 SET-OP-SPAN | Three span fields unpacked and reconstructed around owner validation. |
| src/compiler/ir/build.f:1103 SET-FUN-SPAN | Same span transport around function owner validation. |
| src/compiler/ir/build.f:1139 SET-BLOCK-SPAN | Same span transport around block owner validation. |
| src/compiler/ir/build.f:1270 SPAN-CK | Span unpacked and rebuilt only to put the arena before it. This IR-SOURCE span is arity zero, not SPAN:span<t>. |
| src/compiler/ir/schema.f:1416 VERIFY | Digest unpacked to reach the two arenas beneath it, rebuilt for comparison. |
| src/compiler/ir/schema.f:1578 FVERIFY | Same transport for frozen views. |

Keep schema edits at these two sites: the separate fixed-register constraint
lane will extend the schema later. One reviewed commit per file group.

### Hazel: schedule inside the native lanes that own these files

| Site | Plumbing to remove |
| --- | --- |
| src/compiler/native/tape.f:232 MK | Source span unpacked into three locals and reconstructed unchanged. |
| src/compiler/native/tape.f:587 VERIFY | Four digest words preserved only for reconstruction. |
| src/compiler/native/regalloc-verify.f:1029 SLOT-CK | Fourteen routine fields rebuilt inside the nested slot loop; retain one validated routine. |
| src/compiler/native/regalloc-verify.f:2344 ACCEPT | Fourteen locals reconstruct the routine for pool readers and for every function's SLOT-CK. |
| src/compiler/native/select.f:3469 SELECT | Routine exploded at entry and remade for later consumers; keep one value and project only the fields used. |
| src/compiler/native/regalloc.f:2180 ALLOCATE | Same entry transport and repeated routine reconstruction. |
| src/compiler/native-effect.f:959 CHECK-SLOT | Long drop sequence reaches frame size and machine; a named routine can use existing readers. |
| test/compiler/native-chain-fixture.f:69 SELECTED | Fourteen routine locals survive setup only to reconstruct the selector argument. |
| test/compiler/native-tape.f:699 DG-BODY | Four digest locals rebuilt for six comparisons. |
| test/compiler/native-tape.f:721 DGB-BODY | Same reconstruction for six field-sensitivity comparisons. |
| test/compiler/native-tape.f:743 DGE-BODY | Digest rebuilt for empty/nonempty comparisons. |
| test/compiler/native-tape.f:769 VFX-BODY | Digest unpack/repack around constructing the intentionally different tape. |
| test/compiler/ir-schema.f:1252 FZ-BODY | Two four-word digests carried across freezing and reconstructed for comparisons. |
| test/compiler/ir-schema.f:1454 EMBED-BODY | Digest carried across freezing as four words. Alder can include these last two with schema.f after release. |

### Parametric locals: span lane

lib/span.f:95 SKIP, :100 TAKE, :108 SUB are review candidates for clearer named
span/count transport instead of return-stack juggling; their existing comment
deliberately chooses the stack form, so measure generated code before replacing
it. :112 AT, :120 U8!, :126 CELL-AT, :134 CELL!, :138 COPY and :144 FILL already
name whole spans with inferred types: add span<u8>/span<cell> annotations after
50be4d43. Their field extraction computes bounds or addresses and should stay.
Span band 1 owns these edits. Parametric annotations alone need not reduce LOC.

### Shapes examined and retained

- lib/json-write.f:72 JW-LIVE and its writers inspect capacity/length and write
  a changed record; wide locals alone remove neither the checks nor the update.
- lib/object.f:330 NEXT-LINE converts STR:split to OBJ:line while advancing a
  cursor; it is a domain conversion, not unchanged-value transport.
- lib/elf32-test.f, target/policy/NEFF encoders and field readers destructure to
  inspect fields or emit canonical bytes. Do not change wire layouts or add a
  new family of accessors just to shorten them.
- tools/* process-result MATCH arms and lib/process*-test.f unpack captured
  lengths/exit codes to report or assert them. The fields are the computation.
- src/compiler/native/regalloc.f:2053 SLOTS-CK changes the routine's frame size;
  simple unchanged-value substitution would silently lose that update.
- Native wide-memory/MATCH/rename tests and type/structure declaration suites
  intentionally exercise construction, field order, transport or refusal.
  Preserve their subject, rather than mass-converting their fixtures.
- lib/vector.f:331 EACH, native/codewalk.f and tools/codegen-*-inventory.f need
  quotation-effect annotation support; closures and mutable locals are separate
  features. Do not promise those changes from wide/parametric family annotations.
- regfile/machine/backend comments cite old local limitations, but changing a
  flat record or machine identity is a representation change, outside adoption.

### Consumer trees, read-only

| Owner tree and site | Finding |
| --- | --- |
| loom/maki/infer/gpt2-model.f:1254 OPEN | FS:path is unpacked and immediately reconstructed for HF:OPEN-GPT2. Keep a typed path local, project only when M-OPEN-TOK needs bytes. |
| loom/maki/infer/gpt2-model.f:1265 CONTEXT-LEN, :1274 EOS-ID, :1283 LOGITS, :1302 RESET | M-TAKE/M-SAVE carries many fields around a linear model. Review only after the separate linear-local checker work; wide locals do not authorize duplicating the owner. |
| Tender/src/match-context.f:25 MATCH-PRIOR, :28 MATCH-AT, :32 MATCH-PATTERN | Whole context/evidence already use inferred locals. Annotate the domain values, retain field computations for changed contexts. |
| Tender/src/combine.f:988 REJECTED-BINDING?, server/import.f:532 PLATFORM-KEPT? | Whole proof/row already inferred; annotations improve the boundary, field extraction supplies the actual checks. |
| maki/src/placement-optimize.f:110 SAME-POSE?, :186 PROJECT-PAD, :300 POSE@ | Position locals bp/ap/at lack point annotations; annotate them after the feature lands. Coordinate arithmetic and pose decomposition remain useful. |
| radar/lib/ti-elf.f:21 COUNT, :25 ENTRY, :34 CHECK-TARGET, :55 ADD-SEGMENT, :77 NEXT-EXTENT, :106 SEGMENT+ | ELF fields are read for validation, copy and device addresses, not reconstructed unchanged; no wide-local simplification selected. |
| kiba/src/*.f | No layout UNMAKE/reconstruction candidate found. Raw path buffers are a separate span/API migration, not a typed-local workaround. |

Send consumer findings through the owning agents; do not edit those trees here.
Next: migrate the three released compiler files when bc67d207 is integrated,
recheck this inventory against the feature head, then update the language card
and manual with the tested idiom. This dot remains open until migrations land.
