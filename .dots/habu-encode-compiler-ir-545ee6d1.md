---
title: Encode compiler IR canonically
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:55:16.449418+02:00"
closed-at: "2026-08-05T11:57:17.930933+02:00"
close-reason: "Obsolete: the canonical codec stack this dot built was deleted under CG-31 (no product consumer; hard-cut policy) in ad32f68b"
---

Full context: design section 6.6 requires a versioned canonical wire format and digest. Define magic, major/minor version, little-endian widths, ordered tables, counts/lengths, decoder limits, full-input consumption, reference remap, and SHA-256. Acceptance: encode/decode/re-encode is byte-identical; malformed/noncanonical/oversized/trailing data rejects before unsafe allocation; one semantic field changes digest. Dependency: canonical tables and renderer fixtures.

Claim: agent=irencode workspace=.jj-ws/habu-encode-compiler-ir-545ee6d1

## Result (irencode, measured)

**What landed.** `src/compiler/ir/encode.f` opens package IR-ENCODE and owns one
stage: the canonical wire frame. It turns a canonicalized module into bytes and
back-checks presented bytes before anything believes them. Public surface:
`SIZE` (the exact frame length for a canonical table), `ENCODE` (write the frame
into the caller's byte span, answer the length), `DIGEST` (SHA-256 over exactly
one accepted frame), `FRAME-CK` (the framing check on its own), and the frame
readers `FORMAT-MAJOR`, `FORMAT-MINOR`, `TARGET-DIGEST`, `POLICY-DIGEST`,
`SCHEMA-MAJOR`, `SCHEMA-MINOR`, `DIALECT`, `SYMBOLS`, `TYPES`, `ATTRS`,
`SOURCES`, `CELLS` and `CELL@`.

**The authority split, honoured.** IR-CANON owns the canonical ordinals and the
renumbered cell stream; this file owns the magic, the format major and minor
version, the field width, the counts and lengths, the full-input consumption
rule, and the digest. The encoder never reads inside the payload: it copies
`IR-CANON:CELLS` slots through `IR-CANON:CELL@` and knows nothing about how many
sections the stream has or what a row looks like. So a row-shape change is
canon.f's alone and a framing change is this file's alone.

**The frame.** Nineteen header slots then the payload, every field one
eight-byte little-endian slot. The slot is CDIGEST's preimage slot
(`CDIGEST:SLOT!`/`SLOT@`) rather than a second convention declared here, so a
frame field and a digest preimage field can never drift apart. Header: magic
("HBIR"), format major, format minor, the bound target contract digest (four
slots), the bound numeric policy digest (four slots), the dialect's schema major
and minor version, the dialect name's canonical symbol ordinal, the four
canonical row counts, and the payload slot count. `DIGEST` is SHA-256 over the
whole frame and only over a frame that already passed `FRAME-CK`.

**A frame is a value, not a resource.** ENCODE writes into a byte span the caller
already owns and SIZE tells the caller the length beforehand, so the encoder
holds no registry, takes no arena, and consumes none of IR-CANON's eight
canonical-table slots per context. A frame has no identity beyond its bytes and
every real consumer (a cache record, a witness payload, a content key) has to
copy it into its own storage anyway.

**The pairing proof, and why it is first.** ENCODE is handed a module and a
canonical table, and a header built from one module's schema over another
module's stream would be a lie nothing downstream could catch. The header needs
the dialect name's canonical ordinal, and asking the canonical table for the
ordinal of an identity the module minted is IR-CANON's own owner check - so the
one lookup the header already needed proves the module is frozen (the dialect
name is read through the module's frozen schema view) and that the table numbers
that module's rows. It runs before the first byte is written, so a refused encode
never leaves half a frame behind.

**The finding this lane did not expect.** canon.f's header said IR-SCHEMA already
publishes `FTABLE-DIGEST` for the encoder to bind the schema by. It cannot:
`IR-SCHEMA:FROW-DIGEST` folds each schema record's stored operand, result and
attribute-key lists, and those lists hold module-local INSERTION ordinals, so two
equivalent modules built along two admissible intern orders have two different
schema-table digests. The first version of this encoder bound it, and the
digest-equality acceptance failed on exactly that field. It was measured rather
than argued: `test/compiler/ir-encode.f` now carries a fixture that compares the
two schema-table digests of two equivalent modules directly and requires them to
differ, so a later change that puts the schema digest back into the frame turns
red instead of quietly making equivalent modules differ. The frame binds the
dialect's canonical name ordinal and its schema major and minor version instead,
which is exactly what design section 6.6 asks the header to state
("dialect/schema versions"); the schema digest of design line 602 belongs to the
section 6.7 witness header, which binds one pass over one module in one process.
The missing canonical schema digest is dotted as
`habu-canonicalize-the-dialect-2d9aad97`, owned by IR-SCHEMA or by a
canonicalized schema section in canon.f, explicitly NOT recomputed in the
encoder. canon.f's header comment was corrected in place (comment only, no
interface change) so the false claim does not stay in the authority-split header.

**Error codes.** The reserved sub-block **-8140..-8159** is now claimed in
`lib/errors.f` as E-IR-ENCODE-STATE, -VERSION, -FRAME, -CAP, -ROOM and -BOUND
(six of twenty), each with a checked negative fixture. This is the block
lib/errors.f's own region map already reserved for the canonical wire codec and
digest; the dispatch brief said to take the next free block after
-8100..-8119, but -8120..-8139 is the map's reservation for the renderer and diff
stage (the dot this one blocks), so taking it would have forced that lane to
move. The map line was updated to name IR-ENCODE as the owner instead of the
placeholder IR-CODEC, and to say that a decoder takes the rest of the sub-block.

**Doc half inherited.** `docs/compiler-ir-design.md` section 6.6 gains the
paragraphs the canonicalization lane left for this one: the two-stage split and
which stage owns what, why renumbering is load-bearing and a bare permutation is
not enough, why the re-materialization reading was rejected, the eight
canonical tables per context ceiling against the encoder holding no registry, and
the schema-digest finding above. Noted in
`habu-canonicalize-compiler-tables-e0c7f8f1` as well.

**Measured gate results, all in this workspace over parent 01a544cd:**

- `bin/hb --load test/compiler/ir-encode.f` - exit 0, `test: ok`, 22 cases.
  Before the module existed, `require src/compiler/ir/encode.f` failed with exit
  74 and `include: open failed`.
- `bin/hb --load test/compiler/ir-canon.f` - exit 0, `test: ok`.
- `bin/hb --load test/compiler/ir-build.f` - exit 0, `test: ok`.
- `bin/hb --load test/compiler/ir-verify.f` - exit 0, `test: ok`.
- `bin/hb --load tools/error-code-lint.f` - exit 0,
  `1336 file(s), 891 claim(s), 39 reservation(s), 0 finding(s)`.
- `bin/hb --load tools/suite-coverage-lint.f` - exit 0,
  `169 suite(s), 0 finding(s)` (168 before; the new suite is registered in
  `test/gate-stdlib-cases.f` and routed in `test/gate-stdlib-inline-lib.f`).
- `bin/hb --load tools/typed-local-diff-lint.f -- <jj diff --git>` - exit 0, no
  findings. Falsified first: the same command on the same artifact with one extra
  file carrying an untyped locals group reports two `E-UNTYPED-LOCAL` findings
  and exits 1.
- `bin/hb --load tools/package-diff-lint.f -- <same artifact>` - exit 0, no
  findings. Falsified first: the same command on that artifact reports
  `E-PACKAGE-OWNERSHIP` for the global definition and exits 1.
- `bin/hb --load tools/dot-dep-lint.f` - exit 0,
  `1612 dot(s), 1101 blocker(s), 0 finding(s)`, after the two new dots.
- The load path is fail-closed: giving `SIZE` a stack effect its body does not
  satisfy makes `bin/hb --load src/compiler/ir/encode.f` print
  `habu: in size: at 'IR-CANON:CELLS' expected: ir-canon:table<> actual: n` and
  `hook: non-certified definition`.
- `formal/` was not touched, so Rocq was not run. `test/gate-stdlib.f` was not
  run: the orchestrator owns the loaded-host gate.

**Mutation matrix.** Fourteen single-change mutations of `encode.f`, each run
through `test/compiler/ir-encode.f` and restored; all fourteen are red, and the
restored file is green. The harness is a scratchpad script and is not committed.

| # | mutation | class | what goes red |
|---|---|---|---|
| 1 | payload loop bound set to zero, so the renumbered stream is not consumed | renumber consumption | swapping two operations changes the bytes; every payload slot is the stream cell |
| 2 | `SIZE` computes four bytes per field instead of eight | field width | suite aborts on E-IR-ENCODE-FRAME, exit 67 |
| 3 | `SIZE` keeps eight bytes but the frame length check uses four | field width | trailing/short frame cases |
| 4 | numeric policy digest slot filled with the target digest | header binding | two numeric policies give two digests; the header states the bound policy |
| 5 | full-input consumption relaxed from `<>` to `>` | consumption | bytes trailing the payload reject |
| 6 | dialect ordinal replaced by a constant, dropping the pairing proof | pairing | a canonical table of another module rejects; the header names the dialect ordinal |
| 7 | stated-row-count check dropped for the symbol count | decoder limit | more canonical rows than the payload could hold rejects |
| 8 | format major check removed | version | a frame of another format major version rejects |
| 9 | format minor check removed | version | a frame of a later format minor version rejects |
| 10 | magic check removed | framing | bytes whose leading slot is not the magic reject; a digest of non-frame bytes rejects |
| 11 | destination room check removed | framing | a destination shorter than the frame rejects |
| 12 | committed frame ceiling check removed | decoder limit | a payload past the committed frame ceiling rejects |
| 13 | header slot count reduced by one | framing width | the frame is its header plus eight bytes per slot |
| 14 | payload written from the module's insertion-order tables (first version of the file) | renumber consumption | two topological build orders encode and digest identically |

Mutation 14 is the historical one: the first version of this encoder bound the
non-canonical schema-table digest, and the digest-equality fixture caught it. That
is what produced the finding above.

**Best long-term fix, or a patch?** Long-term. Three decisions carry it, and each
rests on a structural invariant rather than a value heuristic. First, the
module-and-table pairing is proved by asking IR-CANON for the ordinal of an
identity the module minted, which is an existence-and-ownership check the header
already needed, not a serial-range or magic-value comparison. Second, the frame is
written into caller-owned bytes, so the stage adds no registry, no ceiling and no
release discipline for something that is a value; the alternative (a fourth
registry with an eighth-slot ceiling of its own) would have been more machinery
for less safety. Third, the schema-digest question was answered by removing a
field that provably cannot be canonical and dotting the missing capability at its
real owner, rather than by keeping the field and weakening the acceptance, or by
recomputing a second schema digest inside the encoder. The one thing a reviewer
should push back on if they disagree is the frame ceiling `CELL-MAX = 32768`
payload slots: it is a committed number, derived from IR-BUILD's production plan
(1920 rows at up to sixteen stated slots each, plus 1408 pool slots), refused by
name, and stated in the file - but it is still a ceiling rather than a
grow-with-the-module policy, the same capacity decision canon.f and IR-VERIFY
already made.

**Honest gaps, for the orchestrator to weigh:**

1. **No decoder.** The dot asks for encode/decode/re-encode byte identity. What
   landed is encode, the framing check, and readers over an accepted frame, so
   "re-encode is byte identical" is proved as determinism (encoding one module
   into two spans gives identical bytes) and as reader agreement (every header
   field and every payload slot reads back as what was encoded). A decoder that
   rebuilds a MODULE from a frame is a genuinely separate stage: it needs an
   IR-BUILD replay that re-interns rows in canonical order and re-mints a module
   identity, which is the re-materialization design canon.f rejected for the
   canonicalizer. It is not dotted yet because its shape depends on whether the
   pass/witness stage wants a module or just a verified stream; the rest of
   -8140..-8159 is reserved for it.
2. **The canonical stream is not modelled in Rocq.** The canonicalization lane
   recorded this as its MODEL GAP 3 and suggested the encoder lane take it. This
   lane did not: the property worth proving (framing is injective on
   header-plus-stream, so equal frames imply equal streams and equal bound
   contracts) needs a model of the stream first, and that is a larger piece of
   work than this leaf. Not dotted; the orchestrator should decide whether it
   belongs to this family or to the proof family.
3. **The test fixture is duplicated.** `test/compiler/ir-canon.f` and
   `test/compiler/ir-encode.f` each build a module along two intern orders with
   their own private fixture words. Extracting a shared checked fixture module
   would edit a landed test of another leaf, so it is dotted as
   `habu-share-one-checked-5fefa5e7` instead of done here.
4. **`SCHEMA-MAJOR`/`SCHEMA-MINOR` in the frame are the dialect's schema version,
   not a hash of its content.** Until dot 2d9aad97 lands, two modules with the
   same dialect name and version but different declared operation schemas frame
   identically whenever their programs coincide. That is section 6.6's stated
   binding and the witness header is where the content binding belongs, but it is
   a real weakening compared with what canon.f's header promised, and it is
   recorded in the design document rather than left implicit.
5. **Two of the fourteen mutations kill by aborting the suite rather than by
   failing one named case** (numbers 2 and 11 land as an uncaught throw, exit 67,
   because a broken frame length breaks the very first fixture). They are still
   kills, but a reviewer wanting one-case-per-mutation resolution should know the
   signal is coarser there.
