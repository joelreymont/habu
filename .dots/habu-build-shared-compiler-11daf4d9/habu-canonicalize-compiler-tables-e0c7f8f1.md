---
title: Canonicalize compiler tables
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:55:16.430122+02:00"
closed-at: "2026-08-05T11:57:17.922963+02:00"
close-reason: "Obsolete: the canonical codec stack this dot built was deleted under CG-31 (no product consumer; hard-cut policy) in ad32f68b"
blocks:
  - habu-verify-frozen-compiler-224d78ad
---

Full context: design sections 5.7 and 6.6 require construction-order-independent bytes without reordering semantic control flow. Sort strings first, then dependency-order symbols, types, attributes, and sources; rewrite every reference while preserving function/block/op/operand/result/successor order. Acceptance: equivalent modules with reversed intern insertion encode/digest identically; semantic order changes remain observable. Dependency: frozen verifier.

Evidence 2026-07-28 (from formal/Common/Interning.v): the "rewrite every
reference" clause above is not optional bookkeeping — it is load-bearing, and
there is now a machine-checked counterexample proving a bare permutation is
wrong. A pointer row stores its pointee's module-local ORDINAL
(src/compiler/ir/type.f POINTER: `r ptee ID-CK` then INTERN4 stores it), and a
function-type row stores a pool window of ordinals, so reordering the type
table changes stored row content. Theorem Types.ty_both_orders_admissible
builds i8, i16 and pointer-to-i8 in the two admissible orders and gets rows
[i8; i16; TPtr 0 0] versus [i16; i8; TPtr 0 1]; Types.structural_rows_not_permutation
proves those are NOT a permutation of each other, while
Types.ty_denotation_order_independent shows the denotations do agree — which is
the property this encoder must target. IR-ATTR is worse: attr.f ORD-OK admits
foreign-table ordinals (symbol and type ids) into attribute rows, so the
encoder must renumber under the attribute permutation AND under the symbol and
type permutations.

A second unstated premise this encoder depends on: not every insertion order is
admissible. POINTER rejects a pointee ordinal not already below the live count,
so the admissible build orders are exactly the topological orders of the
reference DAG. "Any two orders" means "any two topological orders" for the type
and attribute tables.


Claim: agent=ircanon workspace=.jj-ws/habu-canonicalize-compiler-tables-e0c7f8f1

## Result (ircanon, measured)

**What landed.** `src/compiler/ir/canon.f` opens package IR-CANON and owns one
stage: given a frozen module, it decides the canonical order of the four interned
tables and rewrites every stored reference under that order. Its product is a
canonical table - one owned store holding the canonical ordinal of every interned
row, followed by a cell stream that is design section 6.6's table order with
every reference already renumbered. Public surface: `CANON`, `RELEASE`, `LIVE?`,
`SYMBOL-ORD`, `TYPE-ORD`, `ATTR-ORD`, `SOURCE-ORD`, the four canonical counts,
and `CELLS`/`CELL@` over the stream.

**The authority split, and why.** Section 6.6 names five words (ENCODE, DECODE,
DIGEST, RENDER, DIFF) and no canonicalization word, so the split between this
stage and the encoder is a decision. It was made so that order and renumbering
have exactly one owner (this file) and framing, widths, versions and the
SHA-256 have exactly one owner (the encoder). The alternative reading -
canonicalization re-materialises a second module in canonical order - was
rejected for a structural reason: `IR-BUILD:NEW-BUILDER` interns the dialect's
own name before any caller can intern anything, so a re-materialised module's
symbol table always begins with the dialect name and can never be in the sorted
order this stage must produce. It would also mint a second module identity,
re-run the freeze verifier over content already verified, and cost seventeen more
arena registry slots per module. The reading is documented at the top of
`canon.f` with the section 6.6 quote it rests on; the design document itself was
NOT edited, because it is the upstream spec and section 6.6 does not name this
stage. If the orchestrator wants section 6.6 to name the split, that is a
one-paragraph follow-up the encoder lane can inherit.

**One selection rule for all four tables.** Walk the rows repeatedly and take
the row whose canonical key is smallest among the rows all of whose referents
are already numbered. A row's canonical key is its content with every reference
replaced by the referent's canonical ordinal, so by induction the whole
assignment depends only on the denotations. Symbols have no references, so their
key is their bytes and the rule reduces to "sorted by bytes", which is what
`symbol.f` already said canonicalization needs from it. Tables are ordered
symbols, then types, then attributes, then sources, because that is the order in
which one table's keys can mention another's ordinals.

**Equal keys share one canonical ordinal, and that is one rule, not a special
case.** Symbols, types and attributes are interned, so distinct rows have
distinct keys and the canonical map is a bijection. The source registry
deliberately does not deduplicate, so a module can hold two sources with the
same length, digest and origin; those rows are indistinguishable through every
public reader, and a module pointing at the first copy and a module pointing at
the second are the same module. The selection therefore gives rows with equal
keys one canonical ordinal, which makes the canonical source table
content-addressed and can make it shorter than the registry. For the interned
tables the rule never fires.

**What is not reordered.** Functions, blocks, operations, values, operands,
results, successors and a function's attribute list keep their order exactly:
that order is the program. Their rows are still rewritten, because they name
symbols, types, attributes and sources. An operation's keyed attribute entries
ARE re-sorted, by canonical key, because design line 479 makes an operation's
attributes keyed and their entry order therefore no part of the operation - and
so are a record attribute's pairs, which IR-ATTR stores sorted by the key's
INSERTION ordinal.

**What the stream leaves out, and why.** Pool window starts, the symbol
interner's scan filter and arena capacities are storage, not content (section
6.6: the encoding "does not serialize host addresses or arena capacities"), and
a window is recovered from the explicit counts because the pools are emitted in
row order. A block's terminator and a function's first block are not stated,
because the freeze verifier proves them equal to the block's last operation and
to the previous function's window end. The dialect schema table is not a section:
section 6.6 serializes "dialect/schema versions", and IR-SCHEMA already publishes
`FTABLE-DIGEST` for the encoder to bind it by. The derived block-edge table is
section 6.6's "optional derived indices" and is re-derivable from the successor
pool.

**Wire codes.** The stream spells every closed vocabulary as an integer through
one exhaustive MATCH per family, mirroring the owning table's storage code value
for value - the arrangement `attr.f` already keeps for the CNUM and CTARGET
families. Each table's storage codes stay private to the table; the stream's
meaning is fixed here and versioned by the encoder's format version. Because
every mapping is exhaustive, a new member of any family fails to compile here
rather than serializing as some other member.

**Error codes.** The reserved block -8100..-8119 is now claimed in `lib/errors.f`
as E-IR-CANON-STATE, -OWNER, -BOUND, -CAP, -ORDER, -STALE, -RELEASED, -SLOTS and
-SERIALS (nine of twenty). Six have a checked negative fixture. Three do not, and
each is recorded rather than hidden: -STATE is the header and module-binding
recheck of this package's own store, which no checked caller can reach because the
store handle never leaves the package; -ORDER is the refusal when a round finds no
ready row, which construction cannot produce because references strictly decrease
and a forged table can; -SERIALS needs $7FFFFFFF canonicalizations. A tenth code
for "not a frozen module" was not minted: `IR-BUILD:FROZEN?` answers false for a
module whose context has torn down, and E-IR-CANON-STALE already names exactly
that, so a separate code would have had no distinct meaning.

**Measured gate results, all on this workspace over parent a3d3651b:**

- `bin/hb --load test/compiler/ir-canon.f` - exit 0, `test: ok`, 12 cases.
  Before the module existed the same path failed with exit 74,
  `include: open failed`.
- `bin/hb --load test/compiler/ir-build.f` - exit 0, `test: ok`.
- `bin/hb --load test/compiler/ir-verify.f` - exit 0, `test: ok`.
- `bin/hb --load test/compiler/ir-op.f` - exit 0, `test: ok`.
- `bin/hb --load tools/error-code-lint.f` - exit 0,
  `1329 file(s), 885 claim(s), 39 reservation(s), 0 finding(s)`.
- `bin/hb --load tools/suite-coverage-lint.f` - exit 0, `168 suite(s), 0
  finding(s)` (165 before; the new suite is registered in
  `test/gate-stdlib-cases.f` and routed in `test/gate-stdlib-inline-lib.f`).
- `bin/hb --load tools/package-diff-lint.f -- <jj diff --git>` - exit 0, no
  findings. Falsified first: the same command on a patch adding a global
  `LRD-THING` throws -7400.
- `bin/hb --load tools/typed-local-diff-lint.f -- <same artifact>` - exit 0, no
  findings. Falsified first: the same command on a patch adding an untyped
  locals group throws -7400.
- `make -C formal` - clean, every `Print Assumptions` reports `Closed under the
  global context`, no `Admitted`.
- `bin/hb --load test/compiler/ir-intern-proof.f` - exit 0, `test: ok`, with the
  new theorem bound by `test/compiler/ir-intern-axioms.txt`. Falsified: turning
  the manifest's `<>` into `=` makes that gate red in three assertions.
- `test/gate-stdlib.f` was NOT run: the orchestrator owns the loaded-host gate.

**Mutation matrix.** Fifteen single-line mutations of `canon.f`, each run through
`test/compiler/ir-canon.f` and restored. All fifteen are red; the harness is a
scratchpad script and is not committed.

| # | mutation | case that goes red |
|---|---|---|
| 1 | pointer pointee left unrewritten | two build orders agree |
| 2 | function-type elements left unrewritten | two build orders agree |
| 3 | attribute symbol reference left unrewritten | two build orders agree |
| 4 | attribute type reference left unrewritten | two build orders agree |
| 5 | record pairs not re-sorted under the symbol order | two build orders agree |
| 6 | operation attribute entries not re-sorted | two build orders agree |
| 7 | span source left unrewritten | two build orders agree |
| 8 | function name symbol left unrewritten | two build orders agree |
| 9 | function signature type left unrewritten | two build orders agree |
| 10 | opcode symbol left unrewritten | two build orders agree |
| 11 | value type left unrewritten | two build orders agree |
| 12 | function attribute list left unrewritten | two build orders agree |
| 13 | equal source content no longer shares one ordinal | duplicate sources merge |
| 14 | symbols ordered by insertion instead of by bytes | symbol order pin |
| 15 | types ordered by insertion instead of by canonical key | type order pin |

The first run of that matrix left EIGHT of the fifteen green, which is the
finding worth keeping: the fixture had reversed insertion orders without moving
the ordinals the code has to rewrite - the function name was interned at the same
point in both builds, the pointer sat at the fixed point of a five-item
reversal, and a source registered per span gave both builds the same row. The
fixture now walks each reversed group as an even-length list forwards or
backwards so no member keeps its ordinal, adds the keyed-attribute order to what
the flag reverses, and asserts that four of those insertion ordinals really
differ between the two builds before it compares the streams. That assertion is
what stops the fixture from passing for the wrong reason again.

**Rocq.** `formal/Common/Interning.v` gains the minimal honest model of this
stage inside `Module Types`: `canon_key` (a row's content with references in
canonical numbering, whose `None` case IS the readiness test), `stored_key` (the
same with the renumbering removed and nothing else changed), `ty_ltb`, `pick`,
`canon_rounds` and `canonize`. Three results:
`ty_rows_are_the_built_tables` binds the row lists to what the two builds
actually produce, `ty_canonize_orders_agree` shows both admissible orders
canonicalise to one row list, references and all, and
`ty_canonize_preserves_denotation` shows the canonical rows denote what the
source rows denoted. The published theorem is
`Types.ty_canonize_needs_renumbering`: `canonize_stored rows_a <>
canonize_stored rows_b`, the machine-checked form of mutation 1 - keep the
canonical order, drop the renumbering, and the same module along two admissible
orders no longer has one canonical form.

**MODEL GAPS** (recorded as MODEL GAP 12 in `Interning.v` as well):

1. Agreement is proved for the counterexample INSTANCE, not for arbitrary
   topological orders of arbitrary reference graphs. The general statement is
   unproved, which is why `test/compiler/ir-canon.f` measures whole modules
   through the shipped IR-CANON.
2. The Rocq model covers the type table only. The attribute table's renumbering
   under three permutations at once and the source table's content merge (where
   the canonical map is deliberately not injective) are modelled only by the
   Habu fixtures.
3. The canonical cell stream itself is not modelled. That is IR-CANON's product
   and the encoder's input; modelling it belongs with the encoder, not here.
4. The selection is quadratic in the rows of one table and the symbol comparison
   re-reads both names, which is what the committed ceilings bound (256 symbols,
   128 types, 128 attributes, 64 sources, 256 bytes per name or string value, 64
   keyed entries per record or operation). A module planned larger is refused
   with E-IR-CANON-CAP before anything is ordered or allocated. Growing the
   working set with the module instead of committing to a ceiling is a capacity
   decision, exactly as it was for the verifier's dominator sets.

**Substantive work this lane did NOT do, for the orchestrator to dot:**

1. **Nobody can prove the canonical table's context owns the module.** `CANON`
   takes a context and allocates its store from it, and reads the module through
   the views IR-BUILD publishes. A module built in a different LIVE context would
   be canonicalized anyway, because no public reader exposes a view's owning
   context, so there is nothing to compare. It is a nesting mistake with no named
   refusal. The structural fix is a public owner projection on IR-ARENA views (or
   on IR-BUILD's frozen module), and then an E-IR-CANON-OWNER arm in `CANON`.
2. **A released canonical table's registry slot is reusable, an unreleased one is
   not.** `RELEASE` retires the store and frees its arena slot, but a caller that
   never releases holds one of eight registry slots and one of IR-ARENA's
   sixty-four arena slots until the context tears down. Eight canonical tables
   per context is a committed ceiling with a named refusal (E-IR-CANON-SLOTS),
   not a leak, but the encoder lane should know the number.
3. **The canonical stream has no self-describing framing, on purpose.** A reader
   must know the grammar to walk it; a corrupted store is caught by the header
   recheck and by the per-section counts, not by section tags. If the encoder
   wants tags, they belong to the wire format it owns.

## Doc half discharged by the encoder lane (irencode, 2026-07-30)

The follow-up this report offered - "If the orchestrator wants section 6.6 to
name the split, that is a one-paragraph follow-up the encoder lane can inherit" -
is done, in `docs/compiler-ir-design.md` section 6.6. It names the two stages and
what each owns, says why renumbering is load-bearing and a bare permutation is
not enough (citing `formal/Common/Interning.v`), records why the
re-materialization reading was rejected, states the ceiling of eight canonical
tables per live context against the encoder holding no registry of its own, and
corrects one claim of this lane's report.

The correction: this report's "What the stream leaves out" said IR-SCHEMA already
publishes `FTABLE-DIGEST` for the encoder to bind the schema by. The encoder lane
measured that it cannot. `IR-SCHEMA:FROW-DIGEST` folds each schema record's stored
operand, result and attribute-key lists, and those hold module-local INSERTION
ordinals, so two equivalent modules built along two admissible intern orders have
two different schema-table digests; binding it made the digest-equality acceptance
fail on exactly that field. The canonical frame binds the dialect's canonical name
ordinal and its schema major and minor version instead, which is what section 6.6
asks the header to state, and `test/compiler/ir-encode.f` carries a negative
fixture that pins the non-canonical behaviour so nobody re-adds it silently. The
canon.f header comment was corrected in place (comment only). The missing
capability is dotted as `habu-canonicalize-the-dialect-2d9aad97`.

MODEL GAP 3 of this report (the canonical cell stream is not modelled in Rocq,
and modelling it belongs with the encoder) is still open. The encoder lane did not
take it; see `habu-encode-compiler-ir-545ee6d1` gap 2.
