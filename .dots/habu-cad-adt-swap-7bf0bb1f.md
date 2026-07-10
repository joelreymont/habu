---
title: "CAD: ADT swap for report/IR/schedule internals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.079838+02:00"
blocks:
  - habu-checker-capability-typed-a480c423
  - habu-checker-capability-derive-23788e95
  - habu-checker-capability-layout-4e7f1f03
---

docs/model-cad.md typed backbone. When TFAM 9/10/12/14/15 land: swap cad-0a report, cad-1 IR, cad-4 schedule internals to sum/enum/product families with MATCH dispatch (op-kind enum, verdict sum pass|fail<reason>, fusion sum fused|split<reason>, report rows as products, option/result for lookups). Representation-hiding accessor signatures must not change; tests prove behavior identical. Recursive by-value IR waits for TFAM 16 boxed (habu-epic-adopt-adts child). Depends: TFAM campaign on maki-type-families.

UPDATE 2026-07-04 (user review of cache-key asserts): the section-7.4 schedule
key is the sharpest instance of the gap. Today maki/sched-key.f SK-KEY$ renders
eight semantically distinct fields (region-sig, shape-class, dtype, layout,
align-class, target, engine-key, ptxas) straight into the shared SB builder as a
pipe-delimited string; the replay table and the store key by that string, and
tests must assert rendered text. Stringly fields are a semantic-role hole: dtype
and layout are indistinguishable bytes, so a field swap is silent. Required
shape when TFAM 14 (ENUM) + 15 (PRODUCT) land: SKEY as a product record with
enum fields (DT-*, LAY-*, AL-* constants already exist to become real enums),
typed constructor + field accessors, typed equality for the replay table, ONE
render word at the durable-store boundary (schedules.rows stays line-oriented
text - the on-disk format is a contract with exactly one format regression
test), and key tests move from string asserts to field asserts. Same treatment
for the report gate tags (verdict sum) and evidence rows (product).

UPDATE 2026-07-07 (TFAM 1-8 landed on maki-type-families; real syntax pinned).
Studied the landed authoring surface (src/core/sumtype.f, type-family.f,
type-schema.f; test/type-ctor-suite.f, type-decl-suite.f). What this lane can
lean on vs what is still blocked:

REAL LANDED SYNTAX (verbatim, executable today via `bin/hb`):
- `TYPEFAMILY span 3` — one-cell parametric family, usable in sigs as `span<a,b,c>`.
- `SUMTYPE result 2  VARIANT ok a ;VARIANT  VARIANT err b ;VARIANT  ;SUMTYPE`
  — sum family; payloads are positional letter params (a..z within arity),
  concrete cell types (`n f r u8 ...` via CON-OF), and `ptr T`. Family
  applications / quotations / atoms as payloads reject (E-TDECL-PAYLOAD).
- Generated constructors (TFAM 8): a PUBLIC arity-0 sum generates one checked
  word per variant in a derived package, e.g. `SUMTYPE zres 0  VARIANT ok n
  ;VARIANT  VARIANT err n ;VARIANT ;SUMTYPE` yields `ZRES:OK ( n -- zres )` and
  `ZRES:ERR ( n -- zres )`. Package name = UPPER(pkg-tail '-' family-tail),
  hyphens escaped `-`->`--`, hash-suffixed when too long. In a `package zpub`
  block the tail `tres` derives `ZPUB-TRES:YES`. No TRUST/set-check anywhere.
- Enum shape TODAY is a zero-payload sum: `SUMTYPE zen 0  VARIANT lit ;VARIANT
  VARIANT dark ;VARIANT ;SUMTYPE` -> `ZEN:LIT ( -- zen )`, `ZEN:DARK ( -- zen )`.
- Parametric public sums also publish (TFAM 11 slice 1) but stay one logical
  cell until instantiation proves the args non-linear; genuinely-linear
  payloads reject. Private families export nothing (metadata only) until 9.

STILL BLOCKED (this lane's real dependencies) with authoritative target syntax
from docs/type-families.md §9/§13:
- MATCH dispatch — TFAM 9 (NOT landed). This is the eliminator that replaces
  every `case ... of ... endof ... endcase`:
      : RESULT>CODE ( result<ptr u8,n> -- n )
        MATCH result  ok OF drop 0 ENDOF  err OF \ n on stack ENDOF  ;MATCH ;
  Exhaustive, no default branch; checker knows each branch payload type.
- ENUM block form — TFAM 14 (NOT landed): `ENUM color red green blue ;ENUM`
  (== zero-payload sum). Retires the numeric ENUM/ENUM4 chain first.
- PRODUCT + FIELD accessors — TFAM 15 (NOT landed):
      PRODUCT pair 2  FIELD fst a  FIELD snd b  ;PRODUCT
  Registry substrate (TK-PRODUCT kind, PF-* product-field rows) is already
  present, but there is NO PRODUCT/FIELD grammar or typed field accessors yet,
  so the SKEY record cannot be authored pre-15. NB: VALUE-RECORD is landed but
  its fields are all `n` (no enum typing), so it does NOT close the dtype/layout
  semantic-role hole — it is not a valid interim for SKEY.
- Native + Gforth lowering / bad-tag die — TFAM 10. Layout-aware dup/drop/swap
  over multi-cell product/sum values — TFAM 12. Recursive IR by value — TFAM 16
  (deferred; typed ptr + arena until then, per habu-epic-adopt-adts).

PRE-UNLOCK (executable now, honestly): essentially none of the swap itself.
Enum-shaped SUMTYPE constructors (DT-*, LAY-*, AL-* as zero-payload sums) could
be *declared* today, but nothing can dispatch/read them without MATCH (9), and
the invariant "accessor signatures unchanged, behavior identical" forbids
half-swapping a consumer whose dispatch is a `case`. The only real prep is
scaffolding that does not touch runtime behavior: (a) the schedules.rows
on-disk format-regression test (one golden line, asserts the durable contract
survives the later swap), and (b) the negative/positive checked fixtures the
swap will need (field-assert versions of the SK-KEY tests, verdict/enum
round-trip fixtures) staged as skipped/pending. Do that prep now; hold the swap
for 9/10/12/14/15.

FILE-BY-FILE EXECUTION PLAN (fable side, all under maki/):
1. maki/tensor.f (DT-F32..DT-I32, DT-N) + maki/tensor-value.f (LAY-ROW/COL/N,
   AL-UNKNOWN..AL-16, AL-N): migrate the integer-constant enums to `ENUM dtype
   ... ;ENUM` / `ENUM layout ... ;ENUM` / `ENUM align ... ;ENUM` when TFAM 14
   lands. Keep the range-bound `*-N` sentinels retired by exhaustiveness.
2. maki/report.f: `V-PASS/V-FAIL/V-NOTRUN` -> `SUMTYPE verdict` (payload-free,
   or arity-1 `fail<reason>` per model-cad.md); `RC-*`, `CO-*` -> enums; the
   `case/of/endof` renders (VER$ etc.) -> MATCH. `report` stays a DEFTYPE
   opaque handle so no accessor signature changes; internals swap only.
3. maki/sched-key.f: SKEY as a `PRODUCT` record with enum fields (dtype, layout,
   align) + hash/target/engine/ptxas leaves; typed constructor + field
   accessors; typed equality for the replay table (replaces STR= over the
   rendered key); ONE render word (SK-KEY$) kept only at the durable-store
   boundary. AL-KEY `case` -> MATCH over the align enum.
4. maki/store.f: evidence rows -> product; STORE-V$/PF-NAME stay this file's
   on-disk encoding (wire contract), driven by MATCH over the verdict/verdict
   enums, with the format-regression test pinning schedules.rows/evidence.rows.
5. maki/model-ir.f: MIR-OP op-kind -> ENUM (14); IR node -> product-of-indices
   (15); recursive by-value waits on 16.
Gate each increment with the maki suite; add a checked field-assert test per
swapped word; keep the one on-disk format test green throughout.

FOO/;FOO CONFORMANCE: this lane introduces NO new scope words. It consumes
SUMTYPE/;SUMTYPE, VARIANT/;VARIANT, ENUM/;ENUM, PRODUCT/;PRODUCT, MATCH/;MATCH
— all already FOO/;FOO-conformant as landed/specified. OF/ENDOF inside MATCH are
legacy branch tokens (not scope pairs), exempt; FIELD is a declarator inside
PRODUCT. No renames owed here.

COLLISION MAP: this lane touches only fable-side maki/*.f (report, sched-key,
store, tensor, tensor-value, model-ir + tests) as a *user* of the authoring
words. It does NOT touch src/core/*, verify-source.f, checker.f, or any file the
TFAM campaign owns. Clean vs the campaign; its only gate is TFAM 9/10/12/14/15
landing the capabilities above (no shared files, so no serialization needed once
they land). Depends: TFAM campaign on maki-type-families (9/10/12/14/15).

UPDATE 2026-07-10 (fable-adt lane, audit-first commit). TFAM 9/10/12/14/15
have LANDED and are frozen for this lane (merge b0556262): MATCH executes,
ENUM/PRODUCT declare and generate constructors/MAKE/UNMAKE, construct works.
So the 2026-07-07 "STILL BLOCKED" list is closed for the AUTHORING surface.
Audited each swap target against the landed surface with a capability probe
(scratchpad cad-probe.f, method = CHECK-QUIET-CANDIDATE! candidates +
INCLUDE-EVALUATE decl catch, the type-decl/ctor/match-suite pattern). The swap
is NOT executable as specified, because the landed layout machinery (item 7/12,
docs/type-families.md §17) makes a sum/enum/product value a STACK-ONLY logical
bundle that fails closed on every memory/compare/store touch. Proven walls
(probe verdicts verbatim; also pinned upstream in test/type-decl-suite.f):

  WALL-1 STORAGE. A layout value cannot be `!`/`c!` into a cell, held in a
    `variable`, `create`d array, or `constant`. Probe P1 `( pdt ptr a -- ) !`
    -> reject(0); P2 `( pdt -- ) constant` -> reject(0). Upstream pins:
    TD12-STORE, TD12-CONST, TD12-DEPTH. This is the decisive wall: EVERY CAD
    representation the dot targets stores its tag as an integer in a
    memory-backed record/array and returns it through an n-typed accessor —
    report G-TAG/G-RO/G-RL (report.f:120-122, `tag gid cells G-TAG + !`
    :486), IR MI-OP/MI-DT/MI-LAY/MI-IS-AL (model-ir.f:66-86, `dt k cells
    MI-DT + !` :200), fusion FP-SP-REASON (fusion-plan.f:60, :247). None can
    hold an ADT value. Storing one needs the packed-memory layout policy
    (§22.2) / typed ADT buffer store-load — NOT landed.
  WALL-2 EQUALITY. No `=`/compare on layout values. Probe P3 `( pdt pdt --
    bool ) =` -> reject(0). Derived eq/order/hash is an explicit v1 non-goal
    (§27). So "typed equality for the replay table" over stored SKEY values is
    not expressible; the replay table cannot key on ADT-value identity.
  WALL-3 NESTED LAYOUT FIELDS. A PRODUCT/SUMTYPE field/payload typed as
    another layout family rejects at declaration. Probe P6 `PRODUCT prec 0
    FIELD d pdt FIELD l play ;PRODUCT` -> throws (E-TDECL-PAYLOAD). §18 keeps
    v1 params cell-kinded. So "SKEY as a PRODUCT with enum fields" cannot be
    authored — a product cannot hold dtype/layout/align enum fields.

Proven CAPABILITIES (what v1 CAN do, transient stack only):
  CAP-A n->enum via if/else join certifies (P4 `0 = if PDT:DF32 else PDT:DF16
    then` -> -1); enum->n via MATCH executes (P7 round-trips 1). CASE-join of
    layout branch outputs did NOT certify in the naive form (P4b -> 0); use
    if/else.
  CAP-B positional enum typing catches a dtype/layout SWAP at a transient
    assembly boundary: P5swap `( play pdt -- ) {: d:pdt l:play :}` -> reject(0)
    (binding a `play` into a `pdt` local rejects). This is the ONE piece of the
    dot's priority-1 acceptance ("a swapped dtype/layout field must become a
    CHECKER diagnostic") that is reachable today.
  CAP-C all-cell / ptr-u8 products MAKE/UNMAKE + user field accessors work
    (P8 -> 4, P9 accepted). Products are fine — as long as no field is a layout
    family (WALL-3) and the product value is never stored/compared (WALL-1/2).

PER-PRIORITY RECLASSIFICATION (all four scoped swaps are storage-backed):
  (1) SKEY product w/ enum fields + typed equality: BLOCKED. Needs WALL-1
      (typed ADT store, habu-checker-capability-typed-a480c423), WALL-2
      (habu-checker-capability-derive-23788e95), WALL-3
      (habu-checker-capability-layout-4e7f1f03). A PARTIAL transient win is
      possible — see FORK below.
  (2) report gate tags -> verdict sum w/ MATCH: BLOCKED by WALL-1 (verdict tag
      stored in G-TAG; `fail<reason>` carries a 2-cell string payload, width 3,
      un-storable). The render dispatchers (V-NAME/RC-NAME/CO-NAME, case->text)
      take a STORED n; MATCH would need an n->sum step first (a case) — strictly
      more code, no win, still un-storable.
  (3) fusion fused|split<reason>: BLOCKED by WALL-1 (split reasons stored in
      FP-SP-REASON; FP-FUSED? is a bool over a variable).
  (4) op-kind enum for the IR "where accessor signatures stay stable": BLOCKED
      by WALL-1. op-kind is stored in MI-OP; MIR-OP@ returns n and every
      consumer (OPR-CLASS/OPR-NAME/fusion/lowering) takes n — there is NO
      stable-signature slot to place an enum without WALL-1.
  Recursive by-value IR stays out of scope (TFAM 16 boxed), as before.

This matches the pre-existing project position: habu-checker-capability-typed
already states "Until it lands, tables stay parallel-column records per the cad
staging rule." The audit confirms it with proof and extends it to all four
swaps.

DESIGN FORK (priority 1 residue — orchestrator decides; lane STOPPED here):
  Option A (land now): a transient typed key-field DSL. Add PARALLEL ENUM
    families dtype/layout/align (alongside, NOT replacing, the DT-*/LAY-*/AL-*
    int constants, which stay as the stored/wire encoding), typed field
    accessors DTYPE-OF/LAYOUT-OF/ALIGN-OF ( node -- enum ) that read the stored
    n and convert (if/else, CAP-A), MATCH-based field renderers, and a typed
    SK-KEY assembler consuming dtype/layout/align positionally so an
    assembly-order swap is a checker diagnostic (CAP-B) + a negative fixture.
    SK-KEY$ stays the single durable render (byte-identical; sched-key-test.f:53
    already pins the format). Replay table stays string-keyed (WALL-1/2).
    COST: duplicates the representation (two vocabularies), adds an
    n->enum->string double dispatch on the key path, is NOT a product, has NO
    typed equality, does NOT replace the constants, and closes only the
    assembly-order swap (not a mis-read field, e.g. `MIR-LAY@ N>DTYPE`). It
    would need rework when WALL-1 lands the real swap.
  Option B (hold — RECOMMENDED): keep sched-key as-is; land nothing that
    duplicates the representation. The dot's headline shape (PRODUCT record +
    enums replacing the constants + typed equality + durable typed key) is
    provably unbuildable on the frozen surface; per "dot it rather than force
    it", gate the whole dot on the three capability dots above and revisit when
    they land, at which point priorities 1-4 all become buildable as specified
    with stable accessor signatures. The walls and the durable format are
    already regression-pinned (test/type-decl-suite.f; sched-key-test.f), so
    holding loses no coverage.

NOTE for the eventual dtype/layout enum authoring: single-letter and builtin
type spellings are reserved enum-variant tails — `ENUM pdt a ...` and `ENUM pdt
f32 ...` both reject ("reserved name"). The dtype enum will need non-reserved
tails (spelled-out or prefixed) rendered to the wire strings "f32"/"f16"/... by
its MATCH renderer.

No maki/*.f changed in this commit: the audit is the deliverable. Dot stays
open, now BLOCKED-on the three capability dots (front-matter `blocks:`).

UPDATE 2026-07-10 (fable-swap lane, ENUM-COLUMN slice — capability S1 landed).
WALL-1 storage is now PARTIALLY down: habu-checker-capability-typed S1 landed the
enum-tier (width-1) typed store/fetch through a `ptr family` address, so the
priority 2-4 enum tag columns (NOT the SKEY product, still blocked on WALL-2/3)
are now buildable with STABLE array shapes. Authoring pattern proven on this
fixpoint (scratchpad probes + the landed test/type-decl-suite.f TDS1-*):
  - Declare the ENUM PUBLIC inside `package MAKI` (a PRIVATE family skips
    constructor generation). Constructors derive into package `MAKI-<TAIL>`,
    e.g. `MAKI-REASON:MATMUL`. The ENUM block reads RAW variant tokens — inline
    `\` comments inside the block are a parse error ("name must be a lowercase
    family tail at '\'"), so per-variant notes go in the header comment.
  - The stored column keeps its `create ... cells allot` shape (enum W=1 = 1
    cell). Add a typed slot accessor `: NAME-AT ( n -- ptr fam ) cells ARR + ;`
    — the declared `ptr fam` output binds the body's `ptr a` via the S1
    pointee-bind relax. Store/fetch go `val i NAME-AT !` / `i NAME-AT @`.
  - ENUM-TYPED LOCALS REJECT on this fixpoint: even the identity
    `( fam -- fam ) {: t:fam :} t` is reject(0) (binding a width-1 layout value
    into a local is unsupported — the a480c423 CAP-B "swap reject" was actually
    this, mis-attributed to family mismatch). So a store word must keep the enum
    on the STACK: take it on TOP and store it FIRST, before binding any n local.
  - Readers dispatch with an exhaustive `MATCH fam ... ;MATCH` (uppercase
    OF/ENDOF), which RETIRES the range-`*-N` sentinel and the bad-tag runtime
    throw — an out-of-family tag is now a checker reject.

COLUMN 1 LANDED 2026-07-10 — fusion FP-SP-REASON (fusion-plan.f). Fewest readers
(self-contained: fusion-plan.f + fusion-plan-test.f only; no other maki file reads
it). `ENUM reason multi-use matmul layout barrier backend ;ENUM` replaced the
SR-* int constants (SR-N retired). FP-SP-REASON-AT is the typed slot; FP-SPLIT+
restructured to a stack-only store (no local for the reason); FP-REASON returns
`reason`; FP-SPLIT-REASON@ returns `reason`; FP-REASON-NAME is the ONE render
boundary (case->MATCH, exhaustive, E-FP-IDX default dropped). Readers converted: 1
internal render (FP-REASON-NAME), 1 internal row builder (FP-SPLIT-ROW$, unchanged
— composes over the new types), 6 test assertions (now assert via FP-REASON-NAME).
Boundary kept numeric: NONE (reason has no wire/durable form). Swapped-role
negatives added (fusion-plan-test.f): n->reason store, reason->n fetch, and
reason-through-bare-`ptr a` all reject with cited diagnostics. Gates green: focused
fusion-plan-test.f rc=0; maki/test.f rc=0 (77 suites); namespace/error-code/dot-dep/
host lints 0 findings; typed-local-diff-lint rc=0.

COLUMN 2 LANDED 2026-07-10 — report F-ROOFLINE (report.f). Fewest readers among
the report columns (internal: NEW init, ROOFLINE!/@, R-ROOFLINE, RENDER-HUMAN;
external: tests only, all through the unchanged n accessors). Cross-package
resolution PROVEN by probe: a PUBLIC ENUM declared in `package MAKI` types
signatures, MATCHes (signature scope), and constructs via the generated
`MAKI-ROOFLINE:*` words from inside `package REPORT`. Design differs from column
1 because the accessor signatures ARE the boundary: RC-* constants stay the
public numeric vocabulary of ROOFLINE!/ROOFLINE@ (representation-hiding contract,
signatures unchanged); the STORED value is the enum. `ENUM roofline unknown
memory compute ;ENUM` in the MAKI substrate block; F-ROOFLINE-AT is the typed
`ptr roofline` slot; >ROOFLINE (validate-first RC-CK, then if/else n->enum) is
the ONE parse boundary; ROOFLINE>N (MATCH) is the ONE render-to-code boundary;
RC-NAME becomes ( roofline -- text ) exhaustive MATCH (E-RPT-ROOF render default
dropped — unrepresentable). ROOFLINE! keeps E-RPT-ROOF for out-of-range n (via
RC-CK inside >ROOFLINE; BAD-ROOF test unchanged). MATCH sites: 2 (ROOFLINE>N,
RC-NAME). Readers converted: R-ROOFLINE + RENDER-HUMAN render via enum; ROOFLINE@
converts at the boundary. Swapped-role negatives (report-test.f, reopened REPORT
block): n->roofline store, roofline->n fetch, and FOREIGN-FAMILY store (test-owned
private `ENUM rtalien` — private families DO resolve in candidate signatures
inside their open package; positive identity control pins it) — all reject with
cited diagnostics. Gates green: report-test rc=0, cad-test rc=0, maki/test.f rc=0
(77 suites), all 5 lints clean.
HARNESS NOTE: `bin/hb file.f` script mode drops into a stdin REPL after the file;
under a non-EOF stdin (this agent harness with `timeout`) it hangs. Always run
probes/tests with `< /dev/null` (or stdin-feed the file). `bye` is NOT defined.
The earlier probe "hangs" were exactly this, not checker loops.

COLUMN 3 LANDED 2026-07-10 — report G-TAG verdict (report.f), the audit's
priority-2 headline. Same boundary design as column 2: V-* constants stay the
public numeric vocabulary of GATE!/GATE-TAG@ (signatures unchanged — the many
external readers in cad.f + 9 test files consume GATE-TAG@ n and are untouched);
the four G-TAG cells store `ENUM verdict pass fail not-run ;ENUM`. G-TAG-AT is
the typed `ptr verdict` slot; >VERDICT (V-CK validate-first) the ONE parse
boundary (GATE! rejects a bad tag BEFORE interning the reason, preserving the
original throw-before-ARENA-PUT ordering); VERDICT>N the ONE render-to-code
boundary. MATCH sites: 4 (VERDICT>N, VERDICT-PASS?, V-NAME, P-CLASS) — P-CLASS's
`V-FAIL = if/else` numeric flag check and P-GATE's `V-PASS =` guard are now enum
dispatch; V-NAME's E-RPT-VERDICT render default dropped (unrepresentable).
Readers converted in-file: R-GATE, H-GATE, P-CLASS, P-GATE (2 sites), NEW init,
GATE-TAG@. report.f now requires lib/prelude.f (typed true/false for
VERDICT-PASS?). Swapped-role negatives: n->verdict store, verdict->n fetch, and
the REAL cross-column swaps both directions (roofline value into a G-TAG cell,
verdict value into F-ROOFLINE) — all reject, diagnostics cited in
report-test.f. Gates green: report-test, cad-test, golden-test,
golden-artifact-test, gradcheck-test, demo-ffn-test, mlp-bwd-test,
fusion-plan-test all rc=0; maki/test.f rc=0 (77 suites); all 5 lints clean.

COLUMN 4 LANDED 2026-07-10 — report L-HOT coalescing status (report.f). The
status is the third cell of a stride-3 triple; the typed slot accessor scales
the index itself (`HOT-ST-AT ( n -- ptr costatus ) 3 * 2 + cells L-HOT +`),
proving S1 typed addresses compose with strided list layouts, not just flat
columns/variables. `ENUM costatus unknown coalesced strided unaligned
coalesced-v4 broadcast gathered ;ENUM` — digit-suffixed variant tails
(coalesced-v4) parse fine. CO-* stay the public vocabulary of HOT+/HOT-STATUS@
(signatures unchanged; mem-plan-test's CO-* asserts untouched). >COSTATUS
(CO-CK validate-first; HOT+ still rejects a bad status before interning) parses;
COSTATUS>N + CO-NAME are exhaustive MATCH renders (E-RPT-COAL render default
dropped). Readers converted: R-COALESCE, HOT-STATUS@. Swapped-role negatives:
n-launder both directions + verdict-into-hot-status cross swap, all reject.
Gates green: report-test, mem-plan-test, cad-test rc=0; maki/test.f rc=0 (77
suites); all 5 lints clean.

REMAINING (honest scope for follow-on slices, evidence-based):
- model-ir MI-OP / MI-DT / MI-LAY / MI-IS-AL and the DT-*/LAY-*/AL-* sets
  (tensor.f, tensor-value.f): NOT converted in this slice. Not a capability
  wall — S1 covers the storage — but a SCALE wall under the stable-signature
  rule: MIR-OP@ has 21 reader files and op-kind is a live TABLE INDEX
  (OPR-CLASS/OPR-NAME index op-registry rows with it), and DT-*/LAY-*/AL-* are
  cross-file vocabulary through sched-key/mem-plan/lowering/device paths. The
  honest swap converts the CONSUMERS (OPR-CLASS takes the enum, etc.), which is
  its own multi-file campaign, not a fits-in-one-commit column. Dot stays open
  for these.
- SKEY product with enum fields + typed equality: still BLOCKED on
  habu-checker-capability-layout (enum-kinded product fields) +
  habu-checker-capability-derive (typed equality) — unchanged, do not attempt.
- report evidence rows / measurement history as typed arrays: blocked on S3
  LAYOUT-BUFFER (not landed).
