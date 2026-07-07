---
title: "CAD: ADT swap for report/IR/schedule internals"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:20:50.079838+02:00"
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
