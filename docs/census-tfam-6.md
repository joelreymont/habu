# Definer / Reserved-Token Census — dot habu-tfam-6-typefamily-ccf45abb

PLAN.md item 6 (lines 563-599): add package-aware public defining words for cell
families (`TYPEFAMILY`) and sum families (`SUMTYPE`), each `VARIANT` block closed
by `;VARIANT`, each sum block by `;SUMTYPE`. Token grammar must reject
delimiters, control words, illegal qualified names, empty sums, upper/mixed-case
family names, reserved signature/type tokens (`a`..`z`, `n`/`f`/`r`, `ptr`,
`field`, atom prefixes, existing CT/VREC/type names), unknown payloads, injection
text. Qualified refs split qualifier before case check (`PKG:result<n>` ok,
`PKG:Result<n>` rejects). Bad top-level family decls under multi-error mode must
be reported + rolled back WITHOUT a fake declared stack signature. Registries
must stay transactional on every failure path.

Plan-cited paths for item 6: `src/core/checker.f:1938-2047`,
`src/core/roles.f:172-188`, `src/habu/habu2.f:2993-3004`,
`docs/type-families.md:461-499`, `docs/type-families.md:1301-1354`.

---

## 1. Existing top-level block definers (the template)

### VALUE-RECORD ... END-VALUE-RECORD — the strongest block-definer template

- `src/core/roles.f:172` `VALUE-RECORD ( -- )` — the defining word itself. An
  ORDINARY dictionary word (not a native parser keyword): `parse-name` for the
  family/record name, `VRDEF-CLEAR` to reset the accumulator, then a
  `BEGIN parse-name ... AGAIN` token loop.
- `src/core/roles.f:173-175` — missing-name error (`die` rc 70).
- `src/core/roles.f:178-188` — token loop: each `parse-name`; `0=` -> missing
  terminator error; `VRDEF-END?` detects `END-VALUE-RECORD`; on terminator it
  hands the accumulated body to `CHECKER-DEFRECORD` and `EXIT`s; otherwise
  `VRDEF-TOKEN+` appends the token to the field buffer.
- `src/core/roles.f:169` `VRDEF-END?` — case-insensitive `END-VALUE-RECORD` match
  (via `VRDEF-STR=CI`, fold at `:156` `VRDEF-FOLD-C`, `:161` `VRDEF-STR=CI`).
- `src/core/roles.f:125-154` — the accumulator: `VRDEF-U`/`VRDEF-I` cells,
  `VRDEF-CLEAR` (:128), `VRDEF-ROOM` cap guard (:131), `VRDEF-C,` (:136),
  `VRDEF-SPACE` (:141), `VRDEF-APP` (:144), `VRDEF-TOKEN+` (:152). Template for
  buffering VARIANT/payload tokens before handing to the checker.

Checker-side registration + validation (all in `src/core/checker.f`):
- `checker.f:2050` `CHECKER-DEFRECORD ( name fields -- )` — the entry point:
  `TYPE-RESERVED?` guard first (:2052, rc 70 on reserved/dup), then `VREC-BEGIN`
  reserves the id, `VREC-PARSE-FIELDS`, `VREC-FINISH`.
- `checker.f:1983` `VREC-BEGIN ( a u -- id )` — allocates a record slot:
  `VREC-ROOM` (`VREC-ENSURE` at :1980), bumps `VREC-N`, copies the name
  (`VREC-STR-COPY`), sets name ptr/len + `VREC-START`/`COUNT`/`TVN`/`RVN` cells.
  This is the "reserve id" phase to mirror for a family record.
- `checker.f:1996` `VREC-FINISH ( id -- )` — commits field count + typevar/rowvar
  counts; empty-record guard `n 0 <=` -> die rc 70 (:1998).
- `checker.f:2035` `VREC-PARSE-FIELDS` — the field token loop: resets scan
  cursor + `PKRESET NMAP-RESET ROWMAP-RESET FAM-RESET SGBAD-CLEAR`
  (:2037-2038), then `BEGIN NEXT-SIG-TOK ... AGAIN` reading (name,type) pairs.
- `checker.f:2041-2047` — the ERROR PATHS to mirror: end-of-tokens with
  `SGBAD` set -> die "bad field type"; `VREC-FIELD-BAD?` (delimiter/empty) ->
  `SGBAD-SYNTAX!` + die; `VREC-FIELD-DUP?` -> die "duplicate field"; missing
  type token -> die; `SIG-TYPE` then store; `SGBAD @` after store -> die.
- `checker.f:2003-2013` `VREC-FIELD-WRAP` / `VREC-FIELD-STORE` — build the
  interned `field` param node from lexed atoms (`MK-ATOM`, `MK-PARAM`) — matches
  the plan's "install from lexed tokens/interned metadata, not source strings".
- `checker.f:2015-2033` `VREC-ATOM-COPY=` / `VREC-FIELD-NAME=` /
  `VREC-FIELD-DUP?` / `VREC-FIELD-BAD?` — dup + delimiter detection helpers.

NOTE on transactionality: `VALUE-RECORD` is NOT transactional today — errors
`die` (hard abort, rc 70) mid-parse after `VREC-BEGIN` has already bumped
`VREC-N`. Item 6 explicitly requires transactional rollback on every failure
path (plan Risk line 594), so the family definer needs a checkpoint/restore
discipline the VREC path does not yet have. See the CAND-* snapshot cells at
`checker.f:5010-5019` (`CAND-VREC-N`, `CAND-VREC-FIELD-N`, `CAND-VREC-NODE-N`,
`CAND-CTN`, `CAND-LIN-NDECL`, ...) — the candidate-scope save/restore machinery
(`CHECKER-CANDIDATE-SCOPE-START`/`-DONE`, used by verify-source.f:431-433) is the
existing rollback primitive to extend for family records.

### DEFTYPE / DEFLINEAR — single-name nominal type definers

- `src/core/roles.f:44` `DEFTYPE ( -- )` — `parse-name` (:45, missing->die),
  `CHECKER-DEFTYPE` registers the nominal (:46), then installs cast words
  `DEFTYPE-CAST-IN`/`-OUT` (:47-48, :34/:39 — TRUSTED >NAME / NAME>N pairs).
- `src/core/roles.f:119` `DEFLINEAR ( -- )` — `parse-name` (:120),
  `CHECKER-DEFLINEAR` (:121).
- `checker.f:3910` `CHECKER-DEFTYPE ( a u -- )` -> `CT-ADD-NOMINAL`;
  `checker.f:3913` `CHECKER-DEFLINEAR` -> `CT-ADD-LINEAR`.
- `checker.f:1777` `CT-ADD-NOMINAL` — `TYPE-RESERVED?` guard (:1778), then
  `CT-SET` with `CT-ROLE`. `checker.f:1781` `CT-ADD-LINEAR` — same guard, then
  `CT-SET CT-LINEAR` + bumps `LIN-NDECL` (:1784).
- `checker.f:3357-3358` PRIM decls for `CHECKER-DEFTYPE`/`CHECKER-DEFLINEAR`.
  These are the primitive boundary the plan wants families to reach "through the
  sealed system/friend capability" (plan lines 78-80).

### BEGIN-STRUCTURE / END-STRUCTURE — CREATE/DOES> stateful block definer

- `src/core/structures.f:17` `BEGIN-STRUCTURE ( -- ptr a n )` — sets
  `STRUCT-ACTIVE` (single-open guard `STRUCT-REQUIRE-CLOSED` :11), `create ... 0 ,
  0 does>`.
- `src/core/structures.f:38` `END-STRUCTURE ( ptr a n -- )` — `STRUCT-REQUIRE-OPEN`
  (:14), clears `STRUCT-ACTIVE`, back-patches size (`swap !`).
- `:22 +FIELD`, `:26 PTR-FIELD:`, `:34 CFIELD:` — mid-block field words guarded by
  `STRUCT-REQUIRE-OPEN`. Template for the open/closed state discipline
  `TYPEFAMILY`/`VARIANT`/`;VARIANT` nesting needs (nested-open must reject:
  `STRUCT-REQUIRE-CLOSED` :12 dies on "nested begin").

Best template to mirror: VALUE-RECORD (roles.f:172) for the block token loop +
terminator + CHECKER-DEFRECORD registration, combined with BEGIN-STRUCTURE's
open/closed state guard for the two-level VARIANT nesting, plus the CAND-*
candidate-scope snapshot for the transactional rollback the plan mandates.

---

## 2. Parser keyword dispatch (where top-level keywords hook in)

Two distinct dispatch surfaces exist. VALUE-RECORD/DEFTYPE are plain dictionary
words on one path but explicit keyword-dispatch entries on the other.

### Native compiled engine — `src/habu/habu2.f`

- `habu2.f:2992` `EM-INTERPRET-DEFINE-KEYWORDS` — the interpret-time keyword
  table. Each entry: `s" kw" KEEP? IF LMAIN LABEL@ LKW<X> <len> ['] C-<X>
  CF-ENTRY THEN`. Existing entries (:2993-3004): `package`->`C-PACKAGE`,
  `public`->`C-PUBLIC`, `private`->`C-PRIVATE`, `end-package`->`C-END-PACKAGE`,
  `trusted:`, `defer`, `create`, `variable`, `constant`, `'`, `char`,
  `immediate`. THIS is where `TYPEFAMILY`/`SUMTYPE`/`VARIANT`/`;VARIANT`/
  `;SUMTYPE` add native `KEEP?`-gated `CF-ENTRY` rows (need new `LKW*`
  labels + `C-*` handler words like C-PACKAGE).
- `habu2.f:2941-2965` `C-PUBLIC` and `:2937?`..`C-PACKAGE`, `:2968` `C-PRIVATE`,
  `:2979` `C-END-PACKAGE` — the handler-word template: `C-TASK-LIVE-GUARD`, guard
  on `PKG-PUB-CELL`, `C-CALL-CHECKER-PUBLIC/PRIVATE/END-PACKAGE`, update package
  cells. Each is `s" c-x" s" --" TRUST` (uncheckable boundary). Family handlers
  follow this shape but must `parse-name`/scan the family name + body tokens.
- `habu2.f:3021` `EM-INTERPRET-FIND` — ORDINARY dictionary lookup, runs only
  after the keyword table misses. Keywords are recognized BEFORE this. So a
  `TYPEFAMILY` keyword entry shadows any dictionary word of the same name.
- `habu2.f:3027` `EM-INTERPRET-WORDS` / `:3036` `EM-INTERPRET` — dispatch order:
  colon check -> `EM-INTERPRET-DEFINE-KEYWORDS` -> string keywords -> number ->
  find. Confirms keyword dispatch precedes lookup.

### Habu-native source verifier — `src/habu/verify-source.f` (the clean mirror)

- `verify-source.f:373` `RECORD-DEFINER? ( a u -- bool )` — the pure token-loop
  keyword dispatcher. Case-insensitive `STR=CI` chain: `package`->RECORD-PACKAGE,
  `public`, `private`, `end-package`, `deftype`->RECORD-DEFTYPE,
  `deflinear`->RECORD-DEFLINEAR, `value-record`->RECORD-VALUE-RECORD,
  `constant`/`create`/`variable`->TRUST-NEXT, `defer`, `trusted:`, `undefine`;
  falls through to `0 0= 0=` (false) for ordinary defs. THIS is the cleanest
  place to add TYPEFAMILY/SUMTYPE dispatch (parallel to RECORD-VALUE-RECORD).
- `verify-source.f:358` `RECORD-VALUE-RECORD` — mirrors roles.f:172 as a
  verify-source handler: `NEXT-SCAN` name, `BEGIN NEXT-SCAN ... VALUE-RECORD-END?
  ... CHECKER-DEFRECORD ... AGAIN`. Direct template for RECORD-TYPEFAMILY /
  RECORD-SUMTYPE with `;SUMTYPE`/`;VARIANT` terminators.
- `verify-source.f:345-353` `RECORD-DEFTYPE`/`RECORD-DEFLINEAR` — single-name
  handler template (`NEXT-SCAN` + missing-name die + `CHECKER-DEFTYPE`).
- `verify-source.f:404` `VERIFY-SOURCE` — top loop: `:`->VERIFY-DEFINITION else
  `RECORD-DEFINER?`. `:389` `VERIFY-DEFINITION` for ordinary colon defs.
- `verify-source.f:429` `SOURCE-BUF` — wraps RUN in
  `CHECKER-CANDIDATE-SCOPE-START`/`-DONE` + catch => the existing transactional
  boundary a bad family decl can roll back through.

### Gforth bootstrap mirror

- NOT present. `rg` for `VALUE-RECORD|DEFTYPE|DEFLINEAR|package` across
  `bootstrap/src/*.fs` returns no definer/keyword-dispatch mirror. The Gforth
  bootstrap (`bootstrap/src/defining.fs`, `parsing.fs`, `checker.fs`) does not
  reproduce the value-record/deftype top-level definers, so plan item 6's "Gforth
  mirror" for these keywords does not exist as an editable site — see
  DISCREPANCIES.

---

## 3. Reserved-name enforcement — `tools/reserved-name-lint-core.f`

- `reserved-name-lint-core.f:107` `RNL-RESERVED-CONTROL?` — control words:
  if/then/else/begin/until/again/while/repeat/case/of/endof/endcase/do/?do/
  loop/+loop/i/j/leave/unloop/exit/recurse.
- `:131` `RNL-RESERVED-PARSER?` — parser tokens: `s"`/`c"`/`."`/type/`'`/`[']`/
  `{:`/`:}`/`[:`/`;]`/char/[char]/immediate/postpone/compile,/does>/trusted:/
  trust/kernel:/check-does!.
- `:153` `RNL-RESERVED-DEFINER?` — definers: create/variable/constant/**package/
  public/private/end-package**/undefine. THIS is where item 6's five tokens
  (`TYPEFAMILY`, `SUMTYPE`, `VARIANT`, `;VARIANT`, `;SUMTYPE`) are added,
  by the same `a u s" tok" LINT-STR=CI if LINT-TRUE exit then` pattern.
- `:163` `RNL-RESERVED?` — the aggregator (control OR parser OR definer). Callers
  hit this to reject user words that shadow a reserved token.
- `:168` `RNL-JSON-FINDING` (and following) — how the lint REPORTS a shadowing
  finding (JSON emit, `LJW-RESET`). The report path for a token that collides.
- Note: `deftype`/`deflinear`/`value-record` are NOT in RNL-RESERVED-DEFINER?
  today (they are ordinary words, not reserved). If item 6 makes family keywords
  RESERVED (plan line 78 "may be global/reserved"), they belong in the definer
  list above; confirm whether value-record should also be back-added for
  consistency (currently a gap, out of item-6 scope).

---

## 4. Reserved signature/type tokens the family grammar must reject

Single gate: `checker.f:1767` `TYPE-RESERVED? ( a u -- bool )`. `CHECKER-DEFRECORD`
already calls it (:2052); family definers should call the same gate on the family
name. It consults, in order, these classes (all in `src/core/checker.f`):

1. Empty token — `:1768` `u 0=` -> reserved.
2. Existing VREC/value-record types — `:1769` `VREC-FIND`.
3. `field` layout token — `:1770` literal `s" field"`.
4. Existing CT type names — `:1771` `CT-FIND`. The CT table holds ALL builtins
   (seeded by `CT-INIT` at `:691`, invoked `:723`): `n f r i64 u8 u32 cell char
   str addr bool idx len count off fd rc pid ms ns tok reg label va symidx asm
   img snap f32 u16` (:692-721) — PLUS every user `deftype`/`deflinear` nominal
   (added via CT-ADD-NOMINAL/CT-ADD-LINEAR :1777, 1781). So `CT-FIND` alone
   enumerates builtins + user nominal/linear types. `CON-OF` (:1697) is just a
   thin alias `a u CT-FIND`.
5. Parametric constructor keywords — `:1772` `PARAM-CTOR?` (`:1743`):
   `ptr span matrix gridctx fanctx idxctx uniqidxctx coopctx rowctx tile acc
   mmctx mmacc uniform rowidx`.
6. Atom-prefix tokens — `:1773` `ATOM-TOK?` (`:1731`): prefixes
   `space- extent- mask- block- align-`.
7. Fresh-atom tokens — `:1774` `FRESH-ATOM-TOK?` (`:1737`): prefixes
   `fresh-extent- fresh-mask-`.
8. Single lowercase letter type vars `a`..`z` — `:1775` `TYPE-VAR-TOK?`
   (`:1759`: `u 1 = IF a c@ LOWER?`). NOTE this already covers `n`/`f`/`r`/`a`
   (plan's `TYPEFAMILY a 0`, `SUMTYPE n 0` examples) as single letters, and CT
   covers them again as builtins.
9. Bad chars `<` `>` `,` — `:1776` `TYPE-BAD-CHAR?` (`:1762`: bytes 60/62/44).

So `TYPE-RESERVED?` already rejects every reserved-token class item 6 names
(`a`..`z`, `n`/`f`/`r`, `ptr`, `field`, atom prefixes, existing CT/VREC/type
names). The family definer reuses it verbatim on the family name. GAPS item 6
must ADD beyond TYPE-RESERVED?: (a) the upper/mixed-case family-name check
(plan lines 573,584 — TYPE-RESERVED? does NOT enforce lowercase, and `TOK-TYPE`
:1785 currently treats an unknown mixed-case token as `BAD-SIG-TYPE`, so a
positive lowercase-only rule is new work); (b) qualified-name split before case
check (`PKG:result` vs `PKG:Result`, plan lines 576-579, 588); (c) delimiter /
control-word rejection in NAME position (control words are not in
TYPE-RESERVED?; `RNL-RESERVED-CONTROL?` at reserved-name-lint-core.f:107 is the
existing list to consult).

Related signature-parse classifier (context for payload-type rejection):
`checker.f:1785` `TOK-TYPE` — maps a signature token to a type: `n`/`f`/`r`
single letters, `CON-OF` builtins, `FRESH-ATOM-TOK?`, `ATOM-TOK?`, single-letter
var, else `BAD-SIG-TYPE` (:1793 -> `SGBAD-UNKNOWN!`). `checker.f:1728`
`SIG-PREFIX?` helper. Payload type names in VARIANT bodies flow through
`SIG-TYPE`/`TOK-TYPE`; unknown payloads land on `BAD-SIG-TYPE` -> `SGBAD` set.

---

## 5. Multi-error mode — `src/core/checker.f`

- `checker.f:4889-4891` — `MULTI-ERR` (mode flag), `MULTI-ERR-N` (reject count),
  init.
- `:4900-4904` `MEO-ON` + file-relative origin cells (`MEO-BL`/`MEO-BC`/`MEO-BB`).
- `:4906` `MULTI-ERR? ( -- bool )`.
- `:4907` `MULTI-ERR-BEGIN ( -- )` — `-1 MULTI-ERR ! 0 MULTI-ERR-N ! 0 MEO-ON !`.
- `:4908` `MULTI-ERR-END ( -- n )` — returns reject count, clears mode.
- `:4909` `MULTI-ERR-ORIGIN!` — sets file-relative diagnostic origin.
- The reject/record path is in the per-definition finalizer around
  `:4985-5008`: `CHECK-VERDICT` (:4985) -> `DVERD`; diagnostic emitted via
  `DIAGXT @ execute` (:4989); the multi-error record at `:5003-5008`:
  `dup 0 = MULTI-ERR? and NMU @ 0 > and IF 1 MULTI-ERR-N +! ...`. CRITICAL for
  item 6: to keep later checks sound this path stores the DECLARED signature as a
  cert — `:5005-5007` `VSIG @ SGSEEN @ and IF SGA@ SGU@ NMA@ NMU@
  CHECKER-USIG-CERT-ADD`. It is keyed on `NMU @ 0 >` (a word NAME) and a declared
  `VSIG`/`SGSEEN`. A top-level FAMILY declaration error has neither a `word` name
  in this sense NOR a declared stack signature, so it CANNOT reuse this reject
  path — this is precisely the "diagnostic unit without a declared stack
  signature" the task flags (echoed in PLAN.md:984-985). Item 6 must add a
  family-declaration diagnostic + count path that reports and rolls back WITHOUT
  fabricating `word`/`declared_effect`/USIG-cert rows.
- Rollback substrate for that path: candidate-scope snapshot cells
  `checker.f:5010-5019` (`CAND-UEND CAND-NEND CAND-SYMN CAND-SYMU CAND-CTN
  CAND-CTU CAND-LIN-NDECL CAND-VREC-N CAND-VREC-FIELD-N CAND-VREC-NODE-N`) driven
  by `CHECKER-CANDIDATE-SCOPE-START`/`-DONE` (used at verify-source.f:431-433).
  Family-record cells must be added to this snapshot set for transactional
  rollback of a rejected `TYPEFAMILY`/`SUMTYPE`.

---

## 6. Package visibility plumbing

Checker-owned package state — `src/core/checker.f`:
- `:3408-3410` `CHECKER-PACKAGE-NONE=0 / -PRIVATE=1 / -PUBLIC=2`.
- `:3411-3414` `CHECKER-PACKAGE-CAP=$100`, `CHECKER-PACKAGE-NAME` buffer,
  `CHECKER-PACKAGE-U` (name len), `CHECKER-PACKAGE-MODE` (current visibility).
- `:3441` `CHECKER-PACKAGE-ACTIVE?` — mode != NONE.
- `:3455` `CHECKER-PACKAGE ( a u -- )` — copies folded name, sets mode PRIVATE.
- `:3459` `CHECKER-PUBLIC` / `:3462` `CHECKER-PRIVATE` — flip mode (guarded by
  ACTIVE?). `:3465?` `CHECKER-END-PACKAGE` — mode NONE + clear name len (:3466).
- `:2275-2276` `SYM-PRIVATE=1 / SYM-PUBLIC=2` (symbol visibility codes).
- `:3534` `CHECKER-PKG-SYM ( pkg pkgu vis a u -- n )` — interns a symbol under the
  current package + visibility; `:3543-3544` reads `CHECKER-PACKAGE-NAME`,
  `CHECKER-PACKAGE-U`, `CHECKER-PACKAGE-MODE` when a package is active. `:3537`
  `CHECKER-PKG-SYM?` lookup; `:3551-3553` searches private then public.
  => A family record stores its package id + visibility by reading
  `CHECKER-PACKAGE-NAME/-U/-MODE` at declaration time and interning via the same
  `CHECKER-PKG-SYM` path (or capturing the mode into the family record).

Native engine package cells (compiled path) — `src/habu/habu2.f`:
- `PKG-PUB-CELL`, `PKG-PRI-CELL`, `PKG-PARENT-CELL`, `PKG-REC-CELL`, `CUR-CELL`
  (used :1177-1182, :2744-2748, :2941-2965, :2986-2989). `C-PACKAGE`/`C-PUBLIC`/
  `C-PRIVATE`/`C-END-PACKAGE` (:2941-2990) update these and call
  `C-CALL-CHECKER-PUBLIC/PRIVATE/END-PACKAGE` to keep the checker cells in sync.
  Family handlers on this path read the same cells for the enclosing package.

Verify-source path wrappers — `src/habu/verify-source.f:331-343`:
`RECORD-PACKAGE`/`RECORD-PUBLIC`/`RECORD-PRIVATE`/`RECORD-END-PACKAGE` call
`CHECKER-PACKAGE`/`CHECKER-PUBLIC`/`CHECKER-PRIVATE`/`CHECKER-END-PACKAGE`. Family
`RECORD-*` handlers read the same live checker package state.

---

## DISCREPANCIES vs plan-cited sites

- `src/habu/habu2.f:2993-3004` (cited for item 6 & 8): correct — this IS
  `EM-INTERPRET-DEFINE-KEYWORDS` keyword table. Verified.
- `src/core/checker.f:1938-2047`: correct — VREC-BEGIN..VREC-PARSE-FIELDS +
  CHECKER-DEFRECORD (defrecord at :2050, just past :2047). Verified.
- `src/core/roles.f:172-188`: correct — VALUE-RECORD definer. Verified.
- "Gforth mirror" (task node 2): NO such mirror for these definers/keywords
  exists in `bootstrap/src/*.fs`. The bootstrap does not reproduce
  value-record/deftype/deflinear or a package keyword dispatch table. The real
  second dispatch surface is the Habu-native `src/habu/verify-source.f`
  `RECORD-DEFINER?` (:373), NOT a Gforth file. Implementer should treat
  verify-source.f as the "mirror" site.
- `docs/type-families.md` line ranges (461-499, 1301-1354) cited but not read in
  this census (grammar spec, out of scope for the code census) — verify they
  exist and match the normalized `VARIANT ... ;VARIANT` grammar before coding.
