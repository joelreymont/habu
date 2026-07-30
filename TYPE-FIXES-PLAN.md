# Type Fixes Plan

Everything we have agreed to fix in the type system, in one place. Joel adds
items as he works through `docs/type-system.md`; the joint review with codex
happens when his pass is done. Tracker skeleton: campaign
`habu-convert-the-type-d5aad352` and the collapsed owner-construction campaign
`habu-checker-sealed-destructure-d967fc03`.

Ground rules for the conversion (Joel, 2026-07-30): stop-the-world — all other
work stopped except the read-only audits; codex owns the engine half, claude
owns the tree-migration half; NO gates run until the conversion is complete;
one full gate battery at the end, then one landing.

## Decided fixes

1. **The declaration surface collapses to four words.** `STRUCTURE` (records),
   `ENUM` (alternatives, with or without payloads and parameters), a new
   carrier-form `NEWTYPE`, and the `CONSTRUCT owner` flag. Everything else
   goes.

2. **`NEWTYPE` gets a stated carrier.** `NEWTYPE idx n`, `NEWTYPE eps r` —
   name plus the wrapped type, Haskell-style. One cell at runtime, a distinct
   type to the checker, derived converter pair. Works for any cell-shaped
   carrier, including floats, which today's forms cannot express. It is
   checker-sugar for a 1-field structure. The old `NEWTYPE name arity` grammar
   is deleted: it never states what it wraps.

3. **`DEFTYPE` is deleted.** It duplicates a 1-field structure, is hardwired
   to `n`, and never states its carrier. All declarations migrate to carrier
   form.

4. **`SUMTYPE` and `PRODUCT` are deleted.** They are the pre-unification
   spellings of `ENUM` and `STRUCTURE`. Roughly 40 files still carry live
   declarations (including `result` itself); all migrate to the unified words,
   then the definers go.

5. **`CONSTRUCT owner`, without the armor.** One flag in the existing derive
   cell: a flagged declaration publishes no `MAKE`; the owning package
   constructs through the existing compiler form; `UNMAKE` stays public
   because destructuring cannot mint anything. The thirteen guard/marker dots
   are deleted. The one limitation is stated, not armored against: packages
   are reopenable by design, so foreign construction via reopen is caught by
   diff review, not by a checker theorem.

6. **Proof tokens evaporate.** With `CONSTRUCT owner` on the real structures,
   `cfg-proof`, `layer-proof`, their private `TRUSTED` mints, and their
   `TRUSTED.md` rows are all deleted. No zero-field token type needs to exist.

7. **Generated namespaces nest.** `MDLCFG:CFGKEY:MAKE` replaces
   `MDLCFG-CFGKEY:MAKE`; mangles like `SAFET-MAP--TAKE:MOVED` die. Engine
   side: qualified lookup learns a two-colon split; namespace records gain a
   parent link; the family definer creates a child namespace under the
   declaring package. This is NOT source-level nested `package` blocks — the
   flat package scope stays. Type rendering already prints the nested form
   (`maki:dtype<>`), so words come to agree with types.

8. **The silent hash-name fallback is deleted.** Today a generated name past
   the 32-character limit silently becomes a hash-built name. New behavior: a
   declaration whose generated name exceeds the engine limit is rejected at
   check time with a named error. Nobody wraps long words to appease a
   mangler. With nested namespaces the generated component is family-local,
   so the limit rarely binds. Prerequisite census: prove zero live hash-named
   words exist.

9. **The multi-cell-local rejection gets a real diagnosis.** Binding a
   multi-cell value to a typed local today fails with `unknown type 'p:pair'
   in signature`, which reads as a typo report. The checker knows the value
   is a multi-cell family; the error must say so and teach the idiom:
   "multi-cell value cannot bind to a local; UNMAKE it." Multi-cell locals
   themselves stay unbuilt until a real consumer hurts without them.

10. **`dtype` becomes `datatype`** everywhere, as part of the migration sweep.

## Audit cuts riding the same conversion

From codex's three unanimous whole-tree audits at `77c7366b` (his reports are
the census source):

11. The 12 duplicated identifier-result families collapse to generic
    `result`; so do the 5 `option` duplicates and the duplicate DIAG/OBLIG
    decode result.

12. The 20 broader domain-result families classify by payload kind: untagged
    payloads collapse to generic `result`; tag-enum payloads are blocked by
    fix 14 and either wait, restructure their error arm, or stay bespoke with
    the gap cited; more than two arms or linear payloads means it was never a
    result and stays a designed union.

13. The 11 dead CAD-KIND nominals, misowned global type declarations, and raw
    repeated semantic bundles are deleted or given owners per the audit list.
    The 81 CAD-NUM projector aliases bred by consumer package reopening
    collapse with them.

## Checker capability gaps (fix or explicitly defer)

14. **Tagged families cannot instantiate generic parameters.**
    `option<MAKI:dtype>` is rejected (`expected: a actual: maki:dtype<>`)
    while `option<CAD-NUM:index>` and option-over-structure work. The split
    is tagged vs untagged, not foreign vs own package. Blocks part of fix 12.
    Decide during the joint review: fix in the conversion, or defer with the
    classification rule standing in.

15. **Sealed-destructure residue.** Generated `MAKE`/`UNMAKE` are public even
    for proof-carrying records (a holder can UNMAKE and re-MAKE with a stale
    proof). `CONSTRUCT owner` fixes construction; whether `UNMAKE` ever needs
    owner-scoping waits for a real consumer.

## Open design calls (settle at the joint review)

16. Parameter syntax for the 44 parameterized `NEWTYPE`s migrating to carrier
    form.

17. Converter-name derivation for non-`n` carriers (`>IDX`/`IDX>N` reads
    wrong for a float carrier; pick one scheme, no second one).

18. Whether unconsumed generated `UNMAKE`s justify making `UNMAKE` generation
    opt-in like `DERIVE` — decide from an XREF census, not taste.

19. **The interpreter gets a typed stack; the multi-cell prompt ban dies.**
    Today any word returning a multi-cell value is refused at the interpreter
    prompt outright (`hb: interpret-mode layout value`), because the prompt
    shuffles cells with no type model. Joel's ruling: the information to do
    better already exists — every word declares its effect and every family's
    width is in the registry — so interpret mode keeps a typed shadow stack
    and checks each line incrementally, exactly as the checker walks a body.
    Multi-cell producers, MATCH, and UNMAKE all work at the prompt; the only
    thing rejected is an actual tear (a cell-level shuffle that would split a
    bundle), rejected by name. Design round must cover: shadow-stack
    resynchronization across throws, and an explicit raw-prompt escape for
    unchecked debugging pokes so that capability is deliberate, not
    accidental.

20. **Explicit arity on declarations is deleted; the parser infers it.**
    `ENUM option 1` becomes `ENUM option` — the parser reads to the closer and
    counts the distinct type variables itself (Joel: we know the number, we
    can parse until `;ENUM`). Same for `STRUCTURE name 0` and the carrier
    `NEWTYPE`. Parameter letters already work in any use order — the
    "declare a before b" rule that result.f's old header claimed was probed
    2026-07-30 and does not exist (b-before-a loads, exit 0); the false
    prose is deleted. Engine note: the definer currently
    registers the family row before parsing the body; registration defers or
    the arity cell is patched after the body parses.

21. **One payload spelling in variants.** `option` writes `FIELD value a`,
    `result` writes bare positional `a` — same machinery, two syntaxes. The
    conversion picks one (settle at the joint review) and migrates the other.

22. **`SAFET:map-take` is deleted as an option duplicate.** Its arms —
    `moved(mapping) | empty` — are `some | none` in costume. Probed
    2026-07-30: the generic `option` instantiates at a linear payload
    (wrapping a linear owner in `OPTION:SOME` certifies), so
    `DETACH-MAPPING` returns `option<SAFET:mapping>` and every consumer
    matches `some`/`none`. The migration must verify linearity holds through
    the MATCH arms when consumers convert (constructor side is probed; the
    consumption side rides the sweep). General rule this suggests for the
    sweep: a hyphenated compound family name is a smell — it is usually
    either a disguised generic or a type homed in the wrong package.

23. **Word names stop echoing their argument types.** Where the package plus
    the signature already state what a word consumes, the type-echo suffix is
    deleted: `MEM:RELEASE-BYTES` becomes `MEM:RELEASE` (there is no
    RELEASE-CELLS to distinguish from), `SAFET:UNMAP-MAPPING` becomes
    `SAFET:UNMAP` (one unmap in the package; the old suffix defended against
    the SAFET-MAP package collision that nested namespaces retire). A suffix
    survives only where it distinguishes two real words — `ALLOC-BYTES` /
    `ALLOC-CELLS` stay a pair (cells have live consumers: vector, KV,
    safetensors). The sweep applies this census-driven, not by taste: for
    each suffixed word, does a counterpart exist? No counterpart, no suffix.

24. **Type `catch` honestly.** Today `catch` accepts only a stack-preserving
    shape (inputs equal outputs), which forces shim words whose whole job is
    to satisfy that contract — the `-BODY` convention (`UNMAP-BODY`,
    `PARSE-BODY`, and kin) is ceremony paid to crude typing. New contract:
    `catch` over an arbitrary checked effect, outcome delivered as a two-arm
    value (completed with the effect's outputs, or thrown with the code and
    the inputs restored) that callers MATCH like any result. The `-BODY`
    shims evaporate. This is also the capability whose absence strands
    owners on throw paths (the SEAL/E-UNSET class the old WSTORE header
    described): a catch whose arms may hold different types is the same fix.

25. **"Seal" gets one meaning.** Wordlist sealing (prot-wid, the exit-84
    reopen refusal protecting compiler-internal packages) is a different
    concept and keeps the name. Everything else called seal dies or is
    renamed: owner-wid-emit-seal.f goes with the dead registry; the maki/db
    proof mints (promotion authority, capability grants, stage proofs) are
    the same construction-guard ceremony as cfg-proof and evaporate under
    CONSTRUCT owner; layout-buffer-seal.f and lower-cert-seal.f get a census
    before a verdict — checker bookkeeping stays, ceremony goes.

## Joel's additions (from the type-system.md pass)

- (add items here)
