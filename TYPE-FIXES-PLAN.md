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
    result and stays a designed union. Audit refinement: the classification
    rule picks candidates, but each family still gets an ownership and phase
    check after the declaration cutover before it collapses — materially
    different error domains or protocol phases are not merged mechanically.

13. The 11 dead CAD-KIND nominals, misowned global type declarations, and raw
    repeated semantic bundles are deleted or given owners per the audit list.
    The 81 CAD-NUM projector aliases bred by consumer package reopening
    collapse with them.

## Checker capability gaps (fix or explicitly defer)

14. **Tagged families instantiate generic parameters — implemented in the
    conversion.** (Joel, 2026-07-30: if we need it, we build it.) Today
    `option<MAKI:dtype>` is rejected (`expected: a actual: maki:dtype<>`)
    while `option<CAD-NUM:index>` and option-over-structure work; the split
    is tagged (variant-carrying) vs untagged, not foreign vs own package,
    and it is implementation debt — a tag value is one cell like any
    nominal; the generic-instantiation code was never taught about variant
    families. The engine half fixes it; that unblocks fix 12 completely (all
    20 domain results collapse, no bespoke stragglers).

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
    matches `some`/`none`. Both sides are now probed (2026-07-30): the
    constructor wraps a linear owner, and MATCH consumes it in the some arm
    with an empty none arm — full round trip exit 0. The audit's objection
    that map-take's arms "transfer linear ownership differently" is refuted
    by that probe; option's arms transfer identically. General rule this suggests for the
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

25. **Seal dies entirely.** Package sealing (SEAL-PACKAGE, the prot-wid
    registry, the exit-84 reopen refusal on compiler-internal packages) is
    deleted (Joel, 2026-07-30): we are Forth — everything is open,
    redefinition is a feature, and a diff that reopens a checker package is
    caught by review like any other loud mistake. With it go
    owner-wid-emit-seal.f (dead registry), the maki/db proof mints
    (promotion authority, capability grants, stage proofs — the same
    construction-guard ceremony as cfg-proof, evaporating under CONSTRUCT
    owner), and the doc's sealed-wordlists section. layout-buffer-seal.f and
    lower-cert-seal.f get a census; whatever survives it gets renamed so the
    word seal leaves the codebase.

26. **Unchecked regions become a delimited block, not a variable poke.**
    `0 set-check … 1 set-check` is unbalanced and easy to leave open; Joel's
    ruling: a named pair — `NO-TYPE-CHECK … ;NO-TYPE-CHECK` — that the parser
    balances (an unterminated block is a load error, nesting rejected), with
    `set-check` deleted from the public surface (at most a private cell
    inside the block implementation). This also makes the "unchecked Habu
    only as a named, tested boundary" rule mechanically greppable: the block
    opener IS the boundary marker. Census 2026-07-30: 111 `0 set-check`
    sites across 55 files migrate in the sweep.

27. **One word for unproven effects; TRUSTED.md is deleted.** `PRIM:`,
    `PPRIM:`, and `TRUSTED:` are one mechanism — assert an effect the checker
    cannot prove — split three ways (global prim, packaged prim, trusted
    Forth body). They collapse into `PRIM:` (Joel's ruling: it names the
    thing — a primitive operation of the checked language, be it a syscall,
    a machine op, or a retype axiom the type algebra cannot decompose —
    where "trusted" names an attitude), taking an optionally qualified name
    and an optional body: an engine primitive is the word with no body, a
    laundering retype is the word with a body and a real confession.
    `PPRIM:` and `TRUSTED:` are deleted. A site too large to honestly call a
    primitive is a smell its required justification exposes. The justification and retirement note live AT THE DEFINITION
    in a lint-checkable shape — not in a hand-maintained central ledger.
    TRUSTED.md is duplicate authority (source already declares everything;
    the lint today enforces agreement between two files, which is effort
    spent maintaining drift) and is deleted; any ledger view is generated
    from source on demand. trust-lint.f and the trusted-inventory ratchet
    tools die with the ledger (Joel, 2026-07-30): their only job was
    two-file agreement, and at-site completeness is not a lint's job — the
    declaring word's own grammar REQUIRES the justification and retirement
    fields, so an unjustified trust site fails at load through the
    production path. The compiler is the enforcement; no separate tool.

28. **No pointer lifetimes, ever — the decision is recorded and the dot is
    dead.** habu-add-bounded-host-b40b048f (a full borrow system:
    span<region,type,extent,access,persistence>, generative regions, borrow
    rules, generation counters) is deleted. Linear owners carry the safety
    that matters; borrowed spans are advisory; stash-after-free is a loud
    crash caught in review. The doc states lifetimes as a decision, not a
    gap.

## Audit findings incorporated (three xhigh reviews at 77c7366b, Joel-ratified)

29. **Error-masking defects, fixed in the conversion.** Filesystem predicates
    report permission and I/O errors as "absent" (src/habu/habu1.f:1567-1574,
    1898-1932; lib/fs.f:180-252) — absence and failure become distinct
    answers. BENCH-GET returns invalid miss bytes behind an ignorable
    boolean (maki/competitive-store.f:258-265) — becomes option.
    POLICY:CHECK and ART:PROMOTE still throw around an obsolete
    generic-result limitation that no longer exists — they return result.

30. **Single-authority repairs.** Promotion gets ONE authority (ART:promoted
    and PROMOTE:promoted each prove half the invariant today). Resource
    budgets get ONE dimension enum, and the raw-cell erasure of the survivor
    dies. REPORT splits: table rendering and Model CAD get their own names,
    and the Model CAD handle stops being forgeable singleton state. Process
    completion gets ONE public representation (three incompatible ones
    across 132 files today). Build-cache and replay lifecycle states become
    real state types instead of independently writable flags plus
    side-channel errors.

31. **Positional bundles become types.** Promotion digest/binding, model
    configuration input, KV configuration input, GPT-2 shape, filesystem
    metadata, Gregorian date, and MMA configuration (sixteen mutable globals
    transported through 61 positional tuples) each get a declared type. The
    62 consumers reopening CAD-NUM to publish 81 local projector aliases
    stop; projections live with their owner.

32. **Deletions and renames from the audit.** The twelve identical
    identifier-result families, four evidence-presence families,
    DIFFRUN:ref-result, and the diagnostic/obligation decode duplicates
    collapse into the generics. Eleven public CAD-KIND types and sched have
    no production consumer and are deleted. Globally declared types move to
    their real owners (CUDA handles, PTY lifecycle, nominal builder, PTX
    toolchain policy, autotune census, map types). Misleading names are
    fixed: tcpol, ptxir-node, CEVID:ucat.

33. **Audit non-issues, recorded so nobody relitigates.** Transient
    pointer-length spans stay raw when consumed immediately; CUDA handles,
    PTY process roles, typestate proofs, and live pool handles are real
    nominal separations and stay. (The auditors' fourth non-issue — that
    map-take must stay bespoke — is refuted by probe; see item 22.)

34. **Promotion-digest identity is settled by probe, not debate.** The open
    question: can two distinct policies produce byte-identical artifact
    content? If no, the digest is content identity and the policy identity
    is duplicate authority — delete it. If yes, name the colliding pair and
    keep the policy identity with that pair as its regression fixture.
    Assigned to the engine-half owner during the conversion.

## Implementation plan (for Joel's review)

The 34 items above are the scope; this is the execution order. One shared
conversion branch off master; both of us commit to it; no gates run until
phase 4; the audits are the only other running work. Within the branch the
engine temporarily accepts BOTH old and new forms so the tree stays loadable
mid-sweep — the old forms are deleted before the branch lands, so nothing
shipped ever has two grammars.

### Phase 0 — branch and baseline (hours)

Open bookmark `type-conversion` at master. Record the baseline fixpoint hash
and one full-suite log as the reference point. Freeze the three design calls
(items 16, 17, 21) with codex before any engine commit; each answer is one
line in this file. Absorb the stopped lanes: cherry-pick the already-reviewed
candidates onto the branch as its first commits — the format cut (9fac6471,
dual-accepted), the SAFET DATATYPE= revision and the HFCFG enrollment fix
once their revisions close, and the CUDA cut if Joel rules the HABU_ZED=0
path sufficient. Reviews carry over; their gates run in phase 4 with
everything else. Nothing stopped is left ambient for the sweep to conflict
with.

### Phase 1 — engine, first batch (codex). The grammar.

Order inside the batch, each step buildable on the last:
1. Family definer core: arity inference (20), the agreed payload spelling
   (21), carrier-form NEWTYPE as 1-field-structure sugar (2), CONSTRUCT
   owner flag recorded + parsed + MAKE suppressed + in-package construct
   form checked and lowered (5; absorbs the halted owner-flag hunk and the
   four campaign leaves).
2. Tagged-generic instantiation (14) — unification learns variant families.
3. Nested generated namespaces (7): two-colon lookup, parent-linked records,
   child-namespace generation. Loud name-limit rejection (8) rides this.
4. Multi-cell-local diagnosis (9). PRIM: unification (27): new grammar with
   required justification; old TRUSTED:/PPRIM: arms still parse until phase
   2 ends. NO-TYPE-CHECK block (26): new form; set-check still works until
   the sweep replaces its 111 sites.
5. ENGINE REFRESH #1 (build-fixpoint) — the sweep cannot start without it.

Typed catch (24) and the typed interpreter stack (19) are NOT in this batch:
both are deep engine work whose cascade is unknown. Codex sizes them in his
planning answer; default is a second conversion wave after this one lands,
recorded here as sequenced-after, not dropped.

### Phase 2 — tree migration (claude). The sweep.

File-by-file, in load order so the tree loads at every commit; each file
migrates completely in one commit. Per-file checklist applied mechanically:
SUMTYPE/PRODUCT to ENUM/STRUCTURE (4); DEFTYPE and arity-NEWTYPE to carrier
form (2, 3); proof fields deleted and CONSTRUCT owner added on the owning
structures (5, 6, and the maki/db mints from 25); set-check to NO-TYPE-CHECK
(26); TRUSTED: to PRIM: with its justification moved to the site (27);
generated-name respelling to nested form (7); datatype rename (10); echo
suffixes stripped where no counterpart exists (23); hyphenated family names
reviewed against the item-22 smell rule; map-take replaced by
option<mapping> (22).

Sub-phases in order:
2a. lib/adt (result to ENUM first — everything depends on it), lib core,
    CAD-NUM.
2b. src/core + src/habu (the checker's own sources migrate; the engine
    accepts both forms, so the boot prefix converts like any file).
    ENGINE REFRESH #2 after 2b.
2c. maki + tools + test sweep. Suite inventories updated as files move.
2d. Audit repairs that are TYPE work (30 single-authority, 31 positional
    bundles, 32 deletions/renames/moves). Behavioral repairs (29:
    fs-predicate masking, BENCH-GET, obsolete throws) each get a focused
    red-first test in the same commit as the fix — they change observable
    behavior and are the likeliest final-gate reds, so they are probed
    as they land, not discovered in phase 4.
2e. Package sealing deleted (25: SEAL-PACKAGE, prot-wid, exit-84).
    TRUSTED.md, trust-lint.f, trusted-inventory tools deleted (27).
    Old definer arms deleted from the engine (SUMTYPE, PRODUCT, DEFTYPE,
    arity-NEWTYPE, TRUSTED:, PPRIM:, set-check, hash-name fallback).
    ENGINE REFRESH #3. A grep census proves zero old-form sites remain.

### Phase 3 — reconciliation (claude, small)

docs/type-system.md rewritten against the converted tree (same probe
discipline as its first writing); STATUS.md; refine-lint seed table,
suite-coverage-lint expectations, and any lint expectation touching deleted
or respelled words; this plan file gets a completion stamp per item.

### Phase 4 — the single gate battery, then one landing

In order: fixpoint x2 byte-identity FIRST (everything after runs on the
refreshed engine), then full native test/run.f, full maki, every surviving
lint, native dot gate. Fix reds in place (each red gets its cause
named in the commit that fixes it). When green: one fast-forward of remote
master to the branch tip, verified at origin, workspaces retired, campaign
dots closed.

### Risk register (pre-probe before the phase that triggers them)

R1. The checker's own sources migrating under the both-forms engine (2b) is
    the highest-risk step: a definer bug here corrupts everything after.
    Pre-probe: migrate ONE boot-prefix file first, refresh, run the
    declaration suites, before sweeping the rest.
R2. Tagged-generic unification (14) touches the same CON-OK?/unify path as
    the pointer-element fix; regression pair required with both directions.
R3. The 132-file process-completion unification (30) is the widest single
    consolidation; it migrates as its own 2d slice with a focused suite.
R4. Namespace nesting (7) changes lookup for every qualified reference;
    the respell in 2a-2c must be complete per file or the file fails to
    load — the per-file-complete rule is what keeps this safe.
R5. Behavioral fixes (29) can change outputs recorded in golden logs;
    each carries its red-first test and any golden updates in one commit.
R6. Items deferred by codex's sizing (24, 19) must be recorded as
    sequenced-after with their own entry here — nothing silently dropped.

## Joel's additions (from the type-system.md pass)

- (add items here)
