# Type Fixes Plan

Everything we have agreed to fix in the type system, and the order we fix it
in. Reviewed by both orchestrators; Joel reviews this finished version. The
34 scope items keep their numbers permanently; each carries a wave tag.

**Wave 1** is the stop-the-world conversion: the declaration substrate, the
tree migration, old-form deletion, and the shared GPT-2 model types — the
work that unblocks the vLLM path. **Wave 2+** items are agreed and scheduled
behind it, each as its own campaign; nothing is dropped.

Ground rules (Joel, 2026-07-30): all other work stays stopped during wave 1
except read-only audits. Codex owns the engine half, claude owns the
tree-migration half; work decomposes into leaves under 30 minutes, each in
its own jj workspace off the shared `type-conversion` branch — the branch is
the integration line, not the working surface. NO validation suites run
until the wave-1 gate battery (engine builds are necessary artifacts and are
not validation). One gate battery at the end, then one landing.

## One open decision for Joel

The stopped CUDA-cut lane is complete and dual-reviewed but blocked on the
Zed device gate: approve the Tailscale check, rule the HABU_ZED=0 compile
path sufficient, or hold the lane for a post-conversion landing.

## Decided fixes

1. **The declaration surface collapses to three declarers and one header
   clause.** [wave 1] `STRUCTURE` (records), `ENUM` (alternatives, with or
   without payloads and parameters), a carrier-form `NEWTYPE`, and the
   `CONSTRUCT owner` header clause on any of them. Everything else goes.

2. **`NEWTYPE` gets a stated carrier.** [wave 1] `NEWTYPE idx n`,
   `NEWTYPE eps r` — name plus the wrapped type. One cell at runtime, a
   distinct type to the checker, generated `FAMILY:MAKE`/`FAMILY:UNMAKE`
   converters like every family. Works for any cell-shaped carrier,
   including floats, which today's forms cannot express. It is
   checker-sugar for a 1-field structure. Width invariant (frozen): the
   carrier is ONE complete type term whose instantiated width is exactly one
   cell; a carrier parameter is constrained cell-shaped, and a wide or
   width-unknown instantiation is rejected at declaration. Phantom binders
   (`NEWTYPE ix<e> n`) remain valid. The old `NEWTYPE name arity` grammar is
   deleted: it never states what it wraps.

3. **`DEFTYPE` is deleted.** [wave 1] It duplicates a 1-field structure, is
   hardwired to `n`, and never states its carrier. All declarations migrate
   to carrier form; its derived `>NAME`/`NAME>N` converter scheme dies with
   it — `FAMILY:MAKE`/`FAMILY:UNMAKE` is the one converter spelling.

4. **`SUMTYPE` and `PRODUCT` are deleted.** [wave 1] They are the
   pre-unification spellings of `ENUM` and `STRUCTURE`; all live
   declarations (including `result` itself) migrate, then the definers go.
   The sweep's opening census is the authoritative count.

5. **`CONSTRUCT owner`, without the armor.** [wave 1] A header clause on a
   declaration: a flagged type publishes no `MAKE`; the owning package
   constructs through the compiler's `construct` form, which wave 1 extends
   from sum/enum variants to structure and newtype construction
   (`construct FAMILY make`). `UNMAKE` stays public and always generated —
   destructuring cannot mint anything. The thirteen guard/marker dots are
   deleted. The one limitation is stated, not armored against: packages are
   reopenable by design, so foreign construction via reopen is caught by
   diff review, not by a checker theorem.

6. **Proof tokens evaporate.** [wave 1] With `CONSTRUCT owner` on the real
   structures, `cfg-proof`, `layer-proof`, the maki/db stage proofs, their
   private mints and ledger rows are all deleted. No zero-field token type
   needs to exist.

7. **Generated namespaces nest.** [wave 1] `MDLCFG:CFGKEY:MAKE` replaces
   `MDLCFG-CFGKEY:MAKE`; mangles like `SAFET-MAP--TAKE:MOVED` die. Engine
   side: qualified lookup learns a two-colon split; namespace records gain a
   parent link; the family definer creates a child namespace under the
   declaring package. NOT source-level nested `package` blocks — the flat
   package scope stays. Type rendering already prints the nested form, so
   words come to agree with types.

8. **Long generated names: the hash fallback AND the arbitrary cap both
   die.** [wave 1] The 32-byte limit was a readability cap, not an engine
   limit — DNAME-EXT already stores long names. New behavior: any length
   works up to true storage/token capacity; only a genuine capacity overflow
   is rejected, loudly, at declaration. No silent hash names, no wrapping
   long words to appease a mangler.

9. **The multi-cell-local rejection gets a real diagnosis.** [wave 2+] The
   checker knows the value is a multi-cell family; the error says so and
   teaches the idiom ("multi-cell value cannot bind to a local; UNMAKE
   it") instead of today's `unknown type in signature`. Multi-cell locals
   themselves stay unbuilt until a real consumer hurts without them.

10. **`dtype` becomes `datatype`** everywhere. [wave 1, rides the sweep]

## Audit cuts

11. **The result/option duplicates collapse.** [wave 1 — pure declaration
    rewrites] Twelve identical identifier-result families, four
    evidence-presence families, `DIFFRUN:ref-result`, and the
    diagnostic/obligation decode duplicates become the generics.

12. **The twenty domain-result families classify under one post-fix rule.**
    [wave 1] With item 14 implemented no family is blocked by tagging: a
    family whose arms are ok/err over single payloads becomes generic
    `result`; more than two arms or arms that are not success/error means it
    was never a result and stays a designed union. Each family still gets
    an ownership and phase check before it collapses — materially different
    error domains are not merged mechanically.

13. **Dead and misowned types.** [wave 1: only the dead-type deletions that
    fall out of the declaration sweep (the eleven dead CAD-KIND types,
    sched). wave 2+: the rehomes (CUDA handles, PTY lifecycle, nominal
    builder, PTX toolchain policy, autotune census, map types), the 81
    CAD-NUM projector-alias collapse, and the renames (tcpol, ptxir-node,
    CEVID:ucat).]

## Checker capability work in wave 1

14. **Tagged families instantiate generic parameters.** [wave 1] Today
    `option<MAKI:dtype>` is rejected while untagged payloads work; the
    split is tagged (variant-carrying) vs untagged, and it is
    implementation debt — a tag value is one cell like any nominal. Fixed
    in the engine batch; this is what lets item 12 collapse everything.

15. **Sealed-destructure residue.** [recorded] `CONSTRUCT owner` fixes
    construction. Whether `UNMAKE` ever needs owner-scoping waits for a
    real consumer.

## Settled design calls

16. **Parameters are explicit head binders.** `ENUM result<a,b>`,
    `STRUCTURE pair<a,b>`, `NEWTYPE ix<e> n`. Arity is the binder-list
    length — no naked count, and no inference: phantom parameters (used
    for typing only, appearing in no field — MAKI's ix has one) make
    counting-by-use unsound. Census: 123 NEWTYPE declarations, 15
    parameterized.

17. **One converter scheme:** generated `FAMILY:MAKE` and `FAMILY:UNMAKE`
    for every family including carrier newtypes. DEFTYPE's `>NAME` scheme
    dies with DEFTYPE.

18. **`UNMAKE` is always generated.** Not opt-in.

19. **The interpreter gets a typed stack.** [wave 2+ campaign] Interpret
    mode keeps a typed shadow stack and checks each line incrementally;
    multi-cell producers, MATCH, and UNMAKE work at the prompt; only an
    actual tear is rejected, by name. Sizing (codex): runtime effect
    lookup, shadow-stack state, per-call effect application, throw resync,
    raw escape — its own campaign after GPT-2 runs.

20. *(superseded by 16 — kept for numbering)* Arity inference was ruled
    out by phantom parameters; explicit binders replace both the naked
    count and inference. The "declare a before b" ordering rule claimed by
    old prose was probed 2026-07-30 and does not exist.

21. **Variant payloads always spell `FIELD name type`.** The bare
    positional spelling migrates.

22. **`SAFET:map-take` is deleted as an option duplicate.** [wave 1]
    `moved(mapping) | empty` is `some | none` in costume. Probed both
    sides 2026-07-30: constructing `option` over a linear owner certifies,
    and MATCH consumes the owner in the some arm with an empty none arm —
    full round trip exit 0. (The audit objection that its arms "transfer
    linear ownership differently" is refuted by that probe.) Only this
    enumerated case and the audit-enumerated duplicates migrate — there is
    no open-ended name-smell rule.

23. **Word names stop echoing their argument types.** [wave 2+] `MEM:RELEASE-BYTES` becomes `MEM:RELEASE`; `SAFET:UNMAP-MAPPING`
    becomes `SAFET:UNMAP`. Census-driven: a suffix survives only where a
    real counterpart exists (`ALLOC-BYTES`/`ALLOC-CELLS` stay a pair).

24. **Type `catch` honestly.** [wave 2+ campaign] Outcome as a two-arm
    value; the `-BODY` shim convention evaporates; also closes the
    owner-stranding-on-throw class. Sizing (codex): effect-row outcome
    representation, checker control-edge typing, runtime
    packaging/restoration, lowering, caller migration — its own campaign
    after GPT-2 runs.

25. **Seal dies entirely.** [wave 2+, one campaign] Package sealing
    (SEAL-PACKAGE, prot-wid, the exit-84 reopen refusal) is deleted — we
    are Forth; review catches compiler-package reopens. With it go
    owner-wid-emit-seal.f and the doc's sealed-wordlists section;
    layout-buffer-seal.f and lower-cert-seal.f get a census first. The
    maki/db proof mints go earlier, in wave 1 with item 6.

26. **Unchecked regions become `NO-TYPE-CHECK … ;NO-TYPE-CHECK`.**
    [wave 2+] Parser-balanced block; `set-check` leaves the public
    surface; 111 sites across 55 files migrate in that campaign.

27. **One word for unproven effects: `PRIM:`.** [wave 2+] It names the
    thing — a primitive operation of the checked language (syscall,
    machine op, retype axiom). `PPRIM:` and `TRUSTED:` fold in; the
    justification and retirement note move to the declaration in a
    grammar-required shape; TRUSTED.md, trust-lint.f, and the
    trusted-inventory ratchets are deleted (a ledger view, if ever wanted,
    is generated).

28. **No pointer lifetimes, ever.** [decision, recorded] The borrow-system
    dot is deleted. Linear owners carry the safety that matters; borrowed
    spans are advisory; stash-after-free is a loud crash caught in review.

## Audit repairs

29. **Error-masking defects.** [wave 2+, behavioral — each fix lands with a
    red-first test in its own campaign] Filesystem predicates that report
    permission/IO errors as "absent" (src/habu/habu1.f:1567-1574,
    1898-1932; lib/fs.f:180-252); BENCH-GET's invalid miss bytes behind an
    ignorable boolean; POLICY:CHECK and ART:PROMOTE throwing around an
    obsolete generic-result limitation.

30. **Single-authority repairs.** [wave 2+ except where an item is a pure
    declaration rewrite] One promotion authority; one budget dimension
    enum with no raw erasure; REPORT split between its two unrelated
    owners with an unforgeable handle; ONE process-completion
    representation (132 files); build-cache/replay lifecycle as real state
    types.

31. **Positional bundles become types.** [wave 1: only the GPT-2 bundles
    (configuration input, shape) as part of item 32. wave 2+: promotion
    digest/binding, KV configuration input, filesystem metadata, Gregorian
    date, MMA configuration.]

32. **Shared GPT-2 model types, exact hard-cut contracts.** [wave 1 — the
    product half of the cutover] Declared on the new substrate:
    `MAKI:datatype` (the sole datatype authority, renamed); `SAFET:file`
    (the checkpoint source); `GPT2:config` and `GPT2:tensor-id` — the GPT2
    package owns everything model-specific (weights, execution, catalog);
    `FS:path` as the borrowed ptr-u8 plus `CAD-NUM:byte-len` structure;
    config opening returns generic `result<GPT2:config,n>`, not a bespoke
    config-result family; `DEVRT` owns the generic device runtime; `INFER`
    owns the closed model carrier. `GPT2DEV` and `MDLCFG:mcfg` do not
    survive as names.

33. **Audit non-issues, recorded.** Transient pointer-length spans stay
    raw when consumed immediately; CUDA handles, PTY roles, typestate
    proofs, live pool handles are real nominal separations. (The map-take
    claim is refuted; see 22.)

34. **Promotion-digest identity: settled by probe, owned by MAKI.** Can
    two distinct policies produce byte-identical artifact content? No →
    the policy identity is duplicate authority, delete it. Yes → name the
    colliding pair and keep it with that pair as its fixture.

## Wave 1 implementation plan

### Phase 0 — branch and baseline

Open bookmark `type-conversion` at master. Record the baseline fixpoint
hash and note (no suites run) the last green master as the reference.
Absorb the stopped lanes: jj duplicate the dual-accepted format cut
(9fac6471) onto the branch (rebase preserves the original workspace);
finish the HFCFG enrollment fix inside the branch (two SUITE blocks);
DISCARD the DATATYPE= candidate — the sweep rewrites that exact
surface under the rename, review evidence carries; the CUDA lane follows
Joel's open decision above. Nothing stopped is left ambient.

### Phase 1 — engine batch (codex), in this order

1. Nested namespaces first (7): two-colon lookup, parent-linked records,
   child-namespace generation — the final declarers must emit nested names
   from birth, so lookup exists before they do. Long-name handling (8)
   rides this.
2. Unified declarers (head binders (16), FIELD payload spelling (21),
   carrier NEWTYPE (2), CONSTRUCT owner + construct-family-make (5)).
   They write the registry directly; a failed declaration is undone by
   the bare watermark save/restore (three pointers), which is the same
   mechanism the reject-and-continue candidate checks use. Per item 35
   the declaration event/transaction apparatus is deleted — no staged
   commits, and no eager-row-then-patch either.
3. Tagged-generic instantiation (14).
4. OLD-FORM DELETION, same batch (Joel, 2026-07-30: hard cutover — no
   both-forms engine, not even branch-internal): SUMTYPE, PRODUCT,
   DEFTYPE, the arity-NEWTYPE grammar, and the hash fallback are deleted
   in the engine batch itself. Then ENGINE REFRESH #1. From that moment
   nothing with an old-form declaration loads until it is migrated — the
   boot prefix carries zero old-form declarations (verified), so the
   engine itself builds; the sweep then advances a LOADABLE FRONTIER in
   load order, and an unmigrated file failing loudly is the census.

### Phase 2 — tree migration (claude)

Migration unit = one declaration plus its COMPLETE caller closure. No
old-name aliases, ever. Leaves under 30 minutes, own workspaces,
integrated onto the branch. Hard-cutover check discipline: the tree is
NOT fully loadable mid-sweep by design; each leaf's check is that the
migrated PREFIX loads — bin/hb --load of the deepest migrated file in
load order — plus the leaf's named probe. Full-tree loadability returns
exactly when the sweep completes.

2a. `result` and `option` first (everything depends on them), then
    lib/adt, lib core, CAD-NUM.
2b. src/core + src/habu — the checker's own sources. R1 pre-probe first:
    migrate ONE boot-prefix declaration, refresh, load the tree (no
    suites), then sweep. ENGINE REFRESH #2 after 2b.
2c. maki + tools + test declarations, suite inventories updated as files
    move. The wave-1 rewrites ride here: the duplicate collapses (11, 12),
    the dead-type deletions from 13, map-take (22), the datatype rename
    (10), proof deletion (6), and the exact GPT-2 types (32).
2d. Old-form deletion: SUMTYPE, PRODUCT, DEFTYPE, arity-NEWTYPE grammar,
    the hash fallback, map-take. ENGINE REFRESH #3. An rg source census
    proves zero old-form sites.

### Phase 3 — the gate battery, then one landing

Each gate examined and justified (Joel, 2026-07-30); nothing runs twice.
In order:
1. fixpoint x2 byte-identity FIRST — the conversion rewrites the
   declaration core of a self-hosting compiler; identical-bytes rebuild
   twice is the stability proof. Everything after runs on the refreshed
   engine.
2. full native test/run.f — direct correctness evidence for the new
   declarers and checker.
3. full maki — behavior evidence for every migrated file (the frontier
   only proved loads).
4. ptx-stdlib and the touched native slices (lib/ptx files migrated, so
   their slices are in scope by the touch rule).
5. both exact-diff gates on the final combined diff — the ownership
   check on every migrated definition, run once.
6. the cheap lints, seconds each: host-lint, refine-lint (M18 fixed its
   seeds), suite-coverage-lint, error-code-lint, maki-dep-lint,
   dot-dep-lint. (The former separate "native dot gate" entry is deleted
   as a duplicate of dot-dep-lint.)
STATUS note: stale-status-lint reads STATUS.md, which names deleted
types; M18 therefore includes a MINIMAL factual STATUS touch (delete
sentences naming dead types, add nothing) so the battery can green —
the real STATUS rewrite stays phase 4. Deliberately absent: a
performance gate — declaration-time machinery changed, not runtime hot
paths; boot-time shifts surface in the fixpoint run.
Fix reds in place, each with its cause named. When green: one
fast-forward of remote master, verified at origin, workspaces retired,
campaign dots closed, wave-2 campaigns unblocked.

### Risk register

R1. The checker's own sources migrating under the both-forms engine (2b):
    one-declaration-first pre-probe before the sweep.
R2. Tagged-generic unification (14) touches the CON-OK?/unify path of the
    pointer-element fix; regression pair required both directions.
R3. Namespace nesting changes every qualified lookup; the
    declaration+closure migration unit is what keeps each commit loadable.
R4. The absorbed format cut deletes files 2c also touches — the phase-0
    duplication order prevents the conflict; the sweep treats the branch
    tip, not old master, as its base census.
R5. Anything deferred mid-wave must gain a wave-2 entry here before its
    lane closes — nothing silently dropped.

### Wave-1 leaf list (owner, blocked-by, owned interface, production check)

Engine leaves (codex). Check = a checked load probe in the leaf workspace;
no suites.

- E1 two-colon qualified lookup. blocked-by: none. Interface: XREF resolves
  PKG:CHILD:WORD. Check: probe file resolving a nested spelling loads.
- E2 parent-linked namespace records. blocked-by: E1. Interface: child
  registration under a declaring package. Check: record walk probe.
- E3 definers emit child namespaces. blocked-by: E2. Interface: a family
  declared in a package publishes PKG:FAMILY:MAKE. Check: load probe old
  mangled name E-UNDEFINED, nested name resolves.
- E4 long-name capacity. blocked-by: E3. Interface: no hash fallback; true
  capacity overflow rejects loudly. Check: over-capacity declaration probe.
- E5 binder heads on all three declarers. blocked-by: none (parallel to
  E1-E4). Interface: NAME<a,b> binder list; arity = list length. Check:
  binder probe incl. phantom binder.
- E6 FIELD payload spelling everywhere. blocked-by: E5. Check: bare
  positional payload rejects.
- E7 carrier NEWTYPE. blocked-by: E5. Interface: NEWTYPE name<binders>
  carrier; one-cell width invariant enforced. Check: float carrier loads;
  wide carrier rejects.
- E8 CONSTRUCT owner clause. blocked-by: E5. Interface: header clause
  recorded in the derive cell (absorbs the halted owner-flag hunk); MAKE
  suppressed. Check: foreign qualified MAKE E-UNDEFINED.
- E9 construct FAMILY make for structures/newtypes. blocked-by: E8.
  Interface: in-package construction form checked and lowered. Check:
  owner-package construction probe loads; foreign rejects.
- E10 tagged-generic instantiation. blocked-by: none. Interface:
  option/result instantiate at variant families. Check: the paid
  reproducer pair, both directions.
- E11 ENGINE REFRESH #1. blocked-by: E1-E10.

Migration leaves (claude). Census 2026-07-30, exact: 111 SUMTYPE, 42
PRODUCT, 123 NEWTYPE, 29 DEFTYPE declarations. src/core and src/habu carry
ZERO old-form declarations (verified), so the checker's own sources need
only the generated-namespace respell, not declarer migration. Every leaf:
owner claude, result = the named declarations on the new grammar with their
complete caller closure respelled, check = checked tree load at the leaf
commit plus the named probe. Blocked-by E11 unless noted.

- M1 lib/adt: result.f `SUMTYPE result 2` to binder ENUM; option.f gains
  its binder head. Closure: generated RESULT:/OPTION: constructor and
  MATCH spellings tree-wide (global family — spelling unchanged by
  nesting; binder head only). Probe: lib/adt consumers load.
- M2 lib scalars: map.f `map-loc`, process.f `outcome`, utf8-scalar.f
  `scalar-step` (3 SUMTYPE to ENUM). Closure: their MATCH sites in the
  same files plus lib/process consumers. blocked-by M1.
- M3 lib nominals: nominal/binding.f `binding`, nominal/path.f `path`,
  nominal/row.f `row`, process-pty-handle.f `sup-pid pgrp target-pid
  group-watch target-watch sup-watch` (9 NEWTYPE to carrier form).
  Closure: generated-name consumers of those nine (rg shows none outside
  the declaring files). blocked-by M1.
- M4 lib/ptx + ffi: toolchain.f `tcpol` (SUMTYPE), ir.f `ptxir-node`
  (PRODUCT), cuda-driver.f `CUDA-DEV CUDA-CTX CUDA-MOD CUDA-FN
  CUDA-DEVPTR CUDA-EVENT`, ffi-test.f `FFI-DEV FFI-CTX` (8 DEFTYPE to
  carrier form). Closure: cuda-driver consumers in lib/ptx tests and
  maki device files. blocked-by M1. (Renaming tcpol/ptxir-node is wave
  2+; here they migrate grammar-only under their current names.)
- M5 CAD-NUM: cad-num-types.f ten roles `byte-len item-count cell-count
  index byte-off cell-off alignment positive-divisor alloc-byte-len
  alloc-cell-count` + `ENUM numeric-result 1` binder head. Closure:
  cad-num-arithmetic.f and cad-num-types-test.f (the only two files
  spelling the generated CAD--NUM names — verified). blocked-by M1.
- M6 boot-prefix respell pre-probe: ONE generated-name consumer in
  src/core (the R1 gate: respell, refresh, tree load; no suites).
  blocked-by M2-M5.
- M7 src respell: the remaining generated-name spellings in src/core +
  src/habu (no declarer migration — zero old-form declarations there).
  ENGINE REFRESH #2 closes this leaf. blocked-by M6.
- M8 maki/cad-kinds.f: thirty NEWTYPEs to carrier form; DELETE the
  audited eleven dead CAD-KIND types exactly: `design-id obj-id
  analysis-id plan-id toolchain-id pass-id artifact-kind capability-id
  shape stage effect`. (`suite-id` is live and stays.) Closure: the
  refine sites in maki/tensor.f and the surviving id consumers across
  maki/db. blocked-by M7.
- (dead `sched` — the VALUE-RECORD at maki/schedule.f:42, its four
  words at 251-254, and the schedule-test.f checks — is wave 2+ dead-code
  deletion: it does not block old-form removal or GPT-2 and gets no
  wave-1 leaf. maki/sched-key.f is live.)
- M9 maki singles: async-dag.f `stream-id event-id node-id` + `stats`,
  model-ir.f `input-slot operand-ref input-index ref-pos`,
  tensor-value.f `tensor`, extent.f `ix XR-SLOT XR-SURF-LEN
  XR-TAIL-LEN`, extent-tensor.f `TR-SLOT`, report.f `REPORT`, spec.f
  `SP-FI EQ-SLOT` (17 declarations; artifact.f's id-result belongs to
  M12's direct cut). Closure: their
  generated-name consumers per file (extent's ix carries the phantom
  binder — the item-16 witness). blocked-by M7.
- M10a proofs, typestate: typestate.f `decl elab solved legal draft
  complete drafted verified emitted build-proof` — CONSTRUCT owner on
  the owning stage structures, tokens+mints deleted. blocked-by M7.
- M10b proofs, db promotion: db/promotion.f `cand-proof ver-proof
  meas-proof sat-proof prom-proof` + db/promotion-authority.f
  `auth-proof`. blocked-by M7.
- M10c proofs, db capability: db/capability.f `grant`. blocked-by M7.
- M10d proofs, evidence: evidence/policy.f `grant-proof`,
  evidence/promote.f `promoted`, evidence/schema.f `certify-proof
  golden-proof gradcheck-proof profile-proof`. blocked-by M7.
- M10e proofs, infer: infer/gpt2-tensor.f `layer-proof` only
  (cfg-proof deletion is owned by M14c). Probe (all M10x): foreign
  construction of one formerly guarded type per package rejects.
  blocked-by M7.
- M11a infer deftypes: kv-cache.f `KV-CACHE-ID KV-SEQ-SLOT KV-SEQ-GEN`
  to carrier form. blocked-by M7.
- M11b the datatype rename (item 10): MAKI:dtype family + five
  constructors + DT-KEY/DT-SIZE/EQ and every consumer spelling.
  blocked-by M7.
- M11c map-take deletion (item 22): safetensors.f map-take deleted,
  DETACH-MAPPING returns option<SAFET:mapping>, consumers re-matched.
  Probe: map-take spellings E-UNDEFINED. blocked-by M7.
- M12 the twelve id-result collapses. All twelve are `ok a | wrong-width
  | unknown` (verified). Freeze ONE shared error enum, owned by CAD-KIND:
  `CAD-KIND:id-error = wrong-width | unknown`; all twelve become
  `result<a,CAD-KIND:id-error>` — no result<a,n>, no option, no
  per-owner family: rev.f:67, target/target.f:79,
  artifact.f:44, schema.f:74, db/evidence.f:62, config.f:72,
  db/obligation.f:205, journal.f:54, db/diff-suite-id.f:49,
  producer.f:73, numpolicy.f:153, experiment/run.f:91 — artifact.f's is
  the direct SUMTYPE-to-result hard cut. One leaf each. Closure per
  leaf: that file's constructors + MATCH sites. The CAD-KIND:id-error
  freeze (rides M8) comes first; the twelve conversions then run in
  PARALLEL — disjoint files, disjoint owners. blocked-by M8 only.
- M13 the remaining collapses, each with its frozen error-enum owner and
  the exact variant list (frozen interfaces; arm shapes verified from
  source):
  * DIAG/OBLIG decode: db/diagnostic.f:123 and db/obligation.f:193 share
    exactly `malformed | noncanonical | bounds | duplicate |
    unknown-required`. That five-arm enum lives in the NEW shared file
    `maki/db/codec.f`, package `DBCODEC`; the two become
    `result<DIAG:diagnostic,DBCODEC:decode-error>` and
    `result<OBLIG:obligation,DBCODEC:decode-error>`. DIFFSUITE's
    decode-result (db/diff-suite.f:153, no duplicate, uses unknown) is a
    distinct taxonomy, retained unchanged.
  * The four presence slots (evidence/schema.f:118-130 `certify-slot
    golden-slot gradcheck-slot profile-slot`) and db/diff-runner.f:100
    ref-result: present/absent — become option.
  * Success-carrying families become result<ok, OWNER:error>, error enum
    owned by the declaring package, variants exact:
    art-result -> result<a, ARTIFACT:art-error = malformed |
      noncanonical | bounds | duplicate | unknown-required |
      kind-mismatch | unsupported-migration | digest-mismatch>
      (eight arms, maki/db/artifact.f:127-134);
    authz-result -> result<OBLIG:evidence, DAUTH:authz-error =
      not-discharged | unauthorized>;
    diagnostic build-result -> result<DIAG:diagnostic, DIAG:build-error
      = missing-owner | missing-reproduction>
      (maki/db/diagnostic.f:119-120);
    run-result -> result<n, DIFFRUN:run-error = faulted>;
    transaction result -> result<a, TX:tx-error = duplicate-write |
      omitted-read | malformed | bounds>;
    discharge-result -> result<OBLIG:evidence, OBLIG:discharge-error =
      wrong-subject | wrong-domain | wrong-relation | wrong-environment
      | wrong-verifier-class | not-independent>;
    commit-store keeps THREE CSTORE-owned error enums — the source
      narrows reachable errors by protocol phase and a superset would
      widen authority: commit-result -> result<a, CSTORE:commit-error =
      conflict | duplicate-write | omitted-read>; auth-result ->
      result<a, CSTORE:auth-error = conflict | duplicate-write |
      omitted-read | unauthorized | exhausted FIELD dim BUDGET:dim>;
      commit-discharge-result -> result<a,
      CSTORE:commit-discharge-error = conflict | duplicate-write |
      omitted-read | unauthorized | exhausted FIELD dim BUDGET:dim |
      not-discharged | unauthorized-verifier>;
    attenuate-result -> result<a, CAPTOK:attenuate-error = escape-cap |
      escape-budget FIELD dim BUDGET:dim>;
    diff-case load-result -> result<a, CASESTORE:load-error = absent |
      malformed | mismatch>;
    diff-suite build-result -> result<DIFFSUITE:suite,
      DIFFSUITE:build-error = incomplete | tolerance-mismatch |
      reference-not-independent>;
    register-result -> result<a, ACTION:register-error = incomplete |
      conflict>;
    seal-result -> result<a, RUN:seal-error = incomplete>;
    objective-result -> result<RUNMETRIC:objective-metric,
      RUNMETRIC:objective-error = not-training>;
    competitive load-result -> result<CEVID:evidence, CEVID:load-error
      = absent | malformed>.
  * Designed unions, retained (no success payload to carry, or nullary
    success): verify-result (audit-log.f:96; error arms carry idx),
    loop-result (agent-loop.f:89), budget-result (budget-ledger.f:57),
    dispatch-result (action.f:157; nullary accepted — a result<f,...>
    would invent a meaningless payload).
  One leaf per family. Sequencing: only shared-file/shared-type clusters
  order — maki/db/codec.f (DBCODEC) lands before the DIAG and OBLIG
  conversions, and the three CSTORE results convert together; every
  other family leaf runs in PARALLEL. All M13 clusters blocked-by E11
  only (CAD-KIND:id-error is unrelated); the DBCODEC-first and
  CSTORE-together orderings are internal to their clusters.
- M14a FS: `STRUCTURE path` = `data ptr u8`, `len CAD-NUM:byte-len` —
  borrowed, never retained. Probe: FS:path resolves; path consumers
  load. blocked-by E11.
- M14b SAFET: `SAFET:file` is the RENAMED linear census owner;
  `SAFET:RELEASE ( file -- )` consumes it exactly once. Probe: census
  spellings E-UNDEFINED; the file chain loads. blocked-by M11c.
- M14c GPT2 config: `GPT2:config` fields exactly `datatype
  MAKI:datatype`, `nctx n`, `nvocab n`, `nlayer n`, `nembd n`,
  `nhead n`, `tied f`, `bos n`, `eos n`, `ln-eps r`, `attn-scale f` —
  single-arch, flat. THIS LEAF deletes cfgkey, the arch wrapper, and
  cfg-proof (owning those deletions; M10e narrows to layer-proof).
  `GPT2:BUILD` validates and owner-constructs it (CONSTRUCT owner).
  `HFCFG:OPEN-GPT2 ( FS:path -- result<GPT2:config,n> )` is the exact
  borrowed-path boundary. Probe: MDLCFG spellings E-UNDEFINED; the
  embedded pinned-config fixture builds. blocked-by M11b, M14a.
- M14d GPT2 tensors: `GPT2:layer-id` is the one-cell checked layer
  index, owner-constructed only after range validation against
  `GPT2:config`; `GPT2:tensor-id = global(global-role) |
  layer(layer-id, layer-role)` with the four global roles (wte, wpe,
  lnf-g, lnf-b) and thirteen layer roles (ln1-g ln1-b mask qkv-w qkv-b
  aproj-w aproj-b ln2-g ln2-b fc-w fc-b mproj-w mproj-b). THIS LEAF
  deletes the cfgkey/layer-proof coupling and owns the exact
  constructor/slot closure. Probe: GPT2TENSOR spellings E-UNDEFINED;
  slot bijectivity fixture loads. blocked-by M14c.
  DEVRT runtime and INFER model types are explicitly LATER, unfrozen
  product contracts — the owner names reserve nothing; their types
  freeze in their own design rounds after wave 1.
- M15 declarer test suites rewritten for the new grammar (exact counts):
  type-decl-suite.f 69, type-ctor-suite.f 33, type-match-suite.f 17,
  type-family-suite.f 8, layout-buffer.f 6, engine-suite.f 5,
  deftype-suite.f 4, field-proj-suite.f 4, cast-suite.f 3,
  type-linear-suite.f 3, layout-defer.f 2, deftype-dup-bad.f 2,
  cast-negative-suite.f 1 — one leaf per file, old-grammar rejection
  fixtures added, deftype suites become carrier-form suites. blocked-by
  M7 (engine tests; independent of maki).
- M16 remaining test/tools declarations, ONE LEAF PER NAMED FILE
  (exact): test/extent-product-
  test.f 5, extent-substrate-probe.f 5, bootstrap-wide-memory-src.f 2,
  bootstrap-wide-interpret-src.f 1, bootstrap-wide-tick-src.f 1,
  typed-storage-test.f 4, cad-kinds-test.f 3, owner-wid-role-swap.f 2,
  enum-decl-suite.f 1, type-layout-lower-pending.f 3,
  type-field-owner-suite.f 1, layout-valid-product-bad.f 1,
  lower-cert.f 1, ptx/rep-neg-test.f 1, tools/ptx/autotune-sweep.f
  `census` 1, tools/public-signatures-test.f 3, db test fixtures
  (promotion-authority-test.f 1). blocked-by M15.
- M17 completion gate: with old forms already deleted in phase 1 (hard
  cutover), this leaf is the FULL-TREE load — the frontier reaches the
  end — plus the rg zero-site census and ENGINE REFRESH #3. blocked-by
  M13, M14a-d, M16.
- (docs reconciliation moved to phase 4, after everything — Joel.)
- M18 lint-expectation mechanics: refine-lint seed table and
  suite-coverage expectations updated for deleted/respelled words —
  these are green-tree mechanics and MUST precede the battery (stale
  seeds red the lints). blocked-by M17.

Phase 3 runs after M18. Leaf contracts freeze one at a time on the
blackboard as their predecessors complete; this list is the map, not the
frozen contracts.

35. **The declaration event/transaction apparatus is deleted.** [wave 1,
    engine batch] (Joel, 2026-07-30: no transactions in a single-threaded
    compiler.) The decl-event / declaration-transaction / coordinator
    layer — including the published event log the audit proved nothing
    ever consults — is removed. What stays is the bare savepoint: save
    the dictionary pointer, code pointer, and registry watermark; restore
    on a rejected candidate. That is the whole mechanism reject-and-
    continue testing needs (a fresh process per candidate would recompile
    the boot prefix hundreds of times per suite), and it keeps no
    transaction vocabulary.

### Phase 4 — docs reconciliation, at the very bottom (Joel)

Only after the battery is green and the landing is done:
docs/type-system.md rewritten against the converted tree with the same
probe discipline as its first writing; STATUS.md; this plan gets its
per-item completion stamps. Documentation reconciles once, when
everything is finished — not during.
