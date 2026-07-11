---
title: Audit trusted-inventory classification to row granularity
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T10:30:00.000000+02:00"
---

TRUSTED.md's trusted-inventory-classes block is hand-curated at FILE granularity: one `file class dot` row classifies every trust site in that file, and this dot is the placeholder owner of ~58 rows. Refine it to row granularity: (1) replace each file-level row with `file:name` rows carrying the honest per-site class (builder-emit, stdlib-boundary, test-metaprog, prim-axiom, discharge-candidate), keeping a file-level row only where every site in the file genuinely shares class and owner; (2) reassign ownership from this audit dot to the real capability/discharge dot for each site - discharge-candidate sites each need a discharge dot, checker-capability sites the matching capability dot (hook installs are already row-granular under habu-police-set-check-850bc543); (3) keep `bin/hb --load tools/trusted-inventory.f -- strict` green throughout - it fails on unclassified sites and on owning dots missing from .dots/. Done when no row's owner is this dot.

## Progress (partial)

Row-granularity refinement started on the two big uniform files:
- src/core/roles.f: the single file-level row is replaced by 34 `file:name`
  prim-axiom rows (>IDX/IDX>N ... >SNAP/SNAP>N), one per nominal-cast site.
- test/prop-test-core.f: 15 `file:name` test-metaprog rows for its TRUSTED
  fixtures. The file-level row is retained because the two `0 set-check`
  boundaries have a space in their site name and cannot be a `file:name` key
  (CROW-PARSE splits the row on whitespace); PROP-CHECK-HOOK stays under
  habu-police-set-check-850bc543.

tools/trusted-inventory.f strict mode now emits a `by-file` line per source with
its non-zero per-class site counts (CLASS-BY-FILE-REPORT), covered by FIX-BY-FILE
in tools/trusted-inventory-test.f. Ratchet counts are unchanged.

Increment (prim-axiom class fully re-owned): all 37 `prim-axiom` rows that sat on
this placeholder — the 34 `src/core/roles.f` nominal-cast converters plus the
engine-primitive TRUST rows in `src/core/structures-effects.f` (CELL/+FIELD/…),
`tools/check-core.f` (CHECKER-DEFTYPE/SCOPE/…), and `src/core/include.f`
(INCLUDE-MMAP-PTR, INCLUDE-EVALUATE) — are reassigned to their real owner
`habu-primitive-effect-axiom-1119f176` (the audited axiom table whose mandate is
exactly this class; the 5 prop-test-core AX-* rows already lived there). The
whole `prim-axiom` class (42 rows) is now off the placeholder. Evidence: each
reassigned site is an engine-primitive TRUST row / nominal identity cast the
checker treats as an axiom, i.e. axiom-table scope, not a discharge candidate.
`strict`, the derived ratchet, and the full gate stay green.

Dot stays OPEN: 68 placeholder rows remain (`builder-emit` 34, `test-metaprog`
23, `stdlib-boundary` 10, `discharge-candidate` 1). Its done-criterion ("no row's
owner is this dot") is not met until each is reassigned to a real owner, which
needs per-site domain judgment and, for several classes, a correctly-scoped
capability/discharge owner (builder emitters vs raw-layout axioms differ;
`habu-builder-trust-rows-c5d41af6` owns the dischargeable builder emit effects,
`habu-checker-capability-typed-e0c76a02` the ptx tile sites,
`habu-typed-depth-introspection-18f0efda` the depth-capture test-metaprog class).
Do these as further bounded, evidenced increments — do not bulk-guess owners.

## Increment 2026-07-11 (row granularity complete; fold ratchet)

AUDIT (from the live TSV, 659 sites): 47 placeholder folds covered 510 sites;
15 more folds with real owners covered ~70. Separability: 55 folds were fully
nameable; 7 folds each held exactly one unnameable `0 set-check` site; zero
stale rows (strict was green: no dead/unmatched rows existed).

DONE: all 59 separable folds (except the two contested files) split into 427
`file:name` rows — class and owner UNCHANGED per row (granularity only; no
owner guessing). The 7 set-check files keep count-1 residual file rows.
Duplicate site names carry explicit counts (test/engine-suite.f:T-RDF 2).
Trust surface identical: same 659 sites, same classes per file (by-file lines
unchanged), strict + derived ratchet + baseline mode green.

SKIPPED BY DESIGN: `src/habu/habu2.f` (122 sites) and
`test/type-layout-lower-pending.f` (4) — contested under the wide-ADT stack;
their per-name rows would go stale on that merge. Split them when ownership
releases, lowering the fold-baseline in the same change.

RATCHET: tools/trusted-inventory.f strict now computes the separable-fold
count (a file-level row whose matched sites are all nameable), prints
`separable fold(s) N (baseline M)`, and fails when N exceeds the committed
`fold-baseline` directive (TRUSTED.md block head, currently 2); a missing
directive is a strict failure. Red-first proven: re-folding a split file ->
rc=81 with per-fold detail; deleting the directive -> rc=81 named failure.
CMAX 512 -> 1024 (block now ~525 rows); CTAB gains K-UNNAME.

REMAINING (ownership, unchanged scope): 409 placeholder-owned rows now at
word granularity await per-site owner reassignment (builder-emit ~210 named +
habu2 fold, test-metaprog ~95 named + residuals, stdlib-boundary PTX/engine-id
~71 named, discharge-candidate 4) — per-site domain judgment, further bounded
increments per the rules above.

## Increment 2026-07-11b (discharge-candidate class resolved)

The 4 `src/core/combinators.f` rows (TIMES/EACH/MAP/FOLD, combinators.f:20-34)
are NOT dischargeable today: each re-executes a stored quotation per loop
iteration (`r@ execute` / local-`q execute` inside `?do`), which types only
under the multishot-quotation capability — the file's own boundary comment
says exactly this and names the owner. Reassigned to
`habu-multishot-quotations-typed-8832cace` (whose text lists these words) and
re-classed `discharge-candidate` -> `stdlib-boundary` (the class definition is
"believed checkable today", which the evidence contradicts; the tile-rows
precedent classes capability-blocked library boundaries as stdlib-boundary
owned by the capability dot). BI/TRI in the same file are already plain
checked definitions — no rows. Zero placeholder discharge-candidate rows
remain.

## Increment 2026-07-11c (PTX + engine-id stdlib-boundary owners)

All 69 lib/ptx placeholder rows reassigned to
`habu-ptx-phantom-preserving-3df9db92` — the dot that defines this exact
surface's split and end-state. Per-site signature evidence:
- MINT CORE (11, permanent typed-DSL entry casts): lib/ptx/cg.f:77-85 the nine
  `*-REG` words (`n -> span/uniform/ptr/matrix<...>` register mints) plus
  cg.f:93 `R>BITS`/cg.f:134 `BITS>R` (`r <-> n` bit-casts).
- WRAPPERS (58, retire when phantom-preserving lands): every TRUSTED row in
  lib/ptx/tile.f (31, tile.f:22-112), lib/ptx/collective.f (18,
  collective.f:28-94), lib/ptx/tile-v4.f (9, tile-v4.f:11-35) is typed->typed
  (span/tile/matrix/uniform/ctx in AND out; the sole nullary, collective.f:28
  ROW `-- rowidx<e>`, is a thread-state intrinsic read) — trusted only because
  kernel newtype phantoms cannot thread through the checked EMIT-* words, the
  dot's own definition of its retire list.
The mint/wrapper distinction stays recorded here and in that dot (classes do
not encode it; both halves are stdlib-boundary).

lib/engine-id.f's two rows (ENGINE-SELF-MACOS engine-id.f:44, apple[] startup
vector walk; ENGINE-SELF-LINUX engine-id.f:59, /proc/self/exe readlink) go to
the newly minted `habu-raw-self-path-4514ffd3` — no existing capability dot
covered raw startup-image/self-path reads (the block prose said exactly this);
src/os/env-base.f's same-class sites stay on the placeholder under
builder-emit until that increment.

Placeholder remainder after 2026-07-11b/c: builder-emit (~210 named + the
habu2 fold) and test-metaprog (~95 named + set-check residuals) only.

## Increment 2026-07-11d (builder-emit cluster resolved; 3 discharges)

DISCHARGED (commit A, proven by probe + rewrite + full gate): CODE-BYTE+
(icode.f:49), CRH-BYTE+ (crash.f:9), XREF-REC+ (xref.f:19) — plain
pointer+offset arithmetic; `( ptr u8 n -- ptr u8 ) +` and
`( ptr a n -- ptr a ) +` certify today. TRUSTED: -> : ; rows retired from
both the classification block and the manifest (trust surface 496 -> 493).

REASSIGNED (commit B):
- 30 startup-image/argv raw-read rows -> habu-raw-self-path-4514ffd3:
  src/os/env-base.f (19: ENV-DATA/ENV-DASH/ARGC/ARGV-BASE and the envp/apple
  walkers — the same startup-vector reads the dot was minted for),
  src/os/script-argv.f (7) and src/habu/bundle-argv.f (4) (SCRIPT-ARG-START/
  SCRIPT-ARGC/SCRIPT-ARGV$ etc.: argv-vector views over the same startup
  image).
- 186 builder/engine rows (every remaining builder-emit named row plus the
  deliberately-held src/habu/habu2.f fold) -> habu-builder-trust-rows-c5d41af6:
  the dot's own mandate is the file-by-file TRUST->CHECKED conversion of the
  builder emit/cast surface (~307 at its mint = this whole class), it already
  owned three habu1.f rows, and its text names habu2.f as start-after-merge —
  matching the fold hold. Evidence shape per file: raw-pointer casts into
  engine records (AOT-A>U8, BFR-N>REC, XREF-N>U8), raw region accessors
  (CODE/CRH/STB@/SBUF@/HB@/MK-SBUF@), asm/signal-frame emit entries
  (c-crash-*/c-prof-*/fold-entry/vop-entry/fprim), engine eval/check
  entrypoints (EVAL-HOST/CHECK-BODY/JIT-EVALUATE), image-layout casts and
  constants (LINUX-VA>PTR/MACHO>N-PTR/IMAGE-TEXT-*/SNAP-EXTRA-*), image
  dumper mmap mints (IMGD/IMG-MMAP-PTR).

Placeholder remainder: test-metaprog only (~95 named rows + the set-check
residuals) — increment 4.

## Increment 2026-07-11e (test-metaprog cluster resolved — DONE-CRITERION MET)

Wave E-1 verdicts (commit A): TASK-NULL DISCHARGED (lib/task.f:107, body
`NULL$ drop` — the out-var pointee binds; probed `( ptr u8 n -- ptr a ) drop`
certifies; trust surface 493 -> 492). NOT dischargeable, with probes: BP-NULL
and NULL$ (a null-literal mint `( -- ptr u8 ) 0` still rejects, rc=70);
ENV-FALSE (trivially-checked body but env-base.f loads in the unchecked boot
phase — the TRUST republication is structural until staged checked boot);
c-defer-find-unset/c-defer-cell (contested habu2.f, inside the held fold).

test-metaprog reassignments (commit B, 113 rows, per-site evidence):
- tools/asm-src-test.f (12) + tools/image-bytes-test.f (22) -> builder-trust
  c5d41af6: republications of the asm-encoder / image-bytes surfaces; they
  convert in the same batch as their production emitters.
- test/gate-common-lib.f (6: UEND/USIGS-RESTORE-END/UTERM!/JSON-DIAGS/
  GE-EVAL-SOURCE[-ACT]), test/checker-assert.f:CHECK-QUIET-CANDIDATE!,
  maki/eval.f:CHECK-PASSES?, maki/cad.f:CAP-COMPILE-RUN (no reentrancy dot
  exists; nested-evaluate/check-entry family), tools/codegen-role.f
  CGR-EVALUATE + CGR-CHECK!, prop-test MARK/FORGET/SMARK/SFORGET/CHK-MARK/
  CHK-FORGET/CHK-HOOK/CHK-COMPILE-CERT/CHK -> prim-axiom 1119f176: engine
  check/evaluate entrypoints and dictionary/USIG high-water surgery, the same
  class as the check-core CHECKER-* rows already there.
- prop-test PROP-INSTALL-HOOK, codegen-role CGR-EVALUATE-UNCHECKED,
  engine-suite T-RDF (x2, the audited TRUST-override-path pin) and its
  `set-check` hook row -> police-set-check 850bc543 (hook installs and
  set-check machinery, the dot's own audited-hook mandate).
- prop-test CLEAR-MEAS/ERR@/RUN-MEAS/REND-SIG$/CONFIRM-FR? and
  lib/test/assert.f:TTHROWS-RAW -> typed-depth-introspection 18f0efda ("prop-
  test depth checks and assert-layer helpers" is its mandate verbatim).
- engine-suite: ES-PATCH32 -> capability-gate 14022ba9 (documented
  owner-of-record); T-PTX-*/T-MK-SPAN*/T-PTX-SAME-EXTENT (8) ->
  phantom-preserving 3df9db92; T-ASM-CODE/T-BUILD-*/T-CODESIG2/T-SNAP-EXTRA-*/
  T-LINUX-*/T-SPAWN-* (10) -> builder-trust c5d41af6; the checker-model
  fixtures (T-GIVE/NEED widening, T-*NODE nominal, T-MAKE/FREE-OWN linear,
  package/role tokens a:b:/x:/::x/tq:tail, T-CHECK-PASSES, P5 immediate/
  postpone, T-V14 etc., 27) -> prim-axiom 1119f176 (hand-asserted
  checker-model fixture effects, the AX-* difftest genus).
- The two 0-set-check residual file rows (engine-suite, codegen-role) ->
  seal-set-check b3676b33 (the prop-test-core precedent for test 0-set-check
  boundaries).

DONE-CRITERION MET: no classification row's owner is this dot (0 rows).
Remaining riders live elsewhere: the habu2.f fold (under c5d41af6) and the
test/type-layout-lower-pending.f fold (under 1d70acf7) split to file:name
rows when the wide-ADT stack merges, lowering fold-baseline 2 -> 0 in the
same change.
