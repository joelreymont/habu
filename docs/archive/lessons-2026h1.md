# Lessons archive — 2026 H1 campaign log

Verbatim originals of the dated campaign material distilled out of `LESSONS.md`
(size-review item 1). The durable, transferable rules now live, deduplicated,
in the topical sections of `LESSONS.md`; this file preserves the full detail and
provenance (dates, dot ids, falsification narratives, status) that git history
also carries. Nothing here was edited — it is the exact prior text.

Two parts follow: (1) the leading undated-bullet append zone that sat above the
topical sections, and (2) the dated `##` campaign sections that ran to the end
of the file.

---

## Part 1 — leading append zone (was LESSONS.md, above the topical sections)

- **The shared `result<a,b>` family instantiates with TWO DISTINCT nominal products
  (`result<pcap:captured,pcap:failed>`), and the checker enforces the payload types.**
  (2026-07-18, switchover wave B capture cluster, dot habu-switchover-wave-b-08482d5b.)
  Probed via CHECK-QUIET-CANDIDATE!: OK-with-cap and ERR-with-fail certify (-1); the
  swaps (OK given the fail type, ERR given the cap type) fail-closed (0); a MATCH whose
  arms leak mismatched product payloads rejects (0). The err payload being a nominal
  PRODUCT works because the signature DECLARES it concretely — the old "free err var
  won't unify with a nominal" limit only bites a TOTAL ok-only construction, not a
  declared two-product effect. The LESSONS note that a multi-field PRODUCT can't be a
  sum payload is STALE post-flat-multi-cell (commit 13f8a504); option/result carry
  products fine now.
- **A "capture" primitive's rc is a COMPLETION CODE, never an errno, and the captured
  lengths are valid on BOTH the ok and err arm.** (same dot.) OS spawn/wait failures
  THROW (E-PROC-SPAWN/E-PROC-WAIT) — no negative errno is ever returned; the returned rc
  is the child's own completion code (nonzero exit, or 128+signal). And callers read the
  captured stdout/stderr on the FAILURE path (tools/check-core.f CHK-HANDLE-HB emits the
  child's output when rc!=0 — linters/compilers-under-test exit nonzero with findings on
  their streams). So the census's `result<(outlen,errlen),errno>` model is wrong twice:
  the correct shape is `result<captured(out,err), failed(out,err,code)>` — both products
  carry the lengths, only the failure adds the code, nothing is a sentinel. Do NOT put the
  lengths in the ok arm only.
- **Inline `PKG-PROD:UNMAKE {: f:role ... :}` INSIDE a MATCH arm certifies** (same dot) —
  the "arm payload binds to a word param, not an in-arm local" rule is only about binding
  the PRODUCT ITSELF to a local; UNMAKE-then-bind-the-unpacked-FIELDS is fine inline. A
  result value also survives an intervening store op below it, so you can stash a constant
  buffer ptr (`err ERR-P !`) before MATCHing the result on the stack.
- **Per-file `X-CAPTURE>N ( len len rc -- n n n )` adapters are the migration chokepoint.**
  (same dot.) Dozens of downstream spawn tests funnel every call site through one such
  adapter; migrate the adapter to `( result<...> -- n n n )` (MATCH -> outn errn code, 0
  on clean exit) and its callers stay byte-for-byte unchanged. lib/process.f /
  process-argv.f / process-env.f are in the fixpoint INSTALL prefix but the migration is
  codegen-neutral (build-fixpoint never calls the RC-form words) so the fixpoint stays
  byte-identical; process-cwd.f / process-command.f are on-demand.
- **A new content-addressed CAD-KIND owner with a `TRUSTED: RAW>X`/`X>RAW` pair needs
  FOUR coordinated edits or a gate goes red — trusted-inventory strict and refine-lint
  each read a DIFFERENT registry.** (2026-07-18, maki/experiment/run.f run-id owner, dot
  habu-v2-experiment-run-7c1d1906.) The full checklist: (1) the one-line `TYPEFAMILY x-id`
  in maki/cad-kinds.f; (2) the human `| NAME | sig | desc | test | src | date |` row in
  TRUSTED.md for BOTH `RAW>X` and `X>RAW`; (3) the machine-readable classification-block
  row `maki/.../file.f:RAW>X prim-axiom habu-epic-model-cad-70b629a9` (and `:X>RAW ...`) —
  trusted-inventory strict fails "unclassified site(s)" on the *block*, NOT the human table;
  (4) add `RAW>X` (the mint direction only; the `X>RAW` projection is not mint-shaped) to
  the `RFL-SEED-NAME$`/`RFL-SEED-OWNER$` case tables in tools/refine-lint-core.f and bump
  `RFL-SEED#` — else refine-lint throws NEW-MINT. All four are the artifact/producer/config/
  journal precedent; grep an existing owner (e.g. RAW>CONFIG-ID) across the tree to find
  every site.
- **A verdict-fixture (`CHECK-CANDIDATE!`) candidate string compiles in the CURRENT package
  context, so reopening the owning package lets the candidate name that package's public
  types/words BARE.** (same dot, run-metric-test.f static leg.) `package RUNMETRIC` then
  `s" ( report-metric -- ... ) AS-OBJECTIVE" VCHECK 0 T=` certifies the reject with bare
  names (the cad-kinds-test `content-digest` bare-type precedent); qualified `PKG:` also
  works. Two distinct single-field `PRODUCT`s (report-metric vs objective-metric) never
  unify, so "held-out metric as a training objective" is a compile-time reject — the
  artifact.f weight/kernel flat-family pattern, now the metric-population separation.
- **Width-aware construct/MATCH lowering (layout-cap slice 4) is an EXTRA-PAD fact,
  not a full re-lowering.** (dot habu-checker-capability-layout-9b8540bd.) A
  generated constructor body already emits `declared_pads 0s + tag` and leaves the
  payload untouched, so a wide instantiation needs ONLY `instantiated_pads -
  declared_pads` extra zero pushes — the reserved `construct` and `MATCH OF` legs
  add them to the declared pad count, and the generated-ctor CALL pushes them
  before its `BL`. The checker records the fact only when the delta is positive, so
  every cell/arity-0 family emits no fact, stays pass-1, and is byte-identical
  (proved by AOT cmp of a non-wide program). Because the delta can be 1 (not
  "wide"), the fact needs its OWN flag (`WF-XPAD-FLAG`) to trigger `NEEDS-P2`;
  reusing the width>1 test would silently drop 1-pad cases.
- **Adding a WF-cert flag ripples through FIVE places, and the cert VALIDATOR bites
  first.** A new WF flag (value 4) needs: `lower-cert-base.f` constant, a
  `PPRIM: LOWER-CERT ...` checker model, the `VALIDATE-WF` flag mask (was `& 3`,
  hardcoded to fetch/store) AND its `has-wide`/`NEEDS-CELL` accounting, the
  `primitive-effect-inventory` manifest in TRUSTED.md, and the `prop-test-core.f`
  AX-CENSUS `AX-GEN-LIST` (an unclassified axiom fails the census). First symptom
  of the missing validator branch was `hb: malformed lowering certificate`, not a
  checker miss — the checker recorded the flag fine; the emitter's cert gate
  rejected it. The pass-2 legs also need a found-vs-missing bit from the width
  lookup (missing returns width 1, which a construct leg can't distinguish from a
  real 1-pad fact) — add x11 to `EMIT-WIDTH-LOOKUP`; existing callers reload x11 so
  it is inert for them.
- **The maki cad-replay + candidate-validation "baseline" reds are the SAME
  parent!=child engine artifact, not a regression.** cad-test.f's replay child
  hardcodes `s" bin/hb"` and the CAD store key includes the engine binary hash, so
  running the suite through ANY candidate != the installed bin/hb misses the
  rehydration (F100/F101). Proof it is not the change: cad passes through a
  byte-identical-to-bin/hb candidate; it fails through every slice-4 candidate.
  Both reds clear when bin/hb is refreshed to the new fixpoint at integration
  (identical to slice 3). Do NOT bisect emitter changes for it — check parent vs
  child binary first.
- **FS-SKIP-DIR? skipped .jj/.git/.dots but never .jj-ws, so every walker (lints,
  candidate discovery) saw stale worker-workspace trees — a MAIN-CHECKOUT-ONLY latent
  red.** (2026-07-18, found by the first main-checkout run.f in days: 478
  process-primitive-lint findings and a candidate-validation fail, all from
  ./.jj-ws/<old-lane>/ copies; worker gates never see it because workspace trees
  contain no .jj-ws.) Fixed at the root (lib/fs.f skip list). Rules: (1) new
  conventional untracked directories must be added to FS-SKIP-DIR? when introduced;
  (2) the integrator periodically runs run.f in the MAIN checkout too - it is a
  distinct gate context from worker workspaces.
- **A spawn-isolated subject signals SUCCESS by natural load completion (exit 0),
  never `bye`.** (diff-runner, dot habu-v2-differential-runner-13359019.) `bin/hb
  --load driver.f` that ENDS with `bye` exits **70** (the FFI-file convention code
  eval-device.f documents: it ignores the emit rc and reads captured stdout); a
  driver that just prints its result and lets `--load` finish exits **0**. Any
  spawn taxonomy where the exit code IS the fault signal (produced iff exit 0)
  must have its subject driver complete naturally; `die`/`throw`/hang still fault.
- **`WRITE-ALL ( path-ptr path-len data-ptr data-len -- )` is PATH first, DATA
  second** (verified via `FS-WRITE-BY-FLAGS ( pa pu src u flags )`). Reversing the
  pairs opens the DATA bytes as a path -> `E-FS-OPEN` (-2102). `MAKI-GRADE`'s
  `GRADE-WRITE-DRIVER`/`GRADE-EMIT` are the correct-order precedents.
- **Copy a shared-builder (`SB`) span to a stable buffer before calling anything
  that may reuse `SB`.** `MAKI-GRADE:PREPARE` (via JOIN-PATH) reuses the shared
  builder, so a source word that returns `SB$` and then hands it across `PREPARE`
  reads clobbered bytes. Copy `SB$` into an owned buffer first (the spawn adapter
  copies its source into `SP-SRC` before `PREPARE`/`WRITE-ALL`).
- **A MATCH-arm payload binds to a WORD PARAMETER of its type, not to an in-arm
  typed LOCAL.** `ok OF {: d:PKG:product :} ... ENDOF` fails to certify (`expected:
  pkg:product<> actual: pkg:product<>`), but `ok OF SOME-WORD ENDOF` where
  `SOME-WORD ( PKG:product -- ... )` certifies (the diagnostic-test `ok OF DIAG>`
  precedent, and a declared word param with the SAME product type works). Factor
  the arm body into a word taking the payload as a parameter.
- **The maki suite table `ITEM-MAX` is now 256 (was 128).** `lib/test/suite.f`
  was bumped, so a couple of extra `TEST:SUITE` lines in `maki/test.f` (131 live)
  no longer risk `E-TBL-BOUNDS`; the earlier "aggregate to dodge 128" note is stale.
- **An unforgeable value that CARRIES data uses the refined-nominal-HANDLE
  pattern, not a PRODUCT.** (capbud, dot habu-v2-capability-and-0970a96d.) Probed:
  a `private` PRODUCT has NO construction surface even for its owner
  (`E-UNDEFINED: FAMILY:UNMAKE`, docs §9.4); a `public` PRODUCT's generated
  `FAMILY:MAKE` ACCEPTS a raw n (`( n -- pkg:prod ) PKG-PROD:MAKE` -> verdict -1),
  so a public handle is forgeable-by-alias (fine for a txn handle whose authority
  is only a pool slot, UNSAFE for a capability grant where aliasing slot 0 = root).
  The sound pattern for a value carrying authority: an arity-0 `TYPEFAMILY` over an
  append-only pool, minted ONLY through a private `TRUSTED: RAW>X`/`X>RAW` pair (the
  maki/db/action.f `RAW>ACTION-ID` precedent), pool holds the authority. Statically:
  a raw n where the nominal is required is verdict 0 (forge reject); the private
  mint is verdict 1 (sealed) cross-package; `=` on the value is verdict 0. Confirm
  a package-local `TYPEFAMILY` type is usable in a cross-package public signature
  (it is) before building on it.
- **The `( ... )` stack comment on a `:` IS the checked signature.** A word doing
  `c!`/`c@` on a passed buffer pointer must declare `ptr u8` (not bare `ptr`) or the
  checker rejects at the locals `:}` with `expected: ptr a n actual: ptr n`
  (verbatim `LE-PUT` copied with a `( ptr n -- )` comment failed; `( ptr u8 n -- )`
  fixed it). Parameterized result types (`foo-result<CAPTOK:grant>`) also cannot be
  a `{: :}` local type — specialize the word or keep the value on the stack.
- **The maki suite table is near full (lib/test/suite.f `ITEM-MAX=128`, ~126
  live).** Adding several `*-test.f` `TEST:SUITE` lines overflows it -> `E-TBL-BOUNDS`
  (`-3000`) thrown at LOAD with no suite name printed (stdout lost on the throw).
  lib/* is off-limits, so AGGREGATE the concern suites into ONE maki/test.f entry
  that `require`s them (each keeps its own package + `T-RESET`/`T-REPORT`; a failing
  child `die`s, so the aggregate fails closed) — one concern per test file, one
  suite-table slot.
- **refine-lint's NEW-MINT shape scan only covers `CAD-KIND:`/`MIR:` outputs.** A
  package-local nominal mint (`RAW>GRANT ( n -- CAPTOK:grant )`) is NOT auto-flagged,
  but seed it anyway (name+owner) for confinement, per the `RAW>TENSOR` precedent.
  Closing an audited refinement pair is THREE synced edits: the TRUSTED.md manifest
  row (Site+Tests), the `file:WORD prim-axiom <epic>` classification line, and the
  refine-lint seed (name+owner, bump `RFL-SEED#`). refine-lint was already RED on
  master (`RAW>ACTION-ID` had a row + classification but no seed) — a CAD-KIND mint
  MUST be seeded or NEW-MINT fails.

- **`test/type-decl-suite.f` arms ONE diagnostic swallow buffer at the top
  (`TDIAG-BUF 8192 DIAG-BUFFER!`) and every expected reject APPENDS its rendered
  diagnostic without a reset until the first inspection; the accumulation was
  sized to just fit.** (TFAM-16 niche/recursive lane.) Adding 2 reject fixtures
  overflowed it → `render: diagnostic buffer full` `76 die` mid-suite — a FATAL
  abort, not a catchable throw (the render happens before `TDT-NEG`'s `catch`).
  The same declaration renders fine in isolation because `RDIAG-ON=0` there routes
  to fd 2 (uncapped). Fix = re-arm the swallow buffer (`TDIAG-BUF 8192
  DIAG-BUFFER!`) at the start of the growing reject section, matching the suite's
  own re-arm-per-section idiom; do NOT just bump the cap. When adding reject
  fixtures to an accumulating-buffer suite, re-arm first.
- **Boxed/niche-null are still reject-until-supported by design; ship the sound
  SHARED infra without the accept.** (TFAM-16.) `TFAM-WIDTH@` now reports W=1 for
  `TL-BOXED`/`TL-NICHE` (docs §18/§22.3), reached only via the `TFAM-LAYOUT!`
  mutator (no declaration accepts either — both reject at the `POLICY` clause), so
  it is check-sound metadata the accept slices consume, exactly like `PACKED-DESC`
  landed before its accept-flip. Niche's `some` payload MUST be a `nonnull-ptr<a>`
  (a plain `ptr` could be null and collide with `none`), and that refined type does
  not exist yet, so NO family qualifies today — structural recognition is
  necessary but not sufficient; the honest niche deliverable is validation +
  reject-until-supported, not a half-predicate. Recursive sums reject the same
  under stack-cell-tag AND packed (both keep the cell width); mutual recursion is
  fail-closed via the forward-reference restriction (a payload may reference only
  an already-declared family), so no schema cycle walk is needed until boxed
  accept admits forward refs.

- **Checked Habu has NO fsync/fdatasync/dir-sync primitive; the durable-write surface is
  atomic rename only (`RENAME-FILE`/`ATOMIC-WRITE-FILE` = write-temp + rename).** (atxn lane,
  crash-safe commit slice, dot habu-v2-atomic-txn-a3c26066.) Rename gives ATOMICITY and
  correct PROCESS-crash recovery — the page cache survives process death, which is exactly
  the model the spawn-a-child acceptance exercises — but NOT power-loss / kernel-crash
  durability. The commit store makes the single `HEAD` rename its linearization point
  (recover-old-or-complete-new) and reports the fsync gap as the remaining native capability
  rather than adding host code or trust. When a durability primitive is missing, dot it and
  bound the guarantee precisely; do not fake it.
- **`TX:PROPOSE` interns a revision by the SHA-256 DIGEST of its content, so
  `REV:KEY>WIRE(rev) = SHA-256(SHA-256(revision content))` — a DOUBLE hash.** (same lane.)
  A content-address integrity check on a stored revision object (file bytes = the raw
  content) must therefore hash the file once to get the content digest, then hash THAT to
  match the rev key. A single hash silently fails the integrity check (4 tests red, no type
  error) — the digest-vs-content boundary is invisible to the checker.
- **The general V2 content-addressed OBJECT STORE did not exist yet; `maki/store.f` is a
  different concern (append-only schedule/evidence ROWS keyed by the section 7.4 schedule
  key), not a revision/object store.** (same lane.) The crash-safe commit dot therefore
  landed a MINIMAL file-backed store (`maki/db/commit-store.f`: one file per revision + a
  HEAD marker + an idempotency record) as part of the slice, per the plan's "over the V2
  object store" — decide store-existence from the plan's own sections, not the filename.
- **Checker edges hit building CSTORE: (1) a word consuming a PARAMETERIZED sum needs the
  concrete type arg in its signature — `( commit-result<CAD-KIND:rev-id> -- n )`, not bare
  `commit-result` ("wrong arity for type family"); (2) a pointer LOCAL's element type flows
  from a `ptr u8` INPUT annotation — a bare `ptr` input makes `{: kp:ptr :}` fail with
  "'ptr' needs an element type". Annotate byte-pointer args as `ptr u8` in the stack effect.**
- **A worker editing the MAIN working copy instead of its .jj-ws workspace is a recurring
  failure mode (twice on 2026-07-17: bfeed self-caught, evid caught by the integrator's
  gate).** Detection: the orchestrator runs `jj st` in the main workspace and requires it
  CLEAN before gating any lane's stack there — an unexplained modified file during
  integration is presumed lane leakage: preserve it to a patch, message the lane, restore
  pristine, re-gate. Worker briefs must include the absolute-path check ("every path starts
  with .jj-ws/<lane>/") and a pre-seal `jj -R <main> st` clean assertion.
- **The CPPSLOT typestate threads through the single-buffer MMA-PIPE-KLOOP-SINGLE
  byte-identically and GENUINELY — but only via one new trusted issue-mint, and that
  mint is the sanctioned audited-mint-core class, not debt.** (dot
  habu-wire-cppslot-typestate-ce2463df, cppwire2 lane, host, no device.) Unlike the
  fixtures — which take `cpp-pending` from their SIGNATURE (trust-neutral) — the
  production loop mints a fresh slot PER ITERATION at the in-body cp.async issue, and
  a checked word cannot fabricate a nominal family cell (`( n -- cpp-pending<p> ) 0`
  rejects `expected cpp-pending actual n`). Two-horned: keep the loop checked =>
  external trusted mint; make the loop trusted to host the mint => unchecked body =>
  hollow thread. Sanction rationale (coordinator, on the NP-MINT-CHECK precedent):
  fresh phantom mints are trusted BY DESIGN — a per-iteration `cpp-pending<p>` mint is
  exactly the audited-mint-core class, identical in kind to the CPPSLOT COMMIT/WAIT
  transitions; the dot's reshaped acceptance is "audited mint core + checked
  protocol", so +1 one-line mint (`MMA-STAGE-ISSUE ( n n -- cpp-pending<p> )
  MMA-CP-STAGE 0`) buying fail-closed protocol enforcement on the shipping best
  kernel's path (MMM-WIDE-B-M4-S1, stages=1) is a net soundness win. Falsification
  discipline that separates this from a hollow gesture: (1) fail-closed proof first
  (a stray int in the exact production word exits 70); (2) misorder probes on the
  PRODUCTION word (wait-before-commit, dropped wait/sync both reject); (3) a
  committed read-after-issue negative that PINS the mint's pending state — if the
  mint is ever laundered to return ready, the negative certifies and the suite fails;
  (4) 21-kernel off-device golden dump byte-identical before/after, twice. The
  double-buffer path stays refuted (cross-slot commit/wait pairing, entry above);
  single-buffer is the one place the single-slot lifecycle is real.
- **Migrating a fixed-width scalar SET to 32-byte content-key elements: move BYTES, keep
  concrete-per-buffer sorts, guard with a CKEY comparator + a one-element insertion
  scratch.** (dot habu-wire-content-key, keywire2/item-3: the v1->v2 envelope migration.)
  The dependency/source-revision sets went from 8-byte cell elements to 32-byte content
  keys. A 32-byte element can't ride the data stack, so the insertion sort copies the
  element being placed into a `DKEY`/`SKEY` scratch and shifts with `BYTE-COPY`; ordering
  uses a fixed-width `CKEY-LT?`/`CKEY-EQ?` byte comparator (the transaction.f `BYTES<`
  precedent narrowed to `CKW`). The concrete-per-buffer rule still holds (byte access on a
  `ptr` local is fine, but the buffer touch stays per-buffer: DSCR vs SSCR). The decode
  ascending/dedup check replaces the `-1` raw sentinel with a `k>0` guard (a byte string
  has no smaller-than-all sentinel). Store the wire BYTES (not the nominal) so decode ->
  re-encode is byte-identical; validate each element resolves via `WIRE>KEY` fail-closed.
- **Habu stack-effect comments are CHECKED types, and use TYPES not local names; a
  create-buffer element address is `ptr u8`, never bare `ptr`.** `: DEP-AT ( s k -- ptr )`
  fails `'ptr' needs an element type`; write `( s k -- ptr u8 )`. And `( pa pb -- bool )`
  fails `unknown type 'pa'` — the signature must read `( ptr u8 ptr u8 -- bool )` even
  when the locals are `{: pa:ptr pb:ptr :}`. A loop that `exit`s mid-body must `drop` the
  loop index before pushing the result, or the return arity mismatches.
- **A friend that BUILDS an envelope from outside package ARTIFACT needs the set-builders
  public.** `BUILD-WEIGHT`'s doc names `DEPS-RESET/DEP+`/`SREVS-RESET/SREV+` as the build
  protocol, but they were private, so only code reopening the package could add deps. The
  cross-process ENVELOPE test (its own package) needs them, so they became public (the
  scratch address helpers `DSCR-AT`/`SSCR-AT` stay private). Reopening the package (as
  artifact-test.f does) is the alternative, but it pollutes ARTIFACT with spawn scaffolding.
- **FILEMAP.md sat ~128 bytes under filemap-lint's `FM-BUF-CAP` ($20000); any addition
  overflowed the lint's read buffer** (`lint: file exceeds buffer: FILEMAP.md`). Two lanes
  hit it the same day: the cppslot landing bumped `FM-BUF-CAP` to $40000 (loud-fail
  preserved, test green), and the keywire2 lane routed around it before the bump merged —
  its xproc-env FILEMAP entries were added at integration once the cap fix landed.
- **When the shared maki suite-table fills, raise `lib/test/suite.f` `ITEM-MAX`
  (loud-fail preserved) rather than aggregating unrelated suites into one entry.**
  (dot habu-raise-maki-suite-e1b07544.) It is the FM-BUF-CAP pattern again: a
  fixed cap sized for a statically-known registration set, grown by constant with
  the `E-TBL-BOUNDS` wall kept. The capbud lane first squeezed under the 128 cap by
  aggregating four concern suites behind one `require`-bundle entry; that couples
  concerns and hides the wall. `ITEM-MAX` is only referenced inside suite.f (all
  tables `allot` from it), so bumping it (128 → 256) is fully contained. The bound
  is a real regression: fill the counter to the cap and assert the next
  `ITEM-ALLOC` throws `E-TBL-BOUNDS` (probe the live counter, not a parallel table).
- **Cross-process id identity needs a CONTENT-key wire form, and the decisive test is a
  spawned fresh bin/hb that registers DECOYS FIRST.** (dot habu-wire-content-key, keywire
  lane.) The § 23.9 origin-class table's cross-process form for content-addressed families
  is a 32-byte SHA-256 over the interned descriptor (name/facts/store-key/rev-content),
  interned in a per-id column at REGISTER; `KEY>WIRE` copies it, `WIRE>KEY` resolves it by
  scanning content keys (BY CONTENT, not by registration order). The proof that identity
  survives PROCESS DEATH is a child process that registers decoys first so its real ids get
  DIFFERENT raw indices than the parent, then decodes the parent's keys — the raw-index wire
  form would resolve to a decoy, the content key resolves correctly (maki/db/keywire-xproc-*).
- **Private words persist across `package` REOPENS, so a reopened package's second file
  collides on a duplicate private name.** maki/db/artifact.f reopens `package ARTIFACT` and
  defines a private `R-OK`; adding an id-result `R-OK` to maki/artifact.f (loaded first)
  produced `duplicate definition: R-OK`. Fix: prefix the second file's wrappers distinctly
  (`IDR-OK`). Same-package private words are ONE namespace across every file that reopens it.
- **Add the cross-process codec, don't rename the raw one — a live consumer still needs it.**
  The § 23.9 contract names the wire codec `ID>WIRE`/`WIRE>ID`, but maki/db/diagnostic.f
  (out of the migration's write set) still serializes producer/config/rev via the raw
  `ID>WIRE`. So the content-key codec is a NEW public pair `KEY>WIRE`/`WIRE>KEY` alongside
  the raw pair, not a rename. numeric-policy is the documented exception: its content key IS
  its 8-byte rank (already cross-process-stable), so `KEY>WIRE` delegates to `ID>WIRE`.
- **A later suite test must reuse an already-registered id, never register into a
  cap-filled global registry.** maki/target/target-test.f `CAP-FILL`s the 16-slot target
  registry earlier in maki/test.f; the cross-process test first registered a fresh target
  and threw E-TARGET-CAP in-suite (green standalone). Fix: use `TARGET:SM87` (INIT-registered
  in every process). Process-global append-only registries with caps are a hidden ordering
  coupling between suites. (Re-confirmed by the diff-suite flip-matrix's second target: an
  arch=100 descriptor RESOLVES target-test's existing cap-fill slot rather than minting a
  new one — the maki/db/artifact-test.f ADT-TARGET2 precedent.)
- **A `create` buffer's bare address infers `ptr a`, so byte comparators want it consumed
  IMMEDIATELY, not after pointer arithmetic on other stack items.** In a digest-tail check
  `EA la CKW - DB CKW MEM=` failed with `MEM=` seeing `ptr a` for `EA`: the create address
  keeps its default cell-pointer type while the arithmetic runs on the length below it, and
  `MEM= ( ptr u8 ... )` then rejects it — even though `EA EB la MEM=` (create address
  consumed directly) unifies `EA` to `ptr u8` fine. Fix: `BYTE-COPY` the span into a fresh
  `create` buffer (BYTE-COPY takes any `ptr`), then compare the two buffers passed directly.
  (diff-suite-test ENC-DIGEST-TAIL, 2026-07-18.)
- **Closing a dot that OWNS TRUSTED.md rows requires repointing those rows in the SAME
  commit, and the integrator's battery must run `trusted-inventory -- strict` (owner
  liveness), not just trusted-inventory-test.f.** (cp-async closure incident, 2026-07-17.)
  The closure of habu-checker-cp-async left 8 classification rows citing the archived dot;
  the test fixture stayed green (it checks sync, not owner liveness) and the red only
  surfaced in the NEXT lane's strict run. Rule: `dot off` on any dot named in TRUSTED.md
  = same-commit repoint to a live dot (or sanctioned cap:), verified by strict before push.
- **The cp.async pipeline-slot typestate is nominal state-family transitions + parity
  unification — the four dynamic negatives reject with ZERO checker LOGIC, exactly the M5
  "the negative needs zero machinery" pattern.** (dot habu-checker-cp-async-6ba788a5, host
  lane.) A staged-buffer slot threads `cpp-pending<p>` -> `cpp-committed<p>` -> `cpp-ready<p>`
  (three nominal TK-CELL families over a symbolic parity, baked in type-family.f). Each
  protocol word (CPPSLOT COMMIT/WAIT/READ/READ-STAGE in lib/ptx/cpp-slot.f) demands one state
  and produces the next, so ordinary stack-effect unification rejects read-before-wait (READ
  wants ready, has pending), missing-commit (WAIT wants committed, has pending), double-wait
  (2nd WAIT wants committed, has ready), and parity-mismatch (READ-STAGE ties the slot's
  parity to the mmstage's). No checker.f rule was needed for the negatives — only the family
  registrations + typed words + fixtures. Key move: the fixtures take their INITIAL slot state
  from the fixture SIGNATURE (a `cpp-pending<p>` input), so no ISSUE mint word is needed — the
  issue mint stays the production pipeline's existing trusted boundary, which is what keeps net
  trusted DOWN.
- **bar.sync composition with M5 is a one-clause extension: committed->ready IS a block
  barrier.** WAIT drains the group + bar.sync-fences it, the same block-uniform barrier as a
  tile->uniform reduction, so PTX-CPWAIT-ROWS? (committed-in/ready-out) becomes one more OR
  clause in PTX-BARRIER-ROWS? and a WAIT under divergent control rejects (E-DIVERGENT-BARRIER)
  through the EXISTING BARRIER-CUR?/REJECT-DIVBAR choke — composing, not duplicating M5. Two
  captured family-id cells (PTX-CPCOMMITTED-FAM/PTX-CPREADY-FAM), same late-capture pattern as
  PTX-TILE-FAM/PTX-UNIFORM-FAM.
- **Net trusted-DOWN came from discharging the NON-minting tilepipe bodies, not the mints.**
  RB-FMA (`2drop MM-KSTEP-FMA`) and PIPE-STORE (`MM-WRITE 2drop`) only consume operands and
  emit fixed-register instructions — no phantom mint — so they certify as CHECKED `:` words
  with byte-identical bodies (empirically: they ACCEPT as checked; STAGE-SLICES/A-FRAG/
  PIPE-ACC-ZERO all REJECT because they produce a family cell from a bare int literal). Fold
  the PIPE-RUN adapter into PIPE-LOOP's quotation: -3 trusted, +2 for CPPSLOT COMMIT/WAIT
  transitions = net -1, byte-identical EMIT-PIPED==EMIT-MATMUL. A faithful 3-state transition
  vocabulary needs an irreducible 2-transition trusted core (pending->committed, committed->
  ready re-type a phantom, which no checked rule expresses); don't try to check them away.
- **The CPPSLOT typestate CANNOT be threaded through the SHIPPING double-buffered PIPE-LOOP
  path: the software pipeline's emitted commit and wait land on DIFFERENT slots, so no single
  emit-time cpp-slot token can carry commit->wait->read.** (dot habu-wire-cppslot-typestate-
  ce2463df, host lane, no device; BLOCKED, proven from the EMIT-MATMUL golden + the write set.)
  EMIT-MATMUL's ONE emitted `$KLOOP` body emits `cp.async.commit_group;` for the PREFETCH slot
  (tile kt+1) immediately followed by `cp.async.wait_group 1;` draining the CURRENT slot (tile
  kt); the current slot's commit is the prologue commit (kt=0) or the SAME instruction on the
  prior runtime pass (loop-carried, outside an emit-time stack checker). So the linear
  cpp-pending->committed->ready->read of CPPSLOT (a SINGLE-slot lifecycle, matching only the
  single-buffer MMA-PIPE-KLOOP-SINGLE) has NO same-body commit->wait pair to model on the
  double-buffered f32 path. Three independent walls, any one fatal: (1) BYTE — CPPSLOT:WAIT
  emits `cp.async.wait_group 0;`+`bar.sync 0;` (adjacent), but the double buffer emits
  `wait_group 1;` (overlap arm) / `wait_group 0;` (drain arm) in SEPARATE branches with the
  `bar.sync 0;` after the `$PFDONE` join, matching NEITHER arm — composing CPPSLOT anywhere in
  the loop drifts bytes. (2) OWNERSHIP — the only CHECKED surface on this path is the stored
  compute body RB-TILE (cg-matmul.f), whose `( mmstage mmracc -- mmracc )` effect is SHARED with
  the maki LLM-eval authoring vocab (MM-K-LOOP); PIPE-LOOP is `TRUSTED:` so a token threaded
  inside its quotation is unchecked, and a ready slot PIPE-LOOP mints-and-consumes itself is a
  self-satisfying tautology disconnected from the real issue (a hollow gesture, not a protocol
  check). (3) INTERFACE — MM-PIPE-KLOOP-WITH's `( [ -- ] -- )` compute-quotation interface is
  consumed with UNTYPED `[ -- ]` bodies by maki/lower-mm.f (`[: MMA-KTILE ;]`) and cg-mma.f, so
  giving the quotation a cpp-slot token breaks out-of-write-set callers. The checked cp.async
  protocol therefore fits the single-slot pipeline OR a loop-carried-token capability (out of an
  emit-time stack checker's model), NOT the shipping double-buffered path — the honest close is a
  documented BLOCKED, not byte drift or a laundered ready-mint. No code changed; every byte pin
  stays green.
- **`( ptr n -- )` in a stack comment is ONE input of type `ptr n`, not two (a pointer + an
  index).** (dot habu-v2-structured-diagnostic-18d24536, the Diagnostic IR.) `ptr` is a
  type constructor that consumes the next token, so `: FLAG@ ( ptr n -- bool )` declared a
  single pointer-to-n and `cells` then saw `ptr n` where it wanted `n`. A word that genuinely
  takes a base pointer AND an integer either spells distinct types (`( ptr a n -- )`, two
  inputs) or — cleaner — references the `create` buffer by name directly (the maki/db/artifact.f
  column-accessor style) instead of passing it as a `:ptr` local, which also erases `ptr u8`.
  Refactored 7 per-slot presence flags into one per-slot bitset cell referenced by name.
- **Do not NAME a checked word after a control word (`BEGIN`, `IF`, `DO`, …) — a bare CALL
  resolves to the control word, not your definition.** (same dot.) `DIAG:BEGIN` DEFINED
  cleanly, but a bare `BEGIN` inside a checked body opened a `begin…until` frame and the error
  surfaced far downstream ("at 'BUILD'"). The reserved-name rule bites at CALL sites, not just
  definitions; renamed the builder-open word to `NEW`.
- **A word returning a layout value (PRODUCT/SUMTYPE) cannot be called at TOP LEVEL (interpret
  mode): "interpret-mode layout value: NAME".** (same dot.) Every diagnostic / build-result /
  decode-result producer must be wrapped in a `:` word that returns a plain cell before a
  top-level `T=`/`TTRUE` (the maki/db/artifact-test.f pattern). To read many fields of one
  decoded value across top-level assertions, store the decoded handle's SLOT (`DIAG>` → n) in a
  variable and rebuild the handle (`>DIAG`) inside each reader word.
- **A package's private and public wordlists may hold the SAME tail; in-package bare lookup
  finds PRIVATE first.** (same dot.) A public `CODE@ ( diagnostic -- n )` was silently shadowed
  by a same-named private raw column accessor `CODE@ ( n -- n )` for in-package callers (the
  test and renderer files reopen the package), so `d CODE@` type-errored. Give internal raw
  accessors a distinct spelling (`CODE-R@`) so the clean public handle name resolves in-package.
- **An optional field's presence bit must be set on the DECODE path, not only in the builder —
  and byte-identical re-encode is the test that catches it.** (same dot.) Field-level round-trip
  passed but re-encode was 16 bytes short: decoded `revision` stored its value yet never set its
  presence flag, so re-encode omitted the field. A `decode(encode x) → re-encode → byte-compare`
  assertion catches missing decode-side presence sets that per-field equality checks miss.
- **error-code-lint is part of the OWNING gate for ANY new `E-*` constant, including maki/ — a
  maki-only landing skipped it and put a code collision on master.** (idfam lane integration,
  2026-07-17.) `E-TARGET-WIRE` picked -5251, already claimed by `E-ABL-CAP` (maki/ablate-ptx.f);
  the lane's gate list (focused tests, maki suite, trust/refine/host/filemap lints) and the
  orchestrator's integration battery both omitted error-code-lint, which only surfaced when the
  next engine landing ran the full gate-stdlib slice. Rule: any diff adding or changing an error
  code runs tools/error-code-lint.f before commit, and the integrator's battery includes it for
  every landing that touches `.f` files — grep for a free code first, never pick by adjacency.
- **A typed error result over a NOMINAL diagnostic type needs a custom sum family, not the
  shared `result<a,b>`.** (dot habu-v2-canonical-artifact-ee5121b4, the artifact envelope codec.)
  Constructing `RESULT:OK` in a TOTAL (ok-only) word leaves the err type variable `b` free, and a
  free variable will unify with a structural type (`n`, `ptr u8`) but NOT with a nominal ENUM/TYPEFAMILY
  — `( n -- result<n,e2> ) RESULT:OK` rejects with "expected result<n,e2> actual result<n,a>" while
  `result<n,ptr u8>` passes. This is the same limitation maki/typestate.f flagged as a "DEVIATION
  from result<stage,diag-set>". The fix is the lib/cad-num-types.f `numeric-result` idiom: define a
  bespoke `SUMTYPE foo-result 1` whose ok variant carries the payload `a` and whose error members
  are baked-in nullary variants — no free error-type variable, so a total ok construction certifies.
- **A multi-cell PRODUCT/SUMTYPE value cannot be a typed local NOR a polymorphic sum payload;
  UNMAKE/MATCH it straight off the stack.** (same dot.) `{: d:content-digest :}` fails with
  "unknown type", and a `VARIANT ok a` payload `a` accepts `n` and arity-0 nominals but rejects a
  multi-field PRODUCT (`expected a actual hprod<>`). Keep multi-cell values on the stack and destructure
  immediately; make the sum's ok payload a single cell (an `n` slot/index or an arity-0 handle).
- **Generated PRODUCT/SUMTYPE constructor names double the family's internal hyphens.**
  (same dot.) `PRODUCT content-digest` in `package ARTIFACT` generates `ARTIFACT-CONTENT--DIGEST:MAKE`
  / `:UNMAKE` (package sep is one hyphen, each family hyphen becomes `--`); a nullary variant ctor is
  `PKG-FAMILY:VARIANT` (e.g. `ARTIFACT-ART--RESULT:MALFORMED`) while `MATCH` arms and payload
  destructuring use the BARE variant name. Wrap the long spellings in short private words once.
- **A codec test that needs a SECOND id of a capacity-bounded shared registry can't mint one
  in the integration suite — prove digest coverage on the DECODE side instead.** (dot
  habu-v2-canonical-artifact-ee5121b4 slice 2, the foreign-id envelope fields.) The digest-flip
  test for `target-id` registered a fresh descriptor; that passed in the focused test (registry
  had 2 entries) but threw `E-TARGET-CAP` (-5252) in `maki/test.f` because `TARGET`'s registry is
  a 16-slot shared resource the suite already fills to `TARGET:COUNT = 16`. Interning by name is
  cheap for the 256-slot SCHEMA/PRODUCER/CONFIG families and free for NPOL (dom→rank, no slot),
  but TARGET is tight, and in the full suite you cannot even NAME a second target (only the
  library-registered `sm87`, raw 0, is reachable). Fix: prove target-id is digest-covered by
  swapping the ENCODED target raw for another already-valid raw (`(r0+1) mod TARGET:COUNT`) and
  requiring `digest-mismatch` — no new slot; guard a one-target isolated run by registering one
  alternative only when `TARGET:COUNT < 2`. Lesson: pick digest-coverage proofs that don't grow a
  bounded shared registry, and run the FULL owning suite (not just the focused test) before sealing —
  registry capacity is load-order/global state the focused test never exercises.
- **A byte-pointer stack-effect must be spelled `ptr u8` (two tokens), not bare `ptr`.** (same
  slice.) `: E-WIRE-FIELD ( n ptr n -- ) {: tag:n a:ptr u:n :}` rejects at `:}` with "expected
  n ptr u8 n actual n ptr n": the `a:ptr` local reconstructs to a `ptr u8` in the effect, and a
  producer `( -- ptr )` fails with "'ptr' needs an element type". Write `( n ptr u8 n -- )` and
  `( -- ptr u8 )`. The `:ptr` local shorthand and the `ptr u8` effect spelling denote the same
  one-cell byte pointer.
- **M5 barrier-uniformity: VALUE uniformity was ALREADY expressed; only the CONTROL effect was
  missing.** (dot habu-ptx-m5-mask-eb0716f1, host checker lane.) Leg-1 empty experiment proved the
  M2 families already distinguish `uniform<T>` (block-uniform) from `tile<..>` (lane-varying): a tile
  fed where SCALE/FMA./PTX:B- want `uniform<t>` REJECTS by family unification today — the
  "lane-varying-used-as-uniform" negative needs ZERO checker machinery, just a regression fixture.
  The genuinely missing piece was block-uniform REACHABILITY: `x BLOCK-MAX` inside an `if` CERTIFIED
  (the real miss) because bar.sync reachability is a control property no stack effect expresses. The
  sound minimal model: BLOCK-MAX/BLOCK-SUM have the shape `( tile<..> -- uniform<..> )` — the ONLY
  way to soundly produce a uniform from a tile is a block reduction with bar.sync — so detect that
  shape structurally (no per-word directive), flag `CTL-BARRIER`, and reject the call when the CF
  stack is non-empty (`#CFC>0` = inside if/begin/do/case = not proven block-uniform). Straight-line
  softmax certifies (#CFC=0); collective-inside-any-control rejects. Conservative (over-rejects a
  collective under a proven-`uniform<bool>` branch — the documented remainder) but SOUND, and no
  existing kernel calls a collective under control so nothing regressed.
- **TRUSTED: words BYPASS the CHECK finalize — attach per-word checker metadata at E-ADD-EFFECT, the
  one USIG choke both paths share.** (same dot.) First cut set the barrier flag in CHECK's finalize
  CTL-flags block (checker.f ~8402) and it silently did nothing: `EM-COMPILE-PUBLISH-TRUSTED`
  (habu2.f) branches PAST `EM-P2-CHECK-DEFINER`, so a `TRUSTED:` word (which is where ALL the
  collectives live) never runs CHECK. Both `:` (via CHECKER-USIG-CERT-ADD) and TRUSTED: (via
  TRUST→CHECKER-USIG-ADD) funnel through `USIG-ADD → E-ADD-EFFECT`, which has the din/dout rows AND
  `CHECKER-REC-SYM`. Detect there. E-ADD-EFFECT is defined BEFORE the NORET flag machinery, so the
  flag setter is reached through a forward xt hook (`PTX-BARRIER-SET-XT`, the TFAM-RESOLVE-XT
  pattern), installed after NORET-ADD. Verify a new checker flag actually FIRES on a TRUSTED subject
  before trusting the finalize path — the `:`-only path passes a green build while doing nothing.
- **Derive an iGPU tensor peak from the GPU-ONLY sparse-INT8 TOPS, never the marketing TOPS —
  and let a measured kernel rate falsify a candidate peak.** (dot habu-mma-wave-4, roofline
  verdict, docs/kernel-principles.md.) The Orin NX "100 TOPS" headline is GPU + 2× NVDLA, so it
  cannot back out the GPU tensor rate. The AGX Orin Technical Brief's GPU-ONLY figure (170 sparse
  INT8 TOPS = 2048 dense INT8 MAC/SM/clock at 16 SM / 1.3 GHz) is the clean anchor: it is the
  GA100 (A100) full rate, 2× the GeForce/GA102 consumer rate, which proves Orin's Tensor Cores
  are NOT GeForce-throttled. The fixed 3rd-gen ratio ladder INT8:FP16:TF32 = 4:2:1 then gives
  TF32 = 512 FMA/SM/clock, so the sm_87 Orin NX dense-TF32 peak = 8 SM × 512 × 2 × 918e6 =
  **7520 GFLOP/s** at 918 MHz. Two independent checks kept the derivation honest: the same
  lane×clock×2 method reproduces the NVIDIA-published FP32 roofs (Orin NX 1.9 TFLOPS, AGX Orin
  5.3 TFLOPS = 16×128×2×1.3e9 exactly), and our measured best 3026.6 > 1880 *falsifies* the
  fully-throttled GA102 TF32 rate (128 FMA/SM/clock → 1880 peak) outright — a kernel can't beat
  its roof. A residual 512-vs-256 FP32-accumulate-throttle ambiguity survives the marketing data
  but is discounted by the phase decomposition (only ~27% of runtime is mma-issue, so we are not
  at 80% of a 3760 peak) and does not move the verdict.
- **A perf program closes on a ROOFLINE + LEVER-INVENTORY argument, not on hitting 100% of a
  peak.** (habu-mma-wave-4.) The tf32 mma.sync GEMM is at 40% of the 7520 tf32 tensor peak, 93%
  of its own DCE-safe quarter-B feed-ceiling, and 1.60× Triton (which is at 25% of the same
  peak). The gap to peak is real but *uncapturable*: every tf32 kernel lever from waves 1-3 is
  spent (A- and B-side ldmatrix, MFRAGS=4, single-buffer-static occupancy), the three wave-4
  candidates (wider M-frags, dual-issue across M-frags, wait_group placement) are each a spent
  constraint not headroom, and the 21% residual mma-issue is the Tensor Cores doing the GEMM's
  FIXED mma count (N³/1024 instructions) at the widest tf32 shape that exists — m16n8k8. There is
  no m16n8k16 for tf32; a denser accumulate shape means fp16/bf16, which changes the numerics
  contract. So the honest verdict is CLOSE (no lever dot minted), with fp16/bf16 recorded as a
  USER-GATED numerics-policy question and the residual handed to the autotuner + default-flip
  dots. Lesson: when the roofline shows headroom but the lever inventory is empty and the
  instruction shape is maxed, the deliverable is a documented close, not another kernel rung.
- **The fused cp.async K-loop decomposed into shared CPP-* protocol step words, byte-identical
  across 20 configs — no device time.** (dot habu-decompose-pipelined-staging-49c97cba,
  prerequisite for habu-checker-cp-async-6ba788a5.) MM-PIPE-KLOOP-WITH (cg-matmul-emit.f) and
  cg-mma.f's MMA-PIPE-KLOOP-WITH / MMA-PIPE-KLOOP-SINGLE now COMPOSE named steps defined once in
  cg-matmul-emit.f: CPP-COMMIT / CPP-WAIT n / CPP-SYNC (bar.sync) / CPP-CUR-WINDOW (read-window
  base %r16) / CPP-NEXT-WINDOW (prefetch dst %r18) / CPP-FLIP (parity xor) / CPP-KGUARD / CPP-KTAIL
  / CPP-PF-TEST / CPP-PF-ELSE / CPP-PF-END / CPP-KT-INIT/NEXT/ADVANCE / CPP-PARITY-INIT. The
  STAGE-ISSUE step stays per-file (MM-CP-STAGE / MMA-CP-STAGE — the As/Bs chunk geometry differs);
  the CPP-* steps are geometry-independent and parameterized only by buffer-byte-size and BK, so MM
  (16384/32) and every non-default MMA config thread the SAME words and emit identical bytes (this
  is why MMA-DEFAULT? could already call MM-PIPE-KLOOP-WITH wholesale). The whole safety net was a
  pure off-device emit-dump (EMIT-MATMUL + 19 EMIT-MATMUL-MMA configs: default lmode0/1/2,
  larger-BK single/double, padded, wide MFRAGS=2/4, B-ldmatrix bpad0/4) sha'd before vs after the
  factoring — byte-identical, proven twice. The typestate capability attaches per-step
  issue->commit->wait->bar.sync->read + buffer-parity obligations to these CPP-* seams; per the
  earlier note, same-body parity consistency is in scope for it, runtime loop-carried alternation is
  not.
- **Global process totals are census data, not a phase budget.** The full gate
  deliberately runs large process-API matrices, and pool context adds required
  co-located reapers. Enforce performance with owner-local architectural
  ratchets; otherwise concurrent phases charge each other and valid fixtures
  make an arbitrary global ceiling fail.
- **Post-spawn telemetry is part of process ownership.** A throwing hook after
  successful creation must kill and reap the returned child before propagating;
  an owner label must be armed only after every fallible preflight; and a fork
  child must reset inherited live counters through a distinct child hook.
- **A parent-death reaper fixture must never close its parent-death writer while
  the reaper shares the test process group.** That EOF deliberately makes the
  reaper SIGKILL its whole group, including the harness. Test fork accounting
  through the classified fork seam; leave kill-tree behavior to the isolated
  group-kill fixtures that first establish a separate process group.
- **Library modules must not require their entry wrappers.** A resident runner
  loaded `gate-common-lib.f`, then `gate-validation-worker.f` re-entered it via
  `gate-common.f` and failed at the first duplicate definition. Entry wrappers
  own prerequisites; modules consume the documented load contract.
- **Direct/subject byte parity excludes fatal signal diagnostics.** Crash-handler
  register dumps contain process-specific addresses, so trap/no-handler probes must
  remain direct. Recover the spawn budget by batching deterministic checked sources,
  and require per-source digest telemetry plus a dedicated full parity slice.
- **The checker ALREADY expresses "same phantom in, same phantom out through an emitter" — the
  same-type tile/collective wrappers were trusted UNNECESSARILY.** (dot habu-ptx-phantom-preserving,
  leg 1, host lane, lib/ptx/rep.f.) A row-polymorphic higher-order combinator `( a a [ n n -- n ]
  -- a )` (`PTXREP:REP2`; `REP1`/`REPMIX2` for the unary/first-preserved arities) makes a kernel
  token's `n` register flow THROUGH a checked EMIT-* quotation while the SAME phantom `a` is
  returned. The three forge/kind/arity soundness properties fall out of the EXISTING unifier for
  free, NO checker/type-family change: forge (`mmaslice`->`mmbslice`) rejects because both operands
  and the result must unify to one `a`; a wide layout family cannot bind the single-cell var `a`
  (kind); the `[ n n -- n ]` quotation pins arity. So ~23 per-op TRUSTED: wrappers (`+.` `-.` `*.`
  `/.` `SCALE` `RELU` `EXP.` `U/` `B-` `B/` `NEG` and their `-V4`/`.V4` variants across tile.f /
  tile-v4.f / tile-v4a.f / collective.f / ad-saved.f) became CHECKED `:` callers of three
  forge-proof combinators (net trust -20), byte-identical PTX (verified via PTX-CAPTURE cmp). Empty
  experiment first (exp1: `[: EMIT-ADD ;] REP2` certifies; a wide `SUMTYPE` and a cross-family
  relabel both REJECT) grounded the whole design before any edit.
- **The mission framed this as checker+type-family work, but the honest finding placed it in a lib
  combinator — because the type-CHANGING wrappers, not the type-PRESERVING ones, are what actually
  need a checker capability.** `LOAD` (span->tile), `GRID-CTX`, `STAGE`, `BLOCK-MAX`, `BROADCAST`,
  the ctx/load/store/reduce family MINT a NEW phantom the emitter's output register cannot witness,
  so a same-type combinator can't retire them (its `a` output can only be a type you already hold).
  Those remain the leg-2 remainder and genuinely need the deeper rep-provenance / typed-emitter
  capability (or stay trusted mints, like the ~17 `*-REG` from-register casts). Don't conflate the
  cheap forge-proof preserving case with the hard minting case.
- **Leg 2b: the checked-MINT capability is a checker rule, NOT another lib combinator — and empty
  experiments proved every lib-only mint design is unsound before any machinery.** (dot
  habu-ptx-phantom-preserving, leg 2b, host checker lane, src/core/checker.f NP-MINT-CHECK.) Four
  probes via `CHECK-CANDIDATE!` on the same load path settled the design: (1) a free-output combinator
  `( a [n->n] -- b )` is a UNIVERSAL forge (mints gridctx from a tile — ACCEPT); (2) a
  `fresh-mask-live` output in the combinator can't certify a legit wrapper (the combinator's fresh
  atom and the wrapper's declared fresh atom are DISTINCT rigid tokens, never unify — REJECT
  everything); (3) a flex-mask combinator certifies legit wrappers but lets a `:` wrapper forge mask
  AGREEMENT (two independent gridctx forced to share one mask — ACCEPT); (4) a pure-projection
  combinator enforces the output FAMILY (acc vs tile rejects) and cross-input agreement, but a `:`
  wrapper declaring a FREE output element var (`tile<u,b,m>`, u unbound) still certifies and PUBLISHES
  the over-general sig (f32 span -> claim u32 tile ACCEPTS). The root cause: the checker verifies a
  declared sig is UNIFIABLE with the body, not that it is PRINCIPAL — so any polymorphic mint output
  can be loosened into an unaudited `:` word, strictly EASIER to forge than a visible TRUSTED: row.
  The sound fix is a checker seal, not a combinator: a CHECKED word may not introduce, as an argument
  of a register-phantom CELL family output, a declared type variable unbound in its inputs (producing
  a cell of input-unrelated type is a mint, sound only behind a TRUSTED: boundary the checker cannot
  witness). It rides NP-CHECK's post-body parametricity seal (the E-NONPARAMETRIC-EFFECT choke),
  descending into family args (which NP-CHECK proper does NOT), on RAW declared var identity so a
  laundering combinator that unified `u:=t` can't hide `u`'s sig-level absence. TRUSTED: bypasses
  CHECK, so genuine fresh-mask ctx mints (GRID-CTX's free block) stay legal there — the rule is
  exactly why they cannot be rewritten as checked callers. With the seal, the FEW generic mint
  combinators (`PTXREP:MINT-LOAD`/`MINT-ROW-SPAN`/`MINT-ROW-LOAD`, per-input-pattern to pin
  projection) make many load/repackage wrappers checked callers: LOAD/LOAD-ONCE/ROW-SPAN(-ONCE)/
  ROW-LOAD(-ONCE) converted, net trust -3, byte-identical PTX (24 emit tools). Pure checker+render lib
  change, so the FULL engine battery was owed and paid (fixpoint x2 byte-identical, DDC dual-chain
  byte-identical, test/run.f perf-verdict=pass attempts=1).
- **The mint seal must exempt three checker-owned introductions or it false-rejects legit code, and
  each needs a DISTINCT guard.** (same dot.) (a) VALUE-RECORD/construct machinery mints INTERNAL
  fresh vars whose `NP-LETTER` is '?' (not a user a..z quantifier) — e.g. a generated
  `( a -- box )` constructor expands `box` to hidden `field<>` cells over internal vars; guard: only
  flag a raw output var that is a real declared letter. (b) A hidden physical field / layout
  (product/sum) family output carries its own construct + hidden-field anti-forgery; guard:
  `NP-CELLFAM?` excludes `PARAM>HID>0` and `TFAM-LAYOUT?`. (c) A VALUE-RECORD FIELD ACCESSOR
  (`( rec -- fam<a,..> )`, empty body = width coercion) legitimately surfaces cell vars unbound at the
  sig surface — the record is carried as a reserved `field<rec,name,inner>` term (FIELD-PARAM?), and
  its var identity survives raw AND resolved AND surface-letter checks (the empty body leaves the
  output quantifier unbound, structurally identical to a free mint); the ONLY reliable signal is the
  `field<>` INPUT, so the seal steps aside when an input consumes a record. Each false positive cost a
  fixpoint rebuild to diagnose — build the probe (dump the SGIN term tags/families) before guessing.
- **Leg 2c: the projection-load "batch" had exactly ONE genuine sharing win, because the mint
  combinators are NOMINALLY PINNED and the batch's 8 loads have 8 distinct family shapes.** (dot
  habu-ptx-phantom-preserving, leg 2c.) A MINT-* combinator pins its projection by NAMING concrete
  families (type-family.f registers span/vspan/tile/vtile/gridctx/coopctx/fanctx/idxctx/uniqidxctx as
  distinct TK-CELL families — there is no family variable), so two wrappers SHARE a combinator only if
  their full (input families, output family, quotation arity) coincide. Surveying the 8 candidates:
  LOAD-V4 (span<s,t,e> gridctx<b,e,m> -- tile<t,b,m>) is IDENTICAL to the scalar load shape — v4-ness
  is pure codegen (EMIT-LOAD-V4), not a type change — so it reuses the EXISTING PTXREP:MINT-LOAD (net
  -1, no new combinator, byte-identical PTX proven x2). The other 7 are each a UNIQUE shape:
  FANIN-LOAD (ptr+fanctx), INDEX-DENSE-LOAD (idxctx) vs UNIQUE-INDEX-DENSE-LOAD (uniqidxctx, a DISTINCT
  family), INDEX-LOAD (3-operand), LOAD.V4 (vspan/vtile), STAGE (span<global> -> span<shared>), SLOAD
  (coopctx) — no two share, and none matches an existing combinator. The mission's hypothesized
  "one indexed-load / one V4 / one smem shape" sharings are all FALSE under nominal pinning: idxctx and
  uniqidxctx never unify, LOAD-V4≠LOAD.V4 (scalar vs vec4 families), STAGE≠SLOAD (span-shared vs tile
  outputs). Converting the 7 would need single-use combinators = net 0 each = RELOCATED, not reduced,
  trust (worse: +machinery, +negatives for no gain) — so they STAY TRUSTED. The batch's honest maximum
  is the LOAD-V4 net -1. Loosening a combinator to a family variable to force sharing would reopen
  exactly the projection forge the NP-MINT-CHECK seal closes; don't.
- **Wiring the proven B-side ldmatrix cut the residual B-feed 27%->7% and won +11.9%.** (dot
  habu-mma-wave-3, lib/ptx/cg-mma.f MMA-BLDM.) The MFRAGS=4 winner's un-amortized scalar B feed
  (2 ld.shared + 2 cvt / 8x8 fragment) was replaced by ONE ldmatrix.x2 over a TRANSPOSED staging
  SHM_BT[n][k]=B[k][n] (the device-proven MP-BLDM-ALL law). Measured 3026.6 GFLOP/s at 2048^3
  (918 MHz) = +11.9% over the scalar-B winner 2707.3 and 1.60x Triton, element-exact 256^3/512^3.
  The fresh ablation confirmed the mechanism: B-feed 27%->7% (51.4->12.1 ms, a 4.2x cut), now 93%
  of its own quarter-B ceiling (was 73%); mma-issue (21%, tensor-core throughput) is the new floor.
  The transpose MUST be a SCALAR staging (coalesced global read B[k][n], strided shared write
  BT[n][k]): cp.async copies a contiguous chunk and cannot scatter a transpose, so there is no
  cp.async B path - but the B tile is tiny and reused across all M-frags, so the scalar stores are
  amortized to noise.
- **A transposed-staging ldmatrix lives or dies on its READ bank stride - measure it, again.**
  (habu-mma-wave-3.) The n-major BT row stride BK+bpad sets the ldmatrix read start-bank stride.
  bpad=4 (stride 36 words = 4 mod 32 -> the 8 tile rows hit 8 distinct 4-bank windows, conflict-
  free) = 3026.6; bpad=0 (stride 32 -> all 8 rows alias ONE 4-bank window, 8-way conflict) = 1318.5,
  WORSE than the scalar-B baseline it replaced. Identical trap to the original unpadded-As ldmatrix
  miss: an aliased ldmatrix is far slower than the scalar loads it removes. Also: bpad must keep the
  BT row stride a multiple of 16 B (ldmatrix.m8n8.b16 addresses each 16 B row) - a misaligned stride
  (bpad not a multiple of 4) FAULTS the GPU (sm machine-check err, not a wrong result), so the
  emitter enforces it fail-closed (MMA-CHECK-BLDM -> E-MMA-BLDM) and a bad knob never reaches a
  launch. A bpad=2 typo in a test leg proved this the hard way (2 GPU fault clusters) before the
  guard + negative regression landed.
- **A checker cp.async pipeline-typestate dot cannot discharge its own tilepipe
  TRUSTED rows alone — it depends on THREE prerequisites, two of them unstarted.**
  (dot habu-checker-cp-async-6ba788a5, host lane, no device.) The 9 lib/ptx/tile-pipe.f
  `TRUSTED:` rows are trusted for a DOMINANT reason that is NOT cp.async: every body
  mints a nominal phantom token (`mmctx`/`mmracc`/`mmstage`/`mmaslice`/…) from a bare
  register-number literal, and the checker has no checked mint — minting/casting phantom
  kernel tokens is always a trusted boundary here (MK-SPAN, MM-ABI, `>LEN`/`>IDX`). That
  mint gap is owned by habu-ptx-phantom-preserving-3df9db92 (open, unstarted), so no
  tilepipe row is untrustable until that lands. Landing the cp.async typestate discipline
  alone discharges nothing — mirrors habu-linear-once-resource-4c58a7a1's own note ("no
  TRUSTED row discharged by the execute fix alone … blocked on kinds PLUS rewriting the
  wrappers"). The dynamic protocol (cp.async issue→commit→wait→bar.sync→read, parity flip)
  is emitted ATOMICALLY inside MM-PIPE-KLOOP-WITH (cg-matmul-emit.f), run once at emit time;
  the K-loop is a RUNTIME `$KLOOP` branch, so read-before-wait / missing-commit / double-wait
  are not expressible at the tilepipe surface without DECOMPOSING that shared, byte-sensitive
  emitter (consumed verbatim by cg-matmul.f, cg-mma.f, maki/lower-mm.f). And bar.sync needs
  the M5 barrier model (habu-ptx-m5-mask-eb0716f1, open, unstarted) to compose with.
- **A stack-effect checker over the EMIT-TIME program cannot prove RUNTIME loop-carried
  parity alternation.** The Forth checker verifies emit-time stack effects; the emitter emits
  the double-buffer loop body ONCE and parity lives in a runtime register (`%r15`, flipped by
  `xor`). "parity alternates correctly across `$KLOOP` iterations" is a property of the emitted
  PTX's runtime dataflow, outside the checker's model — this is the deep reason it stays
  trusted. What IS emit-time-checkable (given the decomposed emitter) is the weaker SAME-BODY
  property: within one loop body the read requires a `ready<p>` slot whose symbolic parity `p`
  matches, and the prefetch writes `pending<¬p>`. Don't conflate the runtime alternation (out
  of scope) with same-body parity consistency (in scope once the emitter is decomposed).
- **When a folded-in registry removal is DEFERRED to ride a capability's registry edit, do
  not split it into a lone fixpoint rebake if that capability is blocked.** The `mmacc` TFAM
  row (type-family.f) is verified unreferenced (only prose/comments/one stale LLM transcript
  use `mmacc<…>`; live code uses `mmracc`), but its removal was deliberately deferred to the
  cp.async capability's core edit to avoid a standalone engine rebake. With the capability
  blocked, the honest move is to leave it folded-in-waiting, not to rebake the engine for one
  row a later registry edit removes for free.
- **`num_stages` is tile-size AND occupancy dependent — a bigger register/smem tile can
  flip stages=2 from a win to a loss.** (dot habu-mma-wave-2, MMA-MFRAGS=4.) At the MFRAGS=2
  wide MMA tile, double-buffered cp.async (stages=2) was +2.4% over single-buffer. At MFRAGS=4
  the *opposite* holds: single-buffer STATIC (49152 B = the 48 KiB cap) beats double-buffer
  DYNAMIC (98304 B) by +11.6% (2707.3 vs 2394.1 GFLOP/s at 2048^3). Cause: the double-buffer
  256x64 tile needs 98 KiB so only 1 block/SM (8 warps) resides, while the single-buffer tile
  fits 2-3 blocks/SM, AND the 4x B-feed amortization moved the roofline off the cp.async floor
  so occupancy now beats overlap. Never assume a stages setting carries across a tile resize;
  re-measure it. The measured best (2707.3 = +26.9% over the 2133.9 parity, 1.43x Triton) was
  the config the prior amortize lesson would have predicted to be *worse* (no overlap).
- **A wider register tile re-weights the roofline: attack the phase the FRESH ablation names,
  not last rung's.** (habu-mma-wave-2.) MFRAGS=2's residual was 36% B-feed / 15% mma-issue, so
  "B-side ldmatrix" looked like the next lever. MFRAGS=4's DCE-safe ablation shows B-feed fell
  to 27% but 2nd-4th-M-frag mma-issue ROSE to 32% (now dominant) - amortizing the feed 4x turned
  a feed-bound tile toward tensor-core-issue-bound. Re-run the attribution after every tile
  change; the biggest lever moves.
- **tf32 B-fragment ldmatrix needs a TRANSPOSED staging, never `ldmatrix.trans`.** (habu-mma-
  wave-2, proven element-exact in tools/ptx/mma-probe.f MP-BLDM-ALL before any kernel use - the
  required "prove lane->element FIRST" for the top device-bug class.) The mma m16n8k8 B fragment
  is b0=B[t][gid], b1=B[t+4][gid] (gid=lane>>2, t=lane&3). The non-trans ldmatrix.m8n8 result law
  is reg = tile[row=lane>>2][tf32col=lane&3] (the SAME law the A ldmatrix.x4 proof pins), so the
  8x8 b16 tile it reads must be B-TRANSPOSED: stage SHM_BT[n][k]=B[k][n] (n-major), then one
  ldmatrix.x2 over the two k-half b16 tiles returns {b0,b1} exactly. `ldmatrix.trans` is NOT an
  option for tf32: it transposes at b16 granularity and a tf32 is two adjacent b16 halves, so
  .trans splits every tf32 - the transpose must live in the STAGING, not the load. Proven
  element-exact (integer operands, 0 mismatches of 128); feeds habu-ship-swizzled-mma.
- **Keep a new codegen knob byte-identical for the shipped configs by GATING the register-pool
  header, not just the body.** (habu-mma-wave-2.) MFRAGS=4 needs 64 f32 accumulators/lane, so the
  `.reg .f32 %f<48>`/`.reg .b32 %r<64>` header and the mode-0 cvt temp base (%f42) must grow. Making
  them formula-driven for ALL MFRAGS would have changed the MFRAGS=1/2 emitted text and broken the
  pinned SWZ / SWZ-BK64 / lower-mm / parity goldens. Fix: `MMA-FREGS`/`MMA-RREGS`/`MMA-FTEMP` return
  the exact literal 48/64/42 at MFRAGS<=2 and only enlarge at MFRAGS>2. Prove it: capture a
  golden-dump of every pinned config's emitted PTX BEFORE the edit, sha+cmp AFTER (twice) - byte
  identical. An off-device pure-emit dump is the cheap, exact byte-identity gate.

- **Package rollback has two owners.** Restoring the engine's current/public/private
  WIDs is incomplete unless recovery also resynchronizes the checker's package
  mode before the next token; `get-current` proves only the engine half. Follow a
  caught in-package failure with a canonical checked global definition, a package
  transition, and a checked reference to expose either half drifting.

- **Checker policy that constrains compile-time behavior must run before the
  immediate BLR, not only at definition publication.** The definition hook
  correctly rejected unmodeled immediates after body reconstruction, but
  `include` had already evaluated nested source while the dictionary was RX.
  A checker-owned preflight cell closes that temporal gap generically: the
  compiler invokes it for every source-defined immediate while checking is
  armed; `parse-imm` declarations and outer `TRUSTED:` bodies are the only
  allows. Keep the cell in the sealed hook band, preserve it in snapshots, and
  prove warm-boot execution with a modeled immediate rather than checking only
  that the serialized pointer is nonzero.

- **A "role-typed variable family" ask is often already covered by existing
  definers — check before building one.** (typedefs lane, dot
  habu-typed-defining-words-aa224eb5.) Raw `variable`/`create`/`constant` publish
  TVK-RAW cells (`-- ptr a`; `@`→`a`, and `a` coerces to `n`/`bool`/`ptr u8`), so
  they already own plain-scalar/bool/raw cells; `TYPED-VARIABLE`/`TYPED-BUFFER`
  (layout-buffer.f) own nominal/closed-typed-ptr/xt cells. CHECKER-STORAGE-INFO
  DELIBERATELY rejects `n`/`bool`/`a`/`idx` (throw 7121) — that is the raw
  definer's partition, pinned by typed-storage-test R4 (`( n -- n ) RAWV ! RAWV @`
  certifies; laundering a nominal family through the raw cell rejects). So a
  per-cell `s" X" s" -- ptr n" TRUST` override on a raw `variable` is REDUNDANT
  once the definer auto-registers its effect (verify-source SIG-RAW-MODE!,
  2026-07-15): the 17 treeshake `-- ptr n`/`-- ptr bool` rows predated it; delete
  them and the raw boundary + a-coercion certifies the checked body unchanged
  (proven by fixpoint rebuild — treeshake certifies without the rows, TRUST
  363→346). MLEN already had no row; STB-CELL@ shares a body with STB@.
- **Provenance-mint placement is load-order-bound, and a macOS rebuild can't
  prove Linux/snapshot trust rows.** In build-fixpoint.f the baked order puts
  os/macos/layout.f (pos 2) and image-bytes.f (pos 7) BEFORE roles.f (pos 12), so
  a roles.f mint (MMAP>PTR/VA>PTR) cannot serve earlier consumers (MBUF-RC>PTR,
  DATA-VA) — place the mint before its earliest baked consumer. A macOS fixpoint
  build checks os/macos/* not os/linux/*, and snap-lib.f rides the snapshot
  builder tail (not the main fixpoint); LINUX-VA>PTR, the Linux DATA-VA row, and
  STB-CELL@ must be proven through the Linux gate / snapshot-build path, not a
  macOS-only rebuild.
- **Before deleting a "wrapper" vocabulary, rg its WORDS as strings — checked
  words can be a graded LLM-authoring surface, not just call sites.** (mmstage3
  lane, dot habu-re-express-tiled-9cc4a73a.) The plan said "delete
  MM-BEGIN/MM-K-LOOP/MM-STORE with the MM-STATE boundary", but maki's eval lane
  grades LLM candidates as SOURCE STRINGS composing exactly those three words
  (eval-emit fixtures, eval-matrix VOCAB-NEED rows, live-author), including
  reject-shape fixtures pinned to their effects. The fix that kept every
  consumer green: preserve the three names and stack surfaces, re-express their
  bodies over the typed tile-pipe words (mmacc -> mmracc underneath), and prove
  the candidate emissions byte-identical (a double-MM-K-LOOP capture leg).
  Also: a require cycle from layering a checked kernel above a vocabulary that
  requires the kernel's emitters is broken by splitting emitters into their own
  file (cg-matmul-emit.f) - reversing or mid-file requires is order-fragile.
- **A new checker atom prefix (ATOM-TOK?) reserves the ENTIRE lowercase
  `prefix-*` token namespace across every declaration site — sweep the tree for
  collisions before choosing the spelling.** (tilepipe lane, dot
  habu-typed-pipelined-register-4d20acb5.) Signature atoms are prefix-gated
  (`space-`/`extent-`/`mask-`/`block-`/`align-`; now `geom-`/`parity-`), and
  TYPE-RESERVED? consults ATOM-TOK? for enum/sum variant and nominal-type
  declarations too: a first-choice `layout-` prefix made maki's existing enum
  variant `layout-conflict` a reserved name and failed maki loads with throw
  7110 far from the cause. `rg '\bprefix-'` across `*.f`/`*.fs` before baking;
  the collision surfaces only when the colliding module next loads, not in the
  fixpoint build itself. Atoms are kind-free interned tokens (prefix = lexical
  gate only), so an honest new prefix is a 1-line additive checker row baked by
  the normal fixpoint x2.
- **The native single-pass ARM64 assembler silently wrapped out-of-reach
  relocations while the trusted Gforth seed generator already rejected them —
  derive the native boundary from the seed, don't reinvent it.**
  dot habu-check-arm64-relocation-8eee7fad. `src/arch/arm64/icode.f` masked
  branch/ADR deltas (D19/D26/ENC-ADRD) with no signed-range check, so any
  forward/backward BCOND/CBZ/CBNZ/ADR past reach in the 2 MB code window wrapped
  to a wrong target; `bootstrap/cg/asm.fs` already threw via `?REL26`/`?REL19`/
  `ENC-ADR`'s `within` bounds. Fix: shared `?REL26`/`?REL19`/`?ADR` (each a pure
  `*-OK?` predicate + a die wrapper, mirroring `FX-KIND-OK?`/`?FX-KIND`) called at
  BOTH the immediate encode sites (BR-EMIT/ADR, ELSE) and the deferred chokepoint
  (FX-ENC), before any mutation, with bounds `LO <= d < HI` matching `within`
  exactly (LO inclusive, HI exclusive). Parity was exact — no asm.fs change.
- **Boundary tests for a reach that the buffer can't physically span: craft the
  label position, don't emit MB of code.** REL19/ADR reach (±2^18 words) fits the
  2 MB window so far-branch dies are real; REL26 reach (±2^25 words ≈ ±128 MB)
  overflows the code-buffer guard long before the reach check, so REL26 boundary
  fixtures set the bind position via `ASM-CP !` directly. Deferred PATCH always
  targets the real site word (FXS = word 0), so even a crafted *negative* ASM-CP
  is memory-safe — it only feeds the delta arithmetic, never the write address.
  Backward (immediate) cases naturally exercise max-negative deltas; forward
  (deferred) cases exercise max-positive; each PASS pins the exact instruction
  word, each one-beyond dies exit 72 via a child-process fixture.
- **Deferred backpatch is per-fixup atomic, not per-chain.** FX-ENC validates
  reach before returning patch bits, so a failed patch dies before its PATCH
  write — the failing fixup's code word and FXS/FXK slot stay untouched. But
  LBL, binds the label and clears its FXH head before walking the pending chain,
  so a mid-chain out-of-reach failure leaves earlier in-reach fixups already
  patched and freed. Document the per-fixup invariant honestly rather than
  claiming whole-chain atomicity; `die` exits, so post-die state is only
  observable as the child's exit code + diagnostic.

- **High-frequency architectural defaults need an entry rule and a machine
  gate, not a buried reference.** `docs/forth.md` already made packages the
  default, but the session-level instructions summarized casing without package
  ownership, so generated tests repeatedly fell back to raw stems such as
  `LRD-*`. Repeat the package-first invariant concisely in `AGENTS.md`; reject
  violations by exact-diff inspection until the checked package-diff gate lands.
  Documentation discoverability alone does not constrain generation.

- **A surgical GPU devfreq min=max pin reproduces the shipped 918 MHz clock within
  0.15% — use it to make cross-session perf rows comparable.** dot
  habu-re-measure-mma-9fe40cd1. The larger-BK MMA sweep landed at the UNBOOSTED
  408 MHz DVFS pin, ~half the shipped orin-nx-25w absolutes; re-measuring with the
  GPU devfreq pinned `min_freq=max_freq=918000000` (25W mode-3 untouched, restored
  to the as-found 408 MHz after) reproduced the shipped 2026-07-14 MM 979866 /
  MMM 884889 as 981225 / 885794 (<=0.15%), proving the pin == the historical
  measurement clock — no `jetson_clocks` apply (GPU-only blast radius, exact
  verifiable restore) was needed. The swizzle win is clock-independent as expected:
  MMM-SWZ +53.4% / MMM-SWZ-BK64 +54.6% over the BK=32 baseline at BOTH clocks. At
  918 MHz the swizzled kernel is 1358.9-1369.6 GFLOP/s = 72% of Triton's 1890.5
  (vs the scalar baseline's 47%), so the competitive lever is flipping the emitted
  default to pad=8 ldmatrix (separate dot), NOT the clock — the shipped
  HABU-MMM-TF32 row tracks the scalar default and stays 884889 until the default
  flips (tools/ptx/perf-rows.tsv orin-nx-25w-918mhz rows; tools/eval-triton.f).
- **Automatic-fusion device win is LATENCY (fewer global round-trips), not peak
  GB/s.** Benching the AUTOMATICALLY-emitted region kernels of an Add->Mul->Relu
  chain at scale (1M elems, 200 iters, CUDA events) on the Orin 25W: fused
  (1 kernel, 16 B/elem) and ablated (3 kernels, 32 B/elem) BOTH saturate the same
  ~42 GB/s 1-elem/thread memory roof, so the per-row GBS is ~equal - the fusion
  win shows up as the fused kernel finishing 2.07x faster because it moves 2x
  fewer bytes. Report the sum-of-kernel-ns ratio (like tools/ptx/fusion-compare.f),
  not a bandwidth delta; a "both rows ~roof, ratio in the note" pair is the honest
  shape (maki/fusion-bench-device-test.f, dot habu-automatic-op-fusion).
- **The device emit child re-plans in a fresh process, so the fusion mode must
  ride in the model SOURCE STRING, not just the parent toggle.** `LOWER-DRIVER!`
  writes a child driver that appends a bare `FP-BUILD` (default fusion ON). To
  build ablated cubins, pass the model source with a trailing `; FP-FUSE-OFF!`
  suffix (child order: `MODEL:` -> toggle -> `FP-BUILD`) so child region ids/
  kernels match the parent's ablated plan. `FP-FUSE-OFF!` is persistent across
  `FP-BUILD` and `MODEL:`/`CAP-BEGIN` does not reset it.
- **A new maki device test needs no lint registration.** `filemap-lint` only
  walks src/tools/test/lib/bootstrap (not maki/), and `suite-coverage-lint` only
  scans test/gate-stdlib-*.f - so a `maki/*-device-test.f` (Orin-only, CUDA-probe
  SKIP off-device) keeps both lints green with zero registration; the ONLY rule is
  keep it out of `maki/test.f` (it needs CUDA + a device). This is why
  maki/onnx/deploy-device-test.f appears in neither lint table. Keep a device
  test device-PROVEN before adding it to `maki/test.f`: an unvalidated device fix
  in the shared maki gate can red master on the next on-device run.
- **Untrusted GPU launches must be SPAWN-isolated -- bare fork is NOT enough
  (dot habu-eval-grader-device).** The eval grader's device leg threw E-CUDA
  uncaught on a contained MMU fault (a no-check candidate using a raw span pointer
  as the grid index -> out-of-bounds read) and killed the grader before any tally.
  In-process `catch` alone is insufficient: a faulted CUDA context is not
  trustworthy for the next candidate. A first fix using a bare `PROC-FORK` child
  passed STANDALONE (parent had only dlopen'd libcuda) but failed INSIDE the maki
  gate: an earlier suite (the device numeric goldens) had already initialized CUDA
  in-process, and CUDA is fork-unsafe after init -- the forked child inherits
  poisoned driver state, every launch misgrades as fault, and no journal MMU fault
  appears (the E-CUDA is child-side init failure, not a GPU fault). The correct
  isolation is the GRADE-EMIT discipline: SPAWN a fresh `bin/hb` that loads the
  grader lib plus a generated launcher, classifies the launch under `catch`, and
  `die`s with a small (<256) exit code and an EMPTY message (`s" " code die`; a
  named message prints per launch and pollutes tally output). The parent maps the
  capture outcome (exit code / signal death / timeout) to the verdict; the capture
  timeout also bounds a HUNG kernel (SIGKILL-reaped -> graded fault). Grade a
  launch fault as a DISTINCT bucket (`EVN-DEVICE-FAULT` = "kernel crashed"), never
  as `EVN-DEVICE-WRONG` = "ran, bad values"; the ablation needs to show the
  checker prevents GPU faults, not only wrong numbers. Also measured on-device:
  expected buckets derived by value-level reasoning were wrong for 2/6 candidates
  -- the unchecked emitter turns register-discipline bugs into PTX that ptxas
  itself REJECTS at assembly, so they never reach the GPU; measure per-candidate
  verdicts on device before pinning tallies.
- **zed's ~/Work/habu is stale vs current master (missing files, not just a stale
  bin/hb).** Running a current-master device leg means transferring the FULL
  workspace tree to an isolated /tmp dir on zed and `HABU_ALLOW_BOOTSTRAP=1
  tools/bootstrap.sh` a fresh Linux engine THERE (gforth, ~30s) - never touching
  ~/Work/habu. Working fully in /tmp keeps the box sha-identical as-found with no
  bin/hb backup/restore dance at all. ptxas is at /usr/local/cuda/bin (add to PATH).

- **Jj's default word-level diff can visually concatenate numeric replacements.**
  A deleted `$200000` beside an inserted `$400000` rendered as
  `$200000400000`; inspect the source or `jj diff --git` before diagnosing a
  malformed token. Never probe `jj diff --check`; generate `jj diff --git` and
  run the repository's checked diff lint instead.

- **Track a new local bookmark before its first push.** `jj git push --bookmark`
  refuses to create an untracked remote bookmark; run `jj bookmark track
  <name> --remote=origin` first.

- **The dot CLI search verb is `dot find`, not `dot search`.** Unknown verbs
  fall through to quick-add and create malformed work; use only documented
  subcommands and remove accidental dots immediately before other work relies
  on them.

- **A post-hook replacement seam must not drag its legacy parser across the
  hook.** `sumtype.f` reads private type-family registries that the checker
  correctly rejects outside their package (`E-UNDEFINED ... TFAM-N`). Keep the
  legacy parser in its existing pre-hook phase until the unified checked
  declarers replace it; put only the empty replacement seam and independently
  checkable remaining core after the hook.

- **A downstream snapshot crash needs an exact old-order control before it is
  attributed to loader movement.** Rebuilding the same tree with the prior
  structures/effects order reproduced the identical stale-DATA-pointer fault,
  proving the snapshot regression was baseline rather than causal to the seam.

- **Sealing declared-effect parametricity must target sealed FAMILIES, not all
  concrete specialization.** The nominal-storage effect seal (post-body
  `NP-CHECK`) rejects a declared quantifier that resolves to an arity-0 nominal
  or layout family, plus quantifier aliasing. The first instinct — reject *any*
  concrete specialization of a declared quantifier — is a corpus bloodbath:
  `( ptr a -- n ) @` (46+ sites), `( -- ptr a )` (190+), etc. legitimately let a
  pointee `a` widen to a plain scalar. Grep the corpus for the risky signature
  shapes BEFORE choosing the predicate; the forgery target is validated identity
  (`CAD-KIND`/layout), and plain-scalar widening (`a := n`/`u8`) is the pervasive,
  intended looseness. `NOM-SCALAR? or LAYOUT-PARAM?` is the exact fence.
- **Row-walk the declared sig rows, not `NMAP`, to enumerate quantifiers.** Typed
  local annotations (`{: x:a :}`) route through `VAR-OF` and pollute `NMAP` with
  non-signature vars; walking `SGIN/SGOUT/SGRIN/SGROUT` (S-PUSH chain + `ptr`
  pointee, not into quotation/param subterms) yields exactly the declared
  quantifiers and dodges deferred parametric-cell governance. Reverse-map an id to
  its source letter through `NMAP` only for the diagnostic.
- **Test fixtures needing concrete-effect helper words should use checked words +
  `LAYOUT-BUFFER`, not `TRUST`.** `s" NAME" s" eff" TRUST` is counted as a
  `CL-TRUST` site by the trusted-inventory ratchet, so three scaffolding `TRUST`
  calls in `engine-suite.f` tripped `RATCHET-BAD`. A checked identity word
  (`: ID ( fam -- fam ) ;`), a checked collapse (`( g g -- g ) nip`), and the
  generated `LAYOUT-BUFFER` accessor (`n LAYOUT-BUFFER AT fam` → `AT ( n -- ptr fam )`)
  cover the value/alias/pointer producers with ZERO added trust surface and no
  `TRUSTED.md` edit.
- **A cold-prefix `.f` file sees a baked, name-stripped checker, not
  `checker.f` from disk (top-row landing).** `bin/hb` reloads prefix *content*
  from the checkout, but the visible dictionary at a prefix file's load is the
  last build's baked engine: internal `:`/`constant` names (`E-PTR`, `EN-CON`,
  `CHECKER-FIND-ACTIVE-SYM`, `PE-EFF@`) are treeshaken away, so a new prefix file
  can only use engine prims, core words, the curated public checker API, and
  hardcoded ABI constants. Editing prefix-file *content* needs no rebuild;
  adding a file to the *list* (habu2.f `PFX-LOAD-*`/`PFX-PATH`/`PFX-PROVIDE` +
  `LP*` var + `EMIT-LABEL-SOURCES` + hb-build-lib.f content key) does. Corollary:
  `set-check`/`set-top-check` are guarded-deref trust-boundary prims — a *checked*
  `:` word sees them as "undefined"; install from a `TRUSTED:` word (mirror
  `LOWER-CERT-HOOK:INSTALL`). And install *after* internal-mark: its top-level
  `0 set-check` suspends a token hook whose re-arm lives inside a word body
  (invisible to the hook), so an earlier install stays suspended for user code.
- **The R7 plan's type-schema spellings are pseudocode; probe the checker, don't
  transcribe them.** The addendum writes `STRUCTURE`/`FIELD`/`DERIVE eq`/
  `VARIANT got ... ;VARIANT`, but the real keywords are `TYPEFAMILY` (arity-0
  proof tokens, per sub-dot 1) / `PRODUCT` / `SUMTYPE` (concrete payload
  `VARIANT name type ;VARIANT`), `ENUM` takes no arity, and two footguns bite:
  bare dtype tails like `f32`/`tf32` are RESERVED atom tokens (rejected as
  variant names — class-prefix them), and variant names are PACKAGE-scoped
  (`TDECL-VAR-SCOPE?`), so N same-package slot sums cannot all reuse `got`/
  `absent`. Building the schema file + a `CHECK-QUIET-CANDIDATE!` probe and
  iterating on real checker output found both in minutes; guessing from the
  plan would have wasted a load cycle each.

- **Nullary proof-token mints (`( -- proof )`) aren't "mint-shaped" to
  refine-lint, but still seed them.** `RFL-MINT-SHAPE?` keys on `n -- family`
  (raw-in, family-out), so a `( -- certify-proof )` mint never trips the
  NEW-MINT shape scan — but the refine-lint seed list is the CONFINEMENT set
  (owner-file-only references), independent of shape. Seed private mints there
  anyway (mirroring sub-dot 1's `RAW>*`) so they can't leak, and give each a
  TRUSTED.md main-table row + classification row. The confinement scan skips
  `s" ... "` bodies, so a mint name inside a `CHECK-QUIET-CANDIDATE!` fixture
  string does not count as an outside reference.

- **Resolved fixups are not live assembler state:** counting historical rows
  exhausted `NFX` after `$1000` sequential forward references and made `LBL,`
  quadratic. Keep per-label pending chains, reclaim their slots through a free
  list, and make `NFX` count only simultaneous pending relocations.

- **A free-list head must name an allocated slot:** bounds against capacity do
  not reject a head in the never-allocated suffix. Validate it against the
  allocation frontier before dereferencing its link.

- **Single-pass relocation patching validates signed reach before masking:**
  `D26`, `D19`, and `ENC-ADRD` currently wrap out-of-range deltas silently,
  unlike the recovery assembler. Keep in-range opcode regressions while the
  separately tracked relocation-range fix adds fail-closed bounds.

- **A dependency edge names what the new dot blocks:** `dot add -a TARGET`
  records the new dot as blocking `TARGET`. For successor work, add successor
  IDs to the predecessor's `blocks:` and verify the resulting order with
  `dot ready`.

- **A live low-level word does not prove a matching require-registry row:**
  resident images can contain assembler words while `require` reloads their
  source and hits duplicate definitions. Capability-probe before conditional
  includes, then run both fresh-process and resident focused gates.

- **A boot-prefix primitive is not implemented until both emitters execute its
  focused behavior.** `tok-imm?` existed in `src/habu/habu2.f` but not
  `bootstrap/cg/forth.fs`; native fixpoint passed while no-binary recovery died
  with rc 70. Pair source-shape registration assertions with an actual Gforth
  stage-0 execution before claiming recovery parity.

- **A dot claim names the actual implementation workspace, not the first
  workspace that exposed the bug.** When review splits mixed work into separate
  commits, update and commit the claim before continuing in the new workspace;
  stale claims defeat overlap detection and review routing.

- **Scan every active dot's claimed files before dispatch, not only sibling
  workspaces.** The pointer-storage worker overlapped the active owner-persist
  lane in both engine emitters and recovery inventories. A workspace boundary
  prevents filesystem collisions; only an active-dot ownership census prevents
  semantic merge collisions.
- **Source certification must reproduce the real prefix, not rely on the live
  dictionary.** Once checker layout assertions consumed `CORE-LAYOUT-RC`, the
  standalone checker-self fixture failed while the real util/cell/checker load
  was valid. Build one reusable exact-prefix fixture and extend it for later
  layers; a fictitious standalone source is neither a positive nor a negative
  proof.
- **A pre-checker definer and its post-hook effect are one source-order unit:**
  isolating `PTR-VARIABLE` requires the definer before `checker.f` and its sole
  `TRUST` row after `check-hook.f` in every native, recovery, pin, cache, and
  diagnostic inventory; moving only the body breaks cold boot, while moving
  only the effect preserves the wrong owner.

- **Null-check the backing cell before refining a pointer field:** `ptr-field @`
  returns a typed pointer, so comparing it with numeric zero violates the
  checker contract. Test the raw address cell before allocating, then use the
  refined field accessor after it is nonzero.

- **Grow-on-demand readers consume the requested size exactly once:** duplicating
  a `FILE-SIZE` result before the grow helper left one `n` under `READ-ALL` and
  the checker rejected the reader's declared single-result effect. Let the grow
  helper own the size; read afterward from its stored capacity.

- **A checker-acceptance tightening breaks fixture files that only fail at
  LOAD time:** the wide-PRODUCT minimum-accounting fix made `FIND-SUB`'s
  `option<idx>` reject when bound to an `:n` local, and
  tools/build-fixpoint-test.f had two such latent bindings that turned the
  whole build-fixpoint suite red at load (rc 70) though no checker gate was
  red. After tightening acceptance, run the suites that LOAD tool fixtures,
  not only the checker gates; convert at the producer
  (`FIND-SUB BFT-FOUND {: idx:n :}`), never bind the sum to a scalar local.

- **A reusable buffer is live until its last semantic consumer:** owner-row
  freeze reused `AOT-REC-BUF` after record proof but before boot-manifest lookup,
  so fixture success depended on target records landing beyond the overwritten
  prefix. Give independently live artifacts disjoint regions, and regress the
  final consumer after every intervening phase.
- **A source lint needs a production census, not only mutation fixtures:** the
  clobber lint retained legacy label syntax while production moved to `LABEL@`,
  making a future clean result vacuous after its fixed input cap is repaired.
  Assert nonzero routine and edge counts against authoritative sources so token
  grammar drift fails closed.
- **Generated-call liveness needs explicit return and preserve contracts:** once
  current `LABEL@` calls became visible, seven of nine reports were legitimate
  saved-register or returned-register flows, while `LPROTWIDQ` exposed the real
  x5/x6 loop-state clobber. Keep contract fixtures beside a genuinely clobbering
  control, and back each preserve row with concrete save/restore code until the
  typed machine-effect schema replaces the table.
- **Make the native-process preflight a separate blocking step:** printing
  `pgrep` output and then starting a gate in the same shell still launches the
  gate when another workspace is active. Inspect a standalone process census,
  wait for zero real Habu children, and only then issue the native command.
- **Regenerate the typed-diff artifact after the final edit:** a later census
  helper introduced bare locals after the first lint pass, and only the commit
  checkpoint caught it. The proof must describe the exact tree being committed,
  not an earlier patch snapshot.
- **A typed-locals group does not replace the caller-visible effect:** putting
  `( -- result )` after locals can describe the post-binding stack while hiding
  the inputs from readers. State the full public effect before `{: ... :}` so
  reviews and tools see the same contract.

- **An optional lint root is an audit bypass:** `trust-lint` silently skipped
  absent roots and audited only `src/` plus `lib/`, leaving 16 existing
  `maki/` and `tools/` TRUST sites outside `TRUSTED.md`. Keep one explicit,
  required root set, recurse over both `.f` and `.fs`, and regress nested
  unmanifested sites by exact `file:line: word` diagnostics.

- **Describe bootstrap parity at the implementation boundary:** when the
  Gforth recovery host mirrors pass-2 lowering, calling parity merely
  behavioral or claiming there is no mirror contradicts the code. Name the
  mirrored lookup, validation, and emitter surfaces and prove the recovered
  compiler with the same focused fixtures.

- **Nested family payloads have two widths:** an SC-APP is one schema root but
  occupies the referenced family's full physical `WIDTH`. Sum padding,
  `SUMV-PAYCELLS`, product offsets, and constructor instantiation must all use
  that physical width; enum parsing must reset both root and physical counters.
- **Compute a nested tag offset once:** layout validation must pass the same
  canonical offset to the root check and every variant guard. Re-deriving it
  after cursor movement embedded offset zero in accessor certificates and
  rejected valid inactive variants.
- **A gate reference includes its fixture inventory:** adding child-load rows
  without the files, `FILEMAP.md`, and trust ownership makes the full gate fail
  at `E-FS-OPEN` before the intended assertion runs.
- **Generated checked fixtures are language clients:** their emitted source
  must obey lowercase family/variant grammar and use typed pointer conversions
  plus `ffi-call-bounded`; generated strings do not excuse obsolete primitives
  or untyped boundaries.
- **Sink guards must preserve the sink ABI, not only reject bad addresses:**
  adding an inline code-target guard to `LCEMIT` clobbered x12/x13, while
  lowering callers keep placement state live across emission. Save and restore
  every guard scratch register at the sink boundary, then prove mixed-width
  locals through the cold bootstrap compiler; a valid target address alone does
  not prove the guarded emitter is transparent.
- **Exercise ephemeral compiler mappings while they are live:** a post-compile
  protection test sees only an unmapped artifact. Trust only a syntax-simple raw
  state accessor; keep the immediate probe, sink action, and cleanup assertion
  checked. Enter pass-2 before the sink, use an invalid-fd `read` for non-mutating
  neighbor probes, then assert the mapping state is zero after publication.
- **Proof tables need arena growth when valid source length is not statically
  bounded:** the pass-2 local-bind table capped every definition at 256 bind
  occurrences even though branch joins keep the compiler's 64-live-local frame
  bound intact. Keep the live frame fixed, but grow monotone replay evidence and
  regress 257 sequential branch-scoped binds.
- **Immutable artifacts need size-derived mappings, not a generous fixed band:**
  a 64-KiB lowering blob imposed an undocumented certificate-size language
  limit. Page-round the validated source plus evidence, mmap exactly that span,
  protect it read-only during replay, and unmap it at transaction commit; keep
  only its base and capacity in protected engine state.
- **Consume package operations, not internal implementation words:** the check
  driver duplicated `LOWER-CERT-HOOK` internals to report and classify a verdict.
  Calling the public `LOWER-CERT-HOOK:HOOK` preserves one diagnostic/multi-error
  contract and lets the reporter remain private; do not replace package scope
  with global name prefixes or forwarding aliases.
- **Replay cursors start from the producer's sequence origin, not the pass-mode
  value:** `EM-P2-START` reused `x9=1` after enabling pass 2 and accidentally
  initialized the certified local-bind cursor to row 1. Keep mode and cursor
  initialization distinct; exact-consumption checks catch the tail mismatch,
  but the wrong first width can already emit invalid frame accesses.
- **A protected sink owns a byte interval, not an address:** start-only checks
  let unaligned `!`, syscall buffers, and fixed mappings begin immediately below
  compiler state and overwrite across its boundary. Pass the sink's real byte
  length, reject `addr+len` wrap, and test half-open lower/upper neighbors. When
  a compiler artifact is passed to `mprotect`, mmap it separately and round its
  length to the largest supported target page (64 KiB on arm64 Linux); macOS's
  16 KiB or a local Linux 4 KiB acceptance does not prove deployment safety. When
  an engine layout offset grows past 16 bits, every emitted
  load must use `LIT64,`; `MOVZ,` spills bit 16 into the shift field and silently
  initializes the wrong address. Re-derive each sink's pop order too: `BPATCH32`
  popped target into x9 but guarded x10 (the instruction word), so normal boot
  exposed the wrong-register guard as a false overflow while the real target was
  unprotected. Guarding `cp!` alone is insufficient: an allowed final-word CP
  still lets the next defining word overrun. Put the code-interval check in
  `LCEMIT` so every instruction write, including `CREATE` and `CONSTANT`, owns
  the same fail-closed bound.
- **Probe "over-conservative reject" claims by removing the guard and reading
  what breaks:** the TFAM 11 open-arg layout reject LOOKED like pure
  conservatism (scalar `( a -- a a ) dup` certifies + defers linearity to call
  sites, but layout `( tdlin<a> -- tdlin<a> tdlin<a> ) dup` rejects). Width was a
  red herring — cell-kinded params make a layout's width fixed regardless of args,
  so it's a linearity question, not width. But deleting the `LAYOUT-ARGS-OPEN?`
  gate in `PUSH-LOGICAL` (always expand) broke every construct-accept case
  (type-linear A1/A5/A6/A7 → E-REJECTED with declared==inferred): the 1-cell
  open-arg form is LOAD-BEARING for the constructor's raw→hidden boundary coercion
  (a ctor yields `lq2<ltok,?b>` with the un-provided arg open). Lesson: a
  fail-closed guard can be over-conservative AND structurally load-bearing at once
  — a checker-boot-prefix edit + one suite run separates "flip a fixture" from "a
  dedicated multi-part feature." (habu-tfam-11-linear piece 3: stop-and-report.)
- **Reuse the conservation machinery, don't add a transport special-case:** the
  TFAM 11 move-class relaxation (swap/rot/>r of a linear layout bundle) needed
  ZERO new classification code — deleting XG-READ-GROUP's blanket linear-bundle
  reject let linear bundles read through XG-READ-HID, and XPORT-APPLY's existing
  LIN-SNAPSHOT/LIN-CHECK (bundle counted once at its tag by LAYOUT-LINEAR-COUNT)
  already accepts a permutation (before=after) and rejects copy/drop. Locals
  capture kept rejecting for free (LOC-BIND-GROUPS removes the bundle from the
  counted rows at bind → count drop → reject), and boundary-loss/return-stack
  stranding are caught by the boundary balance. When a fail-closed v1 guard sits
  in front of a general count/conservation path, relaxing = deleting the guard,
  not adding op-class logic. (habu-tfam-11-linear slice 4.)
- **checker.f is a boot-time prefix — a checker edit is live at next boot, no
  rebuild needed to test:** run the fixtures against the current bin/hb
  immediately; the fixpoint rebuild is only for the byte-identity gate. But do
  NOT `rm bin/hb` before the install path (it builds to a temp then installs);
  a manual rm strands you with no builder — restore from a sibling fable
  workspace's bin/hb (same base) if it happens.
- **Snapshot-image regressions scan for the trailer magic and re-sign each
  patch:** the 48-byte trailer is NOT at file-end — SNAP-EXTRA-SIZE pad plus
  the macOS codesign blob follow it (measured magic at size-57392), so locate
  it by scanning for the LAST SNAP-MAGIC occurrence, never FILE-SIZE offsets.
  An un-resigned patched image is SIGKILLed (rc -9) before the loader runs.
  Corrupting the magic is a fall-through COLD BOOT (rc 0), not a reject; the
  rejection legs are version>max (rc 80) and a positive-but-oversized
  region-len/ndict middle byte (rc 79) — data-len top bytes can SIGSEGV
  instead. Fixture: BFT-TEST-SNAP-TRAILER.
- **An assembled-source certify pass makes file-local `0 set-check` windows
  false-reject; convert the window to a named TRUSTED: boundary instead of
  teaching the verifier to skip:** BF-CERTIFY-SNAP rejected snap.f (undefined
  SNAPGO from require'd snap-lib.f inside its `0 set-check` window) and
  blocked `-- snap`. Making VERIFY:SOURCE-BUF honor set-check spans would
  have weakened every generated-source certify; converting SNAP-RETIRE-GO to
  `TRUSTED:` (body skipped by the scanner, effect audited in TRUSTED.md)
  fixed the route, let BF-AUDIT-BOUNDARY pin BFR-CHECK-OFF as the only
  check-off line in generated sources, and retired snap-build.f outright.
- **Generated constructor WID protection belongs after emission, not inside
  `C-STORE`:** a `C-STORE`-time predicate re-enters Forth while the native
  definition machinery is mid-publish and has no stable generated-word identity.
  The sound producer is post-generation: regenerate one constructor name from
  SUMV metadata, validate it with `TFAM-CTOR-WORD?`, resolve the record through
  xref, then native `prot-wid-add` the wordlist. Native seal rejects need a child
  process fixture because rc 84 exits the engine and cannot be caught in-process.
  Calling an unchecked-prefix predicate from checked xref code needs a `PRIM:`
  effect row (type-family.f loads before xref.f in every context) — NOT a
  `TRUSTED: ... evaluate` name-lookup wrapper, which launders the call and adds
  a manifest row for a word that is lexically in scope.
- **bin/hb keeps NO baked checker DATA — the boot prefix re-parses from disk:**
  `EMIT-COLD-PREFIX` (habu2.f PFX-LOAD-BASE-FILES) reloads util→checker→
  type-family→…→sha256→combinators→xref at EVERY boot, so top-level writes to
  checker cells from COMMON-tail files (habu2.f) never reach the installed
  binary — a canary literal store proved it. A hook between a checker-prefix
  file and later support must live in its own prefix file (e.g.
  `src/core/type-family-sha.f`) registered in ALL prefix/source registries:
  habu2.f PFX tables ×3 + label var/init, bootstrap/cg/forth.fs mirrors ×5,
  build-fixpoint COMMON+SNAP-KEEP, srclist.f, bootstrap.sh SRC_COMMON,
  diagnose-hb-core.f (+ COMMON-N count in diagnose-hb-test.f), shadow-lint.f,
  hb-build-lib.f key list, test/run-files.f, FILEMAP.md.
- **Escape every joined segment, including the last:** the constructor-package
  derivation escaped '-' only inside the package segment, so top-level family
  `a-b-c` collided with package `a` family `b-c` (both `A-B-C`) — the pinned
  A-B+c example passed and hid it. An escape/join scheme is injective only if
  EVERY joined component is escaped; a raw final component is safe only behind
  a fixed-width delimiter (the SHA-fallback's 16-hex region).
- **Suite helpers that return shared-buffer strings need an intern before the
  second call:** TF-CTOR-PKG$ returns TF-CTOR-BUF; asserting determinism by
  calling twice compared the buffer with itself (vacuously green) until the
  first result was TF-INTERNed. Copy-out before re-deriving in tests.
- **A "free" DATA offset next to a guarded band may be someone's base pointer:**
  $40C8 (just past the PROT-REG band) looked like the natural home for the
  sealed TOP-HOOK-CELL, but it IS lib/task.f TASK-USER-BASE and test/seal.f
  pins it writable. New sealed cells go into an rg-verified reclaimed hole
  ($27F0, the retired descriptor-hook slot) with their own GUARD-SPAN/
  PROT-GUARD band instead of growing an existing band whose end is a public
  boundary. Also: a pre-BLR dispatch event means the uninstalling
  `0 set-top-check` call logs itself — event-sequence fixtures must count the
  trailing two events.

Concise findings only: what worked, what failed, why. Coding standards live in
`docs/forth.md`; API details in `docs/` near their feature. One tight bullet per
lesson — keep the specific word/code/path, cut the prose.

---

## Part 2 — dated `##` campaign sections (was the tail of LESSONS.md)

## Dict-hash stage 1 landed: dormant table + LHIDXBUILD (2026-07-03)

Third attempt, evidence-first, is green through stage 1 (table infra +
startup build, no consumers): install --force byte-fixpoint holds, gate
PASS, trust-lint 0, typed-local-diff-lint 0, `-- snap` builds. Keys:

- Recovered the verbatim attempt-2 step-2 code from `jj op log` (the
  revert op `jj restore src/habu/{habu1,habu2,layout,snap-lib}.f TRUSTED.md`;
  its hidden pre-image is the working code). Reapply from history, don't
  rewrite.
- The recovered code had a real bug to drop: `LBL LHIDXADD ! LBL
  LHIDXBUILD !` was written TWICE (a half-applied jj restore, finding #6) —
  allocate each label ONCE.
- Startup control flow (verified on the CLI-refactored tree): EM-STARTUP
  runs MMAP-DATA -> DATA-INIT -> SNAPSHOT-RESTORE (falls through both arms)
  -> STARTUP-RUNTIME-STATE, whose `LVRINIT BL,`/EMIT-SOURCE point is reached
  unconditionally after the `cwok` merge with data mapped + NDICT final.
  That is the correct LHIDXBUILD BL site (cold + warm).
- The gate never boots hb-new/snapshot images as launchers (bootstrap.md),
  so the descoped restored-image rc-134 crash cannot redden the gate; `-- snap`
  only *emits* hb-new (runs cold-boot hb-stdin), never boots it.
- Trust-drift minimization: put emitted BLs on the SAME physical line as an
  existing emit call (`LVRINIT LABEL@ BL,  LHIDXBUILD LABEL@ BL,`) so no
  line is added and the 54 downstream habu2.f TRUST rows never drift. Only
  genuinely new lines (snap-lib zeroing, EMIT-HIDX in the section list)
  drifted 4 rows; re-pin by hand from trust-lint's SITE-DRIFT scanned-line.
- `W32` is a BAKED engine word: a `: W32 ... ;` smoke "duplicate definition"
  is pre-existing (identical on the clean baseline), not a hash-table bug.
  Always run the new smoke on the baseline binary before trusting it.

## Dict-hash landed end-to-end (FIND + dup-check probes) (2026-07-03)

Attempt 3 completed all 5 stages, each byte-fixpoint + gate green:

- **First-match-stops is safe because the engine rejects duplicates.** Proved
  with `: R ; : R ;` -> exit 78. So each (wid, folded-name) key is in the table
  at most once; the probe stops at the first VALIDATED slot (idx<NDICT, wid==x2,
  folded name equal) or the first EMPTY slot. `undefine` retires IN PLACE
  (xref.f XREF-RETIRE sets record+40 wordlist to XREF-RETIRED-WL = -2), NOT by
  truncation, so retired records keep their slot but are skipped by the wid
  check (-2 never equals a real search wid). Verified: undefine+redefine and a
  duplicate-behind-a-retired-slot both resolve/reject correctly.
- **The linear FIND-LOOP already "continues past a match" (last-wins), which is
  equivalent to first-match only because dups are rejected.** Don't assume; it
  reads like last-wins but there is exactly one live match.
- **DICT-CAP (8192) < HIDX-SLOTS (16384) is the no-infinite-probe invariant** -
  the open-addressed table is at most 50% full, so both insert and probe always
  hit an empty slot. No bound needed.
- **The dup-check probe is AUTHORITATIVE (not a fallback) on the no-dup path** -
  that is where the O(NDICT)->O(1) win is. Keep the linear scan only behind an
  `HIDXP==0` guard. The self-host fixpoint is the proof: a wrong no-dup verdict
  would allow a double-definition and break the byte fixpoint.
- **Factor an emitted probe into habu1.f (past the last TRUST row) and wire it in
  with SAME-LINE edits to habu2.f for ZERO trust drift.** `C-HIDX-DUP?` lives in
  habu1.f; C-REJECT-DUP-DEF gains only two modified lines. Label LOCALS are
  referenced BARE (`dnext BCOND`); only global label VARIABLES take `LABEL@`
  (`FIND-DONE LABEL@`). Mixing the two is a wrong-address branch = crash.
- **Feature growth trips size/buffer watermarks; bump them same-commit.** The
  engine grew one 16KB page: size ratchet 132343->148855 in gate-build-size.f.
  The AOT maker source (engine + AOT driver, ~$80173) crossed the 512KB
  MK-SOURCE-CAP; the stdin path (S2-SOURCE-CAP, same $80000) had only ~6.4KB
  margin. Both are BUILD-DRIVER buffers (maker.f/stage2.f, not baked into
  bin/hb) -> bumping to $A0000 does NOT change the fixpoint binary.
- **Measure the engine speedup with `0 set-check`** to isolate parse+FIND+dup
  from the (linear) checker: 2.44x at W=6000 and growing with W, vs ~1.16x with
  the checker on. The hash flattens the engine's O(W^2); the checker is the
  linear remainder.

## Design-before-evidence caused two emitter reverts (2026-07-03)

Two dict-hash implementation attempts were reverted for preventable
reasons, all research/design failures, none coding failures:

- Designed around an ASSUMED runtime semantic (newest-wins redefinition)
  that a one-line stdin experiment against bin/hb would have falsified
  (`: R 1 ; : R 2 ;` -> engine REJECTS duplicates, exit 78). Verify every
  semantic assumption a design rests on with a direct experiment BEFORE
  writing code against it.
- Emitted code into unmapped control flow twice: EM-SEED-DICT runs before
  the data region is mapped (the 8-line EM-STARTUP word says so), and the
  EM-STARTUP-RUNTIME-STATE tail after EMIT-SOURCE is unreachable on some
  paths. Before inserting into an emitter region: read the containing
  words end-to-end, list every entry path, and verify reachability with
  the debugger where any doubt remains.
- Used an unvalidated test harness (piping into hb-stage) for hours; the
  clean-tree control run that exposed it took two minutes. Run the control
  FIRST every time a new validation method is introduced.
Debugger note that unblocks all of this: breakpoints on hb binaries only
stick if set on the LIVE process (`process launch -s`, then `br set -a
<addr> -H`, then continue) - pre-launch breakpoints are wiped by startup
text remapping. The binary funnels all exits through two syscall sites,
so "which path exited" is one breakpoint away.
- **A spawned build child must be given /dev/null stdin, never inherit it:** the
  intermittent stdlib/tail-build hang was `BUILD-RUN` (lib/build.f) running
  fixture command scripts through `PROC-RUN-RC`, which spawns with `-1 -1 -1`
  fds = inherit all, including fd 0. A `#!/usr/bin/env bin/hb` script falls into
  the stdin REPL after its body and `read`s fd 0; when the pool worker's
  inherited fd 0 is an open never-EOF pipe, it blocks forever (only reddens via
  the per-phase timeout). Reproduce deterministically: `bin/hb cmd.f` with fd 0 =
  an open fifo (writer held open) hangs; `< /dev/null` runs and exits rc 0. Fix:
  BUILD-RUN opens /dev/null read-only and passes it as infd via PROC-RUN-IO-RC
  (out/err still -1 = inherited so pool logs capture output). This is the
  root-cause fix; adding `bye` to each fixture would be a redundant band-aid.
  Separately, pool children that must die on parent SIGKILL is orphan-cleanup,
  not this hang's cause.

- **An uncaught throw exits with the raw code and ZERO output — always give
  build drivers a reporting boundary:** the post-rebase "in-pool-only" AOT
  gate RED (maker build rc 74, empty logs) was a 104-byte image-buffer
  overrun: engine text + embedded source grew past `MSIZE` ($90000), `M-FAIL`
  `2drop`ped its own message and threw 75, and `BTHROW`'s no-handler path
  (habu1.f) exits with x0 = the throw code, silently. Nothing was
  pool-specific — the pool was just the only place a fresh maker build ran.
  Diagnosis pattern that worked: catch-wrap the driver phase (`WHY-THREW`
  style) to get the code, then phase-bisect inside the driver; renumbering
  candidate exit sites (diagnostic build) proved the code was COMPUTED, not a
  literal — `throw` codes surface as exit codes mod 256 (75 could be -181,
  -2229, ...; here it was literal M-BOUNDS-RC). Fixes: MSIZE derived from the
  loud MPAGE guard + load-time `*-MSIZE-CHECK` asserts in both writers so the
  buffer can never again be the binding constraint; `M-FAIL` now writes its
  message before throwing; `DRV-FAIL` (driver-io.f) reports "driver: uncaught
  throw code N" at every stage/maker/build driver boundary while preserving
  the exit code; hb-build's maker die message carries the child rc.

## AOT-REPL milestone 1: one-word seed baked into bin/hb (2026-07-03)

Proved the EM-SEED-DICT-style AOT seed end to end: a metabuild-compiled word
(`AOT-PROBE`) is emitted into bin/hb as a dict record + code blob and is
callable at boot with no source parse (EM-SEED-AOT in habu2.f, armed by the
stdin driver's AOT-CAPTURE). Fixpoint byte-identical, gate green, size
unchanged (148855). Keys for milestone 2:

- **x13/x14/x15 carry argc/argv/envp until EM-DATA-INIT stores them.** Any
  boot emitter that runs BEFORE EM-DATA-INIT (EM-SEED-DICT, EM-SEED-AOT) must
  not use x13-x15 — a copy-loop counter in x14 clobbered argv and SIGSEGV'd in
  argv processing at boot. Use x12. x20 is XREG-RBASE (text base) pre-DATA-INIT
  and DATA base after — one register, dual role; read RBASE-CELL for the saved
  text base once x20 is repurposed.
- **The AOT sample/REPL word must be defined in a driver loaded ONLY for the
  stdin build (stdin.f), never in habu2.f.** habu2.f is reloaded in every
  build, including the stage2 build an AOT-seeded bin/hb runs to rebuild
  itself; `: WORD` there hits "duplicate definition" against the seeded copy
  and aborts the install. The stdin metabuild host is hb-stage (built from
  stage2.f, empty AOT, no seed), so its definition is always fresh — that IS
  the fixpoint's escape hatch.
- **Don't `evaluate`/`set-check` inside the metabuild GO — the interpreter's
  checker-hook state makes them throw.** Define + measure through the normal
  top-level source path. habu1/jit/habu2 load CHECKED (`' HOOK set-check` after
  the CHECK-OFF span), so raw region byte reads need SRCA@-style TRUST ptr-u8
  accessors (+ TRUSTED.md rows + inventory `TRUST` baseline bump).
- **New emitter label locals need `:label` typing** (`{: x:label :}`) or
  typed-local-diff-lint flags the new group even though older emitter groups
  are untyped (lint only sees the diff).
- **Open-address dictionary indexes must reclaim stale rollback slots on
  insert, not only skip them on lookup.** `ndict!`/`cp!` rollback can leave
  probe-chain entries whose record index is no longer live. Duplicate lookup
  must keep probing past them, but insertion must reuse the first stale or
  empty slot; otherwise repeated checked/evaluated candidates eventually fill
  the fixed table with dead entries and spin.
- **Process-pool orphan protection is fail-closed.** A spawned child whose
  process group or parent-death reaper cannot be armed must fail the test
  immediately through the pool cleanup path. Running the test anyway silently
  drops the only proof that timeout/parent-death cleanup reaches grandchildren.
- **Bootstrap writer caps are temporary-stage caps, not final-binary budgets.**
  The Gforth recovery stage embeds current source in its generated executable;
  when source grows, the bootstrap-only `MPAGE`/`MSIZE` guards must grow
  together or no-binary recovery fails before native fixpoint can produce the
  small `bin/hb`. Final-size ratchets belong in `test/gate-build-size.f`.
- **Diagnostic lint fixtures must mute expected findings.** `MDL-COUNT` was
  correctly counting synthetic `maki/` tokens, but it printed those expected
  findings into the shared lint-tools output and the gate-stats fixture counted
  them as real span noise. Public lint entrypoints stay loud and fail-closed;
  in-memory fixture counters need an explicit quiet/report switch.
- **Do not export build scratch cells as trusted API.** Image writer state such
  as `MLEN` should be hidden behind checked accessors (`MLEN@`/`MLEN!`) and
  typed role constructors (`M-O@` via `>OFF`) instead of manifest rows for raw
  variables. Also keep checker boot independent of helpers loaded later in the
  stage source; a checker self-type helper must use already-available typed
  primitives, not `BYTE+` from `src/core/bytes.f`.
- **Assert durable formats exactly; splice runtime fields at test time.** When a
  committed string (cache key, report row) gains a runtime-dependent field (the
  bin/hb content key), do not weaken the test to substring containment - three
  fragment checks prove neither order nor adjacency nor absence of junk. Build
  the expected string by splicing the runtime field between committed literals
  and compare whole with STR=/T$=. If the actual lives in the shared SB builder
  (SK-KEY$), copy it out before building the expected string in SB.
- **Pipe-delimited keys are a tracked ADT seam, not a design.** Stringly keys
  make semantically distinct fields (dtype vs layout) indistinguishable bytes;
  the typed-record swap is specced in habu-cad-adt-swap (blocked on TFAM 14/15).
  Keep string rendering at the durable-store boundary only.
- **A gate only gates if it guards the push.** Chaining `gate; ...; bookmark set
  && push` runs the push even when the gate failed (the `;` discards the gate's
  exit). fable was briefly pushed red when a clean rebase hid a semantic
  cross-lane conflict (capture-time shape legality vs a lowering test fixture
  that NEEDED an illegal model). Regate the merged tree, CHECK the result, and
  only then move bookmarks - separate commands, or `&&` from the gate onward.
  Cross-lane semantic conflicts do not show up as rebase conflicts.
- **`tools/typed-local-diff-lint.f` returns to the REPL - feed it `</dev/null`.**
  It ends with a plain `throw`-on-findings, not `bye`; a bare
  `bin/hb tools/typed-local-diff-lint.f diff.patch` then blocks reading stdin and
  looks like a multi-minute hang/timeout (rc 124). Redirect `</dev/null` and it
  exits fast (rc 0 clean, rc 1 on findings). Same shape for any tool that does
  not exit at end-of-load.
- **`tools/check.f <file>` preverifies in isolation - not for require-dependent
  files.** It does not process the file's `require` chain or FFI/`deftype`
  metaprogramming, so it reports `E-UNDEFINED` on lib words (`>CSTR`,
  FFI-generated bindings) even for green files. Typecheck a device tool the way
  it actually loads: `bin/hb --load <full prelude> <file>` with the trailing
  `MAIN` run line stripped (`sed '/^MAIN$/d'`) so a clean typecheck exits 0
  instead of throwing at the off-device `CUDA:OPEN`.
- **A launcher that reuses another file's internals rots silently.**
  `tools/ptx/matmul-device-test.f` borrowed `ED-SYM`/`ED-LIB`/`ED-H` from
  `maki/eval-device.f`; when eval-device migrated to the checked bindings those
  DLSYM words vanished and matmul was left uncheckable (nothing gated it). Prefer
  a shared library (`lib/ptx/cuda-driver.f`) over reaching into a peer's cells.
- **Opaque habu exit codes are `throw-code mod 256` - decode before guessing.**
  A device test dying with exit 56 / exit 211 and no diagnostics was
  `E-PROC-TRUNCATED` (-2504) / `E-GA-CAP` (-5165): add multiples of 256 until a
  known error constant appears, then grep `lib/errors.f` and the owning package.
  Two real hits in one session: the blocked-tile PTX (~28 KB) overflowing a
  `$4000` child-capture buffer, and 3 blocked-GEMM inputs (8256 cells)
  overflowing the `$2000` golden input arena. Size capture/arena buffers to the
  cap the admitting path enforces (launch caps: 4 inputs x 4096 elems), not to
  the first example that fits.
- **The generic SBSA torch wheel has no sm_87 ATen kernels - only Triton's own
  JIT output runs.** Anything that calls a torch GPU kernel fails with
  `cudaErrorNoKernelImageForDevice`, including the HIDDEN one inside Triton's
  autotuner (`do_bench` clears L2 via `tensor.zero_()`). Pass a custom
  CUDA-event `do_bench=` to `@triton.autotune` and keep torch to alloc + memcpy.
- **Triton `tl.dot` on fp32 silently runs TF32 tensor cores on sm_87** - a
  measured `rel_err ~8e-4` vs a CPU f32 reference is the fingerprint (pure f32
  FMA gives ~1e-6). Record the arithmetic class next to any GEMM number; a
  Triton-vs-f32-FMA comparison is cross-precision and must say so.
- **A perf hypothesis is only worth its ablation - "load/ALU-bound" was wrong.**
  The MMA rung-1 diagnosis (scalar fragment loads + 48 cvt/tile starve the
  tensor cores) predicted ldmatrix as the biggest jump; a 3-mode single-variable
  ablation (`cg-mma.f MMA-LMODE`) falsified it: dropping every cvt is FLAT and
  ldmatrix.x4 is ~1.2% SLOWER (43 vs 38 reg). When throughput is invariant to a
  mechanism swap, the bottleneck is what the swap holds constant (here the mma
  dependency chain + bar.sync cadence). Build the cheap ablation BEFORE the big
  rewrite; a negative measured result that redirects two dots is a deliverable.
- **tf32 fragments ride ldmatrix.b16 as half-pairs - no tf32 ldmatrix needed.**
  A tf32 value is 2 adjacent b16 halves, so an 8-row x 4-tf32-col tile IS one
  8x8 b16 ldmatrix tile and the 16x8 A fragment = one ldmatrix.x4 whose 4 result
  regs map exactly to the mma.sync A layout. And mma.sync.tf32 reads the top
  bits of the raw f32 register, so `ld.shared.b32` with NO cvt is a valid tf32
  feed (truncation vs cvt.rna RNE: <1 ulp tf32, inside the licensed rtol 2e-3;
  keep cvt.rna where the golden must stay bit-identical).
- **bin/hb bakes ONLY primitives - a generated `constant` is re-read source, not
  image data.** The stage source's colon words/constants (the whole checker
  included) are re-parsed from the checkout at every boot (PFX-LOAD-BASE-FILES);
  EMIT-DICT bakes just the #PL primitive registry, and the AOT boot-run seed
  arms only on the interactive REPL entry. So "bake a digest into the image and
  verify at startup" designs need a metacompiler baked-data capability (dot
  habu-baked-boot-data), not an emitted source line — an injected boot token
  (VERIFY-BOOT-PIN) died E-UNDEFINED on every boot and bricked self-rebuild
  (restore bin/hb from a sibling checkout). Corollary: a fail-closed-by-default
  boot check deadlocks self-hosting — the rebuild command boots the engine over
  the very prefix it needs to rebuild; default off/warn, strict opt-in.
- **CHECK! is line-oriented and registers what it certifies.** Feeding it a
  multi-line definition body returns verdict 1 no matter how well-typed —
  whitespace-normalize to one line first (prop-test builds single-line PBUF
  strings). And after a -1 verdict the name is registered, so a CHECKED
  re-compile of the same text dies duplicate-definition: compile certified text
  under `0 set-check` and reinstall the hook right after (CHK-COMPILE-CERT
  shape). Leaving the hook off compiles later definitions untyped, which makes
  every later CHECK! that references them reject — the failure appears one
  check DOWNSTREAM of the mistake.
- **Driving real emitters in isolation works via `included` trio + certified
  eval, not via the built image.** Retired emitter xts in bin/hb crash on
  `execute`; instead load src/arch/arm64/{asm,icode,mnem}.f live (object-image
  precedent), extract the emitter definition from the stage source, CHECK! it,
  compile, run, and decode CODE words + fixup tables. Generation-suffixed names
  (NAME-A, NAME-B per check region) beat CHK-MARK rollback for repeated checks:
  two engine bugs make the rollback route unusable today (undefined-in-evaluate
  under catch crashes natively, dot habu-undefined-word-in-d9dc3452; CHECK!
  records go stale across ndict rollback + re-definition, dot
  habu-check-records-go-4f62cd2e).
- **Clean up worker workspaces immediately after merging (user rule
  2026-07-07).** Once a lane's commits are merged into fable and the dot is
  closed, `jj workspace forget <name>` + remove the directory in the SAME merge
  window - never leave merged workspaces around. Dozens of stale checkouts
  invite exactly the collision we hit: another session's cleanup swept them
  (with one lane's uncommitted work) into ~/Downloads. Corollaries: (a) prefix
  workspaces by campaign owner (fable-*) so ownership is visible; (b) workers
  commit early - uncommitted work in a workspace is one cleanup away from loss;
  (c) never bulk-remove workspace dirs you do not own.
- **xts live in TWO code regions; validate hook installs against the JIT one
  (dot habu-stdlib-check-hook-fd883aea).** Baked primitives (`' dup`) sit in the
  engine binary's `__text` at the PIE image base (~`$104…`, measured 4.36e9);
  runtime colon/`TRUSTED:` defs are JIT-compiled into the RBASE-VA region
  (`$300000000..+REGION`, x26/DBASE .. x28/CP). Every real checker hook — `HOOK`,
  `SNAP-CHECK-HOOK`, `USER-HOOK`, `ES-VERDICT-HOOK` — is a source-loaded def, so
  its xt is always in `[DBASE, CP)` when `set-check` fires (even inside the baked
  `bin/hb`, the cold prefix re-JITs the checker). So `set-check` can fail-closed
  cheaply with two register compares `DBASE <= xt < CP` (no LIT64): 0 stays legal;
  `1`, a DATA-region address (~4.67e12 > CP), a baked-prim xt, and a code word
  mis-read via `dbase@ HOOK-CELL + @` (~4.34e9 < DBASE) all fall outside and die
  rc-70 with a named diagnostic instead of BLRing into garbage. Limit: the window
  can't tell a true entry from any in-range address (mid-instruction / dict
  record) — it catches wild installs, not a well-formed pointer into live code.
  Measure xt magnitudes with the engine before choosing the predicate; do not
  assume one region.
- **A computed-argument `set-check` (e.g. `check@ set-check`) is a trusted-
  inventory site.** The scanner skips string bodies, so `s" … set-check"` fixtures
  don't count, but a real `set-check` whose argument is neither literal `0` nor a
  ticked name lands in the file's file-level count in `TRUSTED.md`. Adding the
  round-trip test to a file with a `file class dot N` count row means bumping N
  (engine-suite 47→48), not adding a named row — that ratchet is separate from
  `checked-boundary-lint` (which passed it as a test boundary).
- **jj worker workspaces have no `bin/hb`.** `bin/` is gitignored, so a fresh
  `jj workspace add` tree cannot run gates or spawn-based fixtures (task tests
  die with `E-PROC-SPAWN`). Provision each worker workspace with a *copy* of
  `bin/hb` (never a symlink into the main tree — a workspace rebuild through a
  symlinked `bin/` would overwrite the binary other workers are using).
- **Friend-arena write seal (TFAM 2b-i).** The sound way to make checker/wordlist
  state unforgeable from user source is a runtime range guard at every raw write
  *sink*, not name-hiding or type-provenance: only the sink sees the real target
  address, and `data-base <off> + !` computes it with no engine-word name. Layout:
  relocate the crown-jewel cells into ONE contiguous DATA band placed BELOW
  `DATA-START` (so `allot`/`,`/`c,`/DP, bounded ≥ DATA-START by DP-CHECK, cannot
  reach it by construction — no guard needed there). The latch cell IS the band
  base (0 = open, band-len = sealed); the guard reads it, so post-seal any write
  into the band — including the latch — traps: one-way self-sealing. Guard shape
  (two-band since 2b-v, native + stage0 mirror): `ldr latch;cbz open` gate, then
  per band `sub;cmpi;b.cc trap` (exit ENGINE-ERROR:SEAL-VIOLATION), mirroring
  `B-TASK-LIVE-GUARD`/`DP-CHECK`; the branch makes the leaf body non-inlinable so
  it compiles to an out-of-line call (correct, ~size-neutral). Seal is emitted by
  the cold-prefix generator (after `PFX-PROVIDE-FILES` in `LCOLDPFX` and in
  `C-SOURCE-BAKED`) so it fires before the first user token on every entry
  (--load / stdin / evaluate / REPL / baked). Engine writes to the same cells use
  dedicated `DATA <CELL> STR,` prims (BSETCUR/BWORDLIST/BSETCHECK/…), never `!`,
  so word-definition/packages/TRUSTED:/DEFER keep working while raw `!` traps.
- **Emitter words in habu1.f/habu2.f are checked.** `: FOO ( areg -- )` is a typed
  stack effect, not a comment — `areg` is an unknown type. A word taking a register
  number is `( n -- )` (like `DP-CHECK`). And definition ORDER matters: a guard used
  by an early prim (e.g. `BPOLL` ~line 543) must be defined before it, or the build
  dies `E-UNDEFINED` — habu1.f words ARE visible in habu2.f (DP-CHECK is used there),
  so a cross-file E-UNDEFINED is really an earlier same-file forward-reference.
- **Worker workspace path discipline.** Edit/Read the worker workspace absolute
  path (`.jj-ws/<ws>/…`), never the main-tree path — a `cd` in Bash does not change
  where Edit/Read resolve. Editing `/Users/.../habu/src/...` while Bash builds in
  `/Users/.../habu/.jj-ws/<ws>/src/...` silently edits the wrong tree (the build
  never sees your change; the main tree gets polluted). Commit each proven-green
  slice immediately — uncommitted worker edits can be clobbered.
- **Gate timeouts under concurrent-agent load are false.** A gate RED of
  `hb script argv mode`/`process-test` with `outcome: timeout code 9 / rc 137` or
  `fork worker throw -2502` at ~2min wall / <30% CPU is fork/spawn exhaustion, not
  a real break. Re-run `test/run.f -- --pool-slots 2 --nested-pool-slots 1
  --budget-ms 240000` and re-run the failing fixture in isolation `</dev/null`
  before treating it as a regression. (`bin/hb file.f` also drops to a stdin REPL
  after the file, so test file-mode with `</dev/null`.)
- **Linear values laundered through `{: :}` locals — locals bypass `LIN-CHECK`.**
  `LOC-REF?` re-pushes a local's tv straight onto `DCUR` with no CHECKER-STEP, and
  `LOC-BIND` of a *typed* linear local is an explicit step (`LINEXP=1`) that skips
  snapshot/check — so `{: x:own :} x x` (dup), `{: x:own :}` (leak) and
  `x FREE x FREE` (double-consume) all certified while the stack `dup` correctly
  rejected. Fix (option b, a real static discipline, not a guard): reject binding
  any value that resolves linear into a local outright (`LIN-LOCAL-BIND-CHECK` in
  `LOC-BIND`, new `LINLOCBAD` flag → `E-LINEAR-LOCAL`/`factor_linear_local`); plus
  taint every poly local reference (`LIN-LOCAL-REF-TAINT` in `LOC-REF?`) so the
  deferred case `( a -- ) {: x :} x x FREE FREE` (poly local that only later binds
  linear) is caught by `LIN-TAINT-SCAN`. Both are gated on `LIN-ANY?`, so the whole
  no-`deflinear` self-build/stdlib pays nothing and non-linear locals are untouched.
  Full path-sensitive consume-exactly-once (referenced once per live branch instead
  of blanket-reject) needs per-local live state snapshotted on the CF frames —
  deferred to item-12 locals width-awareness, dotted separately. Lesson: an
  explicit-effect step and a bare tv re-push are the two ways a discipline that
  only watches `DCUR`/`RCUR` counts can be blindsided; a new value sink (locals,
  fields, a store) must be checked at its own bind/ref site, not assumed covered.
- **DISCOVER (record-only event log) could not replace check-core's whole-file
  dep scan until its walker scanned colon bodies.** The original
  `tools/source-discovery.f` skipped every colon body, so it never saw the
  dominant real idiom — a `s" path" required`/`included` guarded inside a
  colon-defined helper then bare-called at top level (e.g.
  `tools/check-all-errors-core.f` `CA-MAYBE-VERIFY-SOURCE`) — and EC:BUILD keys
  built on it could return stale hits. RESOLVED (TFAM 5 redrive,
  docs/design-tfam-5-redrive.md): the walker now lexes the ENTIRE token stream,
  records guarded loaders unconditionally (superset of the runtime closure —
  safe for keys, and sound for preverify under the repo's monotone
  load-if-absent guards), and rejects fail-closed on dynamic paths, loader
  shadow/undefine, `UNDEFINE-IF-DEFINED` retirement, and bad openers unless the
  file is a declared boundary in `tools/dynamic-tail-manifest.f` (path+reason
  table; entries skip the offending form, never record it). Verify any future
  closure source against the colon-wrapped idiom before trusting it.
- **A scratch-capacity limit must fail only the consumer that needs the value,
  not the whole scan.** Discovery's loader-path scratch (`SD-PATH`, $400) threw
  `E-DISC-CAPACITY` while merely *consuming* any >1KB string literal, falsely
  rejecting `src/core/checker.f` from whole-file scanning. Runtime loader paths
  are capped at `INCLUDE-PATH-CAP` anyway, so oversized strings are legal data:
  set an overflow flag during the string scan and reject only if that string
  reaches a loader word.
- **`SB-CAP` is 1024 — build >1KB test fixtures with `APPEND-FILE` loops, not
  the string builder.** An SB overflow inside a fixture builder exits the whole
  suite with a bare masked rc (e.g. -3804 -> exit 36) and no output; if a test
  run dies silently with a small rc, suspect an uncaught throw in fixture
  *construction* before T-REPORT.
- **`--all-errors --source-list` was a proven no-op (D4) — the redrive checks
  original files with cross-file support.** The materialized temp is only
  `"path" required` lines (zero defs), so all-errors scanned nothing and only
  preverify's FIRST error surfaced. Now `CHK-RUN-ALL` in list mode walks
  `CHK-DEP-ORDER`, runs `CHECK-ALL-ERRORS-FILE` per original file, and
  registers each clean file through `CHECK-ALL-ERRORS-SUPPORT+` so later files
  see real prefix state (support replayed after every `CHECKER-SCOPE-START`,
  failures annotated and rethrown, never swallowed). Failed files are NOT
  registered as support: replaying a broken file would raw-fail every
  successor; missing-def diagnostics downstream are the honest cascade.
- **all-errors support replay funnels through verify-source.** `CA-COLLECT-SUPPORT`
  only collects byte ranges; the actual replay is `VERIFY:SOURCE-BUF-IN-SCOPE`.
  So an all-errors support gap has two halves: all-errors must COLLECT the form
  (deftype/deflinear/value-record/immediate/EXPORT were missing) AND verify-source
  `RECORD-DEFINER?` must DISPATCH it (top-level `TRUST` was ignored). A collected
  form verify-source ignores is a silent no-op. Fix both ends for parity.
- **verify-source drops top-level strings; recover them with a skip ring.** The
  main scanner uses `NEXT-SCAN` (SKIP-STRINGS set) which discards `s" ..."`
  literals, so a bare `s" NAME" s" SIG" TRUST` loses its args. Record the last two
  skipped top-level literals into a 2-slot ring reset per `NEXT-SCAN` call; at the
  `TRUST` token the ring holds exactly that statement's NAME/SIG (both skipped in
  the same call). Value = literal minus opener+space prefix (3 for `s"`, 4 for
  `S\"`) minus the closing quote.
- **`certify: stage2-src rejected rc 70 (non-blocking) / sig-type: at 'EXIT'` is
  pre-existing.** The build's `BF-CERTIFY-ACT` runs `VERIFY:SOURCE-BUF` over the
  whole engine source; it rejects non-blocking with the original verify-source too
  (proven by rebuilding both ways). Not a signal that an engine edit broke
  verify-source — reproduce with the unmodified tree before attributing it.
- **`test/gate-stdlib.f -- lint-tools` standalone and the gate's phase 17 are
  DIFFERENT code paths.** The standalone slice runs only the `TEST:SUITE` cases
  in `test/gate-stdlib-cases.f` admitted by `SUITE-LINT-TOOLS-LABEL?`; it never
  loads `test/gate-stdlib-lint-tools.f`. The full gate's resident phase 17
  (`TRWS-RUN` -> `GSI-LINT-TOOLS`) loads only that GSI body. Wiring a new lint
  into one place leaves the other silently green — add it to BOTH (GSI body +
  TEST:SUITE row + label allowlist) and prove each path red with a transient
  drift before trusting it (proven for tools/stdin-closure-lint.f and, later,
  tools/trusted-inventory-test.f which lived only in the GSI body until it was
  added to the `trusted-inventory` TEST:SUITE row + allowlist). The standalone
  entry also assumed a caller prelude and died E-UNDEFINED: FS-PATH-CAP at load
  (gate-stats.f uses lib/fs.f without requiring it); an entry file must require
  its own deps so `--load` works standalone.
- **A bundled gate fork hides which sub-suite failed; split one fork per
  sub-suite.** The old `lint-tools/dot-maki` fork ran dot/maki/maki-ns/host/
  trusted-inventory sequentially, so `GT-POOL-FAIL` printed only
  `FAIL: lint-tools/dot-maki`. Worse, a test file's `T-REPORT` `die` exits the
  fork and BYPASSES `GSI-INCLUDE`'s catch/per-file FAIL line, so a
  trusted-inventory ratchet drift left no sub-suite name at all — it was
  mis-blamed on dot-graph churn three times. Fix = one `GT-POOL-START-FORK` per
  sub-suite (`lint-tools/dot|maki|maki-ns|host|trusted-inventory`); each fork's
  exit maps to its own label. Setup is loaded once in the parent and inherited
  copy-on-write, so the split adds no setup cost; captured assert detail is
  preserved above the per-fork FAIL line.
- **The native publish path re-records every signed definition through `TRUST`.**
  `EM-COMPILE-PUBLISH` routes any colon definition WITH a `( ... )` sig through
  `EM-COMPILE-PUBLISH-TRUSTED` → `C-CALL-TRUST-PEND`, i.e. after the hook returns
  the engine calls checker `TRUST(name, declared-sig)` → `USIG-ADD`. Any
  "reject but continue" checker mode must survive that re-parse: before TFAM 6 a
  definition with an unparseable sig under `MULTI-ERR` killed the whole load with
  `checker: bad stored signature` (rc 76) from this second path, not from CHECK.
  `USIG-ADD` now skips the row in multi-error mode (own-name = already counted;
  foreign name = raw TRUST row, counted + reported via `BADSIG-XT`).
- **Top-level `['] evaluate catch` in the stdin interpret loop loses the throw
  code (rc 0).** The same catch executed from *compiled* code receives the real
  code, and an uncaught declaration throw still aborts a file/stdin load with a
  nonzero exit — so loads stay fail-closed. In stdin suites, route eval-catch
  probes through a compiled helper (`TDT-NEG` in test/type-decl-suite.f), never
  call the trusted eval-catch word directly at top level.
- **`bin/hb` loads its checker prefix from source paths at boot.** Edits to an
  already-listed prefix file (checker.f, type-family.f, sumtype.f...) take
  effect on the next `bin/hb` run without a rebuild — but a NEW prefix file
  needs all six manifests (habu2.f PFX rows + label init, bootstrap/cg/forth.fs
  mirror, tools/bootstrap.sh, tools/build-fixpoint.f, tools/diagnose-hb-core.f,
  tools/hb-build-lib.f key list, test/run-files.f) plus a rebuild, and the first
  ceiling it trips is stage2's `S2-SOURCE-CAP` ("stage2: source exceeds buffer").
- **A seed `bin/hb` older than the engine prefix crashes the refresh AND the
  gate with `E-UNDEFINED: <new-word>` + SIGABRT (rc 134) + a crash-reg hex
  dump — and `install --force` still exited 0, leaving the stale binary.**
  After merging an engine-prefix change (new checker word, new prefix file),
  refresh the main tree's `bin/hb` immediately and reseed every live worker
  workspace from it; a worker whose gate dies at load line 1 with
  `E-UNDEFINED` on a freshly-merged word has a stale seed, not a code bug.
  Recovery: copy a current workspace engine over the stale seed, rerun the
  install, rerun the gate. The exit-0-on-crashed-refresh fail-open is dotted
  (habu-install-force-exits-09c3c981).
- **Closing a dot (`dot off`) edits the dot graph, so the archive commit itself
  must pass `dot-dep-lint` before pushing.** Archiving
  habu-fix-ptx-collective-997cfcce left five dots whose `blocks:` lists named
  the now-deleted id; the pushed head went red at gate phase 17 while every
  gate I ran had targeted the pre-archive tree. When closing a dot, `rg` its id
  across `.dots/` and drop it from every `blocks:` list (delete an emptied
  `blocks:` key entirely) in the same commit, then gate that exact tree.
- **Native guard widenings do not propagate to the stage0 mirror by themselves —
  only the seal-absence pins alarm the drift, and the cheap runtime parity proof
  is forging the CHECK_ONLY seed.** 2b-v made native PROT-GUARD two-band while
  the gforth mirror stayed single-band + unguarded cp!/ndict!; nothing failed
  because pins count what IS, not what SHOULD be. When widening a native seal
  surface, in the same effort re-pin `SAB-GUARD-PINS` red-first, mirror the
  emission, then run `HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh` and pipe
  seal.f-style forges (`data-base <off> + cp!` etc.) straight into the built
  `hb-stdin` — rc 83/0 against the actual stage0-built engine beats source
  review. Mind file order: forth.fs defines sinks top-down, so a newly guarded
  early sink (cp!/ndict!) needs PROT-GUARD moved above the primitive bodies,
  where native has it next to the register constants.
- **A mechanical guard on all FFI arg registers is UNSOUND — guard only
  [0..nargs); the trampoline that already carries nargs is the sound sink, and
  the rest is a checker-signature boundary.** ffi-call loads 8 cells into x0-x7
  regardless of arity, so slots past the real args hold STALE values from a prior
  call — PROT-GUARDing all 8 false-traps a legit low-arity call whose stale slot
  holds an old band pointer. Only ffi-call-n carries x14=nargs, so it is the one
  trampoline that can guard soundly (loop argbuf[0..nargs), guard each, BEFORE the
  x20 repurpose so x20 still = DATA). Make the CHECKED library funnel its
  integer/pointer calls through it (CALL0-6/DLOPEN/DLSYM -> ffi-call-n) instead of
  raw ffi-call — a pure lib change, no prim-signature/checker.f edit. Adding nargs
  to ffi-call or int-nargs+sret to ffi-call-abi to guard THEM needs their PRIM:
  sigs widened in src/core/checker.f; if that file is another worker's, it is a
  named boundary + a dot, not a bypass. Red forge: `data-base <band-off> + 0
  FFI-PTR-ARG!  1 0 FFI-CALLN` with fn=0 traps 83 (pre-fix it BLRs to 0 ->
  signal/rc134, a distinct outcome); stale-slot proof is nargs=0 with a band value
  in slot 0 -> NOT trapped.
- **A `0 set-check` span may exist only because ONE primitive lacks an axiom
  row — probe before accepting it as a boundary.** Both hook-install spans
  (tools/lint/text.f, tools/check-core.f) disabled checking solely because the
  hook body calls `CHECK!`, which the checker did not know. A one-line fixture
  (`s" CHECK!" s" ptr u8 n -- n" TRUST` then the hook definition, under the
  baked hook) proved the span retirable; the swap keeps the file fully checked
  and turns an opaque check-off region into a single audited prim-axiom TRUST
  row. Duplicate TRUST of the same primitive in two files is idempotent.
- **Clean up a lane's jj workspace the moment its work is merged and the agent is
  finished — `jj workspace forget <name>` + remove the directory in one step.**
  Finished-lane workspaces accumulated for weeks (~40 dirs) until a manual bulk
  removal looked like data loss and cost a restore-and-verify round trip; stale
  workspaces also hold stale baked `bin/hb` binaries that produce fake gate reds
  when reused. Verify first (no unmerged commits, clean wc), keep active lanes,
  parked device-blocked chains, and the merge-gate workspace.
- **A static checkability probe is only as honest as its registry context:**
  probing src/habu/aot-closure.f through `tools/check.f` came back clean
  because check-core's own requires (tools/json.f) had put JSON-DIAGS into
  the probe registry - the real maker compile then rejected it. When asking
  "does this file check in context X?", the oracle must BE context X (the
  live stage/maker compile), not a tool whose own dependency closure
  overlaps the words under test.
- **Suppression is process-local; ownership is not.** The gate's fork-child
  span dedupe (GS-CHILD-OWNED?) only ever compares spans emitted within one
  process, so a pool parent's authoritative pass-hook spans must bypass it
  (GS-SPAN-AUTH) - a fork child that is itself a pool parent otherwise
  swallows a nested slot's span whenever labels collide. And any identity
  qualifier must be applied identically to both halves of a (test row, span)
  pair; since pairs are always emitted by one process, qualifying with the
  emitting process's generation keeps them matched, while qualifying
  authoritative spans with the slot's generation would silently break
  attribution. Split "who owns this record" from "which identity scope is it
  in" before adding any qualifier to a byte-keyed dedupe.
- **An uncaught throw in a `--load` child exits with the throw code's low 8
  bits and prints nothing** (E-STR-BOUNDS = -2200 surfaced as an opaque rc 104
  with empty stderr). Boundary validation that can fail on external input (env
  vars, argv) must `die` with a source-pointing message naming the input;
  keep bare named throws for in-process programmatic callers, where the
  enclosing harness still has the code.
- **A 1-byte diagnostic + clean exit means a raw engine capacity path, not a
  throw.** The definer code paths write only the CURRENT TOKEN to stderr and
  `exit_group` with a fixed code when NDICT/CP hit their caps (dictionary full
  = ':' + rc 77, code space = rc 76) - uncatchable by `catch`, invisible to
  the BTHROW de-masking, and coincidentally equal to unrelated E- codes
  (E-LINT-TOKEN-CAP is also 77). When `catch` cannot intercept a death, stop
  hunting for throwers and hunt for exit_group emitters; when output is one
  token, suspect a reporter that prints [TKA/TKL] with no label. Falsify
  cheap hypotheses with in-state controls (`' evaluate catch` on a known
  throw) before believing a code-number match.
- **Anything that rides the AOT seed pass is invisible to batch programs.**
  EM-SEED-AOT runs at LEXIT, and batch input (piped stdin AND --load files) is
  consumed by the pre-LEXIT interpret loop, so AOT-seeded words (BP., stepper)
  and AOT-restored state (the protected-WID registry) do not exist while a
  batch program runs - they appear only for post-seed interactive sessions.
  Probe with an AOT-seeded word (`BP.` E-UNDEFINED = pre-seed) before assuming
  boot-restored state is visible. Boot-restored DATA with no name-relocation
  dependency belongs in EM-STARTUP, not the LEXIT seed.
- **A tool's full require closure can outgrow the engine dictionary silently.**
  The gate-runner-support+entry closure registered ~9.3k dict entries; against
  the old DICT-CAP 8192 it exhausted mid-load and the definer exited rc 77 with
  a bare token byte and no label. Nothing in the resident gate noticed because
  phases fork per-phase subsets - only the documented standalone invocation
  loads the whole closure in one process. Lesson: a load path that only ever
  runs subsets needs an EXPLICIT whole-closure regression, and capacity exits
  must carry a label (`hb: dictionary full at: <token>`), never a lone byte.
- **Definition-compile failures under catch+evaluate crash; interpret failures
  are swallowed.** `[: s" : X ( -- ) nosuchword ;" INCLUDE-EVALUATE ;] catch`
  SIGBUS-crashes (rc 134) after printing E-UNDEFINED, and an interpret-level
  failure under the same boundary prints its diagnostic but returns 0 to catch
  — while plain stdin and --load exit an orderly rc 70 for both. Found probing
  the item-9 construct fail-closed contract; repro needs nothing new (dots
  habu-def-compile-failure-7182eeb2, habu-interpret-err-under-8876b500).
  Lesson: never assert engine-compile failures through in-process TCE-CATCH —
  pin them as gate child-process cases (GE-RUN-STDIN + GE-EXPECT-RC), and
  treat catch-around-evaluate outcomes as untrustworthy until those dots close.
- **Reuse the step machinery for new checker forms; a reserved form must
  consume its operand tokens even on failure.** construct's effect is built
  from SUMV metadata and applied through the ordinary CHECKER-STEP, which
  bought unification diagnostics AND exact linear conservation for free (the
  linear-suite parity pins passed first try). And when a capture form's
  resolution fails, still consume the remaining operand tokens (poisoned
  state): letting them fall through to word lookup turned a hard ownership
  reject into a soft uncheckable-undefined verdict, which multi-error loads
  would then trust-and-publish differently.
- **A new consumer is a bug magnet for old producers — instrument the sym/record
  store, don't stare at the new code.** The MATCH suite's CASE-interleave
  fixtures failed on plain `swap`; the new match machinery was innocent. Walking
  the checker's authoritative stores with tiny in-suite probes
  (CHECKER-FIND-ACTIVE-SYM, USIG-FIND-OFF-SYM, SYM-PKG$/VIS dumps) found TWO
  effect records per qualified definition — every `PKG:TAIL` def (generated
  constructors included) also records under the bare-global tail, certifying
  calls the engine rejects and SHADOWING same-named prims for all later
  definitions (dot habu-qualified-defs-leak-aadeb5c9). The TRUSTED:-qualified
  control probe (only ONE record, bare-global) localized it to the engine's
  post-C-QUALIFY-DEF record call in three minutes of probing. Lesson: bisect by
  declaration-set difference, then dump the store — and expect word-named
  ADT variants to surface every latent name-resolution bug in the pipeline.
- **Rank new structural rejects above the uncheckable verdict, or trailing
  tokens soften them.** A hard match reject (unknown family, overflow) leaves
  the rest of the block as undefined-word noise, and UNCK would have won in
  CHECK-VERDICT, blurring verdict 0 into 1 (which multi-error loads trust
  differently). The MREJ latch in the hard-reject class fixed the whole failure
  family at once; construct solved the same problem earlier by consuming its
  full token form even when poisoned. Any future capture form needs one of the
  two from day one.
- **Reason codes must be latched with the token pin, not derived at render
  time.** The §24 match diagnostics work because every failure site latches a
  reason code under the same first-wins discipline as the FAILSET token pin
  (and the nonexhaustive latch carries the family/bitset for the name walk).
  Two ordering traps found by fixtures: truncation must latch BEFORE the
  output-boundary coercion (whose mismatch otherwise steals the reason), and
  the reason arm in DCODE/REPAIR-CLASS must rank ABOVE the generic
  UNDEFERR/DEADERR flags, which post-reject token blur routinely sets.
- **Suite-visible checker words are checked even in test files: bools are not
  n.** Three round-trips lost to `-1`/`0` where `bool` was declared
  (DIAG-JSON!, a CONTAINS? helper, and a render walk flag that only the
  fixpoint CERTIFY pass caught — local stdin runs parse unchecked and stay
  green, so run the certify path early when adding checked prefix words).
  The `0 0=` / `0 0= 0=` literal idiom is the existing convention; T=-style
  n asserts need bool-specific helpers inside checked words.
- **Capacity caps sized to "current largest file + headroom" are time bombs;
  label every capacity exit.** Item 9 grew checker.f past the lint
  tokenizer's TMAX and shadow-lint died a BARE rc 77 (unlabeled throw,
  nothing on stderr standalone) — the same failure class as the DICT-CAP
  lesson above, in a different store. Raised TMAX and the shadow-lint byte
  cap (which was within 2.7KB of tripping too) and gave TOKEN-ENSURE a
  labeled die. When a cap comment names a specific file, grep that file's
  growth in any slice that touches it.
- **Check whether an existing checker-owned bridge generalizes before designing
  a new coercion.** Product destructure looked like it needed a new
  "hidden-run to field-types" unification window; it fell out of TWO existing
  mechanisms unchanged: the pending-constructor window at k=0 certifies any
  empty-bodied generated word whose declared sig is metadata truth (expected
  row = SGIN + 0 cells = inferred row), and the LOGHID logical/hidden row
  coercion is already SYMMETRIC in U-ROW — so parametric UNMAKE's open input
  absorbs a caller's concrete hidden run with no input-side special case.
  Reading the unifier before designing saved a whole checker feature. Same
  story for publication: recording fixed `make`/`unmake` tails as SUMV rows
  bought the entire ctor-package protection wall for free.
- **Stage a cross-layer capability so every commit is one-refresh buildable
  from its parent's binary.** build-fixpoint `require`s tool sources into the
  RUNNING engine, so a habu-layer consumer of a brand-new checker word breaks
  the refresh from the parent binary (TFAM 15 hit E-UNDEFINED CHECKER-DEFPRODUCT
  live). Land the checker word + its PRIM: exposure in commit A (stage-compiled
  from source, so ANY parent binary builds it), and the verify-source/check-tool
  consumers in commit B (A's binary exposes the word). TFAM 14 folded both into
  one commit, which gates green in-session but leaves refresh-from-parent
  broken; the two-commit shape costs nothing and keeps the chain sound.
- **PRIM-FIRST-SYM cannot answer "is this name a prim?" — offset 0 is a real
  record.** It returns the first PES row's effect OFFSET, and the very first
  prim row legitimately lives at USIGS offset 0, so `dup` (row 0) reads as
  "no prim". FIND-SIG's prim fallback carries the same conflation. Existence
  checks must use PRIM-FIRST-IDX (slot+1, 0 = none). Found building
  CHECKER-EXPORT's E-EXPORT-PRIM reject; the suite caught it first run.
- **The $40000 whole-source lint caps all tripped together when checker.f
  crossed 262144 bytes** (EXPORT alias work) — trust-lint (gate + argv),
  trusted-inventory, maki-dep-lint, and error-code-lint each died "file
  exceeds buffer" with NO file name, exactly the labeled-capacity-exit lesson
  above replaying in four more stores. All five caps are now $80000 with
  comments naming checker.f, and the shared lint READ-FILE capacity/IO exits
  print the offending path (LINT-READ-DIE, tools/lint/text.f). When one cap
  named after a growing file trips, sweep EVERY READ-FILE buffer cap in the
  same pass instead of raising them one gate failure at a time.
- **A "no name clash" premise must be checked against TOKEN SURFACES, not just
  defined words.** The EXPORT dot said "no top-level EXPORT exists today" —
  true for the dictionary, false for the tree: hb-build's --repl path treats
  line-leading `EXPORT ` as a build directive (lib/source.f COMMENT-EXPORTS
  strips it), verify-source consumed it as a no-op declaration, public-
  signatures collects the names, and lib/prelude.f even defined a top-level
  `export` shim so directive-carrying programs stayed plainly loadable. The
  keyword landed as a context split (in-package = re-export declaration; top
  level = the directive no-op, engine and verify-source agreeing 1:1), and the
  prelude shim became dead code (a keyword shadows any same-named word at
  interpret level) and was removed. Sweep grep for the token in STRINGS,
  comments, directives, and preprocessors before declaring a name free.
- **checker.f is a boot-time source prefix, not baked engine bytes.** bin/hb
  compiles src/core/checker.f from the WORKING TREE at every launch, so a
  checker.f-only edit changes behavior with byte-identical binaries (the
  byte-fixpoint proves engine-source determinism only), a PRIM: exposure row
  takes effect for any binary running in that tree, and the TFAM-15 two-commit
  staging rule really constrains ENGINE (habu1/habu2) changes plus tool
  sources `require`d into the RUNNING engine — the checker half follows the
  tree, not the binary. Corollary: prefix-internal colon words are dictionary-
  visible but checker-invisible to later tool sources unless a PRIM: row
  persists their effect (CHECKER-PACKAGE-ACTIVE? hit this).
- **The gap you ledger as "follow-up" will bite before you close the dot —
  write the proof that exercises it first.** The EXPORT AOT single-body proof
  failed on first run with E-UNDEFINED because hb-build's COMMENT-EXPORTS
  stripped the in-package re-export line — the exact divergence the slice-2
  ledger had filed as a follow-up. The byte-identity proof (alias-variant AOT
  binary SHA-equal to the source-name-twice variant, same output path so the
  codesign identifier matches) both caught the gap and pinned the tree-shake
  contract; the fix (package-aware directive strip) was 30 lines with both
  miss modes failing safe only BECAUSE the keyword's top-level no-op had
  landed first. Proof-first ordering turned a latent wrong-program bug into a
  same-session fix.
- **Token lints must match dictionary-significant words case-insensitively.**
  The dictionary is case-insensitive, so `CREATE BUF` defines a global and
  `;package` closes a package exactly like the lower-case spellings — a
  lint matching definers/`package` with `LINT-STR=` has both an evasion vector
  (upper `CREATE`, upper closer leaving depth stuck > 0 hides every later def)
  and a false-positive vector (upper opener not counted). Proven red-first in
  the maki-ns-lint reconciliation: 3 misses + 1 false positive on 5 fixtures.
  `LINT-STR=CI` for anything the engine resolves; keep exemption prefixes
  (`E-`) case-sensitive so whitelists stay narrow.
- **Probe the whole USE PATH before sizing a capability slice — the second
  wall hides behind the first.** Storable-layouts S1 was designed as one
  checker change (`!`/`@` accepting a `ptr family` address, width-1 tier);
  a pre-implementation probe of the consumer pattern showed the typed address
  itself was unproducible — `( -- ptr enum ) VAR-NAME` rejected because a var
  could not bind a layout pointee (U-TYPE's row-level rule firing under the
  T-PTR arm's PAIR-STRICT context). Without that probe the mem arm would have
  landed green-but-unreachable from checked code. The fix keyed on an existing
  invariant instead of new state: PAIR-STRICT has exactly one call site (ptr
  pointees) and PAIR inherits it, so CUR-STRICT already IS the "inside a
  pointee" context flag — one gated relax there, no new mode variable.
- **A protected table's adjacent control cell belongs to the same guarded
  metadata band.** Extending the protected-WID table moved `UNCGH-CELL` to its
  former upper boundary but left that reporter pointer writable, so a user
  store could redirect the uncaught-throw branch. Define the band through the
  final engine-owned cell, mirror that length in stage0, and pin both the last
  protected byte and the first writable byte.
- **A raw boxed pointer needs bounds metadata at the allocation boundary.** A
  payload index was applied directly to `[tag|payload]`, so a negative index
  could overwrite an earlier record. Keep the public tag-first pointer ABI,
  store capacity in a hidden preceding cell, reject both signed bounds before
  address arithmetic, and prove the rejected write leaves its neighbor intact.
- **Independent checker passes need independent registry scopes.** The nominal
  scan registered valid type families, then `--all-errors` replayed the same
  source inside that still-mutated scope and diagnosed duplicates. Roll back
  the nominal/lint phase before replay; the outer command scope still preserves
  transactionality without leaking pass-local declarations.
- **Mismatch attribution must come from the failed unification pair, not a row
  scan.** A matched ADT beside the real failure made `family` name the wrong
  type; a scalar failure beside an ADT could invent a family. Capture the first
  failed `(actual, expected)` type pair, process row heads before recursive
  tails so width differences reach that pair before an occurs-check backstop,
  and render optional metadata only from the captured terms.
- **Repair diagnostics are a sum of evidence shapes, not one nullable mega-
  record.** Definition validation required spans and stack rows from declaration
  failures that never owned them, while packet generation dropped declaration
  fields and ADT family identity. Dispatch on the diagnostic shape, preserve its
  required evidence, and never fabricate fields from the other variant.
- **Typed storage with untyped accessors does not preserve semantic roles.** An
  `n` converted independently at a dtype or layout store can satisfy either
  range when their tags overlap, and converting a typed fetch straight back to
  `n` erases the only proof consumers need. Carry the family through internal
  APIs and locals; convert only inside named wire or private table-index owners.
- **Cross-kind name collision checks must be declaration-order symmetric.** The
  variant path rejected family tails, but the family path never scanned prior
  variants, so identical names accepted in one order. Apply the same in-scope
  rule at both declaration gates and pin unrelated-package acceptance.
- **Cache-hit tests must own the cache state that makes a hit possible.** An
  object-cache assertion run without `HABU_BUILD_CACHE` can only miss, while a
  shared warm artifact may bypass object production entirely. Bind a fresh
  gate-local cache before proving store, restore, or relink transitions.
- **Typed layout pointers need a generative introduction boundary and fetch-
  time validation.** Letting an ordinary `ptr a` unify with `ptr FAMILY`
  proves neither extent nor continuing representation validity. A sealed
  allocator must own count, stride, zero-image, and bounds; build its generated
  source before allocation so generator failure cannot leak DATA. Raw aliases
  can still corrupt a valid image, so every typed fetch must validate active
  tags before publishing the bundle. Erase every one-shot authorization word
  and backing cell after compiling its direct callers: a globally callable or
  writable arming surface turns the boundary back into an unchecked cast.
  Validate qualified-name grammar before allocation because native definer
  failures exit rather than unwind `catch`.
- **Compiler replay evidence must be immutable, source-bound, and consumed
  exactly.** Publish a canonical certificate from the checker transaction,
  freeze the checked source and certificate into a read-only compiler-owned
  blob, key rows by source-byte offset, and require pass two to consume every
  width/bind/fetch row once. A live checker lookup or token ordinal lets stale
  state and alternate hooks change lowering after acceptance.
- **Proof tables are arenas, not policy caps.** Width and fetch evidence scale
  with source complexity; fixed row arrays turn a valid large definition into
  an internal failure. Grow the producer tables and validate byte arithmetic at
  the immutable consumer boundary.
- **A growing boot-prefix list needs a growing pin manifest.** Adding core
  dependencies while retaining a fixed file-count buffer makes recovery fail
  before it can certify the new prefix. Store ordered per-file digests in a
  byte-growing manifest; keep the independent exact row-count tripwire.
- **Bootstrap-safe proof producers need an early fail-closed dispatcher.** The
  checker can run before the full layout producer loads, so install a sealed
  package dispatcher immediately after the checker; it may publish only the
  canonical empty certificate until the full producer installs exactly once.
  Define its checker-owned package only after `CHECKER-PACKAGE` exists in the
  cold source stream, and add every new prefix dependency to the transitional
  bootstrap stream; an older engine cannot load a path absent from its baked
  prefix table.
- **Seal the owner package, not only its authority words.** Undefining a
  producer hook removes the old spelling but an unsealed package can republish
  authority-shaped exports. The native reserved-name table, checker predicate,
  and forge tests must cover every trusted package owner.
- **Variant guards certify both the expected tag and its domain.** A guard row
  without the certified variant limit cannot prove that a fetched tag belongs
  to the family. Encode `{offset,tag,limit}`, validate `0 <= tag < limit`, and
  reject malformed descriptors before pass two copies or executes them. Replay
  must also reject an observed guard tag outside that domain before deciding
  whether a guarded nested check is active.
- **Primitive metadata keys include the defining package.** Package-scoped
  words may share an unqualified name, so a primitive/effect registry keyed
  only by spelling can attach the wrong contract. Resolve and record the exact
  package-qualified producer identity.
- **Package new seams; do not namespace-migrate an unrelated regression.** A
  fixed-range test needed three literal updates, not a file-wide API rewrite.
  Preserve established test helpers unless the feature actually changes their
  boundary; put only the new helper surface behind a package.
- **Generated assertions must respect interpret-mode control limits.** Habu's
  top-level interpreter does not own compile-only `if`/`then`; emit a small
  checked assertion word, then call it at top level.
- **Generated failure branches need the full named-error contract.** A numeric
  code alone does not satisfy `die`; emit the diagnostic string and code so the
  assertion itself remains checked and the failure is attributable.
- **Certificate operand positions follow the local-group capacity, not shuffle
  arity.** The four `P2W` scratch cells bound only fixed-arity stack operators;
  local bind evidence can name any of 64 group positions and must validate
  against `TXN-LIVE-W-CAP`.
- **A recovery vocabulary must never leak into the ambient search order.**
  Leaving the private guard vocabulary active made its `BAND` helper shadow the
  existing instruction emitter and crashed stage0 generation. Keep the legacy
  global guard API unchanged; resolve private additions with balanced local
  `also`/`previous` brackets at each call site.
- **Recovery seal guards must use the native friend-latch boundary.** A second
  prefix latch diverges from native self-hosting and can reject the compiler's
  own baked package definitions. Mirror the single friend latch and its cold
  prefix transition instead of adding another mutable authority bit.
- **Seal source transitions at the interpreted token boundary.** A native
  store emitted while assembling the source buffer runs before any prefix word;
  append an idempotent seal token after the trusted prefix instead. Internal
  builder binaries carry an emitter-only package flag and never become the
  installed user engine.
- **Writable extents become sound only in one explicit callee wrapper.** A
  universal binding generator lets generated source mint trust and mutable XT
  caches permit redirection. Keep each symbol in an audited `TRUSTED:` word,
  fix every direction and extent there, and seal the package after definition.
- **Writable extents belong to exact bindings, never callers.** A caller-selected
  length can understate a foreign write or use zero to bypass protection. Reject
  zero extents and have each binding install the callee contract's fixed size.
- **Protecting publication does not protect package reopen.** Registering a
  public WID blocks writes through defining sinks, but `package NAME` can still
  reopen its private dictionary. Package lookup must reject every protected WID.
- **Fixed test emitters are safer than universal patch wrappers.** Keep each raw
  `patch32` boundary private to one fixture with fixed instructions; a checked
  wrapper that accepts arbitrary instruction cells launders code-write authority.
- **Bootstrap IR must grow outside the Gforth dictionary.** Raising a static
  `create ... allot` cap steals space from later image buffers and turns code
  growth into dictionary overflow. Keep the IR heap-backed and resize it with
  checked arithmetic so compiler growth does not consume dictionary capacity.
- **A signature may name only installed types.** Execution tokens are currently
  checker scalars (`n`); spelling an undeclared `xt` role makes self-certification
  fail even when the runtime cell is valid. Add a nominal role first or use the
  primitive model's actual scalar contract.
- **Compiler replay is not a user load.** Sealing before a generated compiler
  payload blocked its own protected packages; leaving every load open would
  expose the same authority to applications. Route only statically certified
  payloads through `--build`, emit `SEAL-FRIEND` before their drivers, and keep
  `--load`, stdin, baked source, and the REPL sealed before the first user token.
- **Sealed packages invalidate reopen-based private tests.** Once a library owns
  and seals a package, later tests cannot enter its private wordlist. Remove
  tests for retired private machinery; test live public contracts without
  reopening the owner package.
- **Capacity guards may precede span guards.** A definition starting four bytes
  below the code limit fails with the named code-space error before any emitted
  write can reach the protected band. Assert the earliest valid invariant, not a
  later sink guard that execution never reaches.
- **Every typevar-indexed array must follow the live typevar capacity.** Turning
  `MAXTV` into a growable high-water left render's fixed `SEEN` array behind;
  its reset loop then filled adjacent hook cells with `-1`. Grow scratch arrays
  to the live cap and reset their process-local pointers before snapshots.
- **Gate retries need fresh `XDG_CACHE_HOME` and `HB_TMP` per attempt.** A
  reused cache can replay a timeout-poisoned result; mint both directories
  inside the retry loop.
- **Overlay byte identity proves gate-tool migrations.** Run migrated and
  baseline tools from separate source overlays on identical clean and dirty
  inputs, then compare stdout, stderr, and exit status.
- **Certify target text before building its generator.** A handwritten target
  probe separates checker limitations from emitter failures before generation
  adds another fault domain.
- **Match structural shapes, not interned ids.** Intern order can change across
  sessions; compare kind, arity, and payload positions unless the ids come from
  the same registry snapshot.
- **Back up divergent workspace edits before recovery.** `jj workspace
  update-stale` rebuilds the working copy and can replace on-disk edits; verify
  file ownership before restoring them on a fresh change.
- **Check live sibling diffs before editing shared files.** A cheap ownership
  check avoids semantic divergence and is preferable to workspace recovery.
- **Run typed-local lint against the exact integration diff.** A focused branch
  gate can miss an earlier untyped local retained in a squashed feature stack;
  the final master-to-feature artifact is the commit proof.
- **Repo-wide lint input buffers must grow from file size.** A fixed ceiling
  turns documentation growth into an unrelated lint failure; reuse a
  high-water allocation rounded by `MEM-ALLOC-64K-SPAN`.
- **Protocol phases need registered nullary type families.** Free-form atoms in
  parametric signatures fail stored-signature parsing; register each nominal
  phase and thread it through the context family so skipped transitions reject.
- **Package-owned CAD ids are arity-zero cell families, not global DEFTYPEs.**
  `TYPEFAMILY id 0` already gives package-qualified family identity, typed
  pointer storage, rollback, snapshots, replay, and qualified diagnostics.
  `DEFTYPE` instead installs a global nominal plus general raw converter words;
  keep authority-bearing raw refinement private to the validating owner.
- **Package words still compete with immediate core syntax.** A packaged word
  named `BEGIN` resolves as the control word while compiling a body; use a
  domain verb such as `START` even when the package would otherwise disambiguate.
- **Generic storage does not retain a nominal pointee between definitions.** A
  probe stored `n` through a `variable` and fetched the same cell as
  `CAD-KIND:node-id`; both definitions certified because each use instantiated
  the generic pointer independently. Model IR therefore keeps raw storage and
  identity projections private, exposes typed accessors, and pins cross-role
  rejection instead of treating `variable` or `create` as a typed container.
- **Never base parallel workspaces on a mutable working-copy commit.** Global
  descendant rewrites stale sibling workspaces, and `workspace update-stale`
  can discard files not yet snapshotted by jj. Branch each workspace from a
  stable commit and apply reviewed API changes independently, or serialize.
- **Rows and columns need different nominal roles.** A single dimension kind
  validates magnitude but cannot reject transpose-order mistakes. Keep role
  changes inside checked shape algebra and project to numbers only at execution,
  rendering, hashing, or ABI boundaries.
- **Transfer trust-row ownership before retiring a dot.** The inventory can stay
  count-green while strict mode is red because classification rows still name a
  removed capability dot. Reassign landed boundaries to a live permanent owner
  in the same change that removes the capability dot.

- **A failing `jj diff` with "sibling of the working copy's operation" mid-gate
  means the snapshot you are gating may be EMPTY — back up edited files BEFORE
  `jj workspace update-stale`:** update-stale RESETS the working copy (reverting
  unsnapshotted edits) and can leave a divergent change twin; the non-empty twin
  holds a possibly STALE snapshot, so restore from your own backups (they are
  the exact gated bytes), abandon the twin, and re-run the cheap gates on the
  restored tree. The 0-byte patch file written by the failed diff is the tell.
- **MATCH arms may produce multiple values, and both arms must agree:** FL-SIG's
  dot-split rewrote four dpos branch expressions into one MATCH whose arms each
  push the (ilen fa flen) triple — the checker unifies multi-cell arm effects
  fine. Diverging arms (`throw`/`exit`/usage-`die`) unify with any shape, so
  option unwraps built on fail-closed reporters (GE-FAIL, BF-BUILD-RC die,
  TR-USAGE) need no sentinel filler in the dead arm.
- **A lagging integration branch can carry a stale plan; check the authoritative
  version before building.** The Model IR descriptor swap was implemented from
  fable's copy of a dot whose plan master had already CORRECTED (the G-TAG "enum
  behind stable n accessors" design was declared INVALID on master, because it
  leaves the dtype/layout field-swap hole open at the API boundary). The
  self-run destruction review passed only because it was handed the stale spec.
  Before executing a dot on a branch behind master, diff the dot against
  master's version and rebase onto the plan-owner's branch BEFORE implementing,
  not after; a destruction review is only as good as the spec it is given.
- **Probe a BLOCKER claim before repeating it.** A plan note said the Model IR
  family-typed migration was blocked on enum-typed locals. A five-pattern
  feasibility probe showed everything the migration needs — family-returning
  accessors, MATCH renders/predicates, dup multi-use, families as word args
  through factored helpers — certifies without any local; only the `{: x:fam :}`
  bind rejects, and stack-discipline style never needs it. A missing capability
  blocks only the code shapes that require it, not the goal; prove the
  intersection empty before declaring work blocked.
- **Snapshot main-workspace file writes with a jj command IMMEDIATELY, or an
  op-state repair will silently revert them.** Three dots created via `dot add`
  in the main workspace were lost this campaign: concurrent worker-workspace jj
  ops fork the operation log, and the next `jj workspace update-stale` resets
  the working copy to the recorded op, discarding unsnapshotted files. Rule:
  after any main-workspace write while workers are live, run `jj st` at once;
  before any `update-stale`, back the working copy up first.
- **A falsification can itself be wrong — record probe CONTEXT.** One review
  "falsified" the .0 literal-parse hazard with a probe in a different resolution
  context (checked-candidate vs colon-compile); a later worker's probe plus a
  structural corroboration (the word missing from the manifest because the
  tokenizer skips parser-claimable names) proved the hazard real. Probes must
  name their context (interpret / colon / candidate), and disagreeing probes
  mean the SEMANTICS are inconsistent — itself a finding (dotted for core).
- **Gate retries need FRESH `XDG_CACHE_HOME` + `HB_TMP` per attempt.** A reused
  cache dir replays a timeout-poisoned result-cache PASS/RED verdict, so the
  retry reproduces the first attempt's flake as a false persistent red. Mint
  both dirs inside the retry loop, not once outside it.
- **Overlay byte-identity is the proof standard for migrating gate-critical
  tools.** Materialize the master copy of the tool in an overlay tree (own
  copies of the changed files, symlinks for the rest, so its require chain
  loads its own old sources), run migrated vs master on the SAME inputs — clean
  and dirty — and diff stdout+stderr+exit. Loading two versions into one
  session double-defines; overlays don't. (Proven on trust-lint,
  stale-status-lint, trusted-inventory, tools/date.f.)
- **Probe-first generation: certify the target text before building its
  generator.** Hand-write the exact checked source a generator must emit and
  run it through the UNMODIFIED engine/checker first; only then automate. A
  generator built against an unproven target conflates emitter bugs with
  checker rejects and burns cycles bisecting the wrong layer.
- **Match on structure, not interned ids.** Term/family ids are
  interning-order artifacts (the payload-pos term-id trap): equality that
  happens to hold in one session breaks when the intern order shifts. Capture
  and compare the structural shape (kind, arity, payload positions); treat raw
  id equality as valid only within one registry snapshot.
- **Divergent-workspace recovery protocol: back up FIRST, then update-stale.**
  When a sibling lane's op lands mid-edit (`jj st` errors or a 0-byte diff with
  edits on disk), copy the edited files out, run `jj workspace update-stale`
  (it rebuilds the working copy and reverts on-disk edits), verify the
  concurrent commit's file set does not overlap yours, restore from the backup
  on a fresh `jj new`, and abandon the divergent empty leftovers. Lossless
  every time; guessing at jj state is not.
- **Check the other lane's live diff before touching shared files; STOP beats
  recovery.** Contested src/core files change under you between fetch and
  commit. `jj file show -r master <file>` (and the ownership list in the task)
  before editing turns a would-be semantic divergence into a cheap
  STOP-and-report; the report is a deliverable, the recovery is pure cost.
- **Probe the other lane's in-flight work before dispatching against a shared
  dot.** The S2 wide-store slice was implemented twice concurrently: a fable
  worker built it from the dot while tfam landed the same slice on master
  (same dot id, cited in the landed checker comment). The full lane (~284k
  tokens, checker+engine+tests, all gates green) was retired unsalvaged —
  master's landed form was equivalent-or-better. Before dispatching a worker
  against a dot that the other lane's epic owns or references, check master's
  recent commit subjects and the dot's blocker graph for an active claim; a
  one-minute `jj log -r 'master ~ fable::'` scan beats a duplicated lane.
  Corollary (now standing practice): `dot on <id>` + commit + push at the
  moment of dispatch — a pushed `active` status is the cross-lane claim
  signal; `dot off` only at landing; parked dots go back to `open` so
  `active` never lies.
- **`dot off` breaks `trusted-inventory --strict` if TRUSTED.md rows cite the
  dot.** Twice now: closing a dot archives its file (gitignored), and every
  manifest row naming it as owner goes strict-red on the next gate. Before
  `dot off`, `rg <dot-id> TRUSTED.md`; re-point rows to the live successor
  owner (friend-latch dot for whitebox shims, the owning epic for campaign
  casts) IN THE SAME COMMIT as the closure.
- **Stage-then-fan-out beats one long worker on multi-file missions.** The R3
  reconciliation (50 files, ~2h) ran as a single agent; only the semantic core
  (the files where the policy decisions live and the API is fixed) needed one
  head. Correct shape: stage 1 = one worker resolves the core and publishes
  the target API/contract; stage 2 = parallel workers port the dependent file
  clusters under disjoint file ownership (one workspace is fine when ownership
  is disjoint); stage 3 = one integrator runs the exact-tree gates. Same rule
  for review (panel of disjoint lenses) and for fix rounds (one worker per
  disjoint finding cluster). Serial is only for: the core contract, the merge
  commit itself, and the gate-ladder/bookmark window.

- **Give every native refresh a private `HB_TMP`.** A refresh overlapping other
  build work surfaced `E-PROC-SPAWN`; the same source reached fixpoint in a fresh
  artifact root. Isolate fixed-name build artifacts and avoid overlapping proof
  builds before treating a process-launch failure as compiler corruption.
- **Retire staged syntax across every token owner.** Removing a compiler keyword
  is incomplete while verifiers, source analyzers, or lints still recognize it;
  sweep those owners and require an exact `E-UNDEFINED` regression.
- **Prove retired keywords against the exact candidate process.** In-process
  `evaluate` can surface a caught checker throw, while a hardcoded `bin/hb` may
  lag the candidate. Spawn `GE-HB$` and assert its rc plus diagnostic.
- **Test private package authority by lookup, not candidate rejection.** An
  undefined private qualified word makes a checker candidate uncheckable rather
  than ill-typed. `search-wl` proves the authority is absent; negative candidates
  remain for public cross-role calls whose signatures are visible.
- **Assert append-only registries relative to their entry count.** Sequential
  suites share one process, so an earlier consumer may intern valid objects.
  Capture the starting count, then prove dedup preserves it and insertion adds one.
- **Never interpolate Markdown backticks into a shell command.** Shell command
  substitution executes them even inside a quoted argument and can silently
  erase a stack effect from `dot add -d`. Pass dot text as data or patch the
  generated record, then inspect the exact stored description.
- **Keep worker prompts inside the claimed leaf.** A native-registry leaf may
  expose lowercase engine primitives; package-scoped checked wrappers belong to
  the dependent syntax/API leaf. Do not widen file ownership to impose the final
  surface before its layer is ready.
- **`dot on` is a transition, not an active-status probe.** The deployed
  serializer re-quotes `created-at` even on the first transition; repair that
  exact scalar until the CLI upgrade lands, and never run `dot on` again on an
  active dot because each rewrite compounds metadata drift. Activate once,
  publish the claim, then inspect the fetched remote dot file and use local
  `dot show` for final pre-spawn owner/status verification. A local clean status
  does not prove the coordination bookmark contains the claim.
- **Native primitive additions must grow the emitter registry deliberately.**
  Five sealed-owner WID primitives crossed `src/habu/habu1.f`'s 160-row
  `PRIM-CAP` and stopped fixpoint with `primitive registry full`; keep the
  named capacity above the emitted primitive count and prove a fresh rebuild.
- **Package-private helpers win unqualified lookup inside the package.** An
  owner-registry test package's private `LF` shadowed the outer `SLV-LF`, so
  multi-line forge source was concatenated and exited rc 74. Qualify the outer
  helper explicitly when two builder scopes share a natural tail.
- **Test hidden native authority through a separate image, never a runtime
  switch.** A build-time environment flag that publishes an internal mutator is
  still a production backdoor. Compose an isolated, content-keyed test image
  whose cold emitter calls the unpublished routine, and keep ordinary lookup
  unable to name it.
- **Isolate destructive build fixtures in a child process.** Resetting temp,
  engine, pin, digest, and extension globals does not restore preexisting
  caller state. Run the whole build in a child, assert its captured result, and
  let process exit revoke every build capability and artifact pointer at once.
- **Private package names are organization, not authority.** Packages reopen by
  design, exposing private words to the reopened scope. After compiling exact
  callers, `undefine` mutable capability names and prove reopen cannot recover
  them.
- **Publish append-only rows with an atomic row and release count.** Store each
  aligned pair in one write, release-store the count last, and acquire-load it
  before scans; clearing only the count on cold reentry leaves stale authority
  material in raw DATA, so erase every reserved row too.
- **Materialize addresses beyond scaled-immediate reach.** A direct `LDRW` at a
  DATA offset above 16380 encoded the wrong cell and made a cold-reentry proof
  loop forever. Build the full address in a register before word loads/stores.
- **Concurrency claims need emitted-code proof.** Single-thread behavior cannot
  distinguish acquire/release instructions from ordinary loads/stores. Scan the
  live emitted routine and pin acquire, atomic row store, release, and order.
- **New stage sources belong in every source owner.** Add post-compiler seal
  files to native builders, recovery composition, cache content keys, file
  inventories, and tests together; one omitted path preserves authority or
  reuses stale output.
- **Erase and verify package words in their owning WID.** Qualified names are
  not entries in wordlist zero. Reopen the package, select `public` or `private`,
  then `undefine` and `search-wl` each unqualified tail in `get-current`.
- **`jj new` off the tip immediately after moving bookmarks onto it.** Leaving
  @ ON a just-pushed commit means the next `jj commit`/snapshot AMENDS it,
  rewriting a pushed commit and forcing sideways bookmark pushes (happened
  with the claim commit amending the pushed DEFTYPE-replay merge). The window
  protocol ends with: move bookmarks, push, `jj new <tip>` — in that order,
  every time.
- **STATUS date rolls use the gate's UTC day, not the local calendar.** The
  run.f stale-status slice computes today from epoch-seconds (UTC). Rolling
  "Last verified" to the local date after midnight CEST made pushed master red
  for the hours until UTC caught up. Check `date -u +%F` before any date roll,
  and pass that same UTC day to manual stale-status/trust-lint invocations.
- **Stored checker effect rows are EN-node encoded — live-row walkers do not
  apply.** Deriving a fact (like min-in cell count) from a stored effect means
  re-walking the encoded rows; compute it once from the live row at build time
  and persist it in EFF-REC instead (ER.MINI pattern).
- **`type`-then-bare-`die` reads below base.** `die` is ( a u code -- ); a
  failure branch that prints its message first and then calls `1 die` hands
  die's operands to an empty stack (shadow-lint had exactly this latent bug,
  exposed by the underdepth guard). Always keep die's message on the stack for
  die itself.
- **Replay generated words in the definer's active package section.** Native
  `DEFTYPE` publishes its derived casts through the current private/public WID;
  verifier replay that registers only the nominal leaves valid cast calls
  undefined. Recreate both derived signatures before leaving that package mode,
  so reopening works while cross-package and global lookup stay closed.
- **`dot off` is not done until its file deletion is COMMITTED.** Closures run
  between merge windows in the main workspace get orphaned by the next
  `jj new <tip>` (the archive copy persists on disk, the tracked open copy
  returns with the checkout, and the CLI then sees both → Ambiguous ID). Six
  closures were silently lost this way in one session. Rule: every dot off
  is immediately followed by dot-dep-lint + `jj commit` in the same breath —
  batch closures if needed, never leave them in the working copy across a
  window.
- **Name the LAYER, not just the context, when recording probe results.** An
  E-UNDEFINED from a checked colon body is a CHECKER-layer verdict about the
  token grammar, not evidence about the runtime resolver. The dictionary-vs-
  literal "contradiction" was three probes hitting different layers (checker
  float grammar was digit-leading-only; the engine always claimed dot-leading)
  - recording "context: colon body" instead of "layer: checker grammar" kept
  the false mystery alive across three campaigns.
- **Host stack effects do not certify generated machine state.** An emitter can
  be perfectly stack-correct while its output clobbers a live register, flags,
  frame slot, SP, or a caller-owned buffer. Give emitted operands and callable
  routines first-class effects, then verify liveness and frame invariants over
  the resolved CFG; handwritten name-to-mask tables remain only transitional
  diagnostics.
- **A rejecting candidate needs a resolving positive control.** A misspelled,
  unavailable, or unmodeled target can produce the same quiet rejection as the
  intended nominal mismatch. Pair each negative matrix with the correct
  signature or assert the exact expected/actual diagnostic before calling the
  type boundary covered.
- **Inspect stored blockers after every multi-dependency `dot add`.** The local
  CLI retained only the last repeated `-a` argument, silently dropping earlier
  prerequisites. Patch the exact frontmatter, run dot-dep-lint, and verify the
  rendered tree before publishing or dispatching the leaf.
- **Seal an owner only after all constituent files are loaded.** Sealing blocks
  later package reopening; it cannot be the first slice of a multi-file API.
  Keep raw-to-nominal mints private and audited while assembling, then let the
  final owner-migration slice seal them and prove no public conversion remains.
- **Test nominal runtime bounds with a valid stale value, not a private mint.**
  Save a publicly validated handle, advance or rebuild its owner, then exercise
  the stale handle against the guarded consumer. This reaches the defensive
  range check without expanding raw refinement authority into a test file.
- **Do not guess dot CLI subcommands.** `dot dep check` was parsed as a request
  to create a task titled `dep`, and even `<unknown> --help` is quick-add syntax,
  leaving stray tracker files. Use only commands listed by `dot --help`; inspect
  or patch dependency frontmatter directly, then validate it through the
  repository's Habu-native `tools/dot-dep-lint.f`.
- **Do not shrink a semantic capacity around checker overcounting.** A W34
  PRODUCT and a repeated W12 input both exceeded the 255-cell minimum-input
  field even though their physical minima were 34 and 24. Reduce and fix
  `ROW-CELLS`/effect recording; a smaller effect row would only hide the
  compiler defect and make composition arbitrarily incomplete.
- **Structural ADTs are untrusted until a validator establishes semantics.** A
  public PRODUCT constructor proves layout and field roles, not mask, ordering,
  bound, or cache-legality invariants. Validate every parser, artifact,
  persistence, FFI, and registry ingress before any permissive decision.
- **A prose dot reference is not a dependency until the ID resolves.** Verify
  every claimed prerequisite with `dot show` before wiring downstream work.
  Capability documentation must label planned enforcement as planned; future
  linear-kind checks must never be described as protecting current code.
- **Run the full native gate without external Habu oversubscription.** Its own
  macOS profile already runs a 10-way pool; overlapping eight standalone Habu
  gates caused a pooled repository lint to throw `E-FS-OPEN` while the same
  exact tree passed alone. Parallelize focused gates together, then give
  `test/run.f` the host by itself.
- **Generated-state proofs need a backend-specific last-mile boundary.** ARM64
  CFG/register verification does not cover PTX merely because both begin with
  typed emitters. PTX must separately bind virtual def/use, predicates,
  barriers, address spaces, `ptxas` facts, cubin/SASS identity, and device
  evidence to the exact target and toolchain before promotion.
- **A representable by-value bound is not a semantic composition bound.** A
  127-cell effect row fits `ER.MINI`, but only carries 25 exact bindings and
  fails ordinary repeated call-site composition. Large canonical typed sets
  need opaque nominal handles over sealed immutable content; handles, offsets,
  allocation order, and hashes must never become authority or identity.
- **Preflight old implementation dots against current code, not their original
  premise.** Shared hardening can land through another owner while a residual
  dot stays open. Re-scope the dot to the exact missing regression or parity
  seam before claiming it; otherwise a worker will duplicate production code
  and miss the remaining proof gap.
- **Dependencies must name closable implementation milestones, not permanent
  trust-row owners.** A capability dot retained only so `TRUSTED.md` rows keep
  resolving can leave unrelated implementation work blocked after its usable
  contract has landed. Transfer durable ownership to a checked permanent-owner
  registry, close the implementation dot on evidence, and depend on that
  completed milestone.
- **Signal handlers need asynchronous-entry effects, not ordinary call
  effects.** The kernel supplies target-specific live registers and a ucontext
  frame, while the handler may edit saved PC/SP state and terminate through
  `sigreturn` or a no-return syscall. A BL routine contract cannot prove that
  boundary; type its frame, allowed operations, reentrancy, and terminator
  explicitly for each target.
- **Package-scoped family ctors live in a DERIVED escaped package.** A
  `PRODUCT pxevid` inside `package PX-PROBE` publishes
  `PX--PROBE-PXEVID:MAKE`, not `PXEVID:MAKE`; the unescaped spelling does not
  resolve, and a candidate probe returns verdict 1 (uncheckable), not 0
  (reject) — which silently satisfies a "not certified" expectation while
  proving nothing about the type wall. Assert exact candidate verdicts
  (-1/0/1) and pair every negative with a certifying positive control that
  names the derived ctor package (docs/type-families.md sec 12).
- **Runtime completion events are not persistent audit identities.** A CUDA or
  executor event is an ephemeral synchronization resource, while an artifact's
  creation event must survive replay and participate in canonical provenance.
  Give them distinct nominal families, and freeze wire ordering, compatibility,
  and digest coverage before implementing the artifact codec.
- **Remove satisfied blocker edges when their owner is archived.** `dot off`
  removes a completed dot from the live graph, so descendants must no longer
  name it in `blocks:`. Close a dependency chain leaf-to-root, delete each
  satisfied edge, and run `dot-dep-lint` before the closure commit.
- **A fresh gate root does not imply zero aggregate cache hits.** The full suite
  deliberately proves maker and artifact cache-hit paths inside one attempt.
  Retry isolation must reject artifacts inherited from a prior attempt while
  preserving and checking the expected within-attempt hit-counter contract.
- **`dot` has no free-form help namespace.** An unknown command is treated as a
  quick-add title, so probes such as `dot dep --help` create stray tasks. Consult
  `dot --help`, use only listed subcommands, and inspect `jj diff` immediately
  after every tracker command so accidental records cannot enter a commit.
- **Constructive ordering rules can make every legal edge forward — keep the
  seal-time cycle check anyway.** The async DAG's program-order threading,
  record-before-wait rule, and same-stream-only explicit deps mean insertion
  order is already a topological order, so smallest-index Kahn reproduces it
  byte-identically; the seal's Kahn pass exists to verify acyclicity against a
  buggy or adversarial builder, not to discover an order. Stating that in the
  file header avoided a "why is replay always 0..N-1?" review round.
- **PRODUCT values cannot ride the interpret-mode stack.** A top-level test
  line calling a `( -- fam-product )` accessor dies with `interpret-mode
  layout value`; wrap the construct/UNMAKE/assert sequence in one checked test
  word and call that from top level.
- **A `catch` effect is sound only if runtime restores the whole typed frame.**
  Restoring data SP and machine SP is insufficient when the checker also
  promises return- and loop-stack preservation; handler frames must save and
  restore every modeled stack before resuming the caller.
- **Balanced stack effects do not prove bounded execution.** A word can return
  every stack to its declared row while recursion retains a live return value or
  loop frame and grows without bound. Pair runtime extent guards with
  compositional peak-use certificates and capacity-checked execution targets.
- **PTX declarations and `ptxas` observations have different authority.** PTX
  state can verify declared parameters, virtual registers, and memory spaces;
  stack and spill facts arise only at the proprietary assembler boundary and
  belong in its content-bound attestation.
- **Tracker dependency arrows must agree with prose ownership.** If resource or
  device evidence consumes a new attestation, the consumer blocks on the
  producer; reversing that edge makes the documented implementation order
  impossible even when dependency lint finds no missing ID.
- **A new TRUSTED nominal mint must update the confinement seed atomically.**
  `TRUSTED.md` rows alone make the boundary auditable but do not teach
  `refine-lint` which owner and cited tests may call it. Add the seed name/owner
  and focused confinement fixtures in the same change; Maki tests do not cover
  the full lint-tools gate.
- **A sole public type surface includes auxiliary definers.** A hard cutover
  from legacy enum syntax must delete counter definers such as `ENUM+` and
  `ENUM4+` too; zero production consumers do not make a second declaration
  surface compatible with the one-DSL invariant.
- **Every device tool's top-level entry gates on `CUDA:OPEN?`.** A bare `MAIN`
  that assumes libcuda throws an opaque `E-CUDA` off-device and reads as a
  broken file in host gates (fusion-compare did exactly this while gemm-bench
  skipped cleanly). Follow the GB-ALL shape: probe `CUDA:OPEN?`, print a
  recorded SKIP line, exit — then the tool composes into host-side suites.
- **A `TEST:SUITE` block is ONE `bin/hb --load` spawn, not one per row.** All
  listed files load into the same image sequentially, so suite files must be
  package-scoped, duplicate-definition-safe, and tolerant of an earlier file's
  installed check hook (tools/lint/text.f's strict hook) and shared library
  state. The resident full-runner adds even more shared state: device/bench
  tools that pass in a fresh spawn can SIGBUS inprocess, so the resident
  inprocess suite list is a subset of the spawned slice, not a mirror.
- **Record token metrics in the unit you actually have, next to a slot for the
  unit you want.** An orchestrator Agent-tool generation round exposes no
  per-sample `usage` token counts, so the 2026-07-13 live transcript stayed
  format v1 (whitespace proxy) instead of inventing model counts; the
  source-derived `GEN-TOK-EST` column is computed at replay, so BOTH units land
  on every row (old transcripts included) without editing artifacts, and real
  v1.1 `tokens` counts slot in later. Estimators belong in the replay engine,
  not the recorded artifact.
- **Phase-token task surfaces make authoring pass@1 near-trivial by design —
  say so in the eval report.** Live gemm/attention rounds went 5/5 first-try
  because the checked phase words admit exactly one type-correct order; the
  honest claim is "the checked-pipeline surface is reliably authorable", not
  "the model wrote a GEMM". The emit-level structural required/forbidden gates
  catch only the SEEDED bug shapes: same-type role swaps (in/out, Q/K, O-into-V),
  dead-value stores, and double-phase kernels retain every required token and
  still grade GREEN — only a device numeric golden closes that class. Pin those
  wrong-but-green shapes as acknowledged regressions so a future grader
  strengthening flips them intentionally, not silently.
- **Bootstrap constants need their own earliest shared owner.** A target-width
  constant used by pre-checker records cannot live in a legacy definer file;
  load one invariant-owning source after `util.f` and before every consumer in
  native, recovery, fixpoint, pin, diagnostic, and cache manifests. Register
  its post-hook effects in a matching one-concern file, never a legacy DSL's
  effect owner.
- **Bootstrap manifest parity needs scoped, complete sequence comparison.** Parse
  bounded loader definitions, include semicolon-terminated final rows, compare
  the full ordered sequences and counts, and reject duplicates. Global substring
  presence and checker-prefix-only tests miss reordered or omitted tail files.
- **Fixed source arenas need composite byte budgets.** The stage2 source and
  cold-prefix sources share `IBUFSZ`; measure both, preserve useful assertions,
  and derive near-cap and cap-plus-one tests from the owner constant before
  increasing it with explicit headroom. Every landing that grows the compiler
  corpus must run `SOURCE-BOUNDARY` on the exact merge tree; claim-only commits
  do not excuse moving master past an exhausted arena. Land a capacity increase
  on the source-growth chain that requires it: a smaller bare tree can correctly
  select the previous power of two while the combined tree selects the next.
  The largest live composite selects the minimal shared power of two; smaller
  consumers prove headroom and power-of-two shape, not individual minimality.
- **Unsupported dot subcommands create dots.** `dot dep --help` is parsed as a
  quick-add for title `dep`; inspect `dot --help`, use `dot add -a` for blockers,
  and remove accidental tracker entries before committing.
- **Pre-checker layout assertions need an owner that survives the cutover.**
  Keep `CORE-LAYOUT-RC` with earliest-loaded `CELL`; schema and family layout
  checks must not retain a dependency on the structure definer being removed.
- **Replay gates need one exact candidate at both execution seams.** A parent
  run from a fresh candidate while spawned fixtures call a stale workspace
  `bin/hb` changes the engine-key component and creates false cache misses;
  install the exact candidate copy before running persistence/replay suites.
- **A folded predicate must seed its boolean before using `and`.** Whitebox
  layout probes return metadata before their predicate; the first comparison
  uses `=`, and only later comparisons use `= and`.
- **A test boundary may cover only one genuinely unmodeled operation.** Keep
  checker-known scope, definer, and rollback calls in checked orchestration;
  isolate each pre-hook registry mutation in its own typed `TRUSTED:` leaf.
- **A new engine primitive used by boot-prefix source lands in TWO stages.** The
  running binary reloads the boot prefix from disk at ITS boot, so checker.f
  cannot reference a primitive the current engine lacks (`E-UNDEFINED` before
  any build step runs). Stage 1: emit + register the primitive and put its
  TRUST row in the prefix (the literal token also keeps it through treeshake);
  build. Stage 2: add the prefix code that CALLS it; build again. The final
  tree still self-hosts (its binary has the prim), and the Gforth recovery
  compiles habu2.f before booting the prefix, so the recovery path holds.
- **The checker scans two different buffer kinds, and a token-skip must know
  which.** Hook-driven loads hand ENGINE-RECONSTRUCTED definition buffers in
  which an immediate's parsed payload is structurally absent (the immediate ran
  during compilation); candidate probes scan RAW text where the payload is
  still present. A payload skip that fires on the wrong kind eats live body
  tokens (`GRID: ceil-n-256 {: x .. :}` lost its locals opener). Default to
  the reconstructed kind: USER-INSTALLED strict hooks (tools/lint/text.f) are
  invisible to the checker, so only the explicit candidate entries may mark
  raw-text. And never exit a check hook bare: verdict-1 with no DIAGXT used to
  throw rc 70 with zero diagnostics - the exact opaque-exit class the
  debugging doc bans; check-hook.f now names the definition and failing token.
- **"Wired into a TEST:SUITE" is not "runs": gate-stdlib-cases.f suites execute
  only if a SUITE-*-LABEL? slice list selects the label AND someone invokes that
  slice - and test/run.f schedules no spawned slice at all.** Four engine-gate
  negative-regression files (internal-word-gate, underdepth-gate,
  immediate-model, top-row-hook) sat in unselected suites, copied from each
  other as templates, and never ran in any automatic gate. Mirroring a suite
  into a scheduled GSI group (stdlib/tail-process forks isolate hook installs
  and child spawns) is what makes it real; the completeness lint that makes
  this class impossible is tracked by habu-derive-inprocess-spawned-a54e760d.
- Off-device SKIP guards are a fail-open class: `CUDA:OPEN?`-style guards made
  fusion-compare.f green for weeks off-device while it would die uncaught
  (missing /tmp cubin, E-CUDA -5002) the moment a device was present. A device
  suite is only proven by an on-device run; prefer self-emit + fail-closed
  throws over prebuilt /tmp artifacts, and key device legs on the probed
  device-FFI capability, not ambient always-off gates.
- Gate slices see different lints: maki-dep-lint (dependency direction) lives
  in the lint-tools slice, so a lane that validates only lint-libs + maki/test
  can land a maki/ reference in the stdlib gate layer and stay green until the
  resident run.f. Integrator must run the slice that owns each touched file
  class, not just the suites the diff obviously participates in.
- Publication chains must be `&&`-guarded end to end: a closure commit went to
  master with dot-dep-lint RED because the chain ran the lint but did not gate
  the commit/push on its exit status (dangling blocker left by the closed dot).
  Run the dot gate, CHECK it, then commit - never `cmd; commit; push` in one
  unguarded script. Closing a dot must also sweep frontmatter `blocks:` lists
  that reference it.
- **Generated ctor-package names silently SHA-fall-back over 16 chars
  (TF-CTOR-NAME-LIMIT, src/core/type-family.f:604), making the family
  unconstructable in readable source.** A hyphenated family in a packaged
  namespace whose escaped `PKG-FAMILY` name exceeds 16 (e.g. `POLICY-REQ--CLASS`
  17, `POLICY-PROMOTE--POLICY` 22, EVID's `EVID-CERTIFY--SLOT` 18) gets an opaque
  `Thexhash-TAIL` ctor package instead of `PKG-FAMILY:MAKE`. It is stable
  (deterministic SHA) but unreadable and fragile, so it cannot appear in
  committed source. Keep `len(PKG) + 1 + len(escaped-family) <= 16` for any
  family a caller must construct (hyphens double: `req-class` -> `REQ--CLASS`);
  this is why R7's `req-class`/`promote-policy` became `req`/`gate-set`, and why
  EVID's slot sums cannot be built cross-package (blocks bundle construction).
  RESOLVED 2026-07-14 (dot habu-raise-or-alias-5d2a6b70): audit proved the 16 was
  NOT a structural bound — the runtime dictionary stores long names via DNAME-EXT
  (habu2.f C-STORE-NAME) and AOT captures them on the EXT kept-source path
  (aot-capture.f); the SHA fallback name is itself > 16 bytes and already works.
  So TF-CTOR-NAME-LIMIT is a readability cap, raised 16 -> 32 (longest real
  escaped name ~25). Names with escaped length <= 32 now keep the readable
  `PKG-FAMILY:MAKE` spelling (SHA only past 32); `EVID-CERTIFY--SLOT:CERTIFY-GOT`
  etc. are constructable by name. Do NOT rename to fit 16 anymore; keep escaped
  `<= 32`. Fixpoint stays byte-identical (limit is single-sourced prefix code).
- **Multi-cell layout values (products, tagged sums with payload) cannot be
  typed locals or `result`/`option` payload params - only single-cell types
  (TYPEFAMILY, payloadless ENUM) can.** `{: s:EVID:certified :}` and
  `result<POLICY:granted,n>` both reject ("unknown type ... in signature" /
  "expected: a actual: granted<>"). Consume multi-cell values straight off the
  stack via `UNMAKE`/`MATCH` (keep them DEEPEST so single-cell operands pop as
  locals first), and signal a fallible product-returning transition by THROWing
  a named code rather than a `result<product,_>` - the same wall the schema.f
  RESULT-DROP deviation names (dot habu-typestate-result-drop-5ae048a7).
- **A near-full fixed arena is a latent capacity bug a downstream lane inherits:
  trusted-inventory's class arena (CSTR-CAP) sat at 65528/65536 with 879
  classification rows, so adding 4 TRUSTED.md rows overflowed it with a bare
  `class arena overflow` die.** The ratchet ceiling is derived from row COUNT
  (fine to grow), but the byte arena was sized with no headroom; bumped
  $10000 -> $20000 to cover CMAX=1024 rows. Whoever grows a ratcheted manifest
  must budget the scratch arena, not just the count.
- The resident `test/run.f` gate does NOT run the full `test/gate-stdlib.f`
  TEST:SUITE inventory: it runs the in-process GSI groups
  (test/gate-stdlib-inline-lib.f) plus a few spawned slices. ~40+ cases suites
  (hashmap, float, prelude, object-*, task, ffi-cabi, device PTX, heavy builds,
  namespace/error-code lints, ...) run ONLY in the standalone
  `bin/hb --load test/gate-stdlib.f` full/slice merge gate. The spawned
  TEST:SUITE lists and the resident GSI lists are hand-synced and drift silently:
  a member mirrored into no scheduled GSI group runs in no automatic gate (four
  checker-invariant suites escaped this way; kbench added 7 spawned-only ptx
  files). `tools/suite-coverage-lint.f` now derives all three lists from the gate
  files each run and forces every member into scheduled / manual-documented /
  spawn-only-documented, and holds inprocess GSI-LINT-LIBS-PTX-TOOL ==
  spawned ptx-toolchain minus the documented SIGBUS bench set. Wire a new lint
  into BOTH the cases suite + the scheduled lint-tools GSI fork, or the lint that
  kills orphans becomes an orphan itself.
- **Package privacy is per-PACKAGE, not per-file: a `private` word is visible to
  any later `package X` reopen in ANY file, but NOT to non-X code or to a
  qualified `X:WORD` from outside.** This is the store-seal mechanism (dot
  habu-v2-typestate-promotion-2266b236): making EVID-PUT/EVID-PUT-G/SCHED-PUT
  `private` inside `package MAKI` keeps every package-MAKI reopen caller working
  (maki/cad.f PROMOTE-EVIDENCE, maki/store-replay.f SK-PUT-DURABLE, the store
  suite - all `package MAKI`) while a cross-package / top-level `MAKI:EVID-PUT`
  stops resolving. So the store-bypass regression is a verdict-1 (unresolvable)
  CHECK-QUIET-CANDIDATE!, proven non-vacuous by a paired `MAKI:EVID-GET` read
  control that still certifies -1 (same qualification mechanism). The threat model
  the R7 candidate suites use is exactly this: cross-package/top-level forging is
  rejected; reopening the owning package is inside the trust boundary (the same
  reason POLICY:MINT-GRANT-PROOF is "sealed" though a package POLICY reopen sees it).
- **A hyphenated ENUM/family in a package doubles the hyphen in its ctor package
  name: `EVID` + `golden-leg` -> `EVID-GOLDEN--LEG:DEVICE` (not `-GOLDEN-LEG:`),
  16 chars so no SHA fallback; `EVID-PREC--CLASS:PREC-F32` likewise.** Assuming the
  single-hyphen spelling gives E-UNDEFINED. The variant tail after the `:` is NOT
  doubled (`:PREC-F32`). MATCH still uses the bare family/variant (`MATCH golden-leg
  host OF ... device OF ... ;MATCH`) cross-package after a require.
- A pipeline eats the exit status the chain is supposed to guard on:
  `bin/hb --load lint.f | tail -1 && jj commit ...` guards on TAIL's exit, so a
  RED lint still commits and pushes (this shipped a red dot gate to master
  TWICE - the same closure/dangling-blocker class both times). Either
  `set -o pipefail` at the top of every publication chain, or capture the exit
  first (`out=$(cmd); rc=$?`). Also: `rg -rln PATTERN` is `--replace ln`, not
  list-files - the closure sweep displayed the dangling blocker with its id
  rewritten to "-ln" and it was misread as prose. Sweep with
  `rg -n 'PATTERN' .dots/` and read the raw lines.
- **`filemap-lint` treats ANY backtick token containing `/` (or a `.f`/`.fs`
  extension) as a path that must exist** (tools/filemap-lint.f LINT-CONTAINS?
  "/" / HAS-EXT?). A FILEMAP.md prose entry like `` `E-FOO/-BAR/-BAZ` `` (compact
  throw-code list) is read as the path `E-FOO/-BAR/-BAZ` and fails FILEMAP-STALE.
  Keep slashes out of backticks in FILEMAP prose: write `` `E-EVID-ROW-*` `` or
  spell the codes separately.
- **Typed store rehydration recovers FIELDS, not proof-carrying products, and
  mints no proof token** (R7 sub-dot 6, maki/store-rehydrate.f). A persisted row
  is untrusted text; `EVID-ROW-DECODE` parses each evidence field through its
  family (verdict / `EVID:golden-leg` / `EVID:prec-class`) so a bad field count /
  unknown label / out-of-domain enum throws a named `E-EVID-ROW-*`, never a silent
  default. It deliberately does NOT build `EVID:certified`/`golden` (those need the
  class-private MINT-*-PROOF and would forge provenance a mere read never earned -
  addendum :1824). Re-render goes back through the sealed `EVID-PUT-G` so the
  read boundary duplicates no wire format and adds no store-planting surface. The
  schedule KEY can't be re-typed on read (no region facts at load -> no `skey`
  FNV hash; sched-key.f durable-text boundary), so the schedule side is closed by
  SEALING the writer (`SK-PUT-DURABLE` -> package-MAKI-private) plus the
  pre-existing fail-closed SCHED-LINE parse, not by key-shape validation (which
  would break the synthetic-key mirror the growth test relies on).
- Third red-master push, third guard shape: the batch DISPLAYED the run.f log
  (RED visible in output) but the push step had no conditional on it at all -
  human eyes are not a gate. Every merge step must MECHANICALLY test the
  verdict: `rg -q 'RUN_EXIT=0' "$LOG" && jj bookmark move ... && push`, never
  "print the log, then push in the same batch". Also: a new lib/ module needs
  a lib/std.manifest module row PLUS a word row per public word
  (stdlib-manifest-test enforces; the lint-manifest slice is the owning gate a
  new-lib lane must run - host/filemap/trust/coverage do NOT cover it).
- **A test suite that opens `package X` must close it with `;package`, or a LATER
  suite in the cumulative `maki/test.f` run dies with an opaque `exit 75` and a
  bare token (e.g. `package`) on stderr.** The `TEST:SUITE` files are `included`
  into one process; a suite leaving its package open leaves that package the
  active wordlist, and a later suite's `package` declaration hits the engine's
  `does>`/quotation compile guard (`J-QUOT`/`J-DOES`/`J-SEMIQUOT`, habu2.f) which
  exits 75 printing the current token. It is NOT a dict/code/family/protected-WID
  capacity limit: the suite that crashes is 1-2 later than the culprit, the
  culprit's own suite PASSES, and the crash reproduces only when the earlier
  suite omits `;package`. Diagnosis wasted a long capacity-bisection detour
  (every "N families / N instantiations overflows" reading was really this same
  unclosed-package bug in the throwaway test harness). Put `;package` before
  `T-REPORT` in every package-scoped suite; if a full-suite run dies at an
  unrelated later suite right after adding yours, check for a missing `;package`
  first.
- **A product TYPE PARAMETER binds only cell-tier types (n or nominal
  `TYPEFAMILY` cells); a sum/enum/product family cannot instantiate it.** So a
  generic `comparison<a>` over a metric unit needs nominal-cell unit witnesses
  minted through TRUSTED casts, and the value inside the parametric slot stays
  unit-agnostic. Two concrete per-unit families (`comparison-gbps`/`-gflops` with
  distinct `gbps`/`gflops` sum readings) are strictly stronger - the NUMBER is
  unit-typed, a gflops value cannot enter a gbps slot - with zero TRUSTED surface.
  Reach for concrete per-variant families over a parametric product when the
  variants are few and the parameter would only be a cell-tier phantom.
- **`error-code-lint` scans `maki/target/` etc. (subdirs); a bare `maki/*.f`
  glob misses them.** `-5252..-5256` are owned by `maki/target/target.f`
  (E-TARGET-*), invisible to a `rg maki/*.f` scan. Always scan `maki/` (recursive)
  for a free `-5NNN` block and confirm with `bin/hb tools/error-code-lint.f`.
- **`jj workspace add` does not change the shell directory.** Run every
  following `jj` command with that workspace as the explicit working directory;
  otherwise a claim merge silently advances the caller's unrelated workspace.
- **A bulk symbol migration must exclude its replacement spelling and end with
  an exact legacy/near-miss scan.** Rewriting `E-*` after introducing
  `ENGINE-ERROR:*` can corrupt the new spelling when the match is not token
  exact; scan for both old tokens and malformed replacement prefixes before
  testing.
- **In effects, `ptr n` is one pointer type, not two stack cells.** An emitter
  taking a pointer plus a numeric length therefore declares `ptr n n`; keep the
  pointee role and the following scalar visually distinct when editing effects.
- **Prefix additions have two independent exact-count ratchets.** Update both
  `test/boot-pin-test.f`'s raw `PFX-LOAD-ROW` count and
  `tools/diagnose-hb-test.f`'s common-source count; one proves the generator
  shape while the other proves every baked source is diagnosed.
- **A reserved package has three independent authorities.** Keep the native
  reserved-name table, recovery mirror, and `CHECKER-SEALED-PKG?` synchronized;
  native seal tests alone do not exercise checker-only export/layout guards.
- **Corrupt the unique embedded lookup token to prove a missing boot bridge.**
  A normal candidate proves the post-seal bridge succeeds; changing only its
  sole checker lookup spelling creates the otherwise-protected missing-hook
  state and gives an exact fail-closed exit regression without a production
  test hook.
- **Result-union cutovers must migrate every direct consumer effect.** The
  process API correctly returned `outcome`, but one gate fixture still declared
  four scalar results and made green source fail at checker load. Audit direct
  call sites and replace scalar kind tests with exhaustive `MATCH outcome`
  storage before declaring the cutover complete.
- **Diagnostic origins belong in the source scanner's single pass.** Recomputing
  line and column by rescanning from byte zero for every definition made
  fixpoint certification quadratic: a 1.0 MB, 3,250-definition source spent
  98% of sampled time in that rescan. Advance byte, line, and line-start state
  together at the scanner cursor, then snapshot the token origin in O(1).
- **Worker preflight must prove required public syntax exists on the claimed
  tree.** An open downstream dot can look dependency-ready while its requested
  schema still requires unlanded STRUCTURE/payload ENUM and compiler lowering.
  Probe the exact grammar and generated words before `dot on`; if absent, wire
  the transitive implementation milestone and leave the downstream dot open.
- **One-shot assembler guards retain nominal inputs until validation ends.** A
  label-binding guard typed as raw `n` weakens the checker at the exact authority
  boundary. Accept `label`, use a named exit code, and place the guard before
  every state write; then snapshot fixup, free-list, label, and code state.
- **Plan dependency claims and dot edges land together.** A plan that says a
  production authority waits for native fencing is false if the owning dot can
  still dispatch without it. Update the prose and `blocks:` edge on one reviewed
  tree, and distinguish baked primitive effects from verifier-added effects.
- Cross-agent engine landings brick sibling binaries: sol's engine-error
  namespace added new COLD-PREFIX files (engine-error.f), and every other
  agent's bin/hb (baked prefix list) then fails AT BOOT with E-UNDEFINED on the
  updated consumers (checker.f, lib/map.f, src/habu/layout.f, xref.f - find the
  baked list with `strings - bin/hb | rg '^(src|lib)/'`). A hybrid
  revert-and-refresh does NOT compose (mixed-generation sources fail the child
  build). The reliable recovery is the documented gforth bootstrap
  (HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh) - it works iff the landing
  mirrored the prefix into bootstrap/cg/forth.fs, which is why that mirror is
  BLOCKING for any new cold-prefix file. Pull-and-refresh after every fetch
  that crosses an engine landing; budget minutes, not hours.
- trusted-inventory STRICT resolves owners only at `.dots/<id>.md` or
  `.dots/<id>/<id>.md` (self-named epic) - a child dot under ANOTHER parent's
  directory is invisible as an owner, and closing a dot orphans every row it
  owns (third occurrence of this class). Re-own rows in the SAME commit that
  closes their owner: prim-axioms to habu-primitive-effect-axiom-1119f176,
  program rows to the live epic's self-named file. Also: sweeping a dot's last
  `blocks:` entry must remove the bare `blocks:` header too, or the frontmatter
  goes unparseable to some tools.
- **Canonical-base snapshot rebasing must classify package records before
  pointer ranges.** At canonical text base zero, a package's raw public/private
  WIDs look like low text offsets; skip record `[0]/[8]` when `[40] = -1`, while
  still rebasing an external name pointer.
- **Snapshot scrubbing must use the exact return-stack extent.** Zeroing from
  `RSTK-OFF` through `DATA-START` erased persistent protected and owner
  registries that share the engine-reserved band; derive `RSTK-END` from the
  256-cell capacity and scrub only that half-open interval.
- **Owner WID numbers are generation-local; canonical package identity is the
  persistence authority.** A refresh can retire and rebuild the package with new
  WIDs, so snapshot capture must rebind the baked AOT name frame and replace
  stale rows before copying DATA.
- **Persisted package visibility needs an executable negative proof.** A
  `search-wl` absence check does not prove qualified compilation is rejected;
  run the AOT and snapshot images against a checked private-word call and require
  the undefined-token diagnostic.
- **Snapshot presence belongs to the loader-authenticated executable extent.**
  Trailer magic distinguishes formats only after the OS header proves appended
  content exists; treating missing magic as a cold image lets corrupted snapshots
  boot through the cold path.
- **Pre-copy snapshot validation must translate canonical external pointers.**
  Long dictionary names point into the persisted region coordinate space; map
  them to the appended source bytes before comparison and never borrow pinned
  DBASE, NDICT, or CP registers as validator scratch.
- **Append-only registries publish rows before counts.** Writers use release
  stores for the count and capture uses acquire loads; model LDAR/STLR as named
  assembler operations instead of embedding instruction words at call sites.
- **Persistent builder state must preserve pointer identity explicitly.** Use a
  typed pointer variable when the pointer itself survives calls, or retain a
  stable record index and derive the pointer only at the use site.
- **Snapshot DATA must exclude invocation-mode hooks and latches.** A snapshot
  built while stdin is a TTY persisted `REPLH-CELL`, so a later `--load` printed
  success and entered the REPL. Canonicalize those cells to zero and recompute
  batch versus interactive state after restore on every boot.
- trusted-inventory --strict needs BOTH a descriptive TRUSTED.md table row AND
  a CMAP classification row for every new TRUSTED word - the table row alone
  reads as unclassified. An overflow boundary over a wrapped raw scanner must
  be proven with an up-front typed guard (ADVANCE-BYTE-OFF before the scan):
  the raw scan cannot reach MAX-N cheaply, so without the guard the "overflow"
  case is untestable and silently wraps. (strnum lane findings, 2026-07-15.)
- **Unknown `dot` forms are mutating until proven otherwise.** Read `dot help`
  before discovery: a bare positional form can create a dot instead of querying
  dependencies.
- A landed "documented skip" claim must be EXECUTED-verified, not
  header-asserted: sub-dot 3's report said "snapshot/AOT boots skip the
  tracker (SNAP-CELL-guarded)" but no such guard existed - every snapshot
  boot SIGSEGVed on the captured-armed hook, and the red (owner-wid-child)
  hid because that leg runs under HABU_OWNER_WID_HARNESS=1 in a forge phase,
  not as a plain scheduled test. Two generalizations: (a) any hook/state a
  cold-prefix file arms must be explicitly disarmed in SND-ZERO-LIVE unless
  its warm-boot behavior is executed-tested; (b) warm boots replay the
  unguarded `provided` re-establishment rows that cold boots never expose to
  late-armed hooks - arm-order equivalence between cold and warm is a claim
  that needs a snapshot-boot test the moment a new boot-time hook lands.
- Two push-verification bugs in one day, same root: (a) `jj log -r 'X & ::Y'`
  prints NOTHING when false - an empty result must be tested with `[ -n ]`,
  never eyeballed (a dot was closed on a landing that never pushed, blocking
  two dependent lanes; found by the dependent worker's E-UNDEFINED probe).
  (b) `jj git push` reporting "Move SIDEWAYS bookmark master" means NOT a
  fast-forward - a sideways master push orphans other agents' commits (sol's
  claim commit was dropped for ~1 minute before a corrective rebase+push).
  The merge protocol is now: positive ancestry check (`[ -n "$(jj log -r
  'origin-tip & ::candidate' ...)" ]`) BEFORE moving bookmarks, and treat the
  word "sideways" in push output as an automatic stop-the-line.
- A whole-model device corruption probe must be MAGNITUDE-INDEPENDENT. A first
  cut used an activation SWAP (gelu-epilogue cubins vs a relu-epilogue model,
  same 4x8 MLP) and it VACUOUSLY PASSED on the Orin: the LINEAR layer amplifies
  the synthetic inputs to large-positive pre-activations where gelu ~= relu to
  well inside the composed f32 tol, so the "wrong pipeline" matched the golden.
  Any smooth-vs-relu swap converges to identity for large-positive activations;
  don't probe a post-LINEAR region with an activation swap (ablate-golden's
  gelu-vs-relu works because it hits the RAW 4x8 input, not an amplified one).
  Use a magnitude-independent op (ablate-ptx.f PTX mutation: operand-base
  redirect %rd1->%rd2 / drop the per-lane index add) on a matmul region - it
  diverges regardless of sign/scale and drove the whole-model golden to V-FAIL.
  Corollary: keep the COMMITTED e2e proof the clean PASS, and demonstrate the
  corruption in a temp copy (a baked probe that fails-to-fail turns the
  committed proof red on device).
- Typed-top tier-2 (sub-dot 7) landed STAGED, not default-on: a mode cell in
  top-row.f defaults to tier-1 (warn), and HABU_TOP_TIER=2 stages tier-2, where
  each tier-1 warning site instead `throw`s RC-REJECT (70) from the PRE-BLR WORD
  hook - a clean rc-70 reject, catchable, REPL-recoverable via the SAME
  EM-REPL-RECOVER path as the native DNAME-MIN-IN underdepth reject, and no crash
  (p2 `0 0 catch` no longer BLRs into xt 0). Default-on was deferred because the
  tracker is cold-boot only (snapshot disarms the hook, blocked on 4bd33351) and
  the child-population blast radius across the suite is large; the parent-level
  tree census is already clean (0 `hb: top-row:` across run.f + maki), so no
  parent-level tree fixes were needed - only the default flip remains.
- Child-env in test fixtures: RUN-ARGV-STDIN-CAPTURE-OUTCOME spawns via the
  non-env `spawn-argv-io`, which INHERITS the parent environ and IGNORES the
  PROC-ENV table entirely. To inject a child env var (e.g. HABU_TOP_TIER=2 to
  force a tier), you MUST use the RUN-ARGV-ENV-* variant + PROC-ENV+; add
  PROC-ENV-INHERIT-MISSING to keep HB_TMP and the rest, and pin the var
  explicitly (set "1" for the off case, not just omit it) so a tier-2 gate leg
  cannot leak into a control child. Cost me one debug cycle (children silently
  ran tier-1 despite the env row).
- **Enum-owned state must stay nominal in memory.** Mirroring variants as raw
  integer constants and storing them in an untyped variable creates a second tag
  authority. Use `LAYOUT-BUFFER`, generated constructors, typed fetch/store, and
  exhaustive `MATCH` from declaration through persistence and rendering.
- **Writable roots need search access too.** `access(W_OK)` accepts mode `0222`,
  but cache clients cannot traverse it. Root readiness must require `W_OK|X_OK`,
  with both the predicate and resolver pinned by mode-specific tests.
- **Capture reports at the completion boundary.** Rendering from mutable worker
  trace cells leaks implementation state into callers. Capture one typed report
  when the build finishes, then expose accessors and wire renderers over it.
- **Failure evidence needs separate owned storage.** Clearing a bounded working
  path before rejecting it also erases the attempted root, while rendering a
  maximum-length path through `SB` can mask the owner error. Retain the complete
  attempt independently and size diagnostic output from its actual length.
- **Invalidate reports before work starts.** A completed report remains stale
  truth after the next build fails unless build entry clears its lifecycle
  state. Invalid report access must fail closed with a named status error.
- **Quote paths in line diagnostics.** A path is arbitrary bytes, not prose;
  appending it raw lets tabs and newlines forge fields or records. JSON-quote it
  even in text mode so each failure remains one exact line.
- **Duplicate loop indices before interposing the base address.** With
  `index byte base`, `over` duplicates the byte and corrupts the computed
  address. Keep `index byte over base + c!` so `over` still selects the index.
- **Baseline a red performance gate before blaming the candidate.** Master and
  the cache branch failed identically while an orphan canary consumed a core;
  compare both trees and audit host workloads before changing code or budgets.
- **Nested fixture forks need fresh pool and temp ownership.** Reusing inherited
  pool state collided with the parent captures, and sharing one gate root raced
  compiler scratch files. Reset the child pool, give each worker its own gate
  root, and keep cache-sharing fixtures in one worker; AOT coverage then fell
  from 44 seconds to about 30 seconds without duplicate cold maker builds.
- Type-family names obey package public/private exactly like words. To let an
  external package name a family as `PKG:name` in signatures/locals, declare the
  `TYPEFAMILY` in the package's `public` section (precedent: lib/cad-num-types.f;
  visibility is captured from `CHECKER-PACKAGE-MODE` at declaration, sumtype.f
  `TDECL-VIS`). `EXPORT` does NOT apply to family names - they are checker
  type-registry entries, not dictionary words with an xt, so there is nothing to
  re-export. A `private`-section family resolves ONLY from inside its own package
  (own private rows in `TFAM-QUAL-RESOLVE`), which is why an in-package test can
  write `NOM:path` while an outside consumer cannot. Moving lib/nominal's
  row/path/binding TYPEFAMILY lines from `private` to `public` let src/cad/effect.f
  name `NOM:row` directly and deleted two no-op `TRUSTED:` brand casts
  (ROW>EFF/EFF>ROW) plus their manifest rows - the family stays fail-closed
  (raw `n` where `NOM:row` is expected still rejects), so publishing the NAME cost
  no soundness. Ordering gotcha: the family must be declared before the private
  mint/erase `TRUSTED:` words that reference it, so open the package with the
  `public` TYPEFAMILY first, then switch to `private` for the arena/mints.
- **A "fork/COW state-leak race" can be deterministic argv contamination in
  disguise.** The perf-regress inprocess flake ("malformed registry row: ...
  device isolation fixture" under `test/run.f -- --under bin/hb`; two lanes filed
  it as a fork worker inheriting the parent PERF ROWS arena) had NOTHING to do
  with fork/COW. Real cause: `tools/ptx/perf-regress.f` is a CLI whose registry
  path is `SCRIPT-ARGC 0 > if 0 SCRIPT-ARGV$`, and it was `included` into the
  resident gate image. In-process, ambient `SCRIPT-ARGV` is the HARNESS'S argv
  (`--under bin/hb`), so it `PERF:LOAD "--under"` -> `FILE-SIZE` throws E-FS-STAT
  (-2101) before any parse. A CLI tool that reads ambient argv must NOT be
  `included` into a shared image carrying foreign argv - run it spawned (clean
  argv) and carry its gate assertion in an argv-free `-test.f` fixture. Two
  reproduce-the-mechanism lessons: (1) `bin/hb --load a.f b.f cli.f -- <args>`
  faithfully reproduces the inprocess-include argv leak - no fork needed; the
  "intermittence" was just which invocations carried `-- <args>`. (2) The reason
  it was misread as a data/arena bug is the SECOND defect: `PERF:RESET` cleared
  the row arena but not the diagnostic cursor (`PF-LOFF`/`PF-LU` that
  `LAST-LINE$` reads), so a throw-before-parse printed a STALE line from the
  previous test - a non-hermetic reset made a path/argv error masquerade as a
  malformed-row error. Reset ALL state a public diagnostic can read, or the error
  text lies about the failure class and sends the next lane down the wrong trail.
- **When a "requested" axis becomes a DERIVED function of the artifact, the
  promote/refuse test must flip the OTHER axis - and inject it through the typed
  seam, not a removed knob.** dot habu-per-op-requested: sched-key.f `REGION-POL`
  now folds each op's intrinsic domain (`NPOL:OP-DOM`) over the region instead of a
  per-class table, so a region's REQUESTED policy equals its op floor
  (`= REGION-ACHIEVED`): a pure-gelu region requests relative, a pure-relu region
  exact - the mixed elementwise class could not express that. Consequence that bit
  the test rewrite: the old cad-test manufactured a numeric-gate REFUSE on host by
  flipping the requested policy INDEPENDENTLY (`POL! exact` on a matmul region while
  achieved stayed relative). Under per-op there is no independent knob, AND on host
  `GOLDEN-GATE-G` always returns `prec-f32`/exact (the strongest), so
  `GOLD-ACHIEVED = exact COMPOSE op-floor = op-floor = requested` -> the gate ALWAYS
  satisfies; you cannot produce an executed refuse from the policy side. The honest
  executed refuse drives `PROMOTE-NPOL` with an injected
  `EVID-PREC--CLASS:PREC-TF32` (the device golden's relative leg the report-path
  gate is parameterized on) against an EXACT-floor region (relu-only): relative
  achieved cannot satisfy the exact request. Flip only the region's op (gelu vs
  relu) under a fixed TF32 leg to prove the gate reacts to the per-op requested
  policy. Removing the ambient table also retired its class error (`E-NPOL-CLASS`,
  -5147) since `NODE-POL` was its only runtime reader.
- **A standards doc can silently adopt a plan's aspirational grammar as if
  shipped — verify every token against the live engine, not a sibling doc.** dot
  habu-reconcile-forth-md-289ac1be: `docs/forth.md` § Structures And Enums (and
  `docs/type-families.md`/`MODEL-CAD-V2-PLAN.md`) documented the planned
  MODEL-CAD-V2 unified `STRUCTURE … ;STRUCTURE` grammar as the current surface and
  listed `TYPEFAMILY`/`SUMTYPE`/`PRODUCT`/`ENUM`/`VALUE-RECORD`/`BEGIN-STRUCTURE`/
  `ENUM+` as "removed syntax" with `E-REMOVED-TYPE-SYNTAX` tombstones. Proven by
  `rg` + one-line `bin/hb` load probes: every "removed" word is a live,
  heavily-used definer (`src/core/sumtype.f`, `roles.f`, `structures.f`,
  `enums.f`); `STRUCTURE` does not exist (`E-UNDEFINED: STRUCTURE`, exit 70, not a
  tombstone); `E-REMOVED-TYPE-SYNTAX` appears nowhere in the engine. The doc even
  inverted the payload rule — `SUMTYPE` variant payloads are POSITIONAL type
  tokens (a named `FIELD` inside a variant rejects, `E-TDECL-SYNTAX` 7107) and
  named `FIELD` belongs to `PRODUCT`; `ENUM` is bare-names-only (an arity or
  `VARIANT` token rejects, 7101). A token's status is whatever `bin/hb` does with a
  one-line fixture, never what a cross-referenced doc claims.
- A snapshot build can carry two checker copies: the engine cold prefix and the
  generated build payload. Calling the newest `CHECKER-SNAPSHOT-PREPARE` only
  resets the payload copy; engine AOT hooks can still dereference the first
  copy's dead mmap owners after restore. Install a fixed first-copy
  `CHECKER-SNAPSHOT-PREPARE` xt after the cold checker loads, invoke that exact
  owner before the payload checker prepares, and clear the xt from persisted
  DATA. Prove the two clearing boundaries independently: inspect the raw image
  cell for zero to cover capture scrubbing, then re-sign a forged-nonzero image
  and read the cell from user code to cover startup reset. A normal boot alone
  lets either boundary hide the other's omission.
- **Engine-layout regressions belong to the candidate gate, not a stale host
  tail.** A host `bin/hb` predating a new sealed DATA cell cannot enforce its
  guard band, so running the fixture there reports a false implementation
  failure. Launch the fixture from candidate validation, pass the same candidate
  through `HABU_UNDER_TEST` to its child probes, and keep host tails for contracts
  shared by both layouts. Suite coverage must derive `GE-SRC-FILE+` candidate
  members too, or moving a real fixture makes the inventory report a false
  orphan.
- **Build-only hooks must branch at the build entry, not inside the shared cold
  prefix.** `LCOLDPFX` and `LCOLDPFXB` assemble one body for ordinary and build
  modes; appending a hook unconditionally during base-file loading exposes it to
  every user source. Reload the saved entry mode at the append seam and regress
  the ordinary `--load` cell as zero.
- **A composed-Gemm DEVICE golden must pick a form whose matmul epilogue is empty
  or a unary activation.** dot habu-device-golden-composed: import.f lowers a
  non-default Gemm by COMPOSITION - transB inserts a TRANSPOSE movement node,
  alpha<>1 an OP-SCALE, a separate bias an OP-BIAS. FP-BUILD makes the transB
  transpose a STANDALONE MATERIALIZED movement region (region 0) that FEEDS the
  matmul region - and that IS device-emittable (`LMDM-EMIT$` dispatches
  `LLA-REGION-MOVE?` -> `LMV-EMIT`; lower-mv-device-test.f already goldens a
  standalone transpose copy kernel), so `Gemm(x,w,transB=1) -> Relu -> Gemm`
  goldens on the whole-model device path (the movement->matmul cross-region
  buffer the pure-matmul ort-ref golden never covered). BUT OP-SCALE/OP-BIAS
  fuse into the matmul region as EPILOGUE nodes, and lower-mm.f `LMM-EPI-OP?`
  accepts ONLY relu/gelu/silu - so a composed Gemm with alpha<>1 or a separate
  bias is NOT device-lowerable and rejects FAIL-CLOSED (`LMM-CHECK-OPS` throws
  E-LMM-OP -5194). Confirmed off-device by calling the private MAKI `LMM-CHECK-OPS`
  from a `package MAKI` test under `catch`. So keep the composed golden fixture
  bias-free / alpha=1; the scale/bias matmul epilogue is a residual owned by the
  FENCED lower-mm.f. Reference: no onnxruntime exists for a composed Gemm, so the
  committed CRF-Y is the HOST executor oracle (validated ==ort at 1e-5 on the
  ort-ref fixture) - device-vs-host discipline, ort leg a documented residual.
  Corruption probe: an in-process PTX capture + `ABL-MUTATE` fma(a,b,acc)->
  fma(a,a,acc) on the matmul (magnitude-independent, in-bounds) drove the
  whole-model golden from PASS to a clean REJECT on the Orin.
- **A reduction-dominated fusion win is bounded well below the equal-cost-op
  round-trip model.** dot habu-automatic-aggressive-fusion leg a (layernorm):
  the maki op set has NO separate MODEL:-parseable mean/variance reduce -
  `OP-LAYERNORM` is ONE row-reduce op doing mean+var+normalize internally (two
  BLOCK-SUMs in `lower-red.f` LRED-EMIT-LN) - so layernorm's fusable seam is the
  reduction's EW EPILOGUE (`LAYERNORM SCALE BIAS` = ROW-REDUCE->EW->EW), not a
  ROW-REDUCE->ROW-REDUCE decomposition. `FP-BUILD` fuses it to ONE region
  (ablated = 3); both device==host golden PASS. But the measured fused-vs-ablated
  win was 1.41x, NOT the ~3x the equal-cost 3-op model predicts: the block-per-row
  reduction dominates (1.07 ms/launch, ~7.8 GB/s - the LRED schedule is not
  bandwidth-optimal), the scalar-scale + 1xC-bias epilogue folds in at ~zero
  marginal cost (fused 214.3 vs ablated layernorm-alone 213.5 ms/200 iters), and
  fusion's only removable work is the two standalone flat-EW passes (43.4+45.6 ms
  of global round-trips). The round-trip savings are real; the RATIO is capped by
  whatever fraction of the chain the reduction is not. Report the honest measured
  ratio with the root cause, never the naive Nx.
- **A sibling kernel's corruption-probe predicate name does NOT transfer - dump
  the actual PTX.** RMSNORM's dropped-mask fault targets `@%p2`
  (`ablate-golden-device-test.f` ABL-G3), but for LAYERNORM `@%p2` is the SCALE
  broadcast-scalar load mask (harmless to drop: all lanes read element 0 and
  inactive lanes are discarded at the masked store, so the golden still PASSes).
  The predicate numbering shifts with region input count: LAYERNORM+affine has 3
  inputs -> load masks `%p1/%p2/%p3`, and the first block-sum's inactive-lane seed
  mask is `@%p4 mov.f32 %f5, %f1` (with `%f1` pre-seeded to `0fFF800000` = -inf).
  Unmasking it leaks -inf into the sum -> mu=-inf -> clean REJECT. Inspect the
  emitted kernel PTX to pick the reduction-seed mask; never copy a sibling's
  `@%pN`. Also: `OP-MUL`/`OP-ADD` require SAME-shape operands (SHP-SAME-OK?), so a
  per-feature 1xC affine gamma is not expressible via MUL - use `OP-SCALE`
  (1x1/same) + `OP-BIAS` (1xC).
- **The block reduction is warp-shfl now; `shfl.sync.down.b32` takes `%f` regs
  directly.** dot habu-ptx-m6-perf-6b979497: `EMIT-REDUCE` in
  `lib/ptx/cg-collective.f` replaced the O(B) thread-0 shared-memory fold with the
  standard two-level warp reduction - a full-warp `shfl.sync.down.b32 %fD, %fS,
  off, 31, -1` tree (offsets 16..1) per 32-lane warp, per-warp partials staged to
  `SMEM[warp]`, then a final single-warp reduce of the `CG-WARP-COUNT` (= B/32)
  partials into `SMEM[0]`. `ptxas` accepts the shuffle DIRECTLY on `.f32`
  registers (c-operand `31` = warp width - 1, membermask `-1` = full 32 lanes),
  exactly as nvcc emits - no `mov.b32 %f<->%r` round-trip. Blocks are always a
  warp multiple (`PTX-BLOCK-LEGAL?` requires `n mod 32 = 0`), so all 32 lanes of
  every warp execute the shuffle and the `-1` membermask is always well-formed;
  the "tail warp" the plan worried about is (a) k not a warp multiple, handled by
  the SAME inactive-lane identity seed (`@%p mov v, tile`, else -inf/0/+inf) that
  threads through BOTH shuffle levels, and (b) the final reduce's up-to-32
  partials, past-the-count lanes seeded identity via `setp.lt tid, CG-WARP-COUNT`.
  The final-reduce guard `@%p bra` is warp-uniform (all lanes of warp 0 fall
  through), so warp 0's shuffle keeps a full membermask - no intra-warp
  divergence. Measured on the Orin (25W, block-per-row LRED layernorm, the
  softmax/rmsnorm/layernorm codegen): LN-FUSE-ON 7.83 -> 11.37 GB/s (1.45x),
  LN-FUSE-OFF 16.64 -> 21.22 GB/s; softmax-launch / sum-launch / gradcheck /
  lower-red-device / lower-model-device all stay device-golden green.
- **A predicate-numbering shift does NOT always move a corruption probe - but
  verify by dumping the PTX.** The warp-shfl rewrite changed the reduction's
  predicate count, yet `maki/ablate-golden-device-test.f` ABL-G3 still strips the
  correct `@%p2` (the RMSNORM inactive-lane identity seed): RMSNORM has one
  ROW-LOAD mask (`@%p1`) then the reduce seed (`@%p2`), and that prefix is
  unchanged, so the FIRST `@%p2` is still the seed. Confirmed by capturing the
  emitted RMSNORM PTX (`PTX-CAPTURE-ON 0 FP-REGION-ID LRED-EMIT`) off-device
  BEFORE trusting the probe - do not assume the shift moved it, and do not assume
  it didn't; the shuffle reduction still fails-closed to V-FAIL when the seed mask
  is dropped (inactive lanes leak +inf into the sum).
- **When an error-recovery mechanism is upgraded, migrate every branch to the old
  entry in the same change — the sibling left behind becomes the residual bug.**
  dot habu-interpret-err-under-8876b500: the compile-abort legs (LUNDEF/LWIDE/
  LMININ) had moved to the catchable RC-REJECT throw via LEVALREC, but the LMAIN
  underflow guard's EVALD>0 leg still used the old rollback-and-return
  (EM-EVAL-UNDEF-ROLLBACK: pop the frame, set EVALERR, return). Rollback-and-return
  is fail-open both ways: `[: INCLUDE-EVALUATE ;] catch` read 0 for E-UNDERFLOW
  while E-UNDEFINED was correctly caught 70, and a handlerless caller kept
  interpreting past the failed evaluate with rc 0. One throw contract per boundary;
  grep for every jump into the retired recovery entry before calling the upgrade
  done. Also: re-verify a dot's exact repro on the current base before red-first
  work — both dots of this pair were already fixed by intervening engine work, and
  the genuine remaining defect was this sibling leg.
- **The swizzle, not the scalar path, was the tensor-core GEMM bottleneck — pad As
  and ldmatrix wins +54.8%.** dot habu-mma-larger-bk (lib/ptx/cg-mma.f). The prior
  design note concluded the MMA kernel was "NOT fragment-feed-bound" because dropping
  cvt (mode 1) and ldmatrix (mode 2) were flat/1% slower at BK=32. That measured the
  UNPADDED ldmatrix, whose 16 fragment-row addresses (As row stride 128 B = 32 words)
  all alias bank 0 — a 16-way shared conflict that serialized the load and hid the
  tensor-core win. Padding the As row to a bank-spread stride (MMA-PAD=8 floats, kept a
  multiple of 4 so cp.async's 16 B chunks stay aligned) made mode-2 ldmatrix jump from
  394 to 612 GFLOP/s at 2048^3 (408 MHz), +54.8% over the scalar baseline (398.5) and
  past the FP32 CUDA-core kernel (441.8). Larger BK (64, needs dynamic .shared for
  stages=2 double-buffering past the 48 KiB static cap) added only +0.8% on top — the
  swizzle is the lever, BK the garnish. Lesson: measure an optimization at the layout
  it needs (ldmatrix wants a bank-free tile), not on top of a conflicting one, before
  concluding a path "doesn't help." Element-exactness (tools/ptx/mma-gemm-check.f) was
  the safety net that let the padded-address rewrite be trusted.
- **A shared emitter word feeds two callers — parameterize with a byte-identical
  default, prove it, don't fork.** cg-mma.f's MMA-SETUP/MMA-KTILE are reused verbatim by
  the FENCED maki/lower-mm.f LMM-MMA-BODY, so the BK/pad/stages knobs had to default to
  values that re-emit the old PTX byte-for-byte (verified by dumping all three fragment
  modes before/after and diffing). shl-vs-mul on the As stride was the trap: emit shl
  when the stride is a power of two (byte-identical at 128) and mul.lo otherwise.
- **A "pure codegen representation" can be upgraded to a checker-PROVEN capability
  additively, by wrapping the SAME emit words in new nominal type families — no
  codegen edit, no legacy-caller churn.** dot habu-ptx-m10-vectorization (lib/ptx/
  tile-v4a.f). tile-v4.f's `-V4` words kept the scalar `tile<t,b,m>` surface ("v4-ness
  is a pure codegen detail, typed alignment proofs remain dotted"), and are consumed
  across maki's fusion codegen + many tools. M10's typed layer (a nominal `vspan`/
  `vtile`, the 16B-alignment obligation, the n-mod-4 masked tail) was delivered as a
  SECOND vocabulary (`V4-ALIGN`/`LOAD.V4`/`STORE.V4`, dot-spelled to distinguish from
  the hyphenated `-V4`) whose TRUSTED bodies REUSE the existing cg-vec.f emit words
  verbatim — so the typed kernel lowers to BYTE-IDENTICAL PTX (proven in-process with
  PTX-CAPTURE + T-STR=). That byte-identity is the device-correctness bridge: the typed
  path inherits the already-passing saxpy-v4-tail device golden (n=4,5,7,1000003, body +
  @%p tail) for free, and the emit/legacy callers (maki fusion, tools) stay untouched.
  Retiring the legacy `-V4` path is a separate migration dot, not this one.
- **Two nominal type families (`vspan`/`vtile`) beat a 4th span parameter for an
  alignment refinement — and the obligation belongs on the memory op, not the ctx.**
  A `span<...,align-16>` 4-arity span would break every existing arity-3 `span` site;
  instead register arity-3 `vspan` (16B-proven global span) + `vtile` (vec4 lane tile)
  as purely-additive TFAM rows (like `acc`; needs one `install --force` fixpoint rebuild
  to bake into bin/hb — a library-only change does not). `V4-ALIGN ( span -- vspan )` is
  the sole route to a vspan (trusted 16B assertion, identity emit, like MK-SPAN asserts
  extent); `vtile` distinct from `tile` makes "store a vec4 tile through scalar STORE" a
  fail-closed type error. Put the vspan requirement on LOAD.V4/STORE.V4 (where the
  ld/st.global.v4 actually touches memory), NOT on the ctx derivation — GRID-CTX.V4
  takes a plain span, so the misaligned-base negative rejects exactly AT `LOAD.V4`
  naming `vspan`, the cleanest diagnostic.
- **New TRUSTED: words need rows in BOTH TRUSTED.md sections, owned by a LIVE dot —
  never the implementing dot (it closes).** trust-lint checks the markdown effect table
  AND the site-registry (`file:name class owner`); a missing row fails the gate. Reuse
  the sibling's `stdlib-boundary` placeholder owner (habu-ptx-phantom-preserving) that
  every ptx stdlib boundary already uses, because "owner must exist in .dots/" and the
  m10 dot is removed at closure. Audit date = today, effect string = the source
  signature verbatim, or trust-lint drift rejects.
- **ncu on the Orin NX likely hard-hung the box on first attach — treat Nsight
  Compute as a device-risk operation.** dot habu-close-mma-gemm (profile-first lane).
  Timeline: fresh bootstrap OK (07:49), one clean MMM single-launch via the new
  tools/ptx/mma-profile.f harness OK (07:52), then the FIRST `sudo ncu -k MMM
  --launch-count 1 --clock-control none <sections>` run printed only "==PROF==
  Connected to process" and never produced a section; within minutes zed dropped
  off the tailnet (tailscale `tx N rx 0`, ssh/ping timeouts, 18+ min, no watchdog
  reboot) — a physical power cycle is required. HYPOTHESIS (strong circumstantial,
  unconfirmed): ncu's perfmon interception wedged the GPU/driver on this
  L4T 5.15.148 + ncu 2024.3.1 combo. Next lane: after power-cycling, prefer (a)
  `nsys` GPU-metrics sampling (less invasive) or (b) the harness's variant-kernel
  timing decomposition over ncu; if trying ncu again, use a minimal single section
  (`--section SpeedOfLight`), a tiny shape (256), and expect to lose the box.
  The one-launch profiling harness (tools/ptx/mma-profile.f, config-driven
  `-- BK PAD STAGES DYN MODE SHAPE`) is host-gate-clean and device-proven; the
  actual profile evidence is still missing.
- **Engine-hook migration off raw `@ execute` cells is doubly gated — natively by
  in-file ordering, and hard-blocked by the stage0 bootstrap mirror.** (dot
  habu-migrate-engine-hooks stage 2.) Native: `defer` needs `DEFER-UNSET`
  (exec-vector.f — now loaded before checker.f) AND the checker's `trust`
  (checker.f ~7706) + `checker-defer` record words already in the dictionary
  (C-DEFER LFINDs them and dies rc 0x46 printing the token), so inside checker.f
  only post-TRUST hooks can become defers; a prefix `variable` gets NO checker
  usig row (the engine registers colon/defer rows, not variables), so a checked
  installer word cannot read a flag variable — set flags at top level like the
  old `' X CELL !` installs. Bootstrap: bootstrap/cg/forth.fs has NO deferred
  words ("deferred-word state is still absent") and the stage0-generated engine
  re-reads the boot prefix at startup, so ANY `defer` in a prefix file breaks
  no-binary recovery (proven: full defer migration was native-green through
  fixpoint x2 + run.f, but test/bootstrap-wide-memory.fs failed; fixture green
  on native bin/hb). TYPED-VARIABLE xt<E> cells are stage0-compatible (`[:` is
  mirrored) but the definer lives in layout-buffer.f (prefix 547), after the
  hook-cell files (540-546). Prereq for stage 2: mirror defer/is into
  bootstrap/cg/forth.fs (or make the typed-cell definer available pre-checker).
- **bin/hb is a THIN binary: it re-reads the src prefix at every startup.**
  Source edits take effect on the next run without `-- install`; an opaque
  startup death printing one token is usually a prefix-order failure (the token
  is the current parse point). A broken `-- install` bakes a broken engine —
  restore bin/hb from jj, not just the source. The prefix load order lives in
  SEVEN synchronized places: habu2.f x3 PFX tables, bootstrap/cg/forth.fs x3,
  tools/bootstrap.sh (emit_src + SRC_COMMON), tools/build-fixpoint.f
  (CHECKER-BOOT / COMMON / SNAP-KEEP), tools/boot-pin.f, tools/diagnose-hb-core.f,
  plus pinned row counts in tools/bootstrap-codegen-test.f.
- **Adding a scratch register to a shared native helper must use a register that
  helper ALREADY clobbers — a sibling caller may hold live state in an
  "untouched" register across the call.** dot habu-cap-native-control-a5669829
  (LCFPUSH control-flow-depth cap, the opposite-direction sibling of the LCFPOP
  orphan-underflow guard). The overflow itself was real and reproduced exactly at
  the region edge: CFSTK is [CFSTK-OFF, DICT-SIZE) = 4096 B = one depth cell +
  floor((4096-8)/24)=170 24-byte records; a watch on `dbase@ DICT-SIZE + @` showed
  N=170 leaves the first code word intact and N=171 (depth-170 push) overwrites it
  0..→0, spilling into the JIT code area (checker-ON it SIGABRTs at ~N>=210 once
  the corrupted code runs; checker-OFF it is silent because DEEP is never
  executed). First guard attempt loaded the cap into x14 — LCFPUSH never touched
  x14, but J-ELSE does `14 9 0 ADDI … C-PUSHCP … 9 14 0 ADDI` to carry the IF
  branch origin across the push, so `mov x14,#cap` clobbered it and LPAT
  dereferenced the cap value (lldb: faulting `ldr w11,[x9]`, x9=0xaa=170). Fix:
  use x12, which LCFPUSH already overwrites for record math (reloaded on the next
  line) — provably dead, no caller can rely on it. Lesson: before picking a
  scratch reg in a BL-called emitter, grep every caller for save/restore of
  "unused" registers across the call, or reuse one the callee already clobbers.
  Also: `data-base` (x20/DATA) != `dbase@` (x26/DBASE) — watch CF-stack memory
  through DBASE, not DATA, or the probe reads the wrong region.
- **Dot frontmatter `blocks:` means BLOCKED-BY (this dot's dependencies), not
  "blocks these" — and `dot ready` honors only recorded edges, so prose
  references never gate dispatch.** Proven by exclusion: cross-check-remapped
  lists validate-canonical-src under `blocks:` and is absent from `dot ready`
  while the listed dot appears. Two premature dispatches followed: the
  quote-diagnostic-paths and validate-canonical-src dots both target files
  (tools/source-compose.f, tools/diag-remap.f, tools/source-map.f) that no
  landed lane has created; `dot ready` offered them because nobody recorded the
  edges. Dispatch protocol now: before claiming a ready dot, verify its Files:
  exist on master (rg/jj file list — one command), and when a lane reports
  premise-missing, record the blocked-by edges on the tracker immediately so
  the ready list stays truthful.
- **Adding a stage0 keyword to bootstrap/cg/forth.fs needs THREE coordinated
  edits — `variable LKWxxx`, the keyword bytes in EMIT-KWDATA, AND the fresh
  `LBL LKWxxx !` in the label-init block — and missing the third is nearly
  undiagnosable from the symptom.** (deferis lane, dot habu-mirror-defer-is.)
  An unassigned label variable stays 0; binding label 0 corrupts the label
  table, and the failure surfaces only as `hb: snapshot trailer corrupt`
  (exit 79) at generated-engine startup — nowhere near the real cause. When a
  stage0-generated engine dies with a trailer/snapshot diagnostic right after
  a forth.fs keyword addition, audit the label-init block first.
- **A diff that adds TRUSTED:/TRUST sites must gate on tools/
  trusted-inventory-test.f, not just trust-lint — their corpora differ.**
  (deferis integration.) The wide-memory defer fixture added three TRUSTED:
  test words; trust-lint stayed at 0 findings (it checks the TRUSTED.md
  manifest/effect tables) while trusted-inventory-test failed under run.f
  (expected 0 unclassified, got 3): every TRUSTED site repo-wide, tests
  included, needs a site-registry row (class + live-dot-or-cap owner). The
  quick-gate set for TRUSTED-touching diffs is trust-lint AND
  trusted-inventory-test.
- **In this engine the checker hook is the compile-time cost center at ~30:1 —
  measure against it before minting any dispatch/lookup perf dot.** (kwfold
  lane, closed habu-fold-compile-keyword.) Checker-ON compile is 5.14us/token
  vs 0.16us/token checker-OFF on a 500k-token worst-case stream; the entire
  parse + ~69-compare keyword/op chain + LFIND + emit path is ~3% of
  checker-on cost, so folding the chain into one hash lookup had a <2%
  ceiling against multi-day seven-mirror fixpoint surgery. The honest move
  was measuring first and closing the dot; compile-latency work belongs in
  single-pass checking (habu-single-pass-checking-aabfb874, evidence copied
  there).
- **Replacing hardcoded codegen text with a data-driven record: seed the
  default record with the exact historical shape, then prove byte-identity by
  sweeping every emitter's stdout.** The ptx scaffolding ABI existed FOUR times
  (CG-ENTRY's entry string, CG-PARAMS' ld.param lines, CG-RESET's register
  seeds 2/3/2, cuda-launch's offsets 0/8/16/20/total 24/block 256) and a dozen
  producers (relu/exp/acc/ops/...) intentionally reuse the SAXPY entry+layout
  so one launcher serves them all — so the KABI record's default HAD to be the
  SAXPY shape or every legacy producer's PTX would shift. Proof method that
  made the refactor safe: before editing, capture stdout goldens for ALL
  tools/ptx/*-cg.f plus a probe hitting bench-only paths (EMIT-MATMUL-MMA in
  each lmode and every GB-MMM-CFG row, EMIT-GELU, EMIT-MATMUL/naive), then
  re-run and cmp after — 27 outputs, zero diffs, no reliance on eyeballs.
  Also: `bin/hb file.f` without `--load` drops into the REPL on open stdin and
  hangs a harness; always `bin/hb --load ... < /dev/null` in capture loops.
- **A TRUST row on a `:` word that CHECK!-certifies is redundant by
  construction — the build's certify pass (verify-source VERIFY-SOURCE) already
  runs CHECK! on every `:` body and throws on reject AND uncheckable.** (btrust
  lane, habu1 batch: 40 of 41 rows deleted with zero code changes, repo TRUST
  398→358.) Before hand-converting any TRUST batch, try mass-removal + rebuild
  first; only rows on non-`:` forms (variables needing cell refinements, real
  machine-code boundaries) can be load-bearing. Expect benign binary drift:
  certification-layout shifts move the baked AOT-REPL data-address immediates
  (MOVZ/MOVK chains EM-SEED-AOT re-relocates at boot); the fixpoint gate is
  x2 self-reproduction, not a frozen sha vs the pre-change baseline.
- **DDC byte-identity must be measured at the FIXPOINT, not the raw bootstrap
  seed.** (ddc lane.) The gforth CHECK_ONLY seed carries dead host-dependent
  movz/movk AOT-REPL address immediates (542 __text + 191 signature bytes)
  that EM-SEED-AOT re-relocates at boot; the native fixpoint refresh
  re-captures the AOT blob from the canonical small engine and erases them, so
  gforth-seed→refresh == native fixpoint byte-for-byte (verified d0db5fe3).
  The earlier fix spec (AOT-capture canonicalization) compared the wrong
  artifact — no engine change was needed; tools/ddc-verify.f encodes the
  correct chain.
- **Device goldens for hand-built (non-capturable) IR: feed the child driver
  via a shared checked source-text builder that the parent also EVALUATEs.**
  (rx1fold lane.) One source of truth for both legs; the off-device load
  already validates the child text (the EVALUATE build must succeed), so the
  only thing pending device time is the compare itself — stronger than the
  MODEL-text duplication in older device tests. Also a dispatch-protocol
  reminder: read the dot BODY (landed-leg/residual annotations), not just the
  Desc head — this dot's host leg had landed 8 days before dispatch and the
  annotation said so.
- **`cp` over a LIVE bin/hb poisons the macOS AMFI cache for that vnode — the
  symptom is exit 137 (SIGKILL) for that path only, with the identical bytes
  running fine from any fresh path.** (relreach integration incident.) A worker
  cp'd a seed binary over the shared checkout's bin/hb while gates were exec'ing
  it; execs racing the truncate+rewrite saw a torn binary, AMFI cached the vnode
  invalid, and every later exec of even VALID bytes at that path was killed —
  including the fixpoint installer's own refresh child (E-BUILD-STATUS -2802),
  and plausibly several phases of an in-flight run.f on a different tree.
  Discriminator: copy the binary to a fresh path and exec (docs/debugging.md
  already documents the AMFI gotcha). Cure: replace the file via a NEW inode
  (rm + cp, or write-temp + rename — which is why the installer itself never
  triggers this). Rules reinforced: workers NEVER write the shared checkout
  (bin/hb included) — seed copies flow orchestrator->workspace only; and a
  multi-phase red on a shared box is suspect if any agent touched bin/hb
  mid-run — re-verify on an isolated workspace before RCA'ing the tree.
- **The checker-defer bridge needs BOTH engine→checker calls — C-CALL-TRUST-PEND
  (the usig row) AND C-CALL-CHECKER-DEFER (the defer flag): checker.f IS-TOK
  requires both before a checked `is NAME` certifies.** (chkdefer lane, stage0
  mirror.) Mirroring one without the other still rejects with the same
  'non-certified definition ... at is' diagnostic. Mirror-side gotchas: define
  C-CALL-CHECKER-DEFER after C-P2-FIND-GLOBAL (gforth forward reference), and
  load x12 = PEND-CELL before C-PUSH-DREC-NAME (the mirror variant reads the
  name from the dictionary record, unlike native's body-buffer read).
- **test/nf.fs hardcodes /tmp/nf-bin — a cross-worker AMFI + debug-session
  hazard on a shared box.** (chkdefer lane.) Overwriting it mid-lldb got the
  binary AMFI-killed and risked another lane's session. Reproduce the
  wide-memory gate at a private path (require forth.fs by absolute path,
  FORTH-EXE to a scratch file, same src + 'ok' assert); run the verbatim gate
  only when /tmp/nf-bin is free (lsof first). Parameterizing nf.fs's paths is
  queued work if the collision recurs.
- **The native message-table diagnostic needs the same three-edit rule as the
  stage0 mirror: variable + EMIT-TRAPH message bytes + EMIT-LABELS label-init
  — and the native build fails CLOSED on the omission ('icode: label
  redefined'), unlike the mirror's exit-79 trailer corruption.** (mmapdiag
  lane.) Also the boot-diagnostic safety pattern: message bytes in the
  loader-mapped __text section are writable to fd 2 before ANY runtime region
  exists; ADRD32 fail-closes ADR reach so the pattern survives text growth.
- **A die-class default-is body must locals-consume its declared inputs or
  the fixpoint certify pass rejects it (E-BUILD-CERTIFY at 'die'), even though
  the engine boots green.** (stage2b clean-4 lane, TF-SHA16-UNSET.) Fresh boot
  loads prefix files before the check hook installs, so only the stage certify
  sees such bodies — certify is the authority and stays fail-closed. Bind with
  typed locals ({: a:ptr u:n dst:ptr :}) before the die, per the LBUF-EVAL
  precedent.
- **Recovery `BPROTWIDADD` was registered with the leaf wrapper despite emitting
  `BL`.** The nested protected-WID query replaced `x30`, so the raw bootstrap
  primitive could not return; `FPRIM` now matches native and preserves the caller.
- **Run proof gates as standalone commands before VCS mutation.** A failed gate
  followed by commit/push commands in one shell invocation can leave a successful
  final exit status; inspect the gate result first, then commit in a later command.
- **Pre-trust defers land via a pending table drained after `: TRUST`, not by
  reordering the checker.** (dot habu-engine-pre-trust-77410827.) Native C-DEFER
  unconditionally ran C-CALL-TRUST-PEND/C-CALL-CHECKER-DEFER, which LFIND
  `trust`(checker.f:7687)/`checker-defer`(5208) and exit 70 — so any `defer` before
  line 7687 (the B5 decls, the render/snapshot hook cells) killed boot. Fix
  (option a): C-DEFER branches on C-PRETRUST-READY? (non-dying finds of both); if
  absent it COPIES the defer's qualified name (via C-PUSH-DREC-NAME) + effect sig
  (TSIG-A/U-CELL, which point into the transient source input) into a fixed
  engine-DATA table, and DRAIN-PRETRUST (an FPRIM baked in habu2.f + the
  forth.fs mirror, `PRIM:`-axiomed like SEAL-CAPTURE) replays both registrations
  from the slot copies at a single checker.f token right after `: TRUST`. Both
  name and sig MUST be copied — the dictionary record holds only the BARE tail
  (native C-PUSH-DREC-NAME deliberately reads the QUALIFIED body-buffer spelling to
  avoid the qualified-defs-leak bug), and the sig is never in the record. Fail
  closed: overflow at declaration (C-PD-DIE-FULL, exit 72) and a non-empty table at
  SEAL-CAPTURE (BSEALCAP backstop, exit 73), both named. The pending band sits at
  the TOP of the reserved region and bumps DATA-START (protected-WID growth
  precedent) so no existing offset moves; because PD-TABLE-OFF ($8000) exceeds the
  DATA-relative scaled-immediate range ($7FF8), slot access computes an explicit
  band base (`reg PD-TABLE-OFF LIT64, reg DATA reg ADD`) then small in-slot offsets.
- **`LQNL` (the newline label) lives in habu2.f, so habu1.f code (BSEALCAP) cannot
  ADR it — emit the newline as an inline `9 $0A LIT64` byte instead.** The build
  fails at certify (`E-UNDEFINED ... in bsealcap: undefined word 'LQNL'`), not at
  assembly. Any habu1.f die that wants a fixed message must build it self-contained
  (LIT64-packed bytes on SP, like habu2.f's E-UNDEFINED die), not reference a
  habu2.f message-table label.
- **Adding a native `TRUST` asm site needs THREE ratchet updates, not one:** a
  TRUSTED.md manifest table row (trust-lint), and — because `src/habu/habu2.f` is a
  coarse `builder-emit` file-level fold — bumping that fold's count in the
  `trusted-inventory-classes` block (trusted-inventory-test RATCHET-BAD#, mapped to
  assertion 12 `TINV:RATCHET-BAD# 0 T=`). trust-lint and trusted-inventory-test
  have DIFFERENT corpora; a diff that passes one can fail the other.
- **The earliest prefix point a `defer` is legal is AFTER exec-vector.f (prefix
  pos 5), which defines DEFER-UNSET — not util.f (pos 1).** A defer in an earlier
  file dies 70 at C-DEFER-FIND-UNSET. Relevant when injecting fixtures: the
  pre-trust-defer overflow regression appends its defers to exec-vector.f.
- **A negative regression for a prefix-load die spawns a child engine with CWD set
  to a copied+patched src tree** (prefix paths are CWD-relative `src/...`; the
  child engine path must be ABSOLUTE since its CWD is the temp root — resolve via
  `s" PWD" GETENV`). Blank exactly the intended prefix region by wrapping it in
  unique sentinel comments; scanning for a bare token name blanks the wrong span
  because the name also appears in a `PRIM:` axiom / comment elsewhere in the
  file. Injected fixtures: the earliest legal `defer` host is exec-vector.f, and
  source appended AFTER check-hook.f compiles CHECKED (the hook installs at its
  load) — use that to prove checked `is`/certify behavior in a child tree.
- **A new engine prim CALLED from the boot prefix breaks every existing binary in
  the ecosystem — the prefix is re-read by old engines, and a bare new-prim token
  is E-UNDEFINED exit 70 for them.** (pretrust integration.) Gates that only boot
  the freshly-built engine cannot see this; probe with the PREVIOUS master
  fixpoint binary against the changed tree. Two facts from the probes: a
  `PRIM: NAME PRIM;` axiom line is tolerated by old engines (it only parses a
  name), and the "prims are baked so tolerance is impossible" intuition is wrong —
  a `TRUSTED:` shim that resolves the prim by RUNTIME `s" NAME" 0 search-wl`
  lookup (both words baked in old engines) boots old engines (miss -> drop) and
  new engines (find -> execute), verified end-to-end. It must be TRUSTED: because
  `search-wl` yields plain `PE-N` and RSEXEC soundly rejects execute-of-plain-n —
  own such a shim with the live stored-xt dot and state the removal condition at
  the site (pretrust: revert to the bare token at the first stage-2b pre-7687
  conversion landing, which inherently requires the new engine anyway). Pin the
  tolerance in-gate by patching the looked-up name to a miss (a gate cannot
  depend on a historical fixpoint binary).
- **A new engine PRIM registered in the stage0 mirror MUST have a dictionary name
  <= DNAME-INL (16 bytes) — the recovery mirror only emits INLINE prim names.** The
  first draft named the drain prim `DRAIN-PRE-TRUST-DEFERS` (22). The NATIVE build
  handled it (native prims like `spawn-argv-env-cwd-io`=21 exist), but the
  gforth-mirror `FORTH-EXE` build wedged: `EMIT-FORTH` (emission) finished fine, but
  `EMIT-EXE`→`BUILD-MACHO` spun forever in a gforth `EXC_BAD_ACCESS address=-16`
  signal/longjmp loop (0% forward progress, no nf-bin, ~40 min). All mirror prims
  were <=12 chars; a >16 name is the first to need external (DNAME-EXT) storage,
  which the mirror's seed-dict/image path does not emit, so the record's name
  pointer is garbage and the fixup walk chases it. Renaming to `DRAIN-PRETRUST` (14)
  fixed it instantly. RCA method that pinned it: `sample <pid>` showed the loop was
  `_sigtramp`/`longjmp`/`segv_handler` (fault loop, not slow build); lldb
  `process handle SIGSEGV --stop` gave address -16; a clean-`master` wide-gate build
  (<45s vs 40 min) proved it was my change; step-probing `EMIT-FORTH` (depth=0, done)
  vs `EMIT-EXE` (hang) localized it to the image writer; bisecting the mirror change
  found the FPRIM reg, then the name length. The recovery mirror's external-prim-name
  gap is a real capability gap (dot it if a long prim name is ever unavoidable);
  until then, keep engine prim names <=16.
- **Engine-prefix landings get an old-binary boot gate: the PREVIOUS master
  fixpoint binary must boot the candidate tree — a commit only its own binary
  can boot strands every puller.** (pretrust integration.) The worker's gates
  structurally cannot see this (they always run the fresh binary); the
  integrator boots the ecosystem binary against the exact rebased tree. When
  a transition is inherent (a prefix that USES a new engine capability), take
  it deliberately: tolerant-shim spelling (TRUSTED: runtime search-wl lookup
  — miss=no-op on old engines, hit=execute on new; probe-proven) as a bounded
  owned migration boundary, transition + shim-revert bound to a named later
  landing. Also disproven: 'prims are baked so old engines cannot tolerate a
  new prim token' — the token never needs parsing; a runtime name lookup
  through search-wl is the tolerant spelling.
- **Checker uninstall must clear its paired compile preflight.** Native rebuilds
  begin with `0 set-check`; leaving the old callback armed makes the new checker
  copy look like a hostile replacement before the canonical prefix can reload.
- **A TRUSTED: body does not bypass sealed-store guards.** Reinstalling the
  checker with raw `!` into its protected preflight cell exits 83 after
  `0 set-check`; the engine-owned `set-preflight` primitive is the required
  mutation boundary. Token tracing plus JIT disassembly exposed the raw store.
- **A custom checker hook is a paired lifecycle, not one `set-check`.** Every
  installer must run `LOWER-CERT-HOOK:INSTALL` first so a preceding
  `0 set-check` cannot leave checked definitions armed without compile-time
  preflight; `set-preflight` itself is unsafe inside checked bodies.
- **Fail-closed diagnostics are byte contracts across both emitters.** Keep the
  fixed length inclusive of exactly one LF, route recovery through its own
  missing-preflight label, and compare the complete stderr span so a trailing
  NUL cannot hide behind substring assertions.
- **Emitter-source parity does not prove recovery runtime parity.** Run the same
  exact-byte subprocess assertion with `HABU_UNDER_TEST` bound to the private
  Gforth-built `hb-stdin`; the recovery gate must prove rc 70, empty stdout, and
  the 35-byte LF-terminated, NUL-free diagnostic before installing anything.
- **Compile preflight diagnostics must enter the canonical checker renderer.**
  Pass the reconstructed body buffer already containing the immediate token to
  `CHECK!`; a bespoke short JSON object silently drops spans, effects, return
  rows, schema validation, and repair-packet compatibility.
- **Test-harness package privacy does not extend into spawned fixtures.** Keep
  harness support short and private, but give generated child words collision-
  resistant names because each fixture loads into the engine's global scope.
- **Compile preflight must force the compiler-provided token into checker
  classification.** A dictionary immediate created while checking is off has no
  signature row, so an ordinary body scan mislabels it undefined. The dedicated
  preflight entry pins the final token as `E-UNMODELED-IMMEDIATE` and emits one
  canonical repair packet for signature-bearing and signatureless immediates.
- **Candidate gates must execute the candidate.** Runtime helpers use `GE-HB$`;
  literal `bin/hb` is reserved for explicit baseline probes. Otherwise an exact
  rebased gate can fail on a stale installed engine while never testing the
  rebuilt artifact it claims to certify.
- **Locals and package words collide case-insensitively.** A local `ba` shadows a
  private word `BA`, so `ba BA !` stores through the local value instead of the
  intended cell. Give persistent state semantic names distinct from every local,
  and use typed pointer accessors for pointer-valued cells.
- **Changing a checker failure class also changes its complete origin.** Repin
  token text, token index, and byte span together; otherwise a canonical repair
  code can identify an earlier signature token.
- **A token inside a definition body has not executed.** Lifecycle lints must
  ignore body-local disable and rearm transitions; only top-level execution
  changes the current checker state.
- **Recovery parity needs a raw-emitter runtime fixture.** Testing the final
  recovery-built `hb-stdin` exercises native stage code and can miss a defect in
  `bootstrap/cg/forth.fs`; compile and run the same fixture through `FORTH-BUILD-EXE`.
- **A no-side-effect regression needs an observable side effect.** A fragment
  that only compiles more code cannot distinguish pre-execution rejection from
  a later reject; emit a compile-time marker and assert it remains absent.
- **The swizzled TF32 mma.sync GEMM is FEED-BOUND on un-amortized B-side
  scalar shared loads, not mma-issue-bound — the earlier dependency-bound
  hypothesis is overturned by ablation.** (mmaparity attribution lane,
  918MHz-pinned variant-kernel timing decomposition; nsys GPU-metrics is
  unsupported on the Orin iGPU, so DCE-safe ablated kernel variants are the
  profiling method there.) Numbers at 2048^3: B-feed 5.04ms of 12.61ms (~40%);
  cp.async staging floor 7.48ms hidden behind the feed; A-side ldmatrix free
  (reused 4x); mma issue ~1%; quarter-B-loads ceiling proxy = 2270 GFLOP/s =
  1.20x Triton. Also: the whole +53% swizzle win was pad=8 unlocking bank-free
  ldmatrix (no-pad ldmatrix is SLOWER than baseline), and cp.async stages 1 vs
  2 measure flat — do not invest in stages until the register tile grows.
- **A wider M register tile amortized the B-side feed and took the TF32 mma.sync
  GEMM PAST Triton parity: 2133.9 GFLOP/s at 2048^3 = 1.13x Triton (1890.5), the
  first Habu GEMM to beat it.** (dot habu-mma-amortize-the-dd182428, lib/ptx/cg-mma.f
  `MMA-MFRAGS` knob.) The feed-bound attribution said each 8x8 B fragment fed exactly
  one mma (zero reuse, ~40% of iteration). Fix = each warp owns MFRAGS stacked 16-row
  M-fragments; per K-substep it loads MFRAGS A fragments (ldmatrix, ~free) and then,
  per n-tile, loads its B fragment ONCE and issues MFRAGS mma against it -> B fragment
  reused MFRAGS times. MFRAGS=2 also grows the block to 128x64 (BM=64*MFRAGS), which
  HALVES global B staging too. Result at 918 MHz vs the shipped swizzled best 1369.6:
  +55.7% (94% of the 2270 quarter-B ceiling); 32 f32 accumulators/lane, 57344 B
  double-buffered dynamic .shared, 2 blocks/SM. Method notes that paid off: (1) gate
  every new behavior behind `MFRAGS>1` so all 15 pinned MFRAGS=1 configs stay
  BYTE-IDENTICAL (captured goldens + cmp before/after) - the wider tile reused the
  device-proven m16n8k8 fragment layout at +f*16-row base offsets, so NO new
  fragment-layout proof was needed, only the full-kernel element-exact golden at
  128^3/256^3 (ldmatrix AND scalar+cvt cross-check, same C[0][0]). (2) stages 1-vs-2
  flipped from FLAT (narrow tile) to +2.4% (wide tile): amortizing the feed re-exposes
  the cp.async floor, so the "don't invest in stages" rule is tile-size-dependent -
  re-measure knobs after the tile changes. (3) The harness must be block-M-aware
  (gridY = M/(64*MFRAGS), shapes multiple of 128, As/Bs staged with independent chunk
  counts since BM!=BN) - a 64^3 check on a 128-row block launches zero blocks and
  silently "passes" all-zero unless the shape is raised.
- **The stage0 mirror's 16-byte inline prim-name cap is now fail-closed at
  registration** (bootstrap/cg/forth.fs PRIM-INL-CAP? in REG-PRIM, dot
  habu-mirror-emit-external-73e98647): a >16-char FPRIM used to overflow the
  fixed EMIT-DICT record and wedge gforth's BUILD-MACHO fixup walk in a ~40-min
  EXC_BAD_ACCESS loop; it now dies in 0.12s with "stage0: prim name exceeds
  inline cap: <name>". Keep prim names <=16 chars (native handles longer via
  DNAME-EXT, the mirror does not); full DNAME-EXT mirror parity was assessed
  and deliberately NOT built - a subtly-wrong external-name seed would be
  masked at the DDC fixpoint because the native refresh re-emits the dict,
  so proving it needs bespoke pre-refresh verification no gate performs today.
- **Keep a permanent emitted-engine region map; historical RCA goes stale.**
  The July cold-prefix duplication was already fixed (`main/startup` fell from
  40724 to 5272 bytes), while later growth moved into definition dispatch,
  compile dispatch, primitives, and the AOT REPL seed. Exact `ASM-LEN` markers
  emit no target bytes, and the fixpoint driver must forward the measurement
  environment so the final stdin engine reports its own map. A mutable exact
  size baseline is only a ratchet; pair it with an immutable architectural
  ceiling so a baseline update cannot normalize growth.
- **Prove emitter branch sharing with a runtime truth table before rewriting
  layout.** An emitter's forward label names the code skipped to, not the body
  adjacent to the conditional. The hooked publish dispatch selects the shared
  tail when `HOOK-CELL=0` or `TSIG-U-CELL=0`; inverting the first condition
  broke the refresh child and the native fixpoint rejected it. Record every
  condition combination, then map each original path to the shared label.
- **Pin generation-sensitive pool fixtures before asserting exact rows.** A
  standalone test inherits generation `0`, while the same test inside the full
  gate inherits its pool slot. Save the caller generation, install a fixture
  generation, assert parent and child rows against it, then restore the caller.
- **Measure one-shot attribution immediately around the target process.** An
  outer pool label is the fallback owner, so later same-path execs can share the
  same owner and path. Snapshot the row count before and after the target spawn,
  then separately assert that the failed one-shot owner never appears.
- **A faster gate is not protected until its profile budget is tightened.** A
  24-second implementation still permits the old regression when the verdict
  allows 70 seconds. Pin the new nominal and wall ceilings for persistent and
  cold-cache paths, then prove both through the real calibrated runner.
- **Profile selection must preserve explicit budget overrides in either CLI
  order.** Applying a profile after `--budget-ms` or `--wall-budget-ms` must not
  clear their user flags; cold-cache policy must honor the same flags.
- **Repository-index linters must use the shared growable source reader.** A
  fixed 128 KiB `FILEMAP.md` buffer failed immediately after independent master
  growth. `LINT-SOURCE` already provides bounded dynamic storage; prove the
  consumer with a fixture larger than the retired cap.
- **Timeout floors and performance calibration are different policies.**
  `HB_LOAD_PCT` includes a 3x structural pool-pressure floor so healthy children
  are not killed; applying it to phase ratchets silently turns nominal 8/10
  seconds into 24/30. Export measured `HB_CAL_PCT` separately for ratchets.
- **Diagnostic probes preserve failure status.** Run optional existence and
  content checks separately; a shell `|| true` hides the exact tool failure the
  investigation needs.
- **A new test's REGISTER names must not collide with another suite's fixtures.**
  `maki/test.f` runs every suite in ONE process over shared append-only registries
  (ARTIFACT/CONFIG/PRODUCER). A later suite that asserts a count DELTA over a name
  it treats as fresh (e.g. producer-test's `s" agent/search"`, `COUNT BASE-N @ 2 +`)
  breaks if an earlier suite already interned that exact name — it passes standalone
  but fails in the full gate (`expected 13 got 12`). Prefix every fixture identity
  with the test's own tag (`oblig-test/...`) per docs/forth.md § "unique test-owned
  names"; the collision is silent because REGISTER interning is a no-op, not an error.
- **Make `T-WIDTH` instantiation-correct by routing through a schema-walk, not by
  reading the declared registry width.** `TFAM-WIDTH@` assumes every family
  parameter is one cell; that is exact ONLY while parameters stay cell-kinded
  (docs §18). The instantiation-correct width substitutes each param slot by its
  arg's `T-WIDTH` while walking the variant/product schemas — the same
  substitution `SCHEMA-TERM`/`ENV-TERM!` already did for lowering-cert offsets.
  Route the arg-aware width via a hook (`TFAM-INST-WIDTH@`, type-family.f) so the
  checker layer stays a backward caller of `T-WIDTH`; the 0-hook must fall back to
  the declared width verbatim so the boot prefix is unchanged.
- **A behaviour-preserving checker source change still moves the fixpoint hash;
  x2-identical is the real invariant, not equal-to-baseline.** The checker is
  baked into the engine image, so editing `checker.f`/`type-family.f` changes the
  binary even when runtime behaviour is byte-identical for every corpus shape.
  Because item 12 rejects a layout value in a cell parameter, EVERY existing
  family arg is width 1, so arg-aware width equals the declared width everywhere —
  proved by all type/layout suites staying green and the fixpoint rebuilding
  itself byte-identically. Report the hash change vs baseline as expected, and
  keep the x2-identical proof as the stability invariant.
- **Unit-test a width/effect function on terms the sig parser would reject by
  building them directly through `MK-PARAM`.** The whole point of a groundwork
  slice is that the accept path is not open yet, so a signature like `opt<pt3>`
  (a multi-cell layout arg) rejects at the sig one-type-per-slot separator check.
  Construct the resolved `T-PARAM` term with `PARAM-SCR-N@`/`PARAM-SCR+`/`MK-PARAM`
  (TRUSTED wrappers in the registry suite) to exercise the width function alone,
  and separately pin the probe shapes as still-rejecting so the groundwork adds no
  new accepts.
- **Probe the live accept boundary with PUBLIC families before scoping parser
  work: the sig one-type-per-slot separator rejects multi-TOKEN runs, not named
  args.** Layout slice 2 was scoped as "PK-LAYOUT parser groups" against probe
  shapes like `option<off len>`; empirically a NAMED layout-family application
  (`option<pt2>`, `result<pt2,n>`, nested `option<result<n,n>>`) ALREADY parses
  and identity-checks — one resolved term per slot satisfies the separator, and
  slice 1's arg-aware width sizes the bundle — while constructing such an
  instantiation fail-closes at the constructor's single-cell payload var
  (`E-MISMATCH` at the constructor token). The previous entry's "`opt<pt3>`
  rejects at the separator" held only for its package-PRIVATE fixture families,
  which are unnameable from a top-level sig; the public/global case accepts.
  Net: the planned parser slice served only raw-run sugar (re-staged by DoR
  amendment), the bounded landing was pins + docs (TDPN1-7), and wave B migrates
  by minting small named payload products instead of anonymous runs.
- **A generated constructor WORD's effect is a fixed `( a -- fam<a> )` stored
  signature, so the multi-cell fix is NOT in TFC alone — the CALL must be
  intercepted.** Layout slice 3: `TFC-PAY-ROW` `MK-PUSH`->`PUSH-LOGICAL` fixes
  MATCH and the RAW `construct` token (their args come resolved from the
  scrutinee / declared output), but `TDPBOPT:SOME` is a normal word whose stored
  1-cell-per-param effect can't consume/produce a wide bundle. Fix: in `DO-TOK`,
  reverse the resolved sym to its variant (`SUMV-CTOR-SYM`) and, only when the
  declared output binds the family to a genuinely multi-cell arg
  (`CONSTRUCT-DECL-MULTICELL?`), route the call through the arg-aware construct
  step instead of the stored effect. Construct recovers its type args from the
  DECLARED OUTPUT (bidirectional), since a payloadless variant (`option none`)
  has no other source — the doc's "one fresh var per param" reconciles as "mint
  fresh, then bind from the expected type".
- **Expand payloads only at width>1, and gate the whole slice on arity>0.**
  `PUSH-LOGICAL` expands ANY closed non-linear layout including a W=1 enum /
  single-field product, turning a usable logical payload into a hidden field and
  breaking existing MATCH arms — expand only when `T-WIDTH > 1` (`TFC-PUSH-PAY`).
  And gate `CONSTRUCT-DECL-MULTICELL?` on `TFAM-ARITY* > 0`: an arity-0 wide sum
  (e.g. `descriptor`, width from concrete payload fields) has no TYPE ARGS and is
  the existing machinery, never this slice's parametric-arg capability.
- **A staged fail-closed pin that evaluates a failing `: NAME ;` prints the
  compile-hook "hook: non-certified definition" to STDERR — put it in a
  candidate-validation `diagnostic` suite, not a `positive` one.**
  `test/candidate-validation.f` (not in the checker write-set) hardcodes
  per-file stderr expectations; a `positive` case (type-decl-suite) must be
  stderr-clean. The lowering fail-closed pins (TDT-EVAL-CATCH -> rc 70) belong in
  type-ctor-suite (a `diagnostic` case that already permits stderr).
- **A checker-only change can't break runtime — a maki test failing on a SCRATCH
  engine is often the fresh-process-replay `bin/hb` engine-binary key.**
  `maki/cad-test.f`'s replay spawns a hardcoded child `bin/hb` whose section-7.4
  schedule key includes the ENGINE BINARY identity ("same engine binary" is a
  documented precondition). Running the PARENT from a scratch fixpoint (`hb-fix`,
  a different hash than the checkout's `bin/hb`) makes parent!=child -> key
  mismatch -> replay MISS -> two flipped asserts, with ZERO source cause. Proof:
  a byte-identical pristine scratch build (== `bin/hb`) passes; pointing the
  child at the scratch engine passes. Verify a suspected maki regression with
  parent==child before touching code.

- **A multi-cell PRODUCT / a wide value cannot be a typed local or a custom-sum
  `ok` payload; it must flow on the stack and be UNMAKEd.** Building the promotion
  typestate (maki/db/promotion.f, dot habu-v2-evidence-promotion-f8312ebe), a
  2-field `authority` product rejected `{: a:authority :}` with "unknown type
  'authority'" (a 1-field product bound fine). Same wall the maki/evidence/policy.f
  CHECK deviation names: sum variant payloads and typed locals are single-cell.
  Consequences that shaped the design: (1) sealed staged products
  (Candidate/Verified/... each `FIELD model .. FIELD tok <proof>`) RETURN directly
  and THROW named refusals (the POLICY:CHECK pattern), since they can't be a
  `VARIANT ok <stage>` payload; the typed reject is the separately-queried
  APPLIC:VERDICT sum. (2) When a transition needs TWO wide operands (SATISFY wanted
  `measured` + the 10-field policy), give the policy a single-cell projection
  (PPOLICY:BIND -> model expiry + 4 digest words) so only ONE wide product is on the
  stack at UNMAKE time - avoids deep juggling and the "no deep stacks" rule.
  Unforgeability + single-cell + free read-back are mutually exclusive under today's
  types: sealed-token products give unforgeable + free UNMAKE read-back but are
  multi-cell; arity-0 nominals + RAW>/>RAW give single-cell + unforgeable but need a
  trust site per direction. The sealed-token + throw route needs the FEWEST trust
  sites (one `-- proof` mint per stage), and `-- proof` mints are NOT mint-shaped so
  refine-lint never demands a seed-list edit (only TRUSTED.md manifest + classification
  rows), keeping the whole change inside a maki/db write-set.

- **A bare cross-package type-family reference is fragile: a second same-tail family
  in another package makes it "unknown type".** DAUTH's `SUMTYPE auth-result` compiled
  and tested fine in isolation and in its own suite, but broke maki/db/capbud-test.f
  ("in acode: unknown type 'auth-result'"): commit-store-auth-test.f's ACODE signature
  used the BARE tail `auth-result`, which had resolved only because CSTORE:auth-result
  was globally unique. Family DECLARATIONS are package-scoped (a same tail in a
  different package is accepted, and qualified refs like PROMOTE:verified are always
  fine), but a BARE reference goes ambiguous once two exist. Fix: rename the new sum
  to a unique tail (authz-result). Lesson: pick distinct tails for new public sums,
  and prefer qualified `PKG:family` in signatures across boundaries.

- **Thread a new gate leg into a widely-called word by a SIBLING entry, not an arity
  change.** The deterministic-audit dot (habu-v2-deterministic-audit-428d27c2) had to fold
  `DAUTH:AUTHORIZED-DISCHARGE` into `CSTORE:COMMIT-AUTHORIZED` as a third validate leg, but
  the agent loop + capbud/aloop suites call `COMMIT-AUTHORIZED ( txn grant ledger -- )` and
  are NOT in the write set. Changing its arity would break them. Fix: factor the existing
  body into a private `AUTHORIZED-PUBLISH` (single-sourced capability+budget publish), keep
  `COMMIT-AUTHORIZED` as a thin wrapper (byte-identical behaviour -> every existing test stays
  green), and add a SIBLING `COMMIT-DISCHARGED ( txn grant ledger obl ev authority -- )` that
  runs the discharge leg first, then delegates to the shared publish. "New parameters, not a
  one-line follow-up" = a new entry point, not a mutated signature.
- **A sum-variant PAYLOAD cannot bind as a typed local INSIDE a MATCH arm, even a single-cell
  product.** `MATCH DAUTH:authz-result ok OF {: e:OBLIG:evidence :} ...` rejected with
  "expected: oblig:evidence<> actual: oblig:evidence<>" (identical types, still a non-certified
  definition) although a 1-field product binds fine at a word's ENTRY (OBLIG:DISCHARGE does
  `{: o:obligation e:evidence :}`). Fix: factor a helper whose entry consumes the payload
  (`: RECORD-DISCHARGE-EVENT ( OBLIG:evidence -- ) {: e:OBLIG:evidence :} ...`) and call it
  bare from the arm (`ok OF RECORD-DISCHARGE-EVENT 0 ENDOF`). The MATCH-test decoders that
  work all `drop`/consume the payload straight off the stack in-arm, never bind it.
- **An arity-1 SUMTYPE needs its type ARGUMENT in a signature (`family<T>`), but its bare
  qualifier in a MATCH selector.** A decoder `: DCODE ( CSTORE:commit-discharge-result -- n )`
  failed `SGBAD-ARITY?` ("wrong arity for type family") because `commit-discharge-result` is
  `SUMTYPE ... 1`; the signature must read `( commit-discharge-result<CAD-KIND:rev-id> -- n )`
  (bare tail + arg, the commit-store-auth-test.f ACODE `auth-result<CAD-KIND:rev-id>`
  precedent) while the body's `MATCH CSTORE:commit-discharge-result` takes the qualifier with
  NO arg. Do not put the qualifier and the `<...>` arg together. Also: the generated
  constructor doubles EVERY hyphen of the tail — `commit-discharge-result` ->
  `CSTORE-COMMIT--DISCHARGE--RESULT:COMMITTED`, `verify-result` -> `AUDIT-VERIFY--RESULT:OK`,
  `event-kind` -> `AUDIT-EVENT--KIND:TXN-COMMIT`.
- **A content-CHAINED audit log must own a self-contained store, not share the occurrence
  journal.** maki/journal.f (the audit-event-id owner) is append-only occurrence identity that
  maki/db/promotion.f also appends closure descriptors to; a hash-chain over that shared
  journal would interleave non-chained records and break verification. So maki/db/audit-log.f
  keeps its OWN fixed-record array + hash chain + sequence, and only REFERENCES the landed
  identities by their cross-process content keys. Byte-stability across processes then comes
  for free: because every event carries content keys (never process-local raws), a fresh
  decoy-shifted child rebuilds a byte-identical serialized frame (the keywire-xproc pattern),
  and `STATE-DIGEST` — a pure fold over the frame bytes, touching no registry — reproduces the
  identical digest. Nondeterminism is enforced by API shape (the only marked-event constructor
  demands a captured evidence-id) plus a `VERIFY-LOG` `bad-nondeterministic` reject for a
  marked record whose capture was zeroed; there is no reachable runtime throw to leave dead.
- **maki/test.f is at the DICT-CAP word-count wall.** The monolithic gate accumulates EVERY
  suite's definitions into ONE image (no per-suite forget), peaking at `ndict@` 16284 of
  `DICT-CAP` 16384 (src/habu/layout.f) on master - only ~100 free word records. Adding a whole
  subsystem's core+test suites (~148 words for the differential-runner tensor leg) overflows it
  as `hb: dictionary full at: :` at a LATER suite (eval-device-fault-test.f), not at the new
  files. Distinguish the two capacity arms: `here`/`allot` track DATA space (the "data payload"),
  while `:`/`create`/`variable` each consume ONE `ndict` record - "dictionary full" is the ndict
  cap, so trimming `create ... allot` buffer SIZES does NOT help word-count overflow. Fix when
  it's your subsystem: keep the new suites gated STANDALONE (their own `bin/hb file-test.f`) and
  do NOT wire them into maki/test.f, or land a DICT-CAP bump in src/habu/layout.f (a precedented
  "gate-runner-support" growth, per the layout.f comment - 8192->16384 already happened).
- **A new §23.9 id-owner mint needs a refine-lint SEED entry, and the POOL refine-lint is
  stricter than standalone.** Adding `TRUSTED: RAW>SUITE-ID ( n -- CAD-KIND:suite-id )` passed
  standalone `tools/refine-lint.f` (0 findings) but the test/gate-stdlib.f POOL refine-lint
  phase went red: `NEW-MINT ... is mint-shaped but not in the refine-lint seed list`. Register
  the mint in tools/refine-lint-core.f: bump `RFL-SEED#`, add a `RFL-SEED-NAME$` case
  (`s" RAW>SUITE-ID"`) and the matching `RFL-SEED-OWNER$` owner file, mirroring the RAW>RUN-ID /
  RAW>EVIDENCE-ID rows. Only the MINT (`n -- CAD-KIND:x`) is shape-scanned; the projection
  (`CAD-KIND:x -- n`) is seed-exempt. Also add BOTH TRUSTED.md forms (the table row AND the
  `file:NAME prim-axiom` line) or trust-lint reports UNMANIFESTED.
- **Test files that reopen a SHARED package need globally-unique helper tails.** diff-runner-test.f
  and a new diff-runner-tensor-test.f both `package DIFFRUN` and both defined `SUBJ-A`/`T1`/`C1`;
  each passed standalone but maki/test.f loads them into ONE image and the second collides
  (`duplicate definition: SUBJ-A rc=78`). Prefix per-file (`T-SUB-A`, `T-TGT`, `T-ENV-A`) so the
  reopened-package tails never clash across all files that reopen it. (A test in its OWN package,
  e.g. SUITEID-TEST, is immune.)
- **hb drops to a REPL after a successful file load; pipe `< /dev/null` for probes.** A probe
  file that loads cleanly (no error) then waits on stdin and looks like a hang under `timeout`
  (rc 124). Errors still exit non-zero immediately. Run one-off measurement scripts as
  `./bin/hb probe.f < /dev/null`. (Gate/test files that call `T-REPORT`/`die` exit on their own.)
- **Float locals `:r` certify; `s>f` is the int->float primitive (engine-spelled `n`).** A
  `{: v:r :}` local binds a float fine (contrary to the ptr-local byte-only caveat), and
  `tol s>f  SCALE s>f  f/` gives a checked fixed-point float bound. For per-loop floats without
  juggling, stash in a 1-cell buffer and reuse array.f's `T-GET`/`T-SET` (proven `@`->r), not a
  bare `variable @` (whose result may not unify with `r`).

## Region growth: mprotect flip cost is linear in region size (2026-07-18)
Doubling REGION to 8 MB for dict growth (DICT-CAP 32768) regressed process
boot +41 ms (+22%) and tripped the gate-engine runtime time ratchet (10.4 s
vs MAX-MS 10000) — reproducible on a quiet host. RCA by constant bisection
(probe builds: 8 MB region + old dict constants = same cost; LPROT flip
window 4/5/8 MB = 184/203/225 ms nop boot): the cost is entirely LPROT's
full-region RW<->RX mprotect brackets, linear in the flip window. Lessons:
(1) the engine battery's ratchet catches real per-process regressions —
never bump MAX-MS to pass; (2) region growth requires narrow protection
windows FIRST (dot habu-lprot-narrow-protection-03cc8d7f), then growth
rides on top; (3) measure JIT code-area use with a chained probe file
(`bin/hb --load maki/test.f probe.f` + LATEST XREF.) — code was at 92% of
the 4 MB split, so dict-only repartition was refuted by measurement before
any build.
- **Evolve the existing registry truth in place.** A richer shared field model
  belongs in PF with its current consumers migrated atomically; a parallel arena,
  name pool, declaration seam, and duplicate roles create conflicting authority.
- **Package reflection needs package-aware checker rows.** Keep raw registry
  words as implementation detail and model the sealed public surface with
  `PPRIM:` package/tail rows; a colon-spelled `PRIM:` name loses package identity.
- **A protected package consumes protected-WID capacity.** Capacity tests must
  probe the clean child baseline, then fill the remaining rows exactly.
- **Primitive rows have two synchronized proofs.** Update the ordered effect
  manifest and the executable/no-exec census classification with every row change.
