# Lessons

Last updated: 2026-08-10

Durable, transferable rules only — "when X, do/never Y because Z", with the
specific word / path / constant / error kept. Coding standards live in
`docs/forth.md`; API details in `docs/` near their feature. Pure status
narrative ("X landed on date D") is not a lesson; the full dated campaign log
with all falsification detail is archived in `docs/archive/lessons-2026h1.md`
and in git history. One tight bullet per lesson; add a section only if none
fits.

- **`XREF-FIND` is not the engine's resolver: it sends a BARE token to the global
  wordlist.** `src/habu/xref.f` XREF-FIND resolves `PKG:TAIL` through the
  namespace record, but an unqualified token goes to wid 0 only — it does not
  walk the open package. The engine's own lookup (`src/habu/habu1.f` EMIT-FIND)
  tries the open package's PRIVATE wordlist, then its PUBLIC one, then wid 0,
  then the used publics. The two agree only at global scope, which is where
  every comment claiming XREF-FIND "is the engine's own resolver" was written.
  Probed directly: a public `PW` in an open `package PX` answers found for
  `s" PX:PW" XREF-FIND` and NOT found for `s" PW" XREF-FIND`. Building the
  staged-callee check on XREF-FIND would have refused three shipped tools —
  `tools/codegen-compare-migrated{2,3,4}.f` stage a BARE `-N` spelling while
  `package CODEGEN-CORPUS{2,3,4}` is open — so the check walks the engine's own
  order instead (`src/compiler/native/migrate.f` SPELL-START). `search-wl` is
  the right primitive for one leg of it: it is the engine's own scan and case
  fold, and it answers the record's slot 0, which IS the code start XREF-START
  reads.
- **Two facts a caller states about one thing are one fact, and the fix is to
  derive one from the other where they arrive together.** `NMIGRATE:CALLEE` took
  a callee's spelling and its entry address and cross-checked neither; every
  caller had obtained the address by resolving that spelling. Downstream checks
  could not close it — the recorded-body name check in
  `src/compiler/native/elaborate.f` reduced a qualified spelling to its bare tail
  and so could not tell `PKG-A:FOO` from `PKG-B:FOO`, and it was not reached at
  all for an address with no recorded row while the emitted CALL still branched
  there. Measured on the pre-change tree: a caller written as `HOLE-B:HOLE-IN`
  compiled at `HOLE-A:HOLE-IN`'s address, migrated with rc 0, and answered 96
  where its own callee answers 27. Once the address is resolved from the
  spelling at the staging seam, the row's own name column stops doing work — it
  can only repeat the dictionary, and where it disagreed it was WRONG, because
  `EXPORT` publishes a second record over one routine's code and an alias names
  that routine as truly as its first name does. Net effect of the guard: the
  name column, `NAMED?`, `BARE-NAME$` and their buffers deleted, 91 lines out of
  the two files that held them.
- **A memoized entry point hides the scan under it from every behavioural test.**
  Replacing `SCAN-USIGS-SYM` in `src/core/checker.f` with an indexed answer, the
  order-pinning cases went through `SIG-MIN-IN` — the real entry point — and
  passed with the scan mutated to report deleted records as live. `HIDX-EFF@`
  answered every one of them from the memo, so the scan never ran. Mutation
  testing found it; the fix was a named whitebox shim onto the scan itself
  (`SCAN-USIGS-SYM` then `FEP-HIT?`/`FEP @ ER.MINI @`) alongside the entry-point
  cases. When a lookup has a cache in front of it, a test through the entry point
  proves the CACHE, and you need a second case that reaches past it.
- **An index with a rebuild safety net makes its own fast path unfalsifiable.**
  The per-symbol store indexes detect a rewind they did not perform (UEND below
  the watermark they were last exact at) and rebuild. That means deleting the
  incremental truncation repair entirely still gives correct answers — every
  "repair no-op" mutation passed. The repair is only observable as the WATERMARK
  it leaves behind, so the regression asserts, before any lookup can rebuild,
  that each index's mark is at or below its store's new end. Assert the cheap
  path's evidence, not just its answer, or the whole optimisation is untested.
- **An empty slot in a complete hash index is an ANSWER; the engine was throwing
  it away.** `src/habu/habu1.f`'s LFIND has had a dictionary hash index for a
  long time, but a probe that reached an EMPTY slot fell through to the full
  linear scan "as the authoritative fallback", so every MISS cost a whole
  dictionary walk. Inside an open package a bare global name misses twice
  (package private, then package public) before the global probe hits — two full
  scans per token, on every token the compiler reads. The index was already
  complete: LHIDXBUILD indexes the dictionary at startup, every publishing site
  increments NDICT and calls LHIDXADD in the same breath, and no entry is ever
  removed (a truncated record's slot goes stale and is skipped, so no chain is
  cut). The fix was to route the empty slot to the same miss handler the scan
  used. Measured on the compile-shaped batch with the checker off: 4.17 ms →
  0.80 ms, and growth per dictionary record 393 ns → 14 ns. When a fast path
  keeps a slow one "for safety", ask what invariant the fast path is missing —
  often it is missing none, and the fallback is only costing.
- **Make a lookup's precondition structural, not a survey of its callers.**
  Reading an empty slot as proof of absence holds only while the table covers
  every record below NDICT, and the one motion that can break that is `ndict!`
  RAISING the mark (it re-exposes records whose slots a later publication was
  free to reuse). Every caller in the tree lowers, but "I read all the callers"
  is not an invariant: BNDSET now drops the table on a raise and the linear scan
  takes over, so the lookup keeps its authority by construction. The last block
  of `test/engine-suite.f` pins it and fails on the pre-change engine.
- **Attribute a "the compiler is slow" number before optimising the compiler.**
  The same generated batch compiled with `0 set-check` costs 0.8 ms and is FLAT
  in the dictionary; with the checker on it is 20–23 ms and still grows 1.13 µs
  per record. So after the engine's lookup was fixed, the ENTIRE remaining
  growth belongs to the checker's own symbol-keyed LINEAR scans
  (`SCAN-USIGS-SYM`, `USIG-MATCH-SYM?`, `NORET-SCAN-SYM`, `SUMV-FROM-CTOR-SYM`,
  `TFAM-FIND-*`). The in-engine sampling profiler names them in one run:
  `n prof-on` … `prof-report` (`src/habu/prof.f`) attributes each 1 ms sample to
  a dictionary word and lumps the engine itself into `(other)` — exactly the
  split that decides whether the engine or the Habu on top of it owns a cost.
  Reach for it before reading assembly.
- **A qualified name used to cost a whole dictionary scan.** `PKG:WORD` resolved
  its qualifier by walking every record looking for the wordlist marker: 10.0 µs
  for a qualified token against 1.1 µs for a bare one at ndict 11354, and the
  tree contains about 70000 qualified tokens. A wordlist is an ordinary record
  carrying wid −1, so the same hash probe finds it with −1 as the wid; both now
  cost 1.05 µs. If a lookup has a special case, check whether the special case
  is really a different KIND of record or just a different key.
- **A hostile fixture can pass for the wrong reason; falsify each guard by
  deletion.** Found while adding manifest-shape fixtures to the identity parity
  gate: the "unpinned theorem" fixture passed because the END-OF-FILE check
  caught it, so the row-order guard it was written for was never tested.
  Delete each guard a fixture is supposed to test, one at a time, and confirm a
  SPECIFIC fixture goes red - otherwise you have tested a guard you did not
  mean to. Same discipline as mutation-testing the implementation, applied to
  the test suite itself.
- **A worker's honest account of what it did NOT cover is a work item, not a
  deliverable.** The first control-flow model reported clearly that it covered
  "branches, `begin` loops, early return and quotation application" but not
  `?do`/`loop`, `case`, `match`, `throw`, `die`, `leave`, locals or linear
  types — and named two places where it was more permissive than the checker
  (`Q>XDEAD` quotation deadness at checker.f:2018/2051, and the 33-frame
  `UNCK` bound at checker.f:7652). Relaying that candour as if the candour
  discharged the obligation is a failure of review. Accurate self-assessment
  earns a follow-up dot and a re-dispatch, never a pass. Grade against the
  goal, not against the report's honesty.
- **Model the compiler from `src/core/checker.f`, never from `docs/`.**
  Scoping the first effect model from `docs/effects.md` omitted control flow
  entirely, because that document specifies signature syntax rather than how a
  body is walked. Worse, the document is wrong where it does speak: its
  top-level return-clause grammar at `docs/effects.md:13` PARSES SILENTLY and
  means something else — `( R a -- R | S -- S a )` loads with exit 0 and the
  use site then fails with exit 70, while the real shape is
  `( Din | Rin -- Dout | Rout )`. Read the code; measure every rule against
  `bin/hb`; treat each divergence as a finding (dot
  habu-correct-effects-grammar-195dae7e).
- **A fresh Jujutsu workspace has no `bin/hb`; seed it and run the NATIVE
  refresh.** Copy a working binary into `<ws>/bin/hb`, then
  `bin/hb --load tools/build-fixpoint-refresh.f -- install` (~1 min, ends
  `bin/hb refresh OK: compiler fixpoint`). Habu bootstraps Habu.
  `tools/bootstrap.sh` is the gforth no-binary RECOVERY route only; it was
  recorded here as failing on an unrelated stage0 mirror defect
  (habu-fix-stage0-pre-88a4297e).
- **A recorded "this gate is currently red" is a measurement with a date, not a
  standing fact — re-measure before you route around it.** On 2026-08-01, on the
  protected-WID bitmap tree (macos-arm64), both documented probes of
  habu-fix-stage0-pre-88a4297e came back GREEN:
  `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh` exits 0 with `bootstrap OK: bin/hb`
  and the recovered binary is byte-identical to the native fixpoint, and the
  dot's own confirmation probe
  (`HABU_ALLOW_BOOTSTRAP=1 HABU_BOOTSTRAP_CHECK_ONLY=1`) exits 0 with zero
  occurrences of its signature line `non-certified definition: install at 'is'`.
  The bullet above had been read for days as "do not run it", which would have
  hidden a real result: the no-binary path is a live end-to-end proof again, and
  it is what proved the stage0 mirror of the protected-WID bitmap correct. Run
  the failing command yourself, on your tree, before believing a note about it.
- **Re-prove a frozen contract's package owner against the live tree at
  dispatch (`rg 'package NAME'`).** The S5a "sealed package MODEL" contract
  collided with the CAD typestate stage package `MODEL` (maki/typestate.f);
  the checkpoint red proof caught it before any code, and the right resolution
  was renaming the SMALLER side — the 8-reference internal stage vocabulary
  became `CADMODEL` — so every frozen cross-contract `MODEL:*` spelling stayed
  literal.
- **Schedule shared types and ownership primitives before their consumers.** If
  a consumer lane discovers that it needs a common record, lifetime token, or
  checked capability, park the consumer at that seam, record the dependency,
  land the common definition, and rebase the consumer onto it. A temporary
  consumer-local replica creates duplicate authority and is not parallel
  progress.
- **Consume every value returned by a mutation-free preflight.** A commit may
  repeat validation to stay safe when called directly, but it must drop or use
  the returned proof value; otherwise the declaration coordinator correctly
  rejects the participant for changing stack depth.
- **Declared growth makes reservation ownership unconditional.** Expose one
  admission path that requires a positive maximum, reserve its exact ceiling
  page count before publishing a generation-bearing handle, and make every
  boundary append consume that handle's ledger. An unreserved append fallback
  hides admission bugs and lets unrelated sequences steal promised capacity.
  Check sequence or copy-on-write ownership before physical availability; an
  owned reservation with no free page is an internal invariant failure, while
  ordinary capacity exhaustion belongs only to admission.
- **A path contract must survive every storage and transport boundary.** If a
  provider accepts `FS-PATH-CAP`, raw consumers need `FS-PATH-CAP` bytes and C
  strings need `FS-PATHZ-CAP`; validate the span before mutation and pass path
  bytes through script arguments instead of interpolating them into generated
  source, where syntax overhead and quoting would narrow or corrupt the domain.

## Checker Soundness

- **Deleting a word does not retire it; only an always-reject token row does.**
  Removing a name from the dictionary makes `CHECK` answer 1 (uncheckable), not
  0 (rejected), because an unknown token is simply unmodelled — and any later
  source can define the same spelling and get it admitted through an ordinary
  signature. Dictionary absence (`search-wl`) and the rc-70 `E-UNDEFINED` load
  failure are therefore both supplemental. The enforcing boundary is a row in
  the checker's retired-token set, consulted in `DO-TOK1` BEFORE the signature
  and primitive lookup, matched as a whole folded token so neighbouring names
  (`PF-COMMIT-N` next to retired `PF-COMMIT`) keep working. Measured on the
  product-field retirement: verdict stayed 1 after the words were deleted and
  only flipped to 0 once the rows existed.
- **Report a retired token as undefined, not as unsafe.** Reusing the
  `UNSAFE` latch would have rendered "Move this compiler or runtime boundary
  behind audited TRUST" — advice that tells the reader to do the exact thing the
  retirement forbids. Latch the retired flag for the verdict and `UNDEFERR` for
  the message, so the checker and the interpreter name the same failure.
- **A package axiom is what removes a trusted shim.** Post-hook code reaching a
  pre-hook package through `TRUSTED:` forwarders stays unaudited by the checker
  at every call site. One `PPRIM: PKG WORD ... PPRIM;` row per public word turns
  those into ordinary checked qualified calls: nine `DEV-FLD-*` trust rows
  disappeared with no replacement, and deleting a single row now fails the
  fixpoint build with "non-certified definition" instead of silently degrading.

- **The registry-cell seal (`REG-PROTECT` + `IMK-SEAL-REGISTRY`, DNAME-INT bit 63)
  only fail-closes INTERPRET dispatch and interpret `'` — compile-mode references
  are untouched (layout.f:62-64), and the checker's own word resolution never
  consults DNAME-INT.** So sealing a cold-prefix registry cell newly breaks ONLY
  top-level interpret-mode `<cell> @`/`<cell> !`/`' <cell>`; a raw cell inside a
  `:` body already resolves through compile-mode (unchanged) and a raw cell in a
  CHECKED body was already `E-UNDEFINED` before any seal (cold-prefix data records
  load before the auto-trust hook, so the checker never knew them). Migrate the
  handful of top-level raw reads to the certified accessor (`TF-STR-U@`, `TFAM-N@`,
  `SCHEMA-N@`, …) — the accessors carry `PRIM:` axioms so they stay top-level
  executable and checker-known. To seal siblings visible from `type-schema.f`
  (loads before `type-family.f`), the `REG-PROTECT` list must live in `util.f`
  (first prefix file); `internal-mark.f` reads it last. Because the whole seal is
  cold-prefix SOURCE, the current `bin/hb` recompiles it every launch — test seal
  behaviour immediately, the x2 byte-identical fixpoint is only the seed gate.
  Beware same-tail collisions: maki's `package SCHEMA` defines its OWN `SCH-N`
  (maki/schema.f:87), a different cell from core `type-schema.f` `SCH-N` — grep
  before assuming a bare read targets the core registry. The `internal-word-gate`
  subject-count ratchet (`SUBJECT-N`) must be bumped by exactly the number of new
  `IWG-EXEC:SUBJECT` fork cases, or `TAIL-RATCHET:CHECK` reds "exact subject
  child-process count".

- **`checker.f` (and `type-family.f`/`sumtype.f`/…) is a boot-time SOURCE prefix,
  not baked engine bytes.** `bin/hb` recompiles `src/core/*.f` from the working
  tree at every launch (`EMIT-COLD-PREFIX`/`PFX-LOAD-BASE-FILES`), so a
  checker-only edit changes behavior with a byte-identical binary and needs no
  rebuild to test — run fixtures against the current `bin/hb` immediately; the
  fixpoint rebuild is only for the byte-identity gate. But a NEW prefix file, or
  an engine (habu1/habu2) change, needs a rebuild and all its manifests (see
  "new baked prefix file" below). Do NOT `rm bin/hb` before an install path (it
  builds to temp then installs) — a manual rm strands you; restore from a sibling
  workspace's `bin/hb`. Prefix-internal `:`/`constant` names are treeshaken away
  and are checker-invisible to later tool sources unless a `PRIM:` row persists
  their effect; a cold-prefix `.f` sees only engine prims, core words, the curated
  public checker API, and hardcoded ABI constants.
- **The `( ... )` stack comment on a `:` IS the checked signature, and its tokens
  are TYPES, not local names.** `( got expected -- bool )` silently binds
  `got`/`expected` as fresh type vars (later `n n` op mismatches "at '<='"); write
  `( n n -- bool )`. Locals `{: got :}` may use any name; the sig may not.
- **A byte pointer is spelled `ptr u8` (two tokens), never bare `ptr`.** `ptr` is
  a constructor that consumes the next token, so `( ptr n -- )` is ONE input of
  type `ptr n`, and a bare `( -- ptr )` fails "'ptr' needs an element type". A
  word doing `c!`/`c@` on a passed buffer, or a `create`-buffer element address,
  declares `ptr u8`. A base pointer AND an integer is `( ptr a n -- )` (two
  inputs) or — cleaner — reference the `create` buffer by name (column-accessor
  style) instead of passing it as a `:ptr` local. `ptr n` (a length after the
  pointer) is `ptr n n` in an effect. The `:ptr` local shorthand reconstructs to
  `ptr u8` in the effect.
- **A `create` buffer's bare address infers `ptr a`; consume it IMMEDIATELY.**
  Pointer arithmetic on other stack items below it leaves the create address at
  its default cell-pointer type, so a later `MEM=`/`c@` rejects it as `ptr a`.
  `BYTE-COPY` the span into a fresh `create` buffer (BYTE-COPY takes any `ptr`)
  and compare buffers passed directly.
- **Cell `@` must reject byte spans.** `ptr u8` uses `c@`/`c!`; pointer-valued
  cells are modeled `ptr ptr u8` and read through `ptr-field @`. `ptr-field` is
  cell-indexed — argv/envp/DATA byte offsets use raw byte access, not
  pre-multiplied `ptr-field` indexes.
- **`close` is `( fd -- )`, no status; `wait-rc` is WEXITSTATUS-only** (a
  signal-killed child reports rc 0 — use `PROC-WAIT-RC` for 128+sig). Modeling a
  phantom result cell gets rejected at branch joins.
- **`?dup` is UNCK (uncheckable), not reject** — unmodeled, so any word using it
  is an escape hatch. `QDUP-STEP?` rejects `?dup` on a layout value; the scalar
  union effect stays dotted (`habu-model-dup-checked`). Malformed control (orphan
  closers, unterminated if/loop/quotation/case) must set `OK=0`; `UNCK` is for
  missing model coverage, not syntax imbalance.
- **A trusted span constructor must MINT a fresh rigid (skolem) extent/mask token
  per call, not reuse a nominal atom or a unification var.** Two parses of
  `extent-n` produce string-equal atoms (independent spans wrongly unify equal);
  a field type-var unifies freely (same unsoundness). `MK-SPAN` mints a per-call
  rigid token that unifies only with itself; `MK-SPAN=` stamps ONE fresh token on
  both outputs (the explicit share-one form). Kernel SIGNATURES use nominal atoms
  (`extent-r`/`extent-c`, equal-by-name, to ASSERT agreement); CONSTRUCTORS mint
  fresh. `GRID-CTX`/`ROW-CTX` mint fresh masks. This is a real checker extension
  (per-call rigid minting), not a signature convention. `TRUSTED:` bypasses CHECK,
  so genuine fresh-mask ctx mints stay legal there — that is exactly why they
  cannot be rewritten as checked callers.
- **The checker "same phantom in, same phantom out through an emitter" is already
  expressed — type-PRESERVING emitter wrappers need NO trust.** A row-polymorphic
  combinator `( a a [ n n -- n ] -- a )` (`PTXREP:REP2`/`REP1`/`REPMIX2`) makes a
  token's `n` register flow through a checked `EMIT-*` quotation while the SAME
  phantom `a` returns; forge/kind/arity soundness fall out of the existing unifier
  (forge rejects because both operands+result unify to one `a`; a wide family can't
  bind single-cell `a`; the quotation pins arity). ~23 per-op TRUSTED wrappers
  became checked callers, byte-identical PTX. Type-CHANGING wrappers (LOAD span→tile,
  GRID-CTX, STAGE, BLOCK-MAX, BROADCAST) MINT a new phantom the emitter output can't
  witness and genuinely need the mint capability — don't conflate the cheap
  forge-proof preserving case with the hard minting case.
- **The checked-MINT capability is a CHECKER SEAL, not a lib combinator; every
  lib-only mint design is unsound.** A CHECKED word may not introduce, as an
  argument of a register-phantom CELL family output, a declared type variable
  unbound in its inputs (producing a cell of input-unrelated type is a mint, sound
  only behind a `TRUSTED:` boundary the checker cannot witness). It rides
  `NP-CHECK`'s post-body parametricity seal (the `E-NONPARAMETRIC-EFFECT` choke),
  descending into family args on RAW declared-var identity so a laundering
  combinator that unified `u:=t` can't hide `u`'s sig-level absence. The seal must
  EXEMPT three checker-owned introductions, each with a distinct guard: (a) internal
  fresh vars whose `NP-LETTER` is '?' (only flag a real declared letter); (b) hidden
  physical field/layout family outputs (`NP-CELLFAM?` excludes `PARAM>HID>0`/
  `TFAM-LAYOUT?`); (c) VALUE-RECORD field accessors (the seal steps aside when an
  input consumes a `field<>` record). Each false positive costs a fixpoint rebuild
  to diagnose — build the SGIN-term-tag probe before guessing. Mint combinators are
  NOMINALLY PINNED (span/tile/gridctx/… are distinct TK-CELL families, no family
  variable), so two wrappers share one only if their full (input families, output
  family, quotation arity) coincide; loosening to a family variable reopens the
  projection forge — don't.
- **The checker verifies a declared sig is UNIFIABLE with the body, not that it is
  PRINCIPAL** — so any polymorphic mint output can be loosened into an unaudited
  `:` word, strictly EASIER to forge than a visible TRUSTED row. That is why the
  seal above is necessary.
- **A stack-effect checker over the EMIT-TIME program cannot prove RUNTIME
  loop-carried parity/alternation.** The Forth checker verifies emit-time stack
  effects; the emitter emits a double-buffer loop body ONCE and parity lives in a
  runtime register (`xor`-flipped). "parity alternates across iterations" is out of
  scope; what IS emit-time-checkable (given a decomposed emitter) is the weaker
  SAME-BODY property (read requires `ready<p>` whose symbolic parity matches, prefetch
  writes `pending<¬p>`). Don't conflate the two. A per-iteration `cpp-pending<p>`
  mint at an in-body cp.async issue is trusted BY DESIGN (audited-mint-core class,
  same as CPPSLOT COMMIT/WAIT transitions); a checked word cannot fabricate a nominal
  family cell (`( n -- cpp-pending<p> ) 0` rejects). CPPSLOT typestate fits the
  SINGLE-buffer `MMA-PIPE-KLOOP-SINGLE` (one same-body commit→wait→read); it CANNOT
  thread the shipping double-buffered `PIPE-LOOP` (commit and wait land on different
  slots, three independent walls: byte drift, trusted-quotation ownership, out-of-write-set
  untyped-quotation callers) — the honest close there is a documented BLOCKED, not byte
  drift or a laundered ready-mint.
- **The cp.async pipeline-slot typestate is nominal state-family transitions +
  parity unification — the dynamic negatives reject with ZERO checker LOGIC (the M5
  "negative needs zero machinery" pattern).** A slot threads
  `cpp-pending<p>`→`committed<p>`→`ready<p>` (three nominal TK-CELL families over a
  symbolic parity); ordinary stack-effect unification rejects read-before-wait,
  missing-commit, double-wait, and parity mismatch with only family registrations +
  typed words + fixtures. Fixtures take their INITIAL slot state from the fixture
  SIGNATURE (a `cpp-pending<p>` input) so no ISSUE mint word is needed — the issue
  mint stays the production pipeline's existing trusted boundary, which keeps net
  trust DOWN. `bar.sync` composition is a one-clause extension: committed→ready IS a
  block barrier, so WAIT drains + fences the same block-uniform barrier as a
  tile→uniform reduction (one OR clause in `PTX-BARRIER-ROWS?`), and WAIT under
  divergent control rejects through the existing `E-DIVERGENT-BARRIER` choke.
- **M5 barrier-uniformity: VALUE uniformity was already expressed; only the CONTROL
  effect was missing.** M2 families already reject a `tile` fed where `uniform<t>`
  is wanted (the lane-varying-as-uniform negative needs zero machinery). The gap was
  block-uniform REACHABILITY: `x BLOCK-MAX` inside an `if` wrongly certified because
  bar.sync reachability is a control property no stack effect expresses. Model:
  `BLOCK-MAX`/`BLOCK-SUM` have shape `( tile<..> -- uniform<..> )` — the only sound
  way to produce a uniform from a tile is a block reduction with bar.sync — so detect
  the shape structurally, flag `CTL-BARRIER`, and reject the call when the CF stack is
  non-empty (`#CFC>0` = inside control = not proven block-uniform). Conservative but
  sound; nothing regressed.
- **`TRUSTED:` words BYPASS the CHECK finalize — attach per-word checker metadata at
  `E-ADD-EFFECT`, the one USIG choke both paths share.** Setting a checker flag in
  CHECK's finalize block silently does nothing for a `TRUSTED:` word (where all the
  collectives live), because `EM-COMPILE-PUBLISH-TRUSTED` branches past
  `EM-P2-CHECK-DEFINER`. Both `:` and `TRUSTED:` funnel through
  `USIG-ADD → E-ADD-EFFECT`; detect there (via a forward xt hook installed after
  NORET-ADD). Adding a new WF-cert flag updates the `lower-cert-base.f` constant,
  the `PPRIM: LOWER-CERT` model, the `VALIDATE-WF` flag mask, and its width
  accounting. The cert VALIDATOR bites first: the first symptom of a missing validator
  branch is `hb: malformed lowering certificate`, not a checker miss — verify a new
  flag actually FIRES on a TRUSTED subject before trusting the finalize path.
- **The native publish path re-records every SIGNED definition through `TRUST`.**
  `EM-COMPILE-PUBLISH` routes any `:` with a `( ... )` sig through
  `EM-COMPILE-PUBLISH-TRUSTED → C-CALL-TRUST-PEND → USIG-ADD`, i.e. after the hook.
  Any "reject but continue" (multi-error) mode must survive that re-parse, else an
  unparseable sig kills the whole load with `checker: bad stored signature` from the
  SECOND path, not from CHECK.
- **A qualified def's engine→checker record call must key off the QUALIFIED name,
  read from a STABLE buffer.** Every `: PKG:tail (..)` recorded twice at `;` — the
  cert under the qualified name (correct) AND a bare-global `tail` row that shadowed
  core prims and certified bare-tail calls the engine rejects. Fix: `C-PUSH-DREC-NAME`
  pushes the body buffer's first token (byte-identical to the cert path). Traps:
  `DEF-TKA` is a raw SOURCE pointer (stale across a multi-line body → boot SIGSEGV;
  the body buffer is fixed engine DATA); scratch must avoid x11 (holds the trust XT
  for the later BLR). Gforth stage0 has no packages, so its record name IS the full
  name — no mirror change. A new consumer is a bug magnet for old producers:
  instrument the sym/record store with tiny probes (`CHECKER-FIND-ACTIVE-SYM`,
  `USIG-FIND-OFF-SYM`), don't stare at the new code.
- **Duplicate package definitions must fail at publish time, in BOTH
  `C-QUALIFY-DEF` and the certified recorder.** Package namespaces concentrate
  natural names; normal defs must not silently replace earlier public/private rows.
  Explicit `TRUST` is the audited override. Private words persist across `package`
  REOPENS (one namespace across every file that reopens it), so a reopened package's
  second file colliding on a private name is `duplicate definition` — prefix the
  second file's tail distinctly. A package's private and public wordlists may hold
  the same tail; in-package bare lookup finds PRIVATE first (give internal raw
  accessors a distinct spelling like `CODE-R@` so the public handle resolves).
- **Package reopen is SCOPE; include is COMPOSITION.** Reopening `package NAME`
  resumes the same wordlists + duplicate set; `include`/source-list still owns file
  dependency order. Do not include a file merely to share a namespace. Namespace
  qualification is only a non-edge colon (`HB:COUNT` qualifies through a wordlist;
  `GE-FILES:` is an ordinary word).
- **Recoverable compile-error die sites route through ONE parametric tail
  (LCOMPILEDIE), not new error codes.** Runtime-compiler rejects that
  `NR-EXIT-GROUP`'d (dup-def $4E, colon/dict overflow $4C/$4D, package misuse,
  locals-in-quotation, does>-body reject) are now catchable inside `evaluate`. Each
  writes its diagnostic to fd 2, then `LCOMPILEDIE` (EVALD>0 → catchable throw after
  eval-frame rollback; EVALD==0 → exact `exit_group(code)`). These are positive
  sysexits-style codes that `error-code-lint` EXCLUDES and that overload across
  meanings (78=dup-def AND mmap-fail; 76=code-full AND counted-string) — NO `E-*`
  fits, keep the number as both exit and throw (like RC-REJECT=70), and do NOT copy
  a sibling em-* TRUST line by habit (a needless TRUST site trips the ratchet).
- **The checker modeling an invariant does not make the ENGINE fail-closed.** The
  native `:`-body compiles token-by-token FIRST; the post-`;` HOOK runs only after,
  so `: XI ( -- ) THEN ;` SIGBUS'd (`LCFPOP` decremented an empty CFSTK to -1 and
  dereferenced a bogus origin). Guard at the shared pop (`LCFPOP` depth-0 → named
  `LORPHAN` reject joining `LDIAGRET`), covering THEN/ELSE/REPEAT/ENDOF/LOOP/+LOOP;
  valid flow keeps depth≥1 so emitted code is byte-identical. Mirror the guard in
  `bootstrap/cg/forth.fs` as a fail-closed hard rc-70.
- **Compile-error recovery must snapshot/restore OPEN-PACKAGE scope AND the
  checker's own package scope together.** Package scope can be legitimately non-zero
  across an `evaluate`/REPL boundary, so it is a boundary SNAPSHOT/RESTORE (like
  CP/NDICT/DP), not a reset — else a failed in-package `:` leaves the package
  dangling open and later top-level defines land in it. Rolling back the ENGINE scope
  without the checker's (`CHECKER-PACKAGE-MODE/NAME/U`) introduces a desync
  (engine→global, checker still in the stale package) → spurious rc70 at the next
  checked reference; keep them in step via a `PKGRESYNC` latch drained at LMAIN. The
  tty-REPL leg of the interpret-reject tail (`LDIAGRET → LUN0`) needs the same RX
  restore (`LPROT`) every sibling leg has, or a `:`-body aborted at the tty W^X-SIGBUSes.
- **Post-check scratch read by a later pass must be keyed by a monotone sequence the
  branch joins never pop.** TFAM-12's pass-2 width-aware emitter read live locals
  widths (`LOCW`), but joins pop it and sibling arms reuse frame slots — reads died
  76 or took a sibling's width. Key the durable table by a monotone bind sequence
  (`LOCW-HW[LOCSEQ]`, finalized at `:}`, reset only in `CHECK-RESET`); host such
  pass-2 scratch in `checker.f`, not new engine DATA cells (the fixed DATA map is
  full). Layout transport is a per-TOKEN mode (`LAYOUT-XPORT`), not a per-var flag —
  set only for whole-bundle ops (dup/drop/swap/…, >r/r>/…, locals) and RESET after
  `CHECK-SCAN` before boundary coercion, else a generic output var wrongly absorbs a
  layout.
- **A cache-rewind/epoch detector belongs at the APPEND choke point, not only
  reads.** Candidate rollback + the next def's recurse-cache regrew `UEND` past the
  read-time watermark and masked the rewind, resolving a stale offset into
  overwritten bytes. `E-REC-START`/`NORET-ADD` now sync before appending. Cached
  arena offsets need a +1 encoding (offset 0 is a legal record position). A
  value-less unification trail (`TV!`/`RV!` record only `(var-id,is-row)`) undoes
  FRESH binds only; path compression must run at trial depth 0, and no `NEW` between
  TRIAL-SAVE/REST.
- **A checker-acceptance TIGHTENING breaks fixture files that only fail at LOAD
  time.** The wide-PRODUCT minimum-accounting fix made an `option<idx>` reject when
  bound to an `:n` local, turning suites red at load though no checker gate was red.
  After tightening acceptance, run the suites that LOAD tool fixtures, not only the
  checker gates; convert at the producer, never bind a sum to a scalar local.
- **Never NAME a word after a control word** (`BEGIN`, `IF`, `DO`, `START` for a
  builder-open) — a bare CALL resolves to the control word, and the error surfaces
  far downstream. Self-calls use `RECURSE`; naming the word inside its own body can
  compile the wrong target. `defer NAME ( effect )` + `[: IMPL ;] is NAME` keeps
  deferred execution checked; `is` needs a checker-visible defer target-kind
  (`tools/check.f` rejects `is` on non-defer before runtime). `create`/`variable`/
  `constant` must publish their `TRUST` effects in the native compiler when the hook
  is installed (a parent preverify can't help the child registry). Parser-word
  payloads (`[char]`/`char`, `s"`) are part of the checked body — the capture must
  append the consumed token.
- **Try a checked factor before new trust.** `FS-BYTE-OFFSET` looked primitive but
  `: BYTE+ ( ptr u8 n -- ptr u8 ) + ;` certified against existing pointer arithmetic
  and retired the trust row. Function-passing IS a checked capability: the checker
  verifies a quotation param executed through a call chain AND a `?do`/`begin` loop,
  so `SORT!` `( ptr a n [ a a -- bool ] -- )` is fully checked — don't copy
  `combinators.f`'s `0 set-check` boundary (an old unchecked boundary, not a model).
- **A `0 set-check` span may exist only because ONE primitive lacks an axiom row —
  probe before accepting it as a boundary.** Both hook-install spans disabled
  checking solely because the hook body calls `CHECK!` (unknown to the checker); a
  one-line `s" CHECK!" s" ptr u8 n -- n" TRUST` + the hook def proved the span
  retirable, turning an opaque check-off region into one audited prim-axiom TRUST
  row. Duplicate TRUST of the same primitive across files is idempotent.
- **A TRUST row on a `:` word that CHECK!-certifies is REDUNDANT by construction** —
  the build's certify pass runs CHECK! on every `:` body and throws on reject/uncheckable.
  Before hand-converting a TRUST batch, try mass-removal + rebuild first; only rows on
  non-`:` forms (variables needing cell refinements, real machine-code boundaries) are
  load-bearing (one batch: 40/41 rows deleted, repo TRUST 398→358). Expect benign
  binary drift (certification-layout shifts move the baked AOT-REPL address immediates
  EM-SEED-AOT re-relocates at boot); the fixpoint gate is x2 self-reproduction, not a
  frozen sha vs baseline. `variable`/`create` publish checker records — define fixture
  cells before mutating capacity state.
- **`create ... allot` checker arenas convert to boot+P without touching callers:**
  `create X ...` → `create X-BOOT ...` + `variable X-P` + `: X ( -- ptr a ) X-P @ ;`;
  `cells X +` sites unchanged. Growable registries that own a string pool must rebase
  on relocation: CT/VREC/SYMS hold ABSOLUTE pointers into their `*-STR` pool; grow the
  RECORD/NODE arrays to mmap BEFORE the string pool so a rebase never mutates the
  baked boot buffer; SYMS must stay a power of 2 and drop+rebuild HIDX on grow.
- **Suite-visible checker words are checked even in TEST files: bools are not n.**
  Round-trips lost to `-1`/`0` where `bool` was declared, and a render flag only the
  fixpoint CERTIFY pass caught (local stdin runs parse unchecked). The `0 0=` / `0 0= 0=`
  literal idiom is the convention; run the certify path early when adding checked
  prefix words. `test/candidate-validation.f` hardcodes per-file stderr — a fail-closed
  pin that evaluates a failing `: NAME ;` prints the hook diagnostic to stderr, so put
  it in a `diagnostic` suite, not a stderr-clean `positive` one.
- **CHECK! is line-oriented and REGISTERS what it certifies.** Feed it one
  whitespace-normalized line (multi-line body → verdict 1 regardless); after a -1
  verdict the name is registered, so a checked re-compile dies duplicate-definition —
  compile certified text under `0 set-check`, reinstall the hook right after
  (`CHK-COMPILE-CERT`). Leaving the hook off compiles later defs untyped, so every
  later CHECK! referencing them rejects — the failure appears one check DOWNSTREAM.
  `CHECK-CANDIDATE!` suppresses duplicate-name rejection only for that candidate and
  restores `USIGS`/`NORETS` after; raw-text scanners must implement engine-normalization
  parity (sig only at token index 1, once, later a skipped comment) or a mid-body
  `( ... )` clobbers SGIN/SGOUT.
- **A checker DRIVER must never EXECUTE candidate code — reuse the verify path, not
  the load path.** The all-errors CLI nearly shipped on `evaluate` (runs top-level
  candidate forms in the checking process, pollutes the live dictionary; checker-scope
  rollback restores registries, NOT cp/ndict). The crash-immune point is
  `VERIFY:SOURCE-BUF` (parse+check, zero execution). Checker.f words are NOT
  registry-published to later checked loads — checker-internal access from tools rides
  small documented `TRUSTED:` one-liners.
- **A new baked prefix file has SEVEN+ synchronized owners; miss one and recovery or
  a puller breaks.** Native `habu2.f`/fixpoint does NOT update the Gforth recovery
  compiler: mirror load/path/provide/label rows in `bootstrap/cg/forth.fs`, concatenate
  in `tools/bootstrap.sh` (SRC_COMMON), and update `build-fixpoint.f` (CHECKER-BOOT/
  COMMON/SNAP-KEEP), `boot-pin.f`, `diagnose-hb-core.f` (+ count in its test),
  `hb-build-lib.f` key list, `test/run-files.f`, and pinned row counts
  (`boot-pin-test.f` PFX-LOAD-ROW, `diagnose-hb-test.f` common-source, `bootstrap-codegen-test.f`).
  The codegen test's expected rows are the order proof, not bookkeeping: update its
  exact native, recovery, and fixpoint sequences whenever a prefix owner is added.
  Baked prefix files must be marked `provided` (else `require src/core/sha256.f`
  reloads `W32`). The first ceiling a new prefix file trips is stage2's `S2-SOURCE-CAP`.
- **A NEW engine PRIM used by boot-prefix source lands in TWO stages** (the running
  binary re-reads the prefix at its boot, so checker.f can't reference a prim the
  current engine lacks → `E-UNDEFINED` before any build): stage 1 emit+register the
  prim + its `PRIM:`/`TRUST` row (the literal token keeps it through treeshake) and
  build; stage 2 add the prefix code that CALLS it and build again. A prim CALLED
  from the prefix bricks every OLD binary in the ecosystem (the prefix is re-read by
  old engines; a bare new-prim token is `E-UNDEFINED` exit 70). A `PRIM: NAME PRIM;`
  axiom line is tolerated (parses a name only); an inherent transition takes a
  bounded tolerant shim — a `TRUSTED:` word resolving the prim by runtime
  `s" NAME" 0 search-wl` (miss→drop on old engines, find→execute on new), owned by a
  live stored-xt dot with a stated removal condition. A stage0-mirror PRIM's
  dictionary name must be ≤16 (`DNAME-INL`): the mirror only emits inline prim names;
  a >16 name wedges gforth's `BUILD-MACHO` fixup walk in a ~40-min EXC_BAD_ACCESS
  loop (now fail-closed `PRIM-INL-CAP?`). Same lands-in-two-stages rule for a
  cross-layer checker word consumed by a habu-layer file.
- **Cross-agent engine landings BRICK sibling binaries.** A new cold-prefix file
  (e.g. `engine-error.f`) makes every other agent's baked-prefix-list `bin/hb` fail
  AT BOOT with `E-UNDEFINED` on updated consumers (find the baked list with
  `strings bin/hb | rg '^(src|lib)/'`). A hybrid revert-and-refresh does NOT compose
  (mixed-generation sources fail the child build); recover via the documented gforth
  bootstrap (which is why mirroring the prefix into `forth.fs` is BLOCKING for any new
  cold-prefix file). Pull-and-refresh after every fetch that crosses an engine landing.
  A seed `bin/hb` older than the engine prefix crashes the refresh AND the gate with
  `E-UNDEFINED` + SIGABRT (rc 134) while `install --force` still exits 0 — refresh the
  main tree immediately and reseed every worker workspace.
- **Extensible nominal types must be explicit.** `DEFTYPE` registers a copied global
  nominal before signatures use it (silent auto-interning would turn typos into valid
  roles). Do not publish facts with empty trusted stubs (`HB-TARGET-LINUX?` as an
  empty `TRUSTED:` made `EM-ENTRY-ARGS` branch on stale stack data) — target
  predicates live in `src/os/<target>/target.f`; only nominal identity casts belong in
  `roles.f`. Keep `roles.f` TRUST-site rows stable (a new definer above the audited
  cast block shifts every manifest site).
- **Ground capability claims in the SOURCE, not the dot tracker + spec.** M2
  (parametric checker) and local type inference were declared "large unbuilt gates"
  from `dot ls` + the spec calling them "large"; a 10-second certify/reject probe
  through `bin/hb` refuted both (fully built+landed). A "missing dot" can mean DONE,
  not unbuilt — probe before sizing a checker feature as a prerequisite. The R7 plan's
  type-schema spellings are pseudocode; the real keywords are `TYPEFAMILY`/`PRODUCT`/
  `SUMTYPE`/`ENUM` — build the file + a `CHECK-QUIET-CANDIDATE!` probe and iterate on
  real checker output rather than transcribing a plan.
- **Diagnose with the row renderer, not by guessing.** A branch-local reject
  reproduced identically at top level (ctor output still in the CTOR-PEND
  signature-boundary window) — test seeds must be checked maker words. A new static
  gate's blast radius includes the test corpus's own metaprogramming (DNAME-WIDE
  marking fired on `' TLP-DUP` — correct enforcement); test-only xt introspection
  lives in the raw-xt TRUSTED boundary once ticking is a gate surface. Rank new
  structural rejects ABOVE the uncheckable verdict (trailing tokens soften a hard
  reject to UNCK, which multi-error loads trust differently); latch reason codes with
  the token pin, not at render time, and truncation must latch BEFORE the
  output-boundary coercion.

## Types, ADTs & Signatures

- **A zero-field STRUCTURE cannot be a product FIELD — the declaration
  fail-closes (`invalid field layout metadata`, throw 7127).** Nested
  payload-ENUM and multi-field STRUCTURE fields both work; an embedded proof
  token therefore stays an arity-0 `TYPEFAMILY` plus a private `TRUSTED:` mint
  (the ART:built / promotion shape) even under the unified-STRUCTURE
  convention (probed 2026-07-26 while building MDLCFG:mcfg).
- **Type-family tail reuse needs lexical tiers, not a declaration ban.** Family
  identity is already the exact `(package, tail)` registry key, so resolution
  must preserve that structure: the open package's exact row first, the exact
  global row second, then one non-global package-public fallback. Folding the
  global row into the public fallback made `span` and `MEM:span` spuriously
  ambiguous and led to rejecting the sound declaration. Keep duplicate checks
  on the exact key and keep multi-package fallback ambiguity fail-closed.

- **Declarable nominals: the mechanism already exists; the gap is package-scoping.**
  `DEFTYPE NAME` (roles.f) mints a CT-ROLE past CC-MAX via CT-SET, auto-derives
  `>NAME`/`NAME>N`, persists via CT-SNAPSHOT-PERSIST, and rolls back through the RBF
  frame (CTN + CT-STR-U saved/restored at checker.f:8979/9005). Declared roles unify
  byte-for-byte like built-ins: same-role accept, other-role reject, **role↔n REJECT
  both ways** (a dot's "generic-int accept" wording was wrong — probe before
  implementing). CT roles are a **global flat code space** (`CON-OF = CT-FIND`, no
  package awareness) — a second package declaring the same tail hard-dies (70);
  package-scoping is a con-resolution-path change, not "declarability". Three nominal
  substrates coexist (CT role / TFAM arity-0 / extent-atom); which one `EXTENT:`
  mints on is a design decision, not settled by declarability existing.
- **The `NOMINAL:` value-nominal surface rides TFAM arity-0, not the CT-role
  package-scoping restructure (`habu-foundation-a1b`).** Probed: a bare arity-0
  `TYPEFAMILY` tail resolves as a standalone scalar signature type with the exact
  DEFTYPE strictness (same-nominal accept; vs-`n` reject both ways; cross-nominal
  reject; converters the only crossing), AND it is package-scoped for free
  (records key on (package,tail)) — two packages both `NOMINAL: SERIAL` stay
  distinct, where two `DEFTYPE SERIAL` hard-die (70). So value nominals need **no**
  `CT-FIND`/`CON-OF` engine change: `lib/type/value-nominal.f` mints an arity-0
  family with `CHECKER-DEFFAMILY` (fails closed on dup/reserved tail: E-TFAM-DUP /
  7110, exit 67) and generates the `>NAME`/`NAME>N` identity casts through one
  audited `evaluate` (`NG-EVAL`, the roles.f `DTC-EVAL` / extent.f `XG-EVAL`
  pattern). A1b is obviated for value nominals. Keep the declarer a single global
  keyword (core language surface, like DEFTYPE/ENUM) with all helpers in a package;
  a package-public declarer would force `PKG:` qualified calls.
- **A multi-cell PRODUCT / wide value (tagged sum with payload) cannot be a typed
  LOCAL nor a polymorphic sum/`result`/`option` payload param — only single-cell
  types (arity-0 `TYPEFAMILY`, payloadless `ENUM`) can.** `{: d:content-digest :}`
  and `result<POLICY:granted,n>` both reject ("unknown type" / "expected a actual
  granted<>"). Consume wide values straight off the stack via `UNMAKE`/`MATCH`, kept
  DEEPEST so single-cell operands pop as locals first. Signal a fallible
  product-returning transition by THROWing a named code, not `result<product,_>`
  (the schema.f RESULT-DROP wall). A 1-field product binds fine at a word's ENTRY. A
  `MATCH`-arm payload also cannot bind to a typed local INSIDE the arm even single-cell
  (`ok OF {: e:evidence :}` rejects with identical expected/actual) — factor a helper
  whose ENTRY consumes the payload and call it bare from the arm. Inline
  `PKG-PROD:UNMAKE {: f:role ... :}` inside an arm certifies (the rule is only about
  binding the PRODUCT itself). A result value survives an intervening store below it.
  (Post-flat-multi-cell commit 13f8a504, a multi-field PRODUCT CAN be a sum payload
  when the signature DECLARES it concretely; only a free err var vs a nominal still bites.)
- **A typed error result over a NOMINAL type needs a bespoke sum family, not shared
  `result<a,b>`.** Constructing `RESULT:OK` in a TOTAL (ok-only) word leaves err var
  `b` free; a free var unifies with a structural type but NOT a nominal ENUM/TYPEFAMILY.
  Define `SUMTYPE foo-result 1` whose ok variant carries the payload and whose error
  members are baked-in nullary variants (the `numeric-result` idiom). An arity-1
  SUMTYPE needs its type ARGUMENT in a signature (`family<CAD-KIND:rev-id>`, "wrong
  arity for type family" otherwise) but its BARE qualifier in a `MATCH` selector — do
  not put qualifier and `<...>` together. A parameterized `result` type can't be a
  `{: :}` local — specialize the word or keep it on the stack.
- **A generated PRODUCT/SUMTYPE constructor DOUBLES every internal hyphen of the
  family and prefixes the (escaped) package.** `PRODUCT content-digest` in `package
  ARTIFACT` generates `ARTIFACT-CONTENT--DIGEST:MAKE`; a nullary variant ctor is
  `PKG-FAMILY:VARIANT`; `MATCH` arms and destructuring use the BARE variant name.
  Package-private family ctors live in a DERIVED escaped package
  (`PRODUCT pxevid` in `package PX-PROBE` → `PX--PROBE-PXEVID:MAKE`). Wrap long
  spellings in short private words. Escaped ctor names >32 chars SHA-fall-back to an
  unreadable `Thexhash-TAIL` (readability cap `TF-CTOR-NAME-LIMIT`, raised 16→32; not a
  structural bound — long names store via DNAME-EXT). Keep `len(PKG)+1+len(escaped-family)
  ≤ 32` for any family a caller must construct by name.
- **A word returning a layout value (PRODUCT/SUMTYPE) cannot be called at TOP LEVEL**
  ("interpret-mode layout value") — wrap the construct/UNMAKE/assert in one checked
  word and call THAT from top level (store the decoded handle's slot in a variable and
  rebuild the handle in each reader). Layout bundles also silently corrupt under
  interpret-mode dup/drop/swap (one physical cell moved); gate wide producers at
  interpret DISPATCH with a `DNAME-WIDE` dict bit (fail-closed, named diagnostic),
  don't type the REPL. Checked compile-mode calls of the same marked word stay legal.
- **Type-family names obey package public/private exactly like words; `EXPORT` does
  NOT apply (they are checker registry entries, no xt).** To name a family `PKG:name`
  in a cross-package signature/local, declare the `TYPEFAMILY` in the package's
  `public` section (visibility captured at declaration). A `private`-section family
  resolves only inside its own package. A BARE cross-package family reference is
  FRAGILE: a second same-tail family in another package makes it "unknown type" (it
  resolved only while globally unique) — pick distinct tails for new public sums and
  prefer qualified `PKG:family` across boundaries. Order gotcha: declare the family
  before the private mint/erase `TRUSTED:` words that reference it.
- **Package-owned CAD ids are arity-0 cell families (`TYPEFAMILY id 0`), not global
  DEFTYPEs.** That already gives package-qualified identity, typed pointer storage,
  rollback, snapshots, replay, and qualified diagnostics; `DEFTYPE` instead installs a
  global nominal + raw converter words. An unforgeable value that CARRIES data uses the
  refined-nominal-HANDLE pattern: an arity-0 `TYPEFAMILY` over an append-only pool,
  minted ONLY through a private `TRUSTED: RAW>X`/`X>RAW` pair, pool holds the
  authority (a `private` PRODUCT has no construction surface even for its owner; a
  `public` PRODUCT's generated `MAKE` accepts a raw n = forgeable-by-alias, fine for a
  pool-slot txn handle, UNSAFE for a capability grant). Generic `variable`/`create`
  storage does NOT retain a nominal pointee between definitions (each use instantiates
  the generic pointer independently, both certify) — keep raw storage private, expose
  typed accessors.
- **A product TYPE PARAMETER binds only cell-tier types (n or nominal `TYPEFAMILY`);
  a sum/enum/product family cannot instantiate it.** So a generic `comparison<a>` over
  a metric unit needs nominal-cell unit witnesses; prefer concrete per-variant families
  (`comparison-gbps`/`-gflops` with distinct readings) when variants are few — the
  number is unit-typed with ZERO trusted surface. `T-WIDTH`/`TFAM-WIDTH@` assume every
  param is one cell (exact only while params stay cell-kinded, docs §18); route
  arg-aware width through a hook (`TFAM-INST-WIDTH@`) that falls back to the declared
  width so the boot prefix is unchanged. `PUSH-LOGICAL` must expand a payload only at
  `T-WIDTH > 1` (expanding a W=1 enum/single-field product breaks existing MATCH arms)
  and gate `CONSTRUCT-DECL-MULTICELL?` on arity>0.
- **A generated constructor WORD's effect is a fixed `( a -- fam<a> )` stored sig, so
  a multi-cell fix must intercept the CALL, not just the ctor.** `MATCH` and the RAW
  `construct` token get args resolved from scrutinee/declared output, but a normal
  ctor word's stored 1-cell-per-param effect can't consume/produce a wide bundle —
  reverse the sym to its variant (`SUMV-CTOR-SYM`) and route through the arg-aware
  construct step only when the declared output binds the family to a genuinely
  multi-cell arg. Construct recovers type args from the DECLARED OUTPUT (bidirectional).
  Reuse the step machinery: construct's effect built from SUMV metadata + applied
  through ordinary `CHECKER-STEP` bought unification diagnostics + linear conservation
  for free. A reserved capture form must CONSUME its operand tokens even on failure
  (poisoned state), else they fall to word lookup and soften a hard reject to
  uncheckable-undefined. Product destructure fell out of TWO existing mechanisms
  unchanged (the k=0 pending-ctor window + the symmetric LOGHID row coercion) — read the
  unifier before designing a new coercion.
- **Layout linearity: a value that resolves linear must be rejected at its OWN
  bind/ref site, not assumed covered by DCUR/RCUR counting.** Locals bypass
  `LIN-CHECK` (`LOC-REF?` re-pushes a tv with no step; a typed linear `LOC-BIND` sets
  `LINEXP=1` and skips the check), so `{: x:own :} x x` certified while stack `dup`
  rejected. Reject binding any linear-resolving value into a local (`LIN-LOCAL-BIND-CHECK`)
  + taint poly local refs (`LIN-LOCAL-REF-TAINT`), both gated on `LIN-ANY?` so the
  no-`deflinear` self-build pays nothing. A layout PARAM hides its payload from the
  linear count (`tdlin<own>` dup/dropped freely while `own drop` rejected) — transport
  binds reject any layout whose family args resolve linear OR are still unbound. When a
  fail-closed v1 guard sits in front of a general count/conservation path, RELAXING =
  deleting the guard (TFAM-11 move-class needed zero new classification code), not
  adding op-class logic — but a guard can be over-conservative AND structurally
  load-bearing at once (the 1-cell open-arg form carries the ctor's raw→hidden
  coercion), so probe by removing it and reading what breaks.
- **A checker atom PREFIX reserves the ENTIRE lowercase `prefix-*` namespace across
  every declaration site — sweep before choosing the spelling.** A `layout-` prefix
  made maki's enum variant `layout-conflict` reserved (throw 7110 far from the cause);
  `rg '\bprefix-'` across `*.f`/`*.fs` first. In PTX signatures `r` is float and `c`
  is char, not type vars — use `e`/`k` for extents. Bare dtype tails (`f32`/`tf32`)
  are reserved atom tokens (rejected as variant names — class-prefix them); variant
  names are PACKAGE-scoped, so N same-package slot sums can't all reuse `got`/`absent`.
- **A checker-visible fact needing an engine record flag prefers a checker-owned latch
  stored BY VALUE at the single record choke (`E-ADD-EFFECT`), consumed by the publish
  tails after ndict++** — staleness is then impossible. A definer's recordable effect
  is bounded by what its runtime actually stores (`C-CONSTANT` pops one cell, so `-- a`
  is the permanent constant contract; wide values are gated at interpret DISPATCH, which
  dominates every downstream consumer — gate production, not each definer). Internal
  checker effects (literals, cell fetch/store, control words) build their rows directly
  and never parse signature strings.
- **`SPEC:` (maki/spec.f) caps a contraction at 2 free + 2 contraction indices, so
  compose multi-head attention accordingly (maki/mha.f).** The head MERGE folds into
  ONE output-projection SPEC over a composite (head, head-dim) index —
  `Y[hq hc] = O[hh hq hd] WO[hh hd hc] * +SUM hh hd` (2 free, 2 contraction, rank-3
  factors) — no concat buffer, no per-head accumulation. The head SPLIT cannot: a
  per-head projection output `O[hh hq hd]` is 3 free indices → `E-SPEC-ARITY`, so
  loop heads at the host level and bind each head's weight/activation block before a
  2-free SPEC projection. Rank-3 `TENSOR:` factors and the 2-contraction `+SUM a b`
  form both work; a swapped operand in either is still an author-time reject.
- **A full payload ENUM carries DEFLINEAR CT tokens as concrete FIELD types, and
  the bundle is then one linear unit** (WSTORE, probed then pinned in
  maki/infer/weight-store-test.f): the generated constructor consumes the linear
  payload, a MATCH arm re-introduces it and must consume or re-mint it, and
  dup/drop/store/reuse of the whole bundle reject — the same conservation the
  parametric suite proved, now through concrete named FIELDs. Two families in
  ONE package may reuse variant tails (`mapped`/`allocated` on both the policy
  and the store enum coexist). But a declaration-grammar keyword is a RESERVED
  family name: `ENUM policy … ;ENUM` throws 7110 because `POLICY` is the layout
  header clause — probe the intended tail with the real front end before
  freezing a contract spelling, and expect to rename (WSTORE spells it
  `residency`).

## Tool & Infra

- **To see the engine reject its OWN prefix source, build a warm snapshot engine
  BEFORE breaking the tree.** `bin/hb` recompiles `src/core/*.f` from the working
  tree at every launch, so a half-migrated prefix edit dies at the first
  declaration in any tool's libraries (`tfam: bad family id`, rc 76) long before
  the fixpoint's stage2 certify pass can print the checker's verdict. Build
  `$HB_TMP/hb-new` with `-- snap` while the tree is still healthy (its prefix is
  baked into the image, not reread), then run
  `hb-new --load <lib preamble> tools/build-fixpoint.f tools/build-fixpoint-main.f -- stage`
  over the broken tree: that stops right after `BF-CERTIFY-STAGE2` and prints
  `certify: stage2-src rejected rc 70` with the offending word and token. Do not
  try `tools/check.f --source-list src/core/<file>.f` for this — a prefix file
  reads sealed cold registry cells (`TFAM-N`) that a user-level check reports as
  `E-UNDEFINED`, which is a harness artifact, not the reject you are after.
- **Do not overlap a top-level gate with a second invocation of one of its own
  suites.** `test/run.f` already launches the Maki core suite; running
  `maki/test.f` beside it made both `maki/cad-test.f` processes contend through
  their repository-local replay/store fixtures even with separate `HB_TMP`
  roots, while the same focused test passed immediately once isolated. Run the
  required top-level/owning gates sequentially unless their resource ownership
  is explicitly disjoint.
- **Repo lints, the lint tokenizer, and whole-source readers all carry a
  largest-file capacity watermark, and an uncaught positive throw dies SILENT.**
  Fixed `$20000`/`$40000`/`$80000` file buffers (shadow-lint, maki-dep-lint,
  error-code-lint) and the tokenizer `TMAX` ($6000→$8000) all trip
  "file exceeds buffer" as `checker.f` grows (it is the largest). Rules:
  size caps from the real corpus with the driver NAMED in a comment; sweep EVERY READ-FILE
  cap in one pass when a named file trips one; route lint CLIs through `LINT-MAIN` (catch,
  print `tool: threw <code> (<name>)`, re-throw); the shared `LINT-READ-DIE` prints the
  offending path; never read `$?` after a pipeline.
- **Source and real consumers are authority.** Hand-maintained ledgers without a
  production consumer drift and must not gate changes.
- **A capacity exit must ATTRIBUTE itself everywhere — a lone token byte or a bare rc
  is unattributable.** Engine dict-full = `hb: dictionary full at: <token>` (77), code
  space (76), and each store labels its own die. Distinguish the two engine arms:
  `here`/`allot` track DATA space; `:`/`create`/`variable` each consume ONE `ndict`
  record — "dictionary full" is the ndict cap, so trimming buffer SIZES does NOT help
  word-count overflow. Watermarks that grow same-commit: `DICT-CAP` (8192→16384→ scaled
  with CFSTK-OFF/DICT-SIZE/HIDX-SLOTS), the boot source-prefix `IBUFSZ` (silent bare `74`,
  1M→1.5M in native + gforth mirror), the image build `MSIZE` (must be ≥ `MPAGE`; silent
  `M-BOUNDS 75`, raised to $120000), the stage2/maker source caps ($C0000). A load path
  that only ever runs SUBSETS needs an explicit whole-closure regression (the
  gate-runner-support closure hit ~9.3k dict entries against the old 8192 cap).
- **`maki/test.f` is at the DICT-CAP word-count wall** (~16284/16384 on master): it
  accumulates EVERY suite's defs into ONE image with no per-suite forget, so a new
  subsystem's suites overflow it as `hb: dictionary full at: :` at a LATER unrelated
  suite. Keep new suites gated STANDALONE (their own `bin/hb file-test.f`), or land a
  precedented `DICT-CAP` bump in `layout.f`.
- **The shared maki suite table (`lib/test/suite.f ITEM-MAX`) is a capacity wall too;
  RAISE it (loud-fail preserved), do not aggregate unrelated suites into one entry** —
  overflow throws `E-TBL-BOUNDS` (-3000) at LOAD with stdout lost. `ITEM-MAX` is only
  referenced inside suite.f (128→256, fully contained). Same FM-BUF-CAP pattern: a fixed
  cap sized for a statically-known registration set, grown by constant with the wall kept.
  The `maki-suite ITEM-MAX=128` note is stale.
- **A copied `bin/hb` is NOT a frozen baseline** — the small engine LOADS `src/core/*.f`
  from the WORKING TREE at boot, so an old binary in an edited tree exhibits the EDITED
  checker; red/green comparisons must pin the SOURCE tree state, not the binary. (Chasing
  "engine words not in any tracked source" wasted a session before one out-of-tree run
  exposed it.) A worker `.jj-ws/*` tree has no `bin/hb` (`bin/` is gitignored) — provision
  each with a COPY (never a symlink into main — a workspace rebuild would overwrite the
  binary other workers use). Stale worker workspaces hold stale baked binaries that produce
  FAKE gate reds when reused.
- **`FS-SKIP-DIR?` must skip every conventional untracked dir, and the integrator runs
  `run.f` in the MAIN checkout too.** It skipped `.jj`/`.git`/`.dots` but not `.jj-ws`, so
  every walker saw stale worker-workspace trees (478 phantom lint findings) — a
  MAIN-CHECKOUT-ONLY latent red workers never see (their trees contain no `.jj-ws`). Fixed
  at the root (`lib/fs.f` skip list); add new conventional untracked dirs there when
  introduced.
- **Stdlib leaves hide missing requires for months — bare-load them to prove it.** A
  module consuming another's words with no `require` line is masked by gate load order and
  surfaces only as a consumer "workaround" require. Proof and regression are the same
  command: `bin/hb --load lib/<mod>.f` + one word call must load green standalone; fix the
  module that CONSUMES the words, delete the consumer workaround. Module ENTRY files own
  dependency setup; test/tool entries `require` their own deps (run comments are not
  dependency setup). Optional loaders use `require`/`required` (include-once), never
  `include`, or a resident worker re-evaluates and hits duplicate definition. Core byte
  helpers used across unrelated libs belong in a narrow `src/core/*.f` prelude, not
  `lib/string.f`.
- **Adding a `require` to a stdlib module breaks `tools/bundle-lib.f` unless the
  bundle marks bundled paths `provided`.** The bundler source-CONCATENATES each module
  (`BL-COPY-FILE`), so a `require lib/errors.f` line inside a bundled module re-reads
  errors.f from disk at bundle-run time and dies `duplicate definition: E-A-FIRST`
  (rc 78) — require/include do not know the inline copy exists. Fix at the bundler, not
  by dropping the require: emit `s" lib/<mod>.f" provided` for every bundled module
  before its source (`BL-EMIT-PROVIDED-ALL`), mirroring how the native engine marks
  baked prefix files provided so a later `require` short-circuits.
- **Repo-scale source lints must STREAM, not vectorize.** Building a per-token vector costs 8
  `VEC-PUSH`es/token plus growth copies, so `LINT-LEX:SOURCE` took 9.2s on one file and a 141k fill
  63.9s; `lib/vector.f` element access is itself constant-time (`VEC-CELL-FIELD` is
  `base off cells +`), so the cost is push/grow churn, not indexing. A streaming scanner over
  the source buffer cut a repo scan 33s→0.9s
  / a 400KB stage2 file 40s→2s, byte-identical output; use raw `parse-name` for definer
  payloads so `(CMP)` isn't mistaken for a comment.
- **Engine-parity lexers need one delimiter predicate.** Native `parse-name`
  splits every byte at or below space; using the narrower lint whitespace set
  let a vertical tab merge top-level words and expose a false `PRIM;` inside a
  row comment. Share the engine predicate across word, comment, and row scans,
  and test every control delimiter the narrower set omits.
- **Token equality is not site classification.** Source-lex records kill comment/string
  false positives, but name-position refs still count (`: TRUSTED:` needed a
  definer-ref filter). Token lints must match dictionary-significant words
  CASE-INSENSITIVELY (`CREATE`/`;package` define/close exactly like lowercase — an upper
  closer leaving depth>0 hides every later def; keep exemption prefixes like `E-`
  case-sensitive). Keep lint classifier fixtures OUTSIDE the linted implementation (an
  inline `s" LAYOUT-BUFFER ..."` self-check got tokenized as real source). Cross-check a new
  scanner against raw `rg` per-file counts and eyeball every difference before trusting
  totals. `reserved-name-lint` runs only over user source (inside `tools/check.f`), never
  core — reserving loader words is gate-safe though `src/core/include.f` defines them.
  `variable I` passes the raw engine but breaks `tools/check.f` (i/j are loop-control) —
  generated converters run reserved-name-lint after prefix stripping (→ `IX`/`JX`).
- **`DISCOVER`/whole-file dep scans must walk COLON BODIES.** The dominant real idiom is a
  `s" path" required` guarded inside a colon helper then bare-called at top level; a
  body-skipping walker never sees it and returns stale keys. Lex the ENTIRE token stream,
  record guarded loaders unconditionally (superset of the runtime closure, safe under
  monotone load-if-absent guards), reject fail-closed on dynamic paths/loader shadow/undefine
  unless the file is a declared boundary in `dynamic-tail-manifest.f`. A scratch-capacity
  limit must fail only the CONSUMER that needs the value, not the whole scan (Discovery's
  `SD-PATH $400` threw on merely consuming any >1KB literal — set an overflow flag, reject
  only if that string reaches a loader word). `include` and `require` do NOT share a
  registry — a widely-`--load`ed tool must not grow new lib requires; for digests use the
  baked `SHA256`/`SHA256-FILE`.
- **Content-key cache keys hash the PATH string, not just the bytes** (`CK-FILE+`), so keys
  over `HB_TMP`-relative artifacts change per tmp root — for emitted intermediates hash a
  stable logical label + the file digest, length-framed with a distinct tag per stage.
  Cache keys carry the PRODUCER's identity: the object cache keying only `sha(bin/hb)`
  missed because the producer is the maker = f(engine, checker/codegen sources) — qualify
  with `HBB-MAKER-KEY-HEX`. `SHA256-FILE` resets the global SHA state (can't nest an outer
  cache key) and writes the digest to `SHA-DIGEST` resetting `SHA-OUT` (hex wrappers keep
  their own output pointer). Content-key caches stay memory-resident during a key build (load
  once per root, append new rows in memory). Content-key REUSE is not a warm snapshot — say
  `cache-root=persistent`/`scratch`, reserve "warm" for `tools/warm-image.f`.
- **One public binary `bin/hb`** (tty REPL / piped stdin / `hb script.f args` /
  `hb --load lib.f tool.f -- args`); build-only engines stay temp under `HB_TMP`, `bin/`
  holds exactly `bin/hb` (a second file fails the gate; persistent tool state goes under
  `XDG_CACHE_HOME`). No-binary recovery installs only `bin/hb` via
  `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh` (Gforth makes `HB_TMP` artifacts, then
  self-refresh); daily work never uses the Gforth path. Pipe vs script vs source-list mode:
  non-tty stdin with bytes = pipeline mode even if `argc>1`; empty non-tty stdin + `argc>1`
  runs `argv[1]`; `--load src... -- args` leaves fd0 as tool data. Reproduce gate failures
  with raw stdin + the wrapper's own prefix, not file-argument mode (different code path).
- **Bootstrap source parity matters, and parity is native fixpoint.** `tools/bootstrap.sh`
  must append the same native layers as `build-fixpoint.f` (incl. `image-bytes.f`); keep
  `bootstrap/cg/forth.fs` emitter calls syntactically real (`MOVZ,`, `BCOND,`) so Gforth
  catches a comma-less word immediately. The proof is byte-for-byte self-rebuild through
  build-fixpoint; retire bootstrap token-diff lints as a second source of truth. Gforth host
  needs locals — Homebrew 0.7.3 can't parse `{:`; use snapshot `0.7.9` (probes check exact `1`
  output AND exit 0, since 0.7.3 reports the error yet continues). Native port gates prove
  `bin/hb`/target-source/syscalls/ELF-AOT/checker/lints/self-refresh/REPL — do not install
  JS/Python/Rust to prove a native port; external Python baselines live as fenced
  ```python``` in docs.
- **`Habu-under-test` is the SMALL engine, not a snapshot; candidate size is
  RATCHETED.** Promoting `hb-new` (snapshot trailer bakes MBs of live DATA → 22MB
  candidates that jump into zeroed code on Linux) is wrong; promote `hb-stdin`, enforce a
  small candidate size. `GE-MAX-CANDIDATE-BYTES` only rejects catastrophic bloat — gradual
  growth needs committed per-target baseline rows (`test/gate-build-size.f`, per-target
  because Mach-O/ELF differ): growth fails until the same commit bumps the row, shrinkage
  prints `STALE-BASELINE`, an unmeasured target (0) fails closed. Bake side-effect-free phase
  libs into a warm gate runner keyed by runner source + seed image; baking the runner FROM
  the candidate on the critical path regressed the gate — start it in the early pool.
- **`bin/hb file.f` (no `--load`) drops to a REPL after a clean load and blocks on stdin
  (looks like a hang / rc 124); pipe `< /dev/null`.** Errors still exit non-zero
  immediately; gate/test files that call `T-REPORT`/`die` exit on their own. Any tool that
  ends with a plain `throw`-on-findings (not `bye`) has the same shape — redirect
  `</dev/null` (rc 0 clean, rc 1 findings). A spawned build child must be given `/dev/null`
  stdin, never inherit it: a `#!/usr/bin/env bin/hb` script falls into the stdin REPL after
  its body and `read`s fd 0; an inherited never-EOF pipe blocks forever (only reddens via the
  per-phase timeout). `BUILD-RUN` opens `/dev/null` and passes it as infd (root fix; adding
  `bye` to each fixture is a band-aid). Pipe-scoped env vars lie: put the env on the Habu
  side (`printf '' | env HB_TMP=/tmp/x bin/hb ...`).
- **`tools/check.f <file>` preverifies in ISOLATION — not for require-dependent files.** It
  does not process the require chain or FFI/`deftype` metaprogramming, so it reports
  `E-UNDEFINED` on lib words even for green files. Typecheck a device tool the way it loads:
  `bin/hb --load <full prelude> <file>` with the trailing `MAIN` run line stripped
  (`sed '/^MAIN$/d'`) so a clean typecheck exits 0 instead of throwing at `CUDA:OPEN`. A
  static checkability probe is only as honest as its registry context: probing a maker-only
  file through `check.f` came back clean because check-core's own requires (`tools/json.f`)
  had put `JSON-DIAGS` into the probe registry — the real maker compile then rejected it. The
  oracle must BE the context (the live stage/maker compile). `--all-errors` checks top-level
  defs independently, so on bundled stdlib it falsely flags deps undefined and multiplies
  time — run the fast bundle check first, all-errors only after failure; `--source-list`
  all-errors walks original files with cross-file support replay through
  `VERIFY:SOURCE-BUF-IN-SCOPE` (a collected-but-undispatched form is a silent no-op — fix
  both ends).
- **A CLI tool that reads AMBIENT argv must NOT be `included` into a shared image
  carrying foreign argv.** `perf-regress.f` (`SCRIPT-ARGV$`) `included` into the resident
  image read the harness's `--under bin/hb` and threw `E-FS-STAT` before parse — a
  deterministic argv contamination misread as a fork/COW race. Run such tools SPAWNED
  (clean argv), carry the assertion in an argv-free `-test.f`. Corollary: a public diagnostic
  must reset ALL state it can read (a stale `LAST-LINE$` cursor made a path error masquerade
  as a malformed-row error). Reproduce the include argv leak with
  `bin/hb --load a.f cli.f -- <args>` — no fork needed.
- **A `TEST:SUITE` block is ONE `bin/hb --load` spawn (all files into one image
  sequentially), so suite files must be package-scoped, duplicate-safe, tolerant of an
  earlier file's installed check hook and shared library/registry state.** A test suite that
  opens `package X` must close it with `;package` before `T-REPORT`, or a LATER suite dies
  `exit 75` printing a bare token (the engine's does>/quotation compile guard hits the
  still-active package) — the crash is 1-2 suites later than the culprit, whose own suite
  PASSES. A later suite reusing an already-registered id (or a name an earlier suite
  interned) fails only in the full gate — prefix every fixture identity with the test's own
  tag; use an already-INIT-registered id (`TARGET:SM87`) rather than minting into a
  cap-bounded shared registry. Test files reopening a shared package need globally-unique
  helper tails (`SUBJ-A` collides across two `package DIFFRUN` files → `duplicate definition
  rc=78`).
- **"Wired into a TEST:SUITE" is not "runs".** `gate-stdlib-cases.f` suites execute only if
  a `SUITE-*-LABEL?` slice selects the label AND someone invokes that slice; the resident
  `test/run.f` runs the in-process GSI groups + a few spawned slices, NOT the full TEST:SUITE
  inventory — the two lists are hand-synced and drift silently (four checker-invariant suites
  ran in NO automatic gate). The standalone slices and the gate's resident groups are
  different execution paths; register a test in the path that must execute it.
- **Gate slices see different lints — the integrator runs the slice that OWNS each touched
  file class.** maki-dep-lint (dependency direction) and error-code-lint live in the
  lint-tools slice; a lane validating only lint-libs + maki/test can land a maki/ reference or
  a code collision in the stdlib layer and stay green until the resident run. `error-code-lint`
  is part of the OWNING gate for ANY new `E-*` (including maki/, whose subdirs a bare `maki/*.f`
  glob misses — scan recursively); grep for a free negative code, never pick by adjacency
  (error codes are a global namespace with real collisions). `maki/` as a code TOKEN trips
  maki-dep-lint anywhere but maki/ files — register only prefixes that actually occur.
- **Off-device `CUDA:OPEN?`-style SKIP guards are a FAIL-OPEN class.** They kept
  `fusion-compare.f` green for weeks off-device while it would die uncaught (missing /tmp cubin,
  `E-CUDA`) the moment a device appeared. A device suite is proven only by an on-device run;
  every device tool's top-level entry probes `CUDA:OPEN?`, prints a recorded SKIP line, exits
  (the GB-ALL shape) so it composes into host suites; prefer self-emit + fail-closed throws
  over prebuilt /tmp artifacts, and key device legs on the probed device-FFI capability. Keep
  a new `maki/*-device-test.f` OUT of `maki/test.f` (needs CUDA) and device-PROVEN
  before adding it anywhere.
- **PTX/emitter shape tests are NOT assembler proof.** `ptxas` rejects undeclared predicates
  above `%p15` and stale resource pools (`%p<8>` when the emitter now needs `%p21`); a text
  fixture rendered plausible text that never assembled. Keep the text fixture, then assemble
  the EXACT emitted artifact through checked emit → `ptxas` → (for perf) CUDA launch. In-process
  PTX text tests use a `PTX-L` sink hook (`PTX-CAPTURE-ON`/`-OFF`, fail-closed on overflow), not
  a subprocess. `zed`'s ptxas is at `/usr/local/cuda-12.6/bin` (not on PATH); the box's
  `~/Work/habu` is stale vs master — run a device leg by transferring the FULL tree to an
  isolated `/tmp` dir and bootstrapping a fresh engine THERE, never touching `~/Work/habu`.
- **Do not spawn assertion tools for semantic checks; keep cores in-process, reserve child
  `hb` for real CLI boundaries.** Gate JSON assertions, checker diagnostics, and lint cores are
  checked library words — calling them in-process cut hot helper spawns (151→123) while
  preserving coverage. Split every CLI tool into a `*-core.f` (reusable, buffered output,
  caller-supplied scratch) + a thin `*-main.f`; the test installs an output buffer and
  `require`s the core (the require-time run becomes the captured end-to-end fixture). Reserve
  child `hb` for argv/env/stdin/exit/source-label contracts, and for negative COMPILER probes
  (rejected source can exit through checker/compiler `die` before `catch` returns) — route
  semantic negatives through `CHECK-CANDIDATE!`/`CHECK-ALL-ERRORS-BUF` + diagnostic rendering.
  Never assert engine-COMPILE failures through in-process `catch`-around-`evaluate`: a
  definition-compile failure SIGBUS-crashes (rc 134) and an interpret-level failure returns 0 to
  catch, while plain stdin/`--load` exit an orderly rc 70 — pin them as gate child-process
  cases (`GE-RUN-STDIN` + `GE-EXPECT-RC`).
- **A core/wrapper split updates every child load list AND every warm-image path.** Missing
  transitive deps show as child rc 70 before the parent can explain it. Entry/core splits must
  not WIDEN worker preload — a core is a win only if loaded by the worker that needs it (pulling
  SARIF into the shared diagnostics lib made every diagnostic worker compile it). Warm image
  entries cannot hide deps behind `include` (the baked source map can crash the include
  boundary) — bake the core into the warm image when it fits + load a no-include `*-main.f`, or
  pass core+entry explicitly.
- **The dot ledger DRIFTS from head — audit before assigning, and `rc 0` is NOT proof.** A
  sweep of 129 open dots found 6 fully landed, 10 with stale premises, and 3 ledger rows owned by
  archived dots, invisible because the gate ran FIXTURES not live strict.
  Verify a dot's claim against head and reconcile current blocker
  references before `dot off`; engine-suite standalone exits 0 after checker errors
  (drop-to-REPL masks) — the last-line `ok` marker or the full gate is the signal. Reproduce
  engine-suite changes through `bin/hb --repl < test/engine-suite.f` (a `cp@ patch32` proof
  passes via `--load` yet SIGILLs via the stdin REPL). Hot-cache full-gate passes do not prove
  the engine-build closure — `test/run.f -- --cold-cache` exercises the native build slice
  (merge-gate runs cold); when a change adds a transitive `require` to any gate file, run cold
  before claiming green.
- **In zsh, `path` is the array tied to `PATH`; never use it as a loop or scratch
  variable.** Assigning it can make the next command disappear. Use a purpose-specific
  name such as `dot_file`, especially in verification scripts that must fail closed.

## Gate Harness, Scheduling & Caching

- **A regression that pins ONE fail-closed exit code stops testing what it
  names the moment an earlier fail-closed boundary appears, and nothing goes
  red to say so.** `test/pre-trust-defer.f` blanks the drain and asserts exit
  73 from the `SEAL-CAPTURE` backstop. Commit `e8c27f225303` added a pre-trust
  `defer PKG-LIVE-XT` (`src/core/checker.f:465`) and a checked
  `is PKG-LIVE-XT` at `src/habu/xref.f:209`, which is 283 lines ABOVE the
  baseline `SEAL-CAPTURE` token at `xref.f:492`, so the check hook now rejects
  at exit 70 first and the backstop is never reached. The backstop still works
  — neutralising that one `is` restores exit 73 — but its only coverage was
  gone. Order between two fail-closed boundaries in the boot prefix is implicit
  source order with no gate; when a test asserts a specific code, either assert
  the first boundary explicitly and cover the later one separately, or add a
  static gate on the ordering.
- **A guard that proves a STATIC property with a wall-clock budget cannot tell
  slow from broken.** `test/lint-cli-standalone-load.f` proves each
  `tools/<name>-lint.f` requires its own dependency closure by spawning it and
  asserting the child exits on its own inside `TIMEOUT-MS` (20000).
  `tools/refine-lint.f` loads in 2.1 s and then scans the whole repository,
  taking 16.5 s to 65.7 s wall on the same idle machine, so it trips the budget
  and reports a bare `expected true got false` with no exit code and no
  elapsed time. Raising the budget to 120000 in a scratch copy greens the file
  — proof that the guard is timing the lint's work, not its load. Make the
  probe stop after the requires, or at minimum report timeout and dead load as
  two distinct named verdicts.
- **SUBJECT:RUN forks the live test process, so call it with NO package open,
  and never gate a CLI file that parses argv.** A suite whose RUN executes
  inside its own package makes the forked child's `package X` a NESTED-package
  reject (exit 75) instead of the behavior under test (weight-store's seal probe
  expected SEAL-PACKAGE 84); close the package and call `PKG:RUN` from top level
  (the json-read-test arrangement).

- **Add no new `TRUSTED:` sites.** Use a properly owned `PRIM:` axiom or wait
  for checker capability; existing `TRUSTED:` sites are migration inventory,
  not precedent.
- **A property test pinning a TRANSITIONAL invariant must be revisited the moment
  the capability it anticipates starts being used for real.** `test/pre-trust-defer.f`
  COMPAT-MISS-CASE asserted that an engine lacking the DRAIN-PRETRUST prim BOOTS
  exit 0 (the "old-engine tolerance" property) — sound only while the boot prefix
  declared ZERO real pre-trust defers. Commit 563b2540 landed the first real prefix
  defers (the five TFAM checker hooks) and the property inverted: a shim-miss engine
  now correctly FAILS CLOSED at seal (exit 73, "undrained pre-trust defer"). That
  merged without the manual/heavy pre-trust-defer suite being rerun on the Mac
  battery, so master carried a red manual-tier suite for a day. Rule: a merge that
  touches `src/core/checker.f` (or any file a manual/heavy suite OWNS but the fast
  tier never forks) must run that owning suite at merge time — a green fast `run.f`
  tier is not proof for suites run only by `test/gate-stdlib.f`.
- **The tail-ratchet asserts EXACT child-process counts AND elapsed ≤ budget
  (`PROCESS-NOMINAL-MS` 10000 × PERF-MS).** An elapsed-only overshoot with no
  child-count delta (e.g. 10099/10000) is machine-load noise, not your change —
  the counts prove your work isn't in that timed group; re-run to confirm it
  clears.
- **Every process-spawning time ratchet must scale its nominal by the host
  calibration (`TEST-BUDGET:PERF-MS`), never a naked wall-clock constant.** The
  engine gate's runtime slice was the lone exception (`10000 constant MAX-MS`,
  test/gate-engine-lib.f) and false-redded 10047–11919 ms on byte-identical
  engines whenever other lanes or user workloads loaded the box; fixed 2026-07-19
  by dot habu-derive-runtime-budget-81b2f538 (derived `NOMINAL-MS`, cal-scaled
  budget, `cal-pct=`/`(saturated)` attribution on every run, RATCHET-SELFTEST
  proving a >3×-nominal engine reds even at the 300% clamp). Two measurement
  rules from the derivation: (1) the calibration spin (cal-pct) tracks a short
  phase's ACTUAL contention better than 1-minute loadavg — a loadavg-3 sample
  showed elapsed 2× the loadavg-6 steady state while cal-pct 151 tracked it;
  attribute with cal-pct, not `uptime`. (2) In-gate elapsed far exceeds the same
  slice standalone because of intra-gate phase concurrency — derive budgets from
  the REAL gate's numbers (extract from `$HB_TMP/pool-*-out.log`), never from a
  standalone harness run.
- **Full-DAG timing beats isolated wins; every focused optimization must survive the whole
  command under contention.** Splitting suites, per-phase forks, higher nested pools, and
  preloading shared setup all passed focused probes but regressed the full gate — record reverted
  timings in the dot so failed variants aren't rediscovered. The winning shape overlaps
  non-stdlib phases with shared stdlib setup, loads the common tool base ONCE as silent suite
  setup, then forks phase-owned resident workers that inherit it copy-on-write (their unchanged
  `require` lists dedupe against the inherited image and load only the family delta). Do NOT
  widen the shared base further: pre-setup fork spans are reap-inflated by the serial setup (a
  phase exiting during setup reads ~the setup duration), so widening puts serial load on every
  post-setup fork's critical path — reclassify only HIGH-redundancy family workers.
- **Gate budgets are stop-line thresholds tuned per HOST PROFILE, not comfort blankets.**
  macOS/generic-Linux/Jetson have different CPU envelopes even at the same pool policy — auto-detect
  a concrete profile; portable budget = base × (probe-ms / profile-reference-ms) clamped
  [100%,300%], factor 100% for unmeasured profiles, never scale user `--budget-ms`. A spin probe
  (~95ms macOS ref) captures load/downclocking (what actually failed green trees); print
  cal-ms/cal-factor so a stretched budget is visible telemetry. Timeout floors ≠ ratchets:
  `HB_LOAD_PCT` carries a 3× structural pool-pressure floor so healthy children aren't killed;
  export measured `HB_CAL_PCT` separately for phase ratchets (applying the load floor silently
  turned nominal 8/10s into 24/30s). A faster gate is not protected until its budget is TIGHTENED
  (a 24s impl still permits the old regression at a 70s verdict). Never bump MAX-MS to pass a
  ratchet — the engine battery's runtime ratchet catches real per-process regressions (region
  growth to 8MB regressed boot +41ms via LPROT's full-region mprotect brackets, linear in the flip
  window). Report maker, artifact, and result cache fills as budget coverage; none changes whether
  phase 15 runs.
- **Every ordinary gate run builds Habu-under-test in phase 15; only explicit `--under` skips it.**
  A persistent candidate cache can publish a binary before phase 15's verdict, then reuse it and
  skip the failing phase on a later run. Build the candidate in the early engine-build slot, then
  release downstream phases onto `HABU_UNDER_TEST` after it is ready. Maker, artifact, and result
  caches may hit, but none may suppress phase 15.
- **Private temp dirs for native builds; shared `/tmp` races parallel agents.** `HB_TMP` defaults
  to `/tmp` with fixed names (`stage2-src`, `hb-stdin-got`) — concurrent workspaces corrupt each
  other's refresh/gate with transient opaque exits. Allocate+export a private `HB_TMP` (create it
  BEFORE spawning makers — a missing temp root collapses to an empty nonzero failure) and
  `HABU_BUILD_CACHE` per workspace; derive parent-artifact and child `HB_TMP` from the same getter.
  Do NOT lock shared maker caches (a mkdir lock timed out under contention, left a stale lock) —
  build makers in each private `HB_TMP`, publish with atomic `rename` (cold races duplicate work,
  never deadlock). Re-emitting source across build stages is a boot-reload TOCTOU: `BF-PIN` pins
  each emitted path's content digest on first read via the single `BF-APPEND-SOURCE` choke and
  re-verifies on reload (throws `E-BUILD-BOOT-DRIFT`); baking the digest for BOOT-time reload
  verification is a separate engine change. `BF-*` spawn helpers must reset `PROC-ARGV` before
  their own stage (warm images reuse one process across phases — stale argv leaks). Full-gate
  timing is meaningless while another worktree runs `test/run.f` — check for active Habu gates /
  stray `bin/hb --load test/run.f` (killed runs orphan spinning fork workers at full CPU) before
  claiming a budget regression.
- **Pool slot state is an invariant (free=-1, active=0, done=1; set active BEFORE spawn); fixed
  artifact slots must not be reused by the dynamic pool.** A live-count pool with free slots still
  marked free spins forever after children exit; a fixed slot started after a general phase resets
  an active slot and orphans the child (`GT-POOL-START-SLOT` fails closed on active-slot reuse —
  start fixed-slot builders before free-slot phases). Put fixed artifacts in slots OUTSIDE the
  dynamic range, let normal phases use `0..N-1`. On the 4-online-core Linux/Orin target six top
  slots beat eight (several phases spawn nested pools); the macOS profile is 10 top / 2 nested;
  choose defaults from documented FULL-gate timings, not a single slice. Keep the pool poll timeout
  small (`$64`ms) — a 1000ms poll can lose the whole margin at the final phase. Nested lint
  subprocesses need their own timeout caps sized for aggregate contention (a fixture spawning a
  repo-scale tool needs a bigger cap; `rc 58` = `E-PROC-TIMEOUT` surfaces only once outcome
  attribution prints the throw code).
- **Pool failures must DRAIN, not kill; and telemetry has one owning emitter per label.**
  `GT-POOL-FAIL` records a red row + capture paths and continues, `GT-POOL-DRAIN` dies after the
  drain when reds exist, `GT-ROOT` survives on failure so per-child captures survive triage. Fork
  children share the stats file, so the pool owns spans for its entries (`GT-POOL-FORK-CHILD`
  records the fork label, `GS-SPAN` skips matching labels, load time is a separate `span-load`
  class); the test pool legitimately reuses one label across entries, so dedup by a single-use
  ownership claim + rejecting duplicate TEST-ROW labels at index time, NOT by rejecting duplicate
  pool labels. Suppression is process-local, ownership is not — a pool parent's authoritative spans
  bypass the fork-child dedupe (`GS-SPAN-AUTH`); qualify a byte-keyed (row,span) pair with the
  EMITTING process's generation, never the slot's. Emit the stats SUMMARY before deleting the gate
  temp tree, then enforce the budget. A per-worker parent-death reaper must NOT be a
  `wait(-1)`-visible child of its worker (`PROC-WAIT-RC` on `-1` blocked forever) — double-fork so
  it reparents to init yet inherits the worker's group. Shard a fuzzer across forked slots but mute
  shard stderr (`/dev/null`) — bounded capture buffers overflow (`E-PROC-TRUNCATED`); a false-cert
  still reports via nonzero exit.
- **An uncaught throw in a `--load`/spawn child exits with the throw code's low 8 bits and
  prints NOTHING — decode before guessing, and give build drivers a reporting boundary.** Opaque
  exits are `throw-code mod 256`: add multiples of 256 until a known `E-*` appears (exit 56 =
  `E-PROC-TRUNCATED` -2504; exit 104 = `E-STR-BOUNDS` -2200). Size capture/arena buffers to the cap
  the ADMITTING path enforces (a ~28KB PTX overflowed a `$4000` child-capture buffer), not the
  first example that fits; capture buffers size for isolated jj-workspace PATHS (long), not the
  short main checkout. Boundary validation on external input (env/argv) must `die` with a
  source-pointing message NAMING the input; keep bare named throws for in-process callers where the
  harness still has the code. `DRV-FAIL`/`M-FAIL` now report "driver: uncaught throw code N" at
  every stage/maker boundary; a 1-byte diagnostic + clean exit means a raw engine capacity path
  (dict/code cap), not a throw — when `catch` cannot intercept a death, hunt for `exit_group`
  emitters, not throwers, and falsify with an in-state control (`' evaluate catch` on a known throw).
- **A pool/gate publication chain must MECHANICALLY test the verdict — human eyes are not a
  gate.** Three red-master pushes, same class: `gate; ...; bookmark set && push` (the `;` discards
  the gate exit); `bin/hb --load lint.f | tail && push` (guards on TAIL's exit → `set -o pipefail`
  or `out=$(cmd); rc=$?`); "print the log then push in the same batch" (`rg -q 'RUN_EXIT=0' "$LOG"
  && move && push`). Regate the MERGED tree, CHECK the result, then move bookmarks — separate
  commands or `&&` from the gate onward; cross-lane semantic conflicts do not show as rebase
  conflicts. Gate entry wrappers catch their own top-level throws, print phase label + code/name,
  rethrow so rc stays useful.
- **Cross-host "gate green" does not transfer — regate on the integrating host before treating
  master as green.** A tree landed green on the spark failed the Mac battery's stale-status doc
  lint (`N/N/N` measurement params read as a count-shaped string); assume per-host slice
  composition differs until proven identical.
- **A `bin/hb` older than the tree's core dies at the first unknown core word (bare name +
  exit 70) — and fixpoint refresh children spawn the checkout's `./bin/hb`, so a fresh DRIVER
  engine is not enough.** Recover by installing the freshest local engine as `bin/hb` first
  (new inode: `cp` to temp + `mv`, never over the live file), then `install --force`.

## VCS, Dots & Parallel Agents

- **A worker's edits exist only after a jj snapshot — hours of validated work
  can vanish without one.** A lane implementing a seed primitive ran every
  query with `--ignore-working-copy` and every build through bash, so no jj
  command ever snapshotted its file edits; when concurrent merge-gate
  operations forked the operation log, its working copy was rebuilt and the
  fully validated, uncommitted slice was discarded. Worker briefs must demand
  an immediate `jj describe -m "wip: ..."` plus a plain `jj st` (which
  snapshots) after each meaningful edit, and the orchestrator should hold
  repo-global operations (rebases, bookmark moves, workspace forgets) to the
  minimum while a lane is mid-implementation. The report-everything discipline
  saved the day: the worker documented every edit, so re-landing is cheap.

- **A rewrite that drops ONE line per record is invisible in a diffstat, so
  count the records.** A commit described as "Record the comparison lane's
  lessons and dots" landed on `proofs` reading `LESSONS.md | 488 ++----` — which
  looks like an archival tidy-up. It had in fact deleted the bold lead line of
  439 of the 442 lessons while adding three: every entry survived as an
  orphaned indented paragraph, so the file still LOOKED like a lessons file and
  no lint noticed. A line-count summary cannot distinguish "moved a block" from
  "decapitated every record", and neither can a reviewer skimming the hunks,
  because each individual hunk is a plausible one-line deletion. For any commit
  that rewrites a whole record-structured file — lessons, manifests, baselines,
  dot bodies — check the RECORD count against the old side
  (`grep -c '^- \*\*'`) and not the byte or line delta. Repaired here by
  rebuilding the file as base ∪ their additions ∪ ours and re-counting.

- **`jj workspace update-stale` blanks an undescribed working copy — and the
  snapshot it took first is how you get the work back.** A lane with a full,
  gate-green but UNDESCRIBED change hit "working copy is stale" because peer
  workspaces had advanced the operation log; `update-stale` reported
  `Added 0 files, modified 10 files, removed 3 files` and left `jj st` saying
  "no changes", with every edit gone from disk. It is recoverable, and the exact
  route matters: `jj op log` shows a `snapshot working copy / args: jj workspace
  update-stale` operation on the OTHER branch of the reconcile merge - that
  operation holds the pre-reset tree. Read the commit out of it with
  `jj --at-op <op> log -r <change-id>` (confirm with
  `jj --at-op <op> diff --stat -r <change-id>`) and bring it back with
  `jj restore --from <commit-id>`, which touches only your own working copy.
  Do NOT reach for `jj op restore`, which rewinds the whole repo and would
  discard every peer workspace's work in the same window. Then describe
  immediately. The cheap prevention is the rule above: `jj describe -m` as soon
  as a change is coherent, not when it is finished.

- **A gate and its push must never share one unconditional command chain.** A
  dot-graph lint threw during a closure batch, but the same shell block carried
  on through seal, bookmark move, and `jj git push`, publishing a red master
  that then needed an immediate fix-forward. Run the gate, READ its exit code,
  and only then issue the push as its own command. The same event carried the
  second half of the lesson: **before closing a dot, search .dots/ for
  references to its id** — a dependent minted meanwhile (here by a parallel
  session) makes the closure a dangling-blocker lint failure; discharge or
  rewrite the dependent's edge in the same closure batch.

- **One workspace name must map to exactly one directory — a nested duplicate
  checkout silently destroys work.** A `jj workspace add` (or a worker's stray
  checkout) whose path resolves INSIDE another workspace's directory leaves two
  directories fighting over one working-copy commit: snapshots from the stale
  directory silently reset the other's edits, and no-description divergent
  copies of commits appear (this lost a .gitignore edit and caused a
  "won't push commit ... has no description" rejection). Create lanes only from
  the repo root as `.jj-ws/<dot-id>`, check `jj workspace list` output maps each
  name to one directory, and treat an unexpected working-copy reset or a
  divergent change id as the tell: stop and hunt for the second directory before
  continuing.

- **A destruction review is not integrated until every unfixed finding has a
  detailed dot.** Reconcile each finding against existing dots, extend a matching
  dot when its acceptance misses part of the finding, and add a new dot for every
  uncovered invariant before landing the reviewed slice.

- **Concurrent jj ops can REVERT uncommitted workspace edits — back up BEFORE
  `update-stale`.** A sibling agent's op forks the operation log; the next
  `jj workspace update-stale` REBUILDS the working copy to the recorded op, discarding
  unsnapshotted on-disk edits and leaving a divergent twin (whose snapshot may be STALE). The
  0-byte patch a failed `jj diff` writes is the tell. Protocol: copy edited files out, run
  update-stale, verify the concurrent commit's file set doesn't overlap yours, restore from backup
  on a fresh `jj new`, abandon the empty leftovers (lossless every time; guessing at jj state is
  not). In shared-repo parallel sessions, `jj commit` each verified change IMMEDIATELY; after any
  main-workspace write while workers are live, `jj st` at once (three `dot add` dots were lost this
  way). Never base parallel workspaces on a mutable working-copy commit — branch each from a
  STABLE commit. Don't parallelize VCS status (`jj st`/`git status` race on `.git/index.lock`).
- **Worker workspace path discipline: Edit/Read the `.jj-ws/<ws>/…` absolute path, never the
  main tree.** A `cd` in Bash does not change where Edit/Read resolve — editing the main path while
  Bash builds in the workspace silently edits the wrong tree (the build never sees it, the main
  tree gets polluted). Worker briefs include the absolute-path check ("every path starts with
  `.jj-ws/<lane>/`") and a pre-seal `jj -R <main> st` clean assertion; the integrator requires the
  main workspace CLEAN before gating (an unexplained modified file = presumed lane leakage: preserve
  to a patch, message the lane, restore pristine, re-gate). Commit each proven-green slice
  immediately — uncommitted worker edits are one cleanup away from loss. Clean up a lane's workspace
  the moment its work merges (`jj workspace forget <name>` + rm the dir in one step); ~40 stale dirs
  invited a bulk removal that looked like data loss. Workers NEVER write the shared checkout
  (`bin/hb` included) — `cp` over a LIVE `bin/hb` poisons the macOS AMFI vnode cache (exit 137
  SIGKILL for that path only, valid bytes at a fresh path run fine); replace via a NEW inode
  (write-temp + rename, which is why the installer never triggers it).
- **Before dispatching a worker against a shared dot, probe the other lane's in-flight work AND
  verify the dot's premise on the CLAIMED tree.** An S2 slice was implemented twice concurrently
  (same dot id) and a full lane retired unsalvaged — a one-minute `jj log -r 'master ~ fable::'`
  scan + the dot's blocker graph beats a duplicated lane. `dot ready` honors only RECORDED edges
  (prose references never gate dispatch) and `blocks:` means BLOCKED-BY (this dot's dependencies),
  so before claiming a ready dot verify its `Files:` exist on master (one `rg`/`jj file list`);
  when a lane reports premise-missing, record the blocked-by edge immediately so the ready list
  stays truthful. A lagging branch can carry a stale plan — diff the dot against master's version
  and rebase onto the plan-owner's branch BEFORE implementing (a destruction review is only as good
  as the spec it is given). Reviewers must load the contract from the intended integration base,
  not the candidate's parent: a CHECK review used a superseded dot and falsely rejected the current
  six-word API as missing a retired capture subsystem. A probe must name its LAYER (an
  `E-UNDEFINED` from a checked colon body
  is a CHECKER-grammar verdict, not runtime-resolver evidence); disagreeing probes mean the
  SEMANTICS are inconsistent — itself a finding. Stage-then-fan-out beats one long worker: stage 1
  resolves the core contract, stage 2 parallel workers port disjoint file clusters, stage 3 one
  integrator gates the exact tree; serial only for the core contract, the merge commit, and the
  gate/bookmark window.
- **`dot on` at DISPATCH is the cross-lane claim; `dot off` only at landing, and closing a dot
  is not done until its file deletion is COMMITTED.** An unpushed active bit is not a claim; parked
  dots go back to `open` so `active` never lies. `dot off` archives the file (gitignored) and
  orphans every `blocks:` edge naming it — sweep those lists in the same commit, remove an emptied
  `blocks:` header, then gate that exact tree with dot-dep-lint. Never leave closures in the working
  copy across a merge window — `jj new <tip>` orphans
  them (archive copy persists, tracked open copy returns → Ambiguous ID); every `dot off` is
  immediately followed by dot-dep-lint + `jj commit`.
- **Use only documented `dot` subcommands — an unknown form is QUICK-ADD and creates a stray
  task.** `dot dep check`, `dot dep --help`, and even `<unknown> --help` all create dots; consult
  `dot --help`, inspect `jj diff` after every tracker command. The search verb is `dot find`, not
  `dot search`. `dot add -a TARGET` records the new dot as BLOCKING TARGET; the local CLI retained
  only the LAST repeated `-a` (dropping earlier prerequisites) — patch the frontmatter, run
  dot-dep-lint, verify the rendered tree. `-P <root-id>` only when `.dots/<root-id>/<root-id>.md`
  exists; never `-P`/`-a` a nested id. `dot on` re-quotes metadata each transition (never re-run on
  an active dot). Never interpolate Markdown backticks into a double-quoted shell `dot add -d` —
  command substitution executes them and can erase a stack effect. Omit Markdown quoting in the
  CLI argument or single-quote the whole argument, then inspect the created file. Mark/close the exact
  PRINTED id, not a filename.
- **Two push-verification failure modes: an empty `jj log -r` result must be tested with
  `[ -n ]`, and "Move SIDEWAYS bookmark master" means NOT a fast-forward.** A dot was closed on a
  landing that never pushed (empty result eyeballed as success, blocking two lanes); a sideways
  master push orphaned another agent's commit. Protocol: positive ancestry check
  (`[ -n "$(jj log -r 'origin-tip & ::candidate')" ]`) BEFORE moving bookmarks; treat "sideways" in
  push output as stop-the-line; end the window with move-bookmarks → push → `jj new <tip>` in that
  order (leaving `@` on a pushed commit makes the next snapshot AMEND it). Track a new local bookmark
  before its first push (`jj bookmark track <name> --remote=origin`). Pushes reject conflicted
  ancestors (a clean worktree + green gate don't clear conflict metadata — resolve the earliest
  `conflict` in the pushed range). Jj's default word-level diff can visually concatenate numeric
  replacements (`$200000` beside `$400000` → `$200000400000`) — inspect source or `jj diff --git`;
  never `jj diff --check`. History filters must include JJ refs (`refs/jj/keep`, `.jjconflict-*`);
  ignore generated output by SHAPE, not run name.
## Code Quality

- **No repository caller does not make a public REPL word dead.** The operator is the caller for
  interactive facilities such as `prof-on` and `prof-report`; remove or unbundle one only after a
  product decision, not from an internal call-site census.
- **Split multi-pass work into cursor/pass/row/render helpers; giant words hide effects even
  with correct signatures.** Put the word effect on the definition line; add body-line effects only
  where they prevent real stack-state reconstruction (empty/no-op line comments are noise — factor
  instead). State the full public effect BEFORE `{: ... :}` (locals-then-`( -- result )` hides the
  inputs from readers). Bare-local lint markers are per locals-group (`typed-local-diff-lint` clears
  allow-state at each `:}`; in `bootstrap/cg/*.fs` stock Gforth forces bare locals, so each changed
  group needs its own `allow-bare-local` marker). Prefer hex for machine-adjacent literals (bytes,
  ASCII, masks, offsets, crypto); decimal for small human counts.
- **Do not port SwiftForth's unchecked contracts by habit.** `PLACE`/`APPEND`/`ZPLACE` don't check
  destination capacity — borrowed string utilities carry capacity+length cells (`BUF-APPEND*`) and
  throw named errors. Don't port relative linked-list words (`@REL`/`,REL`) — model node layout with
  structures, collections with arrays/maps, dispatch with checked `case`/execution vectors.
  `SB-CAP` is 1024 — build >1KB fixtures with `APPEND-FILE` loops, not the string builder (an SB
  overflow in fixture CONSTRUCTION exits the whole suite with a bare masked rc before `T-REPORT`).
  Stack-snapshot DSL is shared (`lib/test/snap.f` owns `T{ ... -> ... }T`); require `lib/test.f`
  instead of copying the trusted boundary.
- **`.` ends the line in the native engine (newline-terminated, not space).** `11 . 22 . cr`
  emits `11\n22\n\n`, so a gate assertion for two dotted numbers on one line never matches — assert
  separately or dot a single derived value. Inline counters/failure text need digit emitters
  (`GT-U-TYPE`, `TS-N.`).

## Runtime, Codegen & AOT

- **Widening a shape check to accept more must add a requirement, not just drop
  one.** `SNAP-RELOC:EMIT-ADDRS` re-verified a recorded address chain by masking
  each of its four words with ADDR-OPC-MASK and comparing against W-MOVZ0 /
  W-MOVK1 / W-MOVK2 / W-MOVK3, which pins Rd = x9 — the register the engine's
  ONE carrier `C-ADDR-RAW` writes into. The native chain's allocator picks its
  own register, so the map has to accept any. The cheap widening — extend the
  mask so the register field is not compared at all — accepts four move-wide
  words naming four DIFFERENT registers, whose four immediates spell out no
  address any site ever pushed, and the pass would rebase them anyway. The
  shipped guard instead reads the register off the site's own word 0 (one
  `ADDR-RD-MASK ANDI`) and requires each remaining lane to equal its scaffold
  carrying THAT register (`LSRI`/`LSLI` clears the scaffold's x9, `ORR` puts the
  site's back). Both halves are stated in `formal/Common/Reloc.v`
  (`chain_in_any_register_is_a_chain`, `a_chain_names_one_register`) and both
  are falsifiable through the shipped instruction sequence: restoring the x9-only
  compare reds `chain_register`, and taking the register-blind widening reds all
  four `chain_mixed_register` rows.
- **A proof about a definition containing `mod` must never be closed with bare
  `cbn`.** Making `Reloc.v`'s `is_chain` register-parametric put `w mod 32`
  inside it; `chain_stays_a_chain`'s existing `cbn` then ran for over five
  minutes without finishing, because `cbn` unfolds `Z.modulo` through
  `Z.pos_div_eucl`. Nothing reported an error — the whole `rocq compile` simply
  never returned, and piping its output through `head`/`tail` hid the timeout
  behind the pipe's own exit status. Name the constants a reduction may touch
  (`cbv beta iota zeta delta [is_chain]`), and read `${PIPESTATUS[0]}` or
  redirect to a file when timing a compile.
- **`RELOC-VM`'s SYM-CAP is a silent-looking wall.** Teaching the relocation
  machine two more shipped constants pushed the symbol table from exactly 32 to
  34 and `RELOC-VM:SYM+` threw `E-CRL-DECODE` (-6882) — the same code an unknown
  mnemonic throws, so the failure reads as "the pass grew an instruction the
  machine was never taught" when it is really "the table is full". Count the
  symbols in `TEACH-MACHINE` before hunting the instruction stream.
- **A fixed-register constraint belongs to the routine's interface, not to the
  instruction form: put it where the rest of that interface already is.** The
  ARM64 chain needed to say "argument two arrives in x1" and "the result leaves
  in x0". The operation schema (`src/compiler/ir/schema.f`) was the tempting
  home because ties live there and the allocator already reads it - but `add`
  never needs x0, and a constraint that varies per ROUTINE cannot be a property
  of a FORM. It went onto the routine contract (`src/compiler/a64-effect.f`),
  which already declared which registers are read and returned, as two ordered
  register lists replacing the two sets - and the sets are now DERIVED from the
  lists, because a stored set beside a stored list is two authorities that can
  disagree. The payoff is structural, not stylistic: `A64RA:ALLOCATE` and
  `A64RAV:ACCEPT` were already handed that one value, so the allocator
  pre-colours from the declaration and the validator checks the same declaration
  independently with no new plumbing and no chance of the two reading different
  copies. Ask "whose fact is this?" before "where is it convenient?".
- **An ordered list can live in a one-cell contract field: pack it, and make the
  packing canonical.** A record field has to be one cell (a multi-cell value
  cannot be a typed local yet), which is why register SETS were used where a
  convention needs an ORDER. A register operand is a five-bit field, so twelve
  positions and a four-bit length fit one 64-bit cell exactly. The rule that
  makes it safe is that every bit past the last position must be zero
  (`A64EFF:SEQ-CK`): without it two cells could mean one list, and the digest
  would stop agreeing with the structural comparison. A forged cell - the
  generated `MAKE` of a public family - is refused by the same check.
- **A returned value's register is a constraint at the RETURN, not at the
  definition; pre-colouring is the optimisation and a copy is the fallback.**
  Pinning the defining operation is what makes an ordinary routine emit no extra
  instruction, but it is not always possible - the value can be an argument the
  caller placed elsewhere, or a tied field, or its register can be busy - and
  after a spill and reload a value is not even in the register it was computed
  in. So `regalloc.f` decides at the terminator: if the returned value is not in
  the declared register it plans an `a64.mov`, and `spill.f` (already the pass
  that turns decisions into operations) inserts it and redirects the return's
  operand. ARM64 has no move instruction - `mov xd, xm` IS `orr xd, xzr, xm` -
  so the encoder is one line in `src/arch/arm64/asm.f` and no caller writes the
  idiom.
- **The allocatable pool is what a routine may WRITE, which is not its destroyed
  set.** `A64EFF` refuses one register in two roles, so a register holding a
  result is deliberately absent from the destroyed set - and a routine that
  could not write its result register could not compute the result. The pool is
  therefore `GPR-WRITABLE` (destroyed plus returned), derived from the contract
  rather than stored, and both the allocator and its validator derive it from
  the contract each is handed.
- **`tools/typed-local-diff-lint.f` only lexes ADDED lines, so a `{:` you change
  whose `:}` is context leaves the group open for the rest of the hunk.** Every
  later added token in that hunk is reported as a bare local. It is not a false
  positive to argue with: re-flow the locals group so the closing `:}` lands on
  a changed line (move one local up), and the lint goes quiet without weakening
  anything.

- **A pass that DECIDES a spill cannot also be the pass that leaves the module
  alone: put the decision in one file and the operations in another, and make
  the wrong order refuse itself.** `src/compiler/native/regalloc.f` now chooses
  a victim and a slot, but a frozen module cannot gain the store and the load
  those decisions are, and a builder cannot gain them in the middle either - a
  block's operations only grow at the end, and a spill store belongs in FRONT of
  the operation that took the register. So the allocator publishes the decisions
  as claims and `src/compiler/native/spill.f` builds the module they are
  operations in; the caller then allocates THAT. The alternative - leaving the
  module alone and having the emitter materialise the stores and loads out of
  the claims - fails the review question rather than the tests: the instruction
  stream would be something no module contains, so the independent validator
  would have nothing to re-derive spills from and would be checking the
  allocator's belief against itself. What makes the two-step safe is that
  skipping the second step cannot pass: an allocation that decided a spill has
  two values in one register (one of them is in a slot for part of its life and
  the module does not say so), and `A64RAV` refuses it under E-A64RAV-OVERLAP.
- **`IR-VERIFY` makes "declares a memory effect" mean "carries a memory token",
  so a store you were about to call pure needs an SSA chain and a second value
  class everywhere.** `EFFECT-CK` in `src/compiler/ir/verify.f` refuses any
  non-pure schema without a memory-token operand or result, which is the right
  rule and is why `a64.reserve` mints a token, `a64.str`/`a64.ldr` pass it on
  and `a64.release` ends it. The cost is not in the dialect, it is in every
  consumer: the allocator and its validator went from "every value is a general
  register" to two classes read off the type, `A64RAV:REG@` had to start
  refusing a token, and the emitter's staleness probe had to stop asking for
  value zero's register because in a lowered module value zero is the token.
  Budget that when adding the first effectful form to a dialect.
- **A frame slot is an ATTRIBUTE, not an operand, and the difference is what can
  be checked.** An operand of the IR is an SSA value - something an operation
  defined - and no operation computes a slot; two operations naming one slot are
  not naming one definition. As an attribute, `IR-SCHEMA` declares the key, the
  freeze verifier proves every operation of the form carries exactly one, and
  the dialect's own checked builder (`A64IR:SLOT-ATTR`) refuses an unreachable
  slot before it is interned. The base register is not an operand either: the
  stack pointer is not a value the allocator may hand out, so the base is a
  property of the form.
- **Two twelve-bit fields on ARM64 are not one bound: a frame slot reaches eight
  times as far as a frame reservation.** `A64EFF:SLOT-REACH` is the unsigned
  offset field SCALED by the access width (4095*8 = 32760 bytes), but the frame
  is claimed with `sub sp, sp, #imm12` and that immediate is unscaled, so the
  deepest frame `A64IR` can RESERVE is 4095 rounded down to the stack alignment
  = 4080 (`A64IR:FRAME-LIMIT`). A slot bound taken from `SLOT-REACH` alone
  passes every test that never builds a frame between 4080 and 32760 - the
  fixture that catches it has to sit in that gap.
- **A frame is a multiple of the stack alignment and a slot is half of one, so
  "one slot short" is not a frame one slot smaller.** `A64EFF:FRAME-CK` refuses
  a frame that is not a multiple of `SP-ALIGN` (16) while a slot is 8 bytes, so
  a one-slot frame is undeclarable and a fixture for "the frame runs out" has to
  make the PROGRAM want an odd number of slots (three slots against a frame of
  sixteen bytes), not shrink the frame to eight.

- **A new keyword in `bootstrap/cg/forth.fs` needs its label allocated in an
  `EMIT-LABEL-*` group, or the engine dies at startup with `hb: snapshot trailer
  corrupt` (79).** The label variables (`variable LKWUSING …`) start at 0, and 0
  is a VALID label id, so `LKWUSING @ LBL,` silently re-places an existing label
  instead of failing with `cg: undefined label`. The image then carries a wrong
  `LSRC` position, and the startup snapshot-presence test (text size vs
  `LSRC`+padded source length) reports a corrupt trailer — a startup error that
  names nothing about keywords. Declaring the variable and emitting its bytes in
  `EMIT-KWDATA` are only two of the three edits; the `LBL LKWFOO !` line in
  `EMIT-LABEL-CONTROL` (or `-CORE` for routine labels) is the one with no
  diagnostic of its own.
- **Compare whole streams in shell gates: the engine's undefined-word diagnostic
  has NO trailing newline.** `if ! IFS= read -r diag < "$err"; then diag=""; fi`
  (the idiom in `tools/bootstrap.sh`'s older gates) turns `BUS-VALUE` into the
  empty string, because `read` returns failure at EOF-without-newline and the
  idiom then discards what it read. A gate written that way passes on
  newline-terminated diagnostics and fails only on the token-only ones. Use
  `diag="$(cat "$err")"`, which strips trailing newlines from both shapes.
- **Fixed engine DATA cells require a LIBRARY-wide offset audit, not just `layout.f`.**
  `layout.f` is not the sole owner: `regalloc.f` (float-pool bitmask), `debug.f`, `lib/task.f`
  (`TASK-USER-BASE`), and `lib/ffi-abi.f` (`FFI-BUF-OFF $3A00..$3C80`) all define cells outside it;
  a documented "free hole" can be a live TABLE's interior (`$260` is `VVAL[2]` of the JIT
  virtual-stack). `grep 'constant .*(OFF|CELL|BASE)'` across `src/ lib/ bootstrap/` AND check every
  RANGED region (table base + capacity) before claiming a slot; free-hole comments rot — correct
  them when a slot is taken or found stale. Runtime spans (source/report/JSONL/capture) use
  OS-backed memory (`MEM-ALLOC-64K-SPAN`/`lib/memory.f`, `$1002` anon mmap), never bigger DATA caps;
  static dict storage + small cells use DATA. Derive the live base through `data-base`, never a
  duplicated numeric address. Evaluate frames + FFI/task scratch live above the full frame area and
  below `DATA-START`; overlapping them corrupts nested `include`/`evaluate`.
- **`evaluate` is a transactional throw boundary (Design-Y).** A throw whose handler is beyond an
  active `evaluate` rolls back each escaped frame (INP/INE/CP/NDICT/XDS/DP + compile state,
  `EVALERR-CELL`=code, `EVALD`--) then reaches the handler; `BTHROW` branches via `EVALREC-CELL` to
  `LEVALREC` when `EVALD>0`. Throws still reach an outer `catch` — do NOT make `evaluate` swallow
  them (the `TTHROWS`/`catch` harness relies on a throw crossing `evaluate`). When an
  error-recovery mechanism is upgraded, migrate EVERY branch to the old entry in the same change —
  the sibling left behind (the LMAIN underflow EVALD>0 leg still using rollback-and-return) is the
  residual bug, fail-open both ways. One throw contract per boundary; re-verify a dot's exact repro
  on the current base before red-first work.
- **Warm snapshots: skip the cold prefix, persist checker stores to DATA, clear transient mmap
  pointers.** A restored image already carries support words/signatures — mark snapshot boot
  (`SNAP-CELL`) and skip the source prefix, or reloaded core words shadow warm support words. `USIGS`
  can grow dynamically but must be COMPACTED into the DATA-resident boot buffer before exec (size the
  persisted capacity for the supported workload). Snapshot writers clear mmap-backed image buffer
  pointers/cursors (`MBUF-A` persisted → crashed the next `IMG-M8`) and reset include state
  (`INCLUDE-BUFS-A` pointed at the baker's mmap → "include: read failed"). Snapshot images relocate
  only engine-text refs (fixed VAs keep dict/data valid). Locate the sole 40-byte
  trailer at the full 64-bit target header value at `IMAGE-TEXT-SIZE-OFF`, plus
  `IMAGE-TEXT-TRAILER-ADJ`, minus 40; never scan for `SNAP-MAGIC`. Validate the
  header-derived trailer fields there and require `region-len + data-len <= trailer
  offset`, so subtracting them cannot underflow the image prefix. Snapshot DATA
  must exclude invocation-mode hooks (`REPLH-CELL` built under a TTY made a later `--load` enter the
  REPL) — canonicalize to zero and recompute batch-vs-interactive after restore; any hook a
  cold-prefix file arms must be explicitly disarmed in the snapshot-zero path unless its warm-boot
  behavior is executed-tested. A snapshot build can carry TWO checker copies (engine cold prefix +
  build payload) — install a fixed first-copy `CHECKER-SNAPSHOT-PREPARE` and invoke it before the
  payload checker prepares.
- **Measure `__text` from the emit cursor (`ASM-LEN`), never gross size deltas.** A zero-byte `SZ`
  probe after each `EMIT-*` gives a byte-exact region map (proof it perturbs nothing: the final probe
  reads true `__text` size). This falsified two "+16KB feature" correlations (the dict-hash HIDX code
  is <1KB `__text`; a "-16KB" was a page-rounding artifact). The 35% `__text` elephant was a
  4×-inlined cold-prefix (`EMIT-COLD-PREFIX` inline at four entry points, per-row append
  char-by-char), fixed by factoring behind `LCOLDPFX` (one BL) + leaf `LAPPPROV` — 148855→99319, byte
  fixpoint held. Emitter helpers called from several `C-*` words are text multipliers (the escape
  decoder expanded inline into all six quote handlers, ~4.1KB) — emit once behind forward labels, and
  callers must re-audit register liveness across the new `BL` (scan count / copy length shared the
  flag register). Keep a PERMANENT emitted-engine region map — historical RCA goes stale (July's
  cold-prefix duplication was already fixed; later growth moved into dispatch/primitives/AOT seed);
  pair a mutable exact-size ratchet with an IMMUTABLE architectural ceiling so a baseline update
  can't normalize growth.
- **A shared native helper that gains a `BL` needs a real LR frame, and a new scratch register
  must be one the helper ALREADY clobbers.** Extending `LCFPOP` to call `LCEMIT` without saving x30
  made `RET` jump back into `LCFPOP` (branch-local defs hung). A cap loaded into x14 (which `LCFPUSH`
  never touched but `J-ELSE` uses to carry the IF-branch origin) clobbered live state → `LPAT`
  dereferenced the cap value; use x12, which `LCFPUSH` already overwrites (provably dead). Before
  picking a scratch reg in a BL-called emitter, grep every caller for save/restore of "unused"
  registers. `data-base` (x20/DATA) ≠ `dbase@` (x26/DBASE) — watch CF-stack memory through DBASE. x20
  is XREG-RBASE (text base) pre-DATA-INIT and DATA base after; x13/x14/x15 carry argc/argv/envp until
  `EM-DATA-INIT` stores them (a boot emitter running earlier must use x12). Values surviving an
  emitted `BL` need explicit stack saves (`EMIT-BCAP` clobbers x16). Dictionary records are
  read-only at runtime (the RX region) — flag writes are engine prims wrapping the store in the LPROT
  RW/RX bracket; Forth-side dict mutation is truncation only (`ndict!`/`cp!`).
- **Mid-compile bridge calls into checker words need an RX window.** The compile loop holds the code
  REGION RW between `:` start and the `;` flush, and the bridged checker words are COMPILED INTO that
  region — a BLR fetches a writable page and SIGBUSes under W^X (signature: pc==x11, SIGBUS,
  mid-body). Bracket find+call with `MOVZ LPROT` (region→RX then →RW). The compile loop's central
  body capture is the `LBCAP` at `EM-COMPILE-KEYWORDS`' head — any dispatch inserted UPSTREAM that
  consumes tokens (an ADT mode) must `LBCAP` them itself or the checked body is silently truncated.
  xts live in TWO regions: baked prims in `__text` at the PIE base; runtime colon/`TRUSTED:` defs in
  the JIT `[DBASE,CP)` region — every real checker hook is a source-loaded def, so `set-check` can
  fail-closed cheaply with `DBASE ≤ xt < CP` (no LIT64), but the window can't tell a true entry from
  any in-range address.
- **AOT compaction/JIT inlining must remap or reject PC-relative branches.** Removing call-stencil
  padding is safe only when every `B/BL`/cond-branch/`ADR` source is remapped and range-checked
  (separate mapper/copy cursors); a byte-copied prim body with an internal branch preserves the old
  target (`epoch-seconds` returned 0) — branch-bearing bodies compile as calls unless relocated.
  Instruction patching flushes the PATCHED line, not `[addr,CP)` (empty range at `cp@` leaves stale
  instructions). Native single-pass ARM64 relocation must validate signed reach BEFORE masking
  (`D26`/`D19`/`ENC-ADRD` silently wrapped out-of-range deltas; the trusted Gforth seed already
  threw via `?REL26`/`?REL19`) — derive the native boundary from the seed via shared
  `?REL26`/`?REL19`/`?ADR` predicates at both the immediate encode sites and the deferred choke,
  before any mutation. Deferred backpatch is per-fixup atomic, not per-chain (`FX-ENC` validates
  reach before the PATCH write, but `LBL` clears the chain head before walking, so a mid-chain
  out-of-reach failure leaves earlier fixups patched). Boundary tests for a reach the buffer can't
  span craft the label position (`ASM-CP !`), don't emit MB of code.
- **Mirrored codegen (`src/habu/` + `bootstrap/cg/`) is one contract, two sources — factor both,
  prove native fixpoint + Gforth recovery.** They may use different assembler vocabularies for the
  same instructions; preserve each file's opcode spelling. A boot-prefix primitive is not implemented
  until BOTH emitters execute its focused behavior (`tok-imm?` existed in habu2.f but not forth.fs →
  native fixpoint passed while no-binary recovery died rc 70). Bootstrap metadata parsing must fail
  closed (`BODY-ARITY`/`EFFECT-FLAGS` rethrow, never default to arity 1 / flags 0). Adding a stage0
  keyword needs THREE coordinated forth.fs edits (`variable LKWxxx`, keyword bytes in `EMIT-KWDATA`,
  the `LBL LKWxxx !` label-init) — missing the third leaves the label 0, binding label 0 corrupts the
  table, and the ONLY symptom is `hb: snapshot trailer corrupt` (exit 79) at generated-engine startup,
  nowhere near the cause. A `LQNL` (newline label in habu2.f) can't be ADR'd from habu1.f code — build
  a fixed message self-contained (`LIT64`-packed bytes on SP), don't reference a habu2.f message-table
  label. A punctuation slip (`BL,` vs `BL`) surfaces only as a terse undefined-token exit in generated
  stage2. Move-wide (`LVMOVK`) refactors need compiled-literal cases (zero, all-ones, MOVZ/MOVK,
  MOVN/MOVK) — top-level parser checks don't prove the JIT materializer.
- **Registry/name growth fails closed at build time.** Process prims overflowed the 96-entry seed
  registry and corrupted the stage image; native primitive additions cross habu1.f's 160-row
  `PRIM-CAP` (`primitive registry full`) — keep named capacities above the emitted count. `EMIT-DICT`
  encodes names > `DNAME-INL` (16) out-of-line (inline-only corrupts `DREC`); dictionary names are
  strings with flags above the length field, one decode path. A seal watermark must be captured where
  the SOURCE ends, not where a file ends (`SEAL-CAPTURE` at xref.f's tail froze the watermark below
  `script-argv.f` loaded after it) — the fix is a second SEAL-CAPTURE token appended after ALL engine
  files + provide rows (capture is monotonic). Don't guard a prim whose legitimate caller is
  indistinguishable from the attacker (the engine refresh legitimately truncates below the watermark
  as ordinary post-seal source) — eliminate the legitimate below-watermark caller first, close the
  spoof one layer up (sealed USIG registry → redefine exits 78).
- **Recoverable engine capacity/reject legs must ROLL BACK every monotonic allocator the def
  touched (CP, NDICT, DP, name bytes) or be documented as leaked.** The hooked publish reject skipped
  the NDICT bump but leaked emitted code (44 bytes/retry) because CP was never rolled back; the pending
  dict record holds the rollback target (slot 0 = post-name, slot 24 = pre-name CP for `DNAME-EXT`
  names). Reject-path matrix: hooked publish (verdict 0) rolls back and continues; trusted publish
  exits via `C-DIE-DOES`; `create`/`variable`/`constant` publish BEFORE `C-DEFHOOK` and discard the
  hook verdict; a throw caught across a compiling definition still leaks CP (dotted). Rejected
  definitions must free every per-definition resource. Exit-status mapping honors deliberate small
  codes: `[1,255]` exits byte-identically, anything else prints `hb: uncaught throw code N` on fd 2
  and exits `UNCAUGHT-RC 67` (the same rule in `die` closed the DRV masked layer); tasked engines use
  `NR-EXIT-GROUP` for process termination (`pthread_exit` is task-local; Linux `exit(93)` terminates
  only the calling pthread and leaves workers alive → process captures time out).
- **Pre-trust defers land via a pending table drained after `: TRUST`, not by reordering the
  checker.** Native `C-DEFER` unconditionally LFINDs `trust`/`checker-defer` and exits 70, so any
  `defer` before their decls killed boot — branch on `C-PRETRUST-READY?`, else copy the qualified name
  + effect sig into a fixed engine-DATA table and replay via `DRAIN-PRETRUST` (an FPRIM + forth.fs
  mirror) right after `: TRUST`. Both name AND sig must be copied (the dict record holds only the bare
  tail; the sig is never in the record). The earliest prefix point a `defer` is legal is AFTER
  `exec-vector.f` (which defines `DEFER-UNSET`), not util.f. The checker-defer bridge needs BOTH
  engine→checker calls (`C-CALL-TRUST-PEND` usig row AND `C-CALL-CHECKER-DEFER` flag); mirroring one
  without the other still rejects `is`. Bootstrap has NO deferred words, so any `defer` in a prefix
  file breaks no-binary recovery until forth.fs mirrors defer/is. Diagnostic origins belong in the
  scanner's SINGLE pass — recomputing line/column by rescanning from byte 0 per definition made
  fixpoint certification quadratic (98% of time on a 1MB/3250-def source); advance byte/line/line-start
  together and snapshot O(1).

## Runtime & REPL

- **A word that `parse-name`s twice hangs under `bin/hb file.f`, not `--load`.** A `TEST:GROUP SEQ|PARA
  name` opener reads two tokens; single-script mode loops on the second `parse-name`, but
  `--load`/`included` (how the gate runs) are fine. Drive rejection paths by calling the factored
  string validators on `s"` literals under `TTHROWS`, not by feeding tokens to the parser at top level.
  DSL keyword names collide case-insensitively (`TEST:GROUP` rejects a group named `seq`/`SEQ`/`PARA`
  with `E-SUITE-NAME` via `STR=CI`) — pick labels outside the reserved set. Locals and package words
  also collide case-insensitively (`ba` shadows a private `BA` → `ba BA !` stores through the local) —
  give persistent state semantic names distinct from every local.
- **Process capture lifecycle has one owner (`lib/process.f`).** Keep fd setup, nonblocking
  probe/drain, stdin write, timeout poll, cleanup, finish there; argv/env/cwd layers prepare state only
  (duplication made every capture variant a stack-juggling audit). Linux spawn needs an exec-failure
  handshake: `clone` success ≠ `execve` success — `PROC-SPAWN-IO` uses a close-on-exec error pipe
  (child writes one byte on chdir/dup2/execve failure; parent reads EOF=success or the byte=fail),
  copy the fd to x0 before reusing that register for the marker byte, else a checked spawn returns a
  pid for a missing exe. `--load` leaves stdin as tool data (a post-load fd0 probe doesn't run — put
  capacity probes in a loaded source file). PTY behavior needs a real pty harness (`script(1)`
  interleaves echo/output) — drive a pty directly and poll for exit. Stdin/Forth byte fixtures: `s" …\n"`
  keeps the literal `\`+`n` — use a byte buffer (`10 c,`) for newlines; `\\` is a different token from
  the comment word `\`. Native crashes need debugger state first (breakpoints, step, data-stack +
  watch cells) before print-marker probes; remove output artifacts before generate-then-run tests
  (stale binaries hide fixes).

## Darwin & Syscalls

- **Raw Darwin syscalls are not libc.** `posix_spawn` (244) takes the private 5-arg kernel ABI
  `pid*, path, adesc, argv, envp`; `wait4` status is `(status>>8)&0xff`; check carry + errno-in-x0;
  initialize ALL expected zero/null arg registers before `svc` (`gettimeofday` returned `EFAULT` from
  stale x2 after a `write`); syscall output buffers use audited DATA scratch, not live `sp`. Spawn
  failure reports carry-set with `x0=errno` (use `LDRW` for the success `pid_t`; `SP` produced giant
  bogus negative pids); prove a missing exe as `-ENOENT`. Process redirection uses XNU spawn
  descriptors (empty file-action blobs are invalid — pass null; mark parent-only pipe/pty fds
  close-on-exec before spawning; PTY = `/dev/ptmx` + ioctl). `F_SETNOSIGPIPE` still needs normalized
  failure (`EPIPE=32` can collide with a valid byte count — normalize carry in the prim). Darwin time
  is not a syscall (`clock_gettime`/`mach_absolute_time` are libSystem/commpage; the no-libSystem clock
  reads `CNTVCT_EL0`/`CNTFRQ_EL0`, converts via quotient/remainder to avoid `ticks*1e9` overflow).
  `LC_MAIN` gets argc/argv/envp in x0-x2 (capture at entry, restore after snapshot boot).
- **Recursive dir walks need per-depth buffers.** `getdirentries64` records are batch-local — index
  dirent buffers, offsets, lengths, cookies, fds by depth (even a global current-record pointer is
  unsafe — the child walk overwrites it). Share filesystem MECHANICS not policy: `WALK-FILES` skips
  repo metadata while `REMOVE-TREE` deletes `.dots` and unlinks symlinks first — factor
  open/read/record/child/close in `lib/fs.f`, keep deletion in `lib/fs-mutate.f`. Symlink deletion
  lstats first (`EXISTS?`/`FILE?`/`DIR?` follow symlinks; broken links look absent — test `SYMLINK?`
  before existence/type). Path syscall tests use stable NUL strings (private path-copy helpers can make
  a smoke fail `EFAULT`, hiding whether the ABI or fixture glue is wrong); same-typed `ptr u8 n` pairs
  (path vs stdin bytes) need an order test — the checker can't distinguish them.

## FFI, GPU & PTX

- **Build IEEE subnormal tie fixtures from the mathematical value, then verify the
  binary64 bits independently.** `3 * 2^-25` normalizes to `1.5 * 2^-24`, so its
  binary64 exponent is the same as `2^-24`; hand-placing it under the `2^-25`
  exponent tests the wrong value and can falsely blame correct ties-to-even code.
- **FFI needs almost no new ABI machinery: an AAPCS64 C-call is a non-leaf `FPRIM`.** It `G-POP`s
  fn + arg buffer, loads x0..x7, `BLR,`, `G-PUSH`es x0; `XDS`=x19 (callee-saved) so the C callee
  preserves the data stack. The CUDA Driver surface (except `cuLaunchKernel`'s 9th–11th stack args) is
  integer/pointer-only, so a float scalar rides in the `kernelParams` buffer — no v-register handling.
  `ffi-call : ( ptr a n -- n )`, fail-closed. Dynamic-ELF FFI is checked emitter code: `bin/hb` is a
  dynamic ET_EXEC (`PT_INTERP`, `DT_NEEDED libc.so.6`, `R_AARCH64_GLOB_DAT` into a fixed-vaddr GOT,
  `DF_BIND_NOW`, no PLT) so ld.so fills `DLOPEN-SLOT`/`DLSYM-SLOT` (`ptr a`, read with `@`); place the
  R+W segment at a FIXED high vaddr so slot addresses are compile-time constants; BOTH AOT
  (`BUILD-ELF`) and snapshot image paths must go dynamic (the self-host fixpoint is the acceptance
  test). no-crt dlopen works (glibc ld.so initializes libc enough that a no-startfiles binary can
  dlopen libcuda). Keep ABI marshalling and loader binding as separate private concerns inside
  the one sealed `FFI` package (`lib/ffi-abi.f`), not as compatibility files.
  A mechanical guard on all 8 FFI arg registers is UNSOUND — ffi-call
  loads 8 cells regardless of arity, so slots past the real args hold STALE values; only `ffi-call-n`
  carries x14=nargs (guard `argbuf[0..nargs)`, BEFORE the x20 repurpose), and the CHECKED library
  funnels integer/pointer calls through it.
- **GPU launch works via the `_v2` driver symbols — the deprecated stub fails.** `dlsym("cuMemAlloc")`
  returns a deprecated stub that returns 201 INVALID_CONTEXT even with the context current; use
  `cuMemAlloc_v2`/`cuMemsetD32_v2`/`cuMemcpyDtoH_v2` (setup/launch non-versioned symbols are fine).
  Use `cuDevicePrimaryCtxRetain` (NOT `cuCtxCreate` — hangs on the Orin's camera primary context); the
  deprecated `cuLaunchGrid`/`cuParamSet*` path (≤8 args) avoids an `ffi-call` >8-arg extension. GPU
  readback args are dst-then-src (`cuMemcpyDtoH_v2 (dstHost srcDevice n)`) — a per-call `rc` print
  localizes a bad readback, and compute goldens by hand (a wrong assertion `2*4+40=48≠44` blamed a
  correct GPU). A retained primary context HANGS AT PROCESS EXIT, not in the kernel — pair
  `cuDevicePrimaryCtxRetain` with `cuModuleUnload` + `cuDevicePrimaryCtxRelease` before `bye`, and
  write launch markers to a FILE (piped stdout block-buffers, lost on a timeout-kill; use fd-2 markers
  to a file). Untrusted GPU launches must be SPAWN-isolated — bare fork is NOT enough (CUDA is
  fork-unsafe after init; a forked child inherits poisoned driver state and misgrades every launch as
  fault); SPAWN a fresh `bin/hb` that classifies the launch under `catch`, `die`s with a small empty-message
  code, and grade a launch FAULT (kernel crashed) as a DISTINCT bucket from WRONG (ran, bad values). On
  device, ptxas itself REJECTS register-discipline bugs at assembly so they never reach the GPU —
  measure per-candidate verdicts on device before pinning tallies.
- **`cp@` is only stable inside a compiled word.** The interpreter compiles each top-level line into a
  transient buffer at `cp@`, so a top-level `cp@ patch32` clobbers the executing line (`SIGILL`); write
  a runtime stub at `cp@` via `patch32` from inside `: WORD ;`. Verify emitted primitive bytes
  statically (compute the exact ARM64 encodings, `grep` the on-disk `bin/hb` for the contiguous stream
  — ASLR slides the xt but file bytes are fixed). Real Triton runs on the Orin (BSP-pinned CUDA 12.6):
  `torch-2.9.1+cu126` matches the 12.6 driver, the generic SBSA wheel has no sm_87 ATen cubins
  (`cudaErrorNoKernelImageForDevice` — Triton JIT-compiles each kernel for sm_87, but pass a custom
  CUDA-event `do_bench=` since autotune's `tensor.zero_()` calls a torch GPU kernel), keep torch to
  alloc + memcpy. Triton catches name/type errors at compile but the stack-discipline class only at
  runtime (a missing store → silent 0.0), where Habu-PTX's checker rejects at author time — that is the
  thesis mechanism, now backed by real-target pass@k data (SAXPY 5/5 both; softmax Triton 5/5, Habu
  3/5→5/5 after diagnostic-guided repair). Triton `tl.dot` on fp32 silently runs TF32 tensor cores on
  sm_87 (`rel_err ~8e-4` fingerprint) — record the arithmetic class next to any GEMM number.
- **v4 vectorization is a pure codegen rep, not a type change — 63 GB/s is the MEMORY ceiling.**
  Scalar `ld.global.f32` (1 elem/thread) → 42.5 GB/s; v4 (`ld.global.v4.f32`, same parametric tile
  types so the SAME SAXPY body certifies) → 63, matching Triton; unrolled grid-strided v4 stays FLAT
  (occupancy 40× saturated, EMC maxed) — you cannot beat the memory system on a memory-bound kernel, so
  "faster than Triton" needs LESS traffic (fusion) or a compute-bound kernel. v4 emit-helper stack sigs
  must use the generic-int token `n` (role names bind as fresh type vars → rejected at `!`). A typed
  vec4 layer can be added ADDITIVELY: register arity-3 `vspan`/`vtile` TFAM rows (like `acc`; needs one
  `install --force` rebuild), `V4-ALIGN ( span -- vspan )` the sole trusted route, put the vspan
  obligation on `LOAD.V4`/`STORE.V4` (where memory is touched) not the ctx — the TRUSTED bodies reuse
  the existing `-V4` emit words verbatim so the typed kernel lowers to BYTE-IDENTICAL PTX (inherits the
  passing device golden for free). Automatic-fusion device win is LATENCY (fewer global round-trips),
  not peak GB/s — report the sum-of-kernel-ns ratio, not a bandwidth delta; the fusion mode must ride
  in the model SOURCE STRING (`; FP-FUSE-OFF!`), not just a parent toggle (the device emit child
  re-plans in a fresh process). A reduction-dominated fusion win is bounded well below the equal-cost-op
  Nx model (layernorm fused-vs-ablated 1.41× not ~3×: the block-per-row reduction dominates, the EW
  epilogue folds in at ~zero marginal cost) — report the honest measured ratio with root cause.
- **TF32 mma.sync: prove the fragment layout element-EXACT in isolation FIRST** (the #1
  "correct in NumPy, garbage on device" bug source). The m16n8k8 tf32 layout (gid=lane>>2, t=lane&3:
  A a0=A[gid][t]…; B b0=B[t][gid] b1=B[t+4][gid]; D d0=D[gid][2t]…) is verified BIT-EXACT vs a host
  matmul with small INTEGER operands (exact in tf32's 10-bit mantissa, sums <2^24 exact in the f32
  accumulator) — any permutation mismatches (`tools/ptx/mma-probe.f` then `mma-gemm-check.f`). tf32
  fragments ride ldmatrix.b16 as half-pairs — a tf32 is 2 adjacent b16 halves, so an 8×8 b16 tile IS
  one 8×8 ldmatrix tile; `ldmatrix.trans` is NOT an option (it transposes at b16 granularity, splitting
  every tf32 — the transpose lives in the STAGING, a scalar coalesced global read + strided shared
  write, since cp.async can't scatter a transpose). mma.sync.tf32 reads the TOP bits of the raw f32
  register, so `ld.shared.b32` with NO cvt is a valid feed (<1 ulp tf32, inside licensed rtol; keep
  cvt.rna where the golden must stay bit-identical). Emit the kernel IN-PROCESS when a runtime `PREC!`
  request must reach the emitter (a child re-builds IR fresh and DROPS the request); share the cp.async
  scaffold via a compute QUOTATION (`MM-PIPE-KLOOP-WITH ( [ -- ] -- )`), not a copy.
- **A transposed-staging ldmatrix lives or dies on its READ bank stride — measure it, and measure an
  optimization AT the layout it needs.** The prior "MMA is not fragment-feed-bound" verdict measured
  the UNPADDED ldmatrix, whose As row stride 128B=32 words aliases bank 0 (16-way conflict) and hides
  the tensor-core win; padding As to a bank-spread stride (`MMA-PAD=8` floats, a multiple of 4 so
  cp.async's 16B chunks stay aligned) made mode-2 ldmatrix jump +54.8% (the swizzle is the lever, BK the
  garnish). bpad sets the ldmatrix read start-bank stride: bpad=4 (stride 36=4 mod 32, conflict-free) =
  3026.6; bpad=0 (all 8 rows alias one window, 8-way conflict) = 1318.5, WORSE than the scalar baseline
  it replaced; bpad must keep the BT row stride a multiple of 16B (ldmatrix.m8n8.b16), and a misaligned
  stride FAULTS the GPU (sm machine-check, not a wrong result) — the emitter enforces it fail-closed
  (`MMA-CHECK-BLDM → E-MMA-BLDM`). B-side ldmatrix over a TRANSPOSED staging cut the residual B-feed
  27%→7% (+11.9%). Element-exactness is the safety net that lets a padded-address rewrite be trusted.
- **A wider M register tile amortizes the B-side feed and re-weights the roofline; re-run the
  attribution after every tile change.** The TF32 mma.sync GEMM was FEED-BOUND on un-amortized B-side
  scalar shared loads (~40%), not mma-issue-bound (the earlier dependency-bound hypothesis overturned by
  DCE-safe ablation — nsys GPU-metrics is unsupported on the Orin iGPU, so ablated kernel variants are
  the profiling method). Each warp owning MFRAGS stacked 16-row M-fragments reuses each B fragment
  MFRAGS times and halves global B staging → past Triton parity (2133.9 GFLOP/s = 1.13×), then B-side
  ldmatrix on the wide tile +11.9%. `num_stages` is tile-size AND occupancy dependent: stages=2 flipped
  from flat (narrow tile) to +2.4% (wide tile), then at MFRAGS=4 single-buffer STATIC (48KiB cap) beat
  double-buffer DYNAMIC by +11.6% (occupancy beats overlap once the feed is amortized) — never assume a
  stages setting carries across a tile resize. Gate every new behavior behind `MFRAGS>1`/`BK>32`/knob so
  all pinned configs stay BYTE-IDENTICAL (capture goldens + cmp before/after twice; the wider tile
  reused the device-proven fragment layout at offset, so only a full-kernel element-exact golden was
  needed); parameterize a shared emitter word (reused verbatim by the FENCED `maki/lower/mm.f`) with a
  byte-identical DEFAULT, don't fork — and emit `shl` when a stride is a power of two, `mul.lo`
  otherwise. The harness must be block-M-aware (a 64^3 check on a 128-row block launches ZERO blocks and
  silently "passes" all-zero).
- **A perf program closes on a ROOFLINE + LEVER-INVENTORY argument, not on hitting 100% of a peak.**
  Derive the iGPU tensor peak from the GPU-ONLY sparse-INT8 TOPS (never the marketing "100 TOPS" =
  GPU+NVDLA), cross-check by reproducing NVIDIA-published FP32 roofs and by a measured kernel rate
  FALSIFYING a candidate peak (a kernel can't beat its roof); the honest verdict when the roofline shows
  headroom but the lever inventory is empty and the instruction shape is maxed (no m16n8k16 for tf32) is
  a documented CLOSE with a user-gated numerics question, not another kernel rung. Build the cheap
  single-variable ablation BEFORE the big rewrite (a negative measured result that redirects two dots is
  a deliverable). A GPU devfreq min=max pin reproduces the shipped 918 MHz within 0.15% (GPU-only blast
  radius, exact restore) to make cross-session rows comparable — the shipped perf row tracks the scalar
  default until the emitted default flips to pad=8 ldmatrix. A whole-model device corruption probe must
  be MAGNITUDE-INDEPENDENT (an activation swap converges to identity for large-positive
  pre-activations — vacuously passes) — use a PTX mutation (operand-base redirect / fma(a,b,c)→fma(a,a,c))
  on a matmul region, and keep the COMMITTED e2e proof the clean PASS (demonstrate corruption in a temp
  copy). A sibling kernel's corruption-probe predicate name does NOT transfer — dump the actual PTX (the
  predicate numbering shifts with region input count). The block reduction is warp-shfl now
  (`shfl.sync.down.b32` takes `%f` regs directly, membermask -1 well-formed since blocks are warp
  multiples).
- **ncu on the Orin NX likely HARD-HUNG the box on first attach — treat Nsight Compute as a
  device-risk operation.** One `sudo ncu -k … --launch-count 1` printed "Connected to process" and never
  produced a section; within minutes zed dropped off the tailnet (18+ min, physical power cycle required).
  Prefer `nsys` GPU-metrics sampling or variant-kernel timing decomposition; if retrying ncu, use a
  minimal single section, a tiny shape, and expect to lose the box.
- **PTX collectives / adjoints need op-local inactive identity and address-space tokens.**
  `cg-collective.f` writes `active ? tile : identity(op)` before the shared fold (`BLOCK-MAX` -inf,
  `BLOCK-SUM` 0) regardless of how `ROW-LOAD` seeded inactive lanes — direct row-sum/backward can't rely
  on softmax's accidental `EXP(-inf)=0`. Read-once is a distinct address-space token (`space-global-once`
  + `LOAD-ONCE`/`STORE-ONCE`), never a cast from ordinary `span<space-global>`; LOAD adjoints default to
  `SCATTER-ADD` (plain store is gated by an affine/read-once proof); indexed memory carries two extents +
  a uniqueness token (`idxctx` → `INDEX-SCATTER-ADD`; plain `INDEX-STORE` needs `uniqidxctx`). Relaxing a
  fail-closed class re-audits every site that leaned on the old rejection (unlocking BC-COL needed a NEW
  input-0-must-be-FULL guard and exposed BC-ROW/BC-SCALAR already silently mis-loadable) — grep every
  consumer of a classification for positional/shape assumptions the old set masked. Generated
  multi-input elementwise kernels reset CG counters from the input COUNT (`CG-NRD=K+2`), not `CG-RESET`
  (which presets the 2-span SAXPY ABI). A composed-Gemm DEVICE golden must pick a form whose matmul
  epilogue is empty or a unary activation: transB becomes a standalone materialized transpose region
  (device-emittable), but OP-SCALE/OP-BIAS fuse as epilogue nodes and `LMM-EPI-OP?` accepts ONLY
  relu/gelu/silu (alpha≠1 or a separate bias rejects fail-closed `E-LMM-OP`). Kernel benchmarks need a
  generic launch layer + device elapsed time (`cuEventElapsedTime` `gpu_elapsed_ns`), not host-loop
  timing called a GPU profile. `.f` emit-driver signatures use checker type tokens (`n`/`ptr`/`bool`),
  never descriptive names (`node`/`rid`).
- **Device-vs-host GOLDEN compares device f32 against the f32-NARROWED host, not the raw f64.** The
  host runs f64, the device f32 — round the host elem onto the f32 grid first (`F32:NARROW F32:WIDEN`) then
  `|dev - host_f32| ≤ atol + rtol*|host_f32|`, else the dtype step folds into the error budget and blows
  the tol. Since no onnxruntime exists for a composed Gemm, the committed host-executor result (validated
  ==ort at 1e-5 on the pure-matmul fixture) is the oracle — device-vs-host discipline, ort leg a
  documented residual. Device goldens for hand-built (non-capturable) IR feed the child driver via a
  shared checked source-text builder the parent also EVALUATEs (one source of truth; the off-device load
  already validates the child text).

## Linux AOT / ELF

- **Linux AOT gates parse ELF structure** (inspect ELF64 program headers, validate the executable
  `PT_LOAD`); Mach-O text-size thresholds aren't portable. Instruction disassemblers read instruction
  WIDTH (`DISASM` loads one ARM64 u32; a u64-load-and-mask can cross a 4-byte mmap fixture end). The
  maker `__text` sits at the `MPAGE` wall (engine text + full baked compiler source, fail-closed at
  `MPAGE - CODE-OFF`; on master exactly at the cap, zero margin) — any net compiler-source growth (even
  +131 bytes) fails the AOT gate `macho: code exceeds __TEXT page`; prove causality with an A/B maker
  build, fix is capacity or source diet, not shaving to squeeze under. The maker CACHE masks builder
  overflows (rebuilds only on content-key miss, so a nearly-full CODE buffer stays green until any
  engine-source edit) — when growing src/core check maker `__text` vs `CODE-CAP-WORDS` (icode.f) AND
  `MPAGE` (macho.f/elf.f), keep both guards aligned. `AOT-LINK` must start on a fresh line (a final
  `\`-comment without a newline swallows the sentinel → maker exits 0 without the artifact). `die`
  modeling belongs to the Forth standard (the Linux AOT failure was treating `0 0` as a fake string).

## Diagnostics & Benchmarks

- **A matching diagnostic string is a lead, never an attribution — reproduce on
  the exact engine and path before you inherit someone else's dot.** The
  pre-trust-defer red printed `hook: non-certified definition: install at 'is'`,
  the verbatim signature of the Gforth stage0 mirror replay defect written up in
  `docs/debugging.md` and owned by dot `habu-fix-stage0-pre-88a4297e`, so the
  red was filed as that defect. It is not: the whole failure reproduces under
  the NATIVE engine on a plain `bin/hb --load` child boot, with no gforth and
  without `tools/bootstrap.sh`. That message is the generic consequence of a
  pre-trust deferred word that has no checker rows, and both engines emit it.
  The falsification that settled it took one run: copy `src/` to a private root,
  patch the copy, boot a child with that root as its working directory (the
  engine re-reads its prefix from source at boot), and see which engine is
  actually speaking.
- **When an opaque exit code names no site, mutate one candidate site's code to
  a unique number and re-run on the real load path.** Exit 67 with
  `hb: uncaught throw code 7136` says only that `E-PKG-CONTEXT` escaped, and
  `src/core/checker.f` throws it from nine places. Copying `src/`, changing
  exactly one of them to `7911` (padded to the same byte length so no offsets
  move) and booting a child engine against the copy printed
  `hb: uncaught throw code 7911` and pinned the site to `checker.f:634` in one
  step. Cheaper and more certain than reading nine call paths, and it works for
  any engine constant that reaches stderr.
- **Diagnostics are an API.** JSON errors carry `schema_version:1`, source spans, verdict, word,
  token, expected, actual; wrappers keep valid JSON object lines and fail nonzero on rejection. Source
  origins are wrapper-owned (definition-relative spans; inject origin markers, keep them out of user
  bundles). Repair/diagnostic rows keep a source-preserving effect field beside the normalized one;
  `fix_return_stack` only when the data stack already matches (a bad `>r` dropping a declared output is
  `add_producer` first). Repair packets are mechanically consumable in checked code (`DIAG-BUFFER!` +
  `DIAG-JSON!` + `DIAG-ORIGIN! 1 1 0` around `CHECK-CANDIDATE!`, no TRUSTED unless mutating `DIAGXT`):
  the checker flags the LAST surplus producer for `remove_producer`, an unconsumed-input surplus flags
  `token_index 0` (the name token — guard it or a mechanical editor deletes the name);
  `add_producer`/`fix_type` carry no replacement token → report honest UNREPAIRABLE. Repair diagnostics
  are a SUM of evidence shapes, not one nullable mega-record (dispatch on shape, preserve its required
  evidence, never fabricate the other variant's fields). Changing a checker failure CLASS also changes
  its complete origin — repin token text, index, and byte span together, or a repair code identifies an
  earlier token. Check phases must be SILENT (any stdout/stderr from a check-only child = rejection to
  live drivers); expected-throw fixtures stay quiet. A compile-preflight diagnostic must enter the
  canonical `CHECK!` renderer (a bespoke short JSON drops spans/effects/repair-compat) and pin an
  unmodeled immediate as `E-UNMODELED-IMMEDIATE`; never exit a check hook bare (verdict-1 with no DIAGXT
  threw rc 70 with zero diagnostics).
- **Error codes are a global namespace — lint uniqueness, claim a fresh block in the file header.**
  Live collisions (E-CUDA/E-FUSE both -5002, etc.) slipped past review; `error-code-lint.f` fails any
  negative code claimed by two `E-*` names (allowing sysexits-style positive exits, range sentinels,
  same-name re-registrations). Keep older widely-used codes stable, renumber the newer claimant; a bulk
  `E-*`→`ENGINE-ERROR:*` migration must EXCLUDE its replacement spelling and end with an exact
  legacy/near-miss scan (a non-token-exact match corrupts the new spelling). Removing a satisfied class
  error is fine only when it had a single runtime reader.
- **A benchmark reports axes SEPARATELY, deterministically, from evidence.** Trial pass, task pass@k,
  repair rounds, wall time, generated-token cost (a proxy, not hidden reasoning); reports are
  evidence-derived text with no wall-clock stamp (provable with `cmp` before archiving outside git);
  Habu-only vs cross-language claims use distinct artifacts. Model-driven pass@k needs an INDEPENDENT
  stochastic generator we don't curate (curating the bug distribution makes the number a construction)
  — used independent Claude subagents (k=5/task) graded through each target's full loop; the
  differentiator is failure MODE (every Habu failure was an author-time static reject with a located
  diagnostic, zero GPU). Log honest caveats (a softmax gap confounded by my own prompt mis-spec of
  arg order; Triton produced no failures to repair so repair-rounds isn't symmetric). Phase-token task
  surfaces make pass@1 near-trivial BY DESIGN (the checked phase words admit one type-correct order) —
  say so; the emit-level gates catch only SEEDED bug shapes (same-type role swaps, dead-value stores,
  double-phase kernels retain every required token and grade GREEN) — only a device numeric golden
  closes that class, so pin those wrong-but-green shapes as acknowledged regressions. Live sweeps
  resume + enforce coverage by identity `(model_id, arm, task_id, trial)`, record expected identities
  during the run, fail before report generation if any row is missing (use `*-OUTCOME` process APIs so
  a timeout still emits a row). Record token metrics in the unit you HAVE next to a slot for the unit you
  want (estimators belong in the replay engine, not the recorded artifact). Codex candidates come from
  `--output-last-message` (stdout `--json` is event streams for token accounting only) with `--cd` to a
  clean temp dir + clean `CODEX_HOME`; background scouts need explicit stdin `/dev/null` + `--output-last-message`.
- **Advisory soundness findings ROT — and can be born rotten.** A prop-test metamorphic amplifier was
  100% inconsistent from introduction (a broken harness contract, not N distinct misses) yet the gate
  stayed green because inconsistencies were "(logged, non-fatal)" and shards mute output — a property
  tester that prints findings and exits 0 is error masking; make the counters FATAL at the summary, and
  a 100% failure rate on a metamorphic leg means the CONTRACT is broken (probe the contract word directly
  before shrinking N "different" cases). Stateful scanners split at cursor phases, delegating
  advance/digit-run/ratio/keyword to typed helpers, with fixtures around the boundary. Report
  reducers use DEDICATED scratch cells (`RR-I/J/K` get clobbered by nested helpers; a `RR-RATIO.` stack
  leak truncated a table) — add row-count regressions and `cmp` regenerated reports. Doc-contract
  fixtures need stable anchors (line wrapping hides a `grep -F` phrase — assert a shorter contiguous
  substring). Dogfood benchmark hot paths (per-call glue is Habu-native; host parsers hide missing Habu
  primitives) and match LLM helper surfaces to validator surfaces exactly.

## Generated-Code Verification & Signal/Async Effects

- **Host stack effects do not certify generated MACHINE state.** An emitter can be stack-correct while
  its output clobbers a live register, flags, frame slot, SP, or a caller-owned buffer — give emitted
  operands and callable routines first-class effects, then verify liveness/frame invariants over the
  resolved CFG (handwritten name-to-mask tables are transitional diagnostics only; raw
  write-before-BL/read-after-BL rules are mostly false). An emitter's forward LABEL names the code
  SKIPPED to, not the body adjacent to the conditional — record every condition combination and map each
  original path to the shared label before rewriting layout (inverting a hooked-publish dispatch
  condition broke the refresh child). Prove emitter branch SHARING with a runtime truth table. Concurrency
  claims need EMITTED-code proof (single-thread behavior can't distinguish acquire/release from ordinary
  loads/stores — scan the live routine, pin LDAR/STLR order); model them as named assembler operations,
  not instruction words at call sites. Generated-state proofs need a BACKEND-specific last mile — ARM64
  CFG/register verification does not cover PTX (bind virtual def/use, predicates, barriers, address
  spaces, `ptxas` facts, cubin/SASS identity, and device evidence separately). PTX declarations and
  `ptxas` observations have DIFFERENT authority (declared params/registers/spaces vs proprietary
  stack/spill facts — the latter belong in a content-bound attestation).
- **Signal handlers need asynchronous-entry effects, not ordinary call effects.** The kernel supplies
  target-specific live registers + a ucontext frame; the handler may edit saved PC/SP and terminate via
  `sigreturn`/a no-return syscall — a BL-routine contract can't prove that boundary; type its frame,
  allowed operations, reentrancy, and terminator per target. A `catch` effect is sound only if runtime
  restores the WHOLE typed frame (data SP + machine SP is insufficient when the checker also promises
  return/loop-stack preservation). Balanced stack effects do not prove BOUNDED execution (a word can
  return every stack to its declared row while recursion retains a live return value and grows without
  bound) — pair runtime extent guards with compositional peak-use certificates. A representable
  by-value bound is not a semantic COMPOSITION bound (a 127-cell effect row fits `ER.MINI` but carries
  only 25 exact bindings and fails ordinary repeated composition) — large canonical typed sets need
  opaque nominal handles over sealed immutable content; handles/offsets/order/hashes must never become
  authority or identity. Do not shrink a semantic capacity around checker OVERcounting — fix
  `ROW-CELLS`/effect recording; a smaller row hides the compiler defect. Proof/replay tables are
  ARENAS, not policy caps (grow the producer tables, validate byte arithmetic at the immutable consumer
  boundary; compiler replay evidence must be immutable, source-byte-keyed, consumed exactly once). A
  guard row must certify both the expected tag AND its domain (`{offset,tag,limit}`, `0 ≤ tag < limit`).
- **Structural ADTs are UNTRUSTED until a validator establishes semantics.** A public PRODUCT
  constructor proves layout + field roles, not mask/ordering/bound/cache-legality — validate every
  parser/artifact/persistence/FFI/registry ingress before any permissive decision. A raw boxed pointer
  needs bounds metadata at the ALLOCATION boundary (store capacity in a hidden preceding cell, reject
  both signed bounds before address arithmetic, prove the rejected write leaves its neighbor intact).
  Typed storage with untyped accessors does not preserve semantic roles (an `n` converted independently
  at a dtype vs layout store satisfies either when tags overlap) — carry the family through internal
  APIs, convert only inside named wire/table-index owners. A typed layout pointer needs a GENERATIVE
  introduction boundary + fetch-time validation (a sealed allocator owns count/stride/zero-image/bounds,
  builds its generated source before allocation so generator failure can't leak DATA, and every fetch
  validates active tags; erase every one-shot arming word + backing cell after compiling its direct
  callers, or a globally-callable arming surface is an unchecked cast). Seal an OWNER only after all
  constituent files load (sealing blocks reopen; keep raw→nominal mints private+audited while assembling,
  seal in the final slice); protecting publication does not protect package REOPEN (package lookup must
  reject every protected WID). A recovery/guard vocabulary must never leak into the ambient search order
  (a private `BAND` helper shadowed the instruction emitter and crashed stage0) — balanced local
  `also`/`previous` at each call site.

## Friend-Arena Seals & Boot Protection

- **Make checker/wordlist state unforgeable from user source with a runtime RANGE GUARD at every raw
  write SINK, not name-hiding or type-provenance.** Only the sink sees the real target address, and
  `data-base <off> + !` computes it with no engine-word name. Layout: relocate the crown-jewel cells into
  ONE contiguous DATA band BELOW `DATA-START` (so `allot`/`,`/DP, bounded ≥ DATA-START by DP-CHECK, can't
  reach it by construction). The latch cell IS the band base (0=open, band-len=sealed); the guard reads
  it, so post-seal any write into the band (including the latch) traps — one-way self-sealing. The seal is
  emitted by the cold-prefix generator (after `PFX-PROVIDE-FILES`) so it fires before the first user token
  on EVERY entry; engine writes to the same cells use dedicated `DATA <CELL> STR,` prims, never `!`. Don't
  guard a prim whose legitimate caller is indistinguishable from an attacker — the checker hook lives in
  the JIT `[DBASE,CP)` region, so `set-check` fail-closes with a two-register range compare. Native seal
  widenings do NOT propagate to the stage0 mirror by themselves (pins count what IS, not what SHOULD be) —
  re-pin `SAB-GUARD-PINS` red-first, mirror the emission, and prove parity by forging the CHECK_ONLY seed
  (`HABU_BOOTSTRAP_CHECK_ONLY=1 tools/bootstrap.sh` + pipe `data-base <off> + cp!` forges → rc 83/0
  against the actual stage0 engine); mind forth.fs top-down file order (a newly guarded early sink needs
  PROT-GUARD moved above the primitive bodies). A protected table's ADJACENT control cell belongs to the
  same guarded band (extending the protected-WID table left `UNCGH-CELL` writable → a user store could
  redirect the uncaught-throw branch); a sealed cell goes into an rg-verified reclaimed hole with its own
  guard band, never grows an existing band whose end is a public boundary. A `TRUSTED:` body does NOT
  bypass sealed-store guards (raw `!` into the protected preflight cell exits 83; the engine-owned
  `set-preflight` prim is the mutation boundary). A custom checker hook is a paired lifecycle
  (`LOWER-CERT-HOOK:INSTALL` first, then `set-check`) — install AFTER internal-mark (a top-level
  `0 set-check` suspends a token hook whose re-arm lives in a word body).
- **Anything that rides the AOT seed pass (`EM-SEED-AOT` at LEXIT) is invisible to BATCH programs.**
  Batch input (piped stdin AND --load) is consumed by the pre-LEXIT interpret loop, so AOT-seeded words
  (BP., stepper) and AOT-restored state (the protected-WID registry) exist only for post-seed interactive
  sessions — probe with an AOT-seeded word (`BP.` E-UNDEFINED = pre-seed). Boot-restored DATA with no
  name-relocation dependency belongs in `EM-STARTUP`, not the LEXIT seed. `bin/hb` bakes ONLY primitives
  (`EMIT-DICT` bakes the #PL registry) — a generated `constant`/checker word is re-parsed source, so
  "bake a digest and verify at startup" needs a metacompiler baked-data capability (an injected boot
  token died E-UNDEFINED on every boot and bricked self-rebuild); a fail-closed-by-default boot check
  deadlocks self-hosting (default off/warn, strict opt-in). The AOT sample/REPL word must be defined in a
  driver loaded ONLY for the stdin build (never habu2.f, reloaded in every build including the stage2 an
  AOT-seeded engine runs to rebuild itself → duplicate definition). Open-address dictionary indexes must
  RECLAIM stale rollback slots on INSERT (not only skip on lookup), or repeated checked candidates fill
  the fixed table with dead entries and spin. Owner WID numbers are generation-local — canonical package
  IDENTITY is the persistence authority (a refresh retires+rebuilds packages with new WIDs, so snapshot
  capture rebinds the baked AOT name frame); canonical-base snapshot rebasing classifies package records
  before pointer ranges (a raw public/private WID at text base 0 looks like a low text offset — skip
  record `[0]/[8]` when `[40]=-1`, still rebase an external name pointer); scrub only the exact RSTK
  extent (zeroing `RSTK-OFF..DATA-START` erased persistent protected/owner registries in the shared
  reserved band). AOT-REPL M2: the metabuild host and `bin/hb` are DIFFERENT engine shapes
  (`STDIN?`-branched — a big C-SOURCE-BAKED 585KB host vs a small C-SOURCE-STDIN 113KB bin/hb), so you
  cannot capture the REPL in the host and base-rebase it in (6.7% byte-identical, non-uniform chunk
  shifts, every word call an absolute movz/movk/blr `LSNAPRBC` can't remap) — capture in a small
  STDIN?=true engine.
- Nested named ADTs needed NO new lowering machinery: the only blocker was the
  flat-only gate `TFC-CON-FLAT?`; replacing it with recursive width-stability
  (`TFC-CON-CLOSED?`, no open type var in the arg tree) flips nested end-to-end
  under the SAME `WF-XPAD` extra-pad model (inner bundle pads at its own site,
  outer adds only its delta). No emitter edit, no new WF fact kind.
- Linearity must be TRANSITIVE through nested layout args: `option<lq2<ltok,n>>`
  dup-laundered the buried `ltok` (NL-DUP was ACCEPTED) because the arg-linear
  tests only saw a direct con/var, not a nested T-PARAM. Recurse
  `LAYOUT-ARG-LINEARISH?`/`LAYOUT-ARG-LIN-N`; keep the count accumulator on the
  STACK (shared `LLC-N` corrupts under recursion).
- Never add a pre-trust `defer` to src/core/checker.f: the tree declares ZERO
  (checker.f:7947) and a non-empty pre-trust pending table on a drain miss trips
  the SEAL-CAPTURE backstop (gate-stdlib pre-trust-defer, exit 73). Use RECURSE
  for forward recursion instead.
- Confirm an adversarial rejects for the RIGHT reason (dump the diagnostic):
  slice-3's linear-payload reject was a false negative — `own` was an UNKNOWN
  type in that suite, not a linear violation.
- Delete bookmarks BY NAME only (`jj bookmark delete <name>`), never with a
  broad `--deleted` push sweep, and inspect a conflicted bookmark's heads
  before dropping it: `sol-fields-add-shared` held a real unmerged engine
  commit (option promotion) that a blind cleanup would have discarded, while
  `maki-layout-valid` held only stale snapshots of work already on master.
  Diff each head against its parent and check the touched files exist on
  master before deciding recover-vs-drop.
- A fresh gate workspace has no `bin/hb` (gitignored), and `test/run.f` spawns
  `bin/hb` children by relative path — the whole battery reds with spawn
  failures that look like real regressions. Install (or copy) the engine into
  the workspace BEFORE gating, and diagnose a surprising all-red battery by
  re-running one suite with output visible before blaming the tree. A lane
  whose diff is a multi-commit stack must be rebased with `-s <stack root>`,
  not `-s <tip>` — tip-only rebase orphans the base and manufactures conflicts.
- A fail-closed toolchain resolver needs per-host-class test assertions. The
  ptxas resolver was correctly changed from a silent dead-path default to a
  named throw, but the tests kept asserting unconditional resolution — green
  on the CUDA dev host, red on the CUDA-less Mac gate host, red master for
  every other lane. When a resource probe can legitimately differ by host,
  the test must assert BOTH contract branches (present → path; absent →
  named throw), never assume the dev host's branch.
- The installed (pre-fix) dots CLI can destroy a dot file: `dot on` against a
  freshly added dot left only the appended claim line — frontmatter and body
  gone. Until the rebuilt zig-0.16 binary ships, prefer editing .dots files
  directly (set `status: active` in frontmatter) and verify with `dot list`
  plus dot-dep-lint after every mutation; never trust a dot-CLI rewrite
  without re-reading the file.
- A concurrently-pushing session can retire a language word out from under an
  in-flight lane: between a worker's green commit and its merge, the other
  session deleted `deftype` (retired onto `NOMINAL:`), which a new test fixture
  used. The merge train caught it only because every fetch is followed by
  re-reading what the moved base actually changed and re-reviewing worker
  commits against it. After any fetch that moves master, diff the new commits
  for surface removals (words, primitives, renamed files) before gating, and
  send affected lanes back for migration rather than patching their diffs in
  the gate workspace.
- Never chain gate commands through a pipe inside an `&&` guard: `bin/hb --load
  gate.f | tail -1 && jj git push` pushes on TAIL's exit code, not the gate's.
  This exact pattern pushed a red master (two suites failing at once)
  because two red gates printed their tails and the push ran
  anyway. Run gates bare and check `$?`, or `set -o pipefail` before any
  gate-then-push chain; the merge command must be structurally unable to run
  when a gate is red.
- Overwriting a signed running binary in place (`cp new bin/hb` onto the
  existing file) gets the next execution SIGKILLed on macOS arm64: the kernel
  caches the old code signature by inode. Remove the target first (`rm bin/hb
  && cp`), or use the engine's own install path, which signs via
  lib/codesign.f. An orchestrator refreshing a worktree's engine from another
  workspace's proven fixpoint binary must rm-then-copy, never overwrite.
- A jj working copy can go stale silently mid-sequence when other workspaces
  create operations: `jj new <rev>` may report success while the on-disk files
  still hold the previous tree, and an expensive step then runs against the
  wrong sources. This produced a fixpoint "proof" that exactly reproduced the
  PREVIOUS engine hash (the giveaway) and a gate run whose path count
  matched the old tree. Before any expensive gate or install, verify the tree
  by CONTENT — a sentinel search for a string the change introduces (e.g.
  `rg -c PROT-GUARD:CALL src/habu/habu1.f`) — not just by `jj log` position,
  and rerun `jj workspace update-stale` after any cross-workspace activity.
- The .jj-ws/ workspace-checkout root vanished from disk mid-session (2026-07-19),
  taking every workspace checkout and seeded engine binary with it. RCA facts:
  no session command or worker command ran a matching rm (transcripts searched);
  no commit anywhere tracked .jj-ws paths (verified per-commit), so a jj checkout
  could not have deleted it; cause remains unproven — treat unexplained
  filesystem loss as possible on this shared machine. NO durable loss: all held
  tips and recover-* bookmarks live in the jj store and on origin, and the
  engine byte-fixpoint rebuilt identically from a rescued gate-pool binary.
  Countermeasures now standing: .jj-ws/ is gitignored (it was untracked-but-not-
  ignored — one bad snapshot away from being swallowed into a commit);
  workspace checkouts are treated as disposable and recreated from the store on
  demand; a rescue engine copy lives outside the repo; and any workspace whose
  directory disappears gets its registration forgotten promptly so stale wc
  pointers cannot confuse later operations.
- **Validate cleanup targets before destructive calls.** If setup fails, stop
  cleanup. Move, trash, or delete only an explicit, nonempty, validated,
  resolved, bounded path. Unsupported `jj diff --check` skipped temporary-path
  assignment, so `gio trash ""` moved the repository root; Trash restoration
  preserved every Jujutsu workspace.
- Clobber-lint was structurally blind to wrapped emitter calls: a `PKG:CALL`
  macro (PROT-GUARD:CALL) is neither a bare mnemonic nor a `LABEL@ BL,` triple,
  so its emitted branch-with-link was never counted, its register-move plus
  guard-body clobbers were never modeled, and the link register was never
  poisoned. The lint now models the guard contract, folds it into the
  transitive-closure pass, and fails closed (named negative code) on any
  `:CALL`-shaped token it does not model, so a future wrapped call cannot be
  silently skipped. When a new emission *shape* is introduced, audit every
  text-scanning lint for that shape the same day it lands.
- A perf-band verdict on a contended machine measures the environment, not the
  tree. With an external process (UnrealEditor) holding two to three cores, the
  identical suite that passed in-band at load 4 failed marginal-fail with
  correctness green at load 8+ on BOTH the merge candidate and the unmodified
  baseline clone; the verdict's own empty=f/admissible=f fields flagged the
  contention. Before treating a band failure as a regression (or stalling a
  merge indefinitely), rerun the same suite on the last in-band-passing tree as
  an A/B control: merge only when correctness is green on the exact candidate
  tree, every owning gate passes, and the control proves the band failure
  environmental. Re-verify the band on the next quiet-machine cold run.
- Never append `|| true` to any investigative command, even a supposedly
  optional probe such as `readlink`. It converts failure into shell success and
  makes the reported exit status false evidence. Run each probe alone, preserve
  its real status, and inspect optional artifacts only after recording failure.
- **The unified payload-bearing `ENUM` and `STRUCTURE` surface is specified, not operational.** Current master still exposes payloadless `ENUM`, payload `SUMTYPE`, and `PRODUCT`; `STRUCTURE` rejects as undefined. New design dots target the hard cutover, but implementation must wait for `habu-type-dsl-prove-93da83c4` or be absorbed into its exact migration owner.
- **Container size steps do not measure feature code.** A 16 KiB macOS file jump can be page padding triggered by a much smaller `__text` increase. Attribute exact emitted regions and `__text` before blaming one feature or approving a ratchet.
- **Dormant at startup is not dead.** Profiler, cross-reference, debugger, and REPL words are product features when users can invoke them later. Measure their resident cost and privatize helpers, but require reachability evidence before deleting them.
- **Parallel arrays turn one logical row into several fallible commits.** Same-cell column swaps still certify, and a throw between stores leaves torn state. Prefer one checked `STRUCTURE`, preflight every backing arena, and publish the row last.
- **A long prefix is not package ownership.** It spends dictionary bytes while leaving scratch state and helpers globally callable. Package legacy subsystems, shorten private tails, and retain only the measured cross-package API.
- **A shared scratch cell binds two modules even when only one reads it.** `tools/lint/lib.f` declared `PSTART` and only the source lexer ever used it, so packaging the lexer had to give it a private cell — but the declaration cannot be deleted in the same change, because `tools/lint/lib.f` has no package owner and the exact-diff package gate rejects every touched definition on that line. Give the borrowed state its private home first and hand the now-unused declaration to the owning module's packaging dot; do not touch an unowned file's definition lines just to tidy up.
- **An authenticated manifest can still be incompatible.** Validate target capability, address width, complete argument layout, launch geometry, and overflow before allocation or module load; signature or digest checks prove identity, not launch safety.
- **Resource state and cleanup ownership belong in one typed lifecycle.** Publish a live handle only after acquisition succeeds, consume it exactly once on close, and keep primary and cleanup failures distinguishable; split flags and sentinels admit leaks, double close, and stale reuse.
- **A committed transaction frame is sealed until finalize or rollback.** Reject nested open before token generation or mutation once a parent has committed; otherwise child publication invalidates the parent watermark and can strand the sole rollback owner.
- **A long audit must rebase and revalidate its evidence when master moves.** File lines, ownership dots, size baselines, and defects can all change underneath a frozen census; resolved findings are removed, surviving ones are rewritten against the exact publish tree.
- **Calling a positional cell array a record does not make it typed.** If fields have roles, use one `STRUCTURE`; if cases carry different fields, use one payload-bearing `ENUM`. Generic field numbers preserve every swap and bounds bug the new type surface exists to reject.
- **Multi-resource initialization publishes one state.** Several fallible allocations followed by several pointer/vector stores are not a transaction. Build an owned aggregate off to the side, unwind completely on failure, and publish one `ready(payload)` variant only after every resource is valid.
- **Coverage gates must not reserve speculative exclusions.** A nonexistent future path in an allowlist is a silent hole waiting to become production code; discovery should schedule every in-scope module and force its owner to make the gate pass.
- **Delete a measured-losing unreachable experiment before polishing it.** Preserve the measurement and product decision, but do not retain emitter, runtime, checker, buffer, and configuration branches merely because the experiment was implemented correctly once.
- **A nullary event tag does not own ambient payload.** If parsing another line can overwrite fields associated with a saved event, return one payload-bearing `ENUM` value whose payload owns or stably borrows every field needed to interpret it.
- **Borrowed parser spans cannot outlive the promised buffer lifetime.** Retaining path or header pointers across later line calls requires owned metadata or offsets into one immutable whole-source owner; stable test literals can hide reusable-buffer corruption.
- **Valid zero length is data, never phase control.** Use an explicit mode or state tag for absence; every production phase must accept an empty owned value instead of letting an early size check bypass validation.
- **Importing a library must not run a benchmark.** Device allocation, reference computation, assembler work, process probes, timing, and report output belong behind an explicit command entry, while reusable packages load without side effects.
- **Benchmark failure classes are data, not booleans.** Infrastructure failure, foreign-process contention, unstable clocks, and an inexact kernel require distinct payload-bearing `ENUM` outcomes; collapsing them to `-1`, zero, or false produces confident false diagnoses.
- **Invariant reference work belongs outside candidate loops.** Prepare shape-dependent fills and an O(n^3) golden once, cache dtype-dependent packing once, then measure only candidate-dependent upload, launch, and comparison.
- **Consume linear authority only after validation succeeds.** A teardown completion that consumes its sole token before checking drained state strands live resources when it rejects; either require a statically drained state or return the still-owned authority in a payload-bearing `ENUM` result.
- **Exact size ratchets are target-specific composed history.** A source primitive landed after one platform was measured makes that platform row stale even when another platform was remeasured; every affected target needs a fresh fixpoint attribution before publication.
- **Accepted syntax is not implemented behavior until lowering consumes it.** A parser and attribute test can prove metadata round trips while generated code ignores it completely; every semantic tag needs an end-to-end emitted-code and runtime golden.
- **A zero-filled lookup table is not initialized state.** Validate readiness and bounds before computing an address, and publish one typed ready value only after every forward and inverse table is complete.
- **Store bounded identifiers in their canonical compact form.** Expanding byte token ids into float cells for an entire corpus used eight times the memory even though only small sampled batches need float conversion.
- **Test paths are owned resources.** Predictable shared `/tmp` names race, leak, falsify absence tests, and permit symlink truncation; use unique private roots and exception-safe cleanup.
- **Never copy a performance bound across targets.** A Spark-derived cold budget was lower than a directly observed macOS suite long pole; keep the last measured target bound until repeated exact-target runs prove a replacement.
- **Compile-once helpers must not become permanent dictionary residents.** If a generated checked word exists only to capture one value, use a checked anonymous or transactionally reclaimed compilation boundary and prove every compiler registry rolls back.
- **One suite needs one canonical inventory.** Repeating membership across a full loader, slices, and runner dispatch creates more reconciliation code than the split saves and lets each copy drift independently.
- **A frame must bind its declared identity to the parsed payload.** Validating path syntax and body syntax independently still accepts path substitution, presence/status contradictions, and several raw files under one declared section.
- **A test outside every owning gate is not regression proof.** Discover test modules, require one registered owner and cache key, and make an intentional failing test prove the full gate executes it.
- **Post-change state cannot prove replacement monotonicity.** An added row can mask the deleted predecessor it replaced; compare an authenticated pre-change baseline or reconstruct deletions before accepting a version increment.
- **An end-to-end gradient check must differentiate the actual objective.** A correct adjoint for an unrelated seeded output sum says nothing about the cross-entropy parameter gradient used by training.
- **Reuse forward activations and loss intermediates within one training step.** Re-running the model and stable exponentiation to obtain values already materialized is execution waste, not a correctness requirement.
- **New code must enter through the existing checked allocator.** Calling a legacy raw allocator creates fresh untyped debt even when release ownership still needs a later capability.
- **Performance evidence needs an exact emitter inventory.** Prefix and suffix heuristics admit convincing paths that do not name a real emitter, so one unrelated measurement can certify untouched code.
- **One deterministic generator needs one nominal state owner.** Copying the same constants, transition, and mutable cell into several simultaneously loaded modules wastes code and permits stream-policy drift; do not add speculative algorithm variants.
- **A detached accessor cannot reuse one shared output buffer.** If two returned spans are promised to coexist, each needs stable backing storage or an explicit borrow lifetime that prevents the second call from overwriting the first.
- **Converting a float index with `f>s` is not validation.** Prove finite, exact integral, nonnegative and in-range before multiplying or indexing, and validate every index before a scatter can partially mutate output.
- **A batch-one positional golden does not prove GPT-2 embedding.** The learned position table is shared across batch items; forward rows must repeat by position and its adjoint must sum every batch contribution into that one row.
- **Host bounds do not protect device lowering.** If generated address arithmetic omits the same checked index contract, an input that rejects on host can still issue an out-of-bounds device read.
- **A test should not import a trainer to borrow two helpers.** Factor the typed capability at its real owner; the import can compile unrelated models, thousands of JIT bytes and ambient state before the test begins.
- **A forward oracle is not a trainable sublayer.** Completion requires parameter/input adjoints, real batch/shape contracts, every architectural bias, model integration and device lowering; a fixed toy forward remains a golden.
- **Optimizer policy and bias state belong to the optimizer.** Rebuilding beta powers and correction helpers in each trainer duplicates code and permits drift; explicit state also makes independent optimizers composable.
- **Fuse Q, K and V before planner boundaries disappear.** Three host-dispatched projections reread the same input and cannot be recovered by a later IR fusion pass that never sees them together.
- **A build-dependency file cannot reference a new engine/checker word in the same commit.** `bin/hb` marks its baked prefix (checker.f, engine-error.f) as `provided`, so a tool the build itself `require`s (verify-source.f, check-core.f) is certified against the OLD baked dictionary; a new `PRIM:`/`:` word or a new `ENGINE-ERROR:` code it names is undefined there. Reference new named codes from the engine as raw numbers (like `C-PACKAGE-FAIL` does) with a naming comment, and defer new-word references from build-dependency tools to a follow-up commit after the word is baked.
- **Acceptance criteria can create a dependency cycle even when the dot says it has no dependencies.** If a foundation task requires future consumers or a future registry to pass, those consumers cannot depend on the foundation honestly. Keep the foundation's proof on current owners plus one representative fixture; enroll future owners in their own dots.
- **A test file is still a module when a suite co-loads it.** Several PTX device tests each defined generic global scratch words such as `HX`; standalone runs passed, but the composed `ptx-toolchain` gate failed at the second definition. Give every co-loaded test a real package owner instead of relying on process isolation, load order, or one-off prefix renames.
- **A semantic payload consumer must use the canonical logical-row seam for every layout width.** `TFC-PUSH-PAY` expanded only layouts wider than one cell, so a width-one enum payload stayed logical and a nested `MATCH` could not consume it even though signature parsing correctly expanded the same type. Delegate to `PUSH-LOGICAL`; do not duplicate its layout-width policy in each consumer.
- **An unbuilt example is an unowned compatibility surface, not verification.** The Zig kernel consumer was excluded from every repository gate, referenced absent generated artifacts, and accumulated three repair tasks while live documentation described it as a working ABI example. Keep executable examples under an owning gate; otherwise remove them and retain only the language-neutral contract they were meant to illustrate.
- **Let one authoritative DATA cell coordinate engine and checker; do not keep two counters.** The `using` depth lives in a single engine DATA cell the machine code owns and the checker reads through `data-base OFFSET +`; every scope boundary (`;using`, `;package`, end-of-file, throw, REPL) just restores that one cell and the checker's parallel name table is automatically bounded by it, so no boundary needs a cross-side resync call and the two views cannot drift.
- **A used-package search belongs at the token-resolution sites, not in the leaf FIND.** Injecting the used-publics scan into `LFIND` itself would fire on the engine's own internal keyword lookups (`trust`, `checker-does`, …); it must sit only in the interpret/compile/tick user-token resolvers so `using` never captures an engine-internal name.
- **Consult the package-consumer convention before writing ad hoc scripts.**
  `require` loads package source; it does not justify repeated `NAME:WORD`
  calls. Every new or changed `/tmp` scratch file, reproducer, performance
  script, generated file, or committed consumer that calls two or more publics
  from one package uses one bounded `using NAME ... ;using` block and bare
  tails. A one-off call may stay qualified.
- **Engine size and candidate-validation coverage move with their owning changes.** Engine growth
  updates the exact-CODELEN baseline, and a new candidate-validation case updates its declared kind
  tally in the same commit.

- **Fix review gate: re-derive the invariant, never accept the fix's own label.**
  The USING seed-boot repair first shipped as a value-range clamp ("depth 0..16
  else 0") framed as a documented tolerant shim; review accepted the label and
  Joel had to ask "best long-term or a patch?" before the invariant was
  re-derived and found probabilistic (it relied on the old seed's heap base
  value happening to be out of range). The upgraded fix bounded the scan by the
  physical mirror capacity with correctness resting on checker-owned,
  sole-writer state — engine-independent. Every fix review must independently
  answer the long-term-vs-patch question; a value heuristic where a structural
  invariant is possible is a patch and goes back.

- **Checker signature comments are typed, not named.** The `( … -- … )` comment on
  a checked definition is the certified signature: write type names (`n`, `ptr`,
  `bool`), never parameter names, or certification fails with "unknown type in
  signature". Found while writing the cache-failure fixtures.
- **Make booleans explicit before `if` when a value rides along.** `dup if throw
  then drop` trips the checker ("expected bool n actual bool") where
  `dup 0 <> if throw then drop` certifies - the comparison makes the consumed
  boolean explicit instead of reusing the numeric value as a flag.

- **A package public whose bareword equals a global is unreachable via `using`.**
  data-loader-test.f passed standalone but failed rc 70 (E-UNDEFINED class) only
  inside maki/test.f. It was NOT an env/argv or child-spawn leak (the loaders
  spawn nothing): DATA-LOADER exported a public word literally named `LOAD`, and
  the test reached it with `using DATA-LOADER` + bareword `LOAD`. Resolution
  order (docs/forth.md Packages) is current-scope + GLOBAL first, then `using`
  publics - `using` is purely additive, so it only resolves a name that is
  otherwise unresolved. lib/ptx/tile.f defines a *global* `LOAD`
  (`span gridctx -- tile`, the LLM-facing kernel DSL). Standalone the kernel DSL
  is never loaded, so the import resolved; in-suite an earlier GPU/eval suite
  (maki/eval/live-test.f -> lib/ptx/test-prelude.f -> lib/ptx/tile.f) had defined
  the global `LOAD`, which shadowed the import, so `DLT-LOAD` bound the kernel
  word and the checker rejected it (fail-closed, clear diagnostic). Fix is
  owner-side: rename the collision-prone public to a distinctive `LOAD-CORPUS`,
  not qualify in the consumer (that leaves the landmine for the next importer).
  Lesson: a package's PUBLIC surface must be uniquely named because `using`
  re-exposes it to bareword collision with globals; a generic public name like
  `LOAD` is a latent "passes alone, fails co-loaded" trap. Gap for a dot: the
  compiler should warn when a `using` import is dead because a global shadows it.

- **The global-vs-used-public shadow is now a hard checker error, not a silent
  dead import (dot habu-err-on-global-e62f806c).** The one enforcement point is
  the checker's sole bareword resolver `CHECKER-FIND-ACTIVE-SYM`: when its global
  branch hits, it also runs the used-publics scan (`CHECKER-USED-SHADOW`), and if
  a live used public exports the same tail it throws `E-USING-SHADOW-GLOBAL`
  (7141) at the reference site, naming both candidates (`global TOK` /
  `PKG:TOK`) with their effect arity. This is the single point because every
  checked bareword reference funnels through it and it is the only side that has
  the effects (the engine's machine-code used-scan has names, not effects); the
  engine keeps global-first resolution as the explicit unchecked boundary, and
  checked code is fail-closed so the checker rejecting means the program never
  runs. The strongest regression is the effects-coincide case (a global whose
  effect equals the used public's): today it certified and silently bound the
  GLOBAL; now it rejects. Escape: qualify `PKG:WORD` for the package word (still
  certifies); the grammar has no bare qualifier for the global "" wordlist, so
  renaming the collision is the documented way to reach the global. The
  diagnostic effects render through the read-only USIGS accessors
  (`CHECKER-FIND-USIG-SYM`/`ER.DIN`/`EFF-ROW-N`), never the bare-name resolver,
  so rendering cannot re-trigger the shadow throw.

- **Diagnostic provenance is separate from semantic identity.** Paths, include
  chains, lines, columns, and spans help locate source but must not alter
  declaration hashes.
- **A soft byte fallback is a state transition, not a sentinel.** A reentrant
  UTF-8 decoder takes its cursor explicitly and returns a sum whose scalar and
  raw-byte arms both carry the absolute next cursor. Validate remaining bytes
  before each continuation read; every malformed sequence returns the lead byte
  with `next = cursor + 1`, leaving the rest for later decoding.
- **An integration workspace must not be a live worker's ancestor.** Reusing an
  old integration workspace by rebasing it can move descendant worker changes
  and leave their working copies stale. Create a fresh workspace at the verified
  base, duplicate reviewed commits onto it, and verify the live workspace graph
  before changing ancestry.
- **Splitting a singleton across modules does not create ownership.** A model is
  instance-owned only when its mapping, validated tensor view, generation, and
  workspace binding are explicit values threaded into every forward and kernel
  call; package-global registries still clobber interleaved models.
- **GPT-2 byte vocabulary makes the classification domain all Unicode scalars.**
  Letter and Number data cannot be bounded to scalars seen in a vocabulary.
- **Generated declarations are one transaction.** Snapshot every mutable owner,
  publish once, and roll all participants back in reverse order on failure.
- **Capacity regressions must assert the reversible preflight owner, not the
  irreversible sink.** Once generated declarations check `prot-wid-room` in
  `PLAN-PREFLIGHT`, one family past capacity correctly throws
  `E-PROTECTION-CAP` (7169), which the uncaught boundary reports with exit 67;
  expecting the engine's `prot-wid-add` exit 84 would require crossing the
  atomic publication boundary and is obsolete.
- **An excluded record kind is corruption, not a compatibility marker.** If a
  compact range contains ordinary dictionary rows only, reject namespace WIDs
  during whole-set preflight; skipping one lets unvalidated authority publish.
- **Rollback is not an atomic publication boundary when a generator yields
  between words.** Render the complete declaration plan, check every name,
  effect, body, visibility rule, and plan-determined capacity in one discardable
  dependency-ordered checker scope, rewind its authorization queue, and only
  then cross one transactional evaluator boundary. One evaluator call without
  that non-publishing preflight still lets the first word become visible before
  a later word fails, even if rollback eventually removes it.
- **Geometric scratch arenas need an explicit arithmetic ceiling.** Bound the
  requested element count, multiplication into bytes, and capacity doubling
  before allocation; signed wrap is not a valid out-of-memory path. If an arena
  can grow into process-local mapped memory, snapshot preparation must also
  repoint it at baked storage while its logical state is idle.
- **A generated-constructor queue is temporary authority, not ordinary scratch
  state.** Its owning catch boundary must start before plan rendering and clear
  it on every render, name, capacity, checker, evaluator, and staging failure; a
  cleanup in the earlier metadata parser does not cover generation that runs
  after that parser returns.
- **Qualified lifecycle operations have two spellings for one identity.** The
  checker owns `PACKAGE:TAIL`, while the dictionary record stores bare `TAIL` in
  the package wordlist; resolve once, mutate checker state with the qualified
  token, and mutate runtime state with the resolved record's tail and wordlist.
- **Transaction finalization releases proof state; it is not a late publication
  phase.** Forward commit may change visible high-water marks only while every
  owner snapshot remains rollbackable, and an exactly preflighted irreversible
  owner commits last. Reverse finalization then only discards snapshots.
  Publishing an earlier owner during reverse cleanup can protect later state
  first and makes a cleanup throw impossible to unwind safely.
- **Make participant registration failure-atomic as one allocation.** Grow one
  aggregate row table before shifting or publishing any row, so allocation
  failure preserves table identity, capacity, count, and allocator high-water.
- **Seal production extension points and isolate test controls.** Production
  coordinators should expose only their runner and read-only telemetry; inject
  allocators, diagnostics, and fake participants into a separate test instance.
- **Nested savepoints must distinguish provisional and published watermarks.**
  Save both values and the owning transaction depth, so nested success cannot
  accidentally publish an outer transaction's still-provisional rows.
- **Dictionary publication spans records, code, and data.** Retain all three
  high-waters until global finalization; restoring only one leaves a generated
  declaration partly visible after rollback. A monotonic, non-reused identity
  counter such as WIDN remains consumed only when record truncation removes every
  lookup path to the identity, leaving an unreachable hole rather than an alias.
- **Every supported transaction commit entry runs the same preflight.** A
  standalone convenience path that jumps directly to commit can publish state
  the coordinated path rejects; route both through one prepare invariant before
  advancing any visible high-water.
- **Keep declaration grammar checked.** Store source spans in typed locals and
  express byte grammar directly, so parsing needs neither trusted character
  predicates nor return-stack hiding.
- **Target-aware sandbox copies must create every selectable target directory.**
  A hard-coded macOS directory made the boot-pin fixture fail opening its first
  Linux source on the GB10.
- **Device snapshots need pins, leases, and completion acknowledgement.** A host
  page reference cannot keep device-visible bytes stable until use completes.
- **Measurement needs structural ownership.** One evidence lane owns timing and
  size artifacts so concurrent work cannot perturb or duplicate the proof.
- **Share scalar conversion without widening an unsafe pointer boundary.** Moving
  exact F32 narrowing and widening into a package-owned library removes duplicate
  numeric logic, but raw load/store/pack operations must stay with their existing
  owner until the common bounded MEM span and subspan types can express capacity.
- **Long decimal reference literals can overflow the fractional scale.** The native
  float-literal path currently accepts a 19-digit fractional denominator that overflows
  its signed cell and silently changes the value. Keep committed f64 references within
  the parser's exact 18-fractional-digit boundary until the compiler rejects or correctly
  scales longer decimals. The compiler work remains recorded in
  `habu-reject-overflowing-decimal-2f3b3a29`.
- **Subagents inherit model and reasoning effort unless explicitly overridden.**
  Dispatch should not silently select a weaker model or effort level.
- **Repair dependency cycles by re-deriving the architecture, not by deleting a
  convenient edge.** The modular-build cycle existed because five flat
  source-map and diagnostic-remap tasks survived after authenticated source
  frames replaced that design. Closing the obsolete work and re-scoping the
  remaining diagnostic owner produced an honest acyclic order; removing an
  arbitrary edge would have left both stale work and a false dependency graph.
- **Table validation must not turn data growth into return-stack growth.**
  A recursive validator may be safe for today's pinned row count but becomes a
  hidden limit as generated tables grow. Thread the index and prior bound
  through one checked iterative loop, and pass table accessors as typed
  quotations so every table shares the same constant-depth canonical proof.
- **A declaration alphabet must be one reversible table.** If parsers derive
  parameter indices with character arithmetic while generators derive spellings
  separately, a reserved scalar token can silently change meaning between the
  schema and its generated signature. Keep one index-to-character table, derive
  character-to-index by searching it, and make every parser, generator, and
  arity gate consume that shared boundary.
- **Test a finite mapping once, then exercise only its behavioral boundaries.**
  A complete direct inverse-table test plus the first reserved-letter boundary
  and the maximum arity proves more than instantiating every intermediate
  declaration, while preserving scarce dictionary space. Diagnostic variable
  rendering and generic effect quantifiers remain separate full alphabet
  domains; declaration spelling rules must not narrow either one.
- **A validated mutable address is not lifetime authority.** Returning a raw
  pointer after checking its cache or page only proves that instant; cancellation
  or disposal can invalidate it while the pointer remains copyable. Keep mutable
  backing addresses private, and give later consumers an immutable snapshot with
  a lease and completion acknowledgement.
- **Process ancestry must survive an exec boundary explicitly.** A fork worker's
  in-memory generation identifies its nested pool, but rebuilding the child
  environment can drop that generation before an executable starts. Then all
  of the executable's subject forks look like unrelated roots. Incident tools
  need both generation and process ID at named stages so the last completed
  source line can be tied to the process that stopped advancing.
- **A background process group must not inherit a controlling terminal for a
  noninteractive capture.** Linux job control can stop the whole group with
  `SIGTTOU` when a nested test issues a terminal-changing `ioctl` on inherited
  standard input. The capture parent then sees neither exit nor output and waits
  until its ordinary timeout. Record process group and terminal foreground group
  in hang diagnostics; a noninteractive worker needs an explicit standard-input
  contract rather than the caller's accidental terminal.
- **Nested engine tests must resolve execution identity through one owner.** A
  relative `bin/hb` fallback silently assumes the current working tree contains
  an ignored build artifact, so it breaks in a clean Jujutsu workspace even when
  that workspace was launched by a valid absolute engine path. Use
  `ENGINE-CANDIDATE:PATH$`: it preserves the explicit `HABU_UNDER_TEST` override,
  otherwise resolves the running engine itself, and validates the executable
  before spawn.
- **A diff hunk cannot prove surrounding lexical scope.** An exact-diff policy
  must verify its new-side lines against the post-change file and derive
  ownership from the complete lexical source. Package boundaries outside hunk
  context and balanced boundary insertions otherwise become false negatives or
  file-wide false positives.
- **A definition lint needs a publication inventory, not a punctuation guess.**
  Audit native definers, every executable `create` owner, and every generated
  definition boundary, then pin each form in one fixture. Keep registry-only
  grammars separate: a suite or primitive-axiom label is data, not a callable
  dictionary word.
- **Deleted syntax needs its complete old lexical context.** Lexing deleted
  lines independently lets a token inside a multiline comment or definition
  impersonate a package boundary and cancel a real owner change. Reconstruct
  the old source from the validated new file and canonical diff events, lex the
  whole old source, then align only genuine top-level transitions to new lines.
- **Close a runner package before it forks package-owning children.** Forks
  inherit the current package state, so a child cannot open its own package
  while the runner's package remains active. Keep the runner private by
  returning a checked quotation from a private helper, close the package, then
  execute that quotation immediately; raw execution tokens, exported aliases,
  and storage cells weaken the boundary.
- **A linear wrapper is not opaque when its generated representation API is
  public.** A public product can preserve its linear owner while replacing raw
  state fields through `UNMAKE` and `MAKE`. Use an opaque linear token and keep
  mint, state access, and consume leaves private in a sealed package; otherwise
  a caller can reopen the package and publish them. Test both the exact rehoming
  attack and package reopening, not only duplicate and drop.
- **Explicit parser state must not add a threaded call per field access.** Derive
  one private state view at the public operation boundary, use named direct
  offsets internally, and publish numeric kind during the existing grammar pass
  instead of rescanning the token. Ratchet both repeated small documents and one
  long stream against the pre-refactor medians.
- **Object-key search is parser state, not token scanning.** Require an active
  object search phase, capture its depth, accept keys only at that depth, skip
  each unmatched value, and stop at that object's close. Compare decoded bytes
  through the shared streaming unescape sink so key length never becomes a
  reader-storage capacity.
- **A performance ratchet must execute every changed production path.** Numeric
  token scans cannot bound string sink dispatch or object-key traversal. Use
  long repeated raw, escape-heavy, hit, and miss workloads; median repeated
  samples; and leave measured headroom above timing noise.
- **A lint claiming grammar parity must be differentially probed, not read.**
  A whole-file trust scanner that mirrored the engine's tokenizer passed a
  hunk-by-hunk review and still diverged four ways: compiled TRUST inside
  definition bodies invisible, unterminated string/PRIM/definition constructs
  ending the scan with zero findings (fail-open at end of input), defining
  words like `create` not consuming their raw name (false positive), and
  non-short-circuit `and` reading past the end of the source buffer. Review
  method for any parser that claims to match another: run the SAME fixture
  through both implementations (engine load and lint) and compare verdicts,
  including malformed and truncated inputs; reading the two sources
  side-by-side is not evidence. The structural fix is one shared lexer both
  consumers import — a clone starts diverging the day it is written.
- **An existence gate is not an accounting of generated behavior.** Exempting
  a build-time emission from manifest checks because its generator FILE exists
  lets a comment-only stub arm the exemption and lets the real emission drift
  invisibly. Parse the actual emission from the generator source and enforce
  the ordinary manifest/effect/test discipline against the parsed facts.
- **Before reviewing an orphan workspace, prove whether its outcome already
  landed.** A stale claimed workspace was destruction-reviewed against its own
  old tip and produced three findings — every one already fixed on master by a
  later, already-accepted commit chain. Search current master for the claimed
  outcome and compare the exact candidate ancestry first; findings issued from
  a superseded tree are false alarms that can spawn duplicate lanes.
- **Replace review vigilance with mechanical checklists.** Joel-directed
  codification (2026-07-22) after a day where every review miss had a known
  rule behind it, applied selectively. Pre-dispatch: the contract determinism
  test — if two different workers could ship two different designs from the
  frozen text, it is not frozen; contracts carry interface, owner,
  dependencies, forbidden alternatives, production-path red proof, acceptance,
  and the suite-inventory rows in the write-set; independent pre-implementation
  review must return READY on the exact text, and any edit re-triggers it.
  Pre-accept: walk the full production path end-to-end (trigger to exit code),
  run the four probes (boundary trace, measured performance, forward
  compatibility, gate enrollment), and verify the parent IS verified
  master@origin — an ancestor of master is not the same thing, and the
  difference was seven silently reversed dot changes in one real rejection.
  Pre-handoff: run the same checks on your own artifact before cross-review
  sees it — the symmetric standard is a checklist step, not a virtue.
  Checklists survive fatigue; sharpness does not.
- **A rollback scope owns references and their targets together.** Retiring a
  type or field registry while keeping declaration events that name those rows
  creates published dangling identities even when both local state machines
  pass. Exercise the real outer scope, compare every related high-water mark,
  and resolve every surviving event after success and failure. A guard that
  rejects an identifier after rollback left its references live is only a
  symptom check; retire references before targets in the rollback owner.
- **With no compatibility contract, a format cut is deletion, not versioning.**
  Remove the old writer, reader, validator, fixtures, and state in one green
  change; do not preserve an empty ABI with versions, tombstones, or migration
  branches.
- **Census namespace claims before accepting a package rename.** `HB-BUILD`
  already owns the user-facing build tool, so renaming the native emission
  lifecycle into it would merge unrelated authorities. Fold lifecycle state
  into its real `HB-EMIT` owner and leave one package per concern.
- **Audit every consumer before reusing a protection registry.** PROT-WID
  membership also blocks ahead-of-time calls, so enrolling owner package WIDs
  would change public callability instead of only guarding publication.
- **Friend-only grammar needs the generated compiler-prefix seam.** Raw
  `--load` and `--build` inputs still see the baked dictionary's internal-word
  marks, so an isolated `PRIM:` corpus correctly rejects. Use the existing
  compiler payload builder: hide and reload the compiler prefix, append the
  exact fixture before its `SEAL-FRIEND`, then run that payload through
  `--build`. Compare the same unmodified fixture bytes through the public
  verifier; never weaken the user boundary or invent a test-only primitive.
- **A cursor regression must prevent later delimiters from rescuing it.** An
  attached primitive closer followed by another same-kind row can be swallowed
  while the scanner falsely closes on the later row. Put the attached closer
  last for its delimiter family, then mutate the cursor owner to consume the
  suffix token; the production verifier must fail on the missing closer.
- **Destructive Jujutsu commands need immutable targets.** A change ID can
  evolve to name different content when concurrent work rewrites the graph.
  Resolve the intended commit ID and verify its description and parent
  immediately before abandoning, rebasing, or otherwise rewriting it.
- **Trace transaction phases before freezing a failure test.** Requiring a
  later participant to fail after COMMIT was impossible when every later
  rejection occurred during PREPARE and the remaining COMMIT callbacks were
  infallible. Prove the real phase order first; test retained committed state
  through the owner's public lifecycle instead of inventing an injection hook.
- **A measurement record must be reproducible by its committed runner.** A
  table assembled from per-cell maxima across uncommitted scratch runs cannot
  be regenerated by a documented single-sweep command. Record the repetition
  and selection policy in the runner and publish structured evidence from that
  exact execution.
- **A leaf cannot accept deletion owned by a later generator leaf.** The parity
  corpus leaf accidentally required legacy fixture names to disappear while
  forbidding edits to the generated module that owns them. Check every
  acceptance clause against the leaf's write set and dependency graph; assign
  removal to the first leaf that actually owns the old definition and callers.
- **A reference-corpus test must not copy the reference corpus.** Duplicating
  every source and expected vector creates a second evidence owner and doubles
  review cost. Keep each row once, pin a canonical digest over the public
  accessors, and separately prove descriptor spans, bounds, and exact public
  inventory.
- **A private name does not make returned storage immutable.** A sealed package
  that returns its backing pointer gives every caller write authority over the
  reference data. Return caller-owned copies, derive descriptor bounds from
  labels, validate the complete descriptor before publication, and mutate the
  copies to prove that a second read still returns the canonical corpus.
- **Copy-out APIs need distinguishable spans and alias preflight.** Passing two
  unrelated containers as the same raw header type lets a role swap certify and
  reinterpret their layouts. Accept typed byte/cell spans, prove capacity and
  pairwise non-overlap before the first write, and serialize raw descriptors as
  well as logical outputs so a hidden count or offset cannot drift.
- **A nominal API still needs a named representation owner.** A typed length or
  index cannot enter pointer arithmetic through an ad hoc caller cast. Keep the
  audited projection private to the module that owns the storage operation,
  inventory it, and migrate production consumers onto the typed interface
  before asking them to consume typed reference data.
- **A package has separate private and public seal boundaries.** Sealing its
  private wordlist does not stop a public package reopen or a qualified public
  definition. Test the public seal through reopen and qualified publication;
  test the private seal by resolving the namespace record's private wordlist
  and publishing through `set-current`. A package reopen reaches the public
  guard first and cannot distinguish a missing private seal. Remove each seal
  in a focused mutation so both publication paths prove their own rejection.
- **Joint master publication needs the other agent's ACK before the push.**
  Publishing an agreed, green commit is still unilateral if the exact commit
  was only announced, not acknowledged. Post the commit and gate evidence,
  receive the explicit ACK, then push and verify the remote revision.
- **A commit is a proof checkpoint, not temporary storage.** Commit `a51f6d33`
  entered history with eight unclassified `TRUSTED` test helpers because strict
  inventory was checked only after descendants removed them. Run every required
  gate on the exact tree before describing the change, and split mixed outcomes
  before commit. If a red checkpoint is found before publication, rebuild its
  accepted content as individually green commits and prove the final tree is
  identical before replacing the history.
- **Do not overlap gates that share a durable default root.** `test/run.f` and
  standalone `maki/test.f` both use `tmp/cad-store`; concurrent runs can reset
  each other's replay rows even when `HB_TMP` differs. Serialize them unless
  each process has a distinct `HABU_CAD_STORE`.
- **A recovered claim must be checked against current master history.** A clean
  metadata delta can still resurrect work that later landed and closed. Before
  publishing or dispatching a recovered claim, inspect its result in
  `master@origin`, its landing commits, and the current open and archived dot
  state; discard the claim when the owned outcome already exists.
- **Rebase a commit delta; do not restore whole files from a divergent tip.**
  Restoring files from a candidate also imports unrelated parent content that
  the candidate did not change. Duplicate or rebase the reviewed commit onto
  the integration parent, then inspect its exact delta before composing it.
- **Serialize device-owning gate slices.** Running the Maki device suite beside
  the PTX toolchain made the PTX phase fail while its isolated `ptx-stdlib`
  members passed. Parallelize CPU-only gates, but give each GPU-owning gate
  exclusive device time so resource contention cannot masquerade as a defect.
- **Inspect legacy timestamps after `dot on`.** Older dot files may contain an
  already-quoted `created-at` value; the current serializer can quote it again
  while changing status. Restore the original timestamp bytes before
  publication and keep the mutation limited to status, claim, and contract.
- **Prove mapping release with an OS-enforced child-process access fault.**
  Ambient counters prove only that a call occurred, not that ownership ended.
- **Kernel idempotence cannot prove exactly-once release.** Consume the owner
  only after successful `munmap` so a duplicate release fails structurally.
- **A package API audit must inspect every forbidden namespace.** Count the
  intended package wordlist, query wordlist zero for each retired alias, inject
  an exact forbidden alias to prove rejection, and keep test evidence private
  to its owning package.
- **Error precedence begins before materialization.** Put every fallible session
  entry operation inside the same caught sequence as execution so cleanup and
  primary-result ordering do not depend on an operation being infallible today.
- **Prove a seal against every owner-private test before freezing it.** A
  package seal can be correct yet make its required proof unloadable when the
  suite reopens that package to define private helpers. Move those tests onto
  production public seams first; never weaken the seal or add a test bridge.
- **A wall-clock ratchet inside a parallel test group measures the contention,
  not the code.** The six JSON reader ratchets ran through
  `lib/json-read-test.f`, a member of the parallel `stdlib/tail-pure` fork
  group, so every budget had to be padded for whatever else the box was doing.
  Split timing from judging - one word that runs the warm-up and stores every
  raw sample, one word that turns those samples into budgets and verdicts - so a
  scheduler can measure while nothing else runs. Extracting them also cut the
  correctness suite from 2.20s to 0.44s.
- **An upper-bound budget check passes trivially on an unmeasured table.** A
  verdict of the form `median <= budget` reads a zeroed sample table as a pass,
  so a skipped or half-finished measurement looks green. Count the stored
  samples as they are appended and make the complete count part of every
  verdict; then a dropped run fails closed instead of certifying nothing.
- **A quiet measurement phase must refuse by construction, not by convention.**
  The one place the JSON reader ratchets now run is scheduled outside the
  numbered phases, after every parallel phase has drained and before the run is
  judged, so the box is idle. That is only trustworthy because the phase proves
  its own preconditions instead of trusting the schedule: a nonzero worker slot
  identity proves the caller is already inside a fork worker and it refuses, a
  caller with pool workers still in flight is refused, and a second call in one
  gate process is refused with the turn claimed only after admission, so a
  refused call cannot burn it. When a check depends on the rest of the system
  being quiet, make the check test that quiet itself.
- **A gate that exits nonzero is not the same as a gate that ran.** On master
  79c50e5a, `package-diff-lint` throws `E-DIFF-SYNTAX` and exits 67 on any real
  diff touching `src/core/checker.f` or `src/core/sumtype.f`, printing no
  findings, so the mandatory package-ownership rule has never been applied to
  the two largest core files; `error-code-lint` decides string membership by
  counting quote characters, so one bare quote token silently skips every claim
  in the rest of a file. Both look fine from the outside. Before trusting any
  gate's verdict on a class of input, feed it a deliberately bad example of that
  exact class on the same command path and confirm it reports the finding you
  expect - a clean run and an opaque throw are indistinguishable in a log.
- **Orchestrator scratch state under /tmp does not survive a reboot.** The
  2026-07-25 reboot destroyed a 45-item control queue that existed only as a
  file in a session scratch directory; the only survivors were the two items
  appended after the reboot and the items already converted into dots. Durable
  coordination state - work queues, contracts waiting to be written up, rulings
  not yet applied - belongs in the repository under `.blackboard/` or in the
  persistent memory directory. Treat anything under /tmp as reproducible
  working material, and convert a decision into a dot or a tracked file as soon
  as it is made rather than at the end of a wave.
- **Evidence before prose: a report line quoting tool output must be written
  after reading that output.** A scheduler-lane worker asserted a gate number
  from expectation before any log existed, then caught itself and re-verified
  non-circularly. Computing the expected value and then asserting it was printed
  is fabrication even when the number turns out to be right, because the claim
  carries no information about what actually ran. Read first, quote second.
- **Shared workspace state makes a reversible probe visible to concurrent
  reviewers.** A field-owner-lane reviewer sampled a real-looking suite failure
  at exactly the moment the worker was reinstalling the engine to probe
  something, and the same failure reproduced 0 times out of 136 outside that
  window. Run probes in throwaway tree copies rather than in a workspace someone
  else may sample, and reconstruct the workspace timeline before calling any
  intermittent failure a flake.
- **Verify every identifier on a closure list against the tracker before acting
  on it.** A metadata batch listed eight dots to close as landed; three of the
  identifiers existed nowhere in `.dots`, while the work they described had in
  fact shipped under different dots. Resolve each identifier first, then prove
  the change is an ancestor of `master@origin` and that the file-level result
  is actually present, and read the dot's own text for atomicity clauses: two
  of the remaining dots said in their contracts that neither could close without
  the other, which the list did not mention.
- **A cold-cache native gate run on this box overruns the performance band while
  correctness stays green.** `bin/hb --load test/run.f` with a fresh `HB_TMP`
  reports `performance=hard-fail correctness=t` at roughly 33s against a 25s
  cold budget; the second, warm run of the same tree passes at ~31s against the
  35s warm budget. Confirm the same cold overrun on the unmodified parent before
  attributing it to a change: parent 32793ms, changed tree 32982ms, a 0.6%
  difference.
- **A reviewer-supplied "verified reference patch" is a claim until you hash it.**
  A round-2 verdict pointed at a scratch tree said to contain three verified
  fixes; the file there was byte-identical to the unfixed original
  (sha256 45ab6386...), so it carried none of them. Re-derive the fix and build
  your own mutants either way - the verdict's *findings* were all three real, and
  a fourth of the same shape was next to them. Diff or hash any handed-over
  artifact before treating it as evidence.
- **Launch every gate and worker `bin/hb` with stdin redirected from /dev/null;
  an inherited terminal or pipe is an undefined input, not a neutral one.** The
  week of "box contention" gate reds was one mechanism: a bare-argv `bin/hb`
  reads stdin to end-of-file before running its script, capture spawns passed
  descriptor -1 so children inherited the orchestrator's never-ending
  background pipe, and every bare-hb capture child blocked until its deadline.
  The red set equalled the bare-hb spawner set exactly; `--load` spawners were
  immune; the HB_TMP-length and contention hypotheses died under experiment.
  Two corollaries: a retry that "passes" proves the launch path changed, not
  the tree; and attributing reds to environment noise without isolating the
  spawner class is how a real seam hides for a week.
- **A checkpoint stop that forces a redesign is the system working, not
  overhead - three stops in one wave each caught a wrong premise before code
  was built on it.** The shared MODEL enums stopped on a package-name
  collision with the CAD typestate stage (resolved by renaming the stage
  package to CADMODEL, not by bending the new owner); the global-ENUM cutover
  stopped on multi-error loads, exposing that reject-swallowing lived in the
  legacy definer and had to be inherited deliberately by the front-end GUARD
  with per-front-end resynchronization; the safetensors S4 seam stopped on the
  discovery that its base surface had never landed on master, flipping the
  order to re-derive the core first. In each case the cheap pre-edit
  checkpoint (green baseline, one real failing check) surfaced an unplanned
  interface or a false ancestry assumption; the redesign cost minutes, and
  building on the wrong premise would have cost the lane.
- **A buffer that cannot represent its input must raise - truncation is a
  value heuristic, and it now belongs on the review-gate checklist.** The
  replay-registration work ratified the rule for BODY-APPEND and every buffer
  builder: raising on overflow keeps the failure at the boundary that
  understands it, while silent truncation converts an input problem into a
  downstream mystery. Reviewers should treat "fits so far" buffers like magic
  ranges and lucky sentinels: if the structural fix (raise, or size from the
  input) is possible, a truncating build is a patch and goes back to the lane.
- **Gate verification and push are always two separate tool calls.** A
  bookmark-set-and-push chained in the same pipeline as the gate run fired the
  push before the gate return code was read; the outcome happened to be safe,
  but the ordering was luck. The push command may only be issued after the
  gate verdict has been read from a completed, separate invocation.
- **An external binary that rewrites tracker state is an untrusted writer -
  the repo must fail closed on its output, because convention is not
  protection until a lint rejects the violation.** The dot CLI corrupted the
  tracker in two ways in one wave: every rewrite added a quoting layer to
  quoted frontmatter scalars (cumulative and silent), and `dot off` moved two
  closed dots into `.dots/archive/` when the repository's canonical form is
  closed-in-place (302 precedents). The archive defect orphaned five
  ledger owner rows and surfaced as a distant gate red at
  landing time, not as a tracker error at mutation time. Both shapes now have
  fail-closed gate dots (habu-reject-re-quoted-e908ece5,
  habu-reject-archived-dots-db3cbf63); the general rule is that any tool
  outside the repo's own gates gets its writes linted, and the lint lands
  before the next use of the tool, not after the next incident.
- **After `jj squash`, a change's commit id moves - re-resolve the change id
  to its current commit id immediately before any rebase or abandon.** Acting
  on the remembered pre-squash commit id rebased a stale copy and created a
  divergent change; recovery required abandoning the stale copy and rebasing
  the squashed one. The existing rule about resolving targets before
  destructive jj operations already covered this and was skipped under
  routine; the id must be re-read after every rewriting operation, not once
  per task.
- **Never read a gate's return code after a pipe - `cmd | tail; echo $?`
  reports the pipe tail's status, not the gate's.** One gate harness echoed
  `run=0` while the suite had printed `native test suite phases failed`; the
  red was caught only from the failure text. Gate harnesses log each command
  to a file and echo the command's own return code (`cmd >log 2>&1; echo $?`);
  a pass verdict requires both the true zero and the positive verdict lines
  from the log.
- **`DERIVE eq` cannot ride on a STRUCTURE that carries an arity-0 TYPEFAMILY
  proof field - the declaration fails outright ("bad structure declaration").**
  The MODELPROV lane hit this building a self-contained identity value. The
  working patterns are the MDLCFG split (keep the key value and the
  proof-carrying record as separate structures) or a hand-written cell
  comparison on the proof-carrying value. Worth remembering before designing
  any new sealed identity record; the engine limit is real, not a syntax slip.
- **A helper that consumes several values off the stack folds them in reverse
  declared order - for a content-key preimage that silently reorders the
  identity with no type error.** The MODELPROV fold helper written as bare
  stack consumption produced rows in the wrong order and only the structural
  test that decodes the real preimage rows caught it; a substring-presence
  test would have passed. Bind locals in fold helpers, and prove fold order by
  decoding the production preimage bytes, never by reading the source.
- **Guard a composed `a*b + c` with one divide-form pre-check, not a check after
  each operation.** GPT2TENSOR's census first multiplied with an overflow check and
  then guarded the add - but no multiple of 13 lands in the 4-value window where
  the add alone overflows, so the second branch was unreachable dead code that
  no fixture could ever exercise. Bounding the input first (`n > (MAX-N - c) / b`
  rejects) makes the whole expression provably safe with a single branch a test
  can actually reach, and it is the same shape MDLCFG's V-CENSUS already used.
- **Test an overflow guard at its exact boundary, derived, not just at the
  MAX-N extreme.** GPT2TENSOR's census and multiply guards were exercised only
  with MAX-N-scale inputs, so loosening either bound by one left the suite
  green; the destruction review caught it. Every guard needs the largest
  accepted input AND that value plus one rejecting, with both values computed
  from expressions over the limit constants so they track the code. Also check
  what a composed expression promises: per-factor checks alone let a
  "d0*d1*d2*d3 fits" claim overflow through the pair product.
- **A public word must not hand out a span into a mutable static; copy into a
  caller buffer and answer option<n>.** GPT2TENSOR's first name reader returned a
  pointer into its render scratch with the invalidation rule in a comment; the
  neighboring SAFET package had already solved this structurally with
  COPY-NAME? (NONE on too-small capacity, SOME of the copied length). When a
  sibling package has a reviewed contract for the same problem, mirror it
  instead of inventing a weaker one - and test the interleave the weaker API
  would fail.
- **Queries defend valid graphs, not forged corruption.** Mutual family cycles
  cannot be declared; a trusted forged-cycle fixture did not justify global
  visited state. Put corruption checks at the writer and specify the smallest
  mechanism a rejection needs.
- **A worker dot is a frozen implementation contract, not a research prompt.**
  Before dispatch, name its owner, interface, reachability, failure, and real
  failing production check; otherwise keep it as an unclaimed design parent.
- **Reuse the repository's proven engine-seam pattern.** For a runner that must
  execute after package close, a private `ACTION` returns a checked quotation
  before `;package`, then the quotation executes after close with zero exports.
  A measurement justifies that mechanism, not a public API.
- **Rollback proof stays with each production owner.** The family suite owns
  its eight registries; `DECL-EVENT` owns its bytes, cursors, and frames;
  candidate validation pins both, so extending the family snapshot duplicates
  authority. The real gap was the copied confinement lexer; prove confinement
  through exact child-process routes against the shipped binary.
- **Reserve a linear transfer record before publishing its source owner.** A
  late allocation forced every caller to carry catch state after publication
  and made repeated transfer fabricate an empty owner. Allocate and initialize
  the record before publication, move it once, and return a typed `empty` arm
  thereafter; expected absence is a value, not an owner or an error.
- **A validated linear owner should not retain an impossible absence arm.**
  Once `SAFET:mapping` can only be minted from a parsed positive-length image,
  `WITH-MAPPING` must always run its body and return the length directly; an
  obsolete option weakens every caller and hides the mint invariant.
- **Name an operation by what it does.** A checkpoint path that reads and
  validates tensors is a load; reserve unfamiliar terms for concepts that
  cannot be stated plainly. A hard rename is also a contract audit: do not
  carry a retired capability caveat into the new source, inventory, or trust
  prose just because the old text already had it.
- **A successful release test does not prove total cleanup.** If a release can
  throw, it can interrupt later cleanup and abandon owners hidden in raw cells.
  State only the valid-state guarantee until the release itself is uncatchably
  fatal or preserves every owner for retry.
- **Mechanical Forth rewrites must preserve line boundaries.** Test the rewrite
  on a scratch copy and compare line counts before touching the working tree;
  an inline `\` comment turns all following code on a collapsed line into a
  comment.
- **`dot on` still double-quotes an already quoted `created-at` value.** It
  reproduced on the current tool, so inspect every status mutation, normalize
  the changed file before publication, and keep the writer-fix dot open until
  a regression proves the bug cannot recur.
- **A new Maki suite needs two registrations: the master list and one slice.**
  Add it to `maki/test.f` and to exactly one of `maki/test-core.f`,
  `maki/test-db.f`, `maki/test-eval.f`, or `maki/test-eval-emit.f`;
  it appears exactly once among the slices.
- **Destructive cleanup requires a validated target.** An unsupported
  `jj diff --check` left a temporary-path variable empty, so unconditional
  `gio trash "$candidate_file"` trashed the current directory. Stop when target
  creation fails; cleanup only an explicit, nonempty path proven inside the
  intended temporary root.

- **Visibility and token spelling are not authority.** Confinement must use
  structural definer and `EXPORT` roles plus generated dictionary ownership,
  while preserving legitimate inherited globals such as `COUNT>N`.
- **A nominal family package is its mint authority.** Keep checked
  representation casts private in the declaring package, protect that
  wordlist, and bind `CAST:` authorization to the engine's live namespace
  record and actual definition wordlist, never a mutable checker scope mirror;
  a second raw-authority package only splits ownership.

- **Never put `$(...)` before `$?` in the same echo.** In zsh the command
  substitution runs first and overwrites `$?`, so `echo "$(basename $p)
  exit=$?"` reports basename's 0, not the command under test. Two whole
  bisect loops returned all-green this way and manufactured a phantom
  "nondeterministic lint" until a loop with `rc=$?` captured first exposed
  the real, deterministic failure. Capture `rc=$?` on its own line before
  any other command runs.

- **Read the convention before "fixing" metadata, and never count a worker
  echo as confirmation.** `docs/critical-path.md:13` states that a dot's
  `blocks:` list names its **prerequisites** — the dots that must land first.
  I assumed the opposite, told a worker its correct edge was "stale", and
  deleted it; a later worker reported the same reading back to me as its own
  finding and I deleted a second correct edge. The echo was not independent
  evidence: I had put the framing in its prompt. Both edges are restored
  (source→arena, types→symbols). Two rules: grep the documented convention
  before editing dependency metadata, and when a worker agrees with a claim
  that originated in my own instructions, treat it as unverified.

- **A passing test is not a scheduled test.** Every compiler substrate lane
  this session reported its suite green, and I re-ran each one myself and saw
  exit 0 — but twelve of those test files are listed in no suite in
  `test/gate-stdlib-cases.f`, so they never execute in a full run. Six
  modules and the raw-storage seal regression landed unprotected: passing by
  hand, invisible to the gate. Nothing catches this: registration is what
  schedules a file, and nothing else proves a file is scheduled.
  When accepting a lane that adds a test file, verify the file
  appears in a suite AND that a full run executes it — "I ran it and it
  passed" is evidence about the code, not about the gate.

- **Never rewrite history while workers are running.** Rebasing the whole
  chain onto a new upstream while three agents held live workspaces cost two
  of them their work: one recovered on its own by re-parenting, two were
  killed mid-recovery and their commits survived only as unreachable
  conflicted objects that had to be excavated by hand. Concurrent jj
  operations also left the repository reconciling divergent operations and
  briefly reporting the bookmark as conflicted. Either drain the workers
  first, or rebase only after every lane has committed. When it does happen,
  the commits are still in the object store: find them with a revset over
  `files(...)`, extract the clean files, and rebuild the workspace rather
  than trying to repair it in place.

- **A structural gate must classify closed under calls, not per-body.** The
  interning parity gate had to assert that every capacity check precedes the
  first arena write. Written as a token scan over each intern word's own body
  it would have passed vacuously forever: `IR-SYM:INTERN` and
  `IR-ATTR:INTERN5` contain no `IR-ARENA:PUSH` token at all — they write
  through `POOL-ADD`/`ROW-ADD`/`ROW-ADD5`. The gate classifies a definition as
  a writer if it pushes *or calls a writer*, and additionally asserts the
  writer and checker counts are both non-zero, so the ordering claim cannot be
  true because it found nothing. Any gate that says "X happens before Y" must
  prove it found an X and a Y.
- **A claim from the other orchestrator is still a claim; verify it before
  freezing it.** A design document named a corrective leaf for a "double
  `TV-VFIELDS!` write" in `TV-NEW-VIEW` that came verbatim from a blackboard
  message. The code writes it once (`maki/tensor-value.f:456`), and the other
  orchestrator's own fresh reviewer caught the invented leaf. Coordination
  messages carry hypotheses, not evidence; read the line before it becomes a
  contract.
- **Absence must be proved in every scope a reintroduction can hide in.** A
  retirement suite proved deleted names unresolvable from outside their
  package and called them absent. Reintroducing them *inside* the package's
  private section left the suite green, because checker probes outside a
  package cannot see its private definitions. Prove membership against each
  real word list (global, package-exported, package-private), give each list a
  production witness so a misbound list fails its own control, and mutate in
  every scope — the attack that only reaches the visible scope proves nothing
  about the hidden one.
- **A lint or checker probe is not visibility evidence.** Package-diff lints
  read diff text and checker probes read resolvability; neither observes the
  dictionary the running image actually built. Publication and retirement
  leaves need runtime word-list assertions, and the contract should freeze
  them up front rather than discovering the gap in review.
- **Apply the duplicate-authority test to the candidate you favour.** A design
  rejected a new span registry for duplicating the tensor authority, then
  proposed a storage owner without naming what it displaced — recreating the
  same duplication one level down, where the existing owner (`WSTORE`) already
  held the model's memory. Whenever a design adds an owner, the frozen answer
  to "what stops owning this, and when" is part of the design, not follow-on
  work.
- **`private` is a convention, not a boundary.** Any file may reopen
  `package NAME private` and call its internals; a proof run drove the single
  fatal `munmap` sink with a raw pointer from outside its package. Designs may
  not claim "no other constructor exists" from package privacy alone until a
  sealing capability lands.
- **Give each lane a private scratch subdirectory.** Two workers wrote generic
  artifact names into the shared scratchpad and one clobbered the other's
  evidence mid-run. Name the directory after the lane in the brief.
- **Ask what breaks if the check is violated, before making the check
  rigorous.** An AOT gate asserted that exactly 69 named definitions lived in
  a module, comparing a literal against a counter those same rows incremented
  — self-satisfying. The fix looked like proving a real bijection over the
  live dictionary interval, and that revision was sound. It was also the wrong
  artifact: adding a private helper to a private package violates nothing the
  lexical wrapper, the ownership gate, the public-surface test and the AOT
  gates do not already cover. The census protected a MIGRATION invariant
  ("everything moved into the package"), true the day packaging landed and
  baggage every day after. Deletion was the fix. Before strengthening any
  census-shaped check, ask what breaks when it fails; if the answer is
  "nothing that is not already caught structurally", delete it however sound
  its arithmetic.
- **Search for an existing frozen design before writing a new one.** A tensor
  ownership design was drafted, reviewed, and rejected twice before a survey
  found that `.blackboard/gpt2-forward-leaf-design-20260727.md` had already
  frozen the answer for weights — a linear owner with non-linear views into
  it, and a byte-owner copy word because quotations are not closures and
  cannot transport a destination. The genuine residue was activations, a much
  narrower question. The pre-mint search gate must cover frozen design
  documents in `.blackboard`, not only dots and code.
- **A review answers the question it was given.** An implementation was
  accepted against "does this preserve the accepted behavior" while being
  simultaneously unmergeable, because its stack had drifted from master and
  dropped changes master had gained. Both verdicts were right about
  different axes. Any review that could precede a merge must also check base
  currency, since master-always-green is a property of the exact rebased tree.
- **Freeze an interface only after a checked candidate compiles and runs
  through the owning path.** A design named five words for its size pipeline.
  All five existed; the design was still unimplementable, because two of them
  sat inside their packages' private sections and the owning package could
  only reach them by illegally reopening those packages. Three review rounds
  ran the same shape: existence verified, then units, then callability — each
  round checking only the property the previous rejection had taught me to
  check. A grep proves a name is spelled somewhere. It does not prove the
  word is public, that its argument roles match, or that the sequence
  type-checks. Write the twenty-line probe in a foreign package and run it
  under `bin/hb` before the interface is frozen, not after it is rejected.
- **The checker refuses nominal roles in raw storage, so a typed value must
  carry them to the boundary.** Storing a `CAD-NUM` role through `!` is
  rejected — probed: `: STORE-ROLE ( ptr a CAD-NUM:alloc-byte-len -- )
  swap ! ;` gives `E-NONPARAMETRIC-EFFECT` and the load fails closed at exit
  70. A design that plans to keep validated sizes and offsets in a raw header
  must carry them in a typed value through every validation step and erase
  them exactly once, inside a single audited mint, rather than re-deriving
  them in raw `n` on the far side.
- **A composition brief must name the whole stack and the expected baseline,
  not one commit.** A worker was told to "compose with commit X" and did
  exactly that. X was the package child of the accepted payload, so its own
  diff omitted the fix that lived in the parent, and every tree in the
  resulting four-way isolation lacked it. Four identical reds read like a
  strong signal and were four instances of one omission — reported as a
  discovered blocker until the other orchestrator asked whether the payload
  layer had been included. Name the full range as explicit layers
  (`base..payload`, then `payload..package`), say what the composed tree must
  CONTAIN rather than which commit to apply, and state the expected
  pre-composition result so a predicted red cannot be mistaken for a finding.
- **`git apply` inside a jj workspace silently does nothing.** It resolves to
  the parent git directory, returns exit 0, and writes no file. A worker
  caught it only by hashing the target afterward rather than trusting the exit
  code, and switched to `patch -p1`. Any tool that reports success without
  changing the tree will manufacture a green composition out of nothing;
  verify by content, never by exit status, when applying a patch across
  workspaces.
- **When you find one untested copy path, sweep for its siblings.** A review
  pass found that the type-family record's growth path copied a newly added
  cell with no test — the one-cell-short mutation survived green — and fixed
  it. An independent reviewer then found the *same* defect one layer over: the
  rollback-frame arena has its own growth path, its own newly added last cell
  (`TFRB.OWNLO`), and the same untested copy, and its one-cell-short mutant
  also survived both suites. Two arenas, two growth paths, one habit of
  checking only the one that failed first. Adding a cell to any record means
  auditing every path that copies that record — growth, persistence, snapshot,
  rollback — not just the path whose absence happened to be noticed.
- **A cleanup that deletes a gate must delete the instructions requiring it,
  in the same change.** Removing a lint and its manual index left the
  operating contract naming that gate as blocking for every master merge, so
  the documented merge procedure became unperformable while every test stayed
  green. Stale prose is usually a hygiene item; prose that *instructs* is part
  of the contract, and a deletion that leaves it behind is not atomic no
  matter how clean the code side looks. Note also that the agent instruction
  files may be symlinks to one another — check before "also updating" the
  other one, since that is the same write and can replace a link with a file.
- **A frozen contract is not dispatch-ready until someone reviews the freeze.**
  A leaf contract was anchored by reading every insertion point, verified
  against the tree, and declared ready. A preflight review then rejected it
  before any code was written, for things reading the insertion points could
  not surface: a consumed structural token had to be pushed back so the
  all-errors resync could not swallow the following declaration, an event tag
  had to be appended without renumbering, specific diagnostic codes had to be
  mapped, and a keyword had to be reserved. Verifying where a change goes is
  not the same as verifying what the change must handle. The freeze itself is
  an artifact that earns a review pass, and that pass is far cheaper than the
  worker discovering the gaps at line 500.
- **A numeric coincidence is not a cause; attribute by differential
  measurement.** A leaf added eight new checked definitions to the engine, and
  two size ratchets drifted by exactly eight bytes. That was reported as the
  leaf's cost and repeated downstream as established fact. Independent
  measurement of the intermediate tips proved the leaf moves ZERO engine
  bytes: the eight belonged to an unrelated commit, and the region map showed
  one region up eight with a compensating pad down eight. The matching number
  made the inference feel verified when nothing had been measured. Attribute a
  size or performance delta by building and mapping the exact tips on either
  side of each candidate, never by counting what a change happens to add.
- **A proof that models another system will lose to it; observe the system
  instead.** Two proof mechanisms died this way in one session. A 1,070-line
  source analyser inferred what the compiler would bind, and five successive
  narrowings still lost to `EXPORT`, `undefine`, `using`, and local shadowing —
  a wrapper exported into the target package ran fifteen times while the check
  reported zero findings. A 243-line lexical scanner counted rows in source to
  prove a case was enrolled, and lost to a dormant conditional, a two-iteration
  loop, and an unreachable decoy dispatcher, because a lexical count is not an
  execution count. Both were replaced by observing the real artifact — the
  compiled call graph in one case, recorded execution in the other — and both
  replacements were smaller than what they deleted. When a check must
  reimplement the semantics of the thing it checks, that is the signal to
  delete it, not to narrow it again.
- **Before naming a file as the unit of deletion, ask whether it holds more
  than one invariant.** A dot was written to delete three files whose lint
  "only checks that a date equals today". The file actually implemented two
  unrelated checks. The date comparison was a pure function of the calendar
  with no reachable stable green state, so a red run carried no information
  about the tree. The other check walked every tracked document and rejected
  any line quoting a self-check count, pointing authors at the single source of
  truth instead; it stays green indefinitely and its findings name a file and a
  line. Deleting the file would have destroyed a working invariant to kill a
  broken one that happened to share a filename. The whole-file scope also
  manufactured a caller cascade — an emptied gate phase, a phase-id retirement,
  documentation-map rows, a coverage entry — that vanished entirely once the
  unit became the invariant rather than the file. The test that separates them
  is whether a check has a reachable stable green state, not whether it looks
  like governance.
- **"Zero consumers" misclassifies every hand-invoked tool, because an entry
  point has no consumer by construction.** A cruft audit listed three files as
  verified dead. Only one was: a strict subset of an enrolled test that had
  moved directories. The second was a diagnostic tool with a real usage
  interface, which our own rule about building tools instead of bisecting by
  hand says to keep. For the third the audit misread a line retiring an
  aggregate performance verdict as retiring the file itself. An audit's
  reasons need re-deriving one by one; its confident line counts are not
  evidence. The same sweep also showed that a trust-ledger row can name test
  files as the consumers of an unchecked boundary that none of them load, so a
  column no gate validates is an unenforced claim.
- **Announcing an objection window and then not waiting for it is worse than
  never offering one.** I posted "unless you object, I will fast-forward and
  push", then pushed about three minutes later, and the other orchestrator's
  hold arrived after the push had already landed. Offering a review period and
  consuming it yourself reads as consultation while functioning as notice, and
  it costs more trust than pushing without comment would have. If the window is
  real, wait for an answer; if the change genuinely cannot wait, say that
  instead and own it.
- **Proving you did not cause a failure is not proving the tree passes, and
  choosing your own gate subset is how a red tip gets published.** I moved
  master to a metadata-only chain after running four gates I picked myself, and
  reasoned that a change touching no `.f` file could not affect a source gate.
  The reasoning was sound and the conclusion was still wrong, because the rule
  is "master is always green", not "master is no worse than I found it". Those
  are different propositions and only the first one is the rule. The tip was
  red on two gates I never ran. Worse, I already knew one of them was failing —
  I had run it that morning and built an entire deletion contract around its
  failure — but I had it filed as "the check I am removing" rather than "a red
  gate on the tree I am about to publish", and a fact can sit vivid in one
  mental slot while being invisible in the slot that would have stopped me. The
  guard is mechanical, not attentional: run the full owning gate set on the
  exact tree, and check whether any active tracker entry blocks the bookmark,
  before the bookmark moves. Both of those would have caught it; neither
  depends on noticing anything.
- **Freeze a write set from the callers a change forces, not from the change
  you picture.** Three contracts in two days named a file set that the delivered
  work had to exceed, and every time the gap was a caller rather than a
  surprise: deleting a word migrates whoever calls it. In the worst case I wrote
  "owner: these three files only", then reviewed and accepted a four-file diff
  whose fourth file was the deleted word's only external caller, and never
  compared the diff against the ownership line I had written myself. Derive the
  write set by asking what breaks when each named definition disappears, and
  when a review shows the delivered file set exceeding the contract, that is the
  contract failing its own review — not a scope question for the worker.
- **An acceptance that names a proof the system cannot perform is not an
  acceptance.** Four of these shipped into contracts in one day, and each looked
  rigorous while being unrunnable. A registry byte-identity comparison was
  specified across two child processes, but the child's state dies with the
  child, so there was nothing left to compare. Stderr was frozen as "one distinct
  diagnostic code per failure", but the renderer emits a message form and cannot
  produce codes. A package-reopen rejection was made an acceptance while four
  live files legitimately reopened that package, so passing it required deleting
  working code. Role-specific non-consuming borrows were required from a design
  whose only exit from the owner consumed it, so no operation could construct
  one. The check is mechanical and takes one pass: for every acceptance, name the
  operation that produces the evidence and confirm the system can run it — not
  that the property is desirable, that the measurement exists.
- **Before counting, say what the count has to predict, then check the
  population matches.** A census of "sites touching the migrated words" was
  offered as the write set for sealing a package. Sealing does not break
  consumers of a word; it breaks reopeners of the package, which is a different
  set of files reached by a different query. The count was also taken with a
  plain text search, so comments counted as callers — substring presence
  masquerading as structural evidence, which the rules already forbid and which I
  reached for anyway because it was the quicker query. Both errors were invisible
  in the number itself: it looked like a census and was one, of the wrong thing.
- **Being handed a problem is not being handed the lane it lives in; re-read the
  claim at dispatch, not at planning time.** Told to drive a stop-line incident to
  a reviewable candidate, I put a worker straight into that incident's registered
  workspace. The claim on it named the other orchestrator's agent. I had read that
  claim earlier the same day and quoted it back to them, so this was not ignorance
  — "you own this problem" silently became "you own this lane" somewhere between
  reading the assignment and writing the dispatch. Their worker was announced into
  the same workspace five minutes later; two workers editing one tree would have
  corrupted the only working repair for a red master. The guard is mechanical and
  costs one command: before any dispatch, read the target dot's `Claim:` line and
  confirm the agent name is yours. An assignment changes who is responsible for an
  outcome, never who may write in a workspace.
- **A multi-row result cannot be followed by repeated fallible single-row
  cleanup.** An early close can destroy ownership before a later refusal, while
  hiding the whole result loses already committed peer tokens. Preflight and
  commit reclamation once for the complete set; if it still refuses, preserve
  every row and publish the complete committed result with the terminal owner.
  The same ownership rule applies to device descriptors: caller memory is not
  immutable merely because a pointer was called a view, so retain descriptor
  storage under the package owner until finalization. Authenticate that
  application descriptor before enqueue; the device completion proof should
  prove only quiescence of its one linear session, not duplicate application
  identity in a second package.
- **Size transport storage from one canonical model-info value.** Passing model
  name, valid-token count, token-byte bound, and batch cap separately duplicates
  authority and makes pre-admission capacity proof depend on callers agreeing.
  Parse only requested syntax; derive storage after the engine and scheduler
  publish their canonical limits.
- **A hard cut is one publication, not one untestable edit.** Add the final new
  surface without forwarding the old one, cut internal callers in green commits
  on an unpublished branch, delete the old surface, then publish only that final
  tree. Never claim a child is green if its provider already deleted its inputs.
- **Cleanup stages must pass the returned owner forward.** A successful stop
  usually exposes the next outer owner; the cleanup chain must consume that
  owner or return the exact stage where cleanup refused. A generic "closed"
  result at every intermediate step silently loses the remaining lifetime.
- **Freeze every state source and validation authority before dispatch.** Name
  who mints and refreshes connection metadata, give same-representation input
  roles distinct nominal types, and authorize result slices once at the batch
  boundary before connection-state validation.

- **A locally-absent workspace does not mean the work is unowned.** Two lanes
  were wasted today duplicating work another orchestrator was already doing:
  the FILEMAP/census removal and the lint-lexer paren fix. Both were dispatched
  after checking that the work existed nowhere — true at the moment I checked,
  false by the time the workers finished. The deeper error was in the stale-claim
  audit: it classified a claim as dead because its workspace directory was
  missing *on this machine*, and I acted on that for claims naming another
  orchestrator's agents (`codex-*`). The audit itself got this right for claims
  tagged `machine=spark` and refused to release them; I did not extend the same
  caution to a peer session's claims on shared infrastructure. Two rules:
  before dispatching anything cross-cutting (shared tooling, a subsystem
  removal, a lint every lane runs), `jj git fetch` immediately beforehand and
  re-check — not once at the start of a stretch; and never release a claim
  naming another orchestrator's agent on local absence alone. Ask, or leave it
  claimed.

- **A proof that restates its own definition proves nothing.** Measured across
  the three compiler-substrate models: 176 published results, of which 43 are
  explicit counterexamples or negative statements — those cannot be vacuous.
  But some are near-restatements: `arena_push_appends` proves the push appends
  when the model *defines* it as appending. That is scaffolding wearing the
  costume of a result. The test for worth is: holding the model faithful, could
  a plausible change to the Habu code falsify this? Mutate the CODE, not the
  model. If nothing in the implementation can break it, demote it to an internal
  lemma or delete it — publishing it in a manifest inflates apparent coverage.
  The real value this session came from statements that could NOT be proved:
  every one of the seven defects was found when a worker tried to state a
  universal property and the code refused it — an operation belonging to no
  block, two values claiming one block-argument identity, a rollback that
  revalidates a stale index, a liveness probe that lies after a throw. Tests
  spot-check and pass exactly where those fail. Keep fewer, sharper statements.

- **A green parity gate does not mean the model is bound to the code.** Auditing
  the checker and identity models, the sharpest tool was mutating
  `src/core/checker.f` and `src/compiler/ir/id.f` and rerunning the owning gate.
  Three mutations that should have been loud were silent: halving `CF-PUSH`'s
  control-frame ceiling from 32 to 16, lowering `MATCH`'s depth guard from 30 to
  10, and letting `INT-WIDENS?` accept any same-class pair so `idx` and `len`
  become interchangeable. Each left `test/compiler/checker-model-proof.f` at
  exit 0, even though `Control.v` publishes results about all three. The reason
  is structural: that gate binds the model to the checker through frozen tables
  and a handful of shared program vectors, and it was ten vectors. Everything
  else the two models state was held only by a reader's promise to keep them
  faithful. Two new vector pairs closed the widening and frame-ceiling gaps and
  both mutations now turn the gate red. The lesson generalises: for every model
  ask "which clause of the gate goes red if I break this in the code?", and if
  the answer is none, the statement is documentation, not verification.

- **A guard that only ever rejects is not bound by a program it rejects.**
  Closing the last two gaps of that audit, the obvious vector for `MATCH`'s
  depth guard — 31 nested `begin`s and then a match, which the checker refuses —
  turned out to bind nothing at all: the body is unbalanced, so it is refused
  whatever the guard's number is, and it is refused just the same when the guard
  is deleted outright. The vector that binds a guard is a PAIR straddling it
  whose verdict changes CLASS. Here it is a match at depth 30, where the form's
  two frames take the last two slots so the next opener overflows and the body
  becomes UNCHECKABLE, against the same shape at depth 31, where the guard's
  hard reject fires first and outranks that: lower the guard and the first flips
  to a refusal, delete it and the second flips to an uncheckable. Same shape for
  the linear conservation count: all three existing linear vectors answered
  identically with the count check made a no-op, because the deferred-taint rule
  was deciding them. The count only decides when the value is on NEITHER row,
  which is an ordinary word carrying a to-r effect — the model's own prose had
  already written that case down, and nobody had turned it into a vector.

- **Duplication with a generated obligation is its own kind of padding.** Nine
  of eleven published `IdAllocator.v` examples were emitted verbatim by
  `test/compiler/ir-id-obligations.f`, which builds them from the frozen schema
  and from constants read structurally out of `src/compiler/ir/id.f` — so the
  generated copy was the one bound to the code and the committed copy was a
  second name for it. `zero_is_initial_not_serial` hardcoded 0 where the
  generated `habu_next_serial_initial` reads `0 NEXT-SERIAL !` from the source;
  changing that initial value turns the gate red at the generated row only.
  Before publishing a concrete example, check whether the gate already generates
  it, and whether the generated form is the stronger one.

- **Mutating the code sorts vacuous proofs from load-bearing ones, and reading
  them does not.** Auditing the three substrate models against the worth test,
  the results that read most trivially split two ways. `arena_reads_ignore_capacity`
  is one line of `reflexivity` and looked like a restatement — but bounding
  `IR-ARENA:IDX-AT` against `ACAP@` instead of `ACOUNT@` reds a vector row, so a
  faithful model would have to make reads depend on the span and the statement
  would be false. `intern_deterministic` reads like a real property and is not
  one: every Rocq definition is a function of its arguments, so it is provable
  of an implementation that reads a clock. The discriminator is whether a code
  edit exists at all, not how the proof script looks. The same runs also
  measure the GATE: three code mutations left the parity gates fully green
  (IR-ARENA:ABORT never retiring its slot, IR-CTX:DEPTH-ROOM's bound raised),
  which is a binding gap, not proof vacuity — the theorem is fine and the
  vector table is missing a row. Write both down separately.

- **jj loses nothing; my search did.** I twice reported work as "destroyed" —
  four task records after an `op restore`, and a worker's commits after a
  rebase. Both claims were false. All four records were sitting in commit
  `04db5383a788`, reachable from the current operation, and I recreated them
  from scratch for nothing. The cause was a broken search: `jj file list -r
  'all()'` **errors** ("resolved to more than one revision") and my loop
  swallowed it as "0 matches", and `files("glob:...")` silently matches
  nothing while `files(glob:"...")` works. So a sweep across 500 operations
  reported clean while checking literally nothing — the same
  gate-that-checks-nothing failure I had spent the day finding in other
  people's tools, committed by me, against my own history.
  Rules: validate a search on a case known to be POSITIVE before trusting any
  negative result from it; a command inside `$( )` in a loop condition hides
  its exit status, so check rc explicitly; and before declaring anything lost,
  search with a validated method across `jj op log` — abandoned commits stay
  reachable via `--at-op`. Genuinely unrecoverable means never committed
  anywhere, which for an agent workspace means it was `rm -rf`'d before any jj
  command snapshotted it.

- **The recovery engine loads the boot prefix twice, and the second load must
  shadow the first.** A Gforth-built stage0 engine reads every prefix file from
  disk at startup (`PFX-LOAD-*` in bootstrap/cg/forth.fs emits `LSRCRD` calls on
  baked paths) and THEN interprets its baked program, which is the prefix all
  over again plus a driver. So `src/core/checker.f` is read twice in one process.
  You can tell the two loads apart in a trace: only the startup load runs
  src/core/include.f, and only the baked program runs src/habu/habu1.f.
  The prologue `emit_boot_hide` in tools/bootstrap.sh is what keeps the second
  load from inheriting the first's dictionary and effect rows, and it was
  emitted only for the stage builds. Without it, `trust` and `checker-defer`
  from the startup load stay resolvable while checker.f is being re-read, so
  every `defer` declared before checker.f's own `: TRUST` published into the
  checker that was being replaced, nothing landed in the pending pre-trust
  table, `DRAIN-PRETRUST` replayed nothing, and the first checked `is` on such
  a defer (src/habu/xref.f `is PKG-LIVE-XT`) could not certify: exit 70, which
  reddened the whole no-binary recovery path. Lesson for next time: when a
  registration "happens" and is then invisible, ask which INSTANCE of the
  registry received it before suspecting the code that registers.

- **Interleave the two probes on one stream, and distrust an inherited
  diagnosis.** The dot for that bug recorded, from an earlier lane, that the
  replayed `trust` and `checker-defer` calls never entered their definitions.
  They entered every time, for all 31 slots. What settled it was putting the
  engine-side probe (write the slot name from the replay loop) and the
  checker-side probe (`type` the name at the head of `: TRUST` and
  `: CHECKER-DEFER`) on the SAME file descriptor, so their order was real
  evidence instead of two logs to correlate. Ten minutes of that replaced a
  week of "the faulty instruction is not yet pinned". Inherited findings are
  leads, not facts; re-measure the one the whole diagnosis rests on.

- **Readiness answered by existence is a proxy, and proxies go stale.**
  `C-PRETRUST-READY?` asks "is a word named `trust` resolvable" when it means
  "is this load's checker live". Any older instance satisfies the proxy. Fixing
  the situation that produced a stale instance is a real fix and can be the
  right one to land, but it is not the same as fixing the proxy, and saying so
  out loud is what keeps the second one from being forgotten (dot
  habu-make-pre-trust-f18dd43a).

- **A sealed handle can be stored without a forging cast.** The IR builder has
  to keep fifteen `IR-ARENA:arena` handles and a module key alive between
  calls, and the obvious way — stash the raw cells and re-mint them with a
  private `CAST:` — would have given the package a forging window over another
  package's sealed family, which is exactly what `arena.f` and `op.f` refuse to
  do. `TYPED-BUFFER NAME <family>` (and `TYPED-VARIABLE`) store the nominal
  itself, so the handles round-trip with no cast anywhere. Reach for typed
  storage before inventing a converter; a stored handle is not a reason to
  break a seal. The context handle is the exception and stays out of storage:
  `IR-CTX`'s whole lifetime argument is that no context handle survives its
  `WITH-CONTEXT` body, so the builder takes the context from its caller on
  every mutating word and checks it against the owner serial instead.

- **A per-process stage claim needs a way to be reclaimed, or one failed test
  poisons every later one.** `IR-OP` and `IR-FUN` keep one open record each per
  process, so the builder records which builder generation holds each stage.
  The first version simply refused a second claimant — and then a fixture that
  threw with a record open held that stage for the rest of the enclosing
  context, because a context abandoned by a throw keeps its registry slot until
  the nearest enclosing live context leaves normally, so its builder still
  looked alive. Two fixes, both needed: the claim is reaped when its holder is
  no longer a live builder, and every fixture that throws with a record open
  gets a harness context of its own, the way `ir-fun.f` already splits its
  negative cases. A claim with no reclamation path is a leak with a good
  excuse.

- **A model example that names a type family has to supply the registry that
  family is declared in.** Until the `MATCH` scrutinee pop was modelled, the
  checker-model examples matched on `fmres` under `sig` — the EMPTY family
  registry — and answered the same verdict as the real checker for a reason the
  real checker never had: with no registry entry the declared family is one
  unexpanded logical cell, while the shipped `SUMTYPE cmres` is a two-cell
  bundle. Both machines said "certified" about two different programs. Modelling
  the pop made the width load-bearing and forced every one of those examples,
  and the frozen vectors under them, into a real registry (`sig_fam`). The
  general lesson: when a model abstracts a value's SHAPE, examples that use it
  stop being about the same program, and the agreement they show is worth
  nothing. Model the shape, then the examples have to say which shape they meant.

- **A falsifying mutation that touches something `lib/` depends on never
  reaches the gate.** Truncating the `MATCH` scrutinee walk to two cells — the
  intended way to show the multi-cell row does work — stopped
  `FIND-EXECUTABLE-IN-PATH` (lib/process-env.f) certifying, because it matches
  an `option` whose payload is itself a multi-cell layout, so the fixpoint
  self-check refused the build and no vector was ever asked. The usable mutation
  had to name a width nothing in `lib/` uses (three cells). That refusal is
  itself evidence worth recording: the shipped library cannot be built if the
  walk stops early, which is a stronger statement than a red row. When a
  mutation cannot be built, say what it broke instead of weakening the row.

- **`catch` restores the stack depth, not the values a locals frame consumed.**
  A fixture that needs its arguments back after a refusal must keep them on the
  data stack (`2dup WORD`), not bind them with `{: :}` and push them again: on
  the throw path the locals frame is gone and `catch` hands back whatever
  happened to be at that depth, which reads as a stale handle several fixtures
  later. The working pattern was already in the file next to the broken one.

- **A frozen-body proof fixture is a design review, not a diff nuisance.**
  Keying operation attributes needed two pool cells per attribute, and the first
  shape stored the ENTRY count in the window-length field and multiplied by the
  stride inside `TILE-CK`. `ir-structure-proof.f` refused it, because
  `formal/Common/Structure.v` is proved against those exact bodies and its
  window length means POOL CELLS. Storing the entry count would have made the
  model's contiguity claim false of the shipped row while every runtime test
  stayed green. Storing the cell length instead left all four tiling bodies
  byte-identical and moved the stride to the one place that addresses an entry.
  When a pinned body fails, ask which of the two the model wanted before
  reaching for the pinned string.

- **Locals are single-assignment; a running accumulator needs a named cell.**
  There is no `to` for `{: :}` locals. Three walks in the verifier carry one -
  the window tilings, the dominator rounds, the block bisection - and each keeps
  it in its own `variable` with a comment saying why they cannot overlap.

- **In nested `?do` loops the inner index shadows the outer.** Two verifier bugs
  were the same slip: `i` used for the outer block while `i` was the inner edge
  index, so predecessor lists were written against the wrong block. `j` is the
  outer index inside a nested loop. A four-block diamond fixture found both;
  a single-block fixture would have passed.

- **An order-independence fixture proves nothing until every reference class
  really moves.** The first canonicalization fixture built the same module along
  two intern orders and passed; then mutating the canonicalizer to leave a
  reference unrewritten kept it green in eight of fifteen classes. In each case
  the referent's insertion ordinal happened to be the same in both builds - the
  function name was interned at the same point, the pointer landed in the middle
  of a five-item list that reverses onto itself, and a source registered per span
  gave both builds the same row. The repair was to make each reversed group an
  even-length list walked forwards or backwards so no member keeps its ordinal,
  and to have the fixture assert that four of those ordinals actually differ
  between the two builds. Reversing an insertion order is not the same as moving
  the numbers the code has to rewrite.

- **A word cannot see a `variable` whose name a local shadows.** A test kept a
  build-order flag in `variable REV` and read it inside a word with a local
  `rev`; name lookup is case-insensitive, so `REV @` read the local and the
  checker reported `expected: a ptr a actual: n n` at the store. Naming the cell
  `REV-CELL` fixed it. The package-wide cells in this substrate are already
  spelled `*-CELL` for the same reason.

- **A refusal that throws an out-of-band code is invisible, not fail-closed.**
  `test/engine-error-package.f` asserts that an engine whose `checker-package`
  lookup token is corrupted still fails closed with rc 70, and it got 67. The
  checker was refusing correctly - it genuinely had no package context to give -
  but it refused by throwing `E-PKG-CONTEXT` (7136), and only codes in `[1,255]`
  survive as a process exit status, so 7136 could only ever land in the generic
  top-level reporter as `hb: uncaught throw code 7136` plus `UNCAUGHT-RC` (67).
  A refusal a program is meant to observe has to be raised with the reject rc the
  engine actually carries (70, `RC-REJECT`) and has to name its state on fd 2.
  When an exit status looks wrong, check the width of the code before hunting for
  the missing guard - and remember the exit status alone is weak evidence: 70 is
  also what an undefined word exits with, so the regression must assert the
  diagnostic text too.

- **A guard keyed on the wrong "is it live yet" probe fires after the failure.**
  The first attempt at the above closed the fail-open at the engine side instead:
  `C-FIND-CHECKER` may silently skip a missing checker bridge word, so make it
  skip only while no check hook is installed (`HOOK-CELL`), the same probe the
  compile-immediate path uses for its preflight hook. It was measured, by printing
  `check@` at the throw site, that `HOOK-CELL` is still 0 when the refusal
  happens: the boot prefix reload runs unchecked, and the declaration front ends
  ask for the package context directly rather than through the hook. A capability
  probe is only structural if it is armed before the state it guards.

- **A child-process budget in a test is a deadlock guard, never a performance
  expectation.** `test/compiler/ir-id.f` gave each spawned engine 2000 ms and
  `tools/check-test-lib.f` gave each of its six children a bare `$2710`, and both
  phases turned red only when the gate pool had eight slots busy. Measured on a
  12-core machine: the ir-id concurrency child costs 0.62-1.10 s idle and
  2.34-3.00 s under eight busy slots, the check-cli cleanup child 4.7-5.0 s and
  11.2-13.4 s. Decisive test: raising only the budget in a scratch copy turned
  eight concurrent runs from eight reds into eight passes, so the concurrency
  property held the whole time and the stopwatch was the only thing failing. The
  fix that lasts is not a bigger number - it is writing the budget as a measured
  worst case times a stated margin, so a reviewer can see what it is guarding
  against, plus a verdict that says which of the three things happened. The
  shared `T-OUTCOME-EXITED=` prints `expected 0 got 1` for a hung child, a
  signalled child and a wrong exit code alike, and an expired capture inside
  `RUN-ARGV-CAPTURE` escaped as `hb: uncaught throw code -2502`, naming no case.
  Both hid a load problem behind a line that looked like a real defect.

- **Check whether the file you want to fix can be edited at all before designing
  the fix.** The natural home for named completion-variant diagnostics is
  `lib/test/outcome.f`, but that file defines its three assertions at global
  scope with no package, so `tools/package-diff-lint.f` reports
  `E-PACKAGE-OWNERSHIP` on any change to them - measured with a one-character
  edit. Unpackaged global surfaces are frozen against edits, not just against
  additions; the fix went into the calling test packages and the library work
  became its own dot.

- **A mutation that moves a guard is not the same experiment as one that removes
  it.** Falsifying the reserved-register half of the instruction-encoding gate,
  the first attempt changed `XREG?` in `src/arch/arm64/asm.f` from refusing x18
  to refusing x19. The gate reported zero failures, which looked like a hole in
  the gate. It was not: three encoding vectors legitimately use x19, so the
  moved guard killed the gate process before it reached a single assertion, and
  a dead run and a clean run print the same nothing to a `grep TFAIL`. Deleting
  the guard body instead - the mutation that actually models "somebody removed
  the check" - turned exactly the twelve refusal rows red and nothing else. Two
  rules came out of it: mutate by deleting the thing under test, not by moving
  it somewhere else the fixtures also use, and never read "no failures" from a
  filtered run without also checking the run reached its report line.

- **A `case` default cannot answer with a value in this Forth.** `ENDCASE`
  consumes the selector, so a default arm that pushes a result leaves the stack
  one item short and the checker rejects the word at `endcase` with
  `expected: n actual:`. Every default in the tree throws for this reason. A
  total function over a small enumeration is better written as named predicates
  and early exits than as a `case` with a fallback, which also reads better:
  `FORM-ARITY` in `test/compiler/insn-schema.f` asks `NULLARY-FORM?`,
  `UNARY-FORM?`, `BINARY-FORM?` in turn and answers 3 at the end.

- **The Forth source lexer the Rocq parity gates read `.v` files with does not
  know Coq comments.** `test/compiler/*-proof.f` counts `Admitted` and `admit`
  tokens structurally through `COMPILER-ID-SRC:SCAN-FILE`, which strips `\` and
  `( )` comments - not `(* *)`. A header sentence in `formal/Common/Insn.v` that
  said "the ranges the ARM64 encodings admit" failed the no-admitted-statement
  check. Prose in a committed `.v` file has to avoid `admit`, `Admitted`,
  `Theorem`, `Corollary`, `Module`, `End` and `Print Assumptions` as words until
  the lexer learns the comment syntax.

- **Truncating a dictionary does not reclaim its heap, and a snapshot copies the
  orphan verbatim.** The native refresh hides the previous generation's
  dictionary back to the primitive boundary (`src/habu/hide.f`) and reloads the
  prefix from source, but DP never moves, so 4.48 MB of the previous run's heap
  sits below the live generation's first allotment holding that run's mmap
  addresses and region pointers - 50 of the 113 cells that differed between two
  builds of one snapshot image. A span with a structural endpoint fixes the whole
  class at once; a list of offsets never can.

- **Find the owner with a tool, and the byte offsets stop mattering.** The
  twenty-row `SND-QUARANTINE` offset table had rotted so far that none of its
  rows named a cell that actually drifted, and twelve of them were clearing live
  checker buffers. What made it fixable was `tools/snap-heap-owner.f`: walk the
  dictionary, recognise a heap owner by the one fixed shape `create` compiles,
  and read the address it owns out of the instruction immediates. 1793 owners,
  every drifting heap cell named, no value guessing anywhere.

- **Confirm which subsystem a symptom belongs to before spending the lane on
  it.** A restored image dying while compiling a definition was recorded as one
  more undeclared DP-heap cell. It is not in DATA at all: an lldb search of the
  whole DATA region finds nothing, and the disassembly at the fault shows the
  four-instruction MOVZ/MOVK x9 chain in region code pushing a quotation's entry
  address for `[: ;] catch`. `EM-SNAPSHOT-REBASE-DICT` walks dictionary records
  only, so no address literal compiled into region code is ever relocated. Ten
  minutes of debugger evidence moved the work to the right dot instead of
  producing a plausible-looking DATA fix that would have changed nothing.

- **A rigid host identity in a candidate's own signature is not an identity.**
  `fresh-region-a` in the signature of the definition under check parses to a
  TEMPLATE slot with a negative kind, and `ATOM-OK?` (src/core/checker.f)
  refuses a negative kind outright — so `( fresh-region-a -- fresh-region-a )`
  with an empty body is REFUSED, and so is a trusted word declared that way
  trying to consume what another one produced. Only a call site mints
  (`E-I-AK`), once per template slot per instantiation. Anyone writing fixtures
  for the identity domains has to produce the identity from a trusted
  constructor and consume it through a type variable; naming the same `fresh-*`
  spelling twice proves nothing, because the two spellings are two arena
  entries. The sharp fixture for the domain rule is instead two constructors in
  DIFFERENT domains: each is the first mint of its own counter, so both ids are
  1, and the refusal can only be coming from the name. Dropping `ATOM-OK?`'s
  name comparison flips exactly that row and no other.

- **A model whose executable configuration cannot hold the real constant should
  make the constant a parameter, not a smaller lie.** The checker's `RIGID-MAX`
  is `$4000000000000000`; a unary Rocq `nat` cannot represent it and
  `vm_compute` would try to build it. `formal/Common/Effects.v` therefore takes
  the bound as an argument, states every result about it for EVERY bound, and
  runs its own executable configuration at a small one — which only ever
  refuses SOONER, so the model still rejects more than the checker and never
  accepts more. The checker's own literal is then held structurally instead:
  the parity gate reads the guard, the advance and the per-check restart out of
  each mint word's body, derived from the counter's name so the name is written
  once.

- **Two builds of one snapshot image, diffed and split by section, separate
  relocation classes faster than any single crash does.** Build the image
  twice from an unchanged engine, `cmp -l` the two files, and bucket every
  differing byte into header/text, region, DATA, trailer and the trailing extra
  section using the trailer's own region and data lengths (region file base =
  trailer base - data length - region length). Region bytes below `DICT-SIZE`
  are dictionary records; above it they are compiled code. On this tree the code
  bucket was 310 differing words, every one of them the second instruction of a
  four-instruction MOVZ/MOVK x9 chain whose value moved by exactly the region
  base delta — which is what proved the address-literal class was real before a
  line of it was written, and what proved it gone afterwards (310 -> 0). Two
  traps: `rg --byte-offset` reports the offset of the LINE, not the match, so
  locating the trailer that way was 16 bytes wrong and made every record look
  like garbage; and `bin/hb --load tools/imgdump.f -- --data <img> <off>`
  answered 0 for a cell whose bytes were plainly non-zero, so read persisted
  DATA out of the file at the computed base rather than through that path until
  it is fixed.

- **A wild jump in a restored image is not evidence of WHICH relocation class
  is missing.** Three different defects in this campaign all present as "jump to
  an address the writing run had": an unrelocated call, an unrelocated address
  literal in code, and a persisted DATA cell that holds an execution token and
  was never declared. Tell them apart before writing anything: search the live
  region for the MOVZ/MOVK chain that would build the crashing value, search
  DATA for the value itself, and — decisively — read the image FILE at the same
  DATA offset. Present in the file means persisted; absent means it was computed
  at run time from something else. In this lane the crashing value was absent
  from the region as a chain and absent from the file as a cell, and an lldb
  write-watchpoint set at `process launch --stop-at-entry` (the only point early
  enough to beat the crash) caught it being written by the snapshot loader's own
  DATA copy, which is what finally named the owner.

- **A cell that holds an execution token has to be a `defer`, not a
  `variable`.** `CHECKER-CERT:PRODUCER-XT` and `LOWER-CERT:FULL-XT` were plain
  variables that an `execute` dispatched through, so nothing declared them to
  the snapshot address-cell table the way `defer`/`is` declare a dispatch cell —

- **A typed local cannot name a structure wider than one cell.** `{:
  f:CTARGET:features :}` binds fine (one cell), but `{: sp:IR-SOURCE:span :}`
  is rejected outright with `unknown type 'sp:IR-SOURCE:span' in signature`,
  and the definition is not certified. So a word that receives a multi-cell
  value has to `UNMAKE` it at entry and bind the pieces — which means the
  value's cells have to be reachable, i.e. it must be the TOP input. That is a
  real constraint on interface design, not a style note: `NTAPE:token` puts its
  span field last precisely so `NTAPE-TOKEN:UNMAKE IR--SOURCE-SPAN:UNMAKE`
  works in one line, and `NTAPE:PUSH-FROM` takes its parent ordinal above the
  token for the same reason. Design the argument order around the unmake before
  writing the body, or the word cannot be written at all. Dotted as
  `habu-bind-multi-cell-d2e153ed`.

- **An enum member list takes no trailing comments.** `ENUM kind ... name \ a
  name` fails with `bad enum declaration 'kind': name must be a lowercase tail
  at '\'`. Put the prose above the `ENUM`.

- **Sixty-four arena slots is a real budget for a test suite.** `IR-ARENA` has
  `SLOT-MAX 64` live-plus-frozen slots, reclaimed only when the owning context
  dies — and a fixture that throws leaves its context alive until the enclosing
  harness context exits. `IR-SOURCE`'s suite gets away with one harness because
  a registry is one arena; a suite whose module is four arenas (source
  registry, symbol pool, symbol rows, tape) runs the registry dry halfway
  through and then every remaining case fails for the wrong reason — the
  symptom is `E-IR-ARENA-SLOTS` (-6657) and a run of unrelated red cases. Split
  the run into several harness contexts so each group's aborted contexts are
  swept before the next group allocates.
  and a restored image jumped to the writing run's address on the first checked
  definition. Both are now defers and that failure is gone. The declared-kind
  design is only as complete as the set of ways a cell can come to hold a token,
  and `variable` + `execute` was outside it.

- **"Is this constant across runs?" is the cheapest way to tell a persisted
  stale address from a live one.** Run the same restored image three times and
  compare the crashing program counter against the live region base printed in
  the same dump. If the crash address is byte-identical every time while the
  region base moves, the value is baked into the image and no amount of reading
  the code that produced it will tell you more than that. Confirm it by
  searching the image FILE for the same 64-bit value little-endian: one
  contiguous match means a data cell, and no match means a MOVZ/MOVK chain in
  code instead, because a chain never stores the address contiguously. Two
  commands, no debugger, and it splits the whole relocation-bug family in half.

- **A quotation is a compile-time construct, so `[: … ;]` at the top level of a
  prefix file is `E-UNDEFINED: [:`.** Every `is` site in the tree wraps the
  hand-over in a one-shot install word (`: X-INSTALL ( -- ) [: W ;] is HOOK ;`
  then `X-INSTALL`) for exactly this reason. Converting a top-level
  `' W SOME-INSTALL` to a quotation means adding that seam, not just changing
  the tick.

- **The declaration points are `defer` and `is`, and neither can name a cell
  whose address is computed at run time.** A table of callbacks — the
  declaration-transaction participant rows, five quotations per participant in a
  `create`d DP-heap array, run with `catch` — is the same persisted-token defect
  in a shape no `defer` can express. When a fix converts the single-cell cases,
  check for the array-shaped ones before believing a restored image is well:
  ours compiled definitions happily and still died on the first `sumtype`.

- **Never delete a lane workspace before the main workspace has re-verified
  with a binary that can compile the new tree.** The callback-declaration
  landing added a new primitive (`xt!`); the lane's freshly built `bin/hb` was
  the only binary that could compile the tree, and I deleted its workspace
  before re-running gates in the main workspace — every gate then exited 70
  (the old binary rejects the unknown primitive), after the bookmark was
  already pushed. Recovery was the no-binary bootstrap path, which worked on
  first try — its own fix from earlier the same day. Order for engine-affecting
  merges: rebase, refresh/install in the MAIN workspace (or copy the lane's
  binary first), run gates, only then move the bookmark, push, and delete the
  lane workspace.

- **Timing a word: seven short runs beat three long ones, and normalising
  against an empty call does not cancel host load.** The codegen comparison
  harness first timed each word three times at a million repetitions and kept
  the fastest. On a 12-core host with 16 competing busy processes, one case had
  all three of its runs hit by the same sustained scheduling delay and came out
  4.1 times its idle cost — a false alarm no useful tolerance can absorb.
  Switching to seven runs of 250,000 repetitions cut the worst drift to about
  3x AND made the whole pass twice as fast (0.5 s against 0.9 s), because the
  fastest-run rule gets more chances to find a clean scheduling window.
  Expressing each cost as a multiple of an empty call measured in the same pass
  is still worth doing — it is what makes the numbers portable between machines
  — but it does not cancel load: a two-nanosecond empty call can find a clean
  window while an eighty-nanosecond body cannot, so under load the ratio grows
  rather than staying put. Budget the tolerance from measurements taken under
  deliberate oversubscription, write the measurements down next to the constant,
  and say plainly that a timing gate catches catastrophic regressions only.

- **Read a compiled word's size from its own dictionary record.** `XREF-FIND`
  returns the record, `XREF-START` its code address and `XREF-LEN` the number of
  bytes of machine code the engine emitted for it. That is a two-line, fully
  checked way to measure code size from inside a running image, with no dumper,
  no disassembler and no second copy of the compiler's own accounting.

- **A vocabulary a later stage must switch on belongs in an `ENUM`, not in a
  string.** The straight-line HIR dialect first bound a source word to an
  opcode by storing the opcode's interned symbol, which needed a runtime
  existence check against the schema table — and `IR-BUILD` hands out no live
  reader for it, so the check could not be written at all. Storing the opcode's
  `ENUM` code instead makes the closed world of design section 5.3 a property of
  the type: naming an operation the dialect does not have is unwritable, and the
  decoder refuses a code outside the five at first touch. When a check you want
  turns out to be impossible, ask whether the thing being checked should have
  been a type.

- **A package cannot be reopened after `get-current prot-wid-add`.** The
  protection idiom at the foot of every substrate file seals the package's
  wordlists, so a second file that reopens the package dies at load with the
  package name as its whole error message (exit 84). Two files that belong
  together either share one package and only the last one seals it, or — better
  — become two packages with a one-way dependency. Watch the generated enum
  namespace when picking the name: a hyphen inside a package name is doubled, so
  package `HIR-WORD` with an `ENUM meaning` generates `HIR--WORD-MEANING:`.
  Putting the enum in the hyphen-free package it really belongs to is the fix.

- **A test fixture that throws holds its context until the enclosing harness
  exits, so throwing fixtures must be cheap.** An `IR-BUILD` module owns fifteen
  arenas against `IR-ARENA`'s sixty-four slots, so four throwing fixtures in one
  harness group exhaust the registry and every later case fails with
  `E-IR-ARENA-SLOTS` instead of its own code. Build the negative fixtures on the
  smallest thing that exercises the path — for a table keyed by module symbols
  that is a plain `IR-CTX:NEW-MODULE` key plus an `IR-SYM` pair, five arenas, not
  a whole module builder.

- **`CAST:` can take a value out of another package's type family but never put
  one back in.** The checker refuses a cast whose *output* is a cell family
  another package declared (`E-CAST-OWNER`, throw 7135, and the load dies with
  nothing but that number). So a package cannot mint a proof token by retyping
  an `IR-ID` identity it was handed. A one-field `STRUCTURE` that *carries* the
  identity does the same job honestly: the generated `MAKE` and `UNMAKE` wrap and
  unwrap it without claiming the power to create one. Declare that structure in a
  `public` section — a `STRUCTURE` (or `ENUM`) declared while the package is
  `private` compiles, but its generated `MAKE`/`UNMAKE` cannot be named, and the
  load fails with `E-UNDEFINED` on the constructor.

- **A proof token turns "every declarer must check first" from a comment into a
  signature.** `HIR-WORD`'s row appender now takes an `interned` rather than a
  symbol id, and the only two words that make one are the two ways to ask a
  module's interner whether the symbol exists. A later declarer that forgets the
  check does not compile, which is a much better guarantee than a note above the
  appender saying to remember it.

- **A partly changed locals group makes `typed-local-diff-lint` report the line
  after it.** The lint only sees added lines, and it tracks whether it is inside
  a `{: ... :}` group across them. If the opening line changed but the closing
  `:}` line did not, the closer arrives as unchanged context, the lint never
  leaves the group, and the first bare word on the next added line is reported as
  an untyped local. Reflow the group so its closing line is part of the change;
  do not silence a real-looking finding with an allow-comment.

- **A compiler-suite fixture that refuses leaks its context's arenas until an
  enclosing context leaves normally.** `IR-CTX:WITH-CONTEXT` releases its
  mapping on the throw path, but the registry slots of an abandoned context are
  reclaimed only when a live enclosing context exits (the note on stale handles
  in `src/compiler/ir/context.f`), and `IR-ARENA` sweeps a slot only when its
  owner is no longer live. A module holds about seventeen arenas and the
  registry holds sixty-four, so a group of refusal cases run at the top level
  exhausts it after three or four and every later case fails with
  `E-IR-ARENA-SLOTS` (-6657) instead of its own error. Run each group inside a
  `WITH-CONTEXT` that leaves normally, keep at most two or three module-building
  refusals per group, and give each positive case that builds two modules a
  context of its own.

- **A pass that reads one module and writes another cannot name the first
  module's opcodes.** Symbols are module-local ordinals, so "is this operation
  `hir.add`" has no answer from outside without either the source dialect's own
  authority or a second copy of its spellings. Restating the spellings is the
  drift-prone answer. Asking the source dialect for its opcode identities while
  its module is still being built, and recording which module the answers came
  from, keeps one authority and turns "bind the module you are about to select"
  into a check rather than a usage rule.

- **A local shadows the package word of the same name, and the error lands on
  the callee.** A private reader `KEY ( -- IR-ID:ir-module-key )` and a local
  named `key:IR-ID:ir-symbol-id` in the same package do not coexist: locals
  resolve first, so every `KEY` inside that word became the symbol, and the
  checker reported the mismatch at the frozen reader that consumed it rather
  than at the local that caused it. The shipped substrate files all use a
  three-letter `KEY` reader, so a comparison local must be spelled something
  else (`want`, `sym`) - and a "expected ir-module-key actual ir-symbol-id" on a
  reader you did not change means look at the locals group, not at the reader.

- **Emitted machine code can be executed inside the same process, and that is
  what makes a byte table worth anything.** `cp@` answers the free code slot,
  the trusted-only `patch32` stores one instruction word into it, and
  `ffi-call-bounded` calls it as a C-ABI leaf routine with arguments in
  x0..x7 - the pattern `lib/ffi-test.f` already uses for its hand-assembled
  stubs. Two consequences worth keeping: the publishing word must be inside a
  definition (a top-level `cp@` patch overwrites the line being interpreted),
  and the address of each instruction should come from the emitter's own source
  map rather than from four times its index, so a map that lost a row stops the
  program from running instead of being checked only where a test looks.

- **A mutation can survive because a neighbouring check makes it
  behaviour-preserving, and that is a seam and not a test gap.** Replacing the
  native emitter's checked register reader (`A64RAV:REG@`) with the allocator's
  raw claim (`A64RA:CLAIM@`) left the whole suite green, because the emitter
  already probes acceptance, freshness and module identity before it reads a
  register. No test can tell the two apart; only review can. The real answer is
  to stop the raw claim being readable at all - the allocator publishes it
  publicly today, so its own header's "the validator is the only door" is a
  convention rather than a structure (dot habu-close-the-alloc-af5b68a2).

- **Two refusing compiler fixtures that each build two modules are already one
  group too many.** The registry note in `src/compiler/ir/context.f` bites at
  four modules, not at four fixtures: a group holding one abandoned two-module
  context plus another one dies with `E-IR-ARENA-SLOTS` and the second case
  reports "expected true got false" for a refusal that really happened. Budget
  a refusal group by MODULES abandoned, not by cases.

- **A green gate proves nothing unless the tree under test is the tree being
  landed.** A landing script computed a revision into a shell variable, the
  lookup silently matched nothing, and `jj new` with an empty argument checked
  out a tree WITHOUT the work being landed - after which every suite "passed"
  vacuously and the bookmark was pushed without the commit it was supposed to
  carry. Two rules: after any checkout, confirm a file the change adds is
  actually present before running gates; and never interpolate a revision
  variable without failing loudly when it is empty.

- **A quotation cannot see the enclosing word's locals, and the engine says so
  with exit 75, not with a checker diagnostic.** `[: c b tp SOMETHING ;] catch`
  inside a word that bound `c b tp` as locals dies at COMPILE time with a bare
  `c` on stdout and rc 75 (`src/habu/habu2.f`: "local referenced inside a
  quotation"), which reads like a crash rather than a type error and points at
  no line. If a quotation needs state from its caller, park the state in a
  `TYPED-BUFFER` and read it inside the quotation.

- **The engine hands the check hook a RECONSTRUCTED definition, not the file's
  bytes.** For `: NAME ( sig ) body ;` the checker receives `NAME ( sig ) body`
  - no leading `:`, no trailing `;`, backslash comments already stripped and
  runs of whitespace collapsed to one space. So every byte offset the checker
  reports is an offset into that text, a `( ... )` comment is still in it while
  a `\ ` comment never was, and anything that wants a file and a line has to be
  told the file separately. Probe it before assuming: a temporary print in
  `CHECK-RESET` and at the `DO-TOK1` call site in `CHECK-SCAN`, gated on a
  marker in the text, answers in one run - an installed `bin/hb` re-reads
  `src/core/checker.f` from disk at boot, so no rebuild is needed.

- **A digest over interned identities cannot see a spelling.** Two source texts
  that differ only inside a name produce byte-identical stage N0 tapes: each
  module numbers its own symbols, so both names take ordinal zero, the spans
  have the same length, and every stored cell matches. Any result that wants to
  say "this is the source I read" therefore has to bind the source registry's
  content digest as well as the tape's - and a test that only compares tapes
  will happily pass while the two texts are different programs.

- **When a producer and a consumer disagree about a shape, ask which of the two
  is describing something real.** The stage N0 tape producer records what the
  engine actually hands the check hook; the elaborator was matching an opening
  `:` and a closing `;` by SPELLING, and neither will ever be on a produced tape
  because the engine consumes both before the checker sees anything. It was not
  a case of "one of the two has to move" on the merits - only one side was
  describing a real token stream, and the other side's tests had been building
  the tape its code wanted. The repair was not to teach the producer to fake
  frame rows but to find what the tape DOES record that draws the same boundary:
  the parser mode. `:` parses the defined name from the outer interpreter before
  it switches, so the name is the one row marked interpreting and every body row
  is marked compiling. A structural fact already on the tape beat a spelling the
  elaborator held privately, and the elaborator now holds no spelling at all.

- **A suite whose fixtures are built by the code under test's own idea of the
  input proves only self-consistency.** Every `NELAB` fixture hand-lexed
  `: NAME body ;` onto a tape, so the suite was green while the elaborator could
  not read a single tape a real compilation produced. The tell is that no test
  in the file ran the production entry point. The fix that makes the class of bug
  visible is one end-to-end case that starts at `evaluate` and ends at executed
  bytes (`test/compiler/native-chain.f`): mutating the walk to start one row late
  or stop one row early leaves nothing hand-built to hide behind, and both the
  leaf suite and the chain go red together.

- **`IR-SOURCE` stores a length and a digest, never the bytes, and the bytes it
  was given are the engine's scratch.** The text the check hook receives lives in
  the checker's token buffer, which the next compiled definition refills, so any
  stage that has to present the same text again - instruction selection does,
  because it re-registers the source into the machine module and proves it by
  digest - cannot hold the pointer and must not reconstruct the text from the
  original source either (the reconstruction has a trailing space:
  `NF-KEPT ( n -- n ) 4 * ` is 23 bytes, not 22). `NFEED:BEGIN-UNIT` therefore
  takes a caller-owned byte buffer and copies the scan into it, on the same terms
  as the tape's own capacity: the ceiling is the caller's commitment and an
  over-long definition is refused (`E-NFEED-TEXT`), never truncated.

- **The native chain runs end to end from source text, so a comparison harness
  never has to hand-build HIR.** The elaboration suite's fixture already lexes a
  line of Habu onto a tape and hands it to `NELAB:COLON`; lifting that rig into
  `test/compiler/native-source-fixture.f` and the select/allocate/accept/emit
  half into `test/compiler/native-chain-fixture.f` let the codegen comparison
  present `: ADD3 + + ;` and get bytes back. That matters for what the numbers
  mean: with hand-built HIR the byte count rests on the harness author's
  translation, and with source text a reader can hold the corpus body and the
  harness's line side by side. The whole chain worked first try on all three
  covered words, which is itself evidence the stage contracts hold.

- **`using` makes a big fixture extraction nearly free, until a package word
  collides with a global.** Moving ~120 lines out of `native-elaborate.f` into a
  shared package cost almost no edits at the call sites because `using NSRC`
  brings the public words in bare. The one casualty was a reader called `MOD`:
  the engine refuses `E-USING-SHADOW-GLOBAL` (throw 7141) because the global
  `mod` and `nsrc:mod` export the same name. Rename the package word rather than
  qualifying every call - and expect the second error to land at the *next* use
  of the old name, several hundred lines away.

- **Machine code called through the FFI trampoline cannot be timed against a
  Habu word.** An emitted `ret` published into code space and entered through
  `ffi-call-bounded` measures 253 ns per call; the old emitter's empty word
  measures 1.9 ns. The gap is `FFI:RESET` looping over every argument slot, the
  per-argument stores, and the call's bounds checks - not the one instruction
  being entered. So a head-to-head cost column across the two paths is a ratio
  to an empty call of the same kind and nothing stronger, and it must say so in
  the report. The byte counts and the executed results are the columns that
  decide anything until the emitted routines can be entered as Habu words.

- **The engine's data-stack pointer is a register, and naming it turns "the
  allocator must never hand it out" into something no contract can say.**
  `src/arch/arm64/mnem.f` calls x19 XDS, `src/habu/rt.f`'s push and pop are a
  store and a load through it, and `src/habu/habu2.f` measures interpreter depth
  as `(XDS - S0) / 8`. Excluding 19 from `A64EFF`'s general-register mask - the
  same line x18, x30 and 31 are excluded on - means every route into a contract
  refuses it: the set constructor, the single-register constructor, a place list,
  and the writable set an allocator derives. There is no check any pass has to
  remember, because there is no contract to remember it about.

- **An emitted routine is callable as a Habu word the moment its arguments come
  off the data stack: an xt in this engine IS a code address.** `execute`
  (`src/habu/habu1.f` BEXEC) pops the address and branches to it with x19 live,
  which is the same branch `EM-INTERPRET-FIND` makes after a dictionary lookup.
  So the whole publication is one trusted word per arity whose body is
  `execute` - `src/habu/habu2.f` already uses that shape for its own keyword
  dispatch - and no dictionary record is needed to enter the code. What that
  bought was the measurement: the FFI trampoline cost 253 ns per call and made
  the nanosecond half of the codegen comparison undecidable; entering the same
  routine as a word costs 4.3 ns, of which 4.3 is the empty call.

- **A convention that names two kinds of place has to pack the kind INTO the
  element, not into a second list.** `A64EFF`'s ordered interface list is one
  cell because a contract field has to be one cell. Adding a parallel kind list
  would be a second field, and a place list of four with a kind list of three is
  two statements about one convention that can disagree - which is exactly what
  making the interface ordered was for. A kind bit over the five-bit payload
  keeps one spelling per list, so the digest still agrees with `SAME?`; the price
  is two positions (ten instead of twelve), and it is the right price.

- **Renaming the reader is what forces every consumer to be revisited.** Widening
  `regseq` into a list of places meant `SEQ@` could no longer answer "the
  register at position i" - a slot index would have read as a register number in
  the allocator's table. Deleting `SEQ@` and publishing `SEQ-REG@` and
  `SEQ-SLOT@`, each refusing the other kind by name, turned a silent wrong number
  into a compile error at every call site. A reader that answers a payload
  without saying what it is, is the bug.

- **A lowering belongs in the pass that already builds the module, unless its
  input only exists later.** Spill lowering builds a SECOND module because the
  spill plan is the allocator's output and the module is frozen by then. A
  routine's calling convention is known before a single operation is selected, so
  the entry loads and exit stores go in the SELECTOR - one module, which the
  independent validator then reads as operations. The one-authority rule is about
  the emitter never materialising instructions no module contains; it does not
  ask for a second module when the first one can hold them.

- **A diff lint that reads only the added lines gets the parser state wrong in
  both directions.** `typed-local-diff-lint` tracked `{:` … `:}` over added lines
  only, so a locals group opened on an UNCHANGED line looked closed - and a bare
  local added inside it was never reported - while one closed on an unchanged
  line looked open, and the ordinary body words after it were reported as untyped
  locals. Both are the same defect: the state is a property of the new file, and
  every line of the new file is either added or context. Reading context lines for
  the two delimiters (and reporting nothing from them) fixes both.

- **A typed local named `i` silently shadows the loop index.** The selector's
  multi-block walk took the block ordinal as `{: bk:… i:n :}` and then read its
  operations with `bk i OP-AT` inside a `?do` loop. The local wins, so every
  iteration read operation number *bk's ordinal* instead of the loop index: block
  zero rebuilt its first operation three times and never reached its terminator,
  and the only sign of it was `E-IR-FUN-TERM` from `END-BLOCK` — a refusal about
  the block, several layers away from the shadowing. The checker cannot catch it
  because both names are legitimately in scope and both are cells. Never name a
  typed local `i`, `j` or `k` in a word that loops; the walks now use `ord`.

- **A forward branch in an append-only IR needs the target ordinal BEFORE the
  target exists, so compute it instead of patching it.** `IR-BUILD` mints a block
  ordinal when the block is *closed*, and a terminator names its successors when
  it is *built*, so the classic Forth backpatch has nowhere to write. The
  elaborator therefore walks the body twice: a skeleton pass applies the same
  block-creation rules (`if` makes two blocks, `then` one, `begin` one, `until`
  two, `?do` three, `loop` three) and records each opener's join ordinal, and the
  build pass then checks the ordinal it really reached against the one the opener
  branched to. Two independent derivations of one number that have to agree beat
  a patch list that nothing checks.

- **Structured Forth control needs block arguments only where two paths meet.**
  A successor of a two-way branch has exactly one predecessor, so every value the
  compile-time stack holds is defined in a block that dominates it and can be
  read by name — no block argument, no copy. Arguments are needed at exactly two
  places: the join of `if`/`then` and `?do`/`loop`, and a loop header reached
  from both its entry and its latch. That is why the conditional branch carries
  no arguments at all and every argument-carrying edge goes through a stub block
  whose terminator is the unconditional branch: ordinary critical-edge splitting,
  and it falls out of the shapes rather than being imposed on them.

- **Splitting a critical edge in BLOCKS does not split it in VALUES.** The
  elaborator already gave every argument-carrying edge a block of its own, and
  the allocator still had no answer for `MAX2`: the two arms hand the join
  `(a, b)` and `(b, a)`, so coalescing each argument with its feeders merged all
  four into one class holding two values that are live at once. What fixes it is
  a copy per argument in the *predecessor*, because a copy's result is defined
  just before the branch and dies at it — two copies on different edges can never
  be live together, and neither can be live with the argument it feeds. The class
  becomes interference-free by construction. A block with the same long-lived
  values flowing through it buys nothing.

- **A conservative live range must not extend past a value's last use in a block
  it is only live-IN to.** Extending to the end of every block a value is live in
  OR out of looked harmlessly conservative and broke `SUM-TO`: the loop-carried
  limit is live-in to the latch, dies there at the copy that hands it back round
  the loop, and the over-extension made the argument and its own copy look like
  two values live at once — `E-A64RA-EDGE` on a program that is correct. Live-in
  earns `lo`, live-OUT earns `hi`, and the use scan already covers the last use
  inside. Conservatism that costs registers is fine; conservatism that invents an
  interference is a wrong answer.

- **A mutation you cannot write as a fixture, you can still run.** The
  block-argument register clause has no hostile module: the allocator decides the
  registers, so a module with a mismatched edge is not buildable by hand. Editing
  ONE line of the allocator — skip the union for the last argument of each edge —
  produced exactly the mismatch and the validator refused it with its own code.
  Mutate the compiler, run the gate, revert: that is evidence about the check,
  and it is worth more than a fixture that can only be built by agreeing with the
  thing under test.
- **A fixture substitute must be production-load-bearing.** When a rejection
  probe's wrong-type operand dies, pick the replacement from nominals with
  real production consumers, not from whatever is still declared — the M8
  repair first chose pass-id, itself dead, and a later ruling deleted it,
  forcing a second repair. Check "does my substitute have production
  consumers?" before substituting, and pin its resolution with a file-local
  positive control so a lost declaration flips a positive instead of letting
  every negative go silently green (dot habu-add-positive-controls-3eff7393).
- **Do not encode a staged source prerequisite as a tracker blocker when both
  leaves stay active until one terminal gate.** Record the exact prerequisite
  commit in the contract and worker parent; otherwise the dependent leaf can
  never become dispatchable before the terminal gate that closes its provider.
- **Probe protected-word reachability in the exact suite load before freezing a
  test seam.** Top-level interpretation does not imply that a protected internal
  colon word is callable: the engine can reject it before its range or behavior
  is exercised. Use an existing public observation when one exists; otherwise
  name and inventory the smallest test-metaprogramming wrapper in the contract.

## The width of a memory access is a form, not a field

Adding `c@` to the native chain looked like a width attribute on one memory
opcode. It is two opcodes instead, on both sides of the chain, and the reason
is what a schema is FOR: every consumer reads the opcode to know what an
operation does, so a width behind an attribute would make the selector, the
emitter and any later alias pass read a field before they knew what they were
looking at - and a number can name a width no encoder exists for. Two forms
make the closed world do the work: every MATCH has to answer for the byte
access, and an unencodable width is unwritable.

## A memory order crosses an edge the way every other value does

The order is an SSA value, so a loop body that loads reaches its order as a
BLOCK ARGUMENT, handed over by the branch with the rest of the live values.
Two things had to change with it, and both were the honest change rather than
the small one.

The first: a terminator's operands ARE its successor's block arguments, so the
successor declares their types and the opcode's schema cannot. The verifier
already checked them against the destination position by position, so the
schema's tail type simply stopped being consulted for a single-successor
terminator - which is what let one branch carry both cells and an order.

The second: "every order is consumed exactly once" counted over the whole
function is only right for a routine of one block. A two-way branch hands its
successors nothing, so both of them read the order the block above them left,
and a loop's latch and its exit both read the order the body left. Those are
mutually exclusive readers, not a fork of memory. The rule is per path: read at
least once, never twice in one block, and no two readers on a common path that
does not redefine it.

## A dialect that grows can outgrow a context mapping

Two more opcodes tipped a geometrically grown table over its next doubling and
took one machine module from seventeen kilobytes of a context to twenty-seven.
The spill lowering holds two modules of that dialect in one context, so 64K
stopped being enough - and that is a real pass, not a fixture. The mapping is
what gives; the giveaway that it was a capacity and not a bug was that the
failure was E-IR-CTX-SCRATCH in a fixture that had not changed.

## A self-call is cheap; what a call costs is the caller's registers

Recursion looked like the branch. It is not. The branch is a displacement known
at layout, exactly like a block branch, and one instruction. What a call really
costs is that the callee's contract destroys the register pool the allocator
hands out, so for a SELF-call every register the caller holds a value in is a
register the recursive instance writes. Nothing in a Habu word's convention is
callee-saved, and `A64EFF` cannot even express it: a register is destroyed or
preserved, and preserved is the complement of destroyed, so "written and put
back" has nowhere to be written down.

The consequence is where the design lives. The values live across a call cross
it on the CALLER's data stack, below the callee's argument base, through the
same `a64.dstore` and `a64.dload` the routine's own entry and exit already use;
the call site is therefore the routine's exit sequence, the call, and the
routine's entry sequence, and it leaves the data-stack pointer where it found
it. Saying that in the source dialect means `hir.call` consumes every live value
and answers each of them again - a variadic operand tail and a variadic result
tail - so the register allocator sees the two lifetimes a call really splits a
value into rather than one lifetime spanning the call that ends it.

Only one thing genuinely needs a frame: x30. It is not a value of the dialect
(the allocator may never hand it out), so the save and the restore are forms
that NAME the register, exactly as the frame forms name the stack pointer.

## A validator's shape rules are the real cost of a new capability

Adding the call to the dialect was a day's worth of schemas and lowerings. What
took the argument was `regalloc-verify.f`: it knew that a routine's data-stack
traffic was exactly an entry sequence at the top of block zero and an exit
sequence in front of the return block's terminator, and that a routine of more
than one block had no frame at all. Both were TRUE and both had to become
narrower true statements rather than be relaxed - the entry and exit windows
shift by the prologue the contract's traits declare, and every other data-stack
touch has to be a whole call site whose two byte counts are exactly the store
run in front of it and the load run behind it. A capability that cannot be
stated as a shape the validator re-derives is a capability that is not checked.

## A count that becomes position-dependent needs one word, not one convention

Eliding a branch to the next block turns a terminator's instruction count from a
property of its FORM into a property of its form and its position, and the
emitter reads that count twice: once to lay the blocks out and once to write the
bytes. The tempting shape is to subtract one in the layout and remember to skip
one in the writer - two statements of the same rule, and the second one is where
the drift lives. Making it one word both passes call (`FALL-THRU?`, taking the
operation and the ordinal of the block it terminates) costs nothing, because the
question is answered from block ordinals alone and so can be asked before a
single offset exists. What made the design safe rather than merely tidy was
adding the cheap statement of the invariant the rule is supposed to guarantee:
`WALK` holds the instruction cursor against the layout at the start of every
block and at the end of the routine. Both drift directions - writer emits a
branch the layout did not count, writer elides one it did - die on that check
with its own error code, and neither can be built as a fixture, because with one
rule there is no module that produces the disagreement. Mutate the compiler, run
the gate, revert.

The elision also moves a cost that used to be invisible: with every branch
emitted, the layout ORDER was irrelevant to what a routine computed. It is
load-bearing now, so the price has to be written where the order is decided, not
only where the branch is skipped.

## An optimiser that fixes a register on purpose finds every constraint that was holding by luck

Copy coalescing is the first pass in the native chain that decides a register
for a reason other than "the scan got here and this one was free". The first
thing it did was turn `test/compiler/native-chain.f` red with `E-A64RAV-TIE` on
the one fixture that materialises `-1`. The cause was not in the new code. The
multi-block allocator never fed schema ties into its union-find at all: a
move-wide overwrite names one register field for its operand and its result, and
that was coming out right only because the operand dies at the overwrite, so its
register is free one position later, and `FREE-REG` hands out the LOWEST free
register - usually the one just released. Coalescing pinned the other end of the
chain to a block argument's register and the coincidence stopped.

Two things follow. The first is the fix: a tie is a must-share constraint of
exactly the same kind as an argument-carrying edge, so it belongs in the same
union-find, and then it holds by construction rather than by the order
`FREE-REG` happens to scan in. The second is where the bug had been hiding. The
multi-block path had no unit tests at the time - `test/compiler/native-regalloc.f`
was straight-line fixtures only - so the only thing checking it was an end-to-end
run that happened to pass, and the validator's own tie clause, which agreed with
the lucky answer because the lucky answer was right. A check cannot tell you
that a rule is unenforced when the accident keeps satisfying it. (That gap is
closed: the same file now carries multi-block fixtures, and the lesson below is
how one of them is built so the accident cannot save it.)

So: before adding a pass that PICKS registers, list the constraints the existing
allocator is supposed to enforce and find where each one is stated. Any that is
"the scan just does it" is a constraint your new pass is about to break.

## The validator for a preference pass checks the answer, not the walk

Coalescing merges classes in an order - candidates in module order, and a merge
can block a later one because it grows a class - so it is tempting to think the
validator has to re-derive the order to check the result. It does not, and it
should not. What a wrong merge does is put two values that are live at the same
instant into one register, and `OVERLAP-CK` already refuses exactly that, from
the module's own liveness and the assignment's own registers. That statement is
about the answer, so it holds whatever order produced it - and it would catch a
coalescer with no order at all, or a hand-written module. Re-deriving the merge
sequence would have been the thing being checked telling the checker what to
check. Both mutations confirmed it: dropping the interference test dies as
`E-A64RA-EDGE` in the allocator's own class invariant, and dropping that too
dies as `E-A64RAV-OVERLAP` in the validator.

The corollary is about the interference question itself. Asking it of the two
VALUES at the ends of the copy instead of the two CLASSES they belong to also
dies (`E-A64RA-EDGE`): the ends may be disjoint while a member the union-find
already put in one of their classes is not. A must-share structure changes what
"do these two interfere" means, and the question has to be asked at the grain
the structure works at.

## To test a must-share rule, build the shape where the lucky answer is the wrong one

A fixture for "the tied result lands in its operand's register" proves nothing
while the scan would put it there anyway. The multi-block tie held for years
because the operand dies at the overwrite, so its register is the lowest free
one where the result is written, and any straightforward fixture agrees with
both the rule and the accident.

What separates them is a shape in which a LOWER register is free at the tied
result's own position. Building it is three moves: put two values in the low
registers so the half-built constant lands above them, give them a last use at
an operation that stands between the constant and the overwrite, and let the
value that operation defines take only one of the two registers it frees. Now
the lowest free register at the overwrite is not the operand's, so an allocator
that leaves the tie to the scan produces a different register and the validator
refuses the routine. The same three moves make the coalescing fixture: a copy
whose ends would land in two registers unless they are deliberately merged.

The general form: write down the accident that has been satisfying the rule,
then build the input that breaks the accident while keeping the rule. If you
cannot describe the accident, the fixture is not yet measuring the rule.

## Suite registration schedules FILES, not cases

Registering a test file in a suite says nothing about whether a `*-CASE` word
inside that file is called from the file's own `RUN`. Deleting a case from `RUN`
leaves the suite green, and the case simply never runs. Adding cases to an
existing suite therefore still needs the eye: read `RUN` and count.

## An engine that bakes call targets makes publication an ordering fact

Making the native chain's output an ordinary word turned out not to need a new
engine mechanism: a word of this engine IS a dictionary record whose first cell
holds the address of its first instruction, and `patch32` - the same primitive
`undefine` uses to retire a record - writes that cell, flips the region
writable, restores execute permission and syncs the instruction cache on the
way. What it DOES need is an honest statement about when it works. The engine
resolves a call when the CALLER is compiled: either a direct branch to the
callee's address or, for a body under forty bytes with no position-dependent
instruction, a verbatim copy of the callee into the caller. Neither can be
revisited - there is no callee-keyed fixup table, and an inlined caller holds
private bytes no patch could reach. So republishing a record is not "every
caller now calls the new code"; it is "every caller compiled from now on does".
That is exactly the definition transaction's own shape, because a migration
belongs immediately after the definition, before anything has called it.

The same fact is what let the code generator comparison keep both columns alive
without a second corpus of hand-copied bodies: the old column's call sites are
compiled before the migration and the new column's after it, so each one bakes
the code generator it is measuring. Load order became load-bearing, which is
worth writing where the order is decided rather than only where the record is
rewritten.

## A caught throw must not unwind past a scoped resource

`IR-CTX:WITH-CONTEXT` gives its arenas back when its quotation returns and not
when a throw passes through it, and the shared arena registry holds sixty-four.
Catching a refused migration OUTSIDE the context therefore worked perfectly for
the first few refusals and then failed the whole suite with
`E-IR-ARENA-SLOTS` - a resource error several cases away from the case that
leaked. The same shape appeared one layer down: a recording unit whose scan
threw, or whose close threw, left the checker's reader armed, so the NEXT
migration was refused for the state its predecessor left rather than for
anything about itself. Both fixes are the same rule: catch INSIDE the scope,
carry the code out as data, and rethrow it above. A refusal path that is not
exercised repeatedly in one process looks correct for exactly as long as
nobody refuses twice.

## A quotation may not be opened inside another

`[: … [: … ;] … ;]` is not a nesting the engine compiles: it fails closed with
exit 75 at the inner opener. The failure surfaces at the file that is being
loaded LATER, because the source that contains the nesting loads fine and only
leaves the quotation-patch cell set - so the next file's first ordinary `[:`
is reported as the nested one. When a quotation opener is rejected for no
visible reason, look for an unbalanced or nested quotation in something already
loaded, not in the file being read. Factor the inner quotation into a named
word; nothing else changes.

## A spill in a routine that branches is a whole CLASS, not a value at a point

The straight-line allocator takes one value's register away at one operation and
leaves the value in its register everywhere before that. Carrying that decision
across blocks looked like an anchoring problem - give the plan row a block and
the store goes in the right place - and the anchoring is the smallest part of
it. A store has to have happened on EVERY path that can reach a load of that
slot, and "the operation where the register was taken" is a point on one path.
What makes it right without a dataflow pass is the structure the multi-block
half already has: a CLASS is what holds a register, so a class is what loses
one. Every member's own definition stores into the class's one slot and every
read of a member loads it back, and the class invariant - no two members are
live at the same instant - is exactly what makes one slot hold the right value
at every read, for the same reason it makes one register hold it.

The price is that a class of more than one value writes its slot more than once,
which the validator's "a slot is written once" refuses; so this only spills
classes of one until that rule is generalised. Writing the restriction down was
worth more than working around it: the rule it rests on is the rule that makes a
reload's value decidable from one module.

## The frame is one region and it needs one owner, or two passes agree by luck

The selector wrote the caller's return address into slot zero of a calling
routine's frame; the register allocator handed out spill slots from offset zero
upward. Both were right on their own and the collision could not happen only
because a third rule - a routine of more than one block could not spill at all -
kept them apart. That is not a design, it is a coincidence with a guard, and the
validator's "no slot is written twice" was catching it under a name that says
nothing about ownership.

The fix is one file that answers where things go from the CONTRACT's own trait
(`src/compiler/native/frame.f`), read by the selector, the allocator and the
validator. What made it load-bearing rather than tidy was giving the validator a
clause of its own - the prologue's access names the link slot and no other
access may, and every other access names a slot at or above the base - and then
mutating the allocator to start at zero: it now dies as E-A64RAV-OWNER, where
before it died as a slot-sharing error two layers away from the cause.

## A frame access may not land inside a data-stack run

A routine's entry sequence, its exit sequence and each call site are contiguous
runs of data-stack operations, and the validator measures each of them as a
shape - the take at this position, the loads at the next ones, in order. A spill
store anchored "right after the definition" lands inside the entry run whenever
the value it stores is an argument the routine just read, and the shape check
then fails several layers from the anchor that broke it (E-A64RAV-DSTACK about a
misplaced adjustment). The store therefore goes in front of the first operation
after the definition that does NOT touch the caller's stack, and a value a
data-stack operation READS is not spilled at all - which is also why a value
live across a call stays in a register: the call site already puts it on the
data stack.

## Compiling a definition that spills holds three modules in one context

The context mapping was sized for two modules of the machine dialect, because
the spill lowering reads one and builds another. A real compilation adds the
source module the elaborator filled, which is still live: the whole run is one
context. A recursive definition that spills does not fit 128K, and the symptom
is E-IR-CTX-SCRATCH in a fixture that has nothing to do with arenas. Raising the
mapping moves three pinned literals in test/compiler/ir-storage-schema.f, the
capacity pin in test/compiler/ir-context.f, and `map_bytes` plus the arena
ceiling finding in formal/Common/Storage.v - the pinned-capacity row is what
makes forgetting any of them fail.

## The two register-allocation walks number positions differently

The allocator and its validator each have a straight-line walk for a routine of
one block and a general walk for the rest, and the two number a routine's
positions differently: within the block (arguments at ENTRY, operation i at i)
against across the whole routine (block b's arguments at its start, operation i
one past it). The dispatch is therefore load-bearing in BOTH files and has to
ask the same question, or a routine is measured in one numbering and checked in
the other - which surfaces as E-A64RAV-INTERVAL in a case that has nothing to do
with intervals. Sending every routine down the general walk to simplify this
does not work for that reason, and the straight-line walk cannot be extended to
serve a routine with a frame because its frame rule and its data-stack rule both
want the block's first operation. So a calling routine of one block - which is
what `: A ( n -- n ) B 1+ ;` is, the commonest call site there is - is sent to
the general walk from both files, and unifying the numberings is its own dot.

## A refused compilation must give its pass bindings back

Each pass of the native chain takes an identity binding over the module it is
about to read, and two of them refuse a second binding over a live one. A
refusal anywhere between taking those bindings and spending them therefore
leaves them live, and the NEXT compilation fails as E-A64RA-BIND - for the state
the previous one left rather than for anything about itself. It stayed invisible
while the only reachable refusals happened before the bindings were taken; the
first refusal raised inside instruction selection exposed it. The migration
entry now releases whatever is still bound when a run fails, asking each pass
about itself rather than counting how far the run got: which bindings are live
depends on which stage refused, and any counter kept outside the passes would be
a second copy of state they already hold.

## A flag has no successors to swap, and that is what decides a condition vocabulary

The machine dialect carried three conditions and a note saying the complements
were "a vocabulary nothing produces": a branch on the falsity of a relation
names the relation and puts its two successors the other way round, so `not lt`
never had to be spellable. Adding `>`, `>=` and `<>` looked like the same
argument one more time - `a > b` IS `b < a`, so turn the operands round - until
the FLAG path was written down beside the branch path. A comparison that answers
a number has no successors, so `<>` cannot be reached from `=` at all, and once
one complement has to be a member the operand-swapping scheme costs more than it
saves: a lowering would then be a condition in one table and an operand order in
another, and two tables that have to agree are two tables that can disagree. One
condition per source relation left the fusion wiring untouched - the polarity
rule is about the flag's meaning and not about which relation it is, so three new
conditions fused for free - and left every emitted comparison byte-identical to
the engine's own primitive for that word, which is the thing the whole chain is
judged against.

## An engine primitive is the specification, and it is cheaper to run than to read

Four of this leaf's words had a semantics question that no amount of reading
settled: what `0=` does to a value that is neither zero nor a flag, what a shift
by 64 does, what `invert` does to -1, what `cells` does to a negative number.
Ten lines evaluated through `bin/hb` answered all four in one run - `5 0=` is 0
(so `0=` is an equality against zero and never a complement), `1 64 lshift` is 1
(so the shift takes its count modulo the register width, because the machine's
shift-by-register form does), and `-3 cells` is -24. Every one of those became a
row of the acceptance fixture, and three of them are rows a plausible wrong
lowering passes without: a complement-based `0=` agrees on 0 and on -1 and
differs only on 5.

## A benchmark corpus is the fastest bug-finder the compiler has

Widening the codegen comparison from eleven synthetic shapes to seven words
taken out of the running system found more in a morning than the acceptance
suites had: hexadecimal literals cannot cross the tape, a migrated body may name
only one constant, one loop needs ten registers where every earlier loop needed
eight, and - the real one - a chain-compiled routine that CALLS another
chain-compiled routine from inside a `?do` body answers the wrong number and
faults when the body stores. Every one of those is a shape a hand-written
fixture had no reason to reach, because a fixture is written by somebody who
already knows what the compiler does. Real code is not.

## Narrow a miscompile by varying ONE thing at a time, and the diagnosis falls out

`4 LC-N` answering 0 where 12 was right could have been anything. Five one-line
variants settled it in minutes: the same call in a `begin … until` BODY is
right, the same call in a `begin … while` TEST is right, the same `?do` body
with NO call is right, and the same `?do` body calling an ENGINE-compiled word
is right. What is left is one sentence - a chain callee destroys registers the
`?do` loop is carrying, and the call site saves the value vector but not the
loop's own state - and the variant that answered 36 instead of 24 says which
register: the trip count, because the loop ran six turns instead of four. Write
the variants before writing the dot; the dot is then a diagnosis rather than a
report.

## "It compiles if I respell it" is a fork in the road, and it needs a measurement

Two of the seven bodies were refused for their SPELLING - four named constants,
three hexadecimal literals - and the tempting move is to write the numbers out
and take the green row. That is only honest if the two spellings are the same
program, and the way to know is to compile both with the engine and compare the
records: 428 bytes either way for the whitespace test, 144 either way for the
case fold. The substitution then went into the file with the dot that removes
it, and the byte equality went into the suite, so the claim is a test rather
than a sentence. The row that could NOT be bought that way - a call in a counted
loop, which the interop would have made green by luck - stayed a gap.

## Survey the engine's float behaviour by RUNNING it, not by knowing IEEE754

Writing the float benchmark meant writing down what the engine does with
doubles, and the parts worth writing down were the parts no amount of standards
knowledge would have supplied. The engine's own source answered the easy half:
one unboxed cell per double, fifteen float words each built on one AArch64
instruction, four of them inlined by the compile-state dispatch and the rest
compiled as calls. Execution answered the half that decides a compiler's
correctness: every comparison is FALSE when either operand is NaN, so
`x f0< if A else B then` takes the else arm for a NaN and a lowering written as
"not (x >= 0)" is a different program; the NaN this engine produces is one
deterministic bit pattern, which is what makes a NaN row pinnable at all; and
`f>s` truncates, saturates and answers zero for a NaN, which is why the library
has a rounding word wrapped around it. Two probes went into the corpus header
because they are surprising rather than because they are hard: the float literal
reader computes int + frac/10^k with three roundings, so a seventeen-digit
literal can land one ulp off the nearest double, and past eighteen fractional
digits its integer accumulator wraps and the literal is silently read as a
NEGATIVE number. A benchmark that pins only short exactly-representable literals
sidesteps both; a compiler that materialises constants cannot.

## Record a float output as the CELL, and the harness gains two free tests

The codegen comparison stores a row's outputs as numbers, so a float row needs
one projection from `r` to `n` - and the honest one is the identity on the cell,
declared with `CAST:` in the one place both columns call, exactly as the flag
projection already was. Bit equality is finer than float equality in precisely
two places, and both turned into checks worth having: +0.0 and -0.0 are equal
numbers in different cells, so a pinned `-0.0` input catches a code generator
that dropped the sign of a zero, and two NaNs are equal cells while being unequal
numbers, so two rows produced by two different words can be asserted to carry the
same NaN. Neither test would exist if the harness had recorded a rounded decimal.

## Re-derive the engine's number reader, bug for bug, or the literal is a different program

The source tape has to record a float literal's VALUE, and the engine's own
parser is not reachable from the compiler - so the value is read back from the
spelling, exactly as the integer literal already was. The temptation is to use
the stdlib's `STR>FLOAT`, which is a better parser: it accumulates the
significand in a double and scales by a power of ten. The engine does something
else - two integer accumulators and a power of ten, finished with three SCVTFs,
one FDIV and one FADD - and the two routes do not agree on every spelling. A
compiled literal one bit from the interpreted literal is a different program, so
the reader reproduces the engine's route instruction for instruction, wrapping
accumulator and all. It reads `0.1234567890123456789` as a NEGATIVE cell because
the engine does, and a test compares it against the engine's own literal on ten
spellings rather than against a table of expected numbers. When the reader bug is
repaired the test goes red and points at the file that has to move with it, which
is what keeps two independently-correct parsers from being two different
compilers.

## A second register file is a second FILE, not a second flag

d0 and x0 are two registers and both are number zero, so every allocator table
keyed by a register number has to be keyed by the file and the number together -
the holder table, the pins, the reload list, the free scan, the eviction scan,
and the pressure count that decides whether to spill. Making the class a flag
beside a shared table looks smaller and is wrong in a way nothing catches: a
routine that ran out of general registers would be "relieved" by a free floating
one. The mutation that proves the tables are really split is to make the float
file hand out one register: two live doubles then land in d0 and the validator
refuses with its own overlap error, which it can only do because it re-derives
the class from the value's TYPE rather than believing the allocator.

## Ask the module for a value's type; a second record is the one that gets believed

The elaborator needed to know whether a compile-time value is a double. The
obvious move is a buffer beside the value vector holding one type per slot - and
it is a second record of something the module already holds, which means it can
disagree with the module and would be the one every reader here consults. Adding
one live reader to the builder instead (`IR-BUILD:VALUE-TYPE@`) made the module
the only authority, and the same discipline caught a real bug one stage down:
the selector's float predicate reads the SOURCE module's types, and handing it a
value of the NEW module threw an ownership error from a table that had every
right to refuse. Two modules, two numbering spaces, one question - ask it of the
module that minted the value.

## A form no program reaches is a promise, so take it out

The floating frame accesses and the floating register move were written,
encoded and wired through four files before it became clear that nothing in this
leaf can reach them: a routine's contract hands out the whole D file, so no
double can run short of registers, and a double may not cross a block edge, so
no double is ever copied. Half-tested machinery is worse than absent machinery -
it looks like coverage and it rots. They came out, `spill.f` refuses a double it
would have to put away by name, and the dot for the leaf that reaches them says
where they land. The dialect's own header had already written the rule down:
an opcode with no lowering and no test is a promise, not a schema.

## A condition code means whatever the instruction that set the flags meant

The float comparison leaf had one real decision in it and it was invisible from
the source language. AArch64's condition field is four bits, and `lt` after a
`Subs` is signed less-than; after an `Fcmp` it is not. `Fcmp` raises the
unordered condition for a NaN - N=0 Z=0 C=1 V=1 - under which `lt` (N != V)
HOLDS while `mi` (N = 1) does not, and the engine's own `f<` uses `mi` for
exactly that reason. A lowering table that read a float comparison's condition
off its relation's NAME would compile a `f<` that answers TRUE for a NaN and a
fused branch that takes the arm the interpreted word does not, and every
ordinary pair of numbers would still look right. The rule that falls out: a
condition belongs to the instruction that wrote the flags, not to the relation
the source word spells, and the only inputs that separate the two are the ones
that raise the unordered flag. Every float comparison test here asks a NaN in
each operand position, and the mutation that flips `mi` to `lt` is killed by
those cases and by nothing else.

## The NaN rule needed no code, which is how you know it was modelled right

`x f0< if A else B then` takes the else arm when x is a NaN, and nothing in the
compiler checks for one. The compare-and-branch is wired condition-true-first,
so a condition that is false sends control to the second successor, which is the
arm the source's `if` takes when its flag is zero - and the three conditions the
engine names (MI, GT, EQ) are exactly the three that are false on unordered.
Both halves were already there for integers; the leaf's whole job was to pick
conditions that keep the property. A guard, a check or a special case anywhere
in that path would have been a sign the model was wrong, not a safety net. Write
the truth table before the code and the code turns out to be a table entry.

## One kind table, three readers - not three tables over eleven opcodes

The float comparisons made the selector ask three questions about a source
comparison: is it fusable, which machine form does it become, and how many
operands does it read. Written as three lists of opcode names those are three
things that can drift, and the drift is silent - a comparison missing from the
fusion list is just slower, and one missing from the operand-count list stages
an operation with an operand nothing computed. One exhaustive table answering a
four-member kind, with the three readers matching over the kind, makes a new
opcode answer once and makes a wrong answer break all three at the same time.
The mutation that proves it: make the table say a float comparison is not a
comparison, and the FLAG path throws before the fusion path is even reached.

## A test that only ever compares unequal operands cannot see `gt` from `ge`

The first mutation run had every float comparison lowered under a wrong
condition and killed - except `f>` under `ge`, which survived. The two differ
only when the operands are EQUAL, and every pinned input was an ordered pair or
a NaN. Three orderings per relation is the minimum: less, greater and equal,
because the six conditions of this machine partition exactly on those three plus
unordered. A comparison suite that omits the equal case is testing five
conditions and reporting six.

## The class's hull crosses the call; no member of it does

Barring a register at a call site looked like one line: a value whose live range
spans the branch may not have a register the callee writes. Over a routine of
more than one block the allocator holds a register for a whole CLASS - the
values an edge joins - and the first version asked whether the class's hull
spanned the call. Every routine that already compiled broke. The reason is the
discipline being narrowed: a call site stores its live values and reads them
back, the store's value and the load's value are two values, and the edges
around the call join them into one class. The class spans every call in the
routine while no member of it does. Asking each MEMBER whether its own interval
spans the branch is the same rule at the granularity the rule is about, and it
answers "no" for exactly the values that are sitting in a data-stack slot when
the branch runs.

## A routine destroys what its callees destroy, and the corpus is what said so

The registers a published routine writes are the registers its accepted
allocation assigns - that argument is exact, and it is also only half. A routine
that CALLS another word destroys everything that word destroys too, and none of
that appears in its own claims. The half-answer passed every suite and crashed
the third corpus, because a suite that migrates one leaf at a time never builds
a caller of a caller. Two lessons: the transitive part of a fact like this is
where it will be wrong, and a benchmark corpus with real nesting in it is a
correctness test that no leaf suite replaces.

## Pin the traffic, not the wall clock

Two rows had their direction pinned - "the new code costs more than the old" -
because that was the finding at the time. When the change flipped them the honest
update was not to flip the assertion: the two columns are now within host noise
of each other, and an assertion either way is a gate that fails for load. What
replaced it is a count of the instructions the change actually removes - the
stores and loads against the caller's data-stack pointer, read off the emitted
word. It is exact, it moves for one reason, and it fails when somebody stops
narrowing instead of failing when somebody else's build is running.

## The size rule an inliner needs was already in the calling convention

The engine copies a callee of forty bytes or less into its caller, and forty is a
fact about that emitter's code, not about anything the chain does. Picking a
number for the chain would have been a value heuristic - the kind of thing a
review is supposed to send back - and the number was not needed. A call to a
routine of arity (in -> out) costs `in + out + 3` instructions on each side: the
site's stores, its branch and two pointer moves, its loads, and the routine's own
mirror of them. So a routine whose whole emission is within twice that has a body
no longer than the call site's own half, and copying it in cannot make the site
bigger. The rule states itself, it scales with arity without being told to, and
the test that pins it is two callees one instruction apart.

## Copy the tokens, not the instructions

The obvious way to inline is to copy the callee's machine code into the caller -
it is what the engine does and the bytes are right there. It cannot be done
honestly in a chain that validates its own allocation: instructions copied into a
module the dialect has no form for are instructions the register allocator cannot
say anything about, so the independent validator would have nothing to re-derive
them from. Copying the callee's SOURCE TOKENS instead puts the splice at the top
of the chain, where the arguments are already values on the compile-time vector
and everything downstream sees one ordinary module. Nothing after the elaborator
learned a new concept, and the whole change is three files.

## What a copy removes is not the branch

Counting the branch is counting the smallest part. A call site that stops being a
call stops publishing its arguments and reading its results back, the callee's own
entry and exit are not paid at all, and - because the definition now contains no
call - it reserves no frame, saves no return address, and stops carrying its loop
counters and its locals across every edge. TINY-CALLEE went from 1.20x slower
than the engine to about eight times faster, and only one instruction of that
margin is the Bl.

## A record of an optimisation is keyed by the address, not the name

The clobber record made the argument first: a call site branches to an ADDRESS,
and the code at an address is written once because the publication seam claims
every code slot exactly once. The recorded body follows it, and gets a check the
clobber record could not: the caller states what effect it believes a callee has,
the callee's own migration recorded what it really declares, and the two are held
against each other. A caller compiled against the wrong effect used to compile
silently and compute the wrong thing; it is now refused by name.

## A microbenchmark delta transfers to a program only where the program spends

The four committed corpora say the native chain emits code that is smaller and
faster, word by word. Timing three real workloads through those same words in one
process said something the corpora cannot: the system-level number is whatever
fraction of the program's time was inside the migrated code, and nothing more.
COUNT-CH is a loop, so a workload that calls it once per buffer spends nearly all
of its time inside it and came out 85 per cent faster - repeatably, to a tenth of
a per cent across runs. FOLD-C is a leaf called once per byte from a loop the old
emitter compiled, and its 0.45 ns of saved routine disappeared into the caller's
own 3.8 ns per byte. Same code generator, same two words, two orders of magnitude
between the two answers. Publish the workload, not the ratio.

## A two-arm timing needs a control AND a floor, and they catch different lies

The control - old code against old code, the two arms compiled either side of the
migration - is the obvious one, and it caught nothing. The floor row is old code
against old code reaching two different PUBLICATIONS of the same body, and it is
the one that mattered: for a workload whose inner loop calls a small word once per
byte, two byte-identical drivers calling two byte-identical copies of one 144-byte
routine differ by twenty to thirty-five per cent, reproducibly, decided by which
copy they reach. Without that row the scan workload's three per cent would have
been reported as a small win. With it, the honest answer is that the measurement
cannot see anything that small at that shape. A control row is not one row.

## The inliner decides what a migration can possibly reach, before any timing

The engine copies a callee's body into a caller when the body is at most forty
bytes and holds no pc-relative instruction, and it decides that while compiling
the CALLER. Both outcomes are permanent: a copy leaves no call site to redirect,
and a call site holds an absolute displacement to the address the callee had then.
So republishing a word reaches only callers compiled afterwards - and the engine,
checker included, is compiled into bin/hb and never recompiled. Read that off the
machine code before timing anything: TAG and PAY are 56 bytes and have zero call
sites in the entire live dictionary, which is why migrating them cannot move the
checker by one nanosecond, and why the compile-shaped workload's honest answer is
a measured zero rather than a failure.

## An inline record holds a routine's operations, not the whole routine

The native inliner records a small callee's body as a row of tokens and splices
that row into every later caller. But a routine is more than the operations it
was written from: it reads its arguments out of data-stack cells and it writes
its results back into them, and neither crossing is a token. The argument end was
reproduced; the result end was not, because the callee's own compilation did it
in EMIT-RETURN and nothing in the row remembered that. So a recorded callee that
left a double left a DOUBLE on the caller's vector where the same call left a
cell, and a caller that stored the result was refused with E-NELAB-TYPE while the
identical source calling an unrecorded callee compiled and ran. Acceptance
depended on whether the optimisation fired.

Two things generalise. First, when an optimisation replays a recorded unit, ask
what that unit's own compilation did OUTSIDE the recording - prologue, epilogue,
calling convention - because those are exactly the parts with no token to copy.
Second, an optimisation's regression test needs the unoptimised half as a
CONTROL in the same file: here, the same caller source against a callee padded
one step past the size rule. The bug is not that the two answers differ; it is
that one of them stops compiling, which only a side-by-side pair shows.

## A ceiling on an optional record must refuse the record, not the operation

`src/compiler/native/inline.f` holds at most 64 recorded bodies, and its ceiling
threw. Because the migration asked about it, the 65th small word did not lose its
row - it failed to compile at all, permanently, with every later small word after
it, while large words kept migrating. The file's own prose justified refusing to
RECORD a body and said nothing about refusing the compilation, and the two other
ceilings in the same file already fell back to emitting a call. The rule this
taught: when a table is an optimisation whose absence costs a caller nothing but
speed, a full table declines the ENTRY and the operation succeeds. What is not
allowed is the silent drop the old comment feared, so the decline is counted and
queryable (`NINL:DECLINED`) - that is what makes "the table filled up" a fact a
test can assert instead of a behaviour change nobody can see.

## Move every refusal to the side of the publication where it is still free

The same file's COMMIT could throw AFTER the word had been republished, leaving a
word running new code while the migration reported that it had failed. Every one
of its questions - is that an address, does it already have a row, is there room
- was answerable before the publication and unchanged by it, so they all moved
into a CLAIM step that runs first, and COMMIT now only asks whether the claim it
completes was made. The general shape: when a sequence has one irreversible step,
every decision belongs before it, and what is left after it should be the
protocol's own fail-closed guard and nothing that can depend on data.

## A suite that writes into a production table needs a mark and a fence

`test/compiler/native-inline.f` wrote rows into the real record and published real
words, so a second run in one process threw on the first duplicate - which meant
"the suite leaves nothing behind" could never be asserted. Giving the suite its
own table would have tested a copy. What worked is two marks over the real state:
`NINL:MARK`/`NINL:RELEASE` for the row table (append-only, so a mark is a prefix)
and a fence word plus `HIDE-DEFS-FROM` for the dictionary. Running the suite twice
in one process is then the assertion itself, and removing either half turns it red
- the row half with E-NINL-DUP, the dictionary half with a duplicate definition.

## Sweep the variable, and keep one arm that cannot have the effect

Two publications of one body measured 18 to 35 per cent apart, and four candidate
mechanisms fitted the pair equally well - line straddling, fetch window, page
crossing, call-site alignment. A pair cannot tell them apart because a pair moves
every address at once. What told them apart was a sweep with each variable pinned
in turn (`tools/codegen-align-sweep.f`), plus two arms per position so the
disagreement between arms at the SAME position is a measured floor rather than an
assumption, plus a control arm built to be immune: the same body, the same byte
count, the same name, the same driver - branches removed. The control was the
whole answer. Every front-end explanation predicts it behaves like the others; it
went flat, which refuted all of them at once and left the branches as the only
survivor. Build the arm that must show nothing if your hypothesis is wrong.

## A per-call figure without its prediction regime is not comparable

Two committed tools disagreed 3.8x about one routine, and neither was broken:
`tools/codegen-compare.f` feeds a branchy word the SAME byte every iteration, so
its branches never mispredict, while the workload feeds it pseudo-random data. A
routine's cost with predicted branches and its cost on real data are different
quantities, and a table that prints one as "ns/call" invites the other to be
subtracted from it. When a microbenchmark drives a body with control flow, the
input distribution is part of the measurement and belongs in the report.

## Probe a shared package's public tails against the global dictionary first

Factoring `tools/codegen-time.f` out of the two codegen harnesses, the new
package exported a word named `FRESH`. Every consumer reached it through
`using CODEGEN-TIME`, and every one of them silently ran the checker's own
global `FRESH ( -- n )` (`src/core/checker.f:1787`) instead: the accumulator was
never cleared, the measurements were nonsense, and there was no diagnostic, no
non-zero exit, nothing. The checker had certified the body against the used
public's effect while the engine executed the global - certification and
execution named different words - and the `E-USING-SHADOW-GLOBAL` rule that
`docs/forth.md` promises for exactly this collision never fired. Dot
`habu-reject-a-bare-1f43a9a6` carries the reproducer and the checker fix.

Until that lands, a new public tail is not safe because it reads well. Probe it
before writing any call site:

```forth
: HAS ( ptr u8 n -- )
   2dup XREF-FIND XREF-FOUND? if s" TAKEN " type else s" free  " type then
   type cr ;
s" FRESH" HAS
```

Of nine proposed tails only `FRESH` was taken; renaming it to `ACC-CLEAR` fixed
every call site at once. The failure mode is worth the thirty seconds: a shadowed
tail does not throw, it answers.

## The line ledger of a consolidation is not the duplication it removes

The same factoring deleted about 50 lines of restated timing code and prose from
the two harnesses and still came out net POSITIVE, because a new checked package
costs its own header, requires, `package`/`public`/`;package`, and a doc header
that states the discipline it now owns - roughly 30 lines before the first word.
Two files that each restate a rule in 20 lines are cheaper in LINES than one file
that states it once, and dearer in every other way: they drift, and these two had
drifted far enough to measure one leaf 3.8x apart. Judge a consolidation by
whether the rule now has one owner, and report the line count honestly rather
than shaving the header until the arithmetic flatters it.

## Measure what a hot word is worth before you migrate it for its worth

The compile-shaped workload row read as nothing for three runs, and the standing
explanation was that a republication cannot reach callers already compiled. It
cannot - that was true and worth fixing - but it was not why the row read as
nothing. Two measurements settled it in ten minutes, both built out of the very
seam being written. Migrating a fold that increments a `create`d cell and moving
the checker's four call sites onto it counted the calls: one batch enters
SYM-FOLD-C 24389 times. Timing 24389 calls of each column's fold gave 80
microseconds either way - 0.3 per cent of a 27 millisecond batch. So the row
could not have moved however good the new code was. Compiling the same source at
four dictionary sizes then found where the time really goes: 1.3 microseconds of
batch per dictionary record, which is a linear name scan and more than half the
batch, and it lives in engine text rather than in a dictionary record, so no
migration can reach it at all. The lesson is the order: a capability that lets
you reach a hot word is worth building on its own terms, but "this row will move
when we reach it" is a claim about a FRACTION, and the fraction is cheap to
measure first. The instrument was a by-product of the capability, which is the
happy case; the unhappy case is publishing the row and calling it a win.

## A one-sided drift is not removed by a bar built from magnitudes

The same row's null draws all came out negative, because a batch costs more the
longer the dictionary is and every draw compares a sequence with the sequence
after it. A bar taken as the largest magnitude of four such draws still lets the
real row inherit the drift: one run printed +1.6 per cent against a 5.1 per cent
bar and the next printed -2.5 per cent against a 1.9 per cent bar and called it a
REAL LOSS, with nothing whatever having slowed down. A bar answers noise. It does
not answer bias, and a null draw that always has the same sign is announcing bias.

## Two resolvers over two dictionaries will disagree, and nothing will say so

The checker and the engine both walked the same scope chain for a bare token —
open package, global, used publics — but over different dictionaries. The engine
walked its wordlists, which hold every word. The checker walked its own symbol
table, which holds only what it recorded, so every engine-prefix word without a
signature and every `0 set-check` definition was invisible to it. Where a `using`
import exported a tail that such a global also owned, the checker walked past the
global, bound the used public, and certified the reference against that word's
effect while the engine compiled a call to the global. Exit 0, no diagnostic,
wrong values — a package public named `FRESH` ran the checker's internal `FRESH`
and every measurement built on it was wrong. The rule meant to catch exactly this
(`E-USING-SHADOW-GLOBAL`, 7141) had been in the checker for months and fired only
when the global happened to carry a signature, which is the case that would have
been caught anyway by the effect mismatch. The lesson is about the shape, not the
bug: when two components must agree on a decision, do not give each its own copy
of the facts and a rule to compare answers afterwards. Give the decision ONE
authority and let the other component ask. Here the engine's `search-wl` — the
same scan and case fold its own lookup uses — answers "which scope claims this
tail?", and the checker's table answers only "what is this word's effect?", which
is a question the engine cannot answer. Two mirrors are a drift waiting to be
found by whoever picks the unlucky name.

## A partial mirror hides its gaps behind the cases you tested

The shadow rule's tests all used globals defined in the test file, so all of them
carried signatures, so all of them passed — and the entire class the rule existed
for, the checker-invisible global, went untested and unenforced. When a test
fixture is something you construct, ask which property of the real hazard you
just constructed away. The repaired tests assert their preconditions (the chosen
name IS in the engine's global wordlist AND is undefined to the checker) so that
if the fixture ever stops being hazardous the precondition fails instead of the
hazard case passing vacuously.

## An optimisation that speculates has to be held against the register pool

If-converting a selection computes both arms, and every value an arm computes is
live from where it is computed to the select that reads it - so a converted
region holds all of them at once, on top of everything the routine already has
live. The first version of the rule bounded the region by blocks and by
operations and left the pool out of it, and the eight-deep early-exit ladder in
`tools/codegen-compare-corpus4.f` stopped compiling: not slower code, a REFUSED
routine, because the allocator ran out of registers and the spiller had nothing
it was allowed to put away. An optimisation that can turn a compilable routine
into a compilation failure is not an optimisation, so the bound is now held
against the routine's own pool and the region stays branched when it does not
fit - refusing the conversion is always correct. The general shape: any transform
that lengthens live ranges owes the allocator a bound, and the bound has to be
against the actual pool rather than a number that looked small.

## A form that reads N registers at once needs a pool of N, whatever it spills

A spill frees a register by moving a value that is NOT wanted at that instant.
Every source of a `csel` is wanted at that instant, so a routine whose pool is
smaller than the form's read count cannot hold the instruction at all and no
allocation exists for it - which is a different and harder failure than pressure.
It is also a clean structural admission test, and the one place a machine form's
register arity belongs in a decision: `src/compiler/native/select.f` refuses the
conversion when the routine has fewer registers than the select reads, and the
three-register pool of the chain suite's spill case is what proved it necessary.

## An index answers a scan only for the keys that can hold one row

`search-wl` (habu1.f BSWL) scanned the whole record table and kept the LAST
match; the dictionary hash index returns the FIRST validated row on a chain.
Those are different rules, and asking "is the index correct?" is the wrong
question — the right one is "for which keys is at most one row live?", which is
a property of the KEY, not of the lookup. For every real wordlist the definer's
duplicate wall (habu2.f `C-REJECT-DUP-DEF`) makes it true, so first and last are
the same row. For one key it is false, and the key is not a wordlist at all:
`undefine` (xref.f `XREF-RETIRE`) stamps `DICT-WL:RETIRED` onto the wordlist
cell of a row that is ALREADY in the table, so the row stays on the chain of the
wid it was PUBLISHED under and this key's chain is empty — and retiring one name
twice puts two rows under the key, which an insert-once table has no slot shape
for. Both failures point the same way, so the probe is not consulted for that
wid and the scan keeps answering it. The general rule: when you replace a scan
with an index, enumerate the writers of the index's KEY. A key written once at
insertion is indexable; a key some later writer re-stamps onto a row that is
already filed is not, and the linear fallback for it is the correct answer
rather than a hedge.

## Pin a lookup by which ROW it returned, not by whether it returned one

The order-pinning fixture for the change above (`test/engine-suite.f`, package
`ES-SWL`) works because no case asserts "non-zero". Each one names the row it
expects by the index the row was published at — `ndict@` in the instant before
publication — and compares `search-wl`'s answer with that row's own first cell.
An answer of the WRONG row of a pair then fails, where "found something" would
have passed. The same battery is captured twice, once with the index live and
once at the end of the file after the `ndict!` block has dropped it, so the
probe and the scan must agree row for row over one dictionary. Mutation:
removing the retired-wid guard reddened four cases including the two-capture
comparison. One mutation SURVIVED and is worth knowing — deleting the probe's
name-length check changed no answer, because the hash already encodes the
length; that check only bites when a slot collision coincides with a prefix
relationship, which no fixture reaches. A surviving mutation is not always a
weak test; sometimes it names a guard whose reachability needs a collision to
construct.

## A correctness fix can be gated on a performance fix, and saying so is the design

The checker's bare-tail resolver had the same hole at the OPEN-PACKAGE leg that
was closed at the used-publics leg (`CHECKER-USED-BIND`): a package word the
checker never recorded — a `0 set-check` definition — was invisible to its
tables, so it fell through to the GLOBAL symbol of that spelling and certified
the body against the wrong word while the engine bound the package one. The
earlier repair stopped at the used-publics leg for a stated reason: that leg is
reached only after a used public has matched, so it pays one wordlist probe per
matched reference, while the package leg would probe on every token whose
package lookup missed — which is every reference to a global or a primitive from
package code. With `search-wl` scanning the record table that was 4.2-5.9 µs per
call and unaffordable; measured on `tools/dict-lookup-cost.f`, a 40-definition
batch went 5.5 ms → 12.8-14.3 ms and the dictionary-size slope came back. Once
the primitive answered from the index (10-21 ns per call, flat) the same leg cost
nothing: 5.3-5.4 ms, flat, indistinguishable from before the leg existed. When a
correct rule is declined on cost, write down the measurement and the cost that
would make it affordable — that turns "we did not do it" into a dependency
another dot can discharge, and the resolver leg lands the day the primitive does.

## A new global in an engine-trunk file has to join a package, even where its neighbours do not

`tools/package-diff-lint.f` admits changes to the BODY of a global that
`src/habu/{layout,habu1,habu2,xref}.f` already defines, and still reports a
genuinely NEW global there — the asymmetry is deliberate, because those files
already open real packages, so a new name has an owner it can join. Adding two
constants beside `DREC` and `OWNER-API-PRI-WID` in `layout.f` was reported
twice; putting the same two in `package DICT-WL` was clean and cost nothing,
because a brand-new name has no bare callers yet and so does not inherit the
`using` blocker that stops the rest of that file from being packaged (dot
habu-give-layout-f-315df2ca). Do not read the surrounding global surface as
permission: check whether the name is new before deciding where it lives.

## A fixture that pins "what the elaborator leaves" outlives its subject

`test/compiler/native-select.f` pinned the fused compare-and-branch on `MAX2` -
`2dup < if swap then drop` - described as the shape the elaborator leaves. The
if-conversion then ate exactly that shape, and the fused-branch cases either
threw or asserted about a module that no longer had a branch in it. The repair
was not to weaken them but to give the fixture a reason to keep its branch that
a reader can see: one arm now divides, a division may trap, so its arm cannot be
speculated and the branch survives. The lesson is that a fixture named after a
SOURCE shape is hostage to every pass that learns to rewrite that shape; a
fixture named after the PROPERTY it needs - here "an arm that may trap" - says
why it still tests what it says it tests.

## A refusal by TYPE can be hiding a refusal by POOL, and only one of them is real

`src/compiler/native/select.f` refused to if-convert a selection whose join
carried a double, and the reason written down was "the assembler has no Fcsel".
Once the encoder existed the refusal came off, and the region promptly stopped
converting anyway - because the routine contract the fixtures used declared no
floating register writable, so there was no pool to hold an Fcsel's sources in.
The type was never the constraint. The pool was, and it had been invisible
underneath the type because the type refused first. Two things follow. Every
count the transform holds against a pool has to be taken PER FILE, since d3 and
x3 are two registers and a sum across the two is held against a pool neither
half comes out of. And when a stated reason is removed, check what the next
refusal is before believing the shape now converts: the suite went green on the
old expectation with the new code, which is exactly what a hidden second
refusal looks like.

## The cell-only path was carrying a latent class bug the refusal kept unreachable

While the join could only carry cells, `REGION-BR` copied every value across the
region's one edge with a hard-coded "this is not a double", so the copy was
always the general move. That was correct for as long as no double could reach
it and wrong the instant one could - it would have moved eight bytes out of a
register the double is not in. Nothing failed while the refusal stood, and no
test could have caught it. A constant passed where a question belongs is a bug
waiting for the guard in front of it to be lifted; when lifting a guard, grep
the code the guard was protecting for constants that were only true because of
it.

## Four names for two axes: how a machine form family stops multiplying by accident

A conditional select of this dialect is a pair - which instruction wrote the
flags, and which register file the chosen value lives in - and the two axes are
independent, so the family has eight members and not two. Shipping the second
column (`a64.selzd`, `a64.cmpseld`) without a name for the other row would have
left the next worker inventing a spelling that collided with the shipped one -
the dot for the fused float select had already proposed `a64.fcmpfsel` for what
this lane needed to call a Cmp feeding an Fcsel. The repair was to write the
naming rule down beside the forms and reserve all four unused names: a leading
`f` says the values COMPARED are doubles, as it does in `a64.fflag`, and a
trailing `d` says the value CHOSEN is, as it does in `a64.fmovxd`. A family that
grows along axes needs its axes named before its second member ships, or the
third arrives with a name that has to be argued about.

## A spawned child inherits the launcher's fd 0, so the launcher decides the verdict

`test/pre-trust-defer.f` spawned its engine-under-test through
`PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE`, which passes `-1` for the child's stdin.
`src/habu/habu1.f` `SPAWN-DUP2-ACTION` skips the dup2 for a negative fd, so the
child kept the FIXTURE's fd 0 — and `posix_spawn` makes it a process-group
leader. Launched from a terminal, that child was a bare engine, found a tty on
fd 0, entered the REPL, and its terminal ioctl stopped it with SIGTTOU as a
background group: the boot never returned and the case died on the 20s timeout
instead of reporting an exit code. From a pipe the same tree gave a clean
verdict. A fixture that asserts a child's exit code must hand that child an
explicit stdin (the empty-pipe `*-STDIN-CAPTURE` variants) — the same promise
`test/gate-env-stdin-tty-test.f` already holds for `GE-RUN-ENV`. `test/gate-pool.f`
`GT-POOL-SPAWN` passes `-1` too, so every pooled job inherits the gate's fd 0;
only jobs that spawn a BARE engine are exposed, because `--load` takes the
file-list path and never reaches the tty REPL branch.

## An exit-code assertion silently rots when an earlier guard learns to fire

The same fixture asserted exit 73, the runtime `SEAL-CAPTURE` backstop, for a
tree with its pre-trust drain blanked. Once the prefix gained a CHECKED `is` on
a pre-trust defer (`src/habu/xref.f` `INSTALL`, `[: LIVE ;] is PKG-LIVE-XT`),
the checker refused that definition first and the boot died at 70 — the property
still held, by a different and earlier guard, and the fixture read as an
environment flake for a day. Two lessons: assert each guard in its own case
(here, one case blanks the drain and expects the checker's 70, another also
blanks the hook install so the runtime backstop is reachable and expects 73,
plus a control proving the blanked hook alone still boots 0); and never let a
child-rc assertion print only a number. `lib/test/spawn-report.f` `CHILD` prints
the child's own stdout/stderr and the launch context on a mismatch — the answer
that took a day was one line of the child's stderr the fixture was discarding.

## An edge value can be the only witness a table has

A condition table where the wrong entry agrees with the right one on every
ordinary input is the dangerous shape: after an Fcmp, `lt` and `mi` differ only
on the unordered flag, so a suite that never hands the compiler a NaN certifies
the swapped condition. When a table's rows can only be distinguished by an edge
value, the test that pins the edge value is the whole test - write it first, and
write down which naive row it excludes (`test/compiler/native-select.f` asserts
both lt-shaped rows come out `mi`; nothing else in the structural suite separates
them).

## Delete a filter a capability made vacuous, don't widen it

When every comparison kind gained a select form, "which comparison may a select
fuse with" became the same question as "which may a branch fuse with", already
answered by FUSE-INDEX. Widening SEL-FUSE-OF to admit every kind would have made
it a word whose body says yes - a second copy of the real rule, free to drift.
It was deleted and its call sites ask the one authority.

## Derive per-file floors from the table that already knows the shape

A select's register floor comes from KIND-OPERANDS (how many registers the
comparison kind reads) plus the kind itself (which file they live in). The
deleted CMPSEL-CMP-REGS was a 2 that happened to agree with KIND-OPERANDS gpr;
restating a floor as a constant per form is how a third form gets a 2 that does
not. A floor and an operand list built from one table cannot disagree.

## Pick a test contract's register count where only the questioned rule refuses

A floor case proves nothing unless the OTHER admission tests already pass at the
chosen count: the fused-float floor pair uses a contract with exactly 3 D
registers (branch kept) against 4 (converted), 3 being the count where the
pressure test passes and only the floor refuses - so the pair reads the floor
alone, from both sides.

## A structural counter earns its place by moving for the change

FCSEL-COUNT and BCOND-COUNT were both unchanged by the float fusion; its witness
is CSET-COUNT going 1 to 0 - the flag materialisation the fusion removes. Pin
the counter that moves, or the assertion certifies the wrong thing.

## A case chain read inside a per-item loop is a quadratic waiting for growth

A Forth `case` chain is a linear scan, so using one as a TABLE READ inside a
per-token loop makes cost quadratic in the table size - and the table grows with
the project (refine-lint asked "is this token seed k?" per (token, seed) pair,
re-deriving both operands each time: 16.5s where its peers cost 2s). Cache the
table once at load, order it, and ask one search per item. The same shape was
found in tools/lint/def.f the same day (dot habu-idx-def-f-220d64e8).

## Explain the outlier before deriving the budget

A flake budget derived around an unexplained cost enshrines the defect. Here
~14.5s of the 16.5s entry was avoidable; removing it gave the original budget 5x
headroom before any budget arithmetic. Measure the lex-only floor first - it
says what "peer class" numerically is (1.9s of the remaining 3.5s is reading and
lexing 1419 files).

## A fixture that resets to clear a deliberate failure erases its own assertions

TIMEOUT-VERDICT drove the real verdict word with an unmeetable budget, then
T-RESET to clear the deliberate failure - which also erased the case's own
assertions, so two mutations printed a mismatch yet the suite exited 0.
Snapshot, reset, then judge - and prove the fixture with a mutation, because
this failure mode is a green suite that prints a mismatch.

## Verify carried-over claims by experiment before ruling on them (2026-08-01)

During the process-train abandonment decision, I grounded a ruling on "master
is unbootstrappable" — a claim carried through context compaction that
actually described the provisional composition, not master. Codex ran the
bootstrap and the full suite on clean master and disproved it in minutes.
What worked: adversarial verification of the other orchestrator's premises
before accepting a program. What didn't: relaying remembered state as a
ruling ground without re-deriving it. Rule: a factual premise that decides a
design ruling gets an experiment, not a recollection — bootstrappability,
greenness, and consumer counts are all cheap to measure.

## Run the ownership census before dispatching a caller wave (2026-08-01)

Seven launched runner-caller lanes (of eight planned; the run-lib lane never
launched — the thread cap was reached) all hit the same E-PACKAGE-OWNERSHIP
wall because the write sets included legacy-global gate files. The
pre-dispatch rule ("prove every planned definition has a package owner; a
legacy global is not an implicit exception") existed and was skipped. One rg
census over the write sets would have surfaced the cascade before the
workers started.

## Publish derived state only after every asset authenticates (2026-08-04)

A model-owned tokenizer can keep all mutable tables, token ids, and generation
scratch in one checked cell mapping while reading pinned source files through
short-lived byte mappings. Build the complete private block, authenticate every
pinned input, and set readiness last; then any failed allocation, read, digest,
parse, or model open has one unpublished owner to release.

## A benchmark reached through FFI needs one entry floor per call shape (2026-08-05)

Adding a `clang -O2` reference column to the codegen comparison meant calling C
through `lib/ffi-abi.f`, and the first honest-looking design measured one
zero-argument floor for the whole column. Measured: an empty foreign call costs
about 7 ns, a three-argument one about 22 ns, and the C body being measured
costs about 2 ns. A single floor would have left every row carrying the
marshalling of its own arity — `WIDE-ARITY` would have read as forty nanoseconds
of C code. The fix is one empty C function per signature and a floor measured by
running the row's *own* timing body against it: identical stores into identical
registers, differing only in what the callee does. Subtract that and what is
left is the emitted code.

The same measurement said something about the marshalling itself: `FFI:VALUE!`
bounds-checks a slot and clears a writable extent per argument, which is about
eleven nanoseconds an argument. The floor subtracts it either way, but a floor
four times the signal turns every small row into noise, so a benchmark's FFI
boundary is worth writing as the store and nothing else.

## An exact ranking must not be tie-broken by a measured one (2026-08-05)

The cross-corpus table of largest chain-vs-clang gaps ranks twice, by bytes and
by time. Bytes are exact and the table should be identical on every run; it was
not — one run in ten came out in a different order — because ties in the byte
gap were broken by the *time* gap. One noisy number in the tie-break made a
column of exact numbers wobble with the host. Ties in an exact ranking fall
through to a fixed order (measurement order) and nothing else.

The time ranking cannot be made exact, and pretending otherwise would be worse
than saying so: it is taken at the resolution it is printed at (whole
nanoseconds), ties there are broken by the exact byte gap, and the table itself
carries a line saying rows within a nanosecond of each other are not being
claimed to differ. Ten consecutive runs keep the top of that table and shuffle
the cluster below it, which is the honest result and is written down in
`docs/codegen-parity.md`.

## Tell "the tool is missing" from "the tool refused" by the exit code (2026-08-05)

The reference column has to be absent on a host with no C compiler, and it has
to be loud when the compiler is there and the reference does not build. Probing
with `--version` is a heuristic and it lied immediately: Apple's `size` has no
`--version` and refuses one, which reads as absence. What is structural is
`/usr/bin/env`'s exit code — POSIX reserves 127 for "command not found" — so 127
means the column is absent with the tool named, and any other nonzero code means
a tool that was there and refused, which throws with its own diagnosis printed.
The whole pipeline (compile, link, nm, size) is the probe, so what is asked is
the question that matters: does the toolchain do the job.

## A lint that says "missing X" must say missing from what (2026-08-05)

The stdlib gate went red with `result-cache closure:
tools/checked-boundary-lint-core.f -> missing tools/hook-sites.f`, the file
plainly existed on disk, and the finding was filed across a merge as a scanner
bug. The scanner was right: "missing" meant missing from the declared phase
file set in test/run-files.f, which had gone stale when 04a7ee9c added the
require edge without keying the file. Two changes close the class, and neither
is the fix itself: the diagnostic now names the set it means (`...which the
phase file set in test/run-files.f does not list`), and the scanner - which had
no fixtures at all - now has sixteen, each pinned by a mutation that turns
exactly its own case red. A lint with no hostile fixtures cannot be trusted the
first time it fires, because the only way to answer "is the tool wrong?" is to
read the tool; give a lint its fixtures the day it is written. And when a task
statement asserts the tool is broken, reproduce and re-derive before accepting
it - here the premise arrived exactly inverted, survived one merge, and cost a
second investigation.

## fork without exec cannot dlopen on macOS, and only the wrapper knows (2026-08-05)

Two codegen-compare members exited 134 under the gate pool and passed
standalone. Not concurrency: reducing to ONE forked member with no siblings
reproduced it at the same pc, inside /usr/lib/dyld. dyld is not fork-safe -
asking it to map an image that is not already mapped faults inside the loader
in a forked child, a signal with no error to catch - so anything a forked
child will call into must be mapped by the process it was forked from. The fix
maps the clang reference in the exec'd gate root before any fork and turns the
abort into a named refusal. Two portability facts worth keeping: a pid
captured at load time cannot detect the fork when the child loads the file
itself, so the fork wrapper is the only honest place to record "entered by
fork"; and "passes standalone, dies in the pool" does not imply a race -
reduce to one member before theorising about siblings.

## A substring assertion protects exactly nothing (2026-08-05)

The bootstrap mirror suite held 259 source-text assertions and the fixpoint
builder an exact-substring window audit. Every one failed both directions: a
comment decoy satisfies the assertion, and a stale comment keeps it green
after the real code is gone. What replaced them is nothing, deliberately -
the fixpoint rebuild compiles what the text described, the certify pre-pass
checks the exact assembled bytes through every window, and the recovery
fixtures execute the paths the text quoted. The few invariants no gate
observes (a mirror constant exercised only during no-binary recovery) kept a
check - but as token-kind lexing with hostile comment/string fixtures, never
text search. When deleting a check, name what still observes its invariant;
where the answer is nothing and the invariant is real, that is a new dot, not
a silent deletion.

## A rollback that runs after the counter moved is worse than none (2026-08-05)

A rejected SUMTYPE left its family chained in the tail index and the next
lookup of that tail died 76 - yet every watermark assertion passed, because
every watermark was right. The declaration layer rewound the counters itself;
the outer restore's retirement then ran against already-rewound counters,
popped nothing, and stamped the index current - erasing the one rewind signal
the index's ENSURE path heals itself with. A no-op retire is an assertion of
cleanliness. The repair removes the second writer rather than guarding the
first: one registry word retires the indexes and then moves every counter, it
is the only thing in the tree that writes them, and retirement arriving after
the fact refuses by name. When composing rollback participants, the ordering
must be a property of the owner, never an agreement between callers - two
individually-correct participants composed into this bug.

## Two load contexts that never coexist need an executed probe (2026-08-05)

The engine's data-stack register is stated in mnem.f (build-side emitter
vocabulary) and in layout.f (re-read by the booted engine at every load).
They cannot share a constant - a derivation through XDS compiles during the
build and dies E-UNDEFINED at runtime, which was tried and hit twice. The
agreement check belongs as an EXECUTED probe in the first file where both
names coexist (rt.f, loaded after both in both build chains), and it is not
believed until mutation-proven in both directions - here by mutating the
layout constant and watching a real hb-build die with the probe's own
message. A text pin would have satisfied a comment; the probe kills builds.

## A seeded engine is not a fixpoint engine (2026-08-05)

A fresh workspace seeded with the main worktree's bin/hb was not at the
compiler fixpoint - install --force changed the binary with zero source
edits, then was stable. Any lane whose gates depend on engine bytes (size
ratchets, published-code decoding, seal probes) is measuring a different
engine than the tree describes until it refreshes. Refresh the seed before
gating, and refresh the main worktree's engine immediately after merging
any engine-text change - the stale-seed false-red class has now bitten
from both directions.

## An error code is not a location, and CSE's prize is its cost (2026-08-05)

Two lessons from one lane. A predecessor pinned E-IR-FUN-BOUND to a specific
bound check and designed a fix for it; the code has twenty-plus throw sites
in one file, a one-line probe on the suspected line never fired, and the real
raiser was the test reading operations by absolute index - a legitimate
optimization changed the count and the fixture called it an invariant
violation. Probe the suspected line before designing against it. Second: the
literal CSE measured 4 corpus rows improved and 0 regressed, and still did
not land - collapsing repeated constants EXTENDS the constant's live range,
and in a starved frame that turned one spill into two, minus one movz plus a
store, a load, and a slot. CSE's benefit is live-range extension and its cost
is the same thing; without rematerialization (re-emitting the movz instead of
a stack round trip) the transform trades a win on wide frames for a loss on
narrow ones. The pin that caught it stays exactly as written - widening a
test to admit the code is how a measured regression becomes invisible.

## A proof carries a stage with it (2026-08-05)

hir.const is operand-free, pure, and non-trapping - in the HIR schema, where
that was proved. Two lowerings later, where the register allocator runs, the
same value is a movz/movk chain up to four instructions long, each movk
READING the previous half, tied into one register class. A design quoted the
HIR proof forward into the allocator stage and prescribed re-emission as
always cheaper than a frame round trip; the corpus already carried the
counterexample (BIG-CONSTS, four-chain literals at every use site).
Re-derive an invariant at the stage that relies on it, never at the stage
where it was convenient to prove - and when a cost claim says "always",
look for the corpus row that says otherwise before believing it.

## Record a resource where it is taken, and test outside the scope (2026-08-05)

Two halves of one repair. A caller-side record of which arenas a builder had
acquired recovered 12 of 13 leaked slots - the constructors each took two or
three slots and handed them back together, so the caller could only see what
came back. Moved into the allocator itself (a scope open across construction,
released newest-first), the record recovers all 13: multi-resource
construction must be recorded where the resource is allocated, because only
the allocator sees what a constructor took before it threw. And the leak
lived undetected because the context suite wrapped every case in an outer
context WHOSE EXIT CLEANED UP THE LEAK - a test that wraps everything in the
scope under test cannot see the failure path a real driver takes. The
regression cases now run at top level, where the driver lives.

## A measured floor decays with the tree (2026-08-05)

The literal CSE's acceptance floor was measured as four improved rows; by
landing day it was eight - master had moved and the stale floor would have
understated the win and hidden that CELL-BUMP's -24 bytes was the
second-biggest gain. Re-derive a measured floor on the tree it will gate,
every time; a number carried across masters is a claim about a tree that no
longer exists. Same discipline as re-pinning: measurements are dated, and
the date is part of the number.

## Brick the fallback to prove the fast path, and never pin a downgrade (2026-08-05)

Two keepers from the lookup lane. Proving an indexed fast path is COMPLETE -
not merely present - cost one hostile falsification: replace the linear
fallbacks with BRK and re-run the real suites; everything passing means every
lookup went through the probe. Cheap, decisive, reusable. And the old churn
test asserted the defect: 'a raise drops the index' pinned the silent
downgrade as expected behavior, which is why the defect survived review for
its whole life. A test that pins a silent downgrade is a bug preservation
order - when the downgrade is the defect, the pin must flip in the same
change that fixes it.

## A zero-headroom wall-clock ratchet is structurally flaky (2026-08-06)

The json scan rows asserted cross-tree wall-clock equality at 0.1%
resolution against a recording-day median; measured run-to-run drift on a
quiet host is 1.4-3.5%, so the row failed whenever drift landed positive -
an estimator change alone passed one run and missed the next by 0.105%. A
timing tolerance must be sized by the measurement's own noise, recorded with
its basis, and judged on the one order statistic host load cannot inflate
(the fastest of interleaved windows). The cure DELETED more than it added:
the verdict engine and the median machinery went, raw paired intervals
replaced them, and the data now speaks for itself. Ops footnote that cost a
peer lane one spurious failure: never pkill by pattern on a shared box -
match your own gate's exact PID lineage; peer lanes run identically-named
binaries.

## A pin must own the state that selects its outcome (2026-08-06)

A guard with two legitimate outcomes - inherited-mapping success and
unmapped-child refusal - was pinned by a test asserting the refusal
unconditionally, inside a gate member whose sibling files had already mapped
the library. In every configuration the gate actually runs, the pin did zero
mutation-killing work and manufactured a red; on hosts without the toolchain
it silently passed. The 'intermittent' red was toolchain availability, and
identifying the mechanism first turned ten-run flake batteries into run-1
determinism. The cure: a case that STAGES its own precondition - exec a
fresh process, assert the state, then demand the refusal - never one that
assumes the member's process history. Green is not evidence when the
configuration selects the other branch.

## An optimisation must never turn a compilable program into a failure (2026-08-06)

The first tail-call version REFUSED `: X ( n -- n ) abs ;` because the tail
branch to engine text could not be relocated. Wrong shape: publishability is
decided in the publisher, the tail decision in the elaborator, and the
earlier pass must ask the later one BEFORE committing - the fix declines the
optimisation (keeps call+ret) instead of declining the program, with the
fail-closed refusal kept as backstop. Related record-keeping trap: a routine
length defined as 'excluding the trailing return' subtracts a return the
tail form does not have - three readers of routine extent went wrong at
once. When a new form breaks a convention's arithmetic, grep every reader
of that convention before trusting green.

## Check binding lifecycles before designing a multi-pass fix (2026-08-06)

The spill rewrite wanted allocate-measure-reallocate, but A64SEL:SELECT
freezes the a64 builder, so a selected module is allocatable exactly once -
measure-while-allocating was the only shape, and knowing that before
designing would have saved a discarded plan. Two corollaries from the same
landing: when a reader's MEANING is wrong, redefining the reader can be
smaller than changing every consumer (A64SPILL needed no edit - it already
asked the right question and had been getting the wrong answer); and a
refusal that changes meaning silently loses its test - repointing
E-A64RA-PRESSURE left the old case proving the opposite thing, a gap that
only surfaced because the case was rewritten by hand. Rename or re-test a
refusal whose bound moves.

## install replaces the binary; a fixpoint alone does not (2026-08-06)

build-fixpoint-refresh -- all --force reaches the fixpoint but does NOT
replace bin/hb - only install does. A working engine fix read as 'still
failing' for a full cycle because the running binary predated it. Check
bin/hb's mtime before believing a negative result on any src/habu change.
And from the same lane: when a seal fires under a new exit, diagnose it
before building around it - this one was RIGHT to fire and one slot too
tight, and the widening's invariant (the pending row is the one slot ':'
writes before raising the count) became the design's atomicity story.

## Read the assert number before charging a red to an incident (2026-08-06)

An open host-contention incident became a place to file reds. A lane with
one red phase charged it to the incident and merged - the red was its own
unbumped content ratchet. The incident lane certified the sibling 'refuted'
over an attribution window that stopped one commit short of the breakage. A
content ratchet cannot flake on timing and a nanosecond budget cannot flake
on content: the assertion number distinguishes them in one line, and a
'green' verdict must state the window it covers. Two more from the same
pair of lanes: a calibration probe is valid only for workloads bound by the
resource it measures (a register spin saw 8% while fork/exec work ran 73%
slow); and measuring finished code undercounts a pre-allocation
optimisation - register reuse hides fusions, so a post-allocation inventory
is a floor, not a prediction.

## A stronger comment than its code is a defect report (2026-08-07)

Two headers in the register allocator described the file rule - a register
is a file and a number, interference is a same-file question - while the
code keyed on value class, correct only by the accident that class and file
were in bijection. The comment was the specification; the code was the bug.
When prose states a stronger invariant than the code implements, believe the
prose and fix the code (or fix the prose and say why the weaker rule is
right, as the coalescing case here deliberately did). And mutate with a
PLAUSIBLE WRONG MODEL, not an arbitrary edit: giving the allocator the
belief that V is a third register file produced exactly the silent
wrong-code failure the future risked, which an arbitrary line flip would
not have shown.

## A certified effect cannot tell a data word from a colon word (2026-08-07)

The plan was to fold resolved variables and constants to literals by
executing them at compile time. Falsified before building: a colon word
`: FORGE-PTR ( -- ptr a ) VX ;` is byte-identical in certified effect to
`variable VX` - so the fold would have executed arbitrary user code during
compilation on the strength of an effect signature. Every resolved name
became a CALL instead, which is correct for all three shapes. The general
rule: an effect signature names what a word LEAVES, never what it IS - any
compile-time action keyed on effect alone will eventually run the wrong
thing. Two siblings from the same lane: a model a pass reads must be
complete before the pass starts (lazy growth makes pre-scans and the walk
see different programs), and a cannot-compile fixture must be refused for
the SAME NAMED REASON, not merely refused, or it stays green when its
capability gap closes.

## Read the twin before claiming the row (2026-08-07)

A 'chain beats clang' headline rested on rows where the C twin's callee is
static and clang INLINED it - the 40-byte column was a deliberate inlined
body, not a failed tail call, so the 4-vs-40 comparison measured a policy
difference (copy vs call), not codegen quality. The harness's own header
states that clang is allowed to inline and that the difference IS the
measurement; the scorekeeping error was reading the byte column past that
sentence. Before claiming a row against a reference compiler, read what the
reference actually emitted and why - a byte win bought by keeping a call
the reference removed may be a time loss, and the time column adjudicates
policy differences.

## Brief from the file, and publish the dot before the lane (2026-08-07)

A lane was dispatched to build a per-call-site inline cost model that the
tree already had - inline.f SMALL? decides from measured post-selection
size against the convention-derived overhead, landed earlier and recorded
in this very file, and the briefing orchestrator wrote the premise from
memory instead of re-reading the owning file. The same dispatch named a dot
that existed only in an unpublished working-copy commit, so the worker
found nothing to claim. Two rules, both old, both violated at once: a
brief's premises come from the tree as it stands (re-read the owning file
the same day), and dot metadata publishes BEFORE any lane depends on it.
The lane's refusal to build was correct and cheap; the wasted dispatch was
the fee. Bonus finding from the same refusal: TINY-CALLEE's gap is clang
CLOSED-FORMING the loop, not inlining - the gap table without mechanism
attribution mis-aimed the whole plan's first item.


## Correlate the flake with one bit, then keep the evidence (2026-08-08)

A gate red that "rotates its victim" inside one parallel group looked like
timing flake. Splitting runs on a single environment bit - HB_TMP set
against unset - came back 3/3 green against 3/3 red, which is not flake,
it is a switch. The switch named the mechanism: the default-TMPDIR arm of
TR-START registers the gate root in lib/fs-mutate.f's cleanup table,
forked members inherit that table, and the first in-gate tests that call
CLEANUP-RUN (the newly scheduled object-* members) executed the DRIVER's
registration and deleted the capture root under their siblings. Two
practices made the diagnosis cheap: HB_TMP keeps the capture root (the
default root cleans itself up even on failure, destroying the evidence),
and a phase can be run standalone by reproducing only its driver preamble
- TEST:PREPARE plus GT-POOL-SLOTS!/GT-POOL-RESET - instead of the whole
gate. Fix at the fork seam (children own no parent registrations), never
per-test.

## One key for the transform and its admission count (2026-08-08)

The if-conversion draft added a region literal memo (a transform that
makes two arms' constants one value) while its pressure count still
assumed arms take turns - so the count and the emission disagreed about
which values exist. The repair was not a bigger bound but the memo's own
key asked twice: the count predicts the fold with exactly the key the
emission folds by, so the two halves of one pass cannot diverge. When a
pass gains a transform, every count it is admitted under must be derived
through that transform, not beside it.

## A two-file form cannot reuse a one-type schema definer (2026-08-08)

The D-file addressed accesses span both register files - an X base, a D
transfer - and the inherited draft declared them through a definer written
with ONE type, which silently said the ADDRESS is floating. The failure
only surfaces when an operand reaches the allocator. The schema test must
assert the refusing half too ("operand 0 is NOT the floating type"): a row
that only asserts the accepting half passes when both types are one
identity. And adding any instruction form silently breaks every
measurement tool that classifies emitted words by encoding - grep for the
encoder name AND the raw base constants, because the tools are written in
both styles; one of the blind counters here was pinning a number that was
already wrong (the engine always stored doubles with str d9, and SGD's
data-stack store count was pinned at zero).

## Two lanes, one test file, one silent duplicate (2026-08-08)

Two concurrent lanes each added an identical helper to the same test file
in different places; jj's textual merge kept both without a conflict, and
only the duplicate-definition guard turned it red - in the full gate, not
in either lane's own green run. Overlapping-file lanes reconcile at
integration by GATE, not by conflict-freeness: a clean automerge of the
same name is still a red tree.

## Process-global registries and fork (2026-08-08)

A child inherits the parent's cleanup table but not the ownership it
records, so any "clean up everything I registered" sweep in a child
destroys the parent's files. The repair belongs in the child arm of the
SINGLE fork call site - never per-caller guards - and the primitive-
confinement lint is what makes "single call site" a fact instead of a
hope. The bug hid for so long because GT-START happens to call
CLEANUP-RESET, accidentally shielding every child that starts its own
capture root: when a bug is masked by an unrelated reset, look for the
seam that should have done that reset on purpose. Corollary for tests:
assertions inside a fork child are worthless (a failed assert still
exits 0) - the parent owns every verdict, and the evidence must live
where the bug under test cannot erase it.

## A benchmark that compares answers cannot measure an address (2026-08-09)

codegen-compare judges three columns by the CELLS a word leaves; a string
literal's address is legitimately different in every column (inline code,
intern arena, hand-written reference), so a corpus row for it can never
agree and a row returning only the length measures nothing. Check the
comparison contract before promising corpus rows; pin the byte cost
through the publication lengths instead when the answer is an address.

## jj squash blocks invisibly on the editor (2026-08-09)

`jj squash` with two described commits opens $EDITOR to combine messages;
piped and backgrounded, that looks like a lock hang at 0% CPU. Use
`--use-destination-message` (or -m) in any non-interactive flow. Check
`ps` for the editor before diagnosing repo locks — and kill the stale
`tail` such a kill leaves behind.

## An empty list is not a statement (2026-08-09)

A64EFF's place list used one value, SEQ-NONE, to mean both "passes
nothing" and "placement unconstrained" - the type's own header said so -
and four passes each guessed which meaning applied, all the same way,
all wrong for a ( -- ) routine: 327 definitions refused. When two facts
share one representation the fix is a FIELD, not a cleverer predicate
(the obvious inference was falsified by the owning pass's own suite),
and the field belongs where the value is BUILT, so no consumer can ever
hold a contradiction. The tell was in the header all along.

## Probe the bucket's entries, and check what the successes cost (2026-08-10)

A refusal histogram keyed on the FIRST refusal and the token AS WRITTEN
hid two different things: 147 of 325 "dialect gaps" were one case-fold
issue wearing four spellings, and capitalised rename words did not
refuse at all - they compiled into real calls where lower case costs
zero, counted as successes. Before sizing a capability off a census
bucket, open the bucket's own entries - and audit what the successes
compiled TO, because a wrong success is worse than a refusal. Corollary
that killed a phantom cut blocker: before designing new IR kinds for a
construct, hand-elaborate its semantics into the shapes the chain
already compiles; the "missing aggregate" was flat cells all along.

## Ask the running engine which lookups fold (2026-08-10)

The engine folds keywords and dictionary names but NOT locals - three
lookups, two rules - and a four-line probe against the running engine
settled in a minute what reading the assembler would have left arguable.
Two census corollaries from the same lane: a spelling can leave the
refusal histogram without the gap closing (the definition now refuses
EARLIER under another code - diff the whole bucket table, not the
spelling list); and "both compile" is not parity - only reading both
frozen modules back, op by op, catches a body that compiled into worse
code than its twin.

## A premise is only as good as its five-minute probe (2026-08-10)

The rename-miscompile leaf said two published counts (terms vs cells)
already distinguish an ADT bundle from loose values. They are equal in
exactly the case that miscompiles - a user signature flattens the bundle
- and the design built on that sentence had to be killed on measurement.
The two-line probe comparing a bundled signature against a two-variable
one with identical counts would have cost five minutes at design time.
Probe the leaf's central premise before building on it, even (especially)
when you wrote the leaf. Second find from the same lane: a ruling's
condition can be necessary but not sufficient - "boundary falls inside a
run" missed swap over two adjacent whole bundles; the built test covers
the whole window. Workers should strengthen a ruling when the code shows
it short, and say so.

## Gate hygiene on a shared box, and dark suites (2026-08-10)

Three from one lane: never edit the tree while test/run.f is running (a
mid-run edit produced reds that looked real and cost a gate cycle); two
concurrent full gates on one host red each other's timing ratchets -
check pgrep -f test/run.f before starting one; and schedule-lint is the
thing that catches a suite that is registered but dark - trust it over
"I added a SUITE block", and prove scheduling by breaking one assertion
and watching the full gate go red.

## A running value across a block walk is a hidden dominance claim (2026-08-10)

The selector kept one running memory-order value across its block walk;
that was sound only while every un-synced access lived in the entry
block or the single exit block. The day a second exit block existed
(the trap), one sibling read the token another defined -
E-IR-VERIFY-DOM naming two sibling blocks is the tell. The repair
states the order on every edge at the ONE word that builds edges, and
refuses disagreement; and the fixture that keeps it honest is the same
shape with the arms swapped, because the original walk order can pass
by luck. Same lane, second find: deriving a new answer (NO-RET) is not
consuming it - grep every reader of the old answer before believing
the derivation landed.

## The memo is a dominance argument, not a liveness one (2026-08-10)

The block-local literal memo hands a value across block boundaries
legally (SSA-wise) and can still be an allocator failure: reusing an
earlier block's constant for UNREACHABLE code gave a real value a live
range across every call for nothing (measured E-A64RA-POOL on the
simplest dead-path shape). Stage unreachable operands fresh. Same lane:
the first DIALECT consumer of an IR form that only hand-built fixtures
exercised is where its missing table rows surface (two passes lacked the
trap's key row) - budget that surfacing into the first consumer's lane.

## Unreachable code still costs registers if you share its constants (2026-08-10)

(From the match lane, generalising the dead-path find.) The block-local
literal memo is a dominance argument, not a liveness one: reusing an
earlier block's constant for an unreachable trap operand gave a real
value a live range across every call. Stage unreachable operands fresh.
And a capacity six passes share (NFROZEN:BMAX) is a product decision to
raise, not a constant to bump inside a feature lane - measure the
unlock cost, pin the current ceiling in a test so the raise has a
number to move, and dot it.

## Three from the closed-forming lane (2026-08-10)

A test whose inputs are only reachable BECAUSE an optimisation fired
must assert the optimisation fired and stop if it did not - otherwise
a regression turns a red gate into a machine that never returns (a
2^63-turn loop was asked for before this was caught). A mutation that
leaves the suite green is a verdict on the fix, not the suite: a
suspected sentinel bug was already caught downstream, and the honest
outcome was a corrected comment, not a claimed repair. And optimising
a corpus row breaks the instruments that used it as a subject - two
loop-measuring suites named SUM-TO because it was the smallest counted
loop, and the fold left them measuring nothing.

## A refusal surviving a lift under the same code is not the same refusal (2026-08-10)

The two-function fixture passed before AND after the allocator lift -
but the check answering had changed (FUN-OF to COVER-CK), so the lift
looked landed and was not. Only neutering the suspected check and
watching a SPECIFIC case go red tells the two refusals apart. Sibling
find: putting one of two agreeing walks on a new number line breaks
the agreement silently - the validator moved module-wide, the
allocator did not, and the tell was E-A64RAV-INTERVAL on a case that
has nothing to do with intervals. The pair moves together.

## A spelling assertion is not a location assertion (2026-08-10)

When two candidate tokens share a spelling, a fixture asserting the
refused SPELLING passes with its guard deleted - both openers are `[:`.
Pin the refused ROW. Same family as the substring-assertion rule, one
level down: the assertion must name the coordinate the guard decides.

## jj split by file breaks stacked concerns (2026-08-10)

jj split is file-granular; a file carrying two concerns splits into a
parent that references names the child introduces - a commit that does
not load. After any split, build every intermediate commit on its own
before believing the stack. Same lane: a refusal that MARKS its token
for a handler is nameless in any pass without one - record the token
at the refusal site, not in the handler.

## The inliner defeats behavioral assertions written as call sites (2026-08-10)

A fixture asserting "the word still runs its own code" is silently
defeated when written as a compiled call site - the inliner may have
copied the body, so the test passes against an engine that corrupted
the record. Enter through the interpreter (evaluate), which uses the
address the record holds. The alias-reclamation case passed against a
BROKEN engine until the call form changed. Sibling rule from the same
lane: a recorded length is not a routine's extent (measured: 554 baked
records where START+LEN+4 is not the next start) - derive floors from
starts and slot-ordering, never from lengths.

## The mutation harness needs its own hygiene (2026-08-11)

Two false verdicts from one battery: an anchor that matched a DIFFERENT
word (assert anchor uniqueness before mutating) and a baseline that
still carried debug prints (assert the baseline green first). And a
fixture that passes for a body whose arity is symmetric proves nothing
about whether the body was walked at all - the S1 skip bug built
( n -- n ) bodies as accidental argument-pass-throughs and every
symmetric fixture passed; only an asymmetric body told the truth.
Sibling rule: carrying a fact on VALUE IDENTITY is a lucky-value
mechanism when calls rename values - acceptance then tracks whether
the inliner fired; carry facts on the vector entry the walk actually
moves.

## Four from the no-return lane (2026-08-11)

Gate evidence must be sequential - three concurrent heavy suites
produced a false red that vanished on rerun. pgrep -f inside a wait
loop matches the waiting shell's own command line and deadlocks; use a
marker file. Habu quotations are not closures - a probe's [: ;] cannot
read the enclosing locals; park values in a variable. And a mutation
that changes nothing falsifies the COMMENT that claimed it would - the
honest outcome is rewriting the claim into a derivation (second
occurrence of this pattern; it is a rule now).

## A token a keyword swallows is a hole in the tape (2026-08-11)

The census names the token the elaborator stood on, never what stands
after it - is/['] consumed their target without reporting it, and the
defer name was simply absent from the tape. Dump the tape before
designing a pass over it (a 20-line dumper falsified a design sentence
two leaves rested on). Corollary: a leaf that names a mechanism has
still not proved the mechanism's INPUT exists - "the MOPERAND?
mechanism" was true of MATCH and false of is, and one contrast probe
with the same dumper separated them.

## The seeded prefix made binary staleness a red class (2026-08-11)

Since the stdlib entered the boot prefix, tests that query the RUNNING
engine's provide surface (bundle assume-or-carry, anything reading
ENGINE-PROVIDES?) red under a bin/hb built before the seed - and bin/hb
is untracked, so every workspace carries its own. install --force before
ANY gate, not just after seed-file edits; a "tools-only change" is not
an exemption because the TREE under the binary moved. Two deterministic
red runs were this, not the change under test. Sibling notes from the
keyfix lane: run the package lint on a one-line probe diff before
designing any build-fixpoint change (it decides the change's shape);
using PKG is file-scoped and never reaches requiring files;
build-fixpoint-test self-runs at load so appended debug drivers never
execute - bisect by unregistering steps.

## Lane temp roots must be lane-private (2026-08-11)

Two concurrent lanes briefly shared one HB_TMP root, and one of them
does rm -rf on that root at the start of every gate run - it wiped the
other lane's build mid-flight. Every gate invocation names a temp root
private to its lane (and ideally to the run); a gate that ran during a
shared-root window proves nothing and must be rerun. This is also the
likeliest source of paired timing-assertion flakes across lanes.

## Packaging a legacy tool file: three checker-scope facts (2026-08-11)

From the BUILD-FIXPOINT packaging. (1) CHECKER-DEFINED? answers in the
checker's currently OPEN package scope - a bare global name asked from
inside a package answers "no", so a load-discipline guard silently
inverts when its file gains a package; make the guard public and ask it
with the package closed (audit dot 1504bbde). (2) Anything that runs a
build or in-process certification (VERIFY:SOURCE-BUF) must execute
after ;package, or the verified source is checked against that
package's wordlist. (3) EXPORT NAME inside a public section is the tool
for packaging a large legacy file: definitions stay private in place
and the export list reads in one block - no forwarding bodies, no
toggles scattered through 300 definitions.

## `using` covers calls, not parsing words (2026-08-11)

From the hb-build packaging: `is` resolves the name it parses through
the engine's own lookup, which does not consult used publics - a bare
defer target under an open `using` import fails (rc 70, bare token on
stderr, no location; diagnostic dot b83bcfa5). Anything that PARSES
its target name needs the qualified spelling even inside the import.
Also: a package whose exported tails share the package's own prefix
walls off future head edits (E-REDUNDANT-PACKAGE-PREFIX fires on the
next change) - pick the package name so the tails don't repeat it.

## Editing keyed phase files runs the gate cold (2026-08-11)

The gate caches phase results keyed on declared file sets
(test/run-files.f). An edit to any file in those sets - lib/content-key.f,
test/run-lib.f itself - invalidates every phase key, so the next battery
runs fully cold and takes minutes longer; under a concurrent lane that is
exactly when timing assertions red (TIMEOUT-UNDER-LOAD, group-time
ratchets). A red whose failing member does not reference the changed
code, on a fully-cold run, is this - prove it (the fold census reporter
reads clean at the throw site), then rerun sequentially. Two sequential
greens settle it.

## Before minting a name, grep for what the engine already bakes (2026-08-11)

Two of the DEFER-DIAG landing's ten lint findings dissolved by deletion:
ONELF duplicated the NL-KW byte the engine already bakes, LDEFLF
duplicated LOPENNL, the newline label three unrelated diagnostics
already write. Finding the existing surface cut the finding count by
two with no new ownership at all.

## A file's own package comments are law (2026-08-11)

The HOLD-EMIT comment in habu2.f ("they own one concern between them
... rather than joining the global emitter surface around them, which
is pre-existing debt and not a pattern to extend") is a general rule,
and reading it overturned a placement ruling made from outside the
file. The precedent that looked like it settled the question
(KWDATA:LKWTRUSTRAW) actually distinguished it: KWDATA holds a label
whose CONSUMER lives in another package. Labels couple to their
concern's package when one exists.

## The relocation lane's twelve (2026-08-11)

Closing lessons from the per-site relocation lane, verbatim from its
two workers:
1. The gate builds its own candidate, so a locally-fresh binary proves
   nothing about what the gate measures.
2. A parity table can be tied to the model by a different suite than
   the one that looks like it owns it; falsify each clause against the
   suite that actually reds.
3. A refusal reached through a shape another guard already rejects
   proves nothing about the guard you meant.
4. A stricter replacement pass reds a size ratchet with the same
   signal as a broken one - explain the count before touching the row.
5. A record replacing a heuristic must enumerate every path that
   REPRODUCES the artifact, not just every path that creates it; the
   inliner's refusal set is the map of what the tree already knew was
   copy-unsafe, so a refusal list is where to look for what a new
   record must propagate.
6. A shape test that counts raw substrings makes source prose part of
   its input - check the counting tests before writing the comment.
7. A test that reports a failure must not then dereference the value
   the failed assertion was about (a clean red became SIGSEGV that
   reported nothing).
8. The package diff lint's refusal names the owner for you - read it.
9. A move-only refactor of a builder file can be proved neutral:
   build, byte-compare, then add the behaviour - which is what makes
   the next size delta attributable.
10. After a merge, start with `jj new <master>` - `jj rebase -r @` on
    already-merged content duplicates the merge commit and conflicts
    on any file the merge deleted.
11. A guard's failure code is not its failure path: read what the
    guard IS (a value in a cell, not a code at a site) before masking
    to a value you assume it recognises. Zero in a dispatch cell was
    measured as a diagnostic-free SIGSEGV.
12. Check that the acceptance has a subject before building the
    mechanism: three cheap reads showed the native chain never runs
    in a capture window, so the intern leaf's acceptance had nothing
    to observe. The feature can be fine while the observation path
    does not exist.

## A refusal that aborts hides every finding behind it (2026-08-11)

The first fail-closed TRUST sweep reported ONE stale row per file -
the first throw hit. Multi-error participation revealed 42 in one
file and a 53rd site that had been exiting with a bare code and no
diagnostic. A check that stops the load at the first finding
understates the work by whatever hides behind it; a fail-closed
check should fail the load, not fail it mute and early.

## A declaration inside an unchecked window is name-only (2026-08-11)

0 set-check zeroes the hook cell, and the definer's publish tail is
gated on it - a TRUSTED: inside such a window publishes the word and
registers NO effect. It fails closed downstream (uncheckable) but
looks exactly like a broken fixture, and it silently turned a
suite's 19 declarations into stubs. Declarations go above the
window. Engine-side loudness is dot 527eea9a.

## A __text change answers two ratchets (2026-08-11)

gate-build-size.f measures the page-rounded file and padding can
absorb growth; gate-size-attribution-test.f measures the bytes and
trips. One green is not evidence about the other - it is evidence
the padding hid the growth, which is the gap the byte ratchet
exists to close.

## An engine older than a two-stage landing cannot skip the stages (2026-08-11)

A pre-stage-1 bin/hb dies at install on stage-2 source (its baked
publish tails call `trust` on not-yet-findable names, which stage 2
refuses - exit 67 mid-boot). That is the two-stage constraint
working, not a defect. Recovery for a stale workspace engine: copy a
current fixpoint binary from a sibling workspace (then install
--force to confirm), pass through the stage-1 tree, or bootstrap.

## The write-window landing's six (2026-08-11)

From the code-region window lane, verbatim: (1) a design sentence
about a hot path must be checked against EVERY writer, not the one
that names it - "LCEMIT is str/add so a write never runs ahead of
CP" was true of LCEMIT and false of seven byte-spill sites; (2) a
stores-based-at-R scan must follow one level of register copy AND
both spellings of the register (CP and 28); (3) install --force
after every mutation experiment, not just before the gate - a
mutant whose build succeeds replaces bin/hb and every later mutant
runs on a broken host; (4) an emit-time macro carrying a bare
register number leaves it in front of the next instruction where a
positional reader takes the wrong operand - give macros a register
ABI; (5) package-diff-lint refuses any changed word in an
unpackaged legacy file, so "fix the lint" can be gated behind
"package the lint" - check before planning a lint fix into a lane;
(6) the crash handler exits 134 itself, so a refused write is an
EXIT not a signal on both targets - assert T-OUTCOME-EXITED= 134.

## A profile of a system that no longer exists (2026-08-11)

The guard inline-latch was specified from a 16.2ms profile taken
BEFORE the write window landed. On the post-window engine the win is
inside the noise floor in both directions, and deleting the guard
call outright wins nothing - the guard's apparent cost had been
serialisation behind the 8MB mprotect churn the window removed. Two
optimizations measured against the same baseline are not additive if
they share a cost; re-profile after the first lands before building
the second. The refutation is recorded on the leaf so the idea is
dead, not pending.

## A mutant that fails to compile tests nothing (2026-08-12)

A mutation whose edit leaves the file uncompilable produces no red
and no green - the suite never ran. Before reading any mutation
result: prove the edit landed (assert the text changed, read it
back) and prove the mutant COMPILES and differs from the original.
"No TFAIL in the output" of a file that never loaded is not a pass.

## The judge lane's four (2026-08-12)

From the five-corpus landing, verbatim: (1) an xor fold of two
observations is vacuous whenever one determines the other -
CELL-BUMP's answer IS its cell, so folding them is identically zero
and hides a column wrong in both; compare separately or prove
independence before folding. (2) Every generated definition is a
dictionary record never reclaimed - a fuzz driver minting one per
(program, input) makes sweep size a dictionary question; one driver
per column reading input cells removes the ceiling and the
literal-spelling problem at once. (3) A generated diagnostic that
prints a shared buffer names the wrong subject the moment two texts
differ. (4) `rg -rn` is --replace n, not recursive-with-line-numbers
- it silently rewrites every match and made a scheduled suite look
unregistered.

## Check what a mechanism buys, not only what it guards (2026-08-12)

The call-crossing threading looked like a duplicate of two downstream
safety nets, and suppressing it passed the corpus row it was blamed
for - but the corpus row's callee was chain-published. Against an
ENGINE-compiled callee (no clobber record) the threading was the only
home a call-surviving value had, and the "proven one-line fix" broke
the stdlib's own multishot site. A fix proved on one corpus shape is
proved for that shape's callee class. "The hazard is enforced
downstream" answers safety; it does not answer whether the mechanism
was also the only place the value could live.

## The literal-authority landing's three (2026-08-12)

(1) "The value doesn't exist yet at that point" is a measurable
question, not a taste question - one read of the engine's literal
hook (bytes handed over, never the number) settled a two-shape
design. (2) A mutation that dies before the assertions run is not
evidence - the register-swap mutant exited 134 and proved nothing;
the +1 mutant lit 27 assertions. (3) Deleting a decoder deletes its
ratchet debt - real-lit.f carried a standing obligation to track a
known engine float bug; asking the engine directly retired the
obligation for free.

## A staged callee list is not a callee selector (2026-08-12)

DEFINE-CALLING resolves the body's names off the dictionary, so a
mutant that swaps the staged row changes nothing and proves nothing
- one ineffective mutation and one dead helper came from assuming
otherwise. The probe file's prose now says where the callee really
comes from.

## A measured number lives in its instrument's units (2026-08-12)

The bitmask landing's row deltas looked either regressed or fabricated
on today's master - but the byte column changed UNITS one commit after
the fold (df83ade99 made it count the trailing return, +4 every row),
exactly cancelling the fold's -4 in absolute numbers. Both lanes
measured truly; the discrepancy was manufactured by comparing across
the unit boundary. Before declaring a cross-commit discrepancy in a
measured number: re-measure both endpoints with each tree's OWN
instrument, and check the instrument's fixed point - the NOOP row's
0->4 announced the change in every report.

## The widen landing's four (2026-08-12)

(1) An emitter that writes an address into region bytes obligates
BOTH halves of the relocation parity gate - formal/Common/Reloc.v
AND the frozen manifest in reloc-schema.f, which names the bare
source tail, so a package rename is a manifest edit. (2) Never edit
the tree while test/run.f runs - forked workers load source files
mid-run and produce reds that are not about your change. (3) jj op
restore is unavailable for undoing one lane's interleaving - the op
log is repo-wide and shared; `jj --at-operation <op>
--ignore-working-copy file show` reads a past state without touching
anything (recovery verified by rebuilding a byte-identical engine).
(4) A new package in a legacy engine file is byte-neutral for the
engine size but shifts baked WIDs - the sha changes while the size
does not; expect it, don't chase it.

## The residency landing's three (2026-08-12)

(1) A build that never loads the mutated file proves nothing -
install --force does not compile the native chain (only migrate.f
requires select.f), so an install rc 0 after a chain mutation is
silence, not evidence; the consumer load is the check. (2) One HB_TMP
per RUN, not just per lane - two gate runs of one lane sharing a root
wiped each other mid-flight. (3) The rg wrapper rewrote matched text
in search output (--replace struck again); file lists came from grep.

## The hoist landing's four (2026-08-13)

(1) RUN THE GATE WITH STDIN FROM /dev/null in agent shells - children
inherit a unix-socket stdin and some block forever probing fd 0; the
hang surfaced as timeouts on UNRELATED process phases (diagnosed with
sample + lsof, fd 0 = unix socket; < /dev/null turned two reds green).
(2) One gate at a time even in your own lane root - a second run
against the same HB_TMP reproduces the shared-root incident within
one lane. (3) A mutation of an analysis in a pass that VALIDATES its
plan yields a decline, not wrong code - check which guard caught it
before claiming the mutation proves the one you edited. (4) A fixture
that merely contains a store may not test the write rule - the rule
needed a read, a write AND an accumulator in one body to be reachable.

## The pool pricing's five (2026-08-13)

(1) Read the diagnostic, not the error text - E-A64RA-POOL says
"every register holds a needed value"; in every measured row it was
"every register is BARRED and nothing holds anything" (free=18
spare=0 forbid=all). (2) Two throw sites sharing one code need
one-at-a-time recoding to attribute a class. (3) Publishing a callee
can make its caller WORSE - a chain routine that can throw records
24 destroyed GPRs, wider than the 18-register no-record worst case.
(4) The census silently skips a package section with no public
definition - examined=0 looks like a broken path. (5) The class's
fix was invisible to its own leaf because the leaf named a mechanism
instead of a measurement - the four-verdict experiment (same body,
constant vs digit) was the whole diagnosis.

## The i/j-locals landing's five (2026-08-13)

(1) A refusal can outlive the collision it guards: the elaborator
refused every local whose name the dialect models, yet six of its
eight body-token readers already asked LOCAL-OF before the word
model, so the two meanings could not have met. Read the pass list
before believing a refusal's own reason. (2) The ENGINE settles what
a name means and it had already been written down - docs/forth.md
said "local-first", and `{: i:n :} 0 3 0 ?do i + loop` answers three
turns of the LOCAL (15 for 5) while the same body without the
declaration answers the index (3). Check the documented convention
before designing a rule. (3) The two passes that read a body token
WITHOUT asking the frame were found by reading COLON's pass order,
not by testing: `is` marks the row after it as an operand, and an
operand is a row the walk steps over, so a local named `is` would
have compiled a SHORTER program with no refusal anywhere. A refusal
list would have hidden that hole; guarding the passes exposed it.
(4) A local's name is the bytes before the annotation's colon, so no
local name can contain one - which quietly makes `[:`, `{:` and `:}`
unspellable as names and left exactly one token to refuse. Derive the
refusal set from what can REACH the check, or half of it is dead
code. (5) The census over `src lib` is reproducible; over `tools` it
is not - most of tools/ runs a MAIN word at load and one of them
(build-fixpoint-main.f) exits the process, so a whole-tree census
stops there and prints no report at all.

## A refusal list can hide the miscompile it was meant to prevent (2026-08-13)

The chain refused every local named after a dialect word - guarding a
collision that could not happen (six of eight readers already asked the
locals frame first) while the two readers that did NOT ask included one
that silently MISCOMPILED a local named `is` (the deferred pre-pass
stepped over it as an operand: shorter program, no refusal anywhere).
Deleting the list and naming the invariant - every meaning-reader asks
the frame first - exposed and closed the hole the list hid. Derive
refusals from what can structurally reach a check, never from a list of
spellings.

## The definer-kind fold's five (2026-08-13)

(1) A field carve in a full cell is never one edit: the name-length
band's 12-bit clear had FOURTEEN native readers plus a hardcoded
duplicate of the mask in aot-capture.f, and the AOT seed's compact
record refuses any record with stray bits in the band it does not
carry (`rec [16] stray high bits`, rc 74) - which is the capture
failing closed on an unconverted record, so the build tells you where
the rest of the owners are. Grep the mask AND the shift.
(2) Folding a named word to a value can make a body WORSE, and the
reason is the literal memo, not the fold: two mentions of one number
become ONE value, and a value defined before a call to a callee with
no clobber record and read after it is barred from every register.
The engine test suite found it (native-migrate.f VOID-CALL-CASE, a
body naming one `variable` twice around two calls). The fix is the
rule the file already states about locals - a call that keeps nothing
ends what may be carried across it - so the memo is emptied at that
call and the second mention is staged again for the price of a move.
(3) A memo with a mark/release scope needs a MONOTONIC release once
anything else can empty it: a mark taken before such a call would put
the emptied rows back, holding value ids the call consumed.
(4) The engine already folds named constants and has since before this
lane: its compile-mode inliner copies a short safe body inline, so
`K7 +` and `7 +` are both 56 bytes of engine code. The chain was the
only compiler in the tree that made a call. Measure the engine's own
answer before calling a shape unsupported.
(5) Wrapping each case of a test file in `catch` HID the failure - the
case that threw ran clean under the wrapper and died without it. Run
the cases SEQUENTIALLY and unwrapped, printing each name before it
runs, when locating a throw; the catch changes what the case does.

## The do landing's two (2026-08-13)

(1) A census run re-reads src/lib file TEXT - editing the tree
mid-census corrupts the measurement exactly as editing during
test/run.f does. (2) (repeat of the no-return lane's lesson, hit
again) pgrep -f <pattern> in a wait loop matches the
waiting shell itself - two wait loops never returned. Wait on a
file, not a process name.

## The return-stack lane's four (2026-08-13)

(1) A leaf can name a capability the tree already has under a
different owner. "Explicit typed return-stack state" was the
checker's return row all along; one grep for the acceptance's own
vocabulary (RCUR) found it before any design work started.
(2) A syntax flag is never the invariant. ER.HASR says whether an
author WROTE a `| rin -- rout` clause; `( n | R -- n | R )` writes
one and moves nothing. Expose what the recorded rows SAY, and build
the fixture as an adversarial pair - two words identical in the
syntax dimension, opposite in the fact dimension - so a reader keyed
on syntax reds one of them whichever way it answers.
(3) An analogy to an existing mechanism must be checked against the
new member's discipline before it prices the work. "A fourth carrier
beside loop counters and crossing locals" was wrong in the way that
mattered: those are scalar carriers with pre-scanned counts, while a
parked-value vector is a variable-height typed stack needing the
data vector's width/glue agreement at every join opener. The seam
map showed this BEFORE the coding started; the lane pressed on
instead of checkpointing. Checkpoint on the first structural
surprise - the half-feature that resulted cost a second lane.
(4) Fixtures for a checker refusal must be type-correct in every
dimension NOT under test: four branch-imbalance probes were refused
for a data-stack arity error and proved nothing about the return
row.

## The again/leave landing's three (2026-08-13)

(1) A leaf's design sentences age. Both of this leaf's premises -
"a while can give the after-loop block an edge" and "the
elaborator cannot say unreachable" - were falsified by two cheap
probes (the checker refuses while+again outright; PATH-DEAD landed
with the dead-call work), and both falsifications made the work
SMALLER.
(2) A contract table's "this cannot arise" is a claim to falsify,
not a wall: abi.f said a callless no-return routine cannot exist
because what makes a body all-dead is the call it dies in;
begin...again is exactly that routine. The repair is a contract
stating field-by-field truths (no call so no trait, nothing writes
x30 so link preserved, control no-return unchanged because callers
were compiled against it), each held against the module by an
existing validator.
(3) A fixture that counts DOWN terminates only for the inputs a
reader checked: `over 0 =` hung the suite on a negative counter
where `over 1 <` does not. Guard loops with an inequality, not
equality.

## The join threading's four (2026-08-13)

(1) A differential fixture that combines carried values with a
commutative operator cannot see them exchanged - it proves only
that the right NUMBER of cells came back. Weight each value with a
distinct odd factor; the order-reversal mutation passed until the
weights went in.
(2) A differential loop fixture must also terminate under the
ENGINE's compilation: a trip count driven by the value under test
hangs the suite at some inputs. Make the value affect the count
without driving it.
(3) When a brief names a field to add, ask whether the field is
the PAIR of one existing field or of several. The join width lived
in three frame fields, so its parked sibling needed three, not one
- reading DO-JOIN-WIDTH clause by clause was the whole diagnosis.
(4) A fixture whose comment says "this must change when X lands"
is a promise to the lane that lands X - two came due here and both
were found by the gate, not by reading. install --force does NOT
compile src/compiler/native/*.f (the chain loads on demand), so a
syntax error there passes the build and only a slice run catches
it.

## The catch landing's three (2026-08-13)

(1) A row measured AFTER unification is the live stack, not the
row: the fit-check binds the quotation's open tail to the rest of
the stack and the two rows become one row. Ask width questions
BEFORE the check that binds - there is no reading of the graph
afterwards that can tell the two apart.
(2) A differential over a multi-cell answer must bind both
compilations' results before comparing; A B T= T= holds each
answer against itself and passes on wrong code.
(3) Size a refusal ceiling so the refusal is REACHABLE: a
32-site catch table could never fire (the 128-token tape refuses
first at 31 sites), so its guard branch would be untestable. The
ceiling below the shadowing cap is the one a test can take.

## The locals-scope landing's three (2026-08-13)

(1) A clean textual merge across an exhaustive MATCH is not a
merge: the new enum member from the other lane red the load with
"missing variants" - and only BECAUSE the match is exhaustive.
Non-exhaustive dispatches would have silently skipped it. After a
rebase, the honest check is "what did the other lane newly
SCHEDULE and newly ENUMERATE", not just "what did it edit".
(2) A new pass over body tokens must ask the locals question
before any classification - the file already said so, and the
pass that didn't broke {: again:n :} by reading the local as the
loop closer. One structural question covers every reserved
spelling; a list never does.
(3) Before believing a fixture exercises a carrier, force the
carrier's value to a constant and check something reds - the
crossing-locals carrier was dead-tested across five suites
because every fixture's callee had a clobber row and nothing
ever travelled.

## The j landing's four (2026-08-13)

(1) The census's pressure/capacity refusal codes are not
reproducible run to run (one definition swapped E-A64SEL-CAP for
E-A64RA-SPILL over the same tree); the totals and dialect buckets
are stable. Never attribute a code swap inside that class to a
change.
(2) A backgrounded subshell survives the harness's completion
report - two censuses collided on one HB_TMP. Let the harness own
background jobs.
(3) The reserved-word rule bites in test fixtures: private
helpers named IF and LEAVE died as "closer without opener" before
any assertion ran.
(4) Compiler test leaves need the fork harness's deps (lib/date,
lib/test, lib/source) - a bare --load of native-hir.f fails on
master too. Run leaves through the gate before concluding your
change broke them.

## The match probe's two (2026-08-13)

(1) A leaf's title can name the wrong axis: "multi-cell payloads
in match" measured as fully supported, while the real blocker was
the parametric instantiation wider than its declaration. Two of
the leaf's three concrete claims were false, its requested
fixtures already shipped registered, and an open dot already
owned the real work. Eight cheap MEASURE-HELD probes separated
the axes before any design was written.
(2) A census histogram and a per-definition row scan must come
from ONE process - two runs of the same instrument over the same
tree disagreed by one row; the run printing both numbers agreed
with itself.

## The quotation-scope landing's three (2026-08-13)

(1) A refusal whose CODE depends on the shape of the surrounding
routine is a missing per-function fact, not a dialect limit - the
same body text refused -8088 under a straight-line definition and
-8091 under one holding an if. Vary the enclosing shape before
designing to the code.
(2) After fixing a producer, look for the mirror bug in every
consumer that rebases the same number: the fix moved the refusal
from the verifier to the allocator because only select.f had done
the R-BASE subtraction. Grep the number's readers, not just its
writer.
(3) A differential over a caught rethrow cannot compare the value
under the code - depth is restored, contents are not, and on that
path the routine that threw is the subject compiled two ways.

## The successor-ordinal landing's five (2026-08-14)

(1) When a producer's fix MOVES a refusal instead of clearing it,
the arriving refusal is the next mirror of the same bug, not a new
one - three passes each hid the next (allocator, verifier, emitter).
Keep going until the probe accepts.
(2) A diagnosis naming N sites is a hypothesis: the sweep found a
sixth reader the leaf never mentioned, correct only because it
declines the whole case - itself a finding (lost optimisation,
dotted). Classify every reader as correct / needs-it / declines-it
and say why for the declines.
(3) Mutate the setter as well as the reader - forcing a base to
zero red DIFFERENT fixtures than dropping the subtraction, because
the contiguity guard fires before any successor is read.
(4) A guard's mutation may surface only as a wrong answer elsewhere
- the raw-read revert changed no refusal code in the shape probe
and only red the suite at a different assertion. A mutation matrix
scored on refusal codes alone calls such a site untested.
(5) Destruction review over a day's merges caught what per-lane
review could not: a later commit REPLACING a sibling's suite
registration (the two suites ran only via the fork list), and a
false measured claim guarding an untested reachable refusal. The
cross-landing interaction probes all passed - the holes were in
registration topology and prose, exactly where fresh eyes look.

## The width-export landing's four (2026-08-14)

(1) To call a failure pre-existing, the base thing must be the
test's INPUT, not the binary that runs it - a source-reading lint
run with an old engine over a modified tree measures the tree.
(2) A lane that adds a checker axiom owns a ratchet it never
opened. Grep finds names; a ratchet is a NUMBER - find it by
asking which tests READ the file you changed.
(3) Belt-and-braces is dead code until a mutation binds it: the
saturation flag survived design review and died in one measurement
(overflow drops a suffix; every suffix token already refuses on
absence). Write it, measure it, delete it, and say why in place.
(4) Publish the count the consumer needs, not a delta it re-adds -
two stages with separate arithmetic can drift; two readings of one
subtraction cannot. And file every new per-token fact against the
ONE shared recording ordinal; a second counter is the mistake the
first table's prose already names.

## The repair lane's two (2026-08-14)

(1) "Reuse the existing wall" is one line only if nothing reads the
wall's side effects: CLOSE-BLOCK looked like a drop-in at the final
block until NB turned out to be the tail-call decision's input -
the failure mode was a silently lost optimisation no suite catches.
Grep the counter's readers before reusing the counter's word.
(2) A refusal ceiling needs its neighbour measured: 17-refused
alone passes against a chain refusing every depth over four;
17-refused beside 16-compiled is the assertion.

## The two wide-family landings' four (2026-08-14)

(1) A number's SHAPE is decided by what its consumer already
emitted: the MATCH half publishes an absolute pad count (its
consumer emitted nothing); the construction half publishes a
DIFFERENCE (both its lowerings already push the declared cells).
"Publish the count the consumer needs" is not "publish an
absolute".
(2) The absence policy of a token-keyed store belongs to each
READER, not the store - a reader that proceeds on absence needs
its own safety argument (here: cell conservation, pinned by a
fixture pair, with the prose naming the change that would break
it).
(3) A refusal can be a per-definition approximation meeting a
per-site truth: "travelling" is meaningless at the call the
routine leaves through. Ask which pass over-approximates before
designing a new mechanism; when four passes ask one question and
three run before a block exists, the block-shaped derivation is
the bug.
(4) The register verifier is a sharper mutation detector than
differentials - two mutations that looked like lost optimisations
were named refusals (DKEEP, DSTACK). Read WHICH pass answers a
mutation, not just that something red.

## The tail landing's two, and a strengthening (2026-08-14)

(1) STRENGTHENED, paid for a third time: pgrep -f in ANY wait loop
- until-loops included - matches the waiting shell's own command
line. Wait on the output FILE (until [ -s out.txt ]), never on a
process pattern.
(2) A generated constructor escapes hyphens in the owning package
name: package NTL-FIXTURE + PRODUCT pt publishes
NTL--FIXTURE-PT:MAKE (TF-CTOR-ESC doubles the hyphen; production
example CNUM-NUMERIC--POLICY:MAKE). A fixture package with a
hyphen pays it - cheaper to know than to E-UNDEFINED into.
(3) An impossibility argument belongs IN the suite's prose with
its measurement: the padded-construction-feeds-tail shape is
unwritable (the checker refuses the required signature), and the
derivation is recorded where the next lane will look before
re-deriving it.

## The singletons diagnosis's five (2026-08-14)

(1) A leaf's refusal list ages faster than the leaf - two of four
named rows were zero after eleven landings; re-censusing cost 13
minutes and deleted half the work order. Re-measure before
diagnosing from a recorded list.
(2) One root can wear three error codes, and the code names the
pass that NOTICED, not the pass that is wrong. Reduce until the
shapes differ in exactly one dimension before believing a bucket
is a class.
(3) A taxonomy's written argument is falsifiable - the census's
claim that an arity refusal always means self-misstatement had a
counterexample in its own output. Test a classifier's reason
against a row, not against its prose.
(4) A refuted road stays refuted only for the population it was
measured on - re-running the refutation against today's rows cost
one mutation and turned an assumption into evidence.
(5) A censused spelling may be the instrument's own renamed
subject leaking into the table.

## The create-axiom landing's three (2026-08-14)

(1) A table keyed by a token can hold a fact about a DIFFERENT
word - the create row stated what create DEFINES, and its own
introducing commit said so. git log -S beat re-derivation: ten
minutes to the why once the archaeology ran.
(2) A wrong axiom shadowed by a special case is invisible where
everyone looks: DEFINER-TOK short-circuits signed bodies, so the
fault lived only in the UNSIGNED leg (where it certified a
miscompile) and in every by-name reader. Test the leg the special
case does not cover.
(3) A comment can move a published measurement two ways: through a
tokenizer that mis-lexes it and through a file-size cliff. Measure
the build report and the file size after adding prose to a
boot-prefix source.

## The bundle-seams landing's four (2026-08-14)

(1) A guard's safe direction is a property of its CONSUMER: the
glue over-approximation was sound for four landings because every
reader refused on it; the first reader that SEGMENTS turned the
same over-answer into a miscompile. When a consumer changes from
deciding to computing, re-derive every producer's fail-closed
direction.
(2) A boolean derived from two counts cannot carry three cases -
store the count, let the consumer state the cases.
(3) A record not trimmed when its subject shrinks holds live and
stale data at adjacent indices - and the boundary between them is
exactly the index a one-more-bit change reaches for. The census
found it; no suite or review did.
(4) Measure a family's row shapes before designing to them - two
of the three buckets the fail-closed design had to handle were
empty, which decides how much machinery to mint.

## The recorder landing's three (2026-08-14)

(1) A fixed whole-file arena is a SHARED tripwire, not a local
choice: four lints sat 59 bytes from red on master, and the first
lane to add a paragraph to the tree's largest file pays for all
of them - then the package lint forces a migration before a
single byte can be edited. Check wc -c against the arenas before
adding prose to src/core/checker.f.
(2) Derive a cap from the constraint that makes it real: TEXT-CAP
IS the engine's capture bound (whose overflow is a FATAL exit,
not a throw - the refusal is what stands between a long body and
a dead process); the tape is a function of its input. A derived
bound moves when its constraint moves and the fixtures follow.
(3) A lifted cap re-prices every ceiling derived from it: CWIN-MAX
16 was unreachable behind 512 bytes and now refuses real tree
code. When a bound moves, sweep everything whose reachability
argument cited it.

## The wide-memory landing's five (2026-08-14)

(1) Before minting a per-token export, grep the PRIM rows for the
fact itself. The width `@` and `!` move was already filed by the
checker AND already exported (WF-W-AT), because the engine's own
pass-2 recompiler needs it - so the chain's half was a two-line
reader and no new store, no new axiom and no ratchet. The three
sibling readers are keyed on the recording ordinal and that made
a fourth ordinal-keyed table look inevitable; the fact's own key
was the token's SOURCE BYTE OFFSET, and the tape carries it,
checked (feed.f validates every span against the bytes at the
offset it claims).
(2) A differential cannot see cell ORDER unless the two compilers
are CROSSED. A store and a load that both walk a bundle the wrong
way round answer every value correctly, so write-with-one,
read-with-the-other is the only fixture that binds the reversal
mutation - and it found a real defect the same day.
(3) The engine is the spec only where the engine is right. Its
wide fetch of a parametric family instantiated WIDER than its
declaration ends the process (`hb: bad layout tag`, exit 85) while
the chain's reads correctly: layout-valid.f QUEUE-SUM takes the
tag's slot from the DECLARED slot count, which at that
instantiation is a payload cell. Twenty lines, no migration in
them. When a differential's engine column aborts, reduce it to an
engine-only reproducer before assuming the new code is wrong.
(4) A per-token fact stops the body being INLINE-recordable. The
record keeps spellings and kinds, not offsets, so a spliced row
would be elaborated against the caller's facts; the rule belongs
where the row is WRITTEN (SPLICEABLE?, where the fact is still
live), and the caller side inherits it exactly as it inherits "no
row holds a call". Its fixture pair must not use a buffer word for
the address - a buffer word is a call, and then NEITHER body is
recorded and the width decides nothing.
(5) A hand-derived expected value is worth writing down and worth
re-deriving from the ENGINE's own MAKE/UNMAKE order rather than
from the field list: six of eight constants in the first draft
were wrong in the same direction, and the differential half of
each case passed while the pinning half failed - which is the
pair working as designed.
