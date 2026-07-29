# Lessons

Last updated: 2026-07-29

Durable, transferable rules only — "when X, do/never Y because Z", with the
specific word / path / constant / error kept. Coding standards live in
`docs/forth.md`; API details in `docs/` near their feature. Pure status
narrative ("X landed on date D") is not a lesson; the full dated campaign log
with all falsification detail is archived in `docs/archive/lessons-2026h1.md`
and in git history. One tight bullet per lesson; add a section only if none
fits.

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
  NORET-ADD). Adding a new WF-cert flag ripples through FIVE places
  (`lower-cert-base.f` constant, `PPRIM: LOWER-CERT` model, the `VALIDATE-WF` flag
  mask — was hardcoded `& 3` — plus its width accounting, the TRUSTED.md
  `primitive-effect-inventory` manifest, and the `prop-test-core.f` AX-CENSUS
  list). The cert VALIDATOR bites first: the first symptom of a missing validator
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
- **A new content-addressed CAD-KIND owner (`TRUSTED: RAW>X`/`X>RAW`) needs FOUR
  coordinated edits or a gate goes red** — trusted-inventory strict and refine-lint
  read DIFFERENT registries: (1) `TYPEFAMILY x-id` in `maki/cad-kinds.f`; (2) human
  TRUSTED.md rows for BOTH `RAW>X` and `X>RAW`; (3) the machine `file:RAW>X prim-axiom
  <epic>` classification block row for both (strict fails "unclassified site(s)" on the
  BLOCK, not the human table); (4) the `RFL-SEED-NAME$`/`RFL-SEED-OWNER$` case + bumped
  `RFL-SEED#` in `refine-lint-core.f` for the MINT direction only (else `NEW-MINT`).
  Only `n -- CAD-KIND:x` is shape-scanned; the projection is seed-exempt. Nullary
  proof-token mints (`( -- proof )`) aren't mint-shaped to refine-lint but STILL seed
  them (the seed list is the CONFINEMENT set). The POOL refine-lint phase is stricter
  than standalone — a mint clean standalone can red the gate.
- **A new TRUSTED: word needs rows in BOTH TRUSTED.md sections (effect table AND
  site-registry `file:name class owner`), owned by a LIVE dot — never the implementing
  dot (it closes).** trust-lint checks the markdown; `trusted-inventory-test` checks
  the site-registry — DIFFERENT corpora, a diff passing one can fail the other, so gate
  TRUSTED-touching diffs on BOTH. Reuse a sibling `stdlib-boundary` placeholder owner.
  A file-level fold (`builder-emit` in habu2.f) needs its count bumped in the
  `trusted-inventory-classes` block too. A computed-argument `set-check`
  (`check@ set-check`, not literal `0` or a ticked name) is a trusted-inventory site
  (file-level count row), separate from `checked-boundary-lint`. A near-full fixed
  arena is a latent capacity bug a downstream lane inherits (the class arena `CSTR-CAP`
  sat at 65528/65536 → a bare `class arena overflow` die on +4 rows) — budget the
  scratch arena when growing a ratcheted manifest, not just the count.
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
  Fixed `$20000`/`$40000`/`$80000` file buffers (shadow-lint, trust-lint, trusted-inventory,
  maki-dep-lint, error-code-lint) and the tokenizer `TMAX` ($6000→$8000) all trip
  "file exceeds buffer" as `checker.f` grows (it is the largest). Rules:
  size caps from the real corpus with the driver NAMED in a comment; sweep EVERY READ-FILE
  cap in one pass when a named file trips one; route lint CLIs through `LINT-MAIN` (catch,
  print `tool: threw <code> (<name>)`, re-throw); the shared `LINT-READ-DIE` prints the
  offending path; never read `$?` after a pipeline.
- **`stdlib-manifest-test` counts every distinct flat library file in the public-signature
  closure, not only manifest module rows.** A new shared dependency can therefore trip
  `SMT-LIB-MAX` even when the number of modules stays below the old limit; size that loud
  wall from the complete flat `lib/*.f` corpus and keep the capacity failure.
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
- **Stdlib files need their source file and a `lib/std.manifest` row, plus
  `TEST:SUITE` + `TRUSTED.md` for any `TRUSTED:`.** Match
  `tools/public-signatures.f` output EXACTLY (`TRUSTED:`/constants get no row; effect must
  sit immediately after the word name, before `{: :}` locals, or it is invisible). Keep an
  unavoidable trusted seam private behind an ordinary checked public wrapper so the general
  signature drift gate owns the public manifest row and `TRUSTED.md` owns only the raw seam.
  Miss the manifest and the direct manifest gate fails. The lint-manifest
  slice is the OWNING gate a new-lib lane must run (host/trust/coverage do NOT cover
  it). `lib/` subdirs (`lib/ptx/`) are research sub-libraries: gate `SMT-COLLECT-LIB-FILE`
  on `SMT-LIB-FILE?` (flat `lib/<module>.f` only) so coverage tracks flat modules; nested
  dirs stay trust-audited + `-test.f` + gate-covered but out of the curated manifest
  (mirrors top-level `maki/`).
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
- **Packaging a stdlib module for the manifest: put the public API in a `public`
  SECTION, not `EXPORT`-from-private.** `tools/public-signatures.f` `PS-PUBLIC?` checks
  `PS-IN-PKG` BEFORE `PS-EXPORTED?`, so a word defined in a package's `private` section
  and later `EXPORT`ed emits NO manifest row (the code still runs and resolves as
  `PKG:WORD`, but the stdlib-manifest gate never sees it). When definition order forbids
  one trailing public section (a public accessor is used by an earlier private word,
  e.g. `JR:SPAN$` used by `JR-READ-NUMBER`), use several `public`/`private` toggles so
  each public word is declared in a `public` section at its natural position. Constants
  get no manifest row even when public, so exported token-kind constants
  (`JR:T-OBJ` ..) never appear in `lib/std.manifest`.
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
  ```python``` in docs (`host-lint` `1 throw`s on any `.py` path).
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
  ran in NO automatic gate). `suite-coverage-lint.f` now derives all three lists each run and
  forces every member into scheduled / manual-documented / spawn-only-documented; wire a new
  lint into BOTH the cases suite + the scheduled lint-tools GSI fork (prove each path red with
  a transient drift). The standalone `-- lint-tools` slice and the gate's resident phase 17
  are DIFFERENT code paths.
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
  sweep of 129 open dots found 6 fully landed, 10 with stale premises, 3 TRUSTED.md rows owned by
  archived dots (`trusted-inventory --strict` red on DOT-EXISTS?, invisible because the gate runs
  FIXTURES not live strict). Verify a dot's claim against head; `dot off` only after
  `rg <id> TRUSTED.md` + reassigning rows; engine-suite standalone exits 0 after checker errors
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

- **SUBJECT:RUN forks the live test process, so call it with NO package open,
  and never gate a CLI file that parses argv.** A suite whose RUN executes
  inside its own package makes the forked child's `package X` a NESTED-package
  reject (exit 75) instead of the behavior under test (weight-store's seal probe
  expected SEAL-PACKAGE 84); close the package and call `PKG:RUN` from top level
  (the json-read-test arrangement).

- **A new file / TRUSTED word / candidate case each trips a specific manifest the
  focused suite never shows — only `test/run.f` does.** (1) A **flat `lib/*.f`**
  must have a `lib/std.manifest` module row (`stdlib-manifest-test`: "missing
  module row"); a **subdir** `lib/<sub>/*.f` is outside the flat-stdlib walk
  (`SMT-FLAT-LIB-FILE?` = exactly one `/`), so type-surface libs like extents and
  value nominals live in a subdir (`lib/type/…`) to avoid the word-row/doc/drift
  contract, matching maki/ precedent. (2) A new **`TRUSTED:`** word needs BOTH a
  `TRUSTED.md` markdown table row (`trust-lint`: "UNMANIFESTED … no TRUSTED.md
  row") AND a per-site line in the `<!-- trusted-inventory-classes -->` block
  (`trusted-inventory` ratchet) — mirror the nearest sibling's class/dot
  (`prim-axiom …`). (3) New **`test/candidate-validation.f`** cases must bump the
  whitebox counts in `test/candidate-validation-test.f` (`s" test/` total, and the
  `construct case-kind positive|negative` counts) and add PATH-PIN + DIRECT-PIN
  rows. Run `test/run.f` before claiming green; a clean focused suite hides all
  three.
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
  tier is not proof for suites documented as manual/slow members
  (`tools/suite-coverage-lint-core.f` SC-MANUAL-TABLE, run via `test/gate-stdlib.f`).
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
  window). Report cache-fill as budget coverage and persistent-cache as the architecture number;
  discovered content-cache misses switch to the scratch-cache budget class.
- **Cache Habu-under-test by CONTENT not path, keep the producer build-only, always validate.**
  Hash `bin/hb` + runner/build harness + every emitted engine/repl source; a hit unblocks
  under-test slices, a miss runs the fixpoint and installs under that key. A hit that also skips
  `GE-ENGINE-SUITE`/hook checks is wrong — candidate PRODUCTION is not VALIDATION; run a candidate
  validation row after the `under` capability is ready. Build the candidate in the early
  engine-build slot, publish atomically (path+SHA), then release downstream phases onto
  `HABU_UNDER_TEST` (pass it INTO the producer phase; make the drain fail once `GT-POOL-LIVE` is
  zero, not poll an empty pool). Cache stamps assert the INSTALLED artifact from CONSUMED inputs
  (record each stage-source digest at emit/consume via `BF-RECORD-STAGE`, assemble from those +
  the post-install engine hash; re-hashing the tree races mid-build edits). Gate retries need FRESH
  `XDG_CACHE_HOME` + `HB_TMP` per ATTEMPT (a reused cache replays a timeout-poisoned verdict as a
  false persistent red). A fresh gate root does not imply zero aggregate cache hits — the suite
  proves maker/artifact hit paths inside one attempt; retry isolation rejects INHERITED artifacts
  while preserving the within-attempt hit-counter contract.
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
  orphans every TRUSTED.md row + every `blocks:` edge naming it — in the SAME commit re-point rows
  to a live successor owner (prim-axioms to the axiom dot, program rows to the live epic's
  self-named file) and sweep `blocks:` lists (remove an emptied `blocks:` header too), then gate
  that exact tree with dot-dep-lint. trusted-inventory --strict resolves owners only at
  `.dots/<id>.md` or `.dots/<id>/<id>.md`; a child dot under another parent's dir is invisible as an
  owner. Never leave closures in the working copy across a merge window — `jj new <tip>` orphans
  them (archive copy persists, tracked open copy returns → Ambiguous ID); every `dot off` is
  immediately followed by dot-dep-lint + `jj commit`.
- **Use only documented `dot` subcommands — an unknown form is QUICK-ADD and creates a stray
  task.** `dot dep check`, `dot dep --help`, and even `<unknown> --help` all create dots; consult
  `dot --help`, inspect `jj diff` after every tracker command. The search verb is `dot find`, not
  `dot search`. `dot add -a TARGET` records the new dot as BLOCKING TARGET; the local CLI retained
  only the LAST repeated `-a` (dropping earlier prerequisites) — patch the frontmatter, run
  dot-dep-lint, verify the rendered tree. `-P <root-id>` only when `.dots/<root-id>/<root-id>.md`
  exists; never `-P`/`-a` a nested id. `dot on` re-quotes metadata each transition (never re-run on
  an active dot). Never interpolate Markdown backticks into a shell `dot add -d` — command
  substitution executes them and can erase a stack effect; pass text as data. Mark/close the exact
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
- **STATUS/trust/audit DATES follow the gate's UTC day, not the operator's local calendar.**
  `stale-status-lint`/`trust-lint` use native `DATE-NOW` UTC; rolling "Last verified" to a local
  date after midnight makes pushed master red until UTC catches up — `date -u +%F` before any date
  roll and pass that UTC day to manual invocations. Diff gates must scan LOCALS (`tools/typed-local-diff-lint.f`,
  not `rg`); run it against the exact integration diff (a squashed stack can hide an earlier untyped
  local), stream large patches (keep a fixture above the old 1024-line limit). Repo edits go through
  patches/Edit even for one-liners; commit is a gate (scan diffs for defs/unchecked boundaries,
  check exact owning `bin/hb --load` paths, boundary tests exist), never "commit now, fix later".

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
  only engine-text refs (fixed VAs keep dict/data valid); accept a trailer only when
  `region-len + data-len` ends exactly at the trailer offset (magic also appears in code); locate the
  trailer by scanning for the LAST `SNAP-MAGIC` (48-byte trailer is NOT at file-end — SNAP-EXTRA-SIZE
  pad + macOS codesign blob follow); an un-resigned patched image is SIGKILLed (rc -9). Snapshot DATA
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
  dlopen libcuda). Keep FFI ABI separate from loader binding (`lib/ffi-abi.f` portable + gateable;
  `lib/ffi.f` the loader layer). A mechanical guard on all 8 FFI arg registers is UNSOUND — ffi-call
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
  host runs f64, the device f32 — round the host elem onto the f32 grid first (`F64>F32 F32>F64`) then
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
  before shrinking N "different" cases). Stateful scanners split at cursor phases (`STALE-STATUS-LINT`s private `COUNT-LINE?`
  delegating advance/digit-run/ratio/keyword to typed helpers) with fixtures around the boundary. Report
  reducers use DEDICATED scratch cells (`RR-I/J/K` get clobbered by nested helpers; a `RR-RATIO.` stack
  leak truncated a table) — add row-count regressions and `cmp` regenerated reports. Doc-contract
  fixtures need stable anchors (line wrapping hides a `grep -F` phrase — assert a shorter contiguous
  substring). Dogfood benchmark hot paths (per-call glue is Habu-native; host parsers hide missing Habu
  primitives) and match LLM helper surfaces to validator surfaces exactly. Subtree status docs use lint
  FENCES not wording games (keep root self-check counts fenced to root `STATUS.md`, skip extracted
  subtrees in `stale-status-lint`).

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
  This exact pattern pushed a red master (maki spec-test + trusted-inventory
  both failing) because two red gates printed their tails and the push ran
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
- **One suite needs one canonical inventory.** Repeating membership across a full loader, slices, runner dispatch, and coverage lint creates more reconciliation code than the split saves and lets each copy drift independently.
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
- **Adding a `PRIM:`/`TRUST` site or a validation-suite case ripples into committed inventories.** A new prim bumps the prop-test axiom ledger count and its per-index `\ AXR` rows, a new TRUST site bumps `TRUSTED.md` rows and its per-file class ceiling, engine growth trips the exact-CODELEN ratchet, and a new candidate-validation case bumps its declared kind tally — each is a committed ratchet that fails loudly and must move in the same commit. Insert each axiom recipe at its exact live slot and shift every later slot; keep read-only zero-argument state readers executable, while state-consuming transaction finalizers need an explicit no-exec rationale.

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
- **Suite coverage currently proves path membership, not scheduler roles.** It
  catches an orphaned case member, but a different scheduling verb, group,
  duplicate, or order can still satisfy the set. Freeze those facts in the dot
  and prove the real production group; do not claim mutations the lint cannot
  observe.
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
  and resolve every surviving event after success and failure.
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
- **A property with no behavioural witness needs a source-level regression, and
  the regression has to be measured against the code it is meant to reject.**
  The declaration transaction's release phase runs only after every reversible
  commit has published, so no declaration a test can write reaches a release
  callback in a state where it would want to reject. Swapping a total release
  word for a validating one therefore passed every suite in the repository. The
  workable witness reads the production sources: find each participant
  registration, take the word in its release slot, close over everything those
  words reach through calls and deferred vectors, and require every reached word
  to be a definition in those sources or a member of a small allowlist of total
  words. An allowlist rather than a list of banned words is what makes it fail
  closed - a newly introduced helper is red because it is unrecognised, not
  because someone remembered to ban it. Prove such an inventory by running it
  against the parent tree first: this one reported ten reachable `throw` sites
  there and zero after the change.
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
- **A source-scanning gate has to fail closed on the shapes it does not
  understand, not skip them.** The release inventory followed deferred words only
  through `[: WORD ;] is VECTOR`. Three sibling holes came from the same habit:
  a root marked reachable instead of resolved (so a deferred root was never
  followed), zero bindings treated as nothing-to-do, and `['] WORD is VECTOR`
  dropped silently. Each was green on the real production sources under a mutation
  that should have been red. The rule that fixes all of them at once is: record
  every occurrence of the construct, resolve roots by the same path as interior
  references, and report an unrecognised or absent target rather than passing
  over it.
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
  TRUSTED.md owner rows and surfaced as a distant trusted-inventory red at
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
  suite-coverage's “exactly once” rule applies among slices, not across both
  levels.
- **Destructive cleanup requires a validated target.** An unsupported
  `jj diff --check` left a temporary-path variable empty, so unconditional
  `gio trash "$candidate_file"` trashed the current directory. Stop when target
  creation fails; cleanup only an explicit, nonempty path proven inside the
  intended temporary root.
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
  dropped manifest rows master had gained. Both verdicts were right about
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
