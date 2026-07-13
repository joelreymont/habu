# Lessons

# FIXME: Rewrite this to be concise without losing precision

Last updated: 2026-07-13

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
- **`-- snap` is certify-blocked; the runtime snapshot route works and is
  cheap:** BF-CERTIFY-SNAP rejects snap.f because VERIFY:SOURCE-BUF does not
  honor `0 set-check` (undefined SNAPGO from require'd snap-lib.f; regressed
  by "Make fixpoint source certification blocking", owned by
  habu-staged-fixpoint-src-0b5fc6e6). Tests needing a snapshot binary use the
  pipeline's own runtime route hb-stdin < hb-snap-src -> hb-snap0 (0.63s),
  then codesign — not the blocked `-- snap` CLI.
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

Concise findings only: what worked, what failed, why. Coding standards live in
`docs/forth.md`; API details in `docs/` near their feature. One tight bullet per
lesson — keep the specific word/code/path, cut the prose.

## Checker Soundness

- **Definer capacities are values, not syntax:** the original
  `LAYOUT-BUFFER NAME family 16` grammar forced every caller to duplicate its
  named capacity and add a drift assertion. `count LAYOUT-BUFFER NAME family`
  lets runtime validation consume the authoritative value while source
  preverification records only the count-independent accessor signature.
- **A raw-storage API needs a validating ADT decoder, not a typed-pointer cast:**
  maps intentionally expose caller-owned cells through `MAP-SLOT-FIELD`, so they
  cannot promise representation integrity. Removing the dedicated state pointer
  still matters: `MAP-SLOT-STATE@` now converts only raw 0/1/2 to constructors
  and throws `E-BAD-TAG` otherwise, while the public nominal getter/setter prevent
  accidental tag laundering in checked code.
- **Make exit-only seal tests mutation-sensitive:** target the sealed latch with
  a zero-valued ADT cell. If a wide-store loop omits a later guard or writes
  before loading the latch, that store opens the seal and the child exits 0
  instead of `E-SEAL-VIOLATION`; emit an armed marker immediately before the
  sink so an earlier compile/setup exit cannot satisfy the rc assertion. Pair
  this runtime probe with the lowering golden that pins STR after the full
  address-range guard.
- **A typed pointee proves identity, not storage capacity:** wide ADT `!`/`@`
  can refine `ptr a` to `ptr family` and lower from compile-time `W`, but a
  plain `variable` still owns one cell; callers allocate `W` cells until
  `LAYOUT-BUFFER` owns stride/bounds, W=0 rejects as non-addressable, and wide
  `!` must run the two-band seal guard per destination cell so a later slot
  cannot cross into protected memory after an unprotected base.
- **A qualified def's engine->checker record call must key off the QUALIFIED
  name, and read it from a STABLE buffer:** every sig'd `: PKG:tail (..)` recorded
  twice at `;` — the hook cert under the qualified body-buffer name (correct) AND
  `C-CALL-TRUST-PEND` under the BARE dict-record tail (`C-PUSH-DREC-NAME` read the
  published record whose name is the rewritten tail), leaking a bare-global `tail`
  effect row that shadowed core prims and certified bare-tail calls the engine
  rejects; `TRUSTED: PKG:tail` had ONLY that bare row (qualified lookup found
  nothing). Fix: `C-PUSH-DREC-NAME` pushes the body buffer's first token
  (BODYBUF-OFF) = byte-identical to the cert path's NMA, so CHECKER-RECORD-SYM
  splits the `:` into the PUBLIC (pkg,tail) sym. Two traps: DEF-TKA is a raw
  SOURCE pointer (stale across a multi-line body → hb-stage boot SIGSEGV), the
  body buffer is a fixed engine DATA region; and scratch must avoid x11 (holds the
  trust XT for the later C-CALL-X11-SAVED BLR). Gforth stage0 has no package
  system, so its record name IS the full name — no mirror change
  (habu-qualified-defs-leak-aadeb5c9).
- **Post-check scratch read by a later pass must be keyed by something the
  branch joins never pop:** TFAM 12's pass-2 width-aware emitter reads checker
  locals widths AFTER the hook certifies, but the live table (`LOCW`) is popped
  at joins (`CF-LOC-REST`) and frame slots are reused by sibling arms — live-
  index reads died 76 or took a sibling's width while the verdict stayed -1.
  Landed fix (habu-tfam-12-pass): key the durable table by a monotone bind
  sequence (`LOCW-HW[LOCSEQ]`, assigned in `LOC-ADD`, finalized via
  `LOCSEQIX[live]` at the `:}` bind, reset only in `CHECK-RESET`), and let the
  pass-2 carve REPLAY binds in textual order (`P2-CARVE-W` consumes one seq per
  group local, rebuilding a live table `P2LW` the slot math reads). Replay
  alignment holds because `LOC-BEGIN` rejects `{:` in dead code and quotations;
  `LOCW-HW@` dies 76 on any skew. Host such pass-2 scratch in checker.f, not
  new engine DATA cells — the fixed DATA map is full ($3A00..$3C88 is the FFI
  block) and the checker already owns the facts.
- **Diagnosing with the row renderer beats guessing:** the branch-local exec row
  first rejected not from the branch at all — a RAW generated-constructor call
  + immediate local bind (`7 TLP--RES:ERR {: r :}`) rejects identically at top
  level (ctor output is still in the CTOR-PEND signature-boundary window). Test
  seeds must be checked maker words; reduce with `CHECK-QUIET-CANDIDATE!`
  probes before blaming the new mechanism.
- **A new static gate's blast radius includes the test corpus's own
  metaprogramming:** checker-computed DNAME-WIDE marking made the interpret
  tick gate fire on the layout goldens' `' TLP-DUP` — correct enforcement, not
  a bug. Test-only xt introspection must live in the raw-xt TRUSTED boundary
  (search-wl), not interpret `'`, once ticking is part of the gate surface.
  When a checker fact needs an engine-side record flag, prefer a checker-owned
  latch stored BY VALUE at the single record choke point (E-ADD-EFFECT) and
  consumed by the publish tails after ndict++ — the existing `wide-mark`
  newest-record prim then needs no pend variant, and staleness is impossible
  because every consuming publish runs its own record flow last.
- **Staged fixtures can encode an unsound expectation — re-derive from physical
  truth before wiring them in:** the staged constant shape-carry fixtures
  (PST-CONST-K-CARRY$ `CAE-CV-K | -- cae-cv`) expected a 2-field record effect
  from a ONE-cell bake — certifying USE words that push fewer cells than
  declared. A definer's recordable effect is bounded by what its runtime
  actually stores (C-CONSTANT pops one cell), and the untyped interpret stack
  has no sound shape source (adjacent-producer inference mis-carries on
  `MK 5 constant K`). Verdict: `-- a` is the permanent constant contract; wide
  values are gated at interpret DISPATCH (DNAME-WIDE), which dominates every
  downstream value-pop consumer — gate production, not each definer.

- **Layout transport is a per-token mode, not a per-var flag:** generic stack
  prims share polymorphic effect vars (`dup` and `0=` both use `PE-A`), so you
  cannot mark a var "layout-transportable". Item 12 sets `LAYOUT-XPORT` in
  `DO-TOK1`/`LOC-BIND` only for the whole-bundle ops (dup/drop/swap/over/nip/
  rot/-rot/tuck/2dup/2drop/2swap/2over, >r/r>/r@/2>r/2r>/2r@, locals) and lets
  `U-TYPE`'s `LAYOUT-BLOCK?` allow a var↔layout-param bind only in that mode;
  every other touch (inspecting prims, control preds, execute/catch, con/ptr/
  atom) still fails closed exactly as item 7. Accepting is sound because a
  layout value is still ONE physical cell (item 7 kept it one `T-PARAM` cell, no
  `LAYOUT-PUSH-FIELDS` yet, no published constructors → wider values aren't even
  constructible). The mode MUST be reset (`0 LAYOUT-XPORT !`) after `CHECK-SCAN`
  before boundary `SUNI-COERCE`, else a generic output var wrongly absorbs a
  layout when the last body token happened to be a transport op.
- **`?dup` was UNCK, not reject:** `?dup` is unmodeled (not a `PRIM:`, not in
  `CF-TOK?`/`RS-TOK?`) so it falls to the undefined path and marks `UNCK=1` —
  any checked word using it is "uncheckable", an escape hatch. Item 12 added
  `QDUP-STEP?` to REJECT `?dup` on a layout value (it branches on the tag cell);
  the scalar union effect (`x -- x x | x`) is still unmodeled and dotted
  (`habu-model-dup-checked`).
- **A layout param hides its payload from the linear count:** the linear
  discipline counts concrete linear CONS on rows, so `tdlin<own>` (one opaque
  `T-PARAM` cell) dup/dropped freely while bare `own drop` rejected — the
  destruction review caught it. Transport binds now reject any layout whose
  family args resolve linear OR are still unbound vars (may later bind linear),
  fail-closed until TFAM 11 counts whole bundles; identity flow stays legal.
- **IBUFSZ is another src/core growth watermark, and it exits SILENTLY:** the
  engine's boot source-prefix copy loops (habu2 `SRC-SFAIL`/`SRC-BFAIL`) exit a
  bare `74` with no message when the concatenated prefix reaches `IBUFSZ`
  (src/habu/layout.f, Gforth mirror bootstrap/cg/forth.fs). Growing checker.f
  ~2.4KB (item 12 slice-3a) made `install --force` fail as an opaque
  `E-BUILD-STATUS: refresh child failed`; the leftover `hb-stdin-mk` run by
  hand reproduced the silent 74. Bumped 1M->1.5M in both mirrors; silent
  capacity exits are dotted to print their own name before dying. The
  definer capacity exits are now labeled (`hb: dictionary full at: <token>`
  77 / `hb: code space full at: <token>` 76, habu-gate-runner-entry-81c84af0)
  — a capacity wall that prints only the current token is unattributable (the
  dict-full one surfaced as a lone ':' byte). DICT-CAP is a growth watermark
  too: the gate-runner-support closure crossed 8192 records; cap now 16384
  with CFSTK-OFF/DICT-SIZE/HIDX-SLOTS/HIDX-BYTES scaled in step and the
  LFIND probe masks derived from HIDX-SLOTS instead of hardcoded.
- **A copied `bin/hb` is not a frozen baseline:** the small engine LOADS
  `src/core/*.f` from the working tree at boot (`hb: cannot open src/core/util.f`
  when run outside the repo), so an old binary run in an edited tree exhibits
  the EDITED checker — red/green comparisons must pin the source tree state,
  not the binary. Chasing "engine words not in any tracked source" wasted a
  session hour before one out-of-tree run exposed the boot-time load.
- **Growable registries that own a string pool must rebase on relocation:** CT/
  VREC/SYMS records hold ABSOLUTE pointers into their `*-STR` pool; growing the
  pool (mmap relocate) or persisting it to fresh DATA dangles them. `*-STR-REBASE`
  adds the move delta to every stored name pointer (VREC also to VR-ATOM/VR-PARAM
  node `VN.A`). Grow the RECORD/NODE arrays to mmap BEFORE the string pool so a
  rebase never mutates the pristine baked boot buffer. SYMS additionally must stay
  a power of 2 (HIDX mask) and drop+rebuild the HIDX index on grow (that IS the
  rehash). Reserve `pkgu+nameu` string bytes up front in SYM-INTERN so no
  mid-record grow dangles a just-written PKG-A.
- **`create ... allot` checker arenas convert to boot+P without touching callers:**
  replace `create X ...` with `create X-BOOT ...` + `variable X-P` + `: X ( -- ptr
  a ) X-P @ ;`; every `cells X +` site is unchanged. Live caps become a `variable
  *-CAP-V` + `: *-CAP *-CAP-V @ ;`; the `create`s use the `*-INIT` constant.
- **The image build buffer MSIZE must be >= MPAGE:** `src/os/image-bytes.f MSIZE`
  ($90000) bounds MBUF, which holds the whole `__text` (up to TEXTSZ). The macho
  code-limit check allows up to `MPAGE` ($100000), so an engine with $8F000..$FF000
  of code passes the 73-check but overflows MBUF with M-BOUNDS `75` (maker builds
  spawn with fd -1, so the error is silent — run the leftover stage2-src by hand
  to see it). Bumping core code past ~585KB tripped this; MSIZE raised to $120000.
  This is the "maker __text wall" the watermark lesson warns about.
- **The lint tokenizer TMAX is a src/core growth watermark:** `tools/lint/token.f
  TMAX` ($6000) caps CODE tokens (comments/`( )` stripped) per linted file;
  checker.f is the largest. Growing it throws E-LINT-TOKEN-CAP `77` from
  shadow-lint. DRY helped once; then bump TMAX ($6000->$8000).
- **A value-less unification trail undoes FRESH binds only:** TV!/RV! record just
  `(var-id, is-row)` and TRIAL-REST unwinds to UNBOUND — correct because U-TYPE/
  U-ROW only bind post-resolution (UNBOUND) vars. It cannot undo a RE-bind
  (compression overwriting an existing binding), so path compression must run only
  at trial depth 0 (permanent, no undo needed) OR the trail must record old
  values. The trail needs one invariant the wholesale memcpy did not: no NEW
  (TRAIL-RESET) between TRIAL-SAVE/REST — verified because EFF-APPLY only does
  FRESH + trailed binds, never reaching NEW/E-PARSE-ADD.
- **Native MULTI-ERR load != the check-all-errors re-driver:** both emit the same
  rich JSON (same DIAGXT), but the native load's line/byte are DEF-BUFFER-relative
  (2nd def reports line 1) while the goldens pin FILE-relative positions
  (`DIAGL0/DIAGB0` set per-def by the re-driver via `VERIFY:SOURCE-BUF-AT`), and
  the native mode suppresses cross-def cascades (trusts the declared sig) where the
  re-driver reports them. Rewiring onto the native mode needs a new capability
  (thread the compiler's per-def file position into DIAG-ORIGIN! during load) plus
  a cascade-policy decision — it is not a drop-in swap.

- **Cache-rewind detectors belong at the append choke point, not only reads:**
  the checker symbol-index epoch cache first compared arena watermarks at read
  time; candidate rollback followed by the next definition's recurse-cache
  record regrew `UEND` past the watermark and masked the rewind, so a stale
  cached effect offset resolved into overwritten bytes. `E-REC-START` and
  `NORET-ADD` now sync before appending — regrowth itself flushes the cache
  before truncated offsets are reused.
- **Cached arena offsets need a +1 encoding:** offset 0 is a legal record
  position after `USIGS-RESET`, so a 0-means-none cache silently dropped the
  first record; the engine-suite grow fixture caught it.
- **`variable`/`create` definitions publish checker records:** a capacity
  fixture that shrinks `USIGS-CAP-U` and then defines a `variable` before
  sampling has already forced the grow it meant to measure. Define fixture
  cells before mutating capacity state.
- **Engine-suite must not assume a runtime-sized checker store:** a restored
  snapshot boots with the persisted (smaller, DATA-backed) `USIGS`; reset
  fixtures normalize with one `USIGS-RESET` first so the suite passes under
  both `bin/hb` and a warm snapshot engine.
- **Giant words hide effects even with correct signatures:** a survey renderer
  combined input scanning, row validation, aggregate mutation, and JSON emission
  in one body. Split multi-pass work into cursor/pass/row/render helpers first;
  line comments then document only the non-obvious transitions that remain.
- **Core checker helpers share the loaded image:** `CK-TRUE` looked local in
  `src/core/checker.f` but collided with `lib/content-key.f` during the full
  suite. Use feature-owned helper prefixes and prove against the full image, not
  only isolated checker probes.
- **Keep `roles.f` TRUST site rows stable:** inserting `DEFLINEAR` above the
  trusted role casts shifted every manifest site. Put new definers below the
  audited cast block unless the manifest is deliberately re-audited.
- **Typed FFI bindings need a resolver word, not a global handle:** `FFI:` can
  generate checked wrappers while CUDA/libc/tasking each keep their own
  `DLSYM` policy. The wrapper should cache the resolved symbol, throw on zero,
  erase roles only at `FFI-ARG!`, and refine the returned cell to `rc`/nominal.
- **Destructure value records before typed locals:** `ptxir-node` fields are
  hidden `field<...>` tokens, so `{: op:n :}` rejects until `PTXIR-NODE>` turns
  the record back into raw field types.
- **Multi-line words need useful line effects:** value-record checker work
  exposed helpers whose stack motion had to be reconstructed. Put the word
  effect on the definition line; add body-line effects only where they improve
  review. Empty/no-op line comments are noise, so factor instead.
- **Parser-word table rows must execute at top level:** `PRIM:` originally sat
  inside `PT-*` colon words, so it would compile instead of consuming the next
  source token; literal names such as `s"` then broke compilation. Primitive
  effect declarations now run top-level: `PRIM:` consumes the name token
  immediately and the `PE-IN`/`PE-OUT` constructors build the stored effect.
- **`tools/check.f` must pre-register source-local `deftype`:** preverify parses
  signatures before the child executes source, so valid nominal declarations
  looked unknown and bad declarations died as raw prose. The tool now scans
  `deftype` first inside a checker scope, registers valid names, and emits JSON
  for invalid names.
- **Symbol interners must consume miss sentinels:** `SYM-INTERN` initially left
  the `0` id returned by a failed `SYM-FIND` under the new id, corrupting the
  first `TRUST` during stage-source loading. For `( id bool )` finders, every
  miss path must `drop` the sentinel before publishing a new record.
- **Checker metadata loops need owned indexes:** the integer-type table crash was
  `CT-FIND` using `over` to recover the loop code after pushing search strings;
  it passed a pointer as the type code and `CT-NAME$` fetched through garbage.
  Store table scan indexes in named cells or locals before pushing payload data.
- **Cell `@` must reject byte spans:** `CHECK!` accepted `ptr u8 @` because the
  primitive model treated `@` as unconstrained `ptr a -- a`. The fix belongs in
  checker primitive semantics: concrete `ptr u8` uses `c@`/`c!`; pointer-valued
  cells are modeled as `ptr ptr u8` and read through `ptr-field @`.
- **Package reopen is scope, include is composition:** reopening `package NAME`
  resumes the same private/public wordlists and duplicate set; `--load`,
  source-list, or include still owns file dependency order. Do not include a
  file merely to share the package namespace.
- **Optional dependency loaders use `required`, not `included`:** `tools/check-all-errors-core.f`
  re-included `src/habu/verify-source.f` after `tools/build-fixpoint.f` had
  required it, so resident engine workers hit duplicate `SOURCE-A`. When a tool
  conditionally loads shared support, keep the XREF guard if useful but use the
  require registry for the actual load.
- **Package public tails are the API:** once a module has `package TASK`, the
  public spelling should be `TASK:KILL`/`TASK:DONE?`, not global-style
  `TASK-KILL`/`TASK-DONE?`. Keep implementation helpers private and put the
  caller-facing surface in the `public` section.
- **Test fixture state belongs in a package:** focused framework tests should use
  a private package (`package FEATURE-TEST`) with short private names instead of
  global stems such as `TST-*`; only the public framework API is qualified.
- **Self-calls use `RECURSE`:** naming the word being defined inside its own body
  can compile the wrong target and crash at runtime. Recursive helpers must use
  `RECURSE`, then keep the checker regression that exercises the recursive path.
- **Checker source expansion keeps lint ownership:** `tools/check.f` flattens
  `require`/`required` dependencies into a temp source for checker execution, but
  trust/signature/boundary/reserved-name lints must still scan the original input
  path so manifest sites stay stable.
- **Core byte helpers are not string-library setup:** `lib/ffi-abi.f` used
  `BYTE-COPY`, so `include lib/ffi.f` failed unless `lib/string.f` happened to
  be loaded first. Small primitive helpers used across unrelated libraries
  belong in a narrow `src/core/*.f` prelude file, and test entry files should
  include their own setup instead of making suites encode broad library order.
- **Scanner opener predicates should read source bytes:** `create`d token tables
  are `ptr a`, not `ptr u8`; use checked byte helpers for token-shape tests
  instead of passing dictionary data to source-string APIs.
- **Runtime data slots must not overlap evaluate frames:** `EVAL-FRAME` spans
  `$3800..$39FF`; putting task or FFI scratch cells inside that range corrupts
  nested `include`/`evaluate` and makes definers fail as if tasks were live.
  Keep per-runtime scratch after the full frame area and before `DATA-START`.
- **Module entry files own dependency setup:** `include lib/task.f` must work
  without the caller knowing `errors`/`memory`/`ffi` order. Test/tool entries
  use `require` for setup, and suite declarations list only entry files plus
  script args. Shared warm setup must also use `required`, and snapshots must
  preserve the require registry so baked support is not reloaded at runtime.
- **Tool entries must not borrow suite prelude:** `tools/dot-dep-lint.f` passed
  inside the stdlib suite but failed standalone at `CLEANUP-RESET` because the
  suite had already loaded `lib/fs-mutate.f`. Every tool entry must require the
  exact files needed by its own top-level path.
- **Test runners must not keep loop state across test execution:** an included
  maki fixture using caught throws (`TTHROWS`) truncated the remaining suite when
  `lib/test/suite.f RUN-BODY` used `?do`; explicit index/count cells keep suite
  iteration outside the return-stack state that tests may legitimately unwind.
- **Run comments are not dependency setup:** `lib/render-test.f` and
  `lib/report-test.f` had accurate command comments but failed when loaded
  directly. Test and library entry files must `require` their own dependencies.
- **Stdlib leaves hid missing requires for months; bare-load them to prove it:**
  `lib/float.f` consumed `lib/string.f` words and `lib/fmt.f` consumed both
  (its header even *documented* the deps) with no `require` lines — masked
  everywhere by gate load order, surfacing only as a consumer "workaround"
  require (`maki/ablate-fusion-test.f`) whose comment mis-attributed the dep.
  The proof and the regression are the same command: a bare
  `bin/hb --load lib/<mod>.f` + one word call must load green standalone; fix
  the module that *consumes* the words, then delete the consumer workaround.
- **Baked prefix files must be marked `provided`:** `bin/hb` loads core prefix
  files before user source, but the `require` registry was empty, so
  `require src/core/sha256.f` reloaded `W32` and hit duplicate definition.
  The source-prefix builder now appends `provided` rows for every baked prefix
  path before user/test source runs.
- **A new baked core file has two prefix owners, not one.** Native
  `habu2.f`/fixpoint registration does not update the Gforth recovery compiler;
  mirror its load, path, provide, and label rows in `bootstrap/cg/forth.fs`, and
  concatenate it in `tools/bootstrap.sh`, or recovery-only consumers see an
  undefined public word despite a green native prefix.
- **Tasked engines need process-wide fatal exits:** Linux `exit(93)` terminates
  only the calling pthread, so checker/die paths can leave workers alive and
  make process captures time out. Native and bootstrap emitters must use
  `NR-EXIT-GROUP` for process termination; `pthread_exit` remains the task-local
  stop boundary.
- **Task facilities need owner state above pthreads:** SwiftForth-style
  facilities make same-owner `GET` and non-owner `RELEASE` no-ops; a raw
  pthread mutex exposes deadlock/error behavior instead. Track an owner token in
  the facility record and keep pthreads as the private blocking primitive.
- **Warm-image tails must not reload prefix-owned layout:** target layout files
  are part of the engine prefix. Snapshot tail tools append image emitters
  (`elf.f`/`macho.f`) only; re-appending `src/os/*/layout.f` redefines image
  constants and breaks warm-image baking.
- **Duplicate package definitions belong at publish time:** package namespaces
  concentrate many natural names, so same-wordlist redefinition must fail in
  both `C-QUALIFY-DEF` and the certified checker signature recorder. Explicit
  `TRUST` remains the audited override/refinement path; normal definitions must
  not silently replace earlier package public/private rows.
- **Parallel implementations need namespaced public words:** `lib/ptx/ad.f` and
  `lib/ptx/ad-dag.f` both used `AD-EMIT-REV`; duplicate-definition rejection
  correctly stopped the combined PTX suite. Keep experimental and replacement
  implementations behind distinct public names or packages until one retires.
- **Local-first compiler dispatch still owns checker capture:** moving local
  references ahead of keyword dispatch means `C-LOCAL-REF` must call `LBCAP`
  after `LLOC-FIND`; otherwise runtime emits the local but the hook checks a
  different body.
- **Namespace qualification is only a non-edge colon:** `HB:COUNT` qualifies
  through a wordlist; `GE-FILES:` stays an ordinary Forth word. Keep `LFIND`,
  definition storage, xref, and docs on that same rule.
- **Forth language rules live in `docs/forth.md`:** checker/type/style guidance
  was moved out of this file so there is one standards source for stack comments,
  `TRUST`, roles, locals, checked DSLs, booleans, pointer fields, quotations,
  `catch`, and phase tokens.
- **Checker model cutovers must rebind the hook:** reloading
  `src/core/checker.f` and `src/core/render.f` without reloading
  `src/core/check-hook.f` left `HOOK` calling the old `CHECK!`; the generated
  bootstrap/fixpoint source has to refresh all three together.
- **Old seeds may lack envp capture:** the Linux seed could read script arguments
  but not `HB_TMP` through `GETENV`, so stage2/fixpoint now passes the temp root
  explicitly after `--` and keeps generated paths under the build driver.
- **`ptr-field` is cell-indexed:** Linux stage2 crashed in `ARGV$` because
  `src/os/*/env.f` used `ptr-field` for DATA byte offsets and pre-multiplied
  argv/envp indexes. Header reads are audited byte-offset boundaries; argv/envp
  entries use raw cell indexes.
- **Do not publish facts with empty trusted stubs:** adding
  `HB-TARGET-LINUX?`/`HB-TARGET-MACOS?` to `src/core/roles.f` as empty
  `TRUSTED:` words made `EM-ENTRY-ARGS` branch on stale stack data during
  stage2 emission. Target predicates live in `src/os/<target>/target.f`; only
  nominal identity casts belong in `roles.f`.
- **Typed storage bugs cluster around raw state:** the Linux tree-shaker/env work
  exposed pointer and boolean state cells that looked like plain `n` cells. The
  durable rule belongs in `docs/forth.md`; the lesson is that removing unchecked
  file scopes quickly finds these hidden contracts.
- **Try a checked factor before new trust:** `FS-BYTE-OFFSET` looked like it
  needed a primitive, but `: BYTE+ ( ptr u8 n -- ptr u8 ) + ;` certified against
  existing pointer arithmetic and let the trust row disappear.
- **Removing a `TRUST` site also removes its inventory class row:** typing
  `src/habu/repl.f`'s `DATAB` and deleting the `0 set-check` REPL append wrapper
  left a stale `trusted-inventory-classes` row, so the full suite failed only in
  `native stdlib lint tools`. Run `tools/trusted-inventory-test.f` whenever a
  trusted boundary disappears.
- **Split generated unchecked spans by evidence:** `build-fixpoint` could keep
  `rt.f`, `crash.f`, `image-bytes.f`, and `regalloc.f` checked; only target
  image/sign, `habu1/prof`, and `jit/habu2` still need the raw emitter boundary.
- **Model phase tokens as checked cells:** `asm`/`img`/`snap` cannot be empty
  trusted ghosts when their defining files are checked; push a nominal cell at
  the producer and erase/preserve it at the next phase boundary.
- **Function-passing is a checked capability — don't default to unchecked:** I
  almost copied `combinators.f`'s `0 set-check` boundary for a generic comparator
  `SORT!`. Empirically the checker verifies a quotation parameter executed through
  a call chain AND a `?do`/`begin` loop (E1–E5 probes), so `lib/sort.f` `SORT!`
  ( `ptr a n [ a a -- bool ] --` ) is fully checked. `combinators.f` MAP/FOLD/EACH
  are an unchecked boundary that predates this and could likely be checked too —
  it is not a model to copy. The reusable rule is in `docs/forth.md`.
- **Deferred execution must carry its effect:** raw xt cells (`variable`/`@ execute`
  or `['] ... is`) let the stored implementation escape checker knowledge.
  `defer NAME ( effect )` plus `[: IMPL ;] is NAME` keeps assignment checked, and
  every source pre-verifier (`hb-build`, all-errors reducers) must learn new
  top-level definers instead of treating them as ordinary tokens.
- **Eval grading is not load checking:** `CHECK!` is the persistent load hook and
  must keep certified signatures for later definitions. Model-candidate grading
  uses `CHECK-CANDIDATE!`, which suppresses duplicate-name rejection only for that
  candidate and restores `USIGS`/`NORETS` afterward.
- **Runtime definers must publish checker facts:** a parent preverify pass cannot
  help the child checker registry. `create`/`variable`/`constant` must publish
  their `TRUST` effects in the native compiler when the hook is installed.
- **Parser-word payloads are part of the checked body:** `[char]`/`char` consume a
  source token before codegen, so the compiler body capture must append that token
  and the checker must model the parser word as one literal plus one skipped token.
- **Keep include as a two-leaf boundary:** source composition needs checked
  path/read/depth code plus only the mmap pointer refinement and final `evaluate`
  crossing as `TRUSTED:` leaves. Do not grow include to hide source-order bugs.
- **Malformed control is not uncheckable:** orphan closers and unterminated
  `if`/loop/quotation/`case` frames must set `OK=0`; `UNCK` is for missing
  checker model coverage, not syntax imbalance.
- **Do not port relative linked-list words by habit:** SwiftForth's
  `@REL`/`!REL`/`,REL`/link traversal words bake dictionary-relative pointer
  arithmetic into APIs. Habu should model node layout with structures, runtime
  collections with arrays/maps, and dispatch with checked `case` or execution
  vectors.
- **Early defining words need post-checker effects:** moving structures before
  `checker.f` let checker records use the structure DSL, but `CREATE ... DOES>`
  cannot publish effects until `trust` exists. Load definitions early, publish
  their checker rows from `src/core/structures-effects.f`, and make `DOES>`
  effect publication skip only when no checker hook is installed.
- **Checker-to-render features need hooks, not load-order shortcuts:** `checker.f`
  loads before `render.f`, so `{: x:? :}` records show-inferred locals in checker
  state and calls `LOCSHOWXT`; `render.f` installs the printer after its type
  renderer exists.
- **Generated candidates must honor package exports:** after `lib/ptx/collective.f`
  moved `B-`/`B/` behind `package PTX`, maki softmax grader strings that still
  used bare `B-`/`B/` certified as rejected. External generated code must call
  exported package words as `PTX:B-`/`PTX:B/`; reopened-package bare names are only
  for code loaded inside that package.

## Tool & Infra

- **Evaluator depth must exceed legal include depth:** `LAYOUT-BUFFER` evaluates
  one generated accessor while loading, so an included test on a legal dependency
  chain reached `EVALD=8` and hit the native depth `BRK` although its focused run
  passed. Reserve frames above task-user cells, derive snapshot clearing from
  `EVAL-MAX-DEPTH`, and pin `EVAL-MAX-DEPTH > INCLUDE-MAX-DEPTH`.
- **Keep lint classifier fixtures outside the linted implementation:** an inline
  `s" LAYOUT-BUFFER DUP ..."` self-check in `shadow-lint.f` was later tokenized
  as source by the live lint and reported its fixture name as a real primitive
  shadow. Put classifier probes in the owning test entry so production scans
  cannot reinterpret test strings as source definitions.
- **Exit-status mapping must honor deliberate small codes, not flatten them:**
  fixing BTHROW's masked no-handler exit (`-2816 throw` exited 0 SILENTLY,
  `-2802` an aliased 14 — fail-open for any tool reading the rc) with an
  always-fixed rc broke the ecosystem's deliberate throw-as-exit contracts:
  lib/argv.f usage 64, the check hook's documented "bad definition exits 70",
  lint findings `1 throw` — all asserted by tests with empty/exact stderr, and
  all REQUIRED to stay throws for in-process catchers (`CHECK-CANDIDATE!`,
  `PARSE-RC`). The sound mapping: [1,255] exits byte-identically (those codes
  were never masked); anything else prints `hb: uncaught throw code N` on fd 2
  and exits UNCAUGHT-RC 67. The same [0,255]-else-67 rule in the `die`
  primitive closed the DRV-FAIL second masked layer without touching any
  driver (driver-io.f loads in OBJIMG contexts without layout.f, so the
  primitive — not the library — owns the mapping). Closed by
  habu-engine-bthrow-no-02c6b017 (reporter baked at the EM-EVAL-THROW-RECOVER
  tail, reached from BTHROW via UNCGH-CELL).
- **Dictionary records are read-only at runtime — flag writes are engine
  prims:** an xref.f Forth-level `!` into a record's name cell SIGBUSed (the
  dict lives in the RX region); the `immediate` keyword's shape is the model —
  an engine prim wrapping the store in the LPROT RW/RX mprotect bracket
  (habu1.f BWIDEMARK for DNAME-WIDE). Forth-side dict mutation is truncation
  only (`ndict!`/`cp!`); anything touching record bytes needs a prim.
- **A seal watermark must be captured where the SOURCE ends, not where a file
  ends:** SEAL-CAPTURE at the tail of xref.f froze the truncation watermark
  below src/os/script-argv.f (loaded after it), leaving the argv tail
  FORGET/HIDEable rc 0. The boot prefix is CONCATENATED before anything
  evaluates (ndict is still the prim boundary at assembly time), so the fix is
  a second SEAL-CAPTURE token appended by the cold-prefix assembler after ALL
  engine files + provide rows (capture is monotonic — re-running only raises
  the watermark; snap-gated so snapshots keep their bake-time value). Guards
  keyed to "end of file X" rot the moment a file loads after X
  (habu-tfam-2b-iii-5d25b52f).
- **Don't guard a prim whose legitimate caller is indistinguishable from the
  attacker:** a raw `ndict!` watermark guard needs a principal, but the engine
  refresh runs as ordinary post-seal `--load` source and legitimately
  truncates below the watermark (hide.f BFR-NDICT!); any exemption reachable
  by that source is reachable by an attacker's. The sound sequencing is to
  eliminate the legitimate below-watermark caller first (staged-fixpoint
  fresh-process refresh), then add the guard — meanwhile the actual spoof is
  closed one layer up (sealed USIG registry → redefine exits 78).
- **Interpret-mode is the untyped surface — gate wide producers, don't type
  the REPL:** layout bundles silently corrupted under top-level dup/drop/swap
  (one physical cell moved; a TRUSTED `( -- pp<n,n> )` maker exposed it). The
  sound v1 is a DNAME-WIDE dict bit tested at interpret dispatch/tick (fail
  closed, named diagnostic, LUNDEF recovery shape) rather than dynamic width
  tagging of the interpret stack, which would re-implement the checker at
  runtime. Checked compile-mode calls of the same marked word stay legal —
  that discrimination is the regression's guard leg
  (habu-tfam-12-interpret-10b385b1; checker-side record-time marking
  sequenced). Growing the engine also crossed the stage2/maker source caps
  ($C0000, 33 bytes over — another loud growth watermark, bumped in step).
- **layout.f is not the sole owner of fixed DATA cells:** a "free hole" read
  from layout.f alone ($36B0, between INE-CELL and FRCLM-CELL) was actually
  FRFREE-CELL — the JIT float-pool bitmask defined in src/habu/regalloc.f —
  and storing the uncaught-throw reporter address there made the engine jump
  to PC=0xff (lldb caught it in one run). Before claiming a DATA slot, `rg`
  the ADDRESS across src/ lib/ bootstrap/ (regalloc.f, debug.f, and lib/task.f
  all define cells outside layout.f); the one proven-safe home was $3D00 with
  lib/task.f TASK-USER-BASE bumped to $3D08.
- **Mid-compile bridge calls into checker words need an RX window:** the compile
  loop holds the code REGION RW between colon start and the `;` flush, and the
  checker words the C-FIND-GLOBAL bridge targets are COMPILED INTO that region —
  a BLR from the loop fetches a writable page and SIGBUSes under W^X (crash
  signature: pc == x11, SIGBUS, mid-body). Bracket the find+call with
  `2 5 MOVZ, LPROT` / `2 3 MOVZ, LPROT` (EM-P2-CARVE-W's "caller holds the RX
  window" is the documented precedent — the P2 calls run at `;` post-flush or
  under a caller window; a NEW mid-body call site must open its own). DATA
  stores are legal in either state; die legs exit, so their prot state is
  irrelevant; anything that may EMIT (LVPUSHC can spill) must be back in RW.
- **The compile loop's central body capture is the LBCAP at
  EM-COMPILE-KEYWORDS' head:** locals capture in C-LOCAL-REF, `;` is never
  captured (EM-COMPILE-SEMI runs earlier), and everything else (keywords,
  literals, ops, calls, undefined) is captured by that one keyword-head LBCAP.
  Any dispatch inserted UPSTREAM that consumes tokens (TFAM 10's ADT mode) must
  LBCAP them itself or the checker's body is silently truncated — construct
  first rejected with MD-CON-TRUNC because the checked body ended at
  `construct` while the engine had already consumed the operand tokens.
- **A documented DATA "free hole" can be a live table's interior:** layout.f
  called $260 (old DEF-WL slot) free, but `VVAL-OFF` ($250) + `VSMAX` cells is
  the JIT virtual-stack value table $250..$350 — $260 is VVAL[2]; DEF-TKA/
  DEF-TKL coexist at $250/$258 only because their liveness is confined to the
  definition NAME token, when the VS is empty. Exact-constant rg is not
  enough: also check every RANGED region (table base + capacity) covering the
  candidate. A mode cell at $260 read VS garbage; its own fail-closed guard
  caught it at the first stage build (TFAM 10 CMM-CELL landed at $27A8, the
  last old CRSIG slot). The other old holes ($2780..$27A0, $27C0..$27E8) were
  meanwhile reclaimed by habu1.f P2-* — free-hole comments rot, so correct
  them whenever a slot is taken or found stale.
- **`wait-rc` is WEXITSTATUS-only:** a signal-killed child (SIGABRT = rc 134)
  reports rc 0 through the raw primitive. All in-repo users are migrated
  (bench.f -> PROC-WAIT-RC; the dead PROC-WAIT-RAW alias retired from
  lib/process.f + manifest + docs) with an end-to-end 128+sig regression
  (TEST-PROC-WAIT-RC-SIGNAL). The primitive itself (habu1.f BWAITRC + its
  checker PES row) still exists engine-side; its removal is the routed
  remainder of habu-wait-rc-masks-9ae37cd0.
- **Path materialization must share one fail-closed emitter:** `s" <path>"`
  loader/prefix lines were hand-rolled with zero escaping in `lib/source.f`
  (`SOURCE-APPEND-PROVIDED`) and `tools/check-core.f` (`CHK-APPEND-REQUIRED`,
  `CHK-BUILD-PREFIX` DIAG-FILE! label). A `"`/newline in a path silently broke
  source structure. One `SOURCE-APPEND-QPATH` (validates via `SOURCE-PATH-SAFE?`,
  throws `E-FS-PATH-UNSAFE` on `"`/`\`/LF/CR) now owns all three sites; output is
  byte-identical for safe paths. Reject fail-closed, do not quote-escape.
- **`required`/`included`/`provided` take stack strings only:** the result-cache
  closure lint (`test/run-result-cache-test.f`) looked like it "missed" them, but
  those words only appear as `s" path" required` — already covered by
  `LINT-SQUOTES` scanning every `s" X.f"`. Only `require`/`include` are the
  parse-name immediate forms (`LINT-REQUIRE`). No standalone gap there; real
  event-log cross-check is the item-5 rewrite.
- **reserved-name-lint runs only over user source, never core:** it fires only
  inside `tools/check.f` (`CHK-RUN-RESERVED-NAMES`) on the checked source, and the
  repo lint slice runs clobber/shadow/host/filemap/trust — not reserved-name over
  `src/`. So reserving loader words (`include`/`included`/`require`/`required`/
  `provided`) is gate-safe even though `src/core/include.f` defines them; core is
  self-checked by the compiler hook, a different path.

- **One image-writing tail; the engine cannot route through OBJLINK:** the ~585 KB
  engine `__text` overflows OBJLINK `MERGE-CAP` ($10000/64 KB), and baking
  `lib/object*.f` into `bin/hb` breaks "small source-loading engine", so unify by
  sharing `DRV-EMIT-IMAGE ( sig$ path$ -- )` in `src/habu/driver-io.f` (loaded after
  macho/sign in every context) across all six emitters — stage2/build/maker/stdin/
  aot-lib and OBJIMG:WRITE — instead of physically merging the engine as an object.
  The OBJIMG `TEXT>ASM`re-load (`ASM-INIT ; <bytes> BYTES,`) is a byte-identity:
  `BUILD-MACHO`/`CODESIG2` read only `CODE`+`CODELEN` (= `ASM-LEN`, always 4-aligned);
  `ASM-INIT` clears only label/fixup state that `BUILD-IMAGE` never reads. Build
  drivers are NOT baked into the final REPL `bin/hb`, so refactoring their emission
  tails leaves`bin/hb` byte-stable — only `hb-stage`/`hb-stdin-mk` intermediates
  change, and the `stage` fixpoint still proves self-rebuild identity.
- **Single-file CLI tools stay testable through the lint sink:** when file
  ownership forbids a core+wrapper split, route all output through
  `LINT-OUT-WRITE`, keep the API in a package `public` section, and let the test
  install `LINT-OUT-BUFFER!` before `require`-ing the tool — the require-time
  main run becomes the captured live end-to-end fixture
  (`tools/trusted-inventory-test.f`).
- **Token equality is not site classification:** source-lex word records already
  kill comment/string false positives, but name-position references still count
  — `: TRUSTED:` in `bootstrap/src/defining.fs` needed a definer-ref filter on
  the previous word token. Cross-check a new scanner against raw `rg` per-file
  counts and eyeball every difference before trusting totals.
- **`lib/vector.f` element access is O(index), so filling a vector is O(n²):**
  `VEC-CELL-FIELD` computes cell slots with `0 ?do cell+ loop`; measured 141k
  `VEC-PUSH-N` = 65s while a 135KB `c@` loop is ~0ms, and `LEX-SOURCE` took
  9.2s on `bootstrap/cg/forth.fs` alone (8 pushes/token). Until
  habu-fix-quadratic-vec-7044f70d lands, keep source-lex off repo-scale paths
  and stream instead — rewriting trusted-inventory as a streaming scanner cut
  the repo scan from 33s to 0.9s with byte-identical TSV.
- **Repo lints carry a largest-source watermark:** shadow-lint, trust-lint
  callers, and stale-status-lint all had fixed $20000 file buffers with
  checker.f already at 96.5% of the cap; growing a core file tripped
  `lint: file exceeds buffer` across the gate. When growing src/core, check
  the lint file-buffer caps like the maker `__text`/`CODE-CAP` walls.
- **Top-level tokens resolve during execution, not per line:** a snapshot
  entry cannot run `FORGET-DEFS-FROM ... SNAPGO` as one top-level line — the
  hide removes `SNAPGO` before it is parsed. Compile the sequence into one
  (unchecked-boundary) word first so every name is resolved before the
  retire runs.
- **Warm snapshots can retire the builder tail and beat the cold engine:**
  the snap source loads the dev keep surface first, marks the builder tail
  (`SNAP-TAIL-MARK`), and `FORGET-DEFS-FROM` retires names+code+sigs before
  `SNAPGO`; with the hash-indexed checker the restored image checks defs-1000
  faster than `bin/hb` (0.15s vs 0.18s) and boots in ~0.02s.
- **Test framework and project policy are separate:** `lib/test.f` owns the
  reusable suite/group/test mechanics and setup/teardown hooks; Habu-specific
  warm images, filters, and process argv live in the Habu test adapter.
- **Source-list preverify needs its own diagnostics:** `tools/check.f --source-list`
  can fail before the child `hb` run, so relying on the child stderr collapses to
  exit 70. Buffer preverify checker diagnostics directly, print the source-list
  entries in default mode, keep `--json-errors` JSON-only, and prove the behavior
  through the `TEST:SUITE check-cli-boundary` runner instead of shell chunk-bisecting.
- **Focused suites go through the runner support entry:** `test/gate-stdlib.f`
  assumes `lib/process-env.f`, `test/gate-pool.f`, and runner setup are already
  loaded; a hand prelude crashed in `PROC-ENV+`. Use
  `bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- lint-libs-ptx`
  or `-- check-cli` so the new suite groups print `GROUP:`/`PASS:` and carry their
  process/env context.
- **Emitter shape tests are not assembler proof:** `tools/ptx/softmax-bwd-cg.f`
  rendered plausible text but `ptxas` rejected undeclared predicates above `%p15`;
  `tools/ptx/softmax-bwd-opt-cg.f` rendered `SOFTMAX_BWD_OPT` while loading stale
  `[p_x]`. For PTX emitters, keep the text fixture, then assemble the exact emitted
  artifact before claiming device proof.
- **Test entry files should not end in `bye`:** `maki/eval-device-test.f`,
  `maki/eval-compare.f`, `tools/ptx/softmax-launch.f`, and
  `tools/ptx/softmax-gradcheck.f` all printed `test: ok` and then exited nonzero
  because of a trailing top-level `bye`. Let `T-REPORT` be the exit boundary for
  loadable tests.
- **In-process eval needs state rollback:** `GE-EVAL-FORGET` must restore more
  than `cp`/`ndict`/`UEND`; current wordlist and `JSON-DIAGS` leaked across
  resident tests until they were snapshotted. Runtime-wide profiler state is
  not covered by that rollback, so profiler tests remain process sentinels.
- **Resident suites must preserve parallelism:** moving all `lint-libs` into one
  in-process runner serialized PTX and regressed time. Split resident groups by
  parallel-safe cohorts (`core`, `ptx`, `ptx-neg`, `ptx-toolchain`, artifact
  fast checks) so setup duplication and child `hb` launches drop without turning
  the suite single-file.
- **Resident forks need explicit harness context:** a forked in-process test does
  not see the `PROC-ENV+` vector prepared for spawned children, and `GETENV`
  still reads the host env. Install Habu-side defaults for helper spawns and
  direct warm/check overrides for in-process helpers, or resident tests rebuild
  warm images and silently lose caches.
- **Nested candidate fixtures need explicit argv targets:** resident defaults can
  select the candidate for the outer test process, but a nested fixture that
  calls `GETENV` may still fall back to stale host `bin/hb`. Pass the candidate
  executable as a script arg for child-spawning fixtures such as `test/proc-pty.f`.
- **In-process tool tests need injectable executable context:** `tools/check-test.f`
  can run standalone or inside the resident suite; nested raw-`hb` sentinels must
  use a library-level `HB!` override or `PROC-ENV-DEFAULT$?` suite default instead
  of rereading process env from a forked worker.
- **End-to-end time includes entry compilation:** `test/run.f` can report
  sub-30s after `TR-GATE-START!` while `/usr/bin/time` is still >40s because
  parent load compiles resident support first. The next architecture cut must
  remove top-level support compilation from the timed command, not just optimize
  scheduled phases.
- **Content-key caches must stay memory-resident during a key build:** re-reading
  `content-key.cache` for every `CK-FILE+` made hot launchers pay repeated I/O.
  Load the cache once per root and append new rows to the in-memory copy.
- **Semantic tool slices should dispatch directly:** routing `tool-boundary-trust`
  through the generic stdlib suite kept a process/test-suite boundary on the
  critical path. Direct `GSI-*` dispatch in resident forks cuts inner `hb`
  launches and makes the slowest slice visible.
- **No-binary bootstrap is its own gate path:** Gforth runs `bootstrap/cg/forth.fs`,
  so Habu typed locals such as `done:label` are literal Gforth names and break
  codegen. The bootstrap data region must also fit static checker state
  (`USIGS-BOOT`), and its native reload prelude must reset user signatures and
  hide from `SEQ` before reloading current source.
- **Do not bake one-off AOT gate support into broad runner images:** adding
  `tools/aot-call-report.f` to the runner image overflowed the checker user-signature
  snapshot even though AOT phases are scheduled cold/early. Keep AOT-only report/assert
  helpers on the cold AOT paths and reserve the runner image for phases it actually runs.
- **Direct hb-build linting is an adapter, not a runner dependency:** baking
  `aot-lint-core`/`signature-lint-core` into `hb-gate-warm` also overflowed the
  user-signature snapshot. Keep `hb-build-lib` defaulting to child CLI lints and
  install direct lint hooks only in cold AOT phases that already load the cores.
- **Gate budgets are stop-line thresholds, not comfort blankets:** raising the
  default native gate budget to 160s hid duplicated gate work after the suite had
  already been cut near 90s. Keep the default at the current green threshold and
  make regressions explicit; use `test/run.f -- --budget-ms N` as the explicit
  slow-host override while the 30s architecture work removes duplicate
  launches/builds.
- **Content-key reuse is not a warm snapshot:** the steady-state suite uses a
  persistent content-key cache for the candidate/build artifacts, not a warm
  snapshot harness. Reports and docs must say `cache-root=persistent`/`scratch`;
  reserve "warm" for the explicit `tools/warm-image.f` feature tests.
- **Cache Habu-under-test by content, not path:** the full gate's first wall was
  a fresh fixpoint build before any under-test phase could start. Hash `bin/hb`,
  the runner/build harness, and every emitted engine/repl source; a persistent
  hit can unblock under-test slices immediately, while a miss still runs the
  normal fixpoint and installs the produced candidate under that key.
- **Candidate production is not validation:** a cache hit correctly skipped the
  fixpoint producer, but also skipped `GE-ENGINE-SUITE` and hook/dictionary
  checks because `GENG-BUILD-SLICE` mixed build and proof. Keep the producer
  build-only; always run a candidate validation row after the `under`
  capability is ready, including cache and `--under PATH` imports.
- **Persistent maker caches must be keyed by emitted maker inputs:** once
  `HABU_BUILD_CACHE` points outside the per-run temp root, `hb-aot-mk` and
  `hb-build-mk` cannot be fixed filenames. Hash the build library, loaded
  helper libs, build-fixpoint source, target source, common engine source, and
  selected AOT/REPL driver, then put the mode in the key.
- **Warm snapshots must serialize used state, not reserved arenas:** hiding
  dictionary names without rewinding `cp` leaves dead code in snapshots, and
  static checker arenas (`create ... allot`) bake reserved capacity into every
  image. Warm images now compact tail code and heap-allocate checker signatures,
  copying only used signature bytes into the snapshot payload.
- **Content keys cannot nest the global SHA context:** `SHA256-FILE` resets the
  global SHA state, so using it while an outer cache key is active collapses the
  key. Build a tagged manifest of version strings, file names, and per-file
  digests, hash the manifest once with `lib/content-key.f`, and prove
  invalidation by changing a baked source and requiring a miss.
- **SHA output ownership is caller-visible state:** `SHA256-FILE` writes the raw
  digest to `SHA-DIGEST` and resets `SHA-OUT`; wrappers such as
  `SHA256-FILE-HEX` must keep their caller output pointer separately and render
  from `SHA-DIGEST`, not from the global `SHA-OUT`.
- **Stdlib files have three registry points:** adding a library file requires
  the source file, `lib/std.manifest`, and `FILEMAP.md`. Miss the manifest and
  the direct manifest gate fails; miss `FILEMAP.md` and the derived
  filemap-lint fails (every .f/.fs under src/tools/test/lib must be listed or
  be a committed exclusion row — the old hand-kept required list is gone).
- **Pool slots are host policy, not universal truth:** on this macOS/aarch64
  12-core host, persistent-cache full suites run 24.341s internal / 26.72s wall with
  8 top-level slots and 2 nested slots. Keep Linux conservative until measured
  there, cap dynamic slots below reserved artifact slots, and use
  discoverable argv knobs:
  `test/run.f -- --pool-slots N --nested-pool-slots M`.
- **`Habu-under-test` is the small engine, not a snapshot:** the native engine
  gate accidentally promoted `hb-new`, whose snapshot trailer baked `$14523b4`
  bytes of live DATA and produced 22 MB candidates that could jump into zeroed
  dictionary code on Linux. Promote `hb-stdin`, enforce a small candidate size,
  and leave snapshots as explicit `tools/warm-image.f` feature artifacts only.
- **Candidate size is ratcheted, not just capped:** `GE-MAX-CANDIDATE-BYTES`
  only rejects catastrophic bloat; gradual engine growth needs the committed
  per-target baseline rows in `test/gate-build-size.f` (TRUSTED.md-style
  manifest). Growth fails the build/validate slices until the same commit bumps
  the row; shrinkage prints `STALE-BASELINE` so the row is lowered; an
  unmeasured target row (0) fails closed printing the measured size. Baselines
  are per-target because Mach-O and ELF candidates differ in size.
- **Pipe-scoped env vars are easy to lie with:** `env HB_TMP=/tmp/x printf '' |
  bin/hb ...` sets `HB_TMP` only for `printf`; to preserve a gate temp root, put
  the env on the Habu side: `printf '' | env HB_TMP=/tmp/x bin/hb ...`.
- **Gate budget proofs need an uncontended Habu host:** full-gate timing is
  meaningless while another worktree is running `test/run.f`; a concurrent
  `habu-maki` gate pushed local runs from ~125s to ~154s and left active `hb`
  children competing for CPU. Check for active Habu gates before claiming a
  budget regression or pass. In this exec environment, focused `--load` smoke
  commands also need explicit stdin EOF (`printf '' | bin/hb --load ...`) or the
  REPL can wait on an open pipe until the harness kills it.
- **Gate phase order must follow measured tails:** adding dictionary/checker
  coverage can turn a formerly short late slice into the critical tail. Keep
  `test/run.f` `TR-LATE-ORDER` sorted by measured post-warm duration so coverage
  additions do not push a 20s+ slice behind every lint cleanup phase. Split
  check-warm-only tails (`engine fixture`, `dictionary/checker`) from
  tools-warm-dependent tails; waiting for both warm images before starting all
  late phases wastes several seconds. Record warm completion in persistent flags,
  not fixed pool-slot probes, before reusing slots for later phases. The gate
  pool poll timeout is part of the budget too; a 1000ms poll can lose the whole
  margin at the final phase, while `$64`ms keeps attribution responsive without
  busy-spinning. On this 4-online-core Linux/aarch64 target, six top-level slots
  outperformed eight because several phases spawn nested pools.
- **Do not spawn assertion tools for semantic checks:** gate JSON assertions are
  checked library words in `tools/gate-json-assert-core.f`; calling them
  in-process preserved coverage and cut hot helper spawns from 151 to 123.
- **Do not run negative compiler probes through raw `evaluate`:** positive
  `GE-EVAL-CAPTURE` can run in-process, but rejected source can exit through
  checker/compiler `die` before `catch` returns. Migrate semantic negative tests
  through `CHECK-CANDIDATE!` plus diagnostic rendering; keep true CLI/die
  boundaries as child-process sentinels.
- **Use checker-core buffer adapters for semantic negatives:** `CHECK-ALL-ERRORS-BUF`
  reuses `VERIFY:SOURCE-BUF` candidate scope and diagnostic-buffer rendering
  without writing a temp file or spawning `hb`. It is valid for ordinary semantic
  rejects such as package no-return flow and duplicate-definition. Full-source
  failures such as duplicates must be handled before per-definition isolation,
  or the fallback can falsely pass. Do not add `TRUST` for checker internals just
  to name the duplicate; keep a generic diagnostic until a typed checker
  diagnostic API exists.
- **Warm image cache publishes must be stamp-last:** a failed warm-tools bake
  wrote a new `hb-tools-warm.trust.f` beside an old image/stamp, so the next
  cache hit paired mismatched artifacts and failed with `checker: duplicate
  definition: repl-file-cap`. Build into candidate image/trust paths, delete
  the old stamp before publish, rename artifacts, then write the stamp last.
- **Warm images must clear include state before snapshot:** a support file that
  used `include` left `INCLUDE-BUFS-A` pointing at an mmap address from the
  baker process. Restored warm images then failed nested includes with
  `include: read failed`. Snapshot prep must close/reset include buffers,
  cursors, depth, and path state before `SNAPGO`.
- **Fixed artifact slots must not be reused by the dynamic pool:** warm/check/AOT
  artifact phases used fixed slots that the normal pool later reused before the
  readiness flags were marked. That made `TR-DRAIN-UNTIL-WARM` wait on unrelated
  engine/debug work and hid cold-cache overlap. Put fixed artifacts in pool
  slots outside the dynamic slot range and let normal phases use slots `0..N-1`.
- **Focused gate wins must survive the full DAG:** splitting
  `tool-boundary-lints` into parallel suites cut the focused tool slice from
  ~22.8s to ~18.3s, but hot full gate regressed slightly under contention and
  added top-level subprocesses. Keep the in-process semantic cuts; revert splits
  that only win in isolation.
- **Do not split resident groups past setup amortization:** splitting combined
  tool-doc/tool-repair residents into five single-purpose resident forks made a
  steady-state macOS run regress from 29.04s/26.760s to 30.40s/28.031s. Parallelism that
  duplicates resident setup and adds fork scheduling loses even when individual
  subtests look independent.
- **Inline resident work must overlap child suites:** the winning
  `tool-boundary-lints` cut split tests into load-only libs, skipped the cold
  child suite, and ran inline lints immediately before `GT-POOL-DRAIN`; running
  them before spawning sibling suites serialized the slice and lost the full-DAG
  win.
- **Do not make inline gate work a serial dumping ground:** inlining
  check-repair/doc/typed-local semantics into the single stdlib hook made zed's
  tool slice wait on one resident thread for ~56s. Split independent semantics
  into first-class resident-runner rows; the steady-state local suite fell to
  24.579s while still running candidate validation.
- **Run the suite from `bin/hb`, not a top snapshot:** replacing a broad
  top-level `hb-test-suite` image with direct `bin/hb --load test/run.f` kept
  the small engine as the entry point, removed a generated setup artifact, and
  still held macOS steady-state wall time under 30s. Cache-fill remains a separate
  profile because candidate and builder artifacts are legitimate misses.
- **Share support at the family-worker level, not the suite root:** preloading
  all runner support in `test/run-resident.f` removed repeated loads but serialized
  an 18.3s `setup/shared-support` span and regressed macOS wall time to ~39s.
  The winning shape overlaps non-stdlib phases with shared stdlib setup, classifies
  every `run-worker-stdlib.f` phase behind that setup, and loads engine/diagnostic
  support once inside a phase-family worker before forking child tests from that
  image. macOS hot fell to ~19s wall; zed hot fell from ~82s to ~60s wall.
- **Warm launchers hide duplicated checker work:** after removing the top
  snapshot, direct checker diagnostics exposed repeated `tools/check.f` support
  loads. Semantic diagnostic tests should call `tools/check-core.f` in-process
  with fd capture; only true CLI stderr/argv contracts keep a child `hb`.
- **Load repeated semantic setup once:** the stdlib tool groups were paying the
  same checked lint/check-all-errors setup in every resident process. The current
  suite loads that common tool base once as silent suite setup, then forks
  phase-owned resident workers without removing CLI boundary coverage.
- **Discovered content-cache misses must switch budget class:** a default run
  after changing `test/run.f` rebuilt candidate/build artifacts but still used
  the steady-state macOS budget, failing despite all tests passing. Candidate and
  builder artifact misses now apply the scratch-cache budget unless the user
  supplied explicit budget args.
- **Stats schemas need content assertions, not just counters:** `GS-ROW` first
  emitted `sha/boundary/runner/subject/label` despite docs promising
  `label/subject/runner/boundary/sha`. Tests must assert representative TSV row
  content so telemetry remains usable for scheduling RCA.
- **Every telemetry label needs one owning emitter:** fork children share the
  stats file, so a child emitting its fork label and the pool pass-hook emitting
  the same label double-counted spans (238 vs 193 real). The pool owns spans for
  its entries: `GT-POOL-FORK-CHILD` records the fork label via `GS-CHILD-LABEL!`
  and `GS-SPAN` skips matching labels; load time is a separate `span-load` class
  so slowest-test attribution stays tests-only.
- **Pool failures must drain, not kill:** dying at the first red child hid
  sibling failures and cost one gate run per red. `GT-POOL-FAIL` records a red
  row (label, outcome, capture paths) and continues; `GT-POOL-DRAIN` stays
  fail-closed by dying after the drain when reds exist, and the top runner keeps
  `GT-ROOT` on failure so the streamed per-child capture files survive triage.
- **Concurrent jj ops can revert uncommitted workspace edits:** a sibling
  agent's operations made this workspace's `jj diff` fail with a divergent-op
  error, and `jj workspace update-stale` then rebuilt the working copy from the
  last snapshot, discarding on-disk edits. In shared-repo parallel sessions,
  `jj commit` each verified change immediately; do not accumulate uncommitted
  work across another agent's active operations.
- **Late-bound suite hooks need `defer`:** redefining `SUITE-INLINE-WORK` after
  `GATE-STDLIB-MAIN` compiled still called the old xt. Use a checked `defer`
  hook installed by the resident entry, and execute inline work at the
  existing pre-drain point so it overlaps sibling child suites.
- **Build caches must be default-on and content-keyed:** a 40s budget failed a
  cache-fill run at 71.015s even though the steady-state path was ~31s. Keep
  `hb-under-test`, maker, artifact, and file-digest caches under the default
  user cache root, remove opt-in cache overrides, and let the budget catch real
  regressions.
- **Artifact caches must key content, not temp paths:** `hb-build` output caching
  first missed every run because `CK-FILE+` included the temporary source path in
  addition to the digest. Use a stable logical label plus the source digest for
  generated/temp-owned inputs. AOT-positive fell to ~13s hot, but the full gate
  barely moved because AOT-negative became the critical path.
- **Schedule early only when the artifact invariant is already true:** hot gates
  restore content-keyed `HABU_UNDER_TEST` before `TR-EARLY-START`, so source-only
  lint slices can run early under the candidate. Guard early manifest/libs on
  `TR-UNDER-READY`; leave artifact lint late because filling the early pool
  regressed the full DAG.
- **Strict duplicates expose generic fixture names:** once redefinition fails
  closed, tool fixtures named `OK`/`BAD`/`FOLD` and shared helpers named `STR=`
  collide with baked or sibling words. Prefix generated names by fixture/tool and
  keep shared lint helpers in a tool-owned vocabulary.
- **Core bootstrap helpers need private names:** do not move public stdlib words
  behind checker-hook trust to dodge duplicate definitions. Give bootstrap-only
  helpers a core-owned name such as `CORE-STR=` and let `lib/string.f` publish
  the public `STR=` contract so stale native `bin/hb` can self-refresh.
- **`is` needs checker-owned target kind:** runtime `DEFER-MAGIC` validation is
  not enough. `defer` must record a checker-visible target-kind entry, and
  `tools/check.f` must reject `is` on non-defer words before runtime.
- **Warm snapshot tails hide, they do not replay:** replaying baked core/target
  files used to dodge duplicate `SNAP-OUT`, but it also reset state by accident.
  Emit `HIDE-DEFS-FROM SNAP-OUT`, append `snap.f`, and test that old tail deps
  are absent.
- **Image mmap pointers are transient:** `MBUF-A` persisted through a restored
  image and crashed the next `IMG-M8`. Snapshot writers must clear mmap-backed
  image buffer pointers/cursors before building a new header.
- **Reserved parser names need a preflight lint:** `variable I` can pass the raw
  engine but make `tools/check.f --source-list` fail as silent rc 70 because `i/j`
  are compiler loop-control tokens. `tools/reserved-name-lint.f` now runs before
  the checker child; generated converters must run it after prefix stripping so
  naturalized names become `IX`/`JX`, not bare `I`/`J`.
- **The emitted stage prefix has two check regions - retire preflight asserts
  only for the checked one:** in the generated stage source the checker-boot
  (util/structures/checker/render) plus asm/icode/mnem load inside the
  `BFR-CHECK-OFF .. ' HOOK set-check` window (unchecked), while
  habu1/prof/regalloc/jit/habu2 load with `HOOK` on (blocking-checked). So a
  stack-effect regression in a habu1/habu2 `:` emitter fails the stage compile
  outright, but icode's does not - only the non-blocking `BF-CERTIFY` static scan
  covers icode. When retiring the `BF-PREFLIGHT` textual asserts, drop the
  habu1/habu2 typed-shape guards (real blocking check now covers them) but keep
  icode's until `BF-CERTIFY` flips blocking, and keep the same-type codegen-role
  asserts (`LABEL@ B,` vs `@ B ;`; `SZA-I @ +` vs `over +`) that no checker can
  express. Prove the replacement with a negative fixture (emitter underflow ->
  `BF-CERTIFY-RC` 70), not by trusting the compile.
- **Re-emitting the same source across build stages is a boot-reload TOCTOU:**
  `build-fixpoint` re-reads the boot-prefix files for the stage2, stdin, and snap
  emissions and the stamp-key re-emit; a mid-build source edit would let stage N
  build from one revision and N+1 from another, silently entering the installed
  image. `BF-PIN` pins each emitted path's content digest (keyed by SHA-256 of
  the path) on first read via the single `BF-APPEND-SOURCE` choke point and
  re-verifies on every reload, so there is no separate manifest to drift and a
  mismatch throws `E-BUILD-BOOT-DRIFT`. Baking the digest for boot-*time* reload
  verification is a separate engine change (dot habu-boot-pin-bake).
- **Focused gate slices need a temp root:** direct-loading
  `test/gate-dictionary.f` does not run `TR-START`; use `test/run.f` or an
  explicit suite temp/cache root, or generated artifacts resolve under `/` and
  fail with `E-FS-OPEN`.
- **Nested lint subprocesses need their own timeout caps:** fast tool probes can
  keep a tight timeout, but a fixture that spawns `trust-lint` or another
  repo-scale tool must use a separate cap sized for aggregate-gate contention.
- **Repo-scale source lints stream, not vectorize:** duplicate-definition linting
  generated stage2 source took >40s when it first built a full token vector. A
  streaming scanner over the source buffer cut the same 400KB stage2 file to ~2s;
  use raw `parse-name` semantics for definer payloads so words such as `(CMP)` are
  not mistaken for comments.
- **Dot blocker edges need a gate:** stale `.dots/*` `blocks:` IDs made work look
  blocked on deleted/completed tasks. `tools/dot-dep-lint.f` now walks `.dots/`
  directly and fails the lint slice on any blocker that is not backed by a dot
  file.
- **Ignored dot archive cannot satisfy active blockers:** `.dots/archive/` is not
  tracked, so a clean host may not have the same archived files. Dot dependency
  lint must ignore archive entries and active dots must drop completed blockers.
- **Arm the opaque throw, don't guess the buffer:** a ~1-in-hundreds fork-worker
  `E-STR-CAPACITY -2201` (event-closure-test, stdlib/tail-pure) would not
  reproduce in 340+ runs and every capacity source was ruled out (all builders
  reset before first use; real host TMPDIR=49 keeps the largest SB build ~336;
  `s"`/`S\"` never touch SB at runtime OR compile; content-key cache inactive).
  Machine saturation only yields 5s WAITs (over-subscription), not the flake. The
  fix for an unreproducible race is not a capacity bump — it is arming the throw
  site: `test/gate-pool.f GT-POOL-FORK-THROW` now calls `tools/why-threw.f`
  `WHY-THREW-DUMP` so ANY worker throw prints one `WHY-THREW:` line per shared
  builder (SB/CK/CK-ROW) fill+cap before dying, self-identifying the buffer on the
  next occurrence. The gate `.f` closure is triple-registered: `FILEMAP.md`, the
  result-cache closure member set (`test/run-files.f TR-GATE-HARNESS-FILES`, or
  `run-result-cache-test.f CLOSURE-LINT` fails), and every `require`d file must be
  a closure member.
- **Hot-cache full-gate passes do not prove the engine-build closure; cold is the
  merge oracle.** A green `test/run.f` at 8.3s was a cache-HOT run that SKIPPED the
  native engine build slice; `test/run.f -- --cold-cache` exercises it and caught a
  `duplicate definition: CK-CAP` (rc 78, "Habu-under-test build artifact missing")
  my change introduced. Root cause: adding one `require` to a gate file (gate-pool.f
  -> why-threw.f -> lib/content-key.f) registered content-key EARLIER, so the later
  `include lib/content-key.f` in `test/gate-stdlib.f`/`test/gate-common.f`
  re-evaluated it. Fix at root with require-dedup, not definition tolerance: those
  build-manifest lines must be `require lib/content-key.f` (include-once), never
  `include`. When a change adds a transitive `require` to any gate file, run the
  cold gate before claiming green — merge-gate runs cold.
- **New PTX trusted primitives need rows before merge:** local `master` had
  `RELU`/`RELU-V4` TRUSTED sites without `TRUSTED.md` rows; the full native lint
  slice caught it. Add the row and a checked kernel fixture in the same change.
- **PTX elementwise ops share one codegen contract:** scalar and v4 tile binary
  ops route through typed opcode-to-mnemonic helpers; adding `-.`/`/.` means
  updating scalar/v4 TRUSTED rows plus both type-check and emitted-mnemonic tests.
- **Device proofs must fail closed:** CUDA/FFI proof tools are not allowed to
  print `NO` and exit success, reuse stale readback buffers, or drop Driver rc
  values. Device gates need rc-checked wrappers, private temp roots, sentinels,
  cleanup, and nonuniform multi-element goldens before they can support claims.
- **Gate speed RCA follows the phase wall clock:** warm images cut repeated
  inner-tool recompiles; the bigger wall cut came from a bounded checked DAG
  pool. Do not mutate `bin/hb` inside the gate: build candidates under private
  `HB_TMP`, run independent stdlib/diagnostic/engine slices concurrently, bound
  nested pools with `--nested-pool-slots`, and delay short timeout-sensitive
  lints until the heavy wave drains.
- **Full DAG timing beats isolated wins:** on Linux/aarch64, separate hb-build
  maker warming and warm-tools manifest loading passed focused probes but
  regressed the full gate under contention. Keep only schedule moves that improve
  the documented full command; record reverted timings in the dot so failed
  variants are not rediscovered.
- **Borrow SwiftForth semantics, not unsafe contracts:** SwiftForth's `PLACE`,
  `APPEND`, `ZPLACE`, and `ZAPPEND` explicitly do not check destination capacity.
  In Habu, borrowed string-buffer utilities must carry capacity and length cells
  (`BUF-APPEND*`) and throw named errors; do not import unchecked string mutation
  under familiar names.
- **Gate budgets must match measured host capacity:** on 2026-06-28 the full
  native gate was all-green at 100.985s internal time on the 4-online-core
  Linux/aarch64 target. A 90s default budget was below measured capacity for the
  current suite; keep the 90s/30s goal active, but use the documented full-gate
  command and phase timings to earn lower budgets instead of dropping coverage.
- **Gate instrumentation must survive cleanup:** appending counters under `GT-ROOT`
  and summarizing after `GT-CLEANUP` produces a false all-zero report. Emit the
  stats summary before deleting the gate temp tree, then enforce the time budget.
- **Shared Habu-under-test is a pipeline prerequisite:** build the candidate in
  the early engine-build slot, publish it atomically, print path+SHA, then release
  downstream phases onto `HABU_UNDER_TEST`. This preserves overlap and proves the
  rebuilt binary without serializing the whole gate behind fixpoint.
- **Candidate-output phases need the parent artifact path:** the resident runner
  waited forever after `native engine build slice` because phase 15 wrote
  `hb-stdin` under its private `HB_TMP`, while `TR-DRAIN-UNTIL-UNDER` waited for
  `GT-ROOT/hb-under-test`. Pass `HABU_UNDER_TEST` into the producer phase and make
  the drain fail once `GT-POOL-LIVE` is zero instead of polling an empty pool.
- **PTY fixtures should wait for events, not fixed quiet time:** `test/proc-pty.f`
  spent ~18.5s wall with <1s CPU because each interaction waited six 50ms quiet
  polls. Preserve max wait windows, but use small named poll intervals and a
  minimal quiet threshold so prompt-driven tests finish when the expected bytes
  arrive.
- **Batch diagnostic fixtures at the source:** if many checked rejects use the
  same tool load path, write one source file, run one `check-all-errors`, and
  assert each JSONL row by `word`/`repair_class`. Per-case checker spawns hide
  real coverage behind startup cost.
- **Merge positive builder proofs by invariant:** if two successful builds only
  differ by fixture payload, one strict feature bundle can own recursion, parser,
  closure, binary-shape, and output checks. Size caps should prove "no engine
  embed" for that bundle, not preserve a tiny-source threshold.
- **Do not lock shared maker caches:** AOT positive/negative can miss the same
  maker key concurrently. A mkdir lock timed out under full-gate contention and
  left a stale lock after pool teardown. Build makers in each private `HB_TMP`
  and publish the completed executable with atomic `rename`; cold races may
  duplicate work, but they cannot deadlock the cache.
- **Semantic check-tool fixtures should not spawn the wrapper:** reserve
  `tools/check.f` subprocesses for argv/env/stdin/exit/source-label contracts.
  `tools/check-test.f` kept positive verification and unterminated-string
  diagnostics as full wrapper subprocesses even though `VERIFY:SOURCE-BUF` and
  `CHECK-ALL-ERRORS-FILE` already owned those invariants.
- **Use checker-warm for checker CLI smokes:** when a test must keep a real
  `tools/check.f` argv/stdin boundary, run it through the checker-warm image
  with `check-core` baked and only `tools/check-main.f` loaded. The general
  tools-warm image should not recompile checker core for each smoke.
- **Batch source-list checks by dependency graph:** engine fixture checks for
  fs/process libraries spawned one checker-warm child per module group even
  though the unique dependency closure is one ordered source list. One combined
  `GE-CHECK-SRC-LIST` preserved certification coverage and cut the focused
  fixture slice by about 10s.
- **Nested gate captures report outcomes:** under full gate concurrency, 1s/5s
  `RUN-ARGV-CAPTURE` calls can throw silently before `T-REPORT` (`rc 58` is
  `E-PROC-TIMEOUT`). Gate boundaries use outcome capture plus attribution:
  case/phase, executable, argv/load list, outcome kind/code, named rc, capture
  bytes/cap, stdout, and stderr.
- **Gate entries catch their own throws:** a direct `test/gate-engine.f -- build`
  can fail before child attribution runs. Entry wrappers catch top-level throws,
  print the phase label plus throw code/name, then rethrow so rc stays useful.
- **Small-engine suites use test-owned checker fixtures:** `bin/hb` must not bake
  image/spawn emitter words just so checker algebra probes can run. Use local
  `T-*` TRUST rows for role algebra in `test/engine-suite.f`; keep real emitter
  source coverage in build-fixpoint/source-shape tests.
- **Manifest lint needs top-level scheduling under load:** `stdlib-manifest-test`
  was fast alone but its internal `public-signatures` child hit the old 5s
  timeout under full-gate contention and surfaced only as `rc 58` until pool
  outcome attribution printed the throw code. Schedule the manifest lint as a
  direct gate phase, size its child timeout for aggregate contention, and keep
  the test instead of hiding it inside a nested batch.
- **Outcome helpers belong in process libraries:** `gate-common` had duplicated
  the env/no-stdin outcome capture loop because `lib/process-env.f` only exposed
  the stdin outcome variant. Add missing `RUN-ARGV-ENV-*-OUTCOME` helpers in the
  process layer, with `process-env-test`, manifest, and docs rows, before wiring
  gate attribution to them.
- **Pool slot state is an invariant:** free=`-1`, active=`0`, done=`1`; set
  active before spawning. A live-count pool with free slots still marked free
  ignores its fds and spins forever after children exit.
- **Native port gates do not need language runtimes:** port validation proves
  `bin/hb`, target source selection, syscalls/env, ELF AOT, checker/lints,
  self-refresh, REPL, and Habu tooling on the target. Do not install JS/Python/
  Rust/TS/model stacks to prove a native port.
- **One public binary:** `bin/hb` (tty REPL, piped stdin, `hb script.f args`, or
  `hb --load lib.f tool.f -- args`). Build-only engines stay temp under `HB_TMP`;
  no legacy aliases/maker binaries.
- **Pipe vs script vs source-list mode:** non-tty stdin with bytes = pipeline mode
  even if `argc>1` (`hb seed count < prog.f`); empty non-tty stdin + `argc>1` runs
  `argv[1]` as a script; `--load src... -- args` reads listed files and leaves fd0
  as tool data (`READ-STDIN-ALL`). Reproduce gate check failures with raw stdin +
  the wrapper's own prefix — not file-argument mode (different code path).
- **No-binary recovery installs only `bin/hb`** (generated/ignored): recover with
  `HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh`; Gforth makes only `HB_TMP`
  artifacts, then `bin/hb` self-refreshes. Daily work never uses the Gforth path.
- **Bootstrap source parity matters:** `tools/bootstrap.sh` must append the same
  native source layers as `tools/build-fixpoint.f` (including
  `src/os/image-bytes.f`). Keep bootstrap `bootstrap/cg/forth.fs` emitter calls
  syntactically real (`MOVZ,`, `BCOND,`, `ADDI,`) because Gforth catches the
  comma-less raw word immediately.
- **Gforth host needs locals:** Homebrew 0.7.3 can't parse `{:` and isn't usable;
  use snapshot `0.7.9_20260610` (macOS: configure `UNSUITABLE_CC=none`, wrapper
  execs `gforth-itc -i gforth-light.fi`) or `GFORTH=…/gforth-fast`. Bootstrap
  probes must check output (exact `1`) AND exit 0 — 0.7.3 reports the locals error
  yet lets later shell stages continue.
- **Target source lists are a build invariant:** bootstrap and native fixpoint
  select `src/os/<target>/{sys,env,image,sign}` together; hard-coding one OS bakes
  its assumptions into later generations.
- **Private temp dirs for native builds:** parallel jj workspaces share `/tmp`;
  fixed names race. Allocate+export private `HB_TMP` (create it before spawning
  makers — a missing temp root collapses into an empty nonzero failure); derive
  parent artifact paths and child `HB_TMP` from the same getter. Cleanup prefers
  `trash` but falls back to direct removal (trash rejects `/var/folders`).
- **Parity is native fixpoint, not mirror drift:** the proof is byte-for-byte
  self-rebuild through build-fixpoint; retire bootstrap token-diff lints as a
  second source of truth.
- **Active work lives in dots, not a root plan:** once tasks have `.dots/*.md`, a
  root plan is drift. Current verification in `STATUS.md`, memory in `LESSONS.md`,
  ready work in `dot ready`. Completed/landed plans retire their root docs; root
  Markdown is contracts, status, or active work only.
- **On-device validated: this environment IS an Orin (2026-06-27).** Local GPU
  (`/dev/nvidia0`, `nvidia-smi` = "Orin (nvgpu)", compute_cap 8.7), `ptxas` 12.6,
  Tegra `libcuda.so`. Proven end-to-end: checked SAXPY → `PTX-EMIT-SAXPY` →
  `ptxas -arch=sm_87` → cubin → **loaded as a live GPU module via the Habu FFI**
  (`tools/ptx/cuda-load.f`): `cuInit`/`cuDeviceGet`/`cuDevicePrimaryCtxRetain`
  (NOT `cuCtxCreate` — that hangs on the Orin's camera primary context)/
  `cuCtxSetCurrent`/`cuModuleLoad`/`cuModuleGetFunction` all rc 0, valid function
  handle. FFI usage: `DLOPEN-SLOT @`/`DLSYM-SLOT @` give dlopen/dlsym; `ffi-call
  ( argbuf fn -- rc )` loads x0..x7 (≤8 args). Run-mode hangs (use `--load`, not
  `bin/hb file.f`); processes hang on exit unless the module/ctx are released.
- **GPU launch WORKS — the blocker was driver SYMBOL VERSIONING, not the FFI/ABI
  (resolved 2026-06-27).** `cuMemAlloc` returned 201 INVALID_CONTEXT *even though*
  `cuCtxGetCurrent` confirmed the context was current (handle matched the retained
  ctx) — which REFUTED the x18/TLS hypothesis. The real cause: the modern CUDA
  driver's actual entry points are the **`_v2` symbols**; `dlsym("cuMemAlloc")`
  returns a deprecated stub that fails. Using `cuMemAlloc_v2` / `cuMemsetD32_v2` /
  `cuMemcpyDtoH_v2` (the non-versioned setup/launch symbols are fine: `cuInit`,
  `cuDeviceGet`, `cuModuleLoad`, `cuModuleGetFunction`, `cuFuncSetBlockShape`,
  `cuParamSetv`, `cuParamSetSize`, `cuLaunchGrid`, `cuCtxSynchronize`) the **full
  SAXPY launch runs and is CORRECT** on the Orin: y = a*x+y = 3*2+0 = 6.0
  (f32 0x40C00000), matching the CPU golden (`tools/ptx/cuda-launch.f`, PASS).
  The deprecated `cuLaunchGrid`/`cuParamSet*` path (<=8 args) avoids needing an
  `ffi-call` >8-arg extension for `cuLaunchKernel`. NO engine change was needed.
  The eval-matrix data path is OPEN: emit checked kernel -> ptxas -> cubin ->
  load -> launch -> compare golden, all from Habu.
- **Real Triton on the Orin without a reflash (eval matrix DONE 2026-06-27).** The
  GPU driver is BSP-pinned at CUDA 12.6 (`cuDriverGetVersion=12060`); a 13.x driver
  needs a destructive JetPack-7 reflash (don't). pypi's default torch is `cu130` →
  won't load (`driver too old`), and the Jetson torch index `pypi.jetson-ai-lab.dev`
  is DNS-unreachable — BUT `download.pytorch.org` carries `torch-2.9.1+cu126`
  aarch64/cp310, and **cu126 matches the 12.6 driver exactly**, so
  `torch.cuda.is_available()` is True. The generic SBSA wheel has no sm_87 ATen cubins
  (`cudaErrorNoKernelImageForDevice` on `x+x`), but **Triton JIT-compiles each kernel
  for sm_87 at runtime**, so Triton kernels run — just avoid ATen GPU ops (alloc +
  H2D/D2H memcpy only). Result (`docs/eval-triton.md`): scalar Habu-PTX measured
  42.5 GB/s vs Triton 63 GB/s; RCA showed the gap was **codegen vectorization**
  (scalar global loads/stores vs vectorized global traffic), and the checked v4
  tile path now reaches ~63 GB/s parity. Triton catches name/type errors at compile
  but the **stack-discipline class only at runtime** (3/5 battery bugs slipped,
  incl. a missing-store → silent 0.0) where Habu-PTX's checker rejects at author
  time. The thesis MECHANISM is now backed by real-target data; do not revive the
  superseded launch-path-gap explanation.
- **No-checker ablation must run the whole unchecked path:** counting
  `GRADE-CANDIDATE` rejects as "without checker" lies because the checker already
  short-circuited them. `maki/eval-compare.f` now emits a throwaway `0 set-check`
  driver for the no-checker arm and attempts emit -> ptxas -> device golden for all
  9 SAXPY candidates. Measured on the Orin: checked catches 5/6 bugs before GPU;
  no-checker catches 0/6 before execution, emits+assembles all 9, and all 6 buggy
  candidates fail only as device-wrong.
- **External baseline stays out of the tree (host-lint).** `host-lint` WALK-FILES the
  whole repo and `1 throw`s on any `.py` path; `.md`/`.f` content is not scanned. So
  the Triton (Python) baseline lives as fenced ```python blocks in `docs/eval-triton.md`
  (reproduction reference) with the working scripts in `/tmp`; the Habu reducers
  (`bandwidth.f`, `eval-compare.f`, `eval-device.f`) remain the live column.
- **Model-driven pass@k needs an INDEPENDENT generator, not a curated fixture.** A
  real pass@k/repair number requires a stochastic generator we don't curate (curating
  the bug distribution makes the number a construction). Used independent Claude
  `general-purpose` subagents (k=5/task/target) as the generator; graded each through
  the target's full loop (Triton compile+device-golden; Habu checker+emit+ptxas+device).
  Result (docs/eval-triton.md): SAXPY 5/5 both; softmax Triton 5/5, Habu 3/5→5/5 after
  diagnostic-guided repair (1–2 rounds). Both highly reachable; the differentiator is
  failure MODE — every Habu failure was an author-time static reject with a located
  order diagnostic (`at 'row-store' expected: tile span rowctx actual: span rowctx tile`),
  zero GPU, which drove repair. Honest caveats logged: the softmax pass@1 gap was
  CONFOUNDED by my own prompt mis-spec of ROW-STORE's arg order (real order tile/span/ctx),
  and Triton produced no failures to repair so repair-rounds isn't a symmetric comparison.
- **Subagent grading harness gotchas:** Triton `@jit` rejects `exec`'d source ("should
  be defined in a Python file") — import the candidate as a real module file. Loading
  `maki/eval-device-sm.f` as a library runs its inline test block + `bye`; strip to a
  definitions-only copy for reuse. Normalize generated Habu candidates (strip a leading
  `:`/trailing `;`) before wrapping as `: K ... ;`. ROW-STORE consumes (tile span ctx)
  with the tile DEEPEST — the natural idiom leaves the tile on the stack then appends
  span+ctx.
- **v4 vectorization closes the bandwidth gap to Triton; 63 GB/s is the MEMORY ceiling,
  not codegen (2026-06-27).** The scalar codegen emitted `ld.global.f32` (1 elem/thread)
  -> 42.5 GB/s vs Triton's `ld.global.v2` 63. Added a checked v4 tile vocab
  (`lib/ptx/cg-vec.f` EMIT-*-V4 + `tile-v4.f`: `ld.global.v4.f32`/`st.global.v4.f32`,
  4 elems/thread, same parametric tile types so the SAME SAXPY body certifies — v4 is a
  codegen rep, not a type change) -> 63 GB/s, MATCHING Triton (device-golden PASS). Then
  tried to BEAT it: unrolled grid-strided v4 K=1/2/4/8 (up to 8 v4 loads in flight) is
  FLAT at 63; occupancy is 40x saturated (Orin NX = 4 SMs / 6144 threads); EMC already
  maxed (3199 MHz, jetson_clocks no-op). So 63 is the achievable streaming bandwidth
  (~62% of the ~102 GB/s Orin NX spec), and BOTH targets sit at it. You cannot beat the
  memory system on a memory-bound kernel — "faster than Triton" needs LESS traffic
  (op fusion) or a COMPUTE-bound kernel. Sig gotcha: v4 emit-helper stack sigs must use
  the generic-int token `n` (role names like `base`/`spanrd` bind as fresh type vars and
  the checker rejected at `!`). Device knobs: nvpmodel -m 0 (MAXN) was rejected on this
  unit; jetson_clocks locks clocks but needs `--store` before `--restore`.
- **Rigid-token fix is now a precise, bounded change (located 2026-06-27).** The
  checker instantiates a called word's effect by RE-PARSING its stored signature
  STRING per call (user/prim sigs are stored as text at `USIGS`, checker.f:886;
  re-parsed through `SIG-TYPE`). Type vars freshen per parse (`NMAP-RESET` +
  `VAR-OF`→`FRESH`), but nominal atoms (`MK-ATOM`) keep their literal name, so two
  parses of `extent-n`/`mask-live` produce string-equal atoms — the exact reason a
  constructor's extent/mask is NOT fresh per call (the soundness gap). FIX: add a
  fresh-marker atom convention (e.g. `mask-fresh`/`extent-fresh`); in `SIG-TYPE`/
  `MK-ATOM`, mint a UNIQUE name (base + a global counter) for a marked atom so each
  call's parse yields a distinct RIGID atom that string-mismatches others. Declare
  `GRID-CTX`/`ROW-CTX`/`MK-SPAN` outputs with the fresh marker. Result: two
  independent ctxs get distinct rigid masks → a mixed-mask op rejects (a generic
  op's var requires both equal; distinct rigid atoms don't unify), while one ctx
  bound to a local reuses its single fresh mask → SAXPY still certifies. The
  checker now stores fresh atom templates and renders recorded rigid outputs back
  as distinct templates, so wrapper words preserve equality/distinctness too.
- **Local type inference is already built too (proven 2026-06-27).** Like M2, the
  inference.md feature ("infer bodies, annotate the edge") is operational in the
  shipped checker: untyped intermediate locals (`{: x :}`/`{: g :}`/`{: c :}`)
  certify, the checker infers each from top-of-stack and threads its tokens, and a
  mis-used inferred local rejects with a precise type (an inferred `gridctx` used
  where a `span` is required → "expected span actual gridctx", exit 70). The
  SAXPY/SOFTMAX kernels rely on it (the mask/extent tokens thread through inferred
  intermediates). Only the `{: x:? :}` show-inferred form (inference.md marks it
  "proposal, not implemented") is unbuilt. Two named "Habu features to land" (M2,
  local inference) were already shipped — always probe the checker before sizing.
- **Native helper leafness is a control-flow invariant:** extending `LCFPOP` to
  emit local-frame teardown made it call `LCEMIT`; without saving `x30`, `RET`
  jumped back into `LCFPOP` (`pc=lr=...aa44`) and branch-local definitions hung
  at compile time. Any helper that gains a `BL` gets a real LR frame in the same
  change.
- **Extensible nominal types must be explicit:** `DEFTYPE` registers a copied,
  global nominal role before signatures use it; unknown signature tokens still
  reject. Silent auto-interning would turn typos into distinct "valid" roles.
- **Ground capability claims in the source, not the dot tracker + spec.** The PLAN
  review concluded "M2 (parametric checker) is a large unbuilt gate" from `dot ls`
  (no `m2` dot) + the spec calling it "large", and elevated it to CRITICAL. A
  10-second empirical check refuted it: M2 is fully built+landed in
  `src/core/checker.f` (`SIG-TYPE`/`MK-PARAM` parse `span<...>`, field-by-field
  unify, `render.f` round-trip, `KERNEL:`/`GRID:`/`WHERE`) and works in the
  installed `bin/hb` — a matching parametric sig certifies (exit 0), space/extent
  mismatches reject with field-precise diagnostics (exit 70). A "missing dot" can
  mean *done*, not *unbuilt*. Before sizing a checker feature as a prerequisite,
  run a minimal certify/reject probe through `bin/hb`; the real unbuilt frontier
  was M4 (the tile *operations*: MK-SPAN/LOAD/STORE/SCALE/collectives — absent),
  not M2.
- **Dot hygiene:** use only documented commands — `dot add "T" -d '…'`,
  `dot ready`, `dot ls`, `dot tree`, `dot show <id>`, `dot on/off <id> -r '…'`.
  Mark/close the exact printed ID (don't infer from filenames). `-P <root-id>`
  only when `.dots/<root-id>/<root-id>.md` exists; never `-P`/`-a` a nested id (it
  writes a blocking edge / stray dirs / ambiguous IDs). Quote `-d` (backticks,
  `$`, Forth punctuation). Handoff: leave the dot open, record commit + pending
  edits + definition-of-done in the committed doc, close only after the validated
  commit; add step-level child dots for in-progress work.
- **Worker ownership starts with `dot on`, not the spawn call.** I dispatched
  isolated workers while their dots still said `open`, so another orchestrator
  could legally select the same work from `dot ready`. The blocking rule now
  lives in `AGENTS.md`: record the agent/workspace, activate and publish the
  exact leaf, synchronize, then re-verify immediately before dispatch. Keep it
  active through review and integration, and close it only after the reviewed
  commit and owning gates land. An unpushed active bit is not a cross-lane claim.
- **Keep debugger docs in the agent index:** list `docs/debugging.md` (`.s`, watch
  cells, REPL `step`, breakpoints, `jitdump`, `imgdump`) in `FILEMAP.md`, guarded
  by `tools/filemap-lint.f`, so RCA starts with existing tools.
- **Focused lint/check reruns copy the test load list:** lint tools have non-obvious
  deps (`tools/date.f`, `lib/memory.f`, `lib/vector.f`, `tools/lint/intern.f`).
  Copy the `TEST:SUITE` entry list from `test/gate-stdlib-cases.f` or the tool
  header instead of reconstructing from memory.
- **Use the current gate command, not stale handoffs:** after the Darwin spawn
  F04 macOS validation, the old handoff gate without `test/gate-pool.f` exited at
  `GT-POOL-START`; the `docs/bootstrap.md` command with
  `test/gate-pool.f test/run.f` passed the full native gate.
- **Close platform dots only with target evidence:** Darwin spawn factoring closed
  after real macOS arm64 refresh, source-shape, process/argv/env/cwd, PTY, and
  full native gate passes; Linux evidence alone was not enough for F04.
- **Checked DSL incidents moved into the standard:** the `GE-FILES:`/`CHK-FILES:`
  generic `execute` failure and `tools/gate-json-assert.f` raw `[']` row failure
  are now covered by the checked DSL/quotation rules in `docs/forth.md`.
- **Layout constants need one owner:** load `src/habu/layout.f` before every
  runtime prefix (env, baked REPL, stepper, watch, debug). Refresh once with
  compatibility constants if the installed `bin/hb` needs them, then remove dups
  and prove with the regenerated binary.
- **Retire stale benchmark programs:** the old cross-language LLM matrix already
  answered the active planning question (Habu cost was roughly 8-10x TypeScript/
  Rust on the hard array tail). Do not maintain that harness while GPU/PTX is the
  focus; keep only the status note unless a new publication-grade claim becomes
  active work.
- **Manifest policy is row data plus one validator:** doc and module-note
  requirements in `tools/stdlib-manifest-test.f` stay maintainable when grouped
  as checked rows; branch ladders hide policy drift and make adding requirements
  look like control-flow work.
- **stdlib gained `ffi`/`float`/`fmt`:** `lib/ffi.f` (typed `DLOPEN`/`DLSYM`/
  `CALL0..6`/`>CSTR` over the `ffi-call` trampoline), `lib/float.f` (`STR>FLOAT`
  decimal→f64 + `POW10`), `lib/fmt.f` (`SB-U`/`SB-INT`/`SB-FIX` builders +
  `.U`/`F.N` — renamed 2026-07 from `U.0`/`.0`: number-shaped names are
  parser-claimable and now lint-rejected, E-NUMERIC-DEFINITION). Registering a
  new lib = `std.manifest` rows for every public
  colon word (match `tools/public-signatures.f` output *exactly* —
  `TRUSTED:`/constants are NOT extracted, so they get no row), a
  `FILEMAP.md` row, a `TEST:SUITE` entry, and `TRUSTED.md` rows for any
  `TRUSTED:`. The doc-contract check is a curated spot-list, not per-module.
- **Warm image entries cannot hide dependencies behind include:** a normal
  `tools/check.f` can `include tools/check-core.f`, but warm images load entry
  files through their baked source map and the include boundary can fail or crash.
  Bake the core into the warm image when it fits, then load a no-include
  `*-main.f` entry; otherwise pass core+entry explicitly with `CLI-TOOLS-LOAD2`.
- **Fixed pool slots must be reserved before dynamic starts:** starting a
  dedicated slot after a general phase can reset an active slot, orphan the child,
  and leave `GT-POOL-LIVE` permanently high. `GT-POOL-START-SLOT` now fails
  closed on active-slot reuse; start fixed-slot artifact builders before free-slot
  phases that might claim their indexes.
- **Habu's locals/loop discipline costs first-time iteration:** porting real code
  (a real application's tegrastats/netpbm parsers to its own Habu tree) hit the same walls
  repeatedly — no local bind after `exit`, a `begin/while` condition may only add
  a flag, a no-`else` `if` must be stack-neutral, `i`/`j`/`k` are reserved loop
  words. The rules now live in `docs/forth.md`; once known, later ports needed
  ~zero checker iterations. The win is verification, not authoring speed.
- **A checker driver must never execute candidate code - reuse the verify
  path, not the load path:** the all-errors CLI rewire nearly shipped on
  `evaluate` (the native MULTI-ERR load), which runs top-level candidate forms
  in the checking process and pollutes the live dictionary (checker-scope
  rollback restores registries, NOT cp/ndict - RBF-POP). The crash-immune
  reuse point is VERIFY:SOURCE-BUF (parse+check, zero execution) with the
  multi-error continue decision inside VERIFY-BODY. Also: checker.f words are
  NOT registry-published to later checked loads (MULTI-ERR? rejected at the
  engine rebuild despite being dictionary-visible) - checker-internal access
  from tools rides small documented TRUSTED: one-liners, same class as
  CHECK-BODY.
- **Advisory soundness findings rot — and can be born rotten:** the prop-test
  metamorphic ROUNDTRIP amplifier was 100% inconsistent from its introduction
  (REND-SIG's "just-checked effect" contract is destroyed by CHECK's own
  certify epilogue: CHECKER-USIG-CERT-ADD → USIG-ADD → NEW wipes BROW/DCUR,
  checker.f:5782/3338/1137) yet the gate stayed green for its whole life
  because inconsistencies were counted "(logged, non-fatal)" and shards mute
  output. A property tester that prints findings and exits 0 is error masking;
  the counters must be fatal at the summary. And a 100% failure rate on a
  metamorphic leg means the HARNESS CONTRACT is broken (one root cause), not N
  distinct checker misses — probe the contract word directly (CHECK! then
  REND-SIG on one line) before shrinking N "different" cases.
- **The emitted stage prefix has two check regions - retire textual asserts
  only against a real check that covers the same region:** in the generated
  stage source the checker-boot (util/structures/checker/render) plus
  asm/icode/mnem load inside the `BFR-CHECK-OFF .. ' HOOK set-check` window
  (unchecked at stage-compile time), while habu1/prof/regalloc/jit/habu2 load
  with `HOOK` on (blocking-checked). The habu1/habu2 typed-shape preflight
  asserts retired against the stage compile; icode's retired only once
  BF-CERTIFY flipped blocking (the static scan checks the whole emitted source,
  set-check windows included). Kept: same-type codegen-role asserts
  (`LABEL@ B,` vs `@ B ;`; `SZA-I @ +` vs `over +`) no checker can express
  (dot habu-preflight-codegen-role), and icode's mmap-fail-closed +
  no-static-allot executable-memory invariants. Prove the replacement with a
  negative fixture (emitter underflow -> `BF-CERTIFY-RC` 70), not by trusting
  the compile. (Ported from fable c33ec3e66479.)
- **Re-emitting the same source across build stages is a boot-reload TOCTOU:**
  `build-fixpoint` re-reads the boot-prefix files for the stage2, stdin, and snap
  emissions and the stamp-key re-emit; a mid-build source edit would let stage N
  build from one revision and N+1 from another, silently entering the installed
  image. `BF-PIN` pins each emitted path's content digest (keyed by SHA-256 of
  the path) on first read via the single `BF-APPEND-SOURCE` choke point and
  re-verifies on every reload, so there is no separate manifest to drift and a
  mismatch throws `E-BUILD-BOOT-DRIFT`. Baking the digest for boot-*time* reload
  verification is a separate engine change (dot habu-boot-pin-bake). (Ported
  from fable eb9ee4631166.)
- **The dot ledger drifts from head — audit before assigning:** the 2026-07-06
  sweep of 129 open dots found 6 already fully landed+proven (object/linker epic
  parent + 5 others), 10 with stale premises ("no tiled GEMM" when cg-matmul.f is
  device-proven; counts off by 30-50 rows), and 3 TRUSTED.md rows owned by
  archived dots (`trusted-inventory -- strict` red on DOT-EXISTS?, invisible to
  the gate because the gate suite runs fixtures, not live strict). Rules: verify
  a dot's claim against head before working it; `dot off` only after checking
  `rg <id> TRUSTED.md` and reassigning owner rows; suite `rc 0` is NOT proof —
  engine-suite standalone exits 0 after checker errors (drop-to-REPL masks), so
  the last-line `ok` marker or the full gate is the signal.

## VCS

- **Diff gates must scan locals:** typed definitions are not enough; added `{:
  ... :}` groups need typed locals unless they are the documented
  role-preserving exception from `docs/forth.md`. Use the Habu-native
  `tools/typed-local-diff-lint.f`; manual `rg` scans are too easy to skip or
  misread.
- **Repo edits go through patches/Edit:** even mechanical/one-line changes, so
  broad rewrites, duplicate defs, and shell-expanded capture groups stay reviewable.
- **Commits are proof checkpoints:** run the path-specific Habu gate before
  `jj commit`; a skipped or failed gate stays uncommitted or becomes a blocker
  dot, never a "commit now, fix later" checkpoint.
- **Don't parallelize VCS status:** `jj st` and `git status` race on
  `.git/index.lock`; run index-touching Git/JJ commands sequentially.
- **Pushes reject conflicted ancestors:** a clean worktree + green gate don't clear
  jj conflict metadata; check the pushed range for `conflict` and resolve the
  earliest one first.
- **History filters must include JJ refs:** after removing generated outputs,
  verify `git rev-list --objects --all`, not just `master`; colocated JJ keeps
  blobs via `refs/jj/keep` and `.jjconflict-*`. Ignore generated output by
  shape (JSON/JSONL/logs/reports), not run name, so reruns can't enter a change.

## Code Quality

- **Stack-snapshot DSL is shared:** `lib/test/snap.f` owns `T{ ... -> ... }T`
  (TRUSTED only for the depth/drain core; counts, stores, judge, and report are
  checked and aggregate into the `T-*` counters). Test files require
  `lib/test.f` instead of copying the trusted boundary.
- **`.` ends the line in Habu:** `value .` inside a message splits it across
  lines; inline counters/failure text need digit emitters (`GT-U-TYPE`,
  `TS-N.`).
- **Line effects must earn their place:** `maki/optim-tensor.f` needed comments
  around the non-obvious `ADAM` return order, but empty line-end stack comments
  are noise. Prefer smaller words; add body-line effects only where they prevent
  real stack-state reconstruction.
- **Bare-local lint markers are per locals group:** `typed-local-diff-lint`
  clears its allow state at each `:}`. In `bootstrap/cg/*.fs`, stock Gforth
  forces bare locals, so every changed `{: ... :}` group needs its own
  `typed-local-lint: allow-bare-local` marker.
- **`cell+` loops are not a pointer idiom, they are O(index):** `lib/vector.f`
  `VEC-CELL-FIELD` computed slot addresses with `off 0 ?do cell+ loop`, making
  every access O(index) and a 141k `VEC-PUSH` fill quadratic (63.9s). The
  checker already models `ptr a n + -> ptr a`, so `off cells +` is the
  constant-time checked idiom (same fill: 0.39s). Guard container hot paths
  with a large-N functional fill in the owning budgeted test, not wall-clock
  asserts.

## Runtime, Codegen, AOT

- **Measure `__text` from the emit cursor, not gross size deltas.** `ASM-LEN`
  (`src/arch/arm64/icode.f`) is the exact byte offset into the `CODE` buffer that
  becomes `__text`; a zero-byte `SZ` probe after each `EMIT-*` in `EMIT-FORTH`
  gives a byte-exact region map (proof it perturbs nothing: the final probe reads
  the true `__text` size). This falsified two "+16 KB feature" correlations: the
  dict-hash (HIDX) code is <1 KB of `__text` (its table is a runtime mmap; only
  148 seed dict records are baked, not the 2099 runtime words), and a "-16 KB"
  from removing it is a page-rounding artifact, not real code. Full map in
  `docs/size-rca.md`. Rule: the map decides which lever pays; never remove a
  wanted feature on an unmeasured delta.
- **The 35% `__text` elephant was a 4x-inlined cold-prefix, not features.**
  `EMIT-SOURCE` emitted the checker/stdlib provided-files prefix (`EMIT-COLD-PREFIX`
  - `PFX-LOAD-SCRIPT-ARGV-COLD` + `PFX-PROVIDE-FILES`) inline at four source-entry
  points (~9.6 KB each = 39568 B). `PFX-PROVIDE-FILES` also built `s" <path>"
  provided\n` for ~19 files CHARACTER-BY-CHARACTER (~36 B/char). Fix, same as the
  escape-decoder BL precedent: factor the trio behind `LCOLDPFX` (one BL, save x30
  across internal calls) and the per-row append behind leaf `LAPPPROV` (x12=str,
  no BL so no frame); branch over both so the fall-through startup skips them.
  148855 -> 99319 (-33%), byte-for-byte fixpoint held. Sole lever; dict schema
  (7156 B, load-bearing) and tree-shake (unsound with REPL source embedded) don't.
- **AOT-REPL M2: the metabuild host and `bin/hb` are DIFFERENT engine shapes —
  you cannot capture the REPL in the host and base-rebase it in.** `EMIT-SOURCE`
  (habu2.f:781) branches `STDIN? @ IF C-SOURCE-STDIN ELSE C-SOURCE-BAKED`. The
  metabuild host that runs `stdin.f GO` (hb-stdin-mk, and hb-stage) is
  `STDIN?=false` = a BIG C-SOURCE-BAKED engine (585 KB `__text`); `bin/hb` is
  `STDIN?=true` (stdin.f:86) = a SMALL C-SOURCE-STDIN engine (113 KB) carrying
  the ~39 KB PFX-load-from-checkout + tty-REPL machinery the host never emits.
  Measured: host vs bin/hb engine code is 6.7% byte-identical, chunk shifts are
  non-uniform (40 chunks at -0x9960, 39 not found). So primitive bodies sit at
  unrelated offsets and every word call is an absolute `movz/movk/movk x16; blr`
  (habu2.f:100) — LSNAPRBC (habu2.f:2475) only base-rebases ONE detect range by
  one delta and skips region VAs, so it CANNOT map host→target offsets nor remap
  region-word calls across different region layouts. M1 worked ONLY because
  `AOT-PROBE 12345` has zero calls. Correct fix: capture in a SMALL STDIN?=true
  engine (region = [cold-prefix][REPL] at bin/hb's offsets) via a dump→rebake
  build pass; then only __text ASLR rebase is needed. Two SMALL engines are
  offset-stable across LSRC edits (verified: 100% identical engine code). Detail
  - evidence in dot habu-decide-unbake-repl-735b1565.md "M2 BLOCKED".
- **`evaluate` is a transactional throw boundary now (Design-Y):** a throw whose
  handler is beyond an active `evaluate` boundary (default `HOOK` `throw`s 70 on a
  checker-rejected `:`) rolls back each escaped eval frame (INP/INE/CP/NDICT/XDS/DP
  - compile-state, `EVALERR-CELL`=code, `EVALD`--) then reaches the handler.
  `BTHROW` branches via `EVALREC-CELL` to `LEVALREC` when `EVALD>0`. Preserve ANS:
  throws still reach an outer `catch`; do NOT make `evaluate` swallow throws
  (Design-X) — the `TTHROWSQ`/`catch` harness relies on a throw crossing
  `evaluate`. In-process negatives: `[: bad evaluate ;] catch` or `evaluate`+`ERR@`.
- **New engine DATA cells must audit LIBRARY offsets, not just layout.f:** placed
  `EVALREC-CELL` at `$3A00` thinking `$3A00..$3C88` was free per layout.f — but
  `lib/ffi-abi.f` claims that whole block (`FFI-BUF-OFF $3A00` … `FFI-KPARAM#-OFF
  $3C80`) and `lib/task.f` grows `TASK-USER-BASE` up from `$3D00`. An FFI call
  overwrote the cell, so a throw crossing `evaluate` in any FFI-using program
  (i.e. run under `include`, EVALD>0) branched to a data address (SIGSEGV). The
  only free engine cell is the `$3CA0..$3D00` gap. Grep `constant .*(OFF|CELL|BASE)`
  across `src/ lib/` before claiming a DATA offset; regression:
  `FFI-T-EVALREC-DISJOINT` in `lib/ffi-abi-test.f`.
- **Top-level escaped literals corrupt positionally:** interpret-mode `S\"`/
  `.\"` can yield an empty/garbage span at a load-composition-dependent
  position (repro: `--load` a file requiring `lib/test/snap.f` +
  `lib/test/suite.f`, then a probe with several long top-level `S\"` lines —
  one prints empty; consumed by a byte loop it segfaults in `c@`). Compiled
  escaped literals inside `:` definitions are stable — bind expected strings
  in checked words. Engine dot: habu-interpret-mode-escaped-d8dad34b.
- **Warm snapshots must not re-run the cold prefix:** restored images already
  carry support words/signatures. Mark snapshot boot (`SNAP-CELL`) and skip the
  source prefix, or reloaded core words can shadow warm support words and bind
  trusted signatures to the wrong runtime code.
- **Snapshot persistent stores before image write:** compact checker stores
  (`USIGS`, `NORET`) back into snapshotted boot arrays and reset only transient
  token buffers. Never snapshot pointers into build-time anonymous mappings.
- **Instruction patching flushes the patched line, not `[addr, CP)`:** `patch32`
  can patch at `cp@`; range flushing through `CP` is empty there and leaves stale
  instructions at cache-line boundaries.
- **Snapshot images relocate only engine-text refs:** fixed VAs keep dict/data
  addresses valid; rebase engine-text call chains + seed-prim slots, restore live
  per-boot cells (argv/envp, RBASE, S0, wordlist, hook) after copying. Accept a
  trailer only when `region-len + data-len` ends exactly at the trailer offset
  (magic also appears in code). Output paths use guarded `PATH0` buffers (a small
  buffer overwrites trailer magic → silent cold image).
- **`evaluate` is re-entrant state:** `B-EVAL` saves INP/INE, SP, XDS, CP, NDICT,
  DP, return addr, then branches to the interpreter top; clean end keeps defs,
  error restores compile state and sets `EVALERR`.
- **Fixed DATA cells require layout audits:** placing `EVAL-FRAME` in a free-looking
  gap overlapped regalloc tables → invalid ARM64 fields. Centralize fixed header
  offsets, verify non-overlap. x20 is the engine DATA base, not scratch — no
  runtime/debug helper reuses it before the next `DATA` access.
- **Runtime spans use OS-backed memory, not bigger DATA caps:** static dict storage
  can use DATA; runtime source/report/JSONL/capture buffers size from measured
  bytes via `MEM-ALLOC-64K-SPAN`/`lib/memory.f`. Derive the live base through
  `data-base`, never a duplicated numeric DATA address (a gen-1 stage runs old
  startup while interpreting new source).
- **Warm composition finds hidden DATA buffers:** `diag-origin`, `signature-lint`,
  `trust-lint`, and `icode` failed only when loaded together. Runtime file
  buffers, scan tables, assembler code, and fixup tables belong in OS mappings;
  keep dictionary DATA for persistent definitions and small cells.
- **Warm JSON parser storage is runtime scratch:** `tools/json.f` parse tables,
  string storage, and writer/error buffers are part of the warm checker prefix;
  keep them in lazy `lib/memory.f` mappings or the 67 KiB warm trust sidecar can
  fail before diagnostics run.
- **Batched warm tools expose static scratch debt:** after xref/checker state
  growth, static 256 KiB buffers in `build-fixpoint`, `hb-build`, `repair-packet`,
  `json-only`, `repair-schema-doc-test`, and `stdlib-manifest-test` failed as
  silent rc 76 under warm suites. Large source, capture, doc, and JSON buffers use
  lazy `lib/memory.f` mappings; update the owning load list, not the batch shape.
- **Code/dict region guards (a lone `:` failing closed):** check `cp@ dbase@ -`.
  At/past `REGION - $4000` → code-region guard (remove one-use compiled fixtures,
  then grow `REGION`). Well below + rc 77 → dictionary guard: grow `DICT-CAP`,
  `CFSTK-OFF`, `DICT-SIZE` together (native + bootstrap mirrors), then recheck code
  headroom (larger `DICT-SIZE` moves code up). Undefined tokens near a huge word =
  BODYBUF pressure: factor the colon body into section words. Capacity bumps are
  layout work — prefer removing one-use signatures first.
- **AOT compaction needs an old→new map:** removing call-stencil padding is safe
  only when every PC-relative source (`B/BL`, cond branches, `ADR`) is remapped and
  range-checked; keep mapper cursors separate from copy cursors. Capability checks
  need a source-level pass (inlined data-space prims like `here` vanish from
  closure scans). Keep the >256-word closure fixture above the old threshold.
- **JIT inliner must reject PC-relative branches:** byte-copying a small prim body
  with an internal branch preserves the old target (compiled `epoch-seconds`
  branched back into the seed prim, returned 0). Branch-bearing bodies compile as
  calls unless relocated.
- **Registry/name growth fails closed at build time:** process prims overflowed the
  96-entry seed registry and corrupted the stage image; keep `REG-PRIM`
  capacities named/checked. `EMIT-DICT` encodes names > `DNAME-INL` out-of-line
  (inline-only corrupts `DREC`). Dictionary names are strings with flags above the
  length field, one decode path (low-bit flags recreate a 255-byte cap).
- **Emitter helpers called from several C-\* words are text multipliers:** the
  escape decoder (`C-ESC-DECODE-BASIC` + `C-ESC-HEX-X9`) expanded inline into
  the scan and copy loops of all six quote handlers - ~4.1KB of duplicate
  engine text. Emitting it once behind forward labels (`EMIT-ESC-DECODE`,
  `LESCDEC`/`LESCHEX`, x9 in -> x9 out + x10 class flag, callers test x10)
  shaved 3308 text bytes and dropped a whole 16KB `__TEXT` page
  (`otool -l` text end vs page boundary tells you if a shave pays). Callers
  must re-audit register liveness across the new `BL`: the scan count and copy
  length lived in x10, exactly the flag register, so scan counts in x11 and
  copy saves x10 with the SP idiom.
- **Factor emitters before growing them:** `EM-COMPILE` is near the body-capture
  limit; new compile paths become named emitter helpers with TRUST rows (a
  body-capacity exit during self-build is a structure signal). Label locals must
  match label values exactly (an extra name consumes stale generator state and
  crashes later). Values surviving an emitted `BL` need explicit stack saves or a
  documented callee-preserved contract — `EMIT-BCAP` clobbers `x16`, so a quote
  start kept there made the maker copy from `0x40`.
- **Dispatch factoring starts with semantic groups:** split long native/recovery
  compiler dispatch chains into checked helper groups before adding a row DSL;
  source-shape tests guard helper presence, call graph, and removal of old chains.
- **Emitter punctuation caused stage2 false trails:** a dropped comma changes the
  token (`BL,` vs `BL`) and surfaced only as a terse undefined-token exit in
  generated stage2; the durable emitter-source rule now lives in `docs/forth.md`.
- **Mirrored codegen lands twice:** `src/habu/` and `bootstrap/cg/` are one
  contract, two sources — factor both, prove native fixpoint + Gforth recovery.
  They may use different assembler-word vocabularies for the same instructions;
  preserve each file's opcode spelling and run the native engine gate + bootstrap
  codegen tests. Move-wide (`LVMOVK`) refactors need compiled-literal cases (zero,
  all-ones, MOVZ/MOVK, MOVN/MOVK) — top-level parser checks don't prove the JIT
  materializer. Recovery seed prims drift unless grouped + bootstrapped (a native
  fixpoint alone doesn't prove no-binary recovery). Bootstrap metadata parsing
  must fail closed (`BODY-ARITY`/`EFFECT-FLAGS` rethrow under the diagnostic
  boundary, never default to arity 1 / flags 0).
- **Encoder factoring needs value fixtures + shape guards:** add executable
  fixtures with known instruction words for every encoder touched; source-shape
  scans only prove the file stayed factored.
- **Target image byte cursors are one contract:** ELF, Mach-O, signing share
  `MBUF`/cursor/endian-patch vocabulary under executable fixtures + shape guards;
  target files hold only format-layout policy.
- **Trust-call emitters name the protocol:** push one data cell, push the signature
  pair, call x11 with LR saved — not raw inline `G-PUSH`/`BLR`. `TRUSTED:` bodies
  don't take locals reliably; factor checked helpers and keep the body to the
  asserted primitive op (parsing stops at `{:` otherwise). Trust audit dates follow
  the gate's epoch/UTC day (near local midnight a local date can be "future").
- **Useful register lint needs contracts:** model callee returns/preserves,
  no-return exits, routine boundaries, syscall clobbers, LR/SP; raw
  write-before-BL/read-after-BL rules are mostly false.
- **Split open vs read failure labels:** `EMIT-SOURCE-READ` shared one label;
  on open failure x12 still held the path → `close` on a non-fd. Distinct labels
  for no-resource-acquired vs acquired, even with the same exit code. Scratch cells
  are caller state — helpers don't reuse a caller's loop-index var (`SOURCE-LINE-END`
  clobbered the outer `SOURCE-I`); branch paths preserve loop indexes.

## Runtime And REPL

- **A word that `parse-name`s twice hangs under `bin/hb file.f`, not `--load`:**
  the new `TEST:GROUP SEQ|PARA name` opener reads two tokens; single-script mode
  loops on the second `parse-name`, but `--load`/`included` (how the gate runs)
  are fine. Test DSL parsing words through `--load`, and drive rejection paths by
  calling the factored string validators (`MODE-OF`/`CHECK-NAME`) on `s"` literals
  under `TTHROWS`, not by feeding tokens to the parser at top level.
- **DSL keyword names collide case-insensitively:** `TEST:GROUP` rejects a group
  literally named `seq`/`SEQ` (and `PARA`, `GROUP`, `SUITE`, …) with `E-SUITE-NAME`
  because names are matched `STR=CI` against the reserved set. The suite-test group
  `seq` became `seq-grp` after the rename. Pick group labels outside the reserved
  keyword set.
- **Process capture lifecycle has one owner:** keep fd setup, nonblocking
  probe/drain, stdin write, timeout poll, cleanup, and finish in `lib/process.f`;
  argv/env/cwd layers prepare state only. Duplication made every capture variant
  a stack-juggling audit.
- **Linux spawn needs an exec-failure handshake:** `clone` success ≠ `execve`
  success. `PROC-SPAWN-IO` uses a close-on-exec error pipe: child writes one byte before
  exiting on `chdir`/`dup2`/`execve` failure; parent reads EOF = success, or reads
  the byte, reaps, returns `-1`. Copy the fd to x0 before reusing that register for
  the marker byte. Else checked `PROC-SPAWN-IO` returns a pid for a missing exe.
- **Baked REPL needs explicit hook boundaries:** the snapshot prepends `0 set-check`
  then reinstalls `HOOK` before user input. The tty bundle emits `0 set-check`
  before `repl-term.f`/`repl.f`/watch/stepper/debug and then only `' HOOK
  set-check`; defining a second `HB-CHECK-HOOK` collides once explicit
  duplicate-definition enforcement is active. Core fixtures (`src/core/sha256.f`)
  don't disable checking themselves; put `0 set-check` in the harness.
- **`--load` leaves stdin as tool data:** so a post-load probe piped to fd0 doesn't
  run — put capacity probes in an explicit loaded source file when measuring
  `here`/metadata. Test load lists factor into `TEST:SUITE … TEST:;SUITE`
  blocks with short lines (long physical lines hit the reader buffer).
- **PTY behavior needs a real pty harness:** `script(1)` interleaves echo/output;
  drive a pty directly and poll for exit when testing prompt, raw mode, history,
  Ctrl-C/Ctrl-D, async termination.
- **`close` is `( fd -- )`, no status:** modeling `( fd -- rc )` leaves a phantom
  cell rejected at branch joins; for atomic output write a temp file and commit
  with fallible filesystem words.
- **Native crashes need debugger state first:** for SIGSEGV/SIGILL/DATA/stack
  corruption set breakpoints, step, inspect data-stack + watch cells; extend the
  debugger before print-marker probes. Stale binaries hide fixes — remove output
  artifacts before generate-then-run tests, never silence the build.
- **Stdin/Forth byte fixtures:** `s" …\n"` keeps the literal `\` and `n`; use a
  byte buffer (`10 c,`/`STR-LF`) for newlines. `\\` is a different token from the
  comment word `\`. Checked helpers are defined before use (a forward ref can
  surface later as an unpublished word).

## Darwin And Syscalls

- **Raw Darwin syscalls are not libc:** `posix_spawn` (244) takes the private 5-arg
  kernel ABI `pid*, path, adesc, argv, envp`; `wait4` status is `(status>>8)&0xff`;
  check carry + errno-in-x0. Initialize ALL expected zero/null arg registers before
  `svc` (`gettimeofday` returned `EFAULT` from stale x2 after a `write`). Syscall
  output buffers use audited DATA scratch, not live `sp`.
- **Process redirection uses XNU spawn descriptors:** empty file-action blobs are
  invalid (pass null when no remap); mark parent-only pipe/pty fds close-on-exec
  before spawning. PTY = `/dev/ptmx` + ioctl, not `forkpty`/`openpty`.
- **`F_SETNOSIGPIPE` still needs normalized failure:** closed-pipe `write` reports
  `EPIPE=32` which can collide with a valid byte count; normalize carry in the
  syscall prim, validate counts, keep a 32-byte closed-pipe regression.
- **Darwin time is not a syscall:** `clock_gettime*`/`mach_absolute_time` are
  libSystem/commpage; the no-libSystem clock reads `CNTVCT_EL0`/`CNTFRQ_EL0` and
  converts via quotient/remainder (avoid `ticks*1e9` overflow).
- **LC_MAIN gets argc/argv/envp in x0-x2** (no kernel start-stack); capture at entry
  and restore after snapshot boot.
- **Recursive dir walks need per-depth buffers:** `getdirentries64` records are
  batch-local; index dirent buffers, offsets, lengths, cookies, fds by depth — even
  a global current-record pointer is unsafe (the child walk overwrites it).
- **Share filesystem traversal mechanics, not policy:** `WALK-FILES` skips repo
  metadata while `REMOVE-TREE` must delete `.dots` and unlink symlinks first.
  Factor open/read/record/child-path/close helpers in `lib/fs.f`; keep deletion
  choices in `lib/fs-mutate.f`.
- **Symlink deletion lstats first:** `EXISTS?`/`FILE?`/`DIR?` follow symlinks and
  broken links look absent; `REMOVE-TREE` tests `SYMLINK?` before existence/type.
  Repeatable fixtures use `MAKE-DIRS` or clean the tree before `MAKE-DIR`.
- **Path syscall tests use stable NUL strings:** private path-copy helpers can make
  a primitive smoke test fail with `EFAULT`, hiding whether the ABI or fixture glue
  is wrong. Same-typed string pairs (`ptr u8 n` path vs stdin bytes) need an
  order test — the checker can't distinguish them.

## FFI / GPU (PTX)

- **Relaxing a fail-closed class re-audits every site that leaned on the old
  rejection:** maki/lower-red.f LRED-BODY hardwires input 0 as the FULL data
  operand's row span; only LRED-CLASSIFY-INS's blanket BC-COL rejection kept a
  hand-built broadcast out of position 0. Unlocking BC-COL (stride-1 span,
  2026-07-08) therefore needed a NEW input-0-must-be-FULL guard + negative
  fixture, and exposed that BC-ROW/BC-SCALAR in position 0 were already silently
  mis-loadable. Before widening an accepted-class set, grep every consumer of
  the classification for positional/shape assumptions the old set masked.
- **TF32 mma.sync: prove the fragment layout element-EXACT in isolation FIRST.**
  The PTX-ISA m16n8k8 tf32 layout (gid=lane>>2, t=lane&3: A a0=A[gid][t]
  a1=A[gid+8][t] a2=A[gid][t+4] a3=A[gid+8][t+4]; B b0=B[t][gid] b1=B[t+4][gid];
  D d0=D[gid][2t] d1=D[gid][2t+1] d2=D[gid+8][2t] d3=D[gid+8][2t+1]) is the course's
  #1 "correct in NumPy, garbage on device" bug source. Make it verifiable: feed
  small INTEGER operands (exact in tf32's 10-bit mantissa, sums < 2^24 exact in the
  f32 accumulator) so a correct kernel is BIT-EXACT vs a host matmul — any
  permutation mismatches. `tools/ptx/mma-probe.f` (one tile) then
  `tools/ptx/mma-gemm-check.f` (full K-loop) caught nothing only because the layout
  was right; the point is they COULD.
- **A correct mma.sync tile does not mean a fast GEMM.** The step-3 MMA kernel
  (`lib/ptx/cg-mma.f`, 8 warps / 16×32 warp tile / manual scalar `ld.shared`
  fragment loads, reusing the f32 path's cp.async double-buffer) is device-correct
  and on the tensor-core roof but measured 375–398 GFLOP/s — UNDER the tuned f32
  cp.async tile (442) and ~21% of Triton. ptxas said 38 reg / 0 spill (vs f32's 56),
  i.e. it is load/ALU-bound (bank-conflicting scalar fragment loads + 48 `cvt.rna.
  tf32`/tile starving the MMA units), not register-bound. Reuse→stage→pipeline: the
  landed rung is the tile+license; ldmatrix / 16×64 warp tile / bigger BK / bank
  swizzle are the measured rungs that follow (dotted).
- **Emit the kernel IN-PROCESS when a runtime request must reach the emitter.**
  `maki/precision-device-test.f` used to spawn a child `bin/hb` to emit the kernel;
  a child re-builds the IR fresh and DROPS the parent's `PREC!` request, so the
  dispatch (`LMM-MMA?` reads `CLASS-MATMUL PREC@`) never saw tf32. In-process
  `PTX-CAPTURE-ON 0 LMM-EMIT PTX-CAPTURE-OFF` (like `gemm-bench.f`) sees the request
  and emits the mma.sync kernel — the passing tf32 golden is the running license.
- **Share the cp.async pipeline scaffold via a compute QUOTATION, not a copy.**
  `MM-PIPE-KLOOP-WITH ( [ -- ] -- )` emits the double-buffer staging once and
  `execute`s the per-tile compute quotation in the compute slot; the f32 4×4 fma
  (`[: MM-KSTEP-TILE ;]`) and the tf32 MMA tile (`[: MMA-KTILE ;]`) both pass their
  compute through it, so both validate the exact same staging. The quotation stays
  on the stack under the stack-neutral emit words and is consumed at the slot — no
  bare-local needed. Signature type is `[ in -- out ]`, caller is `[: … ;]`.
- **Kernel benchmarks need a generic launch layer:** a SAXPY-shaped profiler
  hides the optimization question. Keep CUDA module/function/param/launch/timing
  in `tools/ptx/bench.f`, keep bytes/FLOPs in the workload layer, and always print
  fused-vs-unfused rows when claiming fusion moved memory traffic.
- **Do not call host launch timing a GPU profile:** CUDA kernel optimization
  evidence needs device elapsed time (`cuEventRecord`/`cuEventElapsedTime`) in a
  `gpu_elapsed_ns` row. Keep host-loop timing separate for launch-overhead work.
- **PTX profile runs must include `ptxas`, not just text tests:** the v4 SAXPY
  text fixture expected `%p21`/`%rd22`, but `CG-OPEN` still declared scalar-sized
  pools (`%p<8>`, `%r<16>`, `%rd<16>`). Benchmarking through checked emit →
  `ptxas` → CUDA launch exposed the stale resource contract; keep assembly in
  perf proof loops.
- **FFI needs almost no new ABI machinery:** an AAPCS64 C-call is a non-leaf
  `FPRIM` (gets the x30 frame) that `G-POP`s fn + arg buffer, loads `x0..x7`,
  `BLR,`, `G-PUSH`es `x0`. `XDS` is `x19` (AAPCS64 callee-saved) so the C callee
  preserves the data stack free. The CUDA Driver surface (except `cuLaunchKernel`'s
  9th–11th stack args) is integer/pointer-only, so the float scalar rides in the
  `kernelParams` buffer — no v-register handling. `ffi-call : ( ptr a n -- n )`,
  fail-closed.
- **Dynamic-ELF FFI is checked emitter code:** `bin/hb` is a dynamic ET_EXEC
  (`PT_INTERP=/lib/ld-linux-aarch64.so.1`, `DT_NEEDED libc.so.6`, SysV `.hash`,
  `R_AARCH64_GLOB_DAT` into a fixed-vaddr GOT, `DF_BIND_NOW`, no PLT) so ld.so
  fills `DLOPEN-SLOT`/`DLSYM-SLOT` (`ptr a`, read with `@`) for `ffi-call`. Place
  the R+W segment (.dynamic+GOT) at a FIXED high vaddr so slot addresses are
  compile-time constants independent of code size. Both the AOT (`BUILD-ELF`) and
  snapshot (`BUILD-SNAP-HDR` + `snap.f`) image paths must go dynamic — the
  self-host fixpoint, not just `BUILD-ELF`, is the acceptance test.
- **no-crt dlopen works:** glibc ld.so initializes libc.so enough that a
  no-startfiles binary (own `_start`, no `__libc_start_main`) can `dlopen` libcuda
  - call `cuInit` (proven on the Orin with `gcc -no-pie -nostartfiles`).
- **`cp@` is only stable inside a compiled word:** to emit a runtime stub (a C-ABI
  leaf for `ffi-call`), write at `cp@` via `patch32` and call from inside `: WORD ;`.
  The interpreter compiles each top-level line into a transient buffer at `cp@`, so
  a top-level `cp@ patch32` clobbers the executing line (the `add` ran, `ret`
  landed in junk → `SIGILL`). Engine-suite proofs pass via `--load` yet `SIGILL`
  via the gate's stdin REPL — always reproduce engine-suite changes through
  `bin/hb --repl < test/engine-suite.f`.
- **Verify emitted primitive bytes statically:** compute the exact ARM64 encodings
  and `grep` the on-disk `bin/hb` for the contiguous byte stream (ASLR slides the
  `[']` xt; file bytes are fixed) — proves codegen without a runtime call.
- **M3 has two proof levels:** `tools/ptx/saxpy.f` emits `.version 8.3`/
  `.target sm_87` PTX and `tools/ptx/ptxas-smoke.f` proves `ptxas` assembly on
  Orin without committing `.ptx`/`.cubin` artifacts. GPU launch + CPU golden is
  not an M3 completion proof until the Habu CUDA Driver harness (M1d) launches
  the cubin; the old `/tmp/saxpy_run.c` C smoke was useful evidence, not repo
  infrastructure.

- **M4a crux — trusted span constructors need FRESH extent-token minting
  (proven 2026-06-27).** M2's parametric types are built and work, but the M4
  constructor `MK-SPAN ( ptr u32 -- span<...,extent-n> )` cannot just use the
  nominal atom `extent-n`: a probe (`TRUSTED: MKS … span<…,extent-n>` twice, fed to
  `NEEDSAME ( span<…,extent-n> span<…,extent-n> -- )`) CERTIFIES (exit 0), so two
  *independent* spans wrongly share `extent-n` — violating ptx-sketch.md ("a lone
  MK-SPAN yields a fresh N that unifies with nothing"). The model split: kernel
  *signatures* use nominal atoms (`extent-r`/`extent-c`, equal-by-name, to ASSERT
  agreement), but *constructors* must mint a fresh extent per call (`MK-SPAN=` is
  the explicit share-one-fresh-token form). The landed design: a field
  **type-variable** (`span<…,e>`) does NOT work — fresh unification
  vars unify freely, so two independent spans could always be unified "equal,"
  which is exactly the unsoundness to prevent. The constructor must mint a
  per-call-fresh **rigid (skolem) extent token** that unifies only with itself;
  `MK-SPAN=` mints one rigid token stamped on both outputs. A kernel needing equal
  extents is polymorphic in the extent and requires the SAME token twice, so
  passing two DISTINCT rigid skolems forces them equal and FAILS (reject), while
  two `MK-SPAN=` outputs share one skolem and pass. This is a genuine checker
  extension (per-call rigid-token minting at trusted constructors), not just word
  signatures. Constructor signatures now use `fresh-extent-*` / `fresh-mask-*`
  templates; `GRID-CTX`/`ROW-CTX` mint fresh masks and the engine gate proves
  both direct and recorded-signature reject paths.

- **Device-vs-host GOLDEN compares device f32 against the f32-NARROWED host, not
  the raw f64 (proven 2026-07-04, `maki/lower-golden.f`).** The host executor runs
  f64, the device kernel f32; comparing device bits to the raw host f64 folds the
  dtype step into the error budget and blows the CAD-PLAN §11 tol (atol 1e-6, rtol
  1e-5). Round the host elem onto the f32 grid first (`F64>F32 F32>F64`), then
  `|dev - host_f32| <= atol + rtol*|host_f32|` measures REAL kernel error (f32
  rounding along the chain + `ex2.approx`), not representation. First real
  device-golden of the CAD plan: a GELU→RELU region matched to 6 decimals on Orin.
- **Generated multi-input elementwise kernels reset the CG counters from the input
  count, not `CG-RESET` (`maki/lower-ew.f`).** `CG-RESET` presets the SAXPY ABI
  (2 spans, `%rd3` free). A region kernel has K input spans + 1 output span, so
  after the param loads set `CG-NRD = K+2`, `CG-NF = 1` (no uniform param), `CG-NR
  = 2` (`%r1` = n, the bound `EMIT-GRID-CTX` branches on). Pass any span reg to
  `EMIT-GRID-CTX` (it ignores the base; the index is from `%tid`), share the one
  returned offset across every `EMIT-LOAD`/`EMIT-STORE`.
- **In-process PTX text tests need a `PTX-L` sink hook, not a subprocess
  (`src/arch/ptx/emit.f`).** The cg emitters flush each line through `PTX-L`, so a
  host test can't inspect their output without capture. A one-variable redirect
  (`PTX-CAPTURE-ON`/`$`/`-OFF`) routes lines into a buffer (fail closed on
  overflow), default stdout unchanged — lets `maki/lower-ew-test.f` assert
  "exactly one `.version`, one entry, `ex2.approx`/`max.f32` present" off-device
  with no `bin/hb` spawn. `.f` emit-driver signatures must use checker type tokens
  (`n`/`ptr`/`bool`), never descriptive names (`node`/`rid`) — those parse as
  unknown types.

## Linux AOT / ELF

- **Linux AOT gates parse ELF structure:** inspect ELF64 program headers and
  validate the executable `PT_LOAD` segment; Mach-O text-size thresholds aren't
  portable constants.
- **Instruction disassemblers read instruction width:** `DISASM` loads one ARM64
  u32 at a time; a u64-load-and-mask can cross a 4-byte mmap fixture end.
- **`die` modeling belongs to the Forth standard:** the Linux AOT failure came
  from treating `0 0` as a fake string and mixing `throw`/process-exit control
  effects; the lasting rule now lives in `docs/forth.md`.

## Diagnostics & Benchmarks

- **Diagnostics are an API:** JSON errors carry `schema_version:1`, source spans,
  verdict, word, token, expected, actual; wrappers keep valid JSON object lines and
  fail nonzero on rejection. Source origins are wrapper-owned (definition-relative
  spans; inject origin markers, keep them out of user bundles). Repair/diagnostic
  rows keep a source-preserving effect field (`R x -- R x`) beside the normalized
  one; `fix_return_stack` only when the data stack already matches (a bad `>r` that
  drops a declared output is `add_producer` first).
- **Lint-infra fixed caps are tree-growth time bombs, and uncaught positive
  throws die silent:** `INTERN-MAX` $200 sat below FILEMAP.md's 513 backticked
  paths, so `filemap-lint` exited rc=76 (`E-LINT-INTERN-CAP`) with ZERO output
  and a bogus downstream FILEMAP-UNLISTED artifact — and a piped gate check
  (`... | tail; echo $?`) masked the rc, reporting green. Same disease shape as
  the rc=77 gate-runner death (dot habu-focused-gate-runner-12b9812a): unlabeled
  capacity exits — engine dict-full 77 there, lint intern-cap 76 here —
  different subsystems; capacity exits must attribute themselves everywhere.
  Fixes: size caps from the real corpus with the driver named in a comment;
  route lint CLIs through `LINT-MAIN` (catch, print `tool: threw <code>
  (<name>)`, re-throw); never read `$?` after a pipeline.
- **Error codes are a global namespace; lint it, don't trust review:** three
  live collisions (E-CUDA/E-FUSE both -5002, E-PTX-READBACK/E-MK-EVAL both
  -5003, E-LMV-NOOUT+E-LMV-REG/E-ABL-NOSUB+E-ABL-CAP at -5210/-5211) plus a
  fresh one (E-MECH-* reusing adjoint's -5100..-5103) all slipped past review
  because no gate proved uniqueness. `tools/error-code-lint.f` now fails any
  negative code claimed by two different E- names (allowing sysexits-style
  positive exit codes, -FIRST/-LAST range sentinels, and exact same-name
  re-registrations). Claim a fresh block in the file header before minting
  codes; keep older widely-used codes stable and renumber the newer claimant.
- **Repair packets are mechanically consumable in checked code:** `DIAG-BUFFER!`
  + `DIAG-JSON!` + `DIAG-ORIGIN! 1 1 0` around `CHECK-CANDIDATE!` capture the
  JSONL diagnostic in-process with candidate-relative `byte_start`/`byte_end`
  (no TRUSTED needed — only `DIAGXT` mutation forces trust). The checker flags
  the LAST surplus producer for `remove_producer`, and an unconsumed-input
  surplus flags `token_index` 0 (the definition-name token) — mechanical editors
  must guard index 0 or they delete the name. `add_producer`/`fix_type` packets
  carry no insertable/replacement token, so a mechanical repairer reports those
  honest UNREPAIRABLE (maki/eval-repair-mech.f; gap dots
  habu-repair-packet-machine-879ad716, habu-repair-packet-typed-62bc5df2).
- **JSON quoting/storage split:** the `s"` quoting rule moved to `docs/forth.md`;
  the infra lesson is that row artifacts exceeded fixed builders, so large
  prompt/response/replay fields need chunked Habu emitters or OS-backed storage.
- **Check phases must be silent:** `hb` can emit checker diagnostics yet exit 0 for
  a loaded file; live drivers treat ANY stdout/stderr from a check-only child as
  rejection. Expected-throw fixtures stay quiet (opt-in row/error reporters at the
  CLI boundary) so negative tests assert throw codes without `FAIL` lines.
- **Core/CLI lint splits need output routing:** standalone lint CLIs may write
  findings to stdout, while `tools/check.f` must surface the same findings on
  stderr. When moving a lint in-process, make the checked core accept/configure
  its output fd and add check-wrapper tests for text and JSON stderr routing.
- **Don't pull unchecked parsers into checked tests:** `tools/json.f` is an
  unchecked `catch` boundary; checked reducers use small checked interface fixtures
  for the checker gate and runtime tests with the real parser until the typed
  `catch` dot lands. Checker-only stubs are contracts — update the stub in the same
  change as any consumed API move (runtime tests with the real module don't prove
  the gate models new constants/effects/codes).
- **`tools/check.f` modes:** `--all-errors` checks top-level defs independently, so
  on bundled stdlib it falsely flags deps as undefined and multiplies gate time —
  run the fast fail-closed bundle check first, all-errors only after failure or
  explicit request; use `--json-errors` for full driver bundles. Don't accidentally
  type-check a CLI main: the runner executes the final source after checking, so
  scripts with a top-level `MAIN` need a no-main fixture or safe argv/env.
- **Benchmark axes stay separate:** trial pass, task pass@k, repair rounds, wall
  time, generated-token cost (a proxy, not hidden reasoning). Reports are
  deterministic (evidence-derived text, no wall-clock stamp; provable with `cmp`
  before archiving outside git). Habu-only vs cross-language claims use distinct
  artifacts (Forth-only run = repair/replay behavior; 600-row array run = "best LLM
  target"). Stale-status lint ignores fenced evidence blocks, keeps prose strict.
- **Stateful scanners split at cursor phases:** `tools/stale-status-lint.f` became
  reviewable when `SS-COUNT-LINE?` delegated cursor advance, digit runs, ratio
  tails, whitespace skipping, and keyword checks to typed helpers. Keep fixtures
  around the boundary behavior: short counts, embedded alnums, partial ratios,
  case-insensitive keywords, fenced blocks, and long files.
- **Live sweeps resume + enforce coverage by identity:** model calls/repairs can
  die before a row; resume on `(model_id, arm, task_id, trial)` without duplicating,
  record expected identities during the run, and fail before report generation if
  any row is missing. Required ablations (structured vs raw vs blind feedback) are
  default with tested opt-out — env-gated arms get silently skipped. Use
  `*-OUTCOME` process APIs (`PROC-OUTCOME>RC`) so a timeout still emits a row, and
  include task/model/arm/trial/child-rc in missing-row errors.
- **Codex benchmark candidates come from `--output-last-message`:** `codex exec
  --json` stdout is event streams (tool output, truncation) for token accounting
  only; read the last-message file as the candidate, and use `--cd` to a clean temp
  dir + clean `CODEX_HOME` + disabled apps/plugins (a smoke prompt fell 29k→11.5k
  input tokens; rows still report output tokens only).
- **Background Codex scouts need explicit stdin/output:** redirect stdin from
  `/dev/null`, pass `--output-last-message /tmp/name.out`; stdout logs alone are
  unreliable evidence.
- **Dogfood benchmark hot paths:** per-call glue (model-response parsing) is
  Habu-native; host parsers hide missing Habu JSON/string/file primitives and
  weaken the LLM-native proof. LLM helper surfaces match validator surfaces — if
  the validator accepts `A-ARGMAX`/`A-PREFIX-SUM!`/`A-RUNMAX!`, the prompt names
  those exact helpers (else it rewards stack gymnastics).
- **Schema-v2 changes touch every fixture + spawner:** rows with
  `diagnostic_count>0` need `repair_class_stats` summing to the row total with
  preserved `first_round`/`first_order`; report fixtures build full rows by hand;
  CLI runner `--load` lists and doc/dot verification commands stay synced when a
  reducer gains a dependency (`expanded-report.f`→`validate-results-lib.f`,
  `tools/json.f`→`lib/memory.f`). Prove with the exact failing child fixture before
  the full gate. Report reducers use dedicated scratch cells (`RR-I/J/K` get
  clobbered by nested helpers); a checked reducer exposed a `RR-RATIO.` stack leak
  that truncated a table — add row-count regressions and `cmp` regenerated reports.
- **Doc contract fixtures need stable anchors:** when `grep -F` gates API prose,
  keep the phrase contiguous or assert a shorter stable substring; line wrapping
  hides a present contract. LLM stdlib examples cite `lib/std.manifest`,
  `docs/stdlib.md`, runnable `examples/` — not invented API.
- **Public-signature rows:** `tools/public-signatures.f` records only
  `: WORD ( in -- out ) …` with locals AFTER the effect comment; checked `lib/*.f`
  defs (even internals) are captured, track as `active` rows. Effect-drift compares
  full normalized tokens (returning on first shared whitespace made `n --` equal
  `n -- n`).
- **Capacity-sensitive bundles:** keep narrow source-shape/fixture DSLs in their own
  small `TEST:SUITE` and task-specific byte/source builders in the owning
  test/driver until multiple users prove shared surface — appending to large
  live-driver bundles surfaces as an unrelated rc 76 capacity failure. Load
  transitive deps in child bundles (`lib/build.f` needs `lib/process.f` for
  `PROC-RUN-RC`). Large native tool bundles (lint tables + `json.f` + big buffers) can
  corrupt reads — lean standalone reader + distinct scratch vars; stream large
  JSONL in chunks, reserve fixed buffers only for bounded summaries.
- **Source-shape checks distinguish code from quoted code:** generated builders may
  contain strings such as `s" 0 set-check"` that are intended runtime source.
  Tests that ban generated check-off lines should match newline-delimited source
  lines, not raw substrings.
- **Build helper fixtures stay split by boundary:** composing bootstrap-codegen,
  warm-image, build-fixpoint, hb-build, and codesign tests in one `hb` image hit
  rc 76 before `hb-build-test.f` started, while each boundary group passed alone.
  Keep `bootstrap-helper-fixtures`, `build-fixpoint-fixtures`, and
  `hb-build-fixtures` separate.
- **Captured gate children need heartbeat polls:** a silent child looks hung if
  `poll` waits the full timeout; gate capture loops poll in heartbeat slices,
  print label-only wait lines, keep child stdout/stderr for failures. Progress
  must exist at every blocking layer — a top-level `PROC-WAIT-RC` blinds the user even
  if children print heartbeats.
- **Gate heartbeat capture has one owner:** `lib/test/runner.f` owns
  progress-aware drain/flush/stdin capture. Gate files set up phases and
  assertions only; direct `PROC-PFD`/poll loops there recreate process debt.
- **Habu-spawned `hb` children need explicit env:** `RUN-ARGV-CAPTURE` doesn't
  inherit env like a shell; use env-aware process APIs + `PROC-ENV-INHERIT-MISSING`.
  Process capture buffers size for isolated jj workspace paths (not the short main
  checkout) or valid forks throw `E-PROC-TRUNCATED`. Split setup vs runtime
  timeouts (`DS-HB-TIMEOUT` vs `DS-RUN-TIMEOUT`) so a checker/build subprocess
  isn't timed out by a runtime-loop budget. Live-row extras (`runtime`) are set
  after `DS-LR-PASS`/`DS-LR-FAIL` (`DS-CONFIG-LR-COMMON` resets row fields).
- **Core/wrapper splits update every child load list:** when a CLI tool gains a
  reusable checked core (`tools/diag-origin-core.f`), update the wrapper tests,
  build drivers, gate suites, benchmark checker builders, docs, and filemap in
  the same change. Missing transitive deps (`lib/errors.f`, `lib/string.f`) showed
  up as child rc 70 before the parent test could explain the failure.
- **Checker-loaded cores borrow scratch buffers:** moving a CLI scanner in-process
  under `tools/check.f` must not duplicate large static file/string buffers.
  `trust-lint-core` initially plus `check.f` buffers corrupted `SCRIPT-ARGC`
  before parsing; expose caller-supplied scratch buffers and let wrappers own
  standalone storage.
- **All-errors keeps definition isolation, not wrapper spawning:** split
  `tools/check-all-errors-core.f` from the CLI and call it in-process from
  `tools/check.f`, but keep the per-definition `bin/hb` children: each generated
  checked definition may fail independently while later diagnostics still need to
  be collected. Borrow checker buffers for capture; wrapper-only static buffers
  stay outside the composed checker bundle.
- **`stage2-src` cap is a builder contract:** AOT maker generation can exceed the
  256 KiB stage2 reader for tiny user source; reproduce with `hb-build` child
  output, raise the named cap deliberately, keep fail-closed overflow.
- **Source scanners and lint cores exposed standards gaps:** substring source-use
  matches, same-type numeric buffer arguments, and unchecked shared lint cores now
  have durable rules in `docs/forth.md`. The remaining lesson is to keep bulky
  exact-token scanner helpers out of base bundles unless their driver needs them.
- **Commit is a gate, not a checkpoint:** the project rule now treats `jj commit`
  as blocked until changed Forth diffs are scanned for definitions and unchecked
  boundaries, exact owning `bin/hb --load ...` paths are checked, and boundary
  tests/dots exist. This prevents "commit first, typecheck later" drift.
- **Gate phase files need their loader prefix:** `test/gate-stdlib.f` is not a
  standalone script; running it without the documented `docs/bootstrap.md`
  `--load` prefix omits `lib/process-env.f` and fails at
  `PROC-ENV-INHERIT-MISSING` before testing changed code. Use the exact native
  gate command for commit evidence.
- **Warm gate runners are harness artifacts:** bake side-effect-free phase libs
  into a runner keyed by the runner source and seed image, then pass
  `HABU_UNDER_TEST` as runtime env for candidate checks. Baking the runner from
  the candidate on the critical path regressed the full gate to 98.469s; starting
  the runner in the early pool keeps the gate at 80.467s.
- **Warm-image tails mirror builder deps and trailer layout:** warm snapshot
  sources that append `src/os/*/{elf,macho}.f` must first append ARM64
  assembler/code-buffer sources and target/shared layout
  (`src/arch/arm64/{asm,icode}.f`, `src/os/<target>/layout.f`,
  `src/habu/layout.f`), or child snapshots fail at `ASM-LEN`/`CODE-OFF`. The
  snapshot trailer must remain the final 40 bytes of the rounded text segment;
  absorb alignment padding into the copied DATA span instead of writing zeros
  after the trailer, or startup misses `SNAP-CELL` and replays the cold prefix.
- **Stage2 source input is runtime scratch:** a refreshed snapshot `bin/hb`
  starts with substantial DATA already occupied by the checked prefix. Keeping
  stage2's `$80000` source buffer as `here ... allot` combines with `CODE` and
  `MBUF` and trips the silent native `DP-CHECK` guard on the second refresh. Use
  the same primitive-facing `$1002` anonymous/private `mmap` convention as
  `lib/memory.f` for that buffer; raw Linux `$22` is for emitted startup syscalls,
  not the public `mmap` primitive.
- **Phase-token and row-sealing fixes moved into the standard:** the `asm`/`img`/
  `snap` ordering and implicit-row underflow lessons are now checker/type-model
  rules in `docs/forth.md`.
- **Emitter hard-hook cutovers keep roles until raw opcodes:** checked public
  helpers should retain nominal roles (`reg`, `ptr bool`, `ptr a`) and erase only
  at raw mnemonic/immediate boundaries (`REG>N`, narrow trusted fixed-VA numeric
  helpers). The CF-ENTRY cutover worked by keeping dispatchers checked and moving
  only raw `execute` into `EM-HXT-EXECUTE`; whole trusted dispatcher bodies with
  scratch cells crashed during `EMIT-FORTH`.
- **Benchmark driver buffers are shared infrastructure:** the LLM drivers route
  raw model text, repair packets, bundle output, and checker captures through
  `DS-*` spans. Keep those spans OS-backed and capacity-checked at the shared
  helpers (`DS-OUT-TEXT!`, `DS-READ-OUT-FILE`, `DS-HB-CAPTURE-MS`) so one arm
  cannot silently bias pass/reject accounting with a local fixed-buffer throw.
- **PTX toolchain proof has two gates:** first, Habu must emit header-complete
  PTX and `ptxas` must assemble it; second, the CUDA Driver harness must launch
  the cubin and compare CPU golden. On `zed`, `ptxas` exists at
  `/usr/local/cuda-12.6/bin/ptxas` but is not on `PATH`.
- **Composable gate helpers cannot own large static buffers:** loading
  `tools/hb-build-lib.f` ahead of `test/gate-build-common.f` tripped the native
  data-space guard at the common executable read buffer before any new checked
  helper ran. When a gate helper may compose with other checked tools, model
  file/image storage as runtime `lib/memory.f` allocation sized from `FILE-SIZE`;
  keep always-resident `create ... allot` buffers for small fixed metadata only.
- **`STATUS.md` verified date is UTC for the gate:** `stale-status-lint` uses the
  native `DATE-NOW` UTC date unless an explicit test date is passed. Do not
  update `Last verified` to the operator's local date during a late-night run;
  the full gate will reject it even if a manual lint with a local date passes.
  Trust audit dates follow the same rule through `trust-lint`.
- **Gate phase cuts must move coverage, not drop it:** AOT-negative was slow
  because it repeated checker/signature/AOT-lint failures through full
  `hb-build` children. Keep one CLI boundary for hb-build-only behavior such as
  closure-limit failure, and move semantic assertions to their owning checked
  tool tests (`check-test`, diagnostics, `aot-lint-test`, `hb-build-test`).
- **Do not spawn assertion tools from fixture loops:** repair-hints spent most
  of its time re-invoking `gate-json-assert` for JSON schema/class checks after
  each checker rejection. Split assertion tools into checked cores plus thin CLI
  entries, load the core in the owning fixture, and keep subprocesses only for
  the boundary under test.
- **Gate dependencies are per phase, not global:** waiting for every warm image
  before launching every worker serialized independent builder long poles behind
  unrelated tool images. Start phases as soon as their own prerequisites exist
  and only export warm-tool env to phases that actually use it.
- **Gate pool defaults need full-load measurement:** isolated slices improved
  when the stdlib nested pool rose from 2 to 4, but the full gate only improved
  after measuring outer-pool contention on the 4-thread Orin. Keep a higher
  max for explicit overrides, but choose the default from documented full-gate
  timings, not from a single slice.
- **Warm-image trust export is a batch artifact:** `public-signatures --trust`
  already accepts many files, so warm-image baking should export all support
  signatures in one child and size the runtime capture buffer for the measured
  batched output. Per-file export children hide startup cost and make warm
  phases look inherently slow.
- **Warm trust preludes expose only standard signatures:** a word like
  `tools/date.f` `PARSE-YMD` is invisible to `public-signatures --trust` when
  its `( in -- out )` comment appears after `{: :}` locals. Warm-loaded CLI
  wrappers then fail before runtime. Keep the effect immediately after the word
  name, and pass cross-image buffers as real `ptr u8` values from
  `MEM-ALLOC-BYTES`, not raw `create` storage.
- **Semantic xref belongs in the image:** when investigating dictionary or call
  ownership, prefer baked native Forth words (`LATEST`, `XREF-FIND`, `XREF`,
  `SEE`, `WORDS`) over source search. If the existing words do not expose the
  needed relationship, extend that in-image surface instead of normalizing text
  search as the semantic path.
- **Snapshot checker state must fit DATA, not only mmap:** `USIGS` can grow
  dynamically while checking, but warm snapshot images must persist the table
  into the DATA-resident boot buffer before exec. When warm-image composition
  grows, size the persisted boot capacity for the supported gate workload and
  rerun the native snapshot/full gate.
- **Typed-local checks happen before validation:** splitting or moving Forth
  source can make old bare locals look newly introduced. Run the diff locals
  scan before focused tests, annotate concrete roles (`n`, `len`, etc.), and
  let `bin/hb` reject wrong annotations instead of treating the move as legacy.
- **Diff lint must stream large patches:** `typed-local-diff-lint` read the diff
  dynamically but still fed all lines through the capped shared `SPLIT-LINES`
  table. Commit gates should process patch lines in place and keep a fixture
  above the old 1024-line limit.
- **Keep global warm images lean:** baking large one-off tool cores into the
  shared warm image can leave too little dictionary/data headroom for tools that
  are loaded later. Prefer explicit multi-file warm loads for the boundary that
  needs them, and measure cached child time separately from warm build/cleanup.
- **Do not bake verifier cores into the gate runner for one slice:** adding
  `src/habu/verify-source.f` to `TR-RUNNER-SUPPORT-FILES` overflowed the warm
  runner's persisted checker signature table (`checker: user sigs snapshot too
  large`). Batch dictionary checker cases through the existing checker warm image
  unless the shared runner really needs the verifier resident.
- **GPU readback args are dst-then-src; verify the golden by hand:** maki AXPY on
  the Orin produced `y[0]` unchanged because `cuMemcpyDtoH_v2` was called as
  `(srcDevice dstHost n)` — it is `(dstHost srcDevice n)`. The kernel was correct
  the whole time; only readback failed. A per-call `rc` print localized it (all
  rc=0 except `dtoh=1`). Separately, the test's own golden was wrong: `2*4+40=48`,
  not 44 — the GPU was right, the assertion was the bug. Compute goldens, never eyeball.
- **A retained CUDA primary context hangs at process exit, not in the kernel:**
  the emitted SOFTMAX-ROWS "hung" — but stderr markers (fd 2, unbuffered syscall)
  showed `cuCtxSynchronize` RETURNED; the stall was teardown with a live
  `cuDevicePrimaryCtxRetain` + loaded module. Adding `cuModuleUnload` +
  `cuDevicePrimaryCtxRelease` before `bye` exits clean. Buffered stdout (and any
  `tr`/`tail` in the pipe) is lost on a hang — localize with fd-2 markers to a file.
- **PTX collectives need op-local inactive identity:** `lib/ptx/cg-collective.f`
  now writes `active ? tile : identity(op)` before the shared-memory fold, so
  `BLOCK-MAX` gets -inf and `BLOCK-SUM` gets 0 regardless of how `ROW-LOAD` seeded
  inactive lanes. Direct row-sum/backward paths cannot rely on softmax's accidental
  `EXP(-inf)=0` behavior.
- **Stack-signature tokens must be TYPES, not role words:** `( got expected -- bool )`
  silently binds `got`/`expected` as fresh type vars, so a downstream `n n` op
  mismatches with a confusing `at '<='`. Use `( n n -- bool )`. Locals `{: got :}`
  may use any name; the `( ... )` stack sig may not.
- **lib/ subdirs are research sub-libraries, not flat stdlib:** the stdlib manifest
  grammar (`SMT-LIB-FILE?`) only admits flat `lib/<module>.f`, but the coverage
  walk recursed and demanded module rows for `lib/ptx/*.f` it could not express.
  Gate `SMT-COLLECT-LIB-FILE` on `SMT-LIB-FILE?` so coverage tracks only flat
  modules; nested dirs (`lib/ptx/`) stay trust-audited + `-test.f` + gate-covered
  but out of the curated public-API manifest (mirrors top-level `maki/`).
- **Warm-image tests must use the warm form, not re-`--load` bundled libs:**
  `repair-packet-test`'s no-args case reloaded `lib/... repair-packet.f`
  through the warm image — recompiling libs the warm binary already has (~3s
  uncontended vs ~0.5s warm
  form). Under ~14-way pool concurrency it blew past the 10s timeout, so the
  subprocess came back `TIMEOUT` not `EXIT`: the tell was assertions on `kind`/
  `code`/stderr failing while the `stdout==0` assertion passed. Route every warm
  subprocess through `CLI-TOOLS-LOAD`.
- **DATA layout overlaps can masquerade as checker hangs:** package runtime cells
  at `$268..$280` overlapped the JIT virtual-stack value array (`VVAL-OFF=$250`);
  a fifth literal `10` wrote `$270`, poisoned `PKG-PRI-CELL`, and sent lookup into
  bogus wordlist state. Relocate persistent cells outside JIT scratch ranges and
  gate with a five-literal compiler regression.
- **Device-FFI "launch hang" RCA: write markers to a FILE, not piped stdout.** A
  device gradcheck (`tools/ptx/gradcheck.f`) looked like it hung mid-launch, but
  piped stdout block-buffers so `type cr` progress markers were lost on the
  timeout-kill. File-output markers showed every cuda call returned — the hang was
  at process EXIT from a retained primary context never released (the line-169
  gotcha). Always pair `cuDevicePrimaryCtxRetain` with `cuModuleUnload` +
  `cuDevicePrimaryCtxRelease` before exit (a `GC-FINI`); `acc-device-test.f` works
  only because it does.
- **All-errors checks should be scoped, not subprocessed:** per-definition
  `bin/hb --load ...` gave correct stderr isolation but dominated the tool gate.
  Use a checker registry scope plus diagnostic-buffer capture for normal
  duplicate-definition semantics, and reserve candidate scope only for generated
  candidate checks where shadowing loaded libraries is intentional.
- **Tool fixtures need entry/lib splits:** if a `*-test.f` validates semantic JSON
  or text output by spawning `bin/hb --load ... tool.f`, it rebuilds the same core
  for every case. Put reusable logic and buffered output in `*-core.f`, leave
  `tool.f` as a thin `MAIN` entry, and keep only one CLI smoke for argv/wrapper
  behavior.
- **Evaluate frames must live outside compiler scratch:** `EVAL-FRAME=$280`
  overlapped `VVAL-OFF=$250` and `SNAPSTK-OFF=$360`; compiling a seven-argument
  call inside `evaluate` overwrote the saved outer `INP` and crashed on the next
  token. Keep re-entrant evaluator frames above scratch and below `DATA-START`,
  with an engine-suite high-arity `evaluate` regression.
- **Create persistent gate roots before content-key hashing:** per-file digest
  cache uses the gate content root's `content-key.cache`, so the gate must create
  the persistent root before any `CK-FILE+` key construction, including
  under-test cache restore.
- **Build-fixpoint child helpers own argv state:** warm gate images reuse one
  process across phases, so stale `PROC-ARGV` entries from a prior tool command
  can leak into `BF-RUN-LOAD-STAGE` and run the wrong child entry. Every `BF-*`
  spawn helper resets argv before preparing its own stage/exe command.
- **Keep FFI ABI separate from loader binding:** AAPCS64 marshalling, out-params,
  and `void** kernelParams` are portable and gateable on every host; `DLOPEN`/
  `DLSYM` depend on OS image support (`DLOPEN-SLOT`/`DLSYM-SLOT` on dynamic ELF).
  Put portable helpers in `lib/ffi-abi.f`, keep `lib/ffi.f` as the loader layer,
  and invoke gate slices through their wrapper so includes like `content-key.f`
  are owned in one place.
- **Spawn command builders should return a checked summary:** a helper that mutates
  global `PROC-ARGV`/`PROC-ENV` and then immediately spawns can fail invisibly if
  the caller never observes the prepared state. Return the prepared argc (or an
  equivalent checked summary) and assert it before `PROC-ARGV-PREPARE`; the
  build-fixpoint fixture caught an empty child invocation this way.
- **Suite timing RCA separates cache-fill from steady state:** after a native
  spawn/harness rebuild, the full suite measured 65.093s internal with artifact
  misses/candidate install before the early-lint scheduler fix, 41.551s internal
  with one artifact refill after the fix, then 30.123s internal steady-state with
  candidate-hit=1. Treat cache-fill as budget coverage and persistent-cache runs
  as the harness architecture number; do not compare cache-fill to prior
  steady-state runs.
- **Early lint phases still need build quiescence:** `lint-manifest` and
  `lint-libs` need only the under-test binary semantically, but starting their
  nested stdlib pools while artifact builders are rebuilding exposed intermittent
  `E-FS-IO` under full-DAG contention. Start them early only when
  `TR-WARM-DONE?` is already true; otherwise let the late DAG overlap them with
  the long checker/tool tails.
- **Spawn boundaries preserve errno until attribution:** Darwin `posix_spawn`
  failure reports carry-set with `x0=errno`; using `SP` in that path produced
  giant bogus negative pids, and collapsing to the wrapper throw left only
  `rc 60`. Success writes a 32-bit `pid_t`, so load it with `LDRW`. Keep raw
  spawns returning `pid|-errno`, prove missing executable as `-ENOENT`, and
  print test/path/errno before resetting argv/env state.
- **Small-engine tools own their non-baked layout deps:** after shrinking
  `bin/hb`, tools like `imgdump` cannot assume target executable constants such
  as `DATA-SIZE` are resident. Load target layout on demand, but keep common
  dictionary layout in the cold prefix so duplicate-definition checks stay
  fail-closed.
- **Trust drift is audit maintenance, not architecture work:** moving emitter
  helpers shifts `TRUSTED.md` site pins and correctly fails `trust-lint`; update
  those rows after deciding the boundary still belongs. Do not hide a red
  `trust-lint` by starting a broad trust-reduction refactor in the same change.
- **Trust-lint string storage must scale with the manifest:** adding audited PTX
  boundaries pushed `TRUSTED.md` past the old 64 KiB string arena and failed
  before drift reporting. Keep manifest parsing backed by a generous dynamic
  buffer so the gate remains enforceable as rows grow.
- **Prefer hex for machine-adjacent literals:** byte values, ASCII, masks,
  offsets, format fields, and crypto constants should use `$hex`. Decimal is for
  small counts and human quantities; stage/bootstrap code should not transcribe
  spec constants into decimal just because both parse.
- **Emitter labels must be scoped across nested helpers:** Linux `spawn-io`
  generated a `b .` self-loop because `LINUX-SPAWN` stored branch labels in
  shared `LNX-*` variables, then nested helpers reused `LNX-DONE`. Use typed
  label locals for emitter control flow whenever a word calls another emitter
  helper that can allocate labels.
- **Timing budgets are host profiles, not global truth:** macOS ARM64, generic
  Linux ARM64, and Jetson/Orin have different CPU envelopes even with the same
  4/2 pool policy. Let the suite auto-detect a concrete host profile, use
  `--cold-cache` for cache-fill proofs, and reserve generic budgets for local
  safety checks.
- **In-process fixtures must restore global harness state:** `gate-stats-test`
  set `GS-ROOT!` to a temporary file and deleted it; the next in-process test
  then failed recording `GS-SPAN`. Any fixture that mutates shared harness roots,
  argv/env, output buffers, or cleanup registries must save/restore them before
  returning to a resident runner.
- **Pointer slots deserve typed definers:** repeated `variable FOO` plus
  `FOO 0 ptr-field` wrappers create avoidable trust rows and noisy manifest
  drift. Use `PTR-VARIABLE` for pointer-valued global cells and `PTR-FIELD:`
  for pointer-valued structure fields; keep `TRUST` only for real pointer
  refinement boundaries such as raw `mmap` results or byte-offset arithmetic.
- **Structured variable records must add cursor first:** when replacing
  `cell+ len + UALIGN ...` arena math with structure sizes, compute
  `cursor HEADER + len + UALIGN ...`; `cursor HEADER len + ...` leaves the
  cursor underneath the computed size and can fail as an opaque source-load
  throw. Preserve the tail base while writing later fields such as `*.SYM`.
- **Do not reuse VM layout names for checker structures:** names like `CF-REC`
  may already be target layout constants. Structure size words are real
  dictionary entries, so use distinct checker-local names such as `CFS-REC`
  instead of relying on silent shadowing.
- **Checker delete rows need a nonzero first cell:** append-only delete/clear
  rows can carry flag `0`, so fixed DFER/NORET records put `SYM` first and use
  only a zero symbol cell as the table terminator.
- **Internal checker effects should not parse strings:** literals, cell
  fetch/store, and control words are checker-owned semantics. Build their rows
  directly and reserve signature parsing for source comments and audited
  `TRUST` input.
- **PTX extent variables avoid reserved one-letter types:** in signatures, `r`
  is float and `c` is char, not type variables. Use tokens such as `e`/`k` for
  matrix row/column extents; otherwise diagnostics report concrete `r`/`c`
  mismatches instead of the intended parametric shape.
- **PTX optimizer values should be structure records:** a value-numbered IR fits
  checked Habu as explicit node records plus construction-time canonicalization:
  fold constants and peepholes before interning, use commutative key ordering for
  CSE, and run DCE as a root live-mark pass with static value fixtures before any
  device/lowering claim.
- **AD straight-line boundaries need named errors:** reverse-mode source reversal
  must reject control-flow tokens case-insensitively before VJP lookup. Letting
  `if`/`loop` fall into a generic no-VJP path hides the real unsupported
  capability and gives weaker repair packets.
- **LOAD adjoints default to scatter-add:** without a checked read-once witness,
  the AD pass must accumulate cotangents with `SCATTER-ADD`/`ROW-SCATTER-ADD`.
  Plain store is an optimization gated by an affine/read-once proof, not an
  inference from the current per-thread effect system.
- **Read-once is a distinct PTX address-space token:** model the optimization as
  `space-global-once` plus `LOAD-ONCE`/`STORE-ONCE`, never as a cast from ordinary
  `span<space-global,...>`. The checker can then reject both accidental plain
  stores from normal spans and accidental use of once spans through normal stores.
- **Indexed PTX memory needs two extents and uniqueness as a token:** `idxctx`
  carries both the dense index span extent and the indexed data span extent;
  duplicate-safe updates use `INDEX-SCATTER-ADD`, while plain `INDEX-STORE`
  requires `uniqidxctx` instead of assuming `idx[i]` is unique.
- **PTX IR needs distinct symbolic inputs before AD algebra:** a single generic
  `INPUT` node is enough for peepholes, but softmax backward fixtures need stable
  input symbols (`y`, `dy`) plus block-algebra nodes (`BLOCK-SUM`, `B-`) so the
  closed form can be proven as a value graph instead of an output string.
- **Subtree status docs need lint fences, not wording games:** `maki/STATUS.md`
  should be able to own maki counts without avoiding count-shaped prose. Keep root
  self-check counts fenced to root `STATUS.md`, and explicitly skip extracted
  application subtrees in `stale-status-lint`.
- **Concatenated source must preserve `required` identity:** any tool that
  materializes multiple files into one source stream must emit `provided`
  markers before each file. Otherwise dependencies owned by each test/module
  reload already concatenated files and fail with duplicate definitions.
- **Resident suite setup is not a warm snapshot:** after removing top warm
  launchers, a fully phase-owned split regressed by compiling the stdlib tool
  base independently in many fork workers. Keep the parent scheduler small, load
  the common stdlib tool base once as explicit setup, then fork phase workers so
  they inherit it copy-on-write.
- **Checker tool smokes use the core capture path:** `tools/check-test.f` spent
  over 5s spawning `bin/hb --load tools/check.f` to prove file-label JSON even
  though `CHECK-ALL-ERRORS-FILE` already preserves that label in-process. Keep
  public behavior assertions on loaded checker cores; reserve child `hb` only for
  process-exit, argv, stdin, or stderr routing contracts.
- **AOT semantic diagnostics must not load maker support:** closure-limit JSON is
  a dictionary/closure analysis invariant, not an image-emission invariant.
  Keep `aot-closure.f` resident-testable and load `aot-lib.f` only in the maker
  path; otherwise a small negative diagnostic test inherits seconds of builder
  setup and looks like a cache problem.
- **Entry/core splits must not widen worker preload:** moving a CLI tool to a
  reusable core is only a win if the core is loaded by the worker that needs it.
  Pulling SARIF into the shared diagnostics library made every diagnostic worker
  compile it; isolate heavy cores in phase-owned worker files and prove hot plus
  cold-cache timing.
- **Nested test parallelism needs a top-level slot retune:** worker-local fork
  pools fixed tail/lint span tails without recompiling setup, but keeping the
  old 12-way Mac top pool made cold cache fill slower through contention. The
  measured Mac profile is 10 top slots with 2 nested slots; prove cold wall time
  against the prior clean-tree baseline before landing scheduler changes.
- **Escaped source literals belong in Habu, not test harnesses:** when snapshots
  need embedded quotes or control bytes, implement parser words (`S\"`/`C\"`/
  `.\"`) and teach checker/scanners the same syntax. Do not route expected JSON
  through production renderers just to avoid literal escaping.
- **Source-shape tests need source-sized buffers:** tests that read generated
  sources such as `bootstrap/cg/forth.fs` must size their buffers for the current
  generated file or allocate dynamically. A stale `$20000` cap fails as
  `E-FS-CAPACITY` before assertions run, which hides the actual regression being
  tested.
- **Rejected definitions must free every per-definition resource:** the hooked
  publish reject path skipped the NDICT bump but leaked the emitted machine code
  (44 bytes per `: X drop... ;` retry) because CP was never rolled back; the
  pending dict record already holds the rollback target (slot 0 = post-name
  entry; slot 24 = pre-name CP for `DNAME-EXT` names, whose bytes sit in code
  space before the entry), so the reject branch reloads it instead of adding a
  DATA cell. When auditing reject/abort paths, enumerate every monotonic
  allocator the definition touched (CP, NDICT, DP, name bytes) and prove each
  is restored or explicitly documented as leaked. Reject-path matrix: hooked
  publish (verdict 0) rolls back and continues; trusted publish (signed defs)
  exits via `C-DIE-DOES` on a raw-verdict reject; `create`/`variable`/
  `constant` publish BEFORE `C-DEFHOOK`, which discards the hook verdict
  (habu2.f:1310-1317) — they neither reject nor exit on the raw-verdict path
  (dot habu-definer-verdict-discarded-096e8a01); a throw caught across a
  compiling definition still leaks CP and compile state
  (dot habu-catch-path-reject-60a18d38).
- **TRUSTED.md pins `file:line` for every TRUST site:** any edit that changes a
  source file's line count below an existing site fails `trust-lint` with
  STALE-ROW findings for every later row. When ownership rules forbid touching
  the manifest, keep the edit line-count-neutral (habu2.f's inline-label style
  makes this natural); otherwise refresh the manifest rows in the same change.
- **The maker image sits at the MPAGE wall:** the AOT maker's `__text` is
  engine text plus the full baked compiler source, fail-closed at
  `MPAGE - CODE-OFF` (`src/os/macos/macho.f:160`), and on master it measured
  exactly at the cap — zero margin. Any net growth of compiler source (even
  +131 bytes) fails the hb-build AOT gate with `macho: code exceeds __TEXT
  page`. Prove causality with an A/B maker build (same `bin/hb`, fresh
  `HB_TMP`/`HABU_BUILD_CACHE`, tree with and without the change); the fix is
  capacity (MPAGE) or source diet, not shaving the change to squeeze under.
- **Shared /tmp races parallel agents:** `HB_TMP` defaults to `/tmp` with fixed
  artifact names (`stage2-src`, `hb-stdin-got`); concurrent workspaces corrupt
  each other's refresh/gate runs with transient opaque exits. Always run
  fixpoint/gate/hb-build with a private `HB_TMP` (and `HABU_BUILD_CACHE`) per
  workspace.
- **`CK-FILE+` keys the path, not just the bytes:** it hashes the path string
  into the fragment stream, so keys over `HB_TMP`-relative artifacts change per
  tmp root. For emitted intermediates, hash a stable logical label plus the
  file digest instead, length-framed with a distinct tag per stage
  (`BF-STAMP-DG+`) so a dropped component can never alias another.
- **`include` and `require` do not share a registry:** adding
  `require lib/content-key.f` to a tool duplicated `CK-CAP` in the engine gate
  slice because `test/gate-common.f` already `include`s that lib. Widely
  `--load`ed tools must not grow new lib requires; for digests, use the baked
  `SHA256`/`SHA256-FILE`/`SHA256>HEX` words that every `bin/hb` carries.
- **Cache stamps assert the installed artifact, from consumed inputs:** record
  each stage-source digest at the moment the build emits/consumes it
  (`BF-RECORD-STAGE`/`BF-RECORD-STDIN`) and assemble the stamp from those
  recorded digests plus the post-install engine hash. Re-hashing the tree after
  the build races mid-build edits, and a pre-install engine hash never matches
  the next run. `-- all` stamps only when its product byte-matches the engine.
- **Cache keys carry the producer's identity, not a proxy:** the object cache
  first keyed only sha(bin/hb), but the producer is the maker =
  f(engine, checker/codegen sources); a source-level codegen change with a
  stale engine still hit. Qualify the key with the exact producer key
  (`HBB-MAKER-KEY-HEX`) so key and producer share one identity by construction.
- **AOT-LINK must start on a fresh line:** appending ` AOT-LINK ` directly after
  source bytes lets a final `\` comment without a newline swallow the sentinel,
  so the maker exits 0 without `hb-aot-got`. Emit an LF before the sentinel and
  keep an hb-build fixture whose AOT source ends in a backslash comment with no
  newline.
- **Proof flows must force past caches:** `tools/seed.f` and
  `tools/bootstrap.sh` exist to prove a rebuild, so they pass
  `install --force`; a matching stamp must never stand in for the proof.
  Audit every spawner of a cached command when adding a skip path.
- **`bin/` holds exactly `bin/hb`:** `BLTT-TEST-PUBLIC-BINS` fails the gate on
  any second file there. Persistent tool state (fixpoint stamp) goes under
  `XDG_CACHE_HOME`/`~/.cache` like `TR-PERSIST-DEFAULT`, not next to the binary.
- **Raw-text checker consumers see comments the load path strips:** the engine's
  `EM-COMMENT` removes `( ... )` before capture, but `CHECK!`/`CHECK-CANDIDATE!`
  scan raw text, so CHECK-SCAN's `'( '` handler re-parsed mid-body comments as
  signatures and clobbered SGIN/SGOUT/DCUR (SNEAK certified). Scanner rules must
  implement engine-normalization parity explicitly: sig only at token index 1,
  once, everything later a skipped comment.
- **The maker cache masks builder capacity overflows:** the AOT gate rebuilds
  the maker only on content-key misses, so a nearly full CODE buffer stays green
  until any engine-source edit forces a fresh build (`icode: code buffer
  overflow` / `macho: code exceeds __TEXT page`). When growing src/core, check
  headroom: maker `__text` size vs `CODE-CAP-WORDS` (icode.f) and `MPAGE`
  (macho.f/elf.f), and keep both guards aligned.
- **Share family deltas by fork inheritance, not by widening the suite base:**
  preloading the diag/dict/engine/debug/aot-neg gate libs into the parent
  shared setup regressed the hot gate 16.5s -> 28.3s because the serial setup
  span (7952ms -> 16825ms) sits on the critical path of every post-setup fork.
  The winning move costs zero serial time: reclassify only high-redundancy
  family workers (diag slices, dictionary) to fork after the tool base — their
  unchanged `require` lists dedupe against the inherited image and load only
  the family gate-lib deltas (diag load 11453ms -> 6743ms, dict 2973ms ->
  2453ms, ~6.5s process CPU saved, slowest-test 12260ms -> 10405ms). Widening
  the base further waits on the content-keyed image-restore residual.
- **Pre-setup fork spans are reap-inflated by the serial setup:** the parent
  does not poll the pool while `included` compiles the shared base, so every
  phase that exits during setup gets its span recorded at reap and reads as
  ~the setup duration (baseline debug/dict/aot-neg all "7.96s"; with a 16.8s
  setup, aot-pos read 16.8s). Attribute pre-setup phases with their own
  span-load rows and logs, never the pool span.
- **`aot-closure.f` and `clobber-lint.f` cannot share one image:** both define
  `CX`, so any parent base containing src/habu/aot-closure.f makes the
  lint-tools clobber fork fail with `duplicate definition: CX` (rc 78). Strict
  duplicate rejection caught the debt; rename/package-scope is dotted.
- **Per-phase PASS stamps only pay on critical-path phases:** the result cache
  (key = label + bin/hb + candidate sha + declared TR-FILES: set, stamps under
  the persistent root, red runs never stamp) correctly skipped debug/aot-neg
  as 'PASS (cached)', but zero-diff wall time did not move because those
  phases fork pre-setup off the critical path. The wall win needs declared
  sets for the setup/stdlib/engine tail, which needs enumerable verdict
  inputs first. Undeclared = never cached is the fail-closed default; the
  closure lint in test/run-result-cache-test.f rejects sets missing files
  referenced by member require lines or existing s" source literals.
- **Killed gate runs leave spinning fork workers:** SIGKILLing a hung top
  runner orphans its forked pool workers, which keep polling at full CPU and
  quietly slow every later timing run. Check for and kill stray
  `bin/hb --load test/run.f` processes before believing a gate measurement.
- **A pool parent-death reaper must NOT be a `wait(-1)`-visible child of its
  worker:** the fix for the orphan-spin above arms a per-worker reaper (watches
  a death pipe; SIGKILLs the worker's group on parent death). Arming it as a
  direct child of the worker stalled `stdlib/tail-process` ~190s because
  `lib/process-test.f` TEST-WAIT-BAD does `-1 >PID PROC-WAIT-RC` (`wait4(-1)`,
  expecting ECHILD with no children) and blocked forever on the reaper. Evidence
  was `wait4` (lldb x16=7) at 0% CPU, not a busy loop; isolated repros never ran
  a `wait(-1)` so never reproduced. Fix (`PROC-FORK-REAPER` in
  lib/process-fork.f): double-fork so the reaper reparents to init (invisible to
  `wait(-1)`) yet inherits the worker's process group; it also watches a
  worker-alive pipe and self-exits on the worker's exit, so no orphan leaks and
  the worker never tracks or kills it. Regression: `GPT-WAIT-NEG-CASE`.
- **Shard a fuzzer across forked slots, but mute shard stderr:** the property
  checker fuzzer (`test/prop-test-core.f`) now runs `PROP-SHARD-N` forked slots,
  each a distinct golden-ratio-spread seed for `DEFAULT-COUNT` iterations, so one
  gate phase covers N x count distinct-seed programs in ~one shard's wall time; a
  nonzero shard exit fails the phase. Gotcha: each shard's checker prints a
  per-reject diagnostic to stderr, and the gate's capture buffers are bounded
  (`GT-ERR-CAP` = 32KB); N shards overflow it and trip `E-PROC-TRUNCATED`, failing
  the capture. Fix: each shard redirects fd 2 to `/dev/null` (`SHARD-MUTE-STDERR`)
  — a false-cert still reports on stdout and via the shard's nonzero exit, so the
  signal is preserved. New net-0 generator ops (valid `leave`, `s" .." 2drop`,
  `[: ;] execute`, `>r r@ drop r>`, mid-body `{: zN :} zN`) can only ever cause
  extra *rejections*, never false-certs — a false-cert needs a real checker vs.
  runtime disagreement, which the generator cannot manufacture.
- **Always launch gates with explicit stdin EOF:** two stalls of
  stdlib/tail-build happened only on runs without `printf '' |`; fixture
  scripts spawned by BUILD-RUN inherit the suite stdin and can wait in the
  stdin REPL on an open never-EOF pipe (dot habu-rca-tail-build-d6b0391d).
- **Scale budgets by a measured reference, per profile:** timed gate budgets
  are stop-lines tuned on a reference host, so the portable form is
  base-budget x (probe-ms / profile-reference-ms) clamped to [100%,300%],
  never scaling user-supplied --budget-ms, and factor 100% for profiles
  whose reference probe has not been measured (reference constant 0). A
  spin probe (~95ms macOS ref) captures load/downclocking, which is what
  actually failed green trees; print cal-ms/cal-factor in the perf line so
  a stretched budget is visible telemetry, not a silent comfort blanket.
- **`maki/` as a code token trips maki-dep-lint anywhere but maki/ files:** a
  span-stray prefix table in test/gate-stats.f listed `s" maki/"`; maki-dep-lint
  matches the whole token `maki/` (not in `\` or `( )` comments) and red the
  gate. Register only prefixes that actually occur; speculative `maki/ bench/
  src/ ...` were both dead and lint-forbidden. Real stray roots on a green gate:
  `lib/ tools/ test/ dictionary/ lint-tools/`, `fork` / `fs mutation` /
  `process` / `hb baseline` fixtures, and `*-lint` names -> expected=167,
  unexpected=0 baseline so a new unrowed span is visible, not drowned.
- **The test pool legitimately reuses one label across entries** (battery starts
  12x `soft overflow`), so "reject duplicate pool labels at GT-POOL-START*" is
  wrong. Fix span identity two ways instead: a single-use ownership claim
  (GS-CHILD-CLAIM? consumes GS-CHILD-U so a fork child suppresses its one
  pool-owned span once, not every later same-label span) plus rejecting
  duplicate TEST-ROW labels at index time (GS-LABEL-SUBJ attribution is
  ambiguous otherwise). Both proved by reverting: dedup miscounts to 0, dup
  rows scan without throwing.
- **gate-pool-test.f can `require test/run-lib.f` cheaply:** the resident
  stdlib worker GSI-INCLUDEs it COW from a parent that already loaded run-lib.f
  (no-op), so only standalone/child spawns pay the load. That lets the kept-root
  e2e drive the real TR-COMPLETE->TR-RED-COMPLETE->TR-KEPT-ROOT-LINE red path
  (child keeps its capture root; parent parses `capture root kept:`, asserts the
  pool-*-out.log survives, then REMOVE-TREEs it) without a full injected gate.
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
  per band `sub;cmpi;b.cc trap` (exit E-SEAL-VIOLATION), mirroring
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
  substitution executes them even inside a quoted argument. Pass dot text as
  data or patch the generated record, then inspect the exact stored description.
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
