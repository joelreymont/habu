# habu — Status

Last verified: 2026-07-29
Verification context: repair-control exact-tree verification, DGX Spark linux-arm64.

## What is running and what is next

The active goal is serving large models on DGX Spark (a vLLM replacement),
built as checked Habu Forth. The GPT-2 loading chain is landed on master and
destruction-reviewed end to end: real 548 MB checkpoint (all tensors F32,
160 tensors) → safetensors file mapping and parsed tensor index → GPT2LOAD
`PREPARE` → either `CHECK-MAPPED → LOAD-MAPPED` or
`CHECK-COPY → LOAD-COPIED` → a linear `gpt2-model`. Failed checks return their
owned input unchanged. Ordinary release paths return their owned resources, and
the copied path cleans up partial allocations when release succeeds. A release
can still throw `E-MEM-UNMAP` and interrupt later cleanup; that total-release
fix is tracked by `habu-make-owned-release-79de2b5c`. The checker accepts linear payloads in unified
`ENUM` and `STRUCTURE` declarations, which the loader tests exercise directly
with the model.

In flight, in landing order: sealed-destructure and linear-scope checker
capabilities; owned-release and model-store work tracked by
`habu-make-owned-release-79de2b5c` and `habu-embed-store-in-f8109695`;
signature-lint dynamic input; then GPT-2 forward execution and greedy decode
on mapped storage. Copied storage is the host oracle; production device
residency (cuMemAlloc, bounded streaming, peak of model plus one in-flight
chunk) is a planned task ahead of modern-dense serving.

## Gate state

Correctness gates: maki/test.f (all suites), host-lint,
dot-dep-lint, stale-status-lint, trusted-inventory
(strict + baseline), and the native gate `bin/hb --load test/run.f` — all green
on master at every fast-forward; master only moves by verified-green
fast-forward.

Performance gating (policy since 2026-07-26): the aggregate whole-gate
performance verdict is RETIRED. Only individually timed, sha-pinned benchmarks
gate — currently the six confined JSON read benchmarks, each with its own
calibrated budget, quiescent-fork isolation, median-of-3, and recorded
provenance. Contended attempts are inadmissible and re-measured, never a
verdict; retry exhaustion exits with its own distinct code. Whole-gate
elapsed-ms is informational only; per-phase timeouts remain as hang guards.
Pinned engine/checker/inference benchmarks are tracked as an accepted coverage
gap (habu-add-pinned-engine-90090800).
Certified (linux-arm64): 4197  Uncheckable: 0  Rejected: 0
Certified (macos-arm64): 4232
Host-script workflow hooks: retired and gated

Attribution for the current linux-arm64 row, which is the count this tree
measures today:

- The master commit this stack now sits on measures 4058, and its own row
  already records that corrected number. Two of those definitions are the
  `TFC-XPAD-NARROW-REJECT` and `TFC-TAG-CELLS` words that master added to
  `src/core/type-family.f` after this stack was branched; they arrived with the
  rebase, not with any change here.
- The product-field lifecycle change adds 13, reaching 4071.
- The declaration-event participant rework adds 15, reaching 4086.
- The private primitive axiom row audit changes only
  `tools/primitive-effect-inventory.f` and its test. The census counts only the
  assembled stage2 engine source, and neither file is part of it, so that commit
  adds 0 and the count stays 4086.
- The checker declaration-frame tagging that carries this edit adds 25: 20 new
  top-level definitions in `src/core/checker.f` and 5 in
  `src/core/type-family.f`. So 4058 + 13 + 15 + 0 + 25 = 4111. The two
  build fixes in that same commit add none: the checker default swaps one
  definition for another, and `src/habu/verify-source.f` is not part of the
  assembled stage2 engine source at all.
- The compact `ENUM` header change adds 1: the new `COMPACT-CLAUSE` dispatch
  word in `src/core/enum-decl.f`, which is part of the assembled stage2 engine
  source. So 4111 + 1 = 4112. That same change also edits
  `test/enum-decl-suite.f`, `docs/forth.md`, and `docs/type-families.md`, and
  none of those three files is part of the assembled stage2 engine source, so
  they add nothing.
- The constructor payload provider adds 21, all of them new colon words in
  `src/core/sumtype.f`. Eleven build and validate the payload snapshot:
  `TDPV-THROW`, `TDPV-GROW-CAP`, `TDPV-VAR-ENSURE`, `TDPV-NODE-ENSURE`,
  `TDPV-COUNT`, `TDPV-NODE+`, `TDPV-NODES+`, `TDPV-CELLS`, `TDPV-VARIANT+`,
  `TDPV-CAPTURE`, and `TDPV-SLOT`. Four read it: `TDPV-N@`, `TDPV-CELLS@`,
  `TDPV-NODE@`, and the zero-padding helper `TDGEN-PADS`. Four are the committed
  capabilities and their bundle: `TDECL-SUMV-N`, `TDECL-SUMV-ROOT`,
  `TDECL-SUMV-CELLS`, and `TDECL-SUMV-PROVIDER`. Two are the named generator
  helpers `TDECL-PROD-WORDS-BODY` and `TDECL-GEN-BODY`. The new
  `E-TDECL-PROVIDER` constant, the `TDPV-INIT` size, the four snapshot arenas
  and their capacity and cursor variables are not colon definitions and add
  nothing. So 4112 + 21 = 4133.
- The constructor-generation participant adds 17, all of them new colon words in
  the `GENERATED-DECL-CTOR` package in `src/core/generated-declaration.f`. Six
  own its armed-family slot: `ARM-BASE`, `ARM-SLOT`, `ARM-GROW1`, `ARM-ENSURE`,
  `ARMED-FAM`, and `DISARM`. Three are the generation gate: `GEN-OK?`, the
  already-generated existence check `GENERATED?`, and the throwing form
  `GEN-REQUIRE` that combines them. Five are the participant callbacks:
  `PART-SNAPSHOT`, `PART-PREPARE`, `PART-COMMIT`, `PART-ROLLBACK`, and
  `PART-RELEASE`. One is `INSTALL`, and two are the public surface the ENUM front
  end calls, `ARM` and `OWNS?`. The twelve trusted forwarders are `TRUSTED:`
  boundaries rather than certified definitions, and the `E-CTOR-ARM` constant,
  the participant id, the no-family sentinel, the initial capacity, the boot
  arena, and its two cursor variables are not colon definitions, so none of them
  counts, and neither does the thirteenth trusted forwarder `VAR-CTOR-SYM`.
  `src/core/enum-decl.f` gains no definition: `ED-CLOSE` calls the participant's
  own gate rather than declaring a second one. So 4133 + 17 = 4150, the number
  recorded above and the number the self-check measures on this tree. That change
  also edits `TRUSTED.md`, `tools/decl-gen-probe.f`,
  `test/enum-ctor-collide-bad.f`, and five test files, none of which is part of
  the assembled stage2 engine source. The engine binary is byte-identical across
  the added gate clause, so the CODELEN and per-region rows below are unchanged
  by it.
- The unified declaration reject diagnostics add 28, reaching 4178. Twenty-two
  are the new `DECL-REJECT` package in `src/core/generated-declaration.f`, the
  shared reject packet both typed front ends report through. Eleven are private:
  the two boolean answers `FOUND` and `MISSING`, the two fixed texts `NOTHING$`
  and `FALLBACK$`, the four reason groups `REASON-GRAMMAR`, `REASON-NAME`,
  `REASON-FIELD` and `REASON-TXN`, the table entry `CODE-REASON`, the armed-or-
  table choice `PICK-REASON`, and `RENDER`. Eleven are public: `OPEN`,
  `FAMILY!`, `TOKEN!`, `AT-FAMILY`, `EXPECT`, `REJECT`, `GUARD`, and the four
  reflection words `KIND$`, `FAMILY$`, `TOKEN$` and `REASON$`. Its three
  `TRUSTED:` boundaries `SLOT!`, `SLOT@` and `DIAG`, its slot and code
  constants, its two `create` regions and its one variable are not colon
  definitions and count nothing. The remaining six are three per front end:
  `src/core/structure-decl.f` gains `SD-RAW` (the pushback-honouring read that
  `SD-NEXT` now records the token around), `ARITY-WHY$`, and `SD-BODY`; and
  `src/core/enum-decl.f` gains `ED-RAW`, `ARITY-WHY$`, and `ED-BODY`. So
  4150 + 28 = 4178. That change also edits `TRUSTED.md` and
  three test files including the new `test/decl-diag-capture.f`, none of which
  is part of the assembled stage2 engine source.
- The generation-free registration entry for replayed declarations adds 14,
  reaching 4192. Eight are the new `DECL-REPLAY` package in
  `src/core/generated-declaration.f`, the token stream a tool hands a front end
  when it is registering a declaration it already lexed. Four are private: the
  separator test `RP-SEP?`, the two scan steps `RP-SKIP` and `RP-TOKEN-END`, and
  the body reader `RP-BODY-NEXT`. Four are public: `RP-ACTIVE?`, which both
  front ends and both constructor generators read, `RP-CLAIM`, `RP-RELEASE`, and
  `RP-NEXT`. Its four `TRUSTED:` span boundaries `RP-HEAD!`, `RP-HEAD@`,
  `RP-BODY!` and `RP-BODY@`, its separator and error constants and its seven
  variables are not colon definitions and count nothing. The remaining six are
  three per front end: `src/core/structure-decl.f` gains `SD-GUARDED` (the one
  guarded body both drivers share), `SD-REPLAY-END` and the entry `SD-REPLAY`;
  `src/core/enum-decl.f` gains `ED-GUARDED`, `ED-REPLAY-END` and `ED-REPLAY`. So
  4178 + 14 = 4192. `src/core/structure-make.f` splits its one `TRUSTED:`
  `SM-EMIT` into `SM-EMIT-ROWS` and `SM-EMIT-WORDS`, which are not colon
  definitions and leave the count unchanged. That change also edits
  `tools/check-core.f`, `src/habu/verify-source.f`, `TRUSTED.md`,
  and four test files including the new `test/decl-replay-verify-source.f`, none
  of which is part of the assembled stage2 engine source.
- Giving the control-word list a single owner removes one, reaching 4191.
  `src/core/sumtype.f` had its own copy of that list as `TDECL-CONTROL?`; the
  copy is deleted and `TDECL-RESERVED?` now asks `TYPE-NAME:CONTROL?` in
  `src/core/type-family.f`, which already held the identical list and is now
  public so the unified front ends and the field-name gate can read it too. So
  4192 - 1 = 4191. Nothing is added: `src/core/enum-decl.f` and
  `src/core/structure-decl.f` each gain a `TRUSTED:` forwarder (`CONTROL-KW?`),
  which is not a colon definition and counts nothing, and `PF-RESERVED?` gains a
  line rather than a definition. That change also edits `TRUSTED.md`,
  `docs/extent-substrate.md` and three test files, none of which is part of the
  assembled stage2 engine source.
- Moving the global `ENUM` keyword to the unified front end removes seven
  definitions and adds eight, so the row ends where it started, at 4192. The
  removals: seven definitions leave `src/core/sumtype.f` with the legacy
  compact ENUM parser and generator: `TDECL-ENUM-VARIANT`,
  `TDECL-ENUM-VARIANTS`, `CHECKER-DEFENUM-BODY`, `CHECKER-DEFENUM`,
  `ENUM-COLLECT`, `TDECL-ENUM-NOEND-BODY`, and the old `ENUM` definer itself.
  The additions: `src/core/enum-decl.f` gains `: ENUM ( -- ) ENUM-DECL:ED-RUN ;`,
  the global keyword, written exactly the way `src/core/structure-decl.f` writes
  `STRUCTURE`; each front end gains the three words that resynchronize the input
  after a swallowed reject (`ED-SKIP-BODY`, `ED-RESYNC`, `ED-DRIVE` in
  `src/core/enum-decl.f`, `SD-SKIP-BODY`, `SD-RESYNC`, `SD-DRIVE` in
  `src/core/structure-decl.f`), which is the guarantee the deleted
  collect-then-parse definer used to provide for free; and
  `src/core/generated-declaration.f` gains `DECL-REJECT:MULTI-ERROR?`, the public
  reading of the multi-error flag those front ends ask before skipping. So
  4191 - 7 + 8 = 4192: the row returns to the value it held before the
  control-word change above took one away. Two axiom rows go with the deleted
  words
  (`PRIM: ENUM` in `src/core/sumtype.f` and `PRIM: CHECKER-DEFENUM` in
  `src/core/checker.f`) and `src/core/generated-declaration.f` gains two
  `TRUSTED:` forwarders for the multi-error load flag; neither kind is a colon
  definition and neither counts. That change also edits `TRUSTED.md`,
  `test/prop-test-core.f`'s axiom ledger, `tools/lint/text-foundation-test.f`'s
  registry-row ratchet, `tools/check-all-errors-test.f`, and five test files,
  none of which is part of the assembled stage2 engine source.
- Rejecting pointer fields to linear values adds two, reaching 4194. That change
  landed on 2026-07-26 as `130e7b92` and left this row at 4192; the row above
  and this entry are the correction. Each unified front end gains one
  definition, and both are named `REQUIRE-POINTEE`: one in
  `src/core/enum-decl.f` and one in `src/core/structure-decl.f`. Each is the
  parse-time guard that reads the pointee node and refuses a pointer whose
  destination owns a linear value. The `SCH-OWNS-LINEAR?` forwarder each file
  gains beside it is a `TRUSTED:` boundary rather than a colon definition and
  counts nothing, because the census scanner matches the exact token `:` and not
  every name that merely ends in one. So 4192 + 2 = 4194. That change also edits
  `TRUSTED.md`, `test/enum-decl-suite.f`, and `test/structure-decl-suite.f`,
  none of which is part of the assembled stage2 engine source. Both guards are
  temporary: the family-schema repair train deletes them and moves the rejection
  to one shared close-time family query — `habu-reject-bad-pointers-7c6a5d6e`
  for `ED-CLOSE` and `habu-reject-bad-pointers-230fa9c9` for `SD-CLOSE` — so
  this row is measured and re-ledgered again when that train lands.
- Sealing raw storage on the load path adds three, reaching 4197. Two are in
  `src/core/checker.f`: `TRUST-RAW`, the effect-declaration word that mints the
  type variables of a raw storage cell's effect as `TVK-RAW`, and
  `TRUST-USIG!`, the registration that it and `TRUST` now share. One is
  `C-FIND-TRUST-RAW` in `src/habu/habu2.f`, which resolves `trust-raw` for the
  three created-word publication sites. All three are ordinary colon
  definitions in the assembled stage2 engine source, and none of them is
  conditional on the host, so the count moves by the same three on both rows.
  So 4194 + 3 = 4197. The macos-arm64 row above is the measurement this tree
  took on that host, 4220 + 3 = 4223; the linux-arm64 number is carried over
  from the same three shared definitions and is owed a real measurement the
  next time the gate runs on that host. The change also edits
  `docs/effects.md`, `FILEMAP.md`, and the new
  `test/raw-storage-load-seal-test.f`, none of which is part of the assembled
  stage2 engine source, so none of them counts.

The census ratchet in `test/gate-engine-lib.f` requires this row to equal what
the tree measures, so each commit records its own measurement rather than a
predicted count.

The loop-family opener guard adds one to the macos-arm64 row, reaching 4221. The
one new definition is `J-LVREQUIRE` in `src/habu/habu2.f`: the emitter that every
compile-time loop-family word now calls to prove a `do` level is open before it
touches the DO/LEAVE level stack. Its three callers `J-LVLEAVE`, `J-LOOP` and
`J-+LOOP` already existed and only gained that call. The matching edit in
`bootstrap/cg/forth.fs` and the new engine gate battery in
`test/gate-engine-lib.f` are not part of the assembled stage2 engine source and
count nothing. The linux-arm64 row is owed the same single addition and is
re-measured on that host.

The checker declaration-frame tagging does add engine text: the four
`CHECKER-DECL-FRAME` primitive axiom rows grow the ahead-of-time seed by 16
bytes, so `LINUX-CODE-TEXT` in `test/gate-size-attribution-test.f` moves 122568
to 122584. The same 4 KiB text page absorbs it, so the installed engine stays at
the 127168 bytes committed as `LINUX-TOTAL`, which that gate checks against the
live file on every run.

This is the single source of truth for the self-check counts. Other docs
(`README.md`) point here instead of quoting a number — see
`tools/stale-status-lint.f`, which fails the gate if a count-shaped string
reappears outside this file, `LESSONS.md`, and the archived lesson logs
(`docs/archive/lessons-*.md`, the relocated historical log).

Metric (recorded verbatim, one row per build target): the count on each
`Certified (<target>)` row is the scanner-accurate number of top-level colon
definitions in the assembled stage2 engine source for that target, as counted
by the VERIFY scanner (`VERIFY:CENSUS-COUNT` in `tools/build-fixpoint.f`) over
the full source set including the target's `src/os` leg. Every counted
definition is certified: the stage2 self-check fail-closes (`E-BUILD-CERTIFY`)
on any uncheckable or rejected definition, so those two counts are structurally
0 whenever a row exists. The build-fixpoint self-check emits this triple during
`install`/`stage`, and the `GE-CENSUS-RATCHET` gate slice
(`test/gate-engine-lib.f`) re-measures the current target and fails closed if
its row here drifts, skipping the other target's `owed` row until that platform
fills it — the same per-target asymmetry the CODELEN rows in
`test/gate-build-size.f` already use.

The native engine type-checks its own toolchain source (`src/`) as it compiles
it. "Certified" = body inferred and (where a signature is declared) verified
against it; "Uncheckable" = effect not statically inferable and not trusted;
"Rejected" = inferred effect contradicts the declaration. Native
`tools/build-fixpoint-main.f -- install` refreshes `bin/hb` and runs the self-check;
`bin/hb --load test/run.f` is the Habu-native
test suite. That suite runs native parity, shadow, clobber, trust, and status lints,
the retired host-script token lint, the rebuild fixpoint, JSON diagnostic
assertions, property soundness smoke, PTY/process checks, and AOT/`--repl`
builder checks. No-binary recovery uses `tools/bootstrap.sh`: Gforth creates
only private `HB_TMP` artifacts from `bootstrap/`, then installs exactly
`bin/hb` for macOS ARM64 or Linux AArch64 and immediately refreshes that binary
from current source. The gate,
daily refresh, benchmark, and verification paths remain Habu-native and run with
Gforth absent. The installed `bin/hb` and gate `Habu-under-test` are the small
stdin/TTY engine, not snapshot launchers; the gate rejects an oversized
`Habu-under-test`. Standalone snapshot-launcher tooling has been removed; snapshot
coverage belongs to the native build/fixpoint path.

History: the earlier curated series (`783/0/0` -> `860/0/9` -> `890/0/0` ->
`979/0/0` -> `987/0/0`, the exit/unloop and show-inferred-local milestones) is
RETIRED as unreproducible — it counted a hand-curated word set with no surviving
live definition and no measuring tool, and no measurement of the live tree
reproduces `987` (dot `habu-census-assert-the-f3a20b1f` verified this). From
2026-07-20 the census is the scanner-defined metric above, measured by the
build-fixpoint self-check and gate-asserted per target; the first measured value
is `3530/0/0` on linux-arm64 (DGX Spark; re-measured `3533/0/0` at the merge
after the deferred-column definer landed - the ratchet caught the drift on its
first run). There is no continuity between the
retired `987` and this series — it is a different, now-reproducible metric, not
a delta, so no attribution (including the BTC-7 landing's 5 checker words) is
carried across the break. The macOS row is owed at the next macOS fixpoint. The
retired series and its exit/unloop narrative (the 9 formerly-uncheckable words
`ENV=?`, `GETENV`, `TMP-PATH`, `SHK-TOK=`, `KEEP?`, `FPRIM`, `FPRIM-L`,
`EM-INTERPRET`, `EM-COMPILE`) remain in `LESSONS.md` and the archived lesson
logs as history.

## Native checker surface

The built-in checker (`src/core/checker.f`) covers the full surface the engine
compiles. Two entry points: `CHECK ( a u -- flag )` infers a body's effect
(`-1` certified / `0` rejected / `1` uncheckable); `CHECK!` is `CHECK` with
`VSIG` set, so it additionally VERIFIES the body against a leading declared
`( in -- out )` signature and rejects a mismatch (the standalone REPL hook).

- **Term + row resolution** — HM-style union-find over separate type- and row-var
  id spaces; chains chased to a head.
- **Occurs check through quotations** — descends ptr/quot/push; a self-applying
  quotation is rejected, never loops.
- **Row unification** — full row polymorphism over both the data and return rows.
- **Return-stack ops** — `>R R> R@ 2>R 2R> 2R@` typed; balance enforced.
- **`execute`** — `xt ≡ quot<E>`; all four of the quotation's rows are threaded.
- **Locals** — typed `{: a:n :}` scope, block-scoped: a `{:` group may appear on
  any live path (both `if`/`else` arms, `case` arms, loop bodies), visible until
  that arm closes, then the prior scope is restored. A `{:` group on a dead path
  (immediately after an unconditional `exit`/`leave`/`throw`/`die`/`again`)
  rejects with `E-BAD-LOCAL-SHAPE`; a quotation cannot introduce or reference an
  enclosing local.
- **Control flow** — `IF/ELSE/THEN`, `BEGIN…UNTIL/WHILE…REPEAT/AGAIN`,
  `?DO…LOOP/+LOOP`, `I J UNLOOP RECURSE`; branch states unified at the joins.
- **Leave** — `leave` must carry the loop-exit row (= the post-`?DO` row of a
  neutral body) and kills the path to `loop`; the loop exit stays live (reached
  by the leave or a zero-trip `?do`). Non-neutral leave rejects.
- **Exit** — `exit` accumulates the data+return rows (all returns + the `;`
  fall-through must unify) and marks the path dead; dead branches excluded from
  joins; unbalanced exits reject. `unloop` is a typing no-op.
- **Quotation scoping** — `[: ;]` is a nested scope with its own exit accumulator;
  a quote's early `exit` does NOT leak to the enclosing word.
- **Sig grammar** — distinct concrete types (`i64 u8 u32 cell char str addr bool`,
  `n` = generic int), nominal roles (`idx len count off fd rc pid ms ns tok reg
  label va symidx`), type vars, named row vars, the `| rin -- rout` return
  clause, quotation sub-sigs `[ in -- out [| rin -- rout] ]` (recorded so
  combinator call sites check against them), nested quotations.
- **Parsing words** — `s"`, `c"`, `."`, `[char]`, and interpret-mode `char` are
  modeled and covered in runtime, checker, and AOT tests.
- **Higher-order library** — `DIP KEEP BI TRI TIMES EACH MAP FOLD` are runnable
  native words with audited `TRUST`ed public schemes; callers are checked against
  those schemes.
- **Trust** — `trust` charts an asserted effect for the un-inferable; see
  `TRUSTED.md`. Callers are still checked.
- **Diagnostics** — reject diagnostics to stderr; `JSON-DIAGS ON` switches to a
  structured JSON object per reject (code/repair_class/word/token/expected/actual)
  for LLM repair. The native gate asserts this with `tools/gate-json-assert.f`.
- **Time/date** — `epoch-seconds` and `mono-ns` are checker-modeled native
  primitives. `tools/date.f` provides checked UTC Gregorian helpers:
  `PARSE-YMD`, `FORMAT-YMD`, and `FORMAT-EPOCH-UTC`; lints use them instead of
  host date tools.

## Device leg (Orin "zed") — verified 2026-07-14

- **Provisioned and green.** Recovery gforth 0.7.9_20260610 built on-box
  (`~/.local/lib/habu-gforth-0.7.9_20260610`, wrapper `~/.local/bin/gforth`);
  `bin/hb` bootstrapped on Linux-aarch64, fixpoint x2 byte-identical
  (147648 B, sha256 3d714be6dc1d...). On-device `maki/test.f` PASSes with the
  device-FFI leg REAL (cuInit + cuDeviceGet), and full `test/run.f` is green
  after the Linux size-ratchet re-measure (`test/gate-build-size.f`).
- **25W is the canonical measurement environment** (user decision 2026-07-14):
  nvpmodel mode 3, 4 TPCs/8 SMs. The registry's `orin-nx-15w` rows are
  history; new rows are tagged `orin-nx-25w`. Do not reboot the box to switch
  modes without user approval.
- **Fail-closed device proofs**: `maki/gpu.f` and `tools/ptx/fusion-compare.f`
  self-emit their kernels to private per-run PTXTC roots (no shared
  `/tmp/*.cubin`); fusion-compare proven on-device (real compare, exit 0,
  ~93 GB/s_x1000 family at 918 MHz). Remaining device debt is tracked in dots
  habu-make-ptx-device-c0eb12a3 and habu-perf-registry-re-6be03867.

## Current state and gaps

- **LLM benchmark harness retired** — the cross-language benchmark machinery is
  no longer active infrastructure. The last useful result was enough for current
  planning: Habu used roughly 8-10x the output tokens of TypeScript/Rust on the
  hard array tail. There is no current publication-grade cross-language claim;
  rebuild a fresh harness only if that becomes the active goal again.
- **AOT-strip linker** — done and the DEFAULT. The native `tools/hb-build.f` path AOT-
  compiles `: MAIN ;` to a native binary with the engine stripped (fib __text
  540 B vs 11836 B embed). `--repl` verifies the user source's checked
  definitions at build time, then bundles the full engine + the program's
  library and drops into the REPL on a tty (`EXPORT word…` keeps extra words
  callable). The AOT file is 16628 B — one 16 KB `__TEXT` page + signature, the
  PROVEN hard floor for a signed arm64 macOS executable (a sub-page `__LINKEDIT`
  is SIGKILLed by AMFI). `S"`, `C"`, and `."` parsing words are AOT-safe (string
  bodies are embedded in the blob and pushed/used PC-relative). AOT is stripped
  COMPUTE only, and the two features outside that boundary both fail LOUDLY (no
  silent wrong output): `['] WORD execute` is REJECTED by the checker (an opaque
  xt's effect can't be typed — use a `[: ;]` quotation, which is modeled), and `CREATE` /
  data-region access (`here`/`,`/`@`) is rejected statically with
  `E-AOT-UNSUPPORTED` because AOT maps no data region — persistent data is the
  snapshot/`--repl` path by design, not stripped AOT.
- **`ptr a` (parametric pointer)** — implemented natively. `ptr` requires an
  inner type, memory/path/process primitives are pointer-typed, pointer
  arithmetic preserves pointee type, pointer differences return `n`, and pointer
  comparisons return `bool`.
- **Naked `?DUP`** — runtime exists, but the checker deliberately rejects it as
  value-dependent (`CHECK!` verdict 1); use `?DUP-IF` for a typeable branch.
- **`EXPORT` package re-export** — landed (dot habu-compiler-pkg-re-688212c1).
  Inside an open package, `EXPORT NAME` publishes an existing word under its
  own tail (one body, two names; checker records a fresh alpha-equivalent
  scheme; defer/control flags and immediate/wide bits ride the alias; AOT emits
  one body). Top-level `EXPORT` remains the hb-build `--repl` directive, now a
  keyword no-op on plain loads; hb-build's directive strip is package-aware.
  See docs/forth.md § Packages and test/type-export-suite.f +
  test/export-package.f.
