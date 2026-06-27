# habu — Status

Last verified: 2026-06-27
Gate: passing; 1m21.93s on 2026-06-27 on Linux/aarch64 with batched warm-image trust export, measured 8-way outer and 4-way nested gate pools, early independent builder phases, warm checker/tool images, shared hb-build maker cache, two-build positive AOT coverage, direct repair JSON assertions, deduplicated AOT negative coverage, and the checked DAG pool
Certified: 979  Uncheckable: 0  Rejected: 0
Host-script workflow hooks: retired and gated

This is the single source of truth for the self-check counts. Other docs
(`README.md`) point here instead of quoting a number — see
`tools/stale-status-lint.f`, which fails the gate if a count-shaped string
reappears outside this file and `LESSONS.md` (the historical log).

The native engine type-checks its own toolchain source (`src/`) as it compiles
it. "Certified" = body inferred and (where a signature is declared) verified
against it; "Uncheckable" = effect not statically inferable and not trusted;
"Rejected" = inferred effect contradicts the declaration. Native
`tools/build-fixpoint-main.f -- install` refreshes `bin/hb` and runs the self-check;
`bin/hb --load lib/errors.f lib/string.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f lib/test-runner.f test/gate-pool.f test/run.f` is the Habu-native
gate. That gate runs native parity/shadow/clobber/trust/status/filemap lints,
the retired host-script token lint, the rebuild fixpoint, JSON diagnostic
assertions, property soundness smoke, PTY/process checks, and AOT/`--repl`
builder checks. No-binary recovery uses `tools/bootstrap.sh`: Gforth creates
only private `HB_TMP` artifacts from `bootstrap/`, then installs exactly
`bin/hb` for macOS ARM64 or Linux AArch64 and immediately refreshes that binary
from current source. The gate,
daily refresh, benchmark, and verification paths remain Habu-native and run with
Gforth absent.

History: 783/0/0 in earlier docs, then 860/0/9 before exit/unloop modeling,
890/0/0 after that model landed, and 979/0/0 after the native primitive,
combinator, parsing-word, and stage2 utility signature gap closures. The 9 formerly-uncheckable words
(`ENV=?`, `GETENV`, `TMP-PATH`, `SHK-TOK=`, `KEEP?`, `FPRIM`, `FPRIM-L`,
`EM-INTERPRET`, `EM-COMPILE`) all hinged on early `exit`; teaching the checker a
sound `exit`/`unloop` model certified them and their callers. See `LESSONS.md`
for the full record.

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
- **Locals** — typed `{: a:n :}` scope; locals introduced inside active control
  flow, inside quotations, or after a dead `exit` path reject with
  `E-BAD-LOCAL-SHAPE`.
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
