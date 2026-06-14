# habu — Status

Last verified: 2026-06-14
Gate: passing
Certified: 890  Uncheckable: 0  Rejected: 0

This is the single source of truth for the self-check counts. Other docs
(`README.md`, `PLAN.md`, `CODEGEN-PLAN.md`) point here instead of quoting a
number — see `tools/stale-status-lint.py`, which fails the gate if a count-shaped
string reappears outside this file and `LESSONS.md` (the historical log).

The native engine type-checks its own toolchain source (`src/`) as it compiles
it. "Certified" = body inferred and (where a signature is declared) verified
against it; "Uncheckable" = effect not statically inferable and not trusted;
"Rejected" = inferred effect contradicts the declaration. `tools/build.sh` runs
the self-check on every rebuild; `( cd test && ./run.sh )` is the gate, and
`tools/oracle.sh` is the gforth differential — both green at this verification.

History: 783/0/0 in earlier docs, then 860/0/9 before exit/unloop modeling, now
890/0/0 — the 9 formerly-uncheckable words (`ENV=?`, `GETENV`, `TMP-PATH`,
`SHK-TOK=`, `KEEP?`, `FPRIM`, `FPRIM-L`, `EM-INTERPRET`, `EM-COMPILE`) all hinged
on early `exit`; teaching the checker a sound `exit`/`unloop` model certified them
and their callers. See `LESSONS.md` for the full record.

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
- **Return-stack ops** — `>R R> R@` typed; balance enforced.
- **`execute`** — `xt ≡ quot<E>`; all four of the quotation's rows are threaded.
- **Locals** — typed `{: a:n :}` scope.
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
  `n` = generic int), type vars, named row vars, the `| rin -- rout` return
  clause, quotation sub-sigs `[ in -- out ]` (recorded so combinator call sites
  check against them), nested quotations.
- **Trust** — `trust` charts an asserted effect for the un-inferable; see
  `TRUSTED.md`. Callers are still checked.
- **Diagnostics** — reject diagnostics to stderr; `JSON-DIAGS ON` switches to a
  structured JSON object per reject (code/word/token/expected/actual) for LLM
  repair (`test/t-sh-jdiag.fs`).

## Known gaps

- **AOT-strip linker** — done and the DEFAULT. `hb-build.sh prog.f -o out` AOT-
  compiles `: MAIN ;` to a native binary with the engine stripped (fib __text
  564 B vs 11836 B embed). `--repl` bundles the full engine + the program's
  library and drops into the REPL on a tty (`EXPORT word…` keeps extra words
  callable). The AOT file is 16627 B — one 16 KB `__TEXT` page + signature, the
  PROVEN hard floor for a signed arm64 macOS executable (a sub-page `__LINKEDIT`
  is SIGKILLed by AMFI). `S"` string literals are AOT-safe (their body is
  embedded in the blob and pushed PC-relative). AOT is stripped COMPUTE only,
  and the two features outside that boundary both fail LOUDLY (no silent wrong
  output): `['] WORD execute` is REJECTED by the checker (an opaque xt's effect
  can't be typed — use a `[: ;]` quotation, which is modeled), and `CREATE` /
  data-region access (`here`/`,`/`@`) SIGBUSes because AOT maps no data region —
  persistent data is the snapshot/`--repl` path by design, not stripped AOT.
- **`ptr a` (parametric pointer)** — gforth-tier checker only; native types `ptr`
  as an address (no native prim operates on pointer-types).
