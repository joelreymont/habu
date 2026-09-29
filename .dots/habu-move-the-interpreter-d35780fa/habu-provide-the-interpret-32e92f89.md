---
title: Provide the interpret guards to the Habu loop
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T17:23:41.514821+03:00"
---

Problem: the pre-execute arity guard is a builder table baked as compare code (`habu1.f:129-147`, `habu2.f:10561-10575`) that no loop but the assembly one can read, and no Habu word can observe a below-base stack: after an underflow every push writes the low guard page (`src/habu/rt.f:95-98`), and `depth` and `catch` both push (`habu1.f:1574-1578`). The Habu loop (I4) needs both guards; without them it crashes rc 102 where the assembly loop refuses rc 70.
Acceptance: `PRIM-SPEC:MIN-IN ( n -- n )` answers the row's `PE-IN` count, overridden by a marker `n EMIN-IN!` (shape `ETRUSTED-ONLY!`, `prims.f:225-227`) on `execute` 1, `catch` 1, `evaluate` 2, `?dup` 1, `2>r` 2, `finally` 2 (the rows whose spec has no atoms); every other GD-MIN already equals `PE-IN` (all 40 rows, `ffi-call-abi` 7 at `prims.f:591-592`). `ENGINE-PRIMS:DNAME` folds `MIN-IN 52 lshift` (helpers: `FIND` -1 answers 0), so ARM64 `EMIT-DICT` and the x86 seed writer bake `DNAME-MIN-IN` for every primitive. The band gate (`habu2.f:8378-8381`) then precedes and covers what LARITY did, so `GDEREF-L/F`, `GDR-*`, `ARITY-EMIT`, `EMIT-ARITY-GUARD` and `LARITY` are deleted (`habu1.f:117-147`, `3412-3532`; `habu2.f:8383`, `10545-10575`, `11104-11109`). Those primitives' refusal text becomes `hb: interpret stack underdepth:` (rc 70 unchanged, `habu2.f:9840-9844`, `10185-10199`). New trusted-only row `execute-floor ( n -- bool )`: BLR the xt, then if XDS < `S0-CELL` set XDS := S0 and push true, else push false (body beside `BEXEC`, `habu1.f:3432`). Pins updated: `test/runtime-regression-test.f:565-580,710-747` (`drop drop drop` becomes a `TRUSTED: ( -- ) drop` word so the floor stays covered), `test/engine-stack-lifecycle.f:194`, `test/internal-word-gate.f:258,654-656`, `test/underdepth-gate.f:254-287`, `test/top-row-hook-test.f:42-45,125-147,297-301` (primitive flags now carry min-in), `docs/typed-top-level.md:18,25,38,74,210`, `docs/debugging.md:535-540,590-593`.
Files: `src/habu/prims.f`, `src/habu/primitive-registry.f`, `src/habu/habu1.f`, `src/habu/habu2.f`, the tests and docs above.
Verify: spark: `bin/hb --load test/prim-parity.f`, `test/primitive-registry.f`, the five suites above and `top-row-warn-test`; rebuild; chain gen1 == gen2 (builder change); the gate. `bootstrap/cg/forth.fs` has no LARITY, so no mirror change.
Depends: none (K4 landed). Serialise `habu1.f`/`habu2.f` with I6 and I10c, `prims.f` with I6.
Route: Alder (shared: all four `src/habu` files).
Ownership: krait (Intel lane).
Claim: unassigned.
