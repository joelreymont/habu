---
title: Provide the interpret guards to the Habu loop
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T17:23:41.514821+03:00"
closed-at: "2026-09-30T10:21:53.377546+03:00"
close-reason: Primitive min-in from spec rows via DNAME; LARITY deleted; execute-floor added. Gate 493/493 rc 0 on 347cf5b8; Fable review clean.
---

Problem: the pre-execute arity guard is a builder table baked as compare code (`habu1.f:129-147`, `habu2.f:10561-10575`) that no loop but the assembly one can read, and no Habu word can observe a below-base stack: after an underflow every push writes the low guard page (`src/habu/rt.f:95-98`), and `depth` and `catch` both push (`habu1.f:1574-1578`). The Habu loop (I4) needs both guards; without them it crashes rc 102 where the assembly loop refuses rc 70.
Acceptance: `PRIM-SPEC:MIN-IN ( n -- n )` answers the row's `PE-IN` count, overridden by a marker `n EMIN-IN!` (shape `ETRUSTED-ONLY!`, `prims.f:225-227`) on `execute` 1, `catch` 1, `evaluate` 2, `?dup` 1, `2>r` 2, `finally` 2 (the rows whose spec has no atoms); every other GD-MIN already equals `PE-IN` (all 40 rows, `ffi-call-abi` 7 at `prims.f:591-592`). `ENGINE-PRIMS:DNAME` folds `MIN-IN 52 lshift` (helpers: `FIND` -1 answers 0), so ARM64 `EMIT-DICT` and the x86 seed writer bake `DNAME-MIN-IN` for every primitive. The band gate (`habu2.f:8378-8381`) then precedes and covers what LARITY did, so `GDEREF-L/F`, `GDR-*`, `ARITY-EMIT`, `EMIT-ARITY-GUARD` and `LARITY` are deleted (`habu1.f:117-147`, `3412-3532`; `habu2.f:8383`, `10545-10575`, `11104-11109`). Those primitives' refusal text becomes `hb: interpret stack underdepth:` (rc 70 unchanged, `habu2.f:9840-9844`, `10185-10199`). New trusted-only row `execute-floor ( n -- bool )`: BLR the xt, then if XDS < `S0-CELL` set XDS := S0 and push true, else push false (body beside `BEXEC`, `habu1.f:3432`). Pins updated: `test/runtime-regression-test.f:565-580,710-747` (`drop drop drop` becomes a `TRUSTED: ( -- ) drop` word so the floor stays covered), `test/engine-stack-lifecycle.f:194`, `test/internal-word-gate.f:258,654-656`, `test/underdepth-gate.f:254-287`, `test/top-row-hook-test.f:42-45,125-147,297-301` (primitive flags now carry min-in), `docs/typed-top-level.md:18,25,38,74,210`, `docs/debugging.md:535-540,590-593`.
Files: `src/habu/prims.f`, `src/habu/primitive-registry.f`, `src/habu/habu1.f`, `src/habu/habu2.f`, the tests and docs above.
Verify: spark: `bin/hb --load test/prim-parity.f`, `test/primitive-registry.f`, the five suites above and `top-row-warn-test`; rebuild; chain gen1 == gen2 (builder change); the gate. `bootstrap/cg/forth.fs` has no LARITY, so no mirror change.
Depends: none (K4 landed). Serialise `habu1.f`/`habu2.f` with I6 and I10c, `prims.f` with I6.
Route: Alder (shared: all four `src/habu` files).
Ownership: krait (Intel lane).
Claim: agent=krait workspace=.jj-ws/habu-provide-the-interpret-32e92f89.
Preflight corrections (these override the lines above where they differ):
- Files add `src/core/checker.f`: `PPRIM: PRIM-SPEC MIN-IN PE-N PE-IN  PE-N PE-OUT PPRIM;` beside `FIND` (`checker.f:8845-8853`); `primitive-registry.f` is checked code and its `PRIM-SPEC:FIND` call (`:80`) resolves only through `checker.f:8853`. Files add `docs/x86-64.md:529-547`: row forms gain `n EMIN-IN!`, readers gain `MIN-IN` (`:544` states readers need checker rows).
- Pre-change failing check: `test/underdepth-gate.f` `UDG-NEG-PRIMS` gains `?dup` and `2>r` rows. Today they die rc 102 (`BQDUP` `habu1.f:1700-1703` loads `[XDS-8]` = `S0-8`, inside the guard page, `habu2.f:5101-5103,6276`, `rt.f:95-98`); after, rc 70 underdepth. `execute-floor` gets both branches in `test/runtime-regression-test.f`: a `TRUSTED: ( -- ) drop` xt answers -1 and the next token runs on a reset stack; a `( -- )` no-op xt answers 0.
- Counts: LARITY has 47 registrations (44 in `habu1.f:3412-3532`, including the `GD-MIN !`/`GD-RECORD` pairs at `3463-3478` and `3523-3525`; 3 at `habu2.f:11107-11109`): 43 equal `PE-IN`, and 4 are atom-less (`execute catch evaluate finally`). `?dup` and `2>r` are not LARITY rows but `ELAB:` rows (`prims.f:649-650`) whose bodies read the stack (`habu1.f:1700,1777`), so their overrides close two crash seams; `2r>`/`2r@` need none.
- Deletions also: `habu2.f:4996` (`variable LARITY`), `:10518` (`LBL LARITY !`), the `:11086` comment. `BEXEC`'s body is `habu1.f:2721` (`3432` is its registration).
- The refused set grows by design: every `PE-IN > 0` primitive at top level refuses at the band (bare `drop` moves from the post-token floor, `habu2.f:7724`); bare `?dup`/`2>r`/`nip` move from rc 102 to rc 70. No test pins those crashes.
- Base: master `bc2e9c44` or later. Workspace `.jj-ws/habu-provide-the-interpret-32e92f89`.

Closing note: the Verify line's "chain gen1 == gen2" is replaced by convergence from gen2 (gen2 == gen3 == gen4 == gen5 = `347cf5b8…`, reached independently from a Gforth recovery of the tree). gen1 differs by host-dependent captured DATA values, dotted as `habu-make-the-product-bed415cf`. No x86 seed writer exists yet, so the x86 half of the `DNAME` fold is structural until one does; `docs/x86-64.md` states it conditionally.
