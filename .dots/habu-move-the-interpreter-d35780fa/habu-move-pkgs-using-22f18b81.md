---
title: Move packages, using and EXPORT into Habu
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.680527+03:00"
blocks:
  - habu-add-the-engine-ebe5d757
closed-at: "2026-10-01T00:30:00.000000+03:00"
close-reason: "Package keywords in the Habu loop (packages.f, interpret.f): outer-interpret 81 route-equality cases agree, outer-find and outer-number ok on eff4ff42; Opus review LAND"
---

Problem: `package`, `public`, `private`, `;package`, `using`, `;using` and `EXPORT` run in the assembly interpreter.
Acceptance: those words in `src/habu/packages.f`, with `E-USING-SHADOW-GLOBAL`, the 16 limit and the checker notifications (the `C-CALL-CHECKER-PACKAGE` family) as today.
Files: `src/habu/packages.f`.
Verify: spark: the gate's package and using suites through the Habu loop under the feature cell; gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: Alder (shared: src/habu/packages.f).
Ownership: krait (Intel lane).
Claim: krait.

Preflight corrections (2026-09-30; override the lines above where they differ; re-preflight after I4b lands):
- The seven words are interpret-loop keywords, not records (`habu2.f:8351-8357` `EM-INTERPRET-DEFINE-KEYWORDS`). Through the Habu loop they die `E-UNDEFINED: package|using|export` rc 70; the engine answers 0 (pre-change failing check).
- Depends: I4b (`habu-interpret-literal-keywords-0fa50d62`). I8 adds seven rows to I4b's keyword step in `outer.f`; I4b owns the step's shape.
- No provider writes a namespace row or an EXPORT alias from Habu: `ndict-append` (`habu1.f:3299-3320` BNDAPPEND) refuses unless `DEF-TIER-CELL`=1 and `PEND-CELL` owns the slot; the engine writes both rows under `PROT:LSPAN`/`PROT:LOPEN` (`habu2.f:7879-7894`, `8322-8336`) with `C-STORE-NAME` spilling long names at CP (`habu2.f:3205`). I8 owns a new trusted-only record writer (one row or a namespace/alias pair; the interface is fixed at re-preflight): name via `C-STORE-NAME`, fields [0]/[8]/[16]/[40], `NDICT++`, `LHIDXADD` under `PROT-GUARD:CALL`, DICT-CAP `$4D`. That primitive makes I8 an engine-closure leaf (rebuild, chain converging from gen2, gate).
- Files add: `src/habu/outer.f` (rows; `require src/habu/packages.f`), `src/habu/prims.f` + `src/habu/habu1.f` (the writer), `src/habu/kernel-x64.f` (`REFUSE` row beside `tok-imm?`), `docs/x86-64.md` row table, `test/gate-stdlib-cases.f`. Package `PKGS` in `packages.f`; words `TRUSTED: ( -- )`; scanner `parse-name` (`habu1.f:3261`). `packages.f` never requires `outer.f`.
- Inventory to preserve, in order: `C-TASK-LIVE-GUARD` (token, exit 79, `habu2.f:2501`); `C-PACKAGE` `:7963-7985` ($4B, $4A, checker call before seal guard, seal 84 via RESTAB fold `:3509` and protected wid bitmap `layout.f:907`, existing row keeps wids and allocates a private one only when [8]=0 `:7896`, `USE-PKG-SAVE-CELL`, `PKG-*`/`CUR` cells); `C-PUBLIC`/`C-PRIVATE`/`C-END-PACKAGE` `:7987-8028`; `using` 89/90/91/92 (`USE-MAX` 16, `layout.f:1537`) and CHECKER-USING at pre-increment depth `:8032-8109`; `;using` 93; `C-EXPORT` `:8300-8336` (top-level no-op, seal guard, LFIND without used publics, 70, tail rewrite, dup $4E before checker, flag copy IMM/WIDE/MIN-IN, `C-STORE-DEF-NAME` 84). Checker calls: `DECL-CELL`/`TARGET-DECL-CELL` fields at `CHECKER-OWNER-ABI:*-OFF`, absent skipped, `using` notifies DECL then TARGET unless same (`habu2.f:2557-2570`, `7840-7850`); `checker-export` by global name, exit 70 if absent (`:2753`). `checker-owner.f FIELD` refuses absent fields, so `packages.f` needs its own reader. `LCOMPILEDIE` rcs become `throw`s as I4's 70.
- Verify: the gate's package and using suites define with `:` in-process, which the loop refuses until I5e/I6; instead `test/outer-interpret.f` route-equality cases (prelude defines packages/words under the engine loop; cases hold only these keywords, numbers and prelude words): open/close, $4A/$4B, using then bare call, `;using` then E-UNDEFINED, 89-93, ambiguity by two usings, export no-op/alias/70/$4E, hook window installed. In-process calls of `PKGS:*` from a file the engine loop reads cover the seal and task exits and the checker mirror (`E-USING-SHADOW-GLOBAL`, `checker.f:9149`, via a following `:`). Then rebuild, chain, gate.

Design corrections (2026-09-30; override the lines above and the earlier corrections block where they differ):
- Depends: I4c (`habu-add-the-engine-ebe5d757`, freezes every row used here) and I4b. Habu only: no rebuild or chain. Files: `src/habu/{packages,interpret,outer}.f`, `test/outer-loop-on.f`, `test/outer-interpret.f`, the `src/core/include.f:948-950` comment. Nothing in prims.f, habu1.f, kernel-x64.f or docs.
- After the seal a store into the friend arena (CUR, WIDN, DEF-WL, PKG-*) exits 83 (measured; `data-bands.f:18-29`). So wids come from `namespace-record`/`namespace-private`, CUR from `set-current`, the scope from `package-scope!`, aliases from `alias-record`, each called through one TRUSTED: wrapper (`publish.f:18-40`). USE-* cells and DEF-TKA/DEF-TKL are writable (measured) and written directly.
- First commit, mechanical: STEP/RUN/INTERPRET (`outer.f:744-770`) move to a new `src/habu/interpret.f` (`package OUTER`; requires outer.f, then packages.f); both tests require it.
- `packages.f` reopens `package OUTER` (private) and requires outer.f. It calls TOKEN, SAY, THROW-AT, SEAL-GUARD, FIND-PROBE and FIND-SPLIT/FIND-OPEN/FIND-QUALIFIED directly (EXPORT's chain skips used publics). Its words take a `PKG-` prefix (rc 78 otherwise). outer.f gains the shared `TASK-GUARD` (token on fd 2, exit hook cleared, `die` 79) and `PROTECTED? ( n -- bool )` (the two reserved wids, then the PROT-BITS-OFF bit, as `habu1.f:4046-4058`). STEP runs `PACKAGE?` after LITERAL?. Checker fields are read by a reader that skips an absent field, with TRUSTED: casts in the shape of `checker-owner.f`'s AS-* words.
- Inventory additions: `package`'s $4D prints the stale DEF-TKA/DEF-TKL (`habu2.f:3241-3243`); EXPORT sets both cells. Unit-hook event 2 moves to I8b (`habu-call-the-unit-ec148ccf`).
- Refs: C-PACKAGE `habu2.f:8023`, NEW-RECORD 7939, EXISTING-PRIVATE 7956, using 8097-8186, C-EXPORT 8365-8409, keywords 8415-8422, `checker.f:9115`.
- Verify: `bin/hb --load test/outer-interpret.f` (SUITE outer-interpret) with the corrections-block cases plus `package ENGINE-ERROR` (84), a prelude-spawned task (79), EXPORT into a protected wid (84); the checker mirror: `using P`, then an engine `evaluate` of a `:` that calls a shadowed tail (rc 67). No in-process calls. Gate.
- Rejected from the re-preflight: I8-owned rows taking caller-supplied wids (a live wordlist could get a second namespace name; the writers go to I4c, which allocates wids); a PACKAGE? table in outer.f with PKGS throwing bare codes (splits every refusal across two files).

Re-preflight corrections (2026-09-30; override every block above where they differ):
- Checked on master 1ced9608 with I4, I4b and I4c closed. The Design corrections stand except below.
- Refs (drifted): C-TASK-LIVE-GUARD `habu2.f:2503`; DECL-OWNER `2556-2575`; `checker-export` lookup `2755-2761`; C-QUALIFY-FAIL (stale DEF-TKA/TKL) `3281-3283` via C-QUALIFY-CAP `3727`; dup wall `3741`; C-SEAL-MATCH `3780`; checker calls `8296-8337`; C-PACKAGE family `8341-8512`; using `8523-8611`; C-EXPORT `8748-8835`; keywords `8841-8848`; rows `prims.f:624-648`; `set-current` `prims.f:685` is global, not trusted-only; USE-MAX `layout.f:1534`. The loop to move is `outer.f:741-767` (DISPATCH, STEP, RUN, INTERPRET).
- interpret.f requires outer.f in the mechanical commit; packages.f joins in the second.
- INTERPRET also saves USE-DEPTH and restores it on both exits: usings are file-local (`layout.f:1522-1533`; B-EVAL `habu1.f:1442`, EM-EVAL-CLEAN-EXIT `habu2.f:10595-10598`). Case: a nested include opens `using`, then the parent's bare tail is `E-UNDEFINED` rc 70. Package-scope rollback on a throw stays with habu-roll-back-failed-64bf2ba5.
- EXPORT of a DNAME-INT source: the engine publishes an alias without the bit; `alias-record` exits 83. Right after the lookup, before the checker call, refuse with the interpret gate's `hb: internal engine word: <token>` and throw 70. Test this on the Habu route alone. An integer constant exports normally. The pending-definition 83s and tier-0 `def-open` touch no I8 path.
- The PKG- prefix only avoids OUTER's existing names.
- PROTECTED?: copy `tools/prot-wid-probe.f` MEMBER? (src never requires tools/).
- Tests: outer-interpret is a spawned route-equality suite, so booted-image rules do not apply. Turn AMBIGUITY and TICK-USED into BOTH cases (FXA/FXB move into the prelude), then delete OI-AMBIG, TWIN$ and TWIN-BUF. Keep SEAM. Add a global twin of OI-SEVEN for the shadow case. Update the loop comments (`outer.f:279-284`, outer-loop-on.f, outer-interpret.f header).
- Pre-change failing check: `package OI-X public ;package 1 .` loaded after `test/outer-loop-on.f` gives `E-UNDEFINED: package`, rc 70; the engine prints 1, rc 0.
- Baked: nothing. The include.f edit is a comment. No rebuild, chain or full gate: only outer-number, outer-find and outer-interpret require outer.f.
- Verify: `bin/hb --load test/outer-interpret.f` (44 cases on master), `test/outer-find.f`, `test/outer-number.f`.
