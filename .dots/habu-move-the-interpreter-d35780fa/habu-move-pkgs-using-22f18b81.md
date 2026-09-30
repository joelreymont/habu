---
title: Move packages, using and EXPORT into Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T12:51:36.680527+03:00"
blocks:
  - habu-interpret-literal-keywords-0fa50d62
---

Problem: `package`, `public`, `private`, `;package`, `using`, `;using` and `EXPORT` run in the assembly interpreter.
Acceptance: those words in `src/habu/packages.f`, with `E-USING-SHADOW-GLOBAL`, the 16 limit and the checker notifications (the `C-CALL-CHECKER-PACKAGE` family) as today.
Files: `src/habu/packages.f`.
Verify: spark: the gate's package and using suites through the Habu loop under the feature cell; gate.
Depends: habu-scan-and-interpret-eea996a2 (I4).
Route: Alder (shared: src/habu/packages.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Preflight corrections (2026-09-30; override the lines above where they differ; re-preflight after I4b lands):
- The seven words are interpret-loop keywords, not records (`habu2.f:8351-8357` `EM-INTERPRET-DEFINE-KEYWORDS`). Through the Habu loop they die `E-UNDEFINED: package|using|export` rc 70; the engine answers 0 (pre-change failing check).
- Depends: I4b (`habu-interpret-literal-keywords-0fa50d62`). I8 adds seven rows to I4b's keyword step in `outer.f`; I4b owns the step's shape.
- No provider writes a namespace row or an EXPORT alias from Habu: `ndict-append` (`habu1.f:3299-3320` BNDAPPEND) refuses unless `DEF-TIER-CELL`=1 and `PEND-CELL` owns the slot; the engine writes both rows under `PROT:LSPAN`/`PROT:LOPEN` (`habu2.f:7879-7894`, `8322-8336`) with `C-STORE-NAME` spilling long names at CP (`habu2.f:3205`). I8 owns a new trusted-only record writer (one row or a namespace/alias pair; the interface is fixed at re-preflight): name via `C-STORE-NAME`, fields [0]/[8]/[16]/[40], `NDICT++`, `LHIDXADD` under `PROT-GUARD:CALL`, DICT-CAP `$4D`. That primitive makes I8 an engine-closure leaf (rebuild, chain converging from gen2, gate).
- Files add: `src/habu/outer.f` (rows; `require src/habu/packages.f`), `src/habu/prims.f` + `src/habu/habu1.f` (the writer), `src/habu/kernel-x64.f` (`REFUSE` row beside `tok-imm?`), `docs/x86-64.md` row table, `test/gate-stdlib-cases.f`. Package `PKGS` in `packages.f`; words `TRUSTED: ( -- )`; scanner `parse-name` (`habu1.f:3261`). `packages.f` never requires `outer.f`.
- Inventory to preserve, in order: `C-TASK-LIVE-GUARD` (token, exit 79, `habu2.f:2501`); `C-PACKAGE` `:7963-7985` ($4B, $4A, checker call before seal guard, seal 84 via RESTAB fold `:3509` and protected wid bitmap `layout.f:907`, existing row keeps wids and allocates a private one only when [8]=0 `:7896`, `USE-PKG-SAVE-CELL`, `PKG-*`/`CUR` cells); `C-PUBLIC`/`C-PRIVATE`/`C-END-PACKAGE` `:7987-8028`; `using` 89/90/91/92 (`USE-MAX` 16, `layout.f:1537`) and CHECKER-USING at pre-increment depth `:8032-8109`; `;using` 93; `C-EXPORT` `:8300-8336` (top-level no-op, seal guard, LFIND without used publics, 70, tail rewrite, dup $4E before checker, flag copy IMM/WIDE/MIN-IN, `C-STORE-DEF-NAME` 84). Checker calls: `DECL-CELL`/`TARGET-DECL-CELL` fields at `CHECKER-OWNER-ABI:*-OFF`, absent skipped, `using` notifies DECL then TARGET unless same (`habu2.f:2557-2570`, `7840-7850`); `checker-export` by global name, exit 70 if absent (`:2753`). `checker-owner.f FIELD` refuses absent fields, so `packages.f` needs its own reader. `LCOMPILEDIE` rcs become `throw`s as I4's 70.
- Verify: the gate's package and using suites define with `:` in-process, which the loop refuses until I5e/I6; instead `test/outer-interpret.f` route-equality cases (prelude defines packages/words under the engine loop; cases hold only these keywords, numbers and prelude words): open/close, $4A/$4B, using then bare call, `;using` then E-UNDEFINED, 89-93, ambiguity by two usings, export no-op/alias/70/$4E, hook window installed. In-process calls of `PKGS:*` from a file the engine loop reads cover the seal and task exits and the checker mirror (`E-USING-SHADOW-GLOBAL`, `checker.f:9149`, via a following `:`). Then rebuild, chain, gate.
