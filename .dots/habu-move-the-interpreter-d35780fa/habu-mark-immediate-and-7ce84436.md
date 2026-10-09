---
title: "Mark immediate and declare cast: in the Habu loop"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T14:29:58.194989+03:00"
closed-at: "2026-10-10T03:08:54.210979+03:00"
close-reason: "immediate and cast: run in the Habu loop through two new writer rows, imm-mark and def-cast (prims.f, ARM64 habu2.f DEFWRITE, x86 kernel-x64.f, Gforth prims.fs); the cast: registrar stays typed Habu in definers.f (DEF-DECLARE), and the missing-name refusal lands at end of input as the engine's does. Gates on the rebased tree (master 8d184aaf): native build OK, aot-wid-build, cold-naming, outer-interpret 216 agree, engine-writers ok, test/run.f 632/632, generation chain converges (gen 5 = gen 4), host-test 109/109, error-code-lint 0. Fable review ACCEPT; its three fixes got a focused ACCEPT and built byte-identical. x86 images built, not run (no x86 host). r22 and r40 on Gforth are habu-hand-the-rest-32631946's check."
---

Problem: at interpret level `immediate` (`C-IMMEDIATE`, `src/habu/habu2.f`) and `cast:` (`C-CAST`) are still the assembly interpreter's, so the Habu loop stops at either. Neither has a writer row: `immediate` sets `DNAME-IMM` on the record just published, under the record protection (`LPROTREC`), and `cast:` is the `CHECKER-DEFCAST` registration (`src/core/checker.f`) plus the identity word it publishes. Split from habu-close-definitions-with-8ace78d8 (I5e), which landed `;` and the `def-close` row and found both undesigned.
Acceptance: `immediate` and `cast: NAME ( in -- out )` in the Habu loop leave the dictionary, flags and checker state the engine's dispatch leaves, including `cast:`'s missing-name refusal (`C-CAST-DIE-NO-NAME`); each new row follows `def-close`: a `prims.f` row with an ARM64 body in `habu2.f` DEFWRITE and an x86 body in `src/habu/kernel-x64.f`; cases beside `test/outer-interpret.f` compare the two loops. The Gforth host gives each new row a body in src/host/gforth/prims.fs, and r22.f and r40.f under the Habu loop then match native.
Design: ~/.cache/tmp/carl-cast/design.md (rows `imm-mark ( -- )` and `def-cast ( -- )` plus an OUTER `min-in-mark` export; the registrar stays in typed Habu in definers.f; measured refusal order and probes in ~/.cache/tmp/carl-cast/probes/).
Files: `src/habu/definers.f`, `src/habu/outer.f`, `src/habu/prims.f`, `src/habu/habu2.f`, `src/habu/kernel-x64.f`, `src/host/gforth/prims.fs`, `src/host/gforth/reader.fs`, `test/outer-interpret.f`, `test/engine-writers.f`, `test/engine-writers-prepare.f`, `test/engine-writers-child.f`, `test/x86-64-kernel-definition.f`, `docs/x86-64.md`, `docs/architecture.md`.
Verify: spark: `test/outer-interpret.f`, `test/engine-writers.f`; rebuild, chain, gate; ThinkPad: `test/x86-64-kernel-definition.f` image statuses.
Depends: none open.
Route: Alder (shared: src/habu/definers.f, src/habu/outer.f, src/habu/prims.f, src/habu/habu2.f).
Ownership: the claimant.
Claim: agent=worker-max (lead carl) workspace=.jj-ws/carl-cast.
