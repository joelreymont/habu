---
title: gate-runner-entry standalone load dies rc 77
status: closed
priority: 2
issue-type: task
closed-at: "2026-07-08T00:00:00+02:00"
close-reason: Fixed by the engine lane's DICT-CAP raise (8192 -> 16384, DICT-SIZE $61000 -> $C1000; maki-type-families 9004102c) plus the labeled capacity exit. Verified on a fresh install --force bin/hb (sha f10acb38): the exact GR-USAGE invocation `bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- <phase>` now loads clean - unknown phase exits rc 64 with the full usage banner (ndict after the closure = 9325, comfortably under 16384; it was 8191 vs the old 8192 cap = the exhaustion). The silent lone-':' is also gone: forcing overflow now prints `hb: dictionary full at: :` (label + offending token) on stderr with rc 77, no bare byte. Regression landed: test/gate-runner-entry-test.f (GRE-TEST-UNKNOWN-PHASE spawns the exact closure, asserts EXIT/rc 64/usage banner naming both source files) - red on the pre-DICT-CAP engine (rc 77 dict-full before GR-MAIN, cited from the 2026-07-08 RCA commit 3112de03) and green now. Wired next to test/seal.f in BOTH dispatch manifests (GSI-TAIL-PROCESS fork in test/gate-stdlib-inline-lib.f, which runs in the resident test/run.f gate, and the TEST:SUITE gate-runner-entry-load in test/gate-stdlib-cases.f) so it cannot rot. No residual check-core hook interaction: the poison was pure definition-count, proven by a trivial filler def arming it at the brink (RCA §H1).
created-at: "2026-07-08T09:17:14.697236+02:00"
---

bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f -- <phase> (the exact usage GR-USAGE documents, test/gate-runner-lib.f) dies rc 77 printing a single ':' byte. Bisected via require-prefix files: the first red require is tools/check-core.f when loaded after the 41 preceding gate-runner-support requires (check-core alone fails only E-UNDEFINED CLEANUP-RUN rc 70 = missing prereqs, so the trigger is an interaction, likely check-core's load-time CHK-CHECK-HOOK set-check install rejecting a later gate-runner-support definition). The gate itself never spawns gate-runner-entry (no references in run-lib/run-worker*/run-resident; phases run as resident forks via run-worker.f), so the gate stays green and this is manual-slice tooling debt. Also note the 1-byte ':' diagnostic despite the de-masked top-level throw reporting (44efc694) - the failure path deserves RCA for its silence as well as its cause. Repro artifacts: build a prefix file of gate-runner-support requires and load it; red appears when tools/check-core.f is appended.

## RCA complete (2026-07-08, on be106db9): engine dictionary cap exhaustion

The check-core hook hypothesis from the initial note is FALSIFIED; the cause is
resource exhaustion in the engine, and the fix is engine-lane. Dig chain:

- Hypothesis 1 (CHK-CHECK-HOOK rejects a later definition): falsified - the
  poison predates check-core's hook sites. With the 41 preceding requires
  loaded plus check-core truncated to line 948 (before any set-check install
  runs at that point), ONE further trivial definition succeeds and the SECOND
  dies; any filler definition arms it. Pure definition-count threshold.
- Hypothesis 2 (lint token/intern capacity; E-LINT-TOKEN-CAP = 77 in
  tools/lint/token.f:5): falsified - TN# is 0 at the brink, and the death is
  uncatchable: `s" : ZZP ( -- ) ;" ' evaluate catch` never returns a code
  (catch cannot intercept it), while a control `s" 70 throw" ' evaluate catch`
  at the same brink behaves normally. So the death is a process exit, not a
  throw; the 77 == E-LINT-TOKEN-CAP equality is coincidence.
- Evidence (emitted definer code, src/habu/habu2.f): the definition paths at
  :1514, :1788 (C-TRUSTED), :1858, :3142 each guard
  `NDICT >= DICT-CAP` and on overflow execute
  `write(2, [TKA-CELL], [TKL-CELL])` - the CURRENT TOKEN bytes - then
  `exit_group($4D)`. $4D = 77. The current token at a definition is `:`,
  which is exactly the observed 1-byte output; exit_group is uncatchable and
  message-free, which is exactly the observed silence. The twin CP-overflow
  arm exits $4C = 76 the same way. The 44efc694 de-mask covered BTHROW
  top-level throws, not these raw capacity exits.
- Measurements: DICT-CAP = 8192, DREC = 48 (src/habu/layout.f:13,18;
  DICT-SIZE $61000 = 8192 x 48 + slack, so the cap is layout-sized). Fresh
  bin/hb after lib/errors.f: ndict = 3070 (baked engine+prefix). At the brink
  (41 requires + check-core lines 1-948): ndict = 8191. The remaining
  gate-runner-support closure (check-core tail + requires 42-53 incl.
  gate-*-lib) needs roughly 1.2-1.5k more entries, so the full closure wants
  ~9.5k dictionary entries against a cap of 8192.
- Conclusion: the gate-runner-support tool closure outgrew the engine
  dictionary. In-gate nothing notices because phases run as resident forks
  loading per-phase subsets; the documented standalone invocation is the only
  path that loads the whole closure into one process.

FIX SPEC (engine lane - src/habu/layout.f + src/habu/habu2.f; this lane stops
here per territory rules):
1. Raise DICT-CAP with headroom (16384 doubles it) AND grow DICT-SIZE in step
   (cap x DREC 48 -> $C0000 + slack; today's $61000 fits exactly 8192 x 48).
   Image-layout constants shift, so this is a seed-affecting engine change
   (rebuild + fixpoint + seal watermarks SEAL-NDICT-CELL semantics unchanged).
2. Name the capacity exits: before the token write, emit a fixed diagnostic
   (e.g. "hb: dictionary full (DICT-CAP) at: " / "hb: code space full at: ")
   then the token and a newline, keeping the deterministic exit codes 77/76.
   An operator seeing ':' + rc 77 today has zero clue; the memory cost is two
   string labels next to LUNCMSG (habu2.f:304 pattern).
3. Engine regression: a generated fixture defining past DICT-CAP asserting the
   named diagnostic and rc 77 (engine-suite territory).
4. AFTER the engine fix lands, this lane adds the standalone-entry regression
   (test/gate-runner-* scope): spawn the exact GR-USAGE invocation with an
   unknown phase token and assert rc 64 (GR-USAGE die) - proving the whole
   closure loads standalone - wired into a gate suite so it cannot rot again.

Secondary anomaly found while reducing (dotted separately,
habu-eof-inside-a-7a539941): a file ending mid-definition exits rc 0 silently in
a small context and crashes rc 134 in a large one - EOF-in-definition is
neither rejected nor consistent.

## Engine half LANDED (2026-07-08, engine lane)

Root cause per the 2b RCA: the gate-runner-support closure needs ~9.5k dict
records against DICT-CAP 8192, and the definer capacity arms wrote only the
current token (a lone ':') before exit_group(77) - label-free, uncatchable.

1. CAPACITY: DICT-CAP 8192 -> 16384; CFSTK-OFF $60000 -> $C0000 (= cap x
   DREC); DICT-SIZE $61000 -> $C1000 (CFSTK + $1000; code area follows at
   DBASE+DICT-SIZE inside REGION $400000 - still ~3.2MB code headroom).
   Dict-name hash index doubled in step: HIDX-SLOTS $4000 -> $8000,
   HIDX-BYTES $10000 -> $20000, and the six hardcoded $3FFF probe masks in
   habu1.f now derive as `HIDX-SLOTS 1 -` (drift-proof). tools/imgdump.f
   arrays scale via the constant. Stage0 mirrors the three layout constants
   (no HIDX in stage0 - linear find). Seal watermark semantics unchanged
   (count-based; focused test/seal.f green at the new cap).
2. NAMED EXITS: new LDICTFULL/LCODEFULL 24-byte labels beside LUNCMSG;
   C-DIE-DICT-FULL/C-DIE-CODE-FULL emit label + token + newline, keeping the
   deterministic exit codes ($4D=77 dict, $4C=76 code space). Replaced the
   raw arms in C-TRUSTED, C-DEFER-ROOM, and C-COLON; C-QUALIFY-CAP prefixes
   its $4D C-QUALIFY-FAIL with the label, and C-QUALIFY-FAIL now terminates
   its token with a newline (both its callers benefit). Stage0
   C-COLON-DICT-ROOM mirrors label+token+newline+77; its code-room arm keeps
   the already-labeled static C-EXIT76 message.
3. REGRESSION (committed RED first): test/gate-engine-lib.f GE-DICT-FULL in
   the engine runtime slice - a Habu-generated program of DICT-CAP+1
   unchecked definitions (scales with the baked layout.f constant) asserts
   rc 77 + `hb: dictionary full at: ` on stderr. Child runtime ~0.2s (16400
   unchecked defs) - negligible against the slice budget.

Fail-closed note: the first rebuild attempt was itself rejected by the
staged-source certify (bare `ptr` in a new emitter sig -> stage2-src
rejected rc 70, named diagnostic) - the build path caught the typed-effect
error before any image was produced.

Remaining (2b lane, per the split): the standalone gate-runner-entry
regression proving the original 41-require closure now loads.
