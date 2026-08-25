---
title: event-closure-test flaky E-STR-CAPACITY in gate fork
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T23:33:07.384781+02:00"
---

tools/event-closure-test.f failed once under the full gate (stdlib/tail-pure parallel group) with 'fork worker throw rc -2201' (E-STR-CAPACITY, lib/errors.f:28) on tree 77f2e0bd (item-7 rebase onto 69e152562090). Standalone run of the same test on the same tree passes; immediate gate rerun passes. Not deterministic; not path-length (both gate roots were 15-digit hb-gate ids). Suspect a parallel-group race or a borderline string-builder capacity in the fork-worker capture path. Evidence log (first red run): /var/folders/98/l2ptpkyn41q7d3sp6x4xp87m0000gn/T//hb-gate-887218509776916-7/pool-0-12-23-err.log. Next: reproduce under repeated parallel gate runs, instrument the throw site (WHY-THREW-style catch reporting the throwing word), fix capacity/race at root cause.

## RCA progress (2026-07-06, workspace tfam-2b-iii, head e4ce7798)

STILL OPEN — not reproduced, not fixed. A rare race we cannot reproduce is not fixed by a capacity-bump guess; the armed self-identifying trap IS this checkpoint's deliverable, and closure awaits the next reproduction WITH a WHY-THREW dump.

Reproduction attempts (all against a merge-gate-seeded + fixpoint-rebuilt bin/hb):
- 40 full `test/run.f` + 300 `test/gate-stdlib.f -- tail --pool-slots 20` runs under normal load: 0 reproductions.
- 3 concurrent full gates (machine saturation): only produced 5s WAIT timeouts (over-subscription), NOT the E-STR-CAPACITY flake; one false positive where the grep matched a sha256 substring. tail-pure itself passed. Saturation is the wrong pressure — the original flake was a single full-gate run at normal load.

Every E-STR-CAPACITY (-2201) source reachable from this test was ruled out analytically AND with bounded probes:
- SB (lib/string.f, cap 1024, shared): the test's builders all `SB-RESET` first; with the real host TMPDIR (49 chars) the largest build (ECT-MIXED-ENTRY$, 3 require lines) is ~336 bytes, not borderline. `s"`/`S\"` string literals do NOT use SB at runtime OR at compile time (probed at SB-LEN=1000: no growth, no throw) — so `included` re-compiling event-closure-test.f's ~15 S\" literals in the fork child cannot overflow SB via inherited COW SB-LEN.
- JOIN-PATH (lib/string.f BUF-CHECK-LEN): dst buffers are FS-PATH-CAP=1024; resolved paths ~100. Safe.
- CK / CK-ROW (lib/content-key.f, caps 256KB / 1184): ECT-CLOSURE-KEY `CK-RESET`s CK-U; CK-FILE+ -> CK-ROW-FILE-PREFIX `CK-ROW-RESET`s before appending; file content is tiny. Content-key cache is NOT active (no cache root set or inherited, so no shared-file race). SHA256-FILE streams in bounded chunks and returns a code (not -2201); FILE-META throws E-FS-STAT (not -2201) on a stat race.
- TREC (lib/test/record.f, cap 512): TFAIL records are ~54 bytes and `TREC-RESET` first. Safe.
- source-discovery / event-closure-lib throw E-DISC-CAPACITY, not E-STR-CAPACITY.
- COW-inherited buffer lengths: every buffer the test touches is reset before its first use in the child (SB by TMPDIR-MKDIR/FS-MUT-BUILD-TEMP-TRY, CK/CK-ROW/TREC by their words), so an inherited nonzero length is cleared before any append.

Armed instrumentation (committed): tools/why-threw.f WHY-THREW-DUMP is called by test/gate-pool.f GT-POOL-FORK-THROW, so ANY fork worker's throw now prints, before dying, one `WHY-THREW:` line per shared string-builder fill/cap (SB, CK, CK-ROW) alongside the existing `fork worker throw rc N`. The next occurrence self-identifies the overflowing buffer in the capture out.log; feed that back here to close.
