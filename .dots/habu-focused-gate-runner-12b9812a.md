---
title: Focused gate-runner slices exit 77 with lone-colon output
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T20:39:00.798054+02:00"
---

printf '' | bin/hb --load test/gate-runner-support.f test/gate-runner-entry.f
-- lint-libs-ptx (and -- lint-manifest) exits rc=77 printing a single ':' byte
on a pristine fable tree. Full suite path bin/hb --load test/run.f unaffected.

BLOCKED-ON: the planned fable rebase onto maki-type-families plus the bin/hb
refresh that comes with it. The fix has already landed on that branch; nothing
to implement on fable. Do NOT edit src/habu or raise engine caps here.

## Root cause (verified on fable 2026-07-09)

Engine dictionary-capacity exhaustion, not a lint. The original
E-LINT-TOKEN-CAP hypothesis is RETRACTED: the 77 collision with
tools/lint/token.f is coincidental, and no lint runs during a plain require
chain.

- Loading `test/gate-runner-support.f` ALONE (no entry, no GR-MAIN, no args)
  already exits 77 + lone ':' — the failure is at LOAD time, before dispatch.
- rc 77 = the engine's `$4D` dictionary-full exit. `C-QUALIFY-CAP`
  (src/habu/habu2.f:1465-1469) checks `NDICT >= DICT-CAP` and calls
  `C-QUALIFY-FAIL` (habu2.f:1461-1463), which writes only the current
  def-name token (DEF-TKA/DEF-TKL) to stderr and exits $4D. Sibling guards:
  C-TRUSTED habu2.f:1669-1671, defer path habu2.f:1739-1740.
  DICT-CAP = 8192 (src/habu/layout.f).
- The lone ':' is that token write: the overflow fires at the start of a
  colon definition, so the current token is the ':' definer itself. The abort
  is baked engine machine code — no Habu-level diagnostic can surface.
- Measured with `ndict@`: bare engine NDICT=2162; the support require chain
  through test/gate-dictionary-lib.f reaches NDICT=8185 (7 below cap);
  test/gate-debug-lib.f's next definitions tip it over. Brink proof: that
  prefix + 7 filler `:` definitions loads rc=0; adding an 8th exits rc=77
  with the lone ':' — exactly the reported signature.
- The full suite never sees this because it forks per-family workers
  (test/run-worker-{stdlib,engine,diag,dict,debug}.f), each loading only its
  family subset; only the standalone gate-runner entry loads the whole
  closure (~9.3k dictionary entries) in one process.
- Load-time overflow means EVERY phase token failed identically
  (lint-libs-ptx, lint-manifest, dictionary, debug, repair, diag-*,
  check-cli all rc=77 + ':').

## Fix (already landed on maki-type-families)

Commit db88a576 "Regress gate-runner-entry standalone load; close dot"
(their dot habu-gate-runner-entry-81c84af0) with engine commit 9004102c:

- DICT-CAP 8192 -> 16384 (DICT-SIZE $61000 -> $C1000).
- Labeled capacity exit: `hb: dictionary full at: <token>` instead of the
  bare token byte, so the failure attributes itself.
- Regression test/gate-runner-entry-test.f: spawns the exact
  gate-runner-support + gate-runner-entry closure and asserts rc 64 + usage
  banner for an unknown phase (proves the whole closure loads under the new
  cap); wired into both dispatch manifests.

## Fable action after the rebase + bin/hb refresh

1. Run both focused commands and confirm rc=0 with GROUP:/PASS output:
   `printf '' | bin/hb --load test/gate-runner-support.f
   test/gate-runner-entry.f -- lint-libs-ptx` and `-- lint-manifest`.
2. Confirm test/gate-runner-entry-test.f rides along in the dispatch
   manifests and passes in the full suite.
Then close this dot.

Note: exploratory fable-side harness edits (trimming gate-runner-support.f to
the stdlib family + an off-band dispatch redirect in gate-runner-lib.f) were
verified to work but REVERTED: the upstream fix keeps the entry's full
closure and raises the engine cap instead, and its regression test spawns
that exact closure, so the trim would conflict with the landed fix.
