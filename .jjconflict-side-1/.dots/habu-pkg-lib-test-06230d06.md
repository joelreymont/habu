---
title: Package lib/test/runner.f and retire its lint row
status: open
priority: 2
issue-type: task
created-at: "2026-08-21T10:43:30.460104+02:00"
---

The retirement owner for the exact-path GLOBAL-IMPLEMENTATION? row gtrc-1 is adding for lib/test/runner.f (2026-08-21, measured blanket freeze: editing one body line of an untouched word on pristine master reds E-PACKAGE-OWNERSHIP - the file opens no package and the gate refuses every possible edit, including the repair of the runner's evidence gap). The migration: package runner.f (86 global names) + runner-test.f, qualify/import the ~29 consumer files (test/run-lib.f, gate-pool.f, every gate-*-lib.f), per the seal campaign's using-import doctrine; then the row leaves and its positives become fixture negatives (asm.f precedent). Not small - own review.

RE-SCOPED AS THE GATE-CLUSTER PACKAGING CAMPAIGN (2026-08-21, after the
four-round measurement chain on 395eb72a - waiver / 30-line / 440-edit /
cluster-probe / silent-green stop; the chain is in gtrc-1's report and the
runner.f exception row's header). OPENING STATE, all banked:
- First leg DONE and held: runner pair packaged+stripped (GT 49/35, GTT with
  the WRITE-SRC shadowing precedent), 24 consumers qualified, 558 rewrites,
  loads clean, self-test green, script falsified on a hostile fixture -
  packaging.diff in the session scratchpad, and the lane commit if it
  survives; re-derive from the diff if not.
- Ring-3 census PROVEN EMPTY by the static call-graph method (better
  instrument than the lint run - each ring-2 file's global callers are all
  ring-2): the cluster is exactly runner pair + gate-common-lib(124 names),
  gate-pool(271), gate-build-common(112), gate-engine-lib(186),
  gate-debug-lib(16), gate-stdlib-inline-lib(69), gate-build-hbb(4).
- HARD REQUIREMENTS the first attempt measured: the transform MUST be driven
  by tools/lint/def.f's 57 definer forms (a six-definer hand classifier
  already missed live defers); NINETEEN owned names live in string literals
  with three load-bearing classes needing per-site judgment: (a) GE-FILES: is
  itself a def.f defining form (classifier + fixtures edit), (b) GSI-INCLUDE/
  FORK-INCLUDE/REQUIRE are matched AS TEXT by schedule-lint.f:583-585 -
  renaming without the strings = registrations invisible, exit 0, the
  silent-green worst case, (c) gate-engine-lib.f:779-791 builds child source
  calling names and greps source text for them. Also: rc-75 package nesting,
  rc-82 engine-bare classes, undefine sweep.
- CLOSING GATES beyond the battery: schedule-lint and def.f fixtures
  re-verified explicitly; the runner.f exception row retired, its positives
  becoming fixture negatives (asm.f precedent).
Two intricate files (engine-lib 16 inner pkgs, inline-lib 9) wrap global
REGIONS with close/reopen per the seal-4 shape.
