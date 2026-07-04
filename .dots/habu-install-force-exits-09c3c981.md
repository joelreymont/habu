---
title: install --force exits 0 when stale-seed refresh crashes
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:36:09.876260+02:00"
---

Fail-open in the fixpoint install: running the refresh (install --force) with a STALE seed bin/hb that predates the current engine prefix (missing CHECKER-DEFFAMILY after item 6) crashed the refresh child with E-UNDEFINED + SIGABRT (rc 134, crash-reg hex dump on stdout) yet the install command exited 0 and left the stale binary in place — repro 2026-07-04 main tree, task log: install output = 3 hex lines, exit 0, bin/hb unchanged, subsequent test/run.f crashed E-UNDEFINED: CHECKER-DEFFAMILY. Expected: any crashed/nonzero child in the refresh chain fails the install (E-BUILD-STATUS), bin/hb untouched, nonzero exit. Find the swallowed rc in tools/build-fixpoint.f (BF-RUN-* / BF-RC0 path for the stage that loads the current prefix under the seed) and add a red-first regression: a seed missing a required word must make install exit nonzero. Related lesson: workers must seed bin/hb from a CURRENT engine (main tree bin/hb is refreshed after every engine merge from now on).
