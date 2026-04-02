---
title: Harden trusted-root load contract
status: closed
priority: 1
issue-type: task
created-at: "\"2026-04-01T22:06:02.091380+02:00\""
closed-at: "2026-04-02T16:58:22.309899+02:00"
close-reason: done; relative loads now resolve only through explicit trusted roots, basename trimming/cwd guessing removed, zig build ok, zig build test unchanged 5-error baseline, startup stdlib load bug remains separate
blocks:
  - habu-unify-maxima-manifest-702701ab
---

Problem: generic load still permits ambient roots, raw relative loads, and guessed alternate candidates. Acceptance: trusted roots are explicit, relative resolution stays inside them, and basename trimming or cwd guessing is gone. Files: lib/maxima-loader.lisp:6-26,30-159; lib/stdlib.habu:7319-7347; src/interp/repl.zig load path logic. Verify: path escape probes fail closed and provenance is recorded. Blockers: habu-unify-maxima-manifest-702701ab.
