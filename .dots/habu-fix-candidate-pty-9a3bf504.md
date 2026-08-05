---
title: Fix candidate PTY foreground control
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T03:39:56.197919+02:00"
---

Context: test/proc-pty.f exact-candidate direct run exits 130 and echoes input without REPL output; phase 16 invokes GE-PROCESS-PTY under the candidate, contradicting a prior cold-gate green claim. Cause must be proven with wait/signal, process-group, session, controlling-terminal, foreground-pgrp, and cache-execution evidence. Fix the root PTY ownership/job-control invariant in checked Habu/native primitives as required; no skip, waiver, or cache bypass. Add deterministic direct and phase-16 negative/regression coverage. Acceptance: public and rebuilt exact candidate pass direct proc-pty, candidate-backed cold phase 16 is proven executed and passes, focused process gates pass, full exact candidate gates pass.

Claim: unassigned (stale claim stripped 2026-08-04: the named workspace no longer exists on disk or in `jj workspace list`).

RECOVERY POINTER 2026-07-19: the linear authority registry lib/process-pty-handle.f landed on master in d093449e with getpid support in 98abafeb. Remaining candidate foreground-control implementation exists only in the held history: lib/process-pty-io.f, lib/engine-candidate.f, and src/os/{linux,macos}/proc-watch.f. Use ead2ce14 only as design/evidence input for those unlanded slices; re-derive against the landed handle API and current master. Do not restore the old handle file or raw-rebase the 24-commit lineage.
Release 2026-07-19: claim agent workspace was destroyed in the .jj-ws loss incident (see LESSONS.md); lane returned to open for re-dispatch.
