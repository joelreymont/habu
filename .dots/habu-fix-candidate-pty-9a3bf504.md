---
title: Fix candidate PTY foreground control
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T03:39:56.197919+02:00"
---

Context: test/proc-pty.f exact-candidate direct run exits 130 and echoes input without REPL output; phase 16 invokes GE-PROCESS-PTY under the candidate, contradicting a prior cold-gate green claim. Cause must be proven with wait/signal, process-group, session, controlling-terminal, foreground-pgrp, and cache-execution evidence. Fix the root PTY ownership/job-control invariant in checked Habu/native primitives as required; no skip, waiver, or cache bypass. Add deterministic direct and phase-16 negative/regression coverage. Acceptance: public and rebuilt exact candidate pass direct proc-pty, candidate-backed cold phase 16 is proven executed and passes, focused process gates pass, full exact candidate gates pass.

Claim: agent=checker_loader_fix workspace=.jj-ws/habu-checker-reject-loader-body.

RECOVERY POINTER 2026-07-18 (workspace forensic sweep): the PTY supervisor / candidate-engine / process-watch IMPLEMENTATION for this dot exists only in held workspaces, never on master: lib/process-pty-handle.f + lib/process-pty-io.f (package PROCESS-PTY: linear supervisor authority registry, owner-PID guards, close-syscall status), lib/engine-candidate.f (package ENGINE-CANDIDATE), src/os/{linux,macos}/proc-watch.f (package PROC-WATCH). Fullest tip: workspace habu-pty-integration at commit ead2ce14 (24-commit lineage); older siblings habu-pty-trust-scan ae50b206, habu-nested-trust-owner 22719e26, habu-engine-trust-ratchet f491ffa8, habu-checker-reject-loader-body 4695172f, owner-pid-integration 85925ea3 are earlier attempts on the same lineage. Recover from ead2ce14 (rebase onto current master, re-review, land or explicitly retire per piece); retire the older siblings only after the fullest tip is landed or adjudicated. Do not delete these workspaces before then.
Release 2026-07-19: claim agent workspace was destroyed in the .jj-ws loss incident (see LESSONS.md); lane returned to open for re-dispatch.
