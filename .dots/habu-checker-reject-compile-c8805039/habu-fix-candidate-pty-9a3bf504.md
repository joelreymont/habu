---
title: Fix candidate PTY foreground control
status: active
priority: 2
issue-type: task
created-at: "2026-07-16T03:39:56.197919+02:00"
---

Context: test/proc-pty.f exact-candidate direct run exits 130 and echoes input without REPL output; phase 16 invokes GE-PROCESS-PTY under the candidate, contradicting a prior cold-gate green claim. Cause must be proven with wait/signal, process-group, session, controlling-terminal, foreground-pgrp, and cache-execution evidence. Fix the root PTY ownership/job-control invariant in checked Habu/native primitives as required; no skip, waiver, or cache bypass. Add deterministic direct and phase-16 negative/regression coverage. Acceptance: public and rebuilt exact candidate pass direct proc-pty, candidate-backed cold phase 16 is proven executed and passes, focused process gates pass, full exact candidate gates pass.

Claim: agent=checker_loader_fix workspace=.jj-ws/habu-checker-reject-loader-body.
