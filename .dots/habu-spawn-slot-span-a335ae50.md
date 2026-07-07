---
title: Spawn-slot span generation inheritance
status: open
priority: 2
issue-type: task
created-at: "2026-07-08T06:34:35.552779+02:00"
---

gate-stats generations (GS-GEN$, test/gate-stats.f) qualify span/test-row labels per process; fork children get unique generations via GT-POOL-GEN-CHILD! (test/gate-pool.f), but SPAWNED pool slots boot fresh at gen 0 (GS-GEN-INIT), so every spawned gate-runner shares generation 0 with the top parent and with its spawn siblings, and their fork children can mint identical gen paths (two processes both producing 0-1). Cross-subject qualified-label collisions across spawn boundaries therefore remain possible; they are fail-closed today (colliding test rows are qualified identically, so GS-LABEL-DUP-GUARD dies; suppression is process-local and cannot mis-fire cross-process). Full uniqueness needs: (1) GT-POOL-SPAWN injecting a per-slot generation env var (parent gen + '-' + slot seq, e.g. HABU_GATE_GEN) with REPLACE semantics - PROC-ENV+ appends and PROC-ENV-INHERIT-MISSING may already have copied the parent process's own value, and first match wins in the child getenv, so lib/process-env.f needs a replace-or-add word first; (2) GS-GEN-INIT reading that env (validated by GS-GEN!, digits+dashes) before defaulting to 0. Follow-up to closed dot habu-span-label-identity-af085586.
