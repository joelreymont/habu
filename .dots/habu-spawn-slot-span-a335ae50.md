---
title: Spawn-slot span generation inheritance
status: closed
priority: 2
issue-type: task
closed-at: "2026-07-08T00:00:00+02:00"
close-reason: Spawned pool slots now inherit their slot generation. lib/process-env.f grew a typed replace-or-add API (PROC-ENV-SET over PROC-ENV-NAME-IDX/PROC-ENV-ROW-Z; PROC-ENV+ refactored over the same row writer) with focused replace-vs-append tests, manifest rows, and docs. GT-POOL-SPAWN exports HABU_GATE_GEN = parent gen + "-" + slot seq via PROC-ENV-SET (replacing any row PROC-ENV-INHERIT-MISSING copied from the parent's own environment); GT-POOL-GEN-CHILD! shares the same GT-POOL-SLOT-GEN$ builder. GS-GEN-INIT adopts the env value; absent means root "0"; malformed dies rc 1 with a diagnostic naming the variable (GS-GEN-OK? predicate) instead of an opaque low-byte exit. Proven red-to-green by GPT-SG-CASE (spawned slot at parent gen 5 emits "g5-<seq>:"-qualified spans, never "g0:") and GST-TEST-GEN-ENV (valid adopted rc 0; malformed rc 1 + stderr names HABU_GATE_GEN); full cold gate green with unchanged counters (spans=221 load-spans=28 span-stray=186/186/0 label-dup=0).
created-at: "2026-07-08T06:34:35.552779+02:00"
---

gate-stats generations (GS-GEN$, test/gate-stats.f) qualify span/test-row labels per process; fork children get unique generations via GT-POOL-GEN-CHILD! (test/gate-pool.f), but SPAWNED pool slots boot fresh at gen 0 (GS-GEN-INIT), so every spawned gate-runner shares generation 0 with the top parent and with its spawn siblings, and their fork children can mint identical gen paths (two processes both producing 0-1). Cross-subject qualified-label collisions across spawn boundaries therefore remain possible; they are fail-closed today (colliding test rows are qualified identically, so GS-LABEL-DUP-GUARD dies; suppression is process-local and cannot mis-fire cross-process). Full uniqueness needs: (1) GT-POOL-SPAWN injecting a per-slot generation env var (parent gen + '-' + slot seq, e.g. HABU_GATE_GEN) with REPLACE semantics - PROC-ENV+ appends and PROC-ENV-INHERIT-MISSING may already have copied the parent process's own value, and first match wins in the child getenv, so lib/process-env.f needs a replace-or-add word first; (2) GS-GEN-INIT reading that env (validated by GS-GEN!, digits+dashes) before defaulting to 0. Follow-up to closed dot habu-span-label-identity-af085586.

## CLOSED: implemented (2026-07-08, from head 7e82a0bd)

Landed exactly the two pieces above, plus the loudness fix the review demanded:

- lib/process-env.f: `PROC-ENV-NAME-IDX` ( ptr u8 len -- n ) finds a prepared
  row by name (-1 when absent; `PROC-ENV-HAS-NAME?` now derives from it);
  `PROC-ENV-ROW-Z` factors the NAME=VALUE arena writer out of `PROC-ENV+`;
  `PROC-ENV-SET` replaces an existing row in place or appends when absent.
  Focused tests pin the replace-vs-append distinction (set-replaces /
  set-appends-new / append-keeps-dup, with child-visible env assertions);
  manifest rows and docs/stdlib.md updated.
- test/gate-pool.f: `GT-POOL-SLOT-GEN$` builds parent gen + "-" + slot seq;
  `GT-POOL-GEN-CHILD!` adopts it in the fork child, and `GT-POOL-SPAWN` exports
  it as HABU_GATE_GEN through `PROC-ENV-SET` so exactly one row reaches the
  spawned child even when PROC-ENV-INHERIT-MISSING already copied this
  process's own value.
- test/gate-stats.f: `GS-GEN-INIT` adopts HABU_GATE_GEN; absent means root "0";
  malformed dies rc 1 with "gate-stats: malformed HABU_GATE_GEN in environment"
  via the non-throwing `GS-GEN-OK?` predicate (a bare E-STR-BOUNDS throw exited
  104 with NO message - an opaque exit, not a loud failure). `GS-GEN!` keeps
  named throws for programmatic callers.

Red-to-green evidence: GPT-SG-CASE (test/gate-pool-test.f) - pool-spawned child
under parent gen "5" emitted "g0:spawn gen label" before (count g5- expected 1
got 0), now emits exactly one "g5-<seq>:"-qualified span; GST-TEST-GEN-ENV
(test/gate-stats-test.f) - malformed env exited 0 before (silently ignored),
now rc 1 with stderr naming HABU_GATE_GEN, valid gen adopted rc 0.

Non-pool spawns (TR-SPAWN-CAPTURE top captures, helper/inner-hb spawns that
inherit the full environment) intentionally carry the SPAWNING process's
generation: they are part of the same test slot's identity scope, and their
labels pair within one process. GS-LABEL-DUP-GUARD remains the net for any
same-generation cross-subject reuse.
