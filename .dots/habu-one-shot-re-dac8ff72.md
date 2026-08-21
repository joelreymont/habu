---
title: One-shot re-measure tool for all size ratchets
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T16:31:12.721005+02:00"
---

Orchestration friction recurs when an engine/source landing changes census,
CODELEN, floor distance, whole-file size, and per-region budgets. Build one
checked Habu tool, `tools/re-measure.f`, run only by `bin/hb` with no shell or
host-language measurement or reducer logic. This focused tool runs the canonical fixpoint and size
map once, derives every committed row from the same authenticated artifact,
fails closed if the map does not reconcile, and emits a deterministic proposed
update plus delta/provenance report. Applying the proposal is an explicit
reviewed action; the tool must never silently rewrite unrelated files or predict
measurements. Red-first: a fixture with stale rows reports every exact change,
the reviewed application makes the owning size/census tests green, and a second
measurement emits an empty proposal. It is not a full merge gate, does not run
unrelated Maki, PTX, or native suites, and must not cause redundant full gates.

Claim: RELEASED 2026-07-21. The empty `fable-remeasure` workspace remains
historical evidence; this leaf is open and unassigned.
