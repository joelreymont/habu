---
title: Validate bootstrap load paths
status: active
priority: 1
issue-type: task
created-at: "2026-07-21T22:42:54.547671+02:00"
---

Invariant: every command in bootstrap documentation that names repository load paths is executable on the exact tree it documents. The current native refresh recipe still names the removed tools/date.f path after the date module moved to lib/date.f, so the first prescribed recovery step exits 74 before any build. A one-line documentation edit would rot again because build source lists, refresh drivers, and prose currently repeat path inventories independently.

Make one checked canonical source of truth for the native refresh dependency list and derive or validate the documentation recipe against it. The validator must parse fenced commands without executing destructive publication, resolve every load path, compare ordered dependencies with the build driver, and fail on missing, renamed, duplicated, reordered, or extra modules. Keep the no-binary recovery launcher boundary unchanged; new validation and generation logic must be checked Habu. Correct the date path only as part of this durable contract.

Prove the documented refresh command succeeds from a stale but supported native engine, a missing path fails the documentation gate, ordering drift and duplicate entries reject, generated or validated text is deterministic, and bootstrap, recovery, fixpoint, host, file-map, documentation, and full native gates pass. Add the documentation command gate to the exact module-relocation workflow so future moves cannot land with stale bootstrap instructions. Measure validation time and source duplication; require one authoritative ordered inventory and no hidden fallback paths.

Design decision 2026-07-21: the canonical refresh dependency list belongs in
a self-contained checked Habu entry module, not in Markdown. The documented
refresh command names that one entry module and a build verb. The entry module
requires the existing library preamble in dependency order, loads the existing
fixpoint implementation, and enters its fail-closed command-line boundary; it
does not duplicate build logic. A checked documentation test locates the exact
fenced refresh command, verifies its token sequence and every referenced path,
and proves missing, duplicate, reordered, and extra dependency mutations reject.
Keep the audited no-binary shell launcher unchanged.

Claim: agent=fork-bootstrap-path workspace=.jj-ws/habu-validate-bootstrap-load-927d5544
