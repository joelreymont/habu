---
title: Integrate explicit JSON writer cutover
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T16:07:53.738654+02:00"
blocks:
  - habu-migrate-ptx-manifest-3cf77235
  - habu-migrate-build-report-9e7244d3
  - habu-migrate-diagnostic-json-ebf17ff0
  - habu-migrate-json-writer-c4063806
---

Why: the explicit provider and each caller cut are green checkpoints on one unpublished feature branch, but master must see only the final hard cut. Result: assemble the reviewed checkpoints, resolve conflicts, delete every module-global JSON-WRITE field, old singleton emitter, raw JSON-WRITE:$ word, zero-argument RESET, singleton call, and stale manifest or documentation reference, and retain only the explicit writer plus one canonical COPY result family. Existing singleton words are never forwarded, reimplemented, or published as compatibility; no child reaches master before this deletion. Owner: atomic hard-cut deletion, shared load metadata, and final publication boundary only; no new behavior. Production red: the branch still contains the original singleton solely for callers not yet cut. Acceptance: an exact caller and definition census finds no old surface; every writer has caller-owned state and closes once; every focused child check and required master gate passes on the exact combined tree before one master fast-forward. Forbidden: adapter, alias, dual final surface, separate child publication, lint creation, generated artifact, version, migration reader, metric, or unrelated cleanup. Smallest owning check: the combined focused child checks on the final integration tree. Claim: unassigned.
