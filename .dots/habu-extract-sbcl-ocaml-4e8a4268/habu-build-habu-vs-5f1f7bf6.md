---
title: Build Habu vs SBCL/OCaml gap matrix
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-20T08:55:19.431973+01:00\""
closed-at: "2026-02-20T09:14:12.141832+01:00"
close-reason: completed
blocks:
  - habu-study-ocaml-gc-d799848f
---

File: docs/gc-architecture-comparison.md:120; cause: no explicit capability gap map; fix: matrix for nursery, promotion, major pacing, barrier, LOS, compaction, telemetry; why: plan must map each gap to code changes.
