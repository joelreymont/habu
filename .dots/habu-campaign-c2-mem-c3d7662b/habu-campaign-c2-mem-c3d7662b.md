---
title: "Campaign C2: memory safety the checker can see"
status: open
priority: 1
issue-type: task
created-at: "2026-09-16T13:54:06.629638+03:00"
---

Problem: the checker does not establish lifetime or access authority for raw
borrowed pointers. Tender's shared OPC bytes, independent XML cursors and retained
DOCX tree slices require distinct lifetimes and typed borrowed aggregates.

Contract and delivery: docs/ownership-model.md. Settle the Oracle-reviewed design,
then view/binder/dependency effects, typed storage and loans, authoritative
task-local owner cleanup, MEM/XML, and the isolated OPC/DOCX/XLSX proof. Raw
pointers remain lifetime-free. Linear phases follow a concrete consumer; general
linear locals, all record migrations and region parameters on every pointer are
not gates. master and ~/.local/bin/hb remain the supported Tender release path.

Acceptance: real checked programs reject scope escape, conflicting mutable loans,
release with live loans and authority laundering through raw/stale/deferred/task
state. Shared source bytes, independent readers, tree slices after cursor close,
and owned documents after package close remain legal. Cleanup is exactly once
through return, throw and cooperative halt. Abstract effects survive image
roundtrips; images containing live C2 authority are refused. The design's focused
and integration verification policy applies; no Maki or early Tender adoption gate.

Design record: habu-write-the-checked-035516db. Existing 6218899c is a possible
runtime integration point after reconciliation with the current stale-value model.
The old first-throw-only diagnosis in 56884608 and stack-cell restoration recipe in
9812a28c are not C2 prerequisites: THROW-EDGE now intersects every edge and RSCATCH
marks uncertain cells stale. Reproduce any remaining failure before scheduling a
fix. Pointer/record debts enter only if the actual view path reaches them.

Files: src/core/checker.f, scoped memory and XML libraries, lib/task.f, capture
paths, and the ownership/type/roadmap docs. Implementation remains open.
Verify: design acceptance cases through the real load path; focused development
checks, native suite for shared semantics, and affected image/convergence checks.
Depends: reviewed design. Ownership: checker lane. Claim: unassigned.
Absorbed: the 86 archive entries closed with 'superseded by
habu-campaign-c2-mem-c3d7662b' retain their historical evidence; they are not a
mandatory implementation queue.
