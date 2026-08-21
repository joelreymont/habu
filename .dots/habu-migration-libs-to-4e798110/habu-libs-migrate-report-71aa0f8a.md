---
title: "Libraries: migrate report column"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-24T13:02:19.324997+02:00"
closed-at: "2026-07-24T17:43:03.122488+02:00"
close-reason: Reviewed implementation landed and verified at master@origin 4fb6f52fb815.
---

Why: lib/report.f still declares REPORT:col with legacy PRODUCT even though the unified STRUCTURE production path and generated MAKE/UNMAKE operations are live on master. Owner: lib/report.f and lib/report-test.f only. Replace PRODUCT col 0 ... ;PRODUCT with STRUCTURE col 0 ... ;STRUCTURE inside package REPORT, preserving the exact field names, schemas, declaration order, REPORT-COL:MAKE/UNMAKE package spelling, three-cell layout, zero-allocation column hot path, bounds behavior, and public REPORT API. Update comments so they describe STRUCTURE, not PRODUCT. Forbidden: compatibility aliases, legacy parser edits, package renames, field duplication, casts, layout changes, unrelated report refactors. Baseline and acceptance: the real lib/report-test.f owning path is green before and after; add a production-path assertion only if current coverage does not exercise both generated operations; exact type reflection shows the same three fields in order; token-aware executable census finds no legacy declaration in lib/report.f; focused typed-local/package/trust gates pass. Dependency proof: master 227b5b349702 runs test/structure-decl-suite.f green and already publishes STRUCTURE-generated MAKE/UNMAKE.

Claim: agent=claude-report workspace=.jj-ws/habu-libs-migrate-report-71aa0f8a
