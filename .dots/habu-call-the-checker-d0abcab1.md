---
title: "Call the checker's report words, not checker-owner forwarders"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T17:07:14.204127+01:00"
---

Problem: src/compiler/native/checker-owner.f keeps one-call forwarders RESET-REPORT, WRITE-WINDOW, CALL-CELLS, MATCH-PAYLOAD and an EXPORT of DOES-REPORT, called from src/habu/t0.f (6 sites), src/compiler/native/definition.f and publish.f; t0.f already requires src/core/checker.f, so each site can call CHECKER-REPORT-RESET, CHECKER-WRITE-WINDOW!, CWIN-CELLS, CWIN-MATCH-PAYLOAD or DOES-REPORT itself. Acceptance: the forwarders and the export are gone and every former caller calls the checker word; the engine converges. Depends: none. Claim: unassigned (t0.f is jerry's).
