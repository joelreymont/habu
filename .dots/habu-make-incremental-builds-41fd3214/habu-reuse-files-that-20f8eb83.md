---
title: Reuse files that define does> definers
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:08.056655+02:00"
closed-at: "2026-10-31T20:52:54.487482+02:00"
close-reason: "Landed in lane 11 4b54ed21: files that define does> definers reuse."
---

Problem: warm-diag.md R1: 7 files (411 defs) with a does> definer store no object: rowless-record checker-export (CHECK-EXPORT throws; likely CHECKER-PENDING-EXPORT-REFS refusing a definer's second record). Acceptance: the seven files reused, their definers installed, warm == cold. Files: tools/native-unit-object.f, src/core/checker.f, src/compiler/native/compiler.f. Verify: P0 on the lane, the lane's does> reproduction. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-fix-r1.md. Depends: none. Ownership: CHECK-EXPORT, ROW-KEEP, DOES-SHAPE, CHECKER-PENDING-EXPORT-REFS, DEFINITION-CACHEABLE?. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-fr1

Ruling (carl, 2026-10-30): the checker takes the definition record at its mark (UNIT-NDICT) and drops its one-record count check; the object side (DEFINITION-END) owns the count. brief-fix-r1.md Revision 1.
