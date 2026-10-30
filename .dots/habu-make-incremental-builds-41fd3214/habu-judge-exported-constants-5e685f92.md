---
title: Judge exported constants and private primitives
status: active
priority: 2
issue-type: task
created-at: "2026-10-30T18:12:08.087059+02:00"
---

Problem: warm-diag.md H: checker-surface.f KEEP? (EXPORT of constant IMAGE-WHITEBOX: FIXED-RAW? does not resolve an EXPORT) and publish.f DOES-RECORD (package-private primitive does-record: PRIMITIVE-STATE answers only global records) compile live every warm build. Acceptance: both install; editing IMAGE-WHITEBOX makes KEEP? compile live. Files: tools/native-unit-dependency.f, src/core/checker.f. Verify: P0 on the lane plus the edit case. Brief: .jj-ws.noindex/carl-dots/.out/carl-file/brief-fix-h.md. Depends: none. Ownership: FIXED-RAW?, PRIMITIVE-STATE. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-frh

Ruling (carl, 2026-10-30): measured causes differ from the diag. The export record of a constant is published with fixed kind 0 (RECORD-PUBLISHED-LOCAL lacks RECORD-LINK-FIXED!); the private wrapper DOES-RECORD shadows the primitive does-record (case-insensitive symbol). Fix: set the fixed kind on publication; delete the wrapper. brief-fix-h.md Revision 1.
