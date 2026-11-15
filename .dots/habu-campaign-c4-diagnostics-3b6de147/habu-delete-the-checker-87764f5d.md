---
title: "Delete the checker tools' old record forms"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.421807+01:00"
closed-at: "2026-11-16T04:07:36.392166+01:00"
---

Problem: the C4 review found the pre-C4 forms still in tools: check-all-errors-core.f:397-471 (CA-JSON*, BYTE-ORIGIN, CA-THROW-ORIGIN, CA-PROSE-THROW, CA-THROW-RECORD$, JSON!, THROW-RECORD$), verify-diagnostics.f:10-36, 60-85, gate-json-assert-core.f:206-207 GJA-SCHEMA1, diag-position-test.f:719-724 PREPASS-REFUSED; the 71xx codes E-UNTERMINATED-STRING, E-MALFORMED-REGISTRY-ROW, E-MISSING-SIGNATURE, E-SECTION-WORD (verify-source.f:197-214) with dead readers LEX-FIRST? (check-all-errors-core.f:238-239) and THROW-CODE$ branches; E-STATEMENT-THROW's help is the deleted generic suggestion (verify-diagnostics.f:57-58 via CA-THROW-RECORD, check-all-errors-core.f:209-221). Acceptance: those words, constants and verify-diagnostics.f are gone; E-STATEMENT-THROW's help says how to fix a statement that throws. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
