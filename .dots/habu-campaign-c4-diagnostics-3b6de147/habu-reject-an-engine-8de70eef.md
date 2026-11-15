---
title: Reject an engine-provided input with 70 in every mode
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.985915+01:00"
closed-at: "2026-11-16T04:07:34.616726+01:00"
---

Problem: check.f exits 64 for E-ENGINE-PROVIDED and --verify-only prints a plain line instead of the record. Acceptance: one record, rc 70, in every mode. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
