---
title: Reject an engine-provided input with 70 in every mode
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T05:03:10.985915+01:00"
---

Problem: check.f exits 64 for E-ENGINE-PROVIDED and --verify-only prints a plain line instead of the record. Acceptance: one record, rc 70, in every mode. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-ownfix.
