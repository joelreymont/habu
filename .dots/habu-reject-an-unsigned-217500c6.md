---
title: Reject an unsigned definer in the check
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T19:34:25.805904+01:00"
---

Problem: for ': DF create , does> ( -- n ) @ ;' then '5 DF W1' the load rejects W1 with E-DOES-UNCREATED while check.f --verify-only passes. Acceptance: load and check give one answer at one place; a test fails without it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-ck-def.
