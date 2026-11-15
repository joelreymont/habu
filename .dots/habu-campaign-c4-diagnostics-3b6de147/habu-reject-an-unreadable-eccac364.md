---
title: "Reject an unreadable dependency in check.f's walk"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T06:35:37.178921+01:00"
closed-at: "2026-11-16T04:07:34.538116+01:00"
---

Problem: check.f's walk (tools/check-verify-core.f CHK-PRESENT) reports a required directory or FIFO as E-MISSING-SOURCE, rc 70, where the loader finds a directory and gives E-UNREADABLE-SOURCE 'is a directory', rc 74. Acceptance: plain check.f, --json-errors and --all-errors give a found non-file dependency the loader's record. Depends: none. Claim: agent=W164 workspace=.jj-ws.noindex/jerry-w164.
