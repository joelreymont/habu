---
title: Hold the current section once
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T13:49:36.919310+01:00"
---

Problem: the current section lives in the CUR-CELL header cell and in the checker's mirror (src/core/checker.f ~14016-14060), re-synced by MIRROR (~13079-13089); two copies of one state. Acceptance: one holder, the other derived where read; --load and --verify-only unchanged on test/using-test.f and the section probes. Depends: habu-verifier-rejects-export-e3912170. Claim: unassigned.
