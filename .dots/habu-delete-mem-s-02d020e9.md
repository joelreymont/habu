---
title: "Delete MEM's test-only CELLS>BYTES and UNMAP"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T11:44:45.793931+01:00"
---

Problem: MEM:CELLS>BYTES (src/lib/memory.f:115) and MEM:UNMAP (:141) are called only by tests in habu src, tools, test and Zen, Tender, Maki. Acceptance: both words and the cases that test only them are deleted; rg finds no caller. Depends: habu-prove-role-constants-d1dfa45f (same file). Claim: unassigned.
