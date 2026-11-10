---
title: The hook line prints with checking switched off
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:33:21.460898+02:00"
---

Problem: /Users/joel/Work/reviews/habu-10x-2026-11-10/refusals/unck/unj-recurse.f switches the check off (`check@ 0 set-check`), defines `: DOWN dup 0 > IF 1 - RECURSE THEN ;` and restores it; the load still prints `hook: non-certified definition`. Cause not found. Found by the uncheckable-reason review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a root-cause hunt says why the line prints and whether it should; then either nothing prints for a definition made with the check off, or the line says what it means. Files: src/core/check-hook.f, src/core/checker.f. Verify: the case on a private engine. Depends: none. Ownership: the check hook. Claim: agent=bob workspace=.jj-ws.noindex/habu-the-hook-line-367fa9ae (hunt)
