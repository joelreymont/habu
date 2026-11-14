---
title: Report checker record rejections at their site
status: open
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.354946+01:00"
---

Problem: the 19 record renderers in src/core/render.f are reached through load-order hooks (BADSIG-XT, RECORD-DIAG-XT, SHADOW-DIAG-XT, STORAGE-DIAG-XT) and src/core/checker.f CHECKER-REFUSE; package, using and export rejections end bare (package <300 q> 7154, EXPORT dup 7115, an ambiguous bare word 7144). Acceptance: each prints one diagnostic through habu-print-a-refusal-9bae6f53 at the site that knows its cause, with a specific help; the hooks and CHECKER-REFUSE are gone; the export reasons and package-name text of the halted stack (commits vmkmwtot ae850e5f and tovwpsrx: the four export reasons and fixes, the export words moved to src/core/check-walk.f, the package-name text) are carried over. Depends: habu-print-a-refusal-9bae6f53. Claim: unassigned.
