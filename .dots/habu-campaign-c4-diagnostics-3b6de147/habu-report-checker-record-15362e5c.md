---
title: Report checker record rejections at their site
status: active
priority: 2
issue-type: task
created-at: "2026-11-14T16:00:42.354946+01:00"
---

Problem: the 19 record renderers in src/core/render.f are reached through load-order hooks (BADSIG-XT, RECORD-DIAG-XT, SHADOW-DIAG-XT, STORAGE-DIAG-XT) and src/core/checker.f CHECKER-REFUSE; package, using and export rejections end bare (package <300 q> 7154, EXPORT dup 7115, an ambiguous bare word 7144), and an engine build reports such a throw only as `hb-build: src/habu/link-data.f threw 7113`, naming the top-level file it required, not the file that threw (tools/hb-build-lib.f HBB-LINK-FAILED). Acceptance: each prints one diagnostic through REJECTION (src/core/rejection.f) at the site that knows its cause, with a specific help; the hooks and CHECKER-REFUSE are gone; the export reasons and package-name text of the halted stack (commits vmkmwtot ae850e5f and tovwpsrx: the four export reasons and fixes, the export words moved to src/core/check-walk.f, the package-name text) are carried over. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4c1, jerry-c4c2, jerry-c4c3, jerry-c4cs; integration .jj-ws.noindex/jerry-c4.
