---
title: Capture hb-build maker output without truncation
status: active
priority: 2
issue-type: task
created-at: "2026-10-23T20:10:10.359901+03:00"
---

With HABU_NATIVE_CACHE_DIAG=1 a plain two-file hb-build dies 'uncaught throw code -2504' (E-PROC-TRUNCATED): hb-build's capture of the maker child's output overflows on diagnostic lines. Found by the prep census (carl-prep, 2026-10-23). Diagnostics must never break a build.

Lead fix in carl-diag (urspuwsy on master): HBB-RUN-MAKER-CMD / HBB-RUN-APP-CMD use the growing capture. Worker adding the fail-first hb-build case (brief-diag.md).
