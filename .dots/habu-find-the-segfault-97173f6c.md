---
title: Find the segfault after a rejected top-level line
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T23:07:15.341352+01:00"
---

Problem: /tmp/carl-pkg2b/evc10.f (TEST-EVAL:RC of a package text ending in throw, then '.') prints a checker rejection naming a stale cell and dies SIGSEGV rc 139 instead of exiting 70, on master's installed hb. Acceptance: the rejection exits 70 with no fault, fixed at its cause. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/carl-evalfix (fix).
