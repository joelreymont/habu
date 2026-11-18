---
title: habu1.f says BNDSET calls LREBUILD
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T18:30:41.041076+01:00"
---

Problem: src/habu/habu1.f:132-133 and :4492-4493 say BNDSET calls HIDX-EMIT:LREBUILD; its only calls are in LHIDXADD (:4457) and LHIDXBUILD (:4484). Acceptance: both comments name the callers the code has. Depends: none. Claim: unassigned.
