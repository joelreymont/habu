---
title: "Find the main executable's header by its own name"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T04:56:57.636681+02:00"
closed-at: "2026-11-13T04:59:33.771047+02:00"
---

Problem: src/lib/engine-id.f:99 VERIFY-MACOS takes dyld image 0 as hb's own header; with a library inserted by DYLD_INSERT_LIBRARIES (a profiler, libgmalloc) image 0 is that library, so the vnodes differ and KEY$ throws E-ENGINE-KEY: every warm build and object check fails. Acceptance: KEY$ answers the same key with and without an inserted library; a warm engine build under an inserted library succeeds. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-eid.
