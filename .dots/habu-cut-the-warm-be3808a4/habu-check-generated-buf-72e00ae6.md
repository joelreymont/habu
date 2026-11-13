---
title: Read generated buffers without a bounds check
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:12:00.888957+02:00"
---

Problem: each DYNAMIC-BUFFER accessor (src/core/layout-buffer.f DBUF-SOURCE) calls DYNAMIC-STORAGE:ADDRESS, which checks the index at run time: 30.9M calls, 10.1% of a warm engine build. Joel's rule (AGENTS.md Changes) puts bounds checks only in code instrumented on demand. Acceptance: a default accessor fetches the head and adds index times width, with no check; the stored tag and bound that serve only the check go; the checked read returns only through habu-add-the-checks-a49d054d. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-oe-acc
