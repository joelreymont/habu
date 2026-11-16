---
title: State why a fault writes its own record
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T12:25:48.726955+01:00"
---

Problem: crash.f's fault records (FAULT-OPEN, .. FAULT-STOP,) write REJECTION's prose and JSON forms again in machine code, and nothing beside them says why REJECTION cannot write them. The memory fault's missing help moved to habu-print-an-ffi-d1d7964f. Acceptance: crash.f states why its sites cannot use REJECTION. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w222.
