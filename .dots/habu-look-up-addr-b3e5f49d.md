---
title: Look up address rows through an index on x86
status: active
priority: 1
issue-type: task
created-at: "2026-11-25T16:32:56.703088+01:00"
---

Problem: src/habu/kernel-x64.f's (MARK) scans the address rows linearly where arm64 uses an index (src/habu/habu2.f INDEX-PROBE, INDEX-BUILD): 388 ms of a 7.4 s load. Acceptance: x86 uses the index, the load loses that time, arm64 is unchanged. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-x-mark.
