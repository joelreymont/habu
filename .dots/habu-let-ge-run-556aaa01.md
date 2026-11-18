---
title: Let GE-RUN-ENV and GE-RUN-STDIN take ms
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T12:45:20.173528+01:00"
---

Problem: test/gate-common-lib.f GE-RUN-ENV (:103) and GE-RUN-STDIN (:112) take `timeout:n` and convert it with >MS, so a caller holding an ms converts it back first (`PROCESS:WAIT-FOREVER MS>N`). Acceptance: both take `ms`; every caller passes an ms value and no caller writes MS>N for them; the suites of their callers pass. Depends: none. Claim: unassigned (callers in test/x86-64-*.f are carl's).
