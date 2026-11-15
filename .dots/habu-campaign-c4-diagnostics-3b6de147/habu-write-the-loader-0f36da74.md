---
title: "Write the loader's cwd failure as a record"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:50:22.172408+01:00"
closed-at: "2026-11-16T04:07:33.628178+01:00"
---

Problem: src/core/include.f CWD-INIT (~:225) dies through CWD-DIE with a plain line when the working directory cannot be read (a long cwd reaches it). Acceptance: one record through REJECTION, prose or JSON as asked, with the status the Design allows. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
