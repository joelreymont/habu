---
title: Say what a command line gets wrong
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T12:43:35.692525+01:00"
closed-at: "2026-11-16T13:54:13.928128+01:00"
---

Problem: tools/check-core.f CHK-USAGE (:221-223, ten callers :261-341) prints only the usage line for an unknown flag, a missing value or an out-of-range --deadline-ms (`check.f --deadline-ms 0 x.f`, `check.f --bogus x.f`: usage, rc 64); src/habu/main.f prints `hb: unknown flag: --bogus` and a usage line that omits `hb file.f`. Neither is a record. Acceptance: each rejected command line prints one record naming the argument and what it should be, with the usage as its help, rc 64. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w228.
