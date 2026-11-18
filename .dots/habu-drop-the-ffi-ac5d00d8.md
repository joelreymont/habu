---
title: "Drop the FFI callback test's deadlines"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:19:56.936477+01:00"
---

Problem: src/lib/ffi-callback-test.f CHILD-MS (60 s, :27, :477, :536) and BUILD-MS (600 s, :28, :529) end its children by a wall clock, against test/AGENTS.md 'a hang is named by its WAIT line, never ended by a clock'; whether its own runner (RUN-ARGV-ENV-CAPTURE-OUTCOME) prints a WAIT line is unknown. Acceptance: the children wait forever and a hang is named by a WAIT line; both constants are gone; the ffi-callback suite passes. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-ffiwait.
