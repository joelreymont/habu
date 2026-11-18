---
title: Wait on test children without a clock
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T12:38:59.440297+01:00"
---

Problem: test/split-str-deep.f passes a 10 s wall deadline to SUBJECT:RUN; on a loaded machine (load 34 on 20 cores) a child ran past it and the full suite failed split-str (E-PROC-TIMEOUT, exit 67) though it passes alone; about 14 test files guard children with such deadlines, against test/AGENTS.md's rule that a hang is named by its WAIT line, never ended by a clock. Acceptance: no test passes a wall deadline it does not assert; the touched suites pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-wf.
