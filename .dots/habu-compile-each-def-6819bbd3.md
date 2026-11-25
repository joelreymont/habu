---
title: Compile each definition once in a build
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T23:10:29.017842+01:00"
---

Problem: a census of tier 0's calls (/tmp/jerry-p1/ktc: an instrumented engine m0 printing a KTC record per call site, logs r/d, parse4.py) found 1,041 definitions of the arm64 engine build whose record lists are two equal halves, so tier 0 emitted each twice; the cause is unknown, and every build may pay for the second emission. Acceptance: each definition is emitted once unless a second emission is needed, the reason for any that stays said in the code, and the engine build's time measured before and after. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-tick.
