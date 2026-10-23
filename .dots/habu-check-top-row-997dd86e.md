---
title: Check top-row-warn message on x86-64
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T17:33:29.565700+03:00"
---

Problem: top-row-warn's tier-1 p1 check expects 'hb: interpret stack underdepth: execute' on every target, but xt-effect documents x86-64 reporting 'hb: top-row: execute: xt target underflows the interpret stack', so it may fail on x86-64 (g5 test audit lane; not run on x64).
Acceptance: the suite asserts the code each target actually reports and passes on arm64 and x86-64.
Verification: run top-row-warn on both hosts.
