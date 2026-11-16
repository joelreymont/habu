---
title: "Keep a program's own warnings under check"
status: active
priority: 2
issue-type: task
created-at: "2026-11-16T14:50:57.649274+01:00"
---

Problem: tools/check-core.f 842 writes '0 CHECK:WARN-DIAGS !' into the run, so every W-EFFECT-NOT-RECORDED the program itself captures is lost under check: 'bin/hb --load tools/check.f -- test/engine-suite.f' fails F216/F217 while 'bin/hb --load test/engine-suite.f' passes. Acceptance: a program prints and captures the same warnings under check.f as under --load, and check writes each placed warning once. Depends: none. Claim: agent=Plan workspace=.jj-ws.noindex/carl-wd.
