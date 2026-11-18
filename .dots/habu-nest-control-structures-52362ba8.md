---
title: Nest control structures past 32 in a body
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T13:49:38.587588+01:00"
---

Problem: src/core/checker.f:20272 CF-FRAMES-MAX (32) sizes the CFS frame table and the CF-LOOPS mask (one bit per slot), so a body whose 33rd control structure opens inside the others is rejected with MD-CF-DEPTH (render.f:1446). A limit Habu sets on itself. Acceptance: a body nesting control structures past 32 deep checks and runs; a test pins it and fails first. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-cfdeep
