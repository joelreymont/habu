---
title: Give x86-64-kernel-pure a PTR>N arm
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T12:39:18.220152+02:00"
---

Problem: test/x86-64-kernel-pure.f MEM-SCENARIO (:113-127) has no PTR>N arm, but it includes test/prim-cases.f, which now has a PTR>N case set, so it dies 'x64k-pure: unwired memory row' once it runs; it is switched off today (test/gate-stdlib-cases.f:712-814). Acceptance: the test passes with an arm that answers the address as a number. Depends: x86-64's turn. Claim: unassigned.
