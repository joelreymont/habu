---
title: Test the emulated breakpoint entry
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T23:28:28.162227+01:00"
---

Problem: a persistent breakpoint on a word whose entry is the link save (src/habu/habu2.f C-BP-EMULATE's framed arm) had no test, and test/engine-stack-lifecycle-lib.f CHILD-RC throws E-PROC-TRUNCATED out of the whole test when a child's crash dump exceeds its 512-byte capture, so a crashing case is not named. Acceptance: test/engine-stack-debugger.f checks a persistent breakpoint on a calling word, and with the framed arm broken it fails by that case's label while KEEP's and TWICE's pass. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-bp.
