---
title: Bound engine stacks and certify peak use
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T17:19:44.293694+02:00\""
---

Static invariant: every data-, user-return-, and loop-stack access lies within a named target extent, and every checked callable either carries a finite relative peak-use certificate or is rejected as unbounded. Problem: raw return/loop operations lack complete bounds guards, balanced recursion can retain live stack state indefinitely, and run-in-stack ignores its supplied size, so ordinary input/output stack effects do not prevent capacity clobbering. Fix: coordinate three disjoint leaves: runtime extent guards from DATA-LAYOUT, checker inference and persistence of relative peak certificates, then typed run-in-stack capacity enforcement. Acceptance: all children land; zero-depth pop, capacity+1 push, wide transfer, loop-frame overflow, positive-growth recursion, and one-cell-short target capacity reject; exact finite capacities pass; native/bootstrap metadata and behavior remain identical. This coordinator owns only dependency/order and closes after its three children.
