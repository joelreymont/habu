---
title: Write x86 stack-bound records
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.895339+01:00"
---

Problem: x86 GUARDS, prints 'hb: stack bounds exceeded (X)' where master prints an E-STACK-BOUNDS record; stack-guard and engine-stack-lifecycle hold stale x86 branches. Acceptance: engine-stack-wide, stack-guard, engine-stack-lifecycle and compiler-native-eval's stack case pass on x86. Depends: Write the x86 fault-record emitters. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
