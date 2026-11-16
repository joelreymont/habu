---
title: Write x86 task-guard records
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.989107+01:00"
---

Problem: x86 MUTATION-GUARD, prints 'E-TASK-COMPILE habu: … lr <hex>' where arm64 GUARD-RECORD, writes a record (tasking-threads 49 52 53 59 60 678, compiler-native-eval). Acceptance: those pass on x86. Depends: Write the x86 fault-record emitters. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
