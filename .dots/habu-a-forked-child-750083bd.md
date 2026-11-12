---
title: A forked child cannot map the region alias
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T13:42:11.060777+02:00"
---

Problem: test/compiler/arm64-link-run.f passed on the linker-fix stack's engine and its run printed 'hb: fork: cannot map the child's region alias' once: FORK's macOS child (src/habu/habu1.f:2983-3008) failed REGION-REMAP (:2826) or its mprotect and exited 78, under machine load about 55. Which process forked, why the re-map failed and why the test passed are not known. Acceptance: the cause is found and fixed; a forked child of an engine that has an alias always re-maps it, and a caller whose child dies at start reports it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fork.
