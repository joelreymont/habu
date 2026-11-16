---
title: Fix two stale test expectations
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.681423+01:00"
---

Problem: native-gate-debug assumes a tail call leaves a frame; linux-engine-build expects the pre-record E-TARGET-WORD text and is switched off on every Linux host. Acceptance: both pass on x86, native-gate-debug on arm64, linux-engine-build wherever it can run. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
