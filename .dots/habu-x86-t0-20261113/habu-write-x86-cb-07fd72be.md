---
title: Write x86 callback records
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:57.044209+01:00"
---

Problem: x86 CB-DIE, and CB-ENTER, print old E-CALLBACK lines where arm64 CBT-DIE, writes records with arm64's messages (ffi-callback assert 54); engine-error.f:16 describes the old text. Acceptance: ffi-callback passes on x86. Depends: Write the x86 fault-record emitters. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
