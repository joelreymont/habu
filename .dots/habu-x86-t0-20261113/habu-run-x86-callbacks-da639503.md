---
title: Run x86 callbacks on caller slots
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.750558+01:00"
---

Problem: x86 CB-THUNK, has no caller-row branch, so pg's notice callback dies 106 on x86. Acceptance: pg passes on x86; the x86 gates that hid it are gone. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
