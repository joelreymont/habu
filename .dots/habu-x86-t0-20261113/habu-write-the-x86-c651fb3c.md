---
title: Write the x86 fault-record emitters
status: active
priority: 2
issue-type: task
created-at: "2026-11-17T10:44:56.841919+01:00"
---

Problem: x86 needs crash.f's die line and fault-record writers (open, in with an owner lookup, number, text, address, stop, keep; JSON by REJECTION-JSON-CELL) over the shared FAULT-RECORD layer. Acceptance: x86 emitters exist with stated register contracts, used by no site yet. Depends: Share the fault record's text layer. Claim: agent=carl workspace=.jj-ws.noindex/carl-x86 (merged, lands with the feature).
