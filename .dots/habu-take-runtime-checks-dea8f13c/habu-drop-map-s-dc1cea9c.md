---
title: "Drop MAP's own checks"
status: active
priority: 2
issue-type: task
created-at: "2026-11-23T09:30:30.772467+01:00"
---

Problem: src/lib/map-core.f checks its own invariants and its callers' arguments on every call (CHECK-CAP per probe step, CHECK-HANDLE, CHECK-LEN, CHECK-INDEX, SLOT-FIELD offsets), each catching a program error or an unreachable state. Acceptance: MAP runs no check but SET's E-MAP-FULL; E-MAP-BAD-CAP is gone; map-stdlib passes. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-p4mapc
