---
title: "Reject an open declaration before CALLBACK: claims"
status: active
priority: 2
issue-type: task
created-at: "2026-11-24T20:40:32.311366+01:00"
---

Problem: OPEN-CALLBACK (src/lib/ffi-callback.f) rejected an open declaration early only when it was a callback; a CALLBACK: with a taken name inside an open structure got E-DUPLICATE-DEFINITION at the name from the load and E-UNCLOSED at BEGIN-STRUCTURE from the check. Acceptance: the load rejects at the open declaration's opener before reading or claiming the name, as the check does; a diag-position case pins it. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/carl-taken.
