---
title: Reject an FFI group open across an include in the verifier
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T09:25:16.153230+01:00"
---

Problem: a FUNCTION: group opened in one file and a FUNCTION: or BEGIN-STRUCTURE in a file it includes before ;FUNCTION: the load rejects E-UNCLOSED at the outer opener; the verifier passes (src/habu/verify-source.f FFI-OPEN-HERE? answers false for an opener outside the scanned source, and the nested scan holds only its own bytes). Acceptance: the three readers print one and the same record, rc 70. Depends: none. Claim: unassigned.
