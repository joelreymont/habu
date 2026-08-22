---
title: TFAM-FIELD-PROJ masks every throw
status: open
priority: 1
issue-type: task
created-at: "2026-08-22T22:38:25.826564+02:00"
---

Problem: src/core/type-family.f:3607-3610 '[: TFAM-FIELD-PROJ-DO ;] catch drop' names E-PF-ID in its comment but swallows every code (E-TFAM-PAYLOAD, any E-PF-*, 76-class invariants); the only observable is FIELD-PROJ-REJECT (checker.f:11589) with the generic E-REJECTED. docs/forth.md:932-934 forbids the shape; it is the only catch-drop in src/core. Acceptance: catch into a local, rethrow anything but 0 and E-PF-ID; a test provoking a non-E-PF-ID throw shows it propagate. Files: src/core/type-family.f. Verify: the owning type-family suite plus the new case. Depends: none. Ownership: type families. Claim: unassigned.
