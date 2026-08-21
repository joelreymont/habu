---
title: Add compiler capability record
status: closed
priority: 1
issue-type: task
created-at: "2026-07-26T22:53:56.925238+02:00"
closed-at: "2026-08-15T14:07:47.978198+02:00"
close-reason: "Closed SUPERSEDED (vintage audit 2026-08-15, re-executed): capability record - the disabled-by-default shadow gate is the INVERSE of the hard-cut program; named dots + measured codes + the census do the job (af513a3c precedent); floats free, zero citations."
---

Full context: design Wave 0 and shadow mode require an explicit disabled new-compiler capability plus named unsupported coverage. Add the immutable capability record and production-readable disabled state without routing compilation through the new backend. Acceptance: default is disabled; unknown capability cannot silently enable; capability identity/version is deterministic; production artifact bytes are unchanged.
