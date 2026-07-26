---
title: Rename owned release to MEM RELEASE
status: open
priority: 2
issue-type: task
created-at: "2026-07-26T22:56:29.209632+02:00"
---

Mechanical, behavior-identical, ready leaf split from the fatal flip so it runs in parallel with the injector. RELEASE-BYTES becomes MEM:RELEASE (at the OS ownership boundary memory is a byte mapping; the -BYTES suffix repeated what the CAD-NUM:alloc-byte-len role states; the ALLOC-* family keeps its suffixes - many typed entries, one view-independent exit whose typed extent role is what prevents releasing an arbitrary scalar). Hard-cut every caller and comment, about 55 files, no alias, no forwarder; SAFET:RELEASE is a different package, untouched. Includes the whole-range provenance AUDIT with no behavior change: every call site releases exactly one whole mint-time allocation; BLK-FREE recomputed length proven identical to minted length or the owner grows a carried length; any nonconforming site reported, not silently changed. Acceptance: behavior-identical proof (focused memory and weight-store suites green, byte-identical engine), boundary-aware sweep shows zero old-name references, audit table in the report, both diff lints clean.
