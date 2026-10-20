---
title: Index native map source lookups
status: active
priority: 1
issue-type: task
created-at: "2026-10-15T20:08:24.211230+03:00"
---

Cold native capture profiled on the new definition-cache source spends 25.2% inclusive samples in ENGINE-ORIGIN:MAP-FIND, repeatedly walking the accepted-origin journal and resolving each symbol for every output dictionary row. The run used 309.97s user CPU and 992.64s wall under shared host load. Replace the repeated capture-time journal scan with direct record-indexed lookup at the same map boundary, preserving exact record identity, newest accepted row, source tombstones and EXPORT aliases. Measure that phase and retain existing map/source-origin E2E behavior; this is a measured build-speed issue, not a new public origin API.
