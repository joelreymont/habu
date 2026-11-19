---
title: "Let XML's input reader fill its own chunk"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:23:02.620604+01:00"
---

Problem: XML:INIT-INPUT (src/lib/xml/state.f:686) makes its caller carve the chunk span the fill callback writes (zen src/document/xlsx.f:816 SPAN:MAKE on pool bytes), while the reader already keeps two VEC:vector<u8> of its own (state.f:233-237). Acceptance: no caller carves storage for XML:INIT-INPUT; the reader lends the fill a span over its own vector. Depends: none. Claim: unassigned.
