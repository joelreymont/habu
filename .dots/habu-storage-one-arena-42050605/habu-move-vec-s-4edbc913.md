---
title: "Move VEC's callers onto the typed vector"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:44:44.975793+02:00"
---

Problem: VEC (src/lib/vector.f) is a growable cell vector that maps its own storage; settled (parent, point 5): it becomes the one typed vector on any allocator. Callers: zen 10 VEC:INIT lines, tender 6, habu's own. Acceptance: every caller makes its vector from an allocator and states its element type; no second vector package. Files: src/lib/vector.f, callers. Verify: suite; consumers cold and warm. Depends: the allocator child and the system-heap dot habu-heap-maps-a-657c7988 (a growable vector needs free). Ownership: carl. Claim: unassigned.
