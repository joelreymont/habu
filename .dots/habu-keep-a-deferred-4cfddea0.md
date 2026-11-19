---
title: "Keep a deferred buffer's region through a build"
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:52:55.426455+01:00"
---

Problem: a DEFER-LAYOUT-BUFFER bound at a file's top level loses its region in the built program. The header keeps the region as an offset from itself (src/core/layout-buffer.f LDEFER-BIND, LDEFER-GROW, ELEMENT), and the stripped link keeps only data reached through marked address cells and lays parts out again, so the program reads other data or faults (reproducers /tmp/jerry-eadefer-p top.f, num.f, keep.f). LDEFER-BIND and LDEFER-GROW also mark no element cell, and DEFER-LAYOUT-BUFFER skips the mixed-cell check LAYOUT-BUFFER runs. Acceptance: top.f prints 42 and num.f prints 7 when built; an address or quotation element cell bound at top level survives the build; a mixed sum element is rejected E-BAD-STORAGE. Depends: none. Claim: unassigned.
