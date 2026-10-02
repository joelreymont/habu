---
title: "Name a relative path's directory without a slash"
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T16:39:51.746004+03:00"
---

Found by lane 453 (234934e9, 073ec3fb): src/core/include.f SOURCE-ROOT:DIRNAME (through the private PARENT-U, ~236-240 and ~317-318) answers the first byte for a relative path with no slash: 'stamp' gives 's', so a caller that makes the directory creates a stray one. Every current caller passes an absolute or rooted path, and tools/build-fixpoint.f BF-STAMP-ENSURE-DIR now canonicalises first. Acceptance: DIRNAME answers '.' (or the documented current-directory form) for a bare relative name, the callers that canonicalised only for this drop that step, seen failing first; baked: rebuild, g1 == g2, two-generation build.
