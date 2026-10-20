---
title: "Name a relative path's directory without a slash"
status: closed
priority: 3
issue-type: task
created-at: "2026-10-03T16:39:51.746004+03:00"
closed-at: "2026-10-14T19:38:01.492104+03:00"
close-reason: "Implemented in 11b4109b: SOURCE-ROOT:DIRNAME returns the current directory for a bare relative name and callers dropped duplicate parent helpers; source-root-test passed in this workspace."
---

Found by lane 453 (234934e9, 073ec3fb): src/core/include.f SOURCE-ROOT:DIRNAME (through the private PARENT-U, ~236-240 and ~317-318) answers the first byte for a relative path with no slash: 'stamp' gives 's', so a caller that makes the directory creates a stray one. Every current caller passes an absolute or rooted path, and tools/build-fixpoint.f BF-STAMP-ENSURE-DIR now canonicalises first. Acceptance: DIRNAME answers '.' (or the documented current-directory form) for a bare relative name, the callers that canonicalised only for this drop that step, seen failing first; baked: rebuild, g1 == g2, two-generation build.
