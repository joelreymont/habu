---
title: Let check.f verify tools that include a layout at load
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:42:57.193643+03:00"
---

Problem: tools/check.f rejects tools/engine-size.f and tools/imgdump.f with `source preverify failed before run`, throw 78, because both load the target layout with `included` at load time (private-words lane, 2026-09-16). Two of the trees own instruments therefore cannot be checked by the tool that checks Habu programs. Acceptance: either check.f follows `included` of a repository file the way it follows require (with the same discovery rules), or the two tools express the layout load as a require, and check.f accepts both tools with rc 0; a regression pins a minimal tool that includes a file at load; no unchecked seam added. Files: tools/check.f, tools/imgdump.f, tools/engine-size.f, test/. Verify: bin/hb --load tools/check.f -- tools/imgdump.f tools/engine-size.f; test/run.f. Depends: none. Ownership: tools/check.f. Claim: unassigned.
