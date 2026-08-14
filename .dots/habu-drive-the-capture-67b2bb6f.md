---
title: Drive the capture capacities to their refusals
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T14:31:18.713648+02:00"
---

Found by the widening riders lane: none of the nine capture capacity refusals (too many call sites / records / blob exceeds / names / DATA / sites / dsites / xtoff / xtsite) has a fixture on either side - the derived-bounds discipline is unmet, and the reason is structural: a generated driver big enough to reach them dies FIRST in the stage engine's code window (see the restored icode dot). The vehicle is direct drive of the capture buffers, the way XT-DOCTOR already drives ACAP-ADD-XTSITE. Files: test/aot-wid-build.f or sibling, src/habu/aot-capture.f (test seams if needed). Depends: none (direct drive dodges the icode cap).
