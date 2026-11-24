---
title: Stop on a damaged kernel inputs file
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T17:35:45.474756+01:00"
blocks:
  - habu-stop-loudly-on-6c096ffa
---

Problem: tools/hb-build-core.f INPUTS-READ and INPUTS-READ? (:436-456) answer false for an inputs file shorter than its head, over INPUTS-CAP, or one that cannot be opened, read or mapped, so the kernel rebuilds in silence. Joel: "if one is damaged then this is something to crash loudly on so that the user can take action". Acceptance: a missing file rebuilds; any other failure stops with the damaged-object record naming the file, which carries a checksum as objects do (dot 6c096ffa); INPUTS-CAP goes. Depends: 6c096ffa. Claim: unassigned.
