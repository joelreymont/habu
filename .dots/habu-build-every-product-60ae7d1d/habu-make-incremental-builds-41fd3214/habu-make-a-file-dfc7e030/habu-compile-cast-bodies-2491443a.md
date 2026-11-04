---
title: Compile cast bodies through DEF-FIXED
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T14:14:14.883991+02:00"
---

Step 1 lane kinds-b. definers.f DEF-CAST-ROW and DEF-LINEAR end in the kernel's def-cast, whose shadow is tier 1's SHADOW-FIXED (no unplaced arm64 emission): shadow-kinds measured -6616 for cast: and linear:. They take 0 COMP-BODY:cast DEF-FIXED DEF-CLOSE on every machine; T0's FIXED-BODY gets the identity arm and tier 1's COMPILE-FIXED stops refusing cast (the engine build runs at latch 1; NELAB:FIXED already builds FUN-CAST). Workspace .jj-ws.noindex/jerry-kinds-b on vslnprlw (kinds-a); brief /tmp/jerry-big/kinds-b-brief.md.
