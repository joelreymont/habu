---
title: "Install hb-build's engine through RESERVE-SIBLING"
status: open
priority: 3
issue-type: task
created-at: "2026-10-01T18:20:45.442925+02:00"
---

Problem: tools/hb-build-lib.f:557-566 HBB-INSTALL-TMP! mints its own non-exclusive '.pid-ns.tmp' sibling name, a second sibling-naming implementation beside lib/fs-mutate.f RESERVE-SIBLING (exclusive create, one seed), which r4-snapatomic (dot 2270f44d) made the image writer's and ATOMIC-WRITE-FILE's one implementation. Found by review 212. Acceptance: hb-build's install stages its engine through RESERVE-SIBLING (or ATOMIC-WRITE-FILE) and renames it into place; the private name minting is gone; a failed install leaves the installed engine as it was and no sibling (case through the hb-build test that owns install, failing first only if behaviour changes, else the existing install cases stay green); hb-build's EXISTS? guards that the staging made dead go too. Base: after r4-snapatomic (dot 2270f44d). Files: tools/hb-build-lib.f and its test.
