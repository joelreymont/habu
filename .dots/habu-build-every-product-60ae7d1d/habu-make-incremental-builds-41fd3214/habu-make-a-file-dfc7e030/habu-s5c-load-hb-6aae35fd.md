---
title: "S5c: load hb's startup files into a build"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T02:12:03.638279+02:00"
blocks:
  - habu-s5b2-1-the-361fa9c1
---

Problem: a build that LINKER:WITH opens sees only its own words and WL:PRIMITIVES (src/habu/linker.f:132-135 MOVE-IN, src/habu/xref.f:330), so product text that uses require or any hb library word dies E-UNDEFINED 70: 20 of the 21 stripped suite apps do (carl's step 5a census, dot habu-census-hb-build-371c0805). Design r2 3.4/3.8 (/tmp/jerry-big/design-xbuild-r2.md): a build first loads hb's startup files, src/habu/boot-files.f's list, into the space. The S5a-2 census (/tmp/jerry-s5a/s5a2-report.md) stopped at util.f:7, a checker duplicate S5b2-1 removes; nothing past it was reached. Acceptance: inside LINKER:WITH the FILL rows of boot-files.f through include.f load with hb's included, native-runtime.f's rows (src/habu/native-runtime.f:18-70) run as product text, and a product require of a library file loads; after the close hb's dictionary and checker are as before any build (test/linker.f HOST-CHECK). Order: a census on S5b2-1's hb of how far the list loads (a worker, no tree edit); then S5b3, what product text sees (design /tmp/jerry-s5b3/design-s5b3.md; its declaration-keyword item and its habu2.f EM-REC-WIDE-PUBLISH change wait for Joel); then S5b2-2 and S5b2-3, the remaining checker stores (/tmp/jerry-big/design-s5b2.md:119-134); then this load, as one case in test/linker.f. Files: src/habu/linker.f, src/habu/boot-files.f, test/linker.f; others per the census. Verify: bin/hb --load test/linker.f on an hb rebuilt from the tree, with the case that loads the list. Depends: S5b2-1 (habu-s5b2-1-the-361fa9c1). Blocks: step 5a's driver (habu-build-stripped-apps-805dee2b, carl). Ownership: jerry. Claim: unassigned.
