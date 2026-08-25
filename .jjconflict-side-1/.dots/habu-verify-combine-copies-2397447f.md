---
title: Verify combine copies the address kind
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T18:38:00.887576+02:00"
---

combine.f rewrites the module between selection and emission; its MOVZ-VALUE fold excludes multi-lane chains so it cannot dissolve an address carrier, but whether it COPIES the a64.addr attribute onto a rewritten operation is unverified (aotsite 2026-08-11). Verify in the stage-1 build with a fixture: an address chain through combine keeps its kind and its site is recorded; a dropped attribute must fail loudly, not silently unrecord the site. Files: src/compiler/native/combine.f test. Depends: habu-per-site-relocation-bb9b6d70 stage 1 (fold into that lane if trivial).
