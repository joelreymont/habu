---
title: Say target, not product, for what a build makes
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T08:00:42.317126+02:00"
---

Problem: Joel 2026-11-10: 'call it target, not product. it's the target of cross-compilation, even on the same platform'. The tree calls what a build makes 'the product' in CLAUDE.md (2 places, 'Machinery being replaced'), tools/hb-build-core.f (4 comments) and comments in src/habu/linker.f, link-data.f and other build files (114 files contain the word, most of them for the PRODUCT declaration, RTARGET:PRODUCT or compiler-product, which are other things and stay). Acceptance: prose and comments that mean 'what a build writes' or 'the build's side' say target; no word, package or type is renamed; Files: CLAUDE.md, tools/hb-build*.f, src/habu/linker.f, src/habu/link-data.f, docs/. Verify: a search for 'product' in those files leaves only the declaration and RTARGET senses. Depends: none. Ownership: carl. Claim: unassigned.
