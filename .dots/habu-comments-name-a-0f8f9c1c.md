---
title: Comments name a Gforth recovery compiler the tree no longer has
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T15:47:26.460985+02:00"
---

Problem: src/arch/arm64/icode.f:7-8 (No locals: the Gforth recovery compiler checks this file before the native checker exists) and src/core/util.f:50 (the Gforth fixture's NF-PATH-CAP) and docs/xcomp.md:256 (The Gforth chain emits ARM64 only) name a Gforth chain; no Gforth file is in the tree and docs/bootstrap.md Recovery rebuilds from a trusted native hb. Found by the n rule's load-position prototype (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/nwords-prototype.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each comment states the constraint that still holds and its real reason, or goes with the constraint (icode.f may then use locals). Files: src/arch/arm64/icode.f, src/core/util.f, docs/xcomp.md. Verify: rg -n -i gforth src docs tools test is empty or each hit is true. Depends: none. Ownership: those comments. Claim: agent=bob workspace=.jj-ws.noindex/habu-comments-name-a-0f8f9c1c

All mentions on master feab8fff: src/arch/arm64/icode.f:7, src/core/util.f:50, src/core/type-family.f:1181, src/habu/primitive-registry.f:20, src/habu/rt.f:17, src/habu/arith-abi.f:5, src/habu/layout.f:72, src/habu/stack-abi.f:43, src/habu/habu2.f:2753 (not edited: machinery being replaced), docs/xcomp.md:256, test/engine-suite.f:2.
