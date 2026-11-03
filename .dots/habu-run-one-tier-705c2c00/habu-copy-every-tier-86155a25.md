---
title: Copy every tier-1 package T0 depends on into T0
status: active
priority: 2
issue-type: task
created-at: "2026-11-03T13:55:30.280713+02:00"
---

Joel 2026-11-03, a direction not yet settled as a design (LESSONS "editing tier 1" holds the settled part): review each piece of tier-1 code T0 depends on and COPY it into T0 under a proper package and name; copy, review, adjust; do not rewrite. T0 calls 16 N* packages from src/compiler/native (rg over src/habu/t0*.f at a8885c0f: NEMIT 31, NDICT 19, NHOST 11, NDEFINITION 10, NSTR 8, NPUB 8, NCBSTUB 8, NSHADOW 7, NFETCH 5, NFETCH-CHECK 4, NCOMP-DISPATCH 4, NTRAP 2, NFAM 2, NELAB 2, NCOMP 1, NABI 1) plus the requires in t0.f :16-30, t0-emit.f :10-12, t0-arm64.f :17-19, t0-x64.f :15-17. First step: a census of every word T0 reaches in each, with its transitive tier-1 dependencies and its other readers (capture, aot-shadow, link, tools), then a design naming each new package (names go to Joel), then copy lanes. The callback group is replaced, not copied (habu-make-a-cb-6de47af1). Supersedes the keep list in habu-delete-tier-1-f5c44fb4. Claim: agent=jerry, census running read-only (brief /tmp/jerry-big/t0census-brief.md, report /tmp/jerry-big/t0-census.md).
