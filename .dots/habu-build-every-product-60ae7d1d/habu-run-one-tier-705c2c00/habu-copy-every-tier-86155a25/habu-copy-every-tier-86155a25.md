---
title: Copy every tier-1 package T0 depends on into T0
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T13:55:30.280713+02:00"
---

Joel 2026-11-03, a direction not yet settled as a design (DECISIONS "editing tier 1" holds the settled part): review each piece of tier-1 code T0 depends on and COPY it into T0 under a proper package and name; copy, review, adjust; do not rewrite. T0 calls 16 N* packages from src/compiler/native (rg over src/habu/t0*.f at a8885c0f: NEMIT 31, NDICT 19, NHOST 11, NDEFINITION 10, NSTR 8, NPUB 8, NCBSTUB 8, NSHADOW 7, NFETCH 5, NFETCH-CHECK 4, NCOMP-DISPATCH 4, NTRAP 2, NFAM 2, NELAB 2, NCOMP 1, NABI 1) plus the requires in t0.f :16-30, t0-emit.f :10-12, t0-arm64.f :17-19, t0-x64.f :15-17. First step: a census of every word T0 reaches in each, with its transitive tier-1 dependencies and its other readers (capture, aot-shadow, link, tools), then a design naming each new package (names go to Joel), then copy lanes. The callback group is replaced, not copied (habu-make-a-cb-6de47af1). Supersedes the keep list in habu-delete-tier-1-f5c44fb4. Claim: unassigned. Census done by jerry (report /tmp/jerry-big/t0-census.md, brief /tmp/jerry-big/t0census-brief.md); copy design next, after the T0 release.

Census done 2026-11-03 (/tmp/jerry-big/t0-census.md, 2690 lines; regenerate per its handoff section). T0 calls 114 tier-1 words and reaches 504 through them. It has 210 state cells, and 208 of them are shared with tier 1 or named by capture. NCOMP-DISPATCH is defined in src/habu/layout.f:903, and both tiers write its cells (t0.f:2035-2042, compiler.f:837-838). t0.f:1173 calls NFETCH-CHECK:TAGS by name, and T0 calls CHECKER-OWNER 25 times. Outside T0, only compiler.f and emit.f call the 21-definition callback group. Next: once the callback design (habu-make-a-cb-6de47af1) settles, a Fable Plan writes the copy design. Then the critic reviews it, and Joel names its public items one at a time.
