---
title: "Size the clobber lint's tables from its scan"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:41:40.788231+02:00"
---

Problem: the clobber lint (tools/lint/clobber-lint-core.f, which reports the registers each emitter routine reached by a branch-and-link call clobbers) keeps fixed tables: RRMAX 16 (:416), CMAX $800 callable labels (:712; the label table held 478 entries on the engine), EMAX $2000 call edges (:713), CNBUF-CAP $10000 (:714), OMAX $80 (:778), and the 128-byte WNAME (:1264), which WORD-NAME! (:1269-1270) fills through FOLD-TO with no length check. HOSTS is already sized from the scan (:805, HOSTS-OPEN :1066-1069). The lint also gives a row only to colon definitions, so a name another definer makes does not hide a global host word: a package's '12 constant SCRATCH' counts 1, a false finding; no engine name has that shape today. It reads src/habu/rt.f, crash.f, regalloc.f, habu1.f, jit.f, prof.f and habu2.f (EACH-FILE :1522-1526); habu1.f, habu2.f and jit.f are machinery being replaced (docs/design.md "Machinery being replaced"), so its subject shrinks as they go. Acceptance: every table is sized from the scan as HOSTS is, WORD-NAME! cannot overrun WNAME, and every name a top-level definer makes gets a row that emits nothing, so the constant case counts 0. Files: tools/lint/clobber-lint-core.f, a new test of it. Verify: 'bin/hb --load tools/lint/clobber-lint.f' gives the same findings before and after on master; a test with the package-constant case counts 0. Depends: none. Ownership: unassigned. Claim: unassigned.
