---
title: Make the source loader one package
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T18:10:42.420493+02:00"
---

Problem: src/core/include.f splits one subject, loading source files, into six packages: REQUIRE-REG, INCLUDE-EVAL-BIND, SOURCE-INPUT, SOURCE-ROOT (canonical paths, cwd, engine root, confinement, the include frame stack), SOURCE-UNIT and SOURCE-EVENT, and origin lane O1 adds SOURCE-FILE. Each seam turns an internal call into a public word: REQUIRE-REG:APPEND and PERSIST exist only so SOURCE-ROOT and INCLUDE-SNAPSHOT-PREPARE can call them, and SOURCE-FILE:INTERN and PERSIST (Joel approved 2026-11-02) only so PUSH and the snapshot step can. Joel asked 2026-11-02: 'why does this smell? why do we have both source-file and source-root? why do we need source-root?' His 2026-10-31 compiler ruling (LESSONS 'packages': 'One compiler package', a subject's files reopen one package and call shared words bare in its private section) applies. Acceptance: one loader package (name Joel's), its parts only where clashes demand them; the words only the loader calls go private; the public list (what programs and tools outside include.f call) goes to Joel with each word's callers. Files: src/core/include.f and every caller of the six packages. Verify: full suite, two-generation build. Depends: O1 lands first (its words move with the rest). Ownership: unassigned. Claim: unassigned.
