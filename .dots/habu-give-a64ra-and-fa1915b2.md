---
title: Give A64RA and A64RAV a nominal register-file type
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T11:17:16.537768+02:00"
---

Problem: after habu-key-a64rav-interference-151111d3 every pool, holder-table and interference question in src/compiler/native/regalloc.f and regalloc-verify.f takes a register FILE, obtained only from FILE-OF/FILE-AT. Both a class and a file are a bare 'n' to the checker, so a caller that writes 'i CLS-AT' where 'i FILE-AT' is meant type-checks. The confusion is not hypothetical arithmetic: C-GPR and F-GPR are both 0, so the mistake works for the general file and silently misindexes for the floating one - the same species of quiet failure the dot above closed.

Acceptance: mint a package-scoped nominal for the file (NEWTYPE file 0 or DEFTYPE FILE, docs/forth.md 'Declare application value nominals with DEFTYPE') in A64RA and in A64RAV; FILE-OF answers it; POOL-BITS, CALL-BITS, RIX, HOLD-AT, HOLD!, FREE-REG, POOL-HAS?, MB-FREE-N, MB-FRAMED?, MB-LOAD-N, MB-STORE-N, MB-SHORT!, MB-SPARE-N, MB-FURTHEST, MB-VICTIM, MB-EVICT, IN-FILE?, FILE-POOL and VCALL-BITS declare it; a negative checker fixture proves a class passed where a file is wanted is REJECTED at build time, not at run time.

Probe first (CLAUDE.md Simplify Relentlessly): NOFILE is -1 and the tables index by the file, so decide whether the nominal crosses to a raw cell through an audited private mint or whether RIX keeps the only projection. If the ceremony costs more than the one bounds check RIX already carries, record that and close this dot as answered.

Files: src/compiler/native/regalloc.f, src/compiler/native/regalloc-verify.f. Verify: bin/hb --load test/compiler/native-regalloc.f; test/run.f; codegen-compare. Depends: none. Ownership: those two files. Claim: unassigned.

Found while fixing habu-key-a64rav-interference-151111d3.
