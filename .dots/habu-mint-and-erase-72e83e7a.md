---
title: Mint and erase a DEFLINEAR value from checked code
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T20:26:05.725562+03:00"
---

Problem: a DEFLINEAR type has no checked producer or consumer: CAST: refuses a linear source or destination (E-CAST-LINEAR, 7137) and a colon body cannot produce one, so every shipped linear resource (lib/json-read.f, lib/byte-edit.f, lib/xml/state.f, lib/process-pty-handle.f) mints and erases its token through a TRUSTED: trio, and a library forbidden TRUSTED: (lib/db/pq.f, 2026-09-16) had to replace linear ownership with a runtime slot registry. Acceptance: the checker offers a package-private mint and erase for a DEFLINEAR type declared in that package (the owner-private row mechanism of PPRIM:/CLOSE-PRIVATE, or a LINEAR-MINT:/LINEAR-ERASE: pair the type's defining package alone may call), so the owning module produces and consumes its token in checked code and no other scope can; the four shipped linear modules converted one file per commit with their TRUSTED: trios deleted; lib/db/pq.f's result owner moved to it in a follow-up dot. Files: src/core/checker.f, lib/type/deflinear.f (or where DEFLINEAR lives), the four modules, tests in test/cast-negative-suite.f and lib/type tests. Verify: the linear suites and test/run.f green on a rebuilt engine; rg -c '^TRUSTED:' on the four modules drops by three each. Depends: habu-honour-owner-private-0a19f45d. Ownership: checker and lib/type. Claim: unassigned.
