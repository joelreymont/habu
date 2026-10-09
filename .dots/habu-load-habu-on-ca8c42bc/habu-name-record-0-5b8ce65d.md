---
title: Name record 0 in the bound window and tick events
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-10-09T18:58:37.318658+03:00\\\"\""
closed-at: "2026-10-09T19:27:57.382529+03:00"
---

Problem: src/core/checker.f:12521-12526 REC>INDEX says index 0 "an index no record has, names none", but record 0 exists: native's is `engine-code-origin-set`, which no source can name, and the Gforth host's is `finally`, which src/core/dynamic-storage.f and lib/memory.f use. Index 0 then reads as no record in the tape's K-TICK and K-IS a0 (checker.f:13965-13966, 21076, 21166) and in the bound window's BOUND-RECORD tests (checker.f:13666, 13728; src/compiler/native/elaborate.f:899), so on the host a checked call or tick of `finally` reads as unbound.
Acceptance: REC>INDEX gives -1 for a null record, and its comment and the K-TICK/K-IS comments say every index from 0 names a record and -1 names none. A bound row whose kind is not BOUND-DICT stores -1 in BOUND-RECORD (checker.f:13459, 13484); the readers at checker.f:13666, 13728 and elaborate.f:899 drop their `BOUND-RECORD ... 0=` test, since kind BOUND-DICT already guarantees a record. checker-owner-abi.f states BOUND-RECORD's meaning beside the constant. Native behavior is unchanged; the Gforth codegen dot's cases call and tick `finally`.
Files: src/core/checker.f, src/core/checker-owner-abi.f, src/compiler/native/elaborate.f, and any reader of BOUND-RECORD or the K-TICK/K-IS a0 a search finds.
Verify: rebuild bin/hb per docs/gate.md; `bin/hb --load test/run.f`; two-generation build converges; `bin/hb --load tools/error-code-lint.f`.
Depends: none. Ownership: the files above. Worker: worker. Claim: agent=worker (lead carl) workspace=.jj-ws/carl-rec0.
