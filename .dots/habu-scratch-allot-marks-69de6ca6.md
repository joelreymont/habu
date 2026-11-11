---
title: SCRATCH-ALLOT marks cells before allot checks space
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:24:12.616504+02:00"
---

Problem: src/core/pointer-storage.f:5-6 SCRATCH-ALLOT marks every cell from here upward with scratch-cell-mark and only then calls allot, so a size larger than the space left writes marks past the dictionary before allot can refuse; a very large size loops for as long as the marks take (observed 2026-11-11 by the band review: ~/.cache/tmp/carl-band/rv/p/wrap.f, 'TASK:#USER $7FFFFFFFFFFFFFF0 TASK:+USER PW', hung in SCRATCH-ALLOT on commit b6c80758). The band commit refuses a wrapping +USER claim in USER-NEXT; a large claim that does not wrap still reaches SCRATCH-ALLOT, and +USER (src/lib/task.f) stores TASK-USER-NEXT before SLOT-LAY has laid the body, so a refused body leaves the counter advanced. Acceptance: SCRATCH-ALLOT refuses a size that does not fit before any mark is written (allot first, then mark the cells it took), a negative size included; +USER advances TASK-USER-NEXT only after the body is laid; a test holds 'create X <huge> SCRATCH-ALLOT' refused without a hang, and a refused +USER leaving TASK:#USER unchanged. Files: src/core/pointer-storage.f, src/lib/task.f, their tests. Verify: the two tests on a rebuilt hb; engine third generation equals the second. Depends: habu-user-band-cannot-4ca18d06 step one landed (same lines of task.f) (waits under docs/gate.md 'Until Zen, Tender and Maki are released': no consumer meets it). Ownership: carl. Claim: unassigned.

Reproducer for a claim under the ceiling (band re-review 2026-11-11): ~/.cache/tmp/carl-band/rv2/p/huge.f claims $7FFFFFFFFFFD0000, prints 'before 2896' and hangs (killed at 10 s).
