---
title: "Close DYNAMIC-BUFFER's forge through its generated base and control accessors"
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T00:45:39.569325+02:00"
---

Problem: a DYNAMIC-BUFFER declaration generates public DB#base and DB#ctl accessors (src/core/layout-buffer.f), so user code can store any bytes into the head and read a typed element out of them: the ARENA review's probe p5 forged a ptr n = 65 from a BUFFER: with no CAST. The same class as the ARENA review's findings 1 and 2 (review-arena.md), fixed there for arenas. Acceptance: checked code cannot write a DYNAMIC-BUFFER's head or control cells nor copy them whole; the generated accessors user code can reach are the typed element words only; a probe like p5 is refused with its rule named, as a test/dynamic-buffer.f row; every existing DYNAMIC-BUFFER user (rg DYNAMIC-BUFFER src tools test) still certifies. Files: src/core/layout-buffer.f, src/core/dynamic-storage.f, test/dynamic-buffer.f, docs/forth.md. Verify: test/dynamic-buffer.f, the full suite (layout-buffer.f is engine-baked). Depends: habu-fresh-storage-laundered-75233774 (N9-d, ruling R3: the declared head is the authority). Ownership: those files. Claim: unassigned.
