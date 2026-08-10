---
title: Prove the room is taken where the commit cannot throw
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T10:14:27.813018+02:00"
---

Both grown publication tables (clobber.f rows, publish.f log columns) take their vector room in the VALIDATION phase so the commit phase cannot throw - but removing that room-taking is caught by NO test in either file, because the append's own backstop masks it (the structural ceilings sit at ~2M rows, unreachable by any suite; publog lane 2026-08-10, and clobber.f has the same gap for the same reason). Close it by mutation-as-fixture: a test that runs the seam with the validation-phase room-taking deleted (patch the word under test the way the parity gates mutate) and asserts the commit-phase throw is observed - or derive a cheaper structural proof (e.g. the commit phase words contain no allocation calls, linted). Files: src/compiler/native/{publish,clobber}.f tests. Depends: none.
