---
title: "PROCESS:ENV keeps one row per name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-03T10:17:16.894937+02:00"
---

Problem (env-dup review R3, 2026-11-03): src/lib/spawn.f PROCESS:ENV (:295) and ENV-ROWS (:342) push every row through ADD-ENV (:148-159), which appends, so two rows of one name reach the child's envp twice; a Habu child's GETENV reads the first, /bin/sh the last. INHERIT-ROW (:232) calls HAS-NAME? (:223-228), which scans ENVS for each parent row, so inheriting is quadratic in rows. process-command.f fixed the same two defects for CMD (commit 'Keep one env row per name in a command', dot habu-a-later-cmd-377637bf): a later caller row replaces an earlier one (carl's ruling under Joel's relayed 'use your best judgement', as Go's exec.Cmd.Env and PROC-ENV-SET do), and a MAP name index built per spawn makes both lookups constant time. Acceptance: a failing case first in src/lib/spawn-test.f through a real child: two PROCESS:ENV rows of one name give the child one row with the second value; inheriting a large parent environment is linear (measure rows vs wall before and after). Reuse process-command.f's index approach (or one shared envp builder for both, if the two libraries can share it without a new public word); no new public word without Joel's approval. Files: src/lib/spawn.f, src/lib/spawn-test.f. Verify: bin/hb --load src/lib/spawn-test.f, test/gate-pool.f's spawn rows. Depends: none (check habu-spawn-through-a-6227aeda and habu-convert-spawn-callers-791e8a01 first: if they retire PROCESS:ENV, fold this into them). Ownership: src/lib/spawn.f. Claim: unassigned.
