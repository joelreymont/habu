---
title: Fold HB_TMP into TMPDIR
status: active
priority: 1
issue-type: task
created-at: "2026-10-27T21:55:54.954817+02:00"
---

Joel 2026-10-27 (interview): fold HB_TMP into TMPDIR. HB_SOCK_TMP already carries every pool child's sockets, so the reason TMPDIR stayed unchanged is gone. Target: the gate pool sets TMPDIR to each row's reaped directory; HB_SOCK_TMP stays the short socket root at every nesting level (a nested pool roots its sockets under the inherited HB_SOCK_TMP, not its long TMPDIR); HB_TMP is deleted everywhere (243 mentions in 83 files: src/os/env-base.f:117, src/habu/stage2.f:27, src/lib/fs-mutate.f HB-TMP-MKDIR merges into TMPDIR-MKDIR keeping its refusals, src/lib/test/runner.f, bootstrap/cg/exec.fs, test/nf.fs, tools/bootstrap.sh, tools, tests, docs/gate.md, docs/bootstrap.md, docs/debugging.md, skills, INTEL.md). Every fallback is TMPDIR then /tmp. Hard cutover, no alias. Step 1 measures the open risk: external tools a row spawns (pg, bun, cc, codesign) placing sockets or locks under a long per-row TMPDIR. Clients after install: zen test/run.f, docs/development.md; tender test/package.f. Gates: full suite, two-gen (engine code changes), Gforth recovery (bootstrap files change). Workspace .jj-ws.noindex/carl-tmpdir.
