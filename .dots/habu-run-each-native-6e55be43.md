---
title: Run each native-file e2e on its own in the gate
status: open
priority: 2
issue-type: task
created-at: "2026-10-24T03:46:16.387716+03:00"
---

test/gate-stdlib-cases.f native-file-auto row loads several e2e files in one bin/hb run that stops at the first failing file; auto-e2e fails first, so storage and admission regressions were never reached. Each file needs its own verdict. Workspace: none yet.
