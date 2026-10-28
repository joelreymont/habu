---
title: Fix the admission and storage e2e regressions
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T03:27:44.788064+03:00"
closed-at: "2026-10-28T17:13:33.628923+02:00"
close-reason: landed ec4a6666, b61e06ee; admission and storage e2e pass in the gated native-file-auto row
---

Stack tip fails native-file-admission {30} and native-file-storage {4 6}; master passes; neither file is in test/run.f. Root cause in .jj-ws.noindex/carl-adm (on zoqvzolv), brief ~/.cache/tmp/carl-file/brief-adm.md.
