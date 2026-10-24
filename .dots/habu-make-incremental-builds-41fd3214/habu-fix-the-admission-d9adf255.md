---
title: Fix the admission and storage e2e regressions
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T03:27:44.788064+03:00"
---

Stack tip fails native-file-admission {30} and native-file-storage {4 6}; master passes; neither file is in test/run.f. Root cause in .jj-ws.noindex/carl-adm (on zoqvzolv), brief ~/.cache/tmp/carl-file/brief-adm.md.
