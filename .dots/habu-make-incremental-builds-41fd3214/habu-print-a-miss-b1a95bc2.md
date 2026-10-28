---
title: Print a miss line for a changed source
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T03:46:16.374946+03:00"
closed-at: "2026-10-28T17:13:33.737676+02:00"
close-reason: landed d3a32c33; native-file-auto e2e asserts the reason=input-stale miss line
---

With HABU_NATIVE_CACHE_DIAG=1, a lib.f source change prints no native-file: miss diag line (test/native-file-storage-e2e.f scenario). Found by lane carl-adm. Workspace: none yet.
