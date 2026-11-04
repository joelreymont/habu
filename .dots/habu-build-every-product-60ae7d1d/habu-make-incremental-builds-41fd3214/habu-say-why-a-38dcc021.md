---
title: Say why a stale replay exits 65
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T03:46:16.363227+03:00"
closed-at: "2026-10-28T17:13:33.928350+02:00"
close-reason: "obsolete: BUILD-CACHE:RETRY-RC is gone"
---

A direct maker caller with no exclusion file exits BUILD-CACHE:RETRY-RC (65) on every stale replay with no message (seen in test/native-file-admission-e2e.f). Found by lane carl-adm. Workspace: none yet.
