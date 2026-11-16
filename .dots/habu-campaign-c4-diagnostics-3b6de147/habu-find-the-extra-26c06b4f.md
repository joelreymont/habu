---
title: Find the extra registration after loading ffi-callback.f
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T08:31:47.418843+01:00"
closed-at: "2026-11-16T11:55:24.734582+01:00"
---

Problem: with qnwonwsz's ffi-callback.f, DYNAMIC-STORAGE:REGISTERED-N reads 60 before a fixture's first declaration where the previous file read 59 (/tmp/jerry-cbrest/fx). Acceptance: the registration is named and, if it is storage that should rest empty, released. Depends: none. Claim: unassigned.
