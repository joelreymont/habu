---
title: Delete unused diff subsystems
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.745566+03:00"
---

tools/lint/diff.f plus error module and tools/diff-side-content.f plus reader have no production consumer or documented command; only dedicated tests and gate rows. Delete them and exclusive test/registration code after confirming callers. Preserve other diff behavior and lint tools. Check remaining registrations/load references and focused affected test driver.
