---
title: "Model CODEGEN:BUFFER names in the check.f preverify"
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T10:30:41.189018+03:00"
---

tools/check.f on any file that requires lib/process-env.f exits 70 with E-UNDEFINED at PROC-ENV-DIAG: the name preverify does not model a CODEGEN:BUFFER definition. Reported by two lane workers on 45608866; the engine load path accepts the same files.
