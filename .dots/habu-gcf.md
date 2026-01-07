---
title: Fix FASL extern-call marker handling for multi-file linking
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-03T18:43:13.215403+02:00"
closed-at: "2025-12-03T19:21:06.139757+02:00"
close-reason: ""
---

compile-file-to-fasl fails when source uses syscalls because resolve-calls leaves :extern-call markers in bytecode. The FASL writer tries to write these as bytes and fails. Options: (1) resolve extern-calls before FASL write, (2) store markers separately in FASL format, (3) skip FASL for now, use deliver-file per component.
