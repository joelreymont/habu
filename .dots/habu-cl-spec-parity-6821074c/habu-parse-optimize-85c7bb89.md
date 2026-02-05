---
title: Parse optimize
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-05T20:10:56.979725+01:00\""
closed-at: "2026-02-05T22:16:37.985158+01:00"
close-reason: Compiler now parses optimize in declare/declaim/proclaim and enforces safety=0 by suppressing emitted type checks
---

Context: /Users/joel/Work/habu/src/compiler/compile.zig:9621-9879; cause: optimize ignored; fix: add OptimizeSettings in Compiler/Env, parse optimize qualities in declare/declaim/proclaim; deps: none; verification: unit test compiler settings
