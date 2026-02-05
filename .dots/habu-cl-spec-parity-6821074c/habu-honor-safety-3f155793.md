---
title: Honor safety
status: open
priority: 2
issue-type: task
created-at: "2026-02-05T20:11:00.481309+01:00"
---

Context: /Users/joel/Work/habu/src/compiler/compile.zig:3000-3115,9624; cause: assertions always emitted; fix: skip assert_* IR when safety=0, keep when safety>0; deps: habu-parse-optimize-85c7bb89; verification: test (declare (optimize (safety 0))) suppresses type error
